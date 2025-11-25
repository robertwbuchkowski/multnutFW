#' tracer_function: decoupled tracer ODE using main simulation output
#'
#' @param t    numeric, time
#' @param y    numeric vector, tracer Carbon stocks per node (length = nrow(pars$pmat))
#' @param pars list of parameters, must include everything used by the original model AND:
#'             - pars$main_out: matrix/data.frame from deSolve::ode() for the main run (no tracers),
#'                              with columns c("time", <state names in original order>).
#'             - OPTIONAL pars$state_interps: named list of approxfun() per state column
#'               (built once for speed). If absent, they are built on first call.
#' @return list(dy, fluxTRESPtotal = vector) in the format expected by deSolve
#'
#'@export
tracer_function <- function(t, y, pars) {
  # ---- Basic guards & options ----
  if (!is.null(pars$forcepositive) && pars$forcepositive == 1) {
    y <- pmax(y, 0)  # Clamp to positive tracer stocks
  }
  if (is.null(pars$main_out)) {
    stop("tracer_function() requires pars$main_out from the main ODE run (without tracers).")
  }
  # Machine epsilon for safe divisions:
  eps <- .Machine$double.eps

  # ---- Build (or use cached) interpolation functions for each main state ----
  main_out <- pars$main_out
  if (is.null(pars$state_interps)) {
    # Build once; keep names for direct access
    state_names <- colnames(main_out)[-1]        # exclude "time"
    times_vec   <- main_out[, "time"]
    state_interps <- stats::setNames(
      lapply(state_names, function(sn) stats::approxfun(times_vec, main_out[, sn], method = "linear", rule = 2)),
      state_names
    )
    # Cache into pars (deSolve will pass it back each step)
    pars$state_interps <- state_interps
  } else {
    state_interps <- pars$state_interps
    state_names   <- names(state_interps)
  }

  # ---- Interpolate the full main state vector at time t (in original order) ----
  # IMPORTANT: We assume main_out was generated WITHOUT tracer, so its state ordering matches
  # c(netwithmineralization[,1], D_element_biomass) from your original foodwebode().
  y_main_t <- vapply(state_names, function(sn) state_interps[[sn]](t), numeric(1))

  # ---- Reconstruct unstandardized pools used by the original algebra ----
  # Scale to "yunstd" space as in the original function:
  yunstd <- y_main_t * pars$eqmStandard

  # Index helpers based on parameter sizes:
  n_nodes     <- nrow(pars$pmat)                    # number of nodes
  n_elements  <- ncol(pars$pmat)                    # columns of Qmat/pmat (first = Carbon)
  det_idx     <- pars$detplant$isDetritus == 1      # logical vector of detritus nodes
  n_det       <- sum(det_idx)
  # Detritus element stocks length equals n_det * (n_elements - 1)
  det_len     <- n_det * (n_elements - 1)

  # Partition yunstd (same slicing used in your original code):
  biomass_C   <- yunstd[1:n_nodes]                                            # Carbon biomass per node
  Det_stocks  <- yunstd[(n_nodes + 1):(n_nodes + det_len)]                    # detritus element stocks

  # ---- Build Qmat and ymat exactly as the original function does ----
  Qmat <- pars$Qmat
  # Detritus element stoichiometry from dynamic Det_stocks:
  # Det_Qmat is a row-wise vector for detritus nodes across the non-Carbon elements
  Det_Qmat <- Det_stocks / pmax(biomass_C[det_idx], eps)
  Det_Qmat[!is.finite(Det_Qmat)] <- 0

  # Replace detritus rows in Qmat with dynamic composition: c(Carbon column, Det_Qmat ...)
  # Note: Carbon column in Qmat remains as-provided.
  Qmat[det_idx, ] <- c(Qmat[det_idx, 1], Det_Qmat)

  # Elemental pools by node:
  ymat <- biomass_C * Qmat                       # n_nodes x n_elements

  # ---- Consumption & all core fluxes (copied from original logic) ----
  # Prepare consumer-prey Carbon matrices:
  predC <- matrix(ymat[, 1], nrow = nrow(pars$cij), ncol = ncol(pars$cij))
  preyC <- t(predC)

  # Holling type II (as in original):
  consumption_Carbon <- pars$cij * predC * preyC / (1 + pars$cij * pars$h * preyC)

  # Element-wise consumption list across elements:
  consumption <- lapply(seq_len(n_elements), function(lai) {
    consumption_Carbon * matrix(Qmat[, lai], nrow = nrow(pars$cij), ncol = ncol(pars$cij), byrow = TRUE)
  })
  rm(predC, preyC, consumption_Carbon)

  # Fluxes:
  fluxIO      <- pars$externalinputs                           # gains from outside
  fluxOI      <- pars$nodeloss * ymat                          # losses to outside (per element)
  fluxCONSUMP <- pars$pmat * sapply(Map(function(XX, YY) XX * YY, consumption, pars$assimilation), rowSums)
  fluxPRED    <- sapply(consumption, colSums)

  fluxDEATH <- matrix(
    pars$death[, 1] * (1 - pars$death[, 3]) * ymat[, 1] +         # density-independent
      pars$death[, 2] * (pars$death[, 3]) * ymat[, 1] * ymat[, 1], # density-dependent
    nrow = nrow(ymat), ncol = ncol(ymat)
  ) * Qmat

  # Detritus recycling allocations:
  fluxFAECES <- matrix(pars$detplant$FecalRecycling, nrow = nrow(ymat), ncol = ncol(ymat)) *
    matrix(
      colSums(sapply(
        Map(function(XX, YY) XX * YY, consumption, lapply(pars$assimilation, function(X) 1 - X)),
        rowSums
      )),
      nrow = nrow(ymat), ncol = ncol(ymat), byrow = TRUE
    )

  fluxCARCASS <- matrix(pars$detplant$NecromassRecycling, nrow = nrow(ymat), ncol = ncol(ymat)) *
    matrix(
      colSums(
        matrix(
          pars$death[, 1] * (1 - pars$death[, 3]) * ymat[, 1] +    # density-independent death
            pars$death[, 2] * pars$death[, 3] * ymat[, 1] * ymat[, 1],
          nrow = nrow(ymat), ncol = ncol(ymat)
        ) * Qmat
      ),
      nrow = nrow(ymat), ncol = ncol(ymat), byrow = TRUE
    )

  # ---- Mineralization block to reproduce overflow_resp and respiration terms ----
  netwithoutmineralization <-
    fluxIO - fluxOI + fluxCONSUMP - fluxPRED - fluxDEATH + fluxFAECES + fluxCARCASS

  # Remove minimum respiration based on biomass (Carbon column only):
  netwithrespiration <- netwithoutmineralization -
    matrix(c(pars$ECarbon * ymat[, 1], rep(0, nrow(ymat) * (ncol(ymat) - 1))),
           nrow = nrow(ymat), ncol = ncol(ymat))

  ratios <- netwithrespiration / Qmat                         # element-wise division
  mineralization_0 <- sweep(ratios, 1, netwithrespiration[, 1], "-")
  mineralization_0 <- mineralization_0 * (1 - pars$canIMMmat) # remove immobilization

  overflow_resp <- -1 * apply(mineralization_0, 1, min)
  overflow_resp[det_idx] <- 0                                 # no overflow for detritus

  # Apply overflow respiration back to netwithrespiration (Carbon column):
  netwithrespiration[, 1] <- netwithrespiration[, 1] - overflow_resp

  # Final mineralization and net:
  mineralization <- netwithrespiration - (netwithrespiration[, 1] * Qmat)
  mineralization[which(det_idx), ] <- 0
  netwithmineralization <- netwithrespiration - mineralization

  # ---- Tracer algebra (decoupled): tracer13C computed from current tracer stocks & biomass) ----
  # Tracer stock y is Carbon tracer per node; convert to tracer fraction of Carbon pool:
  denom <- pmax(ymat[, 1], eps)
  if (any(denom == eps)) warning("Tracer_function: biomass Carbon hitting minimum value at time ", t)
  tracer13C <- y / denom

  # Safety: warn if any fraction exceeds 1 (ignoring non-finite)
  if (any(tracer13C > 1 & is.finite(tracer13C))) {
    warning("Tracer fraction exceeds 100% of the pool at time ", t, ".")
  }

  # Tracer fluxes (mirror of original formulas):
  fluxTCONSUMP     <- consumption[[1]] *
    matrix(tracer13C, nrow = nrow(consumption[[1]]), ncol = ncol(consumption[[1]]), byrow = TRUE)

  fluxTPRED        <- fluxPRED[, 1]  * tracer13C
  fluxTDEATH       <- fluxDEATH[, 1] * tracer13C
  fluxTCARCASS     <- pars$detplant$NecromassRecycling * sum(fluxTDEATH)

  # Note: pars$assimilation$Carbon is the Carbon element's assimilation matrix
  fluxTCONSUMPSUM  <- pars$pmat[, 1] * rowSums(pars$assimilation$Carbon * fluxTCONSUMP)
  fluxTFAECES      <- pars$detplant$FecalRecycling * sum((1 - pars$assimilation$Carbon) * fluxTCONSUMP)

  fluxTLOSS        <- fluxOI[, 1] * tracer13C
  fluxTINPUT       <- pars$externalinputs[,1]*pars$tracer

  # Respiration loss of tracer (fractionation can be added via multiplier):
  fluxTRESP        <- (pars$ECarbon * ymat[, 1] + overflow_resp) * tracer13C * 1
  fluxTRESPtotal   <- fluxTRESP + (1 - pars$pmat[, 1]) * rowSums(pars$assimilation$Carbon * fluxTCONSUMP)

  # Net tracer dynamics:
  nettracer <- fluxTINPUT - fluxTLOSS + fluxTCONSUMPSUM - fluxTPRED - fluxTDEATH +
    fluxTFAECES + fluxTCARCASS - fluxTRESP

  # Prepare the final output:
  dy <- nettracer
  names(dy) <- names(y)

  # Return in deSolve format; we keep fluxTRESPtotal as an attribute-like output
  return(list(dy, fluxTRESPtotal = fluxTRESPtotal))
}



#' Prepare tracer simulation inputs from main ODE output
#'
#' @param main_parms list of parameters used in the main simulation. Comes from getPARAMS function.
#' @param tracer_init named numeric vector of initial tracer proportions in the biomass at the equilibrium in main_parms (length = nrow(main_parms$pmat))
#' @param tracer_input_proportion the proportion of tracer in input fluxes
#' @param SIM_TIME The simulation time if main_out is NULL.
#' @param eqm_mod The equilibrium modification if main_out is NULL. It is a vector the same length as the number of state variables.
#' @return The tracer simulation over the same time period as main_out.
#' @export
prepare_tracer_inputs <- function(main_parms = NULL, tracer_init = NULL, tracer_input_proportion = NULL, SIM_TIME = 1:10, eqm_mod = NULL) {

  # y values:
  y_val = main_parms$yeqm
  main_parms = main_parms$parameters

  if(is.null(eqm_mod)){
    eqm_mod = rep(1, length(main_parms$eqmStandard))
  }
  main_out_full = deSolve::ode(
    y = y_val*eqm_mod,
    times = SIM_TIME,
    func = foodwebode,
    parms = main_parms
  )
  main_out = main_out_full[,1:(length(main_parms$eqmStandard) + 1)]

  # Validate main_out
  if (!"time" %in% colnames(main_out)) stop("main_out must include a 'time' column.")

  if(all(colnames(main_out)[-1] != names(main_parms$eqmStandard))) stop("main_out must only include the state variables, not the extra values")

  main_parms$main_out = main_out

  # Attach main_out to tracer_parms
  if(length(tracer_input_proportion)!= nrow(main_parms$cij)) stop("tracer_input_propoation must match the number of state variables.")

  main_parms$tracer = tracer_input_proportion

  # Build interpolation functions for speed
  times_vec <- main_out[, "time"]
  state_names <- colnames(main_out)[-1]  # exclude time
  state_interps <- stats::setNames(
    lapply(state_names, function(sn) stats::approxfun(times_vec, main_out[, sn], method = "linear", rule = 2)),
    state_names
  )
  main_parms$state_interps <- state_interps

  # Validate tracer_init length
  if (length(tracer_init) != nrow(main_parms$pmat)) {
    stop("tracer_init must have length equal to number of nodes (nrow(main_parms$pmat)).")
  }

  # Tracer stocks:
  tracer_stocks = tracer_init*main_parms$eqmStandard[1:length(tracer_init)]

  # Run the ode:
  tracer_output = deSolve::ode(y = tracer_stocks, times = c(main_out[,1]),
               func = tracer_function, parms = main_parms)

  # Mark the tracer pools to combine cleanly:
  tocn = colnames(tracer_output)
  tocn[!grepl("fluxTRESPtotal",tocn)] = paste0(tocn[!grepl("fluxTRESPtotal",tocn)], "_tracer")
  colnames(tracer_output) = tocn

  # Check to make sure the time columns line up:
  if(all(main_out_full[,1] != tracer_output[,1])) stop("Main output and tracer columns are not lining up. Please check the function.")

  cbind(main_out_full, tracer_output[,-1])
}
