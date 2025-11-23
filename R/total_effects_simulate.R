
#' Simulate total effects under an ODE model
#'
#' Runs a simulation of total effects using a user-supplied ODE function.
#'
#' @param PARMSET A list with \code{yeqm} (named numeric vector of equilibrium states)
#'   and \code{parameters} (list), optionally including \code{Qmat}.
#' @param IDS Character vector of node names to perturb; must be present in
#'   \code{names(PARMSET$yeqm)}.
#' @param SIM_TIME Positive integer; number of time steps.
#' @param perturbation_factor Numeric scalar multiplier applied to the perturbed node’s
#'   equilibrium state (e.g., \code{0.9} for a 10\% decrease).
#' @param ode_function A function implementing the ODE system. Must be compatible with
#'   \pkg{deSolve}’s \code{\link[deSolve]{ode}} interface:
#'   \code{function(t, y, parms, ...)} and should return a \emph{list} whose first element
#'   is the derivative vector, and that includes a named component \code{direct_effect}
#'   used by this function. Defaults to \code{\link{foodwebode}}.
#'
#' @return A list with \code{output_direct}, \code{output_total}, and \code{output_baseline}.
#'
#' @seealso [deSolve::ode()]
#' @importFrom deSolve ode
#' @export
total_effect_simulate <- function(PARMSET,
                                  IDS = NA,
                                  SIM_TIME = 10,
                                  perturbation_factor = 0,
                                  ode_function = foodwebode) {

  # ---- Input validation ----
  if (is.null(PARMSET) || !is.list(PARMSET)) {
    stop("PARMSET must be a list containing `yeqm` and `parameters`.")
  }
  if (is.null(PARMSET$yeqm) || is.null(PARMSET$parameters)) {
    stop("PARMSET must include `yeqm` (named numeric vector) and `parameters` (list).")
  }
  if (any(is.na(IDS)) || length(IDS) == 0) {
    stop("IDS must be a non-empty character vector of node names in PARMSET$yeqm.")
  }
  if (!all(IDS %in% names(PARMSET$yeqm))) {
    missing_ids <- IDS[!IDS %in% names(PARMSET$yeqm)]
    stop("All IDS must be present in names(PARMSET$yeqm). Missing: ",
         paste(missing_ids, collapse = ", "))
  }
  if (!is.numeric(SIM_TIME) || length(SIM_TIME) != 1 || SIM_TIME < 1) {
    stop("SIM_TIME must be a positive integer.")
  }
  if (!is.numeric(perturbation_factor) || length(perturbation_factor) != 1) {
    stop("perturbation_factor must be a numeric scalar.")
  }

  # ---- Initialize outputs ----
  output <- vector("list", length(IDS))
  names(output) <- IDS
  output_direct <- output
  output_baseline <- output

  # ---- Pre-compute baseline equilibrium direct effects ----
  y_orig <- PARMSET$yeqm
  baseline <- ode_function(1, y_orig, PARMSET$parameters)$direct_effect

  # ---- Iterate over perturbed nodes ----
  for (idd in seq_along(IDS)) {
    pert_name <- IDS[idd]

    # Perturb equilibrium state
    y_mod <- y_orig
    y_mod[names(y_mod) == pert_name] <- y_mod[names(y_mod) == pert_name] * perturbation_factor

    # ODE integration: Modifed start
    outputsave <- deSolve::ode(y = y_mod,
                               t = 1:SIM_TIME,
                               func = ode_function,
                               parms = PARMSET$parameters)

    # Keep only direct_effect.* columns and strip prefix
    outputsave <- outputsave[, grepl("direct_effect", colnames(outputsave)), drop = FALSE]
    colnames(outputsave) <- gsub("direct_effect\\.", "", colnames(outputsave))

    # Sanity checks align with baseline vector
    stopifnot(length(baseline) == ncol(outputsave))
    stopifnot(identical(colnames(outputsave), names(baseline)))

    # Extract direct effects for each element:
    output_direct_prep <- outputsave[,grepl(paste0("^", pert_name), colnames(outputsave))]
    colnames(output_direct_prep) = sub("^[^_]+_", "", colnames(output_direct_prep))
    output_direct[[idd]] = as.data.frame(time = 1:SIM_TIME, output_direct_prep)

    # Save equilibrium direct contribution for the perturbed node
    output_baseline[[idd]] <- baseline[grepl(paste0("^", pert_name), names(baseline))]

    # Baseline-adjust (column-wise subtraction)
    outputsave <- sweep(outputsave, MARGIN = 2, STATS = baseline, FUN = "-")*-1 # Because we actually want to substract with - without.


    # Extract the suffix (second element) from each column name
    suffix <- sub("^[^_]+_", "", colnames(outputsave))  # everything after the first underscore

    # Sum across columns by suffix, per row
    summed_by_suffix <- t(rowsum(t(outputsave), group = suffix))

    # Store with time
    output[[idd]] <- as.data.frame(cbind(time = 1:SIM_TIME, summed_by_suffix[,colnames(PARMSET$parameters$Qmat)]))
  }

  # ---- Combine per-node outputs; requires add_node_column() to exist ----
  output <- add_node_column(output)
  output_direct <- add_node_column(output_direct)

  # ---- Compile direct contributions table ----
  output_baseline <- do.call("rbind", output_baseline)
  output_baseline <- data.frame(Node = IDS, output_baseline, check.names = FALSE)
  rownames(output_baseline) <- NULL

  # Set column names to match Qmat columns (assumes alignment)
  if (!is.null(PARMSET$parameters$Qmat)) {
    qn <- colnames(PARMSET$parameters$Qmat)
    if (!is.null(qn) && length(qn) == ncol(output_baseline) - 1) {
      colnames(output_baseline) <- c("Node", qn)
    } else {
      warning("PARMSET$parameters$Qmat column names not used: length mismatch or NULL.")
    }
  } else {
    warning("PARMSET$parameters$Qmat is NULL; leaving output_direct column names as-is.")
  }

  # ---- Return ----
  return(list(output_direct = output_direct, output_total = output, output_baseline = output_baseline))
}
