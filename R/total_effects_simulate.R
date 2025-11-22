
#' Simulate total effects under node-specific perturbations
#'
#' @description
#' Runs a short ODE simulation applying a multiplicative perturbation to selected
#' state variables (nodes) and computes the **total effect** time series relative
#' to the original equilibrium, along with the **direct effect** contributions
#' at equilibrium.
#'
#' This function:
#' 1. Takes an equilibrium state `PARMSET$yeqm` and parameter list `PARMSET$parameters`.
#' 2. For each node ID in `IDS`, scales the corresponding state by `perterbation_factor`.
#' 3. Integrates the ODE system via [deSolve::ode()] for `t = 1:SIM_TIME`.
#' 4. Extracts `direct_effect.*` outputs from `foodwebode()`, subtracts the
#'    baseline equilibrium direct effect vector to obtain **total effects** over time.
#' 5. Returns a list containing:
#'    - `output_direct`: a data frame of direct contributions (equilibrium) per perturbed node.
#'    - `output_total`: a long-format data frame of total effects over time for all perturbed nodes.
#'
#' @details
#' - The function assumes your ODE right-hand side function `foodwebode(time, state, parms)`
#'   returns a named list with an element `direct_effect` that is a named numeric vector.
#' - Columns in the ODE output prefixed with `"direct_effect."` are interpreted as the
#'   direct-effect time series and are baseline-adjusted (subtracting the equilibrium direct
#'   effect vector) to form total effects.
#' - The helper `add_node_column()` must accept the list of per-node data frames (each with
#'   a `time` column and direct-effect columns) and return a **wide** data frame that includes
#'   a `Node` column identifying the perturbed node.
#'
#' @param PARMSET A list containing:
#'   - `yeqm`: named numeric vector of equilibrium state values.
#'   - `parameters`: list of parameters to pass into `foodwebode()`. Must include a matrix
#'     `Qmat` used to set column names in the `output_direct` result.
#' @param IDS Character vector of node IDs (names in `PARMSET$yeqm`) to perturb.
#' @param SIM_TIME Integer length of the simulation in time steps (default `10`).
#'   The solver is called with `t = 1:SIM_TIME`.
#' @param perterbation_factor Numeric scalar multiplier applied to the chosen node's
#'   equilibrium state prior to simulation (default `0`). **Note:** parameter name
#'   is intentionally spelled as in the original code for backward compatibility.
#'
#' @returns
#' A list with two elements:
#' \itemize{
#'   \item \code{output_direct}: A data frame with columns
#'     \code{Node} and one column per element of \code{PARMSET$parameters$Qmat}, holding
#'     the equilibrium direct-effect contribution of the perturbed node.
#'   \item \code{output_total}: A long-format data frame with columns
#'     \code{time}, \code{Node}, \code{variable}, and \code{value}, representing baseline-adjusted
#'     total effects over time for each direct-effect variable and perturbed node.
#' }
#'
#' @section Assumptions and checks:
#' - All \code{IDS} must exist in \code{names(PARMSET$yeqm)}.
#' - \code{foodwebode()} must be available in scope and return \code{direct_effect}.
#' - \code{PARMSET$parameters$Qmat} must have column names aligned with \code{direct_effect}.
#'
#' @examples
#' \dontrun{
#' # Minimal example (assuming foodwebode, add_node_column, and PARMSET are defined)
#' res <- total_effect_simulate(
#'   PARMSET = PARMSET,
#'   IDS = c("bacteria", "fungi"),
#'   SIM_TIME = 25,
#'   perterbation_factor = 0.5
#' )
#'
#' # Direct contributions:
#' head(res$output_direct)
#'
#' # Long-format total effects:
#' head(res$output_total)
#' }
#'
#' @seealso [deSolve::ode()]
#' @importFrom deSolve ode
#' @export
total_effect_simulate <- function(PARMSET,
                                  IDS = NA,
                                  SIM_TIME = 10,
                                  perterbation_factor = 0) {

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
  if (!is.numeric(perterbation_factor) || length(perterbation_factor) != 1) {
    stop("perterbation_factor must be a numeric scalar.")
  }

  # ---- Initialize outputs ----
  output <- vector("list", length(IDS))
  names(output) <- IDS
  output_direct <- output

  # ---- Pre-compute baseline equilibrium direct effects ----
  y_orig <- PARMSET$yeqm
  baseline <- foodwebode(1, y_orig, PARMSET$parameters)$direct_effect

  # ---- Iterate over perturbed nodes ----
  for (idd in seq_along(IDS)) {
    pert_name <- IDS[idd]

    # Perturb equilibrium state
    y_mod <- y_orig
    y_mod[names(y_mod) == pert_name] <- y_mod[names(y_mod) == pert_name] * perterbation_factor

    # ODE integration
    outputsave <- deSolve::ode(y = y_mod,
                               t = 1:SIM_TIME,
                               func = foodwebode,
                               parms = PARMSET$parameters)

    # Keep only direct_effect.* columns and strip prefix
    outputsave <- outputsave[, grepl("direct_effect", colnames(outputsave)), drop = FALSE]
    colnames(outputsave) <- gsub("direct_effect\\.", "", colnames(outputsave))

    # Sanity checks align with baseline vector
    stopifnot(length(baseline) == ncol(outputsave))
    stopifnot(identical(colnames(outputsave), names(baseline)))

    # Save equilibrium direct contribution for the perturbed node
    output_direct[[idd]] <- baseline[grepl(paste0("^", pert_name), names(baseline))]

    # Baseline-adjust (column-wise subtraction)
    outputsave <- sweep(outputsave, MARGIN = 2, STATS = baseline, FUN = "-")

    # Store with time
    output[[idd]] <- as.data.frame(cbind(time = 1:SIM_TIME, outputsave))
  }

  # ---- Combine per-node outputs; requires add_node_column() to exist ----
  output <- add_node_column(output)

  # ---- Compile direct contributions table ----
  output_direct <- do.call("rbind", output_direct)
  output_direct <- data.frame(Node = IDS, output_direct, check.names = FALSE)
  rownames(output_direct) <- NULL

  # Set column names to match Qmat columns (assumes alignment)
  if (!is.null(PARMSET$parameters$Qmat)) {
    qn <- colnames(PARMSET$parameters$Qmat)
    if (!is.null(qn) && length(qn) == ncol(output_direct) - 1) {
      colnames(output_direct) <- c("Node", qn)
    } else {
      warning("PARMSET$parameters$Qmat column names not used: length mismatch or NULL.")
    }
  } else {
    warning("PARMSET$parameters$Qmat is NULL; leaving output_direct column names as-is.")
  }

  # ---- Reshape to long format for total effects ----
  id_cols <- c("time", "Node")
  varying_cols <- setdiff(names(output), id_cols)

  output <- reshape(
    output,
    varying = varying_cols,
    v.names = "value",
    timevar = "variable",
    times = varying_cols,
    idvar = id_cols,
    direction = "long"
  )

  row.names(output) <- NULL
  output <- output[order(output$time, output$Node), ]

  # ---- Return ----
  return(list(output_direct = output_direct, output_total = output))
}
