#' Direct and indirect contributions to mineralizations
#'
#' @param usin The community in which we want to calculate mineralization rates.
#' @param selected A vector of names for which you want to calculate the direct and indirect effects. Default NULL means all of them. Useful for excluding nodes whose removal breaks the community (i.e., basal nodes)
#' @param simulation_params The simulation parameter set to calculate the indirect effects after simulating a new equilibrium. If left at default NULL, then only indirect static effects are calculated.
#' @param mod_stoich Should the simulation modify the nutrient content of detritus based on the new simulated equilbrium? TRUE or FALSE.
#' @param extinct_threshold The equilibrium biomass where an organism should be considered extinct when simulating indirect effects. This will zero out the biomass and cause a coextinction flag. This always happens when biomass is negative.
#' @param n_sim_trials The number of trial starting vectors used for the simulation of indirect effects. Larger numbers means slower execution and greater chance of finding equilibria.
#' @return A table of node effects on mineralization rates.
#' @details
#' The results are labeled as follows with direct contributions calculated from the full food web and indirect contributions calculated from the food web without that node. Indirect contributions do not include the direct contribution (i.e., it is subtracted).
#'
#'\describe{
#'   \item{Direct}{The direct contribution to mineralization.}
#'   \item{Indirect_static}{The indirect contribution to mineralization when the species is removed at equilibrium.}
#'   \item{Indirect_dynamic}{The indirect contribution to mineralization when the species is removed and equilibrium is recalculated.}
#' }
#' The indirect contributions are calculated as the total mineralization of the community with the trophic species minus the trophic species direct mineralization minus the total mineralization without the trophic species all divided by the total mineralizaiton with the trophic species.
#'
#' @examples
#' # Basic example for the introductory community:
#' whomineralizes(intro_comm) # For all nodes
#' whomineralizes(intro_comm, selected = c("Pred", "Prey1")) # For certain nodes only
#' @export
whomineralizes <- function(usin,
                           selected = NULL,
                           simulation_params = NULL,
                           mod_stoich = TRUE,
                           extinct_threshold = 1.5e-8,
                           n_sim_trials = 50){
  Nnodes = dim(usin$imat)[1] # Get the number of nodes
  Nnames = usin$prop$general$Carbon$ID # Get the names

  # Select only the chosen nodes:
  if(!is.null(selected)){
    if(!all(selected %in% Nnames)) stop("All selected nodes must be present in the community.")
    Nnames = selected
  }

  # Remove basal pools for now, because the simulator is not set up for a food web without detritus!
  Nnames = Nnames[!Nnames %in% colnames(usin$imat)[which(TLcheddar(usin$imat) == 1)]]

  res1 = comana(usin) # Calculate fluxes

  # Produce a table of the direct effects:
  mindf = as.data.frame(res1$mineralization)
  colnames(mindf) = paste0("min_",colnames(mindf))


  output_direct = cbind(data.frame(ID = colnames(usin$imat),
                      consump = res1$consumption,
                      basal_C_consump = rowSums(res1$fmat$Carbon[,TLcheddar(usin$imat) == 1])),
                 mindf)

  rownames(output_direct) = NULL

  output_indirect_static = vector("list", length = length(Nnames))
  names(output_indirect_static) = Nnames

  output_indirect_dynamic = output_indirect_static
  # Calculate the indirect effects:
  for(rmnode in Nnames){

    # Start with static indirect effect:
    usinmod = removenodes(usin, rmnode) # Remove a node

    # Rescale preferences to 1:
    usinmod$imat = sweep(usinmod$imat, 1, rowSums(usinmod$imat), FUN = "/")
    usinmod$imat[!is.finite(usinmod$imat)] = 0 # Replace non-finite values with 0 because total consumption was zero in this case

    # Calculate the new fluxes:
    res2 = comana(usinmod)

    # Calculate indirect effect on each element:
    mindf_mod = as.data.frame(res2$mineralization)
    colnames(mindf_mod) = paste0("min_",colnames(mindf_mod))

    indirect_static_min = colSums(mindf) - # Flux with the node
      mindf[rmnode,] - # Direct effect of the node
      colSums(mindf_mod) # Flux without the node

    rownames(indirect_static_min) = NULL

    output_indirect_static[[rmnode]] = cbind(
      data.frame(basal_C_consump = sum(res1$fmat$Carbon[,TLcheddar(usin$imat) == 1]) - # Flux with the node
                   - sum(res1$fmat$Carbon[rmnode,TLcheddar(usin$imat) == 1]) - # Direct effect of the node
                   sum(res2$fmat$Carbon[,TLcheddar(usinmod$imat) == 1]) # Flux without the node
      ),
      indirect_static_min
    )

    # Calculate dynamic indirect effect if requested:

    if(!is.null(simulation_params)){

      sim_par_mod = removenodes_sim(simulation_params, toremove = rmnode)

      # Try multiple starting points:
      outputsave = vector('list', n_sim_trials)
      for(trial in 1:n_sim_trials){
        tempout = tryCatch(rootSolve::stode(
          y = sim_par_mod$yeqm*runif(length(sim_par_mod$yeqm), min = 0.1, max = 10),
          func = foodwebode,
          parms = sim_par_mod$parameters),
          error = function(e) NULL)

        if(!is.null(tempout)){
          if(attr(tempout, "steady")){
            outputsave[[trial]] = tempout
          }
        }
      }

      outputsave = do.call("rbind",lapply(outputsave, function(X) X$y))

      if(dim(outputsave)[1] == 0){
        warning(paste("Simulation for the indirect effect of", rmnode, "not converging. Results may be wrong."))
      }else{
        # Remove equilibria that depend on large negative pools.
        outputsave = outputsave[apply(outputsave, 1, min) > -1.5e-8,,drop =F]

        outputsave = outputsave[!duplicated(round(outputsave, digits = 4)),,drop =F]

        outputsave[outputsave < extinct_threshold] = 0

        coextinct = apply(outputsave,1,min) < extinct_threshold

        indirect_dynamic_min = vector(mode = "list", length = dim(outputsave)[1])

        for(new_eqm in 1:dim(outputsave)[1]){
          # Calculate the new equilibrium
          sim_result_eqm = outputsave[new_eqm,]*sim_par_mod$parameters$eqmStandard

          sim_result_eqm_C = sim_result_eqm[!grepl("_",names(sim_result_eqm))]

          sim_result_eqm_other = sim_result_eqm[grepl("_",names(sim_result_eqm))]

          Q_to_calc = unique(do.call("c",lapply(strsplit(names(sim_result_eqm_other), "_"), function(X) X[[1]])))


          usinmod_dynamic = usinmod

          if(!all(names(sim_result_eqm_C) == usinmod_dynamic$prop$general$Carbon$ID)) stop("Name swapping occuring. Check code.")

          usinmod_dynamic$prop$general$Carbon$B = unname(sim_result_eqm_C)

          if(mod_stoich){
            curuse_all = strsplit(names(sim_result_eqm_other), "_")
            for(Qadj in Q_to_calc){
              curuse = sim_result_eqm_other[which(do.call("c",lapply(curuse_all, function(X) X[[1]])) == Qadj)]

              names(curuse) = unique(do.call("c",lapply(curuse_all, function(X) X[[2]])))

              # Calculate new Q values:
              curuse = curuse/
                (usinmod_dynamic$prop$general$Carbon$B/ # Notice that biomass already adjusted to new value...
                   usinmod_dynamic$prop$general$Carbon$Q)[usinmod_dynamic$prop$general$Carbon$ID == Qadj]

              for(el in names(curuse)){
                usinmod_dynamic$prop$general[[el]]$Q[usinmod_dynamic$prop$general[[el]]$ID == Qadj] = unname(curuse[el])
              }
            }
          }

          res3 = comana(usinmod_dynamic)

          # Calculate indirect effect on each element:
          mindf_mod = as.data.frame(res3$mineralization)
          colnames(mindf_mod) = paste0("min_",colnames(mindf_mod))

          indirect_dynamic_min[[new_eqm]] = colSums(mindf) - # Flux with the node
            mindf[rmnode,] - # Direct effect of the node
            colSums(mindf_mod) # Flux without the node

          rownames(indirect_dynamic_min) = NULL

          indirect_dynamic_min[[new_eqm]] = cbind(
            data.frame(basal_C_consump = sum(res1$fmat$Carbon[,TLcheddar(usin$imat) == 1]) - # Flux with the node
                         - sum(res1$fmat$Carbon[rmnode,TLcheddar(usin$imat) == 1]) - # Direct effect of the node
                         sum(res3$fmat$Carbon[,TLcheddar(usinmod$imat) == 1]) # Flux without the node
            ),
            indirect_dynamic_min[[new_eqm]]
          )

        }
        indirect_dynamic = cbind(outputsave, do.call("rbind", indirect_dynamic_min))

        rownames(indirect_dynamic) = NULL

        indirect_dynamic = cbind(indirect_dynamic, NAME = 0)

        colnames(indirect_dynamic)[colnames(indirect_dynamic) == "NAME"] = rmnode

        indirect_dynamic = cbind(indirect_dynamic, Coextinction = coextinct)

        output_indirect_dynamic[[rmnode]] = cbind(indirect_dynamic, ID = rmnode)
      }
    }
  }

  # Clean up data for export:
  output_direct$Effect = "Direct"

  output_indirect_static = do.call("rbind",output_indirect_static)

  output_indirect_static$ID = rownames(output_indirect_static)

  output_indirect_static$consump = NA

  output_indirect_static$Effect = "Indirect static"

  rownames(output_indirect_static) = NULL

  if(!is.null(simulation_params)){
    output_indirect_dynamic = do.call("rbind",output_indirect_dynamic)

    rownames(output_indirect_dynamic) = NULL

    output = list(static = rbind(output_direct, output_indirect_static), dynamic = output_indirect_dynamic)
  }else{
    output = rbind(output_direct, output_indirect_static)
  }
  return(output)
}
