#' A function to convert between respiration by biomass carbon (E) and consumption (p).
#'
#' @param  usin The community that you are analyzing: contains a matrix of interactions and a data frame of properties in a list.
#' @param node The node in the community whose fluxes you want to view. NULL is the default and means all of them. You can also include a vector of the ones that you want.
#' @param Element The element whose fluxes you want to view. NULL is the default and means all of them. You can also include a vector of the ones that you want.
#' @param mfrow_plot Two number vector saying the number of rows and columns for each plot page. Defaults to c(1,1).
#' @return The community where E is now zero and p is not 1.
#' @examples
#' node_flux_view(intro_comm, node = "Pred", Element = "Carbon")
#' @export

node_flux_view <- function(usin, node = NULL, Element= NULL, mfrow_plot = c(1,1)){

  if(is.null(Element)) Element = names(usin$prop$general)
  if(is.null(node)) node = rownames(usin$imat)

  # Calculate the fluxes for use in converting E to p:
  coutput = comana(usin)

  # Get the node inputs in C,N,P:
  consump = lapply(coutput$fmat, rowSums)

  defication = Map(function(x, y) rowSums(x * (1-y)), coutput$fmat, usin$prop$assimilation)

  predation = lapply(coutput$fmat, colSums)

  mineralization = coutput$mineralization

  death = usin$prop$general$Carbon$d*usin$prop$general$Carbon$B

  ratio = lapply(usin$prop$general, function(X) X$Q)

  ratio2 = lapply(ratio, function(X) X/ratio[[1]]) # X/C

  death = lapply(ratio2, function(X) X*death)

  death = lapply(death,
                 replace_inf <- function(x) {
                   x[is.infinite(x)] <- 0
                   names(x) = rownames(usin$imat)
                   return(x)
                 }
  )

  # Test balance:
  net = do.call("cbind", defication) + do.call("cbind", mineralization) + do.call("cbind", predation) + do.call("cbind", death) - do.call("cbind", consump)

  net = net[TLcheddar(usin$imat) > 1,]

  if(any(net > 1.5e-8)){
    print(net)
    warning("Some of the higher trophic levels are not balancing. See net change matrix above to diagnose the issue.")
  }

  graphics::par(mar = c(1, 1, 1, 1)) # Reduce margins
  graphics::par(mfrow = mfrow_plot) # set the plot numbers per panel

  for(i in 1:length(Element)){
    for(j in 1:length(node)){
      fluxmat = matrix(NA, nrow = 6, ncol = 6)
      rownames(fluxmat) = colnames(fluxmat) = c(node[j], "consumption", "defication", "predation", "mineralization", "death")

      fluxmat[1,2] =  unname(consump[[Element[i]]][node[j]])
      fluxmat[3,1] =  unname(defication[[Element[i]]][node[j]])
      fluxmat[4,1] =  unname(predation[[Element[i]]][node[j]])
      fluxmat[5,1] =  unname(mineralization[[Element[i]]][node[j]])
      fluxmat[6,1] =  unname(death[[Element[i]]][node[j]])

      fluxmat = signif(fluxmat, digits = 3)

      # Create the food web diagram

      diagram::plotmat(
        fluxmat,
        pos = matrix(c(0.5, 0.5, 0.1, 0.5, 0.1, 0.9,
                       0.5, 0.1, 0.5, 0.9, 0.9, 0.5), nrow = 6, ncol = 2),
        name = rownames(fluxmat),
        lwd = 1,
        box.type = "circle",
        box.size = 0.08,
        box.prop = 0.5,
        box.col = "lightblue",
        arr.length = 0.2,
        arr.width = 0.1,
        self.cex = 0.6,
        self.shifty = -0.01,
        self.shiftx = 0.01,
        main = paste("Fluxes of", Element[i], "for", node[j])
      )
    }
  }
}
