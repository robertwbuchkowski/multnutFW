test_that("stoichiometry works", {
  p1 = getPARAMS(intro_comm, externalinputs = NA, inorganicinputs = NA, inorganicloss = NA, densitydependence = c(0,0,0,0,0,0))

  delta = matrix(foodwebode(1, y = p1$yeqm*c(rep(c(0.9,0.7,1.2,1.05,1.1,0.5), 4)),
                    pars = p1$parameters)[[1]][1:24],
         nrow = nrow(intro_comm$imat), ncol = length(intro_comm$prop$assimilation))

  # Remove small numbers:
  delta[abs(delta) < 1.5e-8] = 0

  Qmat = do.call("cbind",lapply(intro_comm$prop$general, FUN = function(X) X$Q))

  Qmat = sweep(Qmat, 1, Qmat[, 1], "/")

  tt1 = delta[c(-5),]*Qmat[-5,]

  for(i in 2:ncol(tt1)){
    expect_equal(tt1[,1], tt1[,i])
  }
})
