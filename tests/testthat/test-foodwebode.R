test_that("size of vector correct", {
  p1 = getPARAMS(intro_comm, externalinputs = NA, inorganicinputs = NA, inorganicloss = NA, densitydependence = c(0,0,0,0,0,0))

  expect_equal(length(foodwebode(1, y = p1$yeqm*c(0.9,0.7,1.2,1.05,1.1,0.5,1.1,1.1,1.1),
             pars = p1$parameters)[[1]]), length(p1$yeqm))


})
