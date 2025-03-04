test_that("E to p conversion leads to the same carbon fluxes", {
  expect_equal(comana(intro_comm)$fmat$Carbon, comana(E_to_p(intro_comm))$fmat$Carbon)
})
