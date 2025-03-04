test_that("p to E conversion leads to the same carbon fluxes", {
  expect_equal(comana(p_to_E(E_to_p(intro_comm)))$fmat$Carbon, comana(intro_comm)$fmat$Carbon)
})


test_that("p to E conversion and back leads to the same E values", {
  expect_equal(p_to_E(E_to_p(intro_comm))$prop$general$Carbon$E, intro_comm$prop$general$Carbon$E)
})

test_that("p to E conversion and back preserves Ehat values", {

  temp_comm = intro_comm

  temp_comm$prop$general$Carbon$Ehat = 1

  expect_equal(p_to_E(E_to_p(temp_comm))$prop$general$Carbon$Ehat, temp_comm$prop$general$Carbon$Ehat)

  rm(temp_comm)
})


test_that("p to E conversion and back works with Ehat values", {

  temp_comm = intro_comm

  temp_comm$prop$general$Carbon$Ehat = 1


  expect_equal(p_to_E(E_to_p(temp_comm))$prop$general$Carbon$E, temp_comm$prop$general$Carbon$E)

  rm(temp_comm)
})
