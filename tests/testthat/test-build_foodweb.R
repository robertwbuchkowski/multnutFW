test_that("build_foodweb ouput imat", {
  testthat::expect_true(is.numeric(build_foodweb(feedinglist, properties_example1)$imat))
})

test_that("build_foodweb ouput imat dimensions", {
  testthat::expect_equal(
    colnames(build_foodweb(feedinglist, properties_example1)$imat),
    rownames(build_foodweb(feedinglist, properties_example1)$imat)
  )
})

test_that("build_foodweb ouput prop dimensions", {
  testthat::expect_equal(
    colnames(build_foodweb(feedinglist, properties_example1)$imat),
    build_foodweb(feedinglist, properties_example1)$prop$general[[1]]$ID
  )
})

test_that("build_foodweb ouputt prop IDs", {
  for(i in 2:length(build_foodweb(feedinglist, properties_example1)$prop$general)){
    testthat::expect_equal(
      build_foodweb(feedinglist, properties_example1)$prop$general[[1]]$ID,
      build_foodweb(feedinglist, properties_example1)$prop$general[[i]]$ID
    )
  }
})
