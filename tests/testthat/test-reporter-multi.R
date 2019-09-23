test_that("MultiReporter", {
  reports <- lapply(seq_len(3), function(x) ListReporter$new())
  reporter <- MultiReporter$new(reporters = reports)

  test_file(test_path("context.R"), reporter)

  dfs <- lapply(reports, function(x) as.data.frame(x$get_results()))

  expect_equal(dfs[[2]][1:7], dfs[[1]][1:7])
  expect_equal(dfs[[3]][1:7], dfs[[1]][1:7])
})

test_that("Reporter fails properly", {

  # When a test dies, test_file() should finish properly and report that tests did not succeed.

  finished <- FALSE
  tryCatch({
    result <- test_file(test_path("dying.R"), Reporter$new())
    finished <- TRUE
  }, error=invisible)

  expect_true(finished)
  expect_false(all_passed(result))
})
