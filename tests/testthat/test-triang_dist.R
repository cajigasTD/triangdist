test_that("dtriang basic behavior", {
  expect_equal(dtriang(3, 0, 6, 3), 1/3)
  expect_equal(dtriang(-2, 0, 6, 3), 0)
  expect_equal(dtriang(10, 0, 6, 3), 0)
})

test_that("ptriang basic behavior", {
  expect_equal(ptriang(-1, 0, 6, 3), 0)
  expect_equal(ptriang(7, 0, 6, 3), 1)
  expect_equal(round(ptriang(3, 0, 6, 3), 5), 0.5)
})

test_that("qtriang basic behavior", {
  expect_equal(qtriang(0, 0, 6, 3), 0)
  expect_equal(qtriang(1, 0, 6, 3), 6)
  expect_equal(round(qtriang(0.5, 0, 6, 3), 5), 3)
})

test_that("rtriang output", {
  x <- rtriang(50, 0, 6, 3)
  expect_equal(length(x), 50)
  expect_true(all(x >= 0))
  expect_true(all(x <= 6))
})

test_that("parameter errors", {
  expect_error(dtriang(2, 5, 1, 3))
  expect_error(ptriang(2, 5, 1, 3))
  expect_error(qtriang(0.5, 5, 1, 3))
  expect_error(rtriang(10, 5, 1, 3))
})

test_that("probability errors", {
  expect_error(qtriang(-0.2, 0, 6, 3))
  expect_error(qtriang(1.2, 0, 6, 3))
})

test_that("edge cases covered", {
  expect_true(dtriang(0.1, 0, 1, 0) >= 0)

  expect_true(dtriang(0.9, 0, 1, 1) >= 0)

  expect_equal(ptriang(0, 0, 1, 0.5), 0)
  expect_equal(ptriang(1, 0, 1, 0.5), 1)

  expect_equal(qtriang(0, 0, 1, 0.5), 0)
  expect_equal(qtriang(1, 0, 1, 0.5), 1)
})

test_that("error lines covered", {
  expect_error(dtriang(0.5, 0, 1, 2))
  expect_error(rtriang(0, 0, 1, 0.5))
})
