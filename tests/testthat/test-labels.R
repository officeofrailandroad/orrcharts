test_that("label_orr_*() create functions", {
  expect_type(label_orr_comma(), "closure")
  expect_type(label_orr_percent(), "closure")
  expect_type(label_orr_percentage_point(), "closure")
})


test_that("label_orr_percent creates the expected labeller", {
  lbl_fn <- label_orr_percent()
  expect_equal(lbl_fn(10), "10%")
  expect_equal(lbl_fn(120), "120%")
  expect_equal(lbl_fn(-23), "-23%")
  expect_equal(lbl_fn(1), "1%")
  expect_equal(lbl_fn(0.234), "0.2%")
  expect_equal(lbl_fn(-0.678), "-0.7%")
  expect_equal(lbl_fn(-0.3123), "-0.3%")
  expect_equal(lbl_fn(0), "0.0%")
  expect_equal(lbl_fn(c(0, 2, 0.3, 5)), c("0.0%", "2%", "0.3%", "5%"))
})
