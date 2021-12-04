test_that("cor_function works", {
  expect_equal(cor_fucntion(x1,y1,method="pearson",stat_test = TRUE)$correlation, cor.test(x1,y1,method="pearson")$estimate)
})
