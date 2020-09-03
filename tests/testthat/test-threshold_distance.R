context('distance')
set.seed(62)
thedf <- data.frame(
    ID=rep(LETTERS[1:3], length.out=10),
    x=sample(10),
    y=sample(10),
    extra1=sample(letters, size=10),
    extra2=sample(letters, size=10),
    extra3=sample(10)
)

res1 <- threshold_distance(thedf, threshold=3, as_dataframe=FALSE, cols=c('x', 'y'))
res2 <- threshold_distance(thedf, threshold=3, as_dataframe=TRUE, cols=c('x', 'y'))

res1_extras <- threshold_distance(thedf, threshold=3, as_dataframe=FALSE, extra_columns=c('extra1', 'extra2'))
res2_extras <- threshold_distance(thedf, threshold=3, as_dataframe=TRUE, extra_columns=c('extra1', 'extra2'))

res1_extras_nocheck <- threshold_distance(thedf, threshold=3, as_dataframe=FALSE, extra_columns=c('extra1', 'extra2'), check_id=FALSE)
res2_extras_nocheck <- threshold_distance(thedf, threshold=3, as_dataframe=TRUE, extra_columns=c('extra1', 'extra2'), check_id=FALSE)

test_that("Correct type is returned", {
  expect_is(res1, 'list')
  expect_is(res2, 'data.frame')

  expect_is(res1_extras, 'list')
  expect_is(res2_extras, 'data.frame')

  expect_is(res1_extras_nocheck, 'list')
  expect_is(res2_extras_nocheck, 'data.frame')
})

test_that("Input is restricted", {
    expect_error(threshold_distance(as.list(thedf), threshold=3, as_dataframe=FALSE))
})

test_that("Outcome is the right size", {
    expect_equal(length(res1), 7)
    expect_equal(length(res1_extras), 11)
    expect_equal(length(res1_extras_nocheck), 11)

    expect_equal(dim(res2), c(5, 5))
    expect_equal(dim(res2_extras), c(5, 9))
    expect_equal(dim(res2_extras_nocheck), c(7, 9))
})
