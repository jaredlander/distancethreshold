thedf <- data.frame(
    ID=rep(LETTERS[1:3], length.out=10),
    x=sample(10),
    y=sample(10)
)

res1 <- threshold_distance(thedf, threshold=3, as_dataframe=FALSE)
res2 <- threshold_distance(thedf, threshold=3, as_dataframe=TRUE)

test_that("Correct type is returned", {
  expect_is(res1, 'list')
  expect_is(res2, 'data.frame')
})

test_that("Input is restricted", {
    expect_error(threshold_distance(as.list(thedf), threshold=3, as_dataframe=FALSE))
})
