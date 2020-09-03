thedf <- data.frame(ID=rep(c('A', 'B'), length.out=10), A=sample(10), B=rnorm(10), C=sample(10))
mat1 <- dftomat(thedf, cols=c('A'))
mat2 <- dftomat(thedf, cols=c('A', 'B'))
mat3 <- dftomat(thedf, cols=c('A', 'B', 'C'))

test_that("Output is right type", {
  expect_is(mat1, 'matrix')
  expect_is(mat3, 'matrix')
  expect_is(mat3, 'matrix')
})

test_that("Output is right size", {
    expect_equal(dim(mat1), c(10, 1))
    expect_equal(dim(mat2), c(10, 2))
    expect_equal(dim(mat3), c(10, 3))
})
