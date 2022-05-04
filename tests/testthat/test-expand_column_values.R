thedf_expand <- data.frame(
    ID=rep(LETTERS[1:3], length.out=10),
    x=sample(10),
    y=sample(10),
    extra1=sample(letters, size=10),
    extra2=sample(letters, size=10),
    extra3=sample(10),
    extra4=Sys.time() + 1:10,
    extra5=factor(letters[c(1:5, 1:5)])
)

expanded_1 <- expand_column_values('extra1', thedf_expand$extra1, index_i=c(1, 3), index_j=c(2, 4))
expanded_2 <- expand_column_values('extra2', thedf_expand$extra2, index_i=c(1, 3), index_j=c(2, 4))
expanded_3 <- expand_column_values(c('extra3'), thedf_expand$extra3, index_i=c(1, 3), index_j=c(2, 4))
expanded_4 <- expand_column_values(c('extra4'), thedf_expand$extra4, index_i=c(1, 3), index_j=c(2, 4))
expanded_5 <- expand_column_values(c('extra5'), thedf_expand$extra5, index_i=c(1, 3), index_j=c(2, 4))

test_that("Output is right class", {
  expect_is(expanded_1, 'data.frame')
  expect_is(expanded_2, 'data.frame')
  expect_is(expanded_3, 'data.frame')
  expect_is(expanded_4, 'data.frame')
  expect_is(expanded_5, 'data.frame')
})

test_that("Output is right size", {
    expect_equal(dim(expanded_1), c(2, 2))
    expect_equal(dim(expanded_2), c(2, 2))
    expect_equal(dim(expanded_3), c(2, 2))
    expect_equal(dim(expanded_4), c(2, 2))
    expect_equal(dim(expanded_5), c(2, 2))
})
