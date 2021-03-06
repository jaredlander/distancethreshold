thedf_expand <- data.frame(
    ID=rep(LETTERS[1:3], length.out=10),
    x=sample(10),
    y=sample(10),
    extra1=sample(letters, size=10),
    extra2=sample(letters, size=10),
    extra3=sample(10)
)

expanded_1 <- expand_column_values('extra1', thedf_expand$extra1, index_i=c(1, 3), index_j=c(2, 4))
expanded_2 <- expand_column_values('extra2', thedf_expand$extra2, index_i=c(1, 3), index_j=c(2, 4))

test_that("Output is right class", {
  expect_is(expanded_1, 'data.frame')
  expect_is(expanded_2, 'data.frame')
})

test_that("Output is right size", {
    expect_equal(dim(expanded_1), c(2, 2))
    expect_equal(dim(expanded_2), c(2, 2))
})
