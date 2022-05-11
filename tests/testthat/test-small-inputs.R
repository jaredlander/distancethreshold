context('No matches or empty input')

twodf <- data.frame(
    ID=c('A', 'B'),
    x=c(6, 5),
    y=c(1, 7),
    extra1=c('m', 'b'),
    extra2=c('p', 'o'),
    extra3=c(2, 3)
)

onedf <- twodf[1, ]

zerodf <- twodf[-c(1,2), ]

two1 <- threshold_distance(twodf, threshold=3, as_dataframe=FALSE, cols=c('x', 'y'))
two2 <- threshold_distance(twodf, threshold=3, as_dataframe=TRUE, cols=c('x', 'y'))

two1_extras <- threshold_distance(twodf, threshold=3, as_dataframe=FALSE, extra_columns=c('extra1', 'extra2'))
two2_extras <- threshold_distance(twodf, threshold=3, as_dataframe=TRUE, extra_columns=c('extra1', 'extra2'))
two1_extras_nocheck <- threshold_distance(twodf, threshold=3, as_dataframe=FALSE, extra_columns=c('extra1', 'extra2'), check_id=FALSE)
two2_extras_nocheck <- threshold_distance(twodf, threshold=3, as_dataframe=TRUE, extra_columns=c('extra1', 'extra2'), check_id=FALSE)

one1 <- threshold_distance(onedf, threshold=3, as_dataframe=FALSE, cols=c('x', 'y'))
one2 <- threshold_distance(onedf, threshold=3, as_dataframe=TRUE, cols=c('x', 'y'))

one1_extras <- threshold_distance(onedf, threshold=3, as_dataframe=FALSE, extra_columns=c('extra1', 'extra2'))
one2_extras <- threshold_distance(onedf, threshold=3, as_dataframe=TRUE, extra_columns=c('extra1', 'extra2'))
one1_extras_nocheck <- threshold_distance(onedf, threshold=3, as_dataframe=FALSE, extra_columns=c('extra1', 'extra2'), check_id=FALSE)
one2_extras_nocheck <- threshold_distance(onedf, threshold=3, as_dataframe=TRUE, extra_columns=c('extra1', 'extra2'), check_id=FALSE)

zero1 <- threshold_distance(zerodf, threshold=3, as_dataframe=FALSE, cols=c('x', 'y'))
zero2 <- threshold_distance(zerodf, threshold=3, as_dataframe=TRUE, cols=c('x', 'y'))

zero1_extras <- threshold_distance(zerodf, threshold=3, as_dataframe=FALSE, extra_columns=c('extra1', 'extra2'))
zero2_extras <- threshold_distance(zerodf, threshold=3, as_dataframe=TRUE, extra_columns=c('extra1', 'extra2'))
zero1_extras_nocheck <- threshold_distance(zerodf, threshold=3, as_dataframe=FALSE, extra_columns=c('extra1', 'extra2'), check_id=FALSE)
zero2_extras_nocheck <- threshold_distance(zerodf, threshold=3, as_dataframe=TRUE, extra_columns=c('extra1', 'extra2'), check_id=FALSE)

test_that("Correct type is returned", {
    ## two
    expect_is(two1, 'list')
    expect_is(two2, 'data.frame')

    expect_is(two1_extras, 'list')
    expect_is(two2_extras, 'data.frame')

    expect_is(two1_extras_nocheck, 'list')
    expect_is(two2_extras_nocheck, 'data.frame')

    ## one
    expect_is(one1, 'list')
    expect_is(one2, 'data.frame')
    expect_is(one1_extras, 'list')
    expect_is(one2_extras, 'data.frame')
    expect_is(one1_extras_nocheck, 'list')
    expect_is(one2_extras_nocheck, 'data.frame')

    ## zero
    expect_is(zero1, 'list')
    expect_is(zero2, 'data.frame')
    expect_is(zero1_extras, 'list')
    expect_is(zero2_extras, 'data.frame')
    expect_is(zero1_extras_nocheck, 'list')
    expect_is(zero2_extras_nocheck, 'data.frame')
})

test_that("Input is restricted", {
    expect_error(threshold_distance(as.list(twodf), threshold=3, as_dataframe=FALSE))
    expect_error(threshold_distance(as.list(onedf), threshold=3, as_dataframe=FALSE))
    expect_error(threshold_distance(as.list(zerodf), threshold=3, as_dataframe=FALSE))
})

test_that("Outcome is the right size", {

    # two
    expect_equal(length(two1), 7)
    expect_equal(length(two1_extras), 11)
    expect_equal(length(two1_extras_nocheck), 11)
    expect_equal(dim(two2), c(0, 5))
    expect_equal(dim(two2_extras), c(0, 9))
    expect_equal(dim(two2_extras_nocheck), c(0, 9))

    # one
    expect_equal(length(one1), 7)
    expect_equal(length(one1_extras), 11)
    expect_equal(length(one1_extras_nocheck), 11)
    expect_equal(dim(one2), c(0, 5))
    expect_equal(dim(one2_extras), c(0, 9))
    expect_equal(dim(one2_extras_nocheck), c(0, 9))

    # zero
    expect_equal(length(zero1), 7)
    expect_equal(length(zero1_extras), 11)
    expect_equal(length(zero1_extras_nocheck), 11)
    expect_equal(dim(zero2), c(0, 5))
    expect_equal(dim(zero2_extras), c(0, 9))
    expect_equal(dim(zero2_extras_nocheck), c(0, 9))
})
