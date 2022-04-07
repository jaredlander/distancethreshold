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

test_that("Output is correct", {
    input <- data.frame(
        ID = c("A", "B", "C", "A"),
        x = c(0, 1, 1, 0),
        y = c(0, 1, 3, 2)
    )

    actual <- threshold_distance(input, 3, as_dataframe = TRUE)

    expected <- data.frame(
        i = c(1, 4, 4, 2),
        j = c(2, 2, 3, 3),
        distance = c(
            1.4142135623731,
            1.4142135623731,
            1.4142135623731,
            2
        ),
        ID_1 = c("A", "A", "A", "B"),
        ID_2 = c("B", "B", "C", "C")
    )

    expect_equal(data.frame(actual), data.frame(expected))
})

test_that("Points on a map work as expected", {
    input <- data.frame(
        ID = c("A", "B", "C", "A"),
        lat = c(40.668034, 40.66853, 40.66903, 40.66853),
        lng = c(-73.971291, -73.97079, -73.97079, -73.971291)
    )

    expected <- data.frame(
        i = c(1, 4, 2, 2),
        j = c(2, 3, 4, 3),
        ID_1 = c("A", "A", "B", "B"),
        ID_2 = c("B", "C", "A", "C")
    )

    actual <- threshold_distance(input, threshold = 100, cols = c("lat", "lng"), distance_type = "haversine", as_dataframe = TRUE) |>
        (\(df) df[, c("i", "j", "ID_1", "ID_2")])()

    expect_equal(data.frame(actual), data.frame(expected))
})
