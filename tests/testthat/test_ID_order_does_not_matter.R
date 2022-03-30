id1 <- c("A", "B", "C", "A", "B", "C", "A", "B", "C", "A")
x1 <- c(4, 10, 7, 5, 1, 8, 6, 2, 3, 9)
y1 <- c(9, 2, 5, 7, 1, 6, 3, 8, 10, 4)

id2 <- id1[c(2:10, 1)]
x2 <- x1[c(2:10, 1)]
y2 <- y1[c(2:10, 1)]

thedf1 <- data.frame(
    ID=id1,
    x=x1,
    y=y1,
    extra1=sample(letters, size=10),
    extra2=sample(letters, size=10),
    extra3=sample(10)
)

thedf2 <- data.frame(
    ID=id2,
    x=x2,
    y=y2,
    extra1=sample(letters, size=10),
    extra2=sample(letters, size=10),
    extra3=sample(10)
)

test_that("Results are the same", {
  expect_equal(
      threshold_distance(thedf1, threshold=3, as_dataframe=TRUE) |> dplyr::select(distance, ID_1, ID_2) |> dplyr::arrange(ID_1, ID_2),
      threshold_distance(thedf2, threshold=3, as_dataframe=TRUE) |> dplyr::select(distance, ID_1, ID_2) |> dplyr::arrange(ID_1, ID_2)
  )
})
