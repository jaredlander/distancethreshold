expand_column_values <- function(column, values, index_i, index_j)
{
    stats::setNames(
        data.frame(
            values[index_i],
            values[index_j]
        ),
        sprintf("%s_%s", column, 1:2)
    )
}

#' @title threshold_distance
#' @description Computes the distance between rows and returns those that fall below `threshold`
#' @details Computes the distance between rows and returns those that fall below `threshold`.
#' If two rows have the same ID, they will not be compared and the row-pairs will not be returned.
#' @md
#' @param data `data.frame` of data to compute distance of
#' @param threshold Maximum distance to return
#' @param x_col Name of column holding x data
#' @param y_col Name of column holding y data
#' @param id_col Name of column holding ID data
#' @param extra_columns Names of other columns to expand into the results based on indices.
#' Two new elements will be made for each, one for the i index and one for the j index.
#' @param as_dataframe `logical` if a `list` (default) or `data.frame` should be returned
#'
#' @return Either a `list` or `data.frame` showing which IDs matched with other
#' IDs, the distance between them and the rows numbers where the pairs occured.
#' @export
#'
#' @examples
#' mydf <- tibble::tibble(
#' ID=rep(LETTERS[1:3], length.out=10),
#' x=sample(10),
#' y=sample(10)
#' )
#'
#' threshold_distance(mydf, threshold=3, as_dataframe=FALSE)
#' threshold_distance(mydf, threshold=3, as_dataframe=TRUE)
threshold_distance <- function(data, threshold, x_col="x", y_col="y", id_col="ID", extra_columns=NULL, as_dataframe=FALSE)
{
    # make sure we're only working with data.frames (or tibbles, or data.tables)
    assertthat::assert_that(is.data.frame(data))

    # save generated ID column names for later
    idcol_1 <- sprintf("%s_1", id_col)
    idcol_2 <- sprintf("%s_2", id_col)

    # switch to data.table for fast sorting
    data <- data.table::as.data.table(data)
    data.table::setkeyv(data, x_col)

    # the C++ function needs ID as an integer so make that happen
    data[, .id_integer_:=as.integer(as.factor(.SD[[id_col]])), .SDcols=id_col]

    # call the C++ function
    results <- .Call(`_distancethreshold_threshold_distance`, data, threshold, x_col, y_col, '.id_integer_')

    # expand the IDs according to their corresponding indices
    # could have done this on the C++ side, except we passed integers to C++ instead of the actual IDs
    results[[idcol_1]] <- data[[id_col]][results$i]
    results[[idcol_2]] <- data[[id_col]][results$j]

    # if the user wants other columns to be expanded, do it here
    if(!is.null(extra_columns))
    {
        extras <- mapply(
            expand_column_values,
            extra_columns, data[, extra_columns, with=FALSE],
            MoreArgs=list(index_i=results$i, index_j=results$j),
            SIMPLIFY=FALSE
        )

        # little trick to make sure we get a data.frame
        results <- c(results, as.list(Reduce(cbind, extras)))
    }

    # if we want a data.frame we get a data.table
    if(as_dataframe)
    {
        kept <- results$kept
        skipped <- results$skipped
        results$kept <- NULL
        results$skipped <- NULL
        results <- data.table::setDT(results, key=c(idcol_1, idcol_2))
        attr(results, 'kept') <- kept
        attr(results, 'skipped') <- skipped
    }

    return(results)
}

