#' @title expand_column_values
#' @description  Creates extra columns to store in a list
#' @details Given a `vector` of values, create repeated values of it according to two index variables.
#' @param column Name of column to be expanded
#' @param values Actual `vector` of data to expand
#' @param index_i `vector` of indexes to expand `column`
#' @param index_j Second `vector` of indexes to expand `column`
#' @md
#' @return A list
#' @examples
#'
#' thedf <- data.frame(
#' ID=rep(LETTERS[1:3], length.out=10),
#' x=sample(10),
#' y=sample(10),
#' extra1=sample(letters, size=10),
#' extra2=sample(letters, size=10),
#' extra3=sample(10),
#' extra4=Sys.time() + 1:10
#' )
#' distancethreshold:::expand_column_values('extra1', thedf$extra1, index_i=c(1, 3), index_j=c(2, 4))
#' distancethreshold:::expand_column_values('extra2', thedf$extra2, index_i=c(1, 3), index_j=c(2, 4))
#' distancethreshold:::expand_column_values('extra3', thedf$extra3, index_i=c(1, 3), index_j=c(2, 4))
#' distancethreshold:::expand_column_values('extra4', thedf$extra4, index_i=c(1, 3), index_j=c(2, 4))
expand_column_values <- function(column, values, index_i, index_j)
{
    assertthat::assert_that(is.character(column))
    # values can be a factor or timestamp or other such type that does not
    # pass is.vector
    # so instead we check that it has no dim (vectors don't have rows and columns)
    # and that it has length at least one
    # would be nice if there was a better way to check if something is vector-like
    assertthat::assert_that(is.null(dim(values)))
    assertthat::assert_that(length(values) >= 0)
    assertthat::assert_that(is.vector(index_i))
    assertthat::assert_that(is.vector(index_j))

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
#' @author Jared P. Lander
#' @param data `data.frame` of data to compute distance of
#' @param threshold Maximum distance to return
#' @param cols Names of columns of numeric data. The data will first be sorted on the first of these.
#' @param id_col Name of column holding ID data
#' @param extra_columns Names of other columns to expand into the results based on indices.
#' Two new elements will be made for each, one for the i index and one for the j index.
#' @param as_dataframe `logical` if a `list` (default) or `data.frame` should be returned
#' @param check_id Whether the ID variable should be checked for inclusion
#' @param distance_type What distance function to use
#'
#' @return Either a `list` or `data.frame` showing which IDs matched with other
#' IDs, the distance between them and the rows numbers where the pairs occured.
#' @export
#' @importFrom data.table `:=`
#' @importFrom data.table .I
#' @examples
#' thedf <- data.frame(
#' ID=rep(LETTERS[1:3], length.out=10),
#' x=sample(10),
#' y=sample(10),
#' extra1=sample(letters, size=10),
#' extra2=sample(letters, size=10),
#' extra3=sample(10)
#' )
#'
#' threshold_distance(thedf, threshold=3, as_dataframe=FALSE)
#' threshold_distance(thedf, threshold=3, as_dataframe=TRUE)
#' threshold_distance(thedf, threshold=3, as_dataframe=TRUE, check_id=FALSE)
threshold_distance <- function(data, threshold, cols=c("x", "y"), id_col="ID", extra_columns=NULL, as_dataframe=FALSE, check_id=TRUE, distance_type = c("euclidean", "haversine"))
{
    # make sure we're only working with data.frames (or tibbles, or data.tables)
    assertthat::assert_that(is.data.frame(data))
    assertthat::assert_that(is.numeric(threshold))
    assertthat::assert_that(length(threshold) == 1)
    assertthat::assert_that(is.character(cols))
    assertthat::assert_that(is.character(id_col))
    assertthat::assert_that(is.character(extra_columns) | is.null(extra_columns))
    assertthat::assert_that(is.logical(as_dataframe))
    assertthat::assert_that(is.logical(check_id))

    distance_type <- match.arg(distance_type, c("euclidean", "haversine"))

    # switch to data.table for fast sorting
    data <- data.table::as.data.table(data)
    data[, ".i_original_ordering_" := .I]
    data.table::setkeyv(data, cols[1])

    # the C++ function needs ID as an integer so make that happen
    if(id_col %in% names(data))
    {
        data[, '.id_integer_':=as.integer(as.factor(.SD[[id_col]])), .SDcols=id_col]
    } else
    {
        id_col <- '.id_integer_'
        data[, '.id_integer_':=.I]
    }

    # save generated ID column names for later
    idcol_1 <- sprintf("%s_1", id_col)
    idcol_2 <- sprintf("%s_2", id_col)

    # call the C++ function
    results <- .Call(`_distancethreshold_threshold_distance`, data, threshold, cols, '.id_integer_', check_id, distance_type)

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

    # fix ordering of results
    # sorting data causes the i, j to refer to the sorted data, not the original data
    # this has to happen after extra columns are matched
    # otherwise the indices match the wrong rows
    results$i <- data$.i_original_ordering_[results$i]
    results$j <- data$.i_original_ordering_[results$j]

    # if we want a data.frame we get a data.table
    if(as_dataframe)
    {
        kept <- results$kept
        skipped <- results$skipped
        results$kept <- NULL
        results$skipped <- NULL
        results <- data.table::setDT(results, key=c(idcol_1, idcol_2))
        data.table::setattr(x=results, name='kept', value=kept)
        data.table::setattr(x=results, name='skipped', value=skipped)
    }

    return(results)
}

#' @title threshold_distance2
#'
#' @description Computes the distance between rows of `left_df` (old data)
#' and `right_df` (new data) and returns those that fall below `threshold`.
#'
#' @md
#' @author Michael Beigelmacher
#' @param left_df `data.frame` old data to compare distances to
#' @param right_df `data.frame` new data to check for points in `left_df` that are nearby
#' @param threshold Maximum distance to return
#' @param cols Names of columns of numeric data. The data will first be sorted on the first of these.
#' @param id_col Name of column holding ID data
#' @param extra_columns Names of other columns to expand into the results based on indices.
#' @param as_dataframe `logical` if a `list` (default) or `data.frame` should be returned
#' @param check_id Whether the ID variable should be checked for inclusion
#' @param distance_type What distance function to use
#'
#' @return Either a `list` or `data.frame` showing which IDs matched with other
#' IDs, the distance between them and the rows numbers where the pairs occured.
#' @export
#' @importFrom data.table `:=`
#' @importFrom data.table .I
#' @examples
#' left_df <- data.frame(
#' x = c(0, 1, 1, 0),
#' y = c(0, 1, 3, 2)
#' )
#'
#' right_df <- data.frame(
#'    x = c(0, 0, 2),
#'    y = c(1, 3, 2)
#' )
#'
#' threshold_distance2(left_df, right_df, threshold = 1.5, as_dataframe=FALSE)
#' threshold_distance2(left_df, right_df, threshold = 1.5, as_dataframe=TRUE)
threshold_distance2 <- function(left_df, right_df, threshold, cols = c("x", "y"), id_col="ID", extra_columns=NULL, as_dataframe=FALSE, check_id=FALSE, distance_type = c("euclidean", "haversine"))
{
    # make sure we're only working with data.frames (or tibbles, or data.tables)
    assertthat::assert_that(is.data.frame(left_df))
    assertthat::assert_that(is.data.frame(right_df))
    assertthat::assert_that(is.numeric(threshold))
    assertthat::assert_that(length(threshold) == 1)
    assertthat::assert_that(is.character(cols))
    assertthat::assert_that(is.character(id_col))
    assertthat::assert_that(is.character(extra_columns) | is.null(extra_columns))
    assertthat::assert_that(is.logical(as_dataframe))
    assertthat::assert_that(is.logical(check_id))

    distance_type <- match.arg(distance_type, c("euclidean", "haversine"))

    # switch to data.table for fast sorting
    left_df <- data.table::as.data.table(left_df)
    left_df[, ".i_original_ordering_" := .I]
    data.table::setkeyv(left_df, cols[1])
    right_df <- data.table::as.data.table(right_df)
    right_df[, ".j_original_ordering_" := .I]
    data.table::setkeyv(right_df, cols[1])

    # the C++ function needs ID as an integer so make that happen
    # if the id column doesn't exist, we add a fake one
    if(id_col %in% names(left_df))
    {
        keep_id <- TRUE
        left_df[, '.id_integer_':=as.integer(as.factor(.SD[[id_col]])), .SDcols=id_col]
        right_df[, '.id_integer_':=as.integer(as.factor(.SD[[id_col]])), .SDcols=id_col]
    } else
    {
        keep_id <- FALSE
        id_col <- '.id_integer_'
        left_df[, '.id_integer_':=.I]
        right_df[, '.id_integer_':=.I]
    }

    # save generated ID column names for later
    idcol_1 <- sprintf("%s_1", id_col)
    idcol_2 <- sprintf("%s_2", id_col)

    results <- .Call(`_distancethreshold_threshold_distance2`, left_df, right_df, threshold, cols, '.id_integer_', check_id, distance_type)

    # expand the IDs according to their corresponding indices
    # could have done this on the C++ side, except we passed integers to C++ instead of the actual IDs
    if(isTRUE(keep_id))
    {
        results[[idcol_1]] <- left_df[[id_col]][results$i]
        results[[idcol_2]] <- right_df[[id_col]][results$j]
    }

    # if the user wants other columns to be expanded, do it here
    if(!is.null(extra_columns))
    {
        # extras <- mapply(
        #     expand_column_values,
        #     extra_columns, data[, extra_columns, with=FALSE],
        #     MoreArgs=list(index_i=results$i, index_j=results$j),
        #     SIMPLIFY=FALSE
        # )
        #
        # # little trick to make sure we get a data.frame
        # results <- c(results, as.list(Reduce(cbind, extras)))

        # going to rename columns in the original data so the join is nice
        extra_columns_left <- sprintf('%s_1', c(extra_columns))
        extra_columns_right <- sprintf('%s_2', c(extra_columns))
        data.table::setnames(left_df, extra_columns, extra_columns_left)
        data.table::setnames(right_df, extra_columns, extra_columns_right)

        # now we join in those columns
        # this assumes the data are sorted properly
        right_df[, j:=.I][, .SD, .SDcols=c('j', extra_columns_right)][left_df[, i:=.I][, .SD, .SDcols=c('i', extra_columns_left)][results, on=c('i'='i')], on=c('j'='j')]
    }

    # fix ordering of results
    # sorting data causes the j to refer to the sorted data, not the original data
    results$i <- left_df$.i_original_ordering_[results$i]
    results$j <- right_df$.j_original_ordering_[results$j]

    if(as_dataframe)
    {
        kept <- results$kept
        skipped <- results$skipped
        results$kept <- NULL
        results$skipped <- NULL
        # results <- data.table::setDT(results)
        if(isTRUE(keep_id))
        {
            results <- data.table::setDT(results, key=c(idcol_1, idcol_2))
        } else
        {
            results <- data.table::setDT(results)
        }
        data.table::setattr(x=results, name='kept', value=kept)
        data.table::setattr(x=results, name='skipped', value=skipped)

        # results <- data.frame(
        #     i = results$i,
        #     j = results$j,
        #     distance = results$distance
        # )
    }

    return(results)
}
