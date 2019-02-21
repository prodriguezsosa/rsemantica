#' Partition data into training and test set
#'
#' @param model_data model_data object (output of build_model_data).
#' @param prop_train proportion of observations to be used in training set.
#' @return data partitioned a training- and a test-set.
#' @export
partition_data <- function(model_data, prop_train = 0.9) {
    # partition ids
    ids_split <- subset_ids(model_data, prop_train = prop_train)
    
    # get each partitions model data
    partitions_list <- lapply(ids_split, function(x) subset_data(x, model_data))
    return(partitions_list)
}

#---------------------------------------------
# sub-functions required by partition_data
#---------------------------------------------

#' Partition subject ids into training and test set.
#'
#' @param model_data model_data object (output of build_model_data).
#' @param prop_train proportion of observations to be used in training set.
#' @return subject ids partitioned into a training- and a test-set.
#' @export
subset_ids <- function(model_data, prop_train = 0.9) {
    output_list <- vector("list", 2) %>% setNames(c("train_set", "test_set"))
    id_tbl <- model_data[["id_list"]]
    ids <- 1:nrow(id_tbl)
    train_ids <- sample(ids, ceiling(prop_train * length(ids)))
    test_ids <- ids[-train_ids]
    output_list[["train_set"]] <- id_tbl[train_ids, ]
    output_list[["test_set"]] <- id_tbl[test_ids, ]
    return(output_list)
}

#' Subset model_data provided a set of ids.
#'
#' @param model_data model_data object (output of build_model_data).
#' @param ids subject ids.
#' @return subsetted model_data object.
#' @export
subset_data <- function(ids, model_data) {
    # select rows
    ids_rows <- ids$obs_row
    # copy data (as most parameters remain the same)
    sub_data <- model_data
    # visited states matrix
    sub_data[["parameters"]][["a"]] <- sub_data[["parameters"]][["a"]][ids_rows, ]
    # store number of subjects
    sub_data[["parameters"]][["m"]] <- length(ids_rows)
    # store number of states visited by each subject
    sub_data[["parameters"]][["M"]] <- sub_data[["parameters"]][["M"]][ids_rows]
    # store max # of states visited
    sub_data[["parameters"]][["max_M"]] <- max(sub_data[["parameters"]][["M"]])
    # store new IDs
    sub_data[["id_list"]] <- ids %>% select(pid) %>% mutate(obs_row = row_number())
    # return output
    return(sub_data)
}



