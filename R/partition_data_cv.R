#' Partition data into training and test sets for cross-validation.
#'
#' @param model_data model_data object (output of build_model_data).
#' @param num_folds integer value specifying number of folds for cross-validation.
#' @return list of num_fold training sets each with its respective test set.
#' @export
partition_data_cv <- function(model_data, num_folds = 10) {
    # partition ids
    ids_cv <- subset_ids_cv(model_data, num_folds = num_folds)
    
    # get each partitions model data
    partitions_list <- lapply(ids_cv, function(x) subset_data_cv(x, model_data))
    return(partitions_list)
}

#---------------------------------------------
# sub-functions required by partition_data_cv
#---------------------------------------------

#' Partition subject ids into num_fold folds
#'
#' @param model_data model_data object (output of build_model_data).
#' @param num_folds integer value specifying number of folds for cross-validation.
#' @return subject ids partitioned into num_fold training sets each with its respective test set.
#' @export
subset_ids_cv <- function(model_data, num_folds = 10) {
    fold_tbl <- model_data[["id_list"]]
    fold_tbl <- fold_tbl %>% mutate(fold = rep_len(1:num_folds, length.out = nrow(fold_tbl)) %>% sample())  # generate fold ids and randomize order
    fold_list <- lapply(1:num_folds, function(x) fold_tbl %>% filter(fold == x) %>% select(pid, obs_row)) %>% setNames(paste0("fold", 1:num_folds))
    return(fold_list)
}

#' Subset model_data provided a set of ids for a training and a test set.
#'
#' @param model_data model_data object (output of build_model_data).
#' @param ids subject ids partitioned into training and test set (one fold of subset_ids_cv output).
#' @return model_data object for training and test-set.
#' @export
subset_data_cv <- function(ids, model_data) {
    
    # empty vector to be filled up
    output <- vector("list", 2) %>% setNames(c("train_set", "test_set"))
    
    # train set ---------------------------------------------- select rows
    ids_rows <- ids$obs_row
    # copy data (as most parameters remain the same)
    sub_data <- model_data
    # visited states matrix
    sub_data[["parameters"]][["a"]] <- sub_data[["parameters"]][["a"]][-ids_rows, ]
    # store number of subjects
    sub_data[["parameters"]][["m"]] <- nrow(sub_data[["parameters"]][["a"]])
    # store number of states visited by each subject
    sub_data[["parameters"]][["M"]] <- sub_data[["parameters"]][["M"]][-ids_rows]
    # store max # of states visited
    sub_data[["parameters"]][["max_M"]] <- max(sub_data[["parameters"]][["M"]])
    # store new IDs
    sub_data[["id_list"]] <- ids %>% select(pid) %>% mutate(obs_row = row_number())
    # store train set
    output[["train_set"]] <- sub_data
    
    # test set ---------------------------------------------- select rows
    ids_rows <- ids$obs_row
    # copy data (as most parameters remain the same)
    sub_data <- model_data
    # visited states matrix
    sub_data[["parameters"]][["a"]] <- sub_data[["parameters"]][["a"]][ids_rows, ]
    # store number of subjects
    sub_data[["parameters"]][["m"]] <- nrow(sub_data[["parameters"]][["a"]])
    # store number of states visited by each subject
    sub_data[["parameters"]][["M"]] <- sub_data[["parameters"]][["M"]][ids_rows]
    # store max # of states visited
    sub_data[["parameters"]][["max_M"]] <- max(sub_data[["parameters"]][["M"]])
    # store new IDs
    sub_data[["id_list"]] <- ids %>% select(pid) %>% mutate(obs_row = row_number())
    # store test set
    output[["test_set"]] <- sub_data
    
    # return output
    return(output)
}

