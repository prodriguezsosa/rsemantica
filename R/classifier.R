#' Train a semantic-network based classifier
#'
#' @param cue_set character indicating cue.
#' @param tags character vector or a list of length 2 containing the tags characterizing the two groups of interest.
#' @param sscorpus sscorpus object.
#' @param num_states ingeger value specifying number of top tokens.
#' @param num_folds ingeger value specifying number of folds for cross-validation.
#' @param tags_AND logical function specifying whether tags apply with an AND or an OR condition (e.g. 'republican' and/or 'male').
#' @param remove_cue logical value specifying whether cue should be removed from the top of every list.
#' @param min_count integer value specifying if only tokens with a given minimum count should be selected.
#' @param min_states an integer specifying a minimum semantic fluency list length.
#' @return data partitioned into 10 training sets each with its respective test fold.
#' @export
sn_classifier <- function(cue_set, tags, sscorpus, num_states = 30, num_folds = 10, tags_AND = TRUE, remove_cue = TRUE, min_count = NULL, min_states = 1) {
    
    # get data on the cue of interest
    cue_sscorpus <- subset_sscorpus(sscorpus, cue_set = cue_set, tag_set = NULL, tags_AND = TRUE)
    
    # identify top tokens for each group
    top_states <- lapply(tags, function(x) top_states(cue_sscorpus, N = num_states, cue_set = NULL, tag_set = x, tags_AND = tags_AND, remove_cue = remove_cue, min_count = min_count))
    top_states <- lapply(top_states, function(x) x[["fluency"]])
    top_states <- top_states %>% unlist() %>% unique()  # keep the union
    
    # generate lists to store outputs of the loop by group
    group_fits <- vector("list", length(tags))
    group_test_sets <- vector("list", length(tags))
    names(group_fits) <- names(group_test_sets) <- tags
    
    # for each group
    for (i in tags) {
        
        # get group data
        group_data <- subset_sscorpus(sscorpus, cue_set = cue_set, tag_set = i, tags_AND = tags_AND)
        
        # create model data
        model_data_all <- build_model_data(group_data, states = top_states, min_states = min_states)
        
        # partition data into N folds (these folds are used to evaluate the classifier)
        ids_list <- subset_ids_cv(model_data_all, num_folds = 10)
        
        # generate lists to store outputs of the loop by fold
        fold_fits <- vector("list", num_folds)
        fold_test_sets <- vector("list", num_folds)
        names(fold_fits) <- names(fold_test_sets) <- paste0("fold", 1:num_folds)
        
        # for each fold
        for (f1 in 1:num_folds) {
            
            # select 1 fold to be test set
            test_ids <- ids_list[f1] %>% do.call(rbind, .)
            
            # select 9 folds to be train set
            train_ids <- ids_list[-f1] %>% do.call(rbind, .)
            
            # subset group data for test set
            group_data_test <- subset_sscorpus_ids(group_data, test_ids)
            # group_data_test <- SubsetGroupData(ids = test_ids, data = group_data)
            
            # subset group data for train set
            group_data_train <- subset_sscorpus_ids(group_data, train_ids)
            # group_data_train <- SubsetGroupData(ids = train_ids, data = group_data)
            
            # create model data test
            model_data_test <- build_model_data(group_data_test, states = top_states, min_states = min_states)
            # model_data_test <- SSModelData(data = group_data_test, states = top_states, min_states = 1)
            
            # create model data train
            model_data_train <- build_model_data(group_data_train, states = top_states, min_states = min_states)
            # model_data_train <- SSModelData(data = group_data_train, states = top_states, min_states = 1)
            
            # partition train data to choose optimal c
            model_data_train_cv <- partition_data(model_data_train, prop_train = 0.9)
            # train_ids_cv <- SubsetIds(model_data_train, p = 0.9, seed = 1984L) model_data_train_cv <- PartitionData(data = model_data_train, ids_list = train_ids_cv)
            
            # define parameter grid
            c_grid <- 10^c(1, 0.5, 0, -0.5, -1)
            
            # for each value of c fit model on train set
            train_fits_cv <- lapply(c_grid, function(x) {
                fit_model(model, model_name = "RW_gauss_wo_rep", model_data_train_cv[["train_set"]][["parameters"]], C = x)
            })
            
            # for each value of c evaluate model on test set only the first 2 had finite gradients
            test_lls_cv <- sapply(1:length(c_grid), function(x) {
                model_ll(model = model, model_fit = train_fits_cv[[x]], model_data_train_cv[["test_set"]][["parameters"]])
            })
            
            # select optimal c
            optimal_c <- c_grid[which(test_lls_cv == max(test_lls_cv))]
            cat("optimal c for group", i, "= ", optimal_c, "\n")
            
            # train full model (entire training set) using optimal c
            train_fit <- fit_model(model, model_name = "RW_gauss_wo_rep", model_data_train[["parameters"]], C = optimal_c)
            
            # store fit and test data for this fold
            fold_fits[[f1]] <- train_fit
            fold_test_sets[[f1]] <- model_data_test
        }
        
        # store fit and test data for this group
        group_fits[[i]] <- fold_fits
        group_test_sets[[i]] <- fold_test_sets
    }
    
    # compute within and across lls for test sets
    class_dt <- ll_table(cue = cue_set, fits = group_fits, test_sets = group_test_sets)
    
    # return results
    return(class_dt)
}

#---------------------------------------------
# sub-functions required by sn_classifier
#---------------------------------------------

#' Build classification table given model fits
#'
#' @param cue character indicating cue.
#' @param fits list of model fits of length (num_folds)
#' @param test_sets list of test-sets, one for each model fit.
#' @return data.frame with each subject's log-likelihood under each model, a classification and a true label.
#' @export
ll_table <- function(cue, fits, test_sets) {
    groups <- names(fits)
    test_fits_groups <- vector("list", length(groups))
    names(test_fits_groups) <- groups
    folds <- length(fits[[1]])
    
    # evaluate test sets on their own group model
    for (i in groups) {
        within_lls <- lapply(1:folds, function(x) {
            lls <- compute_ll(model = model, model_fit = fits[[i]][[x]], model_data = test_sets[[i]][[x]]$parameters)
            lls_dt <- tibble(pid = test_sets[[i]][[x]]$id_list$pid, model = x, within = lls)
        })
        # combine all folds
        within_lls <- do.call(rbind, within_lls)
        
        # across_lls <- lapply(1:folds, function(x){ ComputeLL(model = model, model_fit = fits[[setdiff(groups, i)]][[x]], data = test_sets[[i]][[x]]$parameters) })
        
        # evaluate test sets on the opposite group's model
        across_lls <- lapply(1:folds, function(x) {
            data_i <- test_sets[[i]][[x]]$parameters
            lapply(1:folds, function(y) {
                lls <- compute_ll(model = model, model_fit = fits[[setdiff(groups, i)]][[y]], model_data = data_i)
                lls_dt <- tibble(pid = test_sets[[i]][[x]]$id_list$pid, across = lls)
            })
        })
        # take the mean within folds (more than one model for each test set)
        across_lls <- lapply(across_lls, function(x) do.call(rbind, x) %>% group_by(pid) %>% summarise(across = mean(across)))
        # combine folds
        across_lls <- do.call(rbind, across_lls)
        
        # merge
        joint_lls <- inner_join(within_lls, across_lls, by = "pid")
        
        # build tibble
        test_fits_groups[[i]] <- cbind(cue = cue, joint_lls, class = i, across_class = setdiff(groups, i))
    }
    class_dt <- do.call(rbind, test_fits_groups)
    class_dt <- class_dt %>% mutate(model = ifelse(class == groups[2], model + folds, model))
    class_dt <- class_dt %>% mutate(diff = within - across)
    class_dt <- class_dt %>% mutate(cue = as.character(cue), model = as.character(model), class = as.character(class), across_class = as.character(across_class))
    class_dt <- class_dt %>% mutate(predicted = ifelse(within > across, class, across_class))
    class_dt <- class_dt %>% mutate(class = as.factor(class), predicted = as.factor(predicted))
    class_dt <- class_dt %>% select(-across_class)
    
    return(class_dt)
}

#---------------------------------------------
# post-estimation
#---------------------------------------------

#' Compute accuracy based on classificaiton table.
#'
#' @param class_dt output of sn_classifier.
#' @return an accuracy score with upper and lower bounds.
#' @export
accuracy_tibble <- function(class_dt) {
    
    cue <- unique(class_dt$cue)
    # estimate confusion matrix (uses caret package)
    confusion_dt <- confusionMatrix(class_dt$predicted, class_dt$class)
    
    # get accuracy scores
    accuracy_dt <- tibble(cue = cue, acc = confusion_dt$overall[["Accuracy"]], lower = confusion_dt$overall[["AccuracyLower"]], upper = confusion_dt$overall[["AccuracyUpper"]], baseline = confusion_dt$overall[["AccuracyNull"]])
    
    # net out baseline (?)
    accuracy_dt <- accuracy_dt %>% mutate(acc = acc - (baseline - 0.5), lower = lower - (baseline - 0.5), upper = upper - (baseline - 0.5), baseline = baseline - (baseline - 0.5))
    # return output
    return(accuracy_dt)
}

