#' Estimate semantic network
#'
#' @param sscorpus sscorpus object.
#' @param cue character indicating cue.
#' @param tag_set character vector containing the tags characterizing the groups of interest.
#' @param num_states ingeger value specifying number of top tokens.
#' @param num_folds ingeger value specifying number of folds for cross-validation.
#' @param tags_AND logical function specifying whether tags apply with an AND or an OR condition (e.g. 'republican' and/or 'male').
#' @param remove_cue logical value specifying whether cue should be removed from the top of every list.
#' @param min_count integer value specifying if only tokens with a given minimum count should be selected.
#' @param min_states an integer specifying a minimum semantic fluency list length.
#' @param states character vector specifying tokens (states) to be included in model.
#' @return list of length 2: (a) a fitted model (b) paramaters of a fitted model (transition matrix and initial probabilities).
#' @export
estimate_sn <- function(sscorpus, cue_set, tag_set, num_states = 30, num_folds = 10, tags_AND = TRUE, remove_cue = TRUE, min_count = NULL, min_states = 1, states = NULL) {
    
    group_sscorpus <- subset_sscorpus(sscorpus, cue_set = cue_set, tag_set = tag_set, tags_AND = tags_AND)
    
    if (is.null(states)) {
        group_top_states <- top_states(group_sscorpus, N = num_states, cue_set = NULL, tag_set = NULL, tags_AND = tags_AND, remove_cue = remove_cue, min_count = min_count)
        group_top_states <- group_top_states$fluency
    } else {
        group_top_states <- states
    }
    
    # create model data
    model_data <- build_model_data(group_sscorpus, states = group_top_states, min_states = min_states)
    
    # partition data to choose optimal c
    model_data_cv <- partition_data_cv(model_data, num_folds)
    
    # define parameter grid
    c_grid <- 10^c(1.5, 1, 0.5, 0, -0.5, -1, -1.5)
    
    # fit models for all folds
    c_lls <- vector("numeric", length(c_grid))
    for (c_value in 1:length(c_grid)) {
        # fit models on training set
        train_fits_cv <- lapply(1:num_folds, function(x) {
            fit_model(model, model_name = "RW_gauss_wo_rep", model_data_cv[[x]][["train_set"]][["parameters"]], C = c_grid[c_value])
        })
        
        # evaluate models on test set
        test_lls_cv <- sapply(1:num_folds, function(x) {
            model_ll(model = model, model_fit = train_fits_cv[[x]], model_data_cv[[x]][["test_set"]][["parameters"]])
        })
        
        # store optimal c
        c_lls[[c_value]] <- mean(test_lls_cv)
    }
    
    # choose optimal c
    optimal_c <- c_grid[which(c_lls == max(c_lls))]
    
    # train model using optimal c
    train_fit <- fit_model(model, model_name = "RW_gauss_wo_rep", model_data[["parameters"]], C = optimal_c)
    
    # extract parameters
    model_parameters <- extract_parameters(fit = train_fit[["fit"]], states = group_top_states)
    
    # return results
    return(list(fit = train_fit, parameters = model_parameters))
}


