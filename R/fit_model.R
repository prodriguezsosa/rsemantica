#' Fit stan model
#'
#' @param model a stan compiled model
#' @param model_name character indicating model name (for labeling purposes)
#' @param model_data model_data object (output of build_model_data or partition_data/partition_data_cv).
#' @param C regularization parameter.
#' @return list of length 2: (a) model hyperparameters (model and regularization value). (b) fitted stan model.
#' @export
fit_model <- function(model, model_name, model_data, C = 10) {
    # initialize empty list
    fit_list <- vector("list", 2)
    names(fit_list) <- c("hyperparameters", "fit")
    # assign user-defined parameters
    model_data[["C"]] <- C
    fit_list[["hyperparameters"]] <- c(model_name, C)
    # fit model
    fit_list[["fit"]] <- optimizing(model, data = model_data)
    return(fit_list)
}
