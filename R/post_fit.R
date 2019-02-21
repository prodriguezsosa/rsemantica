#' Evaluate log-likelihood of a model_data object under a fitted model (returns sum over all subjects).
#'
#' @param model a stan compiled model.
#' @param model_fit a fitted stan model.
#' @param model_data model_data object for which to compute log-likelihoods.
#' @return sum of log-likelihoods of all subjects in model_data object.
#' @export
model_ll <- function(model, model_fit, model_data){
  ll_list <- compute_ll(model, model_fit, model_data)
  return(sum(ll_list))
}

#---------------------------------------------
# sub-functions required by model_ll
#---------------------------------------------
#' Evaluate log-likelihood of a model_data object under a fitted model.
#'
#' @param model a stan compiled model.
#' @param model_fit a fitted stan model.
#' @param model_data model_data object for which to compute log-likelihoods.
#' @return log-likelihoods, one for subject in model_data object.
compute_ll <- function(model, model_fit, model_data){
  # get model objective function
  expose_stan_functions(model)
  # get parameters
  model_params <- extract_parameters(fit = model_fit[["fit"]], states = NULL)
  # evaluate
  ll_list = get_stan_ll(model_params$pi,
                        model_params$P,
                        # need to make list instead of putting in a as a matrix
                        # see http://discourse.mc-stan.org/t/rstan-error-vector-elt-can-only-be-applied-to-a-list-not-a-double/2093
                        lapply(1:nrow(model_data$a), FUN = function(i) model_data$a[i,]),
                        model_data$M,
                        model_data$m,
                        model_data$max_M,
                        model_data$n)
  # return sum by subject
  return(rowSums(ll_list))
}

#' Extract parameters from a fitted stan model.
#'
#' @param fit fitted model.
#' @param states character vector of tokens (states).
#' @return list of length 2: (a) pi = numeric vector of initial probabilities (b) P = numeric matrix of transition probabilities.
#' @export
extract_parameters <- function(fit, states){
  # extract parameters
  params <- fit$par
  params_names <- names(fit$par)
  # number of states
  n <- length(grep("^pi\\[", params_names))
  # initial probabilities
  pi <- array(params[grepl("^pi\\[", params_names)], n)
  # transition matrix (can check each row sums to 1)
  transit_matrix <- matrix(unname(params[grepl("^P\\[", params_names)]), n, n)
  # assign state names
  if(!is.null(states)){
    names(pi) <- states
    rownames(transit_matrix) <- colnames(transit_matrix) <- states}
  return(list(pi = pi, P = transit_matrix))
}



