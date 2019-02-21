#' Build data to be used for model fitting
#'
#' @param sscorpus sscorpus object.
#' @param states character vector of tokens to be included in the model.
#' @param min_states an integer specifying a minimum semantic fluency list length.
#' @return a list with model parameters.
#' @export
build_model_data <- function(sscorpus, states = NULL, min_states = 1) {
    # generate empty list to be filled with data components ----------------------------------------------
    model_data <- list(parameters = vector("list", 9), id_list = NA)
    names(model_data[["parameters"]]) <- c("m", "n", "M", "a", "max_M", "alpha", "C", "beta_len", "beta_inds")
    # number of states
    n <- length(states)
    # store number of states
    model_data[["parameters"]][["n"]] <- n  # number of states
    
    # assign integer index to each state ----------------------------------------------
    states_index <- state_to_int(states)
    
    # convert lists to integers -------------------------------------------------------
    fluency_index <- fluency_to_int(sscorpus, states_index, min_states)
    
    # generate visited states matrix --------------------------------------------------
    model_data[["parameters"]][["a"]] <- fluency_to_matrix(fluency_index, states_index)
    # store number of subjects
    model_data[["parameters"]][["m"]] <- fluency_index %>% pull(pid) %>% length()
    # store number of states visited by each subject
    model_data[["parameters"]][["M"]] <- fluency_index %>% pull(num_states)
    # store max # of states visited
    model_data[["parameters"]][["max_M"]] <- max(fluency_index$num_states)
    # generate transition matrix -----------------------------------------------------
    model_data[["parameters"]][["beta_inds"]] <- transit_matrix(n)
    # number of off-diagonal entries in transition matrix
    model_data[["parameters"]][["beta_len"]] = (n^2 - n)
    # prior on initial state (Dirichlet)
    model_data[["parameters"]][["alpha"]] = array(1, n)
    # user-defined transition prior
    model_data[["parameters"]][["C"]] <- NA
    # store IDs
    model_data[["id_list"]] <- fluency_index %>% select(pid) %>% mutate(obs_row = row_number())
    # return output
    return(model_data)
}

#---------------------------------------------
# sub-functions required by build_model_data
#---------------------------------------------

#' Assign a unique integer to each token (state).
#'
#' @param states a character vector of tokens.
#' @return a list of 2: (1) the original character vector (2) a vector of integers, one for each token.
state_to_int <- function(states) {
    return(list(states = states, int_id = 1:length(states)))
}

#' Convert lists of tokens into lists of integers using a pre-defined index (output of state_to_int).
#'
#' @param sscorpus an sscorpus (generally a subsetted sscorpus)
#' @param states_index a list of 2: (1) the original character vector (2) a vector of integers, one for each token. Output of state_to_int.
#' @param min_states an integer specifying a minimum semantic fluency list length.
#' @return a data.frame with 3 cols: (1) pid = subject id (2) state_ID = fluency lists in integer form (3) num_states = # of states visited
fluency_to_int <- function(sscorpus, states_index, min_states = 1) {
    # get fluency data
    fluency <- sscorpus[["fluency"]]
    # keep only states in user-determined list
    fluency <- fluency %>% filter(fluency %in% states_index[["states"]])
    # map states to integers using predefined indices
    fluency_index <- fluency %>% mutate(state_ID = plyr::mapvalues(fluency, states_index[["states"]], states_index[["int_id"]])) %>% select(-fluency)
    # reshape to wide format by subject
    fluency_index <- fluency_index %>% group_by(pid) %>% summarise(state_ID = paste0(state_ID, collapse = " ")) %>% ungroup()
    # compute # of states visited by each subject
    fluency_index <- fluency_index %>% mutate(num_states = str_count(state_ID, " ") + 1)
    # apply min_states condition
    fluency_index <- fluency_index %>% filter(num_states >= min_states)
    return(fluency_index)
}

#' Generate a matrix with rows as subjects and columns as step order
#'
#' @param fluency_index a data.frame with 3 cols (output of fluency_to_int): (1) pid = subject id (character) (2) state_ID = state ID (integer) (3) num_states = # of states visited (numeric).
#' @param states_index a list of 2 (output of state_to_int): (1) the original character vector (2) a vector of integers, one for each token.
#' @return matrix with nrows equal to the number of subjects and ncol equal to the longest list length
fluency_to_matrix <- function(fluency_index, states_index) {
    # pad fluency lists to make them all of equal length
    fluency_index <- fluency_index %>% rowwise() %>% mutate(state_ID = pad_fluency(state_ID, states_index)) %>% ungroup()
    # return to long format
    fluency_index <- fluency_index %>% transform(state_ID = strsplit(state_ID, " ")) %>% tidyr::unnest()
    # create state order ID by subject
    fluency_index <- fluency_index %>% group_by(pid) %>% mutate(order_ID = row_number()) %>% ungroup()
    # transform to matrix format
    fluency_matrix <- fluency_index %>% tidyr::spread(order_ID, state_ID, fill = 0) %>% select(-c(pid, num_states)) %>% mutate_all(funs(as.integer)) %>% as.matrix()
    # remove row and column names
    attr(fluency_matrix, "dimnames") <- NULL
    return(fluency_matrix)
}

#' Pad fluency lists
#'
#' @param state_ID  fluency lists in integer form (column of fluency_to_int output).
#' @param states_index a list of 2 (output of state_to_int): (1) the original character vector (2) a vector of integers, one for each token.
#' @return a 1 column data.frame with fluency lists in wide integer form (all lists of equal length).
pad_fluency <- function(state_ID, states_index) {
    # longest list length
    max_N <- max(states_index[["int_id"]])
    # tokenize fluency lists
    fluency_vector <- str_split(state_ID, " ") %>% unlist() %>% as.integer()
    # pad each subject's fluency list such that they are all of equal length (this is simply for convenience, non-reported states are not counted when fitting model)
    fluency_vector <- c(fluency_vector, setdiff(1:max_N, fluency_vector))
    # collapse lists by subject
    fluency_vector <- paste0(fluency_vector, collapse = " ")
    return(fluency_vector)
}

#' Build an indexed transition matrix
#'
#'
#' @param num_states an intenger specifying number of states
#' @return a symmetric matrix with unique indices on each off-diagonal entry (diagonal is always 0 as self-loops are not allowed)
transit_matrix <- function(num_states) {
    beta_matrix <- matrix(NA, num_states, num_states)
    beta_matrix[lower.tri(beta_matrix, diag = FALSE)] <- 1:(num_states * (num_states - 1)/2)
    beta_matrix <- t(beta_matrix)
    beta_matrix[lower.tri(beta_matrix)] <- ((num_states * (num_states - 1)/2) + 1):(num_states * (num_states - 1))
    diag(beta_matrix) <- 0
    return(beta_matrix)
}
