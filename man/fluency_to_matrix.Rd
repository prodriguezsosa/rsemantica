% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_model_data.R
\name{fluency_to_matrix}
\alias{fluency_to_matrix}
\title{Generate a matrix with rows as subjects and columns as step order}
\usage{
fluency_to_matrix(fluency_index, states_index)
}
\arguments{
\item{fluency_index}{a data.frame with 3 cols (output of fluency_to_int): (1) pid = subject id (character) (2) state_ID = state ID (integer) (3) num_states = # of states visited (numeric).}

\item{states_index}{a list of 2 (output of state_to_int): (1) the original character vector (2) a vector of integers, one for each token.}
}
\value{
matrix with nrows equal to the number of subjects and ncol equal to the longest list length
}
\description{
Generate a matrix with rows as subjects and columns as step order
}
