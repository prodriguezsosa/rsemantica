% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_fit.R
\name{model_ll}
\alias{model_ll}
\title{Evaluate log-likelihood of a model_data object under a fitted model (returns sum over all subjects).}
\usage{
model_ll(model, model_fit, model_data)
}
\arguments{
\item{model}{a stan compiled model.}

\item{model_fit}{a fitted stan model.}

\item{model_data}{model_data object for which to compute log-likelihoods.}
}
\value{
sum of log-likelihoods of all subjects in model_data object.
}
\description{
Evaluate log-likelihood of a model_data object under a fitted model (returns sum over all subjects).
}
