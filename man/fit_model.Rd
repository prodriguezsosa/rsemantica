% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_model.R
\name{fit_model}
\alias{fit_model}
\title{Fit stan model}
\usage{
fit_model(model, model_name, model_data, C = 10)
}
\arguments{
\item{model}{a stan compiled model}

\item{model_name}{character indicating model name (for labeling purposes)}

\item{model_data}{model_data object (output of build_model_data or partition_data/partition_data_cv).}

\item{C}{regularization parameter.}
}
\value{
list of length 2: (a) model hyperparameters (model and regularization value). (b) fitted stan model.
}
\description{
Fit stan model
}
