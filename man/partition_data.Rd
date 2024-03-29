% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/partition_data.R
\name{partition_data}
\alias{partition_data}
\title{Partition data into training and test set}
\usage{
partition_data(model_data, prop_train = 0.9)
}
\arguments{
\item{model_data}{model_data object (output of build_model_data).}

\item{prop_train}{proportion of observations to be used in training set.}
}
\value{
data partitioned a training- and a test-set.
}
\description{
Partition data into training and test set
}
