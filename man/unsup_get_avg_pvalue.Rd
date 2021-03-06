% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unsup_get_avg_pvalue.R
\name{unsup_get_avg_pvalue}
\alias{unsup_get_avg_pvalue}
\title{Get average p-value for unsupervised repeated subsampling method}
\usage{
unsup_get_avg_pvalue(Y_subsample_mat, u)
}
\arguments{
\item{Y_subsample_mat}{Data frame where each row is a subsample of one
observation from each of the k subjects}

\item{u}{Observation at which to compute p-value}
}
\value{
Average p-value of u across repeated subsamples
}
\description{
On a subsample b containing 1 observation from each of the k
subjects, the p-value at u is inf\{alpha : u not in [Y^b_{(r)}, Y^b_{(s)}]\},
where r = floor((k+1)(alpha/2)) and s = ceiling((k+1)(1-alpha/2)).
The p-value is 1 if u is in [Y^b_(floor((k+1)/2)), Y^b_(ceiling((k+1)/2))].
Compute p-value of u on each subsample, and return the average p-value.
}
