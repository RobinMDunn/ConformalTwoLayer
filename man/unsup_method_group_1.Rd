% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unsup_method_group_1.R
\name{unsup_method_group_1}
\alias{unsup_method_group_1}
\title{Use only subject 1's observations to construct prediction set for
a new subject 1 observation}
\usage{
unsup_method_group_1(Y, alpha, Y_new = NULL)
}
\arguments{
\item{Y}{List containing data of all subjects. Each item in the list
is a vector with one subject's observations. Only Y[[1]] is used.}

\item{alpha}{Significance level}

\item{Y_new}{New observation on subject 1}
}
\value{
List containing prediction interval size, prediction interval
lower bound, prediction interval upper bound, and whether new
observation is contained inside prediction interval.
}
\description{
To check whether a potential y is in this interval, create
an augmented sample containing subject 1's observations and y.
As the nonconformity score for each observation in the augmented subject 1 data,
compute the absolute difference between each observation in the augmented sample
and the augmented mean. The p-value at y is the proportion of observations in the
augmented sample with nonconformity score >= y's nonconformity score.
The prediction set for a new observation on subject 1 is
\{y : p-value(y) >= alpha\}.
}
