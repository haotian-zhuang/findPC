\name{findPC}
\alias{findPC}
\title{findPC}
\description{
findPC is a function including multiple metrics to automatically determine the optimal number of principal components to retain based on the standard deviations explained by each PC.
}

\usage{
findPC(sdev,method = 'perpendicular line',aggregate = NULL,figure = FALSE)
}

\arguments{
\item{sdev}{The standard deviations explained by each principal components.}
\item{method}{Methods include 'all', 'piecewise linear model', 'first derivative', 'second derivative', 'preceding residual', 'perpendicular line (default)', 'k-means clustering'}
\item{aggregate}{An option to report the mean, median, or voting (median if all are different, otherwise mode) of the results from six methods}
\item{figure}{Whether to generate informative figures based on each method}
}
\examples{
# Return the default result (Perpendicular line):
findPC(sdev = sdev)

# Return the six results simultaneously:
findPC(sdev = sdev,method = 'all')

# Return voting (median if all are different, otherwise mode) of the results from six methods:
findPC(sdev = sdev,method = 'all',aggregate = 'voting')

# Print the figure based on Piecewise linear model:
findPC(sdev = sdev,method = 'piecewise linear model',figure = TRUE)

# Print the figure based on all methods:
findPC(sdev = sdev,method = 'all',figure = TRUE)
}
