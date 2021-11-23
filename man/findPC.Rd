\name{findPC}
\alias{findPC}
\title{findPC}
\description{
findPC is a function including six methods to automatically determine the optimal number of principal components to retain based on the standard deviations explained by each PC.
}

\usage{
findPC(sdev,number = 20,method = 'perpendicular line',aggregate = NULL,figure = FALSE)
}

\arguments{
\item{sdev}{The standard deviations explained by each principal components.}
\item{method}{Methods include 'all', 'piecewise linear model', 'first derivative', 'second derivative', 'preceding residual', 'perpendicular line (default)', 'k-means clustering'}
\item{number}{A vector including number of PCs used in the function}
\item{aggregate}{An option to report the mean, median, or voting (median if all are different, otherwise mode) of the result}
\item{figure}{Whether to print a heatmap showing the result}
}
\examples{
# Return the default result (Perpendicular line with 20 PCs):
findPC(sdev = sdev)

# Return the result based on Piecewise linear model with 16,20 and 28 PCs:
findPC(sdev = sdev,number = c(16,20,28),method = 'piecewise linear model')

# Return the result based on six methods with 16,20 and 28 PCs:
findPC(sdev = sdev,number = c(16,20,28),method = 'all')

# Return voting (median if all are different, otherwise mode) of the results from six methods with 16,20 and 28 PCs:
findPC(sdev = sdev,number = c(16,20,28),method = 'all',aggregate = 'voting')

# Print a heatmap based on six methods with 16,20 and 28 PCs:
findPC(sdev = sdev,number = c(16,20,28),method = 'all',figure = T)
}
