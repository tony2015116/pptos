% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outliers_found.R
\name{outliers_found}
\alias{outliers_found}
\title{outliers_found}
\usage{
outliers_found(data, ...)
}
\arguments{
\item{data}{transformed data from function of transform}

\item{...}{the rubost model fomula}
}
\value{
the weighting of each visit
}
\description{
outliers_found
}
\examples{
#temp2 <- outliers_found(data = temp1, weight ~ seq_days + I(seq_days^2), maxit=120)
}
