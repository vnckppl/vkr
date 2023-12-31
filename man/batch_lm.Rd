% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batch_lm_lm.R
\name{batch_lm}
\alias{batch_lm}
\title{Print summary statistics for a linear model}
\usage{
batch_lm(
  df,
  outcome,
  predictor,
  covariates = NULL,
  numdec = 2,
  np = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{df}{Input data frame}

\item{outcome}{Outcome measure}

\item{predictor}{Predictor}

\item{covariates}{Single covariate or array of covariates}

\item{numdec}{Number of decimals for the beta and std statistics}

\item{np}{If set with two values (Ca and maxIter), the code will run lmp,
from the lmPerm package, i.e., fitting and testing linear models with
permutation tests. Ca=Stop iterations when estimated standard error of the
estimated p is less than Ca*p. maxIter=The maximum number of iterations.
Note that this is a random permutation method, but the seed has been
explicitely set to 357 so that results will be replicable.}

\item{verbose}{Set to TRUE if you want to see the full lm output}
}
\description{
This function prints summary statistics for a linear model such as the
estimate, std.err, p-value, and effect size for a predictor. It will
automatically detect if the predictor variable is numeric or a factor
variable. If it is a factor variable, then estimates etc. will be provided
for all levels of the facor variable (except the first level, that will be
the reference categorie)
}
