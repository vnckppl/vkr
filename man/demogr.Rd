% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/demogr.R
\name{demogr}
\alias{demogr}
\title{Print demograhics for a variable by group}
\usage{
demogr(idf, voi, group, mediqr = FALSE)
}
\arguments{
\item{idf}{Input data frame}

\item{voi}{Variable of interest}

\item{group}{Group variable to stratify the variable of interest by}

\item{mediqr}{If TRUE, then calculate median/IQR instead of mean/stdev}
}
\description{
This function prints demographics for a specific variable and stratifies this
by the factor levels of a second variable. It detects if your first variable
is a factor or numeric variable. Based on that, it will provides means and sd
for all groups of the numeric variable and counts and percentages for all
groups for the factor variable. It also tests the distributions of all
groups against the reference category. For categorical variables, it also
prvovides the chi-square p-value for the entire table. Results are returned
as a data frame.
}
