% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcZScores.R
\name{calcZScores}
\alias{calcZScores}
\title{calcZScores}
\usage{
calcZScores(df, idVar = "ID", ageVar = "AGE", weightVar = "WT",
  heightVar = "HT", heightConv = 0.01, weightConv = 1, femaleSexVal = 2,
  maleSexVal = 1, sexVar = "SEX", ageUnit = "years", missingVal = -99)
}
\arguments{
\item{df}{A data frame with all necessary columns for calculation of Z-scores.}

\item{idVar}{Name of the id column in df. Default is "ID" but it is 
not case sensitive.}

\item{ageVar}{Name of the age column in df. Default is "AGE" but it is not 
case sensitive.}

\item{weightVar}{Name of the weight column in df. Default is "WT" but it is not 
case sensitive.}

\item{heightVar}{Name of the height column in df. Default is "HT" but it is not 
case sensitive.}

\item{heightConv}{Height conversion factor to meters. Height*heightConv 
is height in meters. Default is 0.01, meaning that 
centimeters is expected in data.}

\item{weightConv}{Weight conversion factor to kilograms. Default is 1,
meaning that kilograms is expected in data. There is 
currently no support for stones or pounds.}

\item{femaleSexVal}{The value denoting female sex in the supplied df.
Default is 2.}

\item{maleSexVal}{The value denoting male sex in the supplied df. Default 
is 1.}

\item{sexVar}{Name of the sex column in df. Default is "SEX" but it is not 
case sensitive.}

\item{ageUnit}{A single string describing the unit for age in the data. 
"years", "months", or "days" are allowed. Defult is "years" 
but it is not case sensitive.}

\item{missingVal}{The value that will be inserted in place of Z-scores that
cannot be determined. Either because age is out of bounds 
of the reference WHO data, or because one of the required
values for age, weight, or height was missing from input 
data. Default is -99 according to Perl speaks NONMEM 
standard.}
}
\description{
A function that calculates height-for-age, weight-for-age, and BMI-for-age
Z-scores based on WHO reference data. Please note that height-for-age is 
only covered for up to 10 years old in the 2007 WHO growth reference. 
Returns a data frame with the same number of rows as the input data frame.
}
