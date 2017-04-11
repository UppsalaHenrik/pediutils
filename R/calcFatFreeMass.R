#' calcFatFreeMass
#' 
#' Calculates fat free mass for each row in a data frame and 
#' returns a vector of the same length as the number of rowss
#' in that data frame.
#' 
#' @param df A data frame with all necessary columns for calculation of fat 
#'           free mass.
#' @param idVar Name of the id column in df. Default is "ID" but it is 
#'               not case sensitive.
#' @param ageVar Name of the age column in df. Default is "AGE" but it is 
#'               not case sensitive.
#' @param weightVar Name of the weight column in df. Default is "WT" but it is 
#'               not case sensitive.
#' @param heightVar Name of the height column in df. Default is "HT" but it is 
#'               not case sensitive.
#' @param sexVar Name of the sex column in df. Default is "SEX" but it is 
#'               not case sensitive.
#' @param heightConv Height conversion factor to meters. Height*heightConv 
#'                   is height in meters. Default is 0.01, meaning that 
#'                   centimeters is expected in data.
#' @param weightConv Weight conversion factor to kilograms. Default is 1,
#'                   meaning that kilograms is expected in data. There is 
#'                   currently no support for stones or pounds.
#' @param femaleSexVal The value denoting female sex in the supplied df.
#'                     Default is 2.
#' @param maleSexVal The value denoting male sex in the supplied df. Default 
#'                   is 1.
#' @param ageUnit A single string describing the unit for age in the data. 
#'                "years", "months", or "days" are allowed. Defult is "years" 
#' @param missingVal The value that will be inserted in place of Z-scores that
#'                   cannot be determined. Either because age is out of bounds 
#'                   of the reference WHO data, or because one of the required
#'                   values for age, weight, or height was missing from input 
#'                   data. Default is -99 according to Perl speaks NONMEM 
#'                   standard.
#' @param childCutoff The age, in years, at which a person is considered to be
#'                    an adult. Default is 18.
#' 
#' @export
#' 
#'
#' 
#' 

calcFatFreeMass <- function(df, idVar = "ID", ageVar = "AGE",
                            weightVar = "WT", heightVar = "HT", 
                            sexVar = "SEX", ageUnit = "years", 
                            heightConv = .01, weightConv = 1, 
                            femaleSexVal = 2, maleSexVal = 1,
                            childCutoff = 18, missingVal = -99){
  
  # If not all required columns are there, abort
  sapply(c(idVar, ageVar, weightVar, heightVar, sexVar), function(x){
    if(!x %in% colnames(df)){
      stop(paste("The required column", x, "was not found in the data.",
                 "Did you set all variable names?"))
    }
  })
  
  # Make options lower case to be case insensitive.
  idVar <- tolower(idVar)
  ageVar <- tolower(ageVar)
  weightVar <- tolower(weightVar)
  heightVar <- tolower(heightVar)
  sexVar <- tolower(sexVar)
  ageUnit <- tolower(ageUnit)
  
  # Make column names lower case to match
  names(df) <- tolower(names(df))
  
  # Assign the appripriate age conversion
  if(ageUnit == "days" | ageUnit == "day" | ageUnit == "d"){
    ageConv <- 1/365.25
  } else if(ageUnit == "weeks" | ageUnit == "week" | ageUnit == "w"){
    ageConv <- 1/52
  } else if(ageUnit == "months" | ageUnit == "month" | ageUnit == "m"){
    ageConv <- 1/12
  } else if(ageUnit == "years" | ageUnit == "year" | ageUnit == "y"){
    ageConv <- 1
  } else {
    stop(paste("Option ageUnit must be one of the accepted string options:",
               "days, weeks, months, or years. You specified ageUnit as: ", 
               ageUnit))
  }
  
  # apply over all rows to determine the constants to use
  ffmVec <- unname(apply(df, 1, function(x){
    # First set constants according to sex
    if(x[sexVar] == femaleSexVal){
      alAlpha <- 1.11
      a50     <- 7.1
      alGamma <- 1.1
      whsMax  <- 37.99
      whs50   <- 35.98
    }else{
      alAlpha <- 0.88
      a50     <- 13.4
      alGamma <- 12.7
      whsMax  <- 42.92
      whs50   <- 30.93
    }
    
    # Pick out id
    id <- as.numeric(x[idVar])
    # Calculate height^2 in meters
    ht2 <- (as.numeric(x[heightVar])*heightConv)^2
    # Convert weight if needed
    wt  <- as.numeric(x[weightVar])*weightConv
    # Pick out age and 
    age <- as.numeric(x[ageVar])*ageConv
    # Some presteps to the calculation
    ageGam <- age^alGamma
    a50Gam  <- a50^alGamma
    
    
    # Use FFM formula according to adult or not
    if(age >= childCutoff){
      ffm <- (whsMax*ht2*wt)/(whs50*ht2+wt)
    }else{
      ffm <- ((ageGam+alAlpha*a50Gam)/(ageGam+a50Gam))*
        ((whsMax*ht2*wt)/(whs50*ht2+wt))
    }
    
    # Warn if any FFM values are calculated higher than 100 percent or 
    # lower than 10 percent of weight
    if(ffm/wt >=1 || ffm/wt <=0.1){
      warning(paste("ID", id, "has an implausible FFM value."))
    }
    
    # Return the vector for appending to 
    return(ffm)
  }))
  
  return(ffmVec)
}