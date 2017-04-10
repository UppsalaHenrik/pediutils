#' calcFatFreeMassCol
#' 
#' Calculates fat free mass for each row in a data frame and 
#' returns a vector of the same length as the number of columns
#' in that data frame.
#' 
#' @param df
#' @param idVar
#' @param ageVar
#' @param weightVar
#' @param heightVar
#' @param sexVar
#' @param ageUnit 
#' @param heightConv
#' @param weightConv 
#' @param femaleSexVal
#' @param maleSexVal
#' @param childCutoff
#' @param missingVal
#' 
#' @export

calcFatFreeMassCol <- function(df, idVar = "ID", ageVar = "AGE",
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
  
  # Make age unit option lower case. 
  # TODO: Should handle case for other string options maybe...
  ageUnit <- tolower(ageUnit)
  # Assign the appripriate age conversion
  if(ageUnit == "days" | ageUnit == "day" | ageUnit == "d"){
    ageConv <- 1/365.25
  } else if(ageUnit == "weeks" | ageUnit == "week" | ageUnit == "w"){
    ageConv <- 1/52
  } else if(ageUnit == "months" | ageUnit == "month" | ageUnit == "m"){
    ageConv <- 1/12
  } else if(ageUnit == "months" | ageUnit == "" | ageUnit == "y"){
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
    
    # Warn if any FFM values are calculated higher than 100% or 
    # lower than 10% of weight
    if(ffm/wt >=1 || ffm/wt <=0.1){
      warning(paste("ID", id, "has an implausible FFM value."))
    }
    
    # Return the vector for appending to 
    return(ffm)
  }))
  
  return(ffmVec)
}