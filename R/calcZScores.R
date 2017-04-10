#' calcZScores
#' 
#' A function that calculates height-for-age, weight-for-age, and BMI-for-age
#' Z-scores based on WHO reference data. Please note that height-for-age is 
#' only covered for up to 10 years old in the 2007 WHO growth reference.
#' 
#' @param df A data frame with all necessary columns for calculation of Z-scores.
#' @param ageVar Name of the age column in df. Default is "AGE"
#' @param weightVar Name of the weight column in df. Default is "WT"
#' @param heightVar Name of the height column in df. Default is "HT"
#' @param sexVar Name of the sex column in df. Default is "SEX"
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
#'                   
#' @export
#' 
#'
#' 
#' 




calcZScores <- function(df, ageVar = "AGE", weightVar = "WT", heightVar = "HT",
                        heightConv = .01, weightConv = 1, femaleSexVal = 2, 
                        maleSexVal = 1, sexVar = "SEX", ageUnit = "years", 
                        missingVal = -99){
  
  # If not all required columns are there, abort
  sapply(c(ageVar, weightVar, heightVar, sexVar), function(x){
    if(!x %in% colnames(df)){
      stop(paste("The required column", x, "was not found in the data.",
                 "Did you set all variable names?"))
    }
  })
  
  # Make age unit option lower case. 
  ageUnit <- tolower(ageUnit)
  
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
  
  dfList <- split(df[c(ageVar, weightVar, heightVar, sexVar)], 
                  seq(nrow(df)))
  
  # Apply over data frame
  zScoreList <- lapply(dfList, function(x){
    
    # Unlist to make sure
    x <- unlist(x, use.names = FALSE)
    
    # Convert sex values to WHO standard
    if(x[4] == femaleSexVal){
      whoSex <- 2
    }else if(x[4] == maleSexVal){
      whoSex <- 1
    }else{
      whoSex <- NA
    }
    whoDf <- data.frame(sex = whoSex)
    
    # Convert age to years, months and days
    whoDf$age.years <- x[1]*ageConv
    whoDf$age.mo <- whoDf$age.years*12
    whoDf$age.days <- round(whoDf$age.years*365.25)
    
    # WHO reference heights are in centimeters
    whoDf$htm <- x[3]*heightConv
    whoDf$clenhei <-  whoDf$htm*100
    whoDf$height <-  whoDf$htm*100
    
    # Add weight
    whoDf$weight <- x[2]*weightConv
    
    # Assume no oedema
    whoDf$oedema <- "n"
    
    # Calculate BMI
    whoDf$cbmi <- whoDf$weight/whoDf$htm^2
    
    # Get the number of columns at this point to remove 
    # them when returning Z sores
    numCols <- ncol(whoDf)

    # If over 5 years use WHO 2007 and otherwise use 2006
    if(whoDf$age.days > 1856){
      HAZ2 <- calc.zhfa.over5(whoDf, WHOdata$hfawho2007)[1]
      WAZ2 <- calc.zwei.over5(whoDf, WHOdata$wfawho2007)[1]
      BAZ2 <- calc.zbmi.over5(whoDf, WHOdata$bfawho2007)[1]
    }else{
      HAZ2 <- calc.zlen.upto5(whoDf, WHOdata$lenanthro)[1]
      WAZ2 <- calc.zwei.upto5(whoDf, WHOdata$weianthro)[1]
      BAZ2 <- calc.zbmi.upto5(whoDf, WHOdata$bmianthro)[1]
    }
    
    zVals <- c(HAZ, WAZ, BAZ)
    names(zVals) <- c("HAZ", "WAZ", "BAZ")
    
    # Return in some fashion...
    return(zVals)
  })

  # make it a data frame
  zScoreDf <- do.call("rbind", zScoreList)

  # Insert the requested value for missing calculations
  zScoreDf[is.na(zScoreDf)] <- missingVal 
    
  # Probably should check number of rows is correct before returning them...
  
  # Return only the z-scores
  return(zScoreDf)
}