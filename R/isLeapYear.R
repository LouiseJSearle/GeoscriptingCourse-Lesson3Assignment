isLeapYear <- function(year) {
     result <- ifelse (is.numeric (year),
                    ifelse (year >= 1582,
                         ifelse (((year%%4 == 0) && (year%%100 != 0)) || ((year%%4 == 0) && (year%%100 == 0) && (year%%400 == 0)), T, F),
                         sprintf('%d is out of the valid range', year)),
                    'error: argument of class numeric expected')
     return(result)
     }

