## Louise Searle
## January 07 2015

## isLeapYear function definition.

# Function input: year
# Accepted class is numeric, where decimal values are converted to integers for leap year calculation.
# Function outputs:
# 'TRUE' if year is a leap year.
# 'FALSE' if year is not a leap year.
# '"year" is out of the valid range' if year is not within Gregorian calendar range.
# 'error: argument of class numeric expected' if year is not a numeric value.

isLeapYear <- function(year) {
     # Assign result of nested ifelse statements to 'result' variable. 
     # ifelse condition: is year an integer class variable or not.
     result <- ifelse (is.numeric(year),
                       # if true: 
                       # ifelse condition: is year larger than or equal to 1582 (1582 is start of Gregorian calendar).
                       ifelse (year >= 1582,
                               # if true: 
                               # if else condition: is the year a leap year according to algorithm conditions, assign T is True or F is False.
                               ifelse (((as.integer(year) %% 4 == 0) & (as.integer(year) %% 100 != 0)) | ((as.integer(year) %% 4 == 0) & (as.integer(year) %% 100 == 0) & (as.integer(year) %% 400 == 0)), T, F),
                               # if false:
                               # assign string reporting that year is not within calendar range.
                               sprintf('%d is out of the valid range', year)),
                       # if false:
                       # assign string reporting error that input value of function is not a number. 
                       'error: argument of class numeric expected')
     # return result of iselse statements.
     return(result)
}

