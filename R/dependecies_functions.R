# Climate Dependency Functions

###################################################################################################

# Define `%>%` operator in current environment

`%>%` <- magrittr::`%>%`

###################################################################################################

# Define function to calculate water year

wtr_yr <- function(dates, 
                   start.month) 
{
  dates.posix = as.POSIXlt(dates)
  offset = ifelse(dates.posix$mon >= start.month - 1, 1, 0)
  adj.year = dates.posix$year + 1900 + offset
  adj.year
}

###################################################################################################
