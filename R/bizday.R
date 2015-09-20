#' Business day adjustment
#'
#' Adjusts a non-business day to the appropriate next business day
#' with respect to the given convention. 
#'
#' @param dates Dates to be adjusted.
#' @param convention Business day convention, i.e. "Following", "ModifiedFollowing", 
#' "Preceding", or "ModifiedPreceding".
#' @param ... Expressions evaluated in the context of \code{holidays}, 
#' e.g. \code{holidayNYSE()}, see package \code{timeDate}.
#'
#' @return The adjusted dates.
#' 
# @author Carl Johan Rehn <care02@@gmail.com>.
#'
#' @references 
#' \itemize{
#'   \item \url{http://www.treasurers.org/daycountconventions}
#'   \item \url{http://quantlib.org/index.shtml}
#' }
#' 
#' @examples
#' 
#' dates <- timeSequence(from="2012-03-12", to="2012-04-11")
#' convention <- "Following"
#'
#' adjustBizDay(dates, convention)
#' adjustBizDay(dates, convention, holidayNYSE())
#'
adjustBizDay <- function(dates, convention, ...) {
  advanceBizDay(dates, rep(1, length(dates)), convention, ...)
}

#' Advances dates a given number of business days
#'
#' Advances dates a given number of business days with respect to the given convention. 
#'
#' @param dates Dates to be adjusted.
#' @param nBusinessDays Number of business days to advance.
#' @param convention Business day convention, i.e. "Following", "ModifiedFollowing", 
#' "Preceding", or "ModifiedPreceding".
#' @param ... expressions evaluated in the context of \code{holidays}, 
#' e.g. \code{holidayNYSE()}.
#'
#' @return The advanced dates.
#'
#' @examples 
#' 
#' dates <- timeSequence(from="2012-03-12", to="2012-04-11")
#' convention <- "Following"
#' nBusinessDays <- rep(2, length(dates))
#'
#' advanceBizDay(dates, nBusinessDays, convention)
#' 
#' advanceBizDay(dates, nBusinessDays, convention, holidayNYSE())
#' 
#' @references See \url{http://www.treasurers.org/daycountconventions} and
#' \url{http://quantlib.org/index.shtml}.
#' 
#' @author Carl Johan Rehn <care02@@gmail.com>.

advanceBizDay <- function(dates, nBusinessDays, convention, ...) {
  switch(convention, 
         Following=moveBizDay(dates, nBusinessDays, ...), 
         ModifiedFollowing=modifyBizDay(dates, nBusinessDays, ...), 
         Preceding=moveBizDay(dates, -nBusinessDays, ...), 
         ModifiedPreceding=modifyBizDay(dates, -nBusinessDays, ...), 
         dates)
}

#' Modified following (modified business day)
#'
#' Dates are adjusted to the next good business day unless that day falls in 
#' the next calendar month in which case the date is adjusted to the previous 
#' good business day.
#'
#' @param dates Dates to be adjusted.
#' @param nBusinessDays Number of business days to advance.
#' @param ... expressions evaluated in the context of \code{holidays}, 
#' e.g. \code{holidayNYSE()}.
#'
#' @return The modified dates.
#'
#' @examples 
#' 
#' dates <- timeSequence(from="2012-03-12", to="2012-04-11")
#' nBusinessDays <- rep(2, length(dates))
#'
#' modifyBizDay(dates, nBusinessDays)
#' 
#' modifyBizDay(dates, nBusinessDays, holidayNYSE())
#' 
#' @references See \url{http://www.treasurers.org/daycountconventions} and
#' \url{http://quantlib.org/index.shtml}.
#' 
#' @author Carl Johan Rehn <care02@@gmail.com>.

modifyBizDay <- function(dates, nBusinessDays, ...) {
  modifyDates <- moveBizDay(dates, nBusinessDays, ...)
  nextMonth <- modifyDates > timeLastDayInMonth(dates)
  modifyDates[nextMonth] <- moveBizDay(dates[nextMonth], -nBusinessDays[nextMonth], ...)
  modifyDates
}

#' Following business day (next good business day)
#'
#' Dates are adjusted for weekends and holidays to the next good business day.
#'
#' @param dates Dates to move.
#' @param nBusinessDays Number of business days.
#' @param ... expressions evaluated in the context of \code{holidays}, 
#' e.g. \code{holidayNYSE()}.
#'
#' @return The moved dates.
#'
#' @examples 
#' 
#' dates <- timeSequence(from="2012-03-12", to="2012-04-11")
#' nBusinessDays <- rep(2, length(dates))
#'
#' moveBizDay(dates, nBusinessDays)
#' 
#' moveBizDay(dates, nBusinessDays, holidayNYSE())
#' 
#' @references See \url{http://www.treasurers.org/daycountconventions} and
#' \url{http://quantlib.org/index.shtml}.
#' 
#' @author Carl Johan Rehn <care02@@gmail.com>.

## ... = holidays, e.g. holidayNYSE()

## dates <- timeSequence(from="2012-03-12", to="2012-04-11")
## nBusinessDays <- rep(1, length(dates))
## nBusinessDays <- rep(2, length(dates))
## nBusinessDays <- rep(-1, length(dates))
## nBusinessDays <- rep(-2, length(dates))
## holidays <- holidayZURICH()
## moveBizday(dates, nBusinessDays)
## moveBizday(dates, nBusinessDays, holidayNYSE())

## "2012-01-02" %in% as.character(holidayZURICH(2012:2014))

moveBizDay <- function(dates, nBusinessDays, ...) {
  
  oneDay <- 24 * 60 * 60
  businessDates <- dates + nBusinessDays * oneDay
  moveDay <- sign(nBusinessDays) * oneDay  
  
  anyMove <- TRUE  
  while ( anyMove ) {
    move <- isWeekend(businessDates) | isHoliday(businessDates, ...)
    anyMove <- any(move)
    if ( anyMove )
      businessDates[move] <- businessDates[move] + moveDay[move]  
  } 
  
  businessDates
}

#' End of month (previous good business day)
#' 
#' Dates are adjusted to the last day of the month but if that day is a weekend 
#' or holiday, then it is adjusted backward to the previous good business day.
#' @author Carl Johan Rehn <care02@@gmail.com>.

# convention: "Last"  dates are adjusted to land on last day of the month.
#             "Preceding", dates are adjusted to the last day of the month but if that day
#             is a weekend or holiday, then it is adjusted backward to the previous good business day

endOfMonth <- function(dates, convention="Last", ...) {
  switch(convention, 
         Last=timeLastDayInMonth(dates), 
         Preceding=moveBizDay(timeLastDayInMonth(dates), -1, ...), 
         dates)
}

#' Deposit rollover method
#' 
#' Each date is set so it occurs on the same day of the month as the previous 
#' date. Each date is set to the next good business day but no dates may be 
#' adjusted past the last good business day of the month.
#' @author Carl Johan Rehn <care02@@gmail.com>.

#   Each date is set so it occurs on the same day of the month as the
#   previous date. Each date is set to the next good business day but no
#   dates may be adjusted past the last good business day of the month.

rolloverDay <- function(dates, months, ...) {
  rollover <- as.POSIXlt(dates)  
  rollover$mon <- rollover$mon + months
  adjustBizDay(timeDate(rollover), "ModifiedFollowing", ...)
}

#' settlementDay
#' 
# Settlement T+1 T+3 T+5 -> to yearFraction

settlementDay <- function(valuationDate, nSettlementDays=3, convention="Following", ...) { 
  advanceBizDay(valuationDate + nSettlementDays, convention, ...)
}

#' Two business days prior to third Wednesday of month
#' 
#' Each date is set so it occurs on the same day of the month as the previous 
#' date. Each date is set to the next good business day but no dates may be 
#' adjusted past the last good business day of the month.
#' @author Carl Johan Rehn <care02@@gmail.com>.

## IMM DATES - Third Wednesday of March, June, September and December
## two_business_days_prior_to_third_wednesday_of_month
##dates <- c("2010-03-01", "2010-04-01", "2010-11-01")

## dates <- c("2010-03-01", "2010-04-01", "2010-11-01")

##function [isIMMDay, IMMDays] = isimmday( dateNumber, databaseFilePath )
##
##IMMDays = getimmdays( databaseFilePath );
##isIMMDay = ismember( dateNumber, IMMDays );
##IMMDays = dateNumber( isIMMDay );

IMMDay <- function(dates, ...) {
  moveBizday(timeNthNdayInMonth(timeLastDayInQuarter(dates), nday=3, nth=3), -2, ...)
}

isIMMDay <- function() {NULL}


