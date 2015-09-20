#' Find year fraction between dates
#'
#' This function calculates the year fraction between two dates, given the day count convention.
#'
#' @param date1 First date.
#' @param date2 Second date.
#' @param convention Day count convention, i.e. "Actual360", "Actual365Fixed", "ActualActual", or "Thirty360".
#' @param ... Extra parameters required by the selected convention, e.g. method = "ISDA" for the "ActualActual" convention.
#'
#' @return Year fraction.
#'
#' @author Carl Johan Rehn <care02@@gmail.com>.
#' 
#' @references  
#'  \itemize{
#'    \item \url{http://www.treasurers.org/daycountconventions}
#'    \item \url{http://en.wikipedia.org/wiki/Day_count_convention}
#'    \item \url{http://www.eclipsesoftware.biz/DayCountConventions.html}
#'  }
#'
#' @seealso \code{\link{Actual360}, \link{Actual365Fixed}, \link{ActualActual}, \link{Thirty360}}.
#'
#' @examples
#'
#' date1 <- timeDate("2012-03-31")
#' date2 <- timeDate("2012-12-11")
#' 
#' convention <- "Actual365Fixed"
#'
#' yearFraction(date1, date2, convention)

yearFraction <- 
  function(date1, date2, convention, ...) { 
    eval(parse(text=convention))(date1, date2, ...)
  }

#' Find number of days between dates.
#'
#' This function counts number of days between two dates.
#'
#' @param date1 First date.
#' @param date2 Second date.
#'
#' @return Day count.
#'
#' @author Carl Johan Rehn <care02@@gmail.com>.
#' 
#' @aliases daysBetween
#'
#' @examples
#'
#' date1 <- timeDate("2012-03-31")
#' date2 <- timeDate("2012-12-11")
#'
#' dayCount(date1, date2)

dayCount <- daysBetween <- 
  function(date1, date2, includeFirst=FALSE, includeLast=TRUE) {
    
#    len1 <- length(date1)
#    len2 <- length(date2)
    
#    nDays <- NULL
    
#    if ( len1 == 1 || len2 == 1 || len1 == len2 ) {
      nDays <- (date2 - date1) / ddays(1)
#    }
    
#    nDays
  }

#' Find year fraction for money market basis (Actual/360).
#'
#' This function finds the day count fraction which is defined as the actual
#' day count over 360.
#'
#' @param date1 First date.
#' @param date2 Second date.
#'
#' @return Year fraction.
#'
#' @author Carl Johan Rehn <care02@@gmail.com>.
#' 
#' @references  
#'  \itemize{
#'    \item \url{http://www.treasurers.org/daycountconventions}
#'    \item \url{http://en.wikipedia.org/wiki/Day_count_convention}
#'    \item \url{http://www.eclipsesoftware.biz/DayCountConventions.html}
#'  }
#'
#' @seealso \code{\link{Actual365Fixed}, \link{ActualActual}, \link{Thirty360}}.
#' 
#' @aliases MoneyMarketBasis
#'
#' @examples
#'
#' date1 <- timeDate("2012-03-31")
#' date2 <- timeDate("2012-12-11")
#'
#' Actual360(date1, date2)

Actual360 <- MoneyMarketBasis <- 
  function(date1, date2, ...) {
    dayCount(date1, date2) / 360
  }

#' Find year fraction for Actual/365 Fixed.
#'
#' This function finds the year fraction which is defined as the actual
#' day count over 365.
#'
#' @param date1 First date.
#' @param date2 Second date.
#'
#' @return Year fraction.
#'
#' @author Carl Johan Rehn <care02@@gmail.com>.
#' 
#' @references  
#'  \itemize{
#'    \item \url{http://www.treasurers.org/daycountconventions}
#'    \item \url{http://en.wikipedia.org/wiki/Day_count_convention}
#'    \item \url{http://www.eclipsesoftware.biz/DayCountConventions.html}
#'  }
#'
#' @seealso \code{\link{Actual360}, \link{ActualActual}, \link{Thirty360}}.
#'
#' @examples
#'
#' date1 <- timeDate("2012-03-31")
#' date2 <- timeDate("2012-12-11")
#'
#' Actual365Fixed(date1, date2)

Actual365Fixed <- 
  function(date1, date2, ...) {
    dayCount(date1, date2) / 365
  }

#' Find year fraction for Actual/Actual (ISDA).
#'
#' The Actual/Actual day count convention can be calculated according to: 
#'    - ISDA convention, also known as "Actual/Actual (Historical)", "Actual/Actual", "Act/Act", and 
#'      according to ISDA also "Actual/365", "Act/365", and "A/365"
#'    - ISMA and US Treasury convention, also known as "Actual/Actual (Bond)"
#'    - AFB convention, also known as "Actual/Actual (Euro)".
#'
#'  Observe that only the ISDA convention is implemented in this version of ActualActual.
#'
#' @param date1 First date.
#' @param date2 Second date.
#' @param method "ISDA" (default), "ICMA" (not yet implemented), or "AFB" (not yet implemented).
#'
#' @return Day count fraction.
#' 
#' @author Carl Johan Rehn <care02@@gmail.com>.
#' 
#' @references  
#'  \itemize{
#'    \item \url{http://www.treasurers.org/daycountconventions}
#'    \item \url{http://en.wikipedia.org/wiki/Day_count_convention}
#'    \item \url{http://www.eclipsesoftware.biz/DayCountConventions.html}
#'  }
#'
#' @seealso \code{\link{Actual360}, \link{Actual365Fixed}, \link{Thirty360}}.
#'
#' @aliases Actual365
#'
#' @examples
#'
#'  date1 <- timeDate("2012-03-31")
#'  date2 <- timeSequence(from="2012-03-31", to="2012-12-11", by="month")
#'
#'  ActualActual(date1, date2)

ActualActual <- Actual365 <- 
  function(date1, date2, method = "ISDA") {
    eval(parse(text=method))(date1, date2)
  }

#' The ISDA day count convention.
#
# Description: 
#   The ISDA day count convention, also known as Actual/Actual (Historical),
#   Actual/Actual, Act/Act, and according to ISDA also Actual/365, Act/365,
#   and A/365.
#
# Arguments:
#   date1 : First date.
#   date2 : Second date.
#
# Returns:
#   Day count fraction.
#
# Example:
#   Suppose we have the following two dates:
#
#   date1 <- timeDate("2012-03-31")
#   date2 <- timeSequence(from="2012-03-31", to="2012-12-11", by="month")
#
#   Now, calculate the number of day count fraction:
#
#   > ISDA(date1, date2)
#   [1] 0.00000000 0.08469945 0.16666667 0.25136612 0.33333333 0.41803279 0.50273224 0.58469945 0.66939891
#
# References:
#   http://www.isda.org/publications/pdf/Day-Count-Fracation1999.pdf
#
#' @author Carl Johan Rehn <care02@@gmail.com>.
#' 
#' @references  
#'  \itemize{
#'    \item \url{http://www.eclipsesoftware.biz/DayCountConventions.html}
#'    \item \url{'http://www.isda.org/c_and_a/pdf/ACT-ACT-ISDA-1999.pdf}
#'  }
#'
#' @seealso \code{\link{ActualActual}, \link{ICMA}, \link{AFB}}.
#
# Date: 2012-12-18
#
# Change Log:
# YYYY-MM-DD by author. Change log...

ISDA <- 
  function(date1, date2) {
    
    len1 <- length(date1)
    len2 <- length(date2)
    
    fraction <- NULL
    
    if ( len1 == 1 || len2 == 1 || len1 == len2 ) {
      
      year1 <- year(date1)
      year2 <- year(date2)
      
      days1 <- ydays(date1)
      days2 <- ydays(date2)
      
      f0 <- year2 - year1
      f1 <- dayCount(date1, timeCalendar(year1 + 1, 1, 1)) / days1
      f2 <- dayCount(timeCalendar(year2, 1, 1), date2) / days2
      
      fraction <- f0 + f1 + f2 - 1
      
    }
    
    fraction
  }

#' ICMA
#' 
#' Not yet implemented
#' 
#' @author Carl Johan Rehn <care02@@gmail.com>.
#' 
#' @references  
#'  \itemize{
#'    \item \url{http://www.eclipsesoftware.biz/DayCountConventions.html}
#'    \item \url{'http://www.isda.org/c_and_a/pdf/ACT-ACT-ISDA-1999.pdf}
#'  }
#'
#' @seealso \code{\link{ActualActual}, \link{ISDA}, \link{AFB}}.

ICMA <- 
  function(date1, date2) { 
    NULL 
  }

#' AFB
#' 
#' Not yet implemented
#' 
#' @author Carl Johan Rehn <care02@@gmail.com>.
#' 
#' @references  
#'  \itemize{
#'    \item \url{http://www.eclipsesoftware.biz/DayCountConventions.html}
#'    \item \url{'http://www.isda.org/c_and_a/pdf/ACT-ACT-ISDA-1999.pdf}
#'  }
#'
#' @seealso \code{\link{ActualActual}, \link{ISDA}, \link{ICMA}}.

AFB <- 
  function(date1, date2) { 
    NULL 
  }

#' The 30/360 day count convention
#'
#' An implementation of the 30/360 day count convention.
#'
#' The 30/360 day count can be calculated according to EU or US conventions.
#' 
#' @param date1 First date.
#' @param date2 Second date.
#' @param method "European" (default), "EurobondBasis", "USA", and "BondBasis".
#' 
#' @return Day count fraction.
#' 
#' @author Carl Johan Rehn <care02@@gmail.com>.
#' 
#' @references  
#'  \itemize{
#'    \item \url{http://www.treasurers.org/daycountconventions}
#'    \item \url{http://en.wikipedia.org/wiki/Day_count_convention}
#'    \item \url{http://www.eclipsesoftware.biz/DayCountConventions.html}
#'  }
#'
#' @seealso \code{\link{Actual360}, \link{Actual365Fixed}, \link{ActualActual}}.
#' 
#' @examples
#'
#' date1 <- timeDate("2012-03-31")
#' date2 <- timeSequence(from="2012-03-31", to="2012-12-11", by="month")
#'
#' Thirty360(date1, date2)
#' Thirty360(date1, date2, "USA")

Thirty360 <- 
  function(date1, date2, method="EU") {
    eval(parse(text=method))(date1, date2)
  }

#' The Eurobond basis (30E/360).
#
# Description: 
#   The Eurobond basis is also known as 30E/360.
#
#   This method assumes that all months have 30 days, even February, 
#   and that a year is 360 days. Effectively, if the start date d1 is 31, 
#   it changes to 30, and if the second date d2 is 31, it too changes to 30.  
#
# Arguments:
#   date1 : First date.
#   date2 : Second date.
#
# Returns:
#   Day count fraction.
#
#'
#' @author Carl Johan Rehn <care02@@gmail.com>.
#' 
#' @references  
#'  \itemize{
#'    \item \url{http://www.treasurers.org/daycountconventions}
#'    \item \url{http://en.wikipedia.org/wiki/Day_count_convention}
#'    \item \url{http://www.eclipsesoftware.biz/DayCountConventions.html}
#'  }
#'
#' @aliases EuroBondBasis
#' 
# Example:
#   Suppose we have the following two dates:
#
#   date1 <- timeDate("2012-03-31")
#   date2 <- timeSequence(from="2012-03-31", to="2012-12-11", by="month")
#
#   Now, calculate the number of day count fraction:
#
#   > EU(date1, date2)
#   [1] 0.00000000 0.08611111 0.16666667 0.25277778 0.33333333 0.41666667 0.50277778 0.58333333 0.66944444

EU <- EuroBondBasis <- 
  function(date1, date2) {
    
    len1 <- length(date1)
    len2 <- length(date2)
    
    fraction <- NULL
    
    if ( len1 == 1 || len2 == 1 || len1 == len2 ) {
      
      year1 <- year(date1)
      year2 <- year(date2)
      
      mon1 <- mon(date1)
      mon2 <- mon(date2)
      
      mday1 <- mday(date1)
      mday2 <- mday(date2)
      
      days <- 360 * ( year2 - year1 ) + 30 * ( mon2 - mon1 - 1 ) + 
        pmax(0, 30 - mday1) + pmin(30, mday2)
      
      fraction <- days / 360
    }
    
    fraction
  }

#' The US bond basis (30/360).
#
# Description: 
#   The US bond basis is also known as 30/360.
#
#   Each month is assumed to have 30 days, with an exception that if the last day is
#   the 31st and the first day is not 30th or 31st, that month has 31 days. 
#   So the rule is if d1 is 31 it changes to 30, and if d2 is 31, change it to 30, 
#   but only if d1 is either 30 or 31.
#
# Arguments:
#   date1 : First date.
#   date2 : Second date.
#
# Returns:
#   Day count fraction.
#
#'
#' @author Carl Johan Rehn <care02@@gmail.com>.
#' 
#' @references  
#'  \itemize{
#'    \item \url{http://www.treasurers.org/daycountconventions}
#'    \item \url{http://en.wikipedia.org/wiki/Day_count_convention}
#'    \item \url{http://www.eclipsesoftware.biz/DayCountConventions.html}
#'  }
#'
#' @aliases BondBasis
#' 
# Example:
#   Suppose we have the following two dates:
#
#   date1 <- timeDate("2012-03-31")
#   date2 <- timeSequence(from="2012-03-31", to="2012-12-11", by="month")
#
#   Now, calculate the number of day count fraction:
#
#   > US(date1, date2)
#   [1] 0.00000000 0.08611111 0.16666667 0.25277778 0.33333333 0.41666667 0.50277778 0.58333333 0.66944444

US <- BondBasis <- 
  function(date1, date2) {
  
    len1 <- length(date1)
    len2 <- length(date2)
    
    fraction <- NULL
    
    if ( len1 == 1 || len2 == 1 || len1 == len2 ) {
      
      year1 <- year(date1)
      year2 <- year(date2)
      
      mon1 <- mon(date1)
      mon2 <- mon(date2)
      
      mday1 <- mday(date1)
      mday2 <- mday(date2)
      
      change <- ( mday1 == 30 | mday1 == 31 ) & mday2 == 31
      
      mday2[change] <- 30
      
      days <- 360 * ( year2 - year1 ) + 30 * ( mon2 - mon1 - 1 ) + 
        pmax(0, 30 - mday1) + mday2
      
      fraction <- days / 360
    }
    
    fraction
  }

#' FixedCoupon
#' 
#'
#' @author Carl Johan Rehn <care02@@gmail.com>.
#' 
#' @references  
#'  \itemize{
#'    \item \url{http://www.treasurers.org/daycountconventions}
#'  }
#' 

FixedCoupon <- 
  function(frequency=1) {
    1 / frequency
  }

#' Find number of business days between dates.
#
# Description: This function counts number of business days between two
# dates.
#
# Arguments:
# ----------
# date1           : First date.
# date2           : Second date.
# includeFirst    : Include first date number in business daycount.
# includeLast     : Include second date number in business daycount.
# holidayCalendar : Holiday calendar, e.g. holidaySTOCKHOLM().
#
# Returns:
# ----------
# nBusinessDays: Number of business days.
#
# Example:
# ----------
# dateNumber1 = datenum( 2010, 3, 1 ) + ( 1:31 )'
# dateNumber2 = datenum( 2010, 3, 11 ) + ( 1:31 )'
# calendarName = 'UnitedStates'
# marketName = 'NYSE'
# nBusinessDays = businessDaysBetween(dateNumber1, dateNumber2, false, true, calendarName, marketName)
# 
# nBusinessDays = businessDaysBetween(dateNumber1, dateNumber2, false, true, 'Norway')
# 
# nBusinessDays = businessDaysBetween(dateNumber1, dateNumber2, false, true)
#'
#' @aliases bizDaysBetween
#'
#' @author Carl Johan Rehn <care02@@gmail.com>.

bizDayCount <- bizDaysBetween <- 
  function(date1, date2, includeFirst=TRUE, includeLast=TRUE, holidayCalendar) {

    
# ToDo...
#
#     len1 <- length(date1)
#     len2 <- length(date2)
#     
#     if ( len1 == 1 || len2 == 1 || len1 == len2 ) {
#???    len = max()
      
    nBusinessDays <- c()
    
    for ( iDate in seq(1, len1) ) {
      
      tS <- timeSequence(date1[iDate], date2[iDate])
      
      end <- length(tS)
      
      if ( !includeFirst )
        tS = tS[2:end]
      
      if ( !includeLast )
        tS = tS[1:end-1]
      
      nBusinessDays <- c(nBusinessDays, length(tS[isBizday(tS, holidayCalendar)]))
    }
  
    nBusinessDays
  }

#' Business252
#' 
Business252 <- 
  function(date1, date2) {
    bizDayCount(date1, date2, FALSE) / 252
  }


