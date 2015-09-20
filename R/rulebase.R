holidaySTOCKHOLM <- function (year = getRmetricsOptions("currentYear")) {
  
  holidays <- c(NewYearsDay(year), 
                Epiphany(year), GoodFriday(year), EasterMonday(year), 
                LaborDay(year), 
                Ascension(year), 
                SEMidsummerEve(year), 
                ChristmasEve(year), ChristmasDay(year), BoxingDay(year), 
                SENewYearsEve(year))
  
  if ( year < 2005) {
    holidays <- c(holidays, PentecostMonday(year))
  }
  else {
    holidays <- c(holidays, SENationalDay(year))
  }
  
  holidays <- sort(holidays)
  holidays <- timeDate(format(holidays), zone = "Stockholm", FinCenter = "Stockholm")
  
  holidays
}

SEMidsummerEve <- function (year = getRmetricsOptions("currentYear")) {
  timeNdayOnOrBefore(timeDate(as.character(year * 10000 + 625)), nday = 5)
}

SENationalDay <- function (year = getRmetricsOptions("currentYear")) {
  timeDate(as.character(year * 10000 + 606))
}

SENewYearsEve <- function (year = getRmetricsOptions("currentYear")) {
  timeDate(as.character(year * 10000 + 1231))
}
