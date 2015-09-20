
sec <- function(td) { as.POSIXlt(td)$sec }

min <- function(td) { as.POSIXlt(td)$min }

hour <- function(td) { as.POSIXlt(td)$hour }

mday <- function(td) { as.POSIXlt(td)$mday }

mon <- function(td) { 1 + as.POSIXlt(td)$mon }

year <- function(td) { 1900 + as.POSIXlt(td)$year }

wday <- function(td) { as.POSIXlt(td)$wday }

yday <- function(td) { 1 + as.POSIXlt(td)$yday }

isdst <- function(td) { as.POSIXlt(td)$isdst }

isLeap <- function(td) { y <- year(td); y%%4 == 0 & ( y%%100 != 0 | y%%400 == 0 ) }

ydays <- function(td) { rep(365, length(td)) + as.numeric(isLeap(td)) }

