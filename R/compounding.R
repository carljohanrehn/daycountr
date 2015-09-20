#' The discount-to-rate function
#'
#' Return the discount-to-rate function.
#'
#' @param compounding Choice of compounding method, 
#' i.e. "continuous", "discrete", "annual", or "simple".
#' @param frequency Compounding frequency (discrete compounding).
#' 
#' @details 
#' The discount-to-rate function is defined as follows 
#' \code{discountToRate <- function(times, discountFactors) { ... }}
#' where \code{times} and rates are vectors.  
#' 
#' @return The discount-to-rate function.
#'
# @note 
#'
#' @author Carl Johan Rehn \email{care02@@gmail.com}.
#' 
#' @references
#' \url{http://www.treasurers.org/daycountconventions}.
#'
#' @seealso \code{\link{rateToDiscount}}.
#' 
#' @aliases discount2rate
#' 
#' @export
#' 
#' @examples
#'
#' times <- seq(1, 8, 2)
#' discountFactors <- c(0.99, 0.96, 0.93, 0.90)
#'
#' rates <- discountToRate("continuous")
#' 
#' r <- rates(times, discountFactors)

discountToRate <- discount2rate <- function(compounding = "continuous", frequency = 1, ...) {
  if ( compounding == "continuous" )
    function(times, discountFactors) { 
      -log(discountFactors) / times 
    }
  else if ( compounding == "discrete" )
    function(times, discountFactors) { 
      frequency * ( (1 / discountFactors) ^ ( 1 / (times * frequency) ) - 1 )
    }
  else if ( compounding == "annual" )
    function(times, discountFactors) { 
      (1 / discountFactors) ^ ( 1 / times ) - 1 
    }
  else if ( compounding == "simple" )                
    function(times, discountFactors) { 
      (1 / discountFactors - 1) / times 
    }
  else
    function(times, discountFactors, ...) {
      NULL
    }
}

#' The rate-to-discount function
#'
#' Return the rate-to-discount function.
#'
#' @param compounding Choice of compounding method, 
#' i.e. "continuous", "discrete", "annual", or "simple".
#' @param frequency Compounding frequency (discrete compounding).
#' 
# @details
#'
#' @return The rate-to-discount function.
#'
# @note
#'
#' @author Carl Johan Rehn \email{care02@@gmail.com}.
#' 
#' @references
#' \url{http://www.treasurers.org/daycountconventions}.
#'
#' @seealso \code{\link{discountToRate}}.
#' 
#' @aliases rate2discount
#'
#' @examples 
#' 
#' times <- seq(1, 8, 2)
#' rates <- c(0.01005034, 0.01360733, 0.01451414, 0.01505150)
#' 
#' discountFactors <- rateToDiscount("continuous")
#' 
#' d <- discountFactors(times, rates)

rateToDiscount <- rate2discount <- function(compounding = "continuous", frequency = 1, ...) {  
  if ( compounding == "continuous" )
    function(times, rates) {
      exp(-rates * times)
    }
  else if ( compounding == "discrete" )
    function(times, rates) {
      1 / (1 + rates / frequency) ^ (times * frequency)
    }
  else if ( compounding == "annual" )
    function(times, rates) {
      1 / (1 + rates) ^ times
    }
  else if ( compounding == "simple" )
    function(times, rates) {
      1 / (1 + rates * times)
    }
  else
    function(times, rates, ...) {
      NULL
    }
}

#' Convert rates from continuous to discrete compounding
#' 
#' @seealso \code{\link{discreteToContinuous}, \link{discreteToDiscrete}}.
#' 
#' @aliases continuous2discrete
#'
#' @examples 
#' 
#' rates <- c(0.01005034, 0.01360733, 0.01451414, 0.01505150)
#' 
#' discreteRates <- continuousToDiscrete(rates)

continuousToDiscrete <- continuous2discrete <- function(rates, frequency = 1, ...) { 
  frequency * ( exp(rates / frequency) - 1 ) 
}

#' Convert rates from discrete to continuous compounding
#' 
#' @seealso \code{\link{continuousToDiscrete}, \link{discreteToDiscrete}}.
#' 
#' @aliases discrete2continuous
#'
#' @examples 
#' 
#' discreteRates <- c(0.01010101 0.01370033 0.01461998 0.01516534)
#' 
#' rates <- discreteToContinuous(discreteRates)

discreteToContinuous <- discrete2continuous <- function(rates, frequency = 1, ...) { 
  frequency * ( log(1 + rates / frequency) ) 
}

#' Convert rates from discrete to discrete compounding with different frequencies
#' 
#' @seealso \code{\link{continuousToDiscrete}, \link{discreteToContinuous}}.
#' 
#' @aliases discrete2discrete
#'
#' @examples 
#' 
#' rates1 <- c(0.01005034, 0.01360733, 0.01451414, 0.01505150)
#' 
#' rates2 <- discreteToDiscrete(rates, 1, 2)

discreteToDiscrete <- discrete2discrete <- function(rates, frequency1 = 1, frequency2 = 1, ...) {
  frequency2 * ( ( 1 + rates / frequency1 ) ^ ( frequency1 / frequency2 ) - 1 )
}


#' conventionToConvention
#' 
#' @aliases convention2Convention
#'

conventionToConvention <- convention2Convention <- 
  function(rate, date1, date2, from = "ActualActual", to = "Actual365Fixed", ...) {
    fromFraction <- eval(parse(text = from))(date1, date2, ...)
    toFraction <- eval(parse(text = to))(date1, date2, ...)
    rate * fromFraction / toFraction
  }


#' The present value function
#' 
#' Return the present value function.
#'
#' @param compounding Choice of compounding method, 
#' i.e. "continuous", "discrete", "annual", or "simple".
#' @param frequency Compounding frequency (discrete compounding).
#' 
# @details
#'
#' @return The present value function.
#'
# @note
#'
#' @author Carl Johan Rehn \email{care02@@gmail.com}.
#' 
#' @references
#' \url{http://www.treasurers.org/daycountconventions}.
#'
#' @seealso \code{\link{discountToRate}, \link{rateToDiscount}}.
#' 
#' @aliases pv
#'
#' @examples
#'
#' cashflowTimes <- seq(1, 8, 2)
#' cashflow <- c(5, 5, 5, 105)
#' 
#' rates <- log(seq(0, 3) + 1) / 50 + 0.0175
#'
#' f <- presentValue("continuous")
#'
#' v <- f(cashflowTimes, cashflow, rates)

presentValue <- pv <- function(compounding = "continuous", frequency = 1, ...) {
  function(cashflowTimes, cashflow, rates, frequency, ...) {
    discountFactors <- rateToDiscount(compounding)(cashflowTimes, rates, frequency, ...)
    sum(cashflow * discountFactors)
  }
}

