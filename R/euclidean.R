#' Finding Greatest Common Divisor using Euclidean algorithm
#'
#' @description
#' This function calculates the Greatest Common Divisor of two integer numbers
#' using Euclidean algorithm. The algorithm using a series of modulus operation
#' until the remainder is 0.
#'
#' @param a A number.
#' @param b A number.
#' @returns A number, the greatest common divisor of 'a' and 'b'
#'
#' @references
#' Euclidean algorithm on Wikipedia:
#' \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#'
#' @export

euclidean <- function(a, b){
  stopifnot(is.numeric(a), is.atomic(a))
  stopifnot(is.numeric(b), is.atomic(b))

  a <- floor(a)
  b <- floor(b)

  remainder <- 1

  while(b != 0){
    remainder <- a%%b
    a <- b
    b <- remainder
  }
  return(a)
}
