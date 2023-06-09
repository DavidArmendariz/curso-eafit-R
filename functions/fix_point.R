# Example
# x0 = approximation of an x near the zero of the function
# f = function(x) { return(sqrt(x)-x) }
# phi = function(x) { return(sqrt(x)) }
# fix_point(x0 = 0.6, tolerance = 10^-5, nmax = 100, f = f, phi = phi)

fix_point <- function(x0, tolerance, nmax, f, phi) {
  err <- tolerance + 1
  number_of_iterations <- 0
  xvect <- x0
  x <- x0
  fx <- f(x)
  xdif <- c()
  fx <- f(x)
  while (number_of_iterations < nmax && err > tolerance) {
    number_of_iterations <- number_of_iterations + 1
    x <- xvect[number_of_iterations]
    xn <- phi(x)
    err <- abs(xn - x)
    xdif <- c(xdif, err)
    x <- xn
    xvect <- c(xvect, x)
    fx <- c(fx, f(x))
  }
  result <- list(xvect = xvect, xdif = xdif, fx = fx, number_of_iterations = number_of_iterations)
  return(result)
}
