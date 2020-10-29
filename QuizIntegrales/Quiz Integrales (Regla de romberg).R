####################
# Regla de romberg #
####################

require(pracma)

f <- function(x) sin(x)/x


tol <- 1e-16
a <- 0
b <- 1
maxit = 25

eps <- .Machine$double.eps

if (!is.finite(f(a))) 
  a <- a + eps * sign(b - a)

if (!is.finite(f(b))) 
  b <- b - eps * sign

I <- matrix(0, nrow = maxit + 1, ncol = maxit + 1)
n <- 1
iter <- 0
err <- 1

while (err > tol) 
{
  iter <- iter + 1
  n <- 2 * n
  h <- (b - a)/n
  S <- f(a)
  for (i in 1:(n - 1)) 
  {
    xi <- a + h * i
    S <- S + 2 * f(xi)
  }
  S <- (S + f(b)) * h/2
  I[iter + 1, 1] <- S
  for (k in 2:(iter + 1)) 
  {
    j <- 2 + iter - k
    I[j, k] <- (4^(k - 1) * I[j + 1, k - 1] - I[j, k - 1])/(4^(k - 1) - 1)
  }
  err <- abs(I[1, iter + 1] - I[2, iter])
}

value = I[1, iter + 1]

value
err
iter
