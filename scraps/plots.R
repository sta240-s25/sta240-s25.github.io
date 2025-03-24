# ==============================================================================
# CDF vs PDF
# ==============================================================================

par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))

xstar = 0.5

curve(pnorm(x), from = -3, to = 3, n = 500)
abline(v = xstar)
abline()
curve(dnorm(x), from = -3, to = 3, n = 500)
abline(v = xstar)

# ==============================================================================
# plot a gamma
# ==============================================================================

n <- 5000
alpha = 25
beta = 0.0025
X <- rgamma(n, shape = alpha, rate = beta)
hist(X, breaks = "Scott", freq = FALSE,
     main = "Distribution of a single claim amount")
curve(dgamma(x, shape = alpha, rate = beta),
      from = min(X),
      to = max(X),
      lwd = 2,
      col = "red", 
      n = 500,
      add = TRUE)

mean(X)
sd(X)

1 - pgamma(15000, shape = alpha, rate = beta)
mean(X > 150000)

# ==============================================================================
# chi-squared
# ==============================================================================

simulate_total_claim <- function(){
  N = rpois(1, lambda = 100)
  X = rgamma(N, shape = 25, rate = 0.0025)
  S = sum(X)
  return(S)
}

S = replicate(5000, simulate_total_claim())
hist(S, freq = FALSE, breaks = "Scott")

mean(S > 130000)
mean(S > 1300000)

mean(S)
var(S)

# ==============================================================================
# chi-squared
# ==============================================================================

n <- 5000
Z <- rnorm(n)
Y <- Z^2
hist(Y, breaks = "Scott", freq = FALSE)
curve(dgamma(x, shape = 1/2, rate = 1/2),
      from = 0,
      to = 10,
      lwd = 2,
      col = "red", 
      n = 500,
      add = TRUE)

# ==============================================================================
# transformation practice
# ==============================================================================

X = rgamma(5000, 1, 1)
Y = log(X)
hist(Y, breaks = "Scott", freq = FALSE)
curve(exp(x - exp(x)),
      from = -15,
      to = 5,
      lwd = 2,
      col = "red",
      add = TRUE)

# ==============================================================================
# different distributions, same raw moments.
# ==============================================================================

x = seq(0, 5, length.out = 500)
f1 = dlnorm(x, meanlog = 0, sdlog = 1)
f2 = f1 * (1 + sin(2*pi*log(x)))
plot(x, f1, type = "l", ylim = c(0, 1.5),
     main = "These distributions have the same raw moments",
     ylab = "")
lines(x, f2, col = "red")
legend("topright",
       c(expression(f[1]), expression(f[2])),
       lty = c(1, 1), 
       col = c("black", "red"),
       bty = "n")

# ==============================================================================
# N(0, 1) versus Cauchy
# ==============================================================================

par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))

curve(dnorm(x), from = -5, to = 5, lwd = 2, ylab = "density", n = 500)
curve(dt(x, df = 1), from = -5, to = 5, add = TRUE, col = "red", lwd = 2, n = 500)
legend("topright", c("pdf of N(0, 1)", "pdf of Cauchy"), 
       lty = 1, lwd = 2, col = c("black", "red"), bty = "n")

# ==============================================================================
# plot discrete pmf and cdf side-by-side
# ==============================================================================

discrete_pmf <- function(x, p, ylim, xlim = c(min(x) - 1, max(x) + 1), label = ""){
  plot(x, p,
       pch = 19,
       cex = 0.25,
       xlab = "",
       ylab = "",
       main = "",
       ylim = ylim,
       yaxs = "i",
       yaxt = "n",
       xlim = xlim,
       xaxt = "n",
       bty = "n"
  )
  segments(x,
           rep(0, length(x) + 1),
           x1 = x,
           y1 = p,
           lwd = 3
  )
  axis(1, at = floor(xlim[1]):ceiling(xlim[2]), cex.axis = 1)
  axis(2, at = seq(0, 1, length.out = 11), las = 1, cex.axis = 1)
  legend("topright", label, bty = "n", cex = 3)
}

discrete_cdf <- function(x, p, xlim = c(min(x) - 1, max(x) + 1), label = ""){
  closeddot = cumsum(p)
  opencircle = c(0, closeddot[1:length(x)-1])
  plot(x, closeddot, pch = 19, cex = 0.75,
       ylim = c(0, 1),
       ylab = "", main = "", xlab = "",
       yaxt = "n",
       xlim = xlim,
       xaxt = "n",
       #yaxs = "i", 
       bty = "n")
  points(x, opencircle, cex = 0.75)
  segments(c(xlim[1], x), c(0, closeddot), c(x, xlim[2]), c(0, closeddot), lwd = 1)
  axis(1, at = floor(xlim[1]):ceiling(xlim[2]), cex.axis = 1)
  axis(2, at = seq(0, 1, length.out = 11), las = 1, cex.axis = 1)
  legend("bottomright", label, bty = "n", cex = 3)
}

x <- 0:10
p <- dbinom(x, max(x), prob = 0.5)

par(mfrow = c(1, 2), mar = c(2, 4, 2, 1))

discrete_pmf(x, p, c(0, 0.3))
discrete_cdf(x, p)

# ==============================================================================
# plot the Cantor distribution
# https://stats.stackexchange.com/questions/229556/how-to-sample-from-cantor-distribution
# ==============================================================================

binary.to.ternary <- function(x) {
  y <- 0
  x <- round(2^52 * x)
  for (i in 1:52) {
    y <- y + 2*(x %% 2)
    y <- y/3
    x <- floor(x/2)
  }
  y
}

n <- 50000
x <- runif(n)
y <- binary.to.ternary(x)
plot(ecdf(y), pch = ".")

# ==============================================================================
# plot 3 x 3 CDFs
# ==============================================================================

cartoon_continuous_cdf <- function(x, p, lwd = 2){
  plot(x, p, 
       type = "l",
       xaxt = "n", 
       yaxt = "n", 
       bty = "n", 
       col = "red", 
       ylim = c(0, 1),
       lwd = lwd)
  abline(h = c(0, 1), lwd = lwd)
  mtext(c("0", "1"), side = 2, at = c(0, 1), las = 1, line = 1)
  lines(x, p, col = "red", lwd = lwd)
}

cartoon_discrete_cdf <- function(x, p, xlim = c(min(x) - 1, max(x) + 1), lwd = 2, label = ""){
  closeddot = cumsum(p)
  opencircle = c(0, closeddot[1:length(x)-1])
  plot(x, closeddot, pch = 19, cex = 0.75,
       ylim = c(0, 1),
       ylab = "", main = "", xlab = "",
       yaxt = "n",
       xlim = xlim,
       xaxt = "n",
       col = "white",
       #yaxs = "i", 
       bty = "n")
  #points(x, opencircle, cex = 0.75)
  abline(h = c(0, 1), lwd = lwd)
  mtext(c("0", "1"), side = 2, at = c(0, 1), las = 1, line = 1)
  segments(c(xlim[1], x), c(0, closeddot), c(x, xlim[2]), c(0, closeddot), 
           lwd = lwd, col = "red")
  #axis(1, at = floor(xlim[1]):ceiling(xlim[2]), cex.axis = 1)
  #axis(2, at = seq(0, 1, length.out = 11), las = 1, cex.axis = 1)
  #legend("bottomright", label, bty = "n", cex = 3)
}

par(mfrow = c(3, 3), mar = c(2, 2, 1, 1))

cdf_lwd = 2

# GAUSSIAN

x = seq(-3, 3, length.out = 2500)
y = pnorm(x)
cartoon_continuous_cdf(x, y)

# CANTOR

binary.to.ternary <- function(x) {
  y <- 0
  x <- round(2^52 * x)
  for (i in 1:52) {
    y <- y + 2*(x %% 2)
    y <- y/3
    x <- floor(x/2)
  }
  y
}

n <- 50000
x <- runif(n)
y <- binary.to.ternary(x)
p <- ecdf(y)
cartoon_continuous_cdf(sort(x), p(sort(x)))

# GAMMA

x = seq(-1, 4, length.out = 2500)
y = pgamma(x, shape = 1, rate = 1)
cartoon_continuous_cdf(x, y)

# GAUSSIAN MIXTURE

x = seq(-10, 10, length.out = 2500)
y = 0.2 * pnorm(x, mean = 6) + 0.5 * pnorm(x) + 0.3 * pnorm(x, mean = -7)
cartoon_continuous_cdf(x, y)


# piecewise linear

plot(0, xlim = c(-5, 5), ylim = c(0, 1),
     xaxt = "n", yaxt = "n", bty = "n", col = "white")
abline(h = c(0, 1), lwd = 1.5)
mtext(c("0", "1"), side = 2, at = c(0, 1), las = 1, line = 1)
segments(c(-5, -4, 0, 2), 
         c(0, 0.2, 0.3, 0.85), 
         c(-4, 0, 2, 5), 
         c(0.2, 0.3, 0.85, 1), 
         col = "red", lwd = cdf_lwd)


# binom

plot(0, xlim = c(-5, 5), ylim = c(0, 1),
     xaxt = "n", yaxt = "n", bty = "n", col = "white")
abline(h = c(0, 1), lwd = 1.5)
mtext(c("0", "1"), side = 2, at = c(0, 1), las = 1, line = 1)
segments(-5:4, seq(0, 1, length.out = 10), -4:5, seq(0, 1, length.out = 10), col = "red", lwd = cdf_lwd)

# poisson

x = -2:15
p = dpois(x, 6)
cartoon_discrete_cdf(x, p)

# mixed

plot(0, xlim = c(-5, 5), ylim = c(0, 1),
     xaxt = "n", yaxt = "n", bty = "n", col = "white")
abline(h = c(0, 1), lwd = 1.5)
mtext(c("0", "1"), side = 2, at = c(0, 1), las = 1, line = 1)
segments(-1, 0.5, 1, 0.5, col = "red", lwd = cdf_lwd)
lines(seq(-5, -1, length.out = 500), 
      pnorm(seq(-6, -1.75, length.out = 500), mean = -1), col = "red", lwd = cdf_lwd)
lines(seq(1, 5, length.out = 500), 
      pnorm(seq(1.75, 6, length.out = 500), mean = 1), 
      col = "red", lwd = cdf_lwd)


# mixed

plot(0, xlim = c(-5, 5), ylim = c(0, 1),
     xaxt = "n", yaxt = "n", bty = "n", col = "white")
abline(h = c(0, 1), lwd = 1.5)
mtext(c("0", "1"), side = 2, at = c(0, 1), las = 1, line = 1)
segments(-5, 0, -2, 0, col = "red", lwd = cdf_lwd)
lines(seq(-2, 5, length.out = 1000), 
      pnorm(seq(0, 3, length.out = 1000)), 
      col = "red", lwd = cdf_lwd)