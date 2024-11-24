
# Setting up the plotting space
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))

# Define x-axis values
x <- seq(-6, 6, length.out = 1000)

# Define the functions for Mean, Huber, and Bisquare
mean_rho <- function(u) 0.5 * u^2
mean_psi <- function(u) u
mean_weight <- function(u) rep(1, length(u))

huber_rho <- function(u, k = 1.345) ifelse(abs(u) <= k, 0.5 * u^2, k * (abs(u) - 0.5 * k))
huber_psi <- function(u, k = 1.345) ifelse(abs(u) <= k, u, k * sign(u))
huber_weight <- function(u, k = 1.345) ifelse(abs(u) <= k, 1, k / abs(u))

bisquare_rho <- function(u, k = 4.685) ifelse(abs(u) <= k, k^2 / 6 * (1 - (1 - (u / k)^2)^3), k^2 / 6)
bisquare_psi <- function(u, k = 4.685) ifelse(abs(u) <= k, u * (1 - (u / k)^2)^2, 0)
bisquare_weight <- function(u, k = 4.685) ifelse(abs(u) <= k, (1 - (u / k)^2)^2, 0)

# Plotting
# Row 1: Mean
plot(x, mean_rho(x), type = "l", main = expression(rho[Mean]), xlab = "x", ylab = expression(rho(x)))
plot(x, mean_psi(x), type = "l", main = expression(psi[Mean]), xlab = "x", ylab = expression(psi(x)))
plot(x, mean_weight(x), type = "l", main = expression(w[Mean]), xlab = "x", ylab = expression(w(x)))

# Row 2: Huber
plot(x, huber_rho(x), type = "l", main = expression(rho[Huber]), xlab = "x", ylab = expression(rho(x)))
plot(x, huber_psi(x), type = "l", main = expression(psi[Huber]), xlab = "x", ylab = expression(psi(x)))
plot(x, huber_weight(x), type = "l", main = expression(w[Huber]), xlab = "x", ylab = expression(w(x)))

# Row 3: Bisquare
plot(x, bisquare_rho(x), type = "l", main = expression(rho[Bisquare]), xlab = "x", ylab = expression(rho(x)))
plot(x, bisquare_psi(x), type = "l", main = expression(psi[Bisquare]), xlab = "x", ylab = expression(psi(x)))
plot(x, bisquare_weight(x), type = "l", main = expression(w[Bisquare]), xlab = "x", ylab = expression(w(x)))
