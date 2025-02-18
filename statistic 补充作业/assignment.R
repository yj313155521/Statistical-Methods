#4.8.1
x1 = seq(0,100,length.out = 100)
y1 = dgamma(x,shape = 80,scale = 1)
data1 <- data.frame(x1,y1)
library(ggplot2)
ggplot(data = data1, aes(x = x1, y = y1)) + geom_line(color = "red")

#4.117

x = seq(0, 1, length.out = 100)
y = dbeta(x, shape1 = 12, shape2 = 7)
data2 <- data.frame(x, y)
library(ggplot2)
ggplot(data = data2, aes(x = data2[,1], y = data2[,2])) +geom_line(color = "blue")

#4.118

x = seq(0, 1, length.out = 100)
y = dbeta(x, shape1 = 0.3, shape2 = 15)
data2 <- data.frame(x, y)
library(ggplot2)
ggplot(data = data2, aes(x = x, y = y)) +geom_line(color = "yellow")

#10.19

average_x <- 128.6
standard_deviation <- 2.1
sigma <- standard_deviation / sqrt(39)
z <- (average_x - 130) / sigma
z

#10.21
average_y1 <- 1.65
average_y2 <- 1.43
sigma_1 <- 0.26
sigma_2 <- 0.22
n1 <- 30
n2 <- 35
Dy <- average_y1 - average_y2
SE1 <- sigma_1 / sqrt(n1 - 1)
SE2 <- sigma_2 / sqrt(n2 - 1)
standard_error = sqrt(SE1^2 + SE2^2)
z <- (Dy - 0) / standard_error
z

#11.31
X = c(19.1, 38.2, 57.3, 76.2, 95, 114, 131, 150, 170)
Y = c(0.095, 0.174, 0.256, 0.348, 0.429, 0.500, 0.580, 0.651, 0.722)
average_x <- sum(X) / 9
average_y <- sum(Y) / 9
average_X <- rep(average_x, times = 9)
average_Y <- rep(average_y, times = 9)
x <- X - average_X
y <- Y - average_Y
x <- as.matrix(x)
y <- as.matrix(y)
sum_xy <- t(x) %*% y
sum_x_square <- t(x) %*% x
sum_y_square <- t(y) %*% y
r <- sum_xy / sqrt(sum_x_square * sum_y_square)
t <- (r - 0) / sqrt((1 - r^2) / (9 - 2))
t

#11.69 a)

Y <- c(18.5, 22.6, 27.2, 31.2, 33.0, 44.9, 49.4, 35.0)
X <- c(-7, -5, -3, -1, 1, 3, 5, 7)
mean_y <- mean(Y)
mean_x <- mean(X)
mean_X <- rep(mean_x, times = 8)
mean_Y <- rep(mean_y, times = 8)
x <- X - mean_X
y <- Y - mean_Y
vector_xy <- x*y
Sxy <- sum(vector_xy)

x <- as.matrix(x)
Sxx <- t(x) %*% x
beta_1_hat <- Sxy / Sxx
beta_0_hat <- mean_y - beta_1_hat * mean_x
beta_0_hat
beta_1_hat

#11.69 b)
Y <- c(18.5, 22.6, 27.2, 31.2, 33.0, 44.9, 49.4, 35.0)
X <- c(-7, -5, -3, -1, 1, 3, 5, 7)
vector_1 <- rep(1, times = 8)
vector_3 <- X * X
matrix_x <- cbind(vector_1, X, vector_3)
beta_hat <- solve(t(matrix_x) %*% matrix_x) %*% t(matrix_x) %*% Y

beta_hat
#
lexus.data <- data.frame(
  co.ded_year = c(-7, -5, -3, -1, 1, 3, 5, 7),
  sales = c(18.5, 22.6, 27.2, 31.2, 33.0, 44.9, 49.4, 35.0)

)
lexus.regression <- lm(sales~coded_year, data = lexus.data)
summary(lexus.regression)
