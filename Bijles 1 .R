#### Bijles 1 #### 
library(phaseR)
library(deSolve)

rm(list=ls())
set.seed(18012021)

rep(c(1, 2, 3), each = 10)
seq(1, 100, by = 10)

x <- seq(0, 100)
y <- 3*x + 2 + rnorm(sd = 40, 101)


plot(x, y)

lm(y~x)
summary(lm(y ~ x))
coef(lm(y~x))
coefficients(lm(y~x))

predict(lm(y~x))
lines(predict(lm(y ~ x)), col = 'red')
points(predict(lm(y ~ x), col = "blue"))

plot(predict(lm(y~x)), type = "l")
points(x, y)
## 

D <- 4.1
t <- c(1, 2, 3, 4, 6, 8)
Cp <- c(8.25, 6.46, 4.63, 4.03, 2.11, 1.41)

plot(t, Cp, type="p", pch=16, cex=1.3, col="red",
     xlab="t (in uren)", ylab="C (in microgram/L)",
     main="plasmaconcentratie versus tijd")

fit.1 <- lm(Cp ~ t)
lines(t, predict(fit.1))

#sqrt(Cp) nooit negatief, kwadratische 
#log(Cp), exponentieel

Log.Cp <- log(Cp)
Log.Cp
fit.2 <- lm(log(Cp) ~ t)
fit.2a <- lm(Log.Cp ~ t)
fit.2
lines(t, predict(fit.2))

coef(fit.2)
coefficients(fit.2)
exp(coef(fit.2)[1])
Cp.fit <- exp(coef(fit.2)[1]) * exp(-(-coef(fit.2))[2]*t)
lines(t, Cp.fit)

#### NLS #### 


Cp.nls <- nls(formula = Cp~a*exp(-b*t), start = list(a = 100, b =-2))
Cp.nls
lines(t, predict(Cp.nls), col = "green")

x <- seq(0, 100)
x

x <- list(0, c(1, 2, 3))
x
x[[2]][3]

# COMMENTEN: WAT DOE JE NOU EIGENLIJK
naam_function <- function(x=4){
  x
  return(x)
}
### Gradient fields



model <- function(tijd, begintoestand, parameters){
  with(as.list(c(begintoestand, parameters)), {
    dxdt <- a*x - b*x*y
    dydt <- -c*x + d*x*y
    return(list(c(dxdt, dydt)))
  })
}

params <- c(a = 1, b = 1, c = 1, d = 1)
flowField(model, xlim = c(-1, 1.5), ylim = c(-1, 1.5), 
          parameters = params, system = "two.dim", 
          state.names = c("x", "y"), points = 21,
          add = FALSE)

nullclines(model, xlim = c(-1, 1.5), ylim = c(-1,1.5),
           parameters = params, system = "two.dim",
           state.names = c("x", "y"), points = 200,
           col=c("red","green"), lwd = 3, add = TRUE)

trajectory(model, y0 = c(0.5, 0.6), tlim = c(0, 10),
           parameters = params, state.names = c("x", "y"),
           lwd = 2)

trajectory(model, y0 = c(0.5, 0.9), tlim = c(0, 10),
           parameters = params, state.names = c("x", "y"),
           lwd = 2)
