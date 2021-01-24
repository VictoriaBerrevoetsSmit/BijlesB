t.data.2 <- seq(1, 50)
C.data.2 <- (200*exp(-0.2*t.data.2) - 500*exp(-t.data.2)) + rnorm(length(t.data.2), sd=2)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

data
attach(data)
plot(t, C, type='l')
detach(data)

plot(data$t, data$C, type='l')



(nls(formula = C~a*exp(-b*t)+c*exp(-d*t), data=data,
    start = list(a = 120, b = 0.1, c = -200, d = 1)))
predict(nls(formula = C~a*exp(-b*t)+c*exp(-d*t), data=data,
            start = list(a = 120, b = 0.1, c = -200, d = 1)))


lines(data$t, b(nls(formula = C~a*exp(-b*t)+c*exp(-d*t), data=data,
                          start = list(a = 120, b = 0.1, c = -200, d = 1))), col="red")
