#### Bijles 1 #### 

x <- seq(0, 100)
y <- 3*x + 2 + rnorm(sd = 40, 101)

plot(x, y)

lm(y ~ x)
lines(predict(lm(y ~ x)), col = 'red')
