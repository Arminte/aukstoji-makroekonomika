

# Atsitiktinis klaid?iojimas su dreifu
N <- 100
#startinis lygis
x0 <- log(1000)
#delta koeficientas (siejasi su augimo tempu)
d <- log(1.025)
#Epsilium duoda komanda rnomr, ju komuliatyvine suma cumsum
x <- x0 + d*(1:N) + cumsum(rnorm(N, 0, 0.05))
ts.plot(x)
dx <- diff(x)
ts.plot(dx)
acf(dx)


# Atsitiktinis klaid?iojimas
#imtis
n <- 100
y0 <- 5
y <- y0 + cumsum(rnorm(n, 0, 0.5))
ts.plot(y)
dy <- diff(y)
ts.plot(dy)
acf(dy)
