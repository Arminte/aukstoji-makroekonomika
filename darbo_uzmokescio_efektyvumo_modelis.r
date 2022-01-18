Beta <- 0.03
b <- 0.5
Beta/b
u <- 0.09
c.f <- ((1 - b*u)/(b*u))^Beta
c.a <- 1/Beta^Beta*1/(1 - Beta)^(1 - Beta)*(1 - b*u)
c(c.f, c.a)