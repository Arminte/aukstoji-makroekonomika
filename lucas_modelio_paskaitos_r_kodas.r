g <- seq(1.1, 1.9, 0.1)
#uzimtumo elastignumas realiam darbo uzmokesciui
el <- 1/(g - 1)
#realaus du elastingumas uzimtumui
g - 1
#dispersija
var.rate <- 0.25
b <- el*var.rate
#fefktas gamybai
ef.prod <- b/(1 + b)
#efektas kainoms
ef.prices <- 1/(1 + b)
cbind(g, g - 1, el, ef.prod, ef.prices) 