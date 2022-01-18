# Ribinis darbo "nenaudingumas" 
g <- seq(0.5, 2.5, 0.25)
round(100^(g - 1), 2)
round(200^(g - 1), 2)  
# Dixito-Stiglitzo vartojimo indeksas
cons <- c(1, 2, 3, 4)
eta <- 2.5
eta/(eta - 1)
(eta - 1)  /eta
sum(cons)
sum(cons^((eta - 1)/eta))^(eta/(eta - 1))
# Faktin? gamyba
eta <- 12.5
gama <- seq(1.1, 1.9, 0.1)
gama - 1
#uzimtumo elastingumas darbo uzmmokesciui
1/(gama - 1)
#marza
markup <- eta/(eta - 1)
#pusiausvyros gambyos apimtys
round((1/markup)^(1/(gama - 1)), 4)
# i-tosios prek?s kainos lygties koeficientai
gama <- 1.75
phi <- gama - 1
#visumines paklausos poveikis kainu lygiui
phi
#bendro kainu lygio poveikis
1 - phi
# Paskatos keisti kainas, reaguojant ? paklausos ?okus
eta <- 12.5
#antkainis (marza)
mu <- eta/(eta - 1)
mu
g <- seq(1.1, 1.9, 0.1)
y <- (1/mu)^(1/(g - 1))
#3 proc neigiamas pasiulos sokas
y.act <- y*0.97
#pelnas,, jei kainas perizurime
pi.a <- (mu - 1)*mu^-eta*y.act^(g-eta*g+eta)
#pelnas, jei kainu nepeerziurime
pi.f <- y.act - y.act^g  
round(cbind(y, y.act, g, g - 1, 1/(g - 1), pi.a, pi.f, pi.a - pi.f), 4)
 
