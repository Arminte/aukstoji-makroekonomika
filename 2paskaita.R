 library(Hmisc)
library(zoo)
polinfl <- read.delim("C:/Users/globy/Desktop/aukstoji makro/macro_02/polinfl.txt")
attach(polinfl)
#suteikiam laiko eilutes atributus
pi.a <- ts(pi.a,start = c(2004,1), frequency = 12)
pi.p <- ts(pi.p,start = c(2004,1), frequency = 12)

ts.plot(pi.a)

lines(pi.p, col="blue")
#melyna linija - vartotoju spejimai, juoda - faktiniai

#slankus vidurkis faktinei infliacijai, 12 eiles, left kad priskirtu sausiui o ne liepai pvz 
Pi.a <- rollmean(pi.a, 12, allign="left")

#sutrumpinam pi.p
Pi.p <- window(pi.p, end = c(2015, 7))

ts.plot(Pi.a)
lines(Pi.p,col="blue")

#ivetinam ar prognozinis viertis nera paslinktas
mod1 <- lm(Pi.a ~ Pi.p)
summary(mod1)
#intercept nereiksmingas
(coef(mod1)[2]-1)/diag(vcov(mod1))[2]^0.5
qt(0.975,137)


acf(residuals(mod1))
pacf(residuals(mod1))
#pirmos eiles autoregresija

#progonozes paklaidos kintamasis
Pi.e <- Pi.a - Pi.p

pi.actual <- window(pi.a, end = c(2015, 7))

lm(Pi.e ~ Lag(pi.actual,1))

#NAMU DARBAS

library(Hmisc)
library(zoo)
gerinfl <- read.delim("C:/Users/globy/Desktop/aukstoji makro/macro_02/gerinfl.txt")
attach(gerinfl)
#suteikiam laiko eilutes atributus
infl <- ts(infl,start = 1991)
p.infl <- ts(p.infl,start = 1991)

ts.plot(infl)

lines(p.infl, col="blue")

mod1 <- lm(infl ~ p.infl)
summary(mod1)
#tikrinam nuline hipoteze, ar beta2 yra vienas, apskaiciuojam t=beta2-1/se(beta2)
beta2 <- summary(mod1)$coefficients[2, 1]
se.beta2 <- summary(mod1)$coefficients[2, 2]
(beta2 - 1)/se.beta2
qt(0.975,15)
#kai apskaiciuotos t modulis mazesnis uz kritine t statistika (qt), nuline hipoteze neatmetama, vadinasi pirma salyga patenkinta
res1 <- residuals(mod1)
acf(res1)
#nera paklaidu autokoreliacijos

#tikrinam ar paklaidos nekoreliuoja su informaicja, prieinama prognozes formavimo metu (svarbi prognozes metu)  - pvz faktines infliacijos velavimu
infl.e <- infl - p.infl
#modelis prognozes paklaidos priklausomybe nuo faktines infliacijos velavimo
mod2 <- lm(infl.e ~ Lag(infl, 1))
summary(mod2)
#nekoreliuoja su velavimu, nes beta2 nereiksmingas, t statistika -0,074
res2 <- residuals(mod2)
acf(res2)
#paklaidos nera autokoreliuotos
acf(infl.e)
#panasios autokorelogramos
pacf(infl.e)
#tikrinam ar paklaidos nera autokoreliuotos
mod3 <- lm(infl.e ~ Lag(infl.e, 1))
summary(mod3)
res3 <- residuals(mod3)
acf(res3)
#nera paklaidu autokoreliacijos, nera inertiskumo, 