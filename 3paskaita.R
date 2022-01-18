library(readxl)
lt_labour <- read_excel("C:/Users/globy/Desktop/aukstoji makro/macro_03/lt_labour.xlsx",
                        sheet="data")
attach(lt_labour)
INFL <- ts(INFL,start=c(1998,1),frequency = 4)
U <- ts(U,start=c(1998,1),frequency = 4)
Y <- ts(Y,start=c(1998,1),frequency = 4)
library(mFilter)
#y logoratimuojam ir padaudinam is 100, nes U ir INFL jau yra procentais
y <- log(Y)*100
#potencialus bvp naudojant hodrick prescott filtra
y.pot <- hpfilter(y,freq = 1600)$trend
ts.plot(y)
lines(y.pot,col="blue")
#gamybos spraga
y.gap <- y - y.pot
ts.plot(y.gap)
acf(y.gap)
pacf(y.gap)
#panasu, kad nestaciorus, atsitiktinio klaidziojimo procesas, nes acf
#laipsniskai gestantis o pacf ne tik pirmos eiles, o daugiau

acf(INFL)
pacf(INFL)
#itraukti reiketu 1,2,4,5 laga

library(Hmisc)
library(lmtest)
ar.infl <- arima(INFL,order = c(5,0,0),
                 fixed = c(NA,0,0,NA,NA,NA))
#vietoj summary arima modeliui coeftest
coeftest(ar.infl)
#matome, kad 2 ir 3 nera reiksmingi, juos isimam fixed pagalba
res.ar.infl <- residuals(ar.infl)
acf(res.ar.infl)
ar.infl$aic

#itraukiam nedarbo lygi
plot(U,INFL)
U2 <- U^2
#kryzmine ko reliacija, matome laipsniska gesima, gal reiketu itraukti t-1 periodo reiksmes
ccf(INFL,U)
obs <- length(U)
armax.infl <- arima(INFL[-1],order = c(4,0,2),
                  xreg=cbind(U[-obs],U2[-obs]),
                 fixed = c(0,0,0,NA,NA,NA,NA,NA,NA))
coeftest(armax.infl)
res.armax.infl <- residuals(armax.infl)
armax.infl$aic

#itraukiam gamybos spraga t-1 

xmat <- cbind(U[-obs],U2[-obs],y.gap[-obs])
      
ccf(c(INFL),c(y.gap))
arx.infl <- arima(INFL[-1],order = c(4,0,0),
                    xreg=xmat,
                    fixed = c(0,0,0,NA,NA,NA,NA,NA))
coeftest(arx.infl)
res.arx.infl <- residuals(arx.infl)
acf(res.arx.infl)
arx.infl$aic

armax.infl2 <- arima(INFL[-1], order = c(4, 0, 10),
                     xreg = xmat,
                     fixed = c(0, 0, NA, NA,
                               rep(0, 9), NA,
                               NA, NA, NA, NA))
coeftest(armax.infl2)
res.armax.infl2 <- residuals(armax.infl2)
acf(res.armax.infl2)
armax.infl2$aic

c2m <- coef(armax.infl2)
C2M <- c2m[c2m!=0]
C2M[5] +2*C2M[6]*(4:18)
#jei nedarbo lygis isaugtu nuo 4 proc.p iki 5 proc.p. infliacijos tempas suletetu 0.41 proc. p. 

# Alternatyvus arx.infl lygties i?matavimas
arx.infl0 <- lm(INFL ~ Lag(INFL, 3) + Lag(INFL, 4) 
                + Lag(U, 1) + Lag(U2, 1) + Lag(y.gap, 1))
summary(arx.infl0)
res.arx.infl0 <- residuals(arx.infl0)
acf(res.arx.infl0)
