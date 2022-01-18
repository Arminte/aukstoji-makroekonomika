PRZEMYSL2 <- read.delim("C:/Users/globy/Desktop/aukstoji makro/macro_01/PRZEMYSL2.txt")
library(Hmisc)
attach(PRZEMYSL2)
PROD <- ts(PROD,start=c(2000,1),frequency=4)
PPI <- ts(PPI,start=c(2000,1),frequency=4)
EMPL <- ts(EMPL,start=c(2000,1),frequency=4)
WAGES <- ts(WAGES,start=c(2000,1),frequency=4)
y <- log(PROD)
p <- log(PPI)
l <- log(EMPL)
w <- log(WAGES)
ts.plot(y)
ts.plot(p)
plot.ts(cbind(y, p, l, w))
#nestacionarus kintamieji
#kad butu stacionarus reik paverst augimo tempus
dy <- diff(y)
dp <- diff(p)
dl <- diff(l)
dw <- diff(w) 
plot.ts(cbind(dy, dp, dl, dw))

obs <- length(dy)
#prideta t raide reiskia transformuota funkcija, intercept - laisvasis narys
rss <- function(L) {
dl.t <- dl-L*Lag(dl,1)  
intercept.t <- rep(1-L,obs)
dy.t <- (1-L)*dy
dw.t <- dw - L*Lag(dw,1)
dl.t1 <- Lag(dl,1) - L*Lag(dl,2)
mod <- lm(dl.t ~ intercept.t + dy.t +dw.t + dl.t1 - 1)
deviance(mod)
}
L <- optimize(rss,lower=0.01,upper=0.99)$minimum


dl.t <- dl-L*Lag(dl,1)  
intercept.t <- rep(1-L,obs)
dy.t <- (1-L)*dy
dw.t <- dw - L*Lag(dw,1)
dl.t1 <- Lag(dl,1) - L*Lag(dl,2)
mod <- lm(dl.t ~ intercept.t + dy.t +dw.t + dl.t1 - 1)

summary(mod)
res <- residuals(mod)
acf(res)

#matome, kad tik vienas kintamasis yra reiksmingas pagal t stat, darome modeli t-ajam periodui Y(buvo t+1) 
#todel itrauksime dy pirma velavima, nes naudojam t-1 periodo augimo tempa

rss <- function(L) {
  dl.t <- dl-L*Lag(dl,1)  
  intercept.t <- rep(1-L,obs)
  dy.t <- (1-L)*Lag(dy,1)
  dw.t <- dw - L*Lag(dw,1)
  dl.t1 <- Lag(dl,1) - L*Lag(dl,2)
  mod <- lm(dl.t ~ intercept.t + dy.t +dw.t + dl.t1 - 1)
  deviance(mod)
}
L <- optimize(rss,lower=0.01,upper=0.99)$minimum
L

#Lambda pasikeite

dl.t <- dl-L*Lag(dl,1)  
intercept.t <- rep(1-L,obs)
dy.t <- (1-L)*Lag(dy,1)
dw.t <- dw - L*Lag(dw,1)
dl.t1 <- Lag(dl,1) - L*Lag(dl,2)
mod <- lm(dl.t ~ intercept.t + dy.t +dw.t + dl.t1 - 1)

summary(mod)
acf(dl)
#summary geresne, dl pirma eile autoregresiskas
#jei gamintojai tiketusi 1 proc punkto paspartesiancio gamybos apimciu prieaugio, tai uzimtumo augimo tempas paspartetu 0.33 proc punktu
 
Box.test(res, lag=3, type = "Ljung-Box")

##############################################
############################################# NAMU DARBAI


INVS <- read.delim("C:/Users/globy/Desktop/aukstoji makro/macro_01/INVS.txt")
attach(INVS)

inv <- ts(inv,start=1954,frequency=1)
sal <- ts(sal,start=1954,frequency=1)

ts.plot(inv)
ts.plot(sal)
#matoma kad budinga aiskiai isreiksta augimo tendencija, jie nera stacionarus, kad galetume taikyti arirtmetinio vidurkio formule,
#reikia pakeisti i adityvine forma - logaritmuoti

l.inv <- log(inv)
l.sal <- log(sal)

#susikuriam laga

l.inv1 <- Lag(l.inv,1)

#trendas

t.trend <- 1:length(l.inv)

mod0 <- lm(l.inv~t.trend+l.sal+l.inv1)
summary(mod0)
acf(residuals(mod0))
pacf(residuals(mod0))
#pirmos eiles autoregresinis procesas, vadinas neteisinga modelio dinamika specifikuota
#naujas modelis su sales t-1 itrauktu 
l.sal1 <- Lag(l.sal,1)
mod1 <- lm(l.inv~t.trend+l.sal+l.sal1+l.inv1)
summary(mod1)
acf(residuals(mod1))
pacf(residuals(mod1))
#lambda
l <- coef(mod1)[5]
#delta vienas
d1 <- coef(mod1)[2]/(1-l)
#beta nulinis
b0 <- coef(mod1)[3]/(1-l)
#beta vienas
b1 <- coef(mod1)[4]/(1-l)
#delta nulinis
d0 <- (coef(mod1)[1] -d1*l)/(1-l)

c(d0,d1,b0,b1,l)

#trumpesnis kelias

rss <- function (L) {
  l.inv.t <- l.inv - L*l.inv1
  int.t <- rep(1 - L,length(l.inv))
  t.trend.t <- t.trend - L*(t.trend - 1)
  l.sal.t <- (1-L)*l.sal
  l.sal1.t <- (1-L)*l.sal1
  mod.alt <-lm(l.inv.t ~ int.t + t.trend.t + l.sal.t + l.sal1.t - 1)
  deviance(mod.alt)
}

optimize(rss,lower=0.01, upper = 0.99)$minimum
l.inv.t <- l.inv - L*l.inv1
int.t <- rep(1 - L,length(l.inv))
t.trend.t <- t.trend - L*(t.trend - 1)
l.sal.t <- (1-L)*l.sal
l.sal1.t <- (1-L)*l.sal1
mod.alt <-lm(l.inv.t ~ int.t + t.trend.t + l.sal.t + l.sal1.t - 1)
summary(mod.alt)$coefficients
