# Paketai
library(vars)
#reikalinga hpfilter komandai
library(mFilter)
library(readxl)
###############################
# Lenkijos monetarinis VAR'as #
###############################
# Duomenys
poland_nk_data <- read_excel("C:/Users/globy/Desktop/aukstoji makro/macro_04/poland_nk_data.xlsx", 
                             sheet = "data")
attach(poland_nk_data)
# Kintamieji
#adityvineje formoje
infl <- ts(infl, start = c(1998, 1), frequency = 4)
#tiksline infliacija
t_infl <- ts(t_infl, start = c(1998, 1), frequency = 4)
#multiplikatyvineje formoje
infl_mult <- ts(infl_mult, start = c(1998, 1), frequency = 4)
t_infl_mult <- ts(t_infl_mult, start = c(1998, 1), frequency = 4)
#realus bvp
rgdp <- ts(rgdp, start = c(1998, 1), frequency = 4)
#nominalios palukanu normos pateiktos procentais
r_ref <- ts(r_ref, start = c(1998, 1), frequency = 4)
# Infliacijos nuokrypis nuo tikslin?s infliacijos
infl_gap <- infl - t_infl
ts.plot(infl)
lines(t_infl, col = "blue")
ts.plot(infl_gap)
# Gamybos spraga (gaunasi pakankamai stacionarus)
y_gap <- hpfilter(log(rgdp), freq = 1600)$cycle*100
ts.plot(log(rgdp))
lines(hpfilter(log(rgdp), freq = 1600)$trend, col = "blue")
ts.plot(y_gap)
# Realios pal?kanos (nera stacionarus) 
r_rates <- ((r_ref/100 + 1)/(infl_mult/100 + 1) - 1)*100
ts.plot(r_ref)
lines(r_rates, col = "blue")
# Trij? kintam?j? redukuotos formos VAR'as (is kanoninio modelio)
data_pl <- cbind(infl_gap, y_gap, r_rates)
plot.ts(data_pl)
#informaciniai kriterijaim max eiles 12
VARselect(data_pl, 12)
#eile pagal tai kiek autokoreliuotu yra
var_pl <- VAR(data_pl, p = 5)
acf(residuals(var_pl))
#jungo boxo testas, nuline hipoteze neatmetama, 6 funkcijos autokoreliaijos 6 lage nesukuria, varas tinkamas
Box.test(residuals(var_pl)[, 3], lag = 6, type = "Ljung-Box")
summary(var_pl)
# Trij? kintam?j? A tipo strukt?rinis VAR'as
#tapatybes matrica
AM <- diag(3)
AM[2, 1] <- NA
AM[1, 3] <- NA
AM
#strukturinis var
svar_pl <- SVAR(var_pl, estmethod = "direct", Amat = AM, 
                method = "BFGS", hessian = TRUE)
svar_pl
svar_pl$A
svar_pl$Ase
svar_pl$A/svar_pl$Ase
# Impulso-atsako funkcijos (koki poveiki situ kinamuju sokai turi likusiems kintamiesiems)
irf(svar_pl)
plot(irf(svar_pl))
irf(svar_pl_ab)
plot(irf(svar_pl_ab))
# Blokinio prie?astingumo testai
causality(var_pl, cause = "infl_gap")
#atmetam nuline hipoteze, infliacija yra likusiu kitu kintamuju priezastis, p labai
causality(var_pl, cause = "y_gap")
causality(var_pl, cause = "r_rates")
# Pirmas porinis prie?astingumo testas
data_pl1 <- cbind(infl_gap, y_gap)
var_pl1 <- VAR(data_pl1, p = 4)
acf(residuals(var_pl1))
causality(var_pl1, cause = "infl_gap")
causality(var_pl1, cause = "y_gap")
 # Antras porinis prie?astingumo testas
data_pl2 <- cbind(infl_gap, r_rates)
var_pl2 <- VAR(data_pl2, p = 5)
acf(residuals(var_pl2))
causality(var_pl2, cause = "infl_gap")
causality(var_pl2, cause = "r_rates")
# Tre?ias porinis prie?astingumo testas
data_pl3 <- cbind(y_gap, r_rates)
var_pl3 <- VAR(data_pl3, p = 5)
acf(residuals(var_pl3))
causality(var_pl3, cause = "y_gap")
causality(var_pl3, cause = "r_rates")