# Paketai
#kad galetume isskaidyti bvp i cikla trenda ir tt. - hodrick prestor 
library(mFilter)
#var modeliui
library(vars)
library(readxl)
# Duomenys
poland_nk_data <- read_excel("C:/Users/globy/Desktop/aukstoji makro/macro_04_nd/poland_nk_data.xlsx", 
                             sheet = "data")
attach(poland_nk_data)
# Kintamieji
infl <- ts(infl, start = c(1998, 1), frequency = 4)
t_infl <- ts(t_infl, start = c(1998, 1), frequency = 4)
rgdp <- ts(rgdp, start = c(1998, 1), frequency = 4)
ngdp <- ts(ngdp, start = c(1998, 1), frequency = 4)
gr_rgdp <- ts(gr_rgdp, start = c(1998, 1), frequency = 4)
gr_ngdp <- ts(gr_ngdp, start = c(1998, 1), frequency = 4)

#gamybos spraga, ketvirtiniai duomenys, todel freg = 1600,skirtumas tarp bvp ir trendo yra ciklas, todel ji issitraukiam, procentine israiska *100
y_gap <- hpfilter(log(rgdp), freq = 1600)$cycle*100
#infliacijos nuokrypis nuo tikslines infliacijos (infliacijos spraga)
infl_gap <- infl - t_infl

# Pirmas porinis priezastingumo testas
#apjungiam i duomenu martica nominalaus ir realaus bvp augimo tempus
data_gdp <- cbind(gr_rgdp, gr_ngdp)
plot.ts(data_gdp)
#pagal grafika matosi kad stacionarus augimo tempai
ts.plot(log(ngdp))
#pats nominalus nera stacionarus, tai yra atsitiktinis klaidziojimas
#pagal informacinius kriterijus, maziausia reiksme nurodo tinkama modeli (kompaktiskiausias modelis(maziausiai velavimu) su aksciausiomis paaiskinimo galimybemis)
VARselect(data_gdp, 12)
#daugiausia siulo 3 eiles modeli, vienas siulo 1 eiles modeli
#paziurim eiliu (keiciam p) is eiles modelius ir ju autokorelogramas, kai p=1 matom kad 4 eiles reiskminga, bandom p=4 ir jis tinka
var_gdp <- VAR(data_gdp, p = 4)
acf(residuals(var_gdp))
summary(var_gdp)
causality(var_gdp, cause = "gr_rgdp")
#p-value = 0.01748, nuline hipoteze atmetam, realaus bvp augimo tempai formuoja nominalaus bvp aug.t
causality(var_gdp, cause = "gr_ngdp")
#p-value = 0.001286, nuline hipoteze atmetam, nominalus bvp augimo t. formuoja realaus bvp aug. t. 


# Antras porinis priezastingumo testas
data_gap <- cbind(y_gap, gr_ngdp)
VARselect(data_gap, 12)
var_gap <- VAR(data_gap, p = 4)
acf(residuals(var_gap))
summary(var_gap)
causality(var_gap, cause = "y_gap")
causality(var_gap, cause = "gr_ngdp")
#abiuju nulines hopotezes atmetam

# Visumines paklausos SVAR modelis
data_ad <- cbind(gr_ngdp, infl_gap, y_gap)
VARselect(data_ad, 12)
var_ad <- VAR(data_ad, p = 6)
acf(residuals(var_ad))
summary(var_ad)
#Correlation matrix of residuals:
#gr_ngdp infl_gap  y_gap
#gr_ngdp   1.0000   0.4193 0.4670
#infl_gap  0.4193   1.0000 0.1931
#y_gap     0.4670   0.1931 1.0000
#ten kur 0.4193 0.4670 (tarp pirmo ir antro kintamojo ir tarp pirmo ir treciajo) paklaidos yra reiksmingai koreliuotos
causality(var_ad, cause = "gr_ngdp")
#nom. bvp yra infliacijos spragos ir gamybos spragos priezastis
causality(var_ad, cause = "infl_gap")
#infliacijos spraga nera nominalaus bvp ir gamybos spragos priezastis
causality(var_ad, cause = "y_gap")
#gamybos spraga nera nominalaus bvp ir infliacijos spragos priezastis
 
AM <- diag(3)
AM[2, 1] <- NA
AM[3, 1] <- NA
AM
svar_ad <- SVAR(var_ad, estmethod = "direct", Amat = AM, 
                method = "BFGS", hessian = TRUE)
#vienalaikiu poveikiu matrica
svar_ad
#jei nominalaus bvp augimo tempas padidetu vienu procentiniu punktu, infliaicjos spraga padidetu 0,2 proc p. o gamybos spraga 0,29 proc p. (pasikeicia zenklai)

#padalinam A matricos elementus is standartiniu paklaidu
svar_ad$A/svar_ad$Ase
#impluso atsako funkcijos
plot(irf(svar_ad))
#bvp turi teigiama poveiki infliacijai ir gamybos spragai, tik gamybos spragai poveikis yra didesnis
#inflaicija neturi poveikio gamybai
