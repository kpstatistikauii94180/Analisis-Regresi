#####REGRESI LINIER BERGANDA#### 

data_realestate = read.csv("G:\\SEMESTER 5\\KP\\Analisis Regresi\\data_regresi linier berganda.csv", header = TRUE)
View(data_realestate)

#untuk mengambil salah satu variabel -> $
y = data_realestate$House.price.of.unit.area
x1 = data_realestate$House.age
x2 = data_realestate$Distance.to.the.river
x3 = data_realestate$Distance.to.the.nearest.MRT.station
x4 = data_realestate$Number.of.convenience.stores
x5 = data_realestate$Number.of.neighbour

#regresi
regresi = lm(y~x1+x2+x3+x4+x5, data = data_realestate)
summary(regresi)

regresi2 = lm(y~x1+x3+x4+x5, data = data_realestate)
summary(regresi2)

regresi3 = lm(y~x1+x3+x4, data = data_realestate)
summary(regresi3)

#uji normalitas
library(nortest)
lillie.test(regresi3$residuals)

#uji autokorelasi
library(lmtest)
dwtest(regresi3)

#uji homoskedastisitas
library(lmtest)
bptest(regresi3, studentize = FALSE, data = data_realestate)

#uji multikolinearitas
library(car)
vif(regresi3)