#####REGRESI LINIER SEDERHANA######
datacovid=read.csv("G:\\SEMESTER 5\\KP\\Analisis Regresi\\data_regresi linier sederhana.csv", header = TRUE)
datacovid

x=datacovid$Total.Kasus
x
y=datacovid$Meninggal.Dunia
y

#pemodelan
regresi=lm(y~x, data=datacovid)
summary(regresi) #uji parsial
anova(regresi) #uji overall

#uji asumsi
##normalitas
library(nortest)
lillie.test(regresi$residuals)

##autokorelasi
library(lmtest)
dwtest(regresi)

##heteroskedastisitas
library(lmtest)
bptest(regresi,studentize=FALSE, data=datacovid)
