####### REGRESI ROBUST ###########

lampiran8 <- read.csv("G:\\SEMESTER 5\\KP\\Analisis Regresi\\laporan_regresirobust.csv", sep=",")
names(lampiran8)

#ringkasan data
summary(lampiran8)

#estimasi model menggunakan regresi OLS
fit.ols <- lm(Y ~ X1+X2+X3+X4+X5, data = lampiran8)
summary(fit.ols)

#metode backward 1
fit.ols1 <- lm(Y ~ X1+X2+X3+X4, data = lampiran8)
summary(fit.ols1)

#metode backward 2 
fit.ols2 <- lm(Y ~ X1+X2+X4, data = lampiran8)
summary(fit.ols2)

#metode backward 3
fit.ols3 <- lm(Y ~ X1+X2, data = lampiran8)
summary(fit.ols3)

#uji asumsi klasik
#uji normalitas residual
shapiro.test(fit.ols3$residuals)

#uji homoskedastisitas
library(lmtest)
bptest(fit.ols3, studentize = FALSE, data = lampiran8)

#uji autokorelasi residual
library(lmtest)
dwtest(fit.ols3)

#uji multikolinieritas
library(car)
vif(fit.ols3)

#pendeteksian outlier pada data
#menggunakan plot
par(mforw=c(2,2))
plot(fit.ols3, las=1)
#memperjelas plot
library(olsrr)
ols_plot_cooksd_bar(fit.ols3)

# dengan M
library(MASS)
fit.M <- rlm(Y ~ X1+X2, data = lampiran8, method = "M")
print(fit.M)
summary(fit.M)

library(sfsmisc)
f.robftest(fit.M, var = "X1")
f.robftest(fit.M, var = "X2")

par(mfrow=c(2,2))
plot(fit.M)

# dengan S
library(robustbase)
fit.S <- lmrob(Y ~ X1+X2, data = lampiran8, method = "S")
summary(fit.S)

par(mfrow=c(2,3))
plot(fit.S)

# dengan MM
library(robustbase)
fit.MM <- lmrob(Y ~ X1+X2, data = lampiran8, method = "MM")
summary(fit.MM)

par(mfrow=c(2,3))
plot(fit.MM)