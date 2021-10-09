##########REGRESI DUMMY###########

#Input Data 
laporandummy=read.csv("G:\\SEMESTER 5\\KP\\Analisis Regresi\\laporan_regresidummy.csv", sep=",")
laporandummy

##definisikan variabel
Y  = laporandummy$premium      #variabel respon (Y)
X1 = laporandummy$age          #variabel prediktor kuantitatif 1 (X1)
X2 = laporandummy$bmi          #variabel prediktor kuantitatif 2 (X2)
X3 = laporandummy$children     #variabel prediktor kuantitatif 3 (X3)
D1 = laporandummy$dv_smoker    #variabel prediktor Dummy 1 (D1)
D2 = laporandummy$dv_southwest #variabel prediktor Dummy 2 (D2)

# membuat model regresi dummy
modelregresi1 = lm(Y ~ X1 + X2 + X3 + D1 + D2, data = laporandummy) #model
regresdummy1 = summary(modelregresi1) 
regresdummy1 #ringkasan model regresi

#diperoleh bahwa X3 dan D2 tidak signifikan. maka dihilangkan variabel dengan p-value paling besar, yaitu D2
modelregresi2 = lm(Y ~ X1 + X2 + X3 + D1, data = laporandummy) #model
regresdummy2 = summary(modelregresi2) 
regresdummy2 #ringkasan model regresi

#diperoleh bahwa X3 tidak signifikan, maka dihilangkan variabel X3
modelregresi3 = lm(Y ~ X1 + X2 + D1, data = laporandummy) #model
regresdummy3 = summary(modelregresi3) 
regresdummy3 #ringkasan model regresi

#setelah semua variabel signifikan, maka dilakukan uji asumsi 
#UJI ASUMSI
##Uji normalitas 
library(nortest)
lillie.test(regresdummy3$residuals)

##Autokorelasi
library(lmtest)
dwtest(regresdummy3)

##Uji Homoskedastisitas
library(lmtest)
bptest(regresdummy3, studentize = FALSE, data=laporandummy)

##Uji Multikolinearitas
library(car)
vif(modelregresi3)
#jika hanya terdapat 1 variabel prediktor tidak perlu uji multikolinearitas
#dapat dikeathui bahwa terdapat variabel prediktor yatu X1 dan D2 maka digunakan uji multikolinearitas