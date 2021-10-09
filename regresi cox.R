####### REGRESI COX ##################

#INPUT DATA
lampiran9 <- read.csv("G:\\SEMESTER 5\\KP\\Analisis Regresi\\laporan_regresicox.csv", sep=",")
names(lampiran9)

#PEMBENTUKAN MODEL REGRESI COX/SURVIVAL
#install.packages("survival")
library(survival) #load package#
model1 = coxph(Surv(waktu.sembuh..bulan.) ~ 
                 JK + umur + 
                 jenis.obat, data = lampiran9)
summary(model1)

#Penggunaan model regresi rika12  
#variabel JK
ht_girl=exp(0.071818*(1)- 0.001732*(0)-0.068898*(0)-0.396111*(0)-0.049938*(0))
ht_boy=exp(0.071818*(0)- 0.001732*(0)-0.068898*(0)-0.396111*(0)-0.049938*(0))
ht_girl/ht_boy
#variabel umur 49 dan 60
ht_usia.49=exp(0.071818*(0)- 0.001732*(49)-0.068898*(0)-0.396111*(0)-0.049938*(0))
ht_usia.60=exp(0.071818*(0)- 0.001732*(60)-0.068898*(0)-0.396111*(0)-0.049938*(0))
ht_usia.49/ht_usia.60
#variabel jenis obat 
ht_obat.C=exp(0.071818*(0)- 0.001732*(0)-0.068898*(1)-0.396111*(0)-0.049938*(0))
ht_obat.L=exp(0.071818*(0)- 0.001732*(0)-0.068898*(0)-0.396111*(1)-0.049938*(0))
ht_obat.N=exp(0.071818*(0)- 0.001732*(0)-0.068898*(0)-0.396111*(0)-0.049938*(1))
ht_obat.C/ht_obat.L
ht_obat.L/ht_obat.N
ht_obat.C/ht_obat.N
#uji asumsi proposionalitas
cox.zph(model1)  