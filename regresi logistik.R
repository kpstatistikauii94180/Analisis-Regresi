############ REGRESI LOGISTIK ########################

laporanlogistik=read.csv("G:\\SEMESTER 5\\KP\\Analisis Regresi\\laporan_regresilogistik.csv", sep=",")
head(laporanlogistik) #menampilkan 5 data teratas nya

#1=puas, 0=tidak puas
#baseline model
table(laporanlogistik$Y)

#menghtiung baseline 
28/50 #jumlah 0 dari Y/keseluruhan Y-> untuk mengetahui pelayanan publik baik atau tidak
#hasilnya 0,56 maka pelayanan publik tersebut sedang 

library(caTools)# membagi data menjadi data training dan data testing

#Randomly split data 
set.seed(50) # merandom data sebanyak 50 kali 
split=sample.split(laporanlogistik$Y, SplitRatio=0.56) # kita bangun data dengan merandomnya dengan menjadikan data training dan data testing
split

#creative tranining and testing sets
logistikTrain=subset(laporanlogistik, split==TRUE)
logistikTest=subset(laporanlogistik, split==FALSE)
nrow(logistikTrain)
nrow(logistikTest)

#Logistic Regression Model
logistikLog1 = glm(Y~X1+X2, data=logistikTrain, family = binomial)
Logis1=summary(logistikLog1)
Logis1

##karena hasil logis 1 tidak signifikan maka dihilangkan variabel X1 nya. 
logistikLog2 = glm(Y~X2, data=logistikTrain, family = binomial)
Logis2=summary(logistikLog2)
Logis2

predictTrain=predict(logistikLog2, type="response")
predictTrain

#Penentuan Threshold
library(ROCR)
ROCRpred = prediction(predictTrain, logistikTrain$Y)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

#Menghitung Tingkat Akurasi
predictTest = predict(logistikLog2, type = "response", newdata = logistikTest)
table(logistikTest$Y, predictTest >=0.5) #nilaipredictTest berdasarkan Thresold. 

#Accuracy
(8+9)/22