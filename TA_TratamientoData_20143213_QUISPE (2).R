data_ofi=read.csv("C:/Users/o1/Downloads/dataoficinas_out.csv",header=TRUE)
data_ofi
summary(data_ofi)
install.packages("naniar")
library(naniar)
vis_miss(data_ofi,sort_miss=TRUE,cluster=TRUE)
gg_miss_var(data_ofi)
#PREGUNTA 3: Sí existen variables con datos faltantes
#Son  FTE_Operativo y Clientes_Masivo, con 39 y 13 respectivamente.

boxplot(data_ofi["Rentabilidad"])
boxplot(data_ofi["FTE_Comercial"])
boxplot(data_ofi["FTE_Operativo"])
boxplot(data_ofi["Clientes_Masivo"])
boxplot(data_ofi["Clientes_VIP"])
boxplot(data_ofi["Clientes_Negocios"])

data_rent <- data_ofi["Rentabilidad"]
data_rent <- data_rent[!is.na(data_rent)]

data_comer <- data_ofi[["FTE_Comercial"]]
data_oper <- data_ofi["FTE_Operativo"]
data_oper <- data_oper[!is.na(data_oper)]
data_masi <- data_ofi["Clientes_Masivo"]
data_masi <- data_masi[!is.na(data_masi)]
data_vip <- data_ofi[["Clientes_VIP"]]
data_nego <- data_ofi[["Clientes_Negocios"]]


bench_rent<- 5926300 +1.5*IQR(data_rent)
bench_rent

bench2_rent<- 2231322 -1.5*IQR(data_rent)
bench2_rent

data_rent_vi<- data_rent[data_rent<bench_rent]
data_rent_vi<- data_rent_vi[data_rent_vi>bench2_rent]
length(data_rent)-length(data_rent_vi)
#En rentabilidad hay 9 outliers
bench_comer<- 8 +1.5*IQR(data_comer)
bench_comer

bench2_comer<- 4 -1.5*IQR(data_comer)
bench2_comer

data_comer_vi<- data_comer[data_comer<bench_comer]
data_comer_vi<- data_comer_vi[data_comer_vi>bench2_comer]
length(data_comer)-length(data_comer_vi)
#En comercial hay 10
bench_oper<- 13 +1.5*IQR(data_oper)
bench_oper

bench2_oper<- 9 -1.5*IQR(data_oper)
bench2_oper

data_oper_vi<- data_oper[data_oper<bench_oper]
data_oper_vi<- data_oper_vi[data_oper_vi>bench2_oper]
length(data_oper)-length(data_oper_vi)
#En operativo hay 19
bench_masi<-  3648 +1.5*IQR(data_masi)
bench_masi

bench2_masi<- 2162 -1.5*IQR(data_masi)
bench2_masi

data_masi_vi<- data_masi[data_masi<bench_masi]
data_masi_vi<- data_masi_vi[data_masi_vi>bench2_masi]
length(data_masi)-length(data_masi_vi)
#En masivo hay 8
bench_vip<- 817.5 +1.5*IQR(data_vip)
bench_vip

bench2_vip<- 209.5 -1.5*IQR(data_vip)
bench2_vip

data_vip_vi<- data_vip[data_vip<bench_vip]
data_vip_vi<- data_vip_vi[data_vip_vi>bench2_vip]
length(data_vip)-length(data_vip_vi)
#En vip hay 9
bench_nego<- 764.5 +1.5*IQR(data_nego)
bench_nego

bench2_nego<- 369.5 -1.5*IQR(data_nego)
bench2_nego

data_nego_vi<- data_nego[data_nego<bench_nego]
data_nego_vi<- data_nego_vi[data_nego_vi>bench2_nego]
length(data_nego)-length(data_nego_vi)
#En negocio hay 8

#PREGUNTA 1 : La variable con mayor cantidad de outliers es FTE_Operativo con 19 outliers.

summary(data_rent)
#media de 4804038
summary(data_rent_vi)
#media de 4038009
summary(data_comer)
#media de 6.277
summary(data_comer_vi)
#media de 5.481
summary(data_oper)
#media de 11.29
summary(data_oper_vi)
#media de 10.88
summary(data_masi)
#la ,media es de 2979
summary(data_masi_vi)
#la media es de 2809
summary(data_vip)
#la media es de 589
summary(data_vip_vi)
#la media es de 513.7
summary(data_nego)
#media de 598.1
summary(data_nego_vi)
#media de 551.7
#PREGUNTA 2: Los promedios de las variables considerarando outliers son:
#Rentabilidad 4804038, FTE_Comercial 6.277, FTE_Operativo 11.29
#Clientes_Masivo 2979, Clientes VIP 589, Clientes_Negocios 598.1
#Y los promedios sin considerar outliers son:
#Rentabilidad 4038009, FTE_Comercial 5.481, FTE_Operativo 10.88
#Clientes_Masivo 2809, Clientes VIP 513.7, Clientes_Negocios 551.7
data_ofi_vf <- data_ofi
model <- lm(FTE_Operativo ~ FTE_Comercial, data=data_ofi_vf )
I <- is.na(data_ofi_vf$FTE_Operativo)
data_ofi_vf$FTE_Operativo[I] <- predict(model,newdata=data_ofi_vf[I,])
summary(data_ofi_vf)
#PREGUNTA 4: La media de la variable FTE_Operativo es 11.26 
install.packages("VIM")
library(VIM)
data(data_ofi_vf)
n <- nrow(data_ofi_vf)
for (i in 1:ncol(data_ofi_vf)) {
  data_ofi_vf[sample(1:n, 10, replace=FALSE),i] <- NA
}
data_ofi_vff <- kNN(data_ofi_vf)
summary(data_ofi_vff)
#PREGUNTA 5: La media de Clientes_Masivo es 2986 
