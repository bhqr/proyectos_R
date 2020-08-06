library("MASS")
data("Boston")
head(Boston)
summary(Boston)


install.packages("tidyverse")
library(tidyverse)


Boston.lm1 <- lm(medv~crim, data=Boston)
Boston.lm1
summary(Boston.lm1)
## R cuadrado de 14.91

Boston.lm2 <- lm(medv~zn, data=Boston)
Boston.lm2
summary(Boston.lm2)
## R cuadrado de 12.82
Boston.lm3 <- lm(medv~indus, data=Boston)
Boston.lm3
summary(Boston.lm3)

## R cuadrado de 23.25
Boston.lm4 <- lm(medv~chas, data=Boston)
Boston.lm4
summary(Boston.lm4)

## R cuadrado de 2.88
Boston.lm5 <- lm(medv~nox, data=Boston)
Boston.lm5
summary(Boston.lm5)

## R cuadrado de 18.1
Boston.lm6 <- lm(medv~rm, data=Boston)
Boston.lm6
summary(Boston.lm6)
## R cuadrado de 48.25
Boston.lm7 <- lm(medv~age, data=Boston)
Boston.lm7
summary(Boston.lm7)
## R cuadrado de 14.04
Boston.lm8 <- lm(medv~dis, data=Boston)
Boston.lm8
summary(Boston.lm8)
## R cuadrado de 6.06
Boston.lm9 <- lm(medv~rad, data=Boston)
Boston.lm9
summary(Boston.lm9)
## R cuadrado de 14.39
Boston.lm10 <- lm(medv~tax, data=Boston)
Boston.lm10
summary(Boston.lm10)
## R cuadrado de 21.8
Boston.lm11 <- lm(medv~ptratio, data=Boston)
Boston.lm11
summary(Boston.lm11)
## R cuadrado de 25.64
Boston.lm12 <- lm(medv~black, data=Boston)
Boston.lm12
summary(Boston.lm12)
## R cuadrado de 10.94
Boston.lm13 <- lm(medv~lstat, data=Boston)
Boston.lm13
summary(Boston.lm13)
## R cuadrado de 54.32

#######parte b
fit <- lm(medv ~ crim + zn + indus + chas + nox + rm + age + dis+ rad +tax +ptratio + black + lstat,data=Boston)
summary(fit)

### se observa que las variables age  e indus no son significativas, 73.38
fit <- lm(medv ~ crim + zn + chas + nox + rm + dis+ rad +tax +ptratio + black + lstat,data=Boston)
summary(fit)
### R cuadrado de 73.46

fit1 <- lm(medv ~ crim + zn + chas ,data=Boston)
summary(fit1)
## R cuadrado de tres variables crim,zn e indus es 25.85

fit2 <- lm(medv ~ crim + zn +nox,data=Boston)
summary(fit2)
## R cuadrado de tres variables crim,zn y nox es 25.93
  
fit3 <- lm(medv ~ crim + zn +rm,data=Boston)
summary(fit3)  
## R cuadrado de tres variables crim,zn y rm es 55.32

fit4 <- lm(medv ~ crim + zn +dis,data=Boston)
summary(fit4)
## R cuadrado de tres variables crim,zn y rm es 23.96

fit5 <- lm(medv ~ crim + zn +rad,data=Boston)
summary(fit5)
## R cuadrado de tres variables crim,zn y rad es 24.18

fitv <- lm(medv ~ rm + lstat + dis,data=Boston)
summary(fitv)
