datos <- read.csv("boston.csv",head=T,sep = ",", row.names = 1)
datos

library(ISLR)
library(Auto)
library(rlang)
library(dplyr)

require(MASS)
require(ISLR)

require(psych)
datos <- read.csv("boston.csv",head=T,sep = ",", row.names = 1)
datos

Boston$chas <- as.factor(Boston$chas)
summary(Boston)

multi.hist(x = Boston[,1:3], dcol = c("blue","red"), dlty = c("dotted", "solid"),
           main = "")

modelo_multiple <- lm(formula = medv ~ ., data = Boston)
# También se pueden especificar una a una 
summary(modelo_multiple)

step(modelo_multiple, direction = "both", trace = 0)

modelo_multiple <- lm(formula = medv ~ crim + zn + chas +  nox + rm +  dis +
                        rad + tax + ptratio + black + lstat, data = Boston)
summary(modelo_multiple)

par(mfrow = c(1,2))
plot(modelo_multiple)

par(mfrow = c(1,1))

install.packages("corrplot")
require(corrplot)
corrplot.mixed(corr = cor(Boston[,c("crim", "zn", "nox", "rm", "dis", "rad", 
                                    "tax", "ptratio", "black", "lstat", "medv")],
                          method = "pearson"))

install.packages("car")
install.packages("carData")
require(car)
vif(modelo_multiple)












