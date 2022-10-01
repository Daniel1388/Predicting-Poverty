rm(list = ls())
install.packages('RDS')
library('RDS')
library(pacman)
p_load(tidyverse,knitr,kableExtra,here,jtools,ggstance,broom,broom.mixed,skimr,readxl,boot,openxlsx,rio,caret,glmnet,shiny,MASS,dplyr)
set.seed(10101)

# Carga de información
trainin <- import(here("data","train.csv"))
test <- import(here("data","test.csv"))
validation <- import(here("data","test_personas.RDS"))

#Seleccionando variable Y 
Pobre <- trainin[,13]
head(Pobre)
borrar <- c("Ingpcug", "Pobre", "status")
train2 <- trainin[ , !(names(trainin) %in% borrar)]
train3 <- cbind(train2[,],Pobre)
glimpse(train3)

#escalado de datos y partición 80 train - 20 test
data_scaled <- cbind(scale(train3[,2:342]), train3[,343])
glimpse(data_scaled)
size <- floor(0.8 * nrow(data_scaled))
train_ind <- sample(seq_len(nrow(data_scaled)), size = size)
train <- data_scaled[train_ind,]
head(train[,342])

xtrain<-train[,1:341]
ytrain <- train[,342]
dftrain <- as.data.frame(xtrain, ytrain)

test <- data_scaled[-train_ind,]
xtest <- test[,1:341]
ytest <- test[,342]
dftest <- as.data.frame(xtrain, ytrain)

#Selección de variables por medio de lasso
lambda.array <- seq(from=0.01, to = 100, by= 0.01)
lassofit <- glmnet(xtrain, ytrain, alpha=1, lambda=lambda.array)
summary(lassofit)
plot(lassofit, xvar= 'lambda', label=T)

#Selección de variables por medio de ridge
ridgefit <- glmnet(xtrain, ytrain, alpha=0, lambda=lambda.array)
summary(lassofit)
plot(ridgefit, xvar= 'lambda', label=T)

#Entrenamiento de Logit
logit <- glm(ytrain~P6100_3 +P6100_0+ P6430_8+ P7110_2 + P6210s1_14 + P6630s6_2+P5000,
               data = dftrain, family = "binomial")
summary(logit,type="text")

#Predecir con modelo
selected_variables <- c("P6100_3","P6100_0","P6430_8", "P7110_2", "P6210s1_14", "P6630s6_2", "P5000")
pred_mod_log = predict(logit,newdata =subset(dftest,select = selected_variables),type="response")
head(pred_mod_log)
