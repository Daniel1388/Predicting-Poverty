rm(list = ls())
library(pacman)
p_load(InformationValue,ISLR,tidyverse,knitr,kableExtra,here,jtools,ggstance,broom,broom.mixed,skimr,readxl,boot,openxlsx,rio,caret,glmnet,shiny,fastDummies,caTools)
set.seed(10101)

# Carga de información
train <- import(here("stores","train.csv"))
test <- import(here("stores","test.csv"))
validation <- import(here("stores","df_test_examen.csv"))
val <- import(here("stores","validation_set_id.csv"))

#Seleccionando variable Y 
Ingpcug <- train[,14]
head(Ingpcug)
borrar <- c("Ingpcug", "Pobre", "status")
train2 <- train[ , !(names(train) %in% borrar)]
train3 <- cbind(train2[,],Ingpcug)
glimpse(train3)

#escalado de datos y partición 80 train - 20 test
data_scaled <- cbind(scale(train3[,2:342]), train3[,343])
glimpse(data_scaled)
size <- floor(0.8 * nrow(data_scaled))
train_ind <- sample(seq_len(nrow(data_scaled)), size = size)

train <- data_scaled[train_ind,]
xtrain <- train[,1:341]
ytrain <- train[,342]

test <- data_scaled[-train_ind,]
xtest <- test[,1:341]
ytest <- test[,342]
glimpse(ytest)

#Selección de variables por medio de lasso
lambda.array <- seq(from=0.01, to = 100, by= 0.01)
lassofit <- glmnet(xtrain, ytrain, alpha=1, lambda=lambda.array)
summary(lassofit)
plot(lassofit, xvar= 'lambda', label=T)

#Selección de variables por medio de ridge
ridgefit <- glmnet(xtrain, ytrain, alpha=0, lambda=lambda.array)
summary(lassofit)
plot(ridgefit, xvar= 'lambda', label=T)

#Entrenamiento de Regresión Lineal
mod <- lm("Ingpcug ~ P7495_1+ Lp + P7500s3_2 + P5010", data = train3)
summary(mod)

#Predecir con modelo
selected_variables <- c("P7495_1","Lp","P7500s3_2", "P5010")
pred_mod_log = predict(mod,newdata =subset(validation,select = selected_variables),type="response")
pred <- validation %>% mutate(Pobre_hand=ifelse(pred_mod_log<Lp,1,0))
prediction <- pred %>% mutate(pred=pred_mod_log)
head(prediction)
