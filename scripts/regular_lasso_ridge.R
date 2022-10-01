rm(list = ls())
library(pacman)
p_load(tidyverse,knitr,kableExtra,here,jtools,ggstance,broom,broom.mixed,skimr,readxl,boot,openxlsx,rio,caret,glmnet,shiny,fastDummies,caTools)
set.seed(10101)
here()

# Carga de información
df_hogares_train <- import(here("stores","train_hogares.RDS"))
df_personas_train <- import(here("stores","train_personas.RDS"))
df_hogares_test <- import(here("stores","test_hogares.RDS"))
df_personas_test <- import(here("stores","test_personas.RDS"))

#unión de bases de train y test
data_train <-left_join(df_hogares_train,df_personas_train,by = c("id"))
head(data_train)
data_test <-left_join(df_hogares_test,df_personas_test,by = c("id"))
colnames(data_test)

#Limitando train a columnas de test, agregando columna a predecir "Ingtot"
data_train2 <- data_train[ , (names(data_train) %in% colnames(data_test))]
data_train3 <- cbind(data_train2[,],data_train[,154])
glimpse(data_train3)
glimpse(data_test)

#Preprocesamiento de data
data_train3[is.na(data_train3)] <- 0
cat <- c("Clase.x" , "Dominio.x", "Depto.x", "Clase.y", "Dominio.y", "Depto.y")
datos2 <- data_train3[ , !(names(data_train3) %in% cat)]
glimpse(datos2)

#Escalado de data
data_scaled <- cbind(scale(datos2[,2:72]), datos2[,73])
glimpse(data_scaled)
size <- floor(0.8 * nrow(data_scaled))
train_ind <- sample(seq_len(nrow(data_scaled)), size = size)
train <- data_scaled[train_ind,]
xtrain <- train[,1:71]
ytrain <- train[,72]
test <- data_scaled[-train_ind,]
xtest <- test[,1:71]
ytest <- test[,72]

#LASSO
lambda.array <- seq(from=0.01, to = 100, by= 0.01)
lassofit <- glmnet(xtest, ytest, alpha=1, lambda=lambda.array)
summary(lassofit)
plot(lassofit, xvar= 'lambda', label=T, main="Lasso")
#VARIABLES SELECCIONADAS: P7495+Nper+P6870

#RIDGE
ridgefit <- glmnet(xtest, ytest, alpha=0, lambda=lambda.array)
summary(lassofit)
plot(ridgefit, xvar= 'lambda', label=T, main="Ridge")
#VARIABLES SELECCIONADAS: P7495+Nper+P6870+Npersug

#Regresión con variables lasso
mod <- lm("Ingtot ~ Pet+P7495+Nper+P6870", data = data_train3)
summary(mod)
#loss 1061000 

#Regresión con variables Ridge
mod <- lm("Ingtot ~ Pet+P7495+Nper+P6870+Npersug", data = data_train3)
summary(mod)
#loss 1059000 
