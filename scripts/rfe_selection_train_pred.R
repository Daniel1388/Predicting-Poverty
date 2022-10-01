library(here)
library(tidyverse)
library(caret)
library(doParallel) 
library(randomForest)
library(Boruta)
library(fastDummies)
library(gamlr)
library(xgboost)
library(themis)
library(kableExtra)


cl <- makeCluster(detectCores()- 1, type='PSOCK')

registerDoParallel(cl)



# DATA --------------------------------------------------------------------


train_hogares<-readRDS(here("data","train_hogares.Rds"))
train_personas<-readRDS(here("data","train_personas.Rds"))

test_hogares<-readRDS(here("data","test_hogares.Rds"))
test_personas<-readRDS(here("data","test_personas.Rds"))


#Seleccionar variables que están en test.
train_personas_2 <- train_personas %>% select(colnames(test_personas),Ingtot,-contains("Fex"),
                                              Depto,Clase)
train_hogares_2 <- train_hogares %>% select(colnames(test_hogares),Pobre)


# SPLIT DATA --------------------------------------------------------------


set.seed(1)

#Split para hogares
id_train_sample_hog <- sample(1:nrow(train_hogares_2),
                              nrow(train_hogares_2)*0.1)

training_set_hog <- train_hogares_2[-id_train_sample_hog,]
validation_set_hog <- train_hogares_2[id_train_sample_hog,]


# FEATURE SELECTION -----------------------------------------------------------


# Regresion ---------------------------------------------------------------

set.seed(1)

#Split para individuos (solamente para feature selection)
id_train_sample_indiv <- sample(1:nrow(train_personas_2),
                                round(nrow(train_personas_2)*0.1),0)

training_set_indiv <- train_personas_2[-id_train_sample_indiv,]
validation_set_indiv <- train_personas_2[id_train_sample_indiv,]


set.seed(1)

#No se puede hacer busqueda de features con todas las osbervaciones por el tiempo
#computacional
id_feature_selection_sample_ind <- sample(1:nrow(training_set_indiv),
                                          round(nrow(training_set_indiv)*0.2),0)

#Se seleccionan variables
feature_selection_set <- training_set_indiv[id_feature_selection_sample_ind,] %>% 
  select(
    Ingtot,
    Clase,
    P6020,#genero
    P6040,#años
    P6050,#parentesco jefe de hogar
    P6090,#cotizante#no tiene variación
    P6210,#nivel educativo
    P6240,#grado
    P6800,#HORAS DE TRABAJO#200K NAS
    P6870,#PERSOANS EN LA EMPRESA#200K NAS
    P6920,#COTIZACION PENSIONES, 200K NAS
    P7040,#OTRO NEGOCIO, 
    P7090,#quiere trabajar mas horas
    P7505,#subsidios
    P7495#recibio pago de arrendamiento o pension
  ) %>% 
  filter(complete.cases(.)) %>% 
  mutate(age_2=P6040^2)

#Se convierten a factores
feature_selection_set_2 <- feature_selection_set %>% 
  mutate_at(vars(-contains(c("Ingtot","P6040","P6800","age_2"))),as.factor)

#Se hace one hot encoding de las dummies
feature_selection_set_encoded<- model.matrix(~.-1,feature_selection_set_2)



#Control para el RFE
feature_selection_ctrl <- rfeControl(functions = rfFuncs,
                                     method = "repeatedcv",
                                     repeats = 5,
                                     verbose = FALSE,
                                     allowParallel = T)
#Subconjuntos de variables
subsets <- c(4,6,8)

#Aplicar rfe (toma 9 horas)
rfe_reg <- rfe(x=feature_selection_set_encoded[,-1], 
               y=feature_selection_set_encoded[,1],
               sizes = subsets,
               rfeControl = feature_selection_ctrl,
               na.action=na.omit)

