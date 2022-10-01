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


# Clasificación -----------------------------------------------------------

set.seed(1)
feature_selection_sample_hog <- sample(1:nrow(training_set_hog),
                                       round(nrow(training_set_hog)*0.3),0)

#Muestreo debe ser a nivel de hogar para agregar los ingresos.
id_feature_selection_sample_hog <- training_set_hog[feature_selection_sample_hog,"id"]

#Selección de variables individuales de acuerdo a partición de hogares
feature_selection_set_indiv_clas <- train_personas_2[train_personas_2$id%in%id_feature_selection_sample_hog$id,] %>% 
  select(
    Ingtot,
    id,
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
  mutate(age_2=P6040^2)


#Seleccionar y convetri en factores
feature_selection_set_indiv_clas_2 <- feature_selection_set_indiv_clas %>% 
  mutate_at(vars(-contains(c("Ingtot","P6040","P6800","age_2"))),as.factor)

#Función que crea la moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Seleccionar variables y crear nuevas variables
feature_selection_set_indivhog_clas <- feature_selection_set_indiv_clas_2 %>% 
  mutate(
    jefe_hogar_mujer=ifelse(P6050%in%1&P6020%in%2,1,0)) %>% 
  
  group_by(id) %>% 
  
  mutate(dependency_ratio=sum(P6040>=65,na.rm=T)/n(),
         old_radio=sum(P6040>=60,na.rm=T)/n(),
         age_head=ifelse(P6050%in%1,P6040,NA)
         
  ) %>% 
  
  fill(age_head,.direction = 'downup') %>% 
  
  mutate(
    clase_mode=getmode(Clase),
    P6020_mode=getmode(P6020),
    P6050_mode=getmode(P6050),
    P6090_mode=getmode(P6090),
    P6210_mode=getmode(P6210),
    P6240_mode=getmode(P6240),
    P6870_mode=getmode(P6870),
    P6920_mode=getmode(P6920),
    P7040_mode=getmode(P7040),
    P7090_mode=getmode(P7090),
    P7505_mode=getmode(P7505),
    P7495_mode=getmode(P7495),
    avg_ingtot=mean(Ingtot,na.rm=T),
    median_ingtot=median(Ingtot,na.rm=T),
    avg_age=mean(P6040,na.rm=T),
    median_age=median(Ingtot,na.rm=T),
    avg_horas=mean(P6800,na.rm=T),
    median_horas=median(P6800,na.rm=T)
  ) %>% 
  
  ungroup() %>% 
  select(id,jefe_hogar_mujer,dependency_ratio,old_radio,age_head,contains(c("mode","avg","median"))) %>% 
  distinct


feature_selection_set_indivhog_clas_2 <- feature_selection_set_indivhog_clas %>% 
  left_join(train_hogares_2[,c("id","Pobre","Nper")]) %>% 
  filter(complete.cases(.)) %>% 
  mutate(Pobre=as.factor(Pobre)) 

feature_selection_set_indivhog_clas_2_encoded<- model.matrix(Pobre~.-1,feature_selection_set_indivhog_clas_2[,-1])


#Función personalizada
weighed_fpr_fnr <- function (data, lev = NULL, model = NULL) {
  #1's are true positives and 0's are false positives
  tp <- sum(as.numeric(data$pred[data$obs%in%1]))
  fp <- length(data$pred[data$obs==1])-sum(as.numeric(data$pred[data$obs==1]))
  
  #1's are false negatives and 0's are true negatives
  fn <- sum(as.numeric(data$pred[data$obs==0]))
  tn <- length(data$pred[data$obs==0])-sum(as.numeric(data$pred[data$obs==0]))
  
  #tpr
  
  if((fp+tn)!=0){
    fpr <- fp/(fp+tn)
  }else{
    fpr <- 0
    
  }
  
  if((fn+tp)!=0){
    fnr <- fn/(fn+tp)
    
  }else{
    fnr <- 0
  }
  
  weighted_fnr_fpr=0.75*fnr+0.25*fpr
  names(weighted_fnr_fpr) <- c("weighted_fnr_fpr")
  
  return(weighted_fnr_fpr)
  
  
  
  
  
} 

#Emplear la función en el summary
rfFuncs$summary <- weighed_fpr_fnr

#Pasar función para generar la metrica especial
feature_selection_ctrl <- rfeControl(functions = rfFuncs,
                                     method = "repeatedcv",
                                     repeats = 5,
                                     verbose = T,
                                     allowParallel = T)
#Subconjuntos de variables
subsets <- c(4,6,8)

#Evaluar en la función especial
rfe_clas <- rfe(x=feature_selection_set_indivhog_clas_2_encoded, 
                y=feature_selection_set_indivhog_clas_2[["Pobre"]],
                maximize = F,
                sizes = subsets,
                metric="weighted_fnr_fpr",
                rfeControl = feature_selection_ctrl,
                na.action=na.omit)



# MODEL TRAINING ----------------------------------------------------------


## Regresión ---------------------------------------------------------------


#Crear set de validación
validation_reg <- train_personas_2[train_personas_2$id%in%validation_set_hog$id,]%>% 
  select(
    Ingtot,
    id,
    P6020,#genero
    P6040,#años
    P6210,#nivel educativo
    P6800,#HORAS DE TRABAJO#200K NAS
    P7495#recibio pago de arrendamiento o pension,
  ) %>% 
  filter(complete.cases(.)) %>% 
  mutate(age_2=P6040^2,
         P6210=factor(P6210,levels=levels(train_reg$P6210))) %>% 
  mutate_at(vars(-contains(c("Ingtot","P6040","P6800","age_2"))),~factor(.,exclude=NA))

#Factor debe tener los mismos niveles al momento de predecir
levels(validation_reg$P6210) <- levels(train_reg$P6210)

#Set de entrenamiento
train_reg <- train_personas_2[train_personas_2$id%in%training_set_hog$id,]%>% 
  select(
    Ingtot,
    id,
    P6020,#genero
    P6040,#años
    P6210,#nivel educativo
    P6800,#HORAS DE TRABAJO#200K NAS
    P7495#recibio pago de arrendamiento o pension,
  ) %>% 
  filter(complete.cases(.)) %>% 
  mutate(age_2=P6040^2) %>% 
  mutate_at(vars(-contains(c("Ingtot","P6040","P6800","age_2"))),as.factor)


categoricas <- colnames(train_reg)[-which(colnames(train_reg)%in%c("id","Ingtot","P6040","P6800","age_2"))]


#No se puede usar model.matrix por el tamaño
train_reg_encoded<- dummy_cols(train_reg,
                               select_columns=categoricas,
                               remove_selected_columns=T) %>% select(-id)



validation_reg_encoded <- dummy_cols(validation_reg,
                                     select_columns=categoricas,
                                     remove_selected_columns=T) %>% select(-id)



#LINEAR REGRESSION

reg_lr <- lm(Ingtot~., 
             train_reg[,-2])
pred_reg_lr <- predict(reg_lr,validation_reg)
rmse_reg_lr <- postResample(pred = pred_reg_lr, obs = validation_reg$Ingtot)
rmse_reg_lr[1]



#XGBOOST
train_control_reg <- trainControl(method="cv", 
                                  number=5, 
                                  allowParallel = T)

#Grilla para hiperparámetros
grid_xgboost_reg <- expand.grid(nrounds = c(250,500),
                                max_depth = c(4,6,8),
                                eta = c(0.01,0.3,0.5),
                                gamma = c(0),
                                min_child_weight = c(10,25,50),
                                colsample_bytree = 1,
                                subsample = 1)
#Entrenamiento
train_control_reg <- trainControl(method="cv", 
                                  number=5, 
                                  allowParallel = T)


grid_xgboost_reg <- expand.grid(nrounds = c(250,500),
                                max_depth = c(4,6,8),
                                eta = c(0.01,0.3,0.5),
                                gamma = c(0),
                                min_child_weight = c(10,25,50),
                                colsample_bytree = 1,
                                subsample = 1)

xgboost_reg <- train(
  Ingtot~.,
  data=train_reg_encoded,
  method = "xgbTree",
  trControl = train_control_reg,
  tuneGrid = grid_xgboost_reg,
  preProcess = c("center", "scale")
)


pred_reg_xgb <- predict(xgboost_reg,validation_reg_encoded)
rmse_reg_xgb <- postResample(pred = pred_reg_xgb, obs = validation_reg$Ingtot)
rmse_reg_xgb[1]



#Reporte de resultados
resultados_rmse <- tibble(
  "Modelo"=c("Regresión Lineal",
             "XGBoost"),
  
  "RMSE"=c(rmse_reg_lr[1],
           rmse_reg_xgb[1]),
  
  "Número de variables"=rep(5,2)
  
  
)




#Predicciones de pobreza
pobre_reg_lr <- tibble("id"=validation_reg$id,"ingtot_pred"=pred_reg_lr) %>% 
  group_by(id) %>% 
  mutate(Ingtotug_pred=sum(ingtot_pred,na.rm=T)) %>% 
  select(-ingtot_pred) %>% 
  distinct() %>% 
  left_join(validation_set_hog[,c("id","Nper","Lp","Pobre")]) %>% 
  mutate(ingpcug_pred=Ingtotug_pred/Nper,
         pobre_pred=ifelse(ingpcug_pred<Lp,1,0)
  )


#Evaluar en función personalizada
reg_lr_weighed_fpr_fnr <- weighed_fpr_fnr(tibble("pred" = pobre_reg_lr$pobre_pred, "obs" = pobre_reg_lr$Pobre))



#Predicciones de pobreza
pobre_reg_xgb <- tibble("id"=validation_reg$id,"ingtot_pred"=pred_reg_xgb) %>% 
  group_by(id) %>% 
  mutate(Ingtotug_pred=sum(ingtot_pred,na.rm=T)) %>% 
  select(-ingtot_pred) %>% 
  distinct() %>% 
  left_join(validation_set_hog[,c("id","Nper","Lp","Pobre")]) %>% 
  mutate(ingpcug_pred=Ingtotug_pred/Nper,
         pobre_pred=ifelse(ingpcug_pred<Lp,1,0)
  )


#Evaluar en función personalizada
reg_xbg_weighed_fpr_fnr <- weighed_fpr_fnr(tibble("pred" = pobre_reg_xgb$pobre_pred, "obs" = pobre_reg_xgb$Pobre))

#Reporte de resultados de pobreza
resultados_reg_fpr_fnr <- tibble(
  "Modelo"=c("Regresión Lineal",
             "XGBoost"),
  
  "(0.75)FNR+(0.25)FPR"=c(reg_lr_weighed_fpr_fnr[1],
                          reg_xbg_weighed_fpr_fnr[1]),
  
  "Número de variables"=rep(5,2)
  
  
)

#Tabla en código latex
resultados_reg_fpr_fnr%>% 
  kable(.,escape = F,format="latex", booktabs=T,linesep = "") %>% 
  kable_styling()



## Clasificación ----------------------------------------------------------

#Mejores variables avg_ingtot, avg_age, age_head, Nper

#Set de validación
validation_clas <- train_personas_2[train_personas_2$id%in%validation_set_hog$id,]%>% 
  select(
    Ingtot,
    id,
    P6040,#años
    P6050,#parentesco jefe de hogar
  ) %>% 
  group_by(id) %>% 
  
  mutate(age_head=ifelse(P6050%in%1,P6040,NA)
         
  ) %>% 
  
  fill(age_head,.direction = 'downup') %>% 
  
  mutate(
    avg_ingtot=mean(Ingtot,na.rm=T),
    avg_age=mean(P6040,na.rm=T)) %>% 
  
  ungroup() %>% 
  select(-Ingtot,-P6040,-P6050) %>% 
  distinct() %>% 
  left_join(train_hogares_2[,c("id","Pobre","Nper")]) %>% 
  filter(complete.cases(.)) %>% 
  mutate(Pobre=as.factor(Pobre)) 



#Set de entrenamiento
train_clas <- train_personas_2[train_personas_2$id%in%training_set_hog$id,]%>% 
  select(
    Ingtot,
    id,
    P6040,#años
    P6050,#parentesco jefe de hogar
  ) %>% 
  group_by(id) %>% 
  
  mutate(age_head=ifelse(P6050%in%1,P6040,NA)
         
  ) %>% 
  
  fill(age_head,.direction = 'downup') %>% 
  
  mutate(
    avg_ingtot=mean(Ingtot,na.rm=T),
    avg_age=mean(P6040,na.rm=T)) %>% 
  
  ungroup() %>%
  select(-Ingtot,-P6040,-P6050) %>% 
  distinct() %>% 
  left_join(train_hogares_2[,c("id","Pobre","Nper")]) %>% 
  filter(complete.cases(.)) %>% 
  mutate(Pobre=as.factor(Pobre)) 


#Remuestreo
train_clas_smote <- recipe(Pobre~avg_ingtot+avg_age+age_head+Nper,
                           data=train_clas) %>% 
  themis::step_smote(Pobre,over_ratio=1) %>% 
  prep() %>% 
  bake(new_data=NULL)


train_clas_undersamp <- recipe(Pobre~avg_ingtot+avg_age+age_head+Nper,
                               data=train_clas) %>% 
  themis::step_downsample(Pobre) %>% 
  prep() %>% 
  bake(new_data=NULL)


#Verificación
prop.table(table(train_clas$Pobre))
prop.table(table(train_clas_smote$Pobre))
prop.table(table(train_clas_undersamp$Pobre))



### Sin resampleo

train_control_clas <- trainControl(method="cv", 
                                   number=5, 
                                   summaryFunction = weighed_fpr_fnr,
                                   allowParallel = T)
#Entrenar probit
clas_probit_noresamp <- train(Pobre~avg_ingtot+avg_age+age_head+Nper, 
                              data = train_clas, 
                              method = "glm", 
                              metric="weighted_fnr_fpr",
                              family = binomial(link="probit"),
                              trControl =train_control_clas)
#Entrenar logit
clas_logit_noresamp <- train(Pobre~avg_ingtot+avg_age+age_head+Nper, 
                             data = train_clas, 
                             method = "glm", 
                             metric="weighted_fnr_fpr",
                             family = binomial(link="logit"),
                             trControl =train_control_clas)



#Control de xgboost
train_control_clas <- trainControl(method="cv", 
                                   number=5, 
                                   summaryFunction = weighed_fpr_fnr,
                                   allowParallel = T)
#Grilla de hiperparametros de xgboost

grid_xgboost_clas <- expand.grid(nrounds = c(250,500),
                                 max_depth = c(4,6,8),
                                 eta = c(0.01,0.3,0.5),
                                 gamma = c(0),
                                 min_child_weight = c(10,25,50),
                                 colsample_bytree = 1,
                                 subsample = 1)


#Entrenar xgboost
xgboost_clas <- train(
  Pobre~avg_ingtot+avg_age+age_head+Nper,
  data=train_clas,
  method = "xgbTree",
  trControl = train_control_clas,
  metric = "weighted_fnr_fpr",
  tuneGrid = grid_xgboost_clas,
  preProcess = c("center", "scale"))



### SMOTE

#Entrenar probit
clas_probit_smote <- train(Pobre~avg_ingtot+avg_age+age_head+Nper, 
                           data = train_clas_smote, 
                           method = "glm", 
                           metric="weighted_fnr_fpr",
                           family = binomial(link="probit"),
                           trControl =train_control_clas)
#Entrenar logit
clas_logit_smote <- train(Pobre~avg_ingtot+avg_age+age_head+Nper, 
                          data = train_clas_smote, 
                          method = "glm", 
                          metric="weighted_fnr_fpr",
                          family = binomial(link="logit"),
                          trControl =train_control_clas)


### Undersampling 

#Entrenar probit
clas_probit_undersamp <- train(Pobre~avg_ingtot+avg_age+age_head+Nper, 
                               data = train_clas_undersamp, 
                               method = "glm", 
                               metric="weighted_fnr_fpr",
                               family = binomial(link="probit"),
                               trControl =train_control_clas)
#Entrenar logit
clas_logit_undersamp <- train(Pobre~avg_ingtot+avg_age+age_head+Nper, 
                              data = train_clas_undersamp, 
                              method = "glm", 
                              metric="weighted_fnr_fpr",
                              family = binomial(link="logit"),
                              trControl =train_control_clas)




# Predicción

pred_clas_probit_noresamp <- predict(clas_probit_noresamp,newdata=validation_clas)
eval_pred_clas_probit_noresamp <- weighed_fpr_fnr(tibble("pred"=pred_clas_probit_noresamp,"obs"=validation_clas$Pobre))

pred_clas_logit_noresamp <- predict(clas_logit_noresamp,newdata=validation_clas)
eval_pred_clas_logit_noresamp <- weighed_fpr_fnr(tibble("pred"=pred_clas_logit_noresamp,"obs"=validation_clas$Pobre))



#smote
pred_clas_probit_smote <- predict(clas_probit_smote,newdata=validation_clas)
eval_pred_clas_probit_smote <- weighed_fpr_fnr(tibble("pred"=pred_clas_probit_smote,"obs"=validation_clas$Pobre))

pred_clas_logit_smote <- predict(clas_logit_smote,newdata=validation_clas)
eval_pred_clas_logit_smote <- weighed_fpr_fnr(tibble("pred"=pred_clas_logit_smote,"obs"=validation_clas$Pobre))


#undersampling
pred_clas_probit_undersamp <- predict(clas_probit_undersamp,newdata=validation_clas)
eval_pred_clas_probit_undersamp <- weighed_fpr_fnr(tibble("pred"=pred_clas_probit_undersamp,"obs"=validation_clas$Pobre))

pred_clas_logit_undersamp <- predict(clas_logit_undersamp,newdata=validation_clas)
eval_pred_clas_logit_undersamp <- weighed_fpr_fnr(tibble("pred"=pred_clas_logit_undersamp,"obs"=validation_clas$Pobre))


#XGboost

pred_clas_xgboost <- predict(xgboost_clas,newdata=validation_clas)
eval_pred_clas_xgboost <- weighed_fpr_fnr(tibble("pred"=pred_clas_xgboost,"obs"=validation_clas$Pobre))



#Reporte de resultados

resultados_clas_fpr_fnr <- tibble(
  "Modelo"=c("Probit",
             "Logit",
             "Probit (SMOTE)",
             "Logit (SMOTE)",
             "Probit (Undersampling)",
             "Logit (Undersampling)",
             "XGBoost"
             
             
  ),
  
  "(0.75)FNR+(0.25)FPR"=c(eval_pred_clas_probit_noresamp[1],
                          eval_pred_clas_logit_noresamp[1],
                          
                          eval_pred_clas_probit_smote,
                          eval_pred_clas_logit_smote,
                          eval_pred_clas_probit_undersamp,
                          eval_pred_clas_logit_undersamp,
                          eval_pred_clas_xgboost
  ),
  
  "Número de variables"=rep(4,7)
  
  
)

#Código latex tabla

resultados_clas_fpr_fnr %>% 
  kable(.,escape = F,format="latex", booktabs=T,linesep = "") %>% 
  kable_styling()

