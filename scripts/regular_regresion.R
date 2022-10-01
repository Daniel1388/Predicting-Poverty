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


train_hogares<-readRDS(here("stores","train_hogares.Rds"))
train_personas<-readRDS(here("stores","train_personas.Rds"))

test_hogares<-readRDS(here("stores","test_hogares.Rds"))
test_personas<-readRDS(here("stores","test_personas.Rds"))


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

#Preparación datos

validation_regular <- train_personas_2[train_personas_2$id%in%validation_set_hog$id,]%>% 
  select(
    Ingtot,
    id,
    P7495,
    P6870
    
  ) %>% 
  left_join(train_hogares_2[,c("id","Pobre","Nper","Npersug")]) %>% 
  filter(complete.cases(.)) %>% 
  mutate(Pobre=as.factor(Pobre),
         P7495=as.factor(P7495),
         P6870=as.factor(P6870)
  ) 




train_regular <- train_personas_2[train_personas_2$id%in%training_set_hog$id,]%>% 
  select(
    Ingtot,
    id,
    P7495,
    P6870
    
  ) %>% 
  left_join(train_hogares_2[,c("id","Pobre","Nper","Npersug")]) %>% 
  filter(complete.cases(.)) %>% 
  mutate(Pobre=as.factor(Pobre),
         P7495=as.factor(P7495),
         P6870=as.factor(P6870)
         
  ) 


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


# TRAIN -------------------------------------------------------------------


#Entrenamiento

reg_lr_ridge <- lm(Ingtot~P7495+Nper+P6870+Npersug, 
                   train_regular)


pred_reg_lr_ridge <- predict(reg_lr_ridge,validation_regular)
rmse_reg_lr_ridge <- postResample(pred = pred_reg_lr_ridge, obs = validation_regular$Ingtot)
rmse_reg_lr_ridge[1]



#Predicción pobreza


pobre_reg_lr_ridge <- tibble("id"=validation_regular$id,"ingtot_pred"=pred_reg_lr_ridge) %>% 
  group_by(id) %>% 
  mutate(Ingtotug_pred=sum(ingtot_pred,na.rm=T)) %>% 
  select(-ingtot_pred) %>% 
  distinct() %>% 
  left_join(validation_set_hog[,c("id","Nper","Lp","Pobre")]) %>% 
  mutate(ingpcug_pred=Ingtotug_pred/Nper,
         pobre_pred=ifelse(ingpcug_pred<Lp,1,0)
  )



reg_lr_ridge_weighed_fpr_fnr <- weighed_fpr_fnr(tibble("pred" = pobre_reg_lr_ridge$pobre_pred, "obs" = pobre_reg_lr_ridge$Pobre))


#Entrenamiento

reg_lr_lasso <- lm(Ingtot~P7495+Nper+P6870, 
                   train_regular)


pred_reg_lr_lasso <- predict(reg_lr_lasso,validation_regular)
rmse_reg_lr_lasso <- postResample(pred = pred_reg_lr_lasso, obs = validation_regular$Ingtot)
rmse_reg_lr_lasso[1]


#Predicción pibreza
pobre_reg_lr_lasso <- tibble("id"=validation_regular$id,"ingtot_pred"=pred_reg_lr_lasso) %>% 
  group_by(id) %>% 
  mutate(Ingtotug_pred=sum(ingtot_pred,na.rm=T)) %>% 
  select(-ingtot_pred) %>% 
  distinct() %>% 
  left_join(validation_set_hog[,c("id","Nper","Lp","Pobre")]) %>% 
  mutate(ingpcug_pred=Ingtotug_pred/Nper,
         pobre_pred=ifelse(ingpcug_pred<Lp,1,0)
  )



#Resultados


reg_lr_lasso_weighed_fpr_fnr <- weighed_fpr_fnr(tibble("pred" = pobre_reg_lr_lasso$pobre_pred, "obs" = pobre_reg_lr_lasso$Pobre))


resultados_reg_regularizacio_fpr_fnr <- tibble(
  "Modelo"=c("Regresión Lineal (Especificación Ridge)",
             "Regresión Lineal (Especificación Lasso)"),
  
  "(0.75)FNR+(0.25)FPR"=c(reg_lr_ridge_weighed_fpr_fnr[1],
                          reg_lr_lasso_weighed_fpr_fnr[1]),
  
  "Número de variables"=c(4,3)
  
  
)



resultados_reg_regularizacio_fpr_fnr%>% 
  kable(.,escape = F,format="latex", booktabs=T,linesep = "") %>% 
  kable_styling()
