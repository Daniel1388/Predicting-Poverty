#install.packages("xgboost")
#install.packages("tidyverse")
#install.packages("caret")
#install.packages("groupdata2")
#install.packages("mlr")
library(mlr)
library("xgboost")
library("tidyverse")
library("caret")
require("here")
library(groupdata2)

set.seed(6347)


## 1. Cargue de información--------------------------------------------------------

path <- here()
setwd(path)
train <- read.csv(here("./Data/train.csv"))
test <- read.csv(here("./Data/test.csv"))
val<-read.csv(here("./Data/val.csv"))
test_examen<-read.csv(here("./Data/df_test_examen.csv"))


train_CatPobre <- train$Pobre
test_CatPobre <- test$Pobre
val_CatPobre <- val$Pobre
test_examen_CatPobre <- test_examen$Pobre

train=downsample(train , cat_col = "Pobre")

train=select(train,-Pobre)
test=select(test,-Pobre)
val=select(val,-Pobre)
test_examen=select(test_examen,-Pobre)



train <- train %>%
  rename(Pobre = Ingpcug)

test <- test %>%
  rename(Pobre = Ingpcug)

val <- val %>%
  rename(Pobre = Ingpcug)

test_examen <- test_examen %>%
  rename(Pobre = Ingpcug)

## 2. Entrenamiento XGBoost---------------------------------------------------------

## 2.0 Balance train dataset-------------------------------------

#train=upsample(train , cat_col = "Pobre")

## 2.1. Preparacion train-----------------------------------------------------------

#dim(train)
train_id <- train [,1] 
train_Pobre <- train$Pobre
train_Limit <- train$Lp
train <- train [,-12] 
train <- train [,-1]  

train=select(train,-P6050_1, -P7050_9)



train_xg <- 
  train %>% 
  select(-Pobre) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = train$Pobre)


#train_tune=train

#train_tune$Pobre[train_tune$Pobre == 0]="NP"
#train_tune$Pobre[train_tune$Pobre == 1]="P"
#train_tune$Pobre=as.factor(train_tune$Pobre)

## 2.2. Preparacion test-----------------------------------------------------------

test_id <- test [,1] 
test <- test [,-12] 
test <- test [,-1] 

test_pobre<-test$Pobre
test_Limit <- test$Lp

test=select(test,-P6050_1, -P7050_9)


## 2.3. Preparacion val-----------------------------------------------------------

val_id <- val [,1] 
val <- val [,-12] 
val <- val [,-1] 

val_pobre<-val$Pobre
val_Limit <- val$Lp

val=select(val,-P6050_1, -P7050_9)

val_xg <- 
  val %>% 
  select(-Pobre) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = val$Pobre)


## 2.3.1 Preparacion val exmamen-----------------------------------------------------------

test_examen_id <- test_examen [,1] 
test_examen <- test_examen [,-12] 
test_examen<- test_examen [,-1] 

test_examen_pobre<-test_examen$Pobre
test_examen_Limit <- test_examen$Lp


test_examen=select(test_examen,-P6050_1, -P7050_9)

test_examen_xg <- 
  test_examen %>% 
  select(-Pobre) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = test_examen$Pobre)


## 2.4 Encontrar variables importantes--------------------------------------------


modelo_01 <- xgboost(data = train_xg, 
                     objective = "reg:squarederror",
                     nrounds = 100, max.depth = 10, eta = 0.4, nthread = 2,gamma=0)

predict_01 <- predict(modelo_01, val_xg)

y_test_mean = mean(val_pobre)
# Calculate total sum of squares
tss =  sum((val_pobre - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum((val_pobre - predict_01)^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)

Predictor_df=as.data.frame(cbind(predict_01,val$Lp))

Predictor_df<- Predictor_df %>% mutate(Pobre_hand=ifelse(predict_01<V2,1,0))


cbind(Predictor_df$Pobre_hand ,val_CatPobre) %>% 
  data.frame() %>% 
  table() %>% 
  confusionMatrix()


importance_matrix = xgb.importance(colnames(train_xg), model = modelo_01)

num_val=6

train2=select(
  train,
  importance_matrix$Feature[1:num_val],Pobre 
)

test2=select(
  test,
  importance_matrix$Feature[1:num_val],Pobre 
)

val2=select(
  val,
  importance_matrix$Feature[1:num_val],Pobre 
)

test_examen2=select(
  test_examen,
  importance_matrix$Feature[1:num_val],Pobre 
)


train_xg2 <- 
  train2 %>% 
  select(-Pobre) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = train2$Pobre)

val_xg2 <- 
  val2 %>% 
  select(-Pobre) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = val2$Pobre)

test_examen_xg2 <- 
  test_examen2 %>% 
  select(-Pobre) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = test_examen2$Pobre)


## 2.5. Tuning de hiper parametros-----------------------------------------

xgbGrid = expand.grid(
  nrounds = 10,
  eta = c(0.4, 0.3,0.2),
  max_depth = c(2, 4,6),
  gamma=c(0, 1), 
  colsample_bytree=0.6, 
  min_child_weight=1, 
  subsample=0.6
)


xgb_trcontrol_1 = trainControl(
  classProbs = TRUE,
)

#str(train)
Model_02 <- caret::train(
  as.factor(Pobre) ~ ., 
  data = train_tune,
  method = "xgbTree",
  metric = "Accuracy", 
  trControl = xgb_trcontrol_1,
  preProc = c("center", "scale"),
  tuneGrid = xgbGrid)


Tune_Grid=Model_02$bestTune
Tune_Grid$nrounds=100



## 2.6 Pronostico final--------------------------------------------


modelo_Final <- xgboost(data = train_xg2, 
                        objective = "reg:squarederror",
                        nrounds = 100, max.depth = 4, eta = 0.4, nthread = 2,gamma=0)

predict_val <- predict(modelo_Final , val_xg2)


y_test_mean = mean(val_pobre)
# Calculate total sum of squares
tss =  sum((val_pobre - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum((val_pobre - predict_val)^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)

Predictor_df_val=as.data.frame(cbind(predict_val,val$Lp))

Predictor_df_val<- Predictor_df %>% mutate(Pobre_hand=ifelse(predict_val<V2,1,0))


cbind(Predictor_df_val$Pobre_hand ,val_CatPobre) %>% 
  data.frame() %>% 
  table() %>% 
  
  confusionMatrix()

F_obj=cbind(Predictor_df_val$Pobre_hand, val_CatPobre) %>% data.frame() %>% table() 
F_obj=0.75*(F_obj[1,2]/(F_obj[1,2]+F_obj[2,2]))+0.25*(F_obj[2,1]/(F_obj[2,1]+F_obj[1,1]))#+log10(num_val)
F_obj


## 3. Generacion de Recomendaión--------------------------------------------------------------------

predict_exm_reg <- predict(modelo_Final , test_examen_xg2)

Predictor_df_exam=as.data.frame(cbind(predict_exm_reg,test_examen$Lp))

Predictor_df_exam<- Predictor_df_exam %>% mutate(Pobre_hand=ifelse(predict_exm_reg<V2,1,0))

F_obj_exm=cbind(Predictor_df_exam$Pobre_hand, test_examen_pobre) %>% data.frame()  

TE=cbind(test_examen_id , F_obj_exm$V1)%>% data.frame() 
colnames(TE) <- c('id','regression_model')

TE$regression_model=as.numeric(TE$classification_model)
write.csv(TE,"./Data/output_exam_Regression.csv", row.names = FALSE)