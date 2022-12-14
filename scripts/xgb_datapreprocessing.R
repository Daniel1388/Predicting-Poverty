#install.packages("fastDummies")
#install.packages("caTools")
require("here")
require("tidyverse")
require("fastDummies")
library(caTools)
set.seed(6347)

## 1. Lectura de Datos------------------------------------------------------

path <- here()
setwd(path)
train_hogares <- readRDS(here("./stores/train_hogares.Rds"))
test_hogares <- readRDS(here("./stores/test_hogares.Rds"))
train_personas<-readRDS(here("./stores/train_personas.Rds"))
test_personas<-readRDS(here("./stores/test_personas.Rds"))
Val_team<-read.csv(here("./stores/validation_set_id.csv"))

#data.frame(colnames(train_hogares))


sapply(test_personas, function(x) sum(is.na(x))) ## Nulos
#train_personas$Ingtot  - Variable objetivo en personas
#train_hogares$Ingtotug - Variable objetivo en hogares

sum_ingresos<-train_personas %>% group_by(id) %>% summarize(Ingtot_hogar=sum(Ingtot,na.rm = TRUE)) 
summary(sum_ingresos)

## 2. Validación -------------------------------------------------------------------------

train_hogares<-left_join(train_hogares,sum_ingresos)
#colnames(train_hogares)

head(train_hogares[c("id","Ingtotug","Ingtot_hogar")])


## 3. Calculo de Probreza----------------------------------------------------------------------

table(train_hogares$Pobre)

train_hogares<- train_hogares %>% mutate(Pobre_hand=ifelse(Ingpcug<Lp,1,0))
table(train_hogares$Pobre,train_hogares$Pobre_hand)

train_hogares<- train_hogares %>% mutate(Pobre_hand_2=ifelse(Ingtotugarr<Lp*Npersug,1,0))
table(train_hogares$Pobre,train_hogares$Pobre_hand_2)


## 4. Union y alineacion de variables test y control----------------------------------------------------------------------

train_hogares$status<- "train"
test_hogares$status<- "test"
test_hogares$Pobre<- 0
test_hogares$Ingpcug<- 0

#View(data_hogares)

data_hogares <- union(train_hogares[ , (names(train_hogares) %in% colnames(test_hogares))], test_hogares)
data_personas <- union(train_personas[ , (names(train_personas) %in% colnames(test_personas))], test_personas)
data_hogares[is.na(data_hogares)] <- 0
data_personas[is.na(data_personas)] <- 0
#data.frame(colnames(data_hogares))
#View(train_hogares)

## 5. Generación de variables Dummy hogares------------------------------


#str(data_hogares)

df_hogares=select(
          data_hogares,
          id,
          Clase,
          P5000,
          P5010,
          P5100,
          P5130,
          P5140,
          Nper,
          Npersug,
          Li,
          Lp,
          status,
          Pobre,
          Ingpcug
        )



cols <- c("Dominio", "P5090","Depto")
dummy_hogares=dummy_cols(data_hogares[cols], remove_selected_columns=TRUE)
#View(df_hogares)

df_hogares=(cbind(df_hogares,dummy_hogares))


## 6. Agregación de tabla personas--------------------------------

id=data_personas$id

## 6.1 Variables Agregadas por Suma

temp_dummy_sum=select(
              data_personas,
              P6510,P6545,P6580,P6585s1,P6585s2,P6585s3,P6585s4,P6590,P6600,
              P6610,P6620,P6630s1,P6630s2,P6630s3,P6630s4,P6630s6,P7040,P7110,
              P7120,P7150,P7160,P7310,P7422,P7472,P7495,P7500s2,P7500s3,P7505,
              P7510s1,P7510s2,P7510s3,P7510s5,P7510s6,P7510s7
              )

temp_dummy_sum=replace(temp_dummy_sum, temp_dummy_sum>2, 0)

temp_dummy_sum2=select(
    data_personas,
    P6050,P6100,P6210,P6240,Oficio,P6430,P6920,P7050,P7350,P6020,Clase
)


temp_dummy_sum=(cbind(temp_dummy_sum,temp_dummy_sum2))


cols <- c("P6510","P6545","P6580","P6585s1","P6585s2","P6585s3","P6585s4","P6590","P6600",
          "P6610","P6620","P6630s1","P6630s2","P6630s3","P6630s4","P6630s6","P7040","P7110",
          "P7120","P7150","P7160","P7310","P7422","P7472","P7495","P7500s2","P7500s3","P7505",
          "P7510s1","P7510s2","P7510s3","P7510s5","P7510s6","P7510s7",
          "P6050","P6100","P6210","P6240","Oficio","P6430","P6920","P7050","P7350","P6020","Clase"
          
          )


temp_dummy_sum<-data.frame(lapply(temp_dummy_sum,factor))
temp_dummy_sum=dummy_cols(temp_dummy_sum[cols], remove_selected_columns=TRUE)

temp_sum=select(data_personas,
                P6090,P6426,P6800,P7045,P7090,Pet,Oc,Des,Ina)

temp_dummy_sum=(cbind(id,temp_dummy_sum,temp_sum))

final_dummy_sum=aggregate(.~ id, data = temp_dummy_sum, FUN = sum, na.rm = TRUE)




## 6.2 Variables Agregadas por AVG

temp_dummy_avg=select(data_personas,P6210s1)
cols <- c("P6210s1")




temp_dummy_avg<-data.frame(lapply(temp_dummy_avg,factor))
temp_dummy_avg=dummy_cols(temp_dummy_avg[cols], remove_selected_columns=TRUE)

temp_avg=select(data_personas,
                P6800,P7045,P6040
)

temp_dummy_avg=(cbind(id,temp_dummy_avg,temp_avg))

final_dummy_avg=aggregate(.~ id, data = temp_dummy_avg, FUN = mean, na.rm = TRUE)

names(final_dummy_avg)[names(final_dummy_avg)=="P6800"]="P6800_avg"
names(final_dummy_avg)[names(final_dummy_avg)=="P7045"]="P7045_avg"


df_final<-left_join(df_hogares,final_dummy_sum)
df_final<-left_join(df_final,final_dummy_avg)


## 7. selección de grupo test y validacion


temp_df_train=df_final[df_final$status=="train",]
df_test_examen=df_final[df_final$status=="test",]

temp_df_train2=temp_df_train %>%filter(!id %in% Val_team$id)
df_val=filter(temp_df_train, id %in% Val_team$id)

#use 70% of dataset as training set and 30% as test set
sample <- sample.split(temp_df_train2$status, SplitRatio =0.8)
df_train  <- subset(temp_df_train2, sample == TRUE)
df_test   <- subset(temp_df_train2, sample == FALSE)

#sample2 <- sample.split(df_train2$status, SplitRatio =0.8)

#df_train  <- subset(df_train2, sample2 == TRUE)
#df_test   <- subset(df_train2, sample2 == FALSE)

path <- here()
setwd(path)

#write.csv(df_train,"./stores/train.csv", row.names = FALSE)
#write.csv(df_val,"./stores/val.csv", row.names = FALSE)
#write.csv(df_test,"./stores/test.csv", row.names = FALSE)
#write.csv(df_test_examen,"./stores/df_test_examen.csv", row.names = FALSE)

saveRDS(df_train, file = "./stores/train.rds")
saveRDS(df_val, file = "./stores/val.rds")
saveRDS(df_test, file = "./stores/test.rds")
saveRDS(df_test_examen, file = "./stores/df_test_examen.rds")





