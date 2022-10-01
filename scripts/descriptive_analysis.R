library(ggplot2)#graficar
library(tidyverse)#organizar datos
library(xtable)#tablas
library(openxlsx)#lectura xlsx
library(janitor)#tabla de frecuencia
library(here)

# SOURCE SCRIPT DE FUNCIONES ----------------------------------------------


source("funcion_descriptivas.R")

# LECTURA DE DATOS --------------------------------------------------------

train_hogares<-readRDS(here("data","train_hogares.Rds"))
train_personas<-readRDS(here("data","train_personas.Rds"))

test_hogares<-readRDS(here("data","test_hogares.Rds"))
test_personas<-readRDS(here("data","test_personas.Rds"))


#Seleccionar variables que están en test.
train_personas_2 <- train_personas %>% select(colnames(test_personas),Ingtot,-contains("Fex"),
                                              Depto,Clase)
train_hogares_2 <- train_hogares %>% select(colnames(test_hogares),Pobre)

train_personas_2_subset <- 
  train_personas_2%>% 
  select(
    Ingtot,
    P6100,
    P6020,#genero
    P6040,#años
    P6050,#parentesco jefe de hogar
    P6210,#nivel educativo
    P6800,#HORAS DE TRABAJO#200K NAS
    P6870,#PERSOANS EN LA EMPRESA#200K NAS
    P6920,#COTIZACION PENSIONES, 200K NAS
    P7495,#recibio pago de arrendamiento o pension
    P7500s3
  )

train_personas_2_subset <- train_personas_2_subset %>% 
  mutate_at(vars(-contains(c("Ingtot","P6040","P6800"))),as.factor)

train_hog_2_subset <- 
  train_hogares_2%>% 
  select(
    Pobre,
    P5130,
    P5010,
    Nper
  )

train_hog_2_subset <- train_hog_2_subset %>% 
  mutate_at(vars(-contains(c("Nper","5130","5010"))),as.factor)



#Función para descriptivas de variables continuas
descriptives_table_cont <- function(x){
  x=as.numeric(x)
  mean=mean(x,na.rm=T)
  median=median(x,na.rm=T)
  sd=sd(x,na.rm=T)
  pct90=quantile(x,0.9,na.rm=T)
  return(c(mean,median,sd,pct90))
  
}

#Función para descriptivas de variables categoricas
descriptives_table_cat <- function(x){
  x=as.numeric(x)
  mean=mean(x,na.rm=T)
  return(c(mean))
  
}


#Creación de la tabla de de descriptivas Output es código latex
createDescriptiveTable(list("Individuos"=train_personas_2_subset,"Hogares"=train_hog_2_subset),
                       summary_function = descriptives_table_cont,
                       column_names=c("Media","Mediana","D.E.","Percentil 90"),
                       variable_names=list(c("Ingtot","P6040","P6800"),c("P5130","P5010","Nper")),
                       variable_labels=list(c("Ingreso total","Edad","Horas Trabajo Semana"),c("Estim. Arrendamiento","Núm. Cuartos","Núm. Personas")),
                       arraystretch=1.3,
                       title="Estadísticas descriptivas variables continuas",
                       label="tab:descriptive_cont",
)


#CATEGÓRICAS

createDescriptiveTable(list("Individuos"=train_personas_2_subset,"Hogares"=train_hog_2_subset),
                       summary_function = descriptives_table_cat,
                       column_names=c("Media"),
                       variable_names=list(c("P6100","P6020","P6050","P6210","P6870","P6920","P7495","P7500s3"),c("Pobre")),
                       variable_labels=list(c("Reg. Seg. Soc.",
                                              "Género",
                                              "Parent. Jefe de Hog.",
                                              "Nivel Educ.","Núm. Emple. Emp.",
                                              "Formal","Recibe Arrend.",
                                              "Recibe Cuot. Aliment"),c("Pobre")),
                       arraystretch=1.3,
                       title="Estadísticas descriptivas variables categóricas individuos",
                       label="tab:descriptive_cat_1",
)

