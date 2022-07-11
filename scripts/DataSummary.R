##En este código se obtienen las estadisticas descriptivas de los datos utilizados
##en el PS2

rm(list=ls())
require("pacman")
p_load("here")
p_load("readr")
p_load(ggplot2) # Librería para visualizar datos
p_load(scales) # Formato de los ejes en las gráficas
p_load(ggpubr) # Combinar gráficas
p_load(rio) # Librería para importar datos 
p_load(tidyverse) # Librería para limpiar datos
p_load(e1071) # Tiene la función para calcular skewness
p_load(EnvStats) # Transformación Box-Cox
p_load(tidymodels) # Modelos ML
p_load(ggplot2) # Librería para visualizar datos
p_load(scales) # Formato de los ejes en las gráficas
p_load(ggpubr) # Combinar gráficas
p_load(knitr) # Tablas dentro de Rmarkdown
p_load(kableExtra) # Tablas dentro de Rmarkdown
p_load(dplyr)
p_load(caret)
p_load(glmnet)
p_load(pls)
p_load(tidyr)
p_load(tibble)
p_load(gtsummary)

#Carga de datos
train_personas<-readRDS(here("D:/OneDrive - Universidad de los Andes/Intersemestral 2/Big Data/Taller2/data/train_personas.Rds"))
head(train_personas[c("Ingtot","Oficio")])
test_personas<-readRDS(here("D:/OneDrive - Universidad de los Andes/Intersemestral 2/Big Data/Taller2/data/test_personas.Rds"))
head(test_personas[c("Oficio")])
train_hogares<-readRDS(here("D:/OneDrive - Universidad de los Andes/Intersemestral 2/Big Data/Taller2/data/train_hogares.Rds"))
test_hogares<-readRDS(here("D:/OneDrive - Universidad de los Andes/Intersemestral 2/Big Data/Taller2/data/test_hogares.Rds"))

db_trainper <- train_personas
db_testper <- test_personas
db_trainhog <- train_hogares
db_testhog <- test_hogares

train_personas <- train_personas %>%
  filter(Ingtot != "")
train_personas <- train_personas %>%
  filter(P6800 != "")


sum(is.na(db_trainper$Oficio))


db_trainper$Ingtot[is.na(db_trainper$Ingtot)] <- 0
db_trainper$Oficio[is.na(db_trainper$Oficio)] <- 0
db_trainper$P6210[is.na(db_trainper$P6210)] <- 9
db_trainper$P6430 [is.na(db_trainper$P6430)] <- 9
db_trainper$P6800 [is.na(db_trainper$P6800)] <- 0
db_trainhog$P5130 [is.na(db_trainhog$P5130)] <- 0
db_trainhog$P5140  [is.na(db_trainhog$P5140 )] <- 0

db_testper$Oficio[is.na(db_testper$Oficio)] <- 0
db_testper$P6210[is.na(db_testper$P6210)] <- 9
db_testper$P6430 [is.na(db_testper$P6430)] <- 9
db_testper$P6800 [is.na(db_testper$P6800)] <- 0
db_testhog$P5130 [is.na(db_testhog$P5130)] <- 0
db_testhog$P5140 [is.na(db_testhog$P5140)] <- 0

db_trainper<- db_trainper %>% mutate(P60402 = P6040^2)
db_testper<- db_testper %>% mutate(P60402 = P6040^2)
db_trainhog<- db_trainhog %>% mutate(Arriendo = P5130+P5140)
db_testhog<- db_testhog %>% mutate(Arriendo = P5130+P5140)


#Ver frecuencia de datos
ggplot() +
  geom_bar(data = db_trainper, aes(x = P6430 ), 
           fill = "darkred", alpha = 0.5) +
  labs(x = "Estrato", y = "Frequencia") + 
  theme_classic()

ggplot() +
  geom_bar(data = db_trainhog, aes(x =P5130 ), 
           fill = "darkred", alpha = 0.5) +
  labs(x = "Estrato", y = "Frequencia") + 
  theme_classic()


#Conversion a factores

db_trainper <- db_trainper %>%
  mutate_at(.vars = c(
    "P6210", "Oficio", "P6430"),
    .funs = factor)

db_testper <- db_testper %>%
  mutate_at(.vars = c(
    "P6210", "Oficio", "P6430"),
    .funs = factor)

db_trainhog <- db_trainhog %>%
  mutate_at(.vars = c(
    "P5090"),
    .funs = factor)

db_testhog <- db_testhog %>%
  mutate_at(.vars = c(
    "P5090"),
    .funs = factor)

traintotal <- merge(db_trainhog,db_trainper,by="id")
testtotal <- merge(db_testhog,db_testper,by="id")

trial2 <- traintotal %>% select(Pobre,Ingtot,Ingtotugarr,P60402,P6040, Arriendo, P6800, P5000,P5010, Npersug, Nper, Fex_c.x, Fex_dpto.x)
trial2 %>% tbl_summary(by=Pobre,statistic = list(all_continuous() ~ "{mean} ({sd})",
                                        all_categorical() ~ "{n} / {N} ({p}%)"),
                       label = list(P60402 ~ "Edad^2",  P6040 ~ "Edad", P6800~"HorasTrabajadas",
                                    P5000 ~ "CuartosHogar",P5010 ~ "CuartosUsad", Npersug ~ "NPerUG"
                                    )
                       )
trialcat <- traintotal %>% select(Oficio, P6210, P5090)
trialcat %>% tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                        all_categorical() ~ "{n} / {N} ({p}%)")
            
                       )
)

table(traintotal$Oficio)

