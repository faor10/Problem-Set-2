##Predicción utilizando la estimación del ingreso total
##El código se encuentra divido de la siguiente manera:
#Librerias requeridas
#Carga de datos y tratamiento
#Modelo 1 
#Modelo 2 
#Modelo 3 
#Modelo 4
#Modelo 5
#Modelo 6
# Modelo 7
#   Modelo 7 - Predicciones
#   Modelo 7 - Estimación de FP y FN


##Librerias requeridas
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

#Carga de datos y tratamiento de datos
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


sum(is.na(train_personas$P5140))


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


#Modelo 1 Tradicional Personas****************************************

modelo1 <- lm(Ingtot ~ P6040 + P60402 + P6210 + Oficio + P6800, data = db_trainper)
summary(modelo1)

ing_predicho_test<-predict(modelo1,newdata=db_testper)

model1 <- train(Ingtot ~ P6040 + P60402 + P6210  + Oficio +P6800 ,
                # model to fit
                data = db_trainper,
                trControl = trainControl(method = "cv", number = 5), method = "lm")

df_coeficientes <- modelo1$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo OLS") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 45))

#Modelo 2 Ridge Personas****************************************

x_train <- model.matrix(Ingtot ~ P6040 +P60402+ P6210 + Oficio  + P6800, data = db_trainper)[, -1]
y_train <- db_trainper$Ingtot



#scale(x_train)
model2_ridge <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  nlambda     = 100,
  standardize = T
)

regularizacion <- model2_ridge$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = model2_ridge$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")

cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = T
)

plot(cv_error)
paste("Mejor valor de lambda encontrado:", cv_error$lambda.min)
paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error$lambda.1se)

modelo2 <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  lambda      = cv_error$lambda.min,
  standardize = TRUE
)

df_coeficientes <- coef(modelo2) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo Ridge") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

#Modelo 3 Lasso Personas****************************************

x_train <- model.matrix(~ P6040 +P60402+ P6210  + Oficio + P6800, db_trainper)
y_train <- db_trainper$Ingtot

x_test<- model.matrix(~ P6040 + P60402 +P6210  + Oficio  + P6800, db_testper)


modelo3_lasso <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  nlambda     = 100,
  standardize = TRUE
)

regularizacion <- modelo3_lasso$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo3_lasso$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")


cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error)
paste("Mejor valor de lambda encontrado:", cv_error$lambda.min)
paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error$lambda.1se)


modelo3 <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda      = cv_error$lambda.min,
  standardize = TRUE
)

# Coeficientes del modelo
# ==============================================================================
df_coeficientes <- coef(modelo3) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo Lasso") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

ing_predicho<-data.frame()
ing_predicho<-predict(modelo3,newx=x_test)
ingtot_pred<- cbind(ing_predicho, db_testper$id)
ingtot_pred<-as.data.frame(ingtot_pred)
ingtot_pred$s0 <- as.numeric(ingtot_pred$s0)
aggregate<-aggregate(s0 ~ V2, data=ingtot_pred, FUN=sum)

aggregate<-rename(aggregate, ingtot = s0)
aggregate<-rename(aggregate, id = V2)



db_testhog2<-as.data.frame(db_testhog)
total <- merge(aggregate,db_testhog2,by="id")
total<- total %>% mutate(ingtotper=ingtot/Npersug)
total <- total[c("id","ingtot","ingtotper","Npersug","Lp")]
total$Pobre_ingtot<-ifelse(total$ingtotper<total$Lp,1,0)



#Modelo 4 Hogares****************************************

modelo4 <- lm(Ingtotugarr  ~ P5000+	P5090 +Arriendo+	Npersug, data = db_trainhog)
summary(modelo4)

model4 <- train(Ingtotugarr ~ P5000 +	P5090 +Arriendo +	Npersug  ,
                # model to fit
                data = db_trainhog,
                trControl = trainControl(method = "cv", number = 5), method = "lm")


df_coeficientes <- modelo4$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo OLS") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 45))

#Modelo 5 Ridge Hogares****************************************

x_train <- model.matrix(Ingtotugarr ~ P5000 +	P5090 + Arriendo +	Npersug, data = db_trainhog)[, -1]
y_train <- db_trainhog$Ingtotugarr



#scale(x_train)
model5_ridge <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  nlambda     = 100,
  standardize = T
)

regularizacion <- model5_ridge$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = model5_ridge$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")

cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = T
)

plot(cv_error)
paste("Mejor valor de lambda encontrado:", cv_error$lambda.min)
paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error$lambda.1se)

modelo5 <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  lambda      = cv_error$lambda.1se,
  standardize = TRUE
)

df_coeficientes <- coef(modelo5) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo Ridge") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

#Modelo 6 Lasso Hogares****************************************

x_train <- model.matrix(Ingtotugarr ~ P5000 +	P5090 +Arriendo +	Npersug, data = db_trainhog)[, -1]
y_train <- db_trainhog$Ingtotugarr

modelo6_lasso <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  nlambda     = 100,
  standardize = TRUE
)

regularizacion <- modelo6_lasso$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo6_lasso$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")


cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error)
paste("Mejor valor de lambda encontrado:", cv_error$lambda.min)
paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error$lambda.1se)


modelo6 <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda      = cv_error$lambda.1se,
  standardize = TRUE
)

df_coeficientes <- coef(modelo6) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo Lasso") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))



#Modelo 7 merge entre personas y hogares (Mejor modelo escogido)****************************************

traintotal <- merge(db_trainhog,db_trainper,by="id")
testtotal <- merge(db_testhog,db_testper,by="id")

x_train <- model.matrix(~ P6040 + P60402 + P6210 +  Oficio + P6800 + Arriendo, traintotal)
y_train <- traintotal$Ingtot
x_test<- model.matrix(~ P6040 +P60402+ P6210 +Oficio  + P6800+ Arriendo, testtotal)

modelo7_lasso <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  nlambda     = 100,
  standardize = TRUE
)


regularizacion <- modelo7_lasso$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo7_lasso$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")


cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error)
paste("Mejor valor de lambda encontrado:", cv_error$lambda.min)
paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error$lambda.1se)


modelo7 <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda      = cv_error$lambda.min,
  standardize = TRUE
)


# Coeficientes del modelo
df_coeficientes <- coef(modelo7) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo Lasso") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

#Predicciones del Modelo 7

ing_predicho<-data.frame()
ing_predicho<-predict(modelo7,newx=x_test)
length(ing_predicho)
ingtot_pred<- cbind(ing_predicho, testtotal$id)
ingtot_pred<-as.data.frame(ingtot_pred)
ingtot_pred$s0 <- as.numeric(ingtot_pred$s0)
ingtot_pred<-aggregate(s0 ~ V2, data=ingtot_pred, FUN=sum)
ingtot_pred<-rename(ingtot_pred, ingtot = s0)
ingtot_pred<-rename(ingtot_pred, id = V2)
db_testhog_pred<-as.data.frame(db_testhog)
total <- merge(ingtot_pred,db_testhog_pred,by="id")
total<- total %>% mutate(ingtotper=ingtot/Npersug)
total <- total[c("id","ingtot","ingtotper","Npersug","Lp")]
total$Pobre_ingtot<-ifelse(total$ingtotper<total$Lp,1,0)
write.csv(total,"D:/OneDrive - Universidad de los Andes/Intersemestral 2/Big Data/Taller2/result_pred.csv", row.names = FALSE)


#Evaluacion del modelo 7 para encontrar FN y FP

traintotal <- merge(db_trainhog,db_trainper,by="id")

#traintotal<- traintotal %>% mutate(Arriendo = P5130+P5140)
#traintotal<- traintotal %>% mutate(P60402 = P6040^2)

set.seed(156)
split1 <- createDataPartition(traintotal$Pobre, p = .7)[[1]]
length(split1)

test <- traintotal[-split1,]
training <- traintotal[split1,]

#Modelo 7 Lasso


x_train <- model.matrix(~ P6040 + P60402 + P6210 + Oficio + P6800 + Arriendo, training)
y_train <- training$Ingtot

x_test<- model.matrix(~ P6040 +P60402+ P6210 +Oficio  + P6800+ Arriendo, test)


modelo7_lasso <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  nlambda     = 100,
  standardize = TRUE
)

regularizacion <- modelo7_lasso$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo7_lasso$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")


cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error)
paste("Mejor valor de lambda encontrado:", cv_error$lambda.min)
paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error$lambda.1se)


modelo7 <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda      = cv_error$lambda.min,
  standardize = TRUE
)

# Coeficientes del modelo

df_coeficientes <- coef(modelo7) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo Lasso") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))


ing_predicho<-data.frame()
ing_predicho<-predict(modelo7,newx=x_test)
length(ing_predicho)

test_pred<- cbind(ing_predicho, test$id, test$Pobre, test$Lp, test$Npersug)
test_pred<-as.data.frame(test_pred)
test_pred$s0 <- as.numeric(test_pred$s0)
test_pred<-aggregate(s0 ~ V2 + V3 + V4 + V5, data=test_pred, FUN=sum)
test_pred<-rename(test_pred, ingtot = s0)
test_pred<-rename(test_pred, id = V2)
test_pred<-rename(test_pred, Pobre = V3)
test_pred<-rename(test_pred, Lp = V4)
test_pred<-rename(test_pred, Npersug = V5)
test_pred$Npersug <- as.numeric(test_pred$Npersug)
test_pred<- test_pred %>% mutate(ingtotper=ingtot/Npersug)
test_pred$Pobre_pred<-ifelse(test_pred$ingtotper<test_pred$Lp,1,0)

with(test_pred,table(Pobre,Pobre_pred))





