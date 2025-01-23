# ORTIMAR - Objetivo 2
# Comparación medio natural
# Enero 2025 - revision para publicacion

# Marcadores de estado oxidativo, diferencias de base entre tres entornos naturales

### SETUP ----
library(tidyverse)
library(multcompView)
library(car)
library(rstatix)

setwd("C:/Users/Usuario/Documents/GitHub/Revision_datos_TFG") # Lab
source(file = "./scripts septiembre 2024-2025/0_data_lab.R")
source(file = "./scripts septiembre 2024-2025/1_funciones_graficas.R")

data_1 <- filter(datos, cultivo == "wild")

### Exploracion ----
summ <- data_1 %>% 
  group_by(playa) %>% 
  get_summary_stats(G6PDH_t,type = "mean_se")

ggplot(summ, aes(x = playa)) +
  geom_col(aes(y = mean, fill = playa, color = playa), alpha = 0.6) +
  geom_errorbar(aes(ymax = mean+se, ymin = mean-se, color = playa), linewidth = 1, width = 0.4) +
  labs(title = "Actividad G6PDH_t por playa") +
  theme(legend.position = "none")

ggplot(data_1, aes(x = playa, y = GPx_p)) +
  geom_boxplot(aes(fill = playa, color = playa), alpha = 0.3, linewidth = 1) +
  geom_point(aes(color = playa), position = position_jitterdodge(), size = 2) +
  labs(title = "Actividad GPx_p por playa") +
  theme(legend.position = "none")


data_1$G6PDH_t[12] <- NA # Resuelve la normalidad de residuos


### Ajuste de modelos ----

# Hacemos anova de 1 via si cumple asunciones
modelos <- lapply(colnames(data_1[c(5:24)]), function(x){
  aov(formula = as.formula(paste0(x, " ~ playa")), data_1)})

# Normalidad de residuos
sapply(modelos, function(x){
  print(x$terms[[2]])
  shapiro.test(residuals(x))}) 
# Sin hacer ningun filtrado de outliers:
# G6PDH_t no cumple normalidad

# Con un outlier fuera se arregla

# Homogeneidad de varianzas
sapply(modelos, function(x){
  print(leveneTest(x))})


### RESULTADOS DEL ANOVA ----
for (i in c(1:20)) {
  print(colnames(data_1[5:24][i]))
  print(summary(modelos[[i]]))}


modelos <- lapply(colnames(data_1[c(5:24)]), function(x){
  anova_test(formula = as.formula(paste0(x, " ~ playa")), data_1)})




mshapiro_test(data_1[c(5:24)])

# Protocolo para hacer multiples anovas con rstatix
modelos <- lapply(colnames(data_1[c(5:24)]), function(x){
  anova_test(formula = as.formula(paste0(x, " ~ playa")), data_1)})

(anova_results <- reduce(modelos, full_join) %>% 
  add_column(.before = 1, variable = colnames(data_1[c(5:24)])) %>% 
  adjust_pvalue(method = "BH"))


# Test de Levene se puede computar igual
modelos_levene <- lapply(colnames(data_1[c(5:24)]), function(x){
  levene_test(formula = as.formula(paste0(x, " ~ playa")), data_1)})

(levene_results <- reduce(modelos_levene, full_join) %>% 
    add_column(.before = 1, variable = colnames(data_1[c(5:24)])))

modelos_shapiro <- lapply(data_1[c(5:24)], function(x){
  shapiro_test(x, data_1)})
(shapiro_results <- reduce(modelos_shapiro, full_join) %>% 
    add_column(.before = 1, parametro = colnames(data_1[c(5:24)])))
