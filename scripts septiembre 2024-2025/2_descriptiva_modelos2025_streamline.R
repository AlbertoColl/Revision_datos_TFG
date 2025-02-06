# ORTIMAR - Objetivo 2
# Analisis efecto del corte
# Enero 2025 - revision para publicacion

# Vamos a hacer un diseño cuadrado 2x2 dentro de las anemonas cultivadas: la variable corte, que tiene 2 niveles (dissected y control), y la variable tiempo, con niveles 0 y 1. De esa manera esperamos ver el efecto del corte a corto y largo plazo.

# H0: No hay diferencias entre el estado oxidativo de anémonas control y cortadas. H1: Sí existen alguna diferencia.

# H0: No hay interacción entre el corte y el tiempo en cultivo. H1: Hay interacción entre el corte y el tiempo en cultivo.

# H0: No hay diferencias en el estado oxidativo al estar en cultivo a largo plazo. H1: Hay diferencias en el estado oxidativo al estar en cultivo a largo plazo.

# Las muestras son independientes temporalmente ya que no se trata del mismo animal (mismo cestillo?)

# ESTE SCRIPT ES LA VERSION SIN DISCRIMINAR POR PLAYA DE ORIGEN
# Aunque sabemos que siguen tendencias ligeramente diferentes, no tenemos tamaño de muestra para incluirlo como factor en el analisis. La otra opcion es agrupar por playa y hacer los test 3 veces cada uno

library(tidyverse)
library(multcompView)
library(car)
library(rstatix)
library(ggpubr)

### SETUP y filtrado de datos ----

# En portatil (arreglar)
# setwd("D:/collf/Documents/GitHub/Revision_datos_TFG") # Casa
#source(file = "./scripts enero24/0_data_home.R")

# En laboratorio
setwd("C:/Users/Usuario/Documents/GitHub/Revision_datos_TFG") # Lab
source(file = "./scripts septiembre 2024-2025/0_data_lab.R")
source(file = "./scripts septiembre 2024-2025/1_funciones_graficas.R")

# Filtrar: solo anemonas cultivadas
data_2 <- filter(datos, cultivo == "cultured")

### Exploracion ----

ggplot(data_2, aes(y = CAT_t)) +
  geom_boxplot(aes(x = tiempo:corte, color = tiempo:corte), alpha = 0) +
  geom_point(aes(x = tiempo:corte, color = tiempo:corte), alpha = 1, size = 2)
# Problema: salobreña sigue una tendencia diferente en algunas enzimas como la catalasa

# Deteccion de outliers en SOD_t y CAT_t para normalidad de residuos
view(data_2 %>% 
  group_by(corte:tiempo) %>%
  identify_outliers(CAT_t))

# Se eliminan:
data_2$SOD_t[7] <- NA
data_2$SOD_t[3] <- NA
data_2$CAT_t[35] <- NA # La funcion ha identificado otro, pero su eliminacion no afecta a la normalidad de residuos, este si.


### Ajuste de modelos ----

# Ajuste de modelos ANOVA con rstatix
modelos <- lapply(colnames(data_2[c(5:24)]), function(x){
  anova_test(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2)})
(anova_results <- reduce(modelos, full_join) %>% 
    add_column(.before = 1, variable = rep(colnames(data_2[c(5:24)]), each = 3)) %>% 
    adjust_pvalue(method = "BH"))


# Test de Levenese computa exactamente igual
modelos_levene <- lapply(colnames(data_2[c(5:24)]), function(x){
  levene_test(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2)})
(levene_results <- reduce(modelos_levene, full_join) %>% 
    add_column(.before = 1, variable = colnames(data_2[c(5:24)])))

# Para el test de Shapiro-Wilks se ajustan lm para extraer residuos

modelos_lm <- lapply(colnames(data_2[c(5:24)]), function(x){
  lm(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2)})
modelos_shapiro <- lapply(modelos_lm, function(x){
  shapiro_test(residuals(x))})
(shapiro_results <- reduce(modelos_shapiro, full_join) %>% 
    add_column(.before = 1, parametro = colnames(data_2[c(5:24)])))


# Sin filtrar, no se cumple normalidad de residuos en el caso de:
# SOD_t, CAT_t

### Bucle de construccion de graficas ----


### ACTUALIZAR Y CAMBIAR BUCLE

# Falta:
  # Comprobar normas de la revista
  # Actualizar script de graficas
  # Actualizar llamada al script en este bucle
  # Actualizar directorio de salida de las graficas

for (n in c(1:20)) {
  i <- colnames(data_2[5:24])[[n]]
  tabla_summ <- data_2 %>% group_by(corte:tiempo) %>% 
    get_summary_stats(i, type = "mean_se")
  if (any(filter(as.tibble(anova_results), variable == i)$p.adj <= 0.05, na.rm = T)){
    model <- aov(as.formula(paste0(i, " ~ corte * tiempo")), data_2)
    tukey_loop <- TukeyHSD(model)
    cld.tukey <- multcompLetters4(model, tukey_loop, reversed = T)
    (letras <- rownames_to_column(as.data.frame(cld.tukey$`corte:tiempo`$Letters)))
    colnames(letras) <- c("corte:tiempo", "tukey")
    tabla_summ <- merge(tabla_summ, letras)
  } else {if (n != 5){

    tabla_summ$tukey <- c("", "", "", "")}
  }
  tabla_summ <- tabla_summ %>% separate_wider_delim(`corte:tiempo`, ":", names = c("tratamiento", "tiempo"), cols_remove=F)
    if (n != 5){
  (p <- barras_tfg() + labs(subtitle = case_when(str_detect(i, "_p") == T  ~ "Column",
                                                 str_detect(i, "_t") == T ~ "Tentacle",
                                                 TRUE ~ "")))
  ggsave(paste0("./resultados/graficas2025/", i, ".png"), width = 90, height = 112.5, units = "mm", dpi = 1000)}
}

# En los casos en los que hay interacción, descomponer el efecto y analizar por separado mediante t test.
data_2 %>% 
  group_by(tiempo) %>% 
  t_test(CAT_t ~ corte, p.adjust.method = "BH")

data_2 %>% 
  group_by(tiempo) %>% 
  t_test(GST_t ~ corte, p.adjust.method = "BH")
data_2 %>% 
  group_by(tiempo) %>% 
  t_test(DTD_t ~ corte, p.adjust.method = "BH")


### ANOVA DE 3 VIAS----

# El modelo mas completo seria hacer primero anova de 3 vias para ver si hay interaccion entre el factor playa y el resto de variables (corte y yiempo). Es decir, ¿hay una relacion distinta entre corte y tiempo para los individuos procedentes de cada playa?

# Si la hay, se puede descomponer en anovas de 2 vias para los diferentes niveles de playa
# Si no la hay, se determina si existe alguna interaccion de 2 vias.

# Voy a probar con catalasa que es una enzima en la que habia muchos resultados interesantes

data_2 %>%
  group_by(playa, corte, tiempo) %>%
  get_summary_stats(CAT_p, type = "mean_se")
# Exploracion
(bxp <- ggboxplot(data_2, x = "tiempo", y = "SOD_p", 
  color = "corte", palette = "jco", facet.by = "playa"))

# Normalidad de residuos general
model_3  <- lm(SOD_p ~ playa*corte*tiempo, data = data_2)
ggqqplot(residuals(model_3))
shapiro_test(residuals(model_3))

# Normalidad de residuos por grupo
data_2 %>%
  group_by(playa, corte, tiempo) %>%
  shapiro_test(SOD_p)
ggqqplot(data_2, "SOD_p", ggtheme = theme_bw()) +
  facet_grid(corte + tiempo ~ playa, labeller = "label_both")

# Levene
data_2 %>% levene_test(SOD_p ~ playa*corte*tiempo)

## Computacion del ANOVA
res.aov <- data_2 %>% anova_test(SOD_p ~ playa*corte*tiempo)
res.aov

# Hay una interaccion significativa a 3 vias, es decir, el efecto del corte y del tiempo sobre los niveles de catalasa pedia son diferentes en organismos de diferentes playas.

# Eso quiere decir que se puede analizar este modelo estadistico por separado para cada nivel del factor playa, descomponiendo asi la interaccion

# Vamos a agfrupar por playa y ver si hay interaccion a dos vias en alguna de ellas. Utilizamos los residuos del modelo completo para mas potencia estadistica
modelolm_3  <- lm(CAT_p ~ playa*corte*tiempo, data = data_2)
data_2 %>%
  group_by(playa) %>%
  anova_test(CAT_p ~ corte*tiempo, error = modelolm_3)

# Hay interaccion significativa a p = 0.016 en salobreña. Asi que vamos a descomponerla de nuevo en efectos simples

treatment.effect <- data_2 %>%
  group_by(playa, tiempo) %>%
  anova_test(CAT_p ~ corte, error = modelolm_3)
treatment.effect %>% as.tibble() %>% filter(playa == "Salobreña")

# No hay efecto significativo del corte en las anémonas de Calahonda
# Hay un efecto significativo del corte en las anémonas de Almuñecar, tanto a corto como a largo plazo.
# Hay un efecto signitifactivo del corte a largo plazo en las anémonas de Salobreña


pwc <- data_2 %>%
  group_by(playa, tiempo) %>%
  emmeans_test(CAT_p ~ corte, p.adjust.method = "bonferroni") %>%
  select(-df, -statistic, -p) # Remove details
# Show comparison results for male at high risk
pwc %>% filter(playa == "Salobreña", tiempo == "1")

# Presentacion de resultados

