# ORTIMAR - Objetivo 2
# Analisis efecto del corte, tiempo y playa
# Febrero 2025 - ANOVA de 3 vias

# El modelo mas completo seria hacer primero anova de 3 vias para ver si hay interaccion entre el factor playa y el resto de variables (corte y yiempo). Es decir, ¿hay una relacion distinta entre corte y tiempo para los individuos procedentes de cada playa?

# Si la hay, se puede descomponer en anovas de 2 vias para los diferentes niveles de playa
# Si no la hay, se determina si existe alguna interaccion de 2 vias.

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

### Superoxido dismutasa PIE----
# Exploracion
data_2 %>%
  group_by(playa, corte, tiempo) %>%
  get_summary_stats(SOD_p, type = "mean_se")
(bxp <- ggboxplot(data_2, x = "tiempo", y = "SOD_p", 
                  color = "corte", palette = "frontiers", facet.by = "playa",
                  title = "Superoxido dismutasa columnar"))

# Normalidad de residuos general
modelo_completo  <- lm(SOD_p ~ playa*corte*tiempo, data = data_2)
ggqqplot(residuals(modelo_completo))
shapiro_test(residuals(modelo_completo))

# Normalidad de residuos por grupo
data_2 %>%
  group_by(playa, corte, tiempo) %>%
  shapiro_test(SOD_p)
ggqqplot(data_2, "SOD_p", ggtheme = theme_bw()) +
  facet_grid(corte + tiempo ~ playa, labeller = "label_both")

# En Calahonda dissected 1 no se distribuyen normalmente los 3 puntos, el resto bien

# Homogeneidad de varianzas con Levene
data_2 %>% levene_test(SOD_p ~ playa*corte*tiempo)

## Computacion del ANOVA
res.aov <- data_2 %>% anova_test(SOD_p ~ playa*corte*tiempo)
res.aov

# Hay una interaccion significativa a 3 vias, es decir, el efecto del corte y del tiempo sobre los niveles de catalasa pedia son diferentes en organismos de diferentes playas.
# Eso quiere decir que se puede analizar este modelo estadistico por separado para cada nivel del factor playa, descomponiendo asi la interaccion

# Vamos a agrupar por playa y ver si hay interaccion a dos vias en alguna de ellas. Utilizamos los residuos del modelo completo para mas potencia estadistica

# Descomposicion por playa
data_2 %>%
  group_by(playa) %>%
  anova_test(SOD_p ~ corte*tiempo, error = modelo_completo)

# Hay interaccion significativa a alga = 0.025 solo salobreña. En Calahonda y Almuñecar no hay efectos significativos.

# Descomposicion en Salobreña (tukey?)

efecto.tratamiento <- data_2 %>%
  group_by(playa, tiempo) %>%
  anova_test(SOD_p ~ corte, error = modelo_completo)
efecto.tratamiento %>% as.tibble() %>% filter(playa == "Salobreña")
# Hay efecto significativo tanto a corto plazo como a largo plazo

### Resumen SOD_P
# Hay interacción a 3 vias: la interaccion corte:tiempo depende de la playa
# No hay efecto significativo del corte en las anémonas de Calahonda
# No hay efecto significativo del corte en las anémonas de Calahonda
# En Salobreña, las anemonas cortadas redujeron su SOD_p a corto plazo pero lo aumentan mucho a largo plazo.

# Grafica
tabla_summ <- data_2 %>%
  group_by(playa, corte, tiempo) %>%
  get_summary_stats(SOD_p, type = "mean_se")

modelo <- aov(SOD_p~corte*tiempo, filter(data_2, playa == "Salobreña"))
tukey_loop <- TukeyHSD(modelo)
cld.tukey <- multcompLetters4(modelo, tukey_loop, reversed = T)
(letras <- rownames_to_column(as.data.frame(cld.tukey$`corte:tiempo`$Letters)))
colnames(letras) <- c("corte:tiempo", "tukey")
letras <- letras %>% separate_wider_delim(`corte:tiempo`, ":", names = c("corte", "tiempo"), cols_remove=F) %>% select(-`corte:tiempo`)
letras$playa = "Salobreña"

tabla_summ <- merge(tabla_summ, letras, all.x = T)
tabla_summ$tukey <- replace_na(tabla_summ$tukey, "")

(bxp <- ggboxplot(data_2, x = "tiempo", y = "SOD_p", 
                  fill = "corte", color = "corte", alpha = 0.85, palette = "frontiers", facet.by = "playa",
                  title = "Superoxido dismutasa columnar"))
(bp <- ggbarplot(tabla_summ, x = "tiempo", y = "mean", fill = "corte", color = "corte", alpha = 0.9, facet.by = "playa", position = position_dodge2()) +
    geom_text(tabla_summ, aes(x = tiempo, y = mean+se, label = tukey, group = corte, )))
      
      
    ) x = "tiempo", y = "mean", label = "tukey", grouping.vars = "corte", facet.by = "playa"))

  
# Presentacion de resultados

#### Anova de 3 vias con correción BH ----

#Pivotamos los datos a formato largo para poder agrupar por variable y eliminamos NAs

data_2_long <- data_2 %>% pivot_longer(!c(individuo, playa, corte, cultivo, tiempo), names_to = "variable", values_to = "valor", values_drop_na = TRUE)

# FALTA AQUI HACER LAS ASUNCIONES PARA TODAS LAS VARIABLES
# Computamos los anovas agrupando por variable y aplicamos correción de BH
res.aov <- data_2_long %>% group_by(variable) %>% anova_test(valor ~ playa*corte*tiempo) %>% adjust_pvalue(method = "BH")

# A continuación habria que ir a cada variable una a una y hacer los pertinentes test post hoc, ir incorporando en un informe de markdown.