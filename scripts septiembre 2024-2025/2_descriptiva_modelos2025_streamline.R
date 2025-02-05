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

ggplot(data_2, aes(y = G6PDH_p)) +
  geom_boxplot(aes(x = tiempo:corte, color = tiempo:corte), alpha = 0) +
  geom_point(aes(x = tiempo:corte, color = tiempo:corte), alpha = 1, size = 2)
# Problema: salobreña sigue una tendencia diferente en algunas enzimas como la catalasa

### Ajuste de modelos ----

# Ajuste de modelos ANOVA con rstatix
modelos <- lapply(colnames(data_2[c(5:24)]), function(x){
  anova_test(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2)})

(anova_results <- reduce(modelos, full_join) %>% 
    add_column(.before = 1, variable = rep(colnames(data_2[c(5:24)]), each = 3)) %>% 
    adjust_pvalue(method = "BH"))


# Test de Levene y Shapiro se pueden computar igual
modelos_levene <- lapply(colnames(data_2[c(5:24)]), function(x){
  levene_test(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2)})

(levene_results <- reduce(modelos_levene, full_join) %>% 
    add_column(.before = 1, variable = colnames(data_2[c(5:24)])))

modelos_shapiro <- lapply(data_2[c(5:24)], function(x){
  shapiro_test(x, data_2)})
(shapiro_results <- reduce(modelos_shapiro, full_join) %>% 
    add_column(.before = 1, parametro = colnames(data_2[c(5:24)])))



# No se cumple normalidad de residuos en el caso de:
# GPx_p, GST_t, DTD_t, G6PDH_p, y G6PDH_t

### Bucle de construccion de graficas ----


### ACTUALIZAR Y CAMBIAR BUCLE

# Falta:
  # Comprobar normas de la revista
  # Actualizar script de graficas
  # Actualizar llamada al script en este bucle
  # Actualizar directorio de salida de las graficas

p_valor_adjust_cor <- append(p_valor_adjust, 1,14)


j <- 1
for (n in c(1:20)) {
  i <- colnames(data_2[5:24])[[n]]
  tabla_summ <- data_2 %>%  group_by(corte:tiempo) %>% 
    summarise(media = mean(get(i), na.rm = T),
              desvest = sd(get(i), na.rm = T),
              error = desvest/sqrt(sum(!is.na(get(i)))))
  
  if (((p_valor_adjust_cor[j]) <= .05)|((p_valor_adjust_cor[j+1]) <= .05)|((p_valor_adjust_cor[j+2]) <= .05)) {
    tukey_loop <- TukeyHSD(modelos[[n]])
    cld.tukey <- multcompLetters4(modelos[[n]], tukey_loop, reversed = T)
    
    (letras <- rownames_to_column(as.data.frame(cld.tukey$`corte:tiempo`$Letters)))
    colnames(letras) <- c("corte:tiempo", "tukey")
    tabla_summ <- merge(tabla_summ, letras)
    tabla_summ$`corte:tiempo` <- factor(tabla_summ$`corte:tiempo`, levels = c("control:0", "dissected:0", "control:1", "dissected:1"))
  } else {
    tabla_summ$tukey <- c("", "", "", "")
  }
  tabla_summ <- tabla_summ %>% separate_wider_delim(`corte:tiempo`, ":", names = c("tratamiento", "tiempo"), cols_remove=F)
  j = j + 3
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
