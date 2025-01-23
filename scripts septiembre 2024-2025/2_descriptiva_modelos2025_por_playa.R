# ORTIMAR - Objetivo 2
# Analisis efecto del corte
# Enero 2025 - revision para publicacion

# ESTE SCRIPT ES LA VERSION SEPARANDO PLAYAS

# Vamos a hacer un diseño cuadrado 2x2 dentro de las anemonas cultivadas: la variable corte, que tiene 2 niveles (dissected y control), y la variable tiempo, con niveles 0 y 1. De esa manera esperamos ver el efecto del corte a largo plazo.

# H0: No hay diferencias entre el estado oxidativo de anémonas control y cortadas. H1: Sí existen alguna diferencia.

# H0: No hay interacción entre el corte y el tiempo en cultivo. H1: Hay interacción entre el corte y el tiempo en cultivo.

# H0: No hay diferencias en el estado oxidativo al estar en cultivo a largo plazo. H1: Hay diferencias en el estado oxidativo al estar en cultivo a largo plazo.

# Las muestras son independientes temporalmente ya que no se trata del mismo animal (mismo cestillo?)


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

ggplot(data_2, aes(y = MDA_t)) +
  geom_boxplot(aes(x = tiempo:corte, color = tiempo:corte, fill = tiempo:corte), alpha = 0.4) +
  geom_point(aes(x = tiempo:corte, color = tiempo:corte), alpha = 1, size = 2) +
  facet_wrap(~playa)
# Problema: salobreña sigue una tendencia diferente en algunas enzimas como la catalasa

data_2$SOD_t[7] <- NA
data_2$SOD_t[3] <- NA # Ambos fastidian la normalidad de residuos
data_2$CAT_t[35] <- NA # Afecta a la normalidad de residuos

#data_2$SOD_p[6] <- NA # FUERA DE RANGO IQ Y PARECE INFLUIR EL ANALISIS
#data_2$CAT_p[16] <- NA # FUERA DE RANGO IQ Y PARECE INFLUIR EL ANALISIS
# en g6pdh_t no veo ningun outlier muy claro
#data_2$MDA_p[20] <- NA # Muy fuera de rango IQ

### Ajuste de modelos ----

# Hacemos anova de 2 vias con interaccion, ignorando la variable playa
modelos <- lapply(colnames(data_2[c(5:24)]), function(x){
  aov(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2)})

# Asuncion de NORMALIDAD de residuos

sapply(modelos, function(x){
  print(x$terms[[2]])
  shapiro.test(residuals(x))}) 
# Sin hacer ningun filtrado de outliers:
# SOD_t y CAT_t no cumplen la normalidad de residuos

# Con 3 outliers eliminados se cumple la normalidad


# Asuncion de HOMOCEDASTICIDAD
sapply(modelos, function(x){
  print(leveneTest(x))})
# Sin hacer ningun filtrado de outliers: Todas bien
# Con 3 outliers eliminados se sigue cumpliendo la asuncion


### RESULTADOS DEL ANOVA ----
for (i in c(1:20)) {
  print(colnames(data_2[5:24][i]))
  print(summary(modelos[[i]]))}

### Correcion Benjamin Hoechnberg ----

#Necesito extraer un vector con todos los pvalores del estudio
# Despues aplicar p.adjust() para que me los devuelva ajustados

p_valores <- c(0.0152,0.9248,0.5331,0.7548,0.0722,0.0599,0.6334,0.0943,1.62e-05,0.00544,0.36033,0.53444,0.736,0.447,0.1034, 0.0930,0.0476,0.411,8.82e-05,0.056,0.000288,0.008173,0.404385,0.008673,2.06e-05,0.000391,0.648,0.642,0.875,0.000127,0.002524,0.005489,0.00193,0.20388,0.43785,0.0805,0.2785,0.0821, 0.018,0.579,0.148,0.046,0.724,0.749,0.69437,0.00681,0.29190,0.524,0.598,0.578,0.438,0.407, 0.940,0.00572,0.10445 ,0.85339,0.165,0.053,0.626)

(p_valor_adjust <- p.adjust(p_valores, method = "BH"))

# RESUMEN RESULTADOS:

# SOD, GPx, G6PDH, TEAC no se afectan en ningun tejido

# CAT_t tiene interaccion
# CAT_p tiene corte
# GR_t tiene tiempo
# GR_p tiene corte y tiempo sin interaccion
# GST_t tiene corte y tiempo con interaccion
# DTD_t tiene corte y tiempo con interaccion
# DTD_p tiene corte
# proteina_p tiene tiempo
# MDA_t tiene corte

# GST_p, proteina_t y MDA_p no se afectan



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
