# ORTIMAR - Objetivo 2
# Analisis efecto del corte
# Septiembre 2024 - revision para publicacion

# El objetivo es comparar los parametros entre los controles cultivados (cultivo si, corte no, madurez no), los cortados (corte si, madurez no), y los cortados t2 (corte si, madurez si) para ver el efecto de esta manipulacion a corto y largo plazo en cultivo.

# La hipotesis nula H0 es que no hay diferencias entre el estado oxidativo entre anemonas cortadas y no cortadas. La hipotesis alternativa H1 es que sí que hay diferencias, a corto o largo plazo respecto al control.

# Las muestras son independientes temporalmente ya que no se trata del mismo animal (mismo cestillo?)

library(tidyverse)
library(multcompView)

### SETUP y filtrado de datos ----

# En portatil (arreglar)
# setwd("D:/collf/Documents/GitHub/Revision_datos_TFG") # Casa
#source(file = "./scripts enero24/0_data_home.R")

# En laboratorio
setwd("C:/Users/Usuario/Documents/GitHub/Revision_datos_TFG") # Lab
source(file = "./scripts septiembre 2024/0_data_lab.R")
#source(file = "./analisis 12-2023/0_5_graficas.R")

# Filtrar para Objetivo 2
data_2 <- filter(datos, cultivo == "cultured")

### Exploracion ----

ggplot(data_2, aes(y = SOD_p)) +
  geom_boxplot(aes(x = tiempo:corte, color = tiempo:corte), alpha = 0) +
  geom_point(aes(x = tiempo:corte, color = tiempo:corte), alpha = 1, size = 2) +
  facet_grid(~playa)
# Problema: salobreña sigue una tendencia diferente en algunas enzimas como la catalasa


ggsave("grafica_ejemplo.eps", device = "eps")

data_2$SOD_p[6] <- NA # FUERA DE RANGO IQ Y PARECE INFLUIR EL ANALISIS
data_2$CAT_p[16] <- NA # FUERA DE RANGO IQ Y PARECE INFLUIR EL ANALISIS
# en g6pdh_t no veo ningun outlier muy claro
data_2$MDA_p[20] <- NA # Muy fuera de rango IQ

### Ajuste de modelos ----

modelos <- lapply(colnames(data_2[c(6:25)]), function(x){
  aov(formula = as.formula(paste0(x, " ~ tratamiento")), data_2)})

# Asuncion de NORMALIDAD de residuos

sapply(modelos, function(x){
  print(x$terms[[2]])
  shapiro.test(residuals(x))}) 
# CAT_t no cumple la normalidad de residuos
# GPx_t está al limite
# 66PDH_p no la cumple

# Asuncion de HOMOCEDASTICIDAD
sapply(modelos, function(x){
  print(leveneTest(x))})
# GR_t está al limite

# Transformacion logaritmica para supuesto de normalidad
modelos[[3]] <- aov(log(CAT_t) ~ tratamiento,data = data_2)
modelos[[14]] <- aov(log(G6PDH_p) ~ tratamiento,data = data_2)

### RESULTADOS DEL ANOVA ----
for (i in c(1:20)) {
  print(colnames(data_2[6:25][i]))
  print(summary(modelos[[i]]))}

# SOD_p significativo
# CAT ambas significativas
# GPx_p significativo
# Ambas GRs
# GST t muy significativa
# Ambas DTDs
# G6PDH en tentaculo cercana
# proteina pie significativo por poco
# MDA t es significativo
# MDA p significativo

### Bucle de construccion de graficas ----


### ACTUALIZAR Y CAMBIAR BUCLE

# Falta:
  # Comprobar normas de la revista
  # Actualizar script de graficas
  # Actualizar llamada al script en este bucle
  # Actualizar directorio de salida de las graficas


for (n in c(1:20)) {
  i <- colnames(data_2[6:25])[[n]]
  tabla_summ <- data_2 %>%  group_by(tratamiento) %>% 
    summarise(media = mean(get(i), na.rm = T),
              desvest = sd(get(i), na.rm = T),
              error = desvest/sqrt(sum(!is.na(get(i)))))
  if ((summary(modelos[[n]])[[1]][["Pr(>F)"]][1]) <= 0.05) {
    tukey_loop <- TukeyHSD(modelos[[n]])
    cld.tukey <- multcompLetters4(modelos[[n]], tukey_loop, reversed = T)
    (letras <- rownames_to_column(as.data.frame(cld.tukey$tratamiento$Letters)))
    colnames(letras) <- c("tratamiento", "tukey")
    tabla_summ <- merge(tabla_summ, letras)
  } else {
    tabla_summ$tukey <- c("", "", "", "")
  }
  (p <- barras_tfm() + labs(subtitle = case_when(str_detect(i, "_p") == T  ~ "Column",
                                                 str_detect(i, "_t") == T ~ "Tentacle",
                                                 TRUE ~ "")))
  #saveRDS(p, paste0("./resultados/graficas4/", i, "_RDS"))
  #ggsave(paste0("./resultados/graficas4/", i, ".png"), width = 90, height = 112.5, units = "mm", dpi = 1000)
}

