# ORTIMAR - Objetivo 1
# Analisis efecto del cultivo
# Diciembre 2023 - Enero 2024

# Comparar los parametros entre los salvajes y los controles cultivados 22 semanas (cultivo si, corte no, madurez si)

# Explorar las variables
# Identificar posibles outliers
# Ver si distribucion es normal y transformar
# Considerar alternativa no parametrica
# Hacer modelos y comprobar asunciones
# Hacer graficas

library(tidyverse)
library(car)
library(agricolae)
library(multcompView)

### Cargar datos y filtrar para el analisis ----
# setwd("D:/collf/Documents/GitHub/Revision_datos_TFG") # Casa
#source(file = "./analisis 12-2023/0_data_home.R")

setwd("C:/Users/Usuario/Documents/GitHub/Revision_datos_TFG") # Lab
source(file = "./analisis 12-2023/0_data_lab.R")
source(file = "./analisis 12-2023/0_5_graficas.R")

data_1 <- filter(datos, corte == "control") %>% 
  filter(cultivo == "wild" | madurez == "Si")

### Exploracion ----

ggplot(filter(data_1, tejido == "Tentaculo")) +
  geom_histogram(aes(x = TEAC, fill = playa), bins = 10, alpha = 0.8) +
  facet_wrap(~ madurez)

shapiro.test(filter(data_1, tejido == "Tentaculo")$SOD)


ggplot(data_1) +
  geom_boxplot(aes(x = playa, y = log(MDA), color = playa), alpha = 0.1) +
  geom_point(aes(x = playa, color = playa, y = log(MDA)), alpha = 0.9) +
  facet_wrap(~tejido)

# Observacion n 38 (individuo 14) la GR sale mu alta (columna 10)
# Observación n 12 (individuo 12) la G6PDH sale mu alta (columna 13)
# Observación n 6 (individuo 6) la TEAC sale mu baja (columna 15)
# Observación n 1 (individuo 1) la TEAC sale mu alta (columna 15)

### Potenciales outliers ----
data_1[38, 10] <-  NA
data_1[12, 13] <-  NA
data_1[6, 15] <-  NA
data_1[1, 15] <-  NA
data_1[25, 7] <- NA
data_1[7,13] <- NA
# Revisar

# NORMALIDAD DE LOS DATOS - EVALUACION VISUAL

# SOD bastante aceptables las dos, quizas pie un poco peor
# cat no es muy normal en tentaculo. Transformar
# GPx una mierda, muy pocos datos no se que hacer
# GR mejora con log. Transformar
# DTD bien creo, bastante normal
# G6PDH en pie mejora con transformacion creo
# TEAC perfecto
# MDA bastante bien pero creo que mejora


### Prueba modelos y normalidad ----
# Modelo basico: SOD ~ tiempo
# Modelo con playa: SOD ~ tiempo + playa. Vamos a probar con este

# Prueba normalidad residuos
m1 <- aov(log(MDA) ~ cultivo + playa, data = filter(data_1, tejido == "Tentaculo"))
summary(m1)
plot(m1, 3)

# SOD_tent cumple normalidad mas o menos
# CAT_tent muy normal sin transformar que hacemos?
# GPx tent no es analizable
# GR tent un poco sesgada pero no mucho. No se arregla al transformar
# GST_tent esta muy bien de normalidad
# DTD_tent igual
# G6PDH en tent tiene un outlier en el 7 creo. Transformar para residuos
# TEAC tent ligeramente menos dispersion quenormal, aceptable
# MDA tent tiene un poquto mas de dispersion por un par de datos pero el resto se ajusta genial. No se arregla con transformacion log.

# SOD_pie perfecta
# CAT_pie perfecta
# GPx pie es normal
# GR pie muy normal
# GST pie muy normal
# DTD pie bien
# G6PDH pie aceptablemente bien
# teac bien en pie
# MDA perfecto pie


# Modelos finales en bucle

#data_1 <- select(data_1, -GPx)

modelos_p <- lapply(colnames(data_1[c(7:16)]), function(x){
  Anova(aov(formula = as.formula(paste0(x, " ~ cultivo + playa")), filter(data_1, tejido == "Pie")), type = 3)})

modelos_t <- lapply(colnames(select(data_1, -GPx)[c(7:15)]), function(x){
  Anova(aov(formula = as.formula(paste0(x, " ~ cultivo + playa")), filter(select(data_1, -GPx), tejido == "Tentaculo")), type = 3)})

# Transformar log solo GR y G6PDH en tentaculo

modelos_t[[3]] <- Anova(aov(log(GR) ~ cultivo + playa, data = filter(data_1, tejido == "Tentaculo")), type = 3)
modelos_t[[6]] <- Anova(aov(log(G6PDH) ~ cultivo + playa, data = filter(data_1, tejido == "Tentaculo")), type = 3)

### Evaluacion de modelos ----
for (i in c(1:10)) {
  print(colnames(data_1[7:16][i]))
  print(modelos_p[i])
}
# Pie: Diferencias significativas en GPx, GR, GST, G6PDH y MDA

for (i in c(1:9)) {
  print(colnames(select(data_1, -GPx)[7:15][i]))
  print(modelos_t[i])
}
# Tentaculo: diferencia significativas en SOD, CAT, GST, DTD, TEAC y MDA

### TESST POST-HOC ----
# Rehacemos modelos en clase aov() para el test de tukey
modelos_p <- lapply(colnames(data_1[c(7:16)]), function(x){
  aov(formula = as.formula(paste0(x, " ~ cultivo + playa")), filter(data_1, tejido == "Pie"))})

modelos_t <- lapply(colnames(select(data_1, -GPx)[c(7:15)]), function(x){
  aov(formula = as.formula(paste0(x, " ~ cultivo + playa")), filter(select(data_1, -GPx), tejido == "Tentaculo"))})

# Transformar log solo GR y G6PDH en tentaculo

modelos_t[[3]] <- aov(log(GR) ~ cultivo + playa, data = filter(data_1, tejido == "Tentaculo"))
modelos_t[[6]] <- aov(log(G6PDH) ~ cultivo + playa, data = filter(data_1, tejido == "Tentaculo"))
# Bucle de estadisticos de resumen, test port-hoc y grafica

for (n in c(1:10)) { ### PIE
  i <- colnames(data_1[7:16])[[n]]
  tabla_summ <- data_1 %>%
    filter(tejido == "Pie") %>%   group_by(cultivo, playa) %>% 
    summarise(media = mean(get(i), na.rm = T),
              desvest = sd(get(i), na.rm = T),
              error = desvest/sqrt(sum(!is.na(get(i)))))
  if ((summary(modelos_p[[n]])[[1]][["Pr(>F)"]][2])<= 0.05) {
    # Hacer aov() del parametro primero con cultivo no y luego cultivo si
    tukey_loop <- TukeyHSD(modelos_p[[n]])
    cld.tukey <- multcompLetters4(modelos_p[[n]], tukey_loop, reversed = T)
    (letras <- rownames_to_column(as.data.frame(cld.tukey$tratamiento$Letters)))
    colnames(letras) <- c("tratamiento", "tukey")
    tabla_summ <- merge(tabla_summ, letras)
  } else {
    tabla_summ$tukey <- c("", "", "", "", "", "")
  }
  (p <- barras_tfg()) # CAMBIAR GRAFICA
  ggsave(paste0("./analisis 12-2023/graficas/1/", i, "_pie.png"), width = 800, height = 1000, units = "px", scale = 2, dpi = "retina")
}

for (n in c(1:9)) { ### TENTACULO
  i <- colnames(select(data_1, -GPx)[c(7:15)])[[n]]
  tabla_summ <- data_1 %>%
    filter(tejido == "Tentaculo") %>%   group_by(cultivo, playa) %>% 
    summarise(media = mean(get(i), na.rm = T),
              desvest = sd(get(i), na.rm = T),
              error = desvest/sqrt(sum(!is.na(get(i)))))
  if ((summary(modelos_t[[n]])[[1]][["Pr(>F)"]][2])<= 0.05) {
    tukey_loop <- HSD.test(modelos_t[[n]], c("playa"), unbalanced = T)
    (letras <- letras <- rep( tukey_loop$groups$groups,2))
    tabla_summ$tukey <- letras 
  } else {
    tabla_summ$tukey <- c("", "", "", "", "", "")
  }
  (p <- barras_tfg()) # CAMBIAR GRAFICA
  ggsave(paste0("./analisis 12-2023/graficas/1/", i, "_tent.png"), width = 800, height = 1000, units = "px", scale = 2, dpi = "retina")
}
