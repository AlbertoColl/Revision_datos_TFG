# ORTIMAR - Objetivo 2
# Analisis efecto del corte
# Diciembre 2023

# Comparar los parametros entre los controles cultivados (cultivo si, corte no, madurez no), los cortados (corte si, madurez no), y los cortados t2 (corte si, madurez si)


library(tidyverse)
library(car)
library(multcompView)

### Cargar datos y filtrar para el analisis ----
# setwd("D:/collf/Documents/GitHub/Revision_datos_TFG") # Casa
#source(file = "./analisis 12-2023/0_data_home.R")

setwd("C:/Users/Usuario/Documents/GitHub/Revision_datos_TFG") # Lab
source(file = "./analisis 12-2023/0_data_lab.R")
source(file = "./analisis 12-2023/0_5_graficas.R")

data_2 <- filter(datos, cultivo == "cultured") %>% 
  filter((corte == "dissected") | ((corte == "control") & (madurez == "No"))) %>% 
  mutate(tratamiento = as.factor(case_when(corte == "control" ~ "control",
                                 (corte == "dissected") & (madurez == "No") ~ "t1",
                                 (corte == "dissected") & (madurez == "Si") ~ "t2")))

### Exploracion ----

ggplot(data_2, aes(y = MDA)) +
  geom_boxplot(aes(x = tratamiento, color = tratamiento), alpha = 0) +
  geom_point(aes(x = tratamiento, color = tratamiento), alpha = 0.8) +
  facet_wrap(~tejido)


# Outilers: comprobar cMDA
# Outilers: comprobar con los modelos y solo quitar si necesarios

#data_2[26,8] <- NA # En catalasa, parece que se va bastante pero no tengo mucha confianza
#data_2[17,9] <- NA # en gpx, bastante seguro de que es outlier
#data_2[47,10] <- NA # EN GR, muy bajo posible outlier
#data_2[51,11] <- NA  # EN gst aunque no creo que influencie mucho porque no hay diferencias
#data_2[47,16] <- NA # en MDA, si que puede ser outlier


### Modelos y asunciones ----
 #Prueba: 
m1 <- aov(log(MDA) ~ tratamiento, data = filter(data_2, tejido == "Tentaculo"))
# SOD_p cumple normalidad de residuos bastante bien
# CAT_p tb
# GPx_p se va un poco, inclusp con log
# GR_p es light tailed, pero se supone que es el aptron mas inofensivo y esta balanceado
# GST_p tiene un poco de sesgo hacia la derecha
# DTD_p perfecta
# G6PDH_p mal pero con log se arregla
# TEAC aceptable en pie
# MDA tiene un posible outlier en la obs n 5

# SOD_t un poco heavy tailed, incluso con log
# CAT_t tiene 2 outliers en obs 25 y 27, necesita log creo
# GPx_t un poco de cola, obs 6 y 23 posibles outliers
# GR_t mucha cola, mejor con log
# GST_t una cola se va enorme, mejor con log
# DTD_t bien, menos cola que la normal pero bien
# G6PDH t sorprendemente muy bien
# TEAC regulin pero con poca cola
# MDA t ligeramente sesgada a la izq pero bien

modelos_p <- lapply(colnames(data_2[c(7:16)]), function(x){
  aov(formula = as.formula(paste0(x, " ~ tratamiento")), data = filter(data_2, tejido == "Pie"))})

modelos_p[[3]] <- aov(log(GPx) ~ tratamiento,data = filter(data_2, tejido == "Pie"))
modelos_p[[6]] <- aov(log(G6PDH) ~ tratamiento,data = filter(data_2, tejido == "Pie"))

modelos_t <- lapply(colnames(data_2[c(7:16)]), function(x){
  aov(formula = as.formula(paste0(x, " ~ tratamiento")), data = filter(data_2, tejido == "Tentaculo"))})

modelos_t[[1]] <- aov(log(SOD) ~ tratamiento,data = filter(data_2, tejido == "Tentaculo"))
modelos_t[[2]] <- aov(log(CAT) ~ tratamiento,data = filter(data_2, tejido == "Tentaculo"))
modelos_t[[4]] <- aov(log(GR) ~ tratamiento,data = filter(data_2, tejido == "Tentaculo"))
modelos_t[[5]] <- aov(log(GST) ~ tratamiento,data = filter(data_2, tejido == "Tentaculo"))

sapply(modelos_p, function(x){
  print(leveneTest(x))})

sapply(modelos_t, function(x){
  print(leveneTest(x))})

### Evaluacion de modelos y test post hoc ----
for (i in c(1:10)) {
  print(colnames(data_2[7:16][i]))
  print(summary(modelos_p[[i]]))
}
# En pie hay diferencias en MDA, proteina, GR, y marginalmente en SOD, CAT y GPx.

for (i in c(1:10)) {
  print(colnames(data_2[7:16][i]))
  print(summary(modelos_t[[i]]))
}
# En tent hay diferencias en CAT, GR, GST, DTD, MDA y marginalmente en G6PDH


# Bucle de estadisticos de resumen, test port-hoc y grafica
for (n in c(1:10)) { ### PIE
  i <- colnames(data_2[7:16])[[n]]
  tabla_summ <- data_2 %>%
    filter(tejido == "Pie") %>%   group_by(tratamiento) %>% 
    summarise(media = mean(get(i), na.rm = T),
              desvest = sd(get(i), na.rm = T),
              error = desvest/sqrt(sum(!is.na(get(i)))))
  if ((summary(modelos_p[[n]])[[1]][["Pr(>F)"]][1]) <= 0.05) {
    tukey_loop <- TukeyHSD(modelos_p[[n]])
    cld.tukey <- multcompLetters4(modelos_p[[n]], tukey_loop, reversed = T)
    (letras <- rownames_to_column(as.data.frame(cld.tukey$tratamiento$Letters)))
    colnames(letras) <- c("tratamiento", "tukey")
    tabla_summ <- merge(tabla_summ, letras)
  } else {
    tabla_summ$tukey <- c("", "", "")
  }
  (p <- barras_tfm())
  (p <- p + annotate("text", x = 2, y = 1.3 * (max(tabla_summ$media) + max(tabla_summ$error)), label = "paste(italic(Column))", size = 5, color = "gray30", parse = T))
  ggsave(paste0("./resultados/2/", i, "_pie.png"), width = 400, height = 500, units = "px", scale = 2, dpi = "retina")
  }

for (n in c(1:10)) { ### TENTACULO
  i <- colnames(data_2[7:16])[[n]]
  tabla_summ <- data_2 %>%
    filter(tejido == "Tentaculo") %>%   group_by(tratamiento) %>% 
    summarise(media = mean(get(i), na.rm = T),
              desvest = sd(get(i), na.rm = T),
              error = desvest/sqrt(sum(!is.na(get(i)))))
  if ((summary(modelos_t[[n]])[[1]][["Pr(>F)"]][1]) <= 0.05) {
    tukey_loop <- TukeyHSD(modelos_t[[n]])
    cld.tukey <- multcompLetters4(modelos_t[[n]], tukey_loop, reversed = T)
    (letras <- rownames_to_column(as.data.frame(cld.tukey$tratamiento$Letters)))
    colnames(letras) <- c("tratamiento", "tukey")
    tabla_summ <- merge(tabla_summ, letras)
  } else {
    tabla_summ$tukey <- c("", "", "")
  }
  (p <- barras_tfm())
  (p <- p + annotate("text", x = 2, y = 1.3 * (max(tabla_summ$media) + max(tabla_summ$error)), label = "paste(italic(Tentacle))", size = 5, color = "gray30", parse = T))
  ggsave(paste0("./resultados/2/", i, "_tent.png"), width = 400, height = 500, units = "px", scale = 2, dpi = "retina")
}
