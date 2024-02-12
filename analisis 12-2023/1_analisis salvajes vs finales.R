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

shapiro.test(log(filter(data_1, tejido == "Tentaculo")$GST))
shapiro.test(filter(data_1, tejido == "Tentaculo")$GST)


ggplot(data_1) +
  geom_boxplot(aes(x = playa, y = log(MDA), color = playa), alpha = 0.1) +
  geom_point(aes(x = playa, color = playa, y = log(MDA)), alpha = 0.9) +
  facet_wrap(~tejido)

# Observacion n 38 (individuo 14) la GR sale mu alta (columna 10)
# Observación n 12 (individuo 12) la G6PDH sale mu alta (columna 13)
# Observación n 6 (individuo 6) la TEAC sale mu baja (columna 15)
# Observación n 1 (individuo 1) la TEAC sale mu alta (columna 15)

### Potenciales outliers ----
data_1$GR[38] <-  NA
data_1$G6PDH[12] <-  NA
data_1$TEAC[6] <-  NA # teac
data_1$TEAC[1] <-  NA # este es del teac no se para que quitarlo
data_1$SOD[25] <- NA
data_1$G6PDH[7] <- NA
# Revisar

# NORMALIDAD DE LOS DATOS - EVALUACION VISUAL
# SOD bastante aceptables las dos, quizas pie un poco peor
# cat no es muy normal en tentaculo. Transformar
# GPx una mierda, muy pocos datos no se que hacer
# GR transformar log en tentaculo
# GST no muy bien. transformar las dos
# DTD bien creo, bastante normal
# G6PDH en pie mejora con transformacion creo
# TEAC perfecto
# MDA bastante bien pero creo que mejora


### Prueba modelos y normalidad ----
# Modelo basico: SOD ~ tiempo
# Modelo con playa: SOD ~ tiempo + playa. Vamos a probar con este

# Prueba normalidad residuos
m1 <- aov(log(MDA) ~ cultivo * playa, data = filter(data_1, tejido == "Tentaculo"))
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

modelos_p <- lapply(colnames(select(data_1, -GPx, )[c(7:15)]), function(x){
  aov(formula = as.formula(paste0(x, " ~ cultivo * playa")), filter(data_1, tejido == "Pie"))})

modelos_t <- lapply(colnames(select(data_1, -GPx)[c(7:15)]), function(x){
  aov(formula = as.formula(paste0(x, " ~ cultivo * playa")), filter(data_1, tejido == "Tentaculo"))})

# Transformar log solo GR y G6PDH en tentaculo, y GST en ambos

modelos_t[[3]] <- aov(log(GR) ~ cultivo * playa, data = filter(data_1, tejido == "Tentaculo"))
modelos_t[[4]] <- aov(log(GST) ~ cultivo * playa, data = filter(data_1,tejido == "Tentaculo"))

modelos_p[[4]] <- aov(log(GST) ~ cultivo * playa, data = filter(data_1, tejido == "Pie"))
modelos_p[[3]] <-  aov(log(GR) ~ cultivo * playa, data = filter(data_1, tejido == "Pie"))

### Pruebas de normalidad y homocedasticidad ----
sapply(modelos_p, function(x){
  print(shapiro.test(x$residuals))
  print(leveneTest(x))
})
# GR pie necesita transformacion
# G6PDH no cumple homocedasticidad. No se puede analizar. Quitar del modelo y ver alternativas no parametricas.


### Evaluacion de modelos ----
for (i in c(1:8)) {
  print(colnames(select(data_1, -GPx, -G6PDH)[7:14][i]))
  print(summary(modelos_p[[i]]))
}
# Pie: Diferencias significativas en GPx, GR, GST, G6PDH y MDA

for (i in c(1:8)) {
  print(colnames(select(data_1, -GPx, -G6PDH)[7:14][i]))
  print(summary(modelos_t[[i]]))
}
# Tentaculo: diferencia significativas en SOD, CAT, GST, DTD, TEAC y MDA

### TESST POST-HOC y graficas ----
# Rehacemos modelos en clase aov() para el test de tukey
#modelos_p <- lapply(colnames(select(data_1, -GPx)[c(7:15)]), function(x){
#  aov(formula = as.formula(paste0(x, " ~ cultivo + playa")), filter(data_1, tejido == "Pie"))})

#modelos_t <- lapply(colnames(select(data_1, -GPx)[c(7:15)]), function(x){
#  aov(formula = as.formula(paste0(x, " ~ cultivo + playa")), filter(select(data_1, -GPx), tejido == "Tentaculo"))})

# Transformar log solo GR y G6PDH en tentaculo

#modelos_t[[3]] <- aov(log(GR) ~ cultivo + playa, data = filter(data_1, tejido == "Tentaculo"))
#modelos_t[[6]] <- aov(log(G6PDH) ~ cultivo + playa, data = filter(data_1, tejido == "Tentaculo"))
# Bucle de estadisticos de resumen, test port-hoc y grafica

for (n in c(1:9)) { ### PIE
  i <- colnames(select(data_1, -GPx)[c(7:15)])[[n]]
  tabla_summ <- data_1 %>% # Generamos estadisticos de resumen
    filter(tejido == "Pie") %>%   group_by(cultivo, playa) %>% 
    summarise(media = mean(get(i), na.rm = T),
              desvest = sd(get(i), na.rm = T),
              error = desvest/sqrt(sum(!is.na(get(i)))))
  # Test post-hoc Tukey # Aqui tengo que añadir la interaccion
  m.temp <- Anova(modelos_p[[n]], type = "III")
  if (((m.temp[["Pr(>F)"]][3]) <= 0.05) |  ((m.temp[["Pr(>F)"]][4]) <= 0.05)){
    tukey_loop <- TukeyHSD(modelos_p[[n]])
    cld.tukey <- multcompLetters4(modelos_p[[n]], tukey_loop, reversed = T)
    (letras <- rownames_to_column(as.data.frame(cld.tukey$`cultivo:playa`$Letters)))
    colnames(letras) <- c("rowname", "tukey")
    letras <- letras %>% mutate(
      playa = lapply(strsplit(letras$rowname, ":"), `[[`, 2),
      cultivo = lapply(strsplit(letras$rowname, ":"), `[[`, 1)
    ) %>% select(cultivo, playa, tukey)
    # EN EL CASO DE MDA_p hacemos test de fisher porque tukey no detecta diferencias
    if(n == 9){
      fisher.loop <- LSD.test(modelos_p[[n]], c("cultivo", "playa"))
      (letras <- rownames_to_column(as.data.frame(fisher.loop$groups)))
      letras <- letras %>% mutate(
          playa = lapply(strsplit(letras$rowname, ":"), `[[`, 2),
          cultivo = lapply(strsplit(letras$rowname, ":"), `[[`, 1),
          tukey = case_when(groups == "a" ~ "b",
                            groups == "b" ~ "a",
                            TRUE ~ "ab")) %>%  select(-MDA, -rowname, -groups)
    }
    tabla_summ <- merge(tabla_summ, letras)
  } else {
    tabla_summ$tukey <- c("", "", "", "", "", "")
  }
  # Evalua si hay diferencias sig en cultivo y guarda prob y nivel de significacion
  if ((m.temp[["Pr(>F)"]][2]) <= 0.05){
    diferencias  <-  TRUE
    prob <- m.temp[["Pr(>F)"]][2]
    significacion = case_when((prob <= 0.05) & (prob > 0.01) ~ "*",
                              (prob <= 0.01) & (prob > 0.001) ~ "**",
                              (prob <= 0.001) ~ "***",)
  } else{diferencias <- FALSE ; significacion <- ""}
  (p <- barras_tfg())
  if(diferencias) {p <- p + annotate("segment", x = 0.75, xend = 2.25, y =  1.15 * (max(tabla_summ$media) + max(tabla_summ$error)), yend =  1.15 * (max(tabla_summ$media) + max(tabla_summ$error)), color = "gray40", size = 0.8) +
    annotate("segment", x = 0.75, xend = 0.75, y = 1.15 * (max(tabla_summ$media) + max(tabla_summ$error)), yend = 1.1 * (max(tabla_summ$media) + max(tabla_summ$error)), color = "gray40", size = 0.8) +
    annotate("segment", x = 2.25, xend = 2.25, y = 1.15 * (max(tabla_summ$media) + max(tabla_summ$error)), yend = 1.1 * (max(tabla_summ$media) + max(tabla_summ$error)), color = "gray40", size = 0.8) +
    annotate("text", x = 1.5, y = 1.18 * (max(tabla_summ$media) + max(tabla_summ$error)), label = significacion, size = 7)} # Si hubo diferencias, añade elementos a la grafica
  
  # Guardamos graficas
  (p <- p + annotate("text", x = 1.5, y = 1.3 * (max(tabla_summ$media) + max(tabla_summ$error)), label = "paste(italic(Column))", size = 5, color = "gray30", parse = T))
  ggsave(paste0("./resultados/1/", i, "_pie_v2.png"), width = 340, height = 510, units = "px", scale = 2, dpi = "retina")
} # PIE

for (n in c(1:9)) { ### TENTACULO
  i <- colnames(select(data_1, -GPx)[c(7:15)])[[n]]
  tabla_summ <- data_1 %>% # Generamos estadisticos de resumen
    filter(tejido == "Tentaculo") %>%   group_by(cultivo, playa) %>% 
    summarise(media = mean(get(i), na.rm = T),
              desvest = sd(get(i), na.rm = T),
              error = desvest/sqrt(sum(!is.na(get(i)))))
  # Test post-hoc Tukey
  m.temp <- Anova(modelos_t[[n]], type = "III")
  if (((m.temp[["Pr(>F)"]][3]) <= 0.05) |  ((m.temp[["Pr(>F)"]][4]) <= 0.05)){
    tukey_loop <- TukeyHSD(modelos_t[[n]])
    cld.tukey <- multcompLetters4(modelos_t[[n]], tukey_loop, reversed = T)
    (letras <- rownames_to_column(as.data.frame(cld.tukey$`cultivo:playa`$Letters)))
    colnames(letras) <- c("rowname", "tukey")
    letras <- letras %>% mutate(
      playa = lapply(strsplit(letras$rowname, ":"), `[[`, 2),
      cultivo = lapply(strsplit(letras$rowname, ":"), `[[`, 1)
    ) %>% select(cultivo, playa, tukey)
    tabla_summ <- merge(tabla_summ, letras)
  } else {
    tabla_summ$tukey <- c("", "", "", "", "", "")
  }
  # Evalua si hay diferencias sig en cultivo y guarda prob y nivel de significacion
  if ((m.temp[["Pr(>F)"]][2]) <= 0.05){
    diferencias  <-  TRUE
    prob <- m.temp[["Pr(>F)"]][2]
    significacion = case_when((prob <= 0.05) & (prob > 0.01) ~ "*",
                              (prob <= 0.01) & (prob > 0.001) ~ "**",
                              (prob <= 0.001) ~ "***",)
  } else{diferencias <- FALSE ; significacion <- ""}
  (p <- barras_tfg())
  if(diferencias) {p <- p + annotate("segment", x = 0.75, xend = 2.25, y =  1.15 * (max(tabla_summ$media) + max(tabla_summ$error)), yend =  1.15 * (max(tabla_summ$media) + max(tabla_summ$error)), color = "gray40", size = 0.8) +
    annotate("segment", x = 0.75, xend = 0.75, y = 1.15 * (max(tabla_summ$media) + max(tabla_summ$error)), yend = 1.1 * (max(tabla_summ$media) + max(tabla_summ$error)), color = "gray40", size = 0.8) +
    annotate("segment", x = 2.25, xend = 2.25, y = 1.15 * (max(tabla_summ$media) + max(tabla_summ$error)), yend = 1.1 * (max(tabla_summ$media) + max(tabla_summ$error)), color = "gray40", size = 0.8) +
    annotate("text", x = 1.5, y = 1.18 * (max(tabla_summ$media) + max(tabla_summ$error)), label = significacion, size = 7)} # Si hubo diferencias, añade elementos a la grafica
  
  # Guardamos graficas
  (p <- p + annotate("text", x = 1.5, y = 1.3 * (max(tabla_summ$media) + max(tabla_summ$error)), label = "paste(italic(Tentacle))", size = 5, color = "gray30", parse = T) +
      theme(legend.position = "right"))
  ggsave(paste0("./resultados/1/", i, "_tent_v2.png"), width = 510, height = 510, units = "px", scale = 2, dpi = "retina")
} # TENTACULO

### g6pdh ----

m.p <- Anova(aov(log(G6PDH) ~ cultivo * playa, data = filter(data_1, tejido == "Pie")), type = 3)
leveneTest(aov(log(G6PDH) ~ cultivo * playa, data = filter(data_1, tejido == "Tentaculo")))


m.t <- Anova(aov(log(G6PDH) ~ cultivo * playa, data = filter(data_1, tejido == "Tentaculo")), type = 3)
m.t
