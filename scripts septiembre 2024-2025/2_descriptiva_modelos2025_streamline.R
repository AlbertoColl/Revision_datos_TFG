# ORTIMAR - Objetivo 2
# Analisis efecto del corte
# Febrero 2025 - revision para publicacion

# Vamos a hacer un diseño cuadrado 2x2 dentro de las anemonas cultivadas: la variable corte, que tiene 2 niveles (dissected y control), y la variable tiempo, con niveles 0 y 1. De esa manera esperamos ver el efecto del corte a corto y largo plazo.

# H0: No hay diferencias entre el estado oxidativo de anémonas control y cortadas. H1: Sí existen alguna diferencia.

# H0: No hay interacción entre el corte y el tiempo en cultivo. H1: Hay interacción entre el corte y el tiempo en cultivo.

# H0: No hay diferencias en el estado oxidativo al estar en cultivo a largo plazo. H1: Hay diferencias en el estado oxidativo al estar en cultivo a largo plazo.

# Las muestras son independientes temporalmente ya que no se trata del mismo animal (mismo cestillo?)

# ESTE SCRIPT ES LA VERSION SIN DISCRIMINAR POR PLAYA DE ORIGEN
# Aunque sabemos que siguen tendencias ligeramente diferentes, no tenemos tamaño de muestra para incluirlo como factor en el analisis. La otra opcion es agrupar por playa y hacer los test 3 veces cada uno

# Update 05/3: AL final del script he hecho los analisis separando por playa y no salen muy bonitos, volvemos a perder tamaño de muestra y la interpretacion suele ser identica o muy similar a los resultados agrupando playas.

library(tidyverse)
library(multcompView)
library(car)
library(rstatix)
library(ggpubr)

### SETUP y filtrado de datos ----

#setwd("C:/Users/Usuario/Documents/GitHub/Revision_datos_TFG")
setwd("D:/collf/Documents/GitHub/Revision_datos_TFG")

#source(file = "./scripts septiembre 2024-2025/0_data_lab.R")
source(file = "./scripts septiembre 2024-2025/0_data_laptop.R")

source(file = "./scripts septiembre 2024-2025/1_funciones_graficas.R")

# Filtrar: solo anemonas cultivadas
data_2 <- filter(datos, cultivo == "cultured")

### Exploracion ----

ggplot(data_2, aes(y = Fbasica_p)) +
  geom_boxplot(aes(x = tiempo:corte, color = tiempo:corte), alpha = 0) +
  geom_point(aes(x = tiempo:corte, color = tiempo:corte), alpha = 1, size = 2)

# Deteccion de outliers en SOD_t y CAT_t para normalidad de residuos
# view(data_2 %>%  group_by(corte:tiempo) %>%  identify_outliers(CAT_t))

# Se eliminan:
data_2$SOD_t[7] <- NA
data_2$SOD_t[3] <- NA
data_2$CAT_t[35] <- NA # La funcion ha identificado otro, pero su eliminacion no afecta a la normalidad de residuos, este si.
data_2$Mielo_p[26] <- NA
data_2$Mielo_t[18] <- NA #Sospechoso, y afecta a normalidad
data_2$Facida_p[30] # sospechoso, outlier no extremo
data_2$Fbasica_p[12] <- NA # outlier extremo
data_2$Fbasica_t[9] <- NA # Afecta a homocedasticidad
data_2$Lisozima_p[33] <- NA # outlier extremo
data_2$Lisozima_p[34] <- NA # outlier extremo
data_2$Lisozima_p[2] <- NA # outlier extremo



### Ajuste de modelos ----

# Si se elimina alguna enzima al final, eliminarla aquí
#data_2 <- data_2 %>%   select(-Lisozima_p, -GPx_t, -GPx_p)

# Ajuste de modelos ANOVA con rstatix
modelos <- lapply(colnames(data_2[c(5:31)]), function(x){
  anova_test(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2)})
(anova_results <- reduce(modelos, full_join) %>% 
    add_column(.before = 1, variable = rep(colnames(data_2[c(5:31)]), each = 3)) %>% 
    adjust_pvalue(method = "BH"))


# Test de Levene se computa exactamente igual
modelos_levene <- lapply(colnames(data_2[c(5:31)]), function(x){
  levene_test(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2)})
(levene_results <- reduce(modelos_levene, full_join) %>% 
    add_column(.before = 1, variable = colnames(data_2[c(5:31)])))

# Para el test de Shapiro-Wilks se ajustan lm para extraer residuos

modelos_lm <- lapply(colnames(data_2[c(5:31)]), function(x){
  lm(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2)})
modelos_shapiro <- lapply(modelos_lm, function(x){
  shapiro_test(residuals(x))})
(shapiro_results <- reduce(modelos_shapiro, full_join) %>% 
    add_column(.before = 1, parametro = colnames(data_2[c(5:31)])))

# Sin filtrar, no se cumple normalidad de residuos en el caso de:
# SOD_t (solucionado), CAT_t (solucionado), Fbasica_p (solucionado), Mielo_p (solucionado), Mielo_t (solucionado), lisozima_p (solucionado)

# No hay homocedasticidad en:
# Fbasica_t (solucionado)

### Construccion de graficas ----

# Funcion post-hoc

posthoc <- function(){
  if (any(filter(as.tibble(anova_results), variable == i)$p.adj <= 0.05, na.rm = T)){
    print("There is at least one significative effect")
    if (filter(as.tibble(anova_results), variable == i)$p.adj[3] <= 0.05){
      print("Interaction is significative")
    }
    else{
      print("Interaction is not significative")
      if (any(filter(as.tibble(anova_results), variable == i)$p.adj[-3] <= 0.05, na.rm = F)){
        print("One main effect is significative")
      }
      else{
        print("Two main effects are significative")
      }
    }
    
  }
  else{print("There is no significative effects")}
  
}

### ACTUALIZAR Y CAMBIAR BUCLE

for (n in c(1:27)) {
  i <- colnames(data_2[5:31])[[n]]
  tabla_summ <- data_2 %>% group_by(corte:tiempo) %>% 
    get_summary_stats(i, type = "mean_se") %>% 
    separate_wider_delim(`corte:tiempo`, ":", names = c("tratamiento", "tiempo"), cols_remove=F)
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
  if (n != 5){
  (p <- barras_tfg() + labs(subtitle = case_when(str_detect(i, "_p") == T  ~ "A",
                                                 str_detect(i, "_t") == T ~ "B",
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


# Separacion por playas, no usada
if(FALSE){
  ### Separacion por playas - CALAHONDA ----
  
  data_2c <- filter(datos, cultivo == "cultured", playa == "Calahonda")
  
  ggplot(data_2c, aes(y = CAT_t)) +
    geom_boxplot(aes(x = tiempo:corte, color = tiempo:corte), alpha = 0) +
    geom_point(aes(x = tiempo:corte, color = tiempo:corte), alpha = 1, size = 2)
  
  data_2c$SOD_p[11] # Sospechoso, se va mucho de la tendencia con los otros dos puntos
  
  data_2c <- data_2c %>%   select(-GPx_t, -GPx_p)
  
  
  modelos_c <- lapply(colnames(data_2c[c(5:29)]), function(x){
    anova_test(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2c)})
  (anova_results_c <- reduce(modelos_c, full_join) %>% 
      add_column(.before = 1, variable = rep(colnames(data_2c[c(5:29)]), each = 3)) %>% 
      adjust_pvalue(method = "BH"))
  
  
  # Test de Levene se computa exactamente igual
  modelos_levene_c <- lapply(colnames(data_2c[c(5:29)]), function(x){
    levene_test(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2c)})
  (levene_results_c <- reduce(modelos_levene_c, full_join) %>% 
      add_column(.before = 1, variable = colnames(data_2c[c(5:29)])))
  
  # Para el test de Shapiro-Wilks se ajustan lm para extraer residuos
  
  modelos_lm_c <- lapply(colnames(data_2c[c(5:29)]), function(x){
    lm(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2c)})
  modelos_shapiro_c <- lapply(modelos_lm_c, function(x){
    shapiro_test(residuals(x))})
  (shapiro_results_c <- reduce(modelos_shapiro_c, full_join) %>% 
      add_column(.before = 1, parametro = colnames(data_2c[c(5:29)])))
  
  
  # Problemas con normalidad en:
  # ninguno
  
  #Problemas con homocesdaticidad en:
  # CAT_t -> no se resuelve, es leve
  
  ### Separacion por playas - SALOBREÑA ----
  
  data_2s <- filter(datos, cultivo == "cultured", playa == "Salobreña")
  
  ggplot(data_2s, aes(y = CAT_t)) +
    geom_boxplot(aes(x = tiempo:corte, color = tiempo:corte), alpha = 0) +
    geom_point(aes(x = tiempo:corte, color = tiempo:corte), alpha = 1, size = 2)
  
  data_2s <- data_2s %>%   select(-GPx_t, -GPx_p)
  
  
  modelos_s <- lapply(colnames(data_2s[c(5:29)]), function(x){
    anova_test(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2s)})
  (anova_results_s <- reduce(modelos_s, full_join) %>% 
      add_column(.before = 1, variable = rep(colnames(data_2s[c(5:29)]), each = 3)) %>% 
      adjust_pvalue(method = "BH"))
  
  
  # Test de Levene se computa exactamente igual
  modelos_levene_s <- lapply(colnames(data_2s[c(5:29)]), function(x){
    levene_test(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2s)})
  (levene_results_s <- reduce(modelos_levene_s, full_join) %>% 
      add_column(.before = 1, variable = colnames(data_2s[c(5:29)])))
  
  # Para el test de Shapiro-Wilks se ajustan lm para extraer residuos
  
  modelos_lm_s <- lapply(colnames(data_2s[c(5:29)]), function(x){
    lm(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2s)})
  modelos_shapiro_s <- lapply(modelos_lm_s, function(x){
    shapiro_test(residuals(x))})
  (shapiro_results_s <- reduce(modelos_shapiro_s, full_join) %>% 
      add_column(.before = 1, parametro = colnames(data_2s[c(5:29)])))
  
  
  # Problemas con normalidad en:
  # CAT_t, proteina_t
  
  
  
  #Problemas con homocesdaticidad en:
  # Mielo_p
  
  ### Separacion por playas - ALMUÑECAR ----
  
  data_2a <- filter(datos, cultivo == "cultured", playa == "Almuñecar")
  
  ggplot(data_2a, aes(y = CAT_t)) +
    geom_boxplot(aes(x = tiempo:corte, color = tiempo:corte), alpha = 0) +
    geom_point(aes(x = tiempo:corte, color = tiempo:corte), alpha = 1, size = 2)
  
  data_2a <- data_2a %>%   select(-GPx_t, -GPx_p)
  
  
  modelos_a <- lapply(colnames(data_2a[c(5:29)]), function(x){
    anova_test(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2a)})
  (anova_results_a <- reduce(modelos_a, full_join) %>% 
      add_column(.before = 1, variable = rep(colnames(data_2a[c(5:29)]), each = 3)) %>% 
      adjust_pvalue(method = "BH"))
  
  
  # Test de Levene se computa exactamente igual
  modelos_levene_a <- lapply(colnames(data_2a[c(5:29)]), function(x){
    levene_test(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2a)})
  (levene_results_a <- reduce(modelos_levene_a, full_join) %>% 
      add_column(.before = 1, variable = colnames(data_2a[c(5:29)])))
  
  # Para el test de Shapiro-Wilks se ajustan lm para extraer residuos
  
  modelos_lm_a <- lapply(colnames(data_2a[c(5:29)]), function(x){
    lm(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2a)})
  modelos_shapiro_a <- lapply(modelos_lm_a, function(x){
    shapiro_test(residuals(x))})
  (shapiro_results_a <- reduce(modelos_shapiro_a, full_join) %>% 
      add_column(.before = 1, parametro = colnames(data_2a[c(5:29)])))
  
  
  # Problemas con normalidad en:
  # 
  
  #Problemas con homocesdaticidad en:
  #  
  
}
