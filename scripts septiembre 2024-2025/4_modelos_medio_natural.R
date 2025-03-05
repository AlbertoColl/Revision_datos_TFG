# ORTIMAR - Objetivo 1
# Analisis adaptacion al cultivo
# Marzo 2025 - revision para publicacion

## En este objetivo queremos analizar si las anemonas procendentes de tres entornos naturales distintos se adaptan de diferente manera al cultivo.
## Estamos indecisos entre un modelo 2x3 variable ~ cultivo (natural, cultivadas) * playa (Calahonda, Almuñecar, Salobreña)
# o un modelo 3x3 variable ~ cultivo (natural, t0, t1) * playa (Calahonda, Almuñecar, Salobreña)

# Vamos a comparar los dos, aunque yo de base me decanto mas por el 2x3 para no repetir tantos datos

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
data_1 <- filter(datos, corte != "dissected", tiempo != 1)

### Exploracion ----

ggplot(data_1, aes(y = G6PDH_p)) +
  geom_boxplot(aes(x = playa, color = playa), alpha = 0) +
  geom_point(aes(x = playa, color = playa), alpha = 1, size = 2) +
  facet_wrap(~cultivo)
# Problema: salobreña sigue una tendencia diferente en algunas enzimas como la catalasa

# Deteccion de outliers en SOD_t y CAT_t para normalidad de residuos
view(data_1 %>% 
       group_by(cultivo:playa) %>%
       identify_outliers(G6PDH_p))

# Se eliminan:
data_1$GR_t[9]  <- NA # Outlier no extremo, pero afecta a la normalidad de la variable
data_1$MDA_t[1]  <- NA # El que mas se desvia en el qqplot
data_1$Fbasica_p[11]  <- NA # Outlier extremo y afecta a normalidad
data_1$Fbasica_t[5]  <- NA # Outlier extremo y afecta a normalidad
data_1$Fbasica_t[9]  <- NA # Outlier extremo y afecta a normalidad
data_1$Lisozima_p[1]  <- NA # Outlier extremo y afecta a normalidad

data_1$G6PDH_p[7] # La grafica de residuos vs ajustados los marca
data_1$G6PDH_p[9] # La grafica de residuos vs ajustados los marca



### Ajuste de modelos ----

# Si se elimina alguna enzima al final, eliminarla aquí
#data_2 <- data_2 %>%   select(-Lisozima_p, -GPx_t, -GPx_p)

# Ajuste de modelos ANOVA con rstatix
modelos <- lapply(colnames(data_1[c(5:31)]), function(x){
  anova_test(formula = as.formula(paste0(x, " ~ playa * cultivo")), data_1)})
(anova_results <- reduce(modelos, full_join) %>% 
    add_column(.before = 1, variable = rep(colnames(data_1[c(5:31)]), each = 3)) %>% 
    adjust_pvalue(method = "BH"))


# Test de Levenese computa exactamente igual
modelos_levene <- lapply(colnames(data_1[c(5:31)]), function(x){
  levene_test(formula = as.formula(paste0(x, " ~ playa * cultivo")), data_1)})
(levene_results <- reduce(modelos_levene, full_join) %>% 
    add_column(.before = 1, variable = colnames(data_1[c(5:31)])))

# Para el test de Shapiro-Wilks se ajustan lm para extraer residuos

modelos_lm <- lapply(colnames(data_1[c(5:31)]), function(x){
  lm(formula = as.formula(paste0(x, " ~ playa * cultivo")), data_1)})
modelos_shapiro <- lapply(modelos_lm, function(x){
  shapiro_test(residuals(x))})
(shapiro_results <- reduce(modelos_shapiro, full_join) %>% 
    add_column(.before = 1, parametro = colnames(data_1[c(5:31)])))

#Para ver que puntos afectan a la normalidad de residuos:
qqpoints <- qqnorm(residuals(modelos_lm[[12]])) # cambiar por el n de modelo que sea
identify(qqpoints) # Clicas en el dato y le das a ESC, te dice el indice del punto

# Sin filtrar, no se cumple normalidad de residuos en el caso de:
# GR_t (solucionada), MDA_t (solucionado), Fbasica_p (solucionado), Fbasica_t (solucionado), Lisozima_p (solucionado)

# No hay homocedasticidad en:
# G6PDH_p (solucionado con la grafica de diagnostico)

# Una vez conseguidas las asunciones, se pueden interpretar los resultados del modelo.
# RECORDATORIO:
# 1º Comprobar si existe interaccion entre las variables. 
# 2º Si la hay, significa que los valores entre naturales y cultivadas son diferentes en cada playa, o lo que es lo mismo, las diferencias entre playas son diferentes en salvajes y cultivadas. Esto quiere decir que se puede descomponer el modelo en dos ANOVAs de 1 via: 1 para salvajes y otra para cultivadas. Si alguno es significativo, se procede con Tukey

#2º Si no la hay, significa que la adaptación al cultivo es similar para todas las playas. Se pasa a ver si hay efecto significativo del cultivo o de la playa. Para cada uno de ellos, habrá que realizar Tukey o no segun el numero degrupos. Para playa se hace test de tukey o t test multiple. Para cultivo no hace falta pues son solo dos niveles de la variable

### Bucle de construccion de graficas ----


### ACTUALIZAR Y CAMBIAR BUCLE

# Falta:
# Comprobar normas de la revista
# Actualizar script de graficas
# Actualizar llamada al script en este bucle
# Actualizar directorio de salida de las graficas

for (n in c(1:27)) {
  i <- colnames(data_1[5:31])[[n]]
  tabla_summ <- data_1 %>% group_by(cultivo:playa) %>% 
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


