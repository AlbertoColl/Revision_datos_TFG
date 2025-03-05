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

setwd("C:/Users/Usuario/Documents/GitHub/Revision_datos_TFG")
#setwd("D:/collf/Documents/GitHub/Revision_datos_TFG")


source(file = "./scripts septiembre 2024-2025/0_data_lab.R")
source(file = "./scripts septiembre 2024-2025/1_funciones_graficas.R")

# Filtrar: solo anemonas cultivadas
data_2 <- filter(datos, cultivo == "cultured")


#### Anova de 3 vias con correción BH ----

#Pivotamos los datos a formato largo para poder agrupar por variable y eliminamos NAs

data_2_long <- data_2 %>% pivot_longer(!c(individuo, playa, corte, cultivo, tiempo), names_to = "variable", values_to = "valor", values_drop_na = TRUE)
data_2_long <- filter(data_2_long, variable != "GPx_t")
data_2_long <- filter(data_2_long, variable != "GPx_p")
data_2_long <- filter(data_2_long, variable != "proteina_p")
data_2_long <- filter(data_2_long, variable != "proteina_")

data_2_long$variable <- as.factor(data_2_long$variable)

# Asunciones del ANOVA
for (i in levels(data_2_long$variable)) {
  modelo_completo  <- lm(valor ~ playa*corte*tiempo, data = filter(data_2_long, variable == i))
  print(i)
  print(shapiro_test(residuals(modelo_completo)))
  print(data_2 %>% levene_test(get(i) ~ playa*corte*tiempo))
} # Solo GPx_p viola normalidad de residuos y aun asi es una enzima que se va a volver a medir o excluir del analisis.

# No tiene sentido analizar la normalidad de residuos por grupo si son 3 puntos por grupo. Por la experiencia sabemos que estas variables se comportarse de forma normal


# Computamos los anovas agrupando por variable y aplicamos correción de BH
res.aov <- data_2_long %>% group_by(variable) %>% anova_test(valor ~ playa*corte*tiempo) %>% adjust_pvalue(method = "BH")
res.aov

objetos_ortimar <- list(res.aov)
# A continuación habria que ir a cada variable una a una y hacer los pertinentes test post hoc, ir incorporando en un informe de markdown.

### Catalasa_pie ----

# No hay interaccion a 3 vias con la correcion de BH
# No hay interacciones a dos vias
# Hay efecto principal del corte y de la playa: es decir, las playas tienen niveles diferentes de catalasa y el corte hace que se modifiquen, pero lo hacen igual para todas las playas e igual a corto y largo plazo.
# ¿Como representamos esto?

# Grafica
tabla_summ <- data_2 %>%
  group_by(playa, corte, tiempo) %>%
  get_summary_stats(CAT_p, type = "mean_se")

(p.cat_p <- ggplot(tabla_summ) +
  geom_errorbar(aes(x = tiempo, ymax = mean + se, ymin = mean- se, color = corte), width = 0.3, position = position_dodge(width = 0.9), linewidth = 1) +
  geom_col(aes(x = tiempo, y = mean, color = corte, fill = corte), alpha = 0.4, linewidth = 1, position = "dodge2") +
  facet_wrap(~playa))

objetos_ortimar <- objetos_ortimar %>% append(list(p.cat_p))


### Catalasa_tent ----

#  Hay interaccion a 3 vias con la correcion de BH

# Descomposicion por playas con alfa = 0.05 / 3 = 0.016667
modelo_completo  <- lm(CAT_t ~ playa*corte*tiempo, data = data_2)
view(data_2 %>%
  group_by(playa) %>%
  anova_test(CAT_p ~ corte*tiempo, error = modelo_completo) %>% 
  adjust_pvalue(method = "BH")) # fijarse en el alfa entre 3

# En Calahonda no hay diferencias significativas
# En Almuñécar hay efecto del corte
# En Salobreña no hay NADA

# Descomposición corte y tiempo en Salobreña:

efecto.tratamiento <- data_2 %>%
  group_by(playa, tiempo) %>%
  anova_test(CAT_t ~ corte, error = modelo_completo)
efecto.tratamiento %>% as.tibble() %>% filter(playa == "Salobreña")
# Hay efecto significativo del corte a largo plazo a nivel alfa 0.05/2 efectos = 0.025





### natural ----





### Ejemplo superoxido dismutasa PIE----
if(FALSE){
# Exploracion
data_2 %>%
  group_by(playa, corte, tiempo) %>%
  get_summary_stats(CAT_p, type = "mean_se")
(bxp <- ggboxplot(data_2, x = "tiempo", y = "SOD_p", 
                  color = "corte", palette = "frontiers", facet.by = "playa",
                  title = "Superoxido dismutasa columnar"))

# Normalidad de residuos general
modelo_completo  <- lm(CAT_t ~ playa*corte*tiempo, data = data_2)
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

# Hay interaccion significativa a alfa = 0.025 solo salobreña. En Calahonda y Almuñecar no hay efectos significativos.

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
(bp <- ggbarplot(tabla_summ, x = "tiempo", y = "mean", fill = "corte", color = "corte", alpha = 0.9, facet.by = "playa", position = position_dodge2()))
    #geom_text(tabla_summ, aes(x = tiempo, y = mean+se, label = tukey, group = corte, )))

  
# Presentacion de resultados
}
