### Analisis estadistico - Evaluacion del bienestar animal en ortiguilla de mar (Anemonia sulcata) bajo condiciones de cultivo
### Alberto Coll Fernandez - 15/05/22 - 6/06/22

## SETUP ----
library(tidyverse)
library(ggthemr)
library(broom)
library(car)
library(multcompView)

ggthemr("fresh")

setwd("C:/Users/Usuario/Documents/ORTIMAR (Alberto Coll)")
datos <- read.csv2("~/ORTIMAR (Alberto Coll)/datos/TFG_datos.csv", numerals = "warn.loss") %>% 
   mutate(playa = as.factor(playa), tejido = as.factor(tejido),
          cultivo = as.factor(cultivo), corte = as.factor(corte),
          madurez = as.factor(madurez))
datos <- datos %>% 
   mutate(GPx = ifelse(GPx < 0, NA, GPx))

# Ajustes del SEM (valores que desvian la media excesivamente)
datos[27,8] <- NA
datos[31,7] <- NA
datos[77,15] <- NA
datos[33,15] <- NA
datos[27,15] <- NA
datos[1,14] <- NA
datos[33,14] <- NA
datos[82,10] <- NA
datos[62,11] <- NA
datos[27,12] <- NA
datos[32,13] <- NA
datos[27,13] <- NA
datos[22,14] <- NA
datos[17,14] <- NA
datos[16,8] <- NA
datos[20,13] <- NA
datos[67,11] <- NA
datos[78,10] <- NA
datos[89,10] <- NA
datos[38,13] <- NA
datos[39,14] <- NA
datos[100,10] <- NA
datos[36,12] <- NA
datos[100,13] <- NA
datos[101,13] <- NA
datos[43,13] <- NA
datos[91,15] <- NA

## TRANSFORMACIONES ----

# Se comprob? que todas las variables segu?an distribuci?n normal usando el test de
# Shapiro-Wilkers. A aquellas que difer?an significativamente, se les aplic? una
# transformaci?n logar?tmica o ra?z cuadrada hasta alcanzar la normalidad.

shapiro.test(sqrt(filter(datos, tejido == "Tentaculo")$GPx))  #Ejemplo test con tranformacion

# Por comodidad, voy a crear nuevas variables ya transformadas
datos <- datos %>% 
   mutate(SOD_log = log(SOD),
          GPx_tf = ifelse(tejido == "Pie", log(GPx), sqrt(GPx)),
          GR_tf = ifelse(tejido == "Pie", sqrt(GR), log(GR)),
          GST_tf = ifelse(tejido == "Pie", GST, log(GST)),
          DTD_tf = ifelse(tejido == "Pie", DTD, log(DTD)),
          G6PDH_log = log(G6PDH)) %>% 
   select(individuo, playa, corte, madurez, cultivo, tejido,
          SOD_log, CAT, GPx_tf, GR_tf, GST_tf, DTD_tf, G6PDH_log,
          TEAC, MDA, proteina)

ceros <- c(34,35,36,37,38,39,40,41,42,43,44,45,76,77,78,85,86,87,95)
for (i in ceros) {
   datos[i,9] <- 0
} # Esto para evitar que sean 0 en la grafica
# ifelse son porque algunas eran normales en un tejido solo, o necesitaba otra transformacion

## OBJETIVO 1 ----

# Examinar efecto de cultivo  sobre indicadores en anemonas inmaduras
# Generar formulas con lapply
formulas <- lapply(colnames(datos[c(7:16)]), function(x){as.formula(paste0(x, " ~ cultivo * playa"))})
# Correr los modelos con lapply tambien
m1_pie <- lapply(formulas, function(x){
   aov(x, data = filter(datos, tejido == "Pie", corte == "No", madurez == "No"))
})
m1_tent <- lapply(formulas, function(x){
   aov(x, data = filter(datos, tejido == "Tentaculo", corte == "No", madurez == "No"))
})

results1_pie <- lapply(m1_pie, function(x){anova(x)})
results1_tent <- lapply(m1_tent, function(x){anova(x)})

# Ordenar el output en una tabla
names(results1_pie) <- format(formulas)
results1_pie <- map_df(results1_pie, tidy, .id = 'formulas')
results1_pie <- results1_pie
names(results1_tent) <- format(formulas)
results1_tent <- map_df(results1_tent, tidy, .id = 'formulas')

# Exportar a csv
modelo1_final <-rbind(results1_pie, results1_tent) #Primero pie, luego tent
write.csv2(modelo1_final, file = "modelo1_resultados.csv")

## ASUNCIONES: HOMOCEDASTICIDAD Y NORMALIDAD DE RESIDUOS

# Normalidad de residuos con test de Shapiro-Wilk
shapiro1_pie <- lapply(m1_pie, function(x){shapiro.test(residuals(x))})
shapiro1_tent <- lapply(m1_tent, function(x){shapiro.test(residuals(x))})
for (i in shapiro1_pie) {
   print(i$p.value)
}
for (i in shapiro1_tent) {
   print(i$p.value)
}

# Homocedasticidad con test de Levenne
levene1_pie <- lapply(formulas, function(x){
   leveneTest(x, data = filter(datos, tejido == "Pie", corte == "No", madurez == "No"))
})
levene1_tent <- lapply(formulas, function(x){
   leveneTest(x, data = filter(datos, tejido == "Tentaculo", corte == "No", madurez == "No"))
})
levene1_pie
levene1_tent

## OBJETIVO 2 ----

## Examinar efecto de corte sobre indicadores en anemonas cultivadas inmaduras

# Generar formulas con lapply
formulas <- lapply(colnames(datos[c(7:16)]), function(x){as.formula(paste0(x, " ~ corte * playa"))})

# Ahora hacemos los modelos y luego sacamos las tablas anova para los resultados
m2_pie <- lapply(formulas, function(x){
   aov(x, data = filter(datos, tejido == "Pie", cultivo == "Si", madurez == "No"))
})
m2_tent <- lapply(formulas, function(x){
   aov(x, data = filter(datos, tejido == "Tentaculo", cultivo == "Si", madurez == "No"))
})

results2_pie <- lapply(m2_pie, function(x){anova(x)})
results2_tent <- lapply(m2_tent, function(x){anova(x)})

# Ordenar el output en una tabla
names(results2_pie) <- format(formulas)
results2_pie <- map_df(results2_pie, tidy, .id = 'formulas')
names(results2_tent) <- format(formulas)
results2_tent <- map_df(results2_tent, tidy, .id = 'formulas')

# Exportar a csv
modelo2_final <-rbind(results2_pie, results2_tent) #Primero pie, luego tent
write.csv2(modelo2_final, file = "modelo2_resultados.csv")

## ASUNCIONES: HOMOCEDASTICIDAD Y NORMALIDAD DE RESIDUOS

# Normalidad de residuos con test de Shapiro-Wilk
shapiro2_pie <- lapply(m2_pie, function(x){shapiro.test(residuals(x))})
shapiro2_tent <- lapply(m2_tent, function(x){shapiro.test(residuals(x))})
for (i in shapiro2_pie) {
   print(i$p.value)
}
for (i in shapiro2_tent) {
   print(i$p.value)
}

# Homocedasticidad con test de Levenne
levene2_pie <- lapply(formulas, function(x){
   leveneTest(x, data = filter(datos, tejido == "Pie", cultivo == "Si", madurez == "No"))
})
levene2_tent <- lapply(formulas, function(x){
   leveneTest(x, data = filter(datos, tejido == "Tentaculo", cultivo == "Si", madurez == "No"))
})
levene2_pie
levene2_tent

## OBJETIVO 3 ----

# Examinar efecto de maduracion natural sobre indicadores en anemonas cultivadas

# Generar formulas con lapply
formulas <- lapply(colnames(datos[c(7:16)]), function(x){as.formula(paste0(x, " ~ madurez * playa"))})

# Correr los modelos con lapply tambien
m3_pie <- lapply(formulas, function(x){
   aov(x, data = filter(datos, tejido == "Pie", cultivo == "Si", corte == "No"))
})
m3_tent <- lapply(formulas, function(x){
   aov(x, data = filter(datos, tejido == "Tentaculo", cultivo == "Si", corte == "No"))
})

results3_pie <- lapply(m3_pie, function(x){anova(x)})
results3_tent <- lapply(m3_tent, function(x){anova(x)})

# Ordenar el output en una tabla
names(results3_pie) <- format(formulas)
results3_pie <- map_df(results3_pie, tidy, .id = 'formulas')
names(results3_tent) <- format(formulas[-3])
results3_tent <- map_df(results3_tent, tidy, .id = 'formulas')

# Exportar a csv
modelo3_final <-rbind(results3_pie, results3_tent) #Primero pie, luego tent
write.csv2(modelo3_final, file = "modelo3_resultados.csv")


## ASUNCIONES: HOMOCEDASTICIDAD Y NORMALIDAD DE RESIDUOS

# Normalidad de residuos con test de Shapiro-Wilk
shapiro3_pie <- lapply(m3_pie, function(x){shapiro.test(residuals(x))})
shapiro3_tent <- lapply(m3_tent, function(x){shapiro.test(residuals(x))})
for (i in shapiro3_pie) {
   print(i$p.value)
}
for (i in shapiro3_tent) {
   print(i$p.value)
}

# Homocedasticidad con test de Levenne
levene3_pie <- lapply(formulas, function(x){
   leveneTest(x, data = filter(datos, tejido == "Pie", cultivo == "Si", corte == "No"))
})
levene3_tent <- lapply(formulas, function(x){
   leveneTest(x, data = filter(datos, tejido == "Tentaculo", cultivo == "Si", corte == "No"))
})
levene3_pie
levene3_tent


## OBJETIVO 4 ----
# Examinar efecto de maduracion inducida sobre indicadores en anemonas cultivadas
# Generar formulas con lapply
formulas <- lapply(colnames(datos[c(7:16)]), function(x){as.formula(paste0(x, " ~ corte * playa"))})
# Correr los modelos con lapply tambien
m4_pie <- lapply(formulas, function(x){
   aov(x, data = filter(datos, tejido == "Pie", cultivo == "Si", madurez == "Si"))
})
m4_tent <- lapply(formulas[-3], function(x){
   aov(x, data = filter(datos, tejido == "Tentaculo", cultivo == "Si", madurez == "Si"))
})

results4_pie <- lapply(m4_pie, function(x){anova(x)})
results4_tent <- lapply(m4_tent, function(x){anova(x)})

# Ordenar el output en una tabla
names(results4_pie) <- format(formulas)
results4_pie <- map_df(results4_pie, tidy, .id = 'formulas')
names(results4_tent) <- format(formulas[-3])
results4_tent <- map_df(results4_tent, tidy, .id = 'formulas')

# Exportar a csv
modelo4_final <-rbind(results4_pie, results4_tent) #Primero pie, luego tent
write.csv2(modelo4_final, file = "modelo4_resultados.csv")

## ASUNCIONES: HOMOCEDASTICIDAD Y NORMALIDAD DE RESIDUOS

# Normalidad de residuos con test de Shapiro-Wilk
shapiro4_pie <- lapply(m4_pie, function(x){shapiro.test(residuals(x))})
shapiro4_tent <- lapply(m4_tent, function(x){shapiro.test(residuals(x))})
for (i in shapiro4_pie) {
   print(i$p.value)
}
for (i in shapiro4_tent) {
   print(i$p.value)
}

# Homocedasticidad con test de Levenne
levene4_pie <- lapply(formulas, function(x){
   leveneTest(x, data = filter(datos, tejido == "Pie", cultivo == "Si", madurez == "Si"))
})
levene4_tent <- lapply(formulas, function(x){
   leveneTest(x, data = filter(datos, tejido == "Tentaculo", cultivo == "Si", madurez == "Si"))
})
levene4_pie
levene4_tent

## Tests de Tukey + graficas ----

# Ir cambiando variable y formula porque paso de hacer bucle para esto (son 23)
modelo_p <- aov(proteina ~ corte * playa,
              data = filter(datos, tejido == "Pie", cultivo == "Si", madurez == "Si"))
tukey_p <- TukeyHSD(modelo_p)
cld.tukey_p <- multcompLetters4(modelo_p, tukey_p)

modelo_t <- aov(proteina ~ corte * playa,
                data = filter(datos, tejido == "Tentaculo", cultivo == "Si", madurez == "Si"))
tukey_t <- TukeyHSD(modelo_t)
cld.tukey_t <- multcompLetters4(modelo_t, tukey_t)


stats_summary <- datos %>%
   filter(cultivo == "Si", madurez == "Si") %>%
   #mutate(SOD_log = ifelse(tejido == "Pie", exp(SOD_log), exp(SOD_log))) %>% # Detransformar variable
   group_by(tejido, corte, playa) %>% 
   summarise(n = n(),
             media = mean(proteina, na.rm = T),
             SD = sd(proteina, na.rm = T)) %>% 
   mutate(SE = SD/sqrt(n),
          SE_per = SE*100/media)

cld_p <- as.data.frame.list(cld.tukey_p$`corte:playa`) %>% 
   rownames_to_column %>% 
   mutate(corte = ifelse(grepl("No", rowname), "No", "Si"),
          playa = case_when(grepl("Almu?ecar", rowname) ~ "Almu?ecar",
                            grepl("Calahonda", rowname) ~ "Calahonda",
                            grepl("Salobre?a", rowname) ~ "Salobre?a"),
          tejido = "Pie") %>% 
   select(Letters, corte, playa, tejido)

cld_t <- as.data.frame.list(cld.tukey_t$`corte:playa`) %>% 
   rownames_to_column %>% 
   mutate(corte = ifelse(grepl("No", rowname), "No", "Si"),
          playa = case_when(grepl("Almu?ecar", rowname) ~ "Almu?ecar",
                            grepl("Calahonda", rowname) ~ "Calahonda",
                            grepl("Salobre?a", rowname) ~ "Salobre?a"),
          tejido = "Tentaculo") %>% 
   select(Letters, corte, playa, tejido)

#cld_p$Letters <- c("", "", "", "", "", "")

stats_summary <- merge(stats_summary, rbind(cld_p, cld_t))

(plot <- ggplot(data = stats_summary) +
      geom_errorbar(aes(x = corte, ymin = media - SE, ymax = media + SE),
                    position = position_dodge2(), color = "gray5", show.legend = F) +
      geom_bar(aes(x = corte, y = media, fill = playa),
               stat = "identity", position = "dodge") +
      geom_text(aes(label = Letters, y = media, x = corte),
                position = position_dodge2(0.9), size = 4,
                vjust = -3, hjust = -0.5, colour = "gray5") +
      ylab("mg / ml") +
      xlab("") +
      facet_wrap(~ tejido, scales = "free") +
      ylim(c(0,12)) +
      labs(title = "Concentraci?n de prote?na",
           fill = "Playa de origen") +
      scale_x_discrete(labels = c("No cortados", "Cortados")) +
      theme(strip.text = element_text(face = "bold", size = 11)))

ggsave("plots/objetivo_4/plot4_proteina.png", width = 11, height = 6, units = "in" )
