### Analisis estadistico TFG 1                                          Efecto de la reproducción en cautividad

# Version II (optimizada)

# Alberto Coll Fernandez                                                   Comienzo: 07/12/2022                                                       Fin : 21/12/2022 (Pausa) Retomado el 13/03/2023

# Problemas y aspectos a resolver:                                                  - Implementar test de Tukey en bucle, con ifelse para que solo lo             haga a los que resulten significativos, y al resto les rellene las           letras con "". Problema, igual es mejor hacerlo manualmente si no           son muchas, ya que a Cristina no le gusta cómo opera el test de             tukey en anova de 2 vias.                                                 - Hacer bucle for o sapply que ajuste un modelo sin interaccion, lo           compare con el modelo ya ajustado, extraiga el p-valor y en caso            de que sea significativo, reemplace el modelo en lista por el               parsimonioso   

### SETUP ----
library(tidyverse)
library(ggthemr)
library(multcompView)
library(car)

# Directorio en laboratorio: C:/Users/Usuario/Documents/TFG Alberto Coll

# Directorio en portatil: D:/collf/Documents/GitHub/TFG-Alberto-Coll

setwd("D:/collf/Documents/GitHub/TFG-Alberto-Coll")

# Llamamos el script de lectura
#source(file = "./scripts/0_data_lab.R") # Laboratorio
source(file = "./scripts/0_data_home.R") # En casa
datos <- datos %>% filter(cultivo == "Si")

source(file = "./scripts/1_descriptiva_cultivadas.R") # Para tener las funciones de las graficas

### Ajuste de modelo EXTENDIDO ----

# Creamos una lista de formulas con sapply(), y posteriormente aplicamos a dicha lista el aov()

formulas <- sapply(colnames(datos[c(6:25)]), function(x){as.formula(paste0(x, " ~ playa + corte * tiempo"))}) # En el analisis extendido vamos a ver la interaccion entre corte y tiempo, ademas de incluir el factor playa

modelos <- sapply(formulas, function(x){lm(x, datos)})

# Guardar todos los modelos individualmente en ./resultados/modelos
# Posteriormente se pueden cargar con readRDS( y renombrarlos)

n <-  6
for (i in modelos) {
  saveRDS(i, file = paste0("./resultados/modelos/modelo", colnames(datos[n]), "_extendido.RDS"))
  n <- n + 1
}

# Ahora comprobamos los modelos a ver que tal y en cuales quitamos interaccion (o playa)
sapply(modelos, function(x){
  summary(x)
})


### Asunciones ----
# Siguiente paso: necesitamos ver si los modelos cumnplen asunciones
# Normalidad de residuos: qqplot y test de shapiro wilk
# Homocedasticidad: test de levene

sapply(modelos, function(x){
  shapiro.test(residuals(x)) 
}) # No hay problemas con normalidad de residuos en ningún caso, lo cual esta genial

sapply(modelos, function(x){
  plot(x, which = 3)
  title(main = x$terms[[2]])
})
# En cuanto a homocedasticidad, examinando las graficas de Scale-Location
# G6PDH_p parece un poco rara
# GPx_p regular
# TEAC_t un poco rara
# G6PDH_t mal


### Test post-hoc ----

# Necesito en primer lugar extraer los p valores de los modelos, utilizando la funcion anova() y unlist() que los convierte en vectores
# El elemento 1 es el p valor del corte, el 2 de la playa, y el 3 de la interaccion
# Los tengo que juntar todos en un vector de vectores

p_valores <- sapply(modelos.pie, function(x){
  unlist(anova(x)$`Pr(>F)`)[c(1:3)]
})
sign <- p_valores < 0.05 # Igual pero podemos quedarnos con los TRUE

# Creamos la lista de letras y usarla luego en las graficas
letras_t <- list()
for (j in c(1:10)) {
  letras_t[[j]] <- c("","","","","","")
}
letras_p <- list()
for (j in c(1:10)) {
  letras_p[[j]] <- c("","","","","","")
}

# Ahora para el test de tuckey, primero:
# -Solo lo hacemos cuando hay TRUE en sign
# -Usamos TukeyHSD(modelo)
# -Usamos multcompLetters4(modelo, tukey)
# -Extraemos letras




tukey.sod <- TukeyHSD(m.sod_t)
(cld.tukey1 <- multcompLetters4(m.sod_t, tukey.sod)) # SOD tentaculo
#datos_resumen$letras_SOD <- c("", "a", "", "ab", "", "b")
letras[[1]] <- c("", "ab", "", "a", "", "b")

tukey.cat <- TukeyHSD(m.cat_t)
(cld.tukey2 <- multcompLetters4(m.cat_t, tukey.cat)) # CAT tentaculo
#datos_resumen$letras_CAT <- c("", "b", "", "b", "", "a")
letras[[2]] <- c("", "a", "", "a", "", "b")


tukey.gpx <- TukeyHSD(m.gpx_p)
(cld.tukey3 <- multcompLetters4(m.gpx_p, tukey.gpx)) #GPx pie
#datos_resumen$letras_GPx <- c("b", "", "a", "", "b", "")
letras[[3]] <-c("b", "", "a", "", "a", "")


### Graficas finales ----

# Establecer diseño de las graficas (#EF476F otro color)
theme_ortimar <- function(){
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 11, face = "bold", vjust = 4),
        axis.title.y = element_text(size = 14)
  )
}
ggthemr("pale")

media = NULL
desvest = NULL
error = NULL
tukey = NULL

grafica_barras <- function(data){
  ggplot(data = l_data) +
    geom_errorbar(aes(x = playa, ymax = media + error, ymin = media - error),
                  color = "grey40", width =  0.7) +
    geom_col(aes(x = playa, y = media, fill = playa)) +
    geom_text(aes(x = playa, y = media + error, label = tukey),
              color = "grey5", vjust = -0.5, size = 4) +
    facet_wrap(~tejido) +
    labs(title = case_when(
      i == "proteina" ~ "Protein concentration",
      i == "MDA" ~ "MDA concentration",
      i == "TEAC" ~ "Trolox equivalent antioxidant capacity (TEAC)",
      TRUE ~ paste( i,"activity in pedal disk and tentacle\n\n")),
         fill = "Sampling\npoint") +
    ylab(case_when(
      i == "proteina" ~ "mg/ml",
      i == "MDA" ~ "μM",
      i == "TEAC" ~ "μM Trolox equivalent",
      TRUE ~ "U / mg  of protein")) + 
    xlab("") +
    scale_fill_manual(values = c("#414066", "#69B4AB", "#FBBC4C")) +
    theme_ortimar()
}


# Obtener estadisticos descriptivos para todas las variables y graficar en bucle
n = 0
for (i in colnames(datos[7:16])) {
  print(i)
  n <-  n + 1
  tukey <- letras[[n]]
  l_data <- datos %>%
    group_by(playa, tejido) %>% 
    summarise(
      media = mean(get(i), na.rm = T),
      desvest = sd(get(i), na.rm = T),
      error = desvest/sqrt(sum(!is.na(get(i))))
    )
  
  print(grafica_barras(l_data))
  ggsave(paste0("graficas/", i, "_cultivo1.png"), width = 1000, height = 750, units = "px",
         scale = 2, dpi = "retina")
}


# Colores: #605E97 azul, #69B4AB verde, #FBBC4C amarillo
# Idea para no usar grafica de barras, supuestamente mejor un boxplot con los datos individuales como puntos para ver la dispersion. Problema: son solo 5 datos y queda un poco cutre. Usar las barras oculta el tamaño de muestra.
ggplot(datos) +
  geom_boxplot(aes(x = playa, y = CAT_t), fill = "white", color = "grey60", alpha = 0.2) +
  geom_jitter(aes(x = playa, y = CAT_t, color = playa), size = 4, 
              width = 0.2, alpha = 0.8) +
  scale_color_manual(values = c("#605E97", "#69B4AB", "#FBBC4C")) +
  theme_ortimar() +
  theme(panel.grid.major.x = element_blank()
    
  )
  
