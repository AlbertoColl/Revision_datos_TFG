### Analisis estadistico TFG 1                                            Efecto de la reproducción en cautividad

# Version II (optimizada)

# Alberto Coll Fernandez                                                   Comienzo: 07/12/2022                                                          Fin : 21/12/2022 (Pausa) Retomado el 13/03/2023                                 Fin: 29/03/23

# Problemas y aspectos a resolver:                                                  - Falta solo sacar diferencias significativas con Tuckey pero hay algun          problema. No se puede hacer con anova de 3 vias porque se enmascaran           algunas en los modelos mas simples. Probablemente habra que hacerlo como        lo hace Cristina, y meter las letras/simbolos manualmente en un conjunto        de datos nuevo.

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

source(file = "./scripts/1_funciones_graficas.R") # Para tener las funciones de las graficas

### Ajuste de modelo EXTENDIDO ----

# Creamos una lista de formulas con sapply(), y posteriormente aplicamos a dicha lista el lm()

formulas <- sapply(colnames(datos[c(6:25)]), function(x){as.formula(paste0(x, " ~ playa * corte * tiempo"))}) # En el analisis extendido vamos a ver la interaccion entre corte y tiempo, ademas de incluir el factor playa

# Creamos modelos con esas formulas
modelos <- sapply(formulas, function(x){aov(x, datos)})

# Ahora comprobamos los modelos a ver que tal y en cuales quitamos interaccion (o playa)
sapply(modelos, function(x){summary(x)})

# En el siguiente bloque voy a ajustar un modelo mas parsimonioso, sin la interaccion, para eliminar el termino en aquellos casos donde no haya diferencia significativa entre ambos modelos

formulas_sin <- sapply(colnames(datos[c(6:25)]), function(x){as.formula(paste0(x, " ~ playa * corte + tiempo"))})

n <- 0
for (variable in colnames(datos[c(6:25)])) {
  n <- n + 1
  modelo_sin <- aov(formulas_sin[[n]], datos)
  print("Se ha ajustado el modelo sin interaccion")
  likelihood <- anova(modelos[[n]], modelo_sin)
  print("Se ha hecho el ANOVA")
  if (n != 3) {
    if (likelihood$`Pr(>F)`[2] > 0.05) {
      print("No hay diferencias, sustituimos")
      modelos[[n]] <- modelo_sin
    }  else {print("Los modelos son diferentes, nos quedamos con el complejo")
    }
  }
}

# Hay que hacerlo ahora con playa, para cada uno utilizando playa+corte+tiempo o playa+corte*tiempo segun se haya eliminado la interaccion o no

formulas_sin <- sapply(colnames(datos[c(6:25)]), function(x){as.formula(paste0(x, " ~ playa + corte + tiempo"))})

n <- 0
for (variable in colnames(datos[c(6:25)])) {
  n <- n + 1
  modelo_sin <- aov(formulas_sin[[n]], datos)
  print("Se ha ajustado el modelo sin interaccion")
  likelihood <- anova(modelos[[n]], modelo_sin)
  print("Se ha hecho el ANOVA")
  if (n != 3) {
    if (likelihood$`Pr(>F)`[2] > 0.05) {
      print("No hay diferencias, sustituimos")
      modelos[[n]] <- modelo_sin
    }  else {print("Los modelos son diferentes, nos quedamos con el complejo")}
  }}

### Asunciones modelo EXTENDIDO ----

# Siguiente paso: necesitamos ver si los modelos cumnplen asunciones
# Normalidad de residuos: qqplot y test de shapiro wilk
# Homocedasticidad: test de levene y plot scale-location

sapply(modelos, function(x){
  shapiro.test(residuals(x)) 
}) # No hay problemas con normalidad de residuos en ningún caso, lo cual esta genial
# GR_t no sigue normalidad de residuos, con transformación log se resuelve
modelos[[4]] <- aov(log(GR_t) ~ playa + corte + tiempo, datos)
# La G6PDH_p, SOD_p y G6PDH_t son marginalmente significativas pero en general el histograma de residuos está bien. Hay muy poco n, hay que ser flexible

# En cuanto a homocedasticidad, examinando las graficas de Scale-Location
sapply(modelos, function(x){
  plot(x, which = 3)
  title(main = x$terms[[2]])
})

# Algunas un poco raras pero volvemos a lo mismo, en general bastante bien considerando el poco tamaño de muestra que tenemos

# Finalmente guardamos todos los modelos individualmente en ./resultados/modelos
# Posteriormente se pueden cargar con readRDS (y renombrarlos como objetos)

n <-  6
for (i in modelos) {
  saveRDS(i, file = paste0("./resultados/modelos/modelo", colnames(datos[n]), "_extendido.RDS"))
  n <- n + 1
}

### Tablas ANOVA y test de Tukey ----

# Extraer p_valores de la tabla ANOVA
p_valores <- sapply(modelos, function(x){
  unlist(anova(x)$`Pr(>F)`)[c(1:8)]
})
sign <- p_valores < 0.05 # Igual pero podemos quedarnos con los TRUE


# En los casos en los que haya *algun* p valor signf, correr test de tukey
# Almacenar letras y poner en su grafica de barras correspondiente
# Poner texto correspondiente y p valor en grafica de interaccion

# Ajustar modelos triple * para tukey
formulas_t <- sapply(colnames(datos[c(6:25)]), function(x){as.formula(paste0(x, " ~ playa * corte * tiempo"))})
modelos_t <- sapply(formulas, function(x){aov(x, datos)})

# Comprobar si en sign hay algun TRUE para ese modelo
# En caso de que haya, continuar, si no, omitir esa variable
# TRUE %in% sign[,n], donde n es el n de variable
# Sacar cld con TukeyHSD() y multcompLetters4(reversed = T) para el modelo con el mismo indice
# Crear funcion que convierta cld en cld simplificado con letras, + y *
#generar una nueva columna en datos (mutate()) de combinacion de playa:corte:tiempo en el orden de tuckey
# intentar juntar las cld en datos con esta columna como guia
# sacar grafica de barras y de interaccion con letras


# Este bloque es el encargado de sacar las cld y generar las graficas, falta guardarlas e incluir las graficas de interaccion
for (n in c(1:20)) {
  print(n)
  if (n != 13){
    i <- colnames(datos[6:25])[[n]]
    tabla_summ <- datos %>% group_by(playa, corte, tiempo) %>%
      summarise(media = mean(get(i), na.rm = T),
                desvest = sd(get(i), na.rm = T),
                error = desvest/sqrt(sum(!is.na(get(i))))) %>% 
      arrange(desc(media))
    tabla_summ$Letters <- rep("", 12)
    if (TRUE %in% sign[,n]) {
      cld <- multcompLetters4(modelos_t[[n]], TukeyHSD(modelos_t[[n]]), reversed = T)
      cld <- as.data.frame.list(cld$`playa:corte:tiempo`) %>% select(Letters)
      tabla_summ$Letters <- cld$Letters
      }
    (grafica <- barras())
    ggsave(paste0("./resultados/graficas/", i, "_barras1.png"), width = 1500, height = 1000, units = "px",
           scale = 2, dpi = "retina")
    saveRDS(grafica, file = paste0("./resultados/graficas/", i, "_barras1.RDS"))
    (grafica <- interact())
    ggsave(paste0("./resultados/graficas/", i, "_interaccion1.png"), width = 1500, height = 800, units = "px",
           scale = 2, dpi = "retina")
    saveRDS(grafica, file = paste0("./resultados/graficas/", i, "_interaccion1.RDS"))
    }}


#### A PARTIR DE ESTA PARTE ES ANTIGUO ###
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
  
