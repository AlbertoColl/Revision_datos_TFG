### Analisis estadistico ORTIMAR 1                                      Comparacion entre individuos salvajes de tres entornos naturales diferentes

# Version II (optimizada)

# Alberto Coll Fernandez                                                   Comienzo: 07/12/2022                                                       Fin : 21/12/2022 (Pausa)

# Problemas y aspectos a resolver:                                                  - Revisar asunciones de los modelos                                         - Implementar test de Tukey en bucle, con ifelse para que solo lo             haga a los que resulten significativos, y al resto les rellene las           letras con ""


### SETUP ----
library(dplyr)
library(ggplot2)
library(ggthemr)
library(multcompView)

# Directorio en laboratorio: C:/Users/Usuario/Documents/ORTIMAR (Alberto Coll)

# Directorio en portatil: D:/collf/Documents/Apuntes/TFM/TFG Ortiguilla

setwd("C:/Users/Usuario/Documents/ORTIMAR (Alberto Coll)")
datos <- read.csv2("./datos/TFG_datos.csv", numerals = "warn.loss", encoding = "latin1")%>% 
  mutate(playa = as.factor(playa), cultivo = as.factor(cultivo), 
         corte = as.factor(corte), madurez = as.factor(madurez),
         tejido = as.factor(tejido))

datos$playa <- factor(datos$playa, levels = c("Calahonda", "Almuñecar", "Salobreña")) # Reordenar niveles

# Poner in english los nombres de tejidos
datos$tejido <- factor(datos$tejido, levels = c("Pie", "Tentaculo"), labels = c("pedalDisk", "tentacle"))

# Para ete objetivo me quedo solo con los datos del entorno natural
datos <- datos %>% 
  mutate(GPx = ifelse(GPx < 0, NA, GPx)) %>% # hay datos de gpx negativos
  filter(cultivo == "No")

# Aqui voy eliminando (y marcando) los posibles outliers
datos[13,9] <- NA
datos[12,9] <- NA
datos[6,9] <- NA
datos[16,7] <- NA #posible outlier, en SOD pie. Al mirar en los datos, las dos replicas de abs tenian un error bastante alto ¿Quitamos valor?



### Modelos ----

# Creamos una lista de formulas con sapply(), y posteriormente aplicamos a dicha lista los anovas, filtrando para pie y para tentaculo

formulas <- sapply(colnames(datos[c(7:16)]), function(x){as.formula(paste0(x, " ~ playa"))})
modelos.pie <- sapply(formulas, function(x){
  aov(x, data = filter(datos, tejido == "pedalDisk"))})
modelos.tent <- sapply(formulas, function(x){
  aov(x, data = filter(datos, tejido == "tentacle"))})


# Guardar todos los modelos individualmente en ./resultados/modelos
# Posteriormente se pueden cargar con readRDS( y renombrarlos)
modelos <- list(modelos.pie, modelos.tent)
iteration <- 0
for (j in modelos) {
  n <-  7
  for (i in j) {
    saveRDS(i, file = ifelse(iteration == 0, paste0("./resultados/modelos/aov.",colnames(datos[n]) , "_salvajes_pie.RDS"), paste0("./resultados/modelos/aov.", colnames(datos[n]) , "_salvajes_tent.RDS")))
    n <- n + 1
  }
  iteration <- iteration + 1
}


# Siguiente paso: necesitamos ver si los modelos se ajustan bien


# SOD
m.sod_p <- aov(SOD ~ playa, data = filter(datos, tejido == "pedalDisk"))
summary(m.sod_p)
m.sod_t <- aov(SOD ~ playa, data = filter(datos, tejido == "tentacle"))
summary(m.sod_t) # Efecto significativo

# CAT
m.cat_p <- aov(CAT ~ playa, data = filter(datos, tejido == "pedalDisk"))
summary(m.cat_p)
m.cat_t <- aov(CAT ~ playa, data = filter(datos, tejido == "tentacle"))
summary(m.cat_t) # Efecto significativo

# GPx
m.gpx_p <- aov(GPx ~ playa, data = filter(datos, tejido == "pedalDisk"))
summary(m.gpx_p) #Efecto significativo
m.gpx_t <- aov(GPx ~ playa, data = filter(datos, tejido == "tentacle"))
summary(m.gpx_t)

# MDA
m.mda_p <- aov(MDA ~ playa, data = filter(datos, tejido == "pedalDisk"))
summary(m.mda_p) # Marginalmente significativo
m.mda_t <- aov(MDA ~ playa, data = filter(datos, tejido == "tentacle"))
summary(m.mda_t)

# TEAC
m.teac_p <- aov(TEAC ~ playa, data = filter(datos, tejido == "pedalDisk"))
summary(m.teac_p)
m.teac_t <- aov(TEAC ~ playa, data = filter(datos, tejido == "tentacle"))
summary(m.teac_t)

### Test de Tukey ----

# Esto es para crear la lista de letras y usarla luego en las graficas
letras <- list()
for (j in c(1:10)) {
  letras[[j]] <- c("","","","","","")
}

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

grafica_ortimar <- function(data){
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
  
  print(grafica_ortimar(l_data))
  ggsave(paste0("graficas/", i, "_salvajes.png"), width = 1000, height = 750, units = "px",
         scale = 2, dpi = "retina")
}


# Colores: #605E97 azul, #69B4AB verde, #FBBC4C amarillo
# Idea para no usar grafica de barras, supuestamente mejor un boxplot con los datos individuales como puntos para ver la dispersion. Problema: son solo 5 datos y queda un poco cutre. Usar las barras oculta el tamaño de muestra.
ggplot(datos) +
  geom_boxplot(aes(x = playa, y = CAT), fill = "white", color = "grey60", alpha = 0.2) +
  geom_jitter(aes(x = playa, y = CAT, color = playa), size = 4, 
              width = 0.2, alpha = 0.8) +
  scale_color_manual(values = c("#605E97", "#69B4AB", "#FBBC4C")) +
  facet_wrap(~tejido) +
  theme_ortimar() +
  theme(panel.grid.major.x = element_blank()
    
  )
  
