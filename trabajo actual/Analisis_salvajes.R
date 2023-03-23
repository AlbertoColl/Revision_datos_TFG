### Analisis estadistico ORTIMAR 1 - Comparacion entre individuos salvajes de tres entornos naturales diferentes

### Alberto Coll Fernandez
# Comienzo: 07/12/2022
# Fin : 


### SETUP ----
library(tidyverse)
library(ggthemr)
library(multcompView)

setwd("C:/Users/Usuario/Documents/ORTIMAR (Alberto Coll)")
datos <- read.csv2("~/ORTIMAR (Alberto Coll)/datos/TFG_datos.csv", numerals = "warn.loss", encoding = "latin1")%>% 
  mutate(playa = as.factor(playa), cultivo = as.factor(cultivo), 
         corte = as.factor(corte), madurez = as.factor(madurez),
         tejido = as.factor(tejido))

# Para ete objetivo me quedo solo con los datos del entorno natural
datos <- datos %>% 
  mutate(GPx = ifelse(GPx < 0, NA, GPx)) %>% # hay datos de gpx negativos
  filter(cultivo == "No")

# Aqui voy eliminando (y marcando) los posibles outliers
datos[13,9] <- NA
datos[12,9] <- NA
datos[6,9] <- NA



### Analisis exploratorio ----
datos_resumen <- datos %>% 
  group_by(playa, tejido) %>% 
  summarise(mSOD = mean(SOD, na.rm = T),
            sdSOD = sd(SOD, na.rm = T),
            seSOD = sdSOD/sqrt(n()),
            mCAT = mean(CAT, na.rm = T),
            sdCAT = sd(CAT, na.rm = T),
            seCAT = sdCAT/sqrt(n()),
            mGPx = mean(GPx, na.rm = T),
            sdGPx = sd(GPx, na.rm = T),
            seGPx = sdGPx/sqrt(sum(!is.na(GPx))),
            mGR = mean(GR, na.rm = T),
            sdGR = sd(GR, na.rm = T),
            seGR = sdGR/sqrt(n()),
            mGST = mean(GST, na.rm = T),
            sdGST = sd(GST, na.rm = T),
            seGST = sdGST/sqrt(sum(!is.na(GST))),
            mDTD = mean(DTD, na.rm = T),
            sdDTD = sd(DTD, na.rm = T),
            seDTD = sdDTD/sqrt(n()),
            mG6PDH = mean(G6PDH, na.rm = T),
            sdG6PDH = sd(G6PDH, na.rm = T),
            seG6PDH = sdG6PDH/sqrt(n()),
            mTEAC = mean(TEAC, na.rm = T),
            sdTEAC = sd(TEAC, na.rm = T),
            seTEAC = sdTEAC/sqrt(n()),
            mprot = mean(proteina, na.rm = T),
            sdprot = sd(proteina, na.rm = T),
            seprot = sdprot/sqrt(n()),
            mMDA = mean(MDA, na.rm = T),
            sdMDA = sd(MDA, na.rm = T),
            seMDA = sdMDA/sqrt(n()),
            )

# GRAFICAS DE MEDIA Y ERRORES PARA SOD, CAT, GPX, MDA Y TEAC
(pSOD <- ggplot(data = datos_resumen) +
  geom_col(aes(x = playa, y = mSOD, fill = playa)) +
  geom_errorbar(aes(x = playa, ymax = mSOD + seSOD, ymin = mSOD - seSOD), color = "grey40") +
  facet_wrap(~tejido))

(pCAT <- ggplot(data = datos_resumen) +
    geom_col(aes(x = playa, y = mCAT, fill = playa)) +
    geom_errorbar(aes(x = playa, ymax = mCAT + seCAT, ymin = mCAT - seCAT), color = "grey40") +
    facet_wrap(~tejido))

(pGPX <- ggplot(data = datos_resumen) +
    geom_col(aes(x = playa, y = mGPx, fill = playa)) +
    geom_errorbar(aes(x = playa, ymax = mGPx + seGPx, ymin = mGPx - seGPx), color = "grey40") +
    facet_wrap(~tejido))

(pMDA <- ggplot(data = datos_resumen) +
    geom_col(aes(x = playa, y = mMDA, fill = playa)) +
    geom_errorbar(aes(x = playa, ymax = mMDA + seMDA, ymin = mMDA - seMDA), color = "grey40") +
    facet_wrap(~tejido))

(pTEAC <- ggplot(data = datos_resumen) +
    geom_col(aes(x = playa, y = mTEAC, fill = playa)) +
    geom_errorbar(aes(x = playa, ymax = mTEAC + seTEAC, ymin = mTEAC - seTEAC), color = "grey40") +
    facet_wrap(~tejido))


# Plantilla de boxplot para ver dispersion
ggplot(data = datos) +
  geom_boxplot(aes(y = GPx, x = playa, fill = playa), color = "grey40") +
  facet_wrap(~tejido)



### Modelos ----
# SOD
m.sod_p <- aov(SOD ~ playa, data = filter(datos, tejido == "Pie"))
summary(m.sod_p)
m.sod_t <- aov(SOD ~ playa, data = filter(datos, tejido == "Tentaculo"))
summary(m.sod_t) # Efecto significativo

# CAT
m.cat_p <- aov(CAT ~ playa, data = filter(datos, tejido == "Pie"))
summary(m.cat_p)
m.cat_t <- aov(CAT ~ playa, data = filter(datos, tejido == "Tentaculo"))
summary(m.cat_t) # Efecto significativo

# GPx
m.gpx_p <- aov(GPx ~ playa, data = filter(datos, tejido == "Pie"))
summary(m.gpx_p) #Efecto significativo
m.gpx_t <- aov(GPx ~ playa, data = filter(datos, tejido == "Tentaculo"))
summary(m.gpx_t)

# MDA
m.mda_p <- aov(MDA ~ playa, data = filter(datos, tejido == "Pie"))
summary(m.mda_p) # Marginalmente significativo
m.mda_t <- aov(MDA ~ playa, data = filter(datos, tejido == "Tentaculo"))
summary(m.mda_t)

# TEAC
m.teac_p <- aov(TEAC ~ playa, data = filter(datos, tejido == "Pie"))
summary(m.teac_p)
m.teac_t <- aov(TEAC ~ playa, data = filter(datos, tejido == "Tentaculo"))
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
letras[[1]] <- c("", "a", "", "ab", "", "b")

tukey.cat <- TukeyHSD(m.cat_t)
(cld.tukey2 <- multcompLetters4(m.cat_t, tukey.cat)) # CAT tentaculo
#datos_resumen$letras_CAT <- c("", "b", "", "b", "", "a")
letras[[2]] <- c("", "b", "", "b", "", "a")


tukey.gpx <- TukeyHSD(m.gpx_p)
(cld.tukey3 <- multcompLetters4(m.gpx_p, tukey.gpx)) #GPx pie
#datos_resumen$letras_GPx <- c("b", "", "a", "", "b", "")
letras[[3]] <-c("b", "", "a", "", "b", "")


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


#Reordenar los niveles del factor playa
datos_resumen$playa <- factor(datos_resumen$playa, levels = c("Calahonda", "Almuñecar", "Salobreña"))


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
    labs(title = paste("Actividad", i,"en pie y tentaculo\n\n"), #Cambia a la tilde para guardar la grafica, pero para guardar el script vuelve a ponerla bien
         fill = "Punto de\nmuestreo") +
    ylab("U / mg proteina") + #igual con la tilde
    xlab("") +
    scale_fill_manual(values = c("#414066", "#69B4AB", "#FBBC4C")) +
    theme_ortimar()
}


# Obtener estadisticos descriptivos para todas las variables y graficar en bucle
n = 0
for (i in colnames(datos[7:13])) {
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
  ggsave(paste0("~/ORTIMAR (Alberto Coll)/resultados/graficas/", i, "_salvajes.png"), width = 1000, height = 750, units = "px",
         scale = 2, dpi = "print")
}



### MODELO BASICO DE LA GRAFICA (ESTA HECHA EN EL BUCLE ARRIBA)
# SOD (ponemos tambien pie o solo tentaculo?)
#(pSOD <- ggplot(data = datos_resumen) +
#    geom_errorbar(aes(x = playa, ymax = mSOD + seSOD, ymin = mSOD - seSOD), color = "grey40", width =  0.7) +
#   geom_col(aes(x = playa, y = mSOD, fill = playa)) +
   geom_text(aes(x = playa, y = mSOD + seSOD, label = letras_SOD), color = "grey5",
             vjust = -1, size = 5) +
#   facet_wrap(~tejido) +
#    ylim(NA,110) +
#    labs(title = "Actividad SOD en pie y tentaculo\n\n", #Cambia a la tilde para guardar la grafica, pero para guardar el script vuelve a ponerla bien
#         fill = "Punto de\nmuestreo",
#         tag = "A") +
#    ylab("U / mg proteina") + #igual con la tilde
#    xlab("") + 
#    scale_fill_manual(values = c("#414066", "#69B4AB", "#FBBC4C")) +
#    theme_ortimar()
# )

#ggsave("~/ORTIMAR (Alberto Coll)/resultados/graficas/SOD_salvajes.png", width = 1000, height = 750, units = "px",
#       scale = 2, dpi = "print")





