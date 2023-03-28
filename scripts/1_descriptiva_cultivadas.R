### Analisis estadistico TFG 1                                        Comparacion entre individuos salvajes de tres entornos naturales diferentes

# Alberto Coll Fernandez                                                   Comienzo: 23/03/2022                                                           Fin:

# Problemas y aspectos a resolver:                                                   - Podria hacer otra version de la grafica de interaccion y barras para           los casos en los que no incluya la playa en el modelo, sobre todo en el         caso de interaccion que creo que ayudaria mucho a hacerla mas visual         - Deberia hacer version de las graficas para el modelo simplificado de           corte * playa

### SETUP ----
library(tidyverse)
library(ggthemr)

# Directorio en laboratorio: C:/Users/Usuario/Documents/TFG Alberto Coll

# Directorio en portatil: D:/collf/Documents/GitHub/TFG-Alberto-Coll

setwd("D:/collf/Documents/GitHub/TFG-Alberto-Coll")

# Llamamos el script de lectura
#source(file = "./scripts/0_data_lab.R") # Laboratorio
source(file = "./scripts/0_data_home.R") # En casa
datos <- datos %>% filter(cultivo == "Si")

ggthemr("fresh")
# Definimos el tema de las graficas
theme_ortimar <- function(){
  theme(panel.background = element_rect(fill = "gray99"),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 18),
        strip.text.x = element_text(size = 15, face = "bold", vjust = 0),
        axis.title = element_text(size = 14),
        #strip.background = element_rect(colour = "black")
        
  )
}

### Gráfica de barras ----

# Esta es la definitiva, con manipulacion de strings para poner los titulos bien, usnado get(), str_split(), str_detect()
barras <- function(data){
  ggplot(data = datos, aes(x = tiempo, y = get(i), fill = playa)) +
    stat_summary(aes(group = playa), fun.data = mean_se, geom = "errorbar", width = 0.7, position = position_dodge(width = 0.9), color = "gray25") +
    stat_summary(fun = mean, geom = "col", position = position_dodge(width = 0.9)) +
    facet_wrap(~corte) +
    labs(title = paste(case_when(
      i == "proteina_t" | i == "proteina_p"  ~ "Protein content",
      i == "MDA_t" | i == "MDA_p" ~ "MDA concentration",
      i == "TEAC_t" | i == "TEAC_p"~ "Trolox equivalent antioxidant capacity (TEAC)",
      TRUE ~ paste(str_split(i, "_")[[1]][1], "activity")), case_when(
        str_detect(i, "_p") == TRUE ~ "on pedal disk",
        TRUE ~ "on tentacle")),
      fill = "Sampling\npoint") +
    ylab(case_when(
      i == "proteina_t" | i == "proteina_p"  ~ "protein mg / ml",
      i == "MDA_t" | i == "MDA_p" ~ "MDA μM ",
      i == "TEAC_t" | i == "TEAC_p"~ "Trolox equivalent μM",
      i == "GR_t" | i == "GR_p" ~ "mU / mg  of protein",
      i == "GST_t" | i == "GST_p" ~ "mU / mg  of protein",
      i == "DTD_t" | i == "DTD_p" ~ "mU / mg  of protein",
      TRUE ~ "U / mg  of protein")) +
    xlab("Sampling") +
    scale_fill_manual(values = c("#414066", "#69B4AB", "#FBBC4C")) +
    scale_x_discrete(labels = c("First", "Second")) +
    theme_ortimar()
}


### Gráfica de interacción ----

# Version actualizada, la damos por mas o menos finalizada. Realmente para ver resultados de interaccion es la mejor grafica pero puede ser algo liosa con tanta playa
interact <- function(data){
  ggplot(data = datos, aes(x = tiempo, y = get(i), color = corte:playa, shape = corte:playa)) +
    stat_summary(aes(group = corte), fun.data = mean_se, geom = "errorbar", width = 0.1) +
    stat_summary(fun = mean, geom = "point", size = 4.5) +
    stat_summary(aes(group = corte, linetype = corte),fun = mean, geom = "line", linewidth = 1.5) +
    facet_wrap(~playa) +
    labs(title = paste(case_when(
      i == "proteina_t" | i == "proteina_p"  ~ "Protein content",
      i == "MDA_t" | i == "MDA_p" ~ "MDA concentration",
      i == "TEAC_t" | i == "TEAC_p"~ "Trolox equivalent antioxidant capacity (TEAC)",
      TRUE ~ paste(str_split(i, "_")[[1]][1], "activity")), case_when(
        str_detect(i, "_p") == TRUE ~ "on pedal disk\n",
        TRUE ~ "on tentacle\n"))) +
    ylab(case_when(
      i == "proteina_t" | i == "proteina_p"  ~ "protein mg / ml",
      i == "MDA_t" | i == "MDA_p" ~ "MDA μM ",
      i == "TEAC_t" | i == "TEAC_p"~ "Trolox equivalent μM",
      i == "GR_t" | i == "GR_p" ~ "mU / mg  of protein",
      i == "GST_t" | i == "GST_p" ~ "mU / mg  of protein",
      i == "DTD_t" | i == "DTD_p" ~ "mU / mg  of protein",
      TRUE ~ "U / mg  of protein")) +
    xlab("Sampling") +
    scale_color_manual(name = "Sampling point\n and Treatment",
                       values = c("#414066", "#69B4AB", "#FBBC4C", "#414066", "#69B4AB", "#FBBC4C"),
                       labels = c("Calahonda control", "Almuñécar control", "Salobreña control", "Calahonda dissected", "Almuñécar dissected", "Salobreña dissected")) +
    scale_shape_manual(name = "Sampling point\n and Treatment",
                       values = c(16, 16, 16, 17, 17, 17),
                       labels = c("Calahonda control", "Almuñécar control", "Salobreña control", "Calahonda dissected", "Almuñécar dissected", "Salobreña dissected")) +
    guides(linetype = "none") +
    scale_x_discrete(labels = c("First", "Second")) +
    theme_ortimar()
}

### Boxplot ---- 

# La grafica de cajas con tanta agrupacion es una miseria porque se queda en n = 3 y da verguenza verla. Solo usarla si se tienen dos variables
cajas <- function(data){
  ggplot(data = datos) +
    geom_boxplot(aes(x = tiempo, y = SOD_p, color = playa), fill = "white", alpha = 0.65, position = position_dodge2(width = 0.7), linewidth = 1) +
    geom_jitter(aes(x = tiempo, y = SOD_p, color = playa, group = tiempo), position = position_dodge2(width = 0.7), size = 4) +
    labs(title = case_when(
      i == "proteina_t" | i == "proteina_p"  ~ "Protein content",
      i == "MDA_t" | i == "MDA_p" ~ "MDA concentration",
      i == "TEAC_t" | i == "TEAC_p"~ "Trolox equivalent antioxidant capacity (TEAC)",
      TRUE ~ paste( i,"activity\n\n")),
      fill = "Sampling\npoint") +
    ylab(case_when(
      i == "proteina_t" | i == "proteina_p"  ~ "protein mg / ml",
      i == "MDA_t" | i == "MDA_p" ~ "MDA μM ",
      i == "TEAC_t" | i == "TEAC_p"~ "Trolox equivalent μM",
      TRUE ~ "U / mg  of protein")) + # Corregir unidades segun enzima
    xlab("") +
    scale_color_manual(values = c("#414066", "#69B4AB", "#FBBC4C")) +
    scale_fill_manual(values = c("#414066", "#69B4AB", "#FBBC4C")) +
    facet_wrap(~corte) +
  theme_ortimar()
}


### Aqui debajo delo una version simple de la grafica, la primera que hice
# Colores: #605E97 azul, #69B4AB verde, #FBBC4C amarillo
# Idea para no usar grafica de barras, supuestamente mejor un boxplot con los datos individuales como puntos para ver la dispersion. Problema: son solo 5 datos y queda un poco cutre. Usar las barras oculta el tamaño de muestra.
ggplot(datos) +
  geom_boxplot(aes(x = playa, y = CAT_t), fill = "white", color = "grey60", alpha = 0.2) +
  geom_jitter(aes(x = playa, y = CAT_t, color = playa), size = 4, 
              width = 0.2, alpha = 0.8) +
  scale_color_manual(values = c("#605E97", "#69B4AB", "#FBBC4C")) +
  theme_ortimar() +
  facet_wrap(~corte) +
  theme(panel.grid.major.x = element_blank()
        
  )

### Ejecucion ----

# Este bloque itera cada variable del estudio y genera su grafica de barras y de interaccion con el modelo extendido.
for (i in colnames(datos[6:25])) {
  #print(i)
  print(barras(datos))
  ggsave(paste0("./resultados/graficas/", i, "_barras1.png"), width = 1000, height = 750, units = "px",
         scale = 2, dpi = "retina")
  print(interact(datos))
  ggsave(paste0("./resultados/graficas/", i, "_interaccion1.png"), width = 1500, height = 750, units = "px",
         scale = 2, dpi = "retina")
}
