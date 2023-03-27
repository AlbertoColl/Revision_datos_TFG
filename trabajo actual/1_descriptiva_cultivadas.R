### Analisis estadistico TFG 1                                        Comparacion entre individuos salvajes de tres entornos naturales diferentes

# Alberto Coll Fernandez                                                   Comienzo: 23/03/2022                                                           Fin:

# Problemas y aspectos a resolver:                                                - Hacer que segun el nombre de la variable, la grafica corte antes de la        _ y luego segun sea p o t ponga al final "on pedal disk" o "on               tentacle"                                                                  - Arreglar problema de unidades, tienes que ver cuales estaban en mili         unidades de actividad                                                      - Arreglar el tema de las graficas para incluir la configuracion de            paneles en caso de facet_wrap()                                            - Creo que stat.summary() se puede utilzar para computar medias y error        estandar directamente en la grafica, sin necesidad de hacer el bucle        y sacar un nuevo dataset descriptivo

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

# Definimos el tema de las graficas
theme_ortimar <- function(){
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 11, face = "bold", vjust = 4),
        axis.title.y = element_text(size = 14)
  )
}

### Gráfica de barras ----

# Faltan implementar los titulos y algunos detalles, pero se vera bien ya con el modelo complejo
barras <- function(data){
  ggplot(data = l_data) +
    geom_errorbar(aes(x = corte, ymax = media + error,
                      ymin = media - error, group = playa),
                  color = "grey40", position = position_dodge(width = 0.9), width = 0.75) +
    geom_col(aes(x = corte, y = media, fill = playa), position = position_dodge(width = 0.9)) +
    # geom_text(aes(x = playa, y = media + error, label = tukey),
              # color = "grey5", vjust = -0.5, size = 4) + # Solo si quieres letras
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
      i == "GR_t" | i == "GR_p" ~ "mU / mg  of protein",
      i == "GST_t" | i == "GST_p" ~ "mU / mg  of protein",
      i == "DTD_t" | i == "DTD_p" ~ "mU / mg  of protein",
      TRUE ~ "U / mg  of protein")) +
    xlab("") +
    scale_fill_manual(values = c("#414066", "#69B4AB", "#FBBC4C")) +
    theme_ortimar()
}

### Gráfica de interacción ----
interact <- function(data){
  ggplot(data = l_data) +
    geom_errorbar(aes(x = corte, ymax = media + error,
                      ymin = media - error, group = playa),
                  color = "grey70", width = 0.3) +
    geom_point(aes(x = corte, y = media, color = playa), size = 5) +
    geom_line(aes(x = corte, y = media, color = playa, group = playa)) +
    # geom_text(aes(x = playa, y = media + error, label = tukey),
    # color = "grey5", vjust = -0.5, size = 4) + # Solo si quieres letras
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
    facet_wrap(~playa)
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

# Este bloque saca las medidas descriptivas para la grafica de barras e interaccion
for (i in colnames(datos[6:25])) {
  print(i)
  n <-  n + 1
  #tukey <- letras[[n]] # Solo para incorporar test de Tukey
  l_data <- datos %>%
    group_by(playa, corte) %>% 
    summarise(
      media = mean(get(i), na.rm = T),
      desvest = sd(get(i), na.rm = T),
      error = desvest/sqrt(sum(!is.na(get(i))))
    )
  
  print(barras(l_data))
  ggsave(paste0("./resultados/graficas/", i, "_c1.png"), width = 1000, height = 750, units = "px",
         scale = 2, dpi = "retina")
}