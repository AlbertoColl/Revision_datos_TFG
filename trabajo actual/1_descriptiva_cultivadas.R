### Analisis estadistico TFG 1                                        Comparacion entre individuos salvajes de tres entornos naturales diferentes

# Alberto Coll Fernandez                                                   Comienzo: 23/03/2022                                                           Fin:

# Problemas y aspectos a resolver:                                                - Hacer que segun el nombre de la variable, la grafica corte antes de la        _ y luego segun sea p o t ponga al final "on pedal disk" o "on               tentacle"

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

# Revisar el codigo porque estaba hecho con tejido en variable
barras <- function(data){
  ggplot(data = l_data) +
    geom_errorbar(aes(x = corte, ymax = media + error,
                      ymin = media - error, group = playa),
                  color = "grey40", width =  0.7, position = "dodge2" ) +
    geom_col(aes(x = corte, y = media, fill = playa), position = "dodge2") +
    # geom_text(aes(x = playa, y = media + error, label = tukey),
              # color = "grey5", vjust = -0.5, size = 4) + # Solo si quieres letras
    labs(title = case_when(
      i == "proteina_p" ~ "Protein concentration",
      i == "proteina_t" ~ "Protein concentration",
      i == "MDA_t" ~ "MDA concentration",
      i == "MDA_p" ~ "MDA concentration",
      i == "TEAC_p" ~ "Trolox equivalent antioxidant capacity (TEAC)",
      i == "TEAC_t" ~ "Trolox equivalent antioxidant capacity (TEAC)",
      TRUE ~ paste( i,"activity\n\n")),
      fill = "Sampling\npoint") +
    ylab(case_when(
      i == "proteina_p" ~ "mg/ml",
      i == "proteina_t" ~ "mg/ml",
      i == "MDA_t" ~ "μM",
      i == "MDA_p" ~ "μM",
      i == "TEAC_t" ~ "μM Trolox equivalent",
      i == "TEAC_p" ~ "μM Trolox equivalent",
      TRUE ~ "U / mg  of protein")) + 
    xlab("") +
    scale_fill_manual(values = c("#414066", "#69B4AB", "#FBBC4C")) +
    theme_ortimar()
}

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
