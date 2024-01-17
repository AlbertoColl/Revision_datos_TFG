### ORTIMAR
# Definicion de graficas
# Diciembre 2023


## SETUP ----
library(tidyverse)
library(ggthemr)
library(ggsignif)

# Directorio en laboratorio: C:/Users/Usuario/Documents/TFM-Ortiguilla
# Directorio en portatil: D:/collf/Documents/GitHub/TFM-Anemonia-sulcata

# setwd("D:/collf/Documents/GitHub/Revision_datos_TFG") # Casa
#source(file = "./analisis 12-2023/0_data_home.R")

setwd("C:/Users/Usuario/Documents/GitHub/Revision_datos_TFG") # Lab
source(file = "./analisis 12-2023/0_data_lab.R")

ggthemr("fresh")

## Definicion del tema y formato de las graficas ----
theme_tfm <- function(){
  theme(panel.background = element_rect(fill = "gray99"),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18),
        strip.text.x = element_text(size = 15, face = "bold", vjust = 0),
        axis.title = element_text(size = 15),
        legend.position = "none")
  #strip.background = element_rect(colour = "black")
}

## Definicion de grafica de barras ----


# Este es el corazon del codigo que genera las graficas. Itera los nombres de las variables, y para cada una te da su media, desviacion estandar y error para usar en las graficas. Una vez tengas las funciones de las graficas hechas, se añaden al bucle. Es necesario ponerlo en otro script, en el de descriptiva o de analisis.

barras_tfm <- function(){
  ggplot(tabla_summ) +
    geom_errorbar(aes(x = tratamiento, ymax = media + error, ymin = media- error), width = 0.7, color = "gray55") +
    geom_col(aes(x = tratamiento, y = media, fill = tratamiento)) +
    geom_text(aes(x = tratamiento, y = media + error, label = tukey),
              color = "grey5", vjust = -0.5, size = 4) +
    ylab(case_when(
      i == "proteina" ~ "protein mg / ml",
      i == "MDA" ~ "μM  MDA",
      i == "TEAC" ~ "Trolox equivalent μM",
      i == "GST" ~ "mU / protein mg",
      i == "DTD" ~ "mU / protein mg",
      TRUE ~ "U / protein mg")) +
    xlab("Treatment") +
    #title(main = i) +
    scale_fill_manual(values = c("#0c8890", "#3EB59B", "#FBBC4C")) + # Colores 1
    #scale_fill_manual(values = c("#414066", "#69B4AB", "#FBBC4C", "#EF476F")) + # Colores 2
    #scale_fill_manual(values = c("#1E5D56", "#0C8890", "#3EB59B", "#FBBC4C")) + # Colores 3
    #ylim(c(0, case_when(
     # i == "CAT.pie" ~  90,
      #i == "DTD.pie" ~  15,
      #i == "GST.pie" ~  450,
      #i == "MDA.pie.2"| i == "MDA.tent.2" ~  80,
      #i == "SOD.pie"| i == "SOD.tent" ~  150,
      #i == "TEAC.pie"| i == "TEAC.tent" ~  500,
      #i == "CAT.tent" | i == "DTD.tent" | i == "GST.tent" ~ max(tabla_summ$media) * 2,
      #TRUE ~ max(tabla_summ$media) * 1.2))) +
    theme_tfm()
  
}

barras_tfg <- function(){
  ggplot(tabla_summ) +
    geom_errorbar(aes(x = cultivo, ymax = media + error, ymin = media- error, group = playa), width = 0.7, color = "gray55", position = position_dodge(width = 0.9)) +
    geom_col(aes(x = cultivo, fill = playa, y = media), position = position_dodge(width = 0.9)) +
    geom_text(aes(x = cultivo, group = playa, y = media + error, label = tukey),
              color = "grey5", vjust = -0.5, size = 4, position = position_dodge(width = 0.9)) +
    ylab(case_when(
      i == "proteina" ~ "protein mg / ml",
      i == "MDA" ~ "μM  MDA",
      i == "TEAC" ~ "Trolox equivalent μM",
      i == "GST" ~ "mU / protein mg",
      i == "DTD" ~ "mU / protein mg",
      TRUE ~ "U / protein mg")) +
    xlab("Wild vs Cultured") +
    #title(main = i) +
    scale_fill_manual(values = c("#0c8890", "#3EB59B", "#F58F29")) + # Colores 1
    #scale_fill_manual(values = c("#414066", "#69B4AB", "#FBBC4C", "#EF476F")) + # Colores 2
    #scale_fill_manual(values = c("#1E5D56", "#0C8890", "#3EB59B", "#FBBC4C")) + # Colores 3
    #ylim(c(0, case_when(
    # i == "CAT.pie" ~  90,
    #i == "DTD.pie" ~  15,
    #i == "GST.pie" ~  450,
    #i == "MDA.pie.2"| i == "MDA.tent.2" ~  80,
    #i == "SOD.pie"| i == "SOD.tent" ~  150,
    #i == "TEAC.pie"| i == "TEAC.tent" ~  500,
    #i == "CAT.tent" | i == "DTD.tent" | i == "GST.tent" ~ max(tabla_summ$media) * 2,
    #TRUE ~ max(tabla_summ$media) * 1.2))) +
  theme_tfm()
  
}
