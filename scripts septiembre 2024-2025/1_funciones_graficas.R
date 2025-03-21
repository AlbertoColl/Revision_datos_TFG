### Revision Analisis TFM - Alberto Coll Fernandez
# Definicion de graficas
# Comenzado: 24/01/2023
# Terminado 24/01/2023


## SETUP ----
library(tidyverse)
library(ggthemr)

#setwd("C:/Users/Usuario/Documents/GitHub/Revision_datos_TFG")
setwd("D:/collf/Documents/GitHub/Revision_datos_TFG")

#source(file = "./scripts septiembre 2024-2025/0_data_lab.R")
source(file = "./scripts septiembre 2024-2025/0_data_laptop.R")

## Definicion del tema y formato de las graficas ----
theme_tfm <- function(){
  theme(panel.background = element_rect(fill = "gray99"),
        axis.text = element_text(size = 9),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, vjust = -.5),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12, face = "bold", vjust = 0),
        axis.title = element_text(size = 12),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.line = element_line(colour = "gray10"),
        axis.ticks = element_line(color = "gray10"))
        #strip.background = element_rect(colour = "black")
}

## Definicion de grafica de barras ----

if(FALSE){ # Aqui estan la funcion que utilicé para mi TFM y para su publicacion asociada, pero se pueden extrapolar a cualquier diseño de tipo ANOVA de una via
barras_tfm <- function(){
  ggplot() +
    geom_errorbar(data = tabla_summ, aes(x = tratamiento, ymax = media + error, ymin = media- error), width = 0.7, color = "gray55") +
    geom_col(data = tabla_summ, aes(x = tratamiento, y = media, fill = tratamiento, color = tratamiento),  alpha = 0.1, linewidth = 1) +
    #geom_point(data = datos, aes(x = tratamiento, y = get(i), color = tratamiento), alpha = 0.7, size = 2) +
    geom_text(data = tabla_summ, aes(x = tratamiento, y = media + error, label = tukey), color = "grey5", vjust = -0.8, size = 3.5, fontface = "bold") +
    ylab(case_when(
      i == "clorofila.total" ~ "chlorophyll μg/tissue g",
      i == "proteina.tent" | i == "proteina.pie"  ~ " protein mg / ml",
      i == "MDA.pie.2" | i == "MDA.tent.2" ~ "μM  MDA",
      i == "TEAC.pie" | i == "TEAC.tent"~ "Trolox equivalent μM",
      i == "GST.pie" | i == "GST.tent" ~ "mU / mg  of protein",
      i == "DTD.pie" | i == "DTD.tent" ~ "mU / mg  of protein",
      i == "GR.pie" | i == "GR.tent" ~ "mU / mg  of protein",
      i == "GPx.pie" | i == "GPx.tent" ~ "mU / mg  of protein",
      TRUE ~ "U / mg  of protein")) +
    xlab("Condition") + # Omitir?
    scale_color_manual(values = c("#0c8890", "#54B65D","#E56A1C", "#FBBC4C")) +
    scale_x_discrete(labels = c('C','LS','BW', "IMTA")) +
    ylim(0,110) +
    #ylim(c(0, 1.4*(max(tabla_summ$media) + max(tabla_summ$error)))) +
    theme_tfm()
  
}

barras_articulo <- function(){
  ggplot() +
    geom_errorbar(data = tabla_summ, aes(x = tratamiento, ymax = media + error, ymin = media- error), width = 0.7, color = "gray55") +
    geom_col(data = tabla_summ, aes(x = tratamiento, y = media, fill = tratamiento, color = tratamiento),  alpha = 0.1, linewidth = 1) +
    #geom_point(data = datos, aes(x = tratamiento, y = get(i), color = tratamiento), alpha = 0.7, size = 2) +
    geom_text(data = tabla_summ, aes(x = tratamiento, y = media + error, label = tukey), color = "grey5", vjust = -0.8, size = 3.5, fontface = "bold") +
    facet_wrap(~tejido) +
    ylab(case_when(
      i == "proteina" ~ " protein mg / ml",
      i == "MDA" ~ "μM  MDA",
      i == "TEAC"~ "Trolox equivalent μM",
      i == "GST" ~ "mU / mg  of protein",
      i == "DTD" ~ "mU / mg  of protein",
      i == "GR" ~ "mU / mg  of protein",
      i == "GPx" ~ "mU / mg  of protein",
      TRUE ~ "U / mg  of protein")) +
    xlab("Condition") + # Omitir?
    scale_color_manual(values = c("#0c8890", "#54B65D","#E56A1C", "#FBBC4C")) +
    scale_x_discrete(labels = c('C','LS','BW', "IMTA")) +
    ylim(c(0, 1.4*(max(tabla_summ$media) + max(tabla_summ$error)))) +
    theme_tfm()
  
}
}

# Esta es la que actualmente estoy usando
ggthemr("light")
barras_tfg <- function(){
  return(ggplot(tabla_summ) +
    geom_errorbar(aes(x = time, ymax = mean + se, ymin = mean- se, color = tratamiento), width = 0.3, position = position_dodge(width = 0.9), linewidth = 1, show.legend = F) +
    geom_col(aes(x = time, y = mean, color = tratamiento, fill = tratamiento), alpha = 0.4, linewidth = 1, position = "dodge2") +
    geom_text(aes(x = time, y = mean + se, label = letras, group = tratamiento), color = "grey5", vjust=-0.8, size = ifelse("*" %in% tabla_summ$letras, 5.5, 3.5), fontface = "bold", position = position_dodge(width = 0.9)) +
    ylab(case_when(
      
      str_detect(i, "proteina") == TRUE  ~ " protein mg / ml",
      str_detect(i, "MDA") == TRUE ~ "μM  MDA",
      str_detect(i, "TEAC") == TRUE ~ "Trolox equivalent μM",
      str_detect(i, "Lisozima") == TRUE ~ "ng/mg of protein Eq. HEWL",
      str_detect(i, "Mielo") == TRUE ~ "mU * 10^2 / mg  of protein",
      any(str_detect(i, c("GST", "DTD", "GR", "GPx", "Facida", "Fbasica"))) == T ~ "mU / mg  of protein",
      TRUE ~ "U / mg  of protein")) +
    xlab(NULL) + # Omitir?
    scale_color_manual(values = c("#49AB93","#F59300" ), labels = c("Control", "Sectioned") ) +
    scale_fill_manual(values = c("#49AB93","#F59300" ), labels = c("Control", "Sectioned") ) +
    scale_x_discrete(labels = c("T0", "T1")) + #cambiar al apropiado
    theme(legend.position = "bottom",
    legend.title = element_blank()))
    }
