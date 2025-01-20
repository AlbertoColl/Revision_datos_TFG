### Analisis estadistico TFG 1                                          Funciones de generacion de graficas

# Alberto Coll Fernandez                                                   Comienzo: 23/03/2022                                                       Fin: 28/03/23

# Problemas y aspectos a resolver:                                                  - Podria hacer otra version de la grafica de interaccion y barras             para los casos en los que no incluya la playa en el modelo, sobre           todo en el caso de interaccion que creo que ayudaria mucho a                hacerla mas visual                                                        - Deberia hacer version de las graficas para el modelo simplificado           de corte * playa

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
barras <- function(){
  ggplot(data = tabla_summ, aes(x = corte, fill = playa, y = media)) +
    geom_errorbar(aes(ymax = media + error, ymin = media - error,
                      group = playa), width = 0.7, color = "gray25",
                  position = position_dodge(width = 0.9)) +
    geom_col(position = position_dodge(width = 0.9)) +
    #geom_text(aes(label = Letters, y = media + error + (media * .1), group = playa),
     #         position = position_dodge(width = 0.9), vjust = 0.1) +
    facet_wrap(~tiempo, labeller = as_labeller(c("0" = "First sampling", "1" = "Second sampling"))) +
    labs(fill = "Sampling point:") +
      #caption = "\na,b,c: differences across sampling points    *: differences between treatments    +: differences between samplings") +
    ylab(case_when(
      i == "proteina_t" | i == "proteina_p"  ~ "protein mg / ml",
      i == "MDA_t" | i == "MDA_p" ~ "MDA μM ",
      i == "TEAC_t" | i == "TEAC_p"~ "Trolox equivalent μM",
      i == "GR_t" | i == "GR_p" ~ "mU / mg  of protein",
      i == "GST_t" | i == "GST_p" ~ "mU / mg  of protein",
      i == "DTD_t" | i == "DTD_p" ~ "mU / mg  of protein",
      TRUE ~ "U / mg  of protein")) +
    xlab("Treatment") +
    scale_fill_manual(values = c("#414066", "#69B4AB", "#FBBC4C")) +
    scale_x_discrete(labels = c("Control", "Dissected")) +
    theme_ortimar()
}


barras2 <- function(){
  ggplot(data = tabla_summ, aes(x = tiempo, fill = playa, y = media, group = corte)) +
    geom_errorbar(aes(ymax = media + error, ymin = media*0.99,
                      group = corte), width = 0.7, color = "gray35",
                  position = position_dodge(width = 0.9)) +
    geom_col(aes(alpha = corte), position = position_dodge(width = 0.9)) +
    geom_text(aes(label = Letters, y = media + error + (media * .1), group = corte), position = position_dodge(width = 0.9), vjust = 0.1) +
    facet_wrap(~playa)+
    labs(title = paste(case_when(
      i == "proteina_t" | i == "proteina_p"  ~ "Protein content",
      i == "MDA_t" | i == "MDA_p" ~ "MDA concentration",
      i == "TEAC_t" | i == "TEAC_p"~ "Trolox equivalent antioxidant capacity (TEAC)",
      TRUE ~ paste(str_split(i, "_")[[1]][1], "activity")), case_when(
        str_detect(i, "_p") == TRUE ~ "on pedal disk\n",
        TRUE ~ "on tentacle\n")),
      fill = "Sampling point:",
      caption = "\na,b,c: differences across sampling points    *: differences between treatments    +: differences between samplings") +
    ylab(case_when(
      i == "proteina_t" | i == "proteina_p"  ~ "protein mg / ml",
      i == "MDA_t" | i == "MDA_p" ~ "MDA μM ",
      i == "TEAC_t" | i == "TEAC_p"~ "Trolox equivalent μM",
      i == "GR_t" | i == "GR_p" ~ "mU / mg  of protein",
      i == "GST_t" | i == "GST_p" ~ "mU / mg  of protein",
      i == "DTD_t" | i == "DTD_p" ~ "mU / mg  of protein",
      TRUE ~ "U / mg  of protein")) +
    xlab("Treatment") +
    scale_fill_manual(values = c("#414066", "#69B4AB", "#FBBC4C")) +
    scale_x_discrete(labels = c("First", "Second")) +
    scale_alpha_manual(values = c(1, .95)) +
    #guides(alpha = "none") +
    theme_ortimar() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal")
}
### Gráfica de interacción ----

# Version actualizada, la damos por mas o menos finalizada. Realmente para ver resultados de interaccion es la mejor grafica pero puede ser algo liosa con tanta playa

interact <- function(){
  ggplot(data = tabla_summ, aes(x = tiempo, y = media, color = playa:corte, shape = playa:corte)) +
    geom_errorbar(aes(ymax = media + error, ymin = media - error, group = corte), width = .15, position = position_dodge(width = .25)) +
    geom_point(size = 4.5, alpha = .8, position = position_dodge(width = .25)) +
    geom_line(aes(group = corte, linetype = corte), linewidth = 1.5, position = position_dodge(width = .25)) +
    # geom_text(aes(label = Letters, y = media, group = playa),              position = position_jitter(width = 0.05), color = "gray5",              check_overlap = TRUE, hjust = -0.5, vjust = -0.5) +
    facet_wrap(~playa) +
    #labs(title = paste(case_when(
#      i == "proteina_t" | i == "proteina_p"  ~ "Protein content",
#      i == "MDA_t" | i == "MDA_p" ~ "MDA concentration",
#      i == "TEAC_t" | i == "TEAC_p"~ "Trolox equivalent antioxidant capacity (TEAC)",
#      TRUE ~ paste(str_split(i, "_")[[1]][1], "activity")), case_when(
#        str_detect(i, "_p") == TRUE ~ "on pedal disk\n",
#        TRUE ~ "on tentacle\n"))) +
      #caption = "\na,b,c: differences across sampling points    *: differences between treatments    +: differences between samplings") +
    ylab(case_when(
      i == "proteina_t" | i == "proteina_p"  ~ "protein mg / ml",
      i == "MDA_t" | i == "MDA_p" ~ "MDA μM ",
      i == "TEAC_t" | i == "TEAC_p"~ "Trolox equivalent μM",
      i == "GR_t" | i == "GR_p" ~ "mU / mg  of protein",
      i == "GST_t" | i == "GST_p" ~ "mU / mg  of protein",
      i == "DTD_t" | i == "DTD_p" ~ "mU / mg  of protein",
      TRUE ~ "U / mg  of protein")) +
    xlab("Sampling") +
    scale_color_manual(name = "Sampling point\n and treatment",
                       values = c("#414066", "#414066", "#69B4AB", "#69B4AB", "#FBBC4C", "#FBBC4C"),
                       labels = c("Calahonda control", "Calahonda dissected", "Almuñécar control", "Almuñécar dissected", "Salobreña control", "Salobreña dissected")) +
    scale_shape_manual(name = "Sampling point\n and treatment",
                       values = c(16, 17, 16, 17, 16, 17),
                       labels = c("Calahonda control", "Calahonda dissected", "Almuñécar control", "Almuñécar dissected", "Salobreña control", "Salobreña dissected")) +
    guides(linetype = "none") +
    scale_x_discrete(labels = c("First", "Second")) +
    theme_ortimar() +
    theme(legend.position = "bottom", legend.direction = "horizontal",
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10))
}
