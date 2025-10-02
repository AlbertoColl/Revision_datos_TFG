#### ORTIMAR - Objetivo 2 (V2 cambiando graficas)
# Boxplots para publicacion
# Febrero 2025 - revision para publicacion

library(tidyverse)
library(ggpubr)

### SETUP ----
#setwd("C:/Users/Usuario/Documents/GitHub/Revision_datos_TFG")
setwd("D:/collf/Documents/GitHub/Revision_datos_TFG")

source(file = "./scripts septiembre 2024-2025/0_lectura.R")

data <- filter(datos_long, cultivo == "Si") %>%
  mutate(grupo = time:section)

data$GPx[9] <- NA # Afecta a homcedasticidad y es extremo
data$GPx[6] <- NA #HOMOCEDASTICIDAD
data$GPx[2] <- NA
data$GPx[33] <- NA #posible
data$GPx[4] <- NA # Quitar el otro dato  #4 para l GPx tentacular


data$GPx[38] <- NA # outlier extremo
data$GPx[51] <- NA # extremo
data$GPx[53] <- NA
data$GPx[66] <- NA
data$GPx[69] <- NA

# Se eliminan:
data$SOD[7] <- NA #para normalidad
data$SOD[3] <- NA #para normalidad
# Quitar la  #35 tambien


data$CAT[35] <- NA # La funcion ha identificado otro, pero su eliminacion no afecta a la normalidad de residuos, este si.

data$Mielo[62] <- NA
data$Mielo[18] <- NA #Sospechoso, y afecta a normalidad

data$Facida[66] # sospechoso, outlier no extremo

data$Fbasica[48] <- NA # outlier extremo
data$Fbasica[51] # incrementa mucho error pero no se puede quitar porque violaria homocedasticidad

data$Fbasica[9] <- NA # Afecta a homocedasticidad

data$Lisozima[69] <- NA # outlier extremo
data$Lisozima[70] <- NA # outlier extremo
data$Lisozima[38] <- NA # outlier extremo

data$MDA[65] <- NA # Outlier no extremo pero parece que puede afectar al resultado del analisis y afecta a la varianza del grupo.

### Plot building ----

# Paleta (unificar con PCA)
#c("#12A3F8","#E64B35FF","#0571B0","#BE2F36")

# Funcion de grafica
f.boxplot <- function(x){
  ggboxplot(data, x = "time", y = x, color = "grupo", fill = "grupo", alpha = 0.15, width = 0.5, add = "mean", add.params = list(shape = 1), facet.by = "tejido", panel.labs = list(tejido = c("Column", "Tentacle"))) +
    theme_minimal() + labs(color = "Treatment") +
    #geom_point(data = data, mapping =aes(x = time, y = get(x), color = grupo), alpha = 0.2, position = position_jitterdodge(seed = 42)) +
    ylab(case_when(x == "MDA" ~ "MDA (μM)",
                   x == "TEAC" ~ "TEAC (Trolox equivalent μM)",
                   x == "SOD" ~ "SOD (U / mg  of protein)",
                   x == "CAT" ~ "CAT (U / mg  of protein)",
                   x == "Mielo" ~ "MPx (mU 10^2 / mg of protein)",
                   x == "GPx" ~ "GPx (mU / mg  of protein)",
                   x == "GR" ~ "GR (mU / mg  of protein)",
                   x == "GST" ~ "GST (mU / mg  of protein)",
                   x == "DTD" ~ "NQO1 (mU / mg  of protein)",
                   x == "Facida" ~ "Acid phosphatase (mU / mg  of protein)",
                   x == "Fbasica" ~ "Alkaline phosphatase (mU / mg  of protein)")) +
    labs_pubr() + 
    scale_x_discrete(labels = c("T1", "T2")) +
    scale_fill_manual(values = c("#12A3F8","#E64B35FF","#0571B0","#BE2F36")) +
    scale_color_manual(values = c("#12A3F8","#E64B35FF","#0571B0","#BE2F36"), labels = c("Control T1", "Sectioned T1","Control T2", "Sectioned T2")) + guides(fill = "none") +
    ylim(0, 1.2*max(data %>% select(x), na.rm = T)) +
    theme(axis.title.x=element_blank(),
          axis.text.x = element_text(size = 9),
          axis.title.y = element_text(size = 11),
          axis.line = element_line(color = "gray5"),
          panel.background = element_rect(color = "gray80"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(color = "gray60"))
    
  
  filename = sprintf("%s.png", x)
  filenamesvg = sprintf("%s.svg", x)
  
  ggsave(filename, path = "./resultados/graficas2025/boxplot", width = 7, height = 3.5, units = "in")
  ggsave(filenamesvg, path = "./resultados/graficas2025/boxplot", device = "svg",width = 7, height = 3.5, units = "in")
}

plots <- sapply(colnames(data[6:18]), f.boxplot)
