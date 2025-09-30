#### ORTIMAR - Objetivo 2 (V2 cambiando graficas)
# Analisis de componentes principales tras corte
# Septiembre 2025 - publicacion + JIS

library(tidyverse)
library(psych)
library(MVN)
library(factoextra)
library(ggpubr)
library(patchwork)
library(PCAtest)

# SETUP y Carga de datos ----
#setwd("C:/Users/Usuario/Documents/GitHub/Revision_datos_TFG")
setwd("D:/collf/Documents/GitHub/Revision_datos_TFG")

source(file = "./scripts septiembre 2024-2025/0_lectura.R")

datos_2 <- datos_long %>%  filter(cultivo == "Si")

# PCA conjunto ----
datos_pca_full <- datos_2 %>%
  select(c(6:18), -proteina)

colnames(datos_pca_full) <- c("SOD", "CAT", "GPx", "GR", "GST", "DTD", "G6PDH", "TEAC", "MDA", "Acid.phospatase", "Alkaline.phosphatase", "MPx")
# Test de permutacion de Vieira

pca_per <- PCAtest(datos_pca_full)
#Empirical Psi = 14.2095, Max null Psi = 5.4332, Min null Psi = 1.7918, p-value = 0
#Empirical Phi = 0.3281, Max null Phi = 0.2029, Min null Phi = 0.1165, p-value = 0

#PC 1 is significant and accounts for 34.1% (95%-CI:28.9-41.8) of the total variation
#PC 2 is significant and accounts for 17.6% (95%-CI:14.7-24.2) of the total variation
#2 PC axes are significant and account for 51.6% of the total variation

#Variables SOD, GR, GST, DTD, TEAC, MDA, Alkaline phosphatase, and MDA have significant loadings on PC 1
#Variables CAT, GST, and Acid phosphatase have significant loadings on PC 2

# Test complementarios
KMO(datos_pca_full) # Overall MSA =  0.67

cortest.bartlett(datos_pca_full, nrow(datos_pca_full)) # p value = 1.204688e-31

mvn(datos_pca_full, mvn = "mardia") # Mutivariate normal. PCs are independent


# PCA y biplot

pc_full <- princomp(~., data = as.data.frame(datos_pca_full), cor = TRUE)
summary(pc_full)
(biplot <- fviz_pca_biplot(pc_full, axes = c(1,2), col.ind = na.omit(select(datos_2, -Lisozima))$section:na.omit(select(datos_2, -Lisozima))$time,
                           pointshape = 16, pointsize = 2.5,
                           addEllipses = TRUE, ellipse.type = "confidence",
                           label = "var", col.var = "grey20", repel = T,
                           invisible="quali") +
    scale_color_manual(labels = c("Control T1", "Control T2", "Section T1", "Section T2"), values = c("#92C5DE", "#0571B0", "#D6604D", "#F4A582")) +
    scale_fill_manual(labels = c("Control T1", "Control T2", "Section T1", "Section T2"), values = c("#92C5DE", "#0571B0", "#D6604D", "#F4A582")) +
    #ylab("PC2 (27.6%)") + xlab("PC1 (57.3%)") +
    theme_pubr() +
    labs_pubr() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(face = "italic", size = 40/.pt),
          plot.title = element_blank()))

# PCA columnar ----
datos_pca_p <- datos_2 %>% filter(tejido == "Pie") %>% 
  select(c(6:18), -proteina, -G6PDH)

colnames(datos_pca_p) <- c("SOD", "CAT", "GPx", "GR", "GST", "DTD", "TEAC", "MDA", "Acid.phospatase", "Alkaline.phosphatase", "MPx")

# Test de permutacion de Vieira
pca_per <- PCAtest(datos_pca_p)

#Empirical Psi = 10.9434, Max null Psi = 7.2704, Min null Psi = 2.2453, p-value = 0
#Empirical Phi = 0.3154, Max null Phi = 0.2571, Min null Phi = 0.1429, p-value = 0

#PC 1 is significant and accounts for 33.1% (95%-CI:26.5-46) of the total variation
#PC 2 is significant and accounts for 18.8% (95%-CI:15.9-24.8) of the total variation
#2 PC axes are significant and account for 51.9% of the total variation


#Variables SOD, CAT, GPx, GST, and MDA have significant loadings on PC 1
#Variable MPx has a significant loading on PC 2

# Test complementarios
KMO(datos_pca_p) # Overall MSA =  0.5

cortest.bartlett(datos_pca_p, nrow(datos_pca_p)) # p value =  5.141975e-10

mvn(datos_pca_p, mvn = "mardia") # Hay normalidad multivariable. Los componentes principales son independientes


# PCA y biplot

pc_pie <- princomp(~., data = as.data.frame(datos_pca_p), cor = TRUE)
summary(pc_pie)
(biplot_p <- fviz_pca_biplot(pc_pie, col.ind = na.omit(select(filter(datos_2, tejido == "Pie"), -Lisozima, -G6PDH))$section:na.omit(select(filter(datos_2, tejido == "Pie"), -Lisozima, -G6PDH))$time,
                           pointshape = 16, pointsize = 2.5,
                           addEllipses = TRUE, ellipse.type = "confidence",
                           label = "var", col.var = "grey20", repel = T,
                           invisible="quali") +
    scale_color_manual(labels = c("Control T1", "Control T2", "Section T1", "Section T2"), values = c("#92C5DE", "#0571B0", "#F4A582", "#D6604D")) +
    scale_fill_manual(labels = c("Control T1", "Control T2", "Section T1", "Section T2"), values = c("#92C5DE", "#0571B0", "#F4A582", "#D6604D")) +
    #ylab("PC2 (27.6%)") + xlab("PC1 (57.3%)") +
    theme_pubr() +
    labs_pubr() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(face = "italic", size = 40/.pt),
          plot.title = element_blank()))

# PCA tentacular ----
datos_pca_t <- datos_long2 %>%  filter(cultivo == "Si", tejido == "Tentaculo") %>% 
  select(c(6:18), -proteina, -G6PDH)

colnames(datos_pca_t) <- c("SOD", "CAT", "GPx", "GR", "GST", "DTD", "TEAC", "MDA", "Acid.phospatase", "Alkaline.phosphatase", "MPx")

# Test de permutacion de Vieira
pca_per <- PCAtest(datos_pca_t)

#Empirical Psi = 11.3702, Max null Psi = 7.5203, Min null Psi = 2.0428, p-value = 0
#Empirical Phi = 0.3215, Max null Phi = 0.2615, Min null Phi = 0.1363, p-value = 0

#PC 1 is significant and accounts for 34% (95%-CI:27.2-46) of the total variation

#Variables CAT, GR, GST, DTD, acid phosphatase, alkaline phosphatase and MPx have significant loadings on PC 1

# Test complementarios
KMO(datos_pca_t) # Overall MSA =  0.66

cortest.bartlett(datos_pca_t, nrow(datos_pca_t)) # p value =  1.168326e-11

mvn(datos_pca_t, mvn = "mardia") # Hay normalidad multivariable. Los componentes principales son independientes


# PCA y biplot

pc_tent <- princomp(~., data = as.data.frame(datos_pca_t), cor = TRUE)
summary(pc_tent)
(biplot_t <- fviz_pca_biplot(pc_tent, axes = c(1,2), col.ind = na.omit(select(filter(datos_long2, cultivo == "Si", tejido == "Tentaculo"), -Lisozima, -G6PDH))$section:na.omit(select(filter(datos_long2,cultivo == "Si", tejido == "Tentaculo"), -Lisozima, -G6PDH))$time,
                             pointshape = 16, pointsize = 2.5,
                             addEllipses = TRUE, ellipse.type = "confidence",
                             label = "var", col.var = "grey20", repel = T,
                             invisible="quali") +
    scale_color_manual(labels = c("Control T1", "Control T2", "Section T1", "Section T2"), values = c("#92C5DE", "#0571B0", "#F4A582", "#D6604D")) +
    scale_fill_manual(labels = c("Control T1", "Control T2", "Section T1", "Section T2"), values = c("#92C5DE", "#0571B0", "#F4A582", "#D6604D")) +
    #ylab("PC2 (27.6%)") + xlab("PC1 (57.3%)") +
    theme_pubr() +
    labs_pubr() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(face = "italic", size = 40/.pt),
          plot.title = element_blank()))
