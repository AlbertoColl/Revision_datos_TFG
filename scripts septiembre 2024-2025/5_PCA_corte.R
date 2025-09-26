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
library(PCAtest)

# SETUP y Carga de datos ----
setwd("C:/Users/Usuario/Documents/GitHub/Revision_datos_TFG")
#setwd("D:/collf/Documents/GitHub/Revision_datos_TFG")

source(file = "./scripts septiembre 2024-2025/0_data_lab.R")
#source(file = "./scripts septiembre 2024-2025/0_data_laptop.R")

datos_2 <- datos_long %>%  filter(cultivo == "Si")

datos_pca <- datos_2 %>% #filter(Tejido == "Pie") %>% 
  select(c(6:18)) %>%
  #mutate(G6PDH = sqrt(G6PDH),
  #       GST = log(GST)) %>% # Creo que no merece la pena transformar datos porque no se consigue normaldiad multivariable
  scale(center = TRUE, scale = TRUE)

colnames(datos_pca) <- c("SOD", "CAT", "GPx", "GR", "GST", "DTD", "G6PDH", "Soluble.prot", "TEAC", "MDA", "Acid.phospatase", "Alkaline.phosphatase", "MPx")


# PCA conjunto ----
# Test de permutacion de Vieiraplot()
pca_per <- PCAtest(datos_pca)
#Empirical Psi = 19.9190, Max null Psi = 6.1222, Min null Psi = 2.0908, p-value = 0 
#Empirical Phi = 0.3573, Max null Phi = 0.1981, Min null Phi = 0.1158, p-value = 0

#PC 1 is significant and accounts for 36.8% (95%-CI:31.5-44.4) of the total variation
#PC 2 is significant and accounts for 16.3% (95%-CI:14-22.4) of the total variation
#2 PC axes are significant and account for 53.1% of the total variation

#Variables 1, 4, 5, 6, 8, 9, 10, 12, and 13 have significant loadings on PC 1
#Variables 2, 5, and 11 have significant loadings on PC 2

# Test complementarios
KMO(datos_pca) # Overall MSA =  0.74

cortest.bartlett(datos_pca, nrow(datos_pca)) # p value =  1.450398e-51

mvn(datos_pca, mvn = "mardia") # No multivariate normality


# PCA y biplot

pc_full <- princomp(~., data = as.data.frame(datos_pca), cor = TRUE)
summary(pc_full)
fviz_pca_biplot(pc_full)

# PCA columnar ----
datos_pca <- datos_2 %>% filter(tejido == "Pie") %>% 
  select(c(6:18)) %>%
  #mutate(G6PDH = sqrt(G6PDH),
  #       GST = log(GST)) %>% # Creo que no merece la pena transformar datos porque no se consigue normaldiad multivariable
  scale(center = TRUE, scale = TRUE)
# Test de permutacion de Vieiraplot()
pca_per <- PCAtest(datos_pca)
#Empirical Psi = 19.9190, Max null Psi = 6.1222, Min null Psi = 2.0908, p-value = 0 
#Empirical Phi = 0.3573, Max null Phi = 0.1981, Min null Phi = 0.1158, p-value = 0

#PC 1 is significant and accounts for 36.8% (95%-CI:31.5-44.4) of the total variation
#PC 2 is significant and accounts for 16.3% (95%-CI:14-22.4) of the total variation
#2 PC axes are significant and account for 53.1% of the total variation

#Variables 1, 4, 5, 6, 8, 9, 10, 12, and 13 have significant loadings on PC 1
#Variables 2, 5, and 11 have significant loadings on PC 2

# Test complementarios
KMO(datos_pca) # Overall MSA =  0.74

cortest.bartlett(datos_pca, nrow(datos_pca)) # p value =  1.450398e-51

mvn(datos_pca, mvn = "mardia") # No multivariate normality


# PCA y biplot

pc_full <- princomp(~., data = as.data.frame(datos_pca), cor = TRUE)
summary(pc_full)
fviz_pca_biplot(pc_full)
