#### ORTIMAR - Objetivo 2 (V2 cambiando graficas)
# Interaction plots para publicacion
# Mayo 2026 - peer-review

library(tidyverse)
library(ggpubr)
library(emmeans)
library(patchwork)

### SETUP ----
setwd("C:/Users/Usuario/Documents/GitHub/Revision_datos_TFG")
#setwd("D:/collf/Documents/GitHub/Revision_datos_TFG")

source(file = "./scripts septiembre 2024-2025/0_lectura.R")

data <- filter(datos_long, cultivo == "Si") %>%
  mutate(grupo = time:section,
         tejido = as.factor(tejido))

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
  ggboxplot(data, x = "time", y = x, color = "grupo", fill = "grupo",
            alpha = 0.15, width = 0.5, add = "mean", add.params = list(shape = 1),
            facet.by = "tejido", panel.labs = list(tejido = c("Column", "Tentacle"))) +
    theme_minimal() + labs(color = "Treatment") +
    #geom_point(data = data, mapping =aes(x = time, y = get(x), color = grupo), alpha = 0.2, position = position_jitterdodge(seed = 42)) +
    ylab(case_when(x == "MDA" ~ "TBARS (μmol/mg of tissue)",
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
  
  ggsave(filename, path = "./resultados/graficas2025/boxplot_fix", width = 7, height = 3.5, units = "in")
  ggsave(filenamesvg, path = "./resultados/graficas2025/boxplot_fix", device = "svg",width = 7, height = 3.5, units = "in")
}

plots <- sapply(colnames(data[6:18]), f.boxplot)


### EM MEANS ----
# Ejemplo con MDA_t


fit <- lm(data = filter(data, tejido == "Tentaculo"), formula = GPx ~ section*time)
# con tejido
fit <- data %>% group_by(tejido) %>% 
  do(model = lm(formula = GPx ~ section*time, data = .))


# Get estimated marginal means and errors
emm_p= as.data.frame(emmeans(fit$model[[1]], specs=c("section", "time")))
emm_t= as.data.frame(emmeans(fit$model[[2]], specs=c("section", "time")))
emm <- rbind(emm_p, emm_t) %>% 
  add_column(.before = 1, variable = rep(c("Pie", "Tentaculo"), each = 4))

# Las medias estimadas no coinciden perfectamente con las observadas porque es un diseño no balanceado debido a que se quitaron puntos

tejido.labs <- c("Column", "Tentacle")
names(tejido.labs) <- c("Pie", "Tentaculo")

f.emmeansplot <- function(x, tissue){
  # Fit linear model
  fit <- data %>% filter(tejido == tissue) %>% 
    lm(formula = as.formula(paste0(x," ~ section * time")), data = .)
   # do(model = lm(formula = as.formula(paste0(x," ~ section * time")), data = .))
  
  # Get estimated marginal means and errors
  emm= as.data.frame(emmeans(fit, specs=c("section", "time")))
  #emm <- rbind(emm_p, emm_t) %>% 
   # add_column(.before = 1, variable = rep(c("Pie", "Tentaculo"), each = 4))
  
  # Graficar (poner noombre de grafica para patchwork)
  p <- ggplot(emm) + ylim(0, 1.5*max(emm$emmean)) +
    geom_line(data=emm, linewidth = 1.3, position = position_dodge(width = 0.25),
              alpha =0.7, aes(x=time, y=emmean, group=section, colour=section, linetype = section)) + 
    geom_point(data=emm, size=2,position = position_dodge(width = 0.25),
               aes(x=time, y=emmean, group=section, colour=section, shape = section)) + 
    geom_errorbar(data=emm, size=1, width=.1, position = position_dodge(width = 0.25),
                  alpha =0.8, aes(x=time, y=emmean, group=section, colour=section,
                      ymin=lower.CL, ymax=upper.CL)) +
    scale_colour_discrete(palette = c("#076AA3","#E64B35"), labels = c("Control", "Sectioned")) +
    scale_x_discrete(labels = c("T1", "T2")) +
    scale_linetype_discrete(labels = c("Control", "Sectioned"), guide = "none") +
    scale_shape_discrete(labels = c("Control", "Sectioned")) +
    
    labs_pubr() + 
    ylab(case_when(x == "MDA" ~ "TBARS (μmol/mg of tissue)",
                   x == "TEAC" ~ "TEAC (Trolox equivalent μM)",
                   x == "SOD" ~ "SOD (U / mg  of protein)",
                   x == "CAT" ~ "CAT (U / mg  of protein)",
                   x == "Mielo" ~ "MPO (mU 10^2 / mg of protein)",
                   x == "GPx" ~ "GPx (mU / mg  of protein)",
                   x == "GR" ~ "GR (mU / mg  of protein)",
                   x == "GST" ~ "GST (mU / mg  of protein)",
                   x == "DTD" ~ "NQO1 (mU / mg  of protein)",
                   x == "Facida" ~ "ACP (mU / mg  of protein)",
                   x == "Fbasica" ~ "ALP phosphatase (mU / mg  of protein)")) +
    theme_minimal() +
    labs(color = "Treatment", shape = "Treatment") +
    #facet_grid(~variable, labeller = labeller(variable = tejido.labs)) +
    theme(legend.position=c(.85,.88),
          axis.title.x=element_blank(),
          axis.text.x = element_text(size = 11, face = "bold"),
          axis.title.y = element_text(size = 11),
          axis.line = element_line(color = "gray5"),
          panel.background = element_rect(color = "gray80"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(color = "gray60"))

  filename = sprintf("%s.png", x)
  filenamesvg = sprintf("%s.svg", x)
  
  ggsave(filename, path = "./resultados/graficas2025/emmeans", width = 4, height = 3.5, units = "in")
  ggsave(filenamesvg, path = "./resultados/graficas2025/emmeans", device = "svg",width = 4, height = 3.5, units = "in")
  return(p)
}

# Ahora se hacen las graficas de aquellos parametros que presentaron interaccion:
p_cat <- f.emmeansplot("CAT", "Tentaculo")
p_gpx <- f.emmeansplot("GPx", "Tentaculo")
p_gst <- f.emmeansplot("GST", "Tentaculo")
p_dtd <- f.emmeansplot("DTD", "Tentaculo")
p_mpo <- f.emmeansplot("Mielo", "Pie")

#Para patchwork guaradar cada grafica en objeto
(p <- wrap_plots(p_cat,p_gpx,p_gst,p_dtd,p_mpo)) + plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face ="bold")) #+
    #plot_layout(design = "AABBCC
                          #DDEE#") + plot_annotation(tag_levels = 'A'))

ggsave(filename = "patchwork.png", path = "./resultados/graficas2025/emmeans", width = 12.5, height = 7.5, units = "in")
ggsave(filename = "patchwork.svg", path = "./resultados/graficas2025/emmeans", device = "svg",width = 12.5, height = 7.5, units = "in")
