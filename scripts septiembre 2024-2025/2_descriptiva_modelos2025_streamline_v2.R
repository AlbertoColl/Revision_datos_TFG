# ORTIMAR - Objetivo 2 (V2 cambiando graficas)
# Analisis efecto del corte
# Febrero 2025 - revision para publicacion

# Vamos a hacer un diseño cuadrado 2x2 dentro de las anemonas cultivadas: la variable corte, que tiene 2 niveles (dissected y control), y la variable tiempo, con niveles 0 y 1. De esa manera esperamos ver el efecto del corte a corto y largo plazo.

# H0: No hay diferencias entre el estado oxidativo de anémonas control y cortadas. H1: Sí existen alguna diferencia.

# H0: No hay interacción entre el corte y el tiempo en cultivo. H1: Hay interacción entre el corte y el tiempo en cultivo.

# H0: No hay diferencias en el estado oxidativo al estar en cultivo a largo plazo. H1: Hay diferencias en el estado oxidativo al estar en cultivo a largo plazo.

# Las muestras son independientes temporalmente ya que no se trata del mismo animal (mismo cestillo?)

# ESTE SCRIPT ES LA VERSION SIN DISCRIMINAR POR PLAYA DE ORIGEN
# Aunque sabemos que siguen tendencias ligeramente diferentes, no tenemos tamaño de muestra para incluirlo como factor en el analisis. La otra opcion es agrupar por playa y hacer los test 3 veces cada uno

# Update 05/3: AL final del script he hecho los analisis separando por playa y no salen muy bonitos, volvemos a perder tamaño de muestra y la interpretacion suele ser identica o muy similar a los resultados agrupando playas.

# Update 13/3: He creado las funciones posthoc tree y table maker para sacar el display de las diferencias significativas y para añadir la tabla ANOVA a la grafica con patchwork. Me falta hacer que ambos ejes y tengan el mismo tamaño en pie y tentaculo, y luego pequeños ajustes de cara a publicacion.

library(tidyverse)
library(multcompView)
library(car)
library(rstatix)
library(gt)
library(patchwork)

### SETUP y filtrado de datos ----

#setwd("C:/Users/Usuario/Documents/GitHub/Revision_datos_TFG")
setwd("D:/collf/Documents/GitHub/Revision_datos_TFG")

#source(file = "./scripts septiembre 2024-2025/0_data_lab.R")
source(file = "./scripts septiembre 2024-2025/0_data_laptop.R")

source(file = "./scripts septiembre 2024-2025/1_funciones_graficas.R")

# Filtrar: solo anemonas cultivadas
data_2 <- filter(datos, cultivo == "cultured")

### Exploracion ----

ggplot(data_2, aes(y = GPx_p)) +
  geom_boxplot(aes(x = time:section, color = time:section), alpha = 0) +
  geom_point(aes(x = time:section, color = time:section), alpha = 1, size = 2)

# Deteccion de outliers
view(data_2 %>%  group_by(section:time) %>%  identify_outliers(GPx_t))

data_2$GPx_t[9] <- NA # Afecta a homcedasticidad y es extremo
data_2$GPx_t[6] <- NA #HOMOCEDASTICIDAD
data_2$GPx_t[2] <- NA
data_2$GPx_t[33] <- NA #posible
# Quitar el otro dato  #4 para l GPx tentacular

data_2$GPx_p[2] <- NA # outlier extremo
data_2$GPx_p[15] <- NA # extremo
data_2$GPx_p[17] <- NA
data_2$GPx_p[30] <- NA
data_2$GPx_p[33] <- NA

# Se eliminan:
data_2$SOD_t[7] <- NA #para normalidad
data_2$SOD_t[3] <- NA #para normalidad
# Quitar la  #35 tambien


data_2$CAT_t[35] <- NA # La funcion ha identificado otro, pero su eliminacion no afecta a la normalidad de residuos, este si.

data_2$Mielo_p[26] <- NA
data_2$Mielo_t[18] <- NA #Sospechoso, y afecta a normalidad

data_2$Facida_p[30] # sospechoso, outlier no extremo

data_2$Fbasica_p[12] <- NA # outlier extremo
data_2$Fbasica_t[9] <- NA # Afecta a homocedasticidad

data_2$Lisozima_p[33] <- NA # outlier extremo
data_2$Lisozima_p[34] <- NA # outlier extremo
data_2$Lisozima_p[2] <- NA # outlier extremo

data_2$MDA_p[29] <- NA # Outlier no extremo pero parece que puede afectar al resultado del analisis y afecta a la varianza del grupo.

### Ajuste de modelos ----

# Si se elimina alguna enzima al final, eliminarla aquí
#data_2 <- data_2 %>%   select(-Lisozima_p, -GPx_t, -GPx_p)

# Ajuste de modelos ANOVA con rstatix
modelos <- lapply(colnames(data_2[c(5:31)]), function(x){
  anova_test(formula = as.formula(paste0(x, " ~ section * time")), data_2)})
(anova_results <- reduce(modelos, full_join) %>% 
    add_column(.before = 1, variable = rep(colnames(data_2[c(5:31)]), each = 3)) %>% 
    adjust_pvalue(method = "BH"))


# Test de Levene se computa exactamente igual
modelos_levene <- lapply(colnames(data_2[c(5:31)]), function(x){
  levene_test(formula = as.formula(paste0(x, " ~ section * time")), data_2)})
(levene_results <- reduce(modelos_levene, full_join) %>% 
    add_column(.before = 1, variable = colnames(data_2[c(5:31)])))

# Para el test de Shapiro-Wilks se ajustan lm para extraer residuos

modelos_lm <- lapply(colnames(data_2[c(5:31)]), function(x){
  lm(formula = as.formula(paste0(x, " ~ section * time")), data_2)})
modelos_shapiro <- lapply(modelos_lm, function(x){
  shapiro_test(residuals(x))})
(shapiro_results <- reduce(modelos_shapiro, full_join) %>% 
    add_column(.before = 1, parametro = colnames(data_2[c(5:31)])))

# Sin filtrar, no se cumple normalidad de residuos en el caso de:
# SOD_t (solucionado), CAT_t (solucionado), Fbasica_p (solucionado), Mielo_p (solucionado), Mielo_t (solucionado), lisozima_p (solucionado)

# No hay homocedasticidad en:
# Fbasica_t (solucionado), GPx_t (solucionado)

### Construccion de graficas ----

# Funcion post-hoc para asignar asteriscos y letras

posthoc_tree <- function(){
  letras <- c("", "", "", "")
  pvalues <- filter(as_tibble(anova_results), variable == i)$p.adj
  if (any(pvalues <= 0.05, na.rm = T)){
    if (pvalues[3] <= 0.05){
      t.results <-data_2 %>% 
        group_by(time) %>% 
        t_test(as.formula(paste0(i, " ~ section")), p.adjust.method = "BH")
      if(t.results[1,]$p <= 0.05){
        letras[c(1,3)] <- c("", "*")} 
      if(t.results[2,]$p <= 0.05){
        letras[c(2,4)] <- c("", "*")} 
      #Se pueden añadir tiers con case_when()
      print("Interacion is significant. Grouped t-test performed.")
      return(letras)
    }
    else{
      letras <- case_when(
        pvalues[1] <= 0.05 & pvalues[2] <= 0.05 ~ c("b", "a", "c", "b"),
        pvalues[1] <= 0.05 & tabla_summ[1,]$mean < tabla_summ[3,]$mean ~ c("a", "a", "b", "b"),
        pvalues[1] <= 0.05 & tabla_summ[1,]$mean > tabla_summ[3,]$mean~ c("b", "b", "a", "a"),
        pvalues[2] <= 0.05 & tabla_summ[1,]$mean < tabla_summ[2,]$mean ~ c("a", "b", "a", "b"),
        pvalues[2] <= 0.05 & tabla_summ[1,]$mean > tabla_summ[2,]$mean~ c("b", "a", "b", "a"))
      print("Interacion is not significant. Simple main effects computed.")
      return(letras)}}
  else{
    print("There is no significative effects")
    return(letras)}
  
}
table_maker <- function(){
  t <- anova_results %>% as_tibble() %>%
    filter(variable == i) %>%
    select(-variable, -DFn, -DFd, -p, -`p<.05`, -ges) %>%
    mutate(sign. = case_when(
      p.adj <= 0.001 ~ "***",
      p.adj <= 0.01 ~ "**",
      p.adj <= 0.05 ~ "*",
      TRUE ~ "ns"),
      p.adj = trunc(p.adj*10^4)/10^4) %>% 
    gt() %>% 
    cols_label(
      Effect = md("**Effect**"),
      `F` = md("**F statistic**"),
      p.adj = md("**p value**"),
      sign. = md("")) %>% 
    tab_header(title = md("Two-way ANOVA Table")) %>% 
    tab_options(
      heading.title.font.size = "medium",
      #column_labels.font.weight = "bold",
      table.font.size = "small",
      column_labels.font.size = "medium") %>% 
    tab_style(
      style = cell_text(color = "red3", weight = "normal"),
      locations = cells_body(
        columns = c(p.adj, sign.),
        rows = p.adj <= 0.05))
  return(t)
} # My beautiful beautiful daughter Table_maker()


# Bucle de construccion de graficas
for (n in c(1:27)) {
  i <- colnames(data_2[5:31])[[n]]
  tabla_summ <- data_2 %>% group_by(section:time) %>% 
    get_summary_stats(i, type = "mean_se") %>% 
    separate_wider_delim(`section:time`, ":", names = c("tratamiento", "time"), cols_remove=F)
  if((n %% 2) != 0) {# Memoria del limite
    limite_t = 1.3*(max(tabla_summ$mean) + max(tabla_summ$se))}
  tabla_summ$letras <- posthoc_tree()
  (p <- barras_tfg() +
      ylim(c(0, max(limite_t, (max(tabla_summ$mean) + max(tabla_summ$se))))))
  #(pt <- p/wrap_table(t, panel = "full", space = "fixed")) #Vamos a wrap tables later
  saveRDS(p, file = paste0("./resultados/graficas2025/contabla/", i, ".rds"))
  ggsave(paste0("./resultados/graficas2025/contabla/", i, ".png"), width = 80, height = 90, units = "mm", dpi = 1000)
  (t <- table_maker())
  saveRDS(t, file = paste0("./resultados/graficas2025/contabla/tabla", i, ".rds"))
}

### Construccion de figuras con patchwork ----



# Esto es de alguien de GitHub para corregir un bug de patchwork
guides_build_mod <- function (guides, theme){
  legend.spacing.y <- calc_element("legend.spacing.y", theme)  # modified by me
  legend.spacing.x <- calc_element("legend.spacing.x", theme)  # modified by me
  legend.box.margin <- calc_element("legend.box.margin", theme) %||% 
    margin()
  widths <- exec(unit.c, !!!lapply(guides, gtable_width))
  heights <- exec(unit.c, !!!lapply(guides, gtable_height))
  just <- valid.just(calc_element("legend.box.just", theme))
  xjust <- just[1]
  yjust <- just[2]
  vert <- identical(calc_element("legend.box", theme), "horizontal")
  guides <- lapply(guides, function(g) {
    editGrob(g, vp = viewport(x = xjust, y = yjust, just = c(xjust, 
                                                             yjust), height = if (vert) 
                                                               heightDetails(g)
                              else 1, width = if (!vert) 
                                widthDetails(g)
                              else 1))
  })
  guide_ind <- seq(by = 2, length.out = length(guides))
  sep_ind <- seq(2, by = 2, length.out = length(guides) - 1)
  if (vert) {
    heights <- max(heights)
    if (length(widths) != 1) {
      w <- unit(rep_len(0, length(widths) * 2 - 1), "mm")
      w[guide_ind] <- widths
      w[sep_ind] <- legend.spacing.x
      widths <- w
    }
  }
  else {
    widths <- max(widths)
    if (length(heights) != 1) {
      h <- unit(rep_len(0, length(heights) * 2 - 1), "mm")
      h[guide_ind] <- heights
      h[sep_ind] <- legend.spacing.y
      heights <- h
    }
  }
  widths <- unit.c(legend.box.margin[4], widths, legend.box.margin[2])
  heights <- unit.c(legend.box.margin[1], heights, legend.box.margin[3])
  guides <- gtable_add_grob(gtable(widths, heights, name = "guide-box"), 
                            guides, t = 1 + if (!vert) 
                              guide_ind
                            else 1, l = 1 + if (vert) 
                              guide_ind
                            else 1, name = "guides")
  gtable_add_grob(guides, element_render(theme, "legend.box.background"), 
                  t = 1, l = 1, b = -1, r = -1, z = -Inf, clip = "off", 
                  name = "legend.box.background")
}

environment(guides_build_mod) <- asNamespace('patchwork')
assignInNamespace("guides_build", guides_build_mod, ns = "patchwork")



# Aqui empieza lo mio
plots <- lapply(colnames(data_2[5:31]), function(x){readRDS(paste0("./resultados/graficas2025/contabla/", x, ".rds"))})

tables <- lapply(colnames(data_2[5:31]), function(x){readRDS(paste0("./resultados/graficas2025/contabla/tabla", x, ".rds"))})

## ARTWORK SIZING ELSEVIER
# Double column page: 190 width x 142.5 height
# One and half: 140 width x 105 height
# One column: 90 width x 67.5 height
# Min size: 30 width x 22.5 height


design <- 
  c(area(1,1,3,4), 
    area(4,1,4,4))


# SOD
(pSOD <- plots[[2]] + plots[[1]] + plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        plot.tag.position = c(0.95, 0.95)))
(tSOD <- wrap_table(tables[[2]], panel = "body", space = "fixed") + plot_spacer() + wrap_table(tables[[1]], panel = "body", space = "fixed"))
(ptSOD <- pSOD / (tSOD) + plot_layout(design = design, tag_level = "new") +
    plot_annotation(title = "SOD activity",
                    tag_levels = list(c("A", "B", "", ""))) +
    theme(plot.tag = element_text(size = 7),
          plot.title = element_text(size = 10)))
ggsave("./resultados/graficas2025/finales/1_SOD_pt.png", width = 190, height = 142.5, units = "mm", dpi = 1000)
ggsave("./resultados/graficas2025/finales/1_SOD_pt.eps", width = 190, height = 142.5, units = "mm", dpi = 1000, device = cairo_ps) # Para la revista


# CAT
(pCAT <- plots[[4]] + plots[[3]] + plot_annotation(tag_levels = "A") + 
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          plot.tag.position = c(0.95, 0.95)))
(tCAT <- wrap_table(tables[[4]], panel = "body", space = "fixed") + plot_spacer() + wrap_table(tables[[3]], panel = "body", space = "fixed"))
(ptCAT <- pCAT / (tCAT) + plot_layout(design = design, tag_level = "new") +
    plot_annotation(title = "CAT activity",
                    tag_levels = list(c("A", "B", "", ""))) +
    theme(plot.tag = element_text(size = 7),
          plot.title = element_text(size = 10)))
ggsave("./resultados/graficas2025/finales/2_CAT_pt.png", width = 190, height = 142.5, units = "mm", dpi = 1000)
ggsave("./resultados/graficas2025/finales/2_CAT_pt.eps", width = 190, height = 142.5, units = "mm", dpi = 1000, device = cairo_ps) # Para la revista


# GPx
(pGPx <- plots[[6]] + plots[[5]] + plot_annotation(tag_levels = "A") + 
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          plot.tag.position = c(0.95, 0.95)))
(tGPx <- wrap_table(tables[[6]], panel = "body", space = "fixed") + plot_spacer() + wrap_table(tables[[5]], panel = "body", space = "fixed"))
(ptGPx <- pGPx / (tGPx) + plot_layout(design = design, tag_level = "new") +
    plot_annotation(title = "GPx activity",
                    tag_levels = list(c("A", "B", "", ""))) +
    theme(plot.tag = element_text(size = 7),
          plot.title = element_text(size = 10)))
ggsave("./resultados/graficas2025/finales/3_GPx_pt.png", width = 190, height = 142.5, units = "mm", dpi = 1000)
ggsave("./resultados/graficas2025/finales/3_GPx_pt.eps", width = 190, height = 142.5, units = "mm", dpi = 1000, device = cairo_ps) # Para la revista


# GR
(pGR <- plots[[8]] + plots[[7]] + plot_annotation(tag_levels = "A") + 
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          plot.tag.position = c(0.95, 0.95)))
(tGR <- wrap_table(tables[[8]], panel = "body", space = "fixed") + plot_spacer() + wrap_table(tables[[7]], panel = "body", space = "fixed"))
(ptGR <- pGR / (tGR) + plot_layout(design = design, tag_level = "new") +
    plot_annotation(title = "GR activity",
                    tag_levels = list(c("A", "B", "", ""))) +
    theme(plot.tag = element_text(size = 7),
          plot.title = element_text(size = 10)))
ggsave("./resultados/graficas2025/finales/4_GR_pt.png", width = 190, height = 142.5, units = "mm", dpi = 1000)
ggsave("./resultados/graficas2025/finales/4_GR_pt.eps", width = 190, height = 142.5, units = "mm", dpi = 1000, device = cairo_ps) # Para la revista

# GST
(pGST <- plots[[9]] + plots[[10]] + plot_annotation(tag_levels = "A") + 
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          plot.tag.position = c(0.95, 0.95)))
(tGST <- wrap_table(tables[[9]], panel = "body", space = "fixed") + plot_spacer() + wrap_table(tables[[10]], panel = "body", space = "fixed"))
(ptGST <- pGST / (tGST) + plot_layout(design = design, tag_level = "new") +
    plot_annotation(title = "GST activity",
                    tag_levels = list(c("A", "B", "", ""))) +
    theme(plot.tag = element_text(size = 7),
          plot.title = element_text(size = 10)))
ggsave("./resultados/graficas2025/finales/5_GST_pt.png", width = 190, height = 142.5, units = "mm", dpi = 1000)
ggsave("./resultados/graficas2025/finales/5_GST_pt.eps", width = 190, height = 142.5, units = "mm", dpi = 1000, device = cairo_ps) # Para la revista


# DTD
(pDTD <- plots[[12]] + plots[[11]] + plot_annotation(tag_levels = "A") + 
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          plot.tag.position = c(0.95, 0.95)))
(tDTD <- wrap_table(tables[[12]], panel = "body", space = "fixed") + plot_spacer() + wrap_table(tables[[11]], panel = "body", space = "fixed"))
(ptDTD <- pDTD / (tDTD) + plot_layout(design = design, tag_level = "new") +
    plot_annotation(title = "DTD activity",
                    tag_levels = list(c("A", "B", "", ""))) +
    theme(plot.tag = element_text(size = 7),
          plot.title = element_text(size = 10)))
ggsave("./resultados/graficas2025/finales/6_DTD_pt.png", width = 190, height = 142.5, units = "mm", dpi = 1000)
ggsave("./resultados/graficas2025/finales/6_DTD_pt.eps", width = 190, height = 142.5, units = "mm", dpi = 1000, device = cairo_ps) # Para la revista


# TEAC
(pTEAC <- plots[[17]] + plots[[18]] + plot_annotation(tag_levels = "A") + 
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          plot.tag.position = c(0.95, 0.95)))
(tTEAC <- wrap_table(tables[[17]], panel = "body", space = "fixed") + plot_spacer() + wrap_table(tables[[18]], panel = "body", space = "fixed"))
(ptTEAC <- pTEAC / (tTEAC) + plot_layout(design = design, tag_level = "new") +
    plot_annotation(title = "Total Antioxidant Capacity (TEAC)",
                    tag_levels = list(c("A", "B", "", ""))) +
    theme(plot.tag = element_text(size = 7),
          plot.title = element_text(size = 10)))
ggsave("./resultados/graficas2025/finales/7_TEAC_pt.png", width = 190, height = 142.5, units = "mm", dpi = 1000)
ggsave("./resultados/graficas2025/finales/7_TEAC_pt.eps", width = 190, height = 142.5, units = "mm", dpi = 1000, device = cairo_ps) # Para la revista


# MDA
(pMDA <- plots[[19]] + plots[[20]] + plot_annotation(tag_levels = "A") + 
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          plot.tag.position = c(0.95, 0.95)))
(tMDA <- wrap_table(tables[[19]], panel = "body", space = "fixed") + plot_spacer() + wrap_table(tables[[20]], panel = "body", space = "fixed"))
(ptMDA <- pMDA / (tMDA) + plot_layout(design = design, tag_level = "new") +
    plot_annotation(title = "Malondialdehyde (MDA) levels",
                    tag_levels = list(c("A", "B", "", ""))) +
    theme(plot.tag = element_text(size = 7),
          plot.title = element_text(size = 10)))
ggsave("./resultados/graficas2025/finales/8_MDA_pt.png", width = 190, height = 142.5, units = "mm", dpi = 1000)
ggsave("./resultados/graficas2025/finales/8_MDA_pt.eps", width = 190, height = 142.5, units = "mm", dpi = 1000, device = cairo_ps) # Para la revista


# F Acida
(pFA <- plots[[21]] + plots[[22]] + plot_annotation(tag_levels = "A") + 
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          plot.tag.position = c(0.95, 0.95)))
(tFA <- wrap_table(tables[[21]], panel = "body", space = "fixed") + plot_spacer() + wrap_table(tables[[22]], panel = "body", space = "fixed"))
(ptFA <- pFA / (tFA) + plot_layout(design = design, tag_level = "new") +
    plot_annotation(title = "Acid Phosphatase activity",
                    tag_levels = list(c("A", "B", "", ""))) +
    theme(plot.tag = element_text(size = 7),
          plot.title = element_text(size = 10)))
ggsave("./resultados/graficas2025/finales/9_FA_pt.png", width = 190, height = 142.5, units = "mm", dpi = 1000)
ggsave("./resultados/graficas2025/finales/9_FA_pt.eps", width = 190, height = 142.5, units = "mm", dpi = 1000, device = cairo_ps) # Para la revista


# F Alcalina
(pFAl <- plots[[24]] + plots[[23]] + plot_annotation(tag_levels = "A") + 
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          plot.tag.position = c(0.95, 0.95)))
(tFAl <- wrap_table(tables[[24]], panel = "body", space = "fixed") + plot_spacer() + wrap_table(tables[[23]], panel = "body", space = "fixed"))
(ptFAl <- pFAl / (tFAl) + plot_layout(design = design, tag_level = "new") +
    plot_annotation(title = "Alkaline Phosphatase activity",
                    tag_levels = list(c("A", "B", "", ""))) +
    theme(plot.tag = element_text(size = 7),
          plot.title = element_text(size = 10)))
ggsave("./resultados/graficas2025/finales/10_FAl_pt.png", width = 190, height = 142.5, units = "mm", dpi = 1000)
ggsave("./resultados/graficas2025/finales/10_FAl_pt.eps", width = 190, height = 142.5, units = "mm", dpi = 1000, device = cairo_ps) # Para la revista


# MPx
(pMPx <- plots[[26]] + plots[[25]] + plot_annotation(tag_levels = "A") + 
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          plot.tag.position = c(0.95, 0.95)))
(tMPx <- wrap_table(tables[[26]], panel = "body", space = "fixed") + plot_spacer() + wrap_table(tables[[25]], panel = "body", space = "fixed"))
(ptMPx <- pMPx / (tMPx) + plot_layout(design = design, tag_level = "new") +
    plot_annotation(title = "Mieloperoxidase activity",
                    tag_levels = list(c("A", "B", "", ""))) +
    theme(plot.tag = element_text(size = 7),
          plot.title = element_text(size = 10)))
ggsave("./resultados/graficas2025/finales/11_MPx_pt.png", width = 190, height = 142.5, units = "mm", dpi = 1000)
ggsave("./resultados/graficas2025/finales/11_MPx_pt.eps", width = 190, height = 142.5, units = "mm", dpi = 1000, device = cairo_ps) # Para la revista


if (FALSE) {
# INTENTO ANTERIOR
(pSOD <- wrap_plt_SOD(pSOD <- wrap_plots(c(plots[2], plots[1])) +
    plot_annotation(tag_levels = list(c("A", "", "B", "")),
                    title = "SOD activity") +
    theme(plot.tag = element_text(size = 7),
          plot.title = element_text(size = 10)) +
    plot_layout(guides = "collect")) & theme(
      legend.position = "bottom"))
ggsave("./resultados/graficas2025/finales/1_SOD_p.png", width = 190, height = 142.5, units = "mm", dpi = 1000)
ggsave("./resultados/graficas2025/finales/1_SOD.eps", width = 190, height = 142.5, units = "mm", dpi = 1000, device = cairo_ps) # Para la revista

# CAT
(pCAT <- wrap_plots(c(plots[4], plots[3])) +
    plot_annotation(tag_levels = list(c("A. Column", "", "B. Tentacle", "")),
                    title = "CAT activity") +
    theme(plot.tag = element_text(size = 7),
          plot.title = element_text(size = 10, )))
ggsave("./resultados/graficas2025/finales/2_CAT.png", width = 190, height = 142.5, units = "mm", dpi = 1000)
ggsave("./resultados/graficas2025/finales/2_CAT.eps", width = 190, height = 142.5, units = "mm", dpi = 1000, device = cairo_ps) # Para la revista

# GR
(pGR <- wrap_plots(c(plots[6], plots[5])) +
    plot_annotation(tag_levels = list(c("A. Column", "", "B. Tentacle", "")),
                    title = "GR activity") +
    theme(plot.tag = element_text(size = 7),
          plot.title = element_text(size = 10, )))
ggsave("./resultados/graficas2025/finales/3_GR.png", width = 190, height = 142.5, units = "mm", dpi = 1000)
ggsave("./resultados/graficas2025/finales/3_GR.eps", width = 190, height = 142.5, units = "mm", dpi = 1000, device = cairo_ps) # Para la revista

# GST
(pGST <- wrap_plots(c(plots[7], plots[8])) +
    plot_annotation(tag_levels = list(c("A. Column", "", "B. Tentacle", "")),
                    title = "GST activity") +
    theme(plot.tag = element_text(size = 7),
          plot.title = element_text(size = 10, )))
ggsave("./resultados/graficas2025/finales/4_GST.png", width = 190, height = 142.5, units = "mm", dpi = 1000)
ggsave("./resultados/graficas2025/finales/4_GST.eps", width = 190, height = 142.5, units = "mm", dpi = 1000, device = cairo_ps) # Para la revista

# DTD
(pDTD <- wrap_plots(c(plots[10], plots[9])) +
    plot_annotation(tag_levels = list(c("A. Column", "", "B. Tentacle", "")),
                    title = "DTD activity") +
    theme(plot.tag = element_text(size = 7),
          plot.title = element_text(size = 10, )))
ggsave("./resultados/graficas2025/finales/5_DTD.png", width = 190, height = 142.5, units = "mm", dpi = 1000)
ggsave("./resultados/graficas2025/finales/5_DTD.eps", width = 190, height = 142.5, units = "mm", dpi = 1000, device = cairo_ps) # Para la revista

# TEAC
(pTEAC <- wrap_plots(c(plots[15], plots[16])) +
    plot_annotation(tag_levels = list(c("A. Column", "", "B. Tentacle", "")),
                    title = "Total antioxidant capacity (TEAC)") +
    theme(plot.tag = element_text(size = 7),
          plot.title = element_text(size = 10, )))
ggsave("./resultados/graficas2025/finales/6_TEAC.png", width = 190, height = 142.5, units = "mm", dpi = 1000)
ggsave("./resultados/graficas2025/finales/6_TEAC.eps", width = 190, height = 142.5, units = "mm", dpi = 1000, device = cairo_ps) # Para la revista

# MDA
(pMDA <- wrap_plots(c(plots[17], plots[18])) +
    plot_annotation(tag_levels = list(c("A. Column", "", "B. Tentacle", "")),
                    title = "Malondialdehyde concentration") +
    theme(plot.tag = element_text(size = 7),
          plot.title = element_text(size = 10, )))
ggsave("./resultados/graficas2025/finales/7_MDA.png", width = 190, height = 142.5, units = "mm", dpi = 1000)
ggsave("./resultados/graficas2025/finales/7_MDA.eps", width = 190, height = 142.5, units = "mm", dpi = 1000, device = cairo_ps) # Para la revista

# F. Acida
(pAP <- wrap_plots(c(plots[19], plots[20])) +
    plot_annotation(tag_levels = list(c("A. Column", "", "B. Tentacle", "")),
                    title = "Acid Phosphatase") +
    theme(plot.tag = element_text(size = 7),
          plot.title = element_text(size = 10, )))
ggsave("./resultados/graficas2025/finales/8_AP.png", width = 190, height = 142.5, units = "mm", dpi = 1000)
ggsave("./resultados/graficas2025/finales/8_AP.eps", width = 190, height = 142.5, units = "mm", dpi = 1000, device = cairo_ps) # Para la revista

# F. Alcalina
(pAlP <- wrap_plots(c(plots[22], plots[21])) +
    plot_annotation(tag_levels = list(c("A. Column", "", "B. Tentacle", "")),
                    title = "Alkaline Phosphatase") +
    theme(plot.tag = element_text(size = 7),
          plot.title = element_text(size = 10, )))
ggsave("./resultados/graficas2025/finales/9_AlP.png", width = 190, height = 142.5, units = "mm", dpi = 1000)
ggsave("./resultados/graficas2025/finales/9_AlP.eps", width = 190, height = 142.5, units = "mm", dpi = 1000, device = cairo_ps) # Para la revista


# Mieloperoxidasa
(pMPx <- wrap_plots(c(plots[24], plots[23])) +
    plot_annotation(tag_levels = list(c("A. Column", "", "B. Tentacle", "")),
                    title = "Mieloperoxidase") +
    theme(plot.tag = element_text(size = 7),
          plot.title = element_text(size = 10, )))
ggsave("./resultados/graficas2025/finales/10_MPx.png", width = 190, height = 142.5, units = "mm", dpi = 1000)
ggsave("./resultados/graficas2025/finales/10_MPx.eps", width = 190, height = 142.5, units = "mm", dpi = 1000, device = cairo_ps) # Para la revista
}
### Separacion por playas, no usada ----
if(FALSE){
  ### Separacion por playas - CALAHONDA ----
  
  data_2c <- filter(datos, cultivo == "cultured", playa == "Calahonda")
  
  ggplot(data_2c, aes(y = CAT_t)) +
    geom_boxplot(aes(x = tiempo:corte, color = tiempo:corte), alpha = 0) +
    geom_point(aes(x = tiempo:corte, color = tiempo:corte), alpha = 1, size = 2)
  
  data_2c$SOD_p[11] # Sospechoso, se va mucho de la tendencia con los otros dos puntos
  
  data_2c <- data_2c %>%   select(-GPx_t, -GPx_p)
  
  
  modelos_c <- lapply(colnames(data_2c[c(5:29)]), function(x){
    anova_test(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2c)})
  (anova_results_c <- reduce(modelos_c, full_join) %>% 
      add_column(.before = 1, variable = rep(colnames(data_2c[c(5:29)]), each = 3)) %>% 
      adjust_pvalue(method = "BH"))
  
  
  # Test de Levene se computa exactamente igual
  modelos_levene_c <- lapply(colnames(data_2c[c(5:29)]), function(x){
    levene_test(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2c)})
  (levene_results_c <- reduce(modelos_levene_c, full_join) %>% 
      add_column(.before = 1, variable = colnames(data_2c[c(5:29)])))
  
  # Para el test de Shapiro-Wilks se ajustan lm para extraer residuos
  
  modelos_lm_c <- lapply(colnames(data_2c[c(5:29)]), function(x){
    lm(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2c)})
  modelos_shapiro_c <- lapply(modelos_lm_c, function(x){
    shapiro_test(residuals(x))})
  (shapiro_results_c <- reduce(modelos_shapiro_c, full_join) %>% 
      add_column(.before = 1, parametro = colnames(data_2c[c(5:29)])))
  
  
  # Problemas con normalidad en:
  # ninguno
  
  #Problemas con homocesdaticidad en:
  # CAT_t -> no se resuelve, es leve
  
  ### Separacion por playas - SALOBREÑA ----
  
  data_2s <- filter(datos, cultivo == "cultured", playa == "Salobreña")
  
  ggplot(data_2s, aes(y = CAT_t)) +
    geom_boxplot(aes(x = tiempo:corte, color = tiempo:corte), alpha = 0) +
    geom_point(aes(x = tiempo:corte, color = tiempo:corte), alpha = 1, size = 2)
  
  data_2s <- data_2s %>%   select(-GPx_t, -GPx_p)
  
  
  modelos_s <- lapply(colnames(data_2s[c(5:29)]), function(x){
    anova_test(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2s)})
  (anova_results_s <- reduce(modelos_s, full_join) %>% 
      add_column(.before = 1, variable = rep(colnames(data_2s[c(5:29)]), each = 3)) %>% 
      adjust_pvalue(method = "BH"))
  
  
  # Test de Levene se computa exactamente igual
  modelos_levene_s <- lapply(colnames(data_2s[c(5:29)]), function(x){
    levene_test(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2s)})
  (levene_results_s <- reduce(modelos_levene_s, full_join) %>% 
      add_column(.before = 1, variable = colnames(data_2s[c(5:29)])))
  
  # Para el test de Shapiro-Wilks se ajustan lm para extraer residuos
  
  modelos_lm_s <- lapply(colnames(data_2s[c(5:29)]), function(x){
    lm(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2s)})
  modelos_shapiro_s <- lapply(modelos_lm_s, function(x){
    shapiro_test(residuals(x))})
  (shapiro_results_s <- reduce(modelos_shapiro_s, full_join) %>% 
      add_column(.before = 1, parametro = colnames(data_2s[c(5:29)])))
  
  
  # Problemas con normalidad en:
  # CAT_t, proteina_t
  
  
  
  #Problemas con homocesdaticidad en:
  # Mielo_p
  
  ### Separacion por playas - ALMUÑECAR ----
  
  data_2a <- filter(datos, cultivo == "cultured", playa == "Almuñecar")
  
  ggplot(data_2a, aes(y = CAT_t)) +
    geom_boxplot(aes(x = tiempo:corte, color = tiempo:corte), alpha = 0) +
    geom_point(aes(x = tiempo:corte, color = tiempo:corte), alpha = 1, size = 2)
  
  data_2a <- data_2a %>%   select(-GPx_t, -GPx_p)
  
  
  modelos_a <- lapply(colnames(data_2a[c(5:29)]), function(x){
    anova_test(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2a)})
  (anova_results_a <- reduce(modelos_a, full_join) %>% 
      add_column(.before = 1, variable = rep(colnames(data_2a[c(5:29)]), each = 3)) %>% 
      adjust_pvalue(method = "BH"))
  
  
  # Test de Levene se computa exactamente igual
  modelos_levene_a <- lapply(colnames(data_2a[c(5:29)]), function(x){
    levene_test(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2a)})
  (levene_results_a <- reduce(modelos_levene_a, full_join) %>% 
      add_column(.before = 1, variable = colnames(data_2a[c(5:29)])))
  
  # Para el test de Shapiro-Wilks se ajustan lm para extraer residuos
  
  modelos_lm_a <- lapply(colnames(data_2a[c(5:29)]), function(x){
    lm(formula = as.formula(paste0(x, " ~ corte * tiempo")), data_2a)})
  modelos_shapiro_a <- lapply(modelos_lm_a, function(x){
    shapiro_test(residuals(x))})
  (shapiro_results_a <- reduce(modelos_shapiro_a, full_join) %>% 
      add_column(.before = 1, parametro = colnames(data_2a[c(5:29)])))
  
  
  # Problemas con normalidad en:
  # 
  
  #Problemas con homocesdaticidad en:
  #  
  
}

