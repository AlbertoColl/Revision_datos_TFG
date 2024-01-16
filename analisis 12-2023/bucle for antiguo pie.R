for (n in c(1:9)) { ### PIE
  i <- colnames(select(data_1, -GPx)[c(7:15)])[[n]]
  tabla_summ <- data_1 %>%
    filter(tejido == "Pie") %>%   group_by(cultivo, playa) %>% 
    summarise(media = mean(get(i), na.rm = T),
              desvest = sd(get(i), na.rm = T),
              error = desvest/sqrt(sum(!is.na(get(i)))))
  if ((summary(modelos_p[[n]])[[1]][["Pr(>F)"]][2])<= 0.05) {
    # Hacer aov() del parametro primero con cultivo no y luego cultivo si
    m.wild <- aov(get(i)~ playa, data = filter(data_1, cultivo == "wild", tejido == "Pie"))
    tukey_loop <- TukeyHSD(m.wild)
    cld.tukey <- multcompLetters4(m.wild, tukey_loop, reversed = T)
    (letras_w <- rownames_to_column(as.data.frame(cld.tukey$playa$Letters)))
    colnames(letras_w) <- c("playa", "tukey")
    letras_w$cultivo <- c("wild", "wild", "wild")
    # Necesito almacenar esta informacion y luego unirla con la del otro anova antes de unificarla en tabla_summ con merge().
    m.cult <- aov(get(i)~ playa, data = filter(data_1, cultivo == "cultured", tejido == "Pie"))
    tukey_loop <- TukeyHSD(m.cult)
    cld.tukey <- multcompLetters4(m.cult, tukey_loop, reversed = T)
    (letras <- rownames_to_column(as.data.frame(cld.tukey$playa$Letters)))
    colnames(letras) <- c("playa", "tukey")
    letras$cultivo <- c("cultured", "cultured", "cultured")
    letras <- rbind(letras, letras_w) # GPx no puede hacer este paso, quitar enzima?
    tabla_summ <- merge(tabla_summ, letras)
  } else {
    tabla_summ$tukey <- c("", "", "", "", "", "")
  }
  # Falta añadir seccion que evalue si hay diferencias entre cultivo
  # Activar variable interruptor en caso positivo y dar una etiqueta segun nivel de significacion
  # Modificar funcion grafica para que, si la etiqueta es positiva, añada una capa nueva con las lineas y la etiqueta.
  (p <- barras_tfg()) # CAMBIAR GRAFICA
  ggsave(paste0("./analisis 12-2023/graficas/1/", i, "_pie.png"), width = 800, height = 1000, units = "px", scale = 2, dpi = "retina")
}