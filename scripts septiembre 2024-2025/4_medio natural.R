grupo1 <- datos %>% filter(cultivo == "wild")
grupo2 <- datos %>% filter(cultivo == "cultured", corte == "control")

join(grupo1, grupo2)

t.test(grupo1$SOD_t, grupo2$SOD_t)
