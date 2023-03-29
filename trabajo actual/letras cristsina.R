
m <- aov(CAT_t ~ tiempo, filter(datos, playa == "Almuñecar", corte == "control"))
summary(m)

print(multcompLetters4(m, TukeyHSD(m)), reversed = TRUE)
