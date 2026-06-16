n_control <- 149
n_sectioned <- 151

mortality_control <- 0.00
mortality_sectioned <- 0.08

table(mtcars$cyl, mtcars$am)

M <- as.table(rbind(c(149, 0), c(139, 12)))
dimnames(M) <- list(group = c("Control", "Sectioned"),
                    survival = c("Alive","Dead"))

(xsq <- chisq.test(M))

xsq$expected

(phi <- round(sqrt(xsq$statistic/300), 3))

