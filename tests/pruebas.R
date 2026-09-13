if (!dir.exists("R") && dir.exists("../R")) setwd("..")
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))

y <- as.numeric(datasets::Nile)[1:40]
modelos <- list(
  ajustar_media(y), ajustar_mm(y, 4), ajustar_ses(y, 0.3), ajustar_dmm(y, 4),
  ajustar_tendencia(y, "lineal"), ajustar_tendencia(y, "cuadratica"),
  ajustar_tendencia(y, "exponencial"), ajustar_holt(y, 0.3, 0.2)
)
stopifnot(all(vapply(modelos, function(m) length(m$yhat) == length(y), logical(1))))
stopifnot(all(vapply(modelos, function(m) length(m$pronosticar(7)) == 7L, logical(1))))
stopifnot(all(vapply(modelos, function(m) is.list(m$parametros), logical(1))))

r_manual <- acf_manual(y, 10)
r_stats <- as.numeric(stats::acf(y, lag.max = 10, plot = FALSE)$acf)[-1]
stopifnot(max(abs(r_manual - r_stats)) < 1e-12)

e <- na.omit(y - ajustar_ses(y, 0.3)$yhat)
lb1 <- ljung_box(acf_manual(e, 6), length(e), 6, 0)
lb2 <- stats::Box.test(e, lag = 6, type = "Ljung-Box", fitdf = 0)
stopifnot(abs(lb1$estadistico - unname(lb2$statistic)) < 1e-10)

stopifnot(inherits(try(ajustar_mm(y, 1), silent = TRUE), "try-error"))
stopifnot(inherits(try(ajustar_tendencia(c(1, 2, 0, 4, 5), "exponencial"), silent = TRUE), "try-error"))
message("Todas las pruebas finalizaron correctamente.")
