medidas <- function(real, pronostico, entrenamiento, frecuencia = 1L) {
  ok <- is.finite(real) & is.finite(pronostico)
  real <- real[ok]; pronostico <- pronostico[ok]
  e <- real - pronostico
  if (!length(e)) stop("No hay pares válidos para evaluar.", call. = FALSE)
  escala <- mean(abs(entrenamiento[(frecuencia + 1L):length(entrenamiento)] -
                       entrenamiento[1L:(length(entrenamiento) - frecuencia)]))
  tibble::tibble(
    MSE = mean(e^2), MAD = mean(abs(e)),
    MAPE = if (any(real == 0)) NA_real_ else mean(abs(e / real)) * 100,
    MASE = if (!is.finite(escala) || escala == 0) NA_real_ else mean(abs(e)) / escala
  )
}

ljung_box <- function(r, T = length(r), m = min(floor(T / 4), 24L), p = 0L, alpha = 0.05) {
  r <- as.numeric(r); m <- as.integer(m); df <- m - p
  if (m < 1L || length(r) < m || df <= 0L) stop("Se requiere m >= 1, suficientes autocorrelaciones y m-p > 0.", call. = FALSE)
  Q <- T * (T + 2) * sum(r[seq_len(m)]^2 / (T - seq_len(m)))
  list(estadistico = unname(Q), df = df, critico = stats::qchisq(1 - alpha, df),
       p_valor = stats::pchisq(Q, df, lower.tail = FALSE), alpha = alpha)
}

jarque_bera <- function(e, alpha = 0.05) {
  e <- e[is.finite(e)]; N <- length(e)
  if (N < 3L) stop("Jarque-Bera requiere al menos tres errores.", call. = FALSE)
  z <- (e - mean(e)) / sqrt(mean((e - mean(e))^2))
  A <- mean(z^3); K <- mean(z^4)
  JB <- N / 6 * (A^2 + (K - 3)^2 / 4)
  list(estadistico = unname(JB), df = 2L, critico = stats::qchisq(1 - alpha, 2),
       p_valor = stats::pchisq(JB, 2, lower.tail = FALSE), alpha = alpha,
       advertencia = if (N < 20L) "Interpretar con cautela: menos de 20 errores." else NULL)
}

durbin_watson <- function(e) {
  e <- e[is.finite(e)]
  if (length(e) < 2L || sum(e^2) == 0) stop("DW requiere al menos dos errores no nulos.", call. = FALSE)
  list(estadistico = unname(sum(diff(e)^2) / sum(e^2)), df = NA_integer_,
       critico = NA_real_, p_valor = NA_real_)
}

prueba_media_cero <- function(e, alpha = 0.05) {
  e <- e[is.finite(e)]; n <- length(e); gl <- n - 1L
  t0 <- mean(e) / (stats::sd(e) / sqrt(n))
  list(estadistico = unname(t0), df = gl, critico = stats::qt(1 - alpha / 2, gl),
       p_valor = 2 * stats::pt(abs(t0), gl, lower.tail = FALSE), alpha = alpha)
}

validar_errores <- function(e, p = 0L, m = NULL, alpha = 0.05) {
  e <- e[is.finite(e)]; Tn <- length(e)
  if (Tn < 5L) stop("Se requieren al menos cinco errores de un paso.", call. = FALSE)
  if (is.null(m)) m <- min(floor(Tn / 4), 24L)
  r <- acf_manual(e, m)
  list(n_errores = Tn, media = prueba_media_cero(e, alpha),
       ljung_box = ljung_box(r, Tn, m, p, alpha),
       jarque_bera = jarque_bera(e, alpha), durbin_watson = durbin_watson(e),
       acf = tibble::tibble(lag = seq_len(m), r = r,
                            limite = stats::qnorm(0.975) / sqrt(Tn)))
}
