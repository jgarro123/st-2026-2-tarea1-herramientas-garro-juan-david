# ==============================================================================
# 03-evaluacion.R
# Métricas de pronóstico y diagnósticos estadísticos de los errores
# ==============================================================================
#
# Compara valores observados con pronósticos. entrenamiento se recibe por
# separado porque MASE necesita una escala calculada solamente con datos pasados.
medidas <- function(real, pronostico, entrenamiento, frecuencia = 1L) {
  # El operador & compara elemento a elemento. ok solo es TRUE donde tanto real
  # como pronostico son finitos; los corchetes conservan después esos pares.
  ok <- is.finite(real) & is.finite(pronostico)
  real <- real[ok]; pronostico <- pronostico[ok]
  # Por convención se define error = observado - pronosticado. Las restas se
  # hacen de forma vectorizada. El punto y coma permite dos órdenes en una línea.
  e <- real - pronostico
  # length(e) vale cero si no quedó ningún par. En una condición, !0 se interpreta
  # como TRUE, por lo que se produce un error controlado.
  if (!length(e)) stop("No hay pares válidos para evaluar.", call. = FALSE)
  # Escala de MASE: error absoluto medio del pronóstico ingenuo. Los dos rangos
  # están separados por frecuencia posiciones; con frecuencia 12 se compara
  # cada mes con el mismo mes del año anterior.
  escala <- mean(abs(entrenamiento[(frecuencia + 1L):length(entrenamiento)] -
                       entrenamiento[1L:(length(entrenamiento) - frecuencia)]))
  # Se devuelve una tabla de una fila. MSE promedia errores cuadrados, MAD
  # promedia magnitudes, MAPE expresa el error relativo porcentual y MASE compara
  # la magnitud del método con la del referente ingenuo.
  tibble::tibble(
    MSE = mean(e^2), MAD = mean(abs(e)),
    # MAPE no está definido si algún observado vale cero. NA_real_ es un faltante
    # expresamente numérico y mantiene estable el tipo de la columna.
    MAPE = if (any(real == 0)) NA_real_ else mean(abs(e / real)) * 100,
    MASE = if (!is.finite(escala) || escala == 0) NA_real_ else mean(abs(e)) / escala
  )
}

# Implementación manual de Ljung-Box. r contiene autocorrelaciones, T el número
# de observaciones o errores, m los rezagos y p los parámetros estimados que se
# descuentan de los grados de libertad.
ljung_box <- function(r, T = length(r), m = min(floor(T / 4), 24L), p = 0L, alpha = 0.05) {
  # Se normalizan los tipos de entrada. df abrevia degrees of freedom y aplica
  # la corrección m-p exigida para errores de un modelo estimado.
  r <- as.numeric(r); m <- as.integer(m); df <- m - p
  if (m < 1L || length(r) < m || df <= 0L) stop("Se requiere m >= 1, suficientes autocorrelaciones y m-p > 0.", call. = FALSE)
  # seq_len(m) selecciona r_1,...,r_m y forma T-1,...,T-m. Las operaciones son
  # vectorizadas y sum implementa la sumatoria de la fórmula de Q.
  Q <- T * (T + 2) * sum(r[seq_len(m)]^2 / (T - seq_len(m)))
  # qchisq calcula el valor crítico y pchisq con lower.tail = FALSE calcula la
  # cola derecha, es decir, el valor p. list agrupa resultados con nombres.
  list(estadistico = unname(Q), df = df, critico = stats::qchisq(1 - alpha, df),
       p_valor = stats::pchisq(Q, df, lower.tail = FALSE), alpha = alpha)
}

# Prueba de Jarque-Bera para evaluar conjuntamente asimetría y curtosis.
jarque_bera <- function(e, alpha = 0.05) {
  # Se eliminan calentamientos NA mediante indexación lógica. N conserva la
  # notación usada en la fórmula estadística.
  e <- e[is.finite(e)]; N <- length(e)
  if (N < 3L) stop("Jarque-Bera requiere al menos tres errores.", call. = FALSE)
  # Estandarización con divisor N. El operador ^ eleva cada componente del vector
  # a una potencia; mean resume después todos los elementos.
  z <- (e - mean(e)) / sqrt(mean((e - mean(e))^2))
  A <- mean(z^3); K <- mean(z^4)
  JB <- N / 6 * (A^2 + (K - 3)^2 / 4)
  # La referencia asintótica es chi-cuadrado con dos grados de libertad. La lista
  # incluye además una advertencia cuando hay menos de veinte errores.
  list(estadistico = unname(JB), df = 2L, critico = stats::qchisq(1 - alpha, 2),
       p_valor = stats::pchisq(JB, 2, lower.tail = FALSE), alpha = alpha,
       advertencia = if (N < 20L) "Interpretar con cautela: menos de 20 errores." else NULL)
}

# Durbin-Watson compara cambios consecutivos de los residuos con su suma de
# cuadrados. Un valor cercano a 2 suele indicar poca autocorrelación de orden uno.
durbin_watson <- function(e) {
  e <- e[is.finite(e)]
  if (length(e) < 2L || sum(e^2) == 0) stop("DW requiere al menos dos errores no nulos.", call. = FALSE)
  # diff crea e_2-e_1, e_3-e_2, etc. Los NA tipados indican que aquí no se
  # calculan automáticamente grados de libertad, valor crítico ni valor p.
  list(estadistico = unname(sum(diff(e)^2) / sum(e^2)), df = NA_integer_,
       critico = NA_real_, p_valor = NA_real_)
}

# Prueba t bilateral de que la media de los errores sea cero.
prueba_media_cero <- function(e, alpha = 0.05) {
  e <- e[is.finite(e)]; n <- length(e); gl <- n - 1L
  # sd(e)/sqrt(n) es el error estándar de la media; t0 divide la media observada
  # por ese error para medir cuántos errores estándar se aleja de cero.
  t0 <- mean(e) / (stats::sd(e) / sqrt(n))
  # qt produce el valor crítico bilateral. El factor 2 del valor p incorpora las
  # dos colas de la distribución t.
  list(estadistico = unname(t0), df = gl, critico = stats::qt(1 - alpha / 2, gl),
       p_valor = 2 * stats::pt(abs(t0), gl, lower.tail = FALSE), alpha = alpha)
}

# Orquesta todos los diagnósticos. Devuelve datos estructurados en vez de texto o
# gráficos, de modo que otras funciones puedan presentarlos de distintas formas.
validar_errores <- function(e, p = 0L, m = NULL, alpha = 0.05) {
  e <- e[is.finite(e)]; Tn <- length(e)
  if (Tn < 5L) stop("Se requieren al menos cinco errores de un paso.", call. = FALSE)
  # Si m se omitió, se aplica el mismo criterio de rezagos usado por correlograma.
  if (is.null(m)) m <- min(floor(Tn / 4), 24L)
  r <- acf_manual(e, m)
  # Esta lista anida cada prueba. Por ejemplo, resultado$ljung_box$p_valor entra
  # primero a la lista ljung_box y luego extrae su componente p_valor. La tabla
  # acf conserva los datos necesarios para dibujar el correlograma de errores.
  list(n_errores = Tn, media = prueba_media_cero(e, alpha),
       ljung_box = ljung_box(r, Tn, m, p, alpha),
       jarque_bera = jarque_bera(e, alpha), durbin_watson = durbin_watson(e),
       acf = tibble::tibble(lag = seq_len(m), r = r,
                            limite = stats::qnorm(0.975) / sqrt(Tn)))
}
