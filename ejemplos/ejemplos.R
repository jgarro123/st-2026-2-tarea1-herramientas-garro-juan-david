options(stringsAsFactors = FALSE)
if (!dir.exists("R") && dir.exists("../R")) setwd("..")
if (!dir.exists("R")) stop("Ejecute este archivo desde la raíz del repositorio.", call. = FALSE)

paquetes <- c("dplyr", "tidyr", "purrr", "tibble", "ggplot2", "patchwork")
faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes)) stop("Faltan paquetes permitidos: ", paste(faltantes, collapse = ", "), call. = FALSE)
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))
set.seed(20260911)
dir.create("figs", showWarnings = FALSE, recursive = TRUE)
dir.create("resultados", showWarnings = FALSE, recursive = TRUE)
dir.create("informe", showWarnings = FALSE, recursive = TRUE)

configuraciones <- list(
  list(id = "01-media", metodo = "Media recursiva", serie = "discoveries", objeto = datasets::discoveries,
       unidad = "número de descubrimientos", patron = "nivel aproximadamente estable", p = 1L),
  list(id = "02-mm", metodo = "Media móvil", serie = "Nile", objeto = datasets::Nile,
       unidad = "caudal anual (10^8 m³)", patron = "nivel local que cambia lentamente", p = 1L),
  list(id = "03-ses", metodo = "SES", serie = "LakeHuron", objeto = datasets::LakeHuron,
       unidad = "nivel en pies", patron = "nivel local sin estacionalidad", p = 1L),
  list(id = "04-dmm", metodo = "Doble media móvil", serie = "WWWusage", objeto = datasets::WWWusage,
       unidad = "usuarios conectados", patron = "nivel y tendencia local", p = 2L),
  list(id = "05-lineal", metodo = "Tendencia lineal", serie = "airmiles", objeto = datasets::airmiles,
       unidad = "millas-pasajero (millones)", patron = "tendencia creciente de largo plazo", p = 2L),
  list(id = "06-cuadratica", metodo = "Tendencia cuadrática", serie = "airmiles", objeto = datasets::airmiles,
       unidad = "millas-pasajero (millones)", patron = "curvatura en la tendencia", p = 3L),
  list(id = "07-exponencial", metodo = "Tendencia exponencial", serie = "JohnsonJohnson", objeto = datasets::JohnsonJohnson,
       unidad = "ganancias trimestrales por acción (USD)", patron = "crecimiento exponencial dominante con oscilación estacional", p = 2L),
  list(id = "08-holt", metodo = "Holt", serie = "LakeHuron", objeto = datasets::LakeHuron,
       unidad = "nivel en pies", patron = "nivel y pendiente local", p = 2L)
)

ajustar_config <- function(cfg, y) {
  switch(cfg$metodo,
    "Media recursiva" = list(modelo = ajustar_media(y), optimizacion = NULL),
    "Media móvil" = { o <- optimizar(y, "mm"); list(modelo = o$modelo, optimizacion = o) },
    "SES" = { o <- optimizar(y, "ses"); list(modelo = o$modelo, optimizacion = o) },
    "Doble media móvil" = { o <- optimizar(y, "dmm"); list(modelo = o$modelo, optimizacion = o) },
    "Tendencia lineal" = list(modelo = ajustar_tendencia(y, "lineal"), optimizacion = NULL),
    "Tendencia cuadrática" = list(modelo = ajustar_tendencia(y, "cuadratica"), optimizacion = NULL),
    "Tendencia exponencial" = list(modelo = ajustar_tendencia(y, "exponencial", corregir_sesgo = TRUE), optimizacion = NULL),
    "Holt" = { o <- optimizar(y, "holt"); list(modelo = o$modelo, optimizacion = o) }
  )
}

parametros_breves <- function(modelo) {
  p <- modelo$parametros
  if (!is.null(p$k)) return(paste0("k=", p$k))
  if (!is.null(p$alpha) && !is.null(p$beta)) return(sprintf("alpha=%.2f; beta=%.2f", p$alpha, p$beta))
  if (!is.null(p$alpha)) return(sprintf("alpha=%.2f", p$alpha))
  if (!is.null(p$tipo)) return(paste0(p$tipo, "; R2=", sprintf("%.3f", p$R2)))
  paste0("media=", sprintf("%.2f", p$media))
}

guardar_optimizacion <- function(o, id) {
  if (is.null(o)) return(NA_character_)
  if (all(c("alpha", "beta") %in% names(o$rejilla))) {
    g <- ggplot2::ggplot(o$rejilla, ggplot2::aes(alpha, beta, fill = MSE)) +
      ggplot2::geom_tile() + ggplot2::scale_fill_viridis_c() +
      ggplot2::geom_point(data = o$optimo, shape = 4, size = 4, stroke = 1.2) +
      ggplot2::theme_minimal() + ggplot2::labs(title = "Rejilla de Holt")
  } else {
    eje <- setdiff(names(o$rejilla), "MSE")[1]
    g <- ggplot2::ggplot(o$rejilla, ggplot2::aes(x = .data[[eje]], y = MSE)) +
      ggplot2::geom_line(colour = "#2457A6") + ggplot2::geom_point(size = 1.2) +
      ggplot2::geom_point(data = o$optimo, colour = "#B33A3A", size = 3) +
      ggplot2::theme_minimal() + ggplot2::labs(title = "Error de entrenamiento en la rejilla")
  }
  ruta <- file.path("figs", paste0(id, "-optimizacion.png"))
  ggplot2::ggsave(ruta, g, width = 6.8, height = 4.2, dpi = 130)
  ruta
}

analizar_ejemplo <- function(cfg) {
  datos <- leer_serie(cfg$objeto, paste0("R datasets::", cfg$serie, " (ayuda ?", cfg$serie, ")"), cfg$unidad)
  y <- datos$y; Tn <- length(y); f <- attr(datos, "frecuencia")
  h <- min(12L, floor(0.20 * Tn)); if (f > 1L) h <- max(h, f)
  n_train <- Tn - h; train <- y[seq_len(n_train)]; valid <- y[(n_train + 1L):Tn]
  ajuste <- ajustar_config(cfg, train); modelo <- ajuste$modelo
  pron <- modelo$pronosticar(h)
  referente <- if (f > 1L) "Ingenuo estacional" else "Ingenuo"
  ref_pron <- if (f > 1L) rep(tail(train, f), length.out = h) else rep(tail(train, 1), h)
  met_train <- medidas(train, modelo$yhat, train, f)
  met_valid <- medidas(valid, pron, train, f)
  met_ref <- medidas(valid, ref_pron, train, f)
  errores <- train - modelo$yhat; errores <- errores[is.finite(errores)]
  m_err <- min(floor(length(errores) / 4), 24L)
  diag <- validar_errores(errores, cfg$p, m_err)
  m_serie <- min(floor(n_train / 4), 24L)
  lb_serie <- ljung_box(acf_manual(train, m_serie), n_train, m_serie, 0L)

  g1 <- graficar_serie(datos, paste(cfg$serie, "—", cfg$patron))
  ruta_serie <- file.path("figs", paste0(cfg$id, "-serie.png"))
  ggplot2::ggsave(ruta_serie, g1, width = 7.2, height = 4.2, dpi = 130)
  cg <- correlograma(datos, m_serie)
  stopifnot(attr(cg, "max_diferencia_acf_stats") < 1e-12)
  ruta_corr <- file.path("figs", paste0(cfg$id, "-correlograma.png"))
  ggplot2::ggsave(ruta_corr, cg, width = 8.6, height = 3.8, dpi = 130)
  fechas <- datos$fecha
  df_final <- dplyr::bind_rows(
    tibble::tibble(fecha = fechas, valor = y, tipo = "Observado"),
    tibble::tibble(fecha = fechas[seq_len(n_train)], valor = modelo$yhat, tipo = "Ajuste un paso"),
    tibble::tibble(fecha = fechas[(n_train + 1L):Tn], valor = pron, tipo = "Pronóstico"),
    tibble::tibble(fecha = fechas[(n_train + 1L):Tn], valor = ref_pron, tipo = "Ingenuo")
  )
  gf <- ggplot2::ggplot(df_final, ggplot2::aes(fecha, valor, colour = tipo, linetype = tipo)) +
    ggplot2::geom_line(linewidth = 0.65, na.rm = TRUE) +
    ggplot2::geom_vline(xintercept = as.numeric(fechas[n_train]), linetype = 3) +
    ggplot2::theme_minimal() + ggplot2::labs(title = paste(cfg$metodo, "en", cfg$serie), y = cfg$unidad, x = "Fecha")
  ruta_final <- file.path("figs", paste0(cfg$id, "-pronostico.png"))
  ggplot2::ggsave(ruta_final, gf, width = 7.2, height = 4.2, dpi = 130)
  de <- tibble::tibble(t = seq_along(errores), error = errores)
  ge1 <- ggplot2::ggplot(de, ggplot2::aes(t, error)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey55") +
    ggplot2::geom_line(colour = "#2457A6", linewidth = 0.5) +
    ggplot2::theme_minimal() + ggplot2::labs(title = "Errores de un paso", x = "Índice efectivo", y = "Error")
  ge2 <- ggplot2::ggplot(diag$acf, ggplot2::aes(lag, r)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey55") +
    ggplot2::geom_hline(yintercept = c(-diag$acf$limite[1], diag$acf$limite[1]), linetype = 2, colour = "#B33A3A") +
    ggplot2::geom_segment(ggplot2::aes(xend = lag, yend = 0), colour = "#2457A6") +
    ggplot2::theme_minimal() + ggplot2::labs(title = "ACF de errores", x = "Rezago", y = "r")
  ruta_errores <- file.path("figs", paste0(cfg$id, "-errores.png"))
  ggplot2::ggsave(ruta_errores, ge1 + ge2, width = 8.6, height = 3.8, dpi = 130)
  ruta_opt <- guardar_optimizacion(ajuste$optimizacion, cfg$id)

  list(cfg = cfg, datos = datos, T = Tn, f = f, h = h, n_train = n_train,
       inicio = as.character(min(datos$fecha)), fin = as.character(max(datos$fecha)),
       parametros = parametros_breves(modelo), coeficientes = modelo$parametros$coeficientes %||% NULL,
       referente = referente, met_train = met_train,
       met_valid = met_valid, met_ref = met_ref, diagnostico = diag,
       lb_serie = lb_serie, acf_diferencia = attr(cg, "max_diferencia_acf_stats"),
       figuras = c(serie = ruta_serie, correlograma = ruta_corr, pronostico = ruta_final,
                   errores = ruta_errores, optimizacion = ruta_opt),
       borde = if (!is.null(ajuste$optimizacion)) {
         op <- ajuste$optimizacion$optimo; any(unlist(op[setdiff(names(op), "MSE")]) %in%
           unlist(lapply(ajuste$optimizacion$rejilla[setdiff(names(op), "MSE")], range)))
       } else FALSE)
}

resultados <- lapply(configuraciones, analizar_ejemplo)

# Verificaciones numéricas permitidas por el enunciado.
z <- as.numeric(datasets::Nile)[1:30]
ses_v <- ajustar_ses(z, 0.34)
pesos <- function(t) 0.34 * sum((1 - 0.34)^(0:(t - 2)) * rev(z[1:(t - 1)])) + (1 - 0.34)^(t - 1) * z[1]
stopifnot(max(abs(ses_v$yhat[2:length(z)] - vapply(2:length(z), pesos, numeric(1)))) < 1e-10)
e_ver <- na.omit(z - ses_v$yhat)
m_ver <- min(6L, floor(length(e_ver) / 4))
lb_m <- ljung_box(acf_manual(e_ver, m_ver), length(e_ver), m_ver, 0)
lb_r <- stats::Box.test(e_ver, lag = m_ver, type = "Ljung-Box", fitdf = 0)
stopifnot(abs(lb_m$estadistico - unname(lb_r$statistic)) < 1e-10)

# Contraejemplo explícito: la media sobre una serie cíclica.
y_ce <- as.numeric(datasets::sunspot.year); h_ce <- 12L
tr_ce <- head(y_ce, -h_ce); va_ce <- tail(y_ce, h_ce)
mo_ce <- ajustar_media(tr_ce); pr_ce <- mo_ce$pronosticar(h_ce)
ma_ce <- medidas(va_ce, pr_ce, tr_ce, 1); ma_ref_ce <- medidas(va_ce, rep(tail(tr_ce, 1), h_ce), tr_ce, 1)
er_ce <- na.omit(tr_ce - mo_ce$yhat); dg_ce <- validar_errores(er_ce, p = 1L)
contraejemplo <- list(MASE = ma_ce$MASE, MASE_ingenuo = ma_ref_ce$MASE,
                      p_LB = dg_ce$ljung_box$p_valor, media = mean(tr_ce))

resumen <- dplyr::bind_rows(lapply(resultados, function(x) tibble::tibble(
  metodo = x$cfg$metodo, serie = x$cfg$serie, parametros = x$parametros,
  h = x$h, referente = x$referente, MASE_metodo = x$met_valid$MASE, MASE_referencia = x$met_ref$MASE,
  p_LB_errores = x$diagnostico$ljung_box$p_valor
)))
utils::write.csv(resumen, "resultados/resumen.csv", row.names = FALSE, fileEncoding = "UTF-8")

fmt <- function(x, d = 4) ifelse(is.na(x), "NA", formatC(x, digits = d, format = "f"))
esc <- function(x) gsub("&", "&amp;", gsub("<", "&lt;", gsub(">", "&gt;", x, fixed = TRUE), fixed = TRUE), fixed = TRUE)
prueba_html <- function(nombre, obj, h0, ha, formula, interpretacion) {
  decision <- if (is.na(obj$p_valor)) "No se toma una decisión probabilística" else if (obj$p_valor < 0.05) "Rechazar H0" else "No rechazar H0"
  paste0("<h4>", nombre, "</h4><ol>",
    "<li><b>Hipótesis:</b> H0: ", h0, "; Ha: ", ha, ".</li>",
    "<li><b>Estadístico:</b> ", formula, "; distribución nula y gl: ", ifelse(is.na(obj$df), "no disponibles", paste0("gl=", obj$df)), ".</li>",
    "<li><b>Región de rechazo:</b> α=0.05; rechazar cuando el estadístico excede ", fmt(obj$critico), ".</li>",
    "<li><b>Resultado:</b> estadístico=", fmt(obj$estadistico), "; p=", fmt(obj$p_valor), ".</li>",
    "<li><b>Decisión:</b> ", decision, ".</li>",
    "<li><b>Interpretación:</b> ", interpretacion, ".</li></ol>")
}

coef_html <- function(tabla) {
  if (is.null(tabla)) return("")
  filas <- paste0("<tr><td>", tabla$termino, "</td><td>", fmt(tabla$estimacion), "</td><td>",
                  fmt(tabla$se_ordinario), "</td><td>", fmt(tabla$se_hac), "</td><td>",
                  fmt(tabla$t), "</td><td>", fmt(tabla$p_valor), "</td></tr>", collapse = "")
  paste0("<h3>Coeficientes sobre la muestra de entrenamiento</h3><table><tr><th>Término</th><th>Estimación</th><th>SE ordinario</th><th>SE HAC Bartlett</th><th>t</th><th>p</th></tr>", filas, "</table>")
}

seccion_ejemplo <- function(x, i) {
  d <- x$diagnostico; mejor <- x$met_valid$MASE < x$met_ref$MASE
  conclusion <- sprintf("En validación de %d periodos, %s obtuvo MASE %.3f frente a %.3f del referente %s. %s Se recomienda solo para un horizonte de %d periodos; su límite principal es que no modela componentes omitidos ni cambios de régimen.",
                        x$h, x$cfg$metodo, x$met_valid$MASE, x$met_ref$MASE, tolower(x$referente),
                        ifelse(mejor, "La mejora numérica respalda su uso.", "No supera el referente, por lo que no se recomienda frente a él."), x$h)
  paste0("<section><h2>", i, ". ", x$cfg$metodo, " — ", x$cfg$serie, "</h2>",
    "<p><b>Metadatos verificados por código:</b> objeto <code>", x$cfg$serie, "</code>; ayuda <code>?", x$cfg$serie,
    "</code>; unidad: ", x$cfg$unidad, "; frecuencia=", x$f, "; ", x$inicio, " a ", x$fin, "; n=", x$T, ". Patrón esperado: ", x$cfg$patron, ".</p>",
    "<img src='../", x$figuras['serie'], "'><img src='../", x$figuras['correlograma'], "'>",
    prueba_html("Ljung–Box de la serie", x$lb_serie, "rho_1=...=rho_m=0", "algún rho_h distinto de 0", "Q=T(T+2)Σ r_h²/(T-h), χ²_m", "Evalúa si la serie presenta dependencia lineal global antes del ajuste"),
    "<p><b>Ajuste:</b> entrenamiento n=", x$n_train, "; validación h=", x$h, "; ", esc(x$parametros), ". ",
    ifelse(x$borde, "El óptimo cae en el borde de la rejilla; esto sugiere ampliar o reconsiderar el modelo. ", "El óptimo no queda forzado por el borde de la rejilla. "),
    "La ACF manual coincide con <code>stats::acf</code> con diferencia máxima ", format(x$acf_diferencia, scientific = TRUE), ".</p>",
    coef_html(x$coeficientes),
    ifelse(is.na(x$figuras['optimizacion']), "", paste0("<img src='../", x$figuras['optimizacion'], "'>")),
    "<table><tr><th>Muestra</th><th>MSE</th><th>MAD</th><th>MAPE</th><th>MASE</th></tr>",
    "<tr><td>Entrenamiento, un paso</td><td>", fmt(x$met_train$MSE), "</td><td>", fmt(x$met_train$MAD), "</td><td>", fmt(x$met_train$MAPE), "</td><td>", fmt(x$met_train$MASE), "</td></tr>",
    "<tr><td>Validación, método</td><td>", fmt(x$met_valid$MSE), "</td><td>", fmt(x$met_valid$MAD), "</td><td>", fmt(x$met_valid$MAPE), "</td><td>", fmt(x$met_valid$MASE), "</td></tr>",
    "<tr><td>Validación, ", x$referente, "</td><td>", fmt(x$met_ref$MSE), "</td><td>", fmt(x$met_ref$MAD), "</td><td>", fmt(x$met_ref$MAPE), "</td><td>", fmt(x$met_ref$MASE), "</td></tr></table>",
    "<p>Diagnóstico calculado con ", d$n_errores, " errores de un paso (no con T original).</p>",
    prueba_html("Media cero", d$media, "mu_e=0", "mu_e≠0", "t=promedio(e)/(s_e/sqrt(n)), t_{n-1}", "Determina si existe sesgo medio"),
    prueba_html("Ljung–Box de errores", d$ljung_box, "rho_1=...=rho_m=0", "algún rho_h distinto de 0", "Q=T_e(T_e+2)Σ r_h²/(T_e-h), χ²_{m-p}", "Determina si queda dependencia lineal aprovechable"),
    prueba_html("Jarque–Bera", d$jarque_bera, "asimetría=0 y curtosis=3", "alguna condición no se cumple", "JB=N[A²+(K-3)²/4]/6, χ²_2", "Evalúa compatibilidad aproximada con normalidad"),
    prueba_html("Durbin–Watson descriptivo", d$durbin_watson, "sin hipótesis probabilística aquí", "no aplica", "DW=Σ(e_t-e_{t-1})²/Σe_t²", "Valores cercanos a 2 sugieren poca autocorrelación de primer orden; se reporta sin p-valor"),
    "<img src='../", x$figuras['errores'], "'>",
    "<img src='../", x$figuras['pronostico'], "'><p><b>Conclusión numérica:</b> ", conclusion, "</p></section>")
}

filas <- paste0("<tr><td>", resumen$metodo, "</td><td>", resumen$serie, "</td><td>", esc(resumen$parametros),
                "</td><td>", resumen$referente, "</td><td>", fmt(resumen$MASE_metodo, 3), "</td><td>", fmt(resumen$MASE_referencia, 3), "</td></tr>", collapse = "")
html <- paste0("<!doctype html><html lang='es'><head><meta charset='utf-8'><title>Tarea 1 — Herramientas de pronóstico</title>",
  "<style>body{font-family:system-ui;max-width:1000px;margin:35px auto;line-height:1.48;color:#17202a}img{width:48%;vertical-align:top;margin:1%}table{border-collapse:collapse;width:100%}th,td{border:1px solid #bbb;padding:6px;text-align:right}th:first-child,td:first-child{text-align:left}section{border-top:2px solid #2457A6;margin-top:36px}code{background:#eee;padding:2px 4px}</style></head><body>",
  "<h1>Tarea 1: herramientas de pronóstico</h1><p><b>Autor:</b> Juan David Garro. <b>Curso:</b> Series de Tiempo, 2026-2.</p>",
  "<p>Las implementaciones son manuales y usan pronósticos de un paso, validación de origen fijo y α=0.05. ACF con divisor T y bandas ±qnorm(.975)/sqrt(n). MASE usa el ingenuo no estacional para frecuencia 1 y el ingenuo estacional para frecuencia mayor que 1.</p>",
  "<h2>Resumen</h2><table><tr><th>Método</th><th>Serie</th><th>Parámetros</th><th>Referente</th><th>MASE método</th><th>MASE referente</th></tr>", filas, "</table>",
  paste(mapply(seccion_ejemplo, resultados, seq_along(resultados), SIMPLIFY = TRUE), collapse = ""),
  "<section><h2>Contraejemplo explícito</h2><p>Se repitió la media recursiva sobre <code>sunspot.year</code>, una serie cíclica incompatible con nivel constante. MASE en validación=", fmt(contraejemplo$MASE, 3),
  "; MASE ingenuo=", fmt(contraejemplo$MASE_ingenuo, 3), "; p de Ljung–Box en errores=", fmt(contraejemplo$p_LB),
  ". La combinación de desempeño relativo y dependencia residual muestra por qué una media global no captura el ciclo.</p></section>",
  "<section><h2>Reproducibilidad y uso de IA</h2><p>Se usó asistencia de IA para estructurar funciones, proponer verificaciones y redactar una primera versión. Cada fórmula fue contrastada con las notas de clase; la ACF manual y Ljung–Box se verificaron numéricamente con funciones base permitidas. El autor debe revisar, ejecutar y ser capaz de explicar cada línea antes de entregar.</p></section>",
  "</body></html>")
writeLines(enc2utf8(html), "informe/informe.html", useBytes = TRUE)
writeLines(capture.output(sessionInfo()), "sesion-info.txt", useBytes = TRUE)
message("Ejecución completa: 8 ejemplos, contraejemplo, figuras, resultados e informe generados.")
