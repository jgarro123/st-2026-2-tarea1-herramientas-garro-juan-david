graficar_serie <- function(datos, titulo = "Serie de tiempo") {
  stopifnot(all(c("fecha", "y") %in% names(datos)))
  unidad <- attr(datos, "unidad") %||% "valor"
  fuente <- attr(datos, "fuente") %||% "no indicada"
  ggplot2::ggplot(datos, ggplot2::aes(fecha, y)) +
    ggplot2::geom_line(linewidth = 0.55, colour = "#2457A6") +
    ggplot2::labs(title = titulo, x = "Fecha", y = unidad,
                  caption = paste0("Fuente: ", fuente, "; n = ", nrow(datos))) +
    ggplot2::theme_minimal(base_size = 11)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

acf_manual <- function(y, m) {
  y <- validar_y(y)
  Tn <- length(y)
  yc <- y - mean(y)
  denom <- sum(yc^2)
  if (denom == 0) stop("La ACF no se define para una serie constante.", call. = FALSE)
  vapply(seq_len(m), function(h) sum(yc[(h + 1L):Tn] * yc[1L:(Tn - h)]) / denom, numeric(1))
}

correlograma <- function(datos, m = NULL) {
  y <- if (is.data.frame(datos)) datos$y else datos
  y <- validar_y(y, 5L)
  Tn <- length(y)
  if (is.null(m)) m <- min(floor(Tn / 4), 24L)
  m <- as.integer(m)
  if (m < 1L || m >= Tn) stop("`m` debe estar entre 1 y T-1.", call. = FALSE)
  ra <- acf_manual(y, m)
  rp <- as.numeric(stats::pacf(y, lag.max = m, plot = FALSE)$acf)
  limite <- stats::qnorm(0.975) / sqrt(Tn)
  base <- tibble::tibble(lag = seq_len(m), ACF = ra, PACF = rp)
  panel <- function(columna, nombre) {
    ggplot2::ggplot(base, ggplot2::aes(x = lag, y = .data[[columna]])) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey45") +
      ggplot2::geom_hline(yintercept = c(-limite, limite), linetype = 2, colour = "#B33A3A") +
      ggplot2::geom_segment(ggplot2::aes(xend = lag, yend = 0), linewidth = 0.55, colour = "#2457A6") +
      ggplot2::labs(title = nombre, x = "Rezago", y = "Correlación") +
      ggplot2::theme_minimal(base_size = 10)
  }
  grafico <- panel("ACF", "ACF manual") + panel("PACF", "PACF")
  estructura <- as.numeric(stats::acf(y, lag.max = m, plot = FALSE, demean = TRUE, type = "correlation")$acf)[-1L]
  attr(grafico, "datos") <- base
  attr(grafico, "limite") <- limite
  attr(grafico, "max_diferencia_acf_stats") <- max(abs(ra - estructura))
  grafico
}
