validar_y <- function(y, minimo = 2L) {
  if (!is.numeric(y) || length(y) < minimo || any(!is.finite(y))) {
    stop("`y` debe ser numérico, finito, ordenado y tener al menos ", minimo, " observaciones.", call. = FALSE)
  }
  as.numeric(y)
}

fechas_ts <- function(x) {
  tt <- stats::time(x)
  f <- stats::frequency(x)
  if (f == 12) {
    anio <- floor(tt + 1e-8)
    mes <- round((tt - anio) * 12) + 1L
    return(as.Date(sprintf("%04d-%02d-01", anio, mes)))
  }
  if (f == 4) {
    anio <- floor(tt + 1e-8)
    mes <- 1L + 3L * round((tt - anio) * 4)
    return(as.Date(sprintf("%04d-%02d-01", anio, mes)))
  }
  if (f == 1) return(as.Date(sprintf("%04d-01-01", round(tt))))
  as.Date("1970-01-01") + seq_along(x) - 1L
}

leer_serie <- function(x, fuente, unidad) {
  if (missing(fuente) || !nzchar(fuente) || missing(unidad) || !nzchar(unidad)) {
    stop("`fuente` y `unidad` son textos obligatorios.", call. = FALSE)
  }
  if (inherits(x, "ts")) {
    y <- validar_y(as.numeric(x))
    fecha <- fechas_ts(x)
    frecuencia <- stats::frequency(x)
  } else if (is.character(x) && length(x) == 1L && file.exists(x)) {
    z <- utils::read.csv(x, stringsAsFactors = FALSE)
    if (!all(c("fecha", "valor") %in% names(z))) {
      stop("El CSV debe contener exactamente las columnas requeridas `fecha` y `valor`.", call. = FALSE)
    }
    fecha <- as.Date(z$fecha)
    y <- validar_y(z$valor)
    if (anyNA(fecha) || is.unsorted(fecha, strictly = TRUE)) {
      stop("Las fechas deben ser válidas y estrictamente crecientes.", call. = FALSE)
    }
    pasos <- as.numeric(diff(fecha))
    if (length(pasos) > 1L && any(pasos != pasos[1L])) {
      stop("La serie contiene fechas faltantes o no equiespaciadas.", call. = FALSE)
    }
    frecuencia <- if (length(pasos) && pasos[1L] <= 31) round(365.25 / pasos[1L]) else 1L
  } else {
    stop("`x` debe ser un objeto `ts` o la ruta de un CSV existente.", call. = FALSE)
  }
  out <- tibble::tibble(t = seq_along(y), fecha = fecha, y = y)
  attr(out, "frecuencia") <- frecuencia
  attr(out, "fuente") <- fuente
  attr(out, "unidad") <- unidad
  out
}
