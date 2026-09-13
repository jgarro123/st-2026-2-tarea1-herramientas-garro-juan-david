ajustar_media <- function(y) {
  y <- validar_y(y); Tn <- length(y)
  yhat <- rep(NA_real_, Tn)
  yhat[2:Tn] <- cumsum(y)[1:(Tn - 1L)] / seq_len(Tn - 1L)
  media_final <- mean(y)
  list(yhat = yhat, pronosticar = function(h) rep(media_final, validar_h(h)),
       parametros = list(media = media_final, n = Tn))
}

validar_h <- function(h) {
  if (length(h) != 1L || !is.finite(h) || h < 1 || h != as.integer(h)) {
    stop("`h` debe ser un entero positivo.", call. = FALSE)
  }
  as.integer(h)
}

ajustar_mm <- function(y, k) {
  y <- validar_y(y); k <- as.integer(k); Tn <- length(y)
  if (k < 2L || k >= Tn) stop("`k` debe ser entero, al menos 2 y menor que T.", call. = FALSE)
  yhat <- rep(NA_real_, Tn)
  for (t in (k + 1L):Tn) yhat[t] <- mean(y[(t - k):(t - 1L)])
  ultimo <- mean(tail(y, k))
  list(yhat = yhat, pronosticar = function(h) rep(ultimo, validar_h(h)),
       parametros = list(k = k, ultima_media_movil = ultimo))
}

ajustar_ses <- function(y, alpha) {
  y <- validar_y(y); Tn <- length(y)
  if (length(alpha) != 1L || alpha <= 0 || alpha >= 1) stop("`alpha` debe estar en (0,1).", call. = FALSE)
  yhat <- rep(NA_real_, Tn); nivel <- rep(NA_real_, Tn); nivel[1] <- y[1]
  for (t in 2:Tn) {
    yhat[t] <- nivel[t - 1L]
    nivel[t] <- yhat[t] + alpha * (y[t] - yhat[t])
  }
  final <- nivel[Tn]
  list(yhat = yhat, pronosticar = function(h) rep(final, validar_h(h)),
       parametros = list(alpha = alpha, nivel_final = final, niveles = nivel))
}

ajustar_dmm <- function(y, k) {
  y <- validar_y(y); k <- as.integer(k); Tn <- length(y)
  if (k < 2L || 2L * k > Tn) stop("`k` debe permitir al menos una doble media móvil: 2k <= T.", call. = FALSE)
  M <- M2 <- E <- beta <- rep(NA_real_, Tn)
  for (t in k:Tn) M[t] <- mean(y[(t - k + 1L):t])
  for (t in (2L * k - 1L):Tn) {
    M2[t] <- mean(M[(t - k + 1L):t])
    E[t] <- 2 * M[t] - M2[t]
    beta[t] <- 2 * (M[t] - M2[t]) / (k - 1)
  }
  yhat <- rep(NA_real_, Tn)
  if (2L * k <= Tn) for (t in (2L * k):Tn) yhat[t] <- E[t - 1L] + beta[t - 1L]
  ef <- E[Tn]; bf <- beta[Tn]
  list(yhat = yhat, pronosticar = function(h) { h <- validar_h(h); ef + bf * seq_len(h) },
       parametros = list(k = k, E_final = ef, beta_final = bf,
                         media_movil = M, doble_media = M2, E = E, beta = beta))
}

matriz_tendencia <- function(t, tipo) {
  if (tipo == "lineal") cbind(intercepto = 1, t = t)
  else if (tipo == "cuadratica") cbind(intercepto = 1, t = t, t2 = t^2)
  else cbind(intercepto = 1, t = t)
}

cov_hac <- function(X, e) {
  n <- nrow(X); L <- floor(4 * (n / 100)^(2 / 9))
  S <- matrix(0, ncol(X), ncol(X))
  for (t in seq_len(n)) S <- S + e[t]^2 * tcrossprod(X[t, ])
  if (L > 0L) for (h in seq_len(L)) {
    w <- 1 - h / (L + 1)
    G <- matrix(0, ncol(X), ncol(X))
    for (t in (h + 1L):n) G <- G + e[t] * e[t - h] * tcrossprod(X[t, ], X[t - h, ])
    S <- S + w * (G + t(G))
  }
  B <- solve(crossprod(X))
  list(cov = B %*% S %*% B, rezagos = L)
}

ajustar_tendencia <- function(y, tipo = c("lineal", "cuadratica", "exponencial"), corregir_sesgo = FALSE) {
  tipo <- match.arg(tipo); y <- validar_y(y, 5L); Tn <- length(y)
  if (tipo == "exponencial" && any(y <= 0)) stop("La tendencia exponencial exige observaciones positivas.", call. = FALSE)
  z <- if (tipo == "exponencial") log(y) else y
  X <- matriz_tendencia(seq_len(Tn), tipo); q <- ncol(X)
  coef <- as.numeric(solve(crossprod(X), crossprod(X, z))); names(coef) <- colnames(X)
  ajuste_escala <- as.numeric(X %*% coef); residuos_escala <- z - ajuste_escala
  sigma2 <- sum(residuos_escala^2) / (Tn - q)
  se <- sqrt(diag(sigma2 * solve(crossprod(X))))
  hac <- cov_hac(X, residuos_escala); se_hac <- sqrt(pmax(0, diag(hac$cov)))
  tabla <- tibble::tibble(termino = names(coef), estimacion = coef,
                          se_ordinario = se, se_hac = se_hac,
                          t = coef / se, p_valor = 2 * stats::pt(abs(coef / se), Tn - q, lower.tail = FALSE))
  factor_sesgo <- if (tipo == "exponencial" && corregir_sesgo) exp(sigma2 / 2) else 1
  transformar <- function(eta) if (tipo == "exponencial") exp(eta) * factor_sesgo else eta
  yhat <- rep(NA_real_, Tn)
  for (tt in (q + 1L):Tn) {
    X0 <- matriz_tendencia(seq_len(tt - 1L), tipo)
    b0 <- as.numeric(solve(crossprod(X0), crossprod(X0, z[seq_len(tt - 1L)])))
    yhat[tt] <- transformar(as.numeric(matriz_tendencia(tt, tipo) %*% b0))
  }
  ajuste <- transformar(ajuste_escala)
  ss_tot <- sum((z - mean(z))^2)
  r2 <- 1 - sum(residuos_escala^2) / ss_tot
  fpron <- function(h) {
    h <- validar_h(h)
    transformar(as.numeric(matriz_tendencia(Tn + seq_len(h), tipo) %*% coef))
  }
  list(yhat = yhat, pronosticar = fpron,
       parametros = list(tipo = tipo, coeficientes = tabla, R2 = r2, sigma2 = sigma2,
                         durbin_watson = durbin_watson(residuos_escala)$estadistico,
                         rezagos_hac = hac$rezagos, corregir_sesgo = corregir_sesgo,
                         factor_sesgo = factor_sesgo, ajuste_muestra_completa = ajuste))
}

ajustar_holt <- function(y, alpha, beta) {
  y <- validar_y(y); Tn <- length(y)
  if (any(c(alpha, beta) <= 0) || any(c(alpha, beta) >= 1)) stop("`alpha` y `beta` deben estar en (0,1).", call. = FALSE)
  L <- b <- rep(NA_real_, Tn); yhat <- rep(NA_real_, Tn)
  L[1] <- y[1]; b[1] <- 0
  for (t in 2:Tn) {
    yhat[t] <- L[t - 1L] + b[t - 1L]
    L[t] <- alpha * y[t] + (1 - alpha) * yhat[t]
    b[t] <- beta * (L[t] - L[t - 1L]) + (1 - beta) * b[t - 1L]
  }
  lf <- L[Tn]; bf <- b[Tn]
  list(yhat = yhat, pronosticar = function(h) { h <- validar_h(h); lf + bf * seq_len(h) },
       parametros = list(alpha = alpha, beta = beta, nivel_final = lf, tendencia_final = bf,
                         nivel = L, tendencia = b))
}

mse_un_paso <- function(modelo, y) {
  ok <- is.finite(modelo$yhat)
  if (!any(ok)) Inf else mean((y[ok] - modelo$yhat[ok])^2)
}

optimizar <- function(y, metodo = c("mm", "dmm", "ses", "holt"), rejilla = NULL) {
  y <- validar_y(y); metodo <- match.arg(metodo)
  if (metodo %in% c("mm", "dmm")) {
    ks <- if (is.null(rejilla)) 2:12 else as.integer(rejilla)
    ks <- ks[ks >= 2L & if (metodo == "dmm") 2L * ks <= length(y) else ks < length(y)]
    tab <- tibble::tibble(k = ks, MSE = vapply(ks, function(k) mse_un_paso(if (metodo == "mm") ajustar_mm(y, k) else ajustar_dmm(y, k), y), numeric(1)))
    mejor <- tab[which.min(tab$MSE), ]
    modelo <- if (metodo == "mm") ajustar_mm(y, mejor$k) else ajustar_dmm(y, mejor$k)
  } else if (metodo == "ses") {
    alphas <- if (is.null(rejilla)) seq(0.02, 0.98, 0.02) else as.numeric(rejilla)
    tab <- tibble::tibble(alpha = alphas, MSE = vapply(alphas, function(a) mse_un_paso(ajustar_ses(y, a), y), numeric(1)))
    mejor <- tab[which.min(tab$MSE), ]; modelo <- ajustar_ses(y, mejor$alpha)
  } else {
    if (is.null(rejilla)) rejilla <- expand.grid(alpha = seq(0.05, 0.95, 0.05), beta = seq(0.05, 0.95, 0.05))
    tab <- tibble::as_tibble(rejilla)
    tab$MSE <- mapply(function(a, b) mse_un_paso(ajustar_holt(y, a, b), y), tab$alpha, tab$beta)
    mejor <- tab[which.min(tab$MSE), ]; modelo <- ajustar_holt(y, mejor$alpha, mejor$beta)
  }
  list(rejilla = tab, optimo = mejor, modelo = modelo)
}
