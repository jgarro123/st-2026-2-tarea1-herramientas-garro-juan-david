# ==============================================================================
# 02-metodos.R
# Implementaciones manuales de los ocho métodos de pronóstico
# ==============================================================================
#
# Todos los métodos siguen el mismo contrato: reciben una serie y devuelven una
# lista con yhat, una función pronosticar y una lista parametros. Este diseño
# permite que el resto del proyecto trate modelos diferentes de manera uniforme.
#
# MEDIA RECURSIVA
# En cada instante t se pronostica con la media de la información disponible
# hasta t-1. Por eso la primera posición no tiene pronóstico y queda como NA.
ajustar_media <- function(y) {
  # Primero se valida la entrada y se guarda su longitud en Tn. El punto y coma
  # separa dos instrucciones escritas en una misma línea; se ejecutan en orden.
  y <- validar_y(y); Tn <- length(y)
  # rep(valor, veces) crea un vector de tamaño Tn. NA_real_ es un faltante de
  # tipo numérico y representa las posiciones de calentamiento del método.
  yhat <- rep(NA_real_, Tn)
  # cumsum calcula las sumas acumuladas y_1, y_1+y_2, etc. Se toman solo las
  # primeras T-1 sumas y se dividen por 1,...,T-1. El resultado se guarda desde
  # la posición 2 porque el pronóstico de y_t usa datos disponibles hasta t-1.
  yhat[2:Tn] <- cumsum(y)[1:(Tn - 1L)] / seq_len(Tn - 1L)
  # Para cualquier horizonte futuro, la media simple mantiene constante la media
  # calculada con toda la muestra de entrenamiento.
  media_final <- mean(y)
  # list agrupa objetos heterogéneos con nombres. pronosticar es una función
  # anónima almacenada dentro de la lista. Esta función recuerda media_final aun
  # después de que ajustar_media termine: este comportamiento se llama cierre.
  list(yhat = yhat, pronosticar = function(h) rep(media_final, validar_h(h)),
       parametros = list(media = media_final, n = Tn))
}

# Verifica el horizonte de pronóstico. Se exige un único número, finito, positivo
# y entero. La comparación h != as.integer(h) detecta valores como 2.5.
validar_h <- function(h) {
  if (length(h) != 1L || !is.finite(h) || h < 1 || h != as.integer(h)) {
    stop("`h` debe ser un entero positivo.", call. = FALSE)
  }
  as.integer(h)
}

# MEDIA MÓVIL
# k indica cuántas observaciones recientes se promedian. Los primeros k lugares
# quedan sin pronóstico porque todavía no existe una ventana completa.
ajustar_mm <- function(y, k) {
  # as.integer fuerza el tipo de k. La validación siguiente garantiza que la
  # ventana tenga al menos dos datos y sea menor que la serie completa.
  y <- validar_y(y); k <- as.integer(k); Tn <- length(y)
  if (k < 2L || k >= Tn) stop("`k` debe ser entero, al menos 2 y menor que T.", call. = FALSE)
  # rep(valor, veces) crea un vector de tamaño Tn. NA_real_ es un faltante de
  # tipo numérico y representa las posiciones de calentamiento del método.
  yhat <- rep(NA_real_, Tn)
  # for repite una instrucción por cada t del rango. Para pronosticar y_t, los
  # corchetes seleccionan y_{t-k},...,y_{t-1}; nunca se utiliza el dato futuro.
  for (t in (k + 1L):Tn) yhat[t] <- mean(y[(t - k):(t - 1L)])
  # tail(y, k) extrae los k valores finales. Su media es el nivel que se repetirá
  # en todos los horizontes extramuestrales.
  ultimo <- mean(tail(y, k))
  list(yhat = yhat, pronosticar = function(h) rep(ultimo, validar_h(h)),
       parametros = list(k = k, ultima_media_movil = ultimo))
}

# SUAVIZAMIENTO EXPONENCIAL SIMPLE, SES
# alpha controla cuánto peso recibe la observación más reciente. Debe estar
# estrictamente entre cero y uno: valores grandes reaccionan más rápido.
ajustar_ses <- function(y, alpha) {
  # Primero se valida la entrada y se guarda su longitud en Tn. El punto y coma
  # separa dos instrucciones escritas en una misma línea; se ejecutan en orden.
  y <- validar_y(y); Tn <- length(y)
  if (length(alpha) != 1L || alpha <= 0 || alpha >= 1) stop("`alpha` debe estar en (0,1).", call. = FALSE)
  # rep(valor, veces) crea un vector de tamaño Tn. NA_real_ es un faltante de
  # tipo numérico y representa las posiciones de calentamiento del método.
  # Se reservan dos vectores: yhat para pronósticos y nivel para el estado interno.
  # El nivel inicial se fija en la primera observación según la convención usada.
  yhat <- rep(NA_real_, Tn); nivel <- rep(NA_real_, Tn); nivel[1] <- y[1]
  # El ciclo comienza en 2 porque en t=1 no existe información anterior.
  for (t in 2:Tn) {
    # El pronóstico de y_t es el nivel que se conocía al terminar t-1.
    yhat[t] <- nivel[t - 1L]
    # Forma de corrección de error: nuevo nivel = pronóstico anterior +
    # alpha por el error observado. y[t] - yhat[t] es el error de un paso.
    nivel[t] <- yhat[t] + alpha * (y[t] - yhat[t])
  }
  # Se conserva el último estado; todos los pronósticos futuros de SES son iguales.
  final <- nivel[Tn]
  list(yhat = yhat, pronosticar = function(h) rep(final, validar_h(h)),
       parametros = list(alpha = alpha, nivel_final = final, niveles = nivel))
}

# DOBLE MEDIA MÓVIL
# Se suaviza la serie una vez para obtener M y una segunda vez para obtener M2.
# La diferencia entre ambas permite estimar un nivel E y una pendiente beta.
ajustar_dmm <- function(y, k) {
  # as.integer fuerza el tipo de k. La validación siguiente garantiza que la
  # ventana tenga al menos dos datos y sea menor que la serie completa.
  y <- validar_y(y); k <- as.integer(k); Tn <- length(y)
  if (k < 2L || 2L * k > Tn) stop("`k` debe permitir al menos una doble media móvil: 2k <= T.", call. = FALSE)
  # La asignación encadenada crea un vector y lo asigna a los cuatro nombres.
  # Cada nombre se modificará después de manera independiente.
  M <- M2 <- E <- beta <- rep(NA_real_, Tn)
  # Primera media móvil: desde t=k ya existe una ventana de k datos que termina en t.
  for (t in k:Tn) M[t] <- mean(y[(t - k + 1L):t])
  # Para calcular M2 hacen falta k medias móviles válidas. La primera aparece en
  # t=2k-1. Dentro del ciclo se actualizan doble media, nivel y pendiente.
  for (t in (2L * k - 1L):Tn) {
    # Se promedian las últimas k posiciones válidas de la primera media móvil.
    M2[t] <- mean(M[(t - k + 1L):t])
    # E corrige el retraso producido por suavizar dos veces.
    E[t] <- 2 * M[t] - M2[t]
    # beta estima el cambio por periodo a partir de la separación entre M y M2.
    beta[t] <- 2 * (M[t] - M2[t]) / (k - 1)
  }
  # rep(valor, veces) crea un vector de tamaño Tn. NA_real_ es un faltante de
  # tipo numérico y representa las posiciones de calentamiento del método.
  yhat <- rep(NA_real_, Tn)
  # Solo se pronostica cuando existen E y beta del periodo anterior. El if protege
  # series pequeñas; el for interior genera cada pronóstico de un paso.
  if (2L * k <= Tn) for (t in (2L * k):Tn) yhat[t] <- E[t - 1L] + beta[t - 1L]
  # ef y bf son el nivel y la pendiente finales. La función pronosticar los
  # combina con los horizontes 1,...,h mediante ef + bf*h.
  ef <- E[Tn]; bf <- beta[Tn]
  list(yhat = yhat, pronosticar = function(h) { h <- validar_h(h); ef + bf * seq_len(h) },
       parametros = list(k = k, E_final = ef, beta_final = bf,
                         media_movil = M, doble_media = M2, E = E, beta = beta))
}

# MATRIZ DE DISEÑO PARA LAS TENDENCIAS
# cbind une vectores como columnas. La columna de unos representa el intercepto;
# t representa la pendiente y t^2 añade curvatura al modelo cuadrático.
# IDEA DE UNA MATRIZ DE DISEÑO
# Una ecuación de tendencia se puede expresar como z = X beta + error.
# Cada fila de X corresponde a un instante y cada columna a un coeficiente que
# queremos estimar. El vector beta contiene esos coeficientes.
#
# Para una tendencia lineal:
#
#   z_t = beta_0 + beta_1*t + error_t
#
# se necesita una columna de unos para beta_0 y una columna de tiempos para
# beta_1. Si t = c(1,2,3,4), la matriz queda:
#
#        intercepto   t
#             1       1
#             1       2
#   X =       1       3
#             1       4
#
# Para una tendencia cuadrática:
#
#   z_t = beta_0 + beta_1*t + beta_2*t^2 + error_t
#
# se añade una tercera columna:
#
#        intercepto   t   t2
#             1       1    1
#             1       2    4
#   X =       1       3    9
#             1       4   16
#
# La tendencia exponencial se estima después sobre z=log(y). En esa escala usa
# las mismas columnas que la tendencia lineal: intercepto y t.
matriz_tendencia <- function(t, tipo) {
  # cbind significa column bind: une sus argumentos como columnas. Aunque
  # intercepto recibe solamente el número 1, R lo recicla hasta alcanzar la
  # longitud de t, creando una columna completa de unos.
  #
  # Los nombres escritos a la izquierda de = se convierten en nombres de columna.
  # Así colnames(X) será c("intercepto", "t") y posteriormente esos nombres se
  # asignarán también a los coeficientes estimados.
  if (tipo == "lineal") cbind(intercepto = 1, t = t)
  # La segunda rama agrega t^2. El operador ^ actúa elemento por elemento:
  # si t=c(1,2,3), entonces t^2=c(1,4,9). Esta columna permite que la pendiente
  # cambie con el tiempo y, por tanto, que la trayectoria tenga curvatura.
  else if (tipo == "cuadratica") cbind(intercepto = 1, t = t, t2 = t^2)
  # La rama else corresponde a "exponencial". La selección se validó antes con
  # match.arg, de modo que no puede llegar aquí un tipo desconocido.
  #
  # Cada rama contiene una sola expresión y no necesita llaves. cbind produce la
  # matriz y, por ser la expresión elegida y final, la función la devuelve.
  else cbind(intercepto = 1, t = t)
}

# COVARIANZA HAC CON NÚCLEO DE BARTLETT
# X es la matriz de diseño y e los residuos. Esta estimación permite errores
# heterocedásticos y autocorrelacionados al calcular incertidumbre de coeficientes.
# IDEA ESTADÍSTICA
# Los coeficientes de mínimos cuadrados se calculan de la misma manera; HAC no
# cambia las estimaciones beta. Lo que cambia es la matriz de covarianza usada
# para medir su incertidumbre y, por tanto, sus errores estándar.
#
# La fórmula ordinaria supone errores con varianza constante y sin dependencia
# temporal. En series de tiempo esto puede fallar:
#   - heterocedasticidad: la magnitud de los errores cambia con el tiempo;
#   - autocorrelación: e_t guarda relación con e_{t-h}.
#
# HAC significa Heteroskedasticity and Autocorrelation Consistent. La función
# construye una matriz robusta que incorpora ambos fenómenos.
#
# DIMENSIONES DE ENTRADA
# Si existen n observaciones y q coeficientes:
#   X tiene dimensión n x q. Cada fila X[t, ] describe la observación t.
#   e tiene longitud n. e[t] es el residuo de esa misma observación.
#
# ESQUEMA DE LA FÓRMULA SÁNDWICH
#
#   cov_HAC(beta) = B %*% S %*% B
#
# donde:
#   B = inversa de (X transpuesta por X);
#   S = matriz central que reúne varianza y dependencia temporal.
#
# Se llama fórmula sándwich porque S queda en el centro y B aparece a ambos lados.
cov_hac <- function(X, e) {
  # nrow cuenta observaciones. L es el número automático de rezagos; ^ representa
  # potenciación y floor redondea hacia abajo.
  # nrow(X) obtiene n. L decide hasta cuántos rezagos de los residuos se
  # incorporarán. No usa todos los rezagos porque los muy lejanos suelen añadir
  # ruido a la estimación. La regla automática es:
  #
  #   L = piso de 4 * (n/100)^(2/9)
  #
  # Por ejemplo, con n=100 se obtiene L=floor(4)=4. Se tendrán en cuenta las
  # relaciones de los residuos con uno, dos, tres y cuatro periodos de distancia.
  n <- nrow(X); L <- floor(4 * (n / 100)^(2 / 9))
  # matrix crea una matriz cuadrada de ceros con tantas filas y columnas como
  # regresores tiene X. Allí se acumularán productos de residuos y regresores.
  # q = ncol(X). Por tanto, S comienza como una matriz q x q de ceros. Debe tener
  # esta dimensión porque al final describirá covarianzas entre los q coeficientes.
  #
  # Si el modelo lineal tiene intercepto y pendiente, q=2 y S es 2 x 2. En el
  # modelo cuadrático, q=3 y S es 3 x 3.
  S <- matrix(0, ncol(X), ncol(X))
  # X[t, ] selecciona la fila t. tcrossprod forma su producto exterior x_t x_t
  # transpuesta. Cada término se pondera por el residuo al cuadrado y se acumula.
  # PRIMERA PARTE DE S: REZAGO CERO Y HETEROCEDASTICIDAD
  #
  # X[t, ] extrae la fila t y R la simplifica a un vector de longitud q. Si esa
  # fila fuera x_t = c(1, 5), entonces:
  #
  #   tcrossprod(x_t) =
  #        1   5
  #        5  25
  #
  # Es el producto exterior x_t por x_t transpuesta, no un único número.
  # Multiplicarlo por e[t]^2 hace que una observación con residuo grande contribuya
  # más a la incertidumbre. El ciclo suma una matriz q x q por cada tiempo.
  #
  # Esta suma ya permite que cada observación tenga una varianza distinta; por
  # eso constituye la parte heterocedástica de HAC.
  for (t in seq_len(n)) S <- S + e[t]^2 * tcrossprod(X[t, ])
  # Para cada rezago h se añade la relación entre residuos separados h periodos.
  # SEGUNDA PARTE DE S: AUTOCORRELACIÓN
  # El bloque solo se ejecuta si existe al menos un rezago. Para cada h se
  # comparan residuos separados exactamente h periodos: e[t] con e[t-h].
  if (L > 0L) for (h in seq_len(L)) {
    # Peso de Bartlett: disminuye linealmente al aumentar el rezago.
    # El núcleo de Bartlett asigna el peso w_h = 1 - h/(L+1). Los rezagos
    # cercanos reciben más peso y los lejanos menos. Si L=4, los pesos son:
    #   h=1 -> 0.80, h=2 -> 0.60, h=3 -> 0.40, h=4 -> 0.20.
    # Esto evita tratar una relación lejana como si fuera tan confiable como una
    # relación entre residuos consecutivos.
    w <- 1 - h / (L + 1)
    # G es un acumulador temporal q x q para el rezago h actual. Se reinicia en
    # cero cada vez que cambia h; después su contenido se añade a S.
    G <- matrix(0, ncol(X), ncol(X))
    # tcrossprod con dos vectores crea x_t por x_{t-h} transpuesta. G acumula la
    # covariación correspondiente al rezago h.
    # El ciclo comienza en h+1 porque ese es el primer t para el cual t-h vale 1.
    # Así nunca se intenta acceder a una posición cero o negativa.
    #
    # Para cada pareja temporal se calculan dos partes:
    #
    #   e[t] * e[t-h]
    #     Es positivo si ambos residuos suelen tener el mismo signo y negativo
    #     si presentan signos opuestos.
    #
    #   tcrossprod(X[t, ], X[t-h, ])
    #     Produce x_t por x_{t-h} transpuesta, una matriz q x q que conecta los
    #     regresores actuales con los de h periodos atrás.
    #
    # G suma esas matrices para todas las parejas disponibles del rezago h.
    for (t in (h + 1L):n) G <- G + e[t] * e[t - h] * tcrossprod(X[t, ], X[t - h, ])
    # t(G) transpone la matriz. Sumar G y su transpuesta conserva la simetría.
    # G representa una dirección temporal: t relacionado con t-h. t(G) incorpora
    # la dirección complementaria y hace simétrica la contribución. Esto es
    # necesario porque una matriz de covarianza debe ser simétrica.
    #
    # El peso de Bartlett w reduce la influencia del rezago antes de añadirlo a S.
    S <- S + w * (G + t(G))
  }
  # crossprod(X) calcula X transpuesta por X. solve obtiene su inversa, que ocupa
  # ambos lados de la matriz central en la fórmula tipo sándwich.
  # crossprod(X) equivale a t(X) %*% X, pero suele ser más claro y eficiente.
  # Su resultado es q x q. solve calcula su inversa:
  #
  #   B = (X'X)^(-1)
  #
  # Esta es la misma matriz básica que aparece en la covarianza ordinaria de los
  # estimadores de mínimos cuadrados.
  B <- solve(crossprod(X))
  # %*% es multiplicación matricial. Las dimensiones son:
  #
  #   B        S        B        resultado
  #   q x q  %*% q x q %*% q x q  = q x q
  #
  # La función devuelve una lista:
  #   cov: matriz HAC de covarianzas de los coeficientes;
  #   rezagos: L, para documentar cuánta dependencia temporal se incluyó.
  #
  # Más adelante se extrae diag(cov), la diagonal con las varianzas estimadas, y
  # se aplica sqrt para obtener los errores estándar HAC de cada coeficiente.
  list(cov = B %*% S %*% B, rezagos = L)
}

# TENDENCIAS LINEAL, CUADRÁTICA Y EXPONENCIAL
# El argumento tipo ofrece tres opciones. corregir_sesgo solo afecta el retorno
# desde la escala logarítmica del modelo exponencial.
ajustar_tendencia <- function(y, tipo = c("lineal", "cuadratica", "exponencial"), corregir_sesgo = FALSE) {
  # match.arg comprueba que tipo coincida con una de las opciones declaradas y
  # permite abreviaciones no ambiguas. Después se valida la serie.
  tipo <- match.arg(tipo); y <- validar_y(y, 5L); Tn <- length(y)
  # log no está definido en los reales para cero o negativos. && exige que sea
  # modelo exponencial y además exista al menos un valor no positivo.
  if (tipo == "exponencial" && any(y <= 0)) stop("La tendencia exponencial exige observaciones positivas.", call. = FALSE)
  # if produce el vector de respuesta. El modelo exponencial se vuelve lineal al
  # trabajar con log(y); los otros modelos conservan la escala original.
  z <- if (tipo == "exponencial") log(y) else y
  # seq_len(Tn) crea los tiempos 1,...,T. q cuenta los coeficientes de la matriz X.
  # CONSTRUCCIÓN DE X PARA LA MUESTRA COMPLETA
  # seq_len(Tn) genera c(1,2,...,Tn): el índice temporal de cada observación.
  # matriz_tendencia convierte esos tiempos en las columnas adecuadas.
  #
  # Si Tn=100:
  #   modelo lineal o exponencial -> X tiene dimensión 100 x 2 y q=2;
  #   modelo cuadrático           -> X tiene dimensión 100 x 3 y q=3.
  #
  # q no es el número de observaciones: es el número de coeficientes estimados.
  # El punto y coma separa las dos asignaciones, que se ejecutan de izquierda a derecha.
  X <- matriz_tendencia(seq_len(Tn), tipo); q <- ncol(X)
  # solve(A, b) resuelve A*x=b sin calcular explícitamente la inversa. Aquí aplica
  # las ecuaciones normales: (X'X) beta = X'z. names asigna rótulos al vector.
  # DE DÓNDE SALEN LAS ECUACIONES NORMALES
  # Mínimos cuadrados busca el vector beta que minimiza:
  #
  #   suma de (z_t - valor_ajustado_t)^2
  #   = (z - X*beta)'(z - X*beta)
  #
  # Al derivar con respecto a beta e igualar a cero se obtiene:
  #
  #   X'X*beta = X'z
  #
  # Esta igualdad es un sistema lineal A*beta=b, con:
  #   A = X'X
  #   b = X'z
  #
  # TRADUCCIÓN A R
  # crossprod(X) equivale a t(X) %*% X:
  #   X es Tn x q;
  #   X' es q x Tn;
  #   X'X resulta q x q.
  #
  # crossprod(X, z) equivale a t(X) %*% z:
  #   X' es q x Tn;
  #   z tiene Tn elementos;
  #   X'z produce q resultados, uno por coeficiente.
  #
  # solve(A, b) no significa simplemente invertir A en el código. Resuelve
  # directamente el sistema A*beta=b y devuelve beta. Esto expresa exactamente:
  #
  #   solve(X'X, X'z)
  #
  # Para el modelo lineal devuelve dos valores: intercepto y pendiente. Para el
  # cuadrático devuelve tres: intercepto, pendiente lineal y curvatura.
  #
  # solve devuelve aquí una matriz de una columna. as.numeric elimina esa
  # dimensión matricial y crea un vector numérico sencillo. Luego
  # names(coef) <- colnames(X) asigna nombres comprensibles:
  #
  #   lineal:     c(intercepto=..., t=...)
  #   cuadrática: c(intercepto=..., t=..., t2=...)
  #
  # Esta implementación realiza las ecuaciones normales manualmente y no utiliza
  # lm, tal como exige la actividad.
  coef <- as.numeric(solve(crossprod(X), crossprod(X, z))); names(coef) <- colnames(X)
  # %*% es multiplicación matricial. El ajuste es X beta y el residuo es observado
  # menos ajustado en la escala original o logarítmica, según el modelo.
  # VALORES AJUSTADOS Y RESIDUOS
  # Una vez estimado coef, se reconstruye lo que el modelo explica dentro de la
  # muestra. La multiplicación es:
  #
  #        X        coef        resultado
  #     Tn x q  %*%  q x 1  =    Tn x 1
  #
  # Para una tendencia lineal, cada fila realiza:
  #
  #   ajuste_t = 1*intercepto + t*pendiente
  #
  # Para una cuadrática realiza:
  #
  #   ajuste_t = 1*intercepto + t*beta_1 + t^2*beta_2
  #
  # X %*% coef devuelve una matriz Tn x 1. as.numeric elimina la dimensión de
  # matriz y conserva un vector de longitud Tn, más cómodo para restar y graficar.
  #
  # El nombre ajuste_escala recuerda que la escala depende del tipo:
  #   - lineal y cuadrática: valores ajustados en la escala original de y;
  #   - exponencial: valores ajustados en la escala logarítmica z=log(y).
  #
  # El residuo se define como observado menos ajustado:
  #
  #   residuo_t = z_t - ajuste_t
  #
  # Por eso z, ajuste_escala y residuos_escala tienen exactamente longitud Tn.
  # Estos son residuos del ajuste sobre toda la muestra, no los pronósticos
  # históricos de un paso que se construirán más adelante en yhat.
  ajuste_escala <- as.numeric(X %*% coef); residuos_escala <- z - ajuste_escala
  # Estimación de la varianza residual con T-q grados de libertad.
  # ESTIMACIÓN DE LA VARIANZA RESIDUAL
  # residuos_escala^2 eleva cada residuo al cuadrado y sum produce la suma de
  # cuadrados residual, también llamada SSE:
  #
  #   SSE = suma desde t=1 hasta Tn de residuo_t^2
  #
  # No se divide simplemente por Tn porque para ajustar el modelo se estimaron q
  # coeficientes. Cada coeficiente consume un grado de libertad. Por eso:
  #
  #   grados de libertad residuales = Tn - q
  #   sigma2 = SSE / (Tn - q)
  #
  # Ejemplo: con 100 observaciones y una tendencia lineal hay q=2 parámetros
  # (intercepto y pendiente), así que el divisor es 100-2=98.
  #
  # sigma2 estima la varianza de los errores, no su desviación estándar. La
  # desviación residual estaría dada por sqrt(sigma2).
  sigma2 <- sum(residuos_escala^2) / (Tn - q)
  # diag extrae la diagonal de la matriz de covarianza ordinaria; sqrt convierte
  # las varianzas de los coeficientes en errores estándar.
  # ERRORES ESTÁNDAR ORDINARIOS DE LOS COEFICIENTES
  # Bajo los supuestos clásicos de varianza constante y ausencia de
  # autocorrelación, la matriz de covarianza estimada de coef es:
  #
  #   cov_ordinaria(coef) = sigma2 * (X'X)^(-1)
  #
  # La expresión se evalúa desde adentro:
  #
  #   crossprod(X)             -> X'X, matriz q x q;
  #   solve(crossprod(X))      -> inversa de X'X;
  #   sigma2 * ...             -> matriz de covarianza q x q;
  #   diag(...)                -> sus q elementos diagonales;
  #   sqrt(...)                -> errores estándar de los q coeficientes.
  #
  # La diagonal contiene varianzas individuales. Los elementos fuera de la
  # diagonal contienen covarianzas entre coeficientes y no entran directamente
  # en este vector se.
  #
  # Ejemplo conceptual: si diag(cov) = c(4, 0.09), entonces:
  #   se = sqrt(c(4, 0.09)) = c(2, 0.3).
  #
  # Estos errores se llaman ordinarios porque todavía dependen de los supuestos
  # clásicos; se_hac, calculado a continuación, ofrece la alternativa robusta.
  se <- sqrt(diag(sigma2 * solve(crossprod(X))))
  # $ extrae componentes de una lista. pmax compara elemento a elemento con cero
  # y evita raíces de pequeños valores negativos causados por redondeo numérico.
  # ERRORES ESTÁNDAR ROBUSTOS HAC
  # cov_hac devuelve una lista con dos componentes:
  #   hac$cov      -> matriz q x q de covarianza robusta;
  #   hac$rezagos  -> cantidad de rezagos incorporados.
  #
  # El operador $ entra en la lista y extrae cov. Después:
  #
  #   diag(hac$cov)
  #
  # toma las varianzas HAC de los coeficientes. En teoría deberían ser no
  # negativas. En la práctica, el redondeo numérico puede producir un valor
  # diminuto como -0.00000000000001, cuya raíz no es real.
  #
  # pmax(0, vector) compara cero con cada elemento y conserva el mayor:
  #   pmax(0, c(0.04, -1e-14)) = c(0.04, 0).
  #
  # Finalmente sqrt transforma varianzas robustas en errores estándar robustos.
  # El cálculo HAC no modifica coef; modifica únicamente la incertidumbre que se
  # atribuye a cada coeficiente.
  #
  # El punto y coma separa dos órdenes: primero se crea hac y luego se_hac puede
  # utilizar hac$cov.
  hac <- cov_hac(X, residuos_escala); se_hac <- sqrt(pmax(0, diag(hac$cov)))
  # La tabla reúne cada término, estimación e incertidumbre. pt con
  # lower.tail = FALSE calcula una cola y el factor 2 produce el valor p bilateral.
  # TABLA FINAL DE COEFICIENTES
  # tibble::tibble construye una tabla de q filas. Cada argumento nombrado se
  # convierte en una columna:
  #
  #   termino       nombre del coeficiente: intercepto, t o t2;
  #   estimacion    valor calculado en coef;
  #   se_ordinario  error estándar bajo supuestos clásicos;
  #   se_hac        error estándar robusto a heterocedasticidad y autocorrelación;
  #   t             estadístico para contrastar H0: beta_j=0;
  #   p_valor       valor p bilateral de ese contraste.
  #
  # El estadístico t se calcula como:
  #
  #   t_j = estimacion_j / error_estandar_j
  #
  # En el código actual se usa se, es decir, el error estándar ORDINARIO:
  #
  #   t = coef / se
  #
  # Esto es importante: se_hac se presenta en la tabla, pero las columnas t y
  # p_valor que siguen no se calculan con se_hac. Para un contraste robusto el
  # estadístico correspondiente sería coef/se_hac.
  #
  # CÁLCULO DEL VALOR P ACTUAL
  # abs(coef/se) toma la magnitud del estadístico porque el contraste es bilateral.
  # stats::pt(..., Tn-q, lower.tail=FALSE) calcula la probabilidad de observar,
  # bajo H0, una t mayor que esa magnitud en la cola superior.
  #
  # Como existen dos colas, una positiva y otra negativa, la probabilidad se
  # multiplica por 2:
  #
  #   p_valor = 2 * P(T > abs(t_observado))
  #
  # Los grados de libertad Tn-q son los mismos usados para sigma2.
  tabla <- tibble::tibble(termino = names(coef), estimacion = coef,
                          se_ordinario = se, se_hac = se_hac,
                          t = coef / se, p_valor = 2 * stats::pt(abs(coef / se), Tn - q, lower.tail = FALSE))
  # Al volver de logaritmos, exp del pronóstico representa una mediana. El factor
  # exp(sigma2/2) aplica la corrección lognormal cuando fue solicitada.
  # POR QUÉ EXISTE factor_sesgo
  # En el modelo exponencial no se ajusta y directamente. Se ajusta:
  #
  #   z = log(y)
  #
  # y el modelo produce un valor eta en esa escala:
  #
  #   eta_t = beta_0 + beta_1*t
  #
  # Para volver a la unidad original de y se aplica exp(eta). Sin embargo, cuando
  # los errores en escala logarítmica se modelan como normales:
  #
  #   mediana condicional de y = exp(eta)
  #   media condicional de y   = exp(eta) * exp(sigma2/2)
  #
  # Por eso corregir_sesgo decide qué interpretación se quiere:
  #   FALSE -> factor_sesgo=1 y se obtiene la mediana;
  #   TRUE  -> factor_sesgo=exp(sigma2/2) y se aproxima la media.
  #
  # Para modelos lineal y cuadrático no hubo transformación logarítmica. En esos
  # casos el factor se fija en 1 y no modifica ningún valor.
  factor_sesgo <- if (tipo == "exponencial" && corregir_sesgo) exp(sigma2 / 2) else 1
  # Función interna que unifica el retorno de escala: exponencia solo cuando el
  # tipo es exponencial y deja intactos los modelos lineal y cuadrático.
  # FUNCIÓN INTERNA transformar
  # Esta línea crea una función y la guarda en el objeto llamado transformar.
  # No ejecuta todavía la transformación: define qué deberá ocurrir cuando más
  # adelante se llame, por ejemplo, transformar(ajuste_escala).
  #
  # ¿QUÉ ES eta?
  # eta representa el predictor calculado por la ecuación de tendencia, es decir,
  # el resultado de multiplicar la matriz de diseño por los coeficientes:
  #
  #   eta = X %*% coef
  #
  # No es un coeficiente ni un residuo. Puede ser un solo pronóstico o un vector
  # completo de valores ajustados.
  #
  # La instrucción if está escrita como una expresión de una sola línea:
  #
  #   si tipo es "exponencial":
  #       devolver exp(eta) * factor_sesgo
  #   en caso contrario:
  #       devolver eta sin modificar
  #
  # CASO LINEAL O CUADRÁTICO
  # Estos modelos fueron estimados directamente sobre y. Si:
  #
  #   eta = c(10, 12, 15)
  #
  # la función devuelve exactamente c(10, 12, 15), porque esos valores ya están
  # en la unidad original de la serie.
  #
  # CASO EXPONENCIAL
  # El modelo fue estimado sobre log(y). Si:
  #
  #   eta = c(log(10), log(20))
  #
  # entonces exp actúa elemento a elemento:
  #
  #   exp(eta) = c(10, 20)
  #
  # Después se multiplica cada resultado por factor_sesgo. Si la corrección está
  # desactivada, factor_sesgo=1 y los valores permanecen c(10,20).
  #
  # Esta función es vectorizada porque exp y la multiplicación operan sobre todos
  # los elementos de eta. No necesita un ciclo for.
  #
  # ¿POR QUÉ CREAR UNA FUNCIÓN INTERNA?
  # La misma regla de retorno de escala se necesita en tres lugares:
  #   1. los pronósticos históricos de un paso almacenados en yhat;
  #   2. el ajuste de la muestra completa;
  #   3. los pronósticos futuros producidos por pronosticar.
  #
  # Encapsular la regla evita repetir el if y garantiza que los tres resultados
  # usen exactamente la misma transformación.
  #
  # transformar es también un cierre: recuerda los valores de tipo y
  # factor_sesgo existentes dentro de ajustar_tendencia, aunque no se entreguen
  # como argumentos cada vez que se invoca.
  transformar <- function(eta) if (tipo == "exponencial") exp(eta) * factor_sesgo else eta
  # rep(valor, veces) crea un vector de tamaño Tn. NA_real_ es un faltante de
  # tipo numérico y representa las posiciones de calentamiento del método.
  yhat <- rep(NA_real_, Tn)
  # Para obtener pronósticos históricos honestos, en cada tt se vuelve a estimar
  # el modelo únicamente con 1,...,tt-1. Los primeros q lugares son calentamiento.
  for (tt in (q + 1L):Tn) {
    # X0 y b0 se construyen solo con el pasado. Luego se evalúa la fila de diseño
    # correspondiente al tiempo tt para producir yhat[tt].
    X0 <- matriz_tendencia(seq_len(tt - 1L), tipo)
    b0 <- as.numeric(solve(crossprod(X0), crossprod(X0, z[seq_len(tt - 1L)])))
    yhat[tt] <- transformar(as.numeric(matriz_tendencia(tt, tipo) %*% b0))
  }
  # ajuste usa todos los datos y sirve para resumir el modelo; no debe confundirse
  # con yhat, que contiene verdaderos pronósticos de un paso.
  ajuste <- transformar(ajuste_escala)
  # Suma total de cuadrados en la escala de estimación. R2 compara la variación
  # residual con esta variación total.
  ss_tot <- sum((z - mean(z))^2)
  r2 <- 1 - sum(residuos_escala^2) / ss_tot
  # Esta función queda almacenada en el modelo. Usa los coeficientes finales y
  # construye las filas futuras T+1,...,T+h; es otro ejemplo de cierre en R.
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

# MÉTODO LINEAL DE HOLT
# Mantiene dos estados: nivel L y tendencia b. alpha actualiza el nivel y beta
# determina qué tan rápido cambia la pendiente.
ajustar_holt <- function(y, alpha, beta) {
  # Primero se valida la entrada y se guarda su longitud en Tn. El punto y coma
  # separa dos instrucciones escritas en una misma línea; se ejecutan en orden.
  y <- validar_y(y); Tn <- length(y)
  if (any(c(alpha, beta) <= 0) || any(c(alpha, beta) >= 1)) stop("`alpha` y `beta` deben estar en (0,1).", call. = FALSE)
  # Se reservan los vectores de estados y pronósticos. La asignación encadenada
  # permite inicializar L y b con la misma estructura.
  L <- b <- rep(NA_real_, Tn); yhat <- rep(NA_real_, Tn)
  # Convención inicial de la clase: primer nivel igual al primer dato y pendiente cero.
  L[1] <- y[1]; b[1] <- 0
  # El ciclo comienza en 2 porque en t=1 no existe información anterior.
  for (t in 2:Tn) {
    # Antes de observar y_t se extrapolan el nivel y la pendiente del periodo anterior.
    yhat[t] <- L[t - 1L] + b[t - 1L]
    # El nuevo nivel combina el dato observado y el pronóstico previo.
    L[t] <- alpha * y[t] + (1 - alpha) * yhat[t]
    # La nueva pendiente combina el cambio reciente de nivel y la pendiente previa.
    b[t] <- beta * (L[t] - L[t - 1L]) + (1 - beta) * b[t - 1L]
  }
  # Los estados finales se extrapolan linealmente: lf + bf para h=1, lf + 2bf, etc.
  lf <- L[Tn]; bf <- b[Tn]
  list(yhat = yhat, pronosticar = function(h) { h <- validar_h(h); lf + bf * seq_len(h) },
       parametros = list(alpha = alpha, beta = beta, nivel_final = lf, tendencia_final = bf,
                         nivel = L, tendencia = b))
}

# Función auxiliar para la optimización. Solo evalúa posiciones donde yhat es
# finito, excluyendo automáticamente el calentamiento de cada método.
mse_un_paso <- function(modelo, y) {
  # modelo$yhat accede al componente de la lista. ok es un índice lógico.
  ok <- is.finite(modelo$yhat)
  # Si no existe ningún pronóstico se devuelve Inf, de modo que esa opción nunca
  # sea elegida como óptima. De lo contrario se calcula el MSE.
  if (!any(ok)) Inf else mean((y[ok] - modelo$yhat[ok])^2)
}

# OPTIMIZACIÓN POR REJILLA
# Evalúa todas las combinaciones permitidas y selecciona el menor MSE de un paso.
# metodo se valida con match.arg y rejilla permite sustituir los valores por defecto.
optimizar <- function(y, metodo = c("mm", "dmm", "ses", "holt"), rejilla = NULL) {
  y <- validar_y(y); metodo <- match.arg(metodo)
  # %in% permite compartir una rama para media móvil y doble media móvil.
  if (metodo %in% c("mm", "dmm")) {
    # El operador : crea la secuencia entera 2,3,...,12. Si el usuario proporciona
    # una rejilla, se convierte a enteros.
    ks <- if (is.null(rejilla)) 2:12 else as.integer(rejilla)
    # Indexación lógica: se conservan solo ventanas válidas. La condición cambia
    # porque DMM necesita al menos 2k observaciones, mientras MM necesita k<T.
    ks <- ks[ks >= 2L & if (metodo == "dmm") 2L * ks <= length(y) else ks < length(y)]
    # vapply ajusta un modelo por cada k y devuelve exactamente un MSE numérico.
    # La función anónima elige MM o DMM según el texto guardado en metodo.
    tab <- tibble::tibble(k = ks, MSE = vapply(ks, function(k) mse_un_paso(if (metodo == "mm") ajustar_mm(y, k) else ajustar_dmm(y, k), y), numeric(1)))
    # which.min devuelve la posición del menor MSE. tab[fila, ] conserva todas
    # las columnas de esa fila; la coma separa selección de filas y columnas.
    mejor <- tab[which.min(tab$MSE), ]
    modelo <- if (metodo == "mm") ajustar_mm(y, mejor$k) else ajustar_dmm(y, mejor$k)
  # Segunda rama: SES explora alpha desde 0.02 hasta 0.98 en pasos de 0.02.
  } else if (metodo == "ses") {
    alphas <- if (is.null(rejilla)) seq(0.02, 0.98, 0.02) else as.numeric(rejilla)
    # Cada alpha produce un modelo distinto; vapply recoge sus MSE en un vector
    # que se convierte en la segunda columna de la tabla.
    tab <- tibble::tibble(alpha = alphas, MSE = vapply(alphas, function(a) mse_un_paso(ajustar_ses(y, a), y), numeric(1)))
    # which.min devuelve la posición del menor MSE. tab[fila, ] conserva todas
    # las columnas de esa fila; la coma separa selección de filas y columnas.
    mejor <- tab[which.min(tab$MSE), ]; modelo <- ajustar_ses(y, mejor$alpha)
  # La única opción restante después de match.arg es Holt, que necesita pares
  # de alpha y beta en vez de una sola constante.
  } else {
    # seq crea cada secuencia decimal y expand.grid forma el producto cartesiano:
    # todas las combinaciones posibles de alpha y beta.
    if (is.null(rejilla)) rejilla <- expand.grid(alpha = seq(0.05, 0.95, 0.05), beta = seq(0.05, 0.95, 0.05))
    tab <- tibble::as_tibble(rejilla)
    # mapply recorre varios vectores en paralelo. En cada fila entrega a la función
    # el alpha y beta correspondientes y guarda el MSE en una nueva columna.
    tab$MSE <- mapply(function(a, b) mse_un_paso(ajustar_holt(y, a, b), y), tab$alpha, tab$beta)
    # which.min devuelve la posición del menor MSE. tab[fila, ] conserva todas
    # las columnas de esa fila; la coma separa selección de filas y columnas.
    mejor <- tab[which.min(tab$MSE), ]; modelo <- ajustar_holt(y, mejor$alpha, mejor$beta)
  }
  # Se devuelve tanto la evidencia completa de la búsqueda como la mejor fila y
  # el modelo ya ajustado con esos parámetros.
  list(rejilla = tab, optimo = mejor, modelo = modelo)
}
