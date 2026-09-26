# ==============================================================================
# 01-graficos.R
# Visualización de la serie y construcción manual del correlograma
# ==============================================================================
#
# Produce el gráfico temporal básico. titulo tiene un valor predeterminado, así
# que puede omitirse al llamar la función. El resultado no es una imagen todavía:
# es un objeto ggplot que después puede mostrarse, combinarse o guardarse.
graficar_serie <- function(datos, titulo = "Serie de tiempo") {
  # stopifnot detiene el programa si su expresión no es TRUE. c crea el vector
  # con los nombres requeridos, %in% comprueba cada uno y all exige ambos.
  stopifnot(all(c("fecha", "y") %in% names(datos)))
  # attr recupera los metadatos añadidos por leer_serie. El operador personalizado
  # situado entre ambos valores usa el texto de la derecha cuando el atributo de
  # la izquierda es NULL.
  unidad <- attr(datos, "unidad") %||% "valor"
  fuente <- attr(datos, "fuente") %||% "no indicada"
  # Un gráfico ggplot2 se arma por capas unidas con +. aes(fecha, y) asigna las
  # columnas a los ejes; geom_line dibuja la línea; labs añade título, ejes y
  # caption; theme_minimal controla la apariencia. Como todo este objeto es la
  # última expresión de la función, R lo devuelve automáticamente.
  ggplot2::ggplot(datos, ggplot2::aes(fecha, y)) +
    ggplot2::geom_line(linewidth = 0.55, colour = "#2457A6") +
    ggplot2::labs(title = titulo, x = "Fecha", y = unidad,
                  # paste0 une textos sin separador y nrow cuenta las filas,
                  # que corresponden al número de observaciones de la serie.
                  caption = paste0("Fuente: ", fuente, "; n = ", nrow(datos))) +
    ggplot2::theme_minimal(base_size = 11)
}

# %||% significa: usa x si existe; si x es NULL, usa y como valor alternativo.
`%||%` <- function(x, y) if (is.null(x)) y else x

# El operador definido justo arriba es una función con nombre especial: si su
# primer operando es NULL devuelve el segundo; de lo contrario conserva el
# primero. A continuación se calcula manualmente la autocorrelación.
#
# acf_manual obtiene las autocorrelaciones de los rezagos 1,...,m. Se conserva
# un solo denominador para todos los rezagos, según la definición de la materia.
acf_manual <- function(y, m) {
  y <- validar_y(y)
  Tn <- length(y)
  # Restar un escalar a un vector es una operación vectorizada: R resta la media
  # a todas las observaciones. yc es la serie centrada y denom su suma de cuadrados.
  yc <- y - mean(y)
  denom <- sum(yc^2)
  # En una serie constante todas las desviaciones son cero. La autocorrelación
  # implicaría dividir por cero, por lo que se detiene con un mensaje claro.
  if (denom == 0) stop("La ACF no se define para una serie constante.", call. = FALSE)
  # seq_len(m) crea 1,...,m. vapply ejecuta la función anónima function(h) una
  # vez por rezago y exige que cada resultado sea un número, numeric(1).
  # Los corchetes seleccionan dos fragmentos desplazados de la serie; se
  # multiplican elemento a elemento, se suman y se dividen por denom.
  # DESCOMPOSICIÓN COMPLETA DE LA LÍNEA FUNDAMENTAL
  #
  # La autocorrelación del rezago h compara cada observación centrada con la
  # observación centrada que ocurrió h posiciones antes. Matemáticamente:
  #
  #                  suma desde t=h+1 hasta T de
  #                    (y_t - media)(y_{t-h} - media)
  #   r_h = -------------------------------------------------------------
  #                  suma desde t=1 hasta T de (y_t - media)^2
  #
  # En el código, yc ya significa y - media(y). Por esa razón la fórmula puede
  # escribirse usando solamente productos entre posiciones de yc.
  #
  # ¿POR QUÉ NO APARECE EL DIVISOR T?
  # La autocovarianza del numerador y la varianza del denominador se calculan con
  # el mismo divisor T. Al formar el cociente, los dos factores 1/T se cancelan:
  #
  #   [(1/T) * numerador] / [(1/T) * denominador]
  #       = numerador / denominador
  #
  # Por eso denom = sum(yc^2) es suficiente y permanece igual para todo h.
  #
  # 1. seq_len(m)
  #    Produce el vector 1,2,...,m. Cada número será un rezago diferente. Si
  #    m=4, la función calculará r_1, r_2, r_3 y r_4.
  #
  # 2. function(h)
  #    Define una función anónima, es decir, una función temporal sin nombre.
  #    vapply entrega a h primero el valor 1, después 2 y así hasta m.
  #
  # 3. yc[(h + 1L):Tn]
  #    Selecciona el fragmento tardío de la serie centrada:
  #
  #       yc[h+1], yc[h+2], ..., yc[T]
  #
  # 4. yc[1L:(Tn - h)]
  #    Selecciona el fragmento temprano con exactamente la misma longitud:
  #
  #       yc[1], yc[2], ..., yc[T-h]
  #
  #    Los dos fragmentos quedan alineados con una separación de h periodos.
  #
  # EJEMPLO: si T=6 y h=2, los fragmentos son:
  #
  #   fragmento tardío:  yc[3], yc[4], yc[5], yc[6]
  #   fragmento temprano: yc[1], yc[2], yc[3], yc[4]
  #
  # Los pares comparados son entonces:
  #   (yc[3],yc[1]), (yc[4],yc[2]), (yc[5],yc[3]), (yc[6],yc[4]).
  #
  # 5. El operador *
  #    Multiplica ambos vectores elemento a elemento. No es multiplicación
  #    matricial; produce un producto por cada pareja temporal.
  #
  # 6. sum(...)
  #    Suma todos los productos y obtiene el numerador de r_h. Un resultado
  #    positivo indica que valores separados h periodos tienden a desviarse de
  #    la media en la misma dirección; uno negativo indica direcciones opuestas.
  #
  # 7. / denom
  #    Normaliza el producto cruzado usando la variación total de la serie. Así
  #    el resultado queda expresado como autocorrelación y no como covarianza.
  #
  # 8. numeric(1)
  #    No es un dato del cálculo. Es la plantilla de tipo que vapply exige:
  #    declara que cada ejecución de function(h) debe devolver exactamente un
  #    número. Si una iteración devolviera varios valores, vapply produciría un
  #    error en lugar de construir silenciosamente una estructura inesperada.
  #
  # Finalmente, vapply reúne los m resultados en un vector numérico:
  #   c(r_1, r_2, ..., r_m).
  #
  # La letra L en 1L señala un entero de R. Aquí no modifica la fórmula; hace
  # explícito que los límites usados para indexar posiciones son enteros.
  vapply(seq_len(m), function(h) sum(yc[(h + 1L):Tn] * yc[1L:(Tn - h)]) / denom, numeric(1))
}

# Construye un panel de ACF y PACF. NULL representa la ausencia de un valor: si
# el usuario omite m, la función calcula automáticamente el máximo rezago.
correlograma <- function(datos, m = NULL) {
  # if también puede producir un valor. Si datos es una tabla, $ extrae la
  # columna y; si ya es un vector numérico se utiliza directamente.
  y <- if (is.data.frame(datos)) datos$y else datos
  y <- validar_y(y, 5L)
  Tn <- length(y)
  # floor redondea hacia abajo. min limita el máximo a 24 y la asignación solo
  # ocurre cuando is.null confirma que m no fue especificado.
  if (is.null(m)) m <- min(floor(Tn / 4), 24L)
  m <- as.integer(m)
  if (m < 1L || m >= Tn) stop("`m` debe estar entre 1 y T-1.", call. = FALSE)
  # ra contiene la ACF creada por nuestro algoritmo. La PACF sí se obtiene con
  # stats::pacf, permiso explícito de la tarea. $acf extrae el componente
  # correspondiente y as.numeric simplifica el arreglo a un vector.
  ra <- acf_manual(y, m)
  rp <- as.numeric(stats::pacf(y, lag.max = m, plot = FALSE)$acf)
  # qnorm(0.975) es aproximadamente 1.96. Al dividirlo por la raíz de T se
  # obtienen las bandas descriptivas de ruido blanco al 95 por ciento.
  limite <- stats::qnorm(0.975) / sqrt(Tn)
  # Esta tabla reúne el rezago y las dos correlaciones. Será la fuente de datos
  # común de ambos paneles.
  base <- tibble::tibble(lag = seq_len(m), ACF = ra, PACF = rp)
  # Función interna: solo existe durante la ejecución de correlograma. Recibe el
  # nombre de la columna que se dibujará y el título visible del panel.
  panel <- function(columna, nombre) {
    # .data[[columna]] permite seleccionar una columna cuyo nombre está guardado
    # como texto. Los dobles corchetes extraen exactamente una columna.
    ggplot2::ggplot(base, ggplot2::aes(x = lag, y = .data[[columna]])) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey45") +
      # Un vector de dos interceptos crea a la vez la banda inferior y superior.
      ggplot2::geom_hline(yintercept = c(-limite, limite), linetype = 2, colour = "#B33A3A") +
      # Cada segmento vertical parte de cero y termina en la correlación del rezago.
      ggplot2::geom_segment(ggplot2::aes(xend = lag, yend = 0), linewidth = 0.55, colour = "#2457A6") +
      ggplot2::labs(title = nombre, x = "Rezago", y = "Correlación") +
      ggplot2::theme_minimal(base_size = 10)
  }
  # patchwork interpreta panel1 / panel2 como una composición apilada: el primer
  # objeto queda arriba y el segundo abajo, tal como exige el enunciado (ACF
  # arriba, PACF abajo). El operador + apilaría uno al lado del otro y no cumple
  # ese orden.
  # grafico es la última expresión y, por ello, el valor devuelto.
  grafico <- panel("ACF", "ACF manual") / panel("PACF", "PACF")
  # Bloque de verificación: stats::acf no construye el resultado principal, sino
  # que contrasta la implementación manual. [-1L] elimina el rezago cero.
  estructura <- as.numeric(stats::acf(y, lag.max = m, plot = FALSE, demean = TRUE, type = "correlation")$acf)[-1L]
  # Se adjuntan datos y medidas como atributos del gráfico. No cambian la imagen,
  # pero pueden recuperarse después para verificar los cálculos.
  attr(grafico, "datos") <- base
  attr(grafico, "limite") <- limite
  attr(grafico, "max_diferencia_acf_stats") <- max(abs(ra - estructura))
  # grafico es la última expresión y, por ello, el valor devuelto.
  grafico
}
