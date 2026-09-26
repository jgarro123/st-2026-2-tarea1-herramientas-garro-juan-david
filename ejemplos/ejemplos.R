# ==============================================================================
# ejemplos.R
# Ejecución reproducible de los ocho ejemplos, verificaciones e informe HTML
# ==============================================================================
#
# Este archivo es el punto de entrada del proyecto. Al ejecutarlo desde la raíz,
# carga las funciones, analiza las ocho series, guarda las figuras y resultados,
# construye el informe y registra la información de la sesión de R.
#
# options modifica una preferencia global de la sesión. stringsAsFactors = FALSE
# evita que textos nuevos se transformen automáticamente en variables categóricas.
options(stringsAsFactors = FALSE, encoding = "UTF-8")
# R puede heredar en Windows una configuración C que degrada símbolos como ρ o
# el guion en Ljung–Box. El cambio es local a esta sesión y mantiene el informe
# generado en UTF-8 sin modificar la configuración regional del sistema.
locale_utf8 <- suppressWarnings(Sys.setlocale("LC_CTYPE", ".UTF-8"))
if (!nzchar(locale_utf8)) {
  warning("No fue posible activar una configuración UTF-8; revise los símbolos del informe.", call. = FALSE)
}
# dir.exists comprueba carpetas. Si se lanzó el archivo desde ejemplos/, no existe
# R/ en el directorio actual pero sí ../R; entonces setwd cambia a la raíz.
# && significa que ambas condiciones escalares deben ser verdaderas.
if (!dir.exists("R") && dir.exists("../R")) setwd("..")
# Segunda protección: si incluso después del ajuste no existe R/, se interrumpe
# la ejecución para no crear resultados en una ubicación equivocada.
if (!dir.exists("R")) stop("Ejecute este archivo desde la raíz del repositorio.", call. = FALSE)

# c combina los nombres de paquetes en un vector de texto.
paquetes <- c("dplyr", "tidyr", "purrr", "tibble", "ggplot2", "patchwork")
# vapply llama requireNamespace para cada nombre. logical(1) exige un TRUE/FALSE
# por paquete y quietly evita mensajes de carga. ! invierte el vector y los
# corchetes conservan únicamente los nombres que no están instalados.
faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
# En una condición, una longitud mayor que cero actúa como TRUE. paste con
# collapse une todos los paquetes faltantes en un solo mensaje separado por comas.
if (length(faltantes)) stop("Faltan paquetes permitidos: ", paste(faltantes, collapse = ", "), call. = FALSE)
# list.files busca archivos terminados en .R. lapply llama source sobre cada ruta
# para ejecutar sus definiciones. invisible evita imprimir la lista de resultados.
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))
# set.seed fija el estado del generador aleatorio. Aunque el análisis actual no
# depende de azar, esta línea mantiene reproducible cualquier extensión posterior.
set.seed(20260911)
# dir.create garantiza las carpetas de salida. recursive permite crear niveles
# intermedios y showWarnings = FALSE evita avisos si ya existen.
dir.create("figs", showWarnings = FALSE, recursive = TRUE)
dir.create("informe", showWarnings = FALSE, recursive = TRUE)

# LISTA DE CONFIGURACIONES
# list puede contener otras listas. Cada elemento describe un ejemplo mediante
# campos nombrados: id para archivos, nombre del método, serie, objeto real,
# unidad, patrón esperado y cantidad p de parámetros para Ljung-Box.
configuraciones <- list(
  # datasets::discoveries accede a la serie directamente desde el paquete base
  # datasets. La coma indica que continúan más argumentos de esta lista interna.
  list(id = "01-media", metodo = "Media recursiva", serie = "discoveries", objeto = datasets::discoveries,
       unidad = "número de descubrimientos", patron = "nivel aproximadamente estable", p = 1L),
  list(id = "02-mm", metodo = "Media móvil", serie = "Nile", objeto = datasets::Nile,
       unidad = "caudal anual (10^8 m³)", patron = "nivel local que cambia lentamente", p = 1L),
  list(id = "03-ses", metodo = "SES", serie = "LakeHuron", objeto = datasets::LakeHuron,
       unidad = "nivel en pies", patron = "nivel local sin estacionalidad", p = 1L),
  list(id = "04-dmm", metodo = "Doble media móvil", serie = "WWWusage", objeto = datasets::WWWusage,
       unidad = "usuarios conectados", patron = "nivel y tendencia local", p = 2L),
  list(id = "05-lineal", metodo = "Tendencia lineal", serie = "austres", objeto = datasets::austres,
       unidad = "miles de residentes", patron = "tendencia creciente aproximadamente lineal", p = 2L),
  list(id = "06-cuadratica", metodo = "Tendencia cuadrática", serie = "airmiles", objeto = datasets::airmiles,
       unidad = "millas-pasajero (millones)", patron = "curvatura en la tendencia", p = 3L),
  list(id = "07-exponencial", metodo = "Tendencia exponencial", serie = "JohnsonJohnson", objeto = datasets::JohnsonJohnson,
       unidad = "ganancias trimestrales por acción (USD)", patron = "crecimiento exponencial dominante con oscilación estacional", p = 2L),
  list(id = "08-holt", metodo = "Holt", serie = "LakeHuron", objeto = datasets::LakeHuron,
       unidad = "nivel en pies", patron = "nivel y pendiente local", p = 2L)
)

# DESPACHO DEL MÉTODO
# Esta función conecta cada configuración con la función que realmente ajusta su
# modelo. cfg es una de las listas creadas en configuraciones; por ejemplo, puede
# contener metodo = "Media móvil" y objeto = datasets::Nile. El argumento y no es
# el objeto ts completo, sino el vector numérico del tramo de estimación sobre el
# cual se ajustará el método.
#
# La función siempre intenta producir una estructura uniforme con dos componentes:
#   modelo: el resultado de ajustar el método, con yhat, pronosticar y parametros;
#   optimizacion: evidencia de la búsqueda del parámetro, o NULL si no hubo rejilla.
# Esta estructura común permite que el código posterior trate de la misma manera
# todos los ejemplos, aunque internamente cada método se estime de forma diferente.
ajustar_config <- function(cfg, y) {
  # cfg$metodo usa $ para extraer de la lista cfg el texto guardado bajo el nombre
  # metodo. switch usa ese texto como selector y busca una alternativa cuyo nombre
  # coincida exactamente, incluidos mayúsculas, espacios y tildes. Por ejemplo, si
  # cfg$metodo vale "SES", solo se evalúa la alternativa llamada "SES": las otras
  # siete ramas no se ejecutan. Esta selección equivale a una cadena de if/else if,
  # pero resulta más legible cuando hay varias opciones identificadas por nombres.
  switch(cfg$metodo,
    # La media recursiva no tiene una ventana, alpha ni beta que deban buscarse en
    # una rejilla. Se ajusta directamente con y. optimizacion = NULL comunica que
    # para este método no existe un objeto con resultados de búsqueda.
    "Media recursiva" = list(modelo = ajustar_media(y), optimizacion = NULL),

    # Las llaves { } crean una rama con más de una instrucción. Primero optimizar()
    # prueba k = 2,...,12 para la media móvil y guarda en o la rejilla evaluada, sus
    # MSE, el valor óptimo y el modelo ya reajustado con ese k. Luego se construye
    # la salida uniforme: o$modelo extrae solamente el modelo elegido, mientras que
    # optimizacion = o conserva también todo el proceso que justificó la elección.
    # El punto y coma separa dos instrucciones escritas en la misma línea; cumple la
    # misma función que escribirlas en dos líneas independientes.
    "Media móvil" = { o <- optimizar(y, "mm"); list(modelo = o$modelo, optimizacion = o) },

    # Para SES se repite la misma lógica, pero "ses" le indica a optimizar() que la
    # rejilla corresponde a alpha. Solo se ejecuta esta búsqueda si cfg$metodo es
    # exactamente "SES".
    "SES" = { o <- optimizar(y, "ses"); list(modelo = o$modelo, optimizacion = o) },

    # En la doble media móvil, "dmm" selecciona la búsqueda de la ventana k propia
    # de ese método. De nuevo se entregan por separado el modelo óptimo y el objeto
    # completo de optimización que permite graficar o inspeccionar la rejilla.
    "Doble media móvil" = { o <- optimizar(y, "dmm"); list(modelo = o$modelo, optimizacion = o) },

    # Las tendencias lineal y cuadrática se estiman directamente mediante las
    # ecuaciones normales. Las cadenas "lineal" y "cuadratica" le dicen a
    # ajustar_tendencia() qué columnas debe construir en la matriz de diseño X.
    # Como aquí no se explora una rejilla, optimizacion vuelve a ser NULL.
    "Tendencia lineal" = list(modelo = ajustar_tendencia(y, "lineal"), optimizacion = NULL),
    "Tendencia cuadrática" = list(modelo = ajustar_tendencia(y, "cuadratica"), optimizacion = NULL),

    # La tendencia exponencial también se estima directamente, pero sobre log(y).
    # corregir_sesgo = TRUE solicita multiplicar el resultado al volver a la escala
    # original por el factor exp(sigma^2/2), para corregir el sesgo de transformar
    # un pronóstico desde la escala logarítmica.
    "Tendencia exponencial" = list(modelo = ajustar_tendencia(y, "exponencial", corregir_sesgo = TRUE), optimizacion = NULL),

    # Holt sí requiere optimización: "holt" hace que se evalúen combinaciones de
    # alpha y beta. o$modelo contiene el ajuste con la pareja que minimizó el MSE y
    # o conserva el mapa completo de la búsqueda sobre ambas constantes.
    "Holt" = { o <- optimizar(y, "holt"); list(modelo = o$modelo, optimizacion = o) }
  )

  # No hace falta escribir return(). En R, una función devuelve automáticamente el
  # valor de su última expresión evaluada. Aquí esa expresión es switch(), por lo
  # cual ajustar_config() devuelve la lista creada por la rama seleccionada.
}
# Esta función recibe el objeto devuelto por uno de los ocho métodos y construye
# una frase corta que resume sus parámetros. Esa frase se usará después en tablas
# y encabezados del informe; por eso la salida siempre es un único texto, como
# "k=4", "alpha=0.36; beta=0.15" o "lineal; R2=0.927".
#
# Los métodos no guardan exactamente los mismos campos dentro de parametros:
#   - media móvil y doble media móvil guardan k;
#   - SES guarda alpha;
#   - Holt guarda alpha y beta;
#   - las tendencias guardan tipo y R2;
#   - la media recursiva guarda media.
# La función reconoce de qué clase de modelo se trata preguntando qué campos
# existen. No vuelve a estimar nada ni modifica el modelo: solo prepara un rótulo.
parametros_breves <- function(modelo) {
  # modelo es una lista y $parametros selecciona su componente llamado parametros.
  # Ese componente también es una lista. Se guarda temporalmente bajo el nombre p
  # para escribir p$k, p$alpha, etc., en vez de repetir modelo$parametros cada vez.
  p <- modelo$parametros

  # is.null(p$k) pregunta si el campo k está ausente. El operador ! niega el
  # resultado; por tanto, la condición se cumple cuando k sí existe. paste0 une
  # "k=" con su valor sin agregar espacios. return entrega el texto y termina la
  # función de inmediato, evitando evaluar las condiciones que aparecen después.
  # Esta rama reconoce tanto la media móvil como la doble media móvil.
  if (!is.null(p$k)) return(paste0("k=", p$k))

  # && significa "y" lógico: esta rama solo se cumple cuando existen alpha Y beta.
  # Debe aparecer antes de la rama que pregunta solamente por alpha; de lo contrario,
  # un modelo Holt entraría primero en la condición general de alpha y se perdería
  # beta. sprintf introduce los valores en la plantilla: cada %.2f significa
  # "escriba aquí un número decimal con exactamente dos cifras después del punto".
  # Por ejemplo, alpha=0.3 y beta=0.125 se convierten en
  # "alpha=0.30; beta=0.12". Esta es la rama específica de Holt.
  if (!is.null(p$alpha) && !is.null(p$beta)) return(sprintf("alpha=%.2f; beta=%.2f", p$alpha, p$beta))

  # Si existe alpha pero no se ejecutó la rama anterior, el modelo no tiene beta;
  # corresponde entonces a SES. Se devuelve, por ejemplo, "alpha=0.36".
  if (!is.null(p$alpha)) return(sprintf("alpha=%.2f", p$alpha))

  # Las tendencias guardan un texto en tipo (lineal, cuadratica o exponencial) y
  # una medida de ajuste en R2. Primero se formatea R2 con tres decimales mediante
  # sprintf("%.3f", ...); después paste0 lo concatena con el tipo y el rótulo
  # "; R2=". Un posible resultado es "lineal; R2=0.927".
  if (!is.null(p$tipo)) return(paste0(p$tipo, "; R2=", sprintf("%.3f", p$R2)))

  # Si ninguna condición anterior se cumplió, se asume que es la media recursiva,
  # cuyo campo distintivo es media. Esta es la última expresión de la función y
  # R la devuelve automáticamente, aunque no se escriba return(). El valor se
  # presenta con dos decimales, por ejemplo "media=37.42".
  paste0("media=", sprintf("%.2f", p$media))
}
# Dibuja la evidencia de la búsqueda por rejilla. o es NULL en métodos sin
# parámetros optimizados; en ese caso se devuelve un texto faltante NA_character_.
guardar_optimizacion <- function(o, id) {
  # NA_character_ es un faltante de tipo texto, apropiado porque normalmente esta
  # función devuelve la ruta textual de una imagen.
  if (is.null(o)) return(NA_character_)
  # Si la rejilla contiene alpha y beta, corresponde a Holt y se necesita un mapa
  # bidimensional. all exige que ambos nombres aparezcan.
  if (all(c("alpha", "beta") %in% names(o$rejilla))) {
    # geom_tile dibuja una celda por combinación; fill representa el MSE mediante
    # color. El punto con forma 4 marca la combinación óptima.
    g <- ggplot2::ggplot(o$rejilla, ggplot2::aes(alpha, beta, fill = MSE)) +
      ggplot2::geom_tile() + ggplot2::scale_fill_viridis_c() +
      ggplot2::geom_point(data = o$optimo, shape = 4, size = 4, stroke = 1.2) +
      ggplot2::theme_minimal() + ggplot2::labs(title = "Rejilla de Holt")
    # Los demás métodos optimizan un solo parámetro y se muestran como una curva.
  } else {
    # setdiff elimina el nombre MSE y [1] toma la columna restante como eje x.
    eje <- setdiff(names(o$rejilla), "MSE")[1]
    # .data[[eje]] permite que ggplot elija una columna a partir de un texto.
    g <- ggplot2::ggplot(o$rejilla, ggplot2::aes(x = .data[[eje]], y = MSE)) +
      ggplot2::geom_line(colour = "#2457A6") + ggplot2::geom_point(size = 1.2) +
      ggplot2::geom_point(data = o$optimo, colour = "#B33A3A", size = 3) +
      ggplot2::theme_minimal() + ggplot2::labs(title = "Error de entrenamiento en la rejilla")
  }
  # file.path construye rutas con el separador correcto del sistema operativo.
  ruta <- file.path("figs", paste0(id, "-optimizacion.png"))
  # ggsave renderiza el objeto g. width y height están en pulgadas y dpi controla
  # la densidad de píxeles. La última expresión ruta se devuelve al llamador.
  ggplot2::ggsave(ruta, g, width = 6.8, height = 4.2, dpi = 130)
  ruta
}

# PROTOCOLO COMPLETO PARA UN EJEMPLO
# La función recibe una configuración y devuelve una lista con datos, métricas,
# diagnósticos y rutas. Así se reutiliza exactamente el mismo flujo ocho veces.
analizar_ejemplo <- function(cfg) {
  # Se transforma el objeto ts en tibble y se construye un texto de procedencia.
  # Cada acceso cfg$nombre recupera un campo de la configuración.
  datos <- leer_serie(cfg$objeto, paste0("R datasets::", cfg$serie, " (ayuda ?", cfg$serie, ")"), cfg$unidad)
  # Se crean abreviaciones: y para valores, Tn para tamaño y f para frecuencia.
  y <- datos$y; Tn <- length(y); f <- attr(datos, "frecuencia")
  # h es el menor entre 12 y el 20 por ciento de T. Si la serie es estacional,
  # max garantiza al menos un ciclo completo de validación.
  h <- min(12L, floor(0.20 * Tn)); if (f > 1L) h <- max(h, f)
  # La indexación divide cronológicamente la serie: train usa el inicio y valid
  # las últimas h observaciones. No se mezclan ni se reordenan datos.
  n_train <- Tn - h; train <- y[seq_len(n_train)]; valid <- y[(n_train + 1L):Tn]
  # El modelo se selecciona y estima exclusivamente con entrenamiento. Después
  # se extrae el componente modelo de la lista devuelta.
  ajuste <- ajustar_config(cfg, train); modelo <- ajuste$modelo
  # pronosticar es una función guardada dentro del modelo; se invoca con (h).
  pron <- modelo$pronosticar(h)
  # if produce el nombre y el cálculo del referente según la frecuencia.
  referente <- if (f > 1L) "Ingenuo estacional" else "Ingenuo"
  # Para el ingenuo estacional se repite el último ciclo hasta completar h. En
  # frecuencia 1 se repite únicamente la última observación.
  ref_pron <- if (f > 1L) rep(tail(train, f), length.out = h) else rep(tail(train, 1), h)
  # Se calculan las mismas métricas para pronósticos de un paso, validación del
  # método y validación del referente, siempre con la escala del entrenamiento.
  met_train <- medidas(train, modelo$yhat, train, f)
  met_valid <- medidas(valid, pron, train, f)
  met_ref <- medidas(valid, ref_pron, train, f)
  # Se forman errores y se quitan los NA de calentamiento con indexación lógica.
  errores <- train - modelo$yhat; errores <- errores[is.finite(errores)]
  # Número de rezagos de diagnóstico basado en la cantidad real de errores.
  m_err <- min(floor(length(errores) / 4), 24L)
  # cfg$p ajusta los grados de libertad de Ljung-Box por parámetros estimados.
  diag <- validar_errores(errores, cfg$p, m_err)
  # Diagnóstico de la serie de entrenamiento antes de evaluar el método.
  m_serie <- min(floor(n_train / 4), 24L)
  lb_serie <- ljung_box(acf_manual(train, m_serie), n_train, m_serie, 0L)

  # paste une las piezas con espacios por defecto. Se grafica la serie y se guarda
  # con un nombre derivado del id para evitar colisiones entre ejemplos.
  g1 <- graficar_serie(datos, paste(cfg$serie, "—", cfg$patron))
  ruta_serie <- file.path("figs", paste0(cfg$id, "-serie.png"))
  ggplot2::ggsave(ruta_serie, g1, width = 7.2, height = 4.2, dpi = 130)
  # El correlograma trae como atributo la diferencia contra stats::acf.
  cg <- correlograma(datos, m_serie)
  # stopifnot convierte la tolerancia numérica en una comprobación ejecutable:
  # si la diferencia no es menor que 10^-12, el guion se detiene.
  stopifnot(attr(cg, "max_diferencia_acf_stats") < 1e-12)
  ruta_corr <- file.path("figs", paste0(cfg$id, "-correlograma.png"))
  ggplot2::ggsave(ruta_corr, cg, width = 7.4, height = 6.4, dpi = 130)
  # Se prepara una tabla en formato largo para superponer observado, ajuste,
  # pronóstico del método y referente en una sola gráfica.
  fechas <- datos$fecha
  # bind_rows apila tablas con las mismas columnas. tipo identifica la serie que
  # luego controlará color y tipo de línea en ggplot.
  df_final <- dplyr::bind_rows(
    tibble::tibble(fecha = fechas, valor = y, tipo = "Observado"),
    tibble::tibble(fecha = fechas[seq_len(n_train)], valor = modelo$yhat, tipo = "Ajuste un paso"),
    tibble::tibble(fecha = fechas[(n_train + 1L):Tn], valor = pron, tipo = "Pronóstico"),
    tibble::tibble(fecha = fechas[(n_train + 1L):Tn], valor = ref_pron, tipo = "Ingenuo")
  )
  # Dentro de aes, colour y linetype se asignan a la columna tipo; ggplot crea
  # automáticamente una leyenda. na.rm omite calentamientos sin lanzar avisos.
  gf <- ggplot2::ggplot(df_final, ggplot2::aes(fecha, valor, colour = tipo, linetype = tipo)) +
    ggplot2::geom_line(linewidth = 0.65, na.rm = TRUE) +
    # La línea vertical marca el último instante de entrenamiento. ggplot necesita
    # la representación numérica interna de Date para xintercept.
    ggplot2::geom_vline(xintercept = as.numeric(fechas[n_train]), linetype = 3) +
    ggplot2::theme_minimal() + ggplot2::labs(title = paste(cfg$metodo, "en", cfg$serie), y = cfg$unidad, x = "Fecha")
  ruta_final <- file.path("figs", paste0(cfg$id, "-pronostico.png"))
  ggplot2::ggsave(ruta_final, gf, width = 7.2, height = 4.2, dpi = 130)
  # de reúne errores e índice efectivo para dibujarlos en orden temporal.
  de <- tibble::tibble(t = seq_along(errores), error = errores)
  # Primer panel de diagnóstico: trayectoria de errores alrededor de cero.
  ge1 <- ggplot2::ggplot(de, ggplot2::aes(t, error)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey55") +
    ggplot2::geom_line(colour = "#2457A6", linewidth = 0.5) +
    ggplot2::theme_minimal() + ggplot2::labs(title = "Errores de un paso", x = "Índice efectivo", y = "Error")
  # correlograma() se reutiliza sobre los errores, tal como exige el enunciado
  # ("la misma función sirve para la serie y para los errores de un método"): aquí
  # T pasa a ser el número de errores efectivos, no el de observaciones de la serie,
  # y el panel resultante ya trae ACF arriba y PACF abajo con su propia verificación.
  cg_errores <- correlograma(errores, m_err)
  stopifnot(attr(cg_errores, "max_diferencia_acf_stats") < 1e-12)
  ruta_errores <- file.path("figs", paste0(cfg$id, "-errores.png"))
  # Los errores en el tiempo quedan arriba y el panel ACF/PACF de correlograma()
  # queda abajo, apilados con /.
  ggplot2::ggsave(ruta_errores, ge1 / cg_errores, width = 8.2, height = 9.4, dpi = 130)
  # Para métodos sin optimización se recibe NA; para los demás, la ruta de figura.
  ruta_opt <- guardar_optimizacion(ajuste$optimizacion, cfg$id)

  # La lista final funciona como expediente completo del ejemplo. Combina objetos
  # de clases diferentes: números, textos, tablas, pruebas y rutas de archivos.
  list(cfg = cfg, datos = datos, T = Tn, f = f, h = h, n_train = n_train,
       inicio = as.character(min(datos$fecha)), fin = as.character(max(datos$fecha)),
       parametros = parametros_breves(modelo), coeficientes = modelo$parametros$coeficientes %||% NULL,
       referente = referente, met_train = met_train,
       met_valid = met_valid, met_ref = met_ref, diagnostico = diag,
       lb_serie = lb_serie, acf_diferencia = attr(cg, "max_diferencia_acf_stats"),
       figuras = c(serie = ruta_serie, correlograma = ruta_corr, pronostico = ruta_final,
                   errores = ruta_errores, optimizacion = ruta_opt),
       usa_rejilla = !is.null(ajuste$optimizacion),
       # Esta expresión decide si algún parámetro óptimo coincide con un extremo.
       # setdiff elimina MSE, lapply calcula range por columna, unlist aplana las
       # listas y %in% comprueba si los óptimos pertenecen a esos límites.
       borde = if (!is.null(ajuste$optimizacion)) {
         op <- ajuste$optimizacion$optimo; any(unlist(op[setdiff(names(op), "MSE")]) %in%
           unlist(lapply(ajuste$optimizacion$rejilla[setdiff(names(op), "MSE")], range)))
       } else FALSE)
}

# lapply ejecuta analizar_ejemplo sobre cada una de las ocho configuraciones y
# devuelve una lista de ocho resultados. Esta línea dispara el análisis completo.
resultados <- lapply(configuraciones, analizar_ejemplo)

# ==============================================================================
# VERIFICACIONES NUMÉRICAS
# Se comparan fórmulas propias con expresiones equivalentes o funciones de R.
# Estas verificaciones no sustituyen los métodos; sirven para detectar errores.
# ==============================================================================
# Verificaciones numéricas permitidas por el enunciado.
# [1:30] selecciona las primeras treinta observaciones para una prueba pequeña.
z <- as.numeric(datasets::Nile)[1:30]
ses_v <- ajustar_ses(z, 0.34)
# Función de una sola expresión que escribe SES como promedio ponderado. rev
# invierte el pasado para alinear el peso mayor con la observación más reciente.
pesos <- function(t) 0.34 * sum((1 - 0.34)^(0:(t - 2)) * rev(z[1:(t - 1)])) + (1 - 0.34)^(t - 1) * z[1]
# Se compara cada pronóstico recursivo con la forma ponderada. abs toma diferencias
# absolutas, max la peor y stopifnot exige una tolerancia menor que 10^-10.
stopifnot(max(abs(ses_v$yhat[2:length(z)] - vapply(2:length(z), pesos, numeric(1)))) < 1e-10)
# na.omit elimina la primera posición de calentamiento antes de Ljung-Box.
e_ver <- na.omit(z - ses_v$yhat)
m_ver <- min(6L, floor(length(e_ver) / 4))
lb_m <- ljung_box(acf_manual(e_ver, m_ver), length(e_ver), m_ver, 0)
# Box.test se usa solo como referencia. unname elimina el nombre automático del
# estadístico antes de compararlo con la implementación manual.
lb_r <- stats::Box.test(e_ver, lag = m_ver, type = "Ljung-Box", fitdf = 0)
stopifnot(abs(lb_m$estadistico - unname(lb_r$statistic)) < 1e-10)

# HOLT: se verifica numéricamente la forma de corrección de error de la
# Proposición correspondiente: L_t = L_{t-1} + That_{t-1} + alpha*e_t y
# That_t = That_{t-1} + alpha*beta*e_t, con e_t = y_t - yhat_t.
holt_v <- ajustar_holt(z, 0.3, 0.2)
e_holt <- z - holt_v$yhat
L <- holt_v$parametros$nivel; Th <- holt_v$parametros$tendencia
idx <- 2:length(z)
stopifnot(max(abs(L[idx] - (L[idx - 1L] + Th[idx - 1L] + 0.3 * e_holt[idx]))) < 1e-10)
stopifnot(max(abs(Th[idx] - (Th[idx - 1L] + 0.3 * 0.2 * e_holt[idx]))) < 1e-10)

# TENDENCIAS: los coeficientes y sus errores estándar ordinarios se contrastan
# contra lm(), permitido únicamente en este bloque de verificación y nunca
# dentro de los métodos (instrucción 3 del enunciado).
tt_v <- seq_along(z)
lin_v <- ajustar_tendencia(z, "lineal"); lin_r <- stats::lm(z ~ tt_v)
stopifnot(max(abs(lin_v$parametros$coeficientes$estimacion - unname(coef(lin_r)))) < 1e-8)
stopifnot(max(abs(lin_v$parametros$coeficientes$se_ordinario - unname(summary(lin_r)$coefficients[, 2]))) < 1e-8)

cuad_v <- ajustar_tendencia(z, "cuadratica"); cuad_r <- stats::lm(z ~ tt_v + I(tt_v^2))
stopifnot(max(abs(cuad_v$parametros$coeficientes$estimacion - unname(coef(cuad_r)))) < 1e-8)
stopifnot(max(abs(cuad_v$parametros$coeficientes$se_ordinario - unname(summary(cuad_r)$coefficients[, 2]))) < 1e-8)

exp_v <- ajustar_tendencia(z, "exponencial"); exp_r <- stats::lm(log(z) ~ tt_v)
stopifnot(max(abs(exp_v$parametros$coeficientes$estimacion - unname(coef(exp_r)))) < 1e-8)
stopifnot(max(abs(exp_v$parametros$coeficientes$se_ordinario - unname(summary(exp_r)$coefficients[, 2]))) < 1e-8)

# El contraejemplo (media recursiva sobre sunspot.year) se construye dentro de
# informe.qmd con analizar_ejemplo(), el mismo protocolo que los ocho ejemplos;
# no se duplica aquí.

# Para cada resultado, una función anónima construye una fila de resumen. lapply
# produce una lista de filas y bind_rows las apila en una tabla de ocho filas.
resumen <- dplyr::bind_rows(lapply(resultados, function(x) tibble::tibble(
  metodo = x$cfg$metodo, serie = x$cfg$serie, parametros = x$parametros,
  h = x$h, referente = x$referente, MASE_metodo = x$met_valid$MASE, MASE_referencia = x$met_ref$MASE,
  p_LB_errores = x$diagnostico$ljung_box$p_valor
)))
# resumen queda disponible en memoria para informe.qmd (tabla "Resumen de los
# ocho ejemplos") y para README.md; no se escribe a disco porque resultados/ no
# forma parte de la estructura exigida por el enunciado.

# ==============================================================================
# CIERRE
# El informe entregable (informe/informe.html) es el resultado de renderizar
# informe/informe.qmd con Quarto; este guion no genera HTML por su cuenta para
# no sobreescribir esa salida cuando se ejecuta ejemplos.R de forma aislada,
# como hace el profesor al calificar (instrucción 2 del enunciado).
# ==============================================================================
# capture.output convierte lo que imprimiría sessionInfo en un vector de texto,
# que se guarda para documentar versión de R, plataforma y paquetes.
writeLines(capture.output(sessionInfo()), "sesion-info.txt", useBytes = TRUE)
# message informa éxito al usuario sin convertirse en parte del valor calculado.
message("Ejecución completa: 8 ejemplos, figuras y resultados generados.")
