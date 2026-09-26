# ==============================================================================
# 00-lectura.R
# Lectura, validación y organización inicial de una serie de tiempo
# ==============================================================================
# En R, el símbolo <- asigna un valor a un nombre y la palabra function crea una
# función. Los argumentos van entre paréntesis y el procedimiento entre llaves.
#
# validar_y concentra las comprobaciones que necesitan todos los métodos. El
# argumento minimo = 2L tiene un valor predeterminado; la letra L indica que el
# 2 se almacena como entero y no como número decimal.
validar_y <- function(y, minimo = 2L) {
  # El operador ! niega una condición y || significa O lógico para decisiones
  # escalares. La condición completa pregunta si y no es numérico, si tiene
  # menos observaciones de las permitidas o si contiene NA, Inf o -Inf.
  # is.finite produce un TRUE/FALSE por elemento y any pregunta si al menos uno
  # de ellos cumple la condición. stop interrumpe la ejecución con un mensaje.
  if (!is.numeric(y) || length(y) < minimo || any(!is.finite(y))) {
    stop("`y` debe ser numérico, finito, ordenado y tener al menos ", minimo, " observaciones.", call. = FALSE)
  }
  # En R, la última expresión evaluada se devuelve automáticamente. Por eso no
  # es obligatorio escribir return. as.numeric elimina clases adicionales y
  # entrega un vector numérico simple a los algoritmos posteriores.
  as.numeric(y)
}

# Función auxiliar que convierte el eje temporal de un objeto ts en fechas Date.
# No suele llamarla directamente el usuario: leer_serie la utiliza cuando
# reconoce una serie temporal nativa de R.
fechas_ts <- function(x) {
  # paquete::funcion llama una función indicando explícitamente su paquete.
  # stats::time devuelve tiempos como 2000.00, 2000.08, etc. y frequency indica
  # cuántas observaciones tiene la serie por año.
  # LÓGICA GENERAL DE LA CONVERSIÓN
  # Un objeto ts no guarda fechas como "2000-03-01". R representa cada instante
  # mediante un número decimal. La parte entera identifica el año y la parte
  # decimal indica qué fracción del año había transcurrido antes de observar el
  # dato. La codificación general es:
  #
  #   tiempo = año + (posición_dentro_del_año - 1) / frecuencia
  #
  # Por ejemplo, en una serie mensual:
  #   enero de 2000   -> 2000 + 0/12 = 2000.0000
  #   febrero de 2000 -> 2000 + 1/12 = 2000.0833
  #   marzo de 2000   -> 2000 + 2/12 = 2000.1667
  #
  # Esta función invierte esa codificación: separa el año, recupera el mes y
  # construye una fecha. time(x) devuelve todos los tiempos decimales y
  # frequency(x) informa cuántas observaciones contiene cada año.
  tt <- stats::time(x)
  f <- stats::frequency(x)
  # El operador == compara valores; no debe confundirse con <-, que asigna.
  # Si f vale 12, la serie es mensual. El pequeño 1e-8 evita que errores de
  # representación decimal alteren un valor que debería ser un año exacto.
  # CASO MENSUAL: la estrategia tiene tres pasos.
  #   1. Extraer el año mediante floor.
  #   2. Convertir la fracción del año en el número del mes.
  #   3. Construir un texto año-mes-día y transformarlo a Date.
  #
  # El valor 1e-8 es una tolerancia contra errores de punto flotante. Si un año
  # que debería ser 2000 apareciera como 1999.999999999999, sumar una cantidad
  # diminuta evita que floor lo clasifique accidentalmente como 1999.
  if (f == 12) {
    anio <- floor(tt + 1e-8)
    # Las operaciones son vectorizadas: se aplican a todos los tiempos sin un
    # ciclo explícito. La parte decimal se convierte en los meses 1,...,12.
    # EJEMPLO CON MARZO DE 2000
    # tt vale aproximadamente 2000.1667 y anio vale 2000:
    #
    #   tt - anio          = 0.1667      fracción transcurrida del año
    #   (tt - anio) * 12   = 2           índice mensual usado por ts
    #   round(...) + 1     = 3           mes del calendario
    #
    # Se suma uno porque ts codifica enero con el índice 0, mientras que el
    # calendario numera enero como mes 1. De esta manera 0,...,11 se convierte
    # en 1,...,12. Como tt es un vector, R repite la operación en cada posición.
    mes <- round((tt - anio) * 12) + 1L
    # sprintf sustituye formatos: %04d crea un año de cuatro dígitos y %02d un
    # mes de dos. return termina inmediatamente la función con esas fechas.
    return(as.Date(sprintf("%04d-%02d-01", anio, mes)))
  }
  # Para frecuencia 4 cada trimestre se representa con el primer día de enero,
  # abril, julio u octubre. La fórmula 1 + 3*q transforma 0,1,2,3 en 1,4,7,10.
  # CASO TRIMESTRAL
  # Una serie trimestral usa estas fracciones del año:
  #   trimestre 1 -> 0/4 = 0.00
  #   trimestre 2 -> 1/4 = 0.25
  #   trimestre 3 -> 2/4 = 0.50
  #   trimestre 4 -> 3/4 = 0.75
  #
  # Multiplicar la fracción por 4 recupera q = 0,1,2,3. Cada trimestre comienza
  # tres meses después del anterior, por eso el mes se calcula como 1 + 3*q.
  if (f == 4) {
    anio <- floor(tt + 1e-8)
    # EJEMPLO CON EL TERCER TRIMESTRE DE 2000
    # tt = 2000.50, por lo tanto:
    #
    #   tt - anio            = 0.50
    #   round(0.50 * 4)      = 2
    #   1 + 3 * 2            = 7
    #
    # El mes 7 es julio, primer mes del tercer trimestre. Los cuatro resultados
    # posibles son enero, abril, julio y octubre: 1,4,7,10.
    mes <- 1L + 3L * round((tt - anio) * 4)
    # sprintf sustituye formatos: %04d crea un año de cuatro dígitos y %02d un
    # mes de dos. return termina inmediatamente la función con esas fechas.
    return(as.Date(sprintf("%04d-%02d-01", anio, mes)))
  }
  # Una frecuencia 1 es anual. Este if cabe en una línea porque solo ejecuta
  # una instrucción; return finaliza la función cuando se cumple la condición.
  # CASO ANUAL
  # Con una observación por año no hay que recuperar un mes. round(tt) deja el
  # año entero, sprintf agrega "-01-01" y se usa el primer día del año como fecha
  # representativa. return evita continuar hacia el caso de respaldo.
  if (f == 1) return(as.Date(sprintf("%04d-01-01", round(tt))))
  # Para frecuencias no previstas se crea un eje diario convencional. seq_along
  # produce 1,2,...,length(x); al restar 1 se obtienen desplazamientos desde 0.
  # CASO DE RESPALDO
  # Si f no es 12, 4 ni 1, el código no puede conocer el calendario verdadero.
  # Crea un eje diario artificial que preserva el orden: seq_along produce
  # 1,2,...,length(x); al restar 1 se obtienen desplazamientos 0,1,2,... desde
  # 1970-01-01. Estas fechas sirven para graficar, pero no representan las fechas
  # históricas reales. Al ser la última expresión, R la devuelve sin return.
  as.Date("1970-01-01") + seq_along(x) - 1L
}

# Función pública de lectura. Acepta un objeto ts o una cadena con la ruta de un
# CSV. fuente y unidad se guardan como metadatos para que las gráficas puedan
# declarar después la procedencia y el significado de los valores.
leer_serie <- function(x, fuente, unidad) {
  # missing detecta que un argumento no fue enviado. nzchar verifica que un
  # texto tenga caracteres. Basta que falte fuente o unidad para detenerse.
  if (missing(fuente) || !nzchar(fuente) || missing(unidad) || !nzchar(unidad)) {
    stop("`fuente` y `unidad` son textos obligatorios.", call. = FALSE)
  }
  # inherits pregunta si el objeto pertenece a la clase ts. Esta es la primera
  # rama posible del flujo: se valida el vector y se recupera su frecuencia.
  if (inherits(x, "ts")) {
    y <- validar_y(as.numeric(x))
    fecha <- fechas_ts(x)
    frecuencia <- stats::frequency(x)
  # else if se revisa únicamente si la rama anterior fue falsa. El operador &&
  # es Y lógico escalar: x debe ser texto, contener una sola ruta y existir.
  } else if (is.character(x) && length(x) == 1L && file.exists(x)) {
    # read.csv carga el archivo como data.frame. stringsAsFactors = FALSE evita
    # que las columnas de texto se conviertan automáticamente en factores.
    # LÓGICA DETALLADA DE ESTA LECTURA
    #
    # 1. En este punto, x no contiene los datos: contiene una cadena con la ruta
    #    del archivo, por ejemplo "datos/ventas.csv". read.csv abre esa ruta,
    #    interpreta la primera fila como nombres de columnas y las filas
    #    siguientes como observaciones. Por defecto espera valores separados por
    #    comas y trata de inferir el tipo de cada columna.
    #
    # 2. El resultado es un data.frame. Esta es la estructura tabular tradicional
    #    de R: cada columna es un vector, todas las columnas tienen la misma
    #    longitud y cada una puede tener un tipo distinto. Por ejemplo:
    #
    #        fecha        valor
    #        "2026-01-01" 120.5
    #        "2026-02-01" 123.8
    #
    #    podría quedar como una columna fecha de texto y una columna valor
    #    numérica. El objeto completo se guarda en z; después se accede a sus
    #    columnas con z$fecha y z$valor.
    #
    # 3. utils:: indica que read.csv pertenece al paquete utils. Esta sintaxis
    #    permite usar la función sin ejecutar previamente library(utils).
    #
    # 4. Una cadena de caracteres y un factor no representan lo mismo:
    #      - character conserva directamente textos como "enero" o "febrero";
    #      - factor representa categorías mediante niveles y códigos internos.
    #
    #    Por ejemplo, un factor con valores "bajo", "medio" y "alto" guarda esos
    #    textos como niveles categóricos. Esto es útil en modelos estadísticos,
    #    pero puede ser incómodo al limpiar fechas o modificar texto.
    #
    # 5. stringsAsFactors = FALSE solicita que las columnas textuales permanezcan
    #    como character. Así z$fecha puede convertirse directamente con as.Date
    #    sin que una conversión automática a factor introduzca pasos adicionales.
    #
    # En versiones modernas de R este comportamiento ya suele ser el
    # predeterminado. Escribirlo explícitamente documenta la intención y hace el
    # código más claro al ejecutarlo en instalaciones de distintas versiones.
    z <- utils::read.csv(x, stringsAsFactors = FALSE)
    # c combina valores en un vector. %in% pregunta si cada nombre requerido
    # aparece en names(z), y all exige que ambos resultados sean TRUE.
    if (!all(c("fecha", "valor") %in% names(z))) {
      stop("El CSV debe contener exactamente las columnas requeridas `fecha` y `valor`.", call. = FALSE)
    }
    # El operador $ extrae una columna por nombre. fecha se convierte a Date y
    # valor pasa por la misma validación empleada para una serie ts.
    fecha <- as.Date(z$fecha)
    y <- validar_y(z$valor)
    # anyNA encuentra fechas que no pudieron convertirse. is.unsorted con
    # strictly = TRUE también rechaza fechas repetidas o fuera de orden.
    if (anyNA(fecha) || is.unsorted(fecha, strictly = TRUE)) {
      stop("Las fechas deben ser válidas y estrictamente crecientes.", call. = FALSE)
    }
    # diff calcula fecha[2]-fecha[1], fecha[3]-fecha[2], etc. Después se compara
    # cada salto con el primero para comprobar que la serie sea equiespaciada.
    pasos <- as.numeric(diff(fecha))
    if (length(pasos) > 1L && any(pasos != pasos[1L])) {
      stop("La serie contiene fechas faltantes o no equiespaciadas.", call. = FALSE)
    }
    # Aquí if se usa como una expresión que produce un valor. Cuando hay pasos
    # de hasta 31 días se aproxima la frecuencia anual como 365.25/paso; si no,
    # la serie se considera anual. pasos[1L] selecciona el primer elemento.
    frecuencia <- if (length(pasos) && pasos[1L] <= 31) round(365.25 / pasos[1L]) else 1L
  } else {
    stop("`x` debe ser un objeto `ts` o la ruta de un CSV existente.", call. = FALSE)
  }
  # tibble construye una tabla moderna. Cada argumento nombrado se vuelve una
  # columna y seq_along(y) crea el índice entero 1,2,...,length(y).
  # out es la última expresión y, por tanto, el valor devuelto por leer_serie.
  out <- tibble::tibble(t = seq_along(y), fecha = fecha, y = y)
  # attr(objeto, nombre) <- valor adjunta metadatos sin crear nuevas columnas.
  # Así la tabla conserva solo t, fecha e y, pero recuerda frecuencia, fuente y
  # unidad para que otras funciones puedan recuperarlas.
  attr(out, "frecuencia") <- frecuencia
  attr(out, "fuente") <- fuente
  attr(out, "unidad") <- unidad
  # out es la última expresión y, por tanto, el valor devuelto por leer_serie.
  out
}
