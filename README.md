https://github.com/jgarro123/st-2026-2-tarea1-herramientas-garro-juan-david

# Qué contiene el repositorio

| Archivo o carpeta | Contenido | Dependencias |
|---|---|---|
| `R/00-lectura.R` | Lectura y validación de objetos `ts` o CSV | tibble |
| `R/01-graficos.R` | Serie, ACF manual, PACF y correlograma | ggplot2, patchwork |
| `R/02-metodos.R` | Ocho métodos manuales y optimización por rejilla | tibble |
| `R/03-evaluacion.R` | MSE, MAD, MAPE, MASE y diagnósticos | tibble |
| `ejemplos/ejemplos.R` | Ocho ejemplos, contraejemplo y verificaciones | paquetes permitidos |
| `informe/informe.qmd` | Fuente narrativa del informe | Quarto, solo si se desea renderizarla |
| `informe/informe.html` | Informe completo ya generado | navegador web |
| `figs/` | 28 figuras producidas por el guion | — |
| `resultados/resumen.csv` | Resumen numérico reproducible | — |
| `tests/pruebas.R` | Pruebas de contratos y equivalencias | paquetes permitidos |
| `sesion-info.txt` | Versión de R, plataforma y paquetes cargados | — |

# Cómo se corre

Desde la raíz del repositorio, en R 4.3 o superior:

```r
source("ejemplos/ejemplos.R")
```

Desde una terminal también se puede ejecutar:

```text
Rscript ejemplos/ejemplos.R
Rscript tests/pruebas.R
```

Ejecución observada en Windows con R 4.4.2: aproximadamente 13 segundos para los ocho ejemplos, 36 figuras, resultados e informe HTML. No se necesita Quarto para reproducir el HTML entregado.

# Cómo se usan las funciones

```r
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))
datos <- leer_serie(Nile, fuente = "R datasets::Nile; ayuda ?Nile",
                    unidad = "caudal anual (10^8 m³)")
graficar_serie(datos, "Caudal del Nilo")
correlograma(datos)
seleccion <- optimizar(datos$y, metodo = "ses")
seleccion$modelo$pronosticar(h = 6)
```

Cada método recibe un vector numérico, devuelve `yhat` con la misma longitud, una función `pronosticar(h)` y una lista nombrada `parametros`. Las observaciones de calentamiento aparecen como `NA`.

# Convenciones que fijan los números

- Media recursiva: `yhat[2] = y[1]`.
- Media móvil de orden `k`: el primer pronóstico válido está en `k + 1`.
- SES: nivel inicial `L[1] = y[1]` y `yhat[2] = y[1]`.
- Doble media móvil: primera doble media en `2k - 1` y primer pronóstico en `2k`.
- Holt: `L[1] = y[1]` y tendencia inicial igual a cero.
- Tendencias: coeficientes por ecuaciones normales; el modelo exponencial reporta mediana y aplica corrección `exp(sigma²/2)` cuando `corregir_sesgo = TRUE`.
- ACF muestral manual con divisor `T`, sin rezago cero y bandas `±qnorm(.975)/sqrt(n)`.
- PACF solo usa `stats::pacf` para graficar. `stats::acf` y `stats::Box.test` aparecen únicamente en verificaciones.
- Validación de origen fijo: últimos `h = min(12, floor(0.2T))`, ampliado a un ciclo si la frecuencia es estacional.
- MASE usa el referente ingenuo de un rezago para frecuencia 1 y el ingenuo estacional para frecuencia mayor que 1.
- Pruebas al 5%; Ljung–Box de errores usa `m - p` grados de libertad y el número efectivo de errores.

# Resumen de resultados

| Método | Serie | Parámetros | Referente | MASE método | MASE referente |
|---|---|---:|---|---:|---:|
| Media recursiva | discoveries | media=3.31 | Ingenuo | 0.919 | 1.136 |
| Media móvil | Nile | k=9 | Ingenuo | 0.825 | 0.835 |
| SES | LakeHuron | alpha=0.98 | Ingenuo | 2.141 | 2.164 |
| Doble media móvil | WWWusage | k=2 | Ingenuo | 1.368 | 7.625 |
| Tendencia lineal | airmiles | R²=0.881 | Ingenuo | 6.385 | 4.496 |
| Tendencia cuadrática | airmiles | R²=0.989 | Ingenuo | 1.205 | 4.496 |
| Tendencia exponencial | JohnsonJohnson | R²=0.973, sesgo corregido | Ingenuo estacional | 3.586 | 6.529 |
| Holt | LakeHuron | alpha=0.95, beta=0.05 | Ingenuo | 2.090 | 2.164 |

El informe interpreta también los diagnósticos: superar al referente no garantiza residuos blancos. El contraejemplo aplica la media a `sunspot.year` y muestra por métricas y Ljung–Box por qué un nivel constante no representa una serie cíclica.

# Declaración de uso de IA

Se utilizó asistencia de IA para convertir el enunciado y las notas de clase en una estructura de repositorio, elaborar una primera implementación de las funciones, proponer casos de prueba y redactar un borrador del informe. La respuesta recibida incluyó código, selección inicial de series, verificaciones y organización de resultados.

La verificación independiente incluyó: contraste de fórmulas con las clases 1–4; ejecución completa desde la raíz; pruebas de dimensiones y fallos claros; igualdad de la ACF manual con `stats::acf` a menos de `1e-12`; igualdad de Ljung–Box manual con `stats::Box.test`; comprobación de SES en forma ponderada; revisión de métricas sobre entrenamiento y validación separados. La selección exponencial se cambió después de comparar cuantitativamente varias series. El autor debe revisar el código, sustituir el enlace de GitHub, realizar cambios propios y ser capaz de explicar cada decisión antes de entregar.
