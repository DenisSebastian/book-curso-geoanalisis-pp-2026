# ==============================================================================
# INDICADORES SOCIOECONOMICOS TERRITORIALES - CENSO 2024
# ==============================================================================
#
# Guion de clase basado en el capitulo "Indicadores Territoriales" del libro
# digital del curso. La meta es mostrar, paso a paso, como una pregunta
# territorial se transforma en variables, formulas, indicadores y mapas.
#
# Area de estudio:
#   - zonas urbanas de Coquimbo y La Serena;
#   - Region de Coquimbo (codigo 04);
#   - Censo de Poblacion y Vivienda 2024.
#
# Indicadores calculados:
#   IEM: empleo;
#   IVI: materialidad de la vivienda;
#   ISV: proxy de suficiencia de la vivienda;
#   IEJ: proxy de escolaridad adulta;
#   IRH: proxy de estructura de hogares.
#
# Todos se orientan de la misma manera: un valor alto representa una condicion
# relativamente mas favorable. ISV, IEJ e IRH son aproximaciones indirectas y
# deben interpretarse junto con los supuestos descritos en el libro.
#
# Uso sugerido en RStudio:
#   1. Abrir el proyecto del libro.
#   2. Abrir este archivo.
#   3. Ejecutar las lineas seleccionadas con Cmd/Ctrl + Enter, o usar
#      Code > Run Region > Run Section para avanzar por secciones.
#   4. Detenerse en las preguntas marcadas como "PARA DISCUTIR".
#
# Importante: el archivo supone que el directorio de trabajo es la raiz del
# proyecto cgeoa-2026-book. Compruebelo con getwd().
# ==============================================================================


# 0. Preparacion ---------------------------------------------------------------

# Paquetes utilizados en el capitulo. Esta comprobacion entrega un mensaje mas
# claro que library() cuando falta alguna dependencia.
paquetes <- c("dplyr", "sf", "tidyr", "ggplot2")
paquetes_faltantes <- paquetes[!vapply(
  paquetes,
  requireNamespace,
  quietly = TRUE,
  FUN.VALUE = logical(1)
)]

if (length(paquetes_faltantes) > 0) {
  stop(
    "Faltan paquetes. Instálelos antes de continuar: ",
    paste(paquetes_faltantes, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(ggplot2))

# Parametros del ejercicio. Dejarlos al inicio permite cambiar el territorio
# sin buscar valores escondidos en distintas partes del codigo.
reg <- "04"
comunas_estudio <- c("COQUIMBO", "LA SERENA")
area_estudio <- "URBANO"

# Ruta utilizada por el libro digital. Si el archivo no aparece, revisar getwd()
# y confirmar que se abrio el proyecto correcto.
path_censo_2024 <- "../sesiones/data/censo-2024/censo_2024_nacional.gpkg"

if (!file.exists(path_censo_2024)) {
  stop(
    "No se encontro el insumo 2024 en: ", path_censo_2024, "\n",
    "Directorio de trabajo actual: ", getwd(),
    call. = FALSE
  )
}


# 1. Lectura y seleccion del area de estudio ----------------------------------

# st_read() carga tanto la tabla de atributos como la geometria. quiet = TRUE
# evita imprimir informacion tecnica extensa mientras se desarrolla la clase.
data_nac <- st_read(path_censo_2024, quiet = TRUE)

# Un indicador territorial siempre necesita declarar su universo geografico.
# Aqui seleccionamos una region, dos comunas y solo sus zonas urbanas.
data_reg <- data_nac %>%
  filter(
    COD_REGION == reg,
    COMUNA %in% comunas_estudio,
    AREA_C == area_estudio
  )

if (nrow(data_reg) == 0) {
  stop(
    "No se encontraron zonas urbanas de Coquimbo y La Serena en el insumo.",
    call. = FALSE
  )
}

cat("Geometrias seleccionadas:", nrow(data_reg), "\n")

# Al quitar temporalmente la geometria, las operaciones tabulares son mas
# faciles de leer. count() permite revisar la cobertura por comuna.
conteo_por_comuna <- data_reg %>%
  st_drop_geometry() %>%
  count(COMUNA, name = "n_geometrias", sort = TRUE)

print(conteo_por_comuna)

# PARA DISCUTIR:
# ¿Que cambiaria en la interpretacion si se incluyeran areas rurales?
# ¿Seria correcto comparar resultados comunales con resultados por manzana?


# 2. Funciones auxiliares ------------------------------------------------------

# Comprueba que una tabla contenga todas las variables necesarias. Es preferible
# detenerse aqui con un mensaje informativo que fallar mas adelante en mutate().
assert_has_vars <- function(datos, variables, origen = "datos") {
  faltantes <- setdiff(variables, names(datos))

  if (length(faltantes) > 0) {
    stop(
      "Faltan variables en ", origen, ": ",
      paste(faltantes, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# Una proporcion solo esta definida cuando el denominador existe y es positivo.
# if_else() mantiene un resultado numerico y devuelve NA cuando no es posible
# calcular la razon. NA significa "no calculable", no significa cero.
proporcion_segura <- function(numerador, denominador) {
  if_else(
    !is.na(denominador) & denominador > 0,
    numerador / denominador,
    NA_real_
  )
}

# Restringe resultados al intervalo [0, 1]. Se utiliza para controlar pequeñas
# inconsistencias que pueden aparecer en datos agregados.
acotar_01 <- function(x) {
  pmin(pmax(x, 0), 1)
}

# Normalizacion min-max:
#                  x - minimo(x)
#   x_normalizado = -------------------
#                  maximo(x) - minimo(x)
#
# El minimo pasa a 0 y el maximo a 1. Si no existe variacion, la normalizacion
# no esta definida y se devuelve NA para todas las observaciones.
minmax <- function(x) {
  rango <- range(x, na.rm = TRUE)

  if (!all(is.finite(rango)) || diff(rango) == 0) {
    return(rep(NA_real_, length(x)))
  }

  (x - rango[1]) / diff(rango)
}


# 3. Validacion y aplicacion del ponderador geometrico w -----------------------

# Estas variables son conteos absolutos. Cuando una unidad censal fue separada
# en varias geometrias, cada conteo se reparte multiplicandolo por w.
vars_conteo <- c(
  "n_per",
  "n_vp",
  "n_desocupado",
  "n_ocupado",
  "n_mat_paredes_tabique_sin_forro",
  "n_mat_paredes_artesanal",
  "n_mat_paredes_precarios",
  "n_mat_techo_fonolita",
  "n_mat_techo_precarios",
  "n_mat_techo_sin_cubierta",
  "n_mat_piso_capa_cemento",
  "n_mat_piso_tierra",
  "n_viv_hacinadas",
  "n_vp_ocupada",
  "n_nucleos_hacinados_allegados",
  "n_hog",
  "n_jefatura_mujer",
  "n_hog_menores"
)

vars_requeridas <- c(
  ".id",
  "ID_MANZ",
  "ID_MANZCIT",
  "MANZENT",
  "TIPO_MZ",
  "COD_REGION",
  "COMUNA",
  "AREA_C",
  "n_buildings",
  "area_sum",
  "w",
  "prom_escolaridad18",
  vars_conteo
)

assert_has_vars(data_reg, vars_requeridas, "data_reg")

# w debe pertenecer a [0, 1]. Un w = 1 conserva el conteo completo; un valor
# entre 0 y 1 redistribuye una fraccion; w = 0 es valido y no asigna atributos.
if (any(is.na(data_reg$w) | data_reg$w < 0 | data_reg$w > 1)) {
  stop(
    "El ponderador w contiene valores faltantes o fuera de [0, 1].",
    call. = FALSE
  )
}

# Para cada ID efectivamente redistribuido, las partes deben sumar w = 1. Esta
# condicion conserva el total original de personas, hogares y viviendas.
control_w <- data_reg %>%
  st_drop_geometry() %>%
  group_by(.id) %>%
  summarise(
    n_partes = n(),
    usa_redistribucion = any(w < 1),
    suma_w = sum(w),
    .groups = "drop"
  ) %>%
  filter(usa_redistribucion)

if (any(abs(control_w$suma_w - 1) > 1e-6)) {
  stop(
    "Los pesos de uno o mas IDs redistribuidos no suman 1.",
    call. = FALSE
  )
}

resumen_w <- tibble(
  control = c(
    "Geometrias censales urbanas",
    "IDs con redistribucion",
    "Partes con w = 0",
    "IDs redistribuidos cuya suma de w es 1"
  ),
  valor = c(
    nrow(data_reg),
    nrow(control_w),
    sum(data_reg$w == 0),
    sum(abs(control_w$suma_w - 1) <= 1e-6)
  )
)

print(resumen_w)

# across() aplica la misma operacion a todos los conteos. El promedio de
# escolaridad NO se multiplica por w, porque ya es una medida promedio.
censo <- data_reg %>%
  transmute(
    id_original = .id,
    ID_MANZ,
    ID_MANZCIT,
    MANZENT,
    TIPO_MZ,
    COD_REGION,
    COMUNA,
    AREA_C,
    n_buildings,
    area_sum,
    w,
    prom_escolaridad18,
    across(all_of(vars_conteo), ~ .x * w)
  )

# PARA DISCUTIR:
# Si una unidad se divide en dos partes con w = 0.4 y w = 0.6, ¿cuantas
# personas recibe cada parte cuando el conteo original es 100?
# ¿Por que seria incorrecto copiar las 100 personas en ambas partes?


# 4. IEM: indicador de empleo --------------------------------------------------

# CESA mide la proporcion de personas desocupadas dentro de la poblacion activa:
#
#   CESA = n_desocupado / (n_desocupado + n_ocupado)
#   IEM  = 1 - CESA
#
# Se invierte el sentido para que un IEM alto represente una mejor condicion.
censo <- censo %>%
  mutate(
    CESA_DENS = proporcion_segura(
      n_desocupado,
      n_desocupado + n_ocupado
    ),
    IEM = 1 - acotar_01(CESA_DENS)
  )

summary(censo$IEM)

mapa_iem <- ggplot(censo) +
  geom_sf(aes(fill = IEM), color = NA) +
  scale_fill_viridis_c(
    option = "C",
    limits = c(0, 1),
    na.value = "grey90",
    name = "IEM"
  ) +
  labs(
    title = "Indicador de empleo",
    subtitle = "Zonas urbanas de Coquimbo y La Serena"
  ) +
  theme_void() +
  theme(legend.position = "bottom")

if (interactive()) print(mapa_iem)


# 5. IVI: indicador de materialidad de la vivienda ----------------------------

# Primero se calculan tres proporciones de carencia respecto del total de
# viviendas particulares (n_vp): paredes, techo y suelo.
#
#   IVI = 1 - (paredes + techo + suelo) / 3
#
# La media resume las tres dimensiones y la resta invierte el sentido.
censo <- censo %>%
  mutate(
    paredes = proporcion_segura(
      n_mat_paredes_tabique_sin_forro +
        n_mat_paredes_artesanal +
        n_mat_paredes_precarios,
      n_vp
    ),
    techo = proporcion_segura(
      n_mat_techo_fonolita +
        n_mat_techo_precarios +
        n_mat_techo_sin_cubierta,
      n_vp
    ),
    suelo = proporcion_segura(
      n_mat_piso_capa_cemento + n_mat_piso_tierra,
      n_vp
    ),
    across(c(paredes, techo, suelo), acotar_01),
    IVI = 1 - rowMeans(pick(paredes, techo, suelo), na.rm = FALSE)
  )

summary(censo$IVI)

mapa_ivi <- ggplot(censo) +
  geom_sf(aes(fill = IVI), color = NA) +
  scale_fill_viridis_c(
    option = "D",
    limits = c(0, 1),
    na.value = "grey90",
    name = "IVI"
  ) +
  labs(
    title = "Indicador de materialidad de la vivienda",
    subtitle = "Zonas urbanas de Coquimbo y La Serena"
  ) +
  theme_void() +
  theme(legend.position = "bottom")

if (interactive()) print(mapa_ivi)


# 6. ISV: proxy de suficiencia de la vivienda ---------------------------------

# Este proxy combina dos señales de hacinamiento:
#
#   score_isv = proporcion de viviendas hacinadas +
#               2 * proporcion de nucleos hacinados o allegados
#   ISV = 1 - minmax(score_isv)
#
# w_hac = 2 NO es el ponderador geometrico w. Es una decision conceptual que da
# mayor criticidad al segundo componente. Cambiar este valor cambia el resultado.
w_hac <- 2

censo <- censo %>%
  mutate(
    prop_viv_hacinadas = proporcion_segura(
      n_viv_hacinadas,
      n_vp_ocupada
    ),
    prop_nucleos_hacinados = proporcion_segura(
      n_nucleos_hacinados_allegados,
      n_hog
    ),
    score_isv = prop_viv_hacinadas + w_hac * prop_nucleos_hacinados,
    ISV = 1 - minmax(score_isv)
  )

summary(censo$ISV)

mapa_isv <- ggplot(censo) +
  geom_sf(aes(fill = ISV), color = NA) +
  scale_fill_viridis_c(
    option = "A",
    limits = c(0, 1),
    na.value = "grey90",
    name = "ISV"
  ) +
  labs(
    title = "Proxy de suficiencia de la vivienda",
    subtitle = "Zonas urbanas de Coquimbo y La Serena"
  ) +
  theme_void() +
  theme(legend.position = "bottom")

if (interactive()) print(mapa_isv)

# PARA DISCUTIR:
# La normalizacion depende del minimo y maximo del area de estudio. ¿Que podria
# ocurrir con el ISV si se agrega una comuna con valores mucho mas extremos?


# 7. IEJ: proxy de escolaridad adulta -----------------------------------------

# El nombre historico IEJ se conserva, pero la variable disponible corresponde
# al promedio de escolaridad de TODA la poblacion de 18 anos o mas, no solo a
# jefaturas de hogar. Cuando no hay poblacion, el indicador se registra como NA.
#
#   IEJ = prom_escolaridad18, si n_per > 0
censo <- censo %>%
  mutate(
    IEJ = if_else(n_per > 0, prom_escolaridad18, NA_real_)
  )

summary(censo$IEJ)

mapa_iej <- ggplot(censo) +
  geom_sf(aes(fill = IEJ), color = NA) +
  scale_fill_viridis_c(
    option = "E",
    na.value = "grey90",
    name = "Anos"
  ) +
  labs(
    title = "Proxy de escolaridad adulta",
    subtitle = "Zonas urbanas de Coquimbo y La Serena"
  ) +
  theme_void() +
  theme(legend.position = "bottom")

if (interactive()) print(mapa_iej)


# 8. IRH: proxy de estructura de hogares --------------------------------------

# El IRH multiplica dos proporciones:
#
#   score_irh = (hogares con jefatura femenina / hogares) *
#               (hogares con menores / hogares)
#   IRH = 1 - score_irh
#
# La multiplicacion aproxima una co-ocurrencia esperada, pero los datos
# marginales NO permiten identificar que hogares cumplen ambas condiciones.
censo <- censo %>%
  mutate(
    prop_jefatura_mujer = proporcion_segura(n_jefatura_mujer, n_hog),
    prop_hog_menores = proporcion_segura(n_hog_menores, n_hog),
    score_irh = acotar_01(prop_jefatura_mujer) *
      acotar_01(prop_hog_menores),
    IRH = 1 - score_irh
  )

summary(censo$IRH)

mapa_irh <- ggplot(censo) +
  geom_sf(aes(fill = IRH), color = NA) +
  scale_fill_viridis_c(
    option = "B",
    limits = c(0, 1),
    na.value = "grey90",
    name = "IRH"
  ) +
  labs(
    title = "Proxy de estructura de hogares",
    subtitle = "Zonas urbanas de Coquimbo y La Serena"
  ) +
  theme_void() +
  theme(legend.position = "bottom")

if (interactive()) print(mapa_irh)

# ADVERTENCIA ETICA:
# La jefatura femenina no es por si misma una carencia. El IRH no observa redes
# de apoyo, ingresos ni capacidad de cuidado. No debe usarse para estigmatizar
# tipos de familia ni para clasificar hogares individuales.


# 9. Por que no calculamos IPJ -------------------------------------------------

# Un indicador de participacion juvenil necesita conocer la interseccion entre
# edad, asistencia educativa y situacion laboral. Los tabulados disponibles solo
# entregan totales marginales separados. Por ello no es posible identificar el
# numerador de jovenes que simultaneamente no estudian ni trabajan.
#
# Decision metodologica: IPJ no se calcula. No se deben inventar cruces que no
# existen en los datos.


# 10. Dimension socioeconomica sintetica --------------------------------------

# Los cinco indicadores tienen escalas distintas. Cada uno se normaliza a [0, 1]
# dentro del universo formado por Coquimbo y La Serena urbanas. Luego calculamos
# un promedio simple, asignando el mismo peso a cada componente.
indicadores <- c("IEM", "IVI", "ISV", "IEJ", "IRH")
indicadores_n <- paste0(indicadores, "_n")

etiquetas_indicadores <- c(
  DIM_SOC_2024 = "Dimension socioeconomica",
  IEM = "Empleo (IEM)",
  IVI = "Materialidad de la vivienda (IVI)",
  ISV = "Suficiencia de la vivienda (ISV)",
  IEJ = "Escolaridad adulta (IEJ)",
  IRH = "Estructura de hogares (IRH)"
)

sf24 <- censo %>%
  mutate(
    across(
      all_of(indicadores),
      minmax,
      .names = "{.col}_n"
    ),
    n_indicadores_validos = rowSums(!is.na(pick(all_of(indicadores_n)))),
    DIM_SOC_2024 = rowMeans(pick(all_of(indicadores_n)), na.rm = TRUE),
    # La dimension solo se informa cuando estan disponibles los cinco valores.
    DIM_SOC_2024 = if_else(
      n_indicadores_validos == length(indicadores_n),
      DIM_SOC_2024,
      NA_real_
    )
  ) %>%
  st_make_valid() %>%
  select(
    id_original,
    ID_MANZ,
    ID_MANZCIT,
    MANZENT,
    TIPO_MZ,
    COD_REGION,
    COMUNA,
    AREA_C,
    n_buildings,
    area_sum,
    w,
    all_of(indicadores),
    all_of(indicadores_n),
    DIM_SOC_2024
  )

summary(sf24$DIM_SOC_2024)

mapa_dimension <- ggplot(sf24) +
  geom_sf(aes(fill = DIM_SOC_2024), color = NA) +
  scale_fill_viridis_c(
    option = "F",
    limits = c(0, 1),
    na.value = "grey90",
    name = "Dimension"
  ) +
  labs(
    title = "Dimension socioeconomica 2024",
    subtitle = "Zonas urbanas de Coquimbo y La Serena"
  ) +
  theme_void() +
  theme(legend.position = "bottom")

if (interactive()) print(mapa_dimension)


# 11. Resumen y distribuciones -------------------------------------------------

# La tabla permite comparar cobertura, centro y rango de cada indicador. Los
# valores NA se cuentan de forma explicita porque tambien informan sobre calidad
# y disponibilidad de los datos.
resumen_indicadores <- sf24 %>%
  st_drop_geometry() %>%
  select(all_of(indicadores), DIM_SOC_2024) %>%
  pivot_longer(
    cols = everything(),
    names_to = "indicador",
    values_to = "valor"
  ) %>%
  group_by(indicador) %>%
  summarise(
    n_validos = sum(!is.na(valor)),
    pct_faltantes = mean(is.na(valor)) * 100,
    minimo = min(valor, na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    media = mean(valor, na.rm = TRUE),
    maximo = max(valor, na.rm = TRUE),
    .groups = "drop"
  )

print(resumen_indicadores)

datos_distribucion <- sf24 %>%
  st_drop_geometry() %>%
  select(all_of(indicadores_n), DIM_SOC_2024) %>%
  pivot_longer(
    cols = everything(),
    names_to = "indicador",
    values_to = "valor"
  ) %>%
  mutate(
    indicador = sub("_n$", "", indicador),
    indicador = factor(
      indicador,
      levels = names(etiquetas_indicadores),
      labels = unname(etiquetas_indicadores)
    )
  )

grafico_distribuciones <- ggplot(datos_distribucion, aes(x = valor)) +
  geom_histogram(
    bins = 30,
    fill = "#2C7FB8",
    color = "white",
    linewidth = 0.2
  ) +
  facet_wrap(~ indicador, ncol = 2, scales = "free_y") +
  scale_x_continuous(limits = c(0, 1)) +
  labs(
    title = "Distribucion de indicadores - Coquimbo y La Serena urbanas",
    x = "Valor normalizado",
    y = "Numero de geometrias"
  ) +
  theme_minimal()

if (interactive()) print(grafico_distribuciones)


# 12. Controles de calidad -----------------------------------------------------

# Todo indicador normalizado y la dimension final deben permanecer en [0, 1].
variables_01 <- c(indicadores_n, "DIM_SOC_2024")

fuera_de_rango <- sf24 %>%
  st_drop_geometry() %>%
  summarise(
    across(
      all_of(variables_01),
      ~ sum(.x < 0 | .x > 1, na.rm = TRUE)
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "n_fuera_de_rango"
  )

print(fuera_de_rango)

# stopifnot() detiene el guion si una condicion de calidad no se cumple.
stopifnot(all(fuera_de_rango$n_fuera_de_rango == 0))

cat("Controles aprobados: no hay valores normalizados fuera de [0, 1].\n")


# 13. Guardado opcional --------------------------------------------------------

# FALSE evita sobrescribir resultados accidentalmente durante una demostracion.
# Cambiar a TRUE solo cuando se quiera generar los productos finales.
guardar_resultados <- FALSE

if (guardar_resultados) {
  path_resultados <- "data/socioeconomicos/resultados"
  dir.create(path_resultados, recursive = TRUE, showWarnings = FALSE)

  path_gpkg <- file.path(
    path_resultados,
    "R04_Coquimbo_LaSerena_urbano_ind_socioeconomicos_2024.gpkg"
  )
  path_rds <- file.path(
    path_resultados,
    "R04_Coquimbo_LaSerena_urbano_ind_socioeconomicos_2024.rds"
  )

  st_write(sf24, path_gpkg, delete_dsn = TRUE, quiet = TRUE)
  saveRDS(sf24, path_rds)

  cat("Resultados guardados en:\n", path_gpkg, "\n", path_rds, "\n")
} else {
  cat(
    "Guardado omitido. Cambie guardar_resultados a TRUE para exportar ",
    "GeoPackage y RDS.\n"
  )
}


# 14. Preguntas de cierre ------------------------------------------------------

# 1. ¿Que diferencia hay entre una medicion directa y un proxy?
# 2. ¿Por que los valores altos de todos los indicadores deben apuntar en la
#    misma direccion antes de combinarlos?
# 3. ¿Como afecta el universo territorial a una normalizacion min-max?
# 4. ¿Por que w se aplica a conteos, pero no a prom_escolaridad18?
# 5. ¿Que limites impiden interpretar estos resultados como atributos de cada
#    persona u hogar?
#
# Idea central: un indicador territorial no es solo una formula. Es una cadena
# documentada de decisiones sobre problema, datos, escala, calculo, validacion e
# interpretacion.
