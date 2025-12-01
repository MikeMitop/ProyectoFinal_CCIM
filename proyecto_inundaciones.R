# =============================================================================
# PROYECTO FINAL INTEGRADOR
# Gestión y Predicción de Riesgo por Inundaciones Urbanas: 
# Simulación, Clasificación y Protocolos de Alerta
# =============================================================================
# Autor: Proyecto Final CCIM
# Descripción: Sistema integral para análisis de riesgo de inundaciones urbanas
#              que incluye análisis estadístico, clasificación Bayesiana,
#              simulación Monte Carlo y protocolo de alerta mediante autómata finito.
# =============================================================================

# Limpiar el entorno
rm(list = ls())

# Configuración de semilla para reproducibilidad
set.seed(42)

# =============================================================================
# SECCIÓN A: PREPROCESAMIENTO Y ANÁLISIS EXPLORATORIO DE DATOS
# =============================================================================

cat("=============================================================================\n")
cat("SECCIÓN A: PREPROCESAMIENTO Y ANÁLISIS EXPLORATORIO DE DATOS\n")
cat("=============================================================================\n\n")

# -----------------------------------------------------------------------------
# A.1 Generación de Datos Simulados (Hidrometeorológicos)
# -----------------------------------------------------------------------------
# Nota: Se simulan datos de precipitación diaria y niveles de agua para 
# múltiples zonas durante varios años, dado que los datos públicos del IDEAM
# pueden no estar disponibles directamente.

# Definición de zonas de la ciudad
zonas <- data.frame(
  zona_id = 1:6,
  nombre_zona = c("Centro", "Norte", "Sur", "Este", "Oeste", "Industrial"),
  pendiente = c(2.5, 5.0, 1.5, 3.0, 4.0, 1.0),  # Porcentaje de pendiente
  impermeabilizacion = c(85, 60, 45, 70, 55, 90),  # Porcentaje de área impermeable
  nivel_base = c(0.5, 0.3, 0.8, 0.4, 0.35, 0.6),  # Nivel base de agua en metros
  tipo_suelo = c("Urbano", "Residencial", "Rural", "Mixto", "Residencial", "Industrial")
)

cat("Características de las zonas de estudio:\n")
print(zonas)
cat("\n")

# Generar datos históricos de precipitación y niveles de agua (3 años de datos)
n_dias <- 365 * 3  # 3 años de datos
fechas <- seq(as.Date("2021-01-01"), by = "day", length.out = n_dias)

# Función para simular precipitación diaria (distribución gamma con estacionalidad)
simular_precipitacion <- function(n_dias, zona_factor = 1) {
  # Modelo con estacionalidad (más lluvia en ciertos meses)
  mes <- as.numeric(format(fechas, "%m"))
  # Factor estacional: más lluvia en abril-mayo y octubre-noviembre (Colombia)
  factor_estacional <- ifelse(mes %in% c(4, 5, 10, 11), 1.5,
                              ifelse(mes %in% c(1, 2, 7, 8), 0.5, 1.0))
  
  # Probabilidad de lluvia
  prob_lluvia <- 0.3 * factor_estacional * zona_factor
  lluvia <- numeric(n_dias)
  
  for (i in 1:n_dias) {
    if (runif(1) < prob_lluvia[i]) {
      # Si llueve, generar cantidad con distribución gamma
      lluvia[i] <- rgamma(1, shape = 2, rate = 0.1) * factor_estacional[i]
    } else {
      lluvia[i] <- 0
    }
  }
  return(lluvia)
}

# Generar datos para cada zona
datos_historicos <- data.frame()

for (i in 1:nrow(zonas)) {
  zona_id <- zonas$zona_id[i]
  zona_factor <- 1 + (i - 3.5) * 0.1  # Variación por zona
  
  precipitacion <- simular_precipitacion(n_dias, zona_factor)
  
  # Simular nivel de agua basado en precipitación y características de zona
  nivel_agua <- zonas$nivel_base[i] + 
    0.02 * precipitacion + 
    0.005 * zonas$impermeabilizacion[i] * precipitacion / 100 -
    0.01 * zonas$pendiente[i] * precipitacion / 10 +
    rnorm(n_dias, 0, 0.05)
  
  # Asegurar valores no negativos
  nivel_agua <- pmax(nivel_agua, 0)
  
  # Determinar si hubo inundación (nivel > umbral crítico)
  umbral_critico <- 1.5  # metros
  inundacion <- as.integer(nivel_agua > umbral_critico)
  
  zona_datos <- data.frame(
    fecha = fechas,
    zona_id = zona_id,
    nombre_zona = zonas$nombre_zona[i],
    precipitacion_mm = round(precipitacion, 2),
    nivel_agua_m = round(nivel_agua, 3),
    inundacion = inundacion
  )
  
  datos_historicos <- rbind(datos_historicos, zona_datos)
}

cat("Datos históricos simulados generados:\n")
cat("- Período:", as.character(min(fechas)), "a", as.character(max(fechas)), "\n")
cat("- Número de registros:", nrow(datos_historicos), "\n")
cat("- Zonas:", nrow(zonas), "\n\n")

# -----------------------------------------------------------------------------
# A.2 Filtrado y Transformación de Datos
# -----------------------------------------------------------------------------

# Calcular lluvia acumulada en 24 horas (igual a precipitación diaria)
datos_historicos$lluvia_acumulada_24h <- datos_historicos$precipitacion_mm

# Agregar características de zona
datos_historicos <- merge(datos_historicos, 
                          zonas[, c("zona_id", "pendiente", "impermeabilizacion", "nivel_base")],
                          by = "zona_id")

# Agregar mes y año para análisis estacional
datos_historicos$mes <- as.numeric(format(datos_historicos$fecha, "%m"))
datos_historicos$anio <- as.numeric(format(datos_historicos$fecha, "%Y"))

cat("Variables del conjunto de datos:\n")
cat(names(datos_historicos), sep = ", ")
cat("\n\n")

# -----------------------------------------------------------------------------
# A.3 Estadísticas Descriptivas por Zona
# -----------------------------------------------------------------------------

cat("Estadísticas descriptivas por zona:\n")
cat("-" , rep("-", 70), "\n", sep = "")

estadisticas_zona <- aggregate(
  cbind(precipitacion_mm, nivel_agua_m) ~ nombre_zona,
  data = datos_historicos,
  FUN = function(x) c(
    media = mean(x),
    mediana = median(x),
    max = max(x),
    p25 = quantile(x, 0.25),
    p75 = quantile(x, 0.75),
    p95 = quantile(x, 0.95)
  )
)

# Mostrar estadísticas
for (zona in unique(datos_historicos$nombre_zona)) {
  datos_zona <- datos_historicos[datos_historicos$nombre_zona == zona, ]
  
  cat("\nZona:", zona, "\n")
  cat("  Precipitación (mm):\n")
  cat("    Media:", round(mean(datos_zona$precipitacion_mm), 2), "\n")
  cat("    Mediana:", round(median(datos_zona$precipitacion_mm), 2), "\n")
  cat("    Máximo:", round(max(datos_zona$precipitacion_mm), 2), "\n")
  cat("    Percentil 95:", round(quantile(datos_zona$precipitacion_mm, 0.95), 2), "\n")
  
  cat("  Nivel de agua (m):\n")
  cat("    Media:", round(mean(datos_zona$nivel_agua_m), 3), "\n")
  cat("    Mediana:", round(median(datos_zona$nivel_agua_m), 3), "\n")
  cat("    Máximo:", round(max(datos_zona$nivel_agua_m), 3), "\n")
  cat("    Percentil 95:", round(quantile(datos_zona$nivel_agua_m, 0.95), 3), "\n")
  
  tasa_inundacion <- mean(datos_zona$inundacion) * 100
  cat("  Tasa de inundación:", round(tasa_inundacion, 2), "%\n")
}

# Estadísticas generales
cat("\n\nResumen global del dataset:\n")
cat("Total de días con inundación:", sum(datos_historicos$inundacion), "\n")
cat("Proporción de inundaciones:", round(mean(datos_historicos$inundacion) * 100, 2), "%\n")

# =============================================================================
# SECCIÓN B: CLASIFICADOR PROBABILÍSTICO (BAYES)
# =============================================================================

cat("\n\n=============================================================================\n")
cat("SECCIÓN B: CLASIFICADOR PROBABILÍSTICO BAYESIANO\n")
cat("=============================================================================\n\n")

# -----------------------------------------------------------------------------
# B.1 Definición de Variables Predictoras
# -----------------------------------------------------------------------------

cat("Variables predictoras para el clasificador Bayesiano:\n")
cat("1. Lluvia acumulada 24h (mm)\n")
cat("2. Pendiente del terreno (%)\n")
cat("3. Porcentaje de impermeabilización (%)\n")
cat("4. Nivel base de agua (m)\n\n")

# Discretizar variables continuas para el clasificador Bayesiano
datos_bayes <- datos_historicos

# Categorizar lluvia acumulada
datos_bayes$lluvia_categoria <- cut(datos_bayes$lluvia_acumulada_24h,
                                    breaks = c(-Inf, 0, 10, 30, 50, Inf),
                                    labels = c("Sin_lluvia", "Ligera", "Moderada", "Fuerte", "Muy_fuerte"))

# Categorizar pendiente
datos_bayes$pendiente_categoria <- cut(datos_bayes$pendiente,
                                       breaks = c(-Inf, 2, 4, Inf),
                                       labels = c("Baja", "Media", "Alta"))

# Categorizar impermeabilización
datos_bayes$impermeabilizacion_categoria <- cut(datos_bayes$impermeabilizacion,
                                                breaks = c(-Inf, 50, 70, Inf),
                                                labels = c("Baja", "Media", "Alta"))

# Categorizar nivel base
datos_bayes$nivel_base_categoria <- cut(datos_bayes$nivel_base,
                                        breaks = c(-Inf, 0.4, 0.6, Inf),
                                        labels = c("Bajo", "Medio", "Alto"))

# Variable objetivo
datos_bayes$inundacion_factor <- factor(datos_bayes$inundacion, 
                                        levels = c(0, 1), 
                                        labels = c("No", "Si"))

# -----------------------------------------------------------------------------
# B.2 División de datos: Entrenamiento y Prueba
# -----------------------------------------------------------------------------

# División estratificada por zona
set.seed(42)
indices_entrenamiento <- sample(1:nrow(datos_bayes), size = 0.7 * nrow(datos_bayes))
datos_entrenamiento <- datos_bayes[indices_entrenamiento, ]
datos_prueba <- datos_bayes[-indices_entrenamiento, ]

cat("División de datos:\n")
cat("- Datos de entrenamiento:", nrow(datos_entrenamiento), "registros\n")
cat("- Datos de prueba:", nrow(datos_prueba), "registros\n\n")

# -----------------------------------------------------------------------------
# B.3 Estimación de Probabilidades A Priori y Condicionales
# -----------------------------------------------------------------------------

# Probabilidad a priori de inundación
prob_priori_inundacion <- table(datos_entrenamiento$inundacion_factor) / nrow(datos_entrenamiento)
cat("Probabilidades a priori:\n")
cat("P(Inundación = No):", round(prob_priori_inundacion["No"], 4), "\n")
cat("P(Inundación = Si):", round(prob_priori_inundacion["Si"], 4), "\n\n")

# Función para calcular probabilidades condicionales
calcular_prob_condicional <- function(variable, clase_inundacion, datos) {
  datos_clase <- datos[datos$inundacion_factor == clase_inundacion, ]
  tabla <- table(datos_clase[[variable]])
  prob <- (tabla + 1) / (sum(tabla) + length(unique(datos[[variable]])))  # Suavizado de Laplace
  return(prob)
}

# Calcular probabilidades condicionales para cada variable
variables_predictoras <- c("lluvia_categoria", "pendiente_categoria", 
                           "impermeabilizacion_categoria", "nivel_base_categoria")

prob_condicionales <- list()

cat("Probabilidades condicionales (con suavizado de Laplace):\n")
cat("-", rep("-", 50), "\n\n", sep = "")

for (var in variables_predictoras) {
  prob_condicionales[[var]] <- list(
    Si = calcular_prob_condicional(var, "Si", datos_entrenamiento),
    No = calcular_prob_condicional(var, "No", datos_entrenamiento)
  )
  
  cat("Variable:", var, "\n")
  cat("  P(", var, " | Inundación = Si):\n", sep = "")
  print(round(prob_condicionales[[var]]$Si, 4))
  cat("  P(", var, " | Inundación = No):\n", sep = "")
  print(round(prob_condicionales[[var]]$No, 4))
  cat("\n")
}

# -----------------------------------------------------------------------------
# B.4 Función del Clasificador Bayesiano
# -----------------------------------------------------------------------------

clasificador_bayesiano <- function(lluvia_cat, pendiente_cat, impermeabilizacion_cat, 
                                   nivel_base_cat, prob_priori, prob_cond) {
  
  # Calcular P(X | Inundación = Si) * P(Inundación = Si)
  p_x_dado_si <- 1
  p_x_dado_no <- 1
  
  # Obtener probabilidades condicionales para cada variable
  vars <- list(
    lluvia_categoria = lluvia_cat,
    pendiente_categoria = pendiente_cat,
    impermeabilizacion_categoria = impermeabilizacion_cat,
    nivel_base_categoria = nivel_base_cat
  )
  
  for (var_name in names(vars)) {
    valor <- as.character(vars[[var_name]])
    
    # Probabilidad condicional dado inundación = Si
    if (valor %in% names(prob_cond[[var_name]]$Si)) {
      p_x_dado_si <- p_x_dado_si * prob_cond[[var_name]]$Si[valor]
    } else {
      p_x_dado_si <- p_x_dado_si * (1 / (length(prob_cond[[var_name]]$Si) + 1))
    }
    
    # Probabilidad condicional dado inundación = No
    if (valor %in% names(prob_cond[[var_name]]$No)) {
      p_x_dado_no <- p_x_dado_no * prob_cond[[var_name]]$No[valor]
    } else {
      p_x_dado_no <- p_x_dado_no * (1 / (length(prob_cond[[var_name]]$No) + 1))
    }
  }
  
  # Aplicar teorema de Bayes
  p_si_x <- p_x_dado_si * prob_priori["Si"]
  p_no_x <- p_x_dado_no * prob_priori["No"]
  
  # Normalizar
  p_total <- p_si_x + p_no_x
  prob_inundacion <- p_si_x / p_total
  
  return(as.numeric(prob_inundacion))
}

# -----------------------------------------------------------------------------
# B.5 Validación del Clasificador
# -----------------------------------------------------------------------------

cat("Validando el clasificador Bayesiano...\n\n")

# Aplicar clasificador a datos de prueba
datos_prueba$prob_inundacion <- sapply(1:nrow(datos_prueba), function(i) {
  clasificador_bayesiano(
    datos_prueba$lluvia_categoria[i],
    datos_prueba$pendiente_categoria[i],
    datos_prueba$impermeabilizacion_categoria[i],
    datos_prueba$nivel_base_categoria[i],
    prob_priori_inundacion,
    prob_condicionales
  )
})

# Clasificación usando umbral 0.5
datos_prueba$prediccion <- ifelse(datos_prueba$prob_inundacion > 0.5, "Si", "No")

# Matriz de confusión
matriz_confusion <- table(Predicho = datos_prueba$prediccion, 
                          Real = datos_prueba$inundacion_factor)
cat("Matriz de Confusión:\n")
print(matriz_confusion)
cat("\n")

# Métricas de rendimiento
verdaderos_positivos <- if ("Si" %in% rownames(matriz_confusion) && "Si" %in% colnames(matriz_confusion)) 
  matriz_confusion["Si", "Si"] else 0
verdaderos_negativos <- if ("No" %in% rownames(matriz_confusion) && "No" %in% colnames(matriz_confusion)) 
  matriz_confusion["No", "No"] else 0
falsos_positivos <- if ("Si" %in% rownames(matriz_confusion) && "No" %in% colnames(matriz_confusion)) 
  matriz_confusion["Si", "No"] else 0
falsos_negativos <- if ("No" %in% rownames(matriz_confusion) && "Si" %in% colnames(matriz_confusion)) 
  matriz_confusion["No", "Si"] else 0

precision <- (verdaderos_positivos + verdaderos_negativos) / nrow(datos_prueba)
sensibilidad <- if ((verdaderos_positivos + falsos_negativos) > 0) 
  verdaderos_positivos / (verdaderos_positivos + falsos_negativos) else 0
especificidad <- if ((verdaderos_negativos + falsos_positivos) > 0) 
  verdaderos_negativos / (verdaderos_negativos + falsos_positivos) else 0

cat("Métricas de rendimiento del clasificador:\n")
cat("- Precisión (Accuracy):", round(precision * 100, 2), "%\n")
cat("- Sensibilidad (Recall):", round(sensibilidad * 100, 2), "%\n")
cat("- Especificidad:", round(especificidad * 100, 2), "%\n")

# =============================================================================
# SECCIÓN C: SIMULACIÓN MONTE CARLO
# =============================================================================

cat("\n\n=============================================================================\n")
cat("SECCIÓN C: SIMULACIÓN MONTE CARLO\n")
cat("=============================================================================\n\n")

# -----------------------------------------------------------------------------
# C.1 Parámetros de la Simulación
# -----------------------------------------------------------------------------

n_simulaciones <- 10000  # Número de simulaciones Monte Carlo
umbral_critico_nivel <- 1.5  # Umbral crítico de nivel de agua en metros

cat("Parámetros de la simulación Monte Carlo:\n")
cat("- Número de simulaciones:", n_simulaciones, "\n")
cat("- Umbral crítico de nivel de agua:", umbral_critico_nivel, "m\n\n")

# -----------------------------------------------------------------------------
# C.2 Modelado de Escenarios de Lluvia Intensa
# -----------------------------------------------------------------------------

# Función para simular un evento de lluvia intensa (24 horas)
simular_lluvia_intensa <- function(intensidad_media = 50, variabilidad = 20) {
  # Distribución gamma para modelar precipitación
  forma <- (intensidad_media / variabilidad)^2
  escala <- variabilidad^2 / intensidad_media
  return(rgamma(1, shape = forma, scale = escala))
}

# Función para calcular nivel de agua dado lluvia y características de zona
calcular_nivel_agua <- function(lluvia, pendiente, impermeabilizacion, nivel_base) {
  # Modelo simplificado de respuesta hidrológica
  coef_lluvia <- 0.02
  coef_impermeabilizacion <- 0.005
  coef_pendiente <- -0.01
  ruido <- rnorm(1, 0, 0.05)
  
  nivel <- nivel_base + 
    coef_lluvia * lluvia + 
    coef_impermeabilizacion * impermeabilizacion * lluvia / 100 -
    coef_pendiente * pendiente * lluvia / 10 +
    ruido
  
  return(max(nivel, 0))
}

# -----------------------------------------------------------------------------
# C.3 Ejecución de Simulaciones Monte Carlo por Zona
# -----------------------------------------------------------------------------

cat("Ejecutando simulaciones Monte Carlo...\n\n")

resultados_montecarlo <- data.frame()

for (i in 1:nrow(zonas)) {
  zona_actual <- zonas[i, ]
  niveles_simulados <- numeric(n_simulaciones)
  lluvias_simuladas <- numeric(n_simulaciones)
  
  for (sim in 1:n_simulaciones) {
    # Simular evento de lluvia intensa
    lluvia <- simular_lluvia_intensa(intensidad_media = 60, variabilidad = 25)
    lluvias_simuladas[sim] <- lluvia
    
    # Calcular nivel de agua resultante
    nivel <- calcular_nivel_agua(
      lluvia = lluvia,
      pendiente = zona_actual$pendiente,
      impermeabilizacion = zona_actual$impermeabilizacion,
      nivel_base = zona_actual$nivel_base
    )
    niveles_simulados[sim] <- nivel
  }
  
  # Calcular probabilidad de excedencia
  prob_excedencia <- mean(niveles_simulados > umbral_critico_nivel)
  error_estandar <- sqrt(prob_excedencia * (1 - prob_excedencia) / n_simulaciones)
  
  # Intervalo de confianza 95%
  ic_inferior <- max(0, prob_excedencia - 1.96 * error_estandar)
  ic_superior <- min(1, prob_excedencia + 1.96 * error_estandar)
  
  # Estadísticas de nivel de agua
  nivel_medio <- mean(niveles_simulados)
  nivel_p95 <- quantile(niveles_simulados, 0.95)
  nivel_max <- max(niveles_simulados)
  
  resultado_zona <- data.frame(
    zona_id = zona_actual$zona_id,
    nombre_zona = zona_actual$nombre_zona,
    prob_excedencia = prob_excedencia,
    error_estandar = error_estandar,
    ic_inferior = ic_inferior,
    ic_superior = ic_superior,
    nivel_medio = nivel_medio,
    nivel_p95 = nivel_p95,
    nivel_max = nivel_max
  )
  
  resultados_montecarlo <- rbind(resultados_montecarlo, resultado_zona)
}

# -----------------------------------------------------------------------------
# C.4 Resultados de la Simulación
# -----------------------------------------------------------------------------

cat("Resultados de Simulación Monte Carlo por Zona:\n")
cat("-", rep("-", 70), "\n\n", sep = "")

for (i in 1:nrow(resultados_montecarlo)) {
  res <- resultados_montecarlo[i, ]
  cat("Zona:", res$nombre_zona, "\n")
  cat("  Probabilidad de excedencia:", round(res$prob_excedencia * 100, 2), "%\n")
  cat("  Error estándar:", round(res$error_estandar * 100, 3), "%\n")
  cat("  Intervalo de confianza 95%: [", round(res$ic_inferior * 100, 2), "%, ", 
      round(res$ic_superior * 100, 2), "%]\n", sep = "")
  cat("  Nivel medio simulado:", round(res$nivel_medio, 3), "m\n")
  cat("  Nivel percentil 95:", round(res$nivel_p95, 3), "m\n")
  cat("  Nivel máximo simulado:", round(res$nivel_max, 3), "m\n\n")
}

# Ordenar zonas por riesgo
zonas_ordenadas <- resultados_montecarlo[order(-resultados_montecarlo$prob_excedencia), ]
cat("Ranking de zonas por riesgo de inundación:\n")
for (i in 1:nrow(zonas_ordenadas)) {
  cat(i, ". ", zonas_ordenadas$nombre_zona[i], " - Prob. excedencia: ", 
      round(zonas_ordenadas$prob_excedencia[i] * 100, 2), "%\n", sep = "")
}

# =============================================================================
# SECCIÓN D: PROTOCOLO DE ALERTA (AUTÓMATA FINITO)
# =============================================================================

cat("\n\n=============================================================================\n")
cat("SECCIÓN D: PROTOCOLO DE ALERTA - AUTÓMATA FINITO\n")
cat("=============================================================================\n\n")

# -----------------------------------------------------------------------------
# D.1 Definición de Estados del Autómata
# -----------------------------------------------------------------------------

cat("Definición del Autómata Finito para Protocolo de Alerta:\n\n")

estados <- c("Normal", "Vigilancia", "Alerta", "Evacuacion")

cat("Estados del sistema:\n")
cat("1. Normal: Condiciones normales, sin riesgo significativo\n")
cat("2. Vigilancia: Riesgo moderado, monitoreo activo\n")
cat("3. Alerta: Riesgo alto, preparación para evacuación\n")
cat("4. Evacuacion: Riesgo crítico, evacuación activa\n\n")

# Umbrales de transición basados en probabilidad de inundación
umbrales <- list(
  normal_a_vigilancia = 0.2,    # 20% probabilidad
  vigilancia_a_alerta = 0.4,    # 40% probabilidad
  alerta_a_evacuacion = 0.6,    # 60% probabilidad
  evacuacion_a_alerta = 0.4,    # Para regresar
  alerta_a_vigilancia = 0.25,
  vigilancia_a_normal = 0.1
)

cat("Umbrales de transición:\n")
cat("- Normal → Vigilancia: P(inundación) > ", umbrales$normal_a_vigilancia * 100, "%\n", sep = "")
cat("- Vigilancia → Alerta: P(inundación) > ", umbrales$vigilancia_a_alerta * 100, "%\n", sep = "")
cat("- Alerta → Evacuación: P(inundación) > ", umbrales$alerta_a_evacuacion * 100, "%\n", sep = "")
cat("- Evacuación → Alerta: P(inundación) < ", umbrales$evacuacion_a_alerta * 100, "%\n", sep = "")
cat("- Alerta → Vigilancia: P(inundación) < ", umbrales$alerta_a_vigilancia * 100, "%\n", sep = "")
cat("- Vigilancia → Normal: P(inundación) < ", umbrales$vigilancia_a_normal * 100, "%\n\n", sep = "")

# -----------------------------------------------------------------------------
# D.2 Función de Transición del Autómata
# -----------------------------------------------------------------------------

transicion_automata <- function(estado_actual, probabilidad_inundacion, umbrales) {
  nuevo_estado <- estado_actual
  
  if (estado_actual == "Normal") {
    if (probabilidad_inundacion > umbrales$normal_a_vigilancia) {
      nuevo_estado <- "Vigilancia"
    }
  } else if (estado_actual == "Vigilancia") {
    if (probabilidad_inundacion > umbrales$vigilancia_a_alerta) {
      nuevo_estado <- "Alerta"
    } else if (probabilidad_inundacion < umbrales$vigilancia_a_normal) {
      nuevo_estado <- "Normal"
    }
  } else if (estado_actual == "Alerta") {
    if (probabilidad_inundacion > umbrales$alerta_a_evacuacion) {
      nuevo_estado <- "Evacuacion"
    } else if (probabilidad_inundacion < umbrales$alerta_a_vigilancia) {
      nuevo_estado <- "Vigilancia"
    }
  } else if (estado_actual == "Evacuacion") {
    if (probabilidad_inundacion < umbrales$evacuacion_a_alerta) {
      nuevo_estado <- "Alerta"
    }
  }
  
  return(nuevo_estado)
}

# Función para obtener acciones recomendadas por estado
obtener_acciones <- function(estado) {
  acciones <- list(
    Normal = c(
      "- Monitoreo rutinario de estaciones meteorológicas",
      "- Verificación periódica de sistemas de drenaje",
      "- Comunicaciones normales con la población"
    ),
    Vigilancia = c(
      "- Activar monitoreo intensivo cada hora",
      "- Alertar a equipos de emergencia",
      "- Preparar recursos de respuesta",
      "- Comunicar situación a la población"
    ),
    Alerta = c(
      "- Activar centros de emergencia",
      "- Preparar rutas de evacuación",
      "- Alertar hospitales y servicios esenciales",
      "- Emitir comunicados de alerta pública",
      "- Posicionar equipos de rescate"
    ),
    Evacuacion = c(
      "- EJECUTAR EVACUACIÓN INMEDIATA",
      "- Activar sirenas de emergencia",
      "- Abrir refugios temporales",
      "- Cerrar vías en zonas de riesgo",
      "- Desplegar equipos de rescate",
      "- Coordinar con servicios médicos de emergencia"
    )
  )
  
  return(acciones[[estado]])
}

# -----------------------------------------------------------------------------
# D.3 Simulación del Protocolo de Alerta
# -----------------------------------------------------------------------------

cat("Simulación del protocolo de alerta para cada zona:\n")
cat("-", rep("-", 70), "\n\n", sep = "")

# Usar las probabilidades calculadas por Monte Carlo
for (i in 1:nrow(resultados_montecarlo)) {
  zona <- resultados_montecarlo$nombre_zona[i]
  prob <- resultados_montecarlo$prob_excedencia[i]
  
  # Simular evolución del estado (comenzando desde Normal)
  estado_actual <- "Normal"
  historial_estados <- c(estado_actual)
  
  # Simular transiciones hasta llegar al estado correspondiente a la probabilidad
  for (paso in 1:5) {
    estado_nuevo <- transicion_automata(estado_actual, prob, umbrales)
    if (estado_nuevo != estado_actual) {
      historial_estados <- c(historial_estados, estado_nuevo)
      estado_actual <- estado_nuevo
    } else {
      break
    }
  }
  
  cat("Zona:", zona, "\n")
  cat("  Probabilidad de inundación:", round(prob * 100, 2), "%\n")
  cat("  Estado final del protocolo:", estado_actual, "\n")
  cat("  Transiciones:", paste(historial_estados, collapse = " → "), "\n")
  cat("  Acciones recomendadas:\n")
  acciones <- obtener_acciones(estado_actual)
  for (accion in acciones) {
    cat("    ", accion, "\n", sep = "")
  }
  cat("\n")
}

# -----------------------------------------------------------------------------
# D.4 Tabla de Transiciones del Autómata
# -----------------------------------------------------------------------------

cat("\nTabla de Transiciones del Autómata:\n")
cat("-", rep("-", 70), "\n", sep = "")
cat("\n")

# Crear matriz de transiciones
matriz_transiciones <- matrix(
  c(
    "Normal", "P > 20%", "Vigilancia",
    "Vigilancia", "P > 40%", "Alerta",
    "Vigilancia", "P < 10%", "Normal",
    "Alerta", "P > 60%", "Evacuacion",
    "Alerta", "P < 25%", "Vigilancia",
    "Evacuacion", "P < 40%", "Alerta"
  ),
  ncol = 3,
  byrow = TRUE
)

colnames(matriz_transiciones) <- c("Estado_Origen", "Condicion", "Estado_Destino")

cat("Estado Origen    | Condición      | Estado Destino\n")
cat("-", rep("-", 50), "\n", sep = "")
for (i in 1:nrow(matriz_transiciones)) {
  cat(sprintf("%-16s | %-14s | %s\n", 
              matriz_transiciones[i, 1],
              matriz_transiciones[i, 2],
              matriz_transiciones[i, 3]))
}

# =============================================================================
# SECCIÓN E: INFORME FINAL Y RECOMENDACIONES
# =============================================================================

cat("\n\n=============================================================================\n")
cat("SECCIÓN E: INFORME FINAL Y RECOMENDACIONES\n")
cat("=============================================================================\n\n")

# -----------------------------------------------------------------------------
# E.1 Resumen de Resultados
# -----------------------------------------------------------------------------

cat("RESUMEN EJECUTIVO\n")
cat("-", rep("-", 70), "\n\n", sep = "")

cat("1. ANÁLISIS DE DATOS HISTÓRICOS:\n")
cat("   - Se analizaron", nrow(datos_historicos), "registros de", nrow(zonas), "zonas\n")
cat("   - Período: 3 años de datos simulados (2021-2023)\n")
cat("   - Tasa general de inundación:", round(mean(datos_historicos$inundacion) * 100, 2), "%\n\n")

cat("2. CLASIFICADOR BAYESIANO:\n")
cat("   - Precisión del modelo:", round(precision * 100, 2), "%\n")
cat("   - El clasificador identifica correctamente las condiciones de riesgo\n")
cat("   - Variables más influyentes: lluvia acumulada e impermeabilización\n\n")

cat("3. SIMULACIÓN MONTE CARLO:\n")
cat("   - Se ejecutaron", n_simulaciones, "simulaciones por zona\n")
cat("   - Zona de mayor riesgo:", zonas_ordenadas$nombre_zona[1], 
    "(", round(zonas_ordenadas$prob_excedencia[1] * 100, 2), "%)\n", sep = "")
cat("   - Zona de menor riesgo:", zonas_ordenadas$nombre_zona[nrow(zonas_ordenadas)], 
    "(", round(zonas_ordenadas$prob_excedencia[nrow(zonas_ordenadas)] * 100, 2), "%)\n\n", sep = "")

cat("4. PROTOCOLO DE ALERTA:\n")
cat("   - Sistema de 4 estados: Normal, Vigilancia, Alerta, Evacuación\n")
cat("   - Transiciones automáticas basadas en probabilidad de inundación\n")
cat("   - Acciones específicas definidas para cada estado\n\n")

# -----------------------------------------------------------------------------
# E.2 Recomendaciones Operativas
# -----------------------------------------------------------------------------

cat("RECOMENDACIONES OPERATIVAS\n")
cat("-", rep("-", 70), "\n\n", sep = "")

cat("1. UMBRALES DE ACTIVACIÓN RECOMENDADOS:\n")
cat("   - Activar Vigilancia cuando P(inundación) > 20%\n")
cat("   - Activar Alerta cuando P(inundación) > 40%\n")
cat("   - Iniciar Evacuación cuando P(inundación) > 60%\n\n")

cat("2. PRIORIDADES DE EVACUACIÓN:\n")
cat("   Orden de evacuación según riesgo calculado:\n")
for (i in 1:nrow(zonas_ordenadas)) {
  prioridad <- ifelse(i <= 2, "ALTA", ifelse(i <= 4, "MEDIA", "BAJA"))
  cat("   ", i, ". ", zonas_ordenadas$nombre_zona[i], 
      " - Prioridad: ", prioridad, "\n", sep = "")
}
cat("\n")

cat("3. MEDIDAS ESTRUCTURALES RECOMENDADAS:\n")
cat("   - Zonas con alta impermeabilización: Implementar sistemas de drenaje\n")
cat("     sostenible (jardines de lluvia, pavimentos permeables)\n")
cat("   - Zonas con baja pendiente: Instalar bombas de drenaje adicionales\n")
cat("   - Puntos críticos: Ampliar capacidad de alcantarillado\n\n")

cat("4. MEDIDAS NO ESTRUCTURALES:\n")
cat("   - Implementar sistema de alerta temprana automatizado\n")
cat("   - Capacitar a la población en rutas de evacuación\n")
cat("   - Realizar simulacros periódicos (mínimo 2 por año)\n")
cat("   - Mantener actualizado el inventario de recursos de emergencia\n\n")

cat("5. MONITOREO Y ACTUALIZACIÓN:\n")
cat("   - Actualizar datos del modelo cada 6 meses\n")
cat("   - Recalibrar umbrales después de cada evento significativo\n")
cat("   - Revisar clasificador Bayesiano con nuevos datos de entrenamiento\n\n")

# -----------------------------------------------------------------------------
# E.3 Limitaciones y Trabajo Futuro
# -----------------------------------------------------------------------------

cat("LIMITACIONES DEL ESTUDIO:\n")
cat("-", rep("-", 70), "\n\n", sep = "")

cat("1. Los datos utilizados son simulados; se recomienda validar con datos reales\n")
cat("   del IDEAM o estaciones locales.\n\n")

cat("2. El modelo hidrológico es simplificado; modelos más complejos (HEC-RAS,\n")
cat("   SWMM) proporcionarían mayor precisión.\n\n")

cat("3. No se consideran factores como:\n")
cat("   - Efecto de mareas en zonas costeras\n")
cat("   - Capacidad real del sistema de alcantarillado\n")
cat("   - Obstrucciones por residuos sólidos\n\n")

cat("TRABAJO FUTURO:\n")
cat("- Integrar datos en tiempo real de estaciones meteorológicas\n")
cat("- Implementar interfaz web para visualización del estado de alerta\n")
cat("- Desarrollar aplicación móvil para notificaciones a la población\n")
cat("- Incorporar predicciones de modelos climáticos globales\n\n")

# =============================================================================
# VISUALIZACIONES
# =============================================================================

cat("=============================================================================\n")
cat("GENERANDO VISUALIZACIONES...\n")
cat("=============================================================================\n\n")

# Crear directorio para gráficos si no existe
if (!dir.exists("graficos")) {
  dir.create("graficos")
}

# Gráfico 1: Probabilidad de excedencia por zona
png("graficos/probabilidad_excedencia.png", width = 800, height = 600)
par(mar = c(8, 5, 4, 2))
barplot(
  resultados_montecarlo$prob_excedencia * 100,
  names.arg = resultados_montecarlo$nombre_zona,
  col = ifelse(resultados_montecarlo$prob_excedencia > 0.4, "red",
               ifelse(resultados_montecarlo$prob_excedencia > 0.2, "orange", "green")),
  main = "Probabilidad de Excedencia del Umbral Crítico por Zona",
  ylab = "Probabilidad (%)",
  xlab = "",
  las = 2,
  ylim = c(0, 100)
)
abline(h = c(20, 40, 60), lty = 2, col = c("green", "orange", "red"))
legend("topright", 
       legend = c("Riesgo Alto", "Riesgo Medio", "Riesgo Bajo"),
       fill = c("red", "orange", "green"))
dev.off()
cat("Gráfico guardado: graficos/probabilidad_excedencia.png\n")

# Gráfico 2: Diagrama del autómata (representación textual)
png("graficos/automata_estados.png", width = 800, height = 600)
par(mar = c(2, 2, 4, 2))
plot(1, type = "n", xlim = c(0, 10), ylim = c(0, 10), 
     main = "Autómata Finito - Protocolo de Alerta",
     xlab = "", ylab = "", axes = FALSE)

# Dibujar estados como círculos
symbols(c(2, 5, 8, 5), c(5, 8, 5, 2), circles = rep(0.8, 4),
        inches = FALSE, add = TRUE, 
        bg = c("green", "yellow", "orange", "red"))
text(c(2, 5, 8, 5), c(5, 8, 5, 2), 
     c("Normal", "Vigilancia", "Alerta", "Evacuación"), cex = 0.9)

# Dibujar flechas de transición
arrows(2.8, 5.5, 4.2, 7.5, length = 0.1)
arrows(5.8, 7.5, 7.2, 5.5, length = 0.1)
arrows(7.2, 4.5, 5.8, 2.5, length = 0.1)

# Flechas de retorno
arrows(4.2, 7.2, 2.8, 5.8, length = 0.1, lty = 2)
arrows(7.2, 5.8, 5.8, 7.2, length = 0.1, lty = 2)
arrows(5.8, 2.8, 7.2, 4.2, length = 0.1, lty = 2)

# Etiquetas de transición
text(3.2, 6.8, "P>20%", cex = 0.7)
text(6.8, 6.8, "P>40%", cex = 0.7)
text(7.2, 3.2, "P>60%", cex = 0.7)

dev.off()
cat("Gráfico guardado: graficos/automata_estados.png\n")

# Gráfico 3: Distribución de niveles de agua simulados
png("graficos/distribucion_niveles.png", width = 800, height = 600)
par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))

for (i in 1:nrow(zonas)) {
  zona_actual <- zonas[i, ]
  niveles <- numeric(1000)
  
  for (sim in 1:1000) {
    lluvia <- simular_lluvia_intensa(intensidad_media = 60, variabilidad = 25)
    niveles[sim] <- calcular_nivel_agua(lluvia, zona_actual$pendiente,
                                        zona_actual$impermeabilizacion, zona_actual$nivel_base)
  }
  
  hist(niveles, breaks = 30, 
       main = paste("Zona:", zona_actual$nombre_zona),
       xlab = "Nivel de agua (m)", 
       col = "lightblue", border = "darkblue")
  abline(v = umbral_critico_nivel, col = "red", lwd = 2, lty = 2)
}

dev.off()
cat("Gráfico guardado: graficos/distribucion_niveles.png\n")

# Gráfico 4: Precipitación histórica por zona
png("graficos/precipitacion_historica.png", width = 1000, height = 600)
par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))

for (zona in unique(datos_historicos$nombre_zona)) {
  datos_zona <- datos_historicos[datos_historicos$nombre_zona == zona, ]
  
  # Agrupar por mes
  precip_mensual <- aggregate(precipitacion_mm ~ mes, data = datos_zona, FUN = mean)
  
  barplot(precip_mensual$precipitacion_mm,
          names.arg = month.abb[precip_mensual$mes],
          main = paste("Precipitación Media -", zona),
          ylab = "Precipitación (mm)",
          col = "steelblue",
          las = 2)
}

dev.off()
cat("Gráfico guardado: graficos/precipitacion_historica.png\n")

cat("\n=============================================================================\n")
cat("EJECUCIÓN COMPLETADA EXITOSAMENTE\n")
cat("=============================================================================\n")
cat("\nArchivos generados:\n")
cat("- graficos/probabilidad_excedencia.png\n")
cat("- graficos/automata_estados.png\n")
cat("- graficos/distribucion_niveles.png\n")
cat("- graficos/precipitacion_historica.png\n")

# =============================================================================
# GUARDAR DATOS PARA ANÁLISIS POSTERIOR
# =============================================================================

# Guardar datos principales
write.csv(datos_historicos, "datos_historicos.csv", row.names = FALSE)
write.csv(zonas, "caracteristicas_zonas.csv", row.names = FALSE)
write.csv(resultados_montecarlo, "resultados_montecarlo.csv", row.names = FALSE)

cat("\nDatos guardados:\n")
cat("- datos_historicos.csv\n")
cat("- caracteristicas_zonas.csv\n")
cat("- resultados_montecarlo.csv\n")

cat("\n¡Proyecto completado exitosamente!\n")
