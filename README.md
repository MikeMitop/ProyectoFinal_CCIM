# Proyecto Final Integrador - CCIM

## Gestión y Predicción de Riesgo por Inundaciones Urbanas
### Simulación, Clasificación y Protocolos de Alerta

## Descripción

Este proyecto implementa un sistema integral para el análisis y predicción de riesgo de inundaciones urbanas. Combina técnicas de análisis estadístico, modelado probabilístico Bayesiano, simulación Monte Carlo, y un autómata finito para la gestión de protocolos de alerta.

## Contenido del Proyecto

El sistema está desarrollado completamente en R e incluye:

### A. Preprocesamiento y Análisis Exploratorio de Datos
- Generación de datos hidrometeorológicos simulados (precipitación diaria, niveles de agua)
- Caracterización de 6 zonas urbanas con diferentes atributos (pendiente, impermeabilización, nivel base)
- Cálculo de estadísticas descriptivas: medias, percentiles, máximos por período
- Filtrado por zona y fecha

### B. Clasificador Probabilístico Bayesiano
- Variables predictoras:
  - Lluvia acumulada 24h (mm)
  - Pendiente del terreno (%)
  - Porcentaje de impermeabilización (%)
  - Nivel base de agua (m)
- Estimación de probabilidades a priori y condicionales
- Suavizado de Laplace para manejo de valores ausentes
- Validación con datos de prueba (70% entrenamiento, 30% prueba)

### C. Simulación Monte Carlo
- 10,000 simulaciones por zona
- Modelado de escenarios de lluvia intensa (distribución gamma)
- Estimación de probabilidad de excedencia de umbrales críticos
- Cálculo de error estándar e intervalos de confianza al 95%

### D. Protocolo de Alerta (Autómata Finito)
- **Estados del sistema:**
  1. **Normal**: Condiciones normales, sin riesgo significativo
  2. **Vigilancia**: Riesgo moderado, monitoreo activo
  3. **Alerta**: Riesgo alto, preparación para evacuación
  4. **Evacuación**: Riesgo crítico, evacuación activa

- **Umbrales de transición:**
  - Normal → Vigilancia: P(inundación) > 20%
  - Vigilancia → Alerta: P(inundación) > 40%
  - Alerta → Evacuación: P(inundación) > 60%

### E. Informe y Recomendaciones
- Resumen ejecutivo de resultados
- Recomendaciones operativas
- Prioridades de evacuación por zona
- Medidas estructurales y no estructurales

## Requisitos

- R versión 4.0 o superior
- No requiere paquetes adicionales (usa solo funciones base de R)

## Uso

```bash
# Ejecutar el script principal
Rscript proyecto_inundaciones.R
```

## Archivos Generados

Al ejecutar el script se generan:

### Datos (CSV)
- `datos_historicos.csv`: Registros históricos simulados
- `caracteristicas_zonas.csv`: Atributos de cada zona
- `resultados_montecarlo.csv`: Resultados de simulaciones

### Gráficos (PNG)
- `graficos/probabilidad_excedencia.png`: Probabilidad de excedencia por zona
- `graficos/automata_estados.png`: Diagrama del autómata finito
- `graficos/distribucion_niveles.png`: Distribución de niveles de agua
- `graficos/precipitacion_historica.png`: Precipitación histórica por zona

## Zonas de Estudio

| Zona | Pendiente (%) | Impermeabilización (%) | Tipo de Suelo |
|------|---------------|------------------------|---------------|
| Centro | 2.5 | 85 | Urbano |
| Norte | 5.0 | 60 | Residencial |
| Sur | 1.5 | 45 | Rural |
| Este | 3.0 | 70 | Mixto |
| Oeste | 4.0 | 55 | Residencial |
| Industrial | 1.0 | 90 | Industrial |

## Nota sobre los Datos

Los datos utilizados son **simulados** para demostrar el funcionamiento del sistema. Para implementación real, se recomienda utilizar datos del IDEAM o estaciones hidrométricas locales.

## Autor

Proyecto Final - Ciencias de la Computación e Inteligencia de Máquinas (CCIM)

## Licencia

Este proyecto es de uso académico.