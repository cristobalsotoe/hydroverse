calcular_estadisticas <- function(x, y, na.rm = TRUE) {
  # Si na.rm es TRUE, eliminar valores NA de ambos vectores manteniendo pares
  if (na.rm) {
    # Identificar posiciones sin NA en ninguno de los dos vectores
    valid_indices <- !is.na(x) & !is.na(y)
    x <- x[valid_indices]
    y <- y[valid_indices]
  }
  
  # Verificar que los vectores tienen la misma longitud después de limpiar
  if (length(x) != length(y)) {
    stop("Los vectores x e y deben tener la misma longitud después de remover NAs.")
  }
  
  # Verificar que quedan observaciones válidas
  if (length(x) == 0) {
    stop("No quedan observaciones válidas después de remover NAs.")
  }
  
  # Calcular las diferencias (residuos)
  residuals <- x - y
  
  # Calcular los estadísticos
  residual_mean <- mean(residuals, na.rm = FALSE)  # Ya no hay NAs
  absolute_residual_mean <- mean(abs(residuals), na.rm = FALSE)
  residual_std_dev <- sd(residuals, na.rm = FALSE)
  sum_of_squares <- sum(residuals^2, na.rm = FALSE)
  rms_error <- sqrt(mean(residuals^2, na.rm = FALSE))
  min_residual <- min(residuals, na.rm = FALSE)
  max_residual <- max(residuals, na.rm = FALSE)
  num_observations <- length(x)
  range_in_observations <- max(x, na.rm = FALSE) - min(x, na.rm = FALSE)
  
  # Calcular los estadísticos normalizados
  scaled_residual_std_dev <- residual_std_dev / range_in_observations
  scaled_absolute_residual_mean <- absolute_residual_mean / range_in_observations
  scaled_rms_error <- rms_error / range_in_observations
  scaled_residual_mean <- residual_mean / range_in_observations
  
  # Crear una tabla con los resultados en español
  estadisticas <- data.frame(
    "Estadístico" = c(
      "Media de residuos",
      "Media residual absoluta [m]",
      "Residual de desviación estandar [m]",
      "Suma de residuos cuadrados [m2]",
      "RMS Error [m]",
      "Residuo mínimo [m]",
      "Residuo máximo [m]",
      "Número de observaciones",
      "Rango de observaciones [m]",
      "Residual de desviación estandar normalizado [-]",
      "Media residual absoluta normalizado [-]",
      "RMS Error normalizado [-]",
      "Media de residuo normalizado [-]"
    ),
    "Abreviatura" = c(
      "RM",      # Media residuos
      "MAE",     # Media residual absoluta
      "SD",      # Desviación estándar
      "SSR",     # Suma cuadrados
      "RMSE",    # RMS Error
      "MinR",    # Residuo mínimo
      "MaxR",    # Residuo máximo
      "n",       # Número observaciones
      "Rango",   # Rango observaciones
      "nSD",     # SD normalizado
      "nMAE",    # MAE normalizado
      "nRMSE",   # RMSE normalizado
      "nRM"      # Media residuos normalizado
    ),
    "Valor" = c(
      residual_mean,
      absolute_residual_mean,
      residual_std_dev,
      sum_of_squares,
      rms_error,
      min_residual,
      max_residual,
      num_observations,
      range_in_observations,
      scaled_residual_std_dev,
      scaled_absolute_residual_mean,
      scaled_rms_error,
      scaled_residual_mean
    )
  )
  
  return(estadisticas)
}
