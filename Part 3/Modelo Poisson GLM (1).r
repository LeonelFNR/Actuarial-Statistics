
# Cargar datos
awards <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
awards <- as.data.frame(awards)

# Convertir la variable de programa a factor
awards <- within(awards, {
  prog <- factor(prog, levels = 1:3, labels = c("General", "Academic", "Vocational"))
})

view(awards)
summary(awards)

ggplot(awards, aes(x = math, y = num_awards, color = prog)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(title = "Número de Premios en función de la nota de Matemáticas",
       x = "Nota en Matemáticas",
       y = "Número de Premios",
       color = "Programa") +
  theme_minimal() +
  theme(legend.position = "top")

awards %>%
  ggplot(aes(x = num_awards)) +
  geom_histogram() +
  theme_bw() +
  ylab("Conteo de premios") +
  xlab("Número de premios")


  
  
  # Ajuste de modelos GLM de Poisson
  fit_glm_1 <- glm(num_awards ~ math, data=awards, family=poisson)
  fit_glm_2 <- glm(num_awards ~ math + prog, data=awards, family=poisson)
  
  # Resumen de los modelos
  summary(fit_glm_1)
  summary(fit_glm_2)
  
  anova(fit_glm_1,fit_glm_2)
  
  
  
  
  ##HAY SOBREDISPERSIÓN ? 

  # 1. Comparación entre media y varianza
  mean_awards <- mean(awards$num_awards)
  var_awards <- var(awards$num_awards)
  cat("Media de premios:", mean_awards, " - Varianza de premios:", var_awards, "\n")
  
  # 2. Relación entre deviance y grados de libertad
  deviance_ratio <- deviance(fit_glm_2) / df.residual(fit_glm_2)
  cat("Razón Deviance/GL:", deviance_ratio, "\n")
  
  
  if (deviance_ratio > 1.5) {
    cat("Indicación de sobre-dispersión en el modelo.\n")
  } else {
    cat("No hay evidencia fuerte de sobre-dispersión.\n")
  }
  
  
  # Check if the model fits the data, this is the null hypothesis.
  with(
    fit_glm_2,
    cbind(
      res.deviance = deviance,
      df = df.residual,
      p = pchisq(deviance, df.residual, lower.tail = FALSE)
    )
  )
  
  # MODELOS ERRORES ESTÁNDAR ROBUSTOS
  
  cov.m1 <- sandwich::vcovHC(fit_glm_2, type = "HC0")
  std.err <- sqrt(diag(cov.m1))
  r.est <- cbind(
    Estimate = coef(fit_glm_2),
    "EE heterocedástico" = std.err,
    "Pr(>|z|)" = 2 * pnorm(abs(coef(fit_glm_2) / std.err), lower.tail = FALSE),
    LL = coef(m1) - 1.96 * std.err,
    UL = coef(m1) + 1.96 * std.err
  )
  r.est
  
  #Modelo quasipoisson
  
  modeliq <- glm(num_awards ~ math + prog, family = quasipoisson, data = awards)
  summary(modeliq)
  
  #Modelo binomial negativo
  modelinb <- glm.nb(num_awards ~ math + prog, data = awards)
  summary(modelinb)
  
  #Comparación de los modelos: 
  drop_in_dev <- anova(modeliq, modelinb, test = "F"); drop_in_dev # es mejor el modelo de la BN 
  
  # Calculamos los valores predictos
  
  awards$pred<-predict(fit_glm_2, type = "response")
  
  awards %>%
    ggplot(aes(pred)) + geom_histogram() + theme_bw()
  
  
  # Ordenamos el dataset por programa y luego por math
  awards <- data %>% arrange(prog, math)
  View(data)

  
  # Hacemos el gráfico de medias marginales
  ggplot(awards, aes(x = math, y = pred, color = prog)) +
    # plots the line of the marginal means
    geom_line(size = 1) +
    # plots the fitted values, we use 'jitter' to make it prettier
    geom_point(aes(y = num_awards), alpha = .5, position = position_jitter(h = .2)) +
    labs(title = "Medias marginales ajustadas", x = "Puntaje en matemáticas", y = "Numero previsto de premios") +
    theme_bw()
  
  
  # Diagnóstico del modelo 
  
  
  par(mfrow=c(2,2)) # Configurar panel de gráficos
  plot(fit_glm_2) # Gráficos de diagnóstico estándar
  
  
  # Extraer residuos deviance
  residuos_dev <- residuals(fit_glm_2, type="deviance")
  
  # Histogramas y gráficos de dispersión
  hist(residuos_dev, main="Histograma de Residuos Deviance", col="lightblue", breaks=20)
  qqnorm(residuos_dev)
  qqline(residuos_dev, col="red", lwd=2)
  
  # Identificación de valores atípicos
  outliers <- which(abs(residuos_dev) > 2) # Considerar valores fuera de ÃÂ±2
  cat("Observaciones con residuos altos:", outliers, "\n")  
  
  # Cálculo de medidas de influencia
  cooks_d <- cooks.distance(fit_glm_2)
  hat_values <- hatvalues(fit_glm_2)
  
  # Identificación de puntos influyentes
  threshold_cooks <- 4 / (nrow(awards) - length(coef(fit_glm_2)))
  outliers <- which(cooks_d > threshold_cooks)
  outliers
  
  
  cat("Puntos influyentes identificados:", outliers, "\n")
  
  # Visualización de Cooks Distance
  plot(cooks_d, type="h", col="red", lwd=2, ylab="Cook's Distance", xlab="Observación", 
       main="Distancia de Cook")
  abline(h=threshold_cooks, col="blue", lty=2)  
  
  # Eliminación de outliers y ajuste de nuevo modelo
  awards_clean <- awards[-outliers, ]
  fit_glm_2_clean <- glm(num_awards ~ math + prog, data=awards_clean, family=poisson)
  summary(fit_glm_2_clean)
  
  
  ## PREDICCIONES DEL MODELO SIN OUTLIERS
  
  awards_clean$predicted <- predict(fit_glm_2_clean, type="response")
  
  # Gráfico con jitter para visualizar mejor las coincidencias
  ggplot(awards_clean, aes(x = math, y = predicted, color = prog)) +
    geom_point(aes(y = num_awards), alpha = 0.5, position = position_jitter(h = 0.2)) +
    geom_line(size = 1) +
    labs(title = "Predicción del Número de Premios sin Outliers",
         x = "Nota en Matemáticas",
         y = "Número Esperado de Premios") +
    theme_minimal()
  
  
  
 
  
  
  
  # Añadir predicciones de ambos modelos al dataset limpio
  awards$pred_full <- predict(fit_glm_2, type = "response")
  awards_clean$pred_clean <- predict(fit_glm_2_clean, type = "response")
  
  # Gráfico comparativo
  ggplot() +
    # Datos originales
    geom_point(data = awards, aes(x = math, y = num_awards, color = prog),
               alpha = 0.4, position = position_jitter(h = 0.2)) +
    
    # Línea de predicción con todos los datos
    geom_line(data = awards, aes(x = math, y = pred_full, color = prog),
              size = 1, linetype = "dashed") +
    
    # Línea de predicción sin outliers
    geom_line(data = awards_clean, aes(x = math, y = pred_clean, color = prog),
              size = 1) +
    
    labs(title = "Comparación de modelos Poisson: con y sin outliers",
         x = "Nota en Matemáticas",
         y = "Número esperado de premios",
         color = "Programa") +
    theme_minimal()
  