# ============================================================================
# GLMM_interacciones.R
#
# Modelos Lineales Generalizados de Efectos Mixtos (GLMM) para evaluar
# interacciones entre manejo del sotobosque, edad de la plantación y área
# del rodal sobre la riqueza y abundancia de aves por rasgo ecológico.
#
# Datos: GLMM_Sarita.xlsx
# Paquetes: readxl, lme4, tidyverse, performance, MASS, emmeans, DHARMa
# ============================================================================

rm(list = ls())

library(readxl)
library(lme4)
library(tidyverse)
library(performance)
library(MASS)
library(emmeans)
library(DHARMa)

# ----------------------------------------------------------------------------
# 1. CARGA Y LIMPIEZA DE DATOS
# ----------------------------------------------------------------------------

GLMM_Sarita <- read_excel("GLMM_Sarita.xlsx") %>%
  mutate(
    ageHabitatyears = as.numeric(gsub(",", ".", ageHabitatyears)),
    areaStand       = as.numeric(gsub(",", ".", areaStand)),
    understory      = as.factor(understory),
    Finca_Lote      = as.factor(Finca_Lote)
  ) %>%
  # Se excluyen los registros con edad de plantación >= 20 años (valor 23.4,
  # fuera del rango esperado para las categorías de plantación evaluadas)
  filter(is.na(ageHabitatyears) | ageHabitatyears != 23.4)

# Renombrar columnas con caracteres especiales
names(GLMM_Sarita)[names(GLMM_Sarita) == "R_Carroñero"] <- "R_Carronero"
names(GLMM_Sarita)[names(GLMM_Sarita) == "A_Carroñero"] <- "A_Carronero"

# Unificar nombres de lote con errores de digitación (mismo lote, distinta grafía)
GLMM_Sarita <- GLMM_Sarita %>%
  mutate(Finca_Lote = as.factor(recode(as.character(Finca_Lote),
                                       "Finca_cedral_lote6" = "Finca_Cedral_lote6",
                                       "Finca_Cedra_lote20" = "Finca_Cedral_lote20"
  )))

# Estandarizar edad y área (media = 0, DE = 1) para favorecer la convergencia
# de los modelos con interacciones. Los NA de edad/área se conservan y son
# omitidos automáticamente por glmer/glmer.nb al ajustar.
GLMM_Sarita <- GLMM_Sarita %>%
  mutate(
    age_z  = as.numeric(scale(ageHabitatyears)),
    area_z = as.numeric(scale(areaStand))
  )

table(GLMM_Sarita$understory, useNA = "ifany")

# ----------------------------------------------------------------------------
# 2. VARIABLES RESPUESTA Y FAMILIA DE DISTRIBUCIÓN
# ----------------------------------------------------------------------------

poisson_resp <- c(
  "R_Frug",
  "R_Gran", "A_Gran",
  "R_Nect", "A_Nect",
  "R_Omni",
  "R_Vert", "A_Vert",
  "R_Suelo",
  "R_Sotobosque",
  "R_Amplio",
  "R_Rebuscador",
  "R_Visitante_floral", "A_Visitante_floral",
  "R_Depredador", "A_Depredador",
  "R_Carronero", "A_Carronero",
  "R_ShrLand",
  "R_WooLand", "A_WooLand",
  "R_HumMod", "A_HumMod",
  "R_OpenHab"
)

nb_resp <- c(
  "A_Frug",
  "R_Inver", "A_Inver",
  "A_Omni",
  "A_Suelo",
  "A_Sotobosque",
  "R_Arboreo", "A_Arboreo",
  "A_Amplio",
  "A_Rebuscador",
  "R_Limpiador", "A_Limpiador",
  "R_Atrapador_aereo", "A_Atrapador_aereo",
  "A_ShrLand",
  "R_Forest", "A_Forest",
  "R_DensHab", "A_DensHab",
  "R_SemiHab", "A_SemiHab",
  "A_OpenHab"
)

respuestas <- tibble(
  resp = c(poisson_resp, nb_resp),
  fam  = c(rep("poisson", length(poisson_resp)), rep("nb", length(nb_resp)))
)

# Se excluye Carroñero por tamaño de muestra insuficiente para estimar
# interacciones (10-15 individuos en total, ~97% de ceros)
excluir <- c("R_Carronero", "A_Carronero")
respuestas <- respuestas %>% filter(!resp %in% excluir)

# Umbral orientativo (~10 eventos por parámetro del modelo triple) para
# marcar respuestas con pocos individuos en la tabla de resultados
min_eventos <- 100

# ----------------------------------------------------------------------------
# 3. LAS CUATRO COMBINACIONES DE INTERACCIÓN
#    (la variable que no participa en la interacción entra como aditiva)
# ----------------------------------------------------------------------------

interacciones <- c(
  soto_x_edad      = "understory * age_z + area_z",
  soto_x_area      = "understory * area_z + age_z",
  area_x_edad      = "area_z * age_z + understory",
  soto_x_edad_area = "understory * age_z * area_z"
)

# Modelo aditivo (sin interacciones): referencia para las tres interacciones dobles
base_rhs <- "understory + age_z + area_z"

# Modelo con las tres interacciones dobles: referencia para probar SOLO la
# interacción triple (evita confundir su efecto con el de las dobles)
dobles_rhs <- "understory * age_z + understory * area_z + age_z * area_z"

# ----------------------------------------------------------------------------
# 4. FUNCIÓN PARA AJUSTAR UN MODELO
# ----------------------------------------------------------------------------

ajustar <- function(resp, fam, rhs) {
  f <- as.formula(paste(resp, "~", rhs, "+ (1 | Finca_Lote)"))
  if (fam == "nb") {
    eval(bquote(glmer.nb(.(f), data = GLMM_Sarita)))
  } else {
    eval(bquote(glmer(.(f), data = GLMM_Sarita, family = poisson(link = "log"))))
  }
}

# ----------------------------------------------------------------------------
# 5. AJUSTE DE TODOS LOS MODELOS
# ----------------------------------------------------------------------------

modelos    <- list()
resultados <- list()

for (i in seq_len(nrow(respuestas))) {
  resp <- respuestas$resp[i]
  fam  <- respuestas$fam[i]
  message("== ", resp, " (", fam, ") ==")
  
  m_base   <- tryCatch(ajustar(resp, fam, base_rhs),   error = function(e) NULL)
  m_dobles <- tryCatch(ajustar(resp, fam, dobles_rhs), error = function(e) NULL)
  
  for (nm in names(interacciones)) {
    clave <- paste(resp, nm, sep = "__")
    
    m <- tryCatch(
      ajustar(resp, fam, interacciones[[nm]]),
      error = function(e) { message("  ERROR en ", clave, ": ", e$message); NULL }
    )
    modelos[[clave]] <- m
    if (is.null(m)) next
    
    # Prueba de razón de verosimilitud (test conjunto de los coeficientes de
    # la interacción). Dobles: contra el modelo aditivo. Triple: contra el
    # modelo con las tres dobles, para aislar el aporte de la interacción
    # de tercer orden.
    m_ref <- if (nm == "soto_x_edad_area") m_dobles else m_base
    p_lrt <- tryCatch(
      if (!is.null(m_ref)) anova(m_ref, m)$`Pr(>Chisq)`[2] else NA_real_,
      error = function(e) NA_real_
    )
    
    disp <- tryCatch(check_overdispersion(m)$dispersion_ratio, error = function(e) NA_real_)
    zi   <- tryCatch(check_zeroinflation(m)$ratio,             error = function(e) NA_real_)
    
    resultados[[clave]] <- tibble(
      respuesta     = resp,
      familia       = fam,
      interaccion   = nm,
      AIC           = AIC(m),
      AIC_base      = if (!is.null(m_base)) AIC(m_base) else NA_real_,
      delta_AIC     = AIC(m) - if (!is.null(m_base)) AIC(m_base) else NA_real_,
      p_interaccion = p_lrt,
      disp_ratio    = disp,
      zero_ratio    = zi,
      singular      = isSingular(m),
      n_eventos     = sum(GLMM_Sarita[[resp]], na.rm = TRUE),
      pocos_eventos = sum(GLMM_Sarita[[resp]], na.rm = TRUE) < min_eventos
    )
  }
}

tabla_resumen <- bind_rows(resultados) %>%
  group_by(interaccion) %>%
  # Corrección por comparaciones múltiples (FDR) dentro de cada tipo de interacción
  mutate(p_FDR = p.adjust(p_interaccion, method = "fdr")) %>%
  ungroup()

print(tabla_resumen, n = Inf, width = Inf)

cat("\n== Interacciones con p < 0.05 (sin corregir) ==\n")
tabla_resumen %>%
  filter(p_interaccion < 0.05) %>%
  arrange(p_interaccion) %>%
  print(n = Inf)

# ----------------------------------------------------------------------------
# 6. GUARDAR RESULTADOS DEL AJUSTE
# ----------------------------------------------------------------------------

write.csv(tabla_resumen, "resumen_modelos_interaccion.csv", row.names = FALSE)
saveRDS(modelos, "modelos_interaccion.rds")

# Tabla de coeficientes: una fila por término de cada modelo. "absent
# understory" es el nivel de referencia y no aparece como fila.
tabla_coef <- do.call(rbind, lapply(names(modelos), function(clave) {
  m <- modelos[[clave]]
  if (is.null(m)) return(NULL)
  cf <- as.data.frame(coef(summary(m)))
  data.frame(
    respuesta   = sub("__.*$", "", clave),
    interaccion = sub("^.*__", "", clave),
    termino     = rownames(cf),
    estimado    = cf$Estimate,
    error_est   = cf$`Std. Error`,
    z           = cf$`z value`,
    p           = cf$`Pr(>|z|)`,
    row.names   = NULL
  )
}))
write.csv(tabla_coef, "coeficientes_modelos_interaccion.csv", row.names = FALSE)

cat("\n=== Ajuste de modelos completado ===\n")

# ----------------------------------------------------------------------------
# 7. COMPARACIONES ENTRE TIPOS DE SOTOBOSQUE
#    (para las respuestas donde sotobosque x edad resultó significativa)
# ----------------------------------------------------------------------------

sig <- tabla_resumen %>%
  filter(interaccion == "soto_x_edad", p_interaccion < 0.05) %>%
  pull(respuesta)

# Pendiente del efecto de la edad dentro de cada tipo de sotobosque
pendientes <- do.call(rbind, lapply(sig, function(r) {
  m <- modelos[[paste0(r, "__soto_x_edad")]]
  d <- as.data.frame(emtrends(m, ~ understory, var = "age_z"))
  data.frame(
    respuesta  = r,
    sotobosque = d$understory,
    pendiente  = d$age_z.trend,
    EE         = d$SE,
    IC_inf     = d$asymp.LCL,
    IC_sup     = d$asymp.UCL
  )
}))

# Comparación entre pares de tipos de sotobosque (corrección de Tukey)
comparaciones <- do.call(rbind, lapply(sig, function(r) {
  m <- modelos[[paste0(r, "__soto_x_edad")]]
  p <- as.data.frame(pairs(emtrends(m, ~ understory, var = "age_z")))
  data.frame(respuesta = r, contraste = p$contrast, estimado = p$estimate, p_tukey = p$p.value)
}))

write.csv(pendientes, "pendientes_por_sotobosque.csv", row.names = FALSE)
write.csv(comparaciones, "comparaciones_entre_sotobosques.csv", row.names = FALSE)

# ----------------------------------------------------------------------------
# 8. VERIFICACIONES ADICIONALES PARA LAS INTERACCIONES SIGNIFICATIVAS
#    (multicolinealidad con VIF y diagnóstico de residuales con DHARMa)
# ----------------------------------------------------------------------------

# --- VIF: multicolinealidad ---
# Regla orientativa: VIF > 5 es señal de alerta. Es normal que el término de
# interacción en sí (con ":") tenga VIF más alto; lo relevante son los
# términos principales (understory, age_z, area_z).
vif_resultados <- lapply(sig, function(r) {
  m <- modelos[[paste0(r, "__soto_x_edad")]]
  tryCatch({
    v <- as.data.frame(check_collinearity(m))
    cbind(respuesta = r, v)
  }, error = function(e) { message("Error de VIF en ", r, ": ", e$message); NULL })
})
tabla_vif <- as_tibble(bind_rows(vif_resultados))
write.csv(tabla_vif, "vif_interacciones_significativas.csv", row.names = FALSE)

cat("\n== VIF de los términos principales (sin interacción) ==\n")
tabla_vif %>%
  filter(!grepl(":", Term)) %>%
  dplyr::select(respuesta, Term, VIF) %>%
  print(n = Inf)

# --- DHARMa: diagnóstico de residuales ---
# Genera un PDF con dos gráficos por modelo (QQ plot y residual vs. predicho)
pdf("diagnostico_dharma_soto_x_edad.pdf", width = 10, height = 5)
for (r in sig) {
  m <- modelos[[paste0(r, "__soto_x_edad")]]
  tryCatch({
    sim <- simulateResiduals(m, plot = FALSE)
    plot(sim, main = r)
  }, error = function(e) message("No se pudo simular para ", r, ": ", e$message))
}
dev.off()

cat("\n=== LISTO: verificaciones completadas ===\n")

# ----------------------------------------------------------------------------
# 9. USO: consultar un modelo en particular
# ----------------------------------------------------------------------------
# summary(modelos[["R_Sotobosque__soto_x_edad"]])
# names(modelos)   # lista de todos los modelos ajustados