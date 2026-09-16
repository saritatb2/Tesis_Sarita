library(readxl)
GLMM_Sarita <- read_excel("GLMM_Sarita.xlsx")
View(GLMM_Sarita)
library(lme4)
library(tidyverse)
library(performance)
GLMM_Sarita <- GLMM_Sarita %>%
  mutate(
    # Variables continuas (numéricas)
    ageHabitatyears = as.numeric(ageHabitatyears),
    areaStand       = as.numeric(areaStand),
    
    # Variable discreta / categórica (Factor con sus 3 categorías)
    understory      = as.factor(understory),
    
    # Variable aleatoria (factor)
    Finca_Lote      = as.factor(Finca_Lote)
  )
levels(GLMM_Sarita$understory)
modelo <- glmer(
  R_Frug ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)

summary(modelo)
check_overdispersion(modelo)
summary(GLMM_Sarita$ageHabitatyears)
check_zeroinflation(modelo)

# Modelo de Poisson para la abundancia de frugívoros
modelo_A_frug <- glmer(
  A_Frug ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
summary(modelo_A_frug)
check_overdispersion(modelo_A_frug)
check_zeroinflation(modelo_A_frug)

#Binomial Negativo
# install.packages("MASS")
library(MASS)
modelo_A_frug_nb <- glmer.nb(
  A_Frug ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data = GLMM_Sarita
)
summary(modelo_A_frug_nb)
check_overdispersion(modelo_A_frug_nb)
check_zeroinflation(modelo_A_frug_nb)

#Granivoros
modelo_R_gran <- glmer(
  R_Gran ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
summary(modelo_R_gran)
check_overdispersion(modelo_R_gran)
check_zeroinflation(modelo_R_gran)

modelo_A_gran <- glmer(
  A_Gran ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
check_overdispersion(modelo_A_gran)
summary(modelo_A_gran)
check_zeroinflation(modelo_A_gran)

#Nectarivoros
modelo_R_nect <- glmer(
  R_Nect ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
summary(modelo_R_nect)
check_overdispersion(modelo_R_nect)
check_zeroinflation(modelo_R_nect)

modelo_A_nect <- glmer(
  A_Nect ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
check_overdispersion(modelo_A_nect)
check_zeroinflation(modelo_A_nect)
summary(modelo_A_nect)

#Invertivoros R_Binomial Negativa
modelo_R_inver_nb <- glmer.nb(
  R_Inver ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data = GLMM_Sarita
)
check_overdispersion(modelo_R_inver_nb)
check_zeroinflation(modelo_R_inver_nb)
summary(modelo_R_inver_nb)

modelo_A_inver_nb <- glmer.nb(
  A_Inver ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data = GLMM_Sarita
)

summary(modelo_A_inver_nb)
check_overdispersion(modelo_A_inver_nb)
check_zeroinflation(modelo_A_inver_nb)

#Omnívoros
modelo_R_omni <- glmer(
  R_Omni ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
check_overdispersion(modelo_R_omni)
check_zeroinflation(modelo_R_omni)
summary(modelo_R_omni)

modelo_A_omni_nb <- glmer.nb(
  A_Omni ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data = GLMM_Sarita
)
check_overdispersion(modelo_A_omni_nb)
check_zeroinflation(modelo_A_omni_nb)
summary(modelo_A_omni_nb)

#Vertiboros
modelo_R_vert <- glmer(
  R_Vert ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
check_overdispersion(modelo_R_vert)
check_zeroinflation(modelo_R_vert)
summary(modelo_R_vert)

modelo_A_vert <- glmer(
  A_Vert ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
check_overdispersion(modelo_A_vert)
check_zeroinflation(modelo_A_vert)
summary(modelo_A_vert)
#Estratos de forrajeo
#Suelo
modelo_R_Suelo <- glmer(
  R_Suelo ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
check_overdispersion(modelo_R_Suelo)
check_zeroinflation(modelo_R_Suelo)
summary(modelo_R_Suelo)

modelo_A_suelo_nb <- glmer.nb(
  A_Suelo ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data = GLMM_Sarita
)
check_overdispersion(modelo_A_suelo_nb)
check_zeroinflation(modelo_A_suelo_nb)
summary(modelo_A_suelo_nb)

#Sotobosque
modelo_R_soto <- glmer(
  R_Sotobosque ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
check_overdispersion(modelo_R_soto)
check_zeroinflation(modelo_R_soto)
summary(modelo_R_soto)

modelo_A_soto_nb <- glmer.nb(
  A_Sotobosque ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data = GLMM_Sarita
)

summary(modelo_A_soto_nb)
check_overdispersion(modelo_A_soto_nb)
check_zeroinflation(modelo_A_soto_nb)

#Arboreo
modelo_R_arboreo_nb <- glmer.nb(
  R_Arboreo ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data = GLMM_Sarita
)
check_overdispersion(modelo_R_arboreo_nb)
check_zeroinflation(modelo_R_arboreo_nb)
summary(modelo_R_arboreo_nb)

modelo_A_arboreo_nb <- glmer.nb(
  A_Arboreo ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data = GLMM_Sarita
)
check_overdispersion(modelo_A_arboreo_nb)
check_zeroinflation(modelo_A_arboreo_nb)
summary(modelo_A_arboreo_nb)

#Amplio
modelo_R_amplio <- glmer(
  R_Amplio ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
check_overdispersion(modelo_R_amplio)
check_zeroinflation(modelo_R_amplio)
summary(modelo_R_amplio)

modelo_A_amplio_nb <- glmer.nb(
  A_Amplio ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data = GLMM_Sarita
)

summary(modelo_A_amplio_nb)
check_overdispersion(modelo_A_amplio_nb)
check_zeroinflation(modelo_A_amplio_nb)

#Estrategia de Forrajeo
modelo_R_rebuscador <- glmer(
  R_Rebuscador ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
summary(modelo_R_rebuscador)
check_overdispersion(modelo_R_rebuscador)
check_zeroinflation(modelo_R_rebuscador)

modelo_A_rebuscador_nb <- glmer.nb(
  A_Rebuscador ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data = GLMM_Sarita
)
check_overdispersion(modelo_A_rebuscador_nb)
check_zeroinflation(modelo_A_rebuscador_nb)
summary(modelo_A_rebuscador_nb)

#Visitante Floral
modelo_R_vis_floral <- glmer(
  R_Visitante_floral ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
summary(modelo_R_vis_floral)
check_overdispersion(modelo_R_vis_floral)
check_zeroinflation(modelo_R_vis_floral)

modelo_A_vis_floral <- glmer(
  A_Visitante_floral ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
summary(modelo_A_vis_floral)
check_overdispersion(modelo_A_vis_floral)
check_zeroinflation(modelo_A_vis_floral)

#Limpiador
modelo_A_limpiador_nb <- glmer.nb(
  A_Limpiador ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data = GLMM_Sarita
)
check_overdispersion(modelo_A_limpiador_nb)
check_zeroinflation(modelo_A_limpiador_nb)
summary(modelo_A_limpiador_nb)

#Atrapador Aereo
modelo_R_atrap_aereo_nb <- glmer.nb(
  R_Atrapador_aereo ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data = GLMM_Sarita
)
check_overdispersion(modelo_R_atrap_aereo_nb)
check_zeroinflation(modelo_R_atrap_aereo_nb)
summary(modelo_R_atrap_aereo_nb)

modelo_A_atrap_aereo_nb <- glmer.nb(
  A_Atrapador_aereo ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data = GLMM_Sarita
)
summary(modelo_A_atrap_aereo_nb)
check_overdispersion(modelo_A_atrap_aereo_nb)
check_zeroinflation(modelo_A_atrap_aereo_nb)

#Depredador
modelo_R_depredador <- glmer(
  R_Depredador ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
summary(modelo_R_depredador)
check_overdispersion(modelo_R_depredador)
check_zeroinflation(modelo_R_depredador)

modelo_A_depredador <- glmer(
  A_Depredador ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
summary(modelo_A_depredador)
check_overdispersion(modelo_A_depredador)
check_zeroinflation(modelo_A_depredador)

#Carroñero
#Renombrar
names(GLMM_Sarita)[names(GLMM_Sarita) == "R_Carroñero"] <- "R_Carronero"
names(GLMM_Sarita)[names(GLMM_Sarita) == "A_Carroñero"] <- "A_Carronero"

modelo_R_carronero <- glmer(
  R_Carronero ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
summary(modelo_R_carronero)
check_overdispersion(modelo_R_carronero)
check_zeroinflation(modelo_R_carronero)

modelo_A_carronero <- glmer(
  A_Carronero ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
summary(modelo_A_carronero)
check_overdispersion(modelo_A_carronero)
check_zeroinflation(modelo_A_carronero)

#HABITAT
#Shrland
modelo_R_shrland <- glmer(
  R_ShrLand ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
summary(modelo_R_shrland)
check_overdispersion(modelo_R_shrland)
check_zeroinflation(modelo_R_shrland)

modelo_A_shrland_nb <- glmer.nb(
  A_ShrLand ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data = GLMM_Sarita
)
check_overdispersion(modelo_A_shrland_nb)
check_zeroinflation(modelo_A_shrland_nb)
summary(modelo_A_shrland_nb)

#WooLand
modelo_R_wooland <- glmer(
  R_WooLand ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
check_overdispersion(modelo_R_wooland)
check_zeroinflation(modelo_R_wooland)
summary(modelo_R_wooland)

modelo_A_wooland <- glmer(
  A_WooLand ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
summary(modelo_A_wooland)
check_overdispersion(modelo_A_wooland)
check_zeroinflation(modelo_A_wooland)

#Forest
modelo_R_forest_nb <- glmer.nb(
  R_Forest ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data = GLMM_Sarita
)
check_overdispersion(modelo_R_forest_nb)
check_zeroinflation(modelo_R_forest_nb)
summary(modelo_R_forest_nb)

modelo_A_forest_nb <- glmer.nb(
  A_Forest ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data = GLMM_Sarita
)
summary(modelo_A_forest_nb)
check_overdispersion(modelo_A_forest_nb)
check_zeroinflation(modelo_A_forest_nb)

#HumMod
modelo_R_hummod <- glmer(
  R_HumMod ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
summary(modelo_R_hummod)
check_overdispersion(modelo_R_hummod)
check_zeroinflation(modelo_R_hummod)

modelo_A_hummod <- glmer(
  A_HumMod ~ understory + ageHabitatyears + areaStand + (1 | Finca_Lote), 
  data   = GLMM_Sarita, 
  family = poisson(link = "log")
)
summary(modelo_A_hummod)
check_overdispersion(modelo_A_hummod)
check_zeroinflation(modelo_A_hummod)
