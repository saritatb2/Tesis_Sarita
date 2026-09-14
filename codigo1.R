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
