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
