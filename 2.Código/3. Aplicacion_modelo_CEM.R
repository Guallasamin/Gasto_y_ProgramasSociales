
########################################################################
#### Proyecto: Determinación del efecto de la entrega de programas sociales
#### Documento: Aplicación de Modelo MATCH
#### Elaboración: Econ. Guallasamin Miño Jonathan
########################################################################

# Se establece la carpeta de trabajo

setwd("/Users/jonathanguallasamin/Dropbox/Tareas_Jonathan/2023.07.07 Match_ProgramasSocialesGasto/bases/") 

# Cargar de paquetes
library("foreign")
library("openxlsx")
library("gtsummary")
library("MatchIt")
library("ggplot2")
library("ggeffects")
library("haven")
library("dplyr")


## Modelo de match: https://cran.r-project.org/web/packages/MatchIt
### CEM model: https://cran.r-project.org/web/packages/cem/vignettes/cem.pdf
### Uso de pesos de encuestas: https://cran.r-project.org/web/packages/MatchIt/vignettes/sampling-weights.html

### Para la comprobación del equilibrio de dos grupos de comparación, 
#se emplea la medida de desequilibrio multivariable L1, 
#cuyo tamaño depende del conjunto de datos y de las covariables seleccionadas. 
#L1 varía de 0 a 1, donde 0 y 1 significan un equilibrio global perfecto y 
#un desequilibrio máximo, respectivamente, y un valor más grande representa 
#un desequilibrio más grande entre dos grupos. 
#Una buena coincidencia generalmente trae una reducción sustancial de L1

modelo <- fread("modelo_final.csv")

#https://bmchealthservres.biomedcentral.com/articles/10.1186/s12913-021-06328-0
#Page 4

# Variable 'ent' (estado)
modelo$ent <- factor(modelo$ent)

# Variable 'area' (área geográfica)
modelo$area <- factor(modelo$area)

# Variable 'edad_jef' (edad del jefe de hogar)
modelo$edad_jef <- factor(modelo$edad_jef)

# Variable 'educ_jef' (nivel de educación del jefe de hogar)
modelo$educ_jef <- factor(modelo$educ_jef)

# Variable 'ocupacion' (ocupación del jefe de hogar)
modelo$ocupacion <- factor(modelo$ocupacion)

# Variable 'sexo_jefe' (sexo del jefe de hogar)
modelo$sexo_jefe <- factor(modelo$sexo_jefe)

#########Some check of the dataset 
# Count missing values in each variable
missing_counts <- modelo %>%
  summarise_all(~ sum(is.na(.)))
# Print the results
print(missing_counts)

#Deciels of income variabel
plot(density(pob_trabajo$ing_gubnt))
deciles <- pob_trabajo %>%
  mutate(decile_group = cut(ing_gubnt, breaks = quantile(ing_gubnt, probs = seq(0, 1, 0.1), na.rm = TRUE), include.lowest = TRUE)) %>%
  group_by(decile_group) %>%
  summarise(average = mean(ing_gubnt, na.rm = TRUE))
# Print the results
print(deciles)


##################MODELO DE MATCHING

cemMODEL <- matchit(tratamiento ~ ent + area + edad_jef + educ_jef + sum_ninios + ocupacion 
                    + ing_gubnt + tot_integ.x + sum_ninios + sum_adultos 
                    + sum_amayores + sum_escuela + sexo_jefe, data = pob_trabajo,
                                         s.weights = "factor.x",
                                         method = "cem", 
                                         cutpoint = list(ing_gubnt="q100"))

#summary(cemMODEL)

#plot(summary(cemMODEL))

m.data <- match.data(cemMODEL)

save(m.data, file= "match_final.RData")
