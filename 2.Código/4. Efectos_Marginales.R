
########################################################################
#### Proyecto: Determinación del efecto de la entrega de programas sociales
#### Documento: Obtención de Efectos Marginales
#### Elaboración: Econ. Guallasamin Miño Jonathan
########################################################################

library("ggplot2")
library("marginaleffects")
library("ggeffects")
library("scales")
library("patchwork")
library("stargazer")
library("openxlsx")

# Se establece la carpeta de trabajo

setwd("/Users/jonathanguallasamin/Dropbox/Tareas_Jonathan/2023.07.07 Match_ProgramasSocialesGasto/bases/") 

load("match_final.RData")

variables <- c("cereales", "carnes", "pescado", "leche", "huevo",
               "aceites", "tuberculo", "verduras", "frutas", "azucar",
               "cafe", "especias", "otros_alim", "bebidas", "ali_fuera",
               "tabaco", "vestido", "calzado", "alquiler", "pred_cons",
               "agua", "energia", "cuidados", "utensilios", "enseres",
               "atenc_ambu", "hospital", "medicinas", "publico", "foraneo",
               "adqui_vehi", "mantenim", "refaccion", "combus", "comunica",
               "educacion", "esparci", "paq_turist", "cuida_pers", "acces_pers",
               "otros_gas", "transf_gas")

# Bucle para ajustar modelos, realizar comparaciones y generar gráficos para cada variable
for (var in variables) {
  # Imprimir mensaje de progreso
  cat("Ajustando modelo para", var, "\n")
  
  # Ajustar modelo de regresión lineal
  assign(paste0("fit_", var), lm(as.formula(paste(var, "~ tratamiento * ( ent + area + edad_jef + educ_jef + sum_ninios + ocupacion + tot_integ.x + sum_adultos + sum_amayores + sum_escuela + sexo_jefe)")), data = m.data, weights = weights))
  
  # Imprimir mensaje de finalización del modelo
  cat("Modelo para", var, "completado\n")
  
  # Comparaciones promedio
  assign(paste0("avg_comp_", var), avg_comparisons(get(paste0("fit_", var)), variables = "tratamiento", vcov = ~subclass, newdata = subset(m.data, tratamiento == 1), wts = "weights"))
  
}


# Crear una lista con los nombres de los modelos
modelos <- c("avg_comp_cereales", "avg_comp_carnes", "avg_comp_pescado", "avg_comp_leche", "avg_comp_huevo", "avg_comp_aceites", "avg_comp_tuberculo", "avg_comp_verduras", "avg_comp_frutas", "avg_comp_azucar", "avg_comp_cafe", "avg_comp_especias", "avg_comp_otros_alim", "avg_comp_bebidas", "avg_comp_ali_fuera", "avg_comp_tabaco", "avg_comp_vestido", "avg_comp_calzado", "avg_comp_alquiler", "avg_comp_pred_cons", "avg_comp_agua", "avg_comp_energia", "avg_comp_cuidados", "avg_comp_utensilios", "avg_comp_enseres", "avg_comp_atenc_ambu", "avg_comp_hospital", "avg_comp_medicinas", "avg_comp_publico", "avg_comp_foraneo", "avg_comp_adqui_vehi", "avg_comp_mantenim", "avg_comp_refaccion", "avg_comp_combus", "avg_comp_comunica", "avg_comp_educacion", "avg_comp_esparci", "avg_comp_paq_turist", "avg_comp_cuida_pers", "avg_comp_acces_pers", "avg_comp_otros_gas", "avg_comp_transf_gas")

# Crear un nuevo libro de Excel
wb <- createWorkbook()

# Agregar una hoja de trabajo
addWorksheet(wb, "Modelos")

# Escribir los modelos en la hoja de trabajo con identificadores
for (i in 1:length(modelos)) {
  modelo <- get(modelos[i]) # Obtener el modelo por su nombre
  nombre_modelo <- modelos[i] # Obtener el nombre del modelo
  
  # Escribir el identificador y el modelo en la hoja de trabajo
  writeData(wb, "Modelos", data.frame(Nombre_Modelo = nombre_modelo), startRow = (i-1)*2 + 1)
  writeData(wb, "Modelos", modelo, startRow = (i-1)*2 + 2)
}

# Guardar el libro de Excel en un archivo
saveWorkbook(wb, "modelos_final.xlsx", overwrite = TRUE)
