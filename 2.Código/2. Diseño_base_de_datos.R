
########################################################################
#### Proyecto: Determinación del efecto de la entrega de programas sociales
#### Documento: Construcción de la base de datos
#### Elaboración: Econ. Guallasamin Miño Jonathan
########################################################################

# Este codigo crea las variables que se usaran en el modelo, con excepcion de ingreso sin transferencias gubernamentales que fue creado en el codigo anterior. 

rm(list=ls())
options(digits = 10)

# Se instalan los paquetes y librer?as a utilizar en el c?digo

library("pacman") ; p_load("dplyr", "data.table", "tidyverse", "gdata", "srvyr", "bit64", "devtools", "cem", "haven", "foreign")

# Se establece la carpeta de trabajo

setwd("/Users/jonathanguallasamin/Dropbox/Tareas_Jonathan/2023.07.07 Match_ProgramasSocialesGasto/bases/ENIGH2020") 
#setwd("/Users/virid/Dropbox/Tareas_Jonathan/2023.07.07 Match_ProgramasSocialesGasto/bases/ENIGH2020") 

# Se lee el archivo "poblacion.csv" y se guarda en la variable "poblacion"
poblacion <- fread("poblacion.csv") %>% rename_all(tolower)
# Se lee el archivo "trabajos.csv" y se guarda en la variable "trabajo"
trabajo <- fread("trabajos.csv") %>% rename_all(tolower)
# Se lee el archivo "concentradohogar.csv" y se guarda en la variable "concen"
concen <- fread("concentradohogar.csv") %>% rename_all(tolower)

# Filtrar el dataframe "trabajo" para mantener solo las filas donde "id_trabajo" es igual a 1
trabajo <- subset(trabajo, id_trabajo == 1)

# Realizar una uni?n por las columnas "folioviv", "foliohog" y "numren"
#  entre los dataframes "poblacion" y "trabajo" y guardar el resultado en "pob_trabajo"
pob_trabajo <- left_join(poblacion, trabajo, by = c("folioviv", "foliohog", "numren"))


#Variable de ocupacion del jefe del hogar. Toma el valor cero para PNEA o no ocupada, 1 para subordinado, 2 para independiente y 3 para empleador. 

# Se crea la variable condact la cual tomar? valores de 
# 1 si es empleado subordinado
# 2 si es independiente
# 3 si es empleador

pob_trabajo <- pob_trabajo %>%
  mutate(
    condact = NA,
    condact = ifelse(!is.na(subor) & subor == 1 & id_trabajo == 1 & parentesco == 101 & trabajo_mp == 1, 1, condact),
    condact = ifelse(!is.na(indep) & indep == 1 & id_trabajo == 1 & parentesco == 101 & trabajo_mp == 1, 2, condact),
    condact = ifelse(!is.na(personal) & personal == 1 & id_trabajo == 1 & parentesco == 101 & trabajo_mp == 1, 3, condact)
  )

# Se crea las siguientes dummys
# ninios si la variable edad es menor a 18
# adultos si la variable edad es igual o mauor a 18 y menor a 65
# amayores si la variable edad es igual o mayor a 65
# escuela si la persona asisti? a la escuela 



pob_trabajo <- pob_trabajo %>%
  mutate(
    ninios = ifelse(edad < 18, 1, 0),
    adultos = ifelse(edad >=18 & edad<65, 1, 0),
    amayores = ifelse(edad >= 65, 1, 0),
    escuela = ifelse(asis_esc > 0, 1, 0) | ifelse(is.na(asis_esc), NA, escuela)
  )

summary(pob_trabajo$escuela)

# Count missing values in each variable
missing_counts <- pob_trabajo %>%
  summarise_all(~ sum(is.na(.)))
# Print the results
print(missing_counts)

# Se crea la variable pea la cual tomar? valores de 
# 1 si el jefe de hogar reporto haber trabajadado el mes pasado
# La variable que se utiliza es trabajo_mp
# Seg?n el INEGI esta variable identifica a la PEA de la PNEA

# Realizar la siguiente transformaci?n de la columna "pea" en "pob_trabajo"
pob_trabajo <- pob_trabajo %>%
  mutate(
    pea = ifelse(!is.na(trabajo_mp) & trabajo_mp == 1 & parentesco == 101, 1, 0)
  )

#Variable de numero de integrantes del hogar. Se saca la suma de ninos, suma de adulyos, suma de adultos mayores, y de personas qeu aistan a la escuela independientemente de su edad. 

# Agrupar "pob_trabajo" por "folioviv" y "foliohog" y realizar las siguientes operaciones de resumen
pob_trabajo <- pob_trabajo %>%
  group_by(folioviv, foliohog) %>%
  mutate(
    sum_ninios = sum(ifelse(!is.na(ninios), ninios, 0)),
    sum_adultos = sum(ifelse(!is.na(adultos), adultos, 0)),
    sum_amayores = sum(ifelse(!is.na(amayores), amayores, 0)),
    sum_escuela = sum(ifelse(!is.na(escuela), escuela, 0)),
    max_pea = if (any(!is.na(pea))) max(pea, na.rm = TRUE) else NA,
    max_condact = if (any(!is.na(condact))) max(condact, na.rm = TRUE) else NA
  ) %>%
  ungroup() %>%
  distinct(folioviv, foliohog, .keep_all = TRUE)

#Se afrega la variable de ingresos que ya se habia calculdo. 
# Cargar el archivo "ingresos_final.csv"

p_ing <- fread("/Users/jonathanguallasamin/Dropbox/Tareas_Jonathan/2023.07.07 Match_ProgramasSocialesGasto/bases/ingresos_final.csv") %>% rename_all(tolower)
p_gas <- fread("/Users/jonathanguallasamin/Dropbox/Tareas_Jonathan/2023.07.07 Match_ProgramasSocialesGasto/bases/gasto_final.csv")

#load("/Users/virid/Dropbox/Tareas_Jonathan/2023.07.07 Match_ProgramasSocialesGasto/bases/ingresos_final.RData")

# Realizar una uni?n por las columnas "folioviv" y "foliohog" entre los dataframes "ict" y "pob_trabajo" y guardar el resultado en "pob_trabajo"
pob_trabajo1 <- left_join(pob_trabajo, p_ing, by = c("folioviv", "foliohog"))
pob_trabajo2 <- left_join(pob_trabajo1, concen, by = c("folioviv", "foliohog"))
pob_trabajo_fin <- left_join(pob_trabajo2, p_gas, by = c("folioviv", "foliohog"))

names(pob_trabajo_fin$t)

# Seleccionar las columnas deseadas en el dataframe "pob_trabajo"
pob_trabajo_fin <- pob_trabajo_fin %>%
  select(c("folioviv", "foliohog", "factor.x",
           "tot_integ.x", "ictpcst", "ing_gub",    
           "sum_ninios", "sum_adultos", "sum_amayores", 
           "sum_escuela", "max_pea", "max_condact",
           "clase_hog", "sexo_jefe", "edad_jefe",    
           "educa_jefe", "tam_loc.x",
           "gasto_mon",
           "alimentos",
           "ali_dentro",
           "cereales",
           "carnes",
           "pescado",
           "leche",
           "huevo",
           "aceites",
           "tuberculo",
           "verduras",
           "frutas",
           "azucar",
           "cafe",
           "especias",
           "otro_alim",
           "bebidas",
           "ali_fuera",
           "tabaco",
           "vesti_calz",
           "vestido",
           "calzado",
           "vivienda",
           "alquiler",
           "pred_cons",
           "agua",
           "energia",
           "limpieza",
           "cuidados",
           "utensilios",
           "enseres",
           "salud",
           "atenc_ambu",
           "hospital",
           "medicinas",
           "transporte",
           "publico",
           "foraneo",
           "adqui_vehi",
           "mantenim",
           "refaccion",
           "combus",
           "comunica",
           "educa_espa",
           "educacion",
           "esparci",
           "paq_turist",
           "personales",
           "cuida_pers",
           "acces_pers",
           "otros_gas",
           "transf_gas",
           "ero_nm_viv",
           "ero_nm_hog",
           "erogac_tot",
           "cuota_viv",
           "mater_serv",
           "material",
           "servicio",
           "deposito",
           "prest_terc",
           "pago_tarje",
           "deudas",
           "balance",
           "otras_erog"))

# Renombrar variables que tienen en su nombre un "."

pob_trabajo_fin <- pob_trabajo_fin %>%
  rename(factor = factor.x,
         tot_integ = tot_integ.x,
         tam_loc = tam_loc.x)

# Identificador de Estado

pob_trabajo_fin <- mutate(pob_trabajo_fin,
                      folioviv=str_pad(folioviv, 10, "left", pad = "0"),
                      ent=as.numeric(str_sub(folioviv, 1,2)), 
                      entidad=case_when(ent==1  ~ "Aguascalientes",
                                        ent==2  ~ "Baja California",
                                        ent==3  ~ "Baja California Sur",
                                        ent==4  ~ "Campeche",
                                        ent==5  ~ "Coahuila",
                                        ent==6  ~ "Colima",
                                        ent==7  ~ "Chiapas",
                                        ent==8  ~ "Chihuahua",
                                        ent==9  ~ "Ciudad de Mexico",
                                        ent==10 ~	"Durango",
                                        ent==11 ~	"Guanajuato",
                                        ent==12 ~	"Guerrero",
                                        ent==13 ~	"Hidalgo",
                                        ent==14 ~	"Jalisco",
                                        ent==15 ~	"Mexico",
                                        ent==16 ~	"Michoacan",
                                        ent==17 ~	"Morelos",
                                        ent==18 ~	"Nayarit",
                                        ent==19 ~	"Nuevo Leon",
                                        ent==20 ~	"Oaxaca",
                                        ent==21 ~	"Puebla",
                                        ent==22 ~	"Queretaro",
                                        ent==23 ~	"Quintana Roo",
                                        ent==24 ~	"San Luis Potos",
                                        ent==25 ~	"Sinaloa",
                                        ent==26 ~	"Sonora",
                                        ent==27 ~	"Tabasco",
                                        ent==28 ~	"Tamaulipas",
                                        ent==29 ~	"Tlaxcala",
                                        ent==30 ~	"Veracruz",
                                        ent==31 ~	"Yucatan",
                                        ent==32 ~	"Zacatecas"))

# Identifico el grupo de tratamiento y de control
# El grupo de tratamiento recibe programas sociales, el grupo control no los recibe. 

pob_trabajo_fin <- pob_trabajo_fin %>%
  mutate(
    tratamiento = if_else(is.na(ing_gub), NA_real_, if_else(ing_gub > 0, 1, 0))
  )


# Se crea la variable area la cual tomara valores de 
# 1 si es rural
# 2 si es extencion rural
# 3 si es urbano

pob_trabajo_fin <- pob_trabajo_fin %>%
  mutate(pob_trabajo_fin, 
         area=case_when(tam_loc==4 ~ 1, # Rural
                        tam_loc==3 ~ 2, # Extencion rural
                        tam_loc<=2 ~ 3)) # Urbano

# Se crea la variable edad_jefe la cual tomar? valores de 
# 1 si es menor de 18
# 2 si su edad es igual o mayor a 18 y menor a 65
# 3 si su edad es igual o mayor a 65
pob_trabajo_fin <-pob_trabajo_fin %>%
  mutate(pob_trabajo_fin, 
         edad_jef=case_when(edad_jefe<18 ~ 1, 
                            edad_jefe>=18 & edad_jefe<65 ~ 2,
                            edad_jefe>=65 ~ 3))

# Se crea la variable educ_jef la cual tomar? valores de 
# 1 si su educaaciones es de secundaria o menos
# 2 si su educaaciones es de preparatoria 
# 3 si su educaaciones es de universitario, posgrado, doctorado 
pob_trabajo_fin <- pob_trabajo_fin %>%
  mutate(pob_trabajo_fin, 
         educ_jef=case_when(educa_jefe<07 ~ 1, # Secundaria o menos
                            educa_jefe>=07 & educa_jefe<09 ~ 2, # Preparatoria
                            educa_jefe>=09 ~ 3)) # Profesional posgrado

# Se crea una dummy que defina si es PEA ocupada o PNEA ocupada
pob_trabajo_fin <-pob_trabajo_fin %>%
  mutate(pob_trabajo_fin, 
         ocup=ifelse(max_condact>0, 1 ,0)) # ocupado

pob_trabajo_fin <- pob_trabajo_fin %>%
  mutate(ocupacion_temp = case_when(
    max_condact == 1 & max_pea == 1 & ocup == 1 ~ 1,
    max_condact == 2 & max_pea == 1 & ocup == 1 ~ 2,
    max_condact == 3 & max_pea == 1 & ocup == 1 ~ 3
  )) %>%
  mutate(ocupacion = ifelse(is.na(ocupacion_temp), 0, ocupacion_temp)) %>%
  select(-ocupacion_temp)

# Seleccionar las columnas deseadas en el dataframe "pob_trabajo"
pob_trabajo_fin <- pob_trabajo_fin %>%
  select("ent", "tratamiento","area",
         "edad_jef", "educ_jef", "ocupacion",
         "factor", "tot_integ",   
         "sum_ninios", "sum_adultos",
         "sum_amayores", "sum_escuela",
         "sexo_jefe", "ictpcst",
         "gasto_mon",
         "alimentos",
         "ali_dentro",
         "cereales",
         "carnes",
         "pescado",
         "leche",
         "huevo",
         "aceites",
         "tuberculo",
         "verduras",
         "frutas",
         "azucar",
         "cafe",
         "especias",
         "otro_alim",
         "bebidas",
         "ali_fuera",
         "tabaco",
         "vesti_calz",
         "vestido",
         "calzado",
         "vivienda",
         "alquiler",
         "pred_cons",
         "agua",
         "energia",
         "limpieza",
         "cuidados",
         "utensilios",
         "enseres",
         "salud",
         "atenc_ambu",
         "hospital",
         "medicinas",
         "transporte",
         "publico",
         "foraneo",
         "adqui_vehi",
         "mantenim",
         "refaccion",
         "combus",
         "comunica",
         "educa_espa",
         "educacion",
         "esparci",
         "paq_turist",
         "personales",
         "cuida_pers",
         "acces_pers",
         "otros_gas",
         "transf_gas",
         "ero_nm_viv",
         "ero_nm_hog",
         "erogac_tot",
         "cuota_viv",
         "mater_serv",
         "material",
         "servicio",
         "deposito",
         "prest_terc",
         "pago_tarje",
         "deudas",
         "balance",
         "otras_erog")

fwrite(pob_trabajo_fin, "/Users/jonathanguallasamin/Dropbox/Tareas_Jonathan/2023.07.07 Match_ProgramasSocialesGasto/bases/modelo_final.csv")
