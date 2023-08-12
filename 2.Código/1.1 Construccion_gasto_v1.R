
# Se instalan los paquetes y librerias

library(pacman) ; p_load("dplyr", "data.table", "tidyverse", "gdata", "srvyr", "bit64", "foreign")

# Se establece la carpeta de trabajo

setwd("/Users/jonathanguallasamin/Dropbox/Tareas_Jonathan/2023.07.07 Match_ProgramasSocialesGasto/bases/ENIGH2020") 
#setwd("C:/Users/virid/Dropbox/Tareas_Jonathan/2023.07.07 Match_ProgramasSocialesGasto/bases/ENIGH2020/temp")

ghog <- fread("gastoshogar.csv") %>% 
  rename_all(tolower)   %>% 
  mutate(base=1)

gper <- fread("gastospersona.csv") %>% 
  rename_all(tolower) %>% 
  mutate(base=2)

gastos <- bind_rows(gper, ghog)
gastos <- mutate(gastos, frecuencia=ifelse(base==2, frec_rem, frecuencia), 
                decena=str_sub(str_pad(folioviv, 10, "left", pad = "0"), 8,8))

# Una vez definidos los deflactores, se seleccionan los rubros
gastos <- data.table(gastos)[, c("gasnomon") :=.(gasto_tri)
][(clave>="A215" & clave<="A222"),
  c("beb_no_alcoho") := .(gasnomon)
][(clave>="A223" & clave<="A238"),
  c("beb_alcoho") := .(gasnomon)]

vars <- colnames(select(gastos, c("beb_no_alcoho", "beb_alcoho" )))
esp <- data.table(gastos)[, lapply(.SD, sum, na.rm=T), by=list(folioviv, foliohog), .SDcols = vars ]

concen <- fread("concentradohogar.csv") %>% rename_all(tolower)

gast <- select(concen, folioviv, foliohog, tam_loc, factor, tot_integ, 
              est_dis, upm, ubica_geo,
              gasto_mon,
              alimentos,
              ali_dentro,
              cereales,
              carnes,
              pescado,
              leche,
              huevo,
              aceites,
              tuberculo,
              verduras,
              frutas,
              azucar,
              cafe,
              especias,
              otros_alim,
              bebidas,
              ali_fuera,
              tabaco,
              vesti_calz,
              vestido,
              calzado,
              vivienda,
              alquiler,
              pred_cons,
              agua,
              energia,
              limpieza,
              cuidados,
              utensilios,
              enseres,
              salud,
              atenc_ambu,
              hospital,
              medicinas,
              transporte,
              publico,
              foraneo,
              adqui_vehi,
              mantenim,
              refaccion,
              combus,
              comunica,
              educa_espa,
              educacion,
              esparci,
              paq_turist,
              personales,
              cuida_pers,
              acces_pers,
              otros_gas,
              transf_gas,
              ero_nm_viv,
              ero_nm_hog,
              erogac_tot,
              cuota_viv,
              mater_serv,
              material,
              servicio,
              deposito,
              prest_terc,
              pago_tarje,
              deudas,
              balance,
              otras_erog,
              )

gast <- left_join(gast, esp, by = c("folioviv", "foliohog"))

################################################################################

fwrite(gast, "/Users/jonathanguallasamin/Dropbox/Tareas_Jonathan/2023.07.07 Match_ProgramasSocialesGasto/bases/gasto_final.csv")