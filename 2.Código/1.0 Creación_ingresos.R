
########################################################################
#### Proyecto: Determinación del efecto de la entrega de programas sociales
#### Documento: Creación del Ingreso Corriente
#### Elaboración: Econ. Guallasamin Miño Jonathan
########################################################################

# Limpiar los datos de la consola

rm(list=ls())
options(digits = 10)

# Se instalan los paquetes y librerias

library(pacman) ; p_load("dplyr", "data.table", "tidyverse", "gdata", "srvyr", "bit64", "foreign")

# Se establece la carpeta de trabajo

setwd("/Users/jonathanguallasamin/Dropbox/Tareas_Jonathan/2023.07.07 Match_ProgramasSocialesGasto/bases/ENIGH2020") 

#Se procede a la construcci?n del ingreso corriente del hogar.

#En este proceso, se utilizar? informaci?n sobre la condici?n de ocupaci?n 
  #y los ingresos de los individuos para calcular el ingreso corriente del hogar. 
  #Con el fin de incorporar los ingresos por aguinaldo en la medici?n, 
  #se har? uso de la informaci?n contenida en la base de datos "trabajo.csv". 
  #Esta informaci?n permitir? identificar a la poblaci?n ocupada que declara recibir 
  #el aguinaldo como prestaci?n laboral, tanto en su trabajo principal como en trabajos secundarios.

#C?lculo de los ingresos por aguinaldo.

trabajos <- fread("trabajos.csv") %>% rename_all(tolower)

# Se selecciona unicamente las variables claves (folioviv, foliohog, numren)
# El identificador de trabajo (principal y secundario) y si tuvo aguinaldo
# La variable pres_2 identifica si esa persona recibio ingresos por aguinaldo

trabajos <- dplyr::select(trabajos, folioviv, foliohog, numren, id_trabajo, pres_2) %>% 
  as.data.table() %>% 
  dcast(folioviv + foliohog + numren ~ id_trabajo,value.var ="pres_2") %>% 
  mutate(
    # Poblaci?n con al menos un empleo 
    trabajos=1,
    # Aguinaldo trabajo principal
    aguinaldo1=case_when(`1`==2 ~ 1,  # Dispone de aguinaldo
                         TRUE ~ 0),   # No dispone de aguinaldo
    # Aguinaldo trabajo secundario
    aguinaldo2=case_when(`2`==2 ~ 1,   # Dispone de aguinaldo
                         TRUE ~ 0))%>% # No dispone de aguinaldo
  select(folioviv, foliohog, numren, aguinaldo1, aguinaldo2, trabajos)

fwrite(trabajos, "temp/aguinaldo20.csv")

# Ahora se incorpora a la base de ingresos

ing <- fread("ingresos.csv") %>% rename_all(tolower)

ing <-  full_join(ing, trabajos, by = c("folioviv", "foliohog", "numren")) %>%
  mutate(index=(case_when(clave=="P009" & aguinaldo1!=1 ~ 1,
                          clave=="P016" & aguinaldo2!=1 ~ 1,
                          TRUE ~ 0))) %>%
  filter(index != 1)

# Una vez realizado lo anterior, se procede a deflactar el ingreso recibido
# por los hogares a precios de agosto de 2020. Para ello, se utilizan las 
# variables meses, las cuales toman los valores 2 a 10 e indican el mes en
# que se recibi? el ingreso respectivo


# Definici?n de los deflactores 2020 
{
  dic19	<-	0.98207978340
  ene20	<-	0.98683564020
  feb20	<-	0.99093327890
  mar20	<-	0.99046047450
  abr20	<-	0.98042033240
  may20	<-	0.98419349750
  jun20	<-	0.98957976030
  jul20	<-	0.99607850410
  ago20	<-	1.00000000000
  sep20	<-	1.00228985700
  oct20	<-	1.00840850310
  nov20	<-	1.00916869850
  dic20	<-	1.01301602900
  }

ing <-mutate(ing, 
             ing_6=ifelse(is.na(ing$mes_6), ing_6,
                          case_when(mes_6==2  ~ ing_6/feb20,
                                    mes_6==3  ~ ing_6/mar20,
                                    mes_6==4  ~ ing_6/abr20,
                                    mes_6==5  ~ ing_6/may20)),
             ing_5=ifelse(is.na(ing$mes_5), ing_5,
                          case_when(mes_5==3  ~ ing_5/mar20,
                                    mes_5==4  ~ ing_5/abr20,
                                    mes_5==5  ~ ing_5/may20,
                                    mes_5==6  ~ ing_5/jun20)),
             ing_4=ifelse(is.na(ing$mes_4), ing_4,
                          case_when(mes_4==4  ~ ing_4/abr20,
                                    mes_4==5  ~ ing_4/may20,
                                    mes_4==6  ~ ing_4/jun20,
                                    mes_4==7  ~ ing_4/jul20)),
             ing_3=ifelse(is.na(ing$mes_3), ing_3,
                          case_when(mes_3==5  ~ ing_3/may20,
                                    mes_3==6  ~ ing_3/jun20,
                                    mes_3==7  ~ ing_3/jul20,
                                    mes_3==8  ~ ing_3/ago20)),
             ing_2=ifelse(is.na(ing$mes_2), ing_2,
                          case_when(mes_2==6  ~ ing_2/jun20,
                                    mes_2==7  ~ ing_2/jul20,
                                    mes_2==8  ~ ing_2/ago20,
                                    mes_2==9  ~ ing_2/sep20)),
             ing_1=ifelse(is.na(ing$mes_1), ing_1,
                          case_when(mes_1==7  ~ ing_1/jul20,
                                    mes_1==8  ~ ing_1/ago20,
                                    mes_1==9  ~ ing_1/sep20,
                                    mes_1==10 ~ ing_1/oct20))) 


# Se deflactan las claves P008 y P015 (Reparto de utilidades) y P009 y P016 (aguinaldo)
# con los deflactores de mayo a agosto 2020 y de diciembre de 2019 a agosto 2020, 
# respectivamente, y se obtiene el promedio mensual

index <-c("P008", "P009", "P015", "P016") 

ing <- ing %>%
  mutate(ing_1=ifelse(clave=="P008" | clave=="P015", (ing_1/may20)/12, ing_1), 
         ing_1=ifelse(clave=="P009" | clave=="P016", (ing_1/dic19)/12, ing_1),
         ing_2=ifelse((clave %in%  index) & ing_2 == 0, NA_real_, ing_2),
         ing_3=ifelse((clave %in%  index) & ing_3 == 0, NA_real_, ing_3),
         ing_4=ifelse((clave %in%  index) & ing_4 == 0, NA_real_, ing_4),
         ing_5=ifelse((clave %in%  index) & ing_5 == 0, NA_real_, ing_5),
         ing_6=ifelse((clave %in%  index) & ing_6 == 0, NA_real_, ing_6))

# Una vez realizada la deflactaci?n, se procede a obtener el ingreso mensual 
# promedio en los ?ltimos seis meses, para cada persona y clave de ingreso
ing <- ing %>% 
  mutate(ing_mens = apply(ing[, c("ing_1", "ing_2", "ing_3", "ing_4", "ing_5", "ing_6")], 1, mean, na.rm = TRUE),
         ing_mon = case_when((clave >= "P001" & clave <= "P009") | (clave >= "P011" & clave <= "P016") |
                               (clave >= "P018" & clave <= "P048") | (clave >= "P067" & clave <= "P081") |
                               (clave >= "P101" & clave <= "P108") ~ ing_mens),
         ing_lab = case_when((clave >= "P001" & clave <= "P009") | (clave >= "P011" & clave <= "P016") |
                               (clave >= "P018" & clave <= "P022") | (clave >= "P067" & clave <= "P081") ~ ing_mens),
         ing_ren = case_when((clave >= "P023" & clave <= "P031") ~ ing_mens),
         ing_tra_priv = case_when((clave >= "P032" & clave <= "P037") | (clave >= "P039" & clave <= "P041") ~ ing_mens),
         ing_gub = case_when(clave %in% c("P038", "P043", "P045", "P048") |
                               (clave >= "P101" & clave <= "P108") ~ ing_mens))

# Se estima el total de ingresos de cada  hogar
vars <- colnames(select(ing, ing_mon, ing_lab, ing_ren, ing_tra_priv, 
                        ing_gub))

#####################################################################################
#####################################################################################
# Se realiza un collapse para obtener el ingreso del hogar, las observaciones que
#no tienen datos (NA) no se toman en cuenta. Por esto todos los hogares tienen ingreso
#####################################################################################
#####################################################################################

ing <- data.table(ing)[, lapply(.SD, sum, na.rm=T), by=list(folioviv, foliohog), .SDcols = vars ]

fwrite(ing, "temp/ingreso_deflactado20.csv")

################################################################################
#
#          Creaci?n del ingreso no monetario deflactado a pesos de 
#                                 agosto del 2020
#
################################################################################

#No Monetario

gper <- fread("gastoshogar.csv") %>% 
  rename_all(tolower)   %>% 
  mutate(base=1)
ghog <- fread("gastospersona.csv") %>% 
  rename_all(tolower) %>% 
  mutate(base=2)

nomon <- bind_rows(gper, ghog)
nomon <- mutate(nomon, frecuencia=ifelse(base==2, frec_rem, frecuencia), 
                
                # En el caso de la informaci?n de gasto no monetario, para deflactar se utiliza 
                # la decena de levantamiento de la encuesta, la cual se encuentra en la octava 
                # posici?n del folio de la vivienda. En primer lugar se obtiene una variable que 
                # identifique la decena de levantamiento
                decena=str_sub(str_pad(folioviv, 10, "left", pad = "0"), 8,8))


# Definici?n de los deflactores
{
  # Rubro 1.1 semanal, Alimentos		
  d11w07	<-	0.9939758499
  d11w08	<-	1.0000000000
  d11w09	<-	1.0028338457
  d11w10	<-	1.0098560798
  d11w11	<-	1.0094105066
  
  # Rubro 1.2 semanal, Bebidas alcohólicas y tabaco		
  d12w07	<-	1.0044889115
  d12w08	<-	1.0000000000
  d12w09	<-	0.9980317849
  d12w10	<-	1.0006215416
  d12w11	<-	0.9988864046
  
  # Rubro 2 trimestral, Vestido, calzado y accesorios		
  d2t05	<-	0.9844476202
  d2t06	<-	0.9923010570
  d2t07	<-	1.0003186435
  d2t08	<-	1.0061700977
  
  # Rubro 3 mensual, viviendas		
  d3m07	<-	0.9963402865
  d3m08	<-	1.0000000000
  d3m09	<-	1.0008217634
  d3m10	<-	1.0170085903
  d3m11	<-	1.0374475648
  
  # Rubro 4.2 mensual, Accesorios y art?culos de limpieza para el hogar		
  d42m07	<-	0.9967972340
  d42m08	<-	1.0000000000
  d42m09	<-	1.0024748647
  d42m10	<-	1.0026659388
  d42m11	<-	1.0008279878
  
  # Rubro 4.2 trimestral, Accesorios y art?culos de limpieza para el hogar		
  d42t05	<-	0.9887690885
  d42t06	<-	0.9957963696
  d42t07	<-	0.9997573662
  d42t08	<-	1.0017136011
  
  # Rubro 4.1 semestral, Muebles y aparatos domesticos		
  d41s02	<-	0.9716430784
  d41s03	<-	0.9778718235
  d41s04	<-	0.9849581733
  d41s05	<-	0.9927539810
  
  # Rubro 5.1 trimestral, Salud		
  d51t05	<-	0.9948883582
  d51t06	<-	0.9973496320
  d51t07	<-	1.0009424208
  d51t08	<-	1.0032664489
  
  # Rubro 6.1.1 semanal, Transporte público urbano		
  d611w07	<-	0.9981969820
  d611w08	<-	1.0000000000
  d611w09	<-	1.0030142762
  d611w10	<-	1.0031714624
  d611w11	<-	1.0035043272
  
  # Rubro 6 mensual, Transporte		
  d6m07	<-	0.9987343957
  d6m08	<-	1.0000000000
  d6m09	<-	1.0009587912
  d6m10	<-	1.0003931044
  d6m11	<-	0.9833266218
  
  # Rubro 6 semestral, Transporte		
  d6s02	<-	0.9704468606
  d6s03	<-	0.9685484541
  d6s04	<-	0.9718275199
  d6s05	<-	0.9864091354
  
  # Rubro 7 mensual, Educaci?n y esparcimiento		
  d7m07	<-	0.9984004543
  d7m08	<-	1.0000000000
  d7m09	<-	1.0061994226
  d7m10	<-	1.0076002082
  d7m11	<-	1.0062940703
  
  # Rubro 2.3 mensual, Accesorios y cuidados del vestido		
  d23m07	<-	0.9944542071
  d23m08	<-	1.0000000000
  d23m09	<-	1.0056497175
  d23m10	<-	1.0048844548
  d23m11	<-	0.9969956352
  
  # Rubro 2.3 trimestral,  Accesorios y cuidados del vestido		
  d23t05	<-	0.9806290901
  d23t06	<-	0.9895823492
  d23t07	<-	1.0000346415
  d23t08	<-	1.0035113908
  
  # INPC semestral		
  dINPCs02 <-	0.9886109746
  dINPCs03 <-	0.9901220948
  dINPCs04 <-	0.9920936585
  dINPCs05 <-	0.9967583537
  
}

# Una vez definidos los deflactores, se seleccionan los rubros
nomon <- data.table(nomon)[, c("gasnomon") :=.(gas_nm_tri/3)
][tipo_gasto=="G4", c("esp") := .(1)
][tipo_gasto=="G5" | tipo_gasto=="G6", c("reg") := .(1)
][!(tipo_gasto=="G2" | tipo_gasto=="G3" | tipo_gasto=="G7")
  
  # Control para la frecuencia de los regalos recibidos por el hogar
][!(((frecuencia>=5 & frecuencia<=6) | is.na(frecuencia) | frecuencia==0) & base==1 & tipo_gasto=="G5")
  
  # Control para la frecuencia de los regalos recibidos por persona
][!(((frecuencia==9) | is.na(frecuencia)) & base==2 & tipo_gasto=="G5")
  
][(clave>="A001" & clave<="A222") | (clave>="A242" & clave<="A247"),
  c("ali_nm") := .(gasnomon)
][(clave>="A223" & clave<="A241"),
  c("alta_nm") := .(gasnomon)
][(clave>="H001" & clave<="H122") | (clave=="H136"),
  c("veca_nm") := .(gasnomon)
][(clave>="G001" & clave<="G016") | (clave>="R001" & clave<="R004") | clave=="R013",
  c("viv_nm") := .(gasnomon)
][(clave>="C001" & clave<="C024"),
  c("lim_nm") := .(gasnomon)
][(clave>="I001" & clave<="I026"),
  c("cris_nm") := .(gasnomon)
][(clave>="K001" & clave<="K037"),
  c("ens_nm") := .(gasnomon)
][(clave>="J001" & clave<="J072"),
  c("sal_nm") := .(gasnomon)
][(clave>="B001" & clave<="B007"),
  c("tpub_nm") := .(gasnomon)
][(clave>="M001" & clave<="M018") | (clave>="F007" & clave<="F014"),
  c("tfor_nm") := .(gasnomon)
][(clave>="F001" & clave<="F006") | (clave>="R005" & clave<="R008") | (clave>="R010" & clave<="R011"),
  c("com_nm") := .(gasnomon)
][(clave>="E001" & clave<="E034") | (clave>="H134" & clave<="H135") | (clave>="L001" &  clave<="L029") | (clave>="N003" & clave<="N005") | clave=="R009",
  c("edre_nm") := .(gasnomon)
][(clave>="E002" & clave<="E003") | (clave>="H134" & clave<="H135"),
  c("edba_nm") := .(gasnomon)
][(clave>="D001" & clave<="D026") | (clave=="H132"),
  c("cuip_nm") := .(gasnomon)
][(clave>="H123" & clave<="H131") | (clave=="H133"),
  c("accp_nm") := .(gasnomon)
][(clave>="N001" & clave<="N002") | (clave>="N006" & clave<="N016") | (clave>="T901" &  clave<="T915") | (clave=="R012"),
  c("otr_nm") := .(gasnomon)
][(clave>="T901" & clave<="T915") | (clave=="N013"),
  c("reda_nm") := .(gasnomon)
][, 
  # Se deflactan los rubros del gasto no monetario seg?n la decena de levantamiento 
  
  c("ali_nm", # Gasto no monetario en Alimentos deflactado (semanal)
    "alta_nm", # Gasto no monetario en Alcohol y tabaco deflactado (semanal)
    "veca_nm", # Gasto no monetario en Vestido y calzado deflactado (trimestral)
    "viv_nm",  # Gasto no monetario en viviendas y servicios de conservación deflactado (mensual)
    "lim_nm",  # Gasto no monetario en Art?culos de limpieza deflactado (mensual)
    "cris_nm", # Gasto no monetario en Cristaler?a y blancos deflactado (trimestral)
    "ens_nm",  # Gasto no monetario en Enseres dom?sticos y muebles deflactado (semestral)
    "sal_nm",  # Gasto no monetario en Salud deflactado (trimestral)
    "tpub_nm", # Gasto no monetario en Transporte p?blico deflactado (semanal)
    "tfor_nm", # Gasto no monetario en Transporte for?neo deflactado (semestral)
    "com_nm",  # Gasto no monetario en Comunicaciones deflactado (mensual)
    "edre_nm", # Gasto no monetario en Educaci?n y recreaci?n deflactado (mensual)
    "edba_nm", # Gasto no monetario en Educaci?n b?sica deflactado (mensual)
    "cuip_nm", # Gasto no monetario en Cuidado personal deflactado (mensual)
    "accp_nm", # Gasto no monetario en Accesorios personales deflactado (trimestral)
    "otr_nm",  # Gasto no monetario en Otros gastos y transferencias deflactado (semestral)
    "reda_nm") # Gasto no monetario en Regalos Otorgados deflactado
  
  := .(case_when(decena %in% c(1,2,3) ~ ali_nm/d11w08,
                 decena %in% c(4,5,6) ~ ali_nm/d11w09,
                 decena %in% c(7,8,9) ~ ali_nm/d11w10,
                 decena %in% c(0) ~ ali_nm/d11w11),
       case_when(decena %in% c(1,2,3) ~ alta_nm/d12w08,
                 decena %in% c(4,5,6) ~ alta_nm/d12w09,
                 decena %in% c(7,8,9) ~ alta_nm/d12w10,
                 decena %in% c(0) ~ alta_nm/d12w11),
       case_when(decena %in% c(1,2) ~ veca_nm/d2t05,
                 decena %in% c(3,4,5) ~ veca_nm/d2t06,
                 decena %in% c(6,7,8) ~ veca_nm/d2t07,
                 decena %in% c(9,0) ~ veca_nm/d2t08),
       case_when(decena %in% c(1,2) ~ viv_nm/d3m07,
                 decena %in% c(3,4,5) ~ viv_nm/d3m08,
                 decena %in% c(6,7,8) ~ viv_nm/d3m09,
                 decena %in% c(9,0) ~ viv_nm/d3m10),
       case_when(decena %in% c(1,2) ~ lim_nm/d42m07,
                 decena %in% c(3,4,5) ~ lim_nm/d42m08,
                 decena %in% c(6,7,8) ~ lim_nm/d42m09,
                 decena %in% c(9,0) ~ lim_nm/d42m10),
       case_when(decena %in% c(1,2) ~ cris_nm/d42t05,
                 decena %in% c(3,4,5) ~ cris_nm/d42t06,
                 decena %in% c(6,7,8) ~ cris_nm/d42t07,
                 decena %in% c(9,0) ~ cris_nm/d42t08),
       case_when(decena %in% c(1,2) ~ ens_nm/d41s02,
                 decena %in% c(3,4,5) ~ ens_nm/d41s03,
                 decena %in% c(6,7,8) ~ ens_nm/d41s04,
                 decena %in% c(9,0) ~ ens_nm/d41s05),
       case_when(decena %in% c(1,2) ~ sal_nm/d51t05,
                 decena %in% c(3,4,5) ~ sal_nm/d51t06,
                 decena %in% c(6,7,8) ~ sal_nm/d51t07,
                 decena %in% c(9,0) ~ sal_nm/d51t08),
       case_when(decena %in% c(1,2,3) ~ tpub_nm/d611w08,
                 decena %in% c(4,5,6) ~ tpub_nm/d611w09,
                 decena %in% c(7,8,9) ~ tpub_nm/d611w10,
                 decena %in% c(0) ~ tpub_nm/d611w11),
       case_when(decena %in% c(1,2) ~ tfor_nm/d6s02,
                 decena %in% c(3,4,5) ~ tfor_nm/d6s03,
                 decena %in% c(6,7,8) ~ tfor_nm/d6s04,
                 decena %in% c(9,0) ~ tfor_nm/d6s05),
       case_when(decena %in% c(1,2) ~ com_nm/d6m07,
                 decena %in% c(3,4,5) ~ com_nm/d6m08,
                 decena %in% c(6,7,8) ~ com_nm/d6m09,
                 decena %in% c(9,0) ~ com_nm/d6m10),
       case_when(decena %in% c(1,2) ~ edre_nm/d7m07,
                 decena %in% c(3,4,5) ~ edre_nm/d7m08,
                 decena %in% c(6,7,8) ~ edre_nm/d7m09,
                 decena %in% c(9,0) ~ edre_nm/d7m10),
       case_when(decena %in% c(1,2) ~ edba_nm/d7m07,
                 decena %in% c(3,4,5) ~ edba_nm/d7m08,
                 decena %in% c(6,7,8) ~ edba_nm/d7m09,
                 decena %in% c(9,0) ~ edba_nm/d7m10),
       case_when(decena %in% c(1,2) ~ cuip_nm/d23m07,
                 decena %in% c(3,4,5) ~ cuip_nm/d23m08,
                 decena %in% c(6,7,8) ~ cuip_nm/d23m09,
                 decena %in% c(9,0) ~ cuip_nm/d23m10),
       case_when(decena %in% c(1,2) ~ accp_nm/d23t05,
                 decena %in% c(3,4,5) ~ accp_nm/d23t06,
                 decena %in% c(6,7,8) ~ accp_nm/d23t07,
                 decena %in% c(9,0) ~ accp_nm/d23t08),
       case_when(decena %in% c(1,2) ~ otr_nm/dINPCs02,
                 decena %in% c(3,4,5) ~ otr_nm/dINPCs03,
                 decena %in% c(6,7,8) ~ otr_nm/dINPCs04,
                 decena %in% c(9,0) ~ otr_nm/dINPCs05),
       case_when(decena %in% c(1,2) ~ reda_nm/dINPCs02,
                 decena %in% c(3,4,5) ~ reda_nm/dINPCs03,
                 decena %in% c(6,7,8) ~ reda_nm/dINPCs04,
                 decena %in% c(9,0) ~ reda_nm/dINPCs05))]

fwrite(nomon, "temp/ingresonomonetario_def20.csv")

# Construcci?n de la base de pagos en especie a partir de la base de gasto no monetario

esp <- filter(nomon, esp==1)

vars <- colnames(select(esp, ends_with("_nm")))
esp <- data.table(esp)[, lapply(.SD, sum, na.rm=T), by=list(folioviv, foliohog), .SDcols = vars ]

vars <- c("folioviv", "foliohog", "gasto_nm", "ali_nme", "alta_nme", "veca_nme", 
          "viv_nme", "lim_nme", "cris_nme", "ens_nme", "sal_nme", "tpub_nme", 
          "tfor_nme", "com_nme", "edre_nme", "edba_nme", "cuip_nme", "accp_nme", 
          "otr_nme", "reda_nme")

colnames(esp) <- vars

fwrite(esp, "temp/esp_def20.csv")

# Construcci?n de base de regalos a partir de la base no monetaria

reg <- filter(nomon, reg==1)

vars <- colnames(select(reg, ends_with("_nm")))
reg <- data.table(reg)[, lapply(.SD, sum, na.rm=T), by=list(folioviv, foliohog), .SDcols = vars ]

vars <- c("folioviv", "foliohog", "gasto_nm", "ali_nmr", "alta_nmr", "veca_nmr", 
          "viv_nmr", "lim_nmr", "cris_nmr", "ens_nmr", "sal_nmr", "tpub_nmr", 
          "tfor_nmr", "com_nmr", "edre_nmr", "edba_nmr", "cuip_nmr", "accp_nmr", 
          "otr_nmr", "reda_nmr")

colnames(reg) <- vars

fwrite(reg, "temp/reg_def20.csv")

################################################################################
#
#                Construcci?n del ingreso corriente total
#
################################################################################

concen <- fread("concentradohogar.csv") %>% rename_all(tolower)

ict <- select(concen, folioviv, foliohog, tam_loc, factor, tot_integ, 
              est_dis, upm, ubica_geo)

# Incorporaci?n de la base de ingreso monetario deflactado
ict <- left_join(ict, ing, by = c("folioviv", "foliohog"))

# Incorporaci?n de la base de ingreso no monetario deflactado: pago en especie
ict <- left_join(ict, esp, by = c("folioviv", "foliohog"))

# Incorporaci?n de la base de ingreso no monetario deflactado: regalos en especie
ict <- left_join(ict, reg, by = c("folioviv", "foliohog"))

ict <- mutate(ict, 
              rururb=case_when(tam_loc==4 ~ 1, # Rural
                               tam_loc<=3 ~ 0), # Urbano
              pago_esp = apply(ict[,c("ali_nme", "alta_nme", "veca_nme", "viv_nme", 
                                      "lim_nme", "cris_nme", "ens_nme", "sal_nme", 
                                      "tpub_nme", "tfor_nme", "com_nme", "edre_nme", 
                                      "cuip_nme", "accp_nme", "otr_nme")], 1, sum, na.rm=TRUE), 
              reg_esp  = apply(ict[,c("ali_nmr", "alta_nmr", "veca_nmr", "viv_nmr", 
                                      "lim_nmr", "cris_nmr", "ens_nmr", "sal_nmr", 
                                      "tpub_nmr", "tfor_nmr", "com_nmr", "edre_nmr", 
                                      "cuip_nmr", "accp_nmr", "otr_nmr")], 1, sum, na.rm=TRUE))

ict <- mutate(ict,
              nomon = apply(ict[,c("pago_esp","reg_esp")], 1, sum, na.rm=TRUE))

# Se construye el Ingreso Corriente Total con el ingreso monetario  y
# el ingreso no monetario 

ict <- mutate(ict,
              ict= apply(ict[,c("ing_mon", "nomon")], 1, sum, na.rm=TRUE)) %>%
  select(folioviv,foliohog,ubica_geo,factor,tot_integ,
         ing_gub,ict)

ict <- ict %>%
  mutate(
    ing_gubnt = replace(ict - ing_gub, is.na(ict - ing_gub), NA),
    na.rm = TRUE
  )

save(ict, file= "/Users/jonathanguallasamin/Dropbox/Tareas_Jonathan/2023.07.07 Match_ProgramasSocialesGasto/bases/ingresos_final.RData") 

################################################################################
#
#        Construcción del tamaño de hogar con economías de escala
#                       y escalas de equivalencia
#
################################################################################

tam_hogesc <- fread("poblacion.csv") %>% rename_all(tolower)

# Población objetivo: no se incluye a huéspedes ni trabajadores domésticos
tam_hogesc <- filter(tam_hogesc, !(parentesco>=400 & parentesco <500 | 
                                     parentesco>=700 & parentesco <800))

# Total de integrantes del hogar
tam_hogesc <- as.data.table(tam_hogesc)[, c("ind"):=.(1)][, c("tot_ind"):=.(sum(ind, na.rm = T)), 
                                                          by=.(folioviv, foliohog)]

############################
# Escalas de equivalencia #
############################
tam_hogesc<-mutate(tam_hogesc,
                   tamhogesc=case_when(tot_ind==1 ~ 1,
                                       edad<=5 ~ .7031,
                                       edad>=6 & edad<=12 ~ .7382,
                                       edad>=13 & edad<=18 ~ .7057,
                                       edad>=19 & !is.na(edad) ~ .9945)) %>%
  select(folioviv, foliohog, numren, tamhogesc)

tam_hogesc <- as.data.table(tam_hogesc)[,list(tamhogesc=sum(tamhogesc, na.rm = T)),
                                        by=.(folioviv, foliohog)]

fwrite(tam_hogesc, "tamhogesc20.csv")

################################################################################
#
#                        Construcción del ingreso
#
################################################################################

# Incorporación de la información sobre el tamaño del hogar ajustado

p_ing <- left_join(ict, tam_hogesc, by = c("folioviv", "foliohog"))

# Información per cápita
p_ing <-mutate(p_ing, ictpcst= ing_gubnt/tamhogesc )

################################################################################

fwrite(p_ing, "/Users/jonathanguallasamin/Dropbox/Tareas_Jonathan/2023.07.07 Match_ProgramasSocialesGasto/bases/ingresos_final.csv")
