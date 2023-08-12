rm(list=ls())
options(digits = 10)

# Este programa debe ser utilizado con la versión 4 o superior 

# Se instalan los paquetes y librerías a utilizar en el programa
if (!require(pacman)) install.packages("pacman")
library(pacman) ; p_load("data.table", "tidyverse", "gdata", "srvyr", "bit64")

#setwd("C:/pobreza20/R") # Se puede modificar el directorio si no abre el proyecto

# Todas las bases de datos de la ENIGH 2020 deben estar en formato CSV

# En este programa se utilizan las siguientes bases, 
# nombrándolas de la siguiente forma:

# Base de población: poblacion.csv
# Base de trabajos: trabajos.csv
# Base de ingresos: ingresos.csv
# Base de viviendas: viviendas.csv
# Base de hogares: hogares.csv
# Base de concentrado: concentradohogar.csv
# Base de no monetario hogar: gastoshogar.csv
# Base de no monetario personas: gastospersona.csv

# En este programa de cálculo se utilizan dos tipos de archivos, los cuales 
# están ubicados en las siguientes carpetas:

# 1) Bases originales: "C:\Pobreza 2020\Bases de datos"
# 2) Bases generadas:  "C:\Pobreza 2020\Bases"

################################################################################
#
# PROGRAMA DE CÁLCULO PARA LA MEDICIÓN MULTIDIMENSIONAL DE LA POBREZA* 2020
#
################################################################################
#
# De acuerdo con los Lineamientos y criterios generales para la definición, identificación y 
# medición de la pobreza (2018) que se pueden consultar en el Diario Oficial de la Federación 
# (https://www.dof.gob.mx/nota_detalle.php?codigo=5542421&fecha=30/10/2018) y la Metodología 
# para la medición multidimensional de la pobreza en México, tercera edición 
# (https://www.coneval.org.mx/InformesPublicaciones/InformesPublicaciones/Documents/Metodo
# logia-medicion-multidimensional-3er-edicion.pdf).
################################################################################


################################################################################
# Parte I Indicadores de carencias sociales:
# INDICADOR DE REZAGO EDUCATIVO
################################################################################
pobla <- fread("Bases de datos/poblacion.csv") %>% rename_all(tolower)
rezedu<-pobla

# Población objetivo: no se incluye a huéspedes ni trabajadores domésticos
rezedu <- filter(rezedu, !(parentesco>=400 & parentesco <500 |
                           parentesco>=700 & parentesco <800))
rezedu <- mutate(rezedu, 
                # Año de nacimiento
                anac_e=case_when(!is.na(edad) ~ 2020-edad),
                # Inasistencia escolar (se reporta para personas de 3 años o más)
                inas_esc=case_when(asis_esc==1 ~ 0 ,  # Sí asiste
                                   asis_esc==2 ~ 1),  # No asiste
                ## Nivel educativo  
                niv_ed=case_when(
                                # Con primaria incompleta o menos
                                (nivelaprob<2) | (nivelaprob==2 & gradoaprob<6) ~ 0, 
                                # Primaria completa o secundaria incompleta
                                (nivelaprob==2 & gradoaprob==6) | 
                                (nivelaprob==3 & gradoaprob<3) | 
                                (nivelaprob==5 | nivelaprob==6) & gradoaprob<3 & antec_esc==1 ~ 1,
                                # Secundaria completa o media superior incompleta
                                (nivelaprob==3 & gradoaprob==3) | 
                                (nivelaprob==4 & gradoaprob<3) |
                                (nivelaprob==5 & antec_esc==1 & gradoaprob>=3) |
                                (nivelaprob==6 & antec_esc==1 & gradoaprob>=3) | 
                                (nivelaprob==5 & antec_esc==2 & gradoaprob<3) | 
                                (nivelaprob==6 & antec_esc==2 & gradoaprob<3) ~ 2, 
                                # Media superior completa o mayor nivel educativo
                                (nivelaprob==4 & gradoaprob==3) | 
                                (nivelaprob==5 & antec_esc==2 & gradoaprob>=3) | 
                                (nivelaprob==6 & antec_esc==2 & gradoaprob>=3) |
                                (nivelaprob==5 & antec_esc>2) | 
                                (nivelaprob==6 & antec_esc>2) |
                                (nivelaprob>=7 & !is.na(nivelaprob)) ~ 3))

# Indicador de carencia por rezago educativo
################################################################################
# Se considera en situación de carencia por rezago educativo 
# a la población que cumpla con alguno de los siguientes criterios:
#
# 1. Tiene de tres a 21 años, no cuenta con la educación
#    obligatoria y no asiste a un centro de educación formal.
# 2. Tiene 22 años o más, nació a partir del año 1998 y no ha terminado 
#    la educación obligatoria (media superior).
# 3. Tiene 16 años o más, nació antes de 1982 y no cuenta con el nivel 
#    de educación obligatorio vigente en el momento en que debía haberlo 
#    cursado (primaria completa).	
# 4. Tiene 16 años o más, nació entre 1982 y 1997 y no cuenta con el
#    nivel de educación obligatorio vigente en el momento en que debía  
#    haberlo cursado (secundaria completa).
################################################################################

rezedu <- mutate(rezedu, 
                 ic_rezedu=case_when(
                                   # Presenta carencia
                                    anac_e>=1998 & (edad>=3 & edad <=21) 
                                    & inas_esc==1 & niv_ed<3                  ~ 1, # Presenta carencia
                                    (anac_e>=1982 & anac_e<=1997) 
                                    & edad>=16 & niv_ed<2                     ~ 1, # Presenta carencia
                                    anac_e<=1981 & edad>=16 & niv_ed==0       ~ 1, # Presenta carencia
                                    anac_e>=1998 & edad>=22 & niv_ed<3        ~ 1, # Presenta carencia
                                    # No presenta carencia             
                                     edad>=0 & edad<=2                        ~ 0, 
                                     anac_e>=1998 & (edad>=3 & edad<=21) 
                                     & inas_esc==0                            ~ 0, # No presenta carencia
                                     niv_ed==3                                ~ 0, # No presenta carencia
                                    (anac_e>=1982 & anac_e<=1997) & edad>=16 & 
                                    (niv_ed>=2 & !is.na(niv_ed))              ~ 0, # No presenta carencia
                                     anac_e<=1981 & edad>=16 & 
                                    (niv_ed>=1 & !is.na(niv_ed))              ~ 0)) # No presenta carencia
          

# Hablante de lengua indígena 
rezedu <- mutate(rezedu, 
                 hli=case_when(hablaind==1 & edad>=3 ~ 1,   # Habla lengua indígena
                               hablaind==2 & edad>=3 ~ 0))  # No habla lengua indígena

rezedu <- dplyr::select(rezedu, folioviv, folioviv, foliohog, numren, edad, 
                        anac_e, inas_esc, niv_ed, ic_rezedu, parentesco, hli)

fwrite(rezedu, "Bases/ic_rezedu20.csv", row.names=F)
gdata::keep(rezedu, pobla, sure=T)

################################################################################
# Parte II Indicadores de carencias sociales:
# INDICADOR DE CARENCIA POR ACCESO A LOS SERVICIOS DE SALUD
################################################################################

# Acceso a servicios de salud por prestaciones laborales
ocupados <- fread("Bases de datos/trabajos.csv")%>% rename_all(tolower)

# Tipo de trabajador: identifica la población subordinada e independiente
ocupados <-mutate(ocupados, 
                  tipo_trab=case_when(
                                    #Subordinados
                                    subor==1 ~ 1,
                                    #Independientes que reciben un pago
                                    subor==2 & indep==1 & tiene_suel==1 ~ 2,
                                    subor==2 & indep==2 & pago==1 ~ 2,
                                    #Independientes que no reciben un pago
                                    subor==2 & indep==1 & tiene_suel==2 ~ 3,
                                    subor==2 & indep==2 & (pago==2 | pago==3) ~ 3))

# Ocupación principal o secundaria
ocupados <- mutate(ocupados, 
                   ocupa=case_when(id_trabajo==1 ~ 1, id_trabajo==2 ~ 1)) %>% 
     dplyr::select(folioviv, foliohog, numren, id_trabajo, tipo_trab, ocupa)

# Distinción de prestaciones en trabajo principal y secundario
ocupados <- dcast(as.data.table(ocupados),  folioviv + foliohog + numren ~ 
                  id_trabajo, value.var=c("tipo_trab", "ocupa"), sep="", fill=0) %>% 
            as.data.frame(ocupados)

ocupados <- mutate(ocupados, 
                  # Identificación de la población trabajadora 
                  # (toda la que reporta al menos un empleo en la base de trabajos.csv)
                   trab=1) %>%
            select(folioviv, foliohog, numren, trab,starts_with("tipo_trab"), 
            starts_with("ocupa"))

fwrite(ocupados, "Bases/ocupados20.csv", row.names=F)

# Población objetivo: no se incluye a huéspedes ni trabajadores domésticos
salud <- pobla
salud <- filter(salud, !(parentesco>=400 & parentesco <500 |
                         parentesco>=700 & parentesco <800))

salud <- left_join(salud, ocupados, by = c("folioviv", "foliohog", "numren"))

salud <- mutate(salud, 
                # PEA (personas de 16 años o más)
                pea=case_when(trab==1 & (edad>=16 & !is.na(edad)) ~ 1, # PEA: ocupada
                              (act_pnea1==1 | act_pnea2==1) & 
                              (edad>=16 & !is.na(edad)) ~ 2, # PEA: desocupada
                              (edad>=16 & !is.na(edad)) & 
                              ((act_pnea1!=1 | is.na(act_pnea1)) & 
                              (act_pnea2!=1 | is.na(act_pnea2))) & 
                              ((act_pnea1>=2 & act_pnea1<=6) | 
                              (act_pnea2>=2 & act_pnea2<=6)) ~ 0), # PNEA
                # Tipo de trabajo
                # Ocupación principal
                tipo_trab1=ifelse(pea==1, tipo_trab1, tipo_trab1), # Depende de un patrón, jefe o superior  
                tipo_trab1=ifelse((pea==0 | pea==2), NA_real_, tipo_trab1), # No depende de un jefe y recibe o tiene asignado un sueldo
                tipo_trab1=ifelse(is.na(pea), NA_real_, tipo_trab1), # No depende de un jefe y no recibe o no tiene asignado un sueldo
                              
                # Ocupación secundaria
                tipo_trab2=ifelse(pea==1, tipo_trab2, tipo_trab2), # Depende de un patrón, jefe o superior  
                tipo_trab2=ifelse((pea==0 | pea==2), NA_real_, tipo_trab2), # No depende de un jefe y recibe o tiene asignado un sueldo
                tipo_trab2=ifelse(is.na(pea), NA_real_, tipo_trab2)) # No depende de un jefe y no recibe o no tiene asignado un sueldo

# Servicios médicos prestaciones laborales
salud <- mutate(salud, 
                # Ocupación principal
                smlab1=case_when(ocupa1==1 & atemed==1 & 
                                (inst_1==1 | inst_2==2 |  inst_3==3 |inst_4==4) & 
                                (inscr_1==1) ~ 1, # Con servicios médicos
                                 ocupa1==1 ~ 0), # Sin servicios médicos
                # Ocupación secundaria
                smlab2=case_when(ocupa2==1 & atemed==1 & 
                                (inst_1==1 | inst_2==2 |inst_3==3 | inst_4==4) & 
                                (inscr_1==1) ~  1, # Con servicios médicos  
                                 ocupa2==1 ~ 0), # Sin servicios médicos
                # Contratación voluntaria de servicios médicos
                smcv=case_when(atemed==1 & 
                              (inst_1==1 | inst_2==2 | inst_3==3 | inst_4==4) & 
                               inscr_6==6 & (edad>=12 & !is.na(edad)) ~  1, # Sí cuenta
                              (edad>=12 & !is.na(edad)) ~ 0)) # No cuenta

# Acceso directo a servicios de salud
salud <- mutate(salud, 
                sa_dir=case_when(
                # Ocupación principal
                tipo_trab1==1 & (smlab1==1) ~ 1, # Con acceso
                tipo_trab1==2 & (smlab1==1 | smcv==1) ~ 1, # Con acceso
                tipo_trab1==3 & (smlab1==1 | smcv==1) ~ 1, # Con acceso
                # Ocupación secundaria
                tipo_trab2==1 & (smlab2==1) ~ 1, # Con acceso
                tipo_trab2==2 & (smlab2==1 | smcv==1) ~ 1, # Con acceso
                tipo_trab2==3 & (smlab2==1 | smcv==1 ) ~ 1, # Con acceso
                TRUE ~0)) # Sin acceso

# Núcleos familiares
salud <- mutate(salud,
                par=case_when((parentesco>=100 & parentesco<200) ~ 1, # Jefe o jefa del hogar 
                              (parentesco>=200 & parentesco<300) ~ 2, # Cónyuge del  jefe/a 
                              (parentesco>=300 & parentesco<400) ~ 3, # Hijo del jefe/a 
                               parentesco==601 ~ 4, # Padre o Madre del jefe/a
                               parentesco==615 ~ 5, # Suegro del jefe/a
                               TRUE ~ 6), # Sin parentesco directo
                
# Asimismo, se utilizará la información relativa a la asistencia a la escuela
               inas_esc=case_when(asis_esc==1 ~ 0,   # Sí asiste
                                  asis_esc==2 ~ 1 )) # No asiste

# En primer lugar se identifican los principales parentescos respecto a la 
# jefatura del hogar y si ese miembro cuenta con acceso directo
salud <- mutate(salud,
                jef=case_when(par==1 & sa_dir==1 & 
                              (((inst_2==2 | inst_3==3) & inscr_6==6) & 
                               (is.na(inst_1)  & is.na(inst_4) & is.na(inst_6)) &
                               (is.na(inscr_1)  & is.na(inscr_2)  & is.na(inscr_3)  & 
                                is.na(inscr_4)  & is.na(inscr_5)  & is.na(inscr_7))) ~ NA_real_,
                                par==1 & sa_dir==1 ~ 1),
                cony=case_when(par==2 & sa_dir==1 & 
                              (((inst_2==2 | inst_3==3) & inscr_6==6) &
                               (is.na(inst_1)  & is.na(inst_4) & is.na(inst_6)) &
                               (is.na(inscr_1)  & is.na(inscr_2)  & is.na(inscr_3)  & 
                                is.na(inscr_4)  & is.na(inscr_5)  & is.na(inscr_7) )) ~ NA_real_,
                                par==2 & sa_dir==1 ~ 1),
                hijo=case_when(par==3 & sa_dir==1 & 
                              (((inst_2==2 | inst_3==3) & inscr_6==6) & 
                               (is.na(inst_1)  & is.na(inst_4) & is.na(inst_6)) & 
                               (is.na(inscr_1)  & is.na(inscr_2)  & is.na(inscr_3)  & 
                                is.na(inscr_4)  & is.na(inscr_5)  & is.na(inscr_7) )) ~ NA_real_,
                                par==3 & sa_dir==1  ~ 1))

salud <- as.data.table(salud)[, c("jef_sa", "cony_sa", "hijo_sa") :=
                              .(sum(jef, na.rm=TRUE),
                                sum(cony, na.rm=TRUE),
                                sum(hijo, na.rm=TRUE)), by=.(folioviv, foliohog)] %>% 
          mutate(jef_sa=if_else(jef_sa>0, 1,jef_sa),  # Acceso directo a servicios de salud de la jefatura del hogar
                 cony_sa=if_else(cony_sa>0, 1,cony_sa), # Acceso directo a servicios de salud del cónyuge de la jefatura del hogar
                 hijo_sa=if_else(hijo_sa>0, 1,hijo_sa), # Acceso directo a servicios de salud de hijos(as) de la jefatura del hogar
       
# Otros núcleos familiares: se identifica a la población con acceso a servicios de salud
# mediante otros núcleos familiares a través de la afiliación
# o inscripción a servicios de salud por algún familiar dentro o 
# fuera del hogar, muerte del asegurado o por contratación propia;

                s_salud=case_when(atemed==1 & (inst_1==1 | inst_2==2 | inst_3==3 | inst_4==4) & 
                                 (inscr_3==3| inscr_4==4 | inscr_6==6 | inscr_7==7) ~ 1, # Sí cuenta
                                 !is.na(pop_insabi) & !is.na(atemed) ~ 0)) # No cuenta

# Indicador de carencia por servicios de salud
################################################################################
# Se considera en situación de carencia por acceso a servicios de salud
# a la población que:
#  
#  1. No cuente con adscripción o derecho a recibir servicios médicos de alguna 
#     institución  que  los  preste,  incluyendo  el  Seguro  Popular,  las  
#     instituciones  de  seguridad  social  (IMSS,  ISSSTE  federal  o  estatal,  
#     PEMEX, Ejército o Marina) o los servicios médicos privados
################################################################################

salud <- mutate(salud,
              
# Indicador de carencia por acceso a los servicios de salud

                # Acceso directo
                ic_asalud=case_when(sa_dir==1 ~ 0,
                                    # Parentesco directo: jefatura
                                    par==1 & cony_sa==1 ~ 0, # No presenta carencia
                                    par==1 & pea==0 & hijo_sa==1 ~ 0, # No presenta carencia
                                    # Parentesco directo: cónyuge
                                    par==2 & jef_sa==1 ~ 0, # No presenta carencia
                                    par==2 & pea==0 & hijo_sa==1 ~ 0, # No presenta carencia
                                    # Parentesco directo: descendientes
                                    par==3 & edad<16 & jef_sa==1 ~ 0, # No presenta carencia
                                    par==3 & edad<16 & cony_sa==1 ~ 0, # No presenta carencia
                                    par==3 & (edad>=16 & edad<=25) & inas_esc==0 & jef_sa==1 ~ 0, # No presenta carencia
                                    par==3 & (edad>=16 & edad<=25) & inas_esc==0 & cony_sa==1 ~ 0, # No presenta carencia
                                    # Parentesco directo: ascendientes
                                    par==4 & pea==0 & jef_sa==1 ~ 0, # No presenta carencia
                                    par==5 & pea==0 & cony_sa==1 ~ 0, # No presenta carencia
                                    # Otros núcleos familiares
                                    s_salud==1 ~ 0, # No presenta carencia
                                    # Acceso reportado
                                    pop_insabi==1 | (pop_insabi==2 & atemed==1 & 
                                    (inst_1==1 | inst_2==2 | inst_3==3 | 
                                    inst_4==4 | inst_5==5 | inst_6==6)) | 
                                    segvol_2==2 ~ 0, # No presenta carencia
                                    TRUE~ 1), # Presenta carencia
                # Población con presencia de discapacidad, sea física o mental
                discap=case_when((disc_camin>="1" & disc_camin<="2") ~1, # Con presencia de discapacidad
                                 (disc_ver  >="1" & disc_ver  <="2") ~1, # Con presencia de discapacidad
                                 (disc_brazo>="1" & disc_brazo<="2") ~1, # Con presencia de discapacidad
                                 (disc_apren>="1" & disc_apren<="2") ~1, # Con presencia de discapacidad  
                                 (disc_oir  >="1" & disc_oir  <="2") ~1, # Con presencia de discapacidad
                                 (disc_vest >="1" & disc_vest <="2") ~1, # Con presencia de discapacidad
                                 (disc_habla>="1" & disc_habla<="2") ~1, # Con presencia de discapacidad 
                                 (disc_acti>="1" & disc_acti<="2")  ~1,  # Con presencia de discapacidad
                                 (disc_camin=="&" & disc_ver=="&" &
                                  disc_brazo=="&" & disc_apren=="&" &
                                  disc_oir=="&" & disc_vest=="&" &
                                  disc_habla=="&" & disc_acti=="&") ~ NA_real_,
                                 TRUE ~0)) %>%  # Sin presencia de discapacidad
          select(folioviv, foliohog, numren, sexo, 
                 starts_with("sa_"), ends_with("_sa"), 
                 pop_insabi, atemed, starts_with("inst_"),
                 starts_with("inscr_"), starts_with("segvol_"), 
                 ic_asalud,discap)

fwrite(salud, "Bases/ic_asalud20.csv", row.names=F)
gdata::keep(rezedu, salud, pobla, sure=T)

################################################################################
#  Pararte III Indicadores de carencias sociales:
#  INDICADOR DE CARENCIA POR ACCESO A LA SEGURIDAD SOCIAL 
################################################################################

# Prestaciones laborales
trab <- fread("Bases de datos/trabajos.csv") %>% rename_all(tolower)

# Tipo de trabajador: identifica la población subordinada e independiente
trab<- mutate(trab, 
              tipo_trab=case_when(
                                  # Subordinados   
                                  subor==1 ~ 1,
                                  # Independientes que reciben un pago
                                  subor==2 & indep==1 & tiene_suel==1 ~ 2,
                                  subor==2 & indep==2 & pago==1 ~ 2,
                                  # Independientes que no reciben un pago
                                  subor==2 & indep==1 & tiene_suel==2 ~ 3,
                                  subor==2 & indep==2 & (pago==2 | pago==3) ~ 3) ,
              # Ahorro para el retiro o pensión para la vejez (SAR, Afore)
              aforlab=case_when(is.na(pres_8) ~ 0,
                                pres_8==8 ~ 1),
              # Ocupación principal o secundaria
              id_trabajo= as.numeric(id_trabajo),
              ocupa=case_when(id_trabajo==1 ~ 1,
                              id_trabajo==2 ~ 1))

# Distinción de prestaciones en trabajo principal y secundario
trab <- dplyr::select(trab, folioviv, foliohog, numren, id_trabajo, tipo_trab, aforlab, ocupa) %>% 
               as.data.table() %>% 
               dcast(folioviv + foliohog + numren ~ id_trabajo, 
                     value.var = c("tipo_trab", "aforlab", "ocupa"), 
                     sep="", fill=0) %>%
                # Identificación de la población trabajadora toda 
                # la que reporta al menos un empleo en la base de trabajos.csv)
               mutate(trab=1) %>%
        dplyr::select(folioviv, foliohog, numren, trab, tipo_trab1, tipo_trab2, 
                      aforlab1, aforlab2, ocupa1, ocupa2)

fwrite(trab, "Bases/prestaciones20.csv", row.names=F)

# Ingresos por jubilaciones o pensiones

pens <- fread("Bases de datos/ingresos.csv") %>% 
        rename_all(tolower) %>%
        filter(clave=="P032" | clave=="P033" | clave=="P104" | clave=="P045") 

# Definición de los deflactores 2020 
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

# Se deflactan los ingresos por jubilaciones, pensiones y programas de adultos 
# mayores de acuerdo con el mes de levantamiento

pens <-mutate(pens, 
              ing_6=case_when(mes_6==2 ~ ing_6/feb20,
                              mes_6==3 ~ ing_6/mar20,
                              mes_6==4 ~ ing_6/abr20,
                              mes_6==5 ~ ing_6/may20),
              ing_5=case_when(mes_5==3 ~ ing_5/mar20,
                              mes_5==4 ~ ing_5/abr20,
                              mes_5==5 ~ ing_5/may20,
                              mes_5==6 ~ ing_5/jun20),
              ing_4=case_when(mes_4==4 ~ ing_4/abr20,
                              mes_4==5 ~ ing_4/may20,
                              mes_4==6 ~ ing_4/jun20,
                              mes_4==7 ~ ing_4/jul20),
              ing_3=case_when(mes_3==5 ~ ing_3/may20,
                              mes_3==6 ~ ing_3/jun20,
                              mes_3==7 ~ ing_3/jul20,
                              mes_3==8 ~ ing_3/ago20),
              ing_2=case_when(mes_2==6 ~ ing_2/jun20,
                              mes_2==7 ~ ing_2/jul20,
                              mes_2==8 ~ ing_2/ago20,
                              mes_2==9 ~ ing_2/sep20),
              ing_1=case_when(mes_1==7 ~ ing_1/jul20,
                              mes_1==8 ~ ing_1/ago20,
                              mes_1==9 ~ ing_1/sep20,
                              mes_1==10 ~ ing_1/oct20)) 

pens <- mutate(pens, 
               # Ingreso promedio mensual por jubilaciones y pensiones
               ing_pam=case_when(clave=="P104" | clave=="P045" ~ 
                       apply(dplyr::select(pens, ing_1, ing_2, ing_3, ing_4, ing_5, ing_6), 1, mean),
                       TRUE ~ 0),
               # Ingreso promedio mensual por programas de adultos mayores
               ing_pens=case_when(clave=="P032" | clave=="P033" ~ 
                        apply(dplyr::select(pens, ing_1, ing_2, ing_3, ing_4, ing_5, ing_6), 1, mean),
                        TRUE ~0)) 


pens <- as.data.table(pens)[, lapply(.SD, sum, na.rm=T), 
                           by =list(folioviv, foliohog, numren), 
                           .SDcols = c("ing_pens", "ing_pam")] 

fwrite(pens, "Bases/pensiones20.csv", row.names=F)

# Construcción del indicador
segsoc <- pobla

# Población objetivo: no se incluye a huéspedes ni trabajadores domésticos
segsoc <- filter(segsoc, !(parentesco>=400 & parentesco <500 | 
                           parentesco>=700 & parentesco <800))

# Integración de bases
segsoc <- left_join(segsoc, trab, by = c("folioviv", "foliohog", "numren"))
segsoc <- left_join(segsoc, pens, by = c("folioviv", "foliohog", "numren"))


segsoc <- mutate(segsoc, 
                 # PEA (personas de 16 años o más)
                 pea=case_when(trab==1 & (edad>=16 & !is.na(edad)) ~ 1, # PEA: ocupada
                               (act_pnea1==1 | act_pnea2==1) & 
                               (edad>=16 & !is.na(edad)) ~ 2, # PEA: desocupada
                               (edad>=16 & !is.na(edad)) & 
                               ((act_pnea1!=1 | is.na(act_pnea1)) & 
                               (act_pnea2!=1 | is.na(act_pnea2))) & 
                               ((act_pnea1>=2 & act_pnea1<=6) | 
                               (act_pnea2>=2 & act_pnea2<=6))  ~ 0), # PNEA
                # Tipo de trabajo
                # Ocupación principal
                tipo_trab1=ifelse(pea==1, tipo_trab1, tipo_trab1),  # Depende de un patrón, jefe o superior
                tipo_trab1=ifelse((pea==0 | pea==2), NA_real_, tipo_trab1), # No depende de un jefe y recibe o tiene asignado un sueldo
                tipo_trab1=ifelse(is.na(pea), NA_real_, tipo_trab1), # No depende de un jefe y no recibe o no tiene asignado un sueldo
                               
                # Ocupación secundaria
                tipo_trab2=ifelse(pea==1, tipo_trab2, tipo_trab2), # Depende de un patrón, jefe o superior
                tipo_trab2=ifelse((pea==0 | pea==2), NA, tipo_trab2), # No depende de un jefe y recibe o tiene asignado un sueldo
                tipo_trab2=ifelse(is.na(pea), NA, tipo_trab2), # No depende de un jefe y no recibe o no tiene asignado un sueldo
                               
                # Jubilados o pensionados
                jub=case_when(trabajo_mp==2 & act_pnea1==2 | act_pnea2==2 ~ 1, # Población pensionada o jubilada
                              ing_pens>0 &  !is.na(ing_pens) ~ 1, # Población pensionada o jubilada
                              inscr_2==2 ~ 1, # Población pensionada o jubilada
                              TRUE ~0), # Población no pensionada o jubilada
# Prestaciones básicas

# Prestaciones laborales (Servicios médicos)
               
              # Ocupación principal
              smlab1=case_when(ocupa1==1 & atemed==1 & 
                              (inst_1==1 | inst_2==2 |  inst_3==3 |inst_4==4) & 
                              (inscr_1==1) ~ 1, # Sin servicios médicos
                               ocupa1==1 ~ 0), # Con servicios médicos
              # Ocupación secundaria
              smlab2=case_when(ocupa2==1 & atemed==1 & 
                              (inst_1==1 | inst_2==2 |inst_3==3 | inst_4==4) & 
                              (inscr_1==1) ~  1, # Sin servicios médicos
                               ocupa2==1 ~ 0), # Con servicios médicos
               
# Contratación voluntaria: servicios médicos y ahorro para el retiro o pensión para 
# la vejez (SAR, Afore, Haber de retiro)
              
              # Servicios médicos
              smcv=case_when(atemed==1 & (inst_1==1 | inst_2==2 | inst_3==3 | inst_4==4) &
                             inscr_6==6 & (edad>=12 & !is.na(edad)) ~ 1, # Sí cuenta
                             (edad>=12 & !is.na(edad))  ~ 0), # No cuenta
              # SAR o Afore
              aforecv=case_when(segvol_1==1 & (edad>=12 & !is.na(edad)) ~ 1, # Sí cuenta
                                is.na(segvol_1) & (edad>=12 & !is.na(edad)) ~ 0)) # No cuenta
              

segsoc <- mutate(segsoc,                 
                # Acceso directo a la seguridad social
                ss_dir=case_when(  
                # Ocupación principal
                tipo_trab1==1 & smlab1==1 ~ 1, # Con acceso
                tipo_trab1==2 & ((smlab1==1 | smcv==1) & (aforlab1==1 | aforecv==1)) ~ 1, # Con acceso
                tipo_trab1==3 & ((smlab1==1 | smcv==1) & aforecv==1) ~ 1, # Con acceso
                # Ocupación secundaria
                tipo_trab2==1 & smlab2==1 ~ 1, # Con acceso
                tipo_trab2==2 & ((smlab2==1 | smcv==1) & (aforlab2==1 | aforecv==1)) ~ 1, # Con acceso  
                tipo_trab2==3 & ((smlab2==1 | smcv==1) & aforecv==1) ~ 1, # Con acceso
                # Jubilados y pensionados
                jub==1 ~ 1, # Con acceso
                TRUE ~0), # Sin acceso
                
                # Núcleos familiares
                par=case_when(parentesco>=100 & parentesco<200 ~ 1, # Jefe o jefa del hogar
                              parentesco>=200 & parentesco<300 ~ 2, # Cónyuge del  jefe/a
                              parentesco>=300 & parentesco<400 ~ 3, # Hijo del jefe/a
                              parentesco==601 ~ 4, # Padre o Madre del jefe/a
                              parentesco==615 ~ 5, # Suegro del jefe/a
                              TRUE ~6), # Sin parentesco directo
                # Asimismo, se utilizará la información relativa a 
                # la asistencia a la escuela
                inas_esc=case_when(asis_esc==1 ~ 0,   # Sí asiste
                                   asis_esc==2 ~ 1 )) # No asiste

segsoc <- mutate(segsoc,                 
# En primer lugar se identifican los principales parentescos respecto a la jefatura 
# del hogar y si ese miembro cuenta con acceso directo
                  jef=case_when(par==1 & ss_dir==1 & 
                                (((inst_2=="2" | inst_3=="3") & inscr_6=="6") & 
                                 (is.na(inst_1)  & is.na(inst_4) & is.na(inst_6)) &
                                 (is.na(inscr_1)  & is.na(inscr_2)  & is.na(inscr_3)  & 
                                  is.na(inscr_4)  & is.na(inscr_5)  & is.na(inscr_7))) ~ NA_real_,
                                 par==1 & ss_dir==1 ~ 1),
                  
                  cony=case_when(par==2 & ss_dir==1 & 
                                (((inst_2=="2" | inst_3=="3") & inscr_6=="6") &
                                 (is.na(inst_1)  & is.na(inst_4) & is.na(inst_6)) &
                                  (is.na(inscr_1)  & is.na(inscr_2)  & is.na(inscr_3)  & 
                                   is.na(inscr_4)  & is.na(inscr_5)  & is.na(inscr_7) )) ~ NA_real_,
                                   par==2 & ss_dir==1 ~ 1),
                  
                  hijo=case_when(par==3 & ss_dir==1 & 
                                (((inst_2=="2" | inst_3=="3") & inscr_6=="6") & 
                                 (is.na(inst_1)  & is.na(inst_4) & is.na(inst_6)) & 
                                 (is.na(inscr_1)  & is.na(inscr_2)  & is.na(inscr_3)  & 
                                  is.na(inscr_4)  & is.na(inscr_5)  & is.na(inscr_7) )) ~ NA_real_,
                                  par==3 & ss_dir==1 & jub==1 & (edad>25 & !is.na(edad)) ~ 1,
                                  par==3 & ss_dir==1 & jub==0 ~ 1))

segsoc <- as.data.table(segsoc)[, c("jef_ss", "cony_ss", "hijo_ss") :=
                                .(sum(jef, na.rm=TRUE),
                                  sum(cony, na.rm=TRUE),
                                  sum(hijo, na.rm=TRUE)), by=.(folioviv, foliohog)] %>% 
                          mutate(jef_ss=if_else(jef_ss>0, 1,jef_ss), # Acceso directo a la seguridad social de la jefatura del hogar
                                 cony_ss=if_else(cony_ss>0, 1,cony_ss), # Acceso directo a la seguridad social de cónyuge de la jefatura del hogar
                                 hijo_ss=if_else(hijo_ss>0, 1,hijo_ss)) # Acceso directo a la seguridad social de hijos(as) de la jefatura del hogar

# Otros núcleos familiares: se identifica a la población con acceso a la seguridad
# social mediante otros núcleos familiares a través de la afiliación
# o inscripción a servicios de salud por algún familiar dentro o 
# fuera del hogar, muerte del asegurado o por contratación propia

segsoc <-  mutate(segsoc,
                  s_salud=case_when(atemed==1 & 
                                   (inst_1==1 | inst_2==2 | inst_3==3 |inst_4==4) & 
                                   (inscr_3==3 | inscr_4==4 | inscr_6==6 | inscr_7==7) ~ 1, # Con acceso
                                    !is.na(pop_insabi) & !is.na(atemed) ~ 0)) # Sin acceso

# Programas sociales de pensiones para adultos mayores

# Se identifica a las personas de 65 años o más que reciben un programa para adultos mayores
# si el monto recibido es mayor o igual al promedio de la línea de pobreza extrema
# por ingresos rural y urbana

# Valor monetario de las líneas de pobreza extrema por ingresos rural y urbana
lp1_urb <- 1702.28
lp1_rur <- 1299.30
lp_pam <- (lp1_urb + lp1_rur)/2

segsoc <- mutate(segsoc,
                 pam=case_when(edad>=65 & !is.na(edad) & 
                               ing_pam>=lp_pam & !is.na(ing_pam) ~ 1, # Recibe
                               edad>=65 & !is.na(edad) ~ 0)) # No recibe      

################################################################################
# Indicador de carencia por acceso a la seguridad social
#
# Se encuentra en situación de carencia por acceso a la seguridad social
# a la población que:
#  1. No disponga de acceso directo a la seguridad social.
#  2. No cuente con parentesco directo con alguna persona dentro del hogar
#     que tenga acceso directo.
#  3. No recibe servicios médicos por parte de algún familiar dentro o
#     fuera del hogar, por muerte del asegurado o por contratación propia.
#  4. No recibe ingreso por parte de un programa de adultos mayores donde el
#     monto sea mayor o igual al valor promedio de la canasta alimentaria 
#     rural y urbana.
################################################################################

#Indicador de carencia por acceso a la seguridad social
segsoc <-mutate(segsoc, 
                ic_segsoc=NA,
                ic_segsoc=case_when(
                # Acceso directo
                ss_dir==1 ~ 0, # No presenta carencia
                # Parentesco directo: jefatura
                par==1 & cony_ss==1 ~ 0, # No presenta carencia
                par==1 & pea==0 & hijo_ss==1 ~ 0, # No presenta carencia
                # Parentesco directo: cónyuge
                par==2 & jef_ss==1 ~ 0, # No presenta carencia 
                par==2 & pea==0 & hijo_ss==1 ~ 0, # No presenta carencia
                # Parentesco directo: descendientes
                par==3 & edad<16 & jef_ss==1 ~ 0, # No presenta carencia
                par==3 & edad<16 & cony_ss==1 ~ 0, # No presenta carencia
                par==3 & (edad>=16 & edad<=25) & inas_esc==0 & jef_ss==1 ~ 0, # No presenta carencia
                par==3 & (edad>=16 & edad<=25) & inas_esc==0 & cony_ss==1 ~ 0, # No presenta carencia
                # Parentesco directo: ascendientes
                par==4 & pea==0 & jef_ss==1 ~ 0, # No presenta carencia
                par==5 & pea==0 & cony_ss==1 ~ 0, # No presenta carencia
                # Otros núcleos familiares
                s_salud==1 ~ 0, # No presenta carencia
                # Programa de adultos mayores
                pam==1 ~ 0, # No presenta carencia
                TRUE ~1)) # Presenta carencia
                

segsoc <- select(segsoc, folioviv, foliohog, numren,starts_with("tipo_trab"), 
                 starts_with("aforlab"), starts_with("smlab"), smcv, aforecv, 
                 pea, jub, ss_dir, par, ends_with("_ss"), s_salud, pam, ing_pam,
                 ic_segsoc)

fwrite(segsoc, "Bases/ic_segsoc20.csv", row.names=F)
gdata::keep(rezedu, salud, segsoc, pobla, sure=T)

################################################################################
# Parte IV Indicadores de carencias sociales:
# INDICADOR DE CARENCIA POR CALIDAD Y ESPACIOS DE LA VIVIENDA
################################################################################

# Material de construcción de la vivienda

viviendas <- fread("Bases de datos/viviendas.csv") %>% rename_all(tolower)
cev <- viviendas

concen <- fread("Bases de datos/concentradohogar.csv") %>% rename_all(tolower)
cev <- left_join(cev, concen,  by = c("folioviv")) %>% 
          mutate(
                # Indicador de carencia por material de piso de la vivienda
                icv_pisos=as.numeric(mat_pisos), # Material de los pisos de la vivienda
                icv_pisos=case_when(icv_pisos>=2 ~ 0,
                                   icv_pisos==1 ~ 1),
                # Indicador de carencia por material de techos de la vivienda
                icv_techos=as.numeric(mat_techos), # Material de los techos de la vivienda
                icv_techos=case_when(icv_techos >=3 ~ 0,
                                    icv_techos <=2 ~ 1),
                # Indicador de carencia por material de muros de la vivienda
                icv_muros=as.numeric(mat_pared), # Material de muros en la vivienda
                icv_muros=case_when(icv_muros >=6 ~ 0,
                                   icv_muros <=5 ~ 1),

                # Espacios en la vivienda (Hacinamiento)
                # Número de residentes en la vivienda
                num_ind = tot_resid ,
                # Número de cuartos en la vivienda
                num_cua = num_cuarto ,
                # Índice de hacinamiento
                cv_hac=num_ind/num_cua,
                # Indicador de carencia por hacinamiento en la vivienda
                icv_hac=case_when(
                                 cv_hac>2.5 & !is.na(cv_hac) ~ 1,
                                 cv_hac<=2.5 ~ 0 ))


# Indicador de carencia por calidad y espacios de la vivienda
################################################################################
# Se considera en situación de carencia por calidad y espacios 
# de la vivienda a las personas que residan en viviendas
# que presenten, al menos, una de las siguientes características:
#  
# 1. El material de los pisos de la vivienda es de tierra
# 2. El material del techo de la vivienda es de lámina de cartón o desechos.
# 3. El material de los muros de la vivienda es de embarro o bajareque, de
#    carrizo, bambú o palma, de lámina de cartón, metálica o asbesto, o
#    material de desecho
# 4. La razón de personas por cuarto (hacinamiento) es mayor que 2.5
################################################################################

cev <- mutate(cev,
              ic_cv=case_when(
              is.na(icv_pisos) | is.na(icv_techos) | is.na(icv_muros) | is.na(icv_hac) ~ NA_real_,
              icv_pisos==1 | icv_techos==1 | icv_muros==1 | icv_hac==1 ~ 1,     # Con carencia
              icv_pisos==0 & icv_techos==0 & icv_muros==0 & icv_hac==0 ~ 0)) %>% # Sin carencia
      select(folioviv, foliohog, icv_pisos, icv_techos, 
             icv_muros, icv_hac, ic_cv) 

fwrite(cev, "Bases/ic_cev20.csv", row.names=F)
gdata::keep(rezedu, salud, segsoc, cev, concen, viviendas, pobla, sure=T)

################################################################################
# Parte V Indicadores de Privación Social:
# INDICADOR DE CARENCIA POR ACCESO A LOS SERVICIOS BÁSICOS DE LA VIVIENDA
################################################################################

sbv <- left_join(concen, viviendas, by = c("folioviv")) 

sbv <- mutate(sbv,
              # Indicador de carencia por acceso al agua
              isb_agua=case_when(procaptar==1 & disp_agua=="4" ~ 0,
                                 disp_agua>=3 & !is.na(disp_agua) ~ 1,
                                 disp_agua<=2 & !is.na(disp_agua) ~ 0),
              # Indicador de carencia por servicio de drenaje
              isb_dren=case_when(drenaje>=3 ~1,
                                 drenaje<=2 ~0),
              # Indicador de carencia por servicios de electricidad
              isb_luz=case_when(disp_elect>=5 ~1,
                                disp_elect<=4 ~0),
              # Indicador de carencia por combustible para cocinar
              combus=as.numeric(combustible),
              estufa=as.numeric(estufa_chi),
              isb_combus=(case_when((combus==1 | combus==2) & estufa==2 ~ 1,
                         (combus==1 | combus==2) & estufa==1 ~ 0,
                          combus>=3 & combus<=6 ~ 0)))

# Indicador de carencia por acceso a los servicios básicos en la vivienda
################################################################################
# Se considera en situación de carencia por servicios básicos en la vivienda 
# a las personas que residan en viviendas que presenten, al menos, 
# una de las siguientes características:
#  
# 1. El agua se obtiene de un pozo, río, lago, arroyo, pipa, o bien, el agua 
#    entubada la adquieren por acarreo de otra vivienda, o de la llave
#    pública o hidrante.
# 2. No cuentan con servicio de drenaje o el desagüe tiene conexión a
#    una tubería que va a dar a un río, lago, mar, barranca o grieta.
# 2. No disponen de energía eléctrica.
# 3. El combustible que se usa para cocinar o calentar los alimentos es
#    leña o carbón sin chimenea.
################################################################################

sbv <- mutate(sbv,              
              ic_sbv=case_when(
              is.na(isb_agua) | is.na(isb_dren) | is.na(isb_luz) | is.na(isb_combus) ~ NA_real_,
              isb_agua==1 | isb_dren==1 | isb_luz==1 | isb_combus==1 ~ 1, # Con carencia
              isb_agua==0 & isb_dren==0 & isb_luz==0 & isb_combus==0 ~ 0)) %>% # Sin carencia
       select(folioviv, foliohog, isb_agua, isb_dren, isb_luz, 
              isb_combus, ic_sbv)

fwrite(sbv, "Bases/ic_sbv20.csv", row.names=F)
gdata::keep(rezedu, salud, segsoc, cev, sbv, concen, viviendas, pobla, sure=T)

################################################################################
# Parte VI Indicadores de Privación Social:
# INDICADOR DE CARENCIA POR ACCESO A LA ALIMENTACIÓN NUTRITIVA Y DE CALIDAD
################################################################################

menores <- pobla

# Población objetivo: no se incluye a huéspedes ni trabajadores domésticos
menores <- filter(menores, !(parentesco>=400 & parentesco <500 | 
                             parentesco>=700 & parentesco <800))

# Indicador de hogares con menores de 18 años
menores <- mutate(menores, men=case_when(edad>=0 & edad<=17 ~ 1))

menores <- as.data.table(menores)[, lapply(.SD, sum, na.rm = TRUE), 
                                  by =.(folioviv, foliohog), .SDcols = c("men")] %>%
          mutate(id_men=case_when(men>=1 & !is.na(men) ~ 1, men==0 ~ 0)) %>%
          select(folioviv, foliohog, id_men)

fwrite(menores, "Bases/menores20.csv", row.names=F)

hog <- fread("Bases de datos/hogares.csv") %>% rename_all(tolower)
ali <-hog

# Parte 1. Grado de inseguridad alimentaria
ali <- mutate(ali,
            
# SEIS PREGUNTAS PARA HOGARES SIN POBLACIÓN MENOR A 18 AÑOS
            
            # Algún adulto tuvo una alimentación basada en muy poca variedad de alimentos
            ia_1ad=case_when(acc_alim4== 2 ~ 0, # No
                             acc_alim4== 1 ~ 1, # Sí
                             is.na(acc_alim4) ~ 0), # No
            # Algún adulto dejó de desayunar, comer o cenar
            ia_2ad=case_when(acc_alim5== 2 ~ 0,  # No
                             acc_alim5== 1 ~ 1, # Sí
                             is.na(acc_alim5) ~ 0),  # No
            # Algún adulto comió menos de lo que debía comer
            ia_3ad=case_when(acc_alim6== 2 ~ 0,  # No
                             acc_alim6== 1 ~ 1, # Sí
                             is.na(acc_alim6) ~ 0),  # No
            # El hogar se quedó sin comida
            ia_4ad=case_when(acc_alim2== 2 ~ 0,  # No
                             acc_alim2== 1 ~ 1, # Sí
                             is.na(acc_alim2) ~ 0), # No
            # Algún adulto sintió hambre pero no comió
            ia_5ad=case_when(acc_alim7== 2 ~ 0, # No
                             acc_alim7== 1 ~ 1, # Sí
                             is.na(acc_alim7) ~ 0), # No
            # Algún adulto solo comió una vez al día o dejó de comer todo un día
            ia_6ad=case_when(acc_alim8== 2 ~ 0,  # No
                             acc_alim8== 1 ~ 1, # Sí
                             is.na(acc_alim8) ~ 0), # No
            
# SEIS PREGUNTAS PARA HOGARES CON POBLACIÓN MENOR A 18 AÑOS
            
            # Alguien de 0 a 17 años tuvo una alimentación basada en muy poca variedad de alimentos
            ia_7men =case_when(acc_alim11== 2 ~ 0, # No
                               acc_alim11== 1 ~ 1, # Sí
                               is.na(acc_alim11) ~ 0), # No
            # Alguien de 0 a 17 años comió menos de lo que debía
            ia_8men =case_when(acc_alim12== 2 ~ 0, # No
                               acc_alim12== 1 ~ 1, # Sí
                               is.na(acc_alim12) ~ 0), # No
            # Se tuvo que disminuir la cantidad servida en las comidas a alguien de 0 a 17 años
            ia_9men =case_when(acc_alim13== 2 ~ 0, # No
                               acc_alim13== 1 ~ 1, # Sí
                               is.na(acc_alim13) ~ 0), # No
            # Alguien de 0 a 17 años sintió hambre pero no comió
            ia_10men=case_when(acc_alim14== 2 ~ 0, # No
                               acc_alim14== 1 ~ 1, # Sí
                               is.na(acc_alim14) ~ 0), # No
            # Alguien de 0 a 17 años se acostó con hambre
            ia_11men=case_when(acc_alim15== 2 ~ 0, # No
                               acc_alim15== 1 ~ 1, # Sí
                               is.na(acc_alim15) ~ 0), # No
            # Alguien de 0 a 17 años comió una vez al día o dejó de comer todo un día
            ia_12men=case_when(acc_alim16== 2 ~ 0, # No
                               acc_alim16== 1 ~ 1, # Sí
                               is.na(acc_alim16) ~ 0)) # No

# Construcción de la escala de inseguridad alimentaria
ali <- left_join(ali, menores, by = c("folioviv", "foliohog"))
ali <- mutate(ali,
            
              # Escala para hogares sin menores de 18 años
              tot_iaad=case_when(id_men==0 ~ ia_1ad + ia_2ad + ia_3ad + 
                                 ia_4ad + ia_5ad+ ia_6ad),
              # Escala para hogares con menores de 18 años
              tot_iamen=case_when(id_men==1 ~ ia_1ad + ia_2ad + ia_3ad + ia_4ad + ia_5ad + ia_6ad +
                                  ia_7men + ia_8men + ia_9men+ia_10men + ia_11men+ia_12men), 
              # Grado de inseguridad alimentaria
              ins_ali=case_when(
                                  # Seguridad alimentaria  
                                  tot_iaad==0 | tot_iamen==0 ~ 0, 
                                  # Inseguridad alimentaria leve
                                  tot_iaad==1 | tot_iaad==2 | tot_iamen==1 |tot_iamen==2 |tot_iamen==3 ~ 1,
                                  # Inseguridad alimentaria moderada
                                  tot_iaad==3 | tot_iaad==4 | tot_iamen==4 | tot_iamen==5 |tot_iamen==6 |tot_iamen==7 ~ 2,
                                  # Inseguridad alimentaria severa
                                  tot_iaad==5 | tot_iaad==6 | tot_iamen>=8 & !is.na(tot_iamen) ~ 3 ))

# Se genera el indicador de carencia por acceso a la alimentación que
# considera en situación de carencia a la población en hogares que 
# presenten inseguridad alimentaria moderada o severa

ali <- mutate(ali, 
            
#Indicador de carencia por acceso a la alimentación  
            ic_ali=case_when(ins_ali==2 | ins_ali==3 ~ 1, # Con carencia
                             ins_ali==0 | ins_ali==1 ~ 0)) # Sin carencia

# Parte 2. Limitación en el consumo de alimentos

# Se multiplica cada uno de los 12 grupos de alimentos por el ponderador 
# utilizado por el Programa Mundial de Alimentos (PMA) de las Naciones Unidas
#
# Grupo 1: (maíz, avena, arroz, sorgo, mijo, pan y otros cereales) y 
#          (yuca, papas, camotes y otros tubérculos)
# Grupo 2: frijoles, chícharos, cacahuates, nueces
# Grupo 3: vegetales y hojas
# Grupo 4: frutas
# Grupo 5: carne de res, cabra, aves, cerdo, huevos y pescado
# Grupo 6: leche, yogur y otros lácteos
# Grupo 7: azúcares y productos azucarados
# Grupo 8: aceites, grasas y mantequilla
# Grupo 9: especias, té, café, sal, polvo de pescado, pequeñas cantidades de 
#          leche para el té

# El ponderador para el Grupo 1 es 2, para el Grupo 2 es 3, para el Grupo 3 y 4 
# es 1, para el Grupo 5 y Grupo 6 es 4, para el Grupo 7 y 8 es 0.5, y para el 
# Grupo 9 es 0

ali <- mutate(ali,
              cpond1  = if_else(alim17_1>alim17_2, alim17_1, alim17_2)*2,
                                cpond3  = alim17_3  * 1,
                                cpond4  = alim17_4  * 1,
              cpond5  = apply(ali[,c("alim17_5", "alim17_6", "alim17_7")], 1, max)* 4,
              cpond8  = alim17_8  * 3   ,
              cpond9  = alim17_9  * 4   ,
              cpond10 = alim17_10 * 0.5 ,
              cpond11 = alim17_11 * 0.5 ,
              cpond12 = alim17_12 * 0   ,
              
              # Puntaje total de consumo ponderado de alimentos, indica el número ponderado 
              # de grupos de alimentos que se consumieron en los últimos siete días     
              tot_cpond= cpond1 + cpond3 + cpond4 + cpond5 + 
                         cpond8 + cpond9 + cpond10 + cpond11 + cpond12,
            # Se categoriza la dieta consumida en los hogares
            # Dieta consumida en los hogares              
            dch=case_when(tot_cpond >=  0 & tot_cpond <= 28   ~ 1,  # Pobre 
                         tot_cpond >  28 & tot_cpond <= 42   ~ 2,  # Limítrofe
                         tot_cpond >  42 & !is.na(tot_cpond) ~ 3), # Aceptable
            # Limitación en el consumo de Alimentos
            lca=case_when(dch==1 | dch==2 ~ 1,   # Limitado
                          dch==3 ~ 0))           # No limitado

# Indicador de carencia por acceso a la alimentación nutritiva y de calidad
################################################################################
# Se considera en situación de carencia por acceso a la alimentación 
# a la población en hogares que presenten, al menos, una de las 
# siguientes características:
#  
# 1. Grado inseguridad alimentaria moderada o severa
# 2. Limitación en el consumo de alimentos
################################################################################

ali <- mutate(ali, 
              ic_ali_nc =case_when(ic_ali == 0 & lca == 0 ~ 0, # Sin carencia
                                  (ic_ali == 1 | lca == 1) &
                                  (!is.na(ic_ali) & !is.na(lca))~ 1)) %>% # Con carencia
        select(folioviv, foliohog, id_men, starts_with("ia_"), 
               tot_iaad, tot_iamen, ins_ali, dch, lca, ic_ali, ic_ali_nc)

fwrite(ali, "Bases/ic_ali20.csv", row.names=F)
gdata::keep(rezedu, salud, segsoc, cev, sbv, ali, concen, viviendas, pobla, sure=T)

################################################################################
# Parte VII
# Bienestar económico (ingresos)
################################################################################

# Para la construcción del ingreso corriente del hogar es necesario utilizar
# información sobre la condición de ocupación y los ingresos de los individuos.
# Se utiliza la información contenida en la base trabajo.csv para 
# identificar a la población ocupada que declara tener como prestación laboral aguinaldo, 
# ya sea por su trabajo principal o secundario, a fin de incorporar los ingresos por este 
# concepto en la medición

# Creación del ingreso monetario deflactado a pesos de agosto del 2020

# Ingresos

trab <- fread("Bases de datos/trabajos.csv") %>% rename_all(tolower)

trab <- dplyr::select(trab, folioviv, foliohog, numren, id_trabajo, pres_2) %>% 
        as.data.table() %>% 
        dcast(folioviv + foliohog + numren ~ id_trabajo,value.var ="pres_2") %>% 
        mutate(
              # Población con al menos un empleo 
              trab=1,
              # Aguinaldo trabajo principal
              aguinaldo1=case_when(`1`==2 ~ 1,  # Dispone de aguinaldo
                                   TRUE ~ 0),   # No dispone de aguinaldo
              # Aguinaldo trabajo secundario
              aguinaldo2=case_when(`2`==2 ~ 1,   # Dispone de aguinaldo
                                   TRUE ~ 0))%>% # No dispone de aguinaldo
        select(folioviv, foliohog, numren, aguinaldo1, aguinaldo2, trab)

fwrite(trab, "bases/aguinaldo20.csv")

# Ahora se incorpora a la base de ingresos

ing <- fread("Bases de datos/ingresos.csv") %>% rename_all(tolower)

ing <-  full_join(ing, trab, by = c("folioviv", "foliohog", "numren")) %>%
        mutate(index=(case_when(clave=="P009" & aguinaldo1!=1 ~ 1,
                                clave=="P016" & aguinaldo2!=1 ~ 1,
                                TRUE ~ 0))) %>%
        filter(index != 1)

# Una vez realizado lo anterior, se procede a deflactar el ingreso recibido
# por los hogares a precios de agosto de 2020. Para ello, se utilizan las 
# variables meses, las cuales toman los valores 2 a 10 e indican el mes en
# que se recibió el ingreso respectivo


# Definición de los deflactores 2020 
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

# Una vez realizada la deflactación, se procede a obtener el ingreso mensual 
# promedio en los últimos seis meses, para cada persona y clave de ingreso


ing <- ing %>% 
       mutate(ing_mens = apply(ing[,c("ing_1","ing_2","ing_3",
                                      "ing_4","ing_5","ing_6")], 1, mean, na.rm=TRUE), 
                    # Para obtener el ingreso corriente monetario, se seleccionan las claves de ingreso correspondientes
                    ing_mon=case_when((clave>="P001" & clave<="P009") | (clave>="P011" & clave<="P016") |
                                      (clave>="P018" & clave<="P048") | (clave>="P067" & clave<="P081") |
                                      (clave>="P101" & clave<="P108") ~ ing_mens ),
                    # Para obtener el ingreso laboral, se seleccionan las claves de ingreso correspondientes
                    ing_lab=case_when((clave>="P001" & clave<="P009") | (clave>="P011" & clave<="P016") |
                                     (clave>="P018" & clave<="P022") | (clave>="P067" & clave<="P081") ~ ing_mens ), 
                    # Para obtener el ingreso por rentas, se seleccionan las claves de ingreso correspondientes
                    ing_ren=case_when((clave>="P023" & clave<="P031") ~ ing_mens ), 
                    # Para obtener el ingreso por transferencias, se seleccionan las claves de ingreso correspondientes 
                    ing_tra=case_when((clave>="P032" & clave<="P048") | (clave>="P101" & clave<="P108") ~ ing_mens ))

# Se estima el total de ingresos de cada  hogar
vars <- colnames(select(ing, ing_mon, ing_lab, ing_ren, ing_tra))
ing <- data.table(ing)[, lapply(.SD, sum, na.rm=T), by=list(folioviv, foliohog), .SDcols = vars ]

fwrite(ing, "bases/ingreso_deflactado20.csv")

################################################################################
#
#          Creación del ingreso no monetario deflactado a pesos de 
#                                 agosto del 2020
#
################################################################################

#No Monetario

gper <- fread("Bases de datos/gastoshogar.csv") %>% 
        rename_all(tolower)   %>% 
        mutate(base=1)
ghog <- fread("Bases de datos/gastospersona.csv") %>% 
        rename_all(tolower) %>% 
        mutate(base=2)

nomon <- bind_rows(gper, ghog)
nomon <- mutate(nomon, frecuencia=ifelse(base==2, frec_rem, frecuencia), 
              
# En el caso de la información de gasto no monetario, para deflactar se utiliza 
# la decena de levantamiento de la encuesta, la cual se encuentra en la octava 
# posición del folio de la vivienda. En primer lugar se obtiene una variable que 
# identifique la decena de levantamiento
                decena=str_sub(str_pad(folioviv, 10, "left", pad = "0"), 8,8))


# Definición de los deflactores
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

# Rubro 4.2 mensual, Accesorios y artículos de limpieza para el hogar		
d42m07	<-	0.9967972340
d42m08	<-	1.0000000000
d42m09	<-	1.0024748647
d42m10	<-	1.0026659388
d42m11	<-	1.0008279878

# Rubro 4.2 trimestral, Accesorios y artículos de limpieza para el hogar		
d42t05	<-	0.9887690885
d42t06	<-	0.9957963696
d42t07	<-	0.9997573662
d42t08	<-	1.0017136011

# Rubro 4.1 semestral, Muebles y aparatos dómesticos		
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

# Rubro 7 mensual, Educación y esparcimiento		
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
# Se deflactan los rubros del gasto no monetario según la decena de levantamiento 
                            
 c("ali_nm", # Gasto no monetario en Alimentos deflactado (semanal)
   "alta_nm", # Gasto no monetario en Alcohol y tabaco deflactado (semanal)
   "veca_nm", # Gasto no monetario en Vestido y calzado deflactado (trimestral)
   "viv_nm",  # Gasto no monetario en viviendas y servicios de conservación deflactado (mensual)
   "lim_nm",  # Gasto no monetario en Artículos de limpieza deflactado (mensual)
   "cris_nm", # Gasto no monetario en Cristalería y blancos deflactado (trimestral)
   "ens_nm",  # Gasto no monetario en Enseres domésticos y muebles deflactado (semestral)
   "sal_nm",  # Gasto no monetario en Salud deflactado (trimestral)
   "tpub_nm", # Gasto no monetario en Transporte público deflactado (semanal)
   "tfor_nm", # Gasto no monetario en Transporte foráneo deflactado (semestral)
   "com_nm",  # Gasto no monetario en Comunicaciones deflactado (mensual)
   "edre_nm", # Gasto no monetario en Educación y recreación deflactado (mensual)
   "edba_nm", # Gasto no monetario en Educación básica deflactado (mensual)
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

fwrite(nomon, "bases/ingresonomonetario_def20.csv")

# Construcción de la base de pagos en especie a partir de la base de gasto no monetario

esp <- filter(nomon, esp==1)

vars <- colnames(select(esp, ends_with("_nm")))
esp <- data.table(esp)[, lapply(.SD, sum, na.rm=T), by=list(folioviv, foliohog), .SDcols = vars ]

vars <- c("folioviv", "foliohog", "gasto_nm", "ali_nme", "alta_nme", "veca_nme", 
          "viv_nme", "lim_nme", "cris_nme", "ens_nme", "sal_nme", "tpub_nme", 
          "tfor_nme", "com_nme", "edre_nme", "edba_nme", "cuip_nme", "accp_nme", 
          "otr_nme", "reda_nme")

colnames(esp) <- vars

fwrite(esp, "bases/esp_def20.csv")

# Construcción de base de regalos a partir de la base no monetaria

reg <- filter(nomon, reg==1)

vars <- colnames(select(reg, ends_with("_nm")))
reg <- data.table(reg)[, lapply(.SD, sum, na.rm=T), by=list(folioviv, foliohog), .SDcols = vars ]

vars <- c("folioviv", "foliohog", "gasto_nm", "ali_nmr", "alta_nmr", "veca_nmr", 
          "viv_nmr", "lim_nmr", "cris_nmr", "ens_nmr", "sal_nmr", "tpub_nmr", 
          "tfor_nmr", "com_nmr", "edre_nmr", "edba_nmr", "cuip_nmr", "accp_nmr", 
          "otr_nmr", "reda_nmr")

colnames(reg) <- vars

fwrite(reg, "bases/reg_def20.csv")

################################################################################
#
#                Construcción del ingreso corriente total
#
################################################################################

ict <- select(concen, folioviv, foliohog, tam_loc, factor, tot_integ, 
              est_dis, upm, ubica_geo)

# Incorporación de la base de ingreso monetario deflactado
ict <- left_join(ict, ing, by = c("folioviv", "foliohog"))

# Incorporación de la base de ingreso no monetario deflactado: pago en especie
ict <- left_join(ict, esp, by = c("folioviv", "foliohog"))

# Incorporación de la base de ingreso no monetario deflactado: regalos en especie
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
       select(folioviv,foliohog,ubica_geo,tam_loc,est_dis,upm,factor,tot_integ,
              ing_mon,ing_lab,ing_ren,ing_tra,ali_nme,alta_nme,veca_nme,viv_nme,
              lim_nme,cris_nme,ens_nme,sal_nme,tpub_nme,tfor_nme,com_nme,edre_nme,
              edba_nme,cuip_nme,accp_nme,otr_nme,reda_nme,ali_nmr,alta_nmr,veca_nmr,
              viv_nmr,lim_nmr,cris_nmr,ens_nmr,sal_nmr,tpub_nmr,tfor_nmr,com_nmr,
              edre_nmr,edba_nmr,cuip_nmr,accp_nmr,otr_nmr,reda_nmr,rururb,pago_esp,
              reg_esp,nomon,ict)

fwrite(ict, "bases/ingresotot20.csv")

################################################################################
#
#        Construcción del tamaño de hogar con economías de escala
#                       y escalas de equivalencia
#
################################################################################

tam_hogesc <- pobla

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

fwrite(tam_hogesc, "bases/tamhogesc20.csv")

################################################################################
#
#                        Construcción del ingreso
#
################################################################################

# Incorporación de la información sobre el tamaño del hogar ajustado
p_ing <- left_join(ict, tam_hogesc, by = c("folioviv", "foliohog"))

# Información per cápita
p_ing <-mutate(p_ing, ictpc= ict/tamhogesc )

################################################################################
#
#                        Indicadores de ingresos
#
################################################################################
#
# LP I: Valor de la Canasta alimentaria 
#
# LP II: Valor de la Canasta Alimentaria más el valor de la canasta
# no alimentaria (ver Anexo A del documento metodológico).
#
# En este programa se construyen los indicadores de bienestar por ingresos
# mediante las 2 líneas definidas por CONEVAL , denominándolas:
#  
#  lp1 : Línea de Pobreza Extrema por Ingresos (LPEI)
#  lp2 : Línea de Pobreza por Ingresos (LPI)
#
# Para más información, se sugiere consultar el documento metodológico de Construcción
# de las líneas de pobreza por ingresos. Disponible en:
# https://www.coneval.org.mx/InformesPublicaciones/InformesPublicaciones/Documents/Lineas_pobreza.pdf
################################################################################


#Línea de pobreza extrema por ingresos (LPEI);
# Valor monetario de la canasta alimentaria;
lp1_urb <- 1702.28
lp1_rur <- 1299.30

# Línea de pobreza por ingresos (LPI)
# Valor monetario de la canasta alimentaria más el valor monetario de la canasta no alimentaria
lp2_urb <- 3559.88
lp2_rur <- 2520.16

p_ing <- mutate(p_ing,
                # Se identifica a los hogares bajo lp1
                plp_e=case_when(ictpc <lp1_urb  & rururb==0  ~ 1,
                                 ictpc>=lp1_urb  & rururb==0 & !is.na(ictpc) ~ 0,
                                 ictpc <lp1_rur  & rururb==1  ~ 1,
                                 ictpc>=lp1_rur  & rururb==1 & !is.na(ictpc) ~ 0),
                # Se identifica a los hogares bajo lp2
                plp =case_when(ictpc <lp2_urb  & rururb==0  ~ 1,
                ictpc>=lp2_urb  & rururb==0 & !is.na(ictpc) ~ 0,
                ictpc <lp2_rur  & rururb==1  ~ 1,
                ictpc>=lp2_rur  & rururb==1 & !is.na(ictpc) ~ 0)) %>% 
        select(folioviv, foliohog, factor, tam_loc, rururb, tamhogesc, 
               ict, ictpc, plp_e, plp, est_dis, upm, ubica_geo, 
               tot_integ, ing_mon, ing_lab, ing_ren, ing_tra, nomon, 
               pago_esp, reg_esp)  

fwrite(p_ing, "bases/p_ingresos20.csv")
gdata::keep(rezedu, salud, segsoc, cev, sbv, ali, p_ing,  sure=T)

################################################################################
#
#                           Parte VIII Pobreza multidimensional
#
################################################################################

############################
# Integración de las bases #
############################

pobreza <- left_join(rezedu, salud, by = c("folioviv", "foliohog", "numren"))
pobreza <- left_join(pobreza, segsoc, by = c("folioviv", "foliohog", "numren"))
pobreza <- left_join(pobreza, cev   , by = c("folioviv", "foliohog"))
pobreza <- left_join(pobreza, sbv   , by = c("folioviv", "foliohog"))
pobreza <- left_join(pobreza, ali   , by = c("folioviv", "foliohog"))
pobreza <- left_join(pobreza, p_ing , by = c("folioviv", "foliohog"))

pobreza$ing_mon[is.na(pobreza$ing_mon)] <- 0
pobreza$ing_lab[is.na(pobreza$ing_lab)] <- 0
pobreza$ing_ren[is.na(pobreza$ing_ren)] <- 0
pobreza$ing_tra[is.na(pobreza$ing_tra)] <- 0

# Se eliminan posibles duplicados
pobreza <- pobreza %>% distinct(folioviv, foliohog, numren, .keep_all = TRUE )

pobreza <- mutate(pobreza,
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
                                    ent==9  ~ "Ciudad de México",
                                    ent==10 ~	"Durango",
                                    ent==11 ~	"Guanajuato",
                                    ent==12 ~	"Guerrero",
                                    ent==13 ~	"Hidalgo",
                                    ent==14 ~	"Jalisco",
                                    ent==15 ~	"México",
                                    ent==16 ~	"Michoacán",
                                    ent==17 ~	"Morelos",
                                    ent==18 ~	"Nayarit",
                                    ent==19 ~	"Nuevo León",
                                    ent==20 ~	"Oaxaca",
                                    ent==21 ~	"Puebla",
                                    ent==22 ~	"Querétaro",
                                    ent==23 ~	"Quintana Roo",
                                    ent==24 ~	"San Luis Potosí",
                                    ent==25 ~	"Sinaloa",
                                    ent==26 ~	"Sonora",
                                    ent==27 ~	"Tabasco",
                                    ent==28 ~	"Tamaulipas",
                                    ent==29 ~	"Tlaxcala",
                                    ent==30 ~	"Veracruz",
                                    ent==31 ~	"Yucatán",
                                    ent==32 ~	"Zacatecas"))

##############################
# Índice de Privación Social #
##############################

pobreza <- as.data.table(pobreza)[, 
                                c("i_privacion") := .(sum(ic_rezedu, ic_asalud, ic_segsoc, ic_cv, ic_sbv, ic_ali_nc)), 
                                by=.(folioviv, foliohog, numren)]

##############################
# Pobreza multidimensional   #
##############################
pobreza <- mutate(pobreza, 
                  # Pobreza
                  pobreza=case_when(plp==1 & (i_privacion>=1 & !is.na(i_privacion)) ~ 1,                  # Pobre
                                    (plp==0 | i_privacion==0) & (!is.na(plp) & !is.na(i_privacion)) ~ 0), # No pobre
                  # Pobreza extrema
                  pobreza_e=case_when(plp_e==1 & (i_privacion>=3 & !is.na(i_privacion)) ~ 1,                    # Pobre extremo
                                      (plp_e==0 | i_privacion<3 ) & (!is.na(plp_e) & !is.na(i_privacion)) ~ 0), # No pobre extremo
                  # Pobreza moderada
                  pobreza_m=case_when(pobreza==1 & pobreza_e==0 ~ 1,                # Pobre moderado 
                                      pobreza==0 | (pobreza==1 & pobreza_e==1) ~ 0) # No pobre moderado
                  )

##############################
#   Población vulnerable     #
##############################
pobreza <- mutate(pobreza, 
                  # Vulnerables por carencias
                  vul_car=case_when(plp==0 & (i_privacion>=1 & !is.na(i_privacion)) ~1, # Vulnerable
                                    is.na(pobreza) ~ NA_real_,
                                    TRUE ~ 0), # No vulnerable
                  # Vulnerables por ingresos
                  vul_ing=case_when(plp==1 & i_privacion==0 ~ 1, # Vulnerable
                                    is.na(pobreza)~  NA_real_,
                                    TRUE~0)) # No vulnerable

###########################################
#   Población no pobre y no vulnerable    #
###########################################
pobreza <- mutate(pobreza,
                  # Población no pobre y no vulnerable
                  no_pobv=case_when(plp==0 & i_privacion==0 ~1,  # Vulnerable
                                    is.na(pobreza) ~ NA_real_,
                                    TRUE~ 0))  # No vulnerable

#########################################
#   Población con carencias sociales    #
#########################################
pobreza <- mutate(pobreza,
                  # Población con al menos una carencia
                  carencias = case_when(i_privacion>=1 & !is.na(i_privacion) ~1, # Población sin carencias
                                        is.na(pobreza) ~ NA_real_,
                                        TRUE ~ 0), # Población sin carencias
                  # Población con tres o más carencias                  
                  carencias3 = case_when(i_privacion>=3 & !is.na(i_privacion) ~ 1, # Población con al menos tres o más carencias
                                         is.na(pobreza) ~ NA_real_,
                                         TRUE ~ 0)) # Población con menos de tres carencias

###################
#   Cuadrantes    #
###################
pobreza <- mutate(pobreza, 
                  cuadrantes=case_when(plp==1 & (i_privacion>=1 & !is.na(i_privacion)) ~ 1, # Pobres
                                      plp==0 & (i_privacion>=1 & !is.na(i_privacion)) ~ 2, # Vulnerables por carencias
                                      plp==1 & i_privacion==0 ~ 3,  # Vulnerables por ingresos     
                                      plp==0 & i_privacion==0 ~ 4))  # No pobres y no vulnerables

######################################################
#  Profundidad en el espacio del bienestar económico #
######################################################

#Línea de pobreza extrema por ingresos (LPEI);
# Valor monetario de la canasta alimentaria;
lp1_urb <- 1702.28
lp1_rur <- 1299.30

# Línea de pobreza por ingresos (LPI)
# Valor monetario de la canasta alimentaria más el valor monetario de la canasta no alimentaria
lp2_urb <- 3559.88
lp2_rur <- 2520.16

pobreza <- mutate(pobreza, 
                  # FGT (a=1)
                  # Distancia normalizada del ingreso respecto a la línea de pobreza por ingresos
                  prof1=case_when(rururb==1 & plp==1 ~ (lp2_rur-ictpc)/(lp2_rur), 
                                   rururb==0 & plp==1 ~ (lp2_urb-ictpc)/(lp2_urb),
                                  !is.na(ictpc) ~0),
                  # Distancia normalizada del ingreso respecto a la línea de pobreza extrema por ingresos
                  prof_e1=case_when(rururb==1 & plp_e==1 ~ (lp1_rur-ictpc)/(lp1_rur),
                                    rururb==0 & plp_e==1 ~ (lp1_urb-ictpc)/(lp1_urb),
                                     !is.na(ictpc) ~0))

#############################################
#   Profundidad de la privación social      #
#############################################
pobreza <- mutate(pobreza, 
                  profun=i_privacion/6,
                  
#############################################
#   Intensidad de la privación social       #
#############################################
                  
# Intensidad de la privación social: pobres
 int_pob=profun*pobreza,
# Intensidad de la privación social: pobres extremos
 int_pobe=profun*pobreza_e,
# Intensidad de la privación social: población vulnerable por carencias
 int_vulcar=profun*vul_car,
# Intensidad de la privación social: población con carencias sociales
int_caren=profun*carencias) 

pobreza <- select(pobreza, 
                  folioviv, foliohog, numren, est_dis, upm,
                  factor, tam_loc, rururb, ent, ubica_geo, edad, sexo, parentesco,
                  ic_rezedu, anac_e, inas_esc, niv_ed,
                  ic_asalud, ic_segsoc,sa_dir, ss_dir, s_salud, par, jef_ss, cony_ss, 
                  hijo_ss, pea, jub, pam, ing_pam, ic_cv, icv_pisos, icv_muros, icv_techos, 
                  icv_hac, ic_sbv, isb_agua, isb_dren, isb_luz, isb_combus,
                  ic_ali_nc, id_men, tot_iaad, tot_iamen, ins_ali, ic_ali, lca, dch,
                  plp_e, plp,pobreza, pobreza_e, pobreza_m, vul_car, vul_ing, no_pobv,
                  i_privacion, carencias, carencias3, cuadrantes,prof1, prof_e1, profun, 
                  int_pob, int_pobe, int_vulcar, int_caren,tamhogesc, ictpc, ict, ing_mon, 
                  ing_lab, ing_ren, ing_tra, nomon, pago_esp, reg_esp, hli, discap)



fwrite(pobreza, "Bases/pobreza_20.csv", row.names=F)

################################################################################
#                             Cuadros resultado
################################################################################

# Tabulados básicos 

#############################################
#      RESULTADOS A NIVEL NACIONAL          #
#############################################

pob_w <- as_survey_design(pobreza, weights=factor, nest=TRUE) %>% srvyr::filter(!is.na(pobreza))

vars  <- colnames(select(pob_w, pobreza, pobreza_m, pobreza_e, vul_car, vul_ing, 
                         no_pobv, carencias, carencias3, ic_rezedu, ic_asalud, 
                         ic_segsoc, ic_cv, ic_sbv, ic_ali_nc, plp_e, plp))

por <- as.data.frame(matrix(unlist(pob_w %>% 
                                   srvyr::select(vars) %>%
                                   summarise_all(survey_mean, vartype=NULL)), ncol = 1, byrow=T)*100) %>% round(1)

tot <- as.data.frame(matrix(unlist(pob_w %>% 
                                   srvyr::select(vars) %>%
                                   summarise_all(survey_total, vartype=NULL)), ncol =1 , byrow=T) / 1000000)

nac <- bind_cols(por, tot)
rownames(nac) <- c("pobreza", "pobreza_m", "pobreza_e", "vul_car", "vul_ing", "no_pobv", 
                   "carencias", "carencias3", "ic_rezedu", "ic_asalud", "ic_segsoc", 
                   "ic_cv", "ic_sbv", "ic_ali_nc", "plp_e", "plp")

colnames(nac) <- c("Porcentaje", "Millones de personas")
nac

################################################################################
# PORCENTAJE Y NÚMERO DE PERSONAS POR INDICADOR DE POBREZA, ENTIDAD FEDERATIVA #
################################################################################
vars  <- colnames(select(pob_w, pobreza, pobreza_m, pobreza_e, vul_car, vul_ing, no_pobv))

pob_ent_por <- as.data.frame(pob_w %>% group_by(ent) %>% srvyr::select(vars) %>%
                             summarise_all(survey_mean, vartype=NULL))

pob_ent_tot <- as.data.frame(pob_w %>% group_by(ent) %>% srvyr::select(vars) %>%
                             summarise_all(survey_total, vartype=NULL))

pob_ent_por ; pob_ent_tot
pob_ent <- left_join(pob_ent_por, pob_ent_tot, by = "ent")
##########################################################################################
# PORCENTAJE Y NÚMERO DE PERSONAS POR INDICADOR DE CARENCIA SOCIAL, ENTIDAD FEDERATIVA   #
##########################################################################################
vars  <- colnames(select(pob_w, ic_rezedu, ic_asalud, ic_segsoc, ic_cv, ic_sbv,
                         ic_ali_nc,carencias, carencias3, plp_e, plp))

care_ent_por <- as.data.frame(pob_w %>% group_by(ent) %>% srvyr::select(vars) %>%
                summarise_all(survey_mean, vartype=NULL))

care_ent_tot <- as.data.frame(pob_w %>% group_by(ent) %>% srvyr::select(vars) %>%
                summarise_all(survey_total, vartype=NULL))
care_ent_por ; care_ent_tot

pob_ent <- left_join(pob_ent, care_ent_por, by = "ent") %>% left_join(care_ent_tot, by = "ent")

fwrite(pob_ent, "Bases/pob_ent.csv", row.names=F)
