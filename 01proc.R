
# Procesamiento Informe de Género 2022

# Cargar paquetes
pacman::p_load(tidyverse,data.table,
               car,
               sjmisc,
               janitor,haven)


# Cargar datos
doc_2021<-read.csv2("../genero_2022/input/20210727_Docentes_2021_20210630_PUBL.csv",
                         sep = ";",encoding = "UTF-8", stringsAsFactors = F)

asis_2021<-fread("input/20210723_Asistentes_de_la_Educacin_ESTAB_2021_20210630_PUBL.csv")

rend_2021<-fread("input/20220302_Rendimiento_2021_20220131_PRIV.csv")

mat_2021priv<-fread("input/20210913_Matrícula_unica_2021_20210430_PRIV_MRUN.csv")

# Recodificar datos

# Docentes 2021
doc_proc = doc_2021 %>%
  filter(ESTADO_ESTAB == 1 & PERSONAS == 1) %>% 
  mutate(DOC_GENERO = car::recode(.$DOC_GENERO,"1 = 'Hombre';
                                                2 = 'Mujer'",as.factor=T),
        RURAL_RBD = car::recode(.$RURAL_RBD,"0 = 'Urbana';
                                            1 = 'Rural'",as.factor=T),
         COD_DEPE2 = car::recode(.$COD_DEPE2, "1 = 'Municipal';
                                               2 = 'Particular Subvencionado';
                                               3 = 'Particular Pagado'; 
                                               4 = 'Administración Delegada'; 
                                               5 = 'Servicio Local'",
                                               as.factor= T,
                                 levels = c('Servicio Local', # Estos levels estan al reves por el gráfico
                                            'Administración Delegada',
                                            'Particular Pagado',
                                            'Particular Subvencionado',
                                            'Municipal')),
         NIVEL1 = car::recode(.$NIVEL1,"1 = 'Educación Parvularia';
                                        2 = 'Educación Especial';
                                        3 = 'Enseñanza Básica';
                                        4 = 'Enseñanza Media HC';
                                        5 = 'Enseñanza Media TP'",as.factor= T,
                              levels = c('Enseñanza Media TP', # Estos levels estan al reves por el gráfico
                                         'Enseñanza Media HC',
                                         'Enseñanza Básica',
                                         'Educación Especial',
                                         'Educación Parvularia')),
         ID_IFP = ifelse(ID_IFP %in% c(10:14),6,ID_IFP),
         ID_IFP = car::recode(.$ID_IFP, "1 = 'Docente de aula';
                                         2 = 'Planta técnico-pedagógica';
                                         3 = 'Planta Directiva';
                                         4 = 'Director';
                                         5 = 'Otra en el establecimiento';
                                         6 = 'Otra fuera del establecimiento'; 
                                         7 = 'Jefe unidad técnico- pedagógica';
                                         8 = 'Inspector General';
                                         9 = 'Orientador';
                                         15 = 'subdirector';
                                         16 = 'Profesor encargado del establecimiento';
                                         17 = 'Educador Tradicional'", as.factor = T),
         SECTOR1 = car::recode(.$SECTOR1,"110 = 'Lenguaje y Comunicación'; 
                                         115 = 'Lenguaje Indígena';
                                         120 = 'Matemática'; 130 = 'Ciencia'; 
                                         140 = 'Tecnología';150 = 'Artes';
                                         160 = 'Educación Física'; 170 = 'Orientación';
                                         180 = 'Religión'; 190 = 'Educación General';
                                         200 = 'Educación Especial'; 310 = 'Lenguaje y Comunicación';
                                         320 = 'Matemática'; 330 = 'Historia y Ciencia Sociales';
                                         340 = 'Filosofía y Psicología'; 350 = 'Ciencias Naturales';
                                         360 = 'Educación Tecnológica';370 = 'Educación Artística'; 
                                         380 = 'Educación Física'; 390 = 'Religión';
                                         395 = 'Otros'; 410 = 'Administración y Comercio'; 
                                         510 = 'Construcción'; 520 = 'Metalmecánico'; 
                                         530 = 'Electricidad'; 540 = 'Minero';
                                         550 = 'Gráfico'; 560 = 'Química';
                                         570 = 'Confección'; 
                                         580 = 'Tecnología y Telecomunicaciones';
                                         610 = 'Alimentación'; 
                                         620 = 'Programas y Proyectos Sociales';
                                         630 = 'Hotelería y Turismo';
                                         710 = 'Maderero'; 720 = 'Agropecuario';
                                         810 = 'Marítimo'",as.factor=T),
# Creación variables necesarias         
         year = as.numeric(substr(DOC_FEC_NAC,1,4)),
         mes = as.numeric(substr(DOC_FEC_NAC,5,6)),
         edad = round(2021-year),
         edad = case_when(mes > 6 ~ round(edad-1),
                          mes <= 6 ~ edad),
         tramos = factor(case_when(edad < 26 ~ "Menos de 26",
                                   edad >= 26 & edad <=30 ~ "26 a 30",
                                   edad >= 31 & edad <= 35 ~ "31 a 35",
                                   edad >= 35 & edad <= 40 ~ "36 a 40",
                                   edad >= 41 & edad <= 45 ~ "41 a 45",
                                   edad >= 46 & edad <=50 ~ "46 a 50",
                                   edad >= 51 & edad <= 55 ~ "51 a 55",
                                   edad >= 56 & edad <= 60 ~ "56 a 60",
                                   edad >= 61 & edad <= 65 ~ "61 a 65",
                                   edad >65 ~ "Mas de 65", TRUE ~ NA_character_)),
         tramos_hrs = factor(case_when(HORAS_CONTRATO < 30 ~ "Menos de 30 hrs",
                                           HORAS_CONTRATO == 30 ~ "30 hrs",
                                           HORAS_CONTRATO >= 31 & HORAS_CONTRATO <= 37 ~ "31 - 37 hrs",
                                           HORAS_CONTRATO >= 38 & HORAS_CONTRATO <=43 ~ "38 - 43 hrs",
                                           HORAS_CONTRATO > 43 ~ "44 hrs", TRUE ~ NA_character_),
                                 levels = c('Menos de 30 hrs',
                                            '30 hrs',
                                            '31 - 37 hrs',
                                            '38 - 43 hrs',
                                            '44 hrs')))


# Asistentes 2021

asis_proc = asis_2021 %>% 
  mutate(GEN_ASIS = car::recode(.$GEN_ASIS, "1 = 'Hombre';
                                             2 = 'Mujer'", as.factor = T),
         RURAL_RBD = car::recode(.$RURAL_RBD, "1 = 'Urbano';
                                               2 = 'Rural'", as.factor = T),
         ID_ESTAMENTO = car::recode(.$ID_ESTAMENTO, "1 = 'Profesional';
                                                     2 = 'Paradocente';
                                                     3 = 'Auxiliar';
                                                     4 = 'Administrativo';
                                                     5 = 'Técnico';
                                                     6 = 'Sin Información'", as.factor = T),
         COD_DEPE2 = car::recode(.$COD_DEPE2, "1 = 'Municipal'; 
                                               2 = 'Particular Subvencionado';
                                               3 = 'Particular Pagado';
                                               4 = 'Administración Delegada';
                                               5 = 'Servicio Local'", as.factor = T),
# Creación variables necesarias       
         year = as.numeric(substr(FEC_NAC_ASIS,1,4)),
         mes = as.numeric(substr(FEC_NAC_ASIS,5,6)),
         edad = round(2021-year),
         edad = case_when(mes > 6 ~ round(edad-1),mes <= 6 ~ edad),
         tramos = factor(case_when(edad < 26 ~ "Menos de 26",
                          edad >= 26 & edad <=30 ~ "26 a 30",
                          edad >= 31 & edad <= 35 ~ "31 a 35",
                          edad >= 35 & edad <= 40 ~ "36 a 40",
                          edad >= 41 & edad <= 45 ~ "41 a 45",
                          edad >= 46 & edad <=50 ~ "46 a 50",
                          edad >= 51 & edad <= 55 ~ "51 a 55",
                          edad >= 56 & edad <= 60 ~ "56 a 60",
                          edad >= 61 & edad <= 65 ~ "61 a 65",
                          edad >65 ~ "Mas de 65", TRUE ~ NA_character_)))


# Matricula 2021
mat2021_proc = mat_2021priv %>% 
  filter(GEN_ALU != 0) %>% 
  mutate(GEN_ALU = car::recode(.$GEN_ALU, "1 = 'Hombre';
                                           2 = 'Mujer'", as.factor = T),
         ENS = car::recode(.$ENS, "1 = 'Parvularia Regular'; 
                                   2 = 'Parvular Especial';
                                   3 = 'Básica niños regular'; 
                                   4 = 'Básica niños especial';
                                   5 = 'Media HC ciclo general'; 
                                   6 = 'Media TP ciclo general';
                                   7 = 'Media HC ciclo diferenciado';
                                   8 = 'Media TP ciclo diferenciado'", as.factor = T),
         COD_RAMA = car::recode(.$COD_RAMA, "400 = 'Comercial'; 
                                             500 = 'Industrial';
                                             600 = 'Técnica';
                                             700 = 'Agrícola';
                                             800 = 'Marítima';
                                             900 = 'Artística'", as.factor = T),
         COD_SEC = car::recode(.$COD_SEC,"410 = 'Administración y Comercio';
                                          510 = 'Construcción'; 
                                          520 = 'Metalmecánico'; 
                                          530 = 'Electricidad';
                                          540 = 'Minero'; 
                                          550 = 'Gráfica'; 
                                          560 = 'Químico';
                                          570 = 'Confección';
                                          580 = 'Tecnología y Telecomunicaciones';
                                          610 = 'Alimentación';
                                          630 = 'Hotelería y Turismo';
                                          640 = 'Salud y Educación';
                                          710 = 'Maderero';
                                          720 = 'Agropecuario';
                                          810 = 'Marítimo'; 
                                          910 = 'Artes Visuales'", as.factor = T),
         COD_DEPE2 = car::recode(.$COD_DEPE2, "1 = 'Municipal'; 
                                              2 = 'Particular Subvencionado';
                                              3 = 'Particular Pagado';
                                              4 = 'Administración Delegada';
                                              5 = 'Servicio Local'", as.factor = T),
         COD_ETNIA_ALU = car::recode(.$COD_ETNIA_ALU, " 0 = 'No pertenece a ninguna etnia';
                                                        1 = 'Pertenece a alguna etnia'", as.factor = T),
         RURAL_RBD = car::recode(.$RURAL_RBD, "0 = 'Urbano';
                                               1 = 'Rural'", as.factor = T),
         COD_REG_RBD = car::recode(.$COD_REG_RBD,"1 = 'Región de Tarapacá';
                                                  2 = 'Región de Antofagasta';
                                                  3 = 'Región de Atacama';
                                                  4 = 'Región de Coquimbo';
                                                  5 = 'Región de Valparaíso';
                                                  6 = 'Región del Libertador Gral. Bernardo O´higgins';
                                                  7 = 'Región del Maule';
                                                  8 = 'Región del BioBío';
                                                  9 = 'Región de la Araucanía';
                                                 10 = 'Región de Los Lagos';
                                                 11 = 'Región de Aysén del Gral. Carlos Ibáñez del Campo';
                                                 12 = 'Región de Magallanes y de la Antártica Chilena';
                                                 13 = 'Región Metropolitana de Santiago';
                                                 14 = 'Región de Los Ríos';
                                                 15 = 'Región de Arica y Parinacota';
                                                 16 = 'Región de Ñuble'", as.factor = T,
                                   levels = c('XV. Arica y Parinacota',
                                              'I. Tarapacá',
                                              'II. Antofagasta',
                                              'III. Atacama',
                                              'IV. Coquimbo',
                                              'V. Valparaíso',
                                              'XIII. Metropolitana',
                                              'VI. OHiggins',
                                              'VII. Maule',
                                              'XVI. Ñuble',
                                              'VIII. Bío-bío',
                                              'IX. La Araucanía',
                                              'XIV. Los Ríos',
                                              'X. Los Lagos',
                                              'XI. Aysén',
                                              'XII. Magallanes')))

rend_proc = rend_2021 %>% 
  mutate(GEN_ALU = car::recode(.$GEN_ALU,"1 = 'Hombre';
                                          2 = 'Mujer'", as.factor = T))









# Exportar datos
saveRDS(doc_proc,"output/data/docproc.rds")

saveRDS(asis_proc,"output/data/asisproc.rds")

saveRDS(mat_proc,"output/data/matproc.rds")



