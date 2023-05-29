#Informe Género
#-----------------------------------------------------------------------------#
# 1. Carga de base de datos
#-----------------------------------------------------------------------------#

pacman::p_load(tidyverse,sjmisc,haven,sjPlot,data.table,dplyr,ggrepel,kableExtra,janitor,
               car)

docentes_2021<-read.csv2("../genero_2022/input/20210727_Docentes_2021_20210630_PUBL.csv",
                        sep = ";",encoding = "UTF-8", stringsAsFactors = F)
asistentes_2021<-fread("input/20210723_Asistentes_de_la_Educacin_ESTAB_2021_20210630_PUBL.csv")
rendimiento_2021<-fread("input/20220302_Rendimiento_2021_20220131_PRIV.csv")
matricula_2021priv<-fread("input/20210913_Matrícula_unica_2021_20210430_PRIV_MRUN.csv")
##----------------------------------------------------------------------------#

## RECODIFICACIÓN BBDD DOCENTES
docentes_2021<- docentes_2021 %>% mutate(DOC_GENERO=dplyr::recode(DOC_GENERO,"1"="Hombre","2"="Mujer"))



#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# 2. TABLAS PARA GRAFICOS
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

#Tabla 1 
tabla1<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>% 
  group_by(DOC_GENERO) %>% summarise(Número=n()) %>%  
  mutate(porcentaje=paste0(round(Número/sum(Número)*100,digits=1),"%")) %>%adorn_totals(paste0("row"),"100%")
tabla1$Número<-format(tabla1$Número,big.mark = ".")
#-----------------------------------#

# Gráfico 1

tab1 <- data.frame(Género=rep(c("Hombre", "Mujer"), each=20),
                   Años=rep(c("2002","2003","2004","2005","2006","2007","2008","2009","2010","2011", "2012", "2013",
                              "2014","2015","2016","2017","2018","2019","2020","2021"),2),
                   porcentaje=c(29.9,29.9,29.4,29.4,29.2,28.9,29.0,28.8,28.6,28.0,27.8,27.6,27.4,27.2,27.1,27.1,27.1,26.9,26.6,26.9,
                                70.1,70.1,70.6,70.6,70.8,71.1,71.0,71.2,71.4,72.0,72.2,72.4,72.6,72.8,72.9,72.9,72.9,73.1,73.4,73.1))



# Gráfico

ggplot(tab1, aes(fill=Género, y=porcentaje, x=Años)) + 
  geom_bar(position="fill",stat="identity",orientation = "x") +
  scale_fill_brewer() + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.y = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 1. Evolución de docentes en Chile según sexo, 2002-2021")


#-----------------------------------#


# Tabla 2 
tab2<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>% mutate_at(vars(DOC_FEC_NAC),~(as.character(.))) %>% 
  mutate(year=substr(DOC_FEC_NAC,1,4),mes=substr(DOC_FEC_NAC,5,6)) %>% 
  mutate_at(vars(year,mes),~(as.numeric(.))) %>% mutate(edad=round(2021-year)) %>%
  mutate(edad=case_when(mes>6~round(edad-1),mes<=6~edad)) %>% 
  mutate(tramos=case_when(edad<26~"Menos de 26",edad>=26 & edad<=30~"26 a 30",
                        edad>=31 & edad<=35~"31 a 35",edad>=35 & edad<=40~"36 a 40",
                        edad>=41 & edad<=45~"41 a 45",edad>=46 & edad<=50~"46 a 50",
                        edad>=51 & edad<=55~"51 a 55",edad>=56 & edad<= 60~"56 a 60",
                        edad>=61 & edad<= 65~"61 a 65",edad>65~"Mas de 65")) %>% group_by(tramos,DOC_GENERO) %>%
  summarise(N=n()) %>% mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N)

# Gráfico

ggplot(tab2, aes(fill=DOC_GENERO, y=tramos, x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer() + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 2. Docentes por tramo de edad según sexo, año 2021")

# Tabla recuento
tab2_1<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>% mutate_at(vars(DOC_FEC_NAC),~(as.character(.))) %>% 
  mutate(year=substr(DOC_FEC_NAC,1,4),mes=substr(DOC_FEC_NAC,5,6)) %>% 
  mutate_at(vars(year,mes),~(as.numeric(.))) %>% mutate(edad=round(2021-year)) %>%
  mutate(edad=case_when(mes>6~round(edad-1),mes<=6~edad)) %>% 
  mutate(edad=case_when(edad<26~"Menos de 26",edad>=26 & edad<=30~"26 a 30",
                        edad>=31 & edad<=35~"31 a 35",edad>=35 & edad<=40~"36 a 40",
                        edad>=41 & edad<=45~"41 a 45",edad>=46 & edad<=50~"46 a 50",
                        edad>=51 & edad<=55~"51 a 55",edad>=56 & edad<= 60~"56 a 60",
                        edad>=61 & edad<= 65~"61 a 65",edad>65~"Mas de 65")) %>% group_by(edad) %>%
  summarise(N=n()) %>% pivot_wider(names_from = edad,values_from = N) %>% 
  add_column(Tramos = "Total",.before = "26 a 30") %>% adorn_totals("col")


#-----------------------------------#

#Tabla 3
tab3<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>%
  group_by(RURAL_RBD,DOC_GENERO) %>% summarise(N=n()) %>% 
  mutate(RURAL_RBD=dplyr::recode(RURAL_RBD,"0"="Urbana","1"="Rural")) %>% 
  mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N)

# Gráfico ###

ggplot(tab3, aes(fill=DOC_GENERO, y=RURAL_RBD, x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 3. Docentes por área geográfica según sexo, año 2021")

# Tabla recuento
tab3_1<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>%
  group_by(RURAL_RBD) %>% summarise(N=n()) %>% mutate(RURAL_RBD=dplyr::recode(RURAL_RBD,"0"="Urbana","1"="Rural"))%>% 
  pivot_wider(names_from = RURAL_RBD,values_from = N) %>% 
  add_column(Área_geográfica = "Total",.before = "Urbana") %>% adorn_totals("col") %>% 
  mutate_at(vars(Urbana,Rural,Total),~format(.,big.mark="."))

#-----------------------------------#

# Tabla 4
tab4<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>%
  group_by(DOC_GENERO,RURAL_RBD) %>% summarise(N=n()) %>% 
  mutate(RURAL_RBD=dplyr::recode(RURAL_RBD,"0"="Urbana","1"="Rural")) %>% 
  mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N)

# Gráfico 
ggplot(tab4, aes(fill=RURAL_RBD, y=DOC_GENERO, x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Urbana","Rural")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 4. Docentes sexo según área geográfica, año 2021")

# Tabla recuento

tab4_1<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>%
  group_by(DOC_GENERO) %>% summarise(N=n()) %>% pivot_wider(names_from = DOC_GENERO,values_from = N) %>% 
  add_column(Sexo = "Total",.before = "Hombre") %>% adorn_totals("col") %>% 
  mutate_at(vars(Hombre,Mujer,Total),~format(.,big.mark="."))
  

#-----------------------------------#

#Tabla 5

estees<-docentes_2021 %>% filter(ESTADO_ESTAB==1) %>% group_by(RURAL_RBD,RBD,MRUN,DOC_GENERO) %>% group_by(RURAL_RBD, RBD) %>% 
  summarise(uni_doc=n()) %>% filter(uni_doc==1)

## me faltan 20 docentes en rurales, falta algo del merge, agregar genero
# en la tabla 6 esta la primera parte de esta tabla

#-----------------------------------#

# Tabla 6 (mismo recuento que tab3_1)
tab6<- docentes_2021 %>% filter(ESTADO_ESTAB==1) %>%
  arrange(MRUN, RBD) %>% group_by(MRUN,DOC_GENERO) %>% summarise(Establecimientos=n()) %>% 
  group_by(DOC_GENERO,Establecimientos) %>% 
  mutate(Establecimientos=case_when(Establecimientos==1~"Uno",Establecimientos==2~"Dos",
                                    Establecimientos==3~"Tres",Establecimientos>3~"Más de tres")) %>% 
  summarise(N=n()) %>% mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N)

# Gráfico

ggplot(tab6, aes(fill=fct_relevel(Establecimientos,"Mas de tres","Tres","Dos","Uno"), y=DOC_GENERO, x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Uno","Dos","Tres","Más de tres")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text_repel(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 6. Docentes por sexo según número de establecimientos en los que se desempeñan, año 2021")

#-----------------------------------#

# Tabla 7 y recuento mismo que el 6
tab7<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>% 
  mutate(horas=case_when(HORAS_CONTRATO < 30~1, HORAS_CONTRATO==30~2,
                         HORAS_CONTRATO >= 31 & HORAS_CONTRATO<=37~3,
                    HORAS_CONTRATO >=38&HORAS_CONTRATO<=43~4, HORAS_CONTRATO>43~5)) %>% 
  group_by(DOC_GENERO,horas) %>% summarise(N=n()) %>%
  mutate(horas=dplyr::recode(horas, "1"="<30 hrs","2"="30 hrs",
              "3"="31-37 hrs","4"="38-43 hrs","5"="44 hrs")) %>% 
  mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) 

# Gráfico 7

ggplot(tab7, aes(fill=horas, y=DOC_GENERO, x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("44 hrs","38-43 hrs","31-37 hrs","30 hrs","<30 hrs")) +
  guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text_repel(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 7. Docentes por sexo según horas de contrato, año 2021")


#-----------------------------------#

# Tabla 8
tab8<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>% 
  mutate(ID_IFP=ifelse(ID_IFP%in% c(10,11,12,13,14),6,ID_IFP)) %>%
  group_by(ID_IFP,DOC_GENERO) %>% summarise(N=n()) %>% 
  mutate(ID_IFP=dplyr::recode(ID_IFP,"1"="Docente de aula","2"="Planta técnico-pedagógica",
  "3"="Planta Directiva","4"="Director","5"="Otra en el establecimiento",
  "6"="Otra fuera del establecimiento","7"="Jefe unidad técnico-pedagógica",
  "8"="Inspector General","9"="Orientador","15"="Subdirector","16"="Profesor encargado del establecimiento",
        "17"="Educador Tradicional")) %>%
  mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) 

# Gráfico

ggplot(tab8, aes(y=fct_relevel(ID_IFP,"Educador Tradicional","Profesor encargado del establecimiento",
  "Subdirector","Orientador","Inspector General","Jefe unidad técnico-pedagógica",
  "Otra en el establecimiento","Otra fuera del establecimiento","Director","Planta Directiva",
  "Planta técnico-pedagógica","Docente de aula"),fill=DOC_GENERO,x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer() + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 8. Docentes por función según sexo, año 2021")


# Tabla recuento

tab8_1<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>% 
  mutate(ID_IFP=ifelse(ID_IFP%in% c(10,11,12,13,14),6,ID_IFP)) %>%
  group_by(ID_IFP) %>% summarise(Total=n()) %>% 
  mutate(ID_IFP=dplyr::recode(ID_IFP,"1"="Docente de aula","2"="Planta técnico-pedagógica",
  "3"="Planta Directiva","4"="Director","5"="Otra en el establecimiento",
  "6"="Otra fuera del establecimiento","7"="Jefe unidad técnico-pedagógica",
  "8"="Inspector General","9"="Orientador","15"="Subdirector","16"="Profesor encargado del establecimiento",
  "17"="Educador Tradicional")) %>% adorn_totals("row") %>% 
  mutate_at(vars(Total),~format(.,big.mark="."))

#-----------------------------------#

# Tabla 9
tab9 <- data.frame(Género=rep(c("Hombre", "Mujer"), each=20),
                    Años=rep(c("2002","2003","2004","2005","2006","2007","2008","2009","2010","2011", "2012", "2013",
                               "2014","2015","2016","2017","2018","2019","2020","2021"),2),
                    porcentaje=c(54.3,54.4,51.4,50.4,49.4,48.4,47.8,46.6,45.4,45.4,44.8,44.6,43.5,38.3,37.8,37.3,36.6,36.1,36.0,35.4,
                                 45.7,45.6,48.6,49.6,50.6,51.6,52.2,53.4,54.6,54.6,55.2,55.4,56.5,61.7,62.2,62.7,63.4,63.9,64.0,64.6))

# Gráfico(las labels en x estan muy pegados)

ggplot(data=tab9, aes(x=Años, y=porcentaje, group=Género,color=Género)) +
  geom_line()+geom_point()+ scale_fill_brewer(palette="Paired")+ ylab("")+
  theme(axis.text.y = element_blank(),legend.position = "top")+geom_text_repel(aes(label=paste0(porcentaje,"%")))+
  ggtitle("Gráfico 9. Evolución de directores en Chile según sexo, 2002-2021")


#-----------------------------------#

# Tabla 10 (mismo recuento q graf3)
tab10<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>% 
  mutate(ID_IFP1=ifelse(ID_IFP%in% c(10,11,12,13,14),6,ID_IFP)) %>%  
  mutate(ID_IFP1=ifelse(ID_IFP%in% c(3,16),4,ID_IFP1)) %>% 
  mutate(ID_IFP1=ifelse(ID_IFP1%in% c(5,6,15,17),20,ID_IFP1)) %>% 
  group_by(DOC_GENERO,ID_IFP1) %>% summarise(N=n()) %>% 
  mutate(ID_IFP1=dplyr::recode(ID_IFP1,"1"="Docentes de aula","2"="Planta técnico-pedagógica","4"="Director equipo directivo",
  "7"="Jefe unidad técnico-pedagógica","8"="Inspector General","9"="Orientador","20"= "Otra")) %>% 
  mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N)

# Gráfico (Creo que este se puede graficar solamente en el markdown)

ggplot(tab10, aes(fill=ID_IFP1, y=DOC_GENERO, x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer() + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_label_repel(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 10. Docentes por función según sexo, año 2021")


#-----------------------------------#

# Tabla 11
tab11<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS ==1) %>%
  filter(ID_IFP==1) %>% group_by(COD_DEPE2,DOC_GENERO) %>% summarise(N=n()) %>% 
  mutate(COD_DEPE2=dplyr::recode(COD_DEPE2,"1"="Municipal","2"="Particular Subvencionado",
  "3"="Particular Pagado","4"="Administración Delegada",
  "5"="Servicio Local"))%>% mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N)

# Gráfico

ggplot(tab11, aes(fill=DOC_GENERO, y=fct_relevel(COD_DEPE2,"Servicio Local","Administración Delegada",
  "Particular Pagado","Particular Subvencionado","Municipal"),x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 11. Docentes de aula por tipo de dependencia administrativa del establecimiento según sexo, año 2021")

# Tabla recuento

tab11_1<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS ==1) %>%
  filter(ID_IFP==1) %>% group_by(COD_DEPE2) %>% summarise(N=n()) %>% 
  mutate(COD_DEPE2=dplyr::recode(COD_DEPE2,"1"="Municipal","2"="Particular Subvencionado",
  "3"="Particular Pagado","4"="Administración Delegada",
  "5"="Servicio Local")) %>% pivot_wider(names_from = COD_DEPE2,values_from = N) %>% 
  add_column(Dependencia = "Total",.before = "Municipal") %>% adorn_totals("col") %>% 
  mutate_at(vars(Municipal,"Particular Subvencionado","Particular Pagado","Administración Delegada",
                 "Servicio Local",Total),~format(.,big.mark="."))

#-----------------------------------#

# Tabla 12

tab12<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>% 
  filter(ID_IFP==4) %>% group_by(COD_DEPE2,DOC_GENERO) %>% 
  summarise(N=n())%>% mutate(COD_DEPE2=dplyr::recode(COD_DEPE2,"1"="Municipal","2"="Particular Subvencionado",
          "3"="Particular Pagado","4"="Administración Delegada",
          "5"="Servicio Local")) %>% 
  mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N)

# Gráfico

ggplot(tab12, aes(fill=DOC_GENERO, y=fct_relevel(COD_DEPE2,"Servicio Local","Administración Delegada",
  "Particular Pagado","Particular Subvencionado","Municipal"), x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 12. Directores por tipo de dependencia según sexo, año 2021")

# Tabla recuento

tab12_1<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>% 
  filter(ID_IFP==4) %>% group_by(COD_DEPE2) %>% summarise(N=n()) %>% 
  mutate(COD_DEPE2=dplyr::recode(COD_DEPE2,"1"="Municipal","2"="Particular Subvencionado",
  "3"="Particular Pagado","4"="Administración Delegada","5"="Servicio Local")) %>% 
  pivot_wider(names_from = COD_DEPE2,values_from = N) %>% 
  add_column(Dependencia = "Total",.before = "Municipal") %>% adorn_totals("col") %>% 
  mutate_at(vars(Municipal,"Particular Subvencionado","Particular Pagado","Administración Delegada",
                 "Servicio Local",Total),~format(.,big.mark="."))

#-----------------------------------#

# Tabla 13

tab13<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>%
  filter(NIVEL1 >=1 & NIVEL1<=5 & ID_IFP==1) %>% group_by(NIVEL1,DOC_GENERO) %>% summarise(N=n()) %>% 
  mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N)%>% 
  mutate(NIVEL1=dplyr::recode(NIVEL1,"1"="Educación Parvularia","2"="Educación Especial",
  "3"="Enseñanza Básica","4"="Enseñanza Media HC","5"="Enseñanza Media TP"))

# Gráfico

ggplot(tab13, aes(fill=DOC_GENERO, y=fct_relevel(NIVEL1,"Enseñanza Media TP","Enseñanza Media HC","Educación Especial",
       "Enseñanza Básica","Educación Parvularia") , x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 13. Docentes de aula por nivel de enseñanza en el que se desempeñan (niños y jóvenes) según sexo, año 2021")


# Tabla recuento

tab13_1<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>%
  filter(NIVEL1 >=1 & NIVEL1<=5, ID_IFP==1) %>% group_by(NIVEL1) %>% summarise(N=n()) %>% 
  mutate(NIVEL1=dplyr::recode(NIVEL1,"1"="Educación Parvularia","2"="Educación Especial",
  "3"="Enseñanza Básica","4"="Enseñanza Media HC","5"="Enseñanza Media TP")) %>% 
  pivot_wider(names_from = NIVEL1,values_from = N) %>% 
  add_column(Nivel_Enseñanza = "Total",.before = "Educación Parvularia") %>% adorn_totals("col") %>% 
  mutate_at(vars("Educación Parvularia","Educación Especial","Enseñanza Básica","Enseñanza Media HC",
                 "Enseñanza Media TP",Total),~format(.,big.mark="."))

  

#-----------------------------------#

# Tabla 14 (DIFERENCIA DE 2N, en quinta categoria)

tab14<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>%
  filter(NIVEL1 >=1 & NIVEL1<=5 & ID_IFP==1) %>% group_by(DOC_GENERO,NIVEL1) %>% summarise(N=n()) %>% 
  mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>% 
  mutate(NIVEL1=dplyr::recode(NIVEL1,"1"="Educación Parvularia","2"="Educación Especial",
  "3"="Enseñanza Básica","4"="Enseñanza Media HC","5"="Enseñanza Media TP"))

# Gráfico

ggplot(tab14, aes(fill=fct_relevel(NIVEL1,"Enseñanza Media TP","Enseñanza Media HC",
  "Educación Especial","Enseñanza Básica","Educación Parvularia"),y=DOC_GENERO, x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Educación Parvularia","Enseñanza Básica","Educación Especial",
  "Enseñanza Media HC","Enseñanza Media TP")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text_repel(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 14. Docentes de aula por sexo según el nivel de enseñanza en el que se desempeñan (niños y jóvenes), año 2021")

# Tabla recuento

tab14_1<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>%
  filter(NIVEL1 >=1 & NIVEL1<=5, ID_IFP==1) %>% group_by(DOC_GENERO) %>% summarise(N=n()) %>% 
  pivot_wider(names_from = DOC_GENERO,values_from = N) %>% 
  add_column(Sexo = "Total",.before = "Hombre") %>% adorn_totals("col")%>% 
  mutate_at(vars(Hombre,Mujer,Total),~format(.,big.mark="."))
  

#-----------------------------------#

# Tabla 15
tab15<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>%
  filter(NIVEL1 >=6 & NIVEL1<= 8,ID_IFP==1) %>% group_by(NIVEL1,DOC_GENERO) %>% 
  summarise(N=n()) %>% mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>%
  mutate(NIVEL1=dplyr::recode(NIVEL1,"6"="Enseñanza Básica Adultos","7"="Enseñanza Media HC Adultos",
                              "8"="Enseñanza Media TP Adultos"))
# Gráfico

ggplot(tab15, aes(fill=DOC_GENERO, y=fct_relevel(NIVEL1,"Enseñanza Media TP Adultos",
  "Enseñanza Media HC Adultos","Enseñanza Básica Adultos"), x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 15. Docentes de aula por nivel de enseñanza (adultos) en el que se desempeña según sexo, año 2021")

# Tabla recuento 

tab15_1<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>%
  filter(NIVEL1 >=6 & NIVEL1<= 8,ID_IFP==1) %>% group_by(NIVEL1) %>% 
  summarise(N=n()) %>% mutate(NIVEL1=dplyr::recode(NIVEL1,"6"="Enseñanza Básica Adultos",
                    "7"="Enseñanza Media HC Adultos","8"="Enseñanza Media TP Adultos")) %>% 
  pivot_wider(names_from = NIVEL1,values_from = N) %>% 
  add_column(Nivel_Enseñanza = "Total",.before = "Enseñanza Básica Adultos") %>% adorn_totals("col") %>% 
  mutate_at(vars("Enseñanza Media HC Adultos",Total),~format(.,big.mark="."))

#-----------------------------------#

# Tabla 16

tab16<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>%
  filter(NIVEL1 >=6 & NIVEL1<= 8,ID_IFP==1) %>% group_by(DOC_GENERO,NIVEL1) %>% 
  summarise(N=n()) %>% mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>%
  mutate(NIVEL1=dplyr::recode(NIVEL1,"6"="Enseñanza Básica Adultos","7"="Enseñanza Media HC Adultos",
  "8"="Enseñanza Media TP Adultos"))

# Gráfico

ggplot(tab16, aes(fill=NIVEL1, y=DOC_GENERO, x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Enseñanza Media TP Adultos",
  "Enseñanza Media HC Adultos","Enseñanza Básica Adultos")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 16. Docentes de aula por sexo segun el nivel de enseñanza (adultos) en el que se desempeñan, año 2021")


# Tabla recuento 

tab16_1<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>%
    filter(NIVEL1 >=6 & NIVEL1<= 8,ID_IFP==1) %>% group_by(DOC_GENERO) %>% 
    summarise(N=n()) %>% mutate(DOC_GENERO=dplyr::recode(DOC_GENERO,"1"="Hombre","2"="Mujer")) %>% 
  pivot_wider(names_from = DOC_GENERO,values_from = N) %>% 
  add_column(Nivel_Enseñanza = "Total",.before = "Hombre") %>% adorn_totals("col") %>% 
  mutate_at(vars(Hombre,Mujer,Total),~format(.,big.mark="."))


#-----------------------------------#

# Tabla 17

tab17<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>%
  filter(NIVEL1==2 & ID_IFP==1) %>% group_by(SECTOR1,DOC_GENERO) %>% summarise(N=n()) %>%
  mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>% 
  mutate(SECTOR1=dplyr::recode(SECTOR1,"110"="Lenguaje y Comunicación","115"="Lenguaje Indígena",
  "120"="Matemática","130"="Ciencia","140"= "Tecnología", "150"="Artes",
  "160"="Educación Física", "170"= "Orientación", "180"= "Religión","190"= "Educación General",
  "200"= "Educación Especial"))

# Gráfico

ggplot(tab17, aes(fill=DOC_GENERO, y=fct_relevel(SECTOR1,"Educación Especial","Educación General","Religión",
  "Orientación","Educación Física","Artes","Tecnología","Ciencia","Matemática","Lenguaje Indígena","Lenguaje y Comunicación"), x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 17. Docentes de aula por sector de aprendizaje en el que se desempeñan
          (enseñanza básica niños y jóvenes) según sexo, año 2021")

# Tabla recuento

tab17_1<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>%
  filter(NIVEL1==2 & ID_IFP==1) %>% group_by(SECTOR1) %>% summarise(Total=n()) %>% 
  mutate(SECTOR1=dplyr::recode(SECTOR1,"110"="Lenguaje y Comunicación","115"="Lenguaje Indígena",
  "120"="Matemática","130"="Ciencia","140"= "Tecnología", "150"="Artes",
  "160"="Educación Física", "170"= "Orientación", "180"= "Religión","190"= "Educación General",
  "200"= "Educación Especial")) %>% adorn_totals("row")%>% 
  mutate_at(vars(Total),~format(.,big.mark="."))

#-----------------------------------#

# Tabla 18

tab18<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>%
  filter(ID_IFP==1 & NIVEL1==4) %>% group_by(SECTOR1,DOC_GENERO) %>% summarise(N=n()) %>% 
  mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>%
  mutate(SECTOR1=dplyr::recode(SECTOR1,"200"="Educación Especial","310"="Lenguaje y Comunicación",
  "320"="Matemática","330"="Historia y Ciencia Sociales","340"= "Filosofía y Psicología", "350"="Ciencias Naturales",
  "360"="Educación Tecnológica", "370"= "Educación Artística", "380"= "Educación Física","390"= "Religión",
  "395"= "Otros"))

# Gráfico

ggplot(tab18, aes(fill=DOC_GENERO, y=fct_relevel(SECTOR1,"Otros","Religión","Educación Física","Educación Artística","Educación Tecnológica",
  "Ciencias Naturales","Filosofía y Psicología","Historia y Ciencia Sociales","Matemática","Lenguaje y Comunicación","Educación Especial"), x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 18. Docentes de aula por sector de aprendizaje en el que se desempeñan(enseñanza media HC y jóvenes) según sexo, año 2021")


# Tabla recuento

tab18_1<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>%
  filter(ID_IFP==1 & NIVEL1==4) %>% group_by(SECTOR1) %>% summarise(Total=n()) %>% 
  mutate(SECTOR1=dplyr::recode(SECTOR1,"200"="Educación Especial","310"="Lenguaje y Comunicación",
                               "320"="Matemática","330"="Historia y Ciencia Sociales","340"= "Filosofía y Psicología", "350"="Ciencias Naturales",
                               "360"="Educación Tecnológica", "370"= "Educación Artística", "380"= "Educación Física","390"= "Religión",
                               "395"= "Otros")) %>% adorn_totals("row")%>% 
  mutate_at(vars(Total),~format(.,big.mark="."))

#-----------------------------------#

# Tabla 19 (ME FALTA 1N)
tab19_1<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>%
  filter(ID_IFP==1 & SECTOR1< 410 & NIVEL1==5) %>% group_by(SECTOR1,DOC_GENERO) %>% 
  summarise(N=n()) %>% mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>% 
  mutate(SECTOR1=dplyr::recode(SECTOR1,"200"="Educación Especial","310"="Lenguaje y Comunicación",
  "320"="Matemática","330"="Historia y Ciencia Sociales","340"= "Filosofía y Psicología", "350"="Ciencias Naturales",
  "360"="Educación Tecnológica", "370"= "Educación Artística", "380"= "Educación Física","390"= "Religión",
  "395"= "Otros"))

# Gráfico

ggplot(tab19_1, aes(fill=DOC_GENERO, y=fct_relevel(SECTOR1,"Otros","Religión","Educación Física","Educación Artística","Educación Tecnológica",
  "Ciencias Naturales","Filosofía y Psicología","Historia y Ciencia Sociales","Matemática","Lenguaje y Comunicación","Educación Especial"), x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 19_1. Docentes de aula por sector de aprendizaje en el que se desempeñan(enseñanza media TP niños y jóvenes) según sexo, año 2021")

# Tabla recuento
tab19_1_1<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>%
  filter(ID_IFP==1 & SECTOR1< 410 & NIVEL1==5) %>% group_by(SECTOR1) %>% 
  summarise(Total=n()) %>% mutate(SECTOR1=dplyr::recode(SECTOR1,"200"="Educación Especial","310"="Lenguaje y Comunicación",
                             "320"="Matemática","330"="Historia y Ciencia Sociales","340"= "Filosofía y Psicología", "350"="Ciencias Naturales",
                             "360"="Educación Tecnológica", "370"= "Educación Artística", "380"= "Educación Física","390"= "Religión",
                             "395"= "Otros")) %>% adorn_totals("row") %>% 
  mutate_at(vars(Total),~format(.,big.mark="."))


#-----------------------------------#

# Tabla 19_2
tab19_2<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>%
  filter(ID_IFP==1 & SECTOR1>= 410 & NIVEL1 ==5) %>% group_by(SECTOR1,DOC_GENERO) %>% 
  summarise(N=n()) %>% mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>% 
  mutate(SECTOR1=dplyr::recode(SECTOR1,"410"="Administración y Comercio","510"="Construcción",
                               "520"="Metalmecánico","530"="Electricidad","540"= "Minero", "550"="Gráfico",
                               "560"="Química", "570"= "Confección", "580"= "Tecnología y Telecomunicaciones","610"= "Alimentación",
                               "620"= "Programas y Proyectos Sociales","630"="Hotelería y Turismo","710"="Maderero","720"= "Agropecuario","810"="Marítimo"))
# Gráfico

ggplot(tab19_2, aes(fill=DOC_GENERO, y=fct_relevel(SECTOR1,"Marítimo","Agropecuario","Maderero","Hotelería y Turismo",
  "Programas y Proyectos Sociales","Alimentación","Tecnología y Telecomunicaciones","Confección","Química","Gráfico","Minero","Electricidad",
  "Metalmecánico","Construcción","Administración y Comercio"), x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 19_1. Docentes de aula por sector económico en el que se desempeñan(enseñanza media TP niños y jóvenes) según sexo, año 2021")

# Tabla recuento

tab19_2_2<-docentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>%
  filter(ID_IFP==1 & SECTOR1>= 410 & NIVEL1 ==5) %>% group_by(SECTOR1) %>% 
  summarise(Total=n()) %>% mutate(SECTOR1=dplyr::recode(SECTOR1,"410"="Administración y Comercio","510"="Construcción",
                                                    "520"="Metalmecánico","530"="Electricidad","540"= "Minero", "550"="Gráfico",
                                                    "560"="Química", "570"= "Confección", "580"= "Tecnología y Telecomunicaciones","610"= "Alimentación",
                                                    "620"= "Programas y Proyectos Sociales","630"="Hotelería y Turismo","710"="Maderero","720"= "Agropecuario","810"="Marítimo")) %>% 
  adorn_totals("row") %>% mutate_at(vars(Total),~format(.,big.mark="."))

###------------------------------------------------------------------------####
###------------------------------------------------------------------------####


#3.Asistentes de la educación
# RECODIFICACIÓN BBDD
asistentes_2021<-asistentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>% 
  mutate(GEN_ASIS=dplyr::recode(GEN_ASIS, "1"="Hombre","2"="Mujer"))

###------------------------------------------------------------------------####
###------------------------------------------------------------------------####


tabla2<- asistentes_2021 %>% group_by(GEN_ASIS) %>% summarise(Número=n()) %>%
  mutate(porcentaje=paste0(round(Número/sum(Número)*100,digits=1),"%")) %>% adorn_totals(paste0("row"),"100%")
tabla2$Número<-format(tabla2$Número,big.mark = ".")

#-----------------------------------#

# Tabla 20

tab20<-asistentes_2021 %>% mutate_at(vars(FEC_NAC_ASIS),~(as.character(.))) %>% 
  mutate(year=substr(FEC_NAC_ASIS,1,4),mes=substr(FEC_NAC_ASIS,5,6)) %>% 
  mutate_at(vars(year,mes),~(as.numeric(.))) %>% mutate(edad=round(2021-year)) %>%
  mutate(edad=case_when(mes>6~round(edad-1),mes<=6~edad)) %>% 
  mutate(edad=case_when(edad<26~"Menos de 26",edad>=26 & edad<=30~"26 a 30",
                        edad>=31 & edad<=35~"31 a 35",edad>=35 & edad<=40~"36 a 40",
                        edad>=41 & edad<=45~"41 a 45",edad>=46 & edad<=50~"46 a 50",
                        edad>=51 & edad<=55~"51 a 55",edad>=56 & edad<= 60~"56 a 60",
                        edad>=61 & edad<= 65~"61 a 65",edad>65~"Mas de 65")) %>% group_by(edad,GEN_ASIS) %>%
  summarise(N=n()) %>% mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N)

# Gráfico

ggplot(tab20, aes(fill=GEN_ASIS, y=fct_relevel(edad,"Mas de 65","61 a 65","56 a 60","51 a 55","46 a 50",
  "41 a 45","36 a 40","31 a 35","26 a 30","Menos de 26"), x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 2. Docentes por tramo de edad según sexo, año 2021")

# Tabla recuento

tab20_1<-asistentes_2021 %>% mutate_at(vars(FEC_NAC_ASIS),~(as.character(.)))%>%
  mutate(year=substr(FEC_NAC_ASIS,1,4),mes=substr(FEC_NAC_ASIS,5,6)) %>% 
  mutate_at(vars(year,mes),~(as.numeric(.))) %>% mutate(edad=round(2021-year))%>%
  mutate(edad=case_when(mes>6~round(edad-1),mes<=6~edad)) %>% 
  mutate(edad=case_when(edad<26~"Menos de 26",
                        edad>=26 & edad<=30~"26 a 30",
                        edad>=31 & edad<=35~"31 a 35",
                        edad>=35 & edad<=40~"36 a 40",
                        edad>=41 & edad<=45~"41 a 45",
                        edad>=46 & edad<=50~"46 a 50",
                        edad>=51 & edad<=55~"51 a 55",
                        edad>=56 & edad<= 60~"56 a 60",edad>=61 & edad<= 65~"61 a 65",edad>65~"Mas de 65")) %>% group_by(edad) %>%
  summarise(N=n()) %>% pivot_wider(names_from = edad,values_from = N) %>% 
  add_column(Tramos = "Total",.before = "26 a 30") %>% adorn_totals("col") %>% 
  mutate_at(vars(everything(.)),~format(.,big.mark="."))


#-----------------------------------#

# tabla 21

tab21<- asistentes_2021 %>% group_by(RURAL_RBD,GEN_ASIS) %>% summarise(N=n()) %>% 
  mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>% 
  mutate(RURAL_RBD=dplyr::recode(RURAL_RBD,"0"="Urbano","1"="Rural"))

# Gráfico

ggplot(tab21, aes(fill=GEN_ASIS, y=RURAL_RBD, x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 21. Asistentes de la educación por área geográfica según sexo, año 2021")

# Tabla recuento

tab21_1<- asistentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>% 
  group_by(RURAL_RBD) %>% summarise(N=n()) %>% 
  mutate(RURAL_RBD=dplyr::recode(RURAL_RBD,"0"="Urbano","1"="Rural")) %>% 
  pivot_wider(names_from = RURAL_RBD,values_from = N) %>% 
  add_column(Área_geográfica = "Total",.before = "Urbano") %>% adorn_totals("col") %>%
  mutate_at(vars(2:4), ~(format(., big.mark = ".")))


#-----------------------------------#

# Tabla 22
tab22<- asistentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>% 
  group_by(ID_ESTAMENTO,GEN_ASIS) %>% summarise(N=n()) %>%
  mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>%
  mutate(ID_ESTAMENTO=dplyr::recode(ID_ESTAMENTO,"1"="Profesional","2"="Paradocente","3"="Auxiliar",
  "4"="Administrativo","5"= "Técnico","6"= "Sin Información"))

# Gráfico

ggplot(tab22, aes(fill=GEN_ASIS, y=fct_relevel(ID_ESTAMENTO,"Sin Información","Técnico","Administrativo",
  "Auxiliar","Paradocente","Profesional"), x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 22. Asistentes por función y sexo, año 2021")

# Tabla recuento
tab22_1<- asistentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>% 
  group_by(ID_ESTAMENTO) %>% summarise(N=n()) %>% 
  mutate(ID_ESTAMENTO=dplyr::recode(ID_ESTAMENTO,"1"="Profesional","2"="Paradocente","3"="Auxiliar",
                                 "4"="Administrativo","5"= "Técnico","6"= "Sin Información")) %>% 
  pivot_wider(names_from = ID_ESTAMENTO,values_from = N) %>% 
  add_column(Función = "Total",
              .before = "Profesional") %>% adorn_totals("col") %>% 
  mutate_at(vars(2:8),~(format(.,big.mark=".")))

#-----------------------------------#

# Tabla 23

tab23<- asistentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>% 
  group_by(GEN_ASIS,ID_ESTAMENTO) %>% summarise(N=n()) %>%
  mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>%
  mutate(ID_ESTAMENTO=dplyr::recode(ID_ESTAMENTO,"1"="Profesional","2"="Paradocente","3"="Auxiliar",
                                    "4"="Administrativo","5"= "Técnico","6"= "Sin Información"))

# Gráfico

ggplot(tab23, aes(fill=fct_relevel(ID_ESTAMENTO,"Sin Información","Técnico","Administrativo",
  "Auxiliar","Paradocente","Profesional"), y=GEN_ASIS, x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Profesional","Paradocente","Auxiliar","Administrativo",
  "Técnico","Sin Información")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text_repel(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 23. Asistentes de la educación por sexo según función, año 2021")


# Tabla recuento

tab23_1<- asistentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>% 
  group_by(GEN_ASIS) %>% summarise(N=n()) %>%
  mutate(GEN_ASIS=dplyr::recode(GEN_ASIS,"1"= "Hombre","2"="Mujer")) %>% 
  pivot_wider(names_from = GEN_ASIS,values_from = N) %>% add_column(Sexo="Total",
      .before = "Hombre") %>% adorn_totals("col")

#-----------------------------------#

# Tabla 24
tab24<- asistentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>% 
  group_by(COD_DEPE2,GEN_ASIS) %>% summarise(N=n()) %>% 
  mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>%
  mutate(COD_DEPE2=dplyr::recode(COD_DEPE2,"1"="Municipal","2"="Particular Subvencionado","3"="Particular Pagado",
                                 "4"="Administración Delegada","5"="Servicio Local"))

# Gráfico

ggplot(tab24, aes(fill=GEN_ASIS, y=fct_relevel(COD_DEPE2,"Servicio Local","Administración Delegada",
              "Particular Pagado","Particular Subvencionado","Municipal"),x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 24. Asistentes de la educación por tipo de dependencia según sexo, año 2021")

# Tabla recuento

tab24_1<- asistentes_2021 %>% filter(ESTADO_ESTAB==1 & PERSONAS==1) %>% 
  group_by(COD_DEPE2) %>% summarise(N=n()) %>% 
  mutate(COD_DEPE2=dplyr::recode(COD_DEPE2,"1"="Municipal","2"="Particular Subvencionado","3"="Particular Pagado",
                                "4"="Administración Delegada","5"="Servicio Local")) %>% 
  pivot_wider(names_from = COD_DEPE2,values_from = N) %>% add_column(Dependencia_administrativa="Total",
  .before = "Municipal") %>% adorn_totals("col") %>% mutate_at(vars(2:7),~format(.,big.mark="."))


###------------------------------------------------------------------------####
###------------------------------------------------------------------------####

# Estudiantes en el sistema escolar
# RECODIFICACION BBDD MATRICULA 
matriculamod<-matricula_2021priv %>% filter(GEN_ALU!=0) %>% mutate(GEN_ALU=dplyr::recode(GEN_ALU,"0"="0","1"="Hombre","2"="Mujer"))
rm(matricula_2021priv)# se elimina para evitar tanto objetos


###------------------------------------------------------------------------####


tabla3<- matriculamod %>% filter(ESTADO_ESTAB==1) %>% filter(COD_ENSE3==1 | COD_ENSE3==2 |COD_ENSE3==4 |COD_ENSE3==6) %>% 
  group_by(GEN_ALU) %>% summarise(Número=n()) %>% mutate(porcentaje=paste0(round(Número/sum(Número)*100,digits=1),"%")) %>% adorn_totals(paste0("row"),"100 %")
tabla3$Número<-format(tabla3$Número,big.mark = ".")


#-----------------------------------#

# Tabla 25

tab25<- matriculamod %>% filter(ESTADO_ESTAB==1) %>%
  filter(COD_ENSE3==1 | COD_ENSE3==2 |COD_ENSE3==4 |COD_ENSE3==6) %>% 
  group_by(ENS,GEN_ALU) %>% summarise(N=n())%>% mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>%
  mutate(ENS=dplyr::recode(ENS,"1"="Parvularia Regular","2"="Parvularia Especial","3"="Básica niños regular",
                           "4"="Básica niños especial","5"="Media HC ciclo general","6"="Media TP ciclo general","7"="Media HC ciclo diferenciado",
                           "8"="Media TP ciclo diferenciado"))

# Gráfico

ggplot(tab25, aes(fill=GEN_ALU, y=fct_relevel(ENS,"Media TP ciclo diferenciado","Media HC ciclo diferenciado",
      "Media TP ciclo general","Media HC ciclo general","Básica niños especial",
      "Básica niños regular","Parvularia Especial","Parvularia Regular"), x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 25. Estudiantes por nivel de enseñanza según sexo, jóvenes y niños, año 2021")


# Tabla recuento 

tab25_1<- matriculamod %>% filter(ESTADO_ESTAB==1) %>%
  filter(COD_ENSE3==1 | COD_ENSE3==2 |COD_ENSE3==4 |COD_ENSE3==6) %>% 
  group_by(ENS) %>% summarise(N=n()) %>% mutate(ENS=dplyr::recode(ENS,"1"="Parvularia Regular","2"="Parvularia Especial","3"="Básica niños regular",
                           "4"="Básica niños especial","5"="Media HC ciclo general","6"="Media TP ciclo general","7"="Media HC ciclo diferenciado",
                           "8"="Media TP ciclo diferenciado")) %>% adorn_totals("row")
tab25_1$N<-format(tab25_1$N,big.mark = ".")
#-----------------------------------#

# Tabla 26
tab26<- matriculamod %>% filter(ESTADO_ESTAB==1 & COD_RAMA!=0 & ENS==8 ) %>% 
  group_by(COD_RAMA,GEN_ALU) %>% summarise(N=n()) %>% mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>%
  mutate(COD_RAMA=dplyr::recode(COD_RAMA,"400"="Comercial","500"="Industrial","600"="Técnica","700"="Agrícola",
                                "800"="Marítima","900"="Artística"))
# Gráfico
ggplot(tab26, aes(fill=GEN_ALU, y=fct_relevel(COD_RAMA,"Artística","Marítima","Agrícola","Técnica",
                                              "Industrial","Comercial"),x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 26. Estudiantes educación media TP y artística por rama de especialidad según sexo, jóvenes y niños, año 2021")

# Tabla recuento
tab26_1<- matriculamod %>% filter(ESTADO_ESTAB==1 & COD_RAMA!=0 & ENS==8 ) %>% 
  group_by(COD_RAMA) %>% summarise(N=n()) %>% mutate(COD_RAMA=dplyr::recode(COD_RAMA,"400"="Comercial","500"="Industrial","600"="Técnica","700"="Agrícola",
                                "800"="Marítima","900"="Artística")) %>% adorn_totals("row")
tab26_1$N<-format(tab26_1$N,big.mark = ".")

#-----------------------------------#

# Tabla 27

tab27<- matriculamod %>% filter(ESTADO_ESTAB==1 & COD_RAMA!=0 & ENS==8 ) %>% 
  group_by(GEN_ALU,COD_RAMA) %>% summarise(N=n()) %>% mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>%
  mutate(COD_RAMA=dplyr::recode(COD_RAMA,"400"="Comercial","500"="Industrial","600"="Técnica","700"="Agrícola",
                                "800"="Marítima","900"="Artística"))
# Gráfico

ggplot(tab27, aes(fill=fct_relevel(COD_RAMA,"Artística","Marítima","Agrícola","Técnica",
                  "Industrial","Comercial"), y=GEN_ALU, x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Comercial","Industrial","Técnica","Agrícola",
                             "Marítima","Artística")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text_repel(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 27. Estudiantes educación media TP y artística por sexo según rama de especialidad, jóvenes y niños, año 2021")

# Tabla recuento

tab27_1<- matriculamod %>% filter(ESTADO_ESTAB==1 & COD_RAMA!=0 & ENS==8 ) %>% 
  group_by(GEN_ALU) %>% summarise(N=n()) %>% pivot_wider(names_from = GEN_ALU,values_from = N) %>% add_column(Sexo="Total",
      .before = "Hombre") %>% adorn_totals("col") %>% mutate_at(vars(2:4),~format(.,big.mark="."))

#-----------------------------------#

# Tabla 28
tab28<- matriculamod %>% filter(ESTADO_ESTAB==1 & ENS==8) %>% 
  group_by(COD_SEC,GEN_ALU) %>% summarise(N=n()) %>% mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>% 
  mutate(COD_SEC=dplyr::recode(COD_SEC,"410"="Administración y Comercio","510"="Construcción","520"="Metalmecánico","530"="Electricidad",
                               "540"="Minero","550"="Gráfica","560"="Químico","570"="Confección","580"="Tecnología y Telecomunicaciones",
                               "610"="Alimentación","630"="Hotelería y Turismo","640"="Salud y Educación","710"="Maderero","720"="Agropecuario","810"="Marítimo","910"="Artes Visuales"))
# Gráfico
ggplot(tab28, aes(fill=GEN_ALU, y=COD_SEC, x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 28. Estudiantes educación media TP y artística ciclo diferenciado por sector económico según sexo, jóvenes y niños, año 2021")

# Tabla recuento
tab28_1<- matriculamod %>% filter(ESTADO_ESTAB==1 & ENS==8) %>% 
  group_by(COD_SEC) %>% summarise(Total=n()) %>% mutate(COD_SEC=dplyr::recode(COD_SEC,"410"="Administración y Comercio","510"="Construcción","520"="Metalmecánico","530"="Electricidad",
  "540"="Minero","550"="Gráfica","560"="Químico","570"="Confección","580"="Tecnología y Telecomunicaciones",
  "610"="Alimentación","630"="Hotelería y Turismo","640"="Salud y Educación","710"="Maderero","720"="Agropecuario","810"="Marítimo","910"="Artes Visuales")) %>% 
  adorn_totals("row") %>% mutate_at(vars(Total),~format(.,big.mark="."))

#-----------------------------------#

# Tabla 29

tab29<- matriculamod %>% filter(ESTADO_ESTAB==1) %>% 
  filter(COD_ENSE3==1 | COD_ENSE3==2 |COD_ENSE3==4 |COD_ENSE3==6) %>%
  group_by(COD_DEPE2,GEN_ALU) %>% summarise(N=n()) %>% mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>% 
  mutate(COD_DEPE2=dplyr::recode(COD_DEPE2,"1"="Municipal","2"="Particular Subvencionado","3"="Particular Pagado",
                                 "4"="Administración Delegada","5"="Servicio Local"))
# Gráfico

ggplot(tab29, aes(fill=GEN_ALU, y=COD_DEPE2, x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 29. Estudiantes tipo de dependencia según sexo, jóvenes y niños, año 2021")

# Tabla recuento

tab29_1<- matriculamod %>% filter(ESTADO_ESTAB==1) %>% 
  filter(COD_ENSE3==1 | COD_ENSE3==2 |COD_ENSE3==4 |COD_ENSE3==6) %>%
  group_by(COD_DEPE2) %>% summarise(N=n()) %>% mutate(COD_DEPE2=dplyr::recode(COD_DEPE2,"1"="Municipal","2"="Particular Subvencionado","3"="Particular Pagado",
                                 "4"="Administración Delegada","5"="Servicio Local")) %>% pivot_wider(names_from = COD_DEPE2,values_from = N) %>%
  add_column(Dependencia_administrativa="Total",.before = "Municipal") %>% adorn_totals("col")

#-----------------------------------#

# Tabla 30
tab30_1<- matriculamod %>% filter(ESTADO_ESTAB==1) %>% 
  filter(COD_ENSE3==1 | COD_ENSE3==2 |COD_ENSE3==4 |COD_ENSE3==6, COD_ETNIA_ALU!=11) %>%
  mutate(COD_ETNIA_ALU=ifelse(COD_ETNIA_ALU== 0,0,1)) %>% group_by(COD_ETNIA_ALU,GEN_ALU) %>% summarise(N=n()) %>% 
  mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>%
  mutate(COD_ETNIA_ALU=dplyr::recode(COD_ETNIA_ALU,"0"="No pertenece a ninguna etnia","1"= "Pertenece a alguna etnia"))

# Gráfico
ggplot(tab30_1, aes(fill=GEN_ALU, y=COD_ETNIA_ALU, x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 30_1. Estudiantes con/sin pertencia a alguna ascendencia indígena según sexo, año 2021")

# Tabla recuento

tab30_1_1<- matriculamod %>% filter(ESTADO_ESTAB==1) %>% 
  filter(COD_ENSE3==1 | COD_ENSE3==2 |COD_ENSE3==4 |COD_ENSE3==6, COD_ETNIA_ALU!=11) %>%
  mutate(COD_ETNIA_ALU=ifelse(COD_ETNIA_ALU== 0,0,1)) %>% group_by(COD_ETNIA_ALU) %>% summarise(N=n()) %>% 
  mutate(COD_ETNIA_ALU=dplyr::recode(COD_ETNIA_ALU,"0"="No pertenece a ninguna etnia","1"= "Pertenece a alguna etnia")) %>% 
  pivot_wider(names_from = COD_ETNIA_ALU,values_from = N) %>%
  add_column(Ascendencia_indígena="Total",.before = "No pertenece a ninguna etnia") %>% adorn_totals("col") %>% 
  mutate_at(vars(everything(.)),~format(.,big.mark="."))

#-----------------------------------#

# Tabla 30_2

tab30_2<- matriculamod %>% filter(ESTADO_ESTAB==1) %>% 
  filter(COD_ENSE3==1 | COD_ENSE3==2 |COD_ENSE3==4 |COD_ENSE3==6, COD_ETNIA_ALU!=11, RURAL_RBD==1) %>%
  mutate(COD_ETNIA_ALU=ifelse(COD_ETNIA_ALU== 0,0,1)) %>% group_by(GEN_ALU,COD_ETNIA_ALU) %>% summarise(N=n()) %>% 
  mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N)%>%
  mutate(COD_ETNIA_ALU=dplyr::recode(COD_ETNIA_ALU,"0"="No pertenece a ninguna etnia","1"= "Pertenece a alguna etnia"))

# Gráfico

ggplot(tab30_2, aes(fill=COD_ETNIA_ALU, y=GEN_ALU, x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 30_2. Estudiantes de establecimientos rurales por sexo según su pertenencia a alguna ascendencia indígena, niños y jóvenes, año 2021")

# Tabla recuento

tab30_2_1<- matriculamod %>% filter(ESTADO_ESTAB==1) %>% 
  filter(COD_ENSE3==1 | COD_ENSE3==2 |COD_ENSE3==4 |COD_ENSE3==6, COD_ETNIA_ALU!=11, RURAL_RBD==1) %>%
  mutate(COD_ETNIA_ALU=ifelse(COD_ETNIA_ALU== 0,0,1)) %>% group_by(GEN_ALU) %>% summarise(N=n()) %>% 
  pivot_wider(names_from = GEN_ALU,values_from = N) %>%
  add_column(Sexo="Total",.before = "Hombre") %>% adorn_totals("col") %>% 
  mutate_at(vars(everything(.)),~format(.,big.mark="."))

#-----------------------------------#

# Tabla 30_3

tab30_3<- matriculamod %>% filter(ESTADO_ESTAB==1) %>% 
  filter(COD_ENSE3==1 | COD_ENSE3==2 |COD_ENSE3==4 |COD_ENSE3==6) %>%
  group_by(RURAL_RBD,GEN_ALU) %>% summarise(N=n()) %>% mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>% 
  mutate(RURAL_RBD=dplyr::recode(RURAL_RBD,"0"="Urbano","1"="Rural"))

# Gráfico

ggplot(tab30_3, aes(fill=GEN_ALU, y=RURAL_RBD, x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 30_3. Estudiantes por tipo de establecimiento urbano/rural según sexo,
          niños y jóvenes, año 2021")

# Tabla recuento

tab30_3_1<- matriculamod %>% filter(ESTADO_ESTAB==1) %>% 
  filter(COD_ENSE3==1 | COD_ENSE3==2 |COD_ENSE3==4 |COD_ENSE3==6) %>%
  group_by(RURAL_RBD) %>% summarise(N=n()) %>% mutate(RURAL_RBD=dplyr::recode(RURAL_RBD,"0"="Urbano","1"="Rural")) %>% 
  pivot_wider(names_from = RURAL_RBD,values_from = N) %>%
  add_column(Tipo_establecimiento="Total",.before = "Urbano") %>% adorn_totals("col") %>% 
  mutate_at(vars(everything(.)),~format(.,big.mark="."))

#-----------------------------------#

tabla4<- matriculamod %>% filter(ESTADO_ESTAB==1, COD_NAC_ALU=="E") %>% 
  group_by(COD_REG_RBD,GEN_ALU) %>% summarise(N=n()) %>% 
  mutate(COD_REG_RBD=dplyr::recode(COD_REG_RBD,"1"= "Región de Tarapacá","2"="Región de Antofagasta","3"="Región de Atacama","4"="Región de Coquimbo",
  "5"="Región de Valparaíso","6"="Región del Libertador Gral. Bernardo O´higgins","7"="Región del Maule",
  "8"="Región del BioBío","9"="Región de la Araucanía","10"="Región de Los Lagos",
  "11"="Región de Aysén del Gral. Carlos Ibáñez del Campo","12"="Región de Magallanes y de la Antártica Chilena","13"="Región Metropolitana de Santiago",
  "14"="Región de Los Ríos","15"="Región de Arica y Parinacota","16"="Región de Ñuble")) %>% 
  pivot_wider(names_from = GEN_ALU,values_from = N) %>% adorn_totals("col")%>%mutate(Hporc=round(Hombre/Total*100,digits=1),Mporc=round(Mujer/Total*100,digits=1)) %>% 
  select(COD_REG_RBD,Mujer,Mporc,Hombre,Hporc,Total) %>% adorn_totals("row") %>%
  mutate_at(vars(Mujer,Hombre,Total),~format(.,big.mark="."))

#-----------------------------------#

tabla5<- matriculamod %>% filter(ESTADO_ESTAB==1,COD_NAC_ALU=="E") %>% 
  group_by(COD_DEPE2,GEN_ALU) %>% summarise(N=n()) %>%
  mutate(COD_DEPE2=dplyr::recode(COD_DEPE2,"1"="Municipal","2"="Particular Subvencionado","3"="Particular Pagado",
                                "4"="Administración Delegada","5"="Servicio Local")) %>% 
  pivot_wider(names_from = GEN_ALU,values_from = N) %>% adorn_totals("col")%>%mutate(Hporc=round(Hombre/Total*100,digits=1),Mporc=round(Mujer/Total*100,digits=1)) %>% 
  select(COD_DEPE2,Mujer,Mporc,Hombre,Hporc,Total) %>% adorn_totals("row") %>%
  mutate_at(vars(Mujer,Hombre,Total),~format(.,big.mark="."))

#-----------------------------------#

tabla6<- matriculamod %>% filter(ESTADO_ESTAB==1,COD_NAC_ALU=="E") %>% group_by(COD_ENSE2,GEN_ALU) %>%
  summarise(N=n())%>% mutate(COD_ENSE2=dplyr::recode(COD_ENSE2,"1"="Educación Parvularia","2"="Educación Básica niños","3"="Educación Básica Adultos",
  "4"="Educación Especial","5"="Enseñanza Media HCJóvenes","6"="Enseñanza Media HC Adultos",
  "7"="Enseñanza Media Tp Jóvenes","8"="Enseñanza Media Tp Adultos")) %>% 
  pivot_wider(names_from = GEN_ALU,values_from = N) %>% adorn_totals("col")%>%mutate(Hporc=round(Hombre/Total*100,digits=1),Mporc=round(Mujer/Total*100,digits=1)) %>% 
  select(COD_ENSE2,Mujer,Mporc,Hombre,Hporc,Total) %>% adorn_totals("row") %>% mutate_at(vars(Mujer,Hombre,Total),~format(.,big.mark="."))

###--------------------------------------------------------------------########
###--------------------------------------------------------------------########

# 4. Rendimiento alumnos
# Recodificacion bbdd
rend2021<-rendimiento_2021 %>% filter(ESTADO_ESTAB==1, GEN_ALU!=0,SIT_FIN_R=="P"|SIT_FIN_R=="R"|SIT_FIN_R=="Y") %>% 
  mutate(GEN_ALU=dplyr::recode(GEN_ALU,"1"="Hombre","2"= "Mujer"))
rm(rendimiento_2021)# Se elimina para evitar muchos objetos

###--------------------------------------------------------------------########
###--------------------------------------------------------------------########

# TABLA DEL TOTAL DE MATRICULA(TASAS: APROBADOS/TOTALMATRICULA*100)

#5. Se construyen tablas de totales para calcular tasas

total<- rend2021 %>% filter(COD_ENSE==110) %>%  group_by(GEN_ALU,COD_GRADO) %>% summarise(N=n())

# Tabla matricula total enseñanza media 

total_2<- rend2021 %>% filter(COD_ENSE %in% c(310,410,510,610,710,810,910)) %>% 
  group_by(GEN_ALU,COD_GRADO) %>% summarise(N=n())

# Tabla total matricula extranjeros por dependencia adm
total_3<- rend2021 %>% filter(COD_NAC_ALU=="E") %>% 
  group_by(GEN_ALU,COD_DEPE2) %>% summarise(N=n())

# Tabla total matricula extranjeros por region (no me está dando el total)
total_4<- rend2021 %>% filter(COD_NAC_ALU=="E") %>% 
  group_by(GEN_ALU,COD_REG_RBD) %>% summarise(N=n())

# Tabla total matricula extranjeros de basica
total_5<- rend2021 %>% filter(COD_NAC_ALU=="E",COD_ENSE2==2) %>% 
  group_by(GEN_ALU,COD_GRADO) %>% summarise(N=n())

# Tabla total matricula extranjeros de media (NO ME ESTA DANDO EL TOTAL)
total_6<- rend2021 %>% filter(COD_ENSE2==5|COD_ENSE2==7, COD_NAC_ALU=="E") %>% 
  group_by(GEN_ALU,COD_GRADO) %>% summarise(N=n())# %>% pivot_wider(names_from = GEN_ALU,values_from = N) %>% adorn_totals("col")

# Tabla total alumnos extranjeros aprobados
total_7<- rend2021 %>% filter(COD_NAC_ALU=="E",SIT_FIN_R=="P") %>% 
  group_by(COD_ENSE2) %>% summarise(N=n())# %>% pivot_wider(names_from = GEN_ALU,values_from = N) %>% adorn_totals("col")

# Tabla total situacion extranjeros basica
total_8<- rend2021 %>% filter(COD_ENSE2==3)%>% group_by(GEN_ALU,COD_GRADO) %>% summarise(N=n())# %>% 
  #pivot_wider(names_from = GEN_ALU,values_from = N) %>% adorn_totals("col")

# Tabla total situacion extranjeros media
total_9<- rend2021 %>% filter(COD_ENSE2==6|COD_ENSE2==8)%>% group_by(GEN_ALU,COD_ENSE2) %>% summarise(N=n())# %>% 
  #pivot_wider(names_from = GEN_ALU,values_from = N) %>% adorn_totals("col")



###--------------------------------------------------------------------########
###--------------------------------------------------------------------########



##------------------------------------------------------------######

# Tabla 31_1 (el recuento es la tabla total)

tab31_1<- rend2021 %>% filter(COD_ENSE==110,SIT_FIN_R=="P") %>%  group_by(GEN_ALU,COD_GRADO) %>% summarise(N=n())
tab31_1<-cbind(tab31_1,total=total$N)
tab31_1<-tab31_1 %>% mutate(tasa=round(N/total*100,digits = 1)) %>% 
  select(GEN_ALU,COD_GRADO,tasa)

# Gráfico

ggplot(data=tab31_1, aes(x=COD_GRADO, y=tasa, fill=GEN_ALU)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
  coord_cartesian(ylim = c(95,100))+ylab("Tasa")+xlab("Grado")+
  theme(axis.text.y= element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
  scale_x_continuous(breaks=c(1:8), labels=c("1°","2°","3°","4","5°","6°","7°","8°"),limits=c(0,9))+
  ggtitle("Gráfico 31_1. Tasa de aprobación estudiantes de ensenanza básica por grado
          según sexo, niños y jóvenes, año 2021")


# Recuento

recuentotal<- rend2021 %>% filter(COD_ENSE==110) %>%  group_by(GEN_ALU,COD_GRADO) %>% summarise(N=n())%>% 
  pivot_wider(names_from = GEN_ALU,values_from = N)  %>% adorn_totals("col") %>% 
  mutate_at(vars(Mujer,Hombre,Total),~format(.,big.mark="."))


#-----------------------------------#

# Tabla 31_2
tab31_2<- rend2021 %>% filter(COD_ENSE==110) %>% mutate(situacion=ifelse(SIT_FIN_R=="R",1,0)) %>% 
  filter(situacion==1) %>% group_by(GEN_ALU,COD_GRADO) %>% summarise(N=n())
tab31_2<-cbind(tab31_2,total=total$N)
tab31_2<-tab31_2 %>% mutate(tasa=round(N/total*100,digits = 1)) %>% 
  select(GEN_ALU,COD_GRADO,tasa)

# Gráfico

ggplot(data=tab31_2, aes(x=COD_GRADO, y=tasa, fill=GEN_ALU)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
  coord_cartesian(ylim = c(0,3))+ylab("Tasa")+xlab("Grado")+
  theme(axis.text.y= element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
  scale_x_continuous(breaks=c(1:8), labels=c("1°","2°","3°","4","5°","6°","7°","8°"),limits=c(0,9))+
  ggtitle("Gráfico 31_2. Tasa de reprobación estudiantes de enseñanza básica
          por grado, según sexo, niños y jóvenes, año 2021")

# Recuento

tab31_2_1<- rend2021 %>% filter(COD_ENSE==110) %>% mutate(situacion=ifelse(SIT_FIN_R=="R",1,0)) %>% 
  filter(situacion==1) %>% group_by(GEN_ALU,COD_GRADO) %>% summarise(N=n()) %>% 
  pivot_wider(names_from = GEN_ALU,values_from = N) %>% adorn_totals(c("col","row")) %>% 
  mutate_at(vars(Hombre,Mujer,Total),~format(.,big.mark="."))


#-----------------------------------#

# Tabla 31_3
tab31_3<-rend2021 %>% filter(COD_ENSE==110) %>% mutate(situacion=ifelse(SIT_FIN_R=="Y",1,0)) %>% 
  filter(situacion==1) %>% group_by(GEN_ALU,COD_GRADO) %>% summarise(N=n())
tab31_3<-cbind(tab31_3,total=total$N)
tab31_3<-tab31_3 %>% mutate(tasa=round(N/total*100,digits = 1)) %>% 
  select(GEN_ALU,COD_GRADO,tasa)

# Gráfico

ggplot(data=tab31_3, aes(x=COD_GRADO, y=tasa, fill=GEN_ALU)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
  coord_cartesian(ylim = c(0,2))+ylab("Tasa")+xlab("Grado")+
  theme(axis.text.y= element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
  scale_x_continuous(breaks=c(1:8), labels=c("1°","2°","3°","4°","5°","6°","7°","8°"),limits=c(0,9))+
  ggtitle("Gráfico 31_3. Tasa de abandono estudiantes de enseñanza básica por
          grado, según sexo, niños y jóvenes, año 2021")

# Recuento

tab31_3_1<-rend2021 %>% filter(COD_ENSE==110) %>% mutate(situacion=ifelse(SIT_FIN_R=="Y",1,0)) %>% 
  filter(situacion==1) %>% group_by(GEN_ALU,COD_GRADO) %>% summarise(N=n()) %>% 
  pivot_wider(names_from = GEN_ALU,values_from = N)  %>% adorn_totals("col") %>% 
  mutate_at(vars(Mujer,Hombre,Total),~format(.,big.mark="."))

#-----------------------------------#

# Tabla 32 
tab32 <- data.frame(Género=rep(c("Hombre", "Mujer"), each=20),
                    Años=rep(c("2002","2003","2004","2005","2006","2007","2008","2009","2010","2011", "2012", "2013",
                               "2014","2015","2016","2017","2018","2019","2020","2021"),2),
                    porcentaje=c(94.8,93.7,93.8,93.5,93.0,93.2,93.0,93.7,93.3,93.3,93.5,93.9,94.4,94.7,95.1,95.5,95.8,96.5,98.6,97.7,
                                 96.6,96.1,96.1,96.0,95.7,95.8,95.6,95.9,95.7,95.6,95.7,96.0,96.4,96.6,96.8,97.1,97.3,97.8,98.9,98.2))

# Gráfico

ggplot(data=tab32, aes(x=Años, y=porcentaje, group=Género,color=Género)) +
  geom_line()+geom_point()+ scale_fill_brewer(palette="Paired")+ ylab("")+
  theme(axis.text.y = element_blank(),legend.position = "top")+geom_text_repel(aes(label=paste0(porcentaje,"%")))+
  ggtitle("Gráfico 32. Tasa de aprobación estudiantes de enseñanza básica,
          según sexo, niños y jóvenes,período 2002-2021")

#-----------------------------------#

# Tabla 33
tab33_1<- rend2021 %>% filter(COD_ENSE %in% c(310,410,510,610,710,810,910)) %>% 
  filter(SIT_FIN_R=="P") %>% group_by(GEN_ALU,COD_GRADO) %>% summarise(N=n())
tab33_1<-cbind(tab33_1,total=total_2$N)
tab33_1<-tab33_1 %>% mutate(tasa=round(N/total*100,digits = 1)) %>% 
  select(GEN_ALU,COD_GRADO,tasa)

# Gráfico

ggplot(data=tab33_1, aes(x=COD_GRADO, y=tasa, fill=GEN_ALU)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
  coord_cartesian(ylim = c(90,100))+ylab("Tasa")+xlab("Grado")+
  theme(axis.text.y= element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
  scale_x_continuous(breaks=c(1:4), labels=c("1°","2°","3°","4°"),limits=c(0,5))

# Recuento

tab33_1_1<- rend2021 %>% filter(COD_ENSE %in% c(310,410,510,610,710,810,910)) %>% 
  filter(SIT_FIN_R=="P") %>% group_by(GEN_ALU,COD_GRADO) %>% summarise(N=n()) %>% 
  pivot_wider(names_from = GEN_ALU,values_from = N)  %>% adorn_totals("col") %>% 
  mutate_at(vars(Mujer,Hombre,Total),~format(.,big.mark="."))


# Tabla 33_2
tab33_2<- rend2021 %>% filter(COD_ENSE %in% c(310,410,510,610,710,810,910))  %>% 
  filter(SIT_FIN_R=="R") %>% group_by(GEN_ALU,COD_GRADO) %>% summarise(N=n())
tab33_2<-cbind(tab33_2,total=total_2$N)
tab33_2<-tab33_2 %>% mutate(tasa=round(N/total*100,digits = 1)) %>% 
  select(GEN_ALU,COD_GRADO,tasa)


# Gráfico

ggplot(data=tab33_2, aes(x=COD_GRADO, y=tasa, fill=GEN_ALU)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
  coord_cartesian(ylim = c(0,7))+ylab("Tasa")+xlab("Grado")+
  theme(axis.text.y= element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
  scale_x_continuous(breaks=c(1:4), labels=c("1°","2°","3°","4°"),limits=c(0,5))


#-----------------------------------#

# Tabla 33_3
tab33_3<-rend2021 %>% filter(COD_ENSE %in% c(310,410,510,610,710,810,910)) %>% 
  filter(SIT_FIN_R=="Y") %>% group_by(GEN_ALU,COD_GRADO) %>% summarise(N=n())
tab33_3<-cbind(tab33_3,total=total_2$N)
tab33_3<-tab33_3 %>% mutate(tasa=round(N/total*100,digits = 1)) %>% 
  select(GEN_ALU,COD_GRADO,tasa)

# Gráfico

ggplot(data=tab33_3, aes(x=COD_GRADO, y=tasa, fill=GEN_ALU)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
  coord_cartesian(ylim = c(0,4))+ylab("Tasa")+xlab("Grado")+
  theme(axis.text.y= element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
  scale_x_continuous(breaks=c(1:4), labels=c("1°","2°","3°","4°"),limits=c(0,5))


#-----------------------------------#

# Tabla 34

tab34 <- data.frame(Género=rep(c("Hombre", "Mujer"), each=20),
                 Años=rep(c("2002","2003","2004","2005","2006","2007","2008","2009","2010","2011", "2012", "2013",
                            "2014","2015","2016","2017","2018","2019","2020","2021"),2),
                 porcentaje=c(87.9,86.5,85.7,84.9,83.9,84.5,84.7,86.7,86.7,82.5,86.2,86.2,88,88.4,89.2,93.3,91.7,93.1,96.6,94.3,
                              91.4,90.4,89.7,89.2,88.6,88.6,88.8,89.9,89.7,86.2,89.1,89.1,91.2,91.6,92.2,95.7,94.2,95.3,97.8,96.6))

# Gráfico

ggplot(data=tab34, aes(x=Años, y=porcentaje, group=Género,color=Género)) +
  geom_line()+geom_point()+ scale_fill_brewer(palette="Paired")+ ylab("")+
  theme(axis.text.y = element_blank(),legend.position = "top")+geom_text_repel(aes(label=paste0(porcentaje,"%")))+
  ggtitle("Gráfico 34. wa,período 2002-2021")

## Consider increasing max.overlaps ?

#-----------------------------------#

# Tabla 35 

tab35 <- data.frame(Género=rep(c("Hombre", "Mujer"), each=4),
                 Años=rep(c("2018", "2019", "2020","2021"),2),
                 porcentaje=c(79.6,81,90.3,89.7,83.2,84.2,91.7,92.1))
# Gráfico
ggplot(data=tab35, aes(x=Años, y=porcentaje, group=Género,color=Género)) +
  geom_line()+geom_point()+ scale_fill_brewer(palette="Paired")+ ylab("")+
  theme(axis.text.y = element_blank(),legend.position = "top")+geom_text_repel(aes(label=paste0(porcentaje,"%")))+
  ggtitle("Gráfico 35. Alumnos extranjeros aprobados según sexo, período 2018-2021")



#-----------------------------------#

# Tabla 35_1_1
tab35_1<- rend2021 %>% filter(SIT_FIN_R== "P",COD_NAC_ALU=="E") %>% 
  group_by(GEN_ALU,COD_DEPE2) %>% summarise(N=n())# %>% 
#  pivot_wider(names_from = GEN_ALU,values_from = N) %>% adorn_totals("col")
tab35_1<-cbind(tab35_1,total=total_3$N)
tab35_1<-tab35_1 %>% mutate(tasa=round(N/total*100,digits = 1)) %>% 
  select(GEN_ALU,COD_DEPE2,tasa)

# Gráfico (TODAS LAS ESCALAS TIENEN QUE SER IGUALES, LAS DE 100%)

ggplot(data=tab35_1, aes(x=COD_DEPE2, y=tasa, fill=GEN_ALU)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
  coord_cartesian(ylim = c(80,100))+ylab("Tasa")+xlab("Grado")+
  theme(axis.text.y= element_blank(),legend.position = "top",axis.text.x = element_text(angle = 90))+
  geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
  scale_x_continuous(breaks=c(1:5), labels=c("Municipal","Part. Sub.",
  "Part. Pagado","Corp. Adm. Delegada","Servicio Local de Educ."),limits=c(0,7))+
  ggtitle("Gráfico 35_1. Tasa de aprobación de alumnos extranjeros 
          según dependencia administrativa y sexo, año 2021")



#-----------------------------------#

# Tabla 35_1_2
tab35_1_2<- rend2021 %>% filter(SIT_FIN_R=="P", COD_NAC_ALU=="E") %>% 
  group_by(GEN_ALU,COD_REG_RBD) %>% summarise(N=n())
tab35_1_2<-cbind(tab35_1_2,total=total_4$N)
tab35_1_2<-tab35_1_2 %>% mutate(tasa=round(N/total*100,digits = 1)) %>% 
  select(GEN_ALU,COD_REG_RBD,tasa)# %>% 
  #mutate(COD_REG_RBD=dplyr::recode(COD_REG_RBD,"1"= "Región de Tarapacá","2"="Región de Antofagasta",
  #"3"="Región de Atacama","4"="Región de Coquimbo","5"="Región de Valparaíso","6"="Región del Libertador Gral. Bernardo O´higgins",
  #"7"="Región del Maule","8"="Región del BioBío","9"="Región de la Araucanía","10"="Región de Los Lagos",
  #"11"="Región de Aysén del Gral. Carlos Ibáñez del Campo","12"="Región de Magallanes y de la Antártica Chilena","13"="Región Metropolitana de Santiago",
  #"14"="Región de Los Ríos","15"="Región de Arica y Parinacota","16"="Región de Ñuble"))

# Gráfico (x axis no esta en orden)

ggplot(data=tab35_1_2, aes(x=COD_REG_RBD,y=tasa, fill=GEN_ALU)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
  coord_cartesian(ylim = c(60,100))+ylab("Tasa")+xlab("Grado")+
  theme(axis.text.y= element_blank(),legend.position = "top",axis.text.x = element_text(angle = 90))+
  geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
  scale_x_continuous(breaks=c(1:16),limits=c(0,17))+
  ggtitle("Gráfico 35_1_2. Tasa de aprobación de alumnos
          extranjeros según región y sexo, año 2021")

#labels = c("Región de Arica y Parinacota","Región de Tarapacá",
#"Región de Antofagasta","Región de Atacama","Región de Coquimbo","Región de Valparaíso",
#"Región Metropolitana de Santiago","Región de O´higgins","Región del Maule","Región de Ñuble",
#"Región del BioBío","Región de la Araucanía","Región de Los Ríos","Región de Los Lagos",
#"Región de Aysén","Región de Magallanes")
#levels= c(15,1,2,3,4,5,13,6,7,16,8,9,14,10,11,12)

#-----------------------------------#

# Tabla estudiantes extranjeros aprobados por region y dependencia adm. Tabla 7
# faltan detalles, en kbl: agrupar por region, y cambiar columna codepe a fila

tabla7<- rend2021 %>% filter(SIT_FIN_R=="P",COD_NAC_ALU=="E") %>% 
  group_by(COD_REG_RBD,COD_DEPE2,GEN_ALU) %>% summarise(N=n()) %>% 
  pivot_wider(names_from = GEN_ALU,values_from = N) %>% adorn_totals("col") %>% 
  mutate(COD_REG_RBD=dplyr::recode(COD_REG_RBD,"1"= "Región de Tarapacá","2"="Región de Antofagasta","3"="Región de Atacama","4"="Región de Coquimbo",
                                   "5"="Región de Valparaíso","6"="Región del Libertador Gral. Bernardo O´higgins","7"="Región del Maule",
                                   "8"="Región del BioBío","9"="Región de la Araucanía","10"="Región de Los Lagos",
                                   "11"="Región de Aysén del Gral. Carlos Ibáñez del Campo","12"="Región de Magallanes y de la Antártica Chilena","13"="Región Metropolitana de Santiago",
                                   "14"="Región de Los Ríos","15"="Región de Arica y Parinacota","16"="Región de Ñuble")) %>% 
  mutate(COD_DEPE2=dplyr::recode(COD_DEPE2,"1"="Municipal","2"="Particular Subvencionado","3"="Particular Pagado",
                                 "4"="Administración Delegada","5"="Servicio Local")) %>% 
  mutate_at(vars(Hombre,Mujer,Total),~format(.,big.mark="."))

# Tab estudiantes extranjeros aprobados por nivel enseñanza

tabla8<- rend2021 %>% filter(SIT_FIN_R=="P",COD_NAC_ALU=="E") %>% 
  group_by(COD_ENSE2,COD_GRADO,GEN_ALU) %>% summarise(N=n()) %>% 
  pivot_wider(names_from = GEN_ALU,values_from = N) %>% adorn_totals("col") %>% 
  mutate(COD_ENSE2=dplyr::recode(COD_ENSE2,"1"="Educación Parvularia","2"="Educación Básica niños","3"="Educación Básica Adultos",
                                 "4"="Educación Especial","5"="Enseñanza Media HCJóvenes","6"="Enseñanza Media HC Adultos",
                                 "7"="Enseñanza Media Tp Jóvenes","8"="Enseñanza Media Tp Adultos"))

#-----------------------------------#

# Tabla 35_1_3
tab35_1_3<- rend2021 %>% filter(SIT_FIN_R=="P",COD_NAC_ALU=="E",COD_ENSE2==2) %>% 
  group_by(GEN_ALU,COD_GRADO) %>% summarise(N=n())# %>% 
#  pivot_wider(names_from = GEN_ALU,values_from = N) %>% adorn_totals("col")
tab35_1_3<-cbind(tab35_1_3,total=total_5$N)
tab35_1_3<-tab35_1_3 %>% mutate(tasa=round(N/total*100,digits = 1)) %>% 
  select(COD_GRADO,GEN_ALU,tasa)

# Gráfico

ggplot(data=tab35_1_3, aes(x=COD_GRADO,y=tasa, fill=GEN_ALU)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
  coord_cartesian(ylim = c(60,100))+ylab("")+xlab("")+
  theme(axis.text.y= element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
  scale_x_continuous(breaks=c(1:8),limits=c(0,9))+
  ggtitle("Gráfico 35_1_3. Tasa de aprobación de alumnos extranjeros en enseñanza básica
          y sexo, año 2021")

 
#-----------------------------------#

# Tabla 35_1_4
tab35_1_4<- rend2021 %>% filter(SIT_FIN_R=="P",COD_NAC_ALU=="E",COD_ENSE2==5|COD_ENSE2==7) %>% 
  group_by(GEN_ALU,COD_GRADO) %>% summarise(N=n())
tab35_1_4<-cbind(tab35_1_4,total=total_6$N)
tab35_1_4<-tab35_1_4 %>% mutate(tasa=round(N/total*100,digits = 1)) %>% 
  select(COD_GRADO,GEN_ALU,tasa)

# Gráfico

ggplot(data=tab35_1_4, aes(x=COD_GRADO,y=tasa, fill=GEN_ALU)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
  coord_cartesian(ylim = c(60,100))+ylab("")+xlab("")+
  theme(axis.text.y= element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
  scale_x_continuous(breaks=c(1:4),limits=c(0,5))+
  ggtitle("Gráfico 35_1_4. Tasa de aprobación de alumnos extranjeros en enseñanza media
          y sexo, año 2021")


#-----------------------------------#

# Tabla 36_1(hay que buscar como puedo hacer una columna de la suma h y m)
tab36_1<- rend2021 %>% filter(COD_NAC_ALU=="E",SIT_FIN_R=="P") %>% 
  group_by(GEN_ALU,COD_ENSE2) %>% summarise(N=n()) %>% 
  pivot_wider(names_from = GEN_ALU,values_from = N) %>% adorn_totals("col")
tab36_1<-cbind(tab36_1,total=total_7$N)
tab36_1<-tab36_1 %>% mutate(tasa=round(N/total*100,digits = 1)) %>% 
  select(GEN_ALU,COD_ENSE2,tasa)

# Gráfico

ggplot(data=tab36_1, aes(x=COD_ENSE2,y=tasa, fill=GEN_ALU)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
  coord_cartesian(ylim = c(60,100))+ylab("")+xlab("")+
  theme(axis.text.y= element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
  scale_x_continuous(breaks=c(1:6),limits=c(0,7))+
  ggtitle("Gráfico 36_1. Tasa de aprobación de alumnos extranjeros en enseñanza media
          y sexo, año 2021")


#-----------------------------------#


# Tabla 36_2

tab36_2 <- data.frame(Género=rep(c("Hombre", "Mujer"), each=4),
                 Años=rep(c("2018", "2019", "2020","2021"),2),
                 porcentaje=c(46.5,48.5,52,50,40.8,40.5,41.9,40.7))

# Gráfico

ggplot(data=tab36_2, aes(x=Años, y=porcentaje, group=Género,color=Género)) +
  geom_line()+geom_point()+ scale_fill_brewer(palette="Paired")+ ylab("")+
  theme(axis.text.y = element_blank(),legend.position = "top")+geom_text_repel(aes(label=paste0(porcentaje,"%")))+
  ggtitle("Gráfico 36_2.Tasa de aprobación de alumnos extranjeros sobre el total
          de situación final de rendimiento,años 2018-2021")


#-----------------------------------#

# Tabla 36_3
tab36_3 <- data.frame(Género=rep(c("Hombre", "Mujer"), each=4),
                 Años=rep(c("2018", "2019", "2020","2021"),2),
                 porcentaje=c(47.2,47.7,49.4,48.6,46.3,46.8,47.8,47.3))

# Gráfico

ggplot(data=tab36_3, aes(x=Años, y=porcentaje, group=Género,color=Género)) +
  geom_line()+geom_point()+ scale_fill_brewer(palette="Paired")+ ylab("")+
  theme(axis.text.y = element_blank(),legend.position = "top")+geom_text_repel(aes(label=paste0(porcentaje,"%")))+
  ggtitle("Gráfico 36_3. Porcentaje de aprobación de alumnos nacionales
          sobre el total de situación final de rendimiento según sexo, años 2018-2021")


#-----------------------------------#

# Tabla matricula adultos

tabla9<- matriculamod %>% filter(ESTADO_ESTAB==1) %>%filter(COD_ENSE3== 3| COD_ENSE3== 5|COD_ENSE3==7) %>% 
  group_by(GEN_ALU) %>% summarise(Número=n()) %>% mutate(porcentaje=paste0(round(Número/sum(Número)*100,digits=1),"%")) %>% adorn_totals(paste0("row"),"100 %")


#-----------------------------------#

# Tabla 37
tab37<-matriculamod %>% filter(ESTADO_ESTAB==1) %>% 
  filter(ENS>=9) %>% group_by(ENS,GEN_ALU) %>% summarise(N=n())%>%
  mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>% 
  mutate(ENS=dplyr::recode(ENS,"9"="Básica Adultos","10"="Media HC Adultos ciclo general","11"="Media TP Adultos ciclo general",
                           "12"="Media HC Adultos ciclo diferenciado","13"= "Media TP Adultos ciclo diferenciado"))
# Gráfico
ggplot(tab37, aes(fill=GEN_ALU, y=fct_relevel(ENS,"Media TP Adultos ciclo diferenciado","Media HC Adultos ciclo diferenciado",
  "Media TP Adultos ciclo general","Media HC Adultos ciclo general","Básica Adultos"), x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 37. Estudiantes adultos por tipo y nivel de enseñanza según sexo, niños y jóvenes, año 2021")

# Tabla recuento
tab37_1<-matriculamod %>% filter(ESTADO_ESTAB==1 & GEN_ALU!="0") %>% 
  filter(ENS>=9) %>% group_by(ENS) %>% summarise(Total=n())%>% 
  mutate(ENS=dplyr::recode(ENS,"9"="Básica Adultos","10"="Media HC Adultos ciclo general","11"="Media TP Adultos ciclo general",
                           "12"="Media HC Adultos ciclo diferenciado","13"= "Media TP Adultos ciclo diferenciado")) %>% 
  adorn_totals("row") %>%
  mutate_at(vars(Total),~format(.,big.mark="."))

#-----------------------------------#

# Tabla 38
tab38<-matriculamod %>% filter(ESTADO_ESTAB==1) %>% 
  filter(ENS>=9) %>% group_by(GEN_ALU,ENS) %>% summarise(N=n())%>%
  mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>% 
  mutate(ENS=dplyr::recode(ENS,"9"="Básica Adultos","10"="Media HC Adultos ciclo general","11"="Media TP Adultos ciclo general",
                           "12"="Media HC Adultos ciclo diferenciado","13"= "Media TP Adultos ciclo diferenciado"))
# Gráfico

ggplot(tab38, aes(fill=fct_relevel(ENS,"Media TP Adultos ciclo diferenciado","Media HC Adultos ciclo diferenciado",
  "Media TP Adultos ciclo general","Media HC Adultos ciclo general"), y=GEN_ALU, x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Básica Adultos","Media HC Adultos ciclo general","Media TP Adultos ciclo general",
  "Media HC Adultos ciclo diferenciado","Media TP Adultos ciclo diferenciado")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 38. Estudiantes adultos por sexo según tipo y nivel de enseñanza, niños y jóvenes, año 2021")


# Tabla recuento

tab38_1<-matriculamod %>% filter(ESTADO_ESTAB==1 & GEN_ALU!="0") %>% 
  filter(ENS>=9) %>% group_by(GEN_ALU) %>% summarise(N=n())%>%
  pivot_wider(names_from =GEN_ALU ,values_from = N) %>%
  add_column(Sexo="Total",.before = "Hombre") %>% adorn_totals("col") %>% 
  mutate_at(vars(Hombre,Mujer,Total),~format(.,big.mark="."))

#-----------------------------------#

# Tabla 39 

tab39<-matriculamod %>% filter(ESTADO_ESTAB==1) %>% 
  filter(ENS==13 & COD_RAMA!=0) %>% group_by(COD_RAMA,GEN_ALU) %>% 
  summarise(N=n())%>% mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>%
  mutate(COD_RAMA=dplyr::recode(COD_RAMA,"400"= "Comercial","500"="Industrial","600"="Técnica","700"= "Agrícola",
                                "800"="Marítima"))
# Gráfico

ggplot(tab39, aes(fill=GEN_ALU, y=fct_relevel(COD_RAMA,"Marítima","Agrícola",
                      "Técnica","Industrial","Comercial"), x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 39. Estudiantes adultos TP ciclo diferenciado por rama según sexo, año 2021")

# Tabla recuento
tab39_1<-matriculamod %>% filter(ESTADO_ESTAB==1) %>% 
  filter(ENS==13 & COD_RAMA!=0) %>% group_by(COD_RAMA) %>% 
  summarise(Total=n())%>% mutate(COD_RAMA=dplyr::recode(COD_RAMA,"400"= "Comercial","500"="Industrial","600"="Técnica","700"= "Agrícola",
                                "800"="Marítima")) %>% adorn_totals("row") %>% 
  mutate_at(vars(Total),~format(.,big.mark="."))

#-----------------------------------#

# Tabla 40
tab40<-matriculamod %>% filter(ESTADO_ESTAB==1) %>% 
  filter(ENS==13 & COD_RAMA!=0) %>% group_by(GEN_ALU,COD_RAMA) %>% 
  summarise(N=n())%>% mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>%
  mutate(COD_RAMA=dplyr::recode(COD_RAMA,"400"= "Comercial","500"="Industrial","600"="Técnica","700"= "Agrícola",
                                "800"="Marítima"))

# Gráfico

ggplot(tab40, aes(fill=fct_relevel(COD_RAMA,"Marítima","Agrícola","Técnica","Industrial","Comercial"), y=GEN_ALU, x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Comercial","Industrial","Técnica","Agrícola","Marítima")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text_repel(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 40. Estudiantes adultos TP ciclo diferenciado  por sexo según rama, año 2021")

# Tabla recuento

tab40_1<-matriculamod %>% filter(ESTADO_ESTAB==1) %>% 
  filter(ENS==13 & COD_RAMA!=0) %>% group_by(GEN_ALU) %>% 
  summarise(N=n()) %>% pivot_wider(names_from =GEN_ALU ,values_from = N) %>%
  add_column(Sexo="Total",.before = "Hombre") %>% adorn_totals("col") %>% 
  mutate_at(vars(Hombre,Mujer,Total),~format(.,big.mark="."))

#-----------------------------------#

# Tabla 41
tab41<-matriculamod %>% filter(ESTADO_ESTAB==1) %>% 
  filter(ENS==13 & COD_SEC) %>% group_by(COD_SEC,GEN_ALU) %>% 
  summarise(N=n())%>%mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>%
  mutate(COD_SEC=dplyr::recode(COD_SEC,"410"="Administración y Comercio","510"="Construcción","520"="Metalmecánico","530"="Electricidad",
                               "550"="Gráfica","570"="Confección","610"= "Alimentación","620"="Programas y proyectos sociales",
                               "630"="Hotelería y Turismo","710"="Maderero","810"="Marítimo"))
# Gráfico
ggplot(tab41, aes(fill=GEN_ALU, y=fct_relevel(COD_SEC,"Marítimo","Maderero",
  "Hotelería y Turismo","Programas y proyectos sociales","Alimentación","Confección","Gráfica","Electricidad","Metalmecánico","Construcción","Administración y Comercio"), x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 41. Estudiantes adultos TP ciclo diferenciado  por sector económico según sexo, año 2021")

# Tabla recuento

tab41_1<-matriculamod %>% filter(ESTADO_ESTAB==1) %>% 
  filter(ENS==13 & COD_SEC) %>% group_by(COD_SEC) %>% 
  summarise(Total=n())%>% mutate(COD_SEC=dplyr::recode(COD_SEC,"410"="Administración y Comercio","510"="Construcción","520"="Metalmecánico","530"="Electricidad",
                               "550"="Gráfica","570"="Confección","610"= "Alimentación","620"="Programas y proyectos sociales",
                               "630"="Hotelería y Turismo","710"="Maderero","810"="Marítimo")) %>% adorn_totals("row") %>% 
  mutate_at(vars(Total),~format(.,big.mark="."))

#-----------------------------------#

# Tabla 42 
tab42<-matriculamod %>% filter(ESTADO_ESTAB==1) %>% 
  filter(COD_ENSE3== 3| COD_ENSE3== 5|COD_ENSE3==7) %>% group_by(COD_DEPE2,GEN_ALU) %>% 
  summarise(N=n()) %>% mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>%
  mutate(COD_DEPE2=dplyr::recode(COD_DEPE2,"1"="Municipal","2"="Particular Subvencionado","3"="Particular Pagado",
                                 "4"="Administración Delegada","5"="Servicio Local"))

# Gráfico
ggplot(tab42, aes(fill=GEN_ALU, y=fct_relevel(COD_DEPE2,"Servicio Local","Administración Delegada",
  "Particular Pagado","Particular Subvencionado","Municipal"),x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Mujer","Hombre")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 42. Estudiantes adultos por tipo de dependencia según sexo, año 2021")

# Tabla recuento
tab42_1<-matriculamod %>% filter(ESTADO_ESTAB==1) %>% 
  filter(COD_ENSE3== 3| COD_ENSE3== 5|COD_ENSE3==7) %>% group_by(COD_DEPE2) %>% 
  summarise(N=n()) %>% mutate(COD_DEPE2=dplyr::recode(COD_DEPE2,"1"="Municipal","2"="Particular Subvencionado","3"="Particular Pagado",
                                 "4"="Administración Delegada","5"="Servicio Local")) %>% 
  pivot_wider(names_from =COD_DEPE2,values_from = N) %>%
  add_column(Dependencia_administrativa="Total",.before = "Municipal") %>% adorn_totals("col") %>% 
  mutate_at(vars(Municipal,"Particular Subvencionado", "Particular Pagado","Administración Delegada","Servicio Local"),~format(.,big.mark="."))

#-----------------------------------#

# Tabla 43
tab43<-matriculamod %>% filter(ESTADO_ESTAB==1) %>% 
  filter(COD_ENSE3== 3| COD_ENSE3== 5|COD_ENSE3==7) %>% group_by(GEN_ALU,COD_DEPE2) %>% 
  summarise(N=n()) %>% mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N) %>%
  mutate(COD_DEPE2=dplyr::recode(COD_DEPE2,"1"="Municipal","2"="Particular Subvencionado","3"="Particular Pagado",
                                 "4"="Administración Delegada","5"="Servicio Local"))
# Gráfico
ggplot(tab43, aes(fill=fct_relevel(COD_DEPE2,"Servicio Local","Administración Delegada","Particular Pagado",
  "Particular Subvencionado","Municipal"), y=GEN_ALU, x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y") +
  scale_fill_brewer(breaks=c("Municipal","Particular Subvencionado","Particular Pagado","Administración Delegada",
  "Servicio Local")) + guides(fill =guide_legend(title="")) +
  xlab("")+ylab("")+ theme(axis.text.x = element_blank(),legend.position = "top")+
  geom_text_repel(aes(label=paste0(porcentaje,"%")), position = position_fill(vjust = 0.7))+
  ggtitle("Gráfico 43. Estudiantes adultos por sexo según tipo de dependencia, año 2021")

# Tabla recuento
tab43_1<-matriculamod %>% filter(ESTADO_ESTAB==1) %>% 
  filter(COD_ENSE3== 3| COD_ENSE3== 5|COD_ENSE3==7) %>% group_by(GEN_ALU) %>% 
  summarise(N=n()) %>% pivot_wider(names_from =GEN_ALU ,values_from = N) %>%
  add_column(Sexo="Total",.before = "Hombre") %>% adorn_totals("col") %>% 
  mutate_at(vars(Hombre,Mujer,Total),~format(.,big.mark="."))


#-----------------------------------#
#-----------------------------------#

# Rendimiento de adultos

#-----------------------------------#
#-----------------------------------#

# Tabla 44
tab44<- rend2021 %>% filter(COD_ENSE2==3,SIT_FIN_R=="P")%>% group_by(GEN_ALU,COD_GRADO) %>% summarise(N=n())
tab44<-cbind(tab44,total=total_8$N)
tab44<-tab44 %>% mutate(tasa=round(N/total*100,digits = 1)) %>% 
  select(GEN_ALU,COD_GRADO,tasa)

# Gráfico

ggplot(data=tab44, aes(x=COD_GRADO,y=tasa, fill=GEN_ALU)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
  coord_cartesian(ylim = c(40,80))+ylab("")+xlab("")+
  theme(axis.text.y= element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
  scale_x_continuous(breaks=c(1:3),limits=c(0,4),labels = c("Primer ciclo","Segundo ciclo","Tercer ciclo"))+
  ggtitle("Gráfico 44. Tasa de aprobación estudiantes de enseñanza básica por ciclo según sexo, año 2021")

# Tabla recuento
tab44_1<- rend2021 %>% filter(COD_ENSE2==3,SIT_FIN_R=="P")%>% group_by(COD_GRADO) %>% summarise(N=n()) %>%
  mutate(COD_GRADO=dplyr::recode(COD_GRADO,"1"="Primer ciclo","2"="Segundo ciclo","3"="Tercer ciclo")) %>% 
  pivot_wider(names_from = COD_GRADO,values_from = N)%>% add_column(Ciclo="Total",.before = "Primer ciclo") %>%
  adorn_totals("col") %>% mutate_at(vars("Primer ciclo","Segundo ciclo","Tercer ciclo",Total),~format(.,big.mark="."))

#-----------------------------------#

# Tabla 45
tab45<- rend2021 %>% filter(COD_ENSE2==3, SIT_FIN_R=="R") %>% group_by(GEN_ALU,COD_GRADO) %>% summarise(N=n())
tab45<-cbind(tab45,total=total_8$N)
tab45<-tab45 %>% mutate(tasa=round(N/total*100,digits = 1)) %>% select(GEN_ALU,COD_GRADO,tasa)

# Gráfico

ggplot(data=tab45, aes(x=COD_GRADO,y=tasa, fill=GEN_ALU)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
  coord_cartesian(ylim = c(10,40))+ylab("")+xlab("")+
  theme(axis.text.y= element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
  scale_x_continuous(breaks=c(1:3),limits=c(0,4),labels = c("Primer ciclo","Segundo ciclo","Tercer ciclo"))+
  ggtitle("Gráfico 45. Tasa de reprobación estudiantes de enseñanza básica por ciclo según sexo, año 2021")


# Tabla recuento
tab45_1<- rend2021 %>% filter(COD_ENSE2==3, SIT_FIN_R=="R") %>% group_by(COD_GRADO) %>% summarise(N=n()) %>%
  mutate(COD_GRADO=dplyr::recode(COD_GRADO,"1"="Primer ciclo","2"="Segundo ciclo","3"="Tercer ciclo")) %>% 
  pivot_wider(names_from = COD_GRADO,values_from = N)%>% add_column(Ciclo="Total",.before = "Primer ciclo") %>%
  adorn_totals("col") %>% mutate_at(vars("Tercer ciclo",Total),~format(.,big.mark="."))


#-----------------------------------#

# Tabla 46
tab46<- rend2021 %>% filter(COD_ENSE2==3,SIT_FIN_R=="Y") %>% group_by(GEN_ALU,COD_GRADO) %>% summarise(N=n())
tab46<-cbind(tab46,total=total_8$N)
tab46<-tab46 %>% mutate(tasa=round(N/total*100,digits = 1)) %>% 
  select(GEN_ALU,COD_GRADO,tasa) 

# Gráfico

ggplot(data=tab46, aes(x=COD_GRADO,y=tasa, fill=GEN_ALU)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
  coord_cartesian(ylim = c(10,40))+ylab("")+xlab("")+
  theme(axis.text.y= element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
  scale_x_continuous(breaks=c(1:3),limits=c(0,4),labels = c("Primer ciclo","Segundo ciclo","Tercer ciclo"))+
  ggtitle("Gráfico 46. Tasa de abandono estudiantes de enseñanza básica por ciclo según sexo,adultos, año 2021")


# Tabla recuento
tab46_1<- rend2021 %>% filter(COD_ENSE2==3, SIT_FIN_R=="Y") %>% group_by(COD_GRADO) %>% summarise(N=n()) %>%
  mutate(COD_GRADO=dplyr::recode(COD_GRADO,"1"="Primer ciclo","2"="Segundo ciclo","3"="Tercer ciclo")) %>% 
  pivot_wider(names_from = COD_GRADO,values_from = N)%>% add_column(Ciclo="Total",.before = "Primer ciclo") %>%
  adorn_totals("col") %>% mutate_at(vars("Tercer ciclo",Total),~format(.,big.mark="."))

#-----------------------------------#

# Tabla 47
tab47<- rend2021 %>% filter(COD_ENSE2==6|COD_ENSE2==8,SIT_FIN_R=="P")%>% group_by(GEN_ALU,COD_ENSE2) %>% summarise(N=n())
tab47<-cbind(tab47,total=total_9$N)
tab47<-tab47 %>% mutate(tasa=round(N/total*100,digits = 1)) %>% 
  select(GEN_ALU,COD_ENSE2,tasa) 

# Gráfico

ggplot(data=tab47, aes(x=COD_ENSE2,y=tasa, fill=GEN_ALU)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
  coord_cartesian(ylim = c(50,80))+ylab("")+xlab("")+
  theme(axis.text.y= element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
  scale_x_continuous(breaks=c(1:2),labels = c("Media HC Adulto","Media TP Adulto"))+
  ggtitle("Gráfico 47. Tasa de aprobación estudiantes de enseñanza media según sexo,adultos, año 2021")


# Tabla recuento

tab47_1<- rend2021 %>% filter(COD_ENSE2==6|COD_ENSE2==8,SIT_FIN_R=="P")%>% group_by(COD_ENSE2) %>% summarise(N=n()) %>%
  mutate(COD_ENSE2=dplyr::recode(COD_ENSE2,"6"="Media HC Adulto","8"="Media TP Adulto"))%>%
  pivot_wider(names_from = COD_ENSE2,values_from = N) %>%add_column(Tipo_enseñanza="Total",.before = "Media HC Adulto") %>% 
  adorn_totals("col")

#-----------------------------------#

# Tabla 48
tab48<- rend2021 %>% filter(COD_ENSE2==6|COD_ENSE2==8,SIT_FIN_R=="R")%>% group_by(GEN_ALU,COD_ENSE2) %>% summarise(N=n())
tab48<-cbind(tab48,total=total_9$N)
tab48<-tab48 %>% mutate(tasa=round(N/total*100,digits = 1)) %>% 
  select(GEN_ALU,COD_ENSE2,tasa)

# Gráfico

ggplot(data=tab48, aes(x=COD_ENSE2,y=tasa, fill=GEN_ALU)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
  coord_cartesian(ylim = c(0,40))+ylab("Tasa")+xlab("Grado")+
  theme(axis.text.y= element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
  scale_x_continuous(breaks=c(1:2),labels = c("Media HC Adulto","Media TP Adulto"))+
  ggtitle("Gráfico 48. Tasa de reprobación estudiantes de enseñanza media según sexo, adultos, año 2021")


# Tabla recuento
tab48_1<- rend2021 %>% filter(COD_ENSE2==6|COD_ENSE2==8,SIT_FIN_R=="R")%>% group_by(COD_ENSE2) %>% summarise(N=n()) %>%
  mutate(COD_ENSE2=dplyr::recode(COD_ENSE2,"6"="Media HC Adulto","8"="Media TP Adulto")) %>% pivot_wider(names_from = COD_ENSE2,values_from = N) %>%
  add_column(Tipo_enseñanza="Total",.before = "Media HC Adulto") %>%adorn_totals("col") %>% 
  mutate_at(vars(Total,"Media HC Adulto","Media TP Adulto"),~format(.,big.mark="."))


#-----------------------------------#

# Tabla 49
tab49<- rend2021 %>% filter(COD_ENSE2==6|COD_ENSE2==8,SIT_FIN_R=="Y") %>% group_by(GEN_ALU,COD_ENSE2) %>% summarise(N=n())
tab49<-cbind(tab49,total=total_9$N)
tab49<-tab49 %>% mutate(tasa=round(N/total*100,digits = 1)) %>% 
  select(GEN_ALU,COD_ENSE2,tasa) 

# Gráfico

ggplot(data=tab49, aes(x=COD_ENSE2,y=tasa, fill=GEN_ALU)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
  coord_cartesian(ylim = c(10,30))+ylab("")+xlab("")+
  theme(axis.text.y= element_blank(),legend.position = "top")+
  geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
  scale_x_continuous(breaks=c(1:2),labels = c("Media HC Adulto","Media TP Adulto"))+
  ggtitle("Gráfico 49. Tasa de abandono estudiantes de enseñanza media según sexo, adultos, año 2021")


# Tabla recuento
tab49_1<- rend2021 %>% filter(COD_ENSE2==6|COD_ENSE2==8,SIT_FIN_R=="Y")%>% group_by(COD_ENSE2) %>% summarise(N=n()) %>%
  mutate(COD_ENSE2=dplyr::recode(COD_ENSE2,"6"="Media HC Adulto","8"="Media TP Adulto")) %>% pivot_wider(names_from = COD_ENSE2,values_from = N) %>%
  add_column(Tipo_enseñanza="Total",.before = "Media HC Adulto") %>%adorn_totals("col")

##---------------------------------------------------------------##
##---------------------------------------------------------------##

tab51<-matriculamod %>% filter(COD_RAMA==400,ENS==8) %>% group_by(GEN_ALU,COD_SEC) %>% 
  summarise(N=n())

tab51_1<-matriculamod %>% filter(COD_RAMA==500,ENS==8) %>% group_by(GEN_ALU,COD_SEC) %>% 
  summarise(N=n())

# Estos creo que son pero no tengo como verificar

##---------------------------------------------------------------##
##---------------------------------------------------------------##

# Matricula ed superior egresados media
# Porcentaje matricula media TP
# Matrícula media tp 
# POR RAMA

#tab52_1<-matriculamod %>% filter(COD_RAMA>0,ENS==8,GEN_ALU=="Mujer") %>% group_by(COD_RAMA) %>% 
  summarise(N=n())

b<-matriculamod %>% filter(COD_RAMA>=0,ENS==8) %>% group_by(COD_RAMA,GEN_ALU) %>% 
  summarise(N=n()) %>% pivot_wider(names_from = GEN_ALU,values_from = N) %>% 
  adorn_totals(c("col","row")) %>% mutate_at(vars(Hombre,Mujer),~(round(./Total*100,digits = 1))) %>% 
  mutate(Año=rep("2021"))

bmujer<-b %>% select(Año,COD_RAMA,Mujer)

mujeresaños<- data.frame(Año=rep(c("2019","2020"),each= 7),
                          COD_RAMA=rep(c("400","500","600","700",
                                     "800","900","Total"),2),
                          Mujer=(c(60.1,19.5,78.0,37.1,35.6,53.2,47.1,58.9,19.8,76.7,38.3,34.5,53.5,46.5)))

tabb52_1<- rbind(mujeresaños,bmujer)

ggplot(tabb52_1,aes(COD_RAMA,Mujer))+
  geom_bar(stat = "identity",position = position_dodge())+
  facet_wrap(~Año)



ggplot(data=tabb52_1, aes(x=COD_RAMA,y=Mujer, fill=Año)) +
  geom_bar(stat="identity", position=position_dodge(),width = 0.55)#+
  #scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
  #coord_cartesian(ylim = c(10,30))+ylab("")+xlab("")+
  #theme(axis.text.y= element_blank(),legend.position = "top",plot.title = element_text(hjust = 0.5))+
  #geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
  #scale_x_continuous(breaks=c(1:2),labels = c("Media HC Adulto","Media TP Adulto"))+
  #ggtitle("Gráfico 49. Tasa de abandono \nestudiantes de enseñanza media según \nsexo, adultos, año 2021")

bhombre<-b %>% select(Año,COD_RAMA,Hombre)

hombresaños<- data.frame(Año=rep(c("2019","2020"),each= 7),
                             COD_RAMA=rep(c("400","500","600","700",
                                            "800","900","Total"),2),
                             Hombre=(c(39.9,80.5,22.0,62.9,64.4,46.8,52.9,41.1,80.2,23.3,61.7,65.5,46.5,53.5)))

tabb52_2<- rbind(hombresaños,bhombre)

ggplot(data=tabb52_2, aes(x=COD_RAMA,y=Hombre, fill=Año)) +
  geom_bar(stat="identity", position=position_dodge(),width = 0.55)

# Estos porcentajes se sacan dividiendo n/total(hombresymujer)*100
#-------------------------------------------------#

# Matrícula media por sector economico. Administracion y comercio

tab53_1<-matriculamod %>% filter(COD_SEC==410,ENS==8,GEN_ALU=="Mujer") %>% group_by(COD_RAMA) %>% 
  summarise(N=n()) 

tab53_2<-matriculamod %>% filter(COD_SEC==410,ENS==8,GEN_ALU=="Hombre") %>% group_by(COD_SEC) %>% 
  summarise(N=n()) 


#----------------------------------------------#

# Matrícula media tp según sector economico, Industrial 

tab53_3<-matriculamod %>% filter(COD_RAMA==500,ENS==8,GEN_ALU=="Mujer")%>%
  group_by(COD_SEC) %>% summarise(N=n())

tab53_4<-matriculamod %>% filter(COD_RAMA==500,ENS==8,GEN_ALU=="Hombre")%>%
  group_by(COD_SEC) %>% summarise(N=n())

b2<-matriculamod %>% filter(COD_RAMA==500,ENS==8) %>% group_by(COD_SEC,GEN_ALU) %>% 
  summarise(N=n()) %>% pivot_wider(names_from = GEN_ALU,values_from = N) %>% 
  adorn_totals(c("col","row")) %>% mutate_at(vars(Hombre,Mujer),~(round(./Total*100,digits = 1))) %>% 
  mutate(Año=rep("2021"))

b2mujer<-b2 %>% select(Año,COD_SEC,Mujer)

mujeresañosb2<- data.frame(Año=rep(c("2019","2020"),each= 9),
                         COD_SEC=rep(c("510","520","530","540",
                                        "550","560","570","580","Total"),2),
                         Mujer=(c(23.4,10.1,12.6,39.6,44.1,64.0,91.9,23.7,19.5,24.2,10.7,13.3,40.8,42.9,63.7,89.5,24.0,19.8)))

tabb53_3<- rbind(mujeresañosb2,b2mujer)

ggplot(data=tabb53_3, aes(x=COD_SEC,y=Mujer, fill=Año)) +
  geom_bar(stat="identity", position=position_dodge(),width = 0.55)#+
#scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
#coord_cartesian(ylim = c(10,30))+ylab("")+xlab("")+
#theme(axis.text.y= element_blank(),legend.position = "top",plot.title = element_text(hjust = 0.5))+
#geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
#scale_x_continuous(breaks=c(1:2),labels = c("Media HC Adulto","Media TP Adulto"))+
#ggtitle("Gráfico 49. Tasa de abandono \nestudiantes de enseñanza media según \nsexo, adultos, año 2021")

b2hombre<-b2 %>% select(Año,COD_SEC,Hombre)

hombresañosb2<- data.frame(Año=rep(c("2019","2020"),each= 9),
                         COD_SEC=rep(c("510","520","530","540",
                                        "550","560","570","580","Total"),2),
                         Hombre=(c(76.6,89.9,87.4,60.4,55.9,36.0,8.1,76.3,80.5,75.8,89.3,86.7,59.2,57.1,36.3,10.5,76.0,80.2)))

tabb53_4<- rbind(hombresañosb2,b2hombre)

ggplot(data=tabb53_4, aes(x=COD_SEC,y=Hombre, fill=Año)) +
  geom_bar(stat="identity", position=position_dodge(),width = 0.55)

# Matrícula media tp según sector económico, técnica. No aparece
# programas y proyectos sociales pk es 0%

tab53_5<-matriculamod %>% filter(COD_RAMA==600,ENS==8,GEN_ALU=="Mujer") %>% 
  group_by(COD_SEC) %>% summarise(N=n())# programasyproyecto sociales tiene n=0 en H y M

tab53_6<-matriculamod %>% filter(COD_RAMA==600,ENS==8,GEN_ALU=="Hombre") %>% 
  group_by(COD_SEC) %>% summarise(N=n())

b3<-matriculamod %>% filter(COD_RAMA==600,ENS==8) %>% group_by(COD_SEC,GEN_ALU) %>% 
  summarise(N=n()) %>% pivot_wider(names_from = GEN_ALU,values_from = N) %>% 
  adorn_totals(c("col","row")) %>% mutate_at(vars(Hombre,Mujer),~(round(./Total*100,digits = 1)))%>% 
  mutate(Año=rep("2021"))

b3mujer<-b3 %>% select(Año,COD_SEC,Mujer)

mujeresañosb3<- data.frame(Año=rep(c("2019","2020"),each= 4),
                           COD_SEC=rep(c("610","630","640","Total"),2),
                           Mujer=(c(65.3,64.5,90.6,78.0,63.3,63.8,89.8,76.7)))

tabb53_5<- rbind(mujeresañosb3,b3mujer)

ggplot(data=tabb53_5, aes(x=COD_SEC,y=Mujer, fill=Año)) +
  geom_bar(stat="identity", position=position_dodge(),width = 0.55)#+
#scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
#coord_cartesian(ylim = c(10,30))+ylab("")+xlab("")+
#theme(axis.text.y= element_blank(),legend.position = "top",plot.title = element_text(hjust = 0.5))+
#geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
#scale_x_continuous(breaks=c(1:2),labels = c("Media HC Adulto","Media TP Adulto"))+
#ggtitle("Gráfico 49. Tasa de abandono \nestudiantes de enseñanza media según \nsexo, adultos, año 2021")

b3hombre<-b3 %>% select(Año,COD_SEC,Hombre)

hombresañosb3<- data.frame(Año=rep(c("2019","2020"),each= 4),
                           COD_SEC=rep(c("610","630","640","Total"),2),
                           Hombre=(c(34.7,35.5,9.4,22.0,36.7,36.2,10.2,23.3)))

tabb53_6<- rbind(hombresañosb3,b3hombre)

ggplot(data=tabb53_6, aes(x=COD_SEC,y=Hombre, fill=Año)) +
  geom_bar(stat="identity", position=position_dodge(),width = 0.55)

# Matrícula media tp según sector económico, agrícola

tab53_7<-matriculamod %>% filter(COD_RAMA==700,ENS==8,GEN_ALU=="Mujer") %>% 
  group_by(COD_SEC) %>% summarise(N=n())# programasyproyecto sociales tiene n=0 en H y M

tab53_8<-matriculamod %>% filter(COD_RAMA==700,ENS==8,GEN_ALU=="Hombre") %>% 
  group_by(COD_SEC) %>% summarise(N=n())

b4<-matriculamod %>% filter(COD_RAMA==700,ENS==8) %>% group_by(COD_SEC,GEN_ALU) %>% 
  summarise(N=n()) %>% pivot_wider(names_from = GEN_ALU,values_from = N) %>% 
  adorn_totals(c("col","row")) %>% mutate_at(vars(Hombre,Mujer),~(round(./Total*100,digits = 1))) %>% 
  mutate(Año=rep("2021"))

b4mujer<-b4 %>% select(Año,COD_SEC,Mujer)

mujeresañosb4<- data.frame(Año=rep(c("2019","2020"),each= 3),
                           COD_SEC=rep(c("710","720","Total"),2),
                           Mujer=(c(29.6,38.4,37.1,30.6,39.6,38.3)))

tabb53_7<- rbind(mujeresañosb4,b4mujer)

ggplot(data=tabb53_7, aes(x=COD_SEC,y=Mujer, fill=Año)) +
  geom_bar(stat="identity", position=position_dodge(),width = 0.55)#+
#scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
#coord_cartesian(ylim = c(10,30))+ylab("")+xlab("")+
#theme(axis.text.y= element_blank(),legend.position = "top",plot.title = element_text(hjust = 0.5))+
#geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
#scale_x_continuous(breaks=c(1:2),labels = c("Media HC Adulto","Media TP Adulto"))+
#ggtitle("Gráfico 49. Tasa de abandono \nestudiantes de enseñanza media según \nsexo, adultos, año 2021")

b3hombre<-b3 %>% select(Año,COD_SEC,Hombre)

hombresañosb3<- data.frame(Año=rep(c("2019","2020"),each= 4),
                           COD_SEC=rep(c("610","630","640","Total"),2),
                           Hombre=(c(34.7,35.5,9.4,22.0,36.7,36.2,10.2,23.3)))

tabb53_6<- rbind(hombresañosb3,b3hombre)

ggplot(data=tabb53_6, aes(x=COD_SEC,y=Hombre, fill=Año)) +
  geom_bar(stat="identity", position=position_dodge(),width = 0.55)


#----------------------------------------------#

# Matrícula media tp según sector económico, marítima

tab53_9<-matriculamod %>% filter(COD_RAMA==800,ENS==8,GEN_ALU=="Mujer") %>% 
  group_by(COD_SEC) %>% summarise(N=n())

tab53_10<-matriculamod %>% filter(COD_RAMA==800,ENS==8,GEN_ALU=="Hombre") %>% 
  group_by(COD_SEC) %>% summarise(N=n())

b5<-matriculamod %>% filter(COD_RAMA==800,ENS==8) %>% group_by(COD_SEC,GEN_ALU) %>% 
  summarise(N=n()) %>% pivot_wider(names_from = GEN_ALU,values_from = N) %>% 
  adorn_totals("col") %>% mutate_at(vars(Hombre,Mujer),~(round(./Total*100,digits = 1)))


b3mujer<-b3 %>% select(Año,COD_SEC,Mujer)

mujeresañosb3<- data.frame(Año=rep(c("2019","2020"),each= 4),
                           COD_SEC=rep(c("610","630","640","Total"),2),
                           Mujer=(c(65.3,64.5,90.6,78.0,63.3,63.8,89.8,76.7)))

tabb53_5<- rbind(mujeresañosb3,b3mujer)

ggplot(data=tabb53_5, aes(x=COD_SEC,y=Mujer, fill=Año)) +
  geom_bar(stat="identity", position=position_dodge(),width = 0.55)#+
#scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
#coord_cartesian(ylim = c(10,30))+ylab("")+xlab("")+
#theme(axis.text.y= element_blank(),legend.position = "top",plot.title = element_text(hjust = 0.5))+
#geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
#scale_x_continuous(breaks=c(1:2),labels = c("Media HC Adulto","Media TP Adulto"))+
#ggtitle("Gráfico 49. Tasa de abandono \nestudiantes de enseñanza media según \nsexo, adultos, año 2021")

b3hombre<-b3 %>% select(Año,COD_SEC,Hombre)

hombresañosb3<- data.frame(Año=rep(c("2019","2020"),each= 4),
                           COD_SEC=rep(c("610","630","640","Total"),2),
                           Hombre=(c(34.7,35.5,9.4,22.0,36.7,36.2,10.2,23.3)))

tabb53_6<- rbind(hombresañosb3,b3hombre)

ggplot(data=tabb53_6, aes(x=COD_SEC,y=Hombre, fill=Año)) +
  geom_bar(stat="identity", position=position_dodge(),width = 0.55)




# Matrícula media tp según sector económico, artística

tab53_11<-matriculamod %>% filter(COD_RAMA==900,ENS==8,GEN_ALU=="Mujer") %>% 
  group_by(COD_SEC) %>% summarise(N=n())

tab53_12<-matriculamod %>% filter(COD_RAMA==900,ENS==8,GEN_ALU=="Hombre") %>% 
  group_by(COD_SEC) %>% summarise(N=n()) # hay categorias que no sale pk tienen n=0, tienen q salir igual

b6<-matriculamod %>% filter(COD_RAMA==900,ENS==8) %>% group_by(COD_SEC,GEN_ALU) %>% 
  summarise(N=n()) %>% pivot_wider(names_from = GEN_ALU,values_from = N) %>% 
  adorn_totals("col") %>% mutate_at(vars(Hombre,Mujer),~(round(./Total*100,digits = 1)))


b3mujer<-b3 %>% select(Año,COD_SEC,Mujer)

mujeresañosb3<- data.frame(Año=rep(c("2019","2020"),each= 4),
                           COD_SEC=rep(c("610","630","640","Total"),2),
                           Mujer=(c(65.3,64.5,90.6,78.0,63.3,63.8,89.8,76.7)))

tabb53_5<- rbind(mujeresañosb3,b3mujer)

ggplot(data=tabb53_5, aes(x=COD_SEC,y=Mujer, fill=Año)) +
  geom_bar(stat="identity", position=position_dodge(),width = 0.55)#+
#scale_fill_brewer(palette="Paired")+guides(fill =guide_legend(title=""))+
#coord_cartesian(ylim = c(10,30))+ylab("")+xlab("")+
#theme(axis.text.y= element_blank(),legend.position = "top",plot.title = element_text(hjust = 0.5))+
#geom_text(aes(label=paste0(tasa,"%")),position = position_dodge(width = .9),vjust= -0.5)+
#scale_x_continuous(breaks=c(1:2),labels = c("Media HC Adulto","Media TP Adulto"))+
#ggtitle("Gráfico 49. Tasa de abandono \nestudiantes de enseñanza media según \nsexo, adultos, año 2021")

b3hombre<-b3 %>% select(Año,COD_SEC,Hombre)

hombresañosb3<- data.frame(Año=rep(c("2019","2020"),each= 4),
                           COD_SEC=rep(c("610","630","640","Total"),2),
                           Hombre=(c(34.7,35.5,9.4,22.0,36.7,36.2,10.2,23.3)))

tabb53_6<- rbind(hombresañosb3,b3hombre)

ggplot(data=tabb53_6, aes(x=COD_SEC,y=Hombre, fill=Año)) +
  geom_bar(stat="identity", position=position_dodge(),width = 0.55)



##---------------------------------------------------------------##
##---------------------------------------------------------------##
##---------------------------------------------------------------##




#noseqtabla<- rend2021 %>% filter(SIT_FIN_R=="P") %>% 
#  mutate(ivmedio=ifelse(COD_ENSE%in%c(410,510,610,710,810,910,963) &
#                          COD_GRADO==4,1,0)) %>%
#  filter(COD_ENSE2>=5 & ivmedio==1) %>%
#  arrange(RUN_ALU,desc(FEC_RET_ALU),desc(FEC_ING_ALU),desc(PROM_GRAL),desc(ASISTENCIA)) %>% 
#  distinct(RUN_ALU,.keep_all = T) %>% group_by(GEN_ALU,COD_RAMA) %>% summarise(N=n())
#tampocose<- rend2021 %>% filter(SIT_FIN_R=="P") %>%
#  mutate(ivmedio=ifelse(COD_ENSE%in%c(310,360,410,510,610,710,810,910) &
#                          COD_GRADO==4,1,0)) %>%
#  filter(COD_ENSE2>=5 & ivmedio==1) %>% filter(COD_GRADO==4 |COD_ENSE==363& COD_GRADO==3) %>%
#  arrange(RUN_ALU,desc(FEC_RET_ALU),desc(FEC_ING_ALU),desc(PROM_GRAL),desc(ASISTENCIA)) %>% 
#  distinct(RUN_ALU,.keep_all = T) %>% summarise(N=n())
#tab52prueba<- tab52_1_1 %>% group_by(GEN_ALU,COD_RAMA) %>% summarise(N=n())

