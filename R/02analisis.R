
# Análisis informe de género 2022

# Cargar paquetes
pacman::p_load(tidyverse,sjmisc,
               sjPlot,ggplot2,janitor)


# Cargar datos
data = readRDS("output/data/dataproc.rds")

# Análisis

# Docentes 2021

# Tabla 1

data %>% 
  group_by(DOC_GENERO) %>%
  summarise(n = n(),
            N = round(n()/nrow(.)*100,digits = 1)) %>%
  adorn_totals("row")

# Tabla 2

data %>% 
  group_by(tramos,DOC_GENERO) %>% 
  summarise(n = n(),
            N = round(n()/sum(n),digits = 1))




ggplot(tab2, aes(fill=DOC_GENERO, y=tramos, x=porcentaje)) + 
  geom_bar(position="fill",stat="identity",orientation = "y")


# Tabla 3

data %>% 
  group_by(RURAL_RBD,DOC_GENERO) %>% 
  summarise(n= round(n()/nrow(.),4)*100)# %>% 
  #ggplot(aes(x=RURAL_RBD,y=n, fill=DOC_GENERO)) + 
  geom_bar(position = "fill",stat = "identity") +
  labs(title = " Tabla 3",
       caption = "Fuente: MINEDUC",
       x= "Total",
       y= "") +
  guides(fill=guide_legend(title = "Género")) +
  geom_text(aes(label=n),position = position_fill(vjust = 0.7)) +
  theme(axis.text.x = element_blank(),
       axis.ticks.x = element_blank())

data %>% 
  group_by(RURAL_RBD,DOC_GENERO) %>% 
  summarize(n= round(n/sum(n)*100,digits = 1))
  
  
  mutate(porcentaje=round(N/sum(N)*100,digits=1)) %>% select(-N)  
  
  
  

# Tabla 12

# Tabla 13




#Frq. abs.

data %>% 
  group_by(logro_tot) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x=logro_tot, y=n,fill=logro_tot)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title = "Número de unidades económicas logradas a nivel nacional",
       caption = "Fuente: Elaboración propia",
       x = '',
       y = 'Total') + 
  guides(fill=guide_legend(title="Estado de logro")) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())



#Frq. rel.

data %>% 
  group_by(logro_tot) %>% 
  summarise(n = round(n()/nrow(.), 4)*100) %>%
  ggplot(aes(x=logro_tot, y=n,fill=logro_tot)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title = "Porcentaje de unidades económicas logradas a nivel nacional",
       caption = "Fuente: Elaboración propia",
       x = '',
       y = '%') + 
  guides(fill=guide_legend(title="Estado de logro")) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 

