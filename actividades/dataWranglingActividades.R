# Este documento es para arreglar las actividades...
# Se unificaron en excel
setwd("~/Data Cultura_martin/Actividades/2021")

library(tidyverse)
library(lubridate)
library(readxl)
Actividades_2021 <- read_excel("Actividades 2021.xlsx", 
                               col_types = c("text", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "date", 
                                             "date", "text", "text", "text", "text", 
                                             "text", "text", "text"))
# Doy vuelta aquellos que tienen la fecha de inicio y la fecha de fin alreves
Actividades_2021 <- Actividades_2021 %>% 
  mutate(pivotIni=if_else(
    `Fecha de Inicio`>`Fecha de Fin`,
    `Fecha de Fin`,
    `Fecha de Inicio`
  ),
  pivotFin= if_else(
    `Fecha de Inicio`>`Fecha de Fin`,
    `Fecha de Inicio`,
    `Fecha de Fin`
  ),
  `Fecha de Inicio`= pivotIni, `Fecha de Fin`=pivotFin)

# el campo repetidos es el que dice cuantos hay que agregar. id rownumber
Actividades_2021 <- Actividades_2021 %>% mutate(mesIni= month(`Fecha de Inicio`),
                                                mesFin= month(`Fecha de Fin`),
                                                repetidos=mesFin-mesIni,
                                                id=row_number())
#quito los campos pivots que use antes
Actividades_2021 <- Actividades_2021[,-c(37,38)]

#filtro lo que se va a repetir
Actividades_2021_1 <- Actividades_2021 %>% filter(repetidos >0)

#repito rows
Actividades_2021_1 <- Actividades_2021_1%>% 
            uncount(weights = repetidos, .id="n", .remove = TRUE)

#modifico la fecha de mes ini para contabilizar sin problemas
Actividades_2021_1 <- Actividades_2021_1 %>% mutate(mesIni=mesIni+n)

#saco campo repetidos que ya no lo uso mas
Actividades_2021 <- Actividades_2021[,-39]
#cada campo repetido tiene un n incremental, los que tienen n 0 son los no
#repetidos. Es decir id+n es idunico
Actividades_2021 <- Actividades_2021 %>% mutate(n=0)
Actividades_2021 <- rbind(Actividades_2021,Actividades_2021_1) %>%
  arrange(id,n)
                    

#ActividadesMmes <- Actividades_2021 %>% group_by(mesIni) %>% count()

#ActividadesPagas <- Actividades_2021 %>% filter(Gratuito!= 'Pago') %>% 
  #group_by(mesIni) %>% count()

#ActividadesInfancia <- Actividades_2021 %>% 
              #        filter(Público == 'Primera Infancia (0-3)') %>% 
               #       group_by(mesIni) %>% count()

#Actividades_2021 %>%   group_by(Público) %>% count()

Actividades_2021 <- Actividades_2021 %>% mutate(
  Público_ = case_when(
    Público =='Adultos (41 a 65)' ~ 'Adultos' , 
    Público == 'Adultos jóvenes ( 22 a 40)' ~ 'Adultos',
    Público == 'Adultos jóvenes ( 22 a 40) / Adultos (41 a 65)' ~ 'Adultos',
    Público == 'Adultos jóvenes (22 a 40)' ~ 'Adultos',
    Público == 'Adultos jóvenes ( 22 a 40) Adultos (41 a 65)'~ 'Adultos' ,
    Público == 'Adultos Mayores (+65)' ~ 'Adultos Mayores' ,
    Público == 'atp'~ 'Público General' ,
    Público == 'Jóvenes (13 a 17)'~ 'Jóvenes',
    Público == 'Jóvenes (13 a 21)'~ 'Jóvenes',
    Público == 'Niños (4-7)' ~ 'Niños' ,
    Público == 'Pre adolescentes (8-12)'~ 'Niños' ,
    Público == 'Pre adolscentes (8-12)'~ 'Niños' ,
    Público == 'Primera Infancia (0-3)'~ 'Niños' ,
    Público == 'Programado'~ 'Público General' ,
    is.na(Público) ~ 'Público General',
    TRUE ~ Público
  )
)



Actividades_2021 <- Actividades_2021 %>% mutate(
  disciplinaArtística = case_when(
    `Disciplina Artística` =='Concierto Recital' ~ 'Música' , 
    `Disciplina Artística` == 'Cultura Tradicional Popular y Patrimonio Inmaterial' ~ 'Cultura Tradicional Popular',
    `Disciplina Artística` == 'Cultura Tradicional y Popular y Patrimonio Inmaterial' ~ 'Cultura Tradicional Popular',
    `Disciplina Artística` == 'Literatura Libro y Lectura' ~ 'Literatura libro y lectura',
    `Disciplina Artística` == 'Mixta'~ 'Mixto' ,
    `Disciplina Artística` == 'Paleontología' ~ 'Patrimonio Material' ,
    `Disciplina Artística` == 'Teatro'~ 'Artes Escénicas',
    TRUE ~ `Disciplina Artística`
  )
)

a <- Actividades_2021 %>%group_by(Tipo) %>% count()
Actividades_2021 <- Actividades_2021 %>% mutate(
  tipoActividad = case_when(
    Tipo %in% c('Concierto/recital',
                'Recital') ~ 'Concierto/Recital' , 
    Tipo == 'Celebración' ~ 'Festival',
    Tipo %in% c('Workshop',
                'workshop',
                'Taller',
                'Clase magistral',
                'clase',
                'Charla',
                'Curso') ~ 'Formación',
    Tipo == 'Visita/Recorrido'~ 'Visita / Recorrido',
    Tipo == 'Obra' ~ 'Función',
    TRUE ~ Tipo
  )
)

#SE NORMALIZO Y GEOREFERENCIO POR FUERA !!!!! POR ESO ESTA COMENTADO


Actividades_2021_Direcciones <- Actividades_2021 %>% 
           filter(Formato=='Presencial') %>% 
                      group_by(Lugar, Dirección) %>% count()


#write_csv(Actividades_2021_Direcciones, "direcciones.csv")



#direccionesNormalizadas <- read_csv("direccionesNormalizadas.csv")

#Actividades_2021 <- left_join(Actividades_2021, direccionesNormalizadas, by="Dirección")

#Actividades_2021 <- Actividades_2021 %>% 
#  mutate(Dirección= if_else(!is.na(`Dirección Normalizada`),
#                            `Dirección Normalizada`,
#                            Dirección
#  ))

#SE METE DE PREPO LAS DIR BIEN CON COORDS
direcciones3 <- read_csv("direcciones3.csv")
#Actividades_2021_Direcciones["direccion"] <- direcciones1$`Dirección Normalizada`
#Actividades_2021_Direcciones["georef"] <- direcciones1$Geocodificación
Actividades_2021_Direcciones["BARRIO"] <- direcciones3$nombre
Actividades_2021_Direcciones["COMUNAS"] <- direcciones3$COMUNAS

Actividades_2021_Direcciones["lat"] <- direcciones3$lat
Actividades_2021_Direcciones["long"] <- direcciones3$long



#write_excel_csv(Actividades_2021_Direcciones, "direcciones1.csv", )

#Actividades_2021 <- Actividades_2021[,-c(44:48)]

#EL CAMPO JOIN ES EL CAMPO UNICO PARA QUE EL JOINEADO FUNCIONE BIEN
actividades <- Actividades_2021 %>% mutate(join=paste(Lugar,Dirección))
actividadesD <- Actividades_2021_Direcciones %>% mutate(join=paste(Lugar,Dirección))

# FIN DE NORMALIZACION DE DIRECCIONES
Actividades_2021 <- actividades %>% left_join(actividadesD, by="join")
Actividades_2021 <- Actividades_2021[,-c(44:45)]




write_csv(Actividades_2021,"actividades2021_HastaMitadJunio_Normalizado.csv")






#############################
#QUERYS
Actividades_2021 %>% filter(Público_=='Adultos Mayores') %>% 
  group_by(mesIni) %>% count()

Actividades_2021 %>% filter(Público_=='Niños') %>% 
  group_by(mesIni) %>% count()
Actividades_2021 %>% filter(Público_=='Jóvenes') %>% 
  group_by(mesIni) %>% 
  count()


Actividades_2021 %>% group_by()