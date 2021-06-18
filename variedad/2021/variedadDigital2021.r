library(readxl)
library(tidyverse)






digital <- Actividades_2021 %>% filter(Formato !='Presencial', mesIni<4)
digital <- digital[,41:43]


# PUBLICO
publico <- c("Niños","Jóvenes","Adultos","Público General")
publico <- data.frame(publico)


actividadesPublico <- digital %>% filter(!is.na(Público_))
#actividadesPublico[is.na(actividadesPublico)] <- 0

actividadesPublico <- actividadesPublico %>% group_by(Público_) %>% count() %>% 
                      mutate(variPublico= if_else(n>0,1,0)) %>%
                      rename(publico=Público_)


publico <- publico%>% left_join(actividadesPublico, by="publico")
publico[is.na(publico)] <- 0
publico

### DISCIPLINA
disciplina <- c("Artes Escénicas","Artes Visuales","Cine y Audiovisual","Cultura Tradicional Popular",
                "Diseño","Literatura libro y lectura","Música","Mixto","Otros","Patrimonio Material")
disciplina <- data.frame(disciplina)

actividadesDisciplina <- digital[,2]
#names(actividadesDisciplina)[1] <- "DISCIPLINA_ARTISTICA"
##actividadesDisciplina <- actividadesDisciplina %>%
#  mutate(DISCIPLINA_ARTISTICA=str_to_lower(DISCIPLINA_ARTISTICA))


actividadesDisciplina <- actividadesDisciplina %>% filter(!is.na(disciplinaArtística))
actividadesDisciplina <- actividadesDisciplina %>%
  group_by(disciplinaArtística) %>%
  count() %>% mutate(variDisciplina= if_else(n>0,1,0)) %>% 
  rename(disciplina=disciplinaArtística )
actividadesDisciplina

disciplina <- disciplina%>% left_join(actividadesDisciplina, by="disciplina")
disciplina[is.na(disciplina)] <- 0

disciplina

####### ACTIVIDAD
actividad <- c("Ciclo","Concierto/Recital","Encuentro","Exposición","Feria","Festival",
               "Fiesta popular","Formación","Función","Visita / Recorrido")
actividad <- data.frame(actividad)


actividadesTipo <- digital[,3]

names(actividadesTipo)[1] <- "actividad"


actividadesTipo <- actividadesTipo %>% filter(!is.na(actividadesTipo)) %>% 
                    group_by(actividad) %>%
                    count()


actividad <- actividad%>% left_join(actividadesTipo, by="actividad")
actividad[is.na(actividad)] <- 0
actividad

