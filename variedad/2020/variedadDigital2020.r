library(readxl)
library(tidyverse)

actividades2020 <- read_excel("C:/Users/GCBA/Downloads/actividades2020 (1).xlsx")


actividades2020 <- actividades2020 %>% mutate( Lugar=str_to_lower(Lugar))

actividadesDigitales <- actividades2020 %>% filter(Lugar=='casa de la cultura')
actividadesDigitales <- actividadesDigitales %>% mutate(mes= as.integer(
                                                str_sub(fechaIniNorm,6,7)
                                                                      )
                                          ) %>% 
                        filter(mes<7)
  
  
  actividadesPublico <- actividadesDigitales[,c(11)]

actividadesPublico <- actividadesPublico %>% filter(!is.na(Público))
actividadesPublico[is.na(actividadesPublico)] <- 0
actividadesPublico <- actividadesPublico %>%
  mutate(publicoVariedad=  case_when(
    Público %in% c('Niños (4 a 12)',
                   'Primera Infancia (0-3)')~'Niños',
    Público %in% c('Adultos',
                   'Adultos Mayores (+65)')~'Adultos',
    Público %in% c('Jóvenes (13 a 21)')~'Jóvenes',
    TRUE ~ 'Público'
  ))
actividadesPublico <- actividadesPublico %>% group_by(publicoVariedad) %>% count() 

actividadesPublico

### DISCIPLINA
actividadesDisciplina <- actividadesDigitales[,c(9)]
names(actividadesDisciplina)[1] <- "DISCIPLINA_ARTISTICA"
actividadesDisciplina <- actividadesDisciplina %>%
  mutate(DISCIPLINA_ARTISTICA=str_to_lower(DISCIPLINA_ARTISTICA))

actividadesDisciplina <- actividadesDisciplina %>%
  mutate(DISCIPLINA_ARTISTICA=
           case_when(
             DISCIPLINA_ARTISTICA=='cultura tradicional y popular y patrimonio inmaterial'~
               'cultura tradicional popular y patrimonio inmaterial',
             DISCIPLINA_ARTISTICA=='literatura'~'literatura, libro y lectura',
             TRUE ~ DISCIPLINA_ARTISTICA
           ))


actividadesDisciplina <- actividadesDisciplina %>% filter(!is.na(DISCIPLINA_ARTISTICA))
actividadesDisciplina <- actividadesDisciplina %>%
  group_by(DISCIPLINA_ARTISTICA) %>%
  count()
actividadesDisciplina


####### ACTIVIDAD

actividadesTipo <- actividadesDigitales[,c(10)]
names(actividadesTipo)[1] <- "TIPO_ACTIVIDAD"
actividadesTipo <- actividadesTipo %>%
  mutate(TIPO_ACTIVIDAD=str_to_lower(TIPO_ACTIVIDAD))
actividadesTipo %>% group_by(TIPO_ACTIVIDAD) %>% count()



actividadesTipo <- actividadesTipo %>% filter(!is.na(TIPO_ACTIVIDAD))
actividadesTipo <- 
  actividadesTipo %>%
  group_by(TIPO_ACTIVIDAD) %>%
  count()
actividadesTipo <- actividadesTipo %>%
  filter(!TIPO_ACTIVIDAD %in% c('otro',
                                'curso',
                                'convocatoria',
                                'av. de mayo'))
actividadesTipo
