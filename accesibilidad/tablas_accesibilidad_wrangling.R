
library(tidyverse)
library(readr)
library(sf)
intersecciones <- read_csv("~/Documentos/osrm/v1/final/intersecciones.csv")# Intersecciones con datos Espacios Distancias

#TABLAS TABLERO ACCESIBILIDAD


#tabla base de las distancias a barrios
 espaciosBarriosDistancias <- intersecciones %>% 
                      as.data.frame() %>% 
                      rename(BARRIO=BARRIO_3,
                             COMUNA=COMUNA_3) %>% 
                      group_by(BARRIO,COMUNA) %>% 
                      summarise_at(vars(c("bar":"cine")), mean)%>%  filter(!is.na(BARRIO)) 

 barriosAlternativos <- intersecciones %>% 
   as.data.frame() %>% 
   rename(BARRIO=BARRIO_3,
          COMUNA=COMUNA_3) %>% 
   group_by(BARRIO,COMUNA) %>% 
   summarise_at(vars(c("promediocompuesto":"minimo")), mean)%>%  filter(!is.na(BARRIO)) 
 
 
espaciosBarriosDistancias <- espaciosBarriosDistancias %>% 
                            mutate(
                              pcEspacios = mean(
                                c(cine,biblioteca,musica,museo,formacion,teatro,centroCultural)
                                                       ),
                              minEspacios=min(
                               c(bar,biblioteca,centroCultural,disqueria,exhibicion,escenico,formacion,libreria,cine)
                                          ),
                              pgEspacios=mean(
                                c(bar,biblioteca,centroCultural,disqueria,exhibicion,escenico,formacion,libreria,cine)
                                                )
                                  )  


#formato Long de tabla base de las distancias a espacios

espaciosBarriosDistanciasLong <-  gather(espaciosBarriosDistancias, espacio, distancia, c(3:14), factor_key=TRUE)%>% 
  select(-c(3:5))

#INTERSECCIONES ACTIVIDADES

tablafinalActividades <- read_csv("~/Documentos/osrm/tablafinalActividades.csv")
tablafinalActividades <- tablafinalActividades[,-14]
colnames(tablafinalActividades)
tablafinalActividades <- tablafinalActividades[, c(1:15,18,16,17)]


#tabla base de las distancias a BARRIOS


actividadesBarriosDistancias <- tablafinalActividades %>% 
  as.data.frame() %>% 
  rename(BARRIO=BARRIO_3,
         COMUNA=COMUNA_3) %>% 
  group_by(BARRIO,COMUNA) %>% 
  summarise_at(vars(c("escenicas":"formacion")), mean) %>%  filter(!is.na(BARRIO))


barriosAlternativos2 <- tablafinalActividades %>% 
  as.data.frame() %>% 
  rename(BARRIO=BARRIO_3,
         COMUNA=COMUNA_3) %>% 
  group_by(BARRIO,COMUNA) %>% 
  summarise_at(vars(c("minimo":"promediocompuesto")), mean) %>%  filter(!is.na(BARRIO))

barriosAlternativos["minAct"] <- barriosAlternativos2$minimo
barriosAlternativos["pcAct"] <- barriosAlternativos2$promediocompuesto

rm(barriosAlternativos2)


actividadesBarriosDistancias <- actividadesBarriosDistancias %>% 
              mutate(pcActividades=mean(
                            c(escenicas,visuales,cine,musica,formacion,literatura)
                            ),
            minActividades=min(escenicas, visuales,cine,tradicional,diseno,literatura,mixto,musica,otros,inmaterial,formacion),

            pgActividades=mean(escenicas, visuales,cine,tradicional,diseno,literatura,mixto,musica,otros,inmaterial,formacion)
  ) 


#FORMATOLONGACTIVIDADES

actividadesBarriosDistanciasLong <-  gather(actividadesBarriosDistancias, espacio, distancia, c(3:13), factor_key=TRUE) %>% 
                                     select(-c(3:5))



#INTERSECCIONES INFRAESTRUCTURA PUBLICA

library(readr)
tablaInfraestructuraPublica <- read_csv("~/Documentos/osrm/tablaInfraestructuraPublica.csv")
infraestructuraPublica <- tablaInfraestructuraPublica
rm(tablaInfraestructuraPublica)

infraestructuraPublica["COMUNA"] <- actividadesBarriosDistancias$COMUNA
infraestructuraPublica <- infraestructuraPublica %>% rename(BARRIO=BARRIO_3)

minCiudad <- apply(infraestructuraPublica[,c(2:6)], 1, min )
minNac <- apply(infraestructuraPublica[,c(8:13)], 1, min )
minBoth <- apply(infraestructuraPublica[,c(2:6,8:13)], 1, min )

infraestructuraPublica["minCiudad"] <- minCiudad
infraestructuraPublica["minNac"] <- minNac
infraestructuraPublica["minBoth"] <- minBoth

pgCiudad <- apply(infraestructuraPublica[,c(2:6)], 1, mean )
pgNac <- apply(infraestructuraPublica[,c(8:13)], 1, mean )


minA <- apply(infraestructuraPublica[,c(2,8)], 1, min )
minB <- apply(infraestructuraPublica[,c(3,9)], 1, min )
minC <- apply(infraestructuraPublica[,c(4,11)], 1, min )
minD <- apply(infraestructuraPublica[,c(5,12)], 1, min )
minE <- apply(infraestructuraPublica[,c(6,13)], 1, min )

dataframe <- data.frame(minA,minB,minC,minD,minE,infraestructuraPublica$cinesNac)
promedioPublica <- apply(dataframe, 1, min )


infraestructuraPublica["pgCiudad"] <- pgCiudad
infraestructuraPublica["pgNac"] <- pgNac
infraestructuraPublica["minBoth"] <- minBoth
infraestructuraPublica["promedioPublica"] <- promedioPublica

infraestructuraPublica <- infraestructuraPublica %>% select(-7)
rm(pgCiudad,pgNac,promedioPublica,minA,minB,minBoth,minC,minCiudad,minD,minE,minNac)

infraestructuraPublica["BARRIO"] <- actividadesBarriosDistancias$BARRIO




#INFRAESTRUCTURA PUBLICA LONG

infraestructuraPublicaLong <-  gather(infraestructuraPublica, espacio, distancia, c(2:12), factor_key=TRUE)%>% 
                            select(-c(2,3,5:8)) 


#ESPACIOS

espacios <- st_read("~/Documentos/osrm/v1/espaciosLong.gpkg")
comunas <- str_replace(espacios$COMUNA,"COMUNA 5","COMUNA 05")
espacios["COMUNA"] <- comunas
rm(comunas)

galerias <- str_replace(espacios$SUBCATEGORIA,"GALERÍA DE ARTE","GALERIA DE ARTE")
espacios["SUBCATEGORIA"] <- galerias
rm(galerias)


feria <- str_replace(espacios$FUNCION_PRINCIPAL,"FERIA","ESPACIO FERIAL")
espacios["FUNCION_PRINCIPAL"] <- feria
rm(feria)


espaciosCantidad <- espacios %>%as.data.frame() %>% select(-geom) %>%  group_by(BARRIO,COMUNA) %>%  count() %>% rename(cantidad=n)
espaciosDistintos <- espacios %>% as.data.frame() %>% select(-geom) %>% 
                                  group_by(BARRIO) %>% 
                                  summarise(distintos= n_distinct(FUNCION_PRINCIPAL))


espaciosCantidad["distintos"] <- espaciosDistintos$distintos
rm(espaciosDistintos)

pivotes <- espacios %>% as.data.frame() %>% group_by(BARRIO,COMUNA,FUNCION_PRINCIPAL) %>% count() %>% pivot_wider(names_from = FUNCION_PRINCIPAL, values_from = n)
espaciosCantidad <- espaciosCantidad %>% left_join(pivotes, by="BARRIO") %>% select(-5)
rm(pivotes)

pivotes <- espacios %>% as.data.frame() %>% group_by(BARRIO,COMUNA,SUBCATEGORIA) %>% count() %>%
                                            filter(SUBCATEGORIA %in% c("GALERIA DE ARTE",'MUSEO',"CLUB DE MUSICA EN VIVO","SALA DE TEATRO")) %>% 
                                            pivot_wider(names_from = SUBCATEGORIA, values_from = n) 

espaciosCantidad <- espaciosCantidad %>% left_join(pivotes, by="BARRIO")%>% select(-18)
rm(pivotes)

write_csv(espacios,"accesibilidad/tablas/espacios.csv")

#Cantidad de Espacios del Ministerio
espaciosCantidadCiudad <- espacios %>%as.data.frame() %>% select(-geom) %>%  filter(minCult==1) %>% 
                          group_by(BARRIO,COMUNA) %>%  count() %>% rename(cantidadCiudad=n)

#Variedad de Espacios del Ministerio
espaciosDistintosCiudad <- espacios %>% as.data.frame() %>% select(-geom) %>% filter(minCult==1) %>%   group_by(BARRIO) %>% 
                          summarise(distintosCiudad= n_distinct(FUNCION_PRINCIPAL))

#Joineo a la tabla en general
espaciosCantidad <- espaciosCantidad %>% left_join(espaciosCantidadCiudad, by="BARRIO") 
espaciosCantidad <- espaciosCantidad %>% select(-c(17,21))
espaciosCantidad <- espaciosCantidad %>% left_join(espaciosDistintosCiudad,by="BARRIO")


rm(espaciosCantidadCiudad,espaciosDistintosCiudad)

#################################################
#NACION 

#Cantidad de Espacios Nación
espaciosCantidadNacion <- espacios %>%as.data.frame() %>% select(-geom) %>%  filter(minCult==2) %>% 
  group_by(BARRIO,COMUNA) %>%  count() %>% rename(cantidadNacion=n)

#Variedad de Espacios de Nación
espaciosDistintosNacion <- espacios %>% as.data.frame() %>% select(-geom) %>% filter(minCult==2) %>%   group_by(BARRIO) %>% 
  summarise(distintosNacion= n_distinct(FUNCION_PRINCIPAL))

#Joineo a la tabla en general
espaciosCantidad <- espaciosCantidad %>% left_join(espaciosCantidadNacion, by="BARRIO") 
espaciosCantidad <- espaciosCantidad %>% select(-22)

espaciosCantidad <- espaciosCantidad %>% left_join(espaciosDistintosNacion,by="BARRIO")

rm(espaciosCantidadNacion,espaciosDistintosNacion)


#PUBLICA GENERAL

#Cantidad de Espacios Publicos
espaciosCantidadPublicos <- espacios %>%as.data.frame() %>% select(-geom) %>%  filter(!is.na(minCult)) %>% 
  group_by(BARRIO,COMUNA) %>%  count() %>% rename(cantidadPublico=n)

#Variedad de Espacios de Nación
espaciosDistintosPublicos <- espacios %>% as.data.frame() %>% select(-geom) %>% filter(!is.na(minCult)) %>%   group_by(BARRIO) %>% 
  summarise(distintosPublicos= n_distinct(FUNCION_PRINCIPAL))

#Joineo a la tabla en general
espaciosCantidad <- espaciosCantidad %>% left_join(espaciosCantidadPublicos, by="BARRIO") 
espaciosCantidad <- espaciosCantidad %>% select(-24)

espaciosCantidad <- espaciosCantidad %>% left_join(espaciosDistintosPublicos,by="BARRIO")

rm(espaciosCantidadPublicos,espaciosDistintosPublicos)

espaciosCantidad[is.na(espaciosCantidad)] <- 0


#write_csv(espaciosCantidad,"accesibilidad/tablas/espaciosCantidad.csv")


#DEMOGRAFIA
poblacion <- read_csv("~/Documentos/osrm/v1/poblacion.csv")
poblacion1 <- str_replace(poblacion$BARRIOS,"LA BOCA","BOCA")
poblacion["BARRIOS"] <- poblacion1
rm(poblacion1)
poblacion <- poblacion[,c(3:5)]
poblacion <- poblacion %>% rename(BARRIO=BARRIOS)

espaciosCantidad <- espaciosCantidad %>% left_join(poblacion, by="BARRIO")
espaciosCantidad <- espaciosCantidad %>% select(-26)

espaciosDemografia <- espacios %>% as.data.frame() %>% select(-geom) %>%  group_by(FUNCION_PRINCIPAL) %>% count()

demografia <- espaciosDemografia$n/3078836 *100000
espaciosDemografia["demografia"] <- demografia
rm(demografia)

write_csv(espaciosDemografia,"accesibilidad/tablas/espaciosDemografia.csv")


#UNIFICACION
barrios <- espaciosBarriosDistancias %>% left_join(espaciosCantidad) %>% left_join(actividadesBarriosDistancias, by="BARRIO") 
barrios <- barrios %>% left_join(infraestructuraPublica, by="BARRIO")
barrios <- barrios %>% mutate(minBoth= min(minCiudad,minNac))
barrios["tot_pob"] <- poblacion$tot_pob
barrios <- barrios %>% select(-c(18,43,71))



write_csv(barrios,"accesibilidad/tablas/barrios.csv")
writexl::write_xlsx(barrios,"accesibilidad/tablas/barrios.xlsx")
writexl::write_xlsx(barriosAlternativos,"accesibilidad/tablas/barriosAlternativos.xlsx")

#COMUNAS
comunas2 <- barrios %>% group_by(COMUNA.x) %>%summarise_at(vars(c("bar":"promedioPublica")), mean) 
comunas3 <- barrios %>% group_by(COMUNA.x) %>%summarise_at(vars(c("bar":"promedioPublica")), sum) 
comunas <-  bind_cols(comunas2[,c(1:16)],comunas3[,c(17:40)], comunas2[,c(41:71)])
rm(comunas2,comunas3)


espaciosDistintos <- espacios %>% as.data.frame() %>% select(-geom) %>% 
  group_by(COMUNA) %>% 
  summarise(distintos= n_distinct(FUNCION_PRINCIPAL))


comunas["distintos"] <- espaciosDistintos$distintos
rm(espaciosDistintos)



#Variedad de Espacios del Ministerio
espaciosDistintosCiudad <- espacios %>% as.data.frame() %>% select(-geom) %>% filter(minCult==1) %>%   group_by(COMUNA) %>% 
  summarise(distintosCiudad= n_distinct(FUNCION_PRINCIPAL))

comunas["distintosCiudad"] <- espaciosDistintosCiudad$distintosCiudad
rm(espaciosDistintosCiudad)

#NACION 


#Variedad de Espacios de Nación
espaciosDistintosNacion <- espacios %>% as.data.frame() %>% select(-geom) %>% filter(minCult==2) %>%   group_by(COMUNA) %>% 
  summarise(distintosNacion= n_distinct(FUNCION_PRINCIPAL)) %>% mutate(COMUNA=as.integer(str_sub(COMUNA,8,9)))

comunas["distintosNacion"] <- 0

comunas <- comunas %>% rename("COMUNA"="COMUNA.x")
comunas <- comunas %>% left_join(espaciosDistintosNacion, by='COMUNA') %>% mutate(distintosNacion.x=distintosNacion.y) %>%  select(-72) %>% 
                                  rename("distintoNacion"="distintosNacion.x")
rm(espaciosDistintosNacion)


#PUBLICA GENERAL

#Variedad de Espacios de Nación
espaciosDistintosPublicos <- espacios %>% as.data.frame() %>% select(-geom) %>% filter(!is.na(minCult)) %>%   group_by(COMUNA) %>% 
  summarise(distintosPublicos= n_distinct(FUNCION_PRINCIPAL))

comunas["distintosPublicos"] <- espaciosDistintosPublicos$distintosPublicos
rm(espaciosDistintosPublicos)

comunas[is.na(comunas)] <- 0

writexl::write_xlsx(comunas,"accesibilidad/tablas/comunas.xlsx")

comunasAlternativas <- barriosAlternativos %>% group_by(COMUNA) %>% summarise_at(vars(c("promediocompuesto":"pcAct")), mean)
writexl::write_xlsx(comunasAlternativas,"accesibilidad/tablas/comunasAlternativas.xlsx")


#LONGS
actividadesBarriosDistanciasLong <- actividadesBarriosDistanciasLong %>% mutate(tabla="ActividadesDistancias")
espaciosBarriosDistanciasLong <- espaciosBarriosDistanciasLong %>% mutate(tabla="EspaciosDistancias")
infraestructuraPublicaLong <- infraestructuraPublicaLong %>% mutate(tabla=if_else(str_sub(espacio,-3,str_length(espacio))=='Nac','Nación','Ciudad'))
espaciosDemografiaLong <- barrios[,c(1:2,20:34,41)]
espaciosDemografiaLong <-  gather(espaciosDemografiaLong, espacio, distancia, c(3:17), factor_key=TRUE)

espaciosDemografiaLong <- espaciosDemografiaLong %>% mutate(tabla="Demografia")
                                                                      

barriosLong <- bind_rows(actividadesBarriosDistanciasLong,espaciosBarriosDistanciasLong)
barriosLong <- bind_rows(barriosLong,infraestructuraPublicaLong)
barriosLong <- bind_rows(barriosLong,espaciosDemografiaLong)

write_csv(barriosLong,"accesibilidad/tablas/barriosLong.csv")
writexl::write_xlsx(barriosLong,"accesibilidad/tablas/barriosLong.xlsx")


#LONGS COMUNAS
espaciosComunasDistanciasLong <-  gather(comunas, espacio, distancia, c(2:13), factor_key=TRUE) %>% select(c(1,60,61)) %>% mutate(tabla="distanciaEspacios")

actividadesComunasDistanciasLong <-  gather(comunas, espacio, distancia, c(41:51), factor_key=TRUE) %>% mutate(tabla="distanciaActividades") %>% 
    select(c(1,61:63))

infraestructuraPublicaComunasLong <-  gather(comunas, espacio, distancia, c(55:65), factor_key=TRUE) %>% mutate(tabla="Infraestructura Publica") %>% 
  select(c(1,61:63))


infraestructuraPublicaComunasLong <- infraestructuraPublicaComunasLong %>% 
                                  mutate(tabla=if_else(str_sub(espacio,-3,str_length(espacio))=='Nac','Nación','Ciudad'))

espaciosDemografiaLongComu <- comunas[,c(1,19:33,40)]
espaciosDemografiaLongComu <-  gather(espaciosDemografiaLongComu, espacio, distancia, c(2:16), factor_key=TRUE)
espaciosDemografiaLongComu <- espaciosDemografiaLongComu %>% mutate(tabla="Demografia")

comunasLong <- bind_rows(espaciosComunasDistanciasLong,actividadesComunasDistanciasLong)
comunasLong <- bind_rows(comunasLong,infraestructuraPublicaComunasLong)
comunasLong <- bind_rows(comunasLong,espaciosDemografiaLongComu)

writexl::write_xlsx(comunasLong,"accesibilidad/tablas/comunasLong.xlsx")
