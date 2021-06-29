
library(tidyverse)
library(readr)
library(sf)
intersecciones <- read_csv("~/Documentos/osrm/v1/final/intersecciones.csv")# Intersecciones con datos Espacios Distancias

#TABLAS TABLERO ACCESIBILIDAD


#INTERSECCIONES ESPACIOS


#tabla base de las distancias a barrios
espaciosBarriosDistancias <- intersecciones %>% 
                      as.data.frame() %>% 
                      rename(BARRIO=BARRIO_3,
                             COMUNA=COMUNA_3) %>% 
                      group_by(BARRIO,COMUNA) %>% 
                      summarise_at(vars(c("bar":"cine")), mean)%>%  filter(!is.na(BARRIO)) 

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

#write_csv(espaciosBarriosDistancias,"accesibilidad/tablas/espaciosBarriosDistancias.csv")


#formato Long de tabla base de las distancias a espacios
  
#tabla base de las distancias a espacios
espaciosComunas <- espaciosBarriosDistancias %>% 
 # as.data.frame() %>% 
  group_by(COMUNA) %>% 
  summarise_at(vars(c("bar":"cine")), mean)

pcEspacios <- apply(espaciosComunas[,c(3,4,8:11,13)], 1, mean )
minEspacios <- apply(espaciosComunas[,c(2:8,12:13)], 1, min )
pgEspacios <- apply(espaciosComunas[,c(2:8,12:13)], 1, mean )

espaciosComunas["pcEspacios"] <- pcEspacios
espaciosComunas["minEspacios"] <- minEspacios
espaciosComunas["pgEspacios"] <- pgEspacios
rm(pcEspacios,minEspacios,pgEspacios)

#write_csv(espaciosComunas,"accesibilidad/tablas/espaciosComunas.csv")

#formato Long de tabla base de las distancias a espacios

espaciosBarriosDistanciasLong <-  gather(espaciosBarriosDistancias, espacio, distancia, c(3:14), factor_key=TRUE)%>% 
  select(-c(3:5))

espaciosComunasDistanciasLong <-  gather(espaciosComunas, espacio, distancia, c(2:13), factor_key=TRUE) %>% 
  select(-c(2:4))


#write_csv(espaciosBarriosDistanciasLong,"accesibilidad/tablas/espaciosBarriosDistanciasLong.csv")
#write_csv(espaciosComunasDistanciasLong,"accesibilidad/tablas/espaciosComunasDistanciasLong.csv")




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

actividadesBarriosDistancias <- actividadesBarriosDistancias %>% 
              mutate(pcActividades=mean(
                            c(escenicas,visuales,cine,musica,formacion,literatura)
                            ),
            minActividades=min(escenicas:formacion),

            pgActividades=mean(c(escenicas:formacion))
  ) 
#write_csv(actividadesBarriosDistancias,"accesibilidad/tablas/actividadesBarriosDistancias.csv")


#COMUNAS
actividadesComunasDistancias <- actividadesBarriosDistancias %>% 
  as.data.frame() %>% 
  group_by(COMUNA) %>% 
  summarise_at(vars(c("escenicas":"formacion")), mean) 

pcActividades <- apply(actividadesComunasDistancias[,c(2:4,7,9,12)], 1, mean )
minActividades <- apply(actividadesComunasDistancias[,c(2:12)], 1, min )
pgActividades <- apply(actividadesComunasDistancias[,c(2:12)], 1, mean )

actividadesComunasDistancias["pcActividades"] <- pcActividades
actividadesComunasDistancias["minActividades"] <- minActividades
actividadesComunasDistancias["pgActividades"] <- pgActividades
rm(pcActividades,minActividades,pgActividades, tablafinalActividades)

#write_csv(actividadesComunasDistancias,"accesibilidad/tablas/actividadesComunasDistancias.csv")

#FORMATOLONGACTIVIDADES

actividadesBarriosDistanciasLong <-  gather(actividadesBarriosDistancias, espacio, distancia, c(3:13), factor_key=TRUE) %>% 
                                     select(-c(3:5))

actividadesComunasDistanciasLong <-  gather(actividadesBarriosDistancias, espacio, distancia, c(3:13), factor_key=TRUE) %>% 
  select(-c(3:5))

#write_csv(actividadesBarriosDistanciasLong,"accesibilidad/tablas/actividadesBarriosDistanciasLong.csv")
#write_csv(actividadesComunasDistanciasLong,"accesibilidad/tablas/actividadesComunasDistanciasLong.csv")


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


#write_csv(infraestructuraPublica,"accesibilidad/tablas/infraestructuraPublica.csv")


#COMUNAS

infraestructuraPublicaComunas <- infraestructuraPublica %>% 
  as.data.frame() %>% 
  group_by(COMUNA) %>% 
  summarise_at(vars(c("bibliotecasCiudad":"formacionNac")), mean) 


minCiudad <- apply(infraestructuraPublicaComunas[,c(2:6)], 1, min )
minNac <- apply(infraestructuraPublicaComunas[,c(7:12)], 1, min )
minBoth <- apply(infraestructuraPublicaComunas[,c(2:12)], 1, min )

infraestructuraPublicaComunas["minCiudad"] <- minCiudad
infraestructuraPublicaComunas["minNac"] <- minNac
infraestructuraPublicaComunas["minBoth"] <- minBoth

pgCiudad <- apply(infraestructuraPublicaComunas[,c(2:6)], 1, mean )
pgNac <- apply(infraestructuraPublicaComunas[,c(7:12)], 1, mean )
pgBoth <- apply(infraestructuraPublicaComunas[,c(2:12)], 1, mean )

minA <- apply(infraestructuraPublicaComunas[,c(2,7)], 1, min )
minB <- apply(infraestructuraPublicaComunas[,c(3,8)], 1, min )
minC <- apply(infraestructuraPublicaComunas[,c(4,10)], 1, min )
minD <- apply(infraestructuraPublicaComunas[,c(5,11)], 1, min )
minE <- apply(infraestructuraPublicaComunas[,c(6,12)], 1, min )

dataframe <- data.frame(minA,minB,minC,minD,minE,infraestructuraPublicaComunas$cinesNac)
promedioPublica <- apply(dataframe, 1, min )
rm(dataframe)

infraestructuraPublicaComunas["pgCiudad"] <- pgCiudad
infraestructuraPublicaComunas["pgNac"] <- pgNac
infraestructuraPublicaComunas["pgBoth"] <- pgoth
infraestructuraPublicaComunas["promedioPublica"] <- promedioPublica

rm(pgCiudad,pgNac,promedioPublica,minA,minB,minBoth,minC,minCiudad,minD,minE,minNac,pgBoth)

#write_csv(infraestructuraPublicaComunas,"accesibilidad/tablas/infraestructuraPublicaComunas.csv")



#INFRAESTRUCTURA PUBLICA LONG

infraestructuraPublicaLong <-  gather(infraestructuraPublica, espacio, distancia, c(2:12), factor_key=TRUE)%>% 
                            select(-c(2,3,5:8))

infraestructuraPublicaComunasLong <-  gather(infraestructuraPublicaComunas, espacio, distancia, c(2:12), factor_key=TRUE) %>% select(-c(2:7))

#write_csv(infraestructuraPublicaLong,"accesibilidad/tablas/infraestructuraPublicaLong.csv")
#write_csv(infraestructuraPublicaComunasLong,"accesibilidad/tablas/infraestructuraPublicaComunasLong.csv")


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


#Cantidad de Espacios del Ministerio
espaciosCantidadCiudad <- espacios %>%as.data.frame() %>% select(-geom) %>%  filter(minCult==1) %>% 
                          group_by(BARRIO,COMUNA) %>%  count() %>% rename(cantidadCiudad=n)

#Variedad de Espacios del Ministerio
espaciosDistintosCiudad <- espacios %>% as.data.frame() %>% select(-geom) %>% filter(minCult==1) %>%   group_by(BARRIO) %>% 
                          summarise(distintosCiudad= n_distinct(FUNCION_PRINCIPAL))

#Joineo a la tabla en general
espaciosCantidad <- espaciosCantidad %>% left_join(espaciosCantidadCiudad, by="BARRIO") %>% select(-22) %>%
                                        left_join(espaciosDistintosCiudad,by="BARRIO")
rm(espaciosCantidadCiudad,espaciosDistintosCiudad)


#NACION 

#Cantidad de Espacios Nación
espaciosCantidadNacion <- espacios %>%as.data.frame() %>% select(-geom) %>%  filter(minCult==2) %>% 
  group_by(BARRIO,COMUNA) %>%  count() %>% rename(cantidadNacion=n)

#Variedad de Espacios de Nación
espaciosDistintosNacion <- espacios %>% as.data.frame() %>% select(-geom) %>% filter(minCult==2) %>%   group_by(BARRIO) %>% 
  summarise(distintosNacion= n_distinct(FUNCION_PRINCIPAL))

#Joineo a la tabla en general
espaciosCantidad <- espaciosCantidad %>% left_join(espaciosCantidadNacion, by="BARRIO") %>% left_join(espaciosDistintosNacion,by="BARRIO") %>% select(-24)
rm(espaciosCantidadNacion,espaciosDistintosNacion)


#PUBLICA GENERAL

#Cantidad de Espacios Publicos
espaciosCantidadPublicos <- espacios %>%as.data.frame() %>% select(-geom) %>%  filter(!is.na(minCult)) %>% 
  group_by(BARRIO,COMUNA) %>%  count() %>% rename(cantidadPublico=n)

#Variedad de Espacios de Nación
espaciosDistintosPublicos <- espacios %>% as.data.frame() %>% select(-geom) %>% filter(!is.na(minCult)) %>%   group_by(BARRIO) %>% 
  summarise(distintosPublicos= n_distinct(FUNCION_PRINCIPAL))

#Joineo a la tabla en general
espaciosCantidad <- espaciosCantidad %>% left_join(espaciosCantidadPublicos, by="BARRIO") %>% left_join(espaciosDistintosPublicos,by="BARRIO") %>% select(-26) 

rm(espaciosCantidadPublicos,espaciosDistintosPublicos)

espaciosCantidad <- espaciosCantidad%>% select(-c(2,17,21,23,25))
espaciosCantidad[is.na(espaciosCantidad)] <- 0

barrios <- espaciosBarriosDistancias %>% left_join(espaciosCantidad) %>% left_join(actividadesBarriosDistancias, by="BARRIO") %>% select(-38) %>% 
                                         left_join(infraestructuraPublica, by="BARRIO") %>% select(-65)


#write_csv(espaciosCantidad,"accesibilidad/tablas/espaciosCantidad.csv")
write_csv(barrios,"accesibilidad/tablas/barrios.csv")


#DEMOGRAFIA
poblacion <- read_csv("~/Documentos/osrm/v1/poblacion.csv")
poblacion1 <- str_replace(poblacion$BARRIOS,"LA BOCA","BOCA")
poblacion["BARRIOS"] <- poblacion1
rm(poblacion1)
poblacion <- poblacion[,c(3:5)]
poblacion <- poblacion %>% rename(BARRIO=BARRIOS)

espaciosCantidad <- espaciosCantidad %>% left_join(poblacion, by="BARRIO")

espaciosDemografia <- espacios %>% as.data.frame() %>% select(-geom) %>%  group_by(FUNCION_PRINCIPAL) %>% count()

demografia <- espaciosDemografia$n/3078836 *100000
espaciosDemografia["demografia"] <- demografia
rm(demografia)

#write_csv(espaciosDemografia,"accesibilidad/tablas/espaciosDemografia.csv")



#
barrios <- espaciosBarriosDistancias %>% left_join(espaciosCantidad)
#ACTIVIDADES

