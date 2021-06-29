#Data Wrangling para hacer espacios long con las funciones secundarias


library(sf)
library(tidyverse)
library(leaflet)
library(osrm)
library(htmltools)
setwd("~/Documentos/osrm/v1")
options(osrm.server = "http://127.0.0.1:5000/")

espacios <- st_read("espacios28042021.gpkg")


#espacios <- espacios[,c(1:5)]

#espacios <- espacios %>% as.data.frame()
#espacios <- espacios[,1:5]

espacios <- espacios %>% mutate(FUNCION_SECUNDARIA= as.character(FUNCION_SECUNDARIA))
espacios <- espacios %>% mutate(FUNCION_SECUNDARIA= if_else(str_sub(ESTABLECIMIENTO,1,10)=="NAVE AGORA","1",FUNCION_SECUNDARIA))

#CONTEO DE CUANTOS ROWS HAY QUE AGREGAR
espacios <- espacios %>% mutate(comas= str_count(FUNCION_SECUNDARIA,",")) #cuento comas
espacios <- espacios %>% mutate(FUNCION_SECUNDARIA= if_else(comas==0,paste(FUNCION_SECUNDARIA,",1",sep=""),FUNCION_SECUNDARIA))

#AGREGO ROWS
espaciosLong <- cSplit(espacios, "FUNCION_SECUNDARIA", ",", "long")

#ELIMINO AQUELLOS REGISTROS QUE NO SIRVEN
espaciosLong<-subset(espaciosLong, FUNCION_SECUNDARIA!="1")
espaciosLong<-subset(espaciosLong, FUNCION_SECUNDARIA!="BAR")
espaciosLong<-subset(espaciosLong, FUNCION_SECUNDARIA!="CINEDEBATE")
espaciosLong<-subset(espaciosLong, FUNCION_SECUNDARIA!="RADIO")



#ELIMINO EL CAMPO COMAS
#espaciosLong <- espaciosLong[,-6]
#espacios <- espacios[,-6]
espaciosLong <- espaciosLong[,-54]
espacios <- espacios[,-54]

#ACTUALIZO LA FUNCION PRINCIPAL DE LONG
espaciosLong <- espaciosLong %>% mutate(FUNCION_PRINCIPAL=FUNCION_SECUNDARIA, FUNCION_SECUNDARIA="",SUBCATEGORIA="")


 espaciosLong %>% group_by(FUNCION_PRINCIPAL) %>% count()

 espaciosLong <- espaciosLong %>% mutate(SUBCATEGORIA= case_when(
   FUNCION_PRINCIPAL=='CLUB DE MUSICA' ~ 'CLUB DE MUSICA EN VIVO',
   FUNCION_PRINCIPAL=='CLUB DE MUSICA EN VIVO' ~ 'CLUB DE MUSICA EN VIVO',
   FUNCION_PRINCIPAL=='GALERIA DE ARTE' ~ 'GALERIA DE ARTE',
   FUNCION_PRINCIPAL=='MUSEO' ~ 'MUSEO',
   FUNCION_PRINCIPAL=='SALA DE EXPOSICION' ~ 'GALERIA DE ARTE',
   FUNCION_PRINCIPAL=='MILONGA Y/O TANGUERIA' ~ 'MILONGA Y/O TANGUERIA',
   FUNCION_PRINCIPAL=='CINEDEBATE' ~ 'SALA DE CINE'
  
 ))
 
 #VAMOS A HACER CLUB DE MUSICA Y SALA DE TEATRO TODOS LOS ESPACIOS ESCENICOS
 espaciosLong <- espaciosLong %>% mutate(FUNCION_SECUNDARIA= if_else(
   FUNCION_PRINCIPAL=='ESPACIO ESCENICO',
   "CLUB DE MUSICA EN VIVO, SALA DE TEATRO",
   "1")) #UNO SON LOS ESPACIOS DE MUSICA CON FUNCION SECUNDARIA
 
 espaciosLong <- cSplit(espaciosLong, "FUNCION_SECUNDARIA", ",", "long")
 
 espaciosLong <- espaciosLong %>% mutate(FUNCION_SECUNDARIA= as.character(FUNCION_SECUNDARIA))
 espaciosLong <- espaciosLong %>% mutate(SUBCATEGORIA= if_else(
   FUNCION_SECUNDARIA!='1',
   FUNCION_SECUNDARIA,
   SUBCATEGORIA))
 
 
 espaciosLong <- espaciosLong %>% mutate(FUNCION_SECUNDARIA= if_else(FUNCION_SECUNDARIA=='1',"1", "2")) 
 #LOS VALORES QUE SON 2 son aquellos que son dudosos ya que no 
 # sabemos con exactitud si son club de musica o sala de teatro
 
 espaciosLong <- espaciosLong %>% mutate(FUNCION_PRINCIPAL=as.character(FUNCION_PRINCIPAL))
 espaciosLong <- espaciosLong %>% mutate(FUNCION_PRINCIPAL= case_when(
   FUNCION_PRINCIPAL=='CLUB DE MUSICA' ~ 'ESPACIO ESCENICO',
   FUNCION_PRINCIPAL=='CLUB DE MUSICA EN VIVO' ~ 'ESPACIO ESCENICO',
   FUNCION_PRINCIPAL=='GALERIA DE ARTE' ~ 'ESPACIO DE EXHIBICION',
   FUNCION_PRINCIPAL=='MUSEO' ~ 'ESPACIO DE EXHIBICION',
   FUNCION_PRINCIPAL=='SALA DE EXPOSICION' ~ 'ESPACIO DE EXHIBICION',
   FUNCION_PRINCIPAL=='MILONGA Y/O TANGUERIA' ~ 'BAR',
   FUNCION_PRINCIPAL=='ESPACIO DE FORMACIÓN' ~ 'ESPACIO DE FORMACION',
   FUNCION_PRINCIPAL=='ESPACIO ESCÉNICO' ~ 'ESPACIO ESCENICO',
   FUNCION_PRINCIPAL=='SALA DE TEATRO' ~ 'ESPACIO ESCENICO',
   TRUE ~ FUNCION_PRINCIPAL
    ))
 
#ARREGLO SUBCATEGORIAS MOCHAS DE LA BASE ORIGINAL
 espacios  <- espacios %>% mutate(SUBCATEGORIA= as.character(SUBCATEGORIA))
 espacios <- espacios %>% mutate(SUBCATEGORIA= case_when(
   SUBCATEGORIA=='CLUB DE MÚSICA' ~ 'CLUB DE MUSICA EN VIVO',
   SUBCATEGORIA=='TEATRO' ~ 'SALA DE TEATRO',
   TRUE ~ SUBCATEGORIA
   
 ))
 
 

 comunas <- str_replace(espacios$COMUNA,"COMUNA 5","COMUNA 05")
 espacios["COMUNA"] <- comunas
 rm(comunas)
 
 galerias <- str_replace(espacios$SUBCATEGORIA,"GALERÍA DE ARTE","GALERIA DE ARTE")
 espacios["SUBCATEGORIA"] <- galerias
 rm(galerias)
 
 
 feria <- str_replace(espacios$FUNCION_PRINCIPAL,"FERIA","ESPACIO FERIAL")
 espacios["FUNCION_PRINCIPAL"] <- feria
 rm(feria)
 
 #rbin funciona si es un data frame sin geo
 #espacios <- rbind(espacios,espaciosLong) 
 
 espacios <- union(espacios, espaciosLong)
 st_write(espacios, "espaciosLong.gpkg")
 
 ################################################             HASTA ACA EL DATA WRANGLING!!!!! ######################
 
