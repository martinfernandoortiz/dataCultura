library(sf)
library(tidyverse)
library(leaflet)
library(osrm)
library(htmltools)
setwd("~/Documentos/osrm")
options(osrm.server = "http://127.0.0.1:5000/")
    
    actividades1 <- st_read("actividades2019.gpkg")
    actividades1 <- actividades1 %>% mutate(idUnicoA= row_number())
    
    tipos <- actividades1 %>% as.data.frame() %>% group_by(DISCIPLINA_ARTISTICA) %>% count()
    tipos <- dplyr::pull(tipos, DISCIPLINA_ARTISTICA)
   tipos<- factor(tipos, levels = c(levels(tipos), "Formacion"))
    tipos[12] <- "Formacion"
    tipos2 <- list("a", "b","c","d","e","f","g","h","i","j","k" )
    tipos[12]
    tipos
    
    #CARGA DE INTERSECCIONES DE CALLES
    #COMO VOY A USAR DOS TIPOS DE ARCHIVOS (SF Y DATAFRAME) VOY A TENER INTERSECCIONES GEO  QUE ES SF Y EL OTRO QUE ES DF
    intersecciones1 <- st_read("intersecciones.gpkg")
    interseccionesGEO <- intersecciones1 #formato sf
    tablafinal <- intersecciones1 #esta tabla es para a lo último ponerle todas las distancias
    intersecciones <- st_point_on_surface(intersecciones1)
    
    b <- st_coordinates(intersecciones) #para sacar las coordenadas
    intersecciones["y"] <- b[,1] #campo de coordenadas y
    intersecciones["x"] <- b[,2] #campo de coordenadas x
    intersecciones <- intersecciones %>% as.data.frame() #transformo a DataFrame, osrm usa este formato
    intersecciones <- intersecciones[,c(4,6,7)] #selecciono campo id , x e y. 
    
    start <-Sys.time()
    for (u in 1:11){
      
      tipoactividades <- tipos[u]
      actividades <- actividades1 %>% filter(DISCIPLINA_ARTISTICA %in% (tipoactividades))
      actividadesGEO <- actividades
      a <- st_coordinates(actividades)
      
      actividades["y"] <- a[,1]
      actividades["x"] <- a[,2]
      actividades <- actividades[,c(44,45,46)]
      actividades <- actividades %>% as.data.frame()
      actividades <- actividades[,1:3]
      actividades <-  actividades %>% drop_na()
      
    
    
    
    #Función para seleccionar la distancia mínima. Esto me sirve para cuando uso el st_distance
    distancia1FUN <- function(x) {
      ave <- sort(x)[1]
      return(ave [1])
    }
    
    
    
    #################################################3
    # Distancia VUELO DE PAJARO
    # la razón de calcular las distancias a vuelo de pajaro es poder reducir los tiempos de procesamiento
    # en base a la distancia mínima máxima de cada espacio hacemos un buffer para filtrar actividades asi tarda menos
    
    distanciaMinima <- st_distance(interseccionesGEO, actividadesGEO)
    distanciaMinimaMaxima <- apply(distanciaMinima,1,distancia1FUN)
    round(distanciaMinimaMaxima,3)
    interseccionesGEO["tipoActividad"] <- round(distanciaMinimaMaxima,2)*2 # multiplique por dos para tener un changui
    interseccionesGEO <- interseccionesGEO %>% mutate(tipoActividad = if_else(tipoActividad<20,100,tipoActividad))
    
    # Esta tabla es la que voy a usar en el segundo loop. El que agrega fila por fila
    tablaDistancias <- data.frame(distancia=integer(),
                      idDestino= integer(),
                      lon=double(),
                      lat=double(),
                      id=character(),
                      rowOrden= integer()
                     )
    orden <- 0 # esto es  para ponerle el número de orden 
        
        for (i in 1:18237) {
          orden <- orden+1 #para generar el numero de row
          interseccionesGEO1 <- interseccionesGEO[i,] # filtro loop ya que calculo 1 x 1
          bufi <- st_transform(interseccionesGEO1, 3857) # transformo las intersecciones a 3857 para luego hacer el buffer
          bufi <- st_transform(st_buffer(bufi, bufi$tipoActividad),4326) # luego de hacer el buffer lo vuelvo a poner en 4326
          esp <- bufi %>% st_join(actividadesGEO, left = FALSE) # hago un st_join para ver que actividades son los que estan en el margen
          esp <- (actividades %>% right_join(esp, by = "idUnicoA"))[,1:3] #filtro esos actividades
          
          
          interseccionesLoop <- intersecciones[i,] #filtro la interseccion a trabajar (con formato  DF a diferencias de intersecciones GEO)
      distancetable <- osrmTable( src = interseccionesLoop, dst = esp, measure = "distance") # proceso en osrm
      
      -#genero formato tabla
      print(i)
      b <- distancetable$distances
      c <- apply(b, 2,min)
      prueba <- c %>% as.data.frame()
      prueba  <- prueba %>% mutate(da= as.numeric(row.names(prueba)))
      colnames(prueba) <- c('distancia', 'idDestino') 
      
      mindistances <- bind_cols(prueba, distancetable$destinations)
      mindistances <- mindistances[order(mindistances$distancia),][1,]
      mindistances <- mindistances %>% mutate(rowOrden= orden)
      
      #lo acoplo a la tabla vacia y a medida que loopea se llena la tabla
      tablaDistancias <- rbind(tablaDistancias, mindistances)
      
    }
    #end <- Sys.time()
    
    # esto es para cambiar 
    assign(paste0('distancia_',tipos2[u]),tablaDistancias )
    
    }
    end <- Sys.time()
    start-end


assign(paste0('distancia_',"museo"),tablaDistancias )


# Creo las tablas por separado por si quiero saber las coordenadas
tablafinal["escenicas"] <- distancia_a$distancia 
tablafinal["visuales"] <- distancia_b$distancia 
tablafinal["cine"] <- distancia_c$distancia 
tablafinal["tradicional"] <- distancia_d$distancia 
tablafinal["diseno"] <- distancia_e$distancia 
tablafinal["literatura"] <- distancia_f$distancia 
tablafinal["mixto"] <- distancia_g$distancia 
tablafinal["musica"] <- distancia_h$distancia 
tablafinal["naa"] <- distancia_i$distancia 
tablafinal["otros"] <- distancia_j$distancia 
tablafinal["inmaterial"] <- distancia_k$distancia 

tipos

#Guardo las tablas
write_csv(distancia_a, "distanciasescenicas.csv")
write_csv(distancia_b, "distancia_visuales.csv")
write_csv(distancia_c, "distancia_cine.csv")
write_csv(distancia_d, "distancia_tradicional.csv")
write_csv(distancia_e, "distancia_diseno.csv")
write_csv(distancia_f, "distancia_literatura.csv")
write_csv(distancia_g, "distancia_mixto.csv")
write_csv(distancia_h, "distancia_musica.csv")
write_csv(distancia_i, "distancia_baa.csv")
write_csv(distancia_j, "distancia_otros.csv")
write_csv(distancia_k, "distancia_inmaterial.csv")



##############################################################################################



# MINIMOS
minimos <- tablafinal %>% as.data.frame()
minimos <- minimos[,6:16]
minimos <- apply(minimos, 1, min)
tablafinal["minimo"] <- minimos

promediocompuesto <- tablafinal%>% as.data.frame()
promediocompuesto <- promediocompuesto[,c(6,7,8,11,13)] 
promediocompuesto <- apply(promediocompuesto, 1, mean)

tablafinal["promediocompuesto"] <- promediocompuesto

tablafinalBarriosActividades <- tablafinal %>% as.data.frame()
tablafinalBarriosActividades <- tablafinalBarriosActividades %>% group_by(BARRIO_3) %>% summarise_at(vars(c("escenicas":"promediocompuesto")), mean)
names(tablafinalBarriosActividades)[1] <- "BARRIO"
mean(tablafinalBarriosActividades$minimo)

barrios <- st_read("barrios.gpkg", layer= "barrios4326")
barrios <- barrios %>% left_join(tablafinalBarriosActividades, by= "BARRIO")
st_write(barrios,overwrite = TRUE, delete_dsn = TRUE, "barriosFinalActividades.gpkg")
st_write(tablafinal, overwrite = TRUE, delete_dsn = TRUE,"tablaFinalActividades.gpkg")
write_csv(tablafinal, "tablafinalActividades.csv")
write_csv(barrios, "tablafinalbarriosActividades.csv")




############### SE AGREGO LO DE FORMACION

library(readr)
tablafinalActividades <- read_csv("tablafinalActividades.csv")
tablafinalActividades["formacion"] <- tablaDistancias$distancia 

tablafinalBarriosActividades
#
# MINIMOS
minimos <- tablafinalActividades %>% as.data.frame()
minimos <- minimos[,c(6:13,15,16,19)]
minimos <- apply(minimos, 1, min)
tablafinalActividades["minimo"] <- minimos

promediocompuesto <- tablafinalActividades%>% as.data.frame()
promediocompuesto <- promediocompuesto[,c(6,7,8,11,13,19)] 
promediocompuesto <- apply(promediocompuesto, 1, mean)

tablafinalActividades["promediocompuesto"] <- promediocompuesto



tablafinalBarriosActividades <- tablafinalActividades %>% as.data.frame()
tablafinalBarriosActividades <- tablafinalBarriosActividades %>% group_by(BARRIO_3) %>% summarise_at(vars(c("escenicas":"formacion")), mean)
names(tablafinalBarriosActividades)[1] <- "BARRIO"
mean(tablafinalBarriosActividades$minimo)

barrios <- st_read("barrios.gpkg", layer= "barrios4326")
barrios <- barrios %>% left_join(tablafinalBarriosActividades, by= "BARRIO")
st_write(barrios,overwrite = TRUE, delete_dsn = TRUE, "barriosFinalActividades.gpkg")
st_write(tablafinalActividades, overwrite = TRUE, delete_dsn = TRUE,"tablaFinalActividades.gpkg")
write_csv(tablafinalActividades, "tablafinalActividades.csv")
write_csv(barrios, "tablafinalbarriosActividades.csv")

write_xlsx(barrios, "tablafinalbarriosActividades.xlsx")



###################### FORMATO LONG



tablafinalbarriosActividadesLONG <-  gather(tablafinalBarriosActividades, espacio, distancia, c(2:9, 11:12,15), factor_key=TRUE)
tablafinalbarriosActividadesLONG <- tablafinalbarriosActividadesLONG %>% mutate(gestion= "Otra")
tablafinalbarriosActividadesLONG <- tablafinalbarriosActividadesLONG %>% 
  mutate(gestion= if_else(
    str_sub(espacio,-3,)=="Min","Ministerio","Otra"))
tablafinalbarriosActividadesLONG <- tablafinalbarriosActividadesLONG %>%
                              mutate(espacio= case_when( espacio == "escenicas" ~ tipos[1],
                             espacio == "visuales" ~tipos[2],
                             espacio == "cine" ~tipos[3],
                             espacio == "tradicional" ~tipos[4],
                             espacio == "diseno" ~tipos[5],
                             espacio == "literatura" ~tipos[6],
                             espacio == "mixto" ~tipos[7],
                             espacio == "musica" ~tipos[8],
                             espacio == "naa" ~ tipos[9],
                             espacio == "otros" ~tipos[10],
                             espacio == "inmaterial" ~tipos[11],
                             espacio == "formacion" ~ tipos[12]))

install.packages("writexl")
library(writexl)
write_xlsx(tablafinalbarriosActividadesLONG, "tablafinalbarriosActividadesLONG.xlsx")
