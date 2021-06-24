library(sf)
library(tidyverse)
library(leaflet)
library(osrm)
library(htmltools)
setwd("~/Documentos/osrm/v1")
options(osrm.server = "http://127.0.0.1:5000/")


espacios <- st_read("espaciosLong.gpkg")
espacios <- espacios %>% mutate(id_Long=row_number())

 #### AHORA EMPIEZA ACCESIBILIDAD !!!!!!!!!!!!!!!!! #####################
 
 
 
 
 
espacioscorto <- espacios %>% filter(!FUNCION_PRINCIPAL %in% c('MONUMENTOS Y LUGARES HISTORICOS', 'CALESITA'))
espacios1 <- espacioscorto
tipos <- espacioscorto %>% as.data.frame() %>% group_by(FUNCION_PRINCIPAL) %>% count()
tipos <- dplyr::pull(tipos, FUNCION_PRINCIPAL)



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
for (u in 2:2){
  
  tipoactividades <- tipos[1]
  espacios <- espacios1 %>% filter(FUNCION_PRINCIPAL %in% (tipoactividades))
  espaciosGEO <- espacios
  a <- st_coordinates(espaciosGEO)
  
  espacios["y"] <- a[,1]
  espacios["x"] <- a[,2]
  espacios <- espacios[,c(54:56)]
  espacios <- espacios %>% as.data.frame()
  espacios <- espacios[,1:3]
  
  
  
  
  #Función para seleccionar la distancia mínima. Esto me sirve para cuando uso el st_distance
  distancia1FUN <- function(x) {
    ave <- sort(x)[1]
    return(ave [1])
  }
  
  
  
  #################################################3
  # Distancia VUELO DE PAJARO
  # la razón de calcular las distancias a vuelo de pajaro es poder reducir los tiempos de procesamiento
  # en base a la distancia mínima máxima de cada espacio hacemos un buffer para filtrar actividades asi tarda menos
  
  distanciaMinima <- st_distance(interseccionesGEO, espaciosGEO)
  distanciaMinimaMaxima <- apply(distanciaMinima,1,distancia1FUN)
  round(distanciaMinimaMaxima,3)
  interseccionesGEO["tipoEspacio"] <- round(distanciaMinimaMaxima,2)*2 # multiplique por dos para tener un changui
  interseccionesGEO <- interseccionesGEO %>% mutate(tipoEspacio = if_else(tipoEspacio<20,100,tipoEspacio))
  
  
  library(sf)
  library(tidyverse)
  library(leaflet)
  library(osrm)
  library(htmltools)
  setwd("~/Documentos/osrm/v1")
  options(osrm.server = "http://127.0.0.1:5000/")
  
  espacios <- st_read("espaciosLong.gpkg")
  espacios <- espacios %>% mutate(id_Long=row_number())
  
  
  espacios <- espacios %>% filter(!id_Long %in%
                                    c(3207,3249,3267,
                                      3280,3299,3315,
                                      3323,3363,3366,3476,
                                      3481,3485,3558,3563,3573,
                                      3594,3599,3679,3659,3654,3606,3596,3220,3225,3233,
                                      3270,3290,3334,3337,3341,3378,3379,3395,3396,3401,3411,3416,3417,
                                      3442,3443,3452,3471,3490,3503,3504,3506,3507,3510,3513,
                                      3517,3518,3521,3550,3567,
                                      3583,3591,3596,3604,3605,3606,
                                      3612,3624,3626,3627,3637,3638,
                                      3654,3671,3682
                                    ) )
  
  espacios <- espacios %>% filter(ESTABLECIMIENTO !='LEON LEON')
  espacios <- espacios %>% filter(ESTABLECIMIENTO !='PAPRIKA')
  
  
  #BORRAR ESPACIO PAPRIKA QUE CERRO
  
  espacios <- espacios1
  #
  
  write_csv(espacios1, "espaciosLongBorrar.csv")
  #### AHORA EMPIEZA ACCESIBILIDAD !!!!!!!!!!!!!!!!! #####################
  
  
  
  
  
  espacioscorto <- espacios %>% filter(!FUNCION_PRINCIPAL %in% c('MONUMENTOS Y LUGARES HISTORICOS', 'CALESITA'))
  espacios1 <- espacioscorto
  tipos <- espacioscorto %>% as.data.frame() %>% group_by(FUNCION_PRINCIPAL) %>% count()
  tipos <- dplyr::pull(tipos, FUNCION_PRINCIPAL)
  
  tipos
  tipos 
  2,3,5,6,10,11
  
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
  for (u in c(7)){
    
    tipoactividades <- tipos[u]
    espacios <- espacios1 %>% filter(FUNCION_PRINCIPAL %in% (tipoactividades))
    espaciosGEO <- espacios
    a <- st_coordinates(espaciosGEO)
    
    espacios["y"] <- a[,1]
    espacios["x"] <- a[,2]
    espacios <- espacios[,c(54:56)]
    espacios <- espacios %>% as.data.frame()
    espacios <- espacios[,1:3]
    
    
    
    
    #Función para seleccionar la distancia mínima. Esto me sirve para cuando uso el st_distance
    distancia1FUN <- function(x) {
      ave <- sort(x)[1]
      return(ave [1])
    }
    
    
    
    #################################################3
    # Distancia VUELO DE PAJARO
    # la razón de calcular las distancias a vuelo de pajaro es poder reducir los tiempos de procesamiento
    # en base a la distancia mínima máxima de cada espacio hacemos un buffer para filtrar actividades asi tarda menos
    
    distanciaMinima <- st_distance(interseccionesGEO, espaciosGEO)
    distanciaMinimaMaxima <- apply(distanciaMinima,1,distancia1FUN)
    round(distanciaMinimaMaxima,3)
    interseccionesGEO["tipoEspacio"] <- round(distanciaMinimaMaxima,2)*2 # multiplique por dos para tener un changui
    interseccionesGEO <- interseccionesGEO %>% mutate(tipoEspacio = if_else(tipoEspacio<20,100,tipoEspacio))
    
    
    
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
      bufi <- st_transform(interseccionesGEO1, 3857) # transformo las intersecciones a 8333 (https://spatialreference.org/ref/sr-org/7433/) para luego hacer el buffer
      bufi <- st_transform(st_buffer(bufi, bufi$tipoEspacio),4326) # luego de hacer el buffer lo vuelvo a poner en 4326
      esp <- bufi %>% st_join(espaciosGEO, left = FALSE) # hago un st_join para ver que actividades son los que estan en el margen
      esp <- (espacios %>% right_join(esp, by = "id_Long"))[,1:3] #filtro esos actividades
      
      
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
    assign(paste0('distancia_',tipos[u]),tablaDistancias )
    
  }
  end <- Sys.time()
  start-end
  
  write_csv(distancia_BAR, "tablas/distancia_BAR.csv")
  write_csv(distancia_BIBLIOTECA, "tablas/distancia_BIBLIOTECA.csv")
  write_csv(`distancia_CENTRO CULTURAL`, "tablas/distancia_CC.csv")
  write_csv(`distancia_ESPACIO DE EXHIBICION`, "tablas/distancia_EXH.csv")
  write_csv(`distancia_ESPACIO DE FORMACION`,"tablas/distancia_FOR.csv")
  write_csv(distancia_LIBRERIA, "tablas/distancia_LIB.csv")
  write_csv(`distancia_SALA DE CINE`, "tablas/distancia_CINE.csv")
  write_csv(`distancia_ESPACIO ESCENICO`, "tablas/distancia_ESCENICO.csv")
  
  
  
  
  
  
  ############### PARA SUBCATEGORIAS
  
  
  tipos <- espacios1 %>%as.data.frame() %>% filter(SUBCATEGORIA %in% c('CLUB DE MUSICA EN VIVO', 'SALA DE TEATRO', 'MUSEO')) %>% 
    group_by(SUBCATEGORIA) %>% count() 
  tipos
  tipos <- dplyr::pull(tipos, SUBCATEGORIA)
  
  tipos[4]
  tipos 
  2,3,5,6,10,11
  
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
  for (u in 1:3){
    
    tipoactividades <- tipos[u]
    espacios <- espacios1 %>% filter(SUBCATEGORIA %in% (tipoactividades))
    espaciosGEO <- espacios
    a <- st_coordinates(espaciosGEO)
    
    espacios["y"] <- a[,1]
    espacios["x"] <- a[,2]
    espacios <- espacios[,c(54:56)]
    espacios <- espacios %>% as.data.frame()
    espacios <- espacios[,1:3]
    
    
    
    
    #Función para seleccionar la distancia mínima. Esto me sirve para cuando uso el st_distance
    distancia1FUN <- function(x) {
      ave <- sort(x)[1]
      return(ave [1])
    }
    
    
    
    #################################################3
    # Distancia VUELO DE PAJARO
    # la razón de calcular las distancias a vuelo de pajaro es poder reducir los tiempos de procesamiento
    # en base a la distancia mínima máxima de cada espacio hacemos un buffer para filtrar actividades asi tarda menos
    
    distanciaMinima <- st_distance(interseccionesGEO, espaciosGEO)
    distanciaMinimaMaxima <- apply(distanciaMinima,1,distancia1FUN)
    round(distanciaMinimaMaxima,3)
    interseccionesGEO["tipoEspacio"] <- round(distanciaMinimaMaxima,2)*2 # multiplique por dos para tener un changui
    interseccionesGEO <- interseccionesGEO %>% mutate(tipoEspacio = if_else(tipoEspacio<20,100,tipoEspacio))
    
    
    
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
      bufi <- st_transform(interseccionesGEO1, 3857) # transformo las intersecciones a 8333 (https://spatialreference.org/ref/sr-org/7433/) para luego hacer el buffer
      bufi <- st_transform(st_buffer(bufi, bufi$tipoEspacio),4326) # luego de hacer el buffer lo vuelvo a poner en 4326
      esp <- bufi %>% st_join(espaciosGEO, left = FALSE) # hago un st_join para ver que actividades son los que estan en el margen
      esp <- (espacios %>% right_join(esp, by = "id_Long"))[,1:3] #filtro esos actividades
      
      
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
    assign(paste0('distancia_',tipos[u]),tablaDistancias )
    
  }
  end <- Sys.time()
  start-end
  
  
  write_csv(`distancia_SALA DE TEATRO`, "tablas/distancia_TEATRO.csv")
  write_csv(`distancia_CLUB DE MUSICA EN VIVO`, "tablas/distancia_MUSICA.csv")
  write_csv(distancia_MUSEO, "tablas/distancia_MUSEO.csv")
  
  write_csv(distancia_BAR, "tablas/distancia_BAR.csv")
  write_csv(distancia_BIBLIOTECA, "tablas/distancia_BIBLIOTECA.csv")
  write_csv(`distancia_CENTRO CULTURAL`, "tablas/distancia_CC.csv")
  write_csv(`distancia_ESPACIO DE EXHIBICION`, "tablas/distancia_EXH.csv")
  write_csv(`distancia_ESPACIO DE FORMACION`,"tablas/distancia_FOR.csv")
  write_csv(distancia_LIBRERIA, "tablas/distancia_LIB.csv")
  write_csv(`distancia_SALA DE CINE`, "tablas/distancia_CINE.csv")
  write_csv(`distancia_ESPACIO ESCENICO`, "tablas/distancia_ESCENICO.csv")
  
  
  
  
  # Creo las tablas por separado por si quiero saber las coordenadas
  tablafinal["bar"] <- distancia_BAR$distancia 
  tablafinal["biblioteca"] <- distancia_BIBLIOTECA$distancia 
  tablafinal["centroCultural"] <- `distancia_CENTRO CULTURAL`$distancia 
  distancia_disque <- read_csv("~/Documentos/osrm/distancia_disque.csv")
  tablafinal["disqueria"] <- distancia_disque$distancia 
  tablafinal["escenico"] <- `distancia_ESPACIO ESCENICO`$distancia 
  tablafinal["exhibicion"] <- `distancia_ESPACIO DE EXHIBICION`$distancia 
  tablafinal["formacion"] <- `distancia_ESPACIO DE FORMACION`$distancia 
  tablafinal["libreria"] <- distancia_LIBRERIA$distancia 
  tablafinal["cine"] <- `distancia_SALA DE CINE`$distancia 
  tablafinal["teatro"] <- `distancia_SALA DE TEATRO`$distancia 
  tablafinal["musica"] <- `distancia_CLUB DE MUSICA EN VIVO`$distancia 
  tablafinal["museo"] <- distancia_MUSEO$distancia 
  
  tablafinal
  
  minimos <- tablafinal %>% as.data.frame()
  minimos <- minimos[,c(6:12,16,17)]
  minimos <- apply(minimos, 1, min)
  tablafinal["minimo"] <- minimos
  
  
  promediocompuesto <- tablafinal%>% as.data.frame()
  promediocompuesto <- promediocompuesto[,c(7,8,12:15,17)] 
  promediocompuesto <- apply(promediocompuesto, 1, mean)
  
  tablafinal["promediocompuesto"] <- promediocompuesto
  write_csv(tablafinal, "final/intersecciones.csv")
  
  tablafinalBarrios <- tablafinal %>% as.data.frame()
  tablafinalBarrios <- tablafinalBarrios %>% group_by(BARRIO_3) %>% summarise_at(vars(c("teatro":"escenicoMin")), mean)
  names(tablafinalBarrios)[1] <- "BARRIO"
  
  barrios <- st_read("barrios.gpkg", layer= "barrios4326")
  barrios <- barrios %>% left_join(tablafinalBarrios, by= "BARRIO")
  
