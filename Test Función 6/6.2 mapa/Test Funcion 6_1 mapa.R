rm(list = ls())                                                     # Clear all variables
library(sivirep)
library(readxl)
Results_agrupar_mpio <- read_excel("Results agrupar_mpio.xlsx")

# Inicial ------------------------------------------------------------------
lista_eventos <- list_events()
municipios <- read.csv("codigos_municipios.csv")                    # Tabla de todos los municipios del pais y su dpto
municipios <- head(municipios,-5)                                   # Ignorar las últimas 5 filas porque no son útiles
nom_departamentos <- unique(municipios$Nombre.Departamento)         # Remover duplicados para obtener todos los departamentos
nom_departamentos <- chartr("ÁÉÍÓÚ", "AEIOU", nom_departamentos)    # Quitar tildes
nom_departamentos[3] <- "BOGOTA"                                    # Escribir Bogota corto
nom_departamentos[28] <- "SAN ANDRES"                               # Escribir San Andres corto
nom_departamentos[17] <- "NARINO"                                   # Escribir Nariño sin Ñ
cod_departamentos <- unique(municipios$Código.Departamento)

# Funciones para buscar errores --------------------------------------------
# 1. Try import
try_importdataevent <- function(desease,year) {
  tryCatch(
    {
      data_event <-  import_data_event(year = year, nombre_event = desease)
      list(data=data_event,res="Success",msg="Success importing")                     # Devolver como resultado los datos, "Success" y un mensaje las dimensiones del data frame
    },
    error = function(e){
      err_msg = paste("Error import_data_event",desease,year,": ",e)                         # Mensaje de error
      print(err_msg)
      list(data="",res="Error",msg=e)                                           # Devolver como resultado "Error" y el mensaje de error
    },
    warning = function(w){
      wrn_msg = paste("Warning import_data_event",desease,year,": ",w)                       # Mensaje de warning
      print(wrn_msg)
      list(data=data_event,res="Warning",msg=w)                                 # Devolver como resultado los datos,  "warning" y el mensaje de warning
    }
  )
}

# 2. Probar limpiar_data 
try_limpiardata <- function(data_event, desease, year){
  tryCatch(
    {
      data_event_limp <- limpiar_data_sivigila(data_event = data_event)
      list(data=data_event_limp,res="Success",msg="Success cleaning")                 # Devolver como resultado los datos, "Success" y un mensaje las dimensiones del data frame
    },
    error = function(e){
      err_msg = paste("Error limpiar_data_sivigila",desease,year,":",e)              # Mensaje de error
      print(err_msg)
      list(data="",res="Error",msg=e)                                           # Devolver como resultado "Error" y el mensaje de error
    },
    warning = function(w){
      wrn_msg = paste("Warning limpiar_data_sivigila ",desease,year,": ",w)                       # Mensaje de warning
      print(wrn_msg)
      list(data=data_event,res="Warning",msg=w)                                 # Devolver como resultado los datos,  "warning" y el mensaje de warning
    }
  )
}

# 3. Probar geo_filter 
try_geofilter <- function(data_event_limp, desease, year, dpt) {
  tryCatch(
    {
      data_event_filtrada <- geo_filtro(data_event_limp, dpto = dpt)     # Intentar correr geo_filtro
      result_dim <- dim(data_event_filtrada)                                          # Guardar las dimensiones (filas, columnas) del data frame resultante
      succ_msg <- paste("Success geo_filtro ",result_dim[1]," rows,",result_dim[2]," columns")
      list(data=data_event_filtrada, res="Success",msg=succ_msg)                                                # Devolver como resultado "Success" y un mensaje las dimensiones del data frame
    },
    error = function(e){
      err_msg = paste("Error geo_filtro ",desease,year,dpt,": ",e)                                   # Mensaje de error
      print(err_msg)
      list(data= "", res="Error",msg=e)                                                    # Devolver como resultado "Error" y el mensaje de error
    },
    warning = function(w){
      wrn_msg = paste("Warning geo_filtro ",desease,year,dpt,": ",w)                                 # Mensaje de warning
      print(wrn_msg)
      list(data=data_event_filtrada, res="Warning",msg=w)                                                 # Devolver como resultado "warning" y el mensaje de warning
    }
  )
}


# 4. Funcion para correr agrupar_mpio y que diga si hay error
try_agrupar_mpio<- function(data_event_filtrada, desease, year, dpto) {
  tryCatch(
    {
      dist_esp_dept <-  agrupar_mpio(data_event = data_event_filtrada,dpto=dpto)
      result_dim <- dim(dist_esp_dept)                                          # Guardar las dimensiones (filas, columnas) del data frame resultante
      succ_msg <- paste("Success",desease,year,dpto,":",result_dim[1]," rows,",result_dim[2]," columns")
      list(data=dist_esp_dept,res="Success",msg=succ_msg)                     # Devolver como resultado los datos, "Success" y un mensaje las dimensiones del data frame
    },
    error = function(e){
      err_msg = paste("Error agrupar_mpio",desease,year,dpto, ":",e)                         # Mensaje de error
      print(err_msg)
      list(data="",res="Error",msg=e)                                           # Devolver como resultado "Error" y el mensaje de error
    },
    warning = function(w){
      wrn_msg = paste("Warning agrupar_mpio",desease,year,dpto,":",w)                       # Mensaje de warning
      print(wrn_msg)
      list(data=dist_esp_dept,res="Warning",msg=w)                                 # Devolver como resultado los datos,  "warning" y el mensaje de warning
    }
  )
}

# 5. Funcion para correr agrupar_mpio y que diga si hay error
try_mapa <- function(dist_esp_dept, desease, year, dpto) {
  tryCatch(
    {
      mapa <-  plot_map(data_agrupada = dist_esp_dept, dpto=dpto, col_codigos='cod_mun_r')
      succ_msg <- paste("Success mapa",desease,year,dpto)
      print(succ_msg)
      list(res="Success",msg=succ_msg)                     # Devolver como resultado los datos, "Success" y un mensaje las dimensiones del data frame
    },
    error = function(e){
      err_msg = paste("Error mapa",desease,year,dpto, ":",e)                         # Mensaje de error
      print(err_msg)
      list(data="",res="Error",msg=e)                                           # Devolver como resultado "Error" y el mensaje de error
    },
    warning = function(w){
      wrn_msg = paste("Warning mapa",desease,year,dpto,":",w)                       # Mensaje de warning
      print(wrn_msg)
      list(res="Warning",msg=w)                                 # Devolver como resultado los datos,  "warning" y el mensaje de warning
    }
  )
}
excel <- read.csv("results5.csv", sep=";")                                      #Excel con los resultados de prueba.R
no_error <- excel[excel$Resultado != "Error",]                                  #Filtrar el Excel para tener solo las variables con Success o Warning
des_ano_no_error <-unique(no_error[,c("Desease", "Año", "Dpto")])                       #Combinaciones únicas de desease y año 


results_dist_esp_dept <- data.frame(matrix(ncol=6, nrow=0))                   # Crear el data frame de los resultados y nombrar las columnas
colnames(results_dist_esp_dept) <- c("Desease","Año","Departamento", "Resultado","Funcion donde se obtuvo el resultado","Mensaje")

#Inicio loop de todas las fila Results_agrupar_mpio_noerror
desease <- ""
year <- ""

Results_agrupar_mpio_noerror <- Results_agrupar_mpio[Results_agrupar_mpio$Resultado !="Error",]

for (i in 1:nrow(Results_agrupar_mpio_noerror)) {                                                                            
  newdesease <- Results_agrupar_mpio_noerror$Desease[i]
  newyear <- Results_agrupar_mpio_noerror$Año[i]
  
  #No importar si desease y year es el mismo a la iteración anterior
  if (newdesease!=desease | newyear!=year) {   # Si cualquiera de los dos cambió entra al if, si ambos son iguales se salta el if
    desease = newdesease
    year = newyear
    
    # Importar datos
    data_event_result <- try_importdataevent(desease, year)
    if (data_event_result$res == "Error"){  # Si se obtiene error guardar el resultado y avanzar al siguiente
      result_msg <- data_event_result$msg
      new_row <- list(desease,year,"","Error","import_data_event",result_msg)    # Crear la nueva fila para los resultados con el desase. año y resultado
      results_dist_esp_dept[nrow(results_dist_esp_dept) + 1,] <- new_row                    # Agregar la nueva fila a los resultados
      next    
    }
    data_event <- data_event_result$data
    
    # Limpiar
    limp <- try_limpiardata(data_event, desease, year)
    if (limp$res == "Error"){
      result_msg <- limp$msg
      new_row <- list(desease,year,"","Error","limpiar_data_sivigila",result_msg)    # Crear la nueva fila para los resultados con el desase. año y resultado
      results_dist_esp_dept[nrow(results_dist_esp_dept) + 1,] <- new_row                    # Agregar la nueva fila a los resultados
      next                                                      # Ignorar el resto del código y saltar al siguiente año
    }
    data_event_limp <- limp$data 
  }
 
  #Probar geofiltro
  dpto <- Results_agrupar_mpio_noerror$Departamento[i]
  geo <- try_geofilter(data_event_limp, desease, year,dpto)
  if (geo$res == "Error"){
    result_msg <- geo$msg
    new_row <- list(desease,year,dpto,"Error","geo_filtro",result_msg)    # Crear la nueva fila para los resultados con el desase. año y resultado
    results_dist_esp_dept[nrow(results_dist_esp_dept) + 1,] <- new_row                    # Agregar la nueva fila a los resultados
    next                                                      # Ignorar el resto del código y saltar al siguiente año
  }
  data_event_filtrada <- geo$data
  
  # Agrupar para el mapa
  probar_agrupar_mpio <- try_agrupar_mpio(data_event_filtrada, desease, year, dpto)
  if (probar_agrupar_mpio$res == "Error"){
    new_row <- list(desease, year, dpto, "agrupar_mpio", probar_agrupar_mpio$res, probar_agrupar_mpio$msg)          # Crear la nueva fila para los resultados con el desase, año y resultado
    results_dist_esp_dept[nrow(results_dist_esp_dept) + 1,] <- new_row                    # Agregar la nueva fila a los resultados
    next                                                      # Ignorar el resto del código y saltar al siguiente año
  }
  
  dist_esp_dept <- probar_agrupar_mpio$data
  
  # Probar el mapa
  probar_mapa <- try_mapa(dist_esp_dept, desease, year, dpto)
  new_row <- list(desease, year, dpto, "plot_mapa", probar_mapa$res, probar_mapa$msg)          # Crear la nueva fila para los resultados con el desase, año y resultado
  results_dist_esp_dept[nrow(results_dist_esp_dept) + 1,] <- new_row                    # Agregar la nueva fila a los resultados

}

#write_xlsx(results_dist_esp_dept, "Results plot_mapa.xlsx")

#df <- apply(results_dist_esp_dept,2,as.character)
#write.csv2(df,"Results agrupar_mpioFinal.CSV",row.names = FALSE, na='')