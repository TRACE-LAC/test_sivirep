rm(list = ls())                                                     # Clear all variables
library(sivirep)
library(writexl)  # install.packages("writexl")

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
#Ty import
try_importdataevent <- function(desease,year) {
  tryCatch(
    {
      data_event <-  import_data_event(year = year, nombre_event = desease)
      list(data=data_event,res="Success",msg="Success importing")                     # Devolver como resultado los datos, "Success" y un mensaje las dimensiones del data frame
    },
    error = function(e){
      err_msg = paste("Error import_data_event",desease,year,": ",e)                         # Mensaje de error
      print(err_msg)
      list(data="",res="Error",msg=err_msg)                                           # Devolver como resultado "Error" y el mensaje de error
    },
    warning = function(w){
      wrn_msg = paste("Warning import_data_event",desease,year,": ",w)                       # Mensaje de warning
      print(wrn_msg)
      list(data=data_event,res="Warning",msg=wrn_msg)                                 # Devolver como resultado los datos,  "warning" y el mensaje de warning
    }
  )
}

# Probar limpiar_data 
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
      wrn_msg = paste("Warning limpiar_data_sivigila ",desease,year,":",w)                       # Mensaje de warning
      print(wrn_msg)
      list(data=data_event,res="Warning",msg=w)                                 # Devolver como resultado los datos,  "warning" y el mensaje de warning
    }
  )
}

# 1. Funcion para correr agrupar_fecha_inisintomas y que diga si hay error
try_fechainisintomas <- function (data_event_limp, desease, year) {
  tryCatch(
    {
      casos_ini_sintomas <-  agrupar_fecha_inisintomas(data_event = data_event_limp)
      result_dim <- dim(casos_ini_sintomas)                                          # Guardar las dimensiones (filas, columnas) del data frame resultante
      succ_msg <- paste("Success",desease,year,":",result_dim[1]," rows,",result_dim[2]," columns")
      print(succ_msg)
      list(data=casos_ini_sintomas,res="Success",msg=succ_msg)                     # Devolver como resultado los datos, "Success" y un mensaje las dimensiones del data frame
    },
    error = function(e){
      err_msg = paste("Error grouping ",desease,year,": ",e)                         # Mensaje de error
      print(err_msg)
      list(data="",res="Error",msg=e)                                           # Devolver como resultado "Error" y el mensaje de error
    },
    warning = function(w){
      wrn_msg = paste("Warning grouping ",desease,year,": ",w)                       # Mensaje de warning
      print(wrn_msg)
      list(data=casos_ini_sintomas,res="Warning",msg=w)                                 # Devolver como resultado los datos,  "warning" y el mensaje de warning
    }
  )
}


results_inisin_fecha <- data.frame(matrix(ncol=5, nrow=0))                   # Crear el data frame de los resultados y nombrar las columnas
colnames(results_inisin_fecha) <- c("Desease","Año","Resultado", "Funcion donde se obtuve el resultado","Mensaje")

n_deseases <- nrow(lista_eventos)                               # Cantidad de enfermedades en lista_eventos
for (i in 1:n_deseases) {                                       # Iterar i desde 1 hasta la cantidad de enfermedades
  # print(lista_eventos$enfermedad[i])
  
  desease <- lista_eventos$enfermedad[i]                        # Desease de la iteración i
  years <- strsplit(lista_eventos$aa[i],", ")[[1]]              # Todos los años del desease (como str)
  
  for (year in years) {                                         # Iterar sobre todos los años del desease
    year <- strtoi(year)   
  
    # Importar datos
    data_event_result <- try_importdataevent(desease, year)
    if (data_event_result$res == "Error"){
      new_row <- list(desease,year,"Error","import_data_sivigila",data_event_result$msg)    # Crear la nueva fila para los resultados con el desase. año y resultado
      results_inisin_fecha[nrow(results_inisin_fecha) + 1,] <- new_row                    # Agregar la nueva fila a los resultados
      next    
    }
    data_event <- data_event_result$data
    
    
    # Limpiar
    limp <- try_limpiardata(data_event = data_event, desease, year)
    if (limp$res == "Error"){
      new_row <- list(desease,year,"Error","limpiar_data_sivigila",limp$msg)    # Crear la nueva fila para los resultados con el desase. año y resultado
      results_inisin_fecha[nrow(results_inisin_fecha) + 1,] <- new_row                    # Agregar la nueva fila a los resultados
      next                                                      # Ignorar el resto del código y saltar al siguiente año
    }
    data_event_limp <- limp$data
    
    # probar la función
    fechainisint <- try_fechainisintomas(data_event_limp, desease, year)
    result <- fechainisint$res
    result_msg <- fechainisint$msg
    new_row <- list(desease,year,result,"agrupar_fecha_inisintomas",result_msg)          # Crear la nueva fila para los resultados con el desase, año y resultado
    results_inisin_fecha[nrow(results_inisin_fecha) + 1,] <- new_row                    # Agregar la nueva fila a los resultados
  }
}

#write_xlsx(results_inisin_fecha, "Results agrupar_fecha_inisintomas.xlsx")

df <- apply(results_inisin_fecha,2,as.character)
write.csv2(df,"Results agrupar_fecha_inisintomas.CSV",row.names = FALSE, na='')
