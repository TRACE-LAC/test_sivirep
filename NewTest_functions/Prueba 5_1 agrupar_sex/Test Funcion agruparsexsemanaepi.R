library(sivirep)

# Inicial ------------------------------------------------------------------
lista_eventos <- list_events()
municipios <- read.csv("codigos_municipios.csv")                    # Tabla de todos los municipios del pais y su dpto
municipios <- head(municipios,-5)                                   # Ignorar las últimas 5 filas porque no son útiles
nom_departamentos <- unique(municipios$Nombre.Departamento)         # Remover duplicados para obtener todos los departamentos
nom_departamentos <- chartr("ÁÉÍÓÚ", "AEIOU", nom_departamentos)    # Quitar tildes
nom_departamentos[3] <- "BOGOTA"                                    # Escribir Bogota corto
nom_departamentos[28] <- "SAN ANDRES"                               # Escribir San Andres corto
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
      list(data="",res="Error",msg=e)                                           # Devolver como resultado "Error" y el mensaje de error
    },
    warning = function(w){
      wrn_msg = paste("Warning import_data_event",desease,year,": ",w)                       # Mensaje de warning
      print(wrn_msg)
      list(data=data_event,res="Warning",msg=w)                                 # Devolver como resultado los datos,  "warning" y el mensaje de warning
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
      wrn_msg = paste("Warning limpiar_data_sivigila ",desease,year,": ",w)                       # Mensaje de warning
      print(wrn_msg)
      list(data=data_event,res="Warning",msg=w)                                 # Devolver como resultado los datos,  "warning" y el mensaje de warning
    }
  )
}

# 1. Funcion para correr agrupar_sex_semanaepi y que diga si hay error
try_agrupar_sex_semanaepi <- function (data_event_limp, desease, year) {
  tryCatch(
    {
      casos_sex_semanaepi <-  agrupar_sex_semanaepi(data_event = data_event_limp)
      result_dim <- dim(casos_sex_semanaepi)                                          # Guardar las dimensiones (filas, columnas) del data frame resultante
      succ_msg <- paste("Success",desease,year,":",result_dim[1]," rows,",result_dim[2]," columns")
      print(succ_msg)
      list(data=casos_sex_semanaepi,res="Success",msg=succ_msg)                     # Devolver como resultado los datos, "Success" y un mensaje las dimensiones del data frame
    },
    error = function(e){
      err_msg = paste("Error agrupar_sex_semanaepi",desease,year,": ",e)                         # Mensaje de error
      print(err_msg)
      list(data="",res="Error",msg=e)                                           # Devolver como resultado "Error" y el mensaje de error
    },
    warning = function(w){
      wrn_msg = paste("Warning agrupar_sex_semanaepi",desease,year,": ",w)                       # Mensaje de warning
      print(wrn_msg)
      list(data=casos_sex_semanaepi,res="Warning",msg=w)                                 # Devolver como resultado los datos,  "warning" y el mensaje de warning
    }
  )
}

excel <- read.csv("results5.csv", sep=";")                                      #Excel con los resultados de prueba.R
no_error <- excel[excel$Resultado != "Error",]                                  #Filtrar el Excel para tener solo las variables con Success o Warning
des_ano_no_error <-unique(no_error[,c("Desease", "Año")])                       #Combinaciones únicas de desease y año 


results_casos_sex_semanaepi <- data.frame(matrix(ncol=4, nrow=0))                   # Crear el data frame de los resultados y nombrar las columnas
colnames(results_casos_sex_semanaepi) <- c("Desease","Año","Resultado","Mensaje")

#Inicio loop de todas las fila des_ano_no_error
for (i in 1:nrow(des_ano_no_error)) {                                                                            
  desease <- des_ano_no_error$Desease[i]
  year <- des_ano_no_error$Año[i]
  
  # Importar datos
  data_event_result <- try_importdataevent(desease, year)
  if (data_event_result$res == "Error"){
    result_msg <- data_event_result$msg
    new_row <- list(desease,year,"Error",result_msg)    # Crear la nueva fila para los resultados con el desase. año y resultado
    results_casos_sex_semanaepi[nrow(results_casos_sex_semanaepi) + 1,] <- new_row                    # Agregar la nueva fila a los resultados
    next    
  }
  data_event <- data_event_result$data
  
  
  # Limpiar
  limp <- try_limpiardata(data_event = data_event, desease, year)
  if (limp$res == "Error"){
    result_msg <- limp$msg
    new_row <- list(desease,year,"Error",result_msg)    # Crear la nueva fila para los resultados con el desase. año y resultado
    results_casos_sex_semanaepi[nrow(results_casos_sex_semanaepi) + 1,] <- new_row                    # Agregar la nueva fila a los resultados
    next                                                      # Ignorar el resto del código y saltar al siguiente año
  }
  data_event_limp <- limp$data
  
  
  #Seguir si hay error 
   probarsexsemanaepi <- try_agrupar_sex_semanaepi(data_event_limp, desease, year)
  
  if ( probarsexsemanaepi$res == "Error") {
    result_msg <-  probarsexsemanaepi$msg
    new_row <- list(desease,year,"Error",result_msg)    # Crear la nueva fila para los resultados con el desase. año y resultado
    results_casos_sex_semanaepi[nrow(results_casos_sex_semanaepi) + 1,] <- new_row                    # Agregar la nueva fila a los resultados
    next
    
  } else {
    casos_sex_semanaepi <-  probarsexsemanaepi$data
    new_row <- list(desease,year, probarsexsemanaepi$res, probarsexsemanaepi$msg)          # Crear la nueva fila para los resultados con el desase, año y resultado
    results_casos_sex_semanaepi[nrow(results_casos_sex_semanaepi) + 1,] <- new_row         x           # Agregar la nueva fila a los resultados
  }
  
}
df <- apply(results_casos_sex_semanaepi,2,as.character)
write.csv2(df,"Results agrupar_sex_semanaepi.CSV",row.names = FALSE, na='')
