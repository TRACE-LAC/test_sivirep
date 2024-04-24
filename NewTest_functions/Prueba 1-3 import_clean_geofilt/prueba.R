library(sivirep)

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
# 1. Funcion para correr import_data_event y que siga si hay error
try_importdataevent <- function(desease,year) {
  tryCatch(
    {
      data_event <-  import_data_event(year = year, nombre_event = desease)
      list(data=data_event,res="Success",msg="Success importing")                     # Devolver como resultado los datos, "Success" y un mensaje las dimensiones del data frame
    },
    error = function(e){
      err_msg = paste("Error importing ",desease,year,": ",e)                         # Mensaje de error
      print(err_msg)
      list(data="",res="Error",msg=e)                                           # Devolver como resultado "Error" y el mensaje de error
    },
    warning = function(w){
      wrn_msg = paste("Warning importing ",desease,year,": ",w)                       # Mensaje de warning
      print(wrn_msg)
      list(data=data_event,res="Warning",msg=w)                                 # Devolver como resultado los datos,  "warning" y el mensaje de warning
    }
  )
}

# 2. Funcion para correr limpiar_data_sivigila y que siga si hay error
try_limpiardata <- function(data_event){
  tryCatch(
    {
      data_event_limp <- limpiar_data_sivigila(data_event = data_event)
      list(data=data_event_limp,res="Success",msg="Success cleaning")                 # Devolver como resultado los datos, "Success" y un mensaje las dimensiones del data frame
    },
    error = function(e){
      err_msg = paste("Error limpiar_data_sivigila",desease,year,": ",e)              # Mensaje de error
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

# 3. Funcion que intenta correr geo_filter y devuelve si se obtuvo error, warning o si corrió bien
try_geofilter <- function(data_event_limp, dpt) {
  tryCatch(
    {
      data_event_filtrada <- geo_filtro(data_event = data_event_limp, dpto = dpt)     # Intentar correr geo_filtro
      result_dim <- dim(data_event_filtrada)                                          # Guardar las dimensiones (filas, columnas) del data frame resultante
      succ_msg <- paste("Success: ",result_dim[1]," rows,",result_dim[2]," columns")
      print(succ_msg)
      list(res="Success",msg=succ_msg)                                                # Devolver como resultado "Success" y un mensaje las dimensiones del data frame
    },
    error = function(e){
      err_msg = paste("Error con dpto ",dpt,": ",e)                                   # Mensaje de error
      print(err_msg)
      list(res="Error",msg=e)                                                    # Devolver como resultado "Error" y el mensaje de error
    },
    warning = function(w){
      wrn_msg = paste("Warning con dpto ",dpt,": ",w)                                 # Mensaje de warning
      print(wrn_msg)
      list(res="Warning",msg=w)                                                 # Devolver como resultado "warning" y el mensaje de warning
    }
  )
}

# Parámetros manuales -----------------------------------------------------
# Seleccionar parámetros
desease <- "Dengue"
year <- 2020

# Importar datos
data_event <-  import_data_event(year = year,
                                 nombre_event = desease)
# Limpiar
data_event_limp <- limpiar_data_sivigila(data_event = data_event)

# Filtrar
dpt <- "Choco"
data_event_filtrada <- geo_filtro(data_event = data_event_limp, dpto = dpt)

message("Trying: ",desease, year, dpt)
result <- try_geofilter(data_event_limp, dpt)

limp <- try_limpiardata(data_event)





# For loop ----------------------------------------------------------------
results <- data.frame(matrix(ncol=6, nrow=0))                   # Crear el data frame de los resultados y nombrar las columnas
colnames(results) <- c("Desease","Año","Dpto","Mpio","Resultado","Mensaje")

n_deseases <- nrow(lista_eventos)                               # Cantidad de enfermedades en lista_eventos
for (i in 1:n_deseases) {                                       # Iterar i desde 1 hasta la cantidad de enfermedades
  # print(lista_eventos$enfermedad[i])
  
  desease <- lista_eventos$enfermedad[i]                        # Desease de la iteración i
  years <- strsplit(lista_eventos$aa[i],", ")[[1]]              # Todos los años del desease (como str)
  
  for (year in years) {                                         # Iterar sobre todos los años del desease
    year <- strtoi(year)                                        # Convertir str a integer
    # print(year)
    
    # Importar -----------------------------------------
    import_data <-  try_importdataevent(desease,year)
    # Si importar genera error, guardar el mensaje de error y avanzar al siguiente año, si no genera error guardar data_event
    if (import_data$res == "Error") {
      result_msg <- paste("Error import_data_event with ",desease,year)
      new_row <- list(desease,year,"","","Error",result_msg)    # Crear la nueva fila para los resultados con el desase. año y resultado
      results[nrow(results) + 1,] <- new_row                    # Agregar la nueva fila a los resultados
      next
      
    } else {
      data_event <- import_data$data
    }
    
    limpiar_data <- try_limpiardata(data_event)
    # Si limpiar genera error, guardar el mensaje de error y avanzar al siguiente año, si no genera error guardar data_event_limp
    if (limpiar_data$res == "Error") {
      trying <- paste(desease,year, sep=" - ")
      result_msg <- limpiar_data$msg
      message("Trying: ",trying)
      print(result_msg)                                         # Imprimir que hubo error limpiando la combinación de desease y año
      
      new_row <- list(desease,year,"","","Error",result_msg)    # Crear la nueva fila para los resultados con el desase, año y resultado
      results[nrow(results) + 1,] <- new_row                    # Agregar la nueva fila a los resultados
      
      next                                                      # Ignorar el resto del código y saltar al siguiente año
      
    } else {
      data_event_limp <- limpiar_data$data
    }
    
    for (d in 1:length(cod_departamentos)) {                    # Iterar desde 1 hasta la cantidad de departamentos
      #cod_mpio <- municipios[[m,2]]
      #nom_mpio <- municipios[[m,4]]
      #nom_dpto <- municipios[[m,3]]
      #cod_dpto <- municipios[[m,1]]
      
      cod_dpto <- cod_departamentos[d]
      nom_dpto <- nom_departamentos[d]
      
      # Intentar filtrar para los datos del municipio para la enfermedad y año dados
      msg <- paste(desease, year, nom_dpto, sep=" - ")          # faltaría nom_mpio
      message("Trying: ",msg)
      result <- try_geofilter(data_event_limp, nom_dpto)        # Puede ser con nom_dpto o cod_dpto

      new_row <- list(desease,year,nom_dpto,"",result$res,result$msg)          # Crear la nueva fila para los resultados con el desase, año y resultado
      results[nrow(results) + 1,] <- new_row                    # Agregar la nueva fila a los resultados
    }
  }
}

#x <- apply(results,2,as.character)
#write.csv2(x,"results5.csv",row.names = FALSE, na='')