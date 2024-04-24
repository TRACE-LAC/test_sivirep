rm(list = ls())                                                     # Clear all variables
library(tinytex)
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

# Funcion Try para los PDFs --------------------------------------------------------------------------
#3. Función para intentar crear el PDF y guardar el resultado
try_createpdf <- function(desease,year,dpto){
  tryCatch(
    {
      rmarkdown::render("ResultPDF/ResultPDF.Rmd", params = list(
        nombre_evento = desease,
        year = year,
        departmento = dpto))
      print("Success creating PDF",desease,year,dpto)
      list(res="Success",msg="Success creating PDF")                     # Devolver como resultado los datos, "Success" y un mensaje las dimensiones del data frame
    },
    error = function(e){
      err_msg = paste("Error create_pdf",desease,year,dpto,":",e)                         # Mensaje de error
      print(err_msg)
      list(res="Error",msg=e)                                           # Devolver como resultado "Error" y el mensaje de error
    },
    warning = function(w){
      wrn_msg = paste("Warning create_pdf",desease,year,dpto,": ",w)                       # Mensaje de warning
      print(wrn_msg)
      list(res="Warning",msg=w)                                 # Devolver como resultado los datos,  "warning" y el mensaje de warning
    }
  )
}

# ----------------------------------------------------------------------------------
excel <- read.csv("results5.csv", sep=";")                                      #Excel con los resultados de prueba.R
no_error <- excel[excel$Resultado != "Error",]                                  #Filtrar el Excel para tener solo las variables con Success o Warning
des_ano_no_error <-unique(no_error[,c("Desease", "Año", "Dpto")])                       #Combinaciones únicas de desease y año 

results_pdf <- data.frame(matrix(ncol=5, nrow=0))                   # Crear el data frame de los resultados y nombrar las columnas
colnames(results_pdf) <- c("Desease","Año","Departamento","Resultado","Mensaje")

deseases <- c("Fiebre Amarilla","Chagas","Hepatitis C") # ,"Malaria","Mortalidad Materna"
years <- 2007:2022
for (desease in deseases) {
  data_desease <- des_ano_no_error[des_ano_no_error$Desease == desease,]  # Filtrar la tabla para coger solo las filas del desease
  data_desease <- data_desease[data_desease$Año == 2016,]
  
  for (year in years){
    for (dpto in nom_departamentos){
      
      #year <- data_desease$Año[i]
      #dpto <- data_desease$Dpto[i]
      
      cat(paste("\n","Trying:",desease,year,dpto,"\n"))  # Imprimir mensaje
      
      pdf_result <- try_createpdf(desease,year,dpto)
      result <- pdf_result$res
      result_msg <- pdf_result$msg
      new_row <- list(desease,year,dpto,result,result_msg)          # Crear la nueva fila para los resultados con el desase, año y resultado
      results_pdf[nrow(results_pdf) + 1,] <- new_row                    # Agregar la nueva fila a los resultados
      
    }
  }
}

#write_xlsx(results_pdf, "Results create_pdf 2.xlsx")

df <- apply(results_pdf,2,as.character)
write.csv2(df,"Results group 3.csv",row.names = FALSE, na='')