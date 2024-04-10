install.packages("DBI")
install.packages("RSQLite")

# Cargar la librería necesaria
library(DBI)
library(RSQLite)

# Especificar la ruta de la base de datos SQLite
ruta_base_datos <- "C:\\Program Files\\SQLiteStudio\\prueba"

# Conectar a la base de datos
conexion <- dbConnect(RSQLite::SQLite(), dbname = ruta_base_datos)
dbIsValid(conexion)

# Consultar las tablas disponibles en la base de datos
tablas <- dbListTables(conexion)
print(tablas)

# Realizar una consulta SQL Prestadores
consultaPrestadores <- "SELECT * FROM Prestadores"
resultadoPrestadores <- dbGetQuery(conexion, consultaPrestadores)
print(resultadoPrestadores)

# Realizar una consulta SQL
consultaMunicipios <- "SELECT * FROM Municipios"
resultadoMunicipios <- dbGetQuery(conexion, consultaMunicipios)
print(resultadoMunicipios)

#print(resultadoMunicipios$Region )

# Función para limpiar caracteres especiales de una cadena
limpiar_caracteres_especiales <- function(cadena) {
  cadena <- iconv(cadena, to = "ASCII", sub = "byte")
  # Reemplazar caracteres especiales por nada (eliminarlos), excepto letras, números y caracteres con tilde
  cadena <- gsub("[^[:alnum:]\\sáéíóúüñÁÉÍÓÚÜÑ ]", "", cadena)
  return(cadena)
}

# Recorre las columnas con un índice

limpiar_datos <- function(datos){
  num_columnas <- ncol(datos)
  
  for (i in 1:num_columnas) {
  # Accede a cada columna por su número de índice
    columna <- datos[[i]]
  
  # Aplica la función para limpiar caracteres especiales a la columna
    datos[[i]] <- limpiar_caracteres_especiales(columna)
  
  # Realiza alguna operación con la columna, por ejemplo, mostrar sus primeros elementos
    cat("Columna", i, "limpiada:", "\n")
    print(head(datos[[i]]))
  
  # Aquí puedes agregar cualquier operación adicional que desees realizar con cada columna
  }
  return(datos)
}

resultadoMunicipios<-limpiar_datos(resultadoMunicipios)
resultadoPrestadores<-limpiar_datos(resultadoPrestadores)

años <- as.numeric(substr(resultadoPrestadores$fecha_radicacion, 1, 4))
años_vencimiento <- as.numeric(substr(resultadoPrestadores$fecha_vencimiento, 1, 4))

barplot(table(resultadoPrestadores$depa_nombre), main = "Cantidad de Prestadores por Departamento", xlab = "Departamento", ylab = "Cantidad", col = "skyblue")

hist(años_vencimiento, main = "Distribución de Fechas de Vencimiento", xlab = "Fecha de vencimiento", ylab = "Frecuencia", col = "lightgreen")

plot(as.Date(resultadoPrestadores$fecha_radicacion, format = "%Y%m%d"), as.Date(resultadoPrestadores$fecha_vencimiento, format = "%Y%m%d"), 
     main = "Relación entre Fecha de Radicación y Fecha de Vencimiento", xlab = "Fecha de Radicación", ylab = "Fecha de Vencimiento", col = "blue")

barplot(table(resultadoPrestadores$muni_nombre), main = "Cantidad de Prestadores por Municipio", xlab = "Municipio", ylab = "Cantidad", col = "orange")

pie(table(resultadoPrestadores$clpr_nombre), main = "Proporción de Tipos de Prestadores", col = rainbow(length(unique(resultadoPrestadores$clpr_nombre))))

barplot(table(resultadoPrestadores$nivel), main = "Cantidad de Prestadores por Nivel", xlab = "Nivel", ylab = "Cantidad", col = "purple")

plot(as.numeric(resultadoPrestadores$numero_sede_principal), as.numeric(resultadoPrestadores$nivel), 
     main = "Relación entre Número de Sede Principal y Nivel", xlab = "Número de Sede Principal", ylab = "Nivel", col = "green")

hist(años, main = "Distribución de Años de Fecha de Radicación", xlab = "Año de Fecha de Radicación", ylab = "Frecuencia", col = "red")

print(resultadoMunicipios)
# Cerrar la conexión
dbDisconnect(conexion)
