# Instalar (si no lo tienes) y cargar data.table, que es más rápido y tolerante a diferencias de columnas
if (!require(data.table)) install.packages("data.table")
library(data.table)

# Definir la carpeta donde están tus CSV
setwd("~/ClimateChangeIGC/data/data src")

# Listar todos los archivos .csv en esa carpeta
archivos <- list.files(pattern = "\\.csv$", full.names = TRUE)

# Leer y combinar todos los CSV (aunque tengan diferentes columnas)
ClimateChangeMetadata <- rbindlist(lapply(archivos, fread), fill = TRUE)

# Guardar el resultado en un único archivo
fwrite(ClimateChangeMetadata, "ClimateChangeMetadata.csv")

# Guardar en carpeta data
fwrite(ClimateChangeMetadata,
       "C:/Users/andya/OneDrive/Documentos/ClimateChangeIGC/data")

################################################################################
# Abrir el archivo combinado

ClimateChangeMetadata <- fread("C:/Users/andya/OneDrive/Documentos/ClimateChangeIGC/data/ClimateChangeMetadata.csv")

################################################################################
# Eliminar archivos repetidos

setDT(ClimateChangeMetadata)   # asegurar data.table
backup_ClimateChangeMetadata <- copy(ClimateChangeMetadata)

library(dplyr)

# Ver cuántas filas duplicadas hay
sum(duplicated(ClimateChangeMetadata))

# Ver duplicados basándote en EID
ClimateChangeMetadata %>% 
  group_by(EID) %>% 
  filter(n() > 1) %>% 
  arrange(EID)

# Eliminar duplicados
ClimateChangeMetadata <- ClimateChangeMetadata %>% 
  distinct(EID, .keep_all = TRUE)

# Verificar el nuevo número de filas
nrow(ClimateChangeMetadata)

###############################################################################
# Filtrar afiliaciones a sólo el país del primer autor

# Cargar las librerías necesarias
library(data.table)  # Para procesamiento eficiente en datasets grandes
library(stringr)     # Para manipulación de cadenas

# Lista completa de países en inglés
paises <- c(
  "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", 
  "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", 
  "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan", 
  "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria", 
  "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Canada", 
  "Central African Republic", "Chad", "Chile", "China", "Colombia", "Comoros", 
  "Congo", "Costa Rica", "Croatia", "Cuba", "Cyprus", "Czech Republic", 
  "Democratic Republic of the Congo", "Denmark", "Djibouti", "Dominica", 
  "Dominican Republic", "East Timor", "Ecuador", "Egypt", "El Salvador", 
  "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini", "Ethiopia", "Fiji", 
  "Finland", "France", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", 
  "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", 
  "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel", 
  "Italy", "Ivory Coast", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", 
  "Kiribati", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", 
  "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Madagascar", 
  "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania", 
  "Mauritius", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", 
  "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", 
  "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Korea", "North Macedonia", 
  "Norway", "Oman", "Pakistan", "Palau", "Palestine", "Panama", "Papua New Guinea", 
  "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Qatar", "Romania", 
  "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", 
  "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe", 
  "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", 
  "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Korea", 
  "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Sweden", "Switzerland", 
  "Syria", "Taiwan", "Tajikistan", "Tanzania", "Thailand", "Togo", "Tonga", 
  "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda", 
  "Ukraine", "United Arab Emirates", "United Kingdom", "United States", "Uruguay", 
  "Uzbekistan", "Vanuatu", "Vatican City", "Venezuela", "Vietnam", "Yemen", "Zambia", 
  "Zimbabwe", "USA", "UK", "UAE", "Republic of Korea", "Republic of China", 
  "Hong Kong", "Puerto Rico", "Northern Ireland", "Scotland", "Wales"
)

# Crear una expresión regular que busque cualquier país al final de una cadena
# El patrón busca una coma seguida de espacios y luego cualquiera de los países en la lista
patron_paises <- paste0(",\\s*(?:", paste(paises, collapse = "|"), ")$")

# Función optimizada para extraer el país usando funciones de data.table
extraer_pais <- function(aff) {
  # Si la afiliación está vacía, devolver NA
  if (is.na(aff) || aff == "") return(NA_character_)
  
  # Extraer la primera afiliación (antes del primer punto y coma)
  primera_aff <- unlist(strsplit(aff, ";"))[1]
  if (is.null(primera_aff) || primera_aff == "") return(NA_character_)
  
  # Extraer el país (último segmento después de la última coma)
  partes <- unlist(strsplit(primera_aff, ","))
  if (length(partes) < 1) return(NA_character_)
  
  pais <- trimws(partes[length(partes)])
  
  # Comprobar si el país extraído está en nuestra lista
  for (p in paises) {
    if (pais == p) return(pais)
  }
  
  # Si el país extraído no está en la lista, buscar si contiene un país
  for (p in paises) {
    if (grepl(p, pais, fixed = TRUE)) return(p)
  }
  
  # Si no se encuentra un país exacto, devolver el texto extraído
  return(pais)
}

# Convertir a data.table para procesamiento más rápido
setDT(ClimateChangeMetadata)

# Aplicar la función usando data.table para mayor eficiencia
ClimateChangeMetadata[, Pais_Primer_Autor := sapply(Affiliations, extraer_pais)]

# Ver algunos resultados para verificar
head(ClimateChangeMetadata[, .(Affiliations, Pais_Primer_Autor)], 10)

###############################################################################
# Añadir columna de grupos de ingreso de países según de Banco Mundial

###############################################################################
# Filtrar afiliaciones a sólo el país del primer autor

# Cargar las librerías necesarias
library(data.table)  # Para procesamiento eficiente en datasets grandes
library(stringr)     # Para manipulación de cadenas

# Lista completa de países en inglés
paises <- c(
  "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", 
  "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", 
  "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan", 
  "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria", 
  "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Canada", 
  "Central African Republic", "Chad", "Chile", "China", "Colombia", "Comoros", 
  "Congo", "Costa Rica", "Croatia", "Cuba", "Cyprus", "Czech Republic", 
  "Democratic Republic of the Congo", "Denmark", "Djibouti", "Dominica", 
  "Dominican Republic", "East Timor", "Ecuador", "Egypt", "El Salvador", 
  "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini", "Ethiopia", "Fiji", 
  "Finland", "France", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", 
  "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", 
  "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel", 
  "Italy", "Ivory Coast", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", 
  "Kiribati", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", 
  "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Madagascar", 
  "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania", 
  "Mauritius", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", 
  "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", 
  "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Korea", "North Macedonia", 
  "Norway", "Oman", "Pakistan", "Palau", "Palestine", "Panama", "Papua New Guinea", 
  "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Qatar", "Romania", 
  "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", 
  "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe", 
  "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", 
  "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Korea", 
  "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Sweden", "Switzerland", 
  "Syria", "Taiwan", "Tajikistan", "Tanzania", "Thailand", "Togo", "Tonga", 
  "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda", 
  "Ukraine", "United Arab Emirates", "United Kingdom", "United States", "Uruguay", 
  "Uzbekistan", "Vanuatu", "Vatican City", "Venezuela", "Vietnam", "Yemen", "Zambia", 
  "Zimbabwe", "USA", "UK", "UAE", "Republic of Korea", "Republic of China", 
  "Hong Kong", "Puerto Rico", "Northern Ireland", "Scotland", "Wales"
)

# Crear una expresión regular que busque cualquier país al final de una cadena
# El patrón busca una coma seguida de espacios y luego cualquiera de los países en la lista
patron_paises <- paste0(",\\s*(?:", paste(paises, collapse = "|"), ")$")

# Función optimizada para extraer el país usando funciones de data.table
extraer_pais <- function(aff) {
  # Si la afiliación está vacía, devolver NA
  if (is.na(aff) || aff == "") return(NA_character_)
  
  # Extraer la primera afiliación (antes del primer punto y coma)
  primera_aff <- unlist(strsplit(aff, ";"))[1]
  if (is.null(primera_aff) || primera_aff == "") return(NA_character_)
  
  # Extraer el país (último segmento después de la última coma)
  partes <- unlist(strsplit(primera_aff, ","))
  if (length(partes) < 1) return(NA_character_)
  
  pais <- trimws(partes[length(partes)])
  
  # Comprobar si el país extraído está en nuestra lista
  for (p in paises) {
    if (pais == p) return(pais)
  }
  
  # Si el país extraído no está en la lista, buscar si contiene un país
  for (p in paises) {
    if (grepl(p, pais, fixed = TRUE)) return(p)
  }
  
  # Si no se encuentra un país exacto, devolver el texto extraído
  return(pais)
}

# Convertir a data.table para procesamiento más rápido
setDT(ClimateChangeMetadata)

# Aplicar la función usando data.table para mayor eficiencia
ClimateChangeMetadata[, Pais_Primer_Autor := sapply(Affiliations, extraer_pais)]

# Ver algunos resultados para verificar
head(ClimateChangeMetadata[, .(Affiliations, Pais_Primer_Autor)], 10)

###############################################################################
# Añadir columna de grupos de ingreso de países según de Banco Mundial

library(data.table)
library(dplyr)

# Clasificación de países según World Bank Income Groups 2024
# Fuente: https://datahelpdesk.worldbank.org/knowledgebase/articles/906519

# High income countries
high_income <- c(
  "Andorra", "Australia", "Austria", "Bahamas", "Bahrain", "Barbados", "Belgium", 
  "Brunei", "Canada", "Chile", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
  "Estonia", "Finland", "France", "Germany", "Greece", "Hong Kong", "Hungary", 
  "Iceland", "Ireland", "Israel", "Italy", "Japan", "Kuwait", "Latvia", "Liechtenstein", 
  "Lithuania", "Luxembourg", "Malta", "Monaco", "Netherlands", "New Zealand", "Norway", 
  "Oman", "Panama", "Poland", "Portugal", "Puerto Rico", "Qatar", "Romania", 
  "San Marino", "Saudi Arabia", "Seychelles", "Singapore", "Slovakia", "Slovenia", 
  "South Korea", "Republic of Korea", "Spain", "Sweden", "Switzerland", "Taiwan", 
  "Republic of China", "Trinidad and Tobago", "United Arab Emirates", "UAE", 
  "United Kingdom", "UK", "Northern Ireland", "Scotland", "Wales", 
  "United States", "USA", "Uruguay", "Vatican City"
)

# Upper middle income countries
upper_middle_income <- c(
  "Albania", "Algeria", "Argentina", "Armenia", "Azerbaijan", "Belarus", "Belize", 
  "Bosnia and Herzegovina", "Botswana", "Brazil", "Bulgaria", "China", "Colombia", 
  "Costa Rica", "Cuba", "Dominica", "Dominican Republic", "Ecuador", "Equatorial Guinea", 
  "Fiji", "Gabon", "Georgia", "Grenada", "Guatemala", "Guyana", "Iran", "Iraq", 
  "Jamaica", "Jordan", "Kazakhstan", "Lebanon", "Libya", "Malaysia", "Maldives", 
  "Marshall Islands", "Mauritius", "Mexico", "Moldova", "Montenegro", "Namibia", 
  "Nauru", "North Macedonia", "Palau", "Paraguay", "Peru", "Russia", 
  "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "Serbia", "South Africa", 
  "Suriname", "Thailand", "Tonga", "Turkey", "Turkmenistan", "Tuvalu"
)

# Lower middle income countries
lower_middle_income <- c(
  "Angola", "Bangladesh", "Benin", "Bhutan", "Bolivia", "Cabo Verde", "Cambodia", 
  "Cameroon", "Comoros", "Congo", "Ivory Coast", "Djibouti", "Egypt", "El Salvador", 
  "Eswatini", "Ghana", "Haiti", "Honduras", "India", "Indonesia", "Kenya", 
  "Kiribati", "Kyrgyzstan", "Laos", "Lesotho", "Mauritania", "Micronesia", 
  "Mongolia", "Morocco", "Myanmar", "Nepal", "Nicaragua", "Nigeria", "Pakistan", 
  "Papua New Guinea", "Philippines", "Sao Tome and Principe", "Senegal", 
  "Solomon Islands", "Sri Lanka", "Tanzania", "Tajikistan", "East Timor", 
  "Tunisia", "Ukraine", "Uzbekistan", "Vanuatu", "Vietnam", "Zambia", "Zimbabwe"
)

# Low income countries
low_income <- c(
  "Afghanistan", "Burkina Faso", "Burundi", "Central African Republic", "Chad", 
  "Democratic Republic of the Congo", "Eritrea", "Ethiopia", "Gambia", "Guinea", 
  "Guinea-Bissau", "Liberia", "Madagascar", "Malawi", "Mali", "Mozambique", 
  "Niger", "North Korea", "Rwanda", "Sierra Leone", "Somalia", "South Sudan", 
  "Sudan", "Syria", "Togo", "Uganda", "Yemen"
)

# Función para clasificar el país según grupo de ingreso
clasificar_ingreso <- function(pais) {
  if (is.na(pais) || pais == "") return(NA_character_)
  
  if (pais %in% high_income) return("High income")
  if (pais %in% upper_middle_income) return("Upper middle income")
  if (pais %in% lower_middle_income) return("Lower middle income")
  if (pais %in% low_income) return("Low income")
  
  return("Not classified")
}

# Aplicar la clasificación
ClimateChangeMetadata[, Income_Group := sapply(Pais_Primer_Autor, clasificar_ingreso)]

# Ver resultados
head(ClimateChangeMetadata[, .(Pais_Primer_Autor, Income_Group)], 20)

# Ver distribución de grupos de ingreso
table(ClimateChangeMetadata$Income_Group, useNA = "ifany")

# Ver resumen más detallado
ClimateChangeMetadata %>% 
  group_by(Income_Group) %>% 
  summarise(
    n_articulos = n(),
    porcentaje = round(n() / nrow(ClimateChangeMetadata) * 100, 2)
  ) %>% 
  arrange(desc(n_articulos))

# Ver países que no fueron clasificados
paises_no_clasificados <- ClimateChangeMetadata %>% 
  filter(Income_Group == "Not classified" & !is.na(Pais_Primer_Autor)) %>% 
  group_by(Pais_Primer_Autor) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

print(paises_no_clasificados)

###############################################################################
# Nuevo dataframe agrupado por grupos de ingreso, año y número de publicaciones

# Ver cuantas filas tienen NA y "Not classified" en Income_Group para excluirlas
cat("NA:", sum(is.na(ClimateChangeMetadata$Income_Group)), "\n")
cat("Not classified:", sum(ClimateChangeMetadata$Income_Group == "Not classified", na.rm = TRUE), "\n")

# Crear el nuevo dataframe agrupado
publicaciones_por_ingreso_año <- ClimateChangeMetadata %>%
  filter(!is.na(Income_Group) & Income_Group != "Not classified") %>%
  group_by(Income_Group, Year) %>%
  summarise(n_publicaciones = n(), .groups = 'drop') %>%
  arrange(Income_Group, Year)
