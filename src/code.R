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

library(data.table)
ClimateChangeMetadata <- fread("C:/Users/andya/OneDrive/Documentos/ClimateChangeIGC/data/ClimateChangeMetadata.csv")

################################################################################

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
  "United States", "USA", "Uruguay", "Vatican City",
  # Añadidos:
  "Macao", "Bermuda", "New Caledonia", "French Polynesia", "Greenland", 
  "Guam", "Cayman Islands", "Faroe Islands", "Gibraltar", "Curaçao",
  "Virgin Islands (U.S.)", "Falkland Islands (Malvinas)", "Saint Kitts and Nevis"
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
  "Angola", "Bangladesh", "Benin", "Bhutan","Bolivia", "Cabo Verde", "Cambodia", 
  "Cameroon", "Comoros", "Congo", "Ivory Coast", "Djibouti", "Egypt", "El Salvador", 
  "Eswatini", "Ghana", "Haiti", "Honduras", "India", "Indonesia", "Kenya", 
  "Kiribati", "Kyrgyzstan", "Laos", "Lesotho", "Mauritania", "Micronesia", 
  "Mongolia", "Morocco", "Myanmar", "Nepal", "Nicaragua", "Nigeria", "Pakistan", 
  "Papua New Guinea", "Philippines", "Sao Tome and Principe", "Senegal", 
  "Solomon Islands", "Sri Lanka", "Tanzania", "Tajikistan", "East Timor", 
  "Tunisia", "Ukraine", "Uzbekistan", "Vanuatu", "Vietnam", "Viet Nam", "Zambia", "Zimbabwe",
  # Añadidos:
  "Cote d'Ivoire", "Palestine", "Cape Verde", "Swaziland", "Timor-Leste"
)

# Low income countries
low_income <- c(
  "Afghanistan", "Burkina Faso", "Burundi", "Central African Republic", "Chad", 
  "Democratic Republic of the Congo", "Eritrea", "Ethiopia", "Gambia", "Guinea", 
  "Guinea-Bissau", "Liberia", "Madagascar", "Malawi", "Mali", "Mozambique", 
  "Niger", "North Korea", "Rwanda", "Sierra Leone", "Somalia", "South Sudan", 
  "Sudan", "Syria", "Togo", "Uganda", "Yemen"
)

# Función mejorada para clasificar el país según grupo de ingreso
clasificar_ingreso <- function(pais) {
  if (is.na(pais) || pais == "") return(NA_character_)
  
  # Normalizar algunos nombres comunes
  pais_normalizado <- pais
  
  # Manejar variaciones de nombres
  if (grepl("Venezuela", pais, ignore.case = TRUE)) {
    pais_normalizado <- "Venezuela"  # Necesitas decidir en qué categoría va
  }
  if (grepl("French Guiana", pais, ignore.case = TRUE)) {
    return("High income")  # Territorio francés
  }
  if (grepl("Reunion", pais, ignore.case = TRUE)) {
    return("High income")  # Territorio francés
  }
  if (grepl("Cook Islands", pais, ignore.case = TRUE)) {
    return("Upper middle income")
  }
  
  # Clasificación estándar
  if (pais_normalizado %in% high_income) return("High income")
  if (pais_normalizado %in% upper_middle_income) return("Upper middle income")
  if (pais_normalizado %in% lower_middle_income) return("Lower middle income")
  if (pais_normalizado %in% low_income) return("Low income")
  
  # Venezuela según World Bank 2024 está como país no clasificado por falta de datos
  # pero tradicionalmente se considera upper-middle o actualmente sin clasificar
  if (pais == "Venezuela") return("Not classified - Venezuela")
  
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

# Ver países que aún no fueron clasificados
paises_no_clasificados <- ClimateChangeMetadata %>% 
  filter(Income_Group == "Not classified" & !is.na(Pais_Primer_Autor)) %>% 
  group_by(Pais_Primer_Autor) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

print(paises_no_clasificados, n = 30)

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

# Eliminar "Not classified - Venezuela" de publicaciones_por_ingreso_año
publicaciones_por_ingreso_año <- publicaciones_por_ingreso_año %>%
  filter(Income_Group != "Not classified - Venezuela")


###############################################################################
# 2. Crear nuevo dataframe para análisis con indicadores 
###############################################################################

# Dataframe de grupos de ingreso de 2000-2024

# Filtrar datos desde el año 2000 hasta 2024
publicaciones_2000_2024 <- publicaciones_por_ingreso_año %>%
  filter(Year >= 2000 & Year <= 2024)

# Ver distribución por grupo de ingreso
publicaciones_2000_2024 %>%
  group_by(Income_Group) %>%
  summarise(
    años = n(),
    total_publicaciones = sum(n_publicaciones),
    promedio_anual = mean(n_publicaciones),
    mediana_anual = median(n_publicaciones),
    min_anual = min(n_publicaciones),
    max_anual = max(n_publicaciones)
  )

###############################################################################
# Perspective
###############################################################################

library(dplyr)

porcentajes_income_2024 <- ClimateChangeMetadata |>
  mutate(Year = as.integer(Year)) |>
  filter(
    !is.na(Income_Group),
    !is.na(Year),
    Year <= 2024
  ) |>
  count(Income_Group, name = "n") |>
  mutate(
    total = sum(n),
    prop  = n / total,
    perc  = round(prop * 100, 2)
  )

porcentajes_income_2024




###############################################################################
# Resumen de número de artículos por año y grupo de ingreso para gráficas

library(dplyr)
library(ggplot2)

# Definir los 4 grupos de ingreso que sí queremos
income_levels <- c("High income",
                   "Upper middle income",
                   "Lower middle income",
                   "Low income")

prod_year_income <- ClimateChangeMetadata |>
  filter(
    Income_Group %in% income_levels,
    !is.na(Year)
  ) |>
  mutate(
    Year = as.integer(Year),
    Income_Group = factor(Income_Group, levels = income_levels)
  ) |>
  filter(Year <= 2024) |>
  count(Year, Income_Group, name = "n")   # número de artículos por año y grupo

prod_year_income_prop <- prod_year_income |>
  group_by(Year) |>
  mutate(
    total_year = sum(n),
    prop = n / total_year
  ) |>
  ungroup()

###############################################################################
# Gráfico #1

library(ggplot2)
library(dplyr)
library(RColorBrewer)

income_levels <- c("High income",
                   "Upper middle income",
                   "Lower middle income",
                   "Low income")

# Asegurar el orden de los factores
prod_year_income$Income_Group      <- factor(prod_year_income$Income_Group,
                                             levels = income_levels)
prod_year_income_prop$Income_Group <- factor(prod_year_income_prop$Income_Group,
                                             levels = income_levels)

# Paleta común (4 colores)
pal_income <- brewer.pal(4, "Dark2")
names(pal_income) <- income_levels  # opcional, pero ordenado


library(scales)  # por si no lo habías cargado

ggplot(prod_year_income,
       aes(x = Year, y = n, group = Income_Group)) +
  geom_area(aes(fill = Income_Group),
            position = "stack",
            alpha = 0.25) +
  geom_line(aes(color = Income_Group),
            linewidth = 1.1) +
  scale_x_continuous(
    limits  = c(1946, 2024),
    breaks  = c(1946, seq(1950, 2020, by = 10), 2024),
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    breaks = seq(0, 60000, by = 10000),   # cada 10 mil
    labels = comma                        # 10,000 ; 20,000 ; etc.
  ) +
  scale_color_manual(values = pal_income) +
  scale_fill_manual(values  = pal_income) +
  labs(
    x = "Year",
    y = "Number of climate-change articles",
    fill  = "Income group",
    color = "Income group"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    axis.title  = element_text(face = "bold"),
    plot.title  = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


############################################################################

# Gráfico #2: proporción por año y grupo de ingreso

library(scales)  # para percent_format()

ggplot(prod_year_income_prop,
       aes(x = Year, y = prop, color = Income_Group)) +
  geom_line(linewidth = 1.1) +
  scale_x_continuous(
    limits  = c(1946, 2024),
    breaks  = c(1946, seq(1950, 2020, by = 10), 2024),
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_color_manual(values = pal_income) +
  labs(
    x = "Year",
    y = "Share of articles per year",
    color = "Income group"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    axis.title  = element_text(face = "bold"),
    plot.title  = element_blank(),      # sin título
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


