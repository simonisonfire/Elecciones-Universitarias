library(tidyverse)
library(readxl)
library(geouy)

# Cargamos las bases de datos de las Elecciones Universitarias

elecciones_cf_2021 <- read_excel("Bases/elecciones_cf_2021.xlsx")
elecciones_cf_2022 <- read_excel("Bases/elecciones_cf_2022.xlsx")

# Estandarizamos los nombres por agrupación estudiantil

elecciones_cf_2021 <- elecciones_cf_2021 %>% 
  rename("CGU" = "CGU 200",
         "ARENA" = "ARENA 5",
         "CECSO" = "CECSO 69")

elecciones_cf_2022 <- elecciones_cf_2022 %>% 
  rename("CGU" = "CGU 100",
         "ARENA" = "ARENA 5",
         "CECSO" = "CECSO 69")

# Función para quitar tildes de los departamentos
remove_accents <- function(text) {
  text <- iconv(text, to = "ASCII//TRANSLIT")
  text <- gsub("[^a-zA-Z0-9 -]", "", text)
  return(text)
}

# Aplicar la función a todas las celdas del dataframe
elecciones_cf_2021[] <- lapply(elecciones_cf_2021, function(x) sapply(x, remove_accents))
elecciones_cf_2022[] <- lapply(elecciones_cf_2022, function(x) sapply(x, remove_accents))


# Cargamos el mapa de Uruguay por departamento
departamentos <- load_geouy("Departamentos")

departamentos <- departamentos %>% 
  select(nombre, the_geom)

# Cambiamos el nombre de la columna para que coincida con las bases de las elecciones
colnames(departamentos)[colnames(departamentos) == "nombre"] <- "DEPARTAMENTO"

# Combinamos la base de las elecciones con el mapa
elecciones_2021_mapa <- left_join(departamentos, elecciones_cf_2021, by = "DEPARTAMENTO")

# Convertimos los resultados a tipo numérico
elecciones_2021_mapa$CGU <- as.numeric(elecciones_2021_mapa$CGU)
elecciones_2021_mapa$ARENA <- as.numeric(elecciones_2021_mapa$ARENA)
elecciones_2021_mapa$CECSO <- as.numeric(elecciones_2021_mapa$CECSO)

# Creamos la columna "ganador" con la agrupación con más votos en cada fila
elecciones_2021_mapa <- elecciones_2021_mapa %>%
  mutate(ganador = case_when(
    CGU > ARENA & CGU > CECSO ~ "CGU",
    ARENA > CGU & ARENA > CECSO ~ "ARENA",
    CECSO > CGU & CECSO > ARENA ~ "CECSO",
    TRUE ~ "Empate"  # En caso de empate
  ))

# Graficamos

ggplot(data = elecciones_2021_mapa) +
  geom_sf(aes(fill = ganador)) +
  scale_fill_manual(values = c("CGU" = "#3288bd", "ARENA" = "#f46d43", "CECSO" = "#66bd63", "Empate" = "#ffffbf"), guide = "legend") +
  labs(title = "Ganadores de Elecciones Universitarias por Departamento 2021") +
  theme_minimal()

# Hacemos lo mismo para el año 2022
elecciones_2022_mapa <- left_join(departamentos, elecciones_cf_2022, by = "DEPARTAMENTO")

elecciones_2022_mapa$CGU <- as.numeric(elecciones_2022_mapa$CGU)
elecciones_2022_mapa$ARENA <- as.numeric(elecciones_2022_mapa$ARENA)
elecciones_2022_mapa$CECSO <- as.numeric(elecciones_2022_mapa$CECSO)

elecciones_2022_mapa <- elecciones_2022_mapa %>%
  mutate(ganador = case_when(
    CGU > ARENA & CGU > CECSO ~ "CGU",
    ARENA > CGU & ARENA > CECSO ~ "ARENA",
    CECSO > CGU & CECSO > ARENA ~ "CECSO",
    TRUE ~ "Empate"  # En caso de empate
  ))

ggplot(data = elecciones_2022_mapa) +
  geom_sf(aes(fill = ganador)) +
  scale_fill_manual(values = c("CGU" = "#3288bd", "ARENA" = "#f46d43", "CECSO" = "#66bd63", "Empate" = "#ffffbf"), guide = "legend") +
  labs(title = "Ganadores de Elecciones Universitarias por Departamento 2022") +
  theme_minimal()
