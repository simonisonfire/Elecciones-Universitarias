# Combinar datos de ambos años
elecciones_combinadas <- bind_rows(
  mutate(elecciones_cf_2021, Año = 2021),  # Agregar columna para el año
  mutate(elecciones_cf_2022, Año = 2022)   # Agregar columna para el año
)

# Convertimos a numérico
elecciones_combinadas$CGU <- as.numeric(elecciones_combinadas$CGU)
elecciones_combinadas$ARENA <- as.numeric(elecciones_combinadas$ARENA)
elecciones_combinadas$CECSO <- as.numeric(elecciones_combinadas$CECSO)
elecciones_combinadas$BLANCOS <- as.numeric(elecciones_combinadas$BLANCOS)

# Calculamos porcentajes por agrupación y año
elecciones_porcentaje <- elecciones_combinadas %>%
  group_by(Año) %>%
  summarise(
    porcentaje_CGU = sum(CGU) / sum(CGU + ARENA + CECSO + BLANCOS) * 100,
    porcentaje_ARENA = sum(ARENA) / sum(CGU + ARENA + CECSO + BLANCOS) * 100,
    porcentaje_CECSO = sum(CECSO) / sum(CGU + ARENA + CECSO + BLANCOS) * 100,
    porcentaje_BLANCOS = sum(BLANCOS) / sum(CGU + ARENA + CECSO + BLANCOS) * 100
  )

# Reorganizamos datos para el gráfico
elecciones_long <- elecciones_porcentaje %>%
  pivot_longer(cols = -Año, names_to = "Agrupacion", values_to = "Porcentaje")

# Graficamos
ggplot(elecciones_long, aes(x = factor(Año), y = Porcentaje, color = Agrupacion, group = Agrupacion)) +
  geom_point(size = 2) +
  geom_line() +
  labs(title = "Evolución de porcentajes por agrupación entre 2021 y 2022", x = "Año", y = "Porcentaje") +
  theme_minimal()
