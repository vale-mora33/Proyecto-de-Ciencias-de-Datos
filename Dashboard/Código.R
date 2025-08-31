---
  title: "Dashboard Económico y Laboral en EE.UU. 2023"
output: 
  flexdashboard::flex_dashboard:
  orientation: rows
vertical_layout: fill
---
  
library(tidyverse)
library(plotly)
library(flexdashboard)
library(DT)

# Cargar la base depurada
data <- read_csv("Base_de_datos_depurada.csv")

# Renombrar solo las columnas necesarias
data_clean <- data %>%
  select(
    label = 1,
    estimate = 2,
    percent = 4
  )

# Quitar comas y porcentajes, convertir a numéricos
data_clean <- data_clean %>%
  mutate(
    estimate = as.numeric(gsub(",", "", estimate)),
    percent = as.numeric(gsub("%", "", percent))
  ) %>%
  filter(!is.na(estimate))

datatable(head(data_clean, 20))

fig1 <- data_clean %>%
  filter(str_detect(label, "labor force|Employed|Unemployed")) %>%
  plot_ly(x = ~label, y = ~estimate, type = "bar",
          name = "Fuerza Laboral") %>%
  layout(title = "Indicadores de Fuerza Laboral")

fig1


# Filtramos los labels que son rangos de ingreso
fig_income_ranges <- data_clean %>%
  filter(str_detect(label, "\\$")) %>%  # busca todos los labels que tengan '$'
  plot_ly(
    x = ~label,
    y = ~estimate,
    type = "bar",
    name = "Hogares por ingreso"
  ) %>%
  layout(
    title = "Distribución de Hogares por Rango de Ingreso",
    xaxis = list(title = "Rango de Ingreso"),
    yaxis = list(title = "Número de Hogares")
  )

fig_income_ranges


library(dplyr)
library(plotly)

# Crear una columna para agrupar
data_insurance <- data_clean %>%
  filter(str_detect(label, "coverage|Unemployed|Not in labor force")) %>%
  mutate(group = case_when(
    row_number() <= 5 ~ "Unemployed",
    TRUE ~ "Not in labor force"
  ))

# Gráfico
fig_insurance <- data_insurance %>%
  plot_ly(
    x = ~label,
    y = ~percent,
    type = "bar",
    color = ~group
  ) %>%
  layout(
    title = "Cobertura de Seguro por Estado Laboral",
    xaxis = list(title = ""),
    yaxis = list(title = "Porcentaje (%)"),
    barmode = "group"
  )

fig_insurance



# Valor de ingreso medio y mediano
median_income <- data_clean %>% filter(label == "Median household income (dollars)") %>% pull(estimate)
mean_income <- data_clean %>% filter(label == "Mean household income (dollars)") %>% pull(estimate)

# Desempleo
unemployment_rate <- data_clean %>% filter(label == "Unemployment Rate") %>% pull(percent)

valueBox(scales::dollar(median_income), "Ingreso Mediano")
valueBox(scales::dollar(mean_income), "Ingreso Promedio", color = "blue")
valueBox(paste0(unemployment_rate, "%"), "Tasa de Desempleo", color = "red")


fig1 <- data_clean %>%
  filter(str_detect(label, "In labor force|Employed|Unemployed|Not in labor force")) %>%
  plot_ly(x = ~label, y = ~estimate, type = "bar",
          name = "Fuerza Laboral") %>%
  layout(title = "Indicadores de Fuerza Laboral",
         xaxis = list(title = ""), yaxis = list(title = "Personas"))

fig1


fig_income <- data_clean %>%
  filter(str_detect(label, "\\$")) %>%
  plot_ly(x = ~label, y = ~estimate, type = "bar") %>%
  layout(title = "Distribución de Hogares por Rango de Ingreso",
         xaxis = list(title = "Rangos de Ingreso"), 
         yaxis = list(title = "Número de Hogares"))

fig_income


fig_sector <- data_clean %>%
  filter(str_detect(label, "occupations|trade|services|public administration|manufacturing|construction")) %>%
  plot_ly(labels = ~label, values = ~percent, type = "pie") %>%
  layout(title = "Participación por Sector de Ocupación")

fig_sector


# 1. Definir umbral de pobreza (hogar de 4 personas, 2023)
poverty_threshold <- 30000

# 2. Calcular cuántos hogares están por debajo del umbral
lower_income_households <- data_clean %>%
  filter(label %in% c("Less than $10,000", "$10,000 to $14,999", "$15,000 to $24,999")) %>%
  summarise(total_below = sum(estimate, na.rm = TRUE)) %>%
  pull(total_below)

total_households <- data_clean %>%
  filter(label == "Total households") %>%
  pull(estimate)

pct_below_threshold <- (lower_income_households / total_households) * 100
pct_above_threshold <- 100 - pct_below_threshold

# 3. Gráfico pie con respecto al umbral oficial
fig_poverty_official <- plot_ly(
  labels = c("Por debajo del umbral de pobreza", "Por encima"),
  values = c(pct_below_threshold, pct_above_threshold),
  type = "pie",
  textinfo = "label+percent",
  insidetextorientation = "radial"
) %>%
  layout(
    title = glue::glue("Hogares por debajo del umbral de pobreza (${poverty_threshold})")
  )

fig_poverty_official

#Nota: Se ha considerado como en pobreza a cualquier hogar con ingresos anuales menores a $25,000, usando los rangos disponibles en la base de datos.



















