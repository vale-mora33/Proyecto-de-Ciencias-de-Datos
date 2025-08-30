# --- Cargar librerías ---
library(dplyr)
library(janitor)
library(stringr)
library(readr)

# Importación de datos
Base_de_datos <- read_csv("/Users/valeria/Desktop/Base_de_datos.csv")

# Verificar que se cargó
head(Base_de_datos)



# --- 2. Depuración ---
Base_de_datos_limpia <- Base_de_datos %>%
  # Limpiar nombres de columnas (minúsculas, sin espacios ni caracteres raros)
  clean_names() %>%
  
  # Eliminar filas duplicadas
  distinct() %>%
  
  # Quitar espacios en blanco extra en variables de texto
  mutate(across(where(is.character), ~str_squish(.))) %>%
  
  # Manejar valores faltantes
  drop_na()

# --- 3. Guardar la base depurada ---
write_csv(Base_de_datos_limpia, "Base_de_datos_depurada.csv")

# --- 4. Revisar resultados ---
glimpse(Base_de_datos_limpia)   # Estructura
summary(Base_de_datos_limpia)   # Estadísticos básicos

