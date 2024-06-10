library(tidyverse)
options(digits = 4)

df <- read.csv("reference-values.csv")
temperaturas <- read.csv("temperaturas.csv")

# Definir una función para completar las tablas con múltiples columnas
completar_tabla <- function(temperatura_col, df, columnas, start_10 = TRUE, transmisor = c(-30, 250)) {
    # Crear el DataFrame inicial
    if (start_10) {
        tabla <- data.frame(Temperatura = temperatura_col, 
                            Porcentaje = ((temperatura_col - transmisor[1]) / (transmisor[2] - transmisor[1])) * 100 * 10/11.25)
    } else {
        tabla <- data.frame(Temperatura = temperatura_col, 
                            Porcentaje = ((temperatura_col - transmisor[1]) / (transmisor[2] - transmisor[1])) * 100)
    }
    # Eliminar filas con NA
    tabla <- na.omit(tabla)
    
    # Agregar dinámicamente las columnas de interpolación
    for (columna in columnas) {
        nombre_columna <- rlang::sym(columna)
        tabla <- tabla %>%
            mutate(!!nombre_columna := approx(df$Porcentaje, df[[columna]], Porcentaje)$y)
    }
    
    return(tabla)
}

ejercicio_1 <- completar_tabla(temperaturas$ex1, df, "Bar")
tabla_1 <- completar_tabla(temperaturas$ex2, df, "psi")
tabla_2 <- completar_tabla(temperaturas$ex3, df, "mA")
tabla_3 <- completar_tabla(temperaturas$ex4, df, "V")


# Determinar los valores a la salida. Para un transmisor de -40 a 302°F.
ejercicio_2 <- completar_tabla(temperaturas$ex5, df, c("V", "mA", "psi", "Bar"), start_10 = FALSE, transmisor = c(-40, 302))

