
#### README #### 

# Proyecto:     SAE - Proyección Conflictos     
# Objetivo:     Generar una lista con todas las encuestas de Ipsos en formato
#               data frame
# Output:       lista_encuestas (list)      


#### PAQUETES ==================================================================

library(foreign)        # Importar de otros formatos (SPSS, Stata, etc.)
library(stringr)        # Manipular characters
library(dplyr)          # Manejo eficiente de data frames
library(rlist)          # Trabajo eficiente con listas
library(memisc)         # Importación eficiente desde SPSS
library(sjlabelled)     # Manejo de labels de data importada


#### RUTAS =====================================================================

path <- ("G:/Economia Aplicada/PROYECTOS/2018/2018-050-E SAE Proyeccion conflictos/")
input <- paste0(path, "4 Analisis/1 Bases/1 Input/1 Ipsos/2 Bases/")
output <- paste0(path, "4 Analisis/1 Bases/2 Output/")


#### IDENTIFICAR ENCUESTAS =====================================================

## Cargar diccionario de variables y encuestas ---------------------------------

## Cargar diccionario
diccionario <- read.csv(paste0(input, "variables.csv"))

## Limpiar missings
diccionario <- diccionario[complete.cases(diccionario$Pregunta.Base), ]

## Generar columna con número de indicador
diccionario$check <- gsub("[^0-9]", "", diccionario$Indicador)


## Generar id de encuesta ------------------------------------------------------

## Vector con ID de encuestas IPSOS
id_encuestas <- as.character(unique(diccionario$Código))


#### IMPORTAR ENCUESTAS ========================================================

## Cargar encuesta "6186109" ---------------------------------------------------

df_6186109 <- readRDS(paste0(input, "6186109RDS"))
df_6186109$DISTRITO <- as.character(df_6186109$DISTRITO)


## Cargar encuesta "6183117" ---------------------------------------------------

## Nota: El id de encuesta "6183117" es compartido por dos proyectos. Estas dos
## se juntan para la limpieza preliminar, pero Para el modelo predictivo a nivel
## de proyecto ambas encuestas deben ser separadas nuevamente.

## Leer datos de "Lagunas" y crear columna de Proyecto
df_6183117_1 <- as.data.frame(as.data.set(
                spss.system.file(paste0(input, "6183117 (Lagunas).sav"),
                                 to.lower = FALSE)
))
df_6183117_1$proy <- "Barrick - Lagunas Norte"

## Leer datos de "Pierina" y crear columna de Proyecto
df_6183117_2 <- as.data.frame(as.data.set(
        spss.system.file(paste0(input, "6183117 (Pierina).sav"),
                         to.lower = FALSE)
))
df_6183117_2$proy <- "Barrick - Pierina"

## Consolidar encuestas de Barrick
df_6183117 <- full_join(df_6183117_1, df_6183117_2)


## Cargar resto de encuestas ---------------------------------------------------

nombres_spss <- id_encuestas[!(id_encuestas %in% c("6186109", "6183117"))]


## Crear lista con "data sets" de encuestas importadas de formato SPSS
encuestas_spss <- list()
for (i in seq_along(nombres_spss)) {
        print(i)
        nombre <- nombres_spss[i]
        
        # Cargar encuestas desde formato spss e incorporar en lista
        encuestas_spss[[i]] <- as.data.set(
                spss.system.file(paste0(input, nombre, ".sav"),
                                 to.lower = FALSE),
                stringAsFactors = FALSE)
        
        # Convertir columnas de cada encuesta en vector para eliminar labels
        encuestas_spss[[i]] <- lapply(encuestas_spss[[i]], as.vector)
        
        # Convertir encuesta en DF
        encuestas_spss[[i]] <- as.data.frame(encuestas_spss[[i]],
                                             stringsAsFactors = FALSE)
}


#### CREAR LISTA DE ENCUESTAS ==================================================

## Crear lista (vacía) para todas las encuestas 
lista_encuestas <- list()

## Incorporar a la lista los dos DF trabajados independientemente
lista_encuestas[[1]] <- df_6186109
lista_encuestas[[2]] <- df_6183117

## Incorporar a la lista el resto de DF cargados
for (i in seq_along(nombres_spss)) {
        lista_encuestas[[i+2]] <- encuestas_spss[[i]]
}

## Nombrar cada DF de la lista
names(lista_encuestas) <- c("6186109", "6183117", nombres_spss)


#### GUARDDAR OUTPUT ===========================================================

## Guardar lista de encuestas
save(lista_encuestas, file = paste0(output, "ListaEncuestas.RData"))

## Mantener solo output relevante
rm(list = setdiff(ls(), "lista_encuestas"))


#### FIN ####
