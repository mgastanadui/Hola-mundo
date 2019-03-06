
#### README ==================================================================== 

# Proyecto:     SAE - Proyección Conflictos     
# Objetivo:     Limpiar encuestas realizadas por Ipsos
# Output:       data_completa_ipsos (data frame)      


#### PAQUETES ==================================================================

library(foreign)        # Importar de otros formatos (SPSS, Stata, etc.)
library(stringr)        # Manipular characters
library(dplyr)          # Manejo eficiente de data frames
library(rlist)          # Trabajo eficiente con listas


#### RUTAS =====================================================================

path <- ("G:/Economia Aplicada/PROYECTOS/2018/2018-050-E SAE Proyeccion conflictos/")
input <- paste0(path, "4 Analisis/1 Bases/1 Input/1 Ipsos/2 Bases/")
output <- paste0(path, "4 Analisis/1 Bases/2 Output/")


#### DICCIONARIO ===============================================================

## Cargar diccionario
diccionario <- read.csv(paste0(input, "variables.csv"))

## Limpiar missings
diccionario <- diccionario[complete.cases(diccionario$Pregunta.Base), ]

## Generar columna con número de indicador
diccionario$check <- gsub("[^0-9]", "", diccionario$Indicador)


#### CARGAR LISTA DE ENCUESTAS =================================================

## Cargar "lista_encuestas"
load(paste0(output, "ListaEncuestas.RData"))


#### LIMPIEZA ==================================================================

## Ubigeos ---------------------------------------------------------------------

## Nota: Cada encuesta tiene su propio poceso de limpieza.

## 6183117 (Barrick)
lista_encuestas[["6183117"]] <- lista_encuestas[["6183117"]] %>%
        mutate(DISTRITO = as.character(DISTRITO))

## 6183410
lista_encuestas[["6183410"]] <- lista_encuestas[["6183410"]] %>%
        mutate(DISTRITO = "080101")

## 6185016
lista_encuestas[["6185016"]] <- lista_encuestas[["6185016"]] %>%
        mutate(SHELL_SCH2 = str_extract(SHELL_SCH2, "[A-Z]+")) %>%
        mutate(DISTRITO = "") %>%
        mutate(DISTRITO = replace(DISTRITO, SHELL_SCH2 == "CUS", "080101")) %>%
        mutate(DISTRITO = replace(DISTRITO, SHELL_SCH2 == "ICA", "110101")) %>%
        mutate(DISTRITO = replace(DISTRITO, SHELL_SCH2 == "HYO", "120101")) %>%
        mutate(DISTRITO = replace(DISTRITO, SHELL_SCH2 == "LIM", "150101")) %>%
        mutate(DISTRITO = replace(DISTRITO, SHELL_SCH2 == "IQT", "160101"))

# 6181017
lista_encuestas[["6181017"]] <- lista_encuestas[["6181017"]] %>%
        mutate(DISTRITO = as.character(DISTRITO)) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "1", "040701")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "2", "040702")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "4", "040705")) %>%
        mutate(DISTRITO = replace(DISTRITO, !(DISTRITO %in% c("040701", 
                                                              "040702",
                                                              "040705")),
                                  "040706"))

# 6185215
lista_encuestas[["6185215"]] <- lista_encuestas[["6185215"]] %>%
        mutate(DISTRITO = as.character(DISTRI)) %>%
        mutate(DISTRI = str_extract(toupper(DISTRI), "[A-Z]+")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRI %in% c("SATIP",
                                                          "SATIPO",
                                                          "SATPO",
                                                          "SSATIPO",
                                                          "STIPO"),
                                  "120601")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRI %in% c("MASAMARI",
                                                          "mazamari",
                                                          "MAZAMARI",
                                                          "MAZAMATI",
                                                          "MAZARI"),
                                  "120604")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRI %in% c("VILLA",
                                                          "VILLARICA",
                                                          "VILLARICA",
                                                          "VILLARICQ"),
                                  "190307")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRI %in% c("MASAMARI",
                                                          "mazamari",
                                                          "MAZAMARI",
                                                          "MAZAMATI",
                                                          "MAZARI"),
                                  "120604")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRI == "PISCO",
                                  "110501")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRI == "PUENTE",
                                  "120302")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRI == "HUASAHUASI",
                                  "120704")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRI == "IQUITOS",
                                  "160101"))

## 6182015 
lista_encuestas[["6182015"]] <- lista_encuestas[["6182015"]] %>%
        mutate(DISTRITO = as.character(distrito))

## 6182314
lista_encuestas[["6182314"]] <- lista_encuestas[["6182314"]] %>%
        mutate(DISTRITO = as.character(distrito))

# 6186017
lista_encuestas[["6186017"]] <- lista_encuestas[["6186017"]] %>%
        mutate(DISTRITO = DISTRITOA) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "1", "040101")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "2", "040102")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "3", "040103")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "4", "040104")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "5", "040107")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "6", "040109")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "7", "040110")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "8", "040112")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "9", "040116")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "10", "040117"))

# 6188716
lista_encuestas[["6188716"]] <- lista_encuestas[["6188716"]] %>%
        mutate(DISTRITO = "") %>%
        mutate(DISTRITO = replace(DISTRITO, CIUDAD == 40, "061309")) %>%
        mutate(DISTRITO = replace(DISTRITO, CIUDAD == 3, "060411" )) %>%
        mutate(DISTRITO = replace(DISTRITO, CIUDAD == 4, "060409" )) %>%
        mutate(DISTRITO = replace(DISTRITO, CIUDAD %in% 9:36, "060415" )) %>%
        mutate(DISTRITO = replace(DISTRITO, CIUDAD %in% c(5, 37:399), "060607"))

# 6480811
lista_encuestas[["6480811"]] <- lista_encuestas[["6480811"]] %>%
        mutate(DISTRITO = as.character(DISTRI)) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "1", "060101")) %>%
        mutate(DISTRITO = replace(DISTRITO, DISTRITO == "2", "060105")) %>%
        mutate(DISTRITO = replace(DISTRITO, !(DISTRITO %in% c("060101",
                                                              "060105")),
                                  "060108"))

# 6185210
lista_encuestas[["6185210"]] <- lista_encuestas[["6185210"]] %>%
        mutate(DISTRITO = replace(DISTRITO, CIUDAD %in% c(2:5,7:9,11:13),
                                  "060101")) %>%
        mutate(DISTRITO = replace(DISTRITO, CIUDAD %in% c(1,15:17),
                                  "060108")) %>%
        mutate(DISTRITO = replace(DISTRITO, CIUDAD %in% c(6,10,14,18,19),
                                  "060105"))

# 6181509
lista_encuestas[["6181509"]] <- lista_encuestas[["6181509"]] %>%
        mutate(DISTRITO = DISTRITO, AREA %in% c(1, 4), "030506") %>%
        mutate(DISTRITO = DISTRITO, AREA == 2, "030501") %>%
        mutate(DISTRITO = DISTRITO, AREA == 3, "030401") %>%
        mutate(DISTRITO = DISTRITO, AREA == 5, "030503") %>%
        mutate(DISTRITO = DISTRITO, AREA == 6, "030708") %>%
        mutate(DISTRITO = DISTRITO, AREA == 7, "030504")

# 6181109
lista_encuestas[["6181109"]] <- lista_encuestas[["6181109"]] %>%
        mutate(DISTRITO = DISTRITO, CIUDAD == "1", "080801") %>%
        mutate(DISTRITO = DISTRITO, CIUDAD == "5", "080802") %>%
        mutate(DISTRITO = DISTRITO, CIUDAD == "2", "080803") %>%
        mutate(DISTRITO = DISTRITO, CIUDAD == "4", "080804") %>%
        mutate(DISTRITO = DISTRITO, CIUDAD == "7", "080805") %>%
        mutate(DISTRITO = DISTRITO, CIUDAD == "3", "080806") %>%
        mutate(DISTRITO = DISTRITO, CIUDAD == "8", "080807") %>%
        mutate(DISTRITO = DISTRITO, CIUDAD == "6", "080808")

# 6184109
lista_encuestas[["6184109"]] <- lista_encuestas[["6184109"]] %>%
        mutate(DISTRITO = replace(DISTRITO,  CIUDAD %in% c(2:5,7:9,11:13),
                                  "060101")) %>%
        mutate(DISTRITO = replace(DISTRITO, CIUDAD %in% c(1,15:17),
                                  "060108")) %>%
        mutate(DISTRITO = replace(DISTRITO, CIUDAD %in% c(6,10,14,18,19),
                                  "060105"))

        
## ID --------------------------------------------------------------------------

## Generar ID de cada observación
lista_encuestas <- lapply(lista_encuestas, 
                          function(x) {
                                  x$id <- c(1:nrow(x))
                                  x
                          })
