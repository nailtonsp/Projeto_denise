## caregando base de dados 

renda_data <- read_xlsx("data/bruto/data1.xlsx")


##
## ===========================================   trantando dados ================================================================
##

##  ========================================  renomeando variaveis ==============================================================

renda_data <- renda_data |>
  rename(adolecentes_15_17 = `% de adolescentes de 15 a 17 anos de idade que tiveram filhos 2016`)

renda_data <- renda_data |>
  rename(meninas_10_14 = `% de meninas de 10 a 14 anos de idade que tiveram filhos 2016`)

renda_data <- renda_data |>
  rename(PIB_2016_percapta = `Produto Interno Bruto per capita 2016`)

## =========================== editanda a variavel Territorialidades para mergiar com a base com geo-data =======================

renda_data$Territorialidades <- gsub("\\(BA\\)", "", renda_data$Territorialidades)

renda_data$Territorialidades <- trimws(renda_data$Territorialidades)

renda_data$Territorialidades <- toTitleCase(renda_data$Territorialidades)


##  ======================================== inportando geo data ================================================================

ba_geo_data <- read_municipality(code_muni = "BA", year = 2016)


## ======================= Verificando se as variaveis a serem mergiadas sao iguais  ============================================

diferenca <- c()

for (i in 1:nrow(renda_data)){
    if (!(ba_geo_data$name_muni[i] %in% renda_data$Territorialidades)){
      diferenca <-c(ba_geo_data$name_muni[i], diferenca)
  }
}

##======================================= corrigindo obs para mergiar as bases  =================================================

renda_data$Territorialidades[348] <- "Santa Terezinha"
renda_data$Territorialidades[269] <- "Muquém Do São Francisco"
renda_data$Territorialidades[205] <- "Iuiu"
renda_data$Territorialidades[119] <- "Dias D'ávila"
renda_data$Territorialidades[23] <- "Araçás"
  


## ========================================= merge entre renda_data e ba_geo_data ===============================================

distribuicao_renda <- merge(renda_data, ba_geo_data, by.x = "Territorialidades", by.y = "name_muni")



## =========================================  caregando base com lomgitude e latitude ===========================================

coordanadas <- read_xls("data/bruto/anexo_16261_Coordenadas_Sedes_5565_Municípios_2010.xls")


## ========================================= merge entre distribuicao_renda e coordenadas =======================================

distribuicao_renda <- merge(distribuicao_renda, coordanadas, by.x = "code_muni", by.y = "GEOCODIGO_MUNICIPIO")

## =============================================== deletando colunas repididas ==================================================
coluna <- c(9)
distribuicao_renda <- distribuicao_renda[,-coluna]

## ================================================ geo.data para mapa de pontos ================================================

coordenadas_renda <- as.geodata(data.frame(coords = cbind(distribuicao_renda$LONGITUDE, distribuicao_renda$LATITUDE), 
                                            data = distribuicao_renda$PIB_2016_percapta))

