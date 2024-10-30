##  ============================================== testando normalidade da variavel PIB_2016_percapta ===========================

shapiro.test(renda_data$PIB_2016_percapta) ## os dados nao seguem uma distribuição normal


## =======================================================  calculando matriz_distancia A =======================================

matriz_cidades <- as.matrix(cbind(distribuicao_renda$LONGITUDE, distribuicao_renda$LATITUDE))

A <- spDists(x = matriz_cidades, y = matriz_cidades, longlat = FALSE)

## =========================================== calculando matriz diferenca entre pib B ==========================================

B <- outer(distribuicao_renda$PIB_2016_percapta, distribuicao_renda$PIB_2016_percapta, function(x,y) sqrt((x-y)^2))

## ============================================ rodando Teste de Mantel para detecção de padrão espacial ========================

options(mc.cores = parallel::detectCores())

mantel_teste <- mantel(A, B, permutations = 100000) ## 
