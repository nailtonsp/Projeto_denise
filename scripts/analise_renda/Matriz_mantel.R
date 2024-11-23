##  ============================================== testando normalidade da variavel PIB_2016_percapta ===========================

shapiro.test(renda_data$PIB_2016_percapta) ## os dados nao seguem uma distribuição normal


## =======================================================  calculando matriz_distancia A =======================================

matriz_cidades <- as.matrix(cbind(distribuicao_renda$LONGITUDE, distribuicao_renda$LATITUDE))

A <- spDists(x = matriz_cidades, y = matriz_cidades, longlat = FALSE)

## =========================================== calculando matriz diferenca entre pib B ==========================================

B <- outer(distribuicao_renda$PIB_2016_percapta, distribuicao_renda$PIB_2016_percapta, function(x,y) sqrt((x-y)^2))

## ============================================ Definindo numero de nucleeos que saram usados ===================================

options(mc.cores = 12)

## =========================================== Transformando as matrizes A e B em vetores ========================================

vec_a <- as.vector(A)
vec_b <- as.vector(B)

## ============================================ CALCULANDO CORELACAO USANDO METODO DE SPERMAN ====================================

cor(vec_a, vec_b, method="spearman")

cor(sample(vec_a), vec_b, method="spearman")

cor(sample(vec_a), vec_b, method="spearman")

cor(sample(vec_a), vec_b, method="spearman")

##  ===================================================== CALCULANDO O TESTE DE CORELACAO ========================================

cor.test(sample(vec_a),vec_b)
  
#  ===============================================================Teste de Mantel ================================================
 
proc = function(n,vector_coords,vector_resp){
# Lendo os dados
 
aux <- integer(n)

# Teste de aleatoriza

for (i in 1:n){
            A1.al <-  sample(vector_coords)
            
correlAl <-  cor(A1.al,vector_resp, method = "pearson")  
corelacao <-   cor(vector_coords,vector_resp, method = "pearson")

if (abs(correlAl) >= abs(corelacao))
            aux[i] <-  1
else aux[i] <- 0
}
 pvalor <- mean(aux)
 
 return(list(aux = aux, pvalor = pvalor))
 
}
 
resultados <- proc(n = 10000, vector_coords = vec_a, vector_resp = vec_b)



is.aux.null <- all(resultados$aux == 0)
