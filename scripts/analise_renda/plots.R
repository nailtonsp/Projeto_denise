## mapa para variavel PIB_2016_per_capta 


##  ====================================================  intervalos ==========================================================

summary(distribuicao_renda$PIB_2016_percapta)

intervalo <- c(-Inf,4.522,5.485 ,7.058,Inf)
intervalo <- findInterval(distribuicao_renda$PIB_2016_percapta, intervalo)

distribuicao_renda <- distribuicao_renda |>
  mutate(interval_plot = factor(intervalo))

##  ===================================================  criando paleta de cores ==============================================


palete_cores <- c("#FFFFC8",
                  "#F9D67E",
                  "#F39300",
                  "#DA3500",
                  "#7D0025")


##  =======================================================  criando legenda ===================================================

legend <- c("-Inf a 4.52", "4.52 a 5.48", "5.48, a 7.05", "7.05, a Inf")

## ======================== mapa para variavel PIB_2016_per_capta ===============================================================

distribuicao_renda |>
  ggplot() +
  geom_sf(aes(geometry = geom, fill = interval_plot),
          color = "black") +
  labs(title = "Distribuicao De Renda",
       fill = "Legenda") +
  scale_fill_manual(values = palete_cores, labels = legend) +
  theme_minimal()

## ============================================ mapa de pontos ==================================================================

points(coordenadas_renda, pt.divide = "quart", cex.min = .8, cex.max = .8, borders = coordenadas_renda$borda)

