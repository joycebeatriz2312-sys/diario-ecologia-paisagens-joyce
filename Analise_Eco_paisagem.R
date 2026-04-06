# Vetor com todos os pacotes necessários
pacotes <- c(
  # Dados espaciais
  "terra",          # Análise de dados raster (sucessor moderno do 'raster')
  "sf",             # Dados vetoriais em formato tidy (Simple Features)
  
  # Métricas de paisagem
  "landscapemetrics", # >130 métricas de padrão de paisagem (substitui FRAGSTATS)
  
  # Visualização
  "tmap",           # Mapas temáticos estáticos e interativos
  "ggplot2",        # Gráficos e visualizações gerais
  "tidyterra",      # Integração do terra com ggplot2
  
  # Ecologia
  "vegan",          # Diversidade, ordenação, análises multivariadas
  "iNEXT",          # Rarefação e extrapolação de diversidade
  
  # Manipulação de dados (Tidyverse)
  "dplyr",          # Manipulação de tabelas (filter, select, mutate...)
  "tidyr",          # Organização de dados (pivot_longer, pivot_wider...)
  "readr",          # Leitura de arquivos CSV e similares
  "tibble",         # Tabelas melhoradas (alternativa ao data.frame)
  "purrr",          # Programação funcional (map, walk...)
  
  # Acesso a dados abertos
  "geodata",        # Download de dados climáticos, elevação, etc.
  "rgbif",          # Acesso ao GBIF (ocorrências de espécies)
  "rnaturalearth",  # Fronteiras e mapas-base do Natural Earth
  "rnaturalearthdata", # Dados complementares do Natural Earth
  "geobr" ,        # Shapefiles da geografia Brasileira
  
  # Utilitários
  "remotes",        # Instalar pacotes do GitHub
  "here"            # Gerenciamento de caminhos de arquivo
)

# Instalar apenas os que ainda não estão instalados
novos <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]

if (length(novos) > 0) {
  cat("Instalando", length(novos), "pacote(s)...\n")
  install.packages(novos)
} else {
  cat("Todos os pacotes já estão instalados!\n")
}

install.packages("tidyverse")

library(terra)
library(sf)
library(landscapemetrics)
library(tmap)
library(vegan)
library(tidyverse)

cat("✓ Todos os pacotes carregados com sucesso!\n")

###########################################################

library(terra)

#terra 1.8.93
# Criar um raster de exemplo (paisagem simulada)
set.seed(42)
r <- rast(nrows = 50, ncols = 50, 
          xmin = 0, xmax = 50, ymin = 0, ymax = 50)

# Simular 4 classes de uso do solo
values(r) <- sample(1:4, ncell(r), replace = TRUE,
                    prob = c(0.50, 0.20, 0.20, 0.10))

# Nomear as classes
levels(r) <- data.frame(
  id    = 1:4,
  label = c("Floresta", "Pastagem", "Agricultura", "Água")
)

# Visualizar
plot(r, 
     col     = c("darkgreen", "lightgreen", "orange", "lightblue"),
     main    = "Paisagem simulada — uso do solo",
     mar     = c(3, 3, 2, 6))

# Informações do raster
print(r)          # Dimensões, extensão, CRS
res(r)            # Resolução espacial
ext(r)            # Extensão geográfica
ncell(r)          # Número total de células
freq(r)           # Frequência de cada classe (tabela de área)

#4.2 sf — Dados Vetoriais

library(sf)

# Criar pontos de amostragem simulados (ex: parcelas de campo)
set.seed(7)
parcelas <- st_as_sf(
  data.frame(
    id        = 1:10,
    longitude = runif(10, 5, 45),
    latitude  = runif(10, 5, 45),
    riqueza   = rpois(10, lambda = 12)
  ),
  coords = c("longitude", "latitude"),
  crs    = NA  # sem CRS definido para este exemplo
)

# print(parcelas)

# Visualizar sobre o raster
plot(r, col = c("darkgreen", "lightgreen", "orange", "lightblue"),
     main = "Parcelas sobre o mapa de uso do solo")
plot(st_geometry(parcelas), add = TRUE, pch = 21, 
     bg = "red", cex = 1.5)

#landscapemetrics

library(landscapemetrics)
library(terra)

# Carregar paisagem de exemplo interna do pacote
paisagem <- terra::rast(landscapemetrics::landscape)

# ---- VERIFICAÇÃO OBRIGATÓRIA antes de calcular métricas ----
check_landscape(paisagem)

# -------- VER A PAISAGEM CRIADA ARTIFICIALMENTE

plot(paisagem)

# ---- Métricas de PATCH (nível de fragmento) ----

# Área de cada fragmento (em hectares)
areas <- lsm_p_area(paisagem)
head(areas)

# Distância ao vizinho mais próximo (isolamento)
isolamento <- lsm_p_enn(paisagem)
head(isolamento)

# ---- Métricas de CLASS (nível de classe) ----

# Proporção de cada classe na paisagem
proporcao <- lsm_c_pland(paisagem)
print(proporcao)

# Total de borda por classe
borda <- lsm_c_te(paisagem)
print(borda)

# ---- Métricas de LANDSCAPE (nível de paisagem) ----

# Índice de diversidade de Shannon
shannon <- lsm_l_shdi(paisagem)
print(shannon)

# Área total
area_total <- lsm_l_ta(paisagem)
print(area_total)

#calculando multiplas metricas de uma vez

# Calcular várias métricas simultaneamente
metricas <- calculate_lsm(
  paisagem,
  what = c(
    "lsm_l_ta",    # Área total
    "lsm_l_shdi",  # Diversidade de Shannon
    "lsm_l_pd",    # Densidade de fragmentos
    "lsm_c_pland", # Proporção de cada classe
    "lsm_c_np"     # Número de fragmentos por classe
  )
)

print(metricas)

# Listar TODAS as 130+ métricas disponíveis
 list_lsm()  # descomente para ver a lista completa

 #visualizando fragmentos
 
 # Visualizar fragmentos rotulados por classe
 show_patches(paisagem, class = "all", labels = TRUE)
 
 # Visualizar área de núcleo (interior dos fragmentos)
 show_cores(paisagem, class = c(1, 2))

 
 #4.4 tmap — Mapas Temáticos
 
 #| eval: true
 #| warning: false
 
 library(tmap)
 library(terra)
 
 # Criar paisagem simulada para o mapa
 set.seed(42)
 r <- rast(nrows = 50, ncols = 50,
           xmin = -35.5, xmax = -35.0,
           ymin = -8.5,  ymax = -8.0,
           crs  = "EPSG:4326")
 values(r) <- sample(1:4, ncell(r), replace = TRUE,
                     prob = c(0.50, 0.20, 0.20, 0.10))
 levels(r) <- data.frame(
   id    = 1:4,
   label = c("Floresta", "Pastagem", "Agricultura", "Água")
 )
 
 # Modo interativo
 tmap_mode("view")  # mapa interativo (navegável)
 
 tm_shape(r) +
   tm_raster(
     col.scale = tm_scale_categorical(
       values = c("darkgreen", "lightgreen", "orange", "lightblue")
     ),
     col.legend = tm_legend(title = "Uso do Solo")
   ) +
   tm_title("Paisagem Simulada — Pernambuco") +
   tm_scalebar()
 
 #| eval: true
 #| warning: false
 
 # Modo estático (para publicação)
 tmap_mode("plot")
 
 tm_shape(r) +
   tm_raster(
     col.scale = tm_scale_categorical(
       values = c("darkgreen", "lightgreen", "orange", "lightblue")
     ),
     col.legend = tm_legend(title = "Uso do Solo")
   ) +
   tm_title("Paisagem Simulada") +
   tm_scalebar(position = c("left", "bottom")) +
   tm_compass(position = c("right", "top"))

 
 #4.5 vegan — Ecologia de Comunidades
 
 library(vegan)
 
 # Dados simulados: matriz de abundância (sites × espécies)
 set.seed(99)
 comunidades <- matrix(
   rpois(10 * 20, lambda = c(rep(5, 100), rep(1, 100))),
   nrow = 10, ncol = 20,
   dimnames = list(
     paste0("Site_", 1:10),
     paste0("Sp_",   1:20)
   )
 )
 
 comunidades

 # ---- Diversidade alfa ----
 div_shannon <- diversity(comunidades, index = "shannon")
 div_simpson <- diversity(comunidades, index = "simpson")
 riqueza     <- specnumber(comunidades)
 
 resultados_div <- data.frame(
   site      = rownames(comunidades),
   riqueza   = riqueza,
   shannon   = round(div_shannon, 3),
   simpson   = round(div_simpson, 3)
 )
 
 print(resultados_div) 

 # ---- Ordenação (NMDS) ----
 nmds <- metaMDS(comunidades, distance = "bray", 
                 trymax = 50, trace = FALSE)
 
 cat("Stress do NMDS:", round(nmds$stress, 3), "\n")
 
 # Stress < 0.1 = excelente; < 0.2 = bom; > 0.3 = problemático
 
 # Visualizar
 plot(nmds, type = "t", main = "NMDS — Composição de Comunidades") 
 
 
 #5 Fluxo de trabalho integrado
 
#| eval: true 
   
   library(terra)
 library(landscapemetrics)
 library(ggplot2)
 library(dplyr)
 
 # 1. CARREGAR paisagem (aqui usamos exemplo interno)
 paisagem <- terra::rast(landscapemetrics::landscape)
 
 # 2. VERIFICAR a paisagem
 check_landscape(paisagem)
 
 # 3. CALCULAR métricas de fragmento (área e isolamento)
 metricas_patch <- calculate_lsm(
   paisagem,
   what = c("lsm_p_area", "lsm_p_enn", "lsm_p_para")
 )
 
 # 4. ORGANIZAR os resultados com dplyr
 fragmentos <- metricas_patch |>
   select(class, id, metric, value) |>
   tidyr::pivot_wider(names_from = metric, values_from = value) |>
   rename(area = area, isolamento = enn, perimetro_area = para)
 
 head(fragmentos)
 
 # 5. VISUALIZAR a relação área × isolamento
 ggplot(fragmentos, aes(x = area, y = isolamento, color = factor(class))) +
   geom_point(size = 3, alpha = 0.7) +
   scale_color_manual(
     values = c("1" = "darkgreen", "2" = "orange", "3" = "steelblue"),
     labels = c("Classe 1", "Classe 2", "Classe 3"),
     name   = "Classe"
   ) +
   labs(
     title   = "Relação Área × Isolamento dos Fragmentos",
     x       = "Área do fragmento (ha)",
     y       = "Distância ao vizinho mais próximo (m)",
     caption = "Métricas calculadas com landscapemetrics"
   ) +
   theme_classic(base_size = 13)
 
 