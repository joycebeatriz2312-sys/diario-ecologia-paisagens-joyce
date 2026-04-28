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
 
#2. EXERCICIO 4 ----------------------------------------------
 
 library(terra)
 library(tidyverse)
 library(tidyterra)   
 library(landscapemetrics)
 
setwd("C:/Users/Joyce Borges/OneDrive/Documentos/diario_ecologia_joyceb/exercicios") # mude para o seu próprio diretório
 
 r_murici <- rast("2024_murici.tif") 
 
 r_utm <- project(r_murici, "EPSG:31985", method = "near")
 
 legenda <- data.frame(
   id = c(3, 4, 11, 15, 20, 21, 24, 25, 33),
   Categoria = c("Formação Florestal", "Formação Savânica", "Campo Alagado", 
                 "Pastagem", "Cana", "Mosaico de Usos", "Área Urbanizada", 
                 "Outras Áreas não Vegetadas", "Corpo d'Água"),
   Cores = c("#006400", "#7cfc00", "#00ffff", "#ffff00", "#ff4500", 
             "#f4a460", "#ff0000", "#d3d3d3", "#0000ff")
 )
 
 levels(r_utm) <- legenda[,1:2] 
 vetor_total <- as.polygons(r_utm, dissolve = TRUE)
 municipio_limite <- aggregate(vetor_total)
 limite_seguro <- buffer(municipio_limite, width = -2000)
 
 floresta <- vetor_total[vetor_total$Categoria == "Formação Florestal", ]
 area_sorteio <- crop(floresta, limite_seguro)
 
 if (is.list(area_sorteio)) {
   area_sorteio <- do.call(rbind, area_sorteio)
 }
 
 if (nrow(area_sorteio) == 0) {
   stop("Erro: Nenhuma floresta encontrada a mais de 2km da borda.")
 } 
 ggplot() +
   geom_spatraster(data = r_utm) +
   scale_fill_manual(values = legenda$Cores, na.value = "white", name = "Uso do Solo") +
   geom_spatvector(data = municipio_limite, fill = NA, color = "black", linewidth = 1) +
   geom_spatvector(data = limite_seguro, fill = NA, color = "black", linetype = "dashed", linewidth = 1) +
   geom_spatvector(data = floresta, fill = "darkgreen", alpha = 0.3, color = NA) +
   geom_spatvector(data = area_sorteio, fill = "forestgreen", alpha = 0.5, color = NA) +
   labs(title = "Área de sorteio (floresta > 2 km da borda)") +
   theme_minimal() 
 
 #5 Sorteio de pontos com distância mínima
 
 set.seed(1234)
 pontos_finais <- NULL
 tentativas <- 0
 
 while(is.null(pontos_finais) || nrow(pontos_finais) < 15) {
   tentativas <- tentativas + 1
   cand <- spatSample(area_sorteio, size = 1, method = "random")
   
   if (nrow(cand) > 0) {
     if (is.null(pontos_finais)) {
       pontos_finais <- cand
     } else {
       dists <- distance(cand, pontos_finais)
       if (min(dists) >= 1000) { 
         pontos_finais <- rbind(pontos_finais, cand)
       }
     }
   }
   
   if (tentativas > 5000) break
 }
 
 #6 Buffers e extração da composição da paisagem
 
 # Verificar CRS
 if (!identical(crs(pontos_finais), crs(r_utm))) {
   pontos_finais <- project(pontos_finais, crs(r_utm))
 }
 
 buffers <- buffer(pontos_finais, width = 500)
 buffers$id_paisagem <- 1:nrow(buffers)
 
 # Extração e processamento
 extracao <- terra::extract(r_utm, buffers)
 nome_col_raster <- names(extracao)[2]
 
 tabela_final <- extracao %>%
   rename(id_buffer = ID, categoria_bruta = !!sym(nome_col_raster)) %>%
   group_by(id_buffer) %>%
   mutate(total_px = n()) %>%
   group_by(id_buffer, categoria_bruta) %>%
   summarise(
     pixels = n(),
     percentagem = (pixels / first(total_px)) * 100,
     .groups = "drop"
   ) %>%
   mutate(Categoria_Limpa = as.character(categoria_bruta)) %>%
   left_join(legenda %>% select(Categoria, Cores), by = c("Categoria_Limpa" = "Categoria"))
 
 write.csv(tabela_final, "uso_solo_murici_final.csv", row.names = FALSE)
 writeVector(pontos_finais, "pontos_final.shp", overwrite=TRUE)

 writeVector(buffers, "buffers_500m.shp", overwrite=TRUE)

 ggplot() +
   geom_spatraster(data = r_utm) +
   scale_fill_manual(values = legenda$Cores, na.value = "white", name = "Uso do Solo") +
   geom_spatvector(data = buffers, fill = NA, color = "black", linewidth = 1) +
   geom_spatvector(data = pontos_finais, color = "red", size = 2, shape = 19) +
   geom_spatvector_text(data = pontos_finais, aes(label = 1:15), 
                        color = "white", size = 3, vjust = -0.8) +
   labs(title = "Pontos amostrais e buffers de 500 m") +
   theme_minimal() +
   theme(legend.position = "bottom") 
 
 #7 Análise com landscapemetrics
 
 # Identificar o valor numérico da classe Floresta
 id_floresta <- legenda$id[legenda$Categoria == "Formação Florestal"]
 
 # Lista para armazenar métricas de cada buffer
 metricas_lista <- list()
 
 for(i in 1:nrow(buffers)) {
   # Recortar o raster para o buffer i
   crop_i <- crop(r_utm, buffers[i,])
   mask_i <- mask(crop_i, buffers[i,])
   
   # Calcular Shannon diversity (nível da paisagem)
   shannon <- lsm_l_shdi(mask_i)
   
   # Calcular métricas de classe para todas as classes
   pland_all <- lsm_c_pland(mask_i)   # percentual de cada classe
   ed_all <- lsm_c_ed(mask_i)         # densidade de borda por classe
   
   # Filtrar apenas a classe floresta
   pland_floresta <- pland_all %>% filter(class == id_floresta)
   ed_floresta <- ed_all %>% filter(class == id_floresta)
   
   # Extrair valores (se não houver floresta, colocar NA ou 0)
   div_shannon <- shannon$value
   perc_floresta <- ifelse(nrow(pland_floresta) > 0, pland_floresta$value, 0)
   dens_borda <- ifelse(nrow(ed_floresta) > 0, ed_floresta$value, NA)
   
   # Guardar
   metricas_lista[[i]] <- data.frame(
     id_buffer = i,
     diversidade_shannon = div_shannon,
     perc_floresta = perc_floresta,
     densidade_borda_m_ha = dens_borda
   )
 }
 
 # Combinar tudo
 metricas_paisagem <- bind_rows(metricas_lista)
 
 print(metricas_paisagem)
 
 #8 Painel de paisagens
 
 lista_recortes <- list()
 for(i in 1:nrow(buffers)) {
   crop_i <- crop(r_utm, buffers[i, ])
   mask_i <- mask(crop_i, buffers[i, ])
   df_i <- as.data.frame(mask_i, xy = TRUE, cells = TRUE)
   df_i$id_buffer <- paste("Paisagem", i)
   lista_recortes[[i]] <- df_i
 }
 df_painel <- bind_rows(lista_recortes)
 
 ggplot(df_painel) +
   geom_tile(aes(x = x, y = y, fill = Categoria)) +
   scale_fill_manual(values = setNames(legenda$Cores, legenda$Categoria)) +
   facet_wrap(~id_buffer, nrow = 3, ncol = 5, scales = "free") +
   theme_minimal() +
   labs(title = "Painel de Amostragem: 15 Paisagens de Murici-AL",
        subtitle = "Recortes circulares de 500m de raio (Interior de Floresta)",
        fill = "Uso do Solo") +
   theme(axis.text = element_blank(),
         axis.ticks = element_blank(),
         panel.grid = element_blank(),
         legend.position = "bottom")
 
 
 
 