
# MAPA DE USO E COBERTURA DO SOLO DO CARIRI

# 1. Carregar as bibliotecas
library(sf)
library(terra)
library(ggplot2)
library(ggspatial)
library(tidyterra)
library(dplyr)

# 2. Carregar os Shapefiles
message("Carregando os limites territoriais...")
limite_cariri     <- st_read("Cariri_Limites.shp")
municipios_cariri <- st_read("Cariri_municipios.shp")

# 3. Carregar o Raster de Uso do Solo
message("Carregando o raster LULC_FINAL_CARIRI.tif...")
lulc_raster <- rast("LULC_FINAL_CARIRI.tif")


#  RECORTE E REPROJEÇÃO PARA UTM (SIRGAS 2000 / EPSG:31984)
#  Alinhar o limite temporariamente ao CRS do raster para fazer o corte quadrado

limite_original_crs <- st_transform(limite_cariri, crs(lulc_raster))
lulc_crop           <- crop(lulc_raster, limite_original_crs)

#  Reprojetar o Raster cortado para SIRGAS 2000 / UTM Zona 24S (EPSG:31984)

lulc_utm <- project(lulc_crop, "EPSG:31984", method = "near")

# Reprojetar os Shapefiles de Limites e Municípios para o mesmo EPSG:31984
limite_cariri_utm     <- st_transform(limite_cariri, "EPSG:31984")
municipios_cariri_utm <- st_transform(municipios_cariri, "EPSG:31984")

# Mascarar o raster final usando o contorno exato do Cariri em UTM

lulc_cariri <- mask(lulc_utm, vect(limite_cariri_utm))

# 4. Configurar e Nomear as 5 Classes de Cobertura

lulc_cariri <- as.factor(lulc_cariri)

tabela_classes <- data.frame(
  ID = c(1, 2, 3, 4, 5),
  Classe = c("Formação florestal",
             "Formação arbustiva",
             "Solo exposto",
             "Corpos hídricos",
             "Agricultura ou pasto")
)
lulc_cariri <- as.factor(lulc_cariri)

levels(lulc_cariri) <- list(tabela_classes)


# Paleta de Cores Customizada
cores_custom <- c(
  "Formação florestal"      = "#1f4423",
  "Formação arbustiva"               = "#7dc975",
  "Solo exposto"               = "#d4271e",
  "Corpos hídricos"            = "#2532e4",
  "Agricultura ou pasto"  = "#ffefc3"
)


# 5. Construção do Layout do Mapa Temático


mapa_final <- ggplot() +
  # Camada 1: O Raster reprojetado em UTM
  geom_spatraster(data = lulc_cariri) +

  # Camada 2: Limites dos Municípios em UTM
  geom_sf(data = municipios_cariri_utm, fill = NA, color = "grey35", linewidth = 0.25) +

  # Camada 3: Limite Geral do Cariri em UTM
  geom_sf(data = limite_cariri_utm, fill = NA, color = "black", linewidth = 0.7) +

  # Legenda e Cores
  scale_fill_manual(
    values = cores_custom,
    na.value = "transparent",
    name = "Classes de Uso e Cobertura",
    drop = TRUE
  ) +

  # Elemento Cartográfico: Escala Gráfica
  annotation_scale(
    location = "bl",
    width_hint = 0.2,
    style = "ticks",
    text_args = list(size = 9)
  ) +

  # Elemento Cartográfico: Seta do Norte
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
    style = north_arrow_fancy_orienteering(
      fill = c("grey10", "white"),
      line_col = "grey20"
    )
  ) +

  # Títulos e Metadados
  labs(
    title = "Uso e Cobertura do Solo no Cariri Paraibano",
    subtitle = "Cenário Atual",
    caption = "Fonte: Uso e cobertura do solo – Cenário Atual; Limites territoriais: IBGE. Organização e elaboração: Joyce Beatriz (2026)",
    x = "Easting (m)",
    y = "Northing (m)"
  ) +

  # Estética visual limpa
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 12)),
    plot.caption = element_text(size = 8, face = "italic", hjust = 0, margin = margin(t = 10)),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    panel.grid.major = element_line(color = "gray92", linetype = "dashed"),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Exibir o mapa no RStudio
print(mapa_final)

# 6. Salvar o arquivo final em alta resolução
ggsave(
  filename = "mapa_cariri_lulc.png",
  plot = mapa_final,
  width = 10,
  height = 8,
  dpi = 300
)



# CALCULO DE MÉTRICAS DA PAISAGEM


# 1.  carregar o pacote landscapemetrics

library(landscapemetrics)

# 2. Verificar se o raster atende aos requisitos do pacote
check_landscape(lulc_cariri)

# 3. Cálculo das métricas nos 3 níveis hierárquicos


# NÍVEL CLASS (Classe) - Composição (PLAND) e Configuração (NP, AI, ENN_MN)
metricas_class <- calculate_lsm(lulc_cariri, level = "class",
                                metric = c("pland", "np", "ai", "enn_mn"))

# NÍVEL LANDSCAPE (Paisagem) - Composição (SHDI) e Configuração (ED)
metricas_landscape <- calculate_lsm(lulc_cariri, level = "landscape",
                                    metric = c("shdi", "ed"))

# 4. Visualizar os resultados estruturados no console
print("--- MÉTRICAS NO NÍVEL DE CLASSE ---")
print(metricas_class)

print("--- MÉTRICAS NO NÍVEL DA PAISAGEM ---")
print(metricas_landscape)

# NÍVEL PATCH (Mancha) -  Calcular a Área de cada mancha

metricas_patch_area <- calculate_lsm(lulc_cariri, level = "patch", metric = "area")

#  Processar os dados para classificar os fragmentos (Pequeno, Médio, Grande)
library(dplyr)

resumo_tamanho_fragmentos <- metricas_patch_area %>%
  # Filtrar apenas as classes naturais (1 = Forest, 2 = Shrubland)
  filter(class %in% c(1, 2)) %>%
  # Criar a regra de classificação de tamanhos
  mutate(
    Categoria_Tamanho = case_when(
      value < 5  ~ "1. Pequeno (< 5 ha)",
      value >= 5 & value <= 50 ~ "2. Médio (5 a 50 ha)",
      value > 50 ~ "3. Grande (> 50 ha)"
    ),
    Nome_Classe = if_else(class == 1, "Formação florestal", "Formação arbustiva")
  ) %>%
  # Contar quantos fragmentos existem em cada categoria
  group_by(Nome_Classe, Categoria_Tamanho) %>%
  summarise(
    Quantidade_Manchas = n(),
    Area_Total_ha = sum(value),
    .groups = 'drop'
  ) %>%
  # Calcular a porcentagem
  group_by(Nome_Classe) %>%
  mutate(Porcentagem = round((Quantidade_Manchas / sum(Quantidade_Manchas)) * 100, 1)) %>%
  arrange(Nome_Classe, Categoria_Tamanho)


print("--- DISTRIBUIÇÃO DE TAMANHO DOS FRAGMENTOS NATURAIS ---")
print(resumo_tamanho_fragmentos)


# CALCULO DE ENN PARA OS 368 MAIORES FRAGMENTOS DE FLORESTA

library(terra)
library(dplyr)

# 1. Isolar apenas a classe de Floresta
floresta_mask <- lulc_cariri == 1
floresta_mask[floresta_mask == 0] <- NA  # Define o restante como NA

# 2. Agrupar os pixels em manchas individuais
floresta_manchas <- terra::patches(floresta_mask, directions = 8)

# 3. Vetorizar as manchas de floresta
floresta_poligonos <- terra::as.polygons(floresta_manchas)
names(floresta_poligonos) <- "patch_id" # Nomeia a coluna de IDs

# 4. Calcular a área exata de cada polígono em hectares
floresta_poligonos$area_ha <- terra::expanse(floresta_poligonos, unit = "ha")

# 5. Filtrar os 368 fragmentos GRANDES (> 50 hectares)
fragmentos_grandes <- floresta_poligonos[floresta_poligonos$area_ha > 50, ]

# 6. Calcular a matriz de distância (Borda a Borda)
# Entre os 368 grandes (linhas) e TODOS os 70k fragmentos de floresta (colunas)
matriz_distancia <- terra::distance(fragmentos_grandes, floresta_poligonos)

# 7. Remover a auto-distância (evitar que o fragmento ache a si mesmo como vizinho)
ids_todos <- floresta_poligonos$patch_id
ids_grandes <- fragmentos_grandes$patch_id

for(i in 1:nrow(matriz_distancia)) {
  mesmo_id_idx <- which(ids_todos == ids_grandes[i])
  matriz_distancia[i, mesmo_id_idx] <- Inf
}

# 8. Extrair a menor distância de cada linha
fragmentos_grandes$enn_m <- apply(matriz_distancia, 1, min)

# 9. Gerar Dataframe final
ranking_conservacao <- as.data.frame(fragmentos_grandes) %>%
  select(patch_id, area_ha, enn_m) %>%
  arrange(enn_m)

# Visualizar os 15 fragmentos mais conectados
print("TOP 15 FRAGMENTOS GRANDES E CONECTADOS (PRIORIDADE DE CONSERVAÇÃO)")
print(head(ranking_conservacao, 15))

# Salvar em CSV
write.csv(ranking_conservacao, "ranking_fragmentos_grandes_cariri.csv", row.names = FALSE)


# GERADOR DE PAINEL DE AMMOSTRAGEM EM GGPLOT2 PARA OS TOP 15 FRAGMENTOS

library(terra)
library(dplyr)
library(ggplot2)

# 1. IDs dos 15 fragmentos do ENN
ids_top15 <- c(277, 14418, 17889, 18291, 19741, 23649, 32066, 32377, 34348, 36378, 36394, 36822, 37885, 38737, 39326)

# 2. Filtrar esses 15 polígonos das manchas de floresta
poligonos_top15 <- floresta_poligonos[floresta_poligonos$patch_id %in% ids_top15, ]

# 3. Loop customizado usando sua estrutura com corte circular dinâmico
lista_recortes <- list()

for(i in 1:nrow(poligonos_top15)) {
  patch_i <- poligonos_top15[i, ]
  id_patch <- patch_i$patch_id
  area_ha <- round(patch_i$area_ha, 1)

  # Extrai o centroide para garantir um buffer perfeitamente redondo
  centroide_i <- terra::centroids(patch_i)

  # CALCULO DO RAIO DINÂMICO: Transforma ha em m², tira a raiz para achar o raio estimado
  # e soma 1.500 metros para mapear a vizinhança da matriz ao redor
  raio_dinamico <- sqrt((patch_i$area_ha * 10000) / pi) + 1500

  # Criar o buffer circular estável
  buffer_circular <- terra::buffer(centroide_i, width = raio_dinamico)

  # Cortar (crop) e mascarar (mask) no formato do círculo do buffer
  crop_i <- terra::crop(lulc_cariri, buffer_circular)
  mask_i <- terra::mask(crop_i, buffer_circular)

  # Converter para dataframe
  df_i <- as.data.frame(mask_i, xy = TRUE, cells = TRUE)

  # Identificar dinamicamente o nome da coluna de uso do solo
  coluna_classe <- names(df_i)[ncol(df_i)]
  names(df_i)[names(df_i) == coluna_classe] <- "Categoria"

  # Criar o rótulo elegante para o título de cada quadradinho
  df_i$id_buffer <- paste0("Patch ", id_patch, " (", area_ha, " ha)")

  lista_recortes[[i]] <- df_i
}

# 4. Consolidar todos os recortes em um único dataframe master
df_painel <- bind_rows(lista_recortes)

# 5. Paleta de cores
cores_cariri <- c(
  # Se o raster estiver como Texto:
  "Formação florestal" = "#006400",
  "Formação arbustiva"          = "#9ACD32",
  "Solo exposto"          = "#E6C280",
  "Corpos hídricos"              = "#4169E1",
  "Agricultura ou pasto"        = "#FF8C00",

  # Se o raster estiver como Número:
  "1" = "#006400", "2" = "#9ACD32", "3" = "#E6C280", "4" = "#4169E1", "5" = "#FF8C00"
)

# 6. Plotagem
  geom_tile(aes(x = x, y = y, fill = as.character(Categoria))) +
  scale_fill_manual(values = cores_cariri) +

  facet_wrap(~id_buffer, nrow = 3, ncol = 5, scales = "free") +
  theme_minimal() +
  labs(
    title = "Painel de Amostragem: 15 Maiores Fragmentos de Floresta",
    subtitle = "Recortes circulares com raio adaptativo de contexto na matriz do Cariri",
    fill = "Uso e Cobertura do Solo"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 9),
    legend.position = "bottom",
    legend.text = element_text(size = 10)
  )

# Exibir o gráfico
print(plot_painel)

# 7. Salvar o gráfico em alta definição
ggsave("painel_top15_patches_cariri.png", plot = plot_painel, width = 14, height = 9, dpi = 300)

#FIMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
