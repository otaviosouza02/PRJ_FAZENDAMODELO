# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PRJ_Modelo_Inv1: Inventário da Fazenda Modelo   ~~~~~~~~~~~~~~~~~~~~~
#
# Autor: Luiz Carlos Estraviz Rodriguez
#        Otávio Magalhães Silva Souza
#        Departamento de Ciências Florestais
#        ESALQ/USP - 22/Set/2024
#
#   - Estimativas de inventário com amostragem de uma fase
#        Casual Simples (ACS)                             
#        Casual Estratificada (ACE)                             
#
# Linguagem de programação:
#       R (v 4.4.1)
#       package: forestinventory (v 1.0.0 2021-01-08 - Andreas Hill)
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls(all=TRUE))                                   # Limpa memória
gc()

if(!require(lidR))
  install.packages("lidR")
library(lidR)

if(!require(rio))
  install.packages("rio")
library(rio)

# Define diretórios
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
prjNome <- "PRJ_Modelo"                               # Nomeia o projeto
dirProjet <- str_c('C:/GitRepo/',prjNome) # Define diretório de trabalho
if (!dir.exists(dirProjet)) {          # Cria diretório caso não exista
  dir.create('C:/GitRepo/', showWarnings = F)
  dir.create(dirProjet, showWarnings = F)
}
setwd(dirProjet)                     # Define pasta default de trabalho

datDir <- str_c('C:/LiDAR/', prjNome)    # Define raiz da pasta de dados
if (!dir.exists(datDir)) {           # Cria diretórios caso não existam
  dir.create('C:/LiDAR/', showWarnings = F)
  dir.create(datDir, showWarnings = F)
}

lidDir <- str_c(datDir, '/NUVENS/')     # Cria sub-pasta p/ dados LiDAR
if (!dir.exists(lidDir)) {
  dir.create(lidDir, showWarnings = F)
}

shpDir <- str_c(dirProjet, '/SHAPES') # Define diretório para os shapes
if (!dir.exists(shpDir)) 
  
grdDir <- str_c(datDir, '/DADOS/')     # Cria sub-pasta p/ dados LiDAR
if (!dir.exists(lidDir)) {
  dir.create(lidDir, showWarnings = F)
}

# Download da nuvem de pontos LiDAR para o ano de 2013
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
gitOnde <-
  "https://github.com/FlorestaR/dados/blob/main/5_LIDARF/Modelo/CLOUDS/"
gitNome <-c("L0004-C0005.laz", "L0004-C0006.laz",
            "L0005-C0005.laz", "L0005-C0006.laz",      # arquivos LiDAR
            "L0006-C0005.laz", "L0006-C0006.laz")    
anoData <- c("A13")                  # Lê apenas os dados LiDAR de 2013
# anoData <- c("A13", "A14")       # Se quiser baixar os dois conjuntos

# Download dos tiles
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
options(timeout=1000) # Reset timeout oferecendo mais tempo de download
for (ano in anoData) {     # guarda na sub-pasta de dados LiDAR por ano
  dirLAZ <- str_c(lidDir, ano)
  dir.create(dirLAZ, showWarnings = T)
  for (nome in gitNome){         # acrescenta "?raw=true" no fim do URL
    gitFile <- str_c(gitOnde, ano, "/", ano, nome, "?raw=true")
    localFl <- str_c(dirLAZ, "/", nome)
    if(!file.exists(localFl))           # garante download binário (wb)
      download.file(gitFile, mode="wb", destfile = localFl)
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Leitura do catálogo de dados LiDAR                     (2013 ou 2014)
# Exibe conteúdo do catálogo e confere se os mapas de tiles são iguais
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dirLAZ  <- str_c(lidDir, anoData[1])   #  Define local das nuvens LiDAR
ctg_orig <- readLAScatalog(dirLAZ) # LEITURA DAS NUVENS DE PONTOS LiDAR
opt_select(ctg_orig) <- "xyzic" 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lê shape e atributos dos talhoes
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(!require("sf"))
  install.packages(sf)
library(sf)                                # deleta o arquivo zipado

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Download do shape da Fazenda Modelo (2 layers: talhoes e parcelas)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gitOnde <- "https://github.com/FlorestaR/dados/blob/main/5_LIDARF/Modelo/SHAPES"
gitNome <- "fazmodelo.zip"
gitArqv <- file.path(gitOnde, gitNome) %>% paste0("?raw=true")

tmpd <- tempdir(check = TRUE)                    # diretório temporário
zipf <- file.path(tmpd, "shapes.zip")              # arquivo temporário

options(timeout=1000) # Reset timeout oferecendo mais tempo de download
if(!file.exists(zipf))  # garante download de dados binários (wb)
  download.file(gitArqv, mode="wb", destfile = zipf) 

unzip(zipf, exdir = tmpd)     # shape é unziped no diretório temporário
unlink(zipf)   

shpArq <- paste0(tmpd, "/Modelo_talhoes.shp")       # shape com talhões
tal_shp <- read_sf(shpArq)                    # completo com geom

shpGrd <- paste0(tmpd, "/Modelo_grid.shp")       # shape com talhões
grid <- read_sf(shpGrd)  

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Os tiles de dados originais abrangem uma área maior do que a
# necessária. Para reduzir a informação LiDAR que será usada, uma 
# alternativa é usar os limites dos talhões mais um buffer de 10m
# como "tesoura" para clipar as nuvens.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dirTal <- str_c(dirLAZ, '/TALHOES')        # Pasta p/ nuvens por talhao
if (!dir.exists(dirTal)) {
  dir.create(dirTal, showWarnings = F)
}

opt_chunk_buffer(ctg_orig) <- 10                          # 10 m buffer
opt_output_files(ctg_orig) <- str_c(dirTal, "/{SUBTALHAO}")
opt_laz_compression(ctg_orig) <- TRUE              # Mantém formato LAZ
ctg_talh = clip_roi(ctg_orig, tal_shp) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Classifica pontos de solo             (este passo pode ser bem lento)
#       ... espere até que o sinal "stop" desapareça na janela de "log"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dirNoN <- str_c(dirTal, '/NoNORM') # Nuvens c/ ground não normalizadas
if (!dir.exists(dirNoN)) {
  dir.create(dirNoN, showWarnings = F)
}
opt_output_files(ctg_talh) <- str_c(dirNoN, "/{*}_g")
ctg_gNoN                   <- classify_ground(ctg_talh, csf())

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Normaliza as nuvens de pontos dos talhões          (igualmente lento)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dirNrm <- str_c(dirTal, '/SiNORM')       # Pasta p/ nuvens normalizadas
if (!dir.exists(dirNrm)) {
  dir.create(dirNrm, showWarnings = F)
}
opt_output_files(ctg_gNoN ) <- str_c(dirNrm, "/{*}SiN")
ctg_gSiN                    <- normalize_height(ctg_gNoN, tin())

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Retile dos talhões normalizados para geração de um conjunto de núvens
# mais apropriado para a correta clipagem das parcelas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dirTNrm <- str_c(dirNrm, '/TILES')        # Pasta p/ tiles normalizados
if (!dir.exists(dirTNrm)) {
  dir.create(dirTNrm, showWarnings = F)
}
# crashou
ctg_gSiN <- readLAScatalog(dirNrm)
# ctg_gSiN <- readLAScatalog(dirTNrm)
opt_output_files(ctg_gSiN) <-     # Onde guardar as nuvens normalizadas
  str_c(dirTNrm, "/FzMod_{XLEFT}_{YBOTTOM}")    # renomeadas com coords
opt_chunk_buffer(ctg_gSiN) <- 0     # sem buffers ao redor de cada tile
opt_chunk_size(ctg_gSiN) <- 100                     # em tiles de 100 m
opt_laz_compression(ctg_gSiN) <- TRUE              # Mantém formato LAZ
ctg_tile <- catalog_retile(ctg_gSiN)                        # e executa
rm(ctg_gNoN, ctg_gSiN, ctg_orig, ctg_talh) # limpa da memória catálogos

# Exibe localização dos tiles, talhoes e parcelas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot(ctg_tile, mapview = TRUE, map.type = "Esri.WorldImagery")
plot(ctg_tile)
plot(tal_shp)

# Clipa e salva as núvens das parcelas, e calcula
# métricas para cada "voxel" clipado com o shape de parcelas no tiles
# normalizados usando a função .stdmetrics_z de métricas genéricas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dirParc <- str_c(dirNrm, '/PARCELAS')      # Pasta p/ parcelas clipadas
if (!dir.exists(dirParc)) {
  dir.create(dirParc, showWarnings = F)
}

opt_output_files(ctg_tile) <-     # Onde guardar as nuvens das parcelas
  str_c(dirParc, "/P_{parcela}") # renomeadas pelo respectivo número
opt_select(ctg_tile) <- "xyz"   # Carrega na memória apenas coordenadas   
opt_filter(ctg_tile) <- "-drop_z_below 0"     # Ignora pontos com z < 0


# Organização dos dados para cálculo das estimativas e inferências
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
cellweight <- grid$areacell/400 
grid <- cbind(grid, cellweight) # Cria coluna para cell weight

# Seleciona apenas as parcelas em que a área é maior do que 1 m²
grid_final <- grid %>%
  filter_at(vars(areacell), all_vars(. >= 1))

metrics_list <- list() # Inicializa uma lista para armazenar as métricas de cada parcela

# Loop sobre cada parcela no shapefile
for (i in 1:nrow(grid_final)) { # Para cada número de linhas do data frame que contém as informações 
  shp_parcela <- grid_final[i, ]  # Pegue a i-ésima parcela
  if (nrow(clip_roi(ctg_tile, shp_parcela))  == 0) {
    cat("Nenhum ponto encontrado para a parcela ", i, "\n")  # Depuração
  } else {
  las_crop <- clip_roi(ctg_tile, shp_parcela)  # Recorte os pontos LiDAR para essa parcela e converta para LAS
  if (class(las_crop) == "LAScatalog") { # Verifique se o retorno é um LAScatalog, e se sim, converta para LAS. 
    las_crop <- readLAS(las_crop) 
  }
  # Verifique se a parcela contém pontos LiDAR
  if (!is.null(las_crop)) {
    metrics <- cloud_metrics(las_crop, func = .stdmetrics_z)     # Calcule as métricas para essa parcela
    # Converta as métricas para um data frame e adicione um identificador da parcela
    metrics_df <- as.data.frame(t(metrics))  # Transforma as métricas em um data frame com uma linha
    grid_final$identificador[i] <- i
    metrics_df$identificador <- i
    metrics_list[[i]] <- metrics_df  # Armazene as métricas no data frame
  } 
}
}

metrics_df_final <- do.call(rbind, metrics_list) # Combine todas as métricas em um único data frame

# Fazer o merge das métricas com os dados
final_df <- merge(grid_final, metrics_df_final, by = "identificador") %>%
  filter_at(vars(areacell), all_vars(. >= 1))

final_df <- as.data.frame(final_df)

final_df <- final_df %>% 
  mutate(across(where(is.list), ~ sapply(., unlist))) %>%
  select(-geometry)


metricas_lidarEXC <- str_c(datDir, "/DADOS/", prjNome, "_metrics.xlsx")
write.csv(final_df, metricas_lidarEXC, row.names = FALSE)
