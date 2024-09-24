# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Inventário com dados LiDAR na Fazenda Modelo ~~~~~~~~~~~~~~~~~~~~~~~~
#
# Autor: Luiz Carlos Estraviz Rodriguez
#        Departamento de Ciências Florestais
#        ESALQ/USP - 02/Dez/2023
#
# Inventário florestal da Fazenda Modelo
#   - download dos dados mantidos em um repositório github público
#      - shape files dos talhões florestais
#      - LiDAR multitemporal (2013 e 2014)
#   - sugestão de pastas locais
#        C:/LiDAR/PRJ_Modelo/NUVENS/     como pasta para os dados LidAR
#        C:/GitRepo/PRJ_Modelo/     sendo GitRepo a pasta onde ficam os
#                                      projetos sincronizados no github
#
# Linguagem de programação:
#       R (v 4.3)
#       RStudio (v. 2023.09.1 Build 494)
#       Pacote lidR* (v 4.0.3 March 11th, 2023) Jean Romain Roussel
#
# Bibliografia complementar: 
#  https://github.com/r-lidar/lidR/wiki/Area-based-approach-from-A-to-Z
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Carrega pacotes, organiza pastas e define funções locais
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
if(!require(tidyverse))    # Para melhor manipulação de dados e funções
  install.packages("tidyverse")
library(tidyverse)

if(!require(ggplot2))
  install.packages("ggplot2")
library(ggplot2)

if(!require(ggpubr))
  install.packages("ggpubr")
library(ggpubr)

if(!require(sf))            # Para manipulação de shapes e outros tipos
  install.packages("sf")                      # de dados espacializados 
library(sf)

if(!require(foreach))
  install.packages("foreach")
library(foreach)

if(!require(tidyterra))           # Package para processar mapas raster
  install.packages("tidyterra")
library(tidyterra)

if(!require(terra))           # Package para processar mapas raster
  install.packages("terra")
library(terra)

if(!require(stars))                  # Package que permite rasterização
  install.packages("stars")
library(stars)

if(!require(tools))           # Package para manipular nomes de aquivos
  install.packages("tools")
library(tools)

if(!require(RColorBrewer))          # Package com mais paletas de cores
  install.packages("RColorBrewer")
library(RColorBrewer)

if(!require(progress))          # Package com mais paletas de cores
  install.packages("progress")
library(progress)

if(!require(reshape2))              # Usado na função que gera gráficos
  install.packages("reshape2")         # mais caprichados de correlação
library(reshape2)

if(!require(mapview))             # Package para incluir mapas de fundo
  install.packages("mapview")
library(mapview)

if(!require(forestinventory))             # Package para incluir mapas de fundo
  install.packages("forestinventory")
library(forestinventory)

# devtools::install_github("Jean-Romain/rlas", dependencies=TRUE)
# devtools::install_github("Jean-Romain/lidR")
if(!require(lidR))                     # PARA MANPULAÇÃO DE DADOS LiDAR
  install.packages("lidR") # Se preferir instalar a versão mais recente 
library(lidR)              # e tiver o package devtools instalado, rode
# alternativamente as duas linhas acima

if(!require(RCSF))    #        Pacote complementar do lidR e necessário
  install.packages("RCSF")           # rodar a função classify_ground()
library(RCSF)

if(!require(future))         # Package que permite ao lidR rodar usando
  install.packages("future")          # processamento paralelo de dados
library(future)
#cores  <- as.integer(parallel::detectCores() - 4)
#plan(multisession, workers = cores)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define pastas e local de trabalho
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
prjNam <- "PRJ_FAZENDAMODELO"                               # Nomeia o projeto
varteste = 0
dirProjet <- str_c('C:/GitRepo/',prjNam) # Define diretório de trabalho
if (!dir.exists(dirProjet)) {          # Cria diretório caso não exista
  dir.create('C:/GitRepo/', showWarnings = F)
  dir.create(dirProjet, showWarnings = F)
}
setwd(dirProjet)                     # Define pasta default de trabalho

datDir <- str_c('C:/LiDAR/', prjNam)    # Define raiz da pasta de dados
if (!dir.exists(datDir)) {           # Cria diretórios caso não existam
  dir.create('C:/LiDAR/', showWarnings = F)
  dir.create(datDir, showWarnings = F)
}

lidDir <- str_c(datDir, '/NUVENS/')     # Cria sub-pasta p/ dados LiDAR
if (!dir.exists(lidDir)) {
  dir.create(lidDir, showWarnings = F)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cálculo do número ideal de unidades amostrais em double sampling, ou
# seja, da intensidade amostral necessária para gerar o erro admissível
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dsNumberOfPlots = function(y, x, xLarge, Cpg = 300, 
                           errDesired = 0.1, alpha = 0.05){
  
  rho = cor(x,y)
  a = var(y) * (1 - rho^2)
  b = var(y) * rho^2
  
  B = errDesired * mean(y)
  qt = qt(1 - alpha/2, length(y)-1)
  
  nG = (sqrt( a*b*Cpg ) + b) / (B^2 / qt^2)
  nP = (sqrt( a*b/Cpg ) + a) / (B^2 / qt^2)
  
  return(nP)
}

seramesmo = dsNumberOfPlots(x = X$zq95,
                            y = X$VTCC)
seramesmo
# ----


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Leitura de dados
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define o nome do arquivo com os dados espacializados disponíveis para
# a área de estudo, faz o download e salva esses dados na devida pasta
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
# Define o nome do arquivo com os dados espacializados disponíveis para
# a área de estudo, faz o download e salva esses dados na devida pasta
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gitOnde <-
  "https://github.com/FlorestaR/dados/blob/main/5_LIDARF/Modelo/SHAPES"
gitNome <- "fazmodelo.zip"                   # shapes da fazenda modelo
gitArqv <- file.path(gitOnde, gitNome) %>% str_c("?raw=true")

tmpd <- tempdir(check = TRUE)                    # diretório temporário
zipf <- file.path(tmpd, "shapes.zip")              # arquivo temporário

# Download e unzip do coonteúdo do arquivo zipado
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
options(timeout=1000)
if(!file.exists(zipf)) {
  download.file(gitArqv, mode="wb", destfile = zipf) 
}

shpDir <- str_c(dirProjet, '/SHAPES') # Define diretório para os shapes
if (!dir.exists(shpDir)) {             # Cria diretório caso não exista
  dir.create(shpDir, showWarnings = F)
}

unzip(zipf, exdir = shpDir)   # unzipa shps e guarda na pasta de shapes
unlink(zipf)                                  # deleta o arquivo zipado
# ----


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Processamento da variável auxiliar LiDAR e estudo da correlação
#    das métricas LiDAR com os parâmetros de interesse
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Leitura do catálogo de dados LiDAR                     (2013 ou 2014)
# Exibe conteúdo do catálogo e confere se os mapas de tiles são iguais
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dirLAZ  <- str_c(lidDir, anoData[1])   #  Define local das nuvens LiDAR
ctg_orig <- readLAScatalog(dirLAZ) # LEITURA DAS NUVENS DE PONTOS LiDAR
opt_select(ctg_orig) <- "xyzic" # Só atributos xyz intensid. e classif.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lê shape e atributos dos talhoes
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shpArq  <- str_c(shpDir, "/Modelo_talhoes.shp")      # shape de talhões
tal_shp <- st_read(shpArq)
st_is_valid(tal_shp)        # confere se os elementos do shape estão OK

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
ctg_talh = clip_roi(ctg_orig, tal_shp)       # Usa shape como "tesoura"

# Para plotar a nuvem LiDAR de um dos talhões no catálogo ctg_tal,
# basta ler o respectivo arquivo salvo pela função clip_roi(), 
# como no exemplo abaixo:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# las <- readLAS(ctg_tal$filename[3])
# plot(las)

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

# Lê shape e atributos das parcelas p/ acessar parâmetros de interesse
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shpPar  <- str_c("C:/GitRepo/PRJ_FAZENDAMODELO/SHAPES/Modelo_parcentr.shp")    # shape de parcelas
par_shp <- st_read(shpPar)
st_is_valid(par_shp)        # confere se os elementos do shape estão OK

dirParc <- str_c(dirNrm, '/PARCELAS')      # Pasta p/ parcelas clipadas
if (!dir.exists(dirParc)) {
  dir.create(dirParc, showWarnings = F)
}

# Exibe localização dos tiles, talhoes e parcelas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot(ctg_tile, mapview = TRUE, map.type = "Esri.WorldImagery")
plot(ctg_tile)
plot(tal_shp)
plot(par_shp)

shp_talhoes_dir = str_c(shpDir, "/Grid_finalizado.shp")
shp_talhoes = st_read(shp_talhoes_dir)

# Clipa e salva as núvens das parcelas, e calcula
# métricas para cada "voxel" clipado com o shape de parcelas no tiles
# normalizados usando a função .stdmetrics_z de métricas genéricas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
opt_output_files(ctg_tile) <-     # Onde guardar as nuvens das parcelas
  str_c(dirParc, "/P_{NUMPARCELA}") # renomeadas pelo respectivo número
opt_select(ctg_tile) <- "xyz"   # Carrega na memória apenas coordenadas   
opt_filter(ctg_tile) <- "-drop_z_below 0"     # Ignora pontos com z < 0

# 4. Organização dos dados para cálculo das estimativas e inferências
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
# Seleciona apenas as parcelas em que a área é maior do que 1 m²
talhoes_final <- shp_talhoes %>%
  filter_at(vars(AREAPARCEL), all_vars(. >= 1))


metrics_list <- list() # Inicializa uma lista para armazenar as métricas de cada parcela

# Loop sobre cada parcela no shapefile
for (i in 1:nrow(talhoes_final)) { # Para cada número de linhas do data frame que contém as informações 
  shp_parcela <- talhoes_final[i, ]  # Pegue a i-ésima parcela
  las_crop <- clip_roi(ctg_tile, shp_parcela)  # Recorte os pontos LiDAR para essa parcela e converta para LAS
  if (class(las_crop) == "LAScatalog") { # Verifique se o retorno é um LAScatalog, e se sim, converta para LAS. 
    las_crop <- readLAS(las_crop) 
  }
  # Verifique se a parcela contém pontos LiDAR
  if (!is.null(las_crop)) {
    metrics <- cloud_metrics(las_crop, func = .stdmetrics_z)     # Calcule as métricas para essa parcela
    # Converta as métricas para um data frame e adicione um identificador da parcela
    metrics_df <- as.data.frame(t(metrics))  # Transforma as métricas em um data frame com uma linha
    metrics_df$id <- talhoes_final$id[i]  # Identficador da parcela
    metrics_list[[i]] <- metrics_df  # Armazene as métricas no data frame
  } else {
    print("Erro: parcela", parcela_id[i], "sem pontos") # Indica parcela sem ponto
  }
}

metrics_df_final <- do.call(rbind, metrics_list) # Combine todas as métricas em um único data frame

# Fazer o merge das métricas com os dados
final_df <- merge(talhoes_final, metrics_df_final, by = "id") %>%
  filter_at(vars(AREAPARCEL), all_vars(. >= 1)) # Seleciona apenas parcelas com pelo menos 1m²

# Cálculo do boundary weights. Representação percentual do tamanho da parcela (Ex. 1 = 400m²; 0.5 = 200m²)
var_boundaryweights <- (final_df$AREAPARCEL)/400
final_df$boundaryweights <- var_boundaryweights

# Escolhe um subgrupo de métricas e dados para estudo da correlação
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
X <- tibble(final_df) %>% select(Inventario, boundaryweights, MHDOM, IDINV, zmean, 
                          zq45, zq75, zq95, VTCC, VCCC)

X$Inventario <- as.numeric(X$Inventario) # Converter coluna Inventario para Integer (número) - Deve estar em algum formato numérico para ser reconecida pela função twophase()
X$zq95 <- as.numeric(X$zq95) # Conversões
X$zq45 <- as.numeric(X$zq45)
X$zq75 <- as.numeric(X$zq75)
X$zmean <- as.numeric(X$zmean)
X <- as.data.frame(X) # Tibble não é uma função nativa do R, é uma função chamada pelo tidyverse. Dessa forma, o formato da tabela que é gerada não é reconhecido pela função twophase() do package forestinventory. O forestinventory reconhece data frames, já que são nativos do R.

# Análise de regressão linear para verificar a correlação
# entre o p95 e a idade do inventário com o VTCC
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
m <- lm(VTCC ~ zq95 + IDINV, data = X)    # Análise de Regressão Linear
summary(m)                          # Mostra os resultados da regressão
VTCCparcelas <- X$VTCC[!is.na(X$VTCC)] # VTCCparcelas recebe os valores não nulos de VTCC contidos em X, ou seja, recebe os valores de VTCC das parcelas de campo
plot(VTCCparcelas, predict(m))              # Gráfico de observado vs predito
abline(0,1)

# 5. Retorna as estimativas e as inferências para cada uma das abordagens
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----

# Amostragem Casual Simples (ACS)
op_ACS <- onephase(formula = VTCC~1, data = X, # VTCC~1, pois não é feita regressão linear. X é a base de dados
               phase_id = list(phase.col = "Inventario",terrgrid.id = 2)) # phase.col solicita o nome da coluna que identifica a informação de campo
               # terrgrid solicita o identificador da informação de campo. Ex. todas as linhas do data.frame em que Inventario vale 2, há informação de VTCC coletada em campo

summary(op_ACS) # Printa a estimativa da variável de interesse, a variância e a quantidade de amostras
confint(op_ACS) # Printa a estimativa da variável de interesse e o int. de confiança
# Calculam o erro associado à estimativa (%)
aux_ACS = confint(op_ACS)
erroPercentualACS = ((aux_ACS$ci$estimate - aux_ACS$ci$ci_lower_op)*100)/aux_ACS$ci$estimate

# Amostragem Casual Estratificada (ACE)
op_ACE <- onephase(formula = VTCC~1, data = X,
                   phase_id =list(phase.col = "Inventario",terrgrid.id = 2),
                   area = list(sa.col = "IDINV", areas = c("3.7", "5.2"))) # sa.col indica a coluna a ser utilizada como estrato
                    # areas identifica os estratos pelos valores contidos na coluna indicada a sa.col
summary(op_ACE)
confint(op_ACE)

# Unifica as estimativas e as inferências obtidas por estrato. O resultado final diz respeito à área inteira
idadesEstratos = c(unique(talhoes$IDINV)) # Indica quais são os valores presentes na coluna, sem repetições
VTCCponderadaACE = c()
areaTotal = sum(talhoes$AREA) # Área total da fazenda
foreach(i = 0:length(unique(talhoes$IDINV)), .combine = 'c') %do% { 
  VTCCponderadaACE[i] = ((sum(talhoes$AREA[talhoes$IDINV == idadesEstratos[i]]))/areaTotal)*(op_ACE$estimation$estimate[op_ACE$estimation$area == idadesEstratos[i]])
}
VTCCponderadaACE_final = sum(VTCCponderadaACE) # Retorna uma única estimativa do VTCC para a fazenda inteira

VarponderadaACE = c()
foreach(i = 0:length(unique(talhoes$IDINV)), .combine = 'c') %do% {
  VarponderadaACE[i] = (((sum(talhoes$AREA[talhoes$IDINV == idadesEstratos[i]]))/areaTotal)^2)*(op_ACE$estimation$variance[op_ACE$estimation$area == idadesEstratos[i]])
}
VarponderadaACE_final = sum(VarponderadaACE) # Retorna uma única inferência sobre a variância associada ao VTCC da fazenda

calcCI = function(err, n, alpha=.05){ # err = desvio padrão, n = tamanho da amostra e alpha = nível de significância (95% de confiança)
  return(
    qt(1 - alpha/2, n-1) * err # Calcula o quantil da distribuição de t de Student para o nível de liberdade desejado. Multiplica pelo erro para obter o intervalo de confiança
  ) # retorno do ic
}
intervaloConfiancaACE = calcCI(erroPadrao_ponderadoACE, numeroAmostras) # Chama a função calcCI para calcular o Intervalo de Confiança

erroPadrao_ponderadoACE = sqrt(VarponderadaACE_final) # Cálculo do desvio padrão
numeroAmostras = sum(op_ACE$estimation$n2) # Retorna o número de amostras na fazenda
erroPercentualACE = (intervaloConfiancaACE*100)/VTCCponderadaACE_final # Cálculo do erro percentual associado à estimativa

# Resultados finais da ACE:
VTCCponderadaACE_final
VarponderadaACE_final
erroPadrao_ponderadoACE
intervaloConfiancaACE

# Dupla Amostragem
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reg2p_nex <- twophase(formula = VTCC ~ zq95 + IDINV, # formula relaciona os valores de VTCC com zq95 e IDINV (análise de regressão)
  data = X, #  Base de dados utilizada
  phase_id = list(phase.col = "Inventario", terrgrid.id = 2)) # phase_id recebe uma lista em que a coluna a ser analisada é a "Inventario" e o identificador da segunda fase é 2
summary(reg2p_nex) # Dá os resultados da Dupla Amostragem
confint(reg2p_nex) # Estatística de confiança

aux_reg2p_nex = confint(reg2p_nex) # Variável auxiliar
erroPercentualDA = ((aux_reg2p_nex$ci$estimate - aux_reg2p_nex$ci$ci_lower_ext)*100)/aux_reg2p_nex$ci$estimate
# Retorna o erro percentual 


# Dupla Amostragem Estratificada
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reg2p_nex_est = twophase(
  formula = VTCC ~ zq95 + IDINV,
  data = X,
  phase_id =list(phase.col = "Inventario", terrgrid.id = 2),
  small_area = list(sa.col = "IDINV", areas = c("3.7", "5.2"), unbiased = FALSE)) 
summary(reg2p_nex_est)

VTCCponderadaDAE = c()
foreach(i = 0:length(unique(talhoes$IDINV)), .combine = 'c') %do% {
  VTCCponderadaDAE[i] = ((sum(talhoes$AREA[talhoes$IDINV == idadesEstratos[i]]))/areaTotal)*(reg2p_nex_est$estimation$estimate[reg2p_nex_est$estimation$area == idadesEstratos[i]])
}
VTCCponderadaDAE_final = sum(VTCCponderadaDAE)

erroPadrao_ponderadoDAE = sqrt(VarponderadaDAE_final)
intervaloConfiancaDAE = calcCI(erroPadrao_ponderadoDAE, reg2p_nex_est$estimation$n2[1])

#Esquisito esse valor da variância - verificar cálculo. A variância em 5.2 ta mt mais alta, mas tem mais parcelas de campo
VarponderadaDAE = c()
foreach(i = 0:length(unique(talhoes$IDINV)), .combine = 'c') %do% {
  VarponderadaDAE[i] = (((sum(talhoes$AREA[talhoes$IDINV == idadesEstratos[i]]))/areaTotal)^2)*(reg2p_nex_est$estimation$g_variance[reg2p_nex_est$estimation$area == idadesEstratos[i]])
}
VarponderadaDAE_final = sum(VarponderadaDAE)

erroPercentualDAE = (intervaloConfiancaDAE*100)/VTCCponderadaDAE_final

# Resultados da Dupla Amostragem Estratificada
VTCCponderadaDAE_final # VTCC
VarponderadaDAE_final # Variância
erroPadrao_ponderadoDAE # Desvio Padrão
intervaloConfiancaDAE # Intervalo de Confiança
erroPercentualDAE # Erro percentual

# Cria tabela para facilitar a visualização dos resultados
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aux_ciop = confint(op_ACS)
ciop = aux_ciop$ci$estimate - aux_ciop$ci$ci_lower_op # Corresponde ao intervalo de confiança da ACS

aux_cir2p = confint(reg2p_nex)
cir2p = aux_cir2p$ci$estimate - aux_cir2p$ci$ci_lower_g # Corresponde ao intervalo de confiança da AD
# Intervalos de confiança das amostragens estratificadas já foram calculados previamente

# Cria um data frame que servirá como base de dados para a tabela
data <- data.frame(
  Resultados = c("Estimativa VTCC", "Variância", "Desvio Padrão", "Intervalo de confiança (95%)", "Erro (%)"),
  ACS = c(op_ACS$estimation$estimate, op_ACS$estimation$variance, sqrt(op_ACS$estimation$variance), ciop, erroPercentualACS),
  ACE = c(VTCCponderadaACE_final, VarponderadaACE_final, erroPadrao_ponderadoACE, intervaloConfiancaACE, erroPercentualACE),
  AD = c(reg2p_nex$estimation$estimate, reg2p_nex$estimation$g_variance, sqrt(reg2p_nex$estimation$g_variance), cir2p, erroPercentualDA),
  ADE = c(VTCCponderadaDAE_final, VarponderadaDAE_final, erroPadrao_ponderadoDAE, intervaloConfiancaDAE, erroPercentualDAE)
)

# Crie a tabela como um gráfico usando ggtexttable
tabela = ggtexttable(data, theme = ttheme("mBlue"))

tabela # Plot da tabela

# Extra:
#IA = str_c("ha/1 parc. para erro de ", as.character(erro), "%")
# data <- data.frame(
#   Resultados = c("Estimativa VTCC", "Variância", "Desvio Padrão", "Intervalo de confiança (95%)", "Erro (%)", IA),
#   ACS = c(op_ACS$estimation$estimate, op_ACS$estimation$variance, sqrt(op_ACS$estimation$variance), ciop, erroPercentualACS, tamanhoIdealACS),
#   ACE = c(VTCCponderadaACE_final, VarponderadaACE_final, erroPadrao_ponderadoACE, intervaloConfiancaACE, erroPercentualACE, tamanhoIdealACE),
#   AD = c(reg2p_nex$estimation$estimate, reg2p_nex$estimation$g_variance, sqrt(reg2p_nex$estimation$g_variance), cir2p, erroPercentualDA, 0),
#   ADE = c(VTCCponderadaDAE_final, VarponderadaDAE_final, erroPadrao_ponderadoDAE, intervaloConfiancaDAE, erroPercentualDAE, 0)
# )
# 
# # Crie a tabela como um gráfico usando ggtexttable
# tabela = ggtexttable(data, theme = ttheme("mBlue"))
# tabela




# 
# erro <- 10 # erro de 10%                                 # Aqui o alpha antes estava como alpha = erro/100, isso não funciona, pois, para o caso de um erro de 10%, o alpha valeria 0.1 e nós queremos um alpha de 0.05 (IC de 95%). Um alpha = 0.1 indica um IC de 90% (1 - 0.1/2 = 0.95, onde 0.5 é a cauda superior e 0.5 a cauda inferior)
# tamanhoIdealAC_fun = function(y, N, errDesired=erro/100, alpha=.05){ # Y vai receber o data.frame com a VTCC de cada parcela de campo e N vai receber o número máximo de parcelas de campo
#   B  =  errDesired * mean(y) # B representa o erro absoluto; é a média das VTCC * 0,1 -> é uma constante que representa o valor do erro de 10% em m³ por ha, se passar do valor de B, então o erro é maior do que 10%
#   qt = qt(1 - alpha/2, length(y)-1) # quantil 97,5% - 95% de confiança para o grau de liberdade delimitado (13 parcelas de campo - 1 = 12)
#   n  = N*var(y)*qt^2 / (N * B^2 + qt^2 * var(y)) # Calcula o tamanho ideal de amostra baseando-se na variância de y, no tamanho máx. da pop (N), no limite do erro B e na confiança desejada (95%)
#   return(n)
# }
# 
# 
# parcelaAreaMedia <- mean(final_df$AREAPARCEL) / 10000          # quantidade de parcelas de campo por ha com base na média de suas áreas
# N_max <- round(areaTotal / parcelaAreaMedia , 0)
# 
# N_max_est = c()
# foreach (i = 1:length(unique(X$IDINV))) %do% {
#   N_max_est[i] <- round((sum(talhoes$AREA[talhoes$IDINV == idadesEstratos[i]]) / parcelaAreaMedia), 2)
# }
# 
# 
# 
# 
# ### Intensidade Amostral: ACS - resultado em n° parc. por ha
# tamanhoIdealACS = round(areaTotal/(tamanhoIdealAC_fun(X$VTCC[X$Inventario == 2], N_max)), 2)
# 
# ### Intensidade Amostral: ACE -                !!!!!!!depois fazer na mão!!!!!!!
# tamanhoIdealACE_aux = c()
# foreach (i = 0:length(unique(X$IDINV))) %do% {
#   tamanhoIdealACE_aux[i] = c(round(areaTotal/(tamanhoIdealAC_fun(X$VTCC[X$Inventario == 2 & X$IDINV == idadesEstratos[i]], N_max_est)), 2))
#   tamanhoIdealACE_aux[i] = tamanhoIdealACE_aux[i]*(sum(talhoes$AREA[talhoes$IDINV == idadesEstratos[i]]))/(areaTotal)
# }
# tamanhoIdealACE = sum(tamanhoIdealACE_aux)
# 
# teste = (((1417.71*40.05)/sqrt(5))/(((1417*40.05)/sqrt(5))+((2027.85*41.7)/(sqrt(8)))))
# print(teste) # W para 3.7
# 
# teste1 = (((2027.85*41.7)/sqrt(8))/(((1417*40.05)/sqrt(5))+((2027.85*41.7)/(sqrt(8)))))
# print(teste1)
# 
# Sy_aux = c(X$VTCC[X$Inventario == 2 & X$IDINV == 3.7])
# Sy = sum(Sy_aux^2) - (sum(var(X$VTCC[X$Inventario == 2 & X$IDINV == 3.7]))^2)/5
# 
# S2h = Sy/4 # variancia
# 
# idade_menor = ((N_max_est[1]^2)*var(X$VTCC[X$Inventario == 2 & X$IDINV == 3.7]))/teste
# idade_maior = ((N_max_est[2]^2)*var(X$VTCC[X$Inventario == 2 & X$IDINV == 5.2]))/teste1
# fim = idade_menor + idade_maior
# 
# aux_B_N_t = (mean(X$VTCC[X$Inventario == 2])*0.1*N_max^2)/(qt(1 - 0.05/2, 12))^2
# 
# soma_final_id_menor = N_max_est[1]*var(X$VTCC[X$Inventario == 2 & X$IDINV == 3.7])
# soma_final_id_maior = N_max_est[2]*var(X$VTCC[X$Inventario == 2 & X$IDINV == 5.2])
# fim1 = soma_final_id_maior + soma_final_id_menor
# 
# print(fim/(aux_B_N_t + fim1))
### DEPOIS DE TUDO PRECISO VERIFICAR OS CÁLCULOS!!!!

###

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Este script foi atualizado em Dezembro/2023 depois de instalar,
# na seguinte sequência, as mais recentes versões do pacote lidR*:
#
#   devtools::install_github("Jean-Romain/rlas", dependencies=TRUE)
#   devtools::install_github("Jean-Romain/lidR")
#
#   * lidR latest development version
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  ----