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

if(!require(sf))            # Para manipulação de shapes e outros tipos
  install.packages("sf")                      # de dados espacializados 
library(sf)

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
# Este bloco cria funções que permitem produzir gráficos esteticamente
# caprichados para apresentar os índices de correlação de Pearson
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parâmetros dos gráficos  
txt.size = 6
thm.size = (14/5) * txt.size

Tema <- theme(
  axis.line    = element_line(size = .5, colour = "black"),
  axis.text.x  = element_text(angle = 45, size = thm.size, vjust = 1, hjust = 1),
  axis.title.y = element_blank(),
  axis.text.y  = element_text(angle = 45, size = thm.size),
  axis.ticks   = element_blank(),
  legend.justification = c(1, 0),
  legend.position  = c(1, 0.5),
  legend.text      = element_text(size = 12, colour="black"),
  legend.title = element_blank(),
  panel.border     = element_blank(),
  panel.background = element_blank(),
  panel.grid.major = element_line(colour = "grey"),
  title = element_text(size = thm.size, colour="black", face="bold"),
  plot.background = element_rect(colour = "black", fill=NA, size=1)
) 

# Função para reordenamento da cormat (correlation matrix)
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Função para definição do triângulo superior da cormat
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# Função para embelazamento da cormat
graphCormat <- function(cormat){
  # Resets font size for correlation matrix graph
  txt.size = 6
  thm.size = (14/5) * txt.size
  upper_tri <- get_upper_tri(cormat)
  # Melt the correlation matrix
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  p <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    coord_fixed() +
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 0.7*txt.size) +
    theme(
      axis.line    = element_line(size = .5, colour = "black"),
      axis.title.x = element_blank(),
      axis.text.x  = element_text(angle = 45, size = thm.size, vjust = 1, hjust = 1),
      axis.title.y = element_blank(),
      axis.text.y  = element_text(angle = 45, size = thm.size),
      axis.ticks   = element_blank(),
      legend.direction = "horizontal",
      legend.justification = c(1, 0),
      legend.position  = c(0.6, 0.7),
      legend.text      = element_text(size = 12, colour="black"),
      panel.border     = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_line(colour = "grey"),
      title = element_text(size = thm.size, colour="black", face="bold"),
      plot.background = element_rect(colour = "black", fill=NA, size=1)
    ) +
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
  return(p)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Estimativas. e estatísticas de qualidade da inferência, resultantes
# da amostragem dupla (double sampling)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dsStats = function(y, x, xLarge, alpha=.05){
  n = length(y)
  beta = ( sum(y*x, na.rm=T) - ( sum(x, na.rm=T)*sum(y, na.rm=T) / n ))  / ( sum(x^2, na.rm=T) - (sum(x, na.rm=T)^2 / n) )
  
  rho = cor(y,x)
  N = length(xLarge)
  
  ydsr = mean(y, na.rm=T) + beta * ( mean(xLarge, na.rm=T) - mean(x, na.rm=T) )
  vardsr = (var(y, na.rm=T)/n)*(1 - (rho^2)*(N-n)/N)
  stderr = sqrt(vardsr)
  ci     = calcCI(stderr, n, alpha)
  
  out = c(media = ydsr, var = vardsr, dp = stderr, ic = ci, erro = 100*ci/ydsr, n=n, rho=rho)
  
  return(out)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cálculo do número ideal de unidades amostrais em double sampling, ou
# seja, da intensidade amostral necessária para gerar o erro admissível
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dsNumberOfPlots = function(y, x, xLarge, Cpg = 300, 
                           errDesired = .05, alpha = .05){
  
  rho = cor(x,y)
  a = var(y) * (1 - rho^2)
  b = var(y) * rho^2
  
  B = errDesired * mean(y)
  qt = qt(1 - alpha/2, length(y)-1)
  
  nG = (sqrt( a*b*Cpg ) + b) / (B^2 / qt^2)
  nP = (sqrt( a*b/Cpg ) + a) / (B^2 / qt^2)
  
  return(nP)
}

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

# regiao bugada id = 5603

plot(shp_talhoes$id == 5603)



# Clipa e salva as núvens das parcelas, e calcula
# métricas para cada "voxel" clipado com o shape de parcelas no tiles
# normalizados usando a função .stdmetrics_z de métricas genéricas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
opt_output_files(ctg_tile) <-     # Onde guardar as nuvens das parcelas
  str_c(dirParc, "/P_{NUMPARCELA}") # renomeadas pelo respectivo número
opt_select(ctg_tile) <- "xyz"   # Carrega na memória apenas coordenadas   
opt_filter(ctg_tile) <- "-drop_z_below 0"     # Ignora pontos com z < 0

D <- plot_metrics(ctg_tile, .stdmetrics_z, par_shp, radius = 11.28)

df <- df[-3, ]

df_shp_talhoes <- shp_talhoes[-1593, ]
D1 <- plot_metrics(ctg_tile, .stdmetrics_z, df_shp_talhoes)

var_boundaryweights <- (D1$AREAPARCEL)/400
D1$boundaryweights <- var_boundaryweights

Xteste <- tibble(D1) %>% select(Inventario, boundaryweights, MHDOM, IDINV, zmean, 
                          zq45, zq75, zq95, VTCC, VCCC)

##plot(D)
# Escolhe um subgrupo de métricas e dados para estudo da correlação
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
X <- tibble(D) %>% select(VTCC, MHDOM, IDINV, zmean, 
                          zq45, zq75, zq95, zpcum2, zpcum4, zpcum6,
                          pzabovezmean, pzabove2)

Xteste <- tibble(D) %>% select(VTCC, MHDOM, IDINV, zmean, 
                          zq45, zq75, zq95, zpcum2, zpcum4, zpcum6,
                          pzabovezmean, pzabove2)

Xteste$Inventario <- as.integer(Xteste$Inventario)
Xteste$IDINV <- as.numeric(Xteste$IDINV)

# EU POR ALGUM MOTIVO NAO CONSIDEREI AS PARCELAS DE CAMPO COM 3.7 ANOS. O CAMINHO ESTÁ CERTO, MAS HOUVE ESSE ERRO

teste_reg2p_nex <- twophase(formula = VTCC ~ zq75 + zmean, 
                            data = Xteste, phase_id = list (phase.col = "Inventario",
                            terrgrid.id = 2), boundary_weights = "boundaryweights")
summary(teste_reg2p_nex)

class(Xteste$Inventario)

m <- lm(VTCC ~ zq95 + IDINV, data = Xteste)    # Análise de Regressão Linear
summary(m)                          # Mostra os resultados da regressão
plot(Xteste$VTCC, predict(m))              # Gráfico de observado vs predito
abline(0,1)

head(Xteste)


 
mediazq95 = sum(Xteste$zq95)/13
mediaidinv = sum(Xteste$IDINV)/13

teste.true.means.Z <- c(1, 24.29965, 4.623077)
teste_reg2p_ex <- twophase(formula = VTCC ~ zq95 + IDINV,
                     data = Xteste, phase_id = list (phase.col = "id",
                      terrgrid.id = 2), exhaustive = teste.true.means.Z)
summary(teste_reg2p_ex)

# Prepara o gráfico para análise das correlações
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cormat <- X %>% cor()
cormat <- round(cor(cormat), 2) %>% reorder_cormat()
p      <- graphCormat(cormat)

grfDir <- str_c(dirProjet, '/RESULTADOS/') # Define pasta p/ resultados
if (!dir.exists(grfDir)) {                # Cria pasta, caso não exista
  dir.create(grfDir, showWarnings = F)
}

fileGrf <- str_c(grfDir, "MatrizDeCorrelacoes.jpg")   # Arq para matriz
tituGrf <- "Matriz de Correlacoes de Pearson"        # Define um título

# Abre "impressora" para gerar e salvar o gráfico no formato JPEG
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
jpeg(fileGrf, width = 1000, height = 1000, units = "px", quality = 200)
plot(p + labs(title=tituGrf))
dev.off()                                        # Fecha a "impressora"

# Análise de regressão por quadrados mínimos ordinários (OLS) para
# determinação do modelo de predição.  Testando: VTCC ~f(zq90,IDINV)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
m <- lm(VTCC ~ zq95 + IDINV, data = X)    # Análise de Regressão Linear
summary(m)                          # Mostra os resultados da regressão
plot(X$VTCC, predict(m))              # Gráfico de observado vs predito
abline(0,1)

# ----


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Criação de mapas raster com as métricas mais promissoras
#    É possível processar a partir deste bloco, desde que já tenham
#    sido processadas as linhas 1 a 229 (Bloco 1.)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
prjNam    <- "PRJ_FAZENDAMODELO"                             # Nome do projeto
dirRaizNuvens <- str_c('C:/LiDAR/',prjNam, '/NUVENS') # Raiz das nuvens
dirDadosLiDAR <- str_c(dirRaizNuvens, '/A13/TALHOES/SiNORM')

# Lê catálogo de com as núvens por talhão normalizadas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ctg_taln <- readLAScatalog(dirDadosLiDAR)
nNuvens  <- length(ctg_taln$filename)    # Número de núvens no catálogo

# Criação de mapas raster formato tif para as métricas de interesse
#   Atribua à variável "interesse" uma das métricas calculadas pela
#   função padrão ".stdmetrics_z", dentre as várias: zmax, zmean, 
#   zsd, zskew, zkurt, zentropy, pzabovezmean, pzabove2, zq5, zq10,
#   zq15, zq20, zq25, zq30, zq35, zq40, zq45, zq50, zq55, zq60, zq65,
#   zq70, zq75, zq80, zq85, zq90, zq95, zpcum1, zpcum2, zpcum3, zpcum4,
#   zpcum5, zpcum6, zpcum7, zpcum8, zpcum9
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
interesse <- "zq95"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Criação de mapas raster para a métrica de interesse  (PIXEL QUADRADO)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dirDadosRaster <- str_c(dirDadosLiDAR, '/RSTR_qua/')        # Quadrados
if (!dir.exists(dirDadosRaster)) {
  dir.create(dirDadosRaster, showWarnings = F)
}
#pixelmetrics para um lasCatalog
pb <- progress_bar$new(total = 2*nNuvens) # Reset da barra de progresso
for (i in 1:nNuvens) {
  nuvem   <- ctg_taln$filename[i]
  talhao  <- ctg_taln$filename[i] %>% basename() %>% file_path_sans_ext()
  
  las <- readLAS(nuvem, select = "xyz", 
                 filter = "-drop_z_below 2")    # lê apenas pontos > 2m
  
  pb$tick()                                 # Avança barra de progresso
  inteRaster <- (las %>%   # Cria objeto raster da métrica de interesse
                   pixel_metrics(.stdmetrics_z, res = 20))[[interesse]]
  
  rstNome <- str_c(dirDadosRaster, talhao,'_sqr_', interesse, '.tif')
  writeRaster(inteRaster, rstNome, overwrite=TRUE)  # Salva arquivo tif
  
  pb$tick()                                 # Avança barra de progresso
  rstNome <- str_c(dirDadosRaster, talhao,'_sqr_', interesse, '.png')
  # Cria parâmetros para melhorar a legenda do gráfico raster
  minL = round(minmax(inteRaster)[1])-1         # Menor valor no raster
  maxL = round(minmax(inteRaster)[2])+1         # Maior valor no raster
  brkL = round((maxL - minL) / 5)            # Legenda com cinco breaks
  intL = c(minL + brkL, minL + 2*brkL, minL + 3*brkL, minL + 4*brkL)
  limL = c(minL, maxL)
  titulo  <- str_c('Talhao: ', talhao, ' | Metrica: ', interesse, 
                   ' | Min: ', minL, ' | Max: ', maxL)
  p <- ggplot() + geom_spatraster(data = inteRaster, aes()) +
    guides(fill = guide_legend(reverse=F)) +  
    coord_sf(datum=st_crs(31983)) +       # Gráfico numa certa projeção
    scale_fill_whitebox_c(palette = "atlas", direction = -1,
                          breaks = intL, limits = limL)+ggtitle(titulo)
  png(rstNome, 30, 20, 'cm', res = 200)              # Abre "impressão"
  print(p)
  dev.off()                               # Fecha "impressão" do aquivo
}
teste = 0
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# !!! Método alternativo:
# Criação de mapas raster para a métrica de interesse (PIXEL HEXAGONAL)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# dirDadosRaster <- str_c(dirDadosLiDAR, '/RSTR_hex/')       # Hexagonais
# if (!dir.exists(dirDadosRaster)) {
#   dir.create(dirDadosRaster, showWarnings = F)
# }
# 
# pb <- progress_bar$new(total = 2*nNuvens) # Reset da barra de progresso
# for (i in 1:nNuvens) {
#   nuvem   <- ctg_taln$filename[i]
#   talhao  <- ctg_taln$filename[i] %>% basename() %>% file_path_sans_ext()
# 
#   las <- readLAS(nuvem, select = "xyz",
#                  filter = "-drop_z_below 2")    # lê apenas pontos > 2m
# 
#   pb$tick()                                 # Avança barra de progresso
#   inteRaster <- las %>%
#     hexagon_metrics(.stdmetrics_z, area = 400) %>%
#     select(contains(c(interesse, "geometry"))) %>%
#     st_rasterize()
#   
#   rstNome <- str_c(dirDadosRaster, talhao,'_hex_', interesse, '.tif')
#   write_stars(inteRaster, rstNome)
#   
#   pb$tick()                                 # Avança barra de progresso
#   rstNome <- str_c(dirDadosRaster, talhao,'_hex_', interesse, '.png')
#   titulo  <- str_c('Talhão: ', talhao, '  /  Métrica: ', interesse)
#   inteRaster <- as(inteRaster, "SpatRaster")
#   # Cria parâmetros para melhorar a legenda do gráfico raster
#   minL = round(minmax(inteRaster)[1])-1         # Menor valor no raster
#   maxL = round(minmax(inteRaster)[2])+1         # Maior valor no raster
#   brkL = round((maxL - minL) / 5)            # Legenda com cinco breaks
#   intL = c(minL + brkL, minL + 2*brkL, minL + 3*brkL, minL + 4*brkL)
#   limL = c(minL, maxL)
#   titulo  <- str_c('Talhão: ', talhao, ' | Métrica: ', interesse, 
#                    ' | Mín: ', minL, ' | Máx: ', maxL)
#   p <- ggplot() + geom_spatraster(data = inteRaster, aes()) +
#     guides(fill = guide_legend(reverse=F)) +  
#     coord_sf(datum=st_crs(31983)) +       # Gráfico numa certa projeção
#     scale_fill_whitebox_c(palette = "atlas", direction = -1,
#                           breaks = intL, limits = limL)+ggtitle(titulo) 
#   png(rstNome, 30, 20, 'cm', res = 200)              # Abre "impressão"
#   print(p)
#   dev.off()                               # Fecha "impressão" do aquivo
# }

# Cálculo de mapas raster como função dos mapas rasters de métricas
# criados nos passos anteriores.  Por exemplo, MAPA DE VOLUME
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ctg_taln <- readLAScatalog(dirDadosLiDAR)
nNuvens  <- length(ctg_taln$filename)    # Número de núvens no catálogo

# Criação de mapas raster formato tif para as métricas de interesse
#   Atribua à variável "interesse" uma das métricas calculadas pela
#   função .stdmetrics_z: zmax, zmean, zsd, zskew, zkurt, zentropy,
#   pzabovezmean, pzabove2, zq5, zq10, zq15, zq20, zq25, zq30, zq35, 
#   zq40, zq45, zq50, zq55, zq60, zq65, zq70, zq75, zq80, zq85, zq90,
#   zq95, zpcum1, zpcum2, zpcum3, zpcum4, zpcum5, zpcum6, zpcum7,
#   zpcum8, zpcum9
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
interesse <- "zq95"

# Criação de lista de mapas raster criados nos passos anteriores
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
listaRasters <- list()
for (i in 1:nNuvens) {
  nuvem   <- ctg_taln$filename[i]
  talhao  <- ctg_taln$filename[i] %>% basename() %>% file_path_sans_ext()
  rstNome <- str_c(dirDadosRaster, talhao,'_sqr_', interesse, '.tif')
  
  listaRasters[[i]] <- rast(rstNome)
}

# Junta os rasters para gerar um mapa único com a métrica de interesse
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rastersJuntos <- do.call(merge, listaRasters)

# Cria gráfico caprichado da métrica de interesse com todos os rasters  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
minL = round(minmax(rastersJuntos)[1])-1        # Menor valor no raster
maxL = round(minmax(rastersJuntos)[2])+1        # Maior valor no raster
brkL = round((maxL - minL) / 5)              # Legenda com cinco breaks
intL = c(minL + brkL, minL + 2*brkL, minL + 3*brkL, minL + 4*brkL)
limL = c(minL, maxL)
titulo  <- str_c('Métrica: ', names(rastersJuntos),
                 ' | Mín: ', minL, ' | Máx: ', maxL)
p <- ggplot() + geom_spatraster(data = rastersJuntos, aes()) +
  guides(fill = guide_legend(reverse=F)) +  
  coord_sf(datum=st_crs(31983)) +       # Gráfico numa certa projeção
  scale_fill_whitebox_c(palette = "atlas", direction = -1,
                        breaks = intL, limits = limL)+ggtitle(titulo)
rstNome <- str_c(dirDadosRaster, 'talhoes_', names(rastersJuntos), '.png')
png(rstNome, 30, 20, 'cm', res = 200)              # Abre "impressão"
print(p)
dev.off()                               # Fecha "impressão" do aquivo

# Lê o arquivo shape com os limites e atributos dos talhoes
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dirShape <- str_c('C:/GitRepo/',prjNam, '/SHAPES')   # Pasta com shapes
arqShape <- str_c(dirShape, '/Modelo_talhoes.shp')      # Nome do shape

# Cria estrutura de dados (data frame) com os atributos dos talhoes
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
poligonos <- vect(arqShape)

# Plota mapa de rasters juntados sobreposto com os limites dos talhões
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot(rastersJuntos)
plot(poligonos, add = TRUE, col = "white")
# ----


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5 . Cria um novo raster com as estimativas de VOLUME para produção
#     do mapa de VTCC. A esrimativa será obtida a partir do modelo
#     ajustado na fase anterior:
#                           VTCC = -444.142 + 24.63 zq95 + 20.133 IDINV
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----

# A lista poligonos$SUBTALHAO contém os nomes dos talhões da Fazenda
# Modelo:  "302c" "301a" "301d" "302a". Criação de um vetor com as
# correspondentes idades na mesma ordem, para inclusão no objeto 
# com informações sobres os poligonos
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Idades <- as.numeric(c(5.2, 3.7, 3.7, 5.2))
poligonos$IDINV <- Idades

valoresAtrib <- poligonos$IDINV

# Cria um novo raster com a idade do respectivo polígono atribuída ao
# respectivo pixel sobreposto
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rastersIdade <- rasterize(poligonos, rastersJuntos,
                          field=valoresAtrib, ext=ext(rastersJuntos), 
                          res=res(rastersJuntos)) %>% rename(IDINV = layer)

# Junta rasters, mantendo os respecivos valores em layers separados,
# renomeia as colunas e assim, agora rastersJuntos tem as varíaveis
# necessárias para estimar o parâmetro de interesse, uma em cada layer.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rastersFinal <- c(rastersJuntos, rastersIdade)

# Usa a equação ajustada (final do bloco 3 deste script) para estimar
# o parâmetro de interesse
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rastersVolume <- -444.142 + 24.630 * rastersFinal["zq95"] +
  20.133 * rastersFinal["IDINV"]
rastersVolume <- rastersVolume %>% rename(VTCC = zq95)

# Junta layer VTCC como novo layer no raster final
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rastersFinal = c(rastersFinal, rastersVolume) 

# Cria novo raster de volumes mudando para zero valores negativos
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vtcc <- ifel(rastersFinal[["VTCC"]] < 0, 0, rastersFinal)[["VTCC"]]
minmax(vtcc)[1]
minmax(vtcc)[2]

# Apaga rasters intermediários
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(inteRaster, rastersIdade, rastersJuntos, rastersVolume)

# /////////////////////////////////////////////////////////////////////
#       MAPA DE ESTIMATIVAS DE VOLUME TOTAL COM CASCA (VTCC)
# /////////////////////////////////////////////////////////////////////
minL = round(minmax(vtcc)[1])                   # Menor valor no raster
maxL = round(minmax(vtcc)[2])                   # Maior valor no raster
brkL = round((maxL - minL) / 6)              # Legenda com cinco breaks
intL = c(minL+brkL, minL+2*brkL, minL+3*brkL, minL+4*brkL, minL+5*brkL)
limL = c(minL, maxL)
titulo  <- str_c('Mapa de estimativas de VTCC | Mín: ', minL, 
                 ' | Máx: ', maxL)
p <- ggplot() + geom_spatraster(data = vtcc, aes()) +
  guides(fill = guide_legend(reverse=F)) +  
  coord_sf(datum=st_crs(31983)) +       # Gráfico numa certa projeção
  scale_fill_whitebox_c(palette = "atlas", direction = -1,
                        breaks = intL, limits = limL)+ggtitle(titulo)
rstNome <- str_c(dirDadosRaster, 'talhoes_VTCC.png')
png(rstNome, 30, 20, 'cm', res = 200)              # Abre "impressão"
print(p)
dev.off()                               # Fecha "impressão" do aquivo


# ----

# /////////////////////////////////////////////////////////////////////
#    TENTE AGORA CRIAR A TABELA COM A ESTIMATIVA DE VOLUME CALCULADA
#    PELA AMOSTRAGEM DUPLA (SEMELHANTE À QUE FIZEMOS PARA ACS E ACE)
# /////////////////////////////////////////////////////////////////////


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