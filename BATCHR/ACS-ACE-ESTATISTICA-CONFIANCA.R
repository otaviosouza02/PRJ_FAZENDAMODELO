# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Inventário Convencional na Fazenda Modelo ~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Autor: Luiz Carlos Estraviz Rodriguez
#        Departamento de Ciências Florestais
#        ESALQ/USP - 16/Jan/2024
#
# Inventário florestal da Fazenda Modelo
#   - download dos dados mantidos em um repositório github público
#      - shape files dos talhões florestais
#      - LiDAR multitemporal (2013 e 2014)
#   - sugestão de pasta para armazenamento local:
#        C:/LiDAR/PRJ_Modelo/
#
# Linguagem de programação:
#       R (v 4.3)
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls(all=TRUE))                                   # Limpa memória
gc()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Leitura e organização de dados
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
if(!require(tidyverse))  # Para melhor manipulação de dados e funções
  install.packages("tidyverse")
library(tidyverse)
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
unlink(zipf)                                  # deleta o arquivo zipado


if(!require(sf))                           # Para manipulação de shapes
  install.packages("sf")
library(sf)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lê atributos dos talhoes
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shpArq <- paste0(tmpd, "/Modelo_talhoes.shp")       # shape com talhões
talhoesComGeo <- read_sf(shpArq)                    # completo com geom
talhoesSemGeo <- tibble(sf::st_drop_geometry(talhoesComGeo))  # s/ geom

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lê atributos das parcelas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shpArq <- paste0(tmpd, "/Modelo_parcelas.shp")     # shape com parcelas
parcelasComGeo <- read_sf(shpArq)               # completo com geom
parcelasSemGeo <- tibble(sf::st_drop_geometry(parcelasComGeo)) # s/ geom

plot(c(parcelasComGeo$geometry, talhoesComGeo$geometry))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cria coluna IDINV na tabela "talhões", extraída da tabela "parcelas"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
talhoes <-
  parcelasSemGeo %>% # Seleciona o df parcelasSemGeo
  group_by(SUBTALHAO) %>% # Agrupa por subtalhao
  summarise(IDINV = unique(IDINV)) %>% # Retorna a IDINV 
  left_join(talhoesSemGeo) %>% # Funde com a tabela talhoesSemGeo
  select("SUBTALHAO", "IDINV", "AREA") %>% # Seleciona apenas esses atributos de interesse
  arrange(SUBTALHAO) %>% as.data.frame # organiza de forma crescente e transforma em data frame

talhoesComGeo <- inner_join(talhoesComGeo, talhoes, by="SUBTALHAO") %>% # funde com talhoes a fim de 
  select("OBJECTID", "SUBTALHAO", "IDINV") # selecionar atributos de interesse classificados por subtalhao

talhoesComGeo <- # Adiciona área de cada subtalhao
  talhoesComGeo %>%
  group_by(SUBTALHAO) %>%
  left_join(talhoes) %>%
  select("OBJECTID","SUBTALHAO", "IDINV", "AREA") %>%
  arrange(SUBTALHAO) %>% as.data.frame


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reorganiza colunas da tabela "parcelas"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parcelas <-
  parcelasSemGeo %>%
  select(SUBTALHAO, CHAVE2, DATAREALIZ, IDINV, AREAPARCEL, MHDOM, VTCC) %>% 
  arrange(SUBTALHAO) %>% as.data.frame

# Cria diretórios e pastas para onde alguns dados serão copiados
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
prjNome <- 'PRJ_FAZENDAMODELO'
dirNome <- paste0('C:/LiDAR/', prjNome)
dir.create(dirNome, showWarnings = F)

# Salva e imprime dados reorganizados
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dirNome <- paste0(dirNome, '/DADOS/')
dir.create(dirNome, showWarnings = F)

# Leitura da mais atual versão rio para facilmente ler e salvar Excel
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!require("remotes")){install.packages("remotes")}
if (!require(rio))      {remotes::install_github("gesistsa/rio")}
library(rio)
# Salva dados em planilha Excel
arqNome <- paste0(dirNome, prjNome, '.xlsx')
export(list(talhoes = talhoes,
            parcelas  = parcelas),
       file = arqNome)

# Mostra tabelas no Viewer
# Use o "Export" do Viewer para salvar em diferentes formatos
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(!require(kableExtra))
  install.packages("kableExtra")
library(kableExtra)
# Mostra talhões
AreaTotal <- talhoes$AREA %>% sum
NotaDeRodape <- paste0(": ", AreaTotal)
talhoes %>%
  kbl(caption = "Talhões da Fazenda Modelo", align = "r") %>%
  kable_classic(full_width = F) %>%
  footnote(general = NotaDeRodape, 
           general_title = "Área total",
           footnote_as_chunk = T)
# Mostra parcelas
parcelas %>%
  kbl(caption = "Parcelas da Fazenda Modelo", align = "r") %>%
  kable_classic(full_width = F)

# Cria diretório para salvar mapas, figuras e gráficos
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dirNome <- paste0('C:/LiDAR/', prjNome)
dir.create(dirNome, showWarnings = F)
dirNome <- paste0(dirNome, '/GRAPH/')
dir.create(dirNome, showWarnings = F)

# Cria mapa dos talhões com localização das parcelas (EPSG: 31983) - consulta no diretório
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(sf)
library(ggplot2)

arqNome <- paste0(dirNome, prjNome, '.png')
png(arqNome, 30, 20, 'cm', res = 200)                # abre "impressão"

ggplot() +        #         plot dos talhões e parcelas (cor por idade)
  geom_sf(data = st_as_sf(talhoesComGeo), colour = "black", fill="white") + # usei st_as_sf para converter o df para sf
  geom_sf(data = st_as_sf(parcelasComGeo), aes(fill = factor(IDINV))) +
  scale_fill_discrete(name = "Idade") +
  guides(fill = guide_legend(reverse=F)) +
  coord_sf(datum=st_crs(31983)) +        # Especifica sistema de coord.
  scale_y_continuous(breaks = seq(from=7356500,to=7359000, by=200)) +
  scale_x_continuous(breaks = seq(from=206200, to=207600,  by=200))

dev.off()                                # fecha "impressão" do aquivo

# ----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Amostragem Casual Simples (ACS)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----

# Função para cálculo do Intervalo de Confiança
calcCI = function(err, n, alpha=.05){ # err = desvio padrão, n = tamanho da amostra e alpha = nível de significância (95% de confiança)
  return(
    qt(1 - alpha/2, n-1) * err # Calcula o quantil da distribuição de t de Student para o nível de liberdade desejado. Multiplica pelo erro para obter o intervalo de confiança
  ) # retorno do ic
}

  
# Função para cálculo dos parâmetros de estimação e inferência
calcPars = function(df, N, alpha=.05){ 
  means = apply(df, 2, mean) # Calcula a média de cada variável do data.frame df. 2 indica que as médias serão calculadas a partir das colunas do data.frame (se fosse 1, calcularia a partir das linhas)
  vars   = apply(df, 2, function(y){ # Aplica uma função em cada coluna para calcular a variância ajustadas
    (var(y) / length(y)) * ((N - length(y)) / N)
  })
  err = sqrt(vars) # calcula o erro padrão -> é a raiz quadrada das variâncias ajustadas
  cis = calcCI(err, nrow(df), alpha) # Calcula os IC baseado no erro padrão e no tamanho das amostras
  err_pc = 100*cis/means # calcula o erro percentual
  out = rbind(means, vars, err, cis, err_pc, nrow(df)) %>% as.data.frame # Organiza em df
  row.names(out) = c('media', 'var', 'dp', 'ic', 'erro', 'n')
  names(out) = names(df)
  return(out)
}


# Cria lista com variáveis de interesse
variaveisDeInteresse <- c("MHDOM", "VTCC")

# Cria dataframe com valores observados das variáveis de interesse
df <- parcelas %>% select(all_of(variaveisDeInteresse))

# Calcula o número máximo de unidades amostrais possíveis (N)
parcelaAreaMedia <- mean(parcelas$AREAPARCEL) / 10000          # quantidade de parcelas de campo por ha com base na média de suas áreas
N_ACS <- round(AreaTotal / parcelaAreaMedia , 0)    # N = número máximo de parcelas possível

# Resultados do inventário (estimação + inferência)
#             pelo método Amostragem Casual Simples (ACS)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

paramEstatisticosACS <- calcPars(df, N_ACS) # Dá parâmetros para função calcPars e retorna data.frame com as estimativas

# Função para cálculo da intensidade amostral recomendada
# (ha por parcela) da ACS que garante ~10% de erro (altere se desejar)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
erro <- 10 # erro de 10%                                 # Aqui o alpha antes estava como alpha = erro/100, isso não funciona, pois, para o caso de um erro de 10%, o alpha valeria 0.1 e nós queremos um alpha de 0.05 (IC de 95%). Um alpha = 0.1 indica um IC de 90% (1 - 0.1/2 = 0.95, onde 0.5 é a cauda superior e 0.5 a cauda inferior)
tamanhoIdealACS = function(y, N, errDesired=erro/100, alpha=.05){ # Y vai receber o data.frame com a VTCC de cada parcela de campo e N vai receber o número máximo de parcelas de campo
  B  =  errDesired * mean(y) # B representa o erro absoluto; é a média das VTCC * 0,1 -> é uma constante que representa o valor do erro de 10% em m³ por ha, se passar do valor de B, então o erro é maior do que 10%
  qt = qt(1 - alpha/2, length(y)-1) # quantil 97,5% - 95% de confiança para o grau de liberdade delimitado (13 parcelas de campo - 1 = 12)
  n  = N*var(y)*qt^2 / (N * B^2 + qt^2 * var(y)) # Calcula o tamanho ideal de amostra baseando-se na variância de y, no tamanho máx. da pop (N), no limite do erro B e na confiança desejada (95%)
  return(n)
}
# Intensidade amostral usadaalpha
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IAS <- paste0(" usada: 1 parc/",
              round(AreaTotal/paramEstatisticosACS$VTCC[6],0), " ha.") # Área total/número de parcelas existentes no campo

# Intensidade amostral que seria necessária para ACS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IA <- round(AreaTotal/tamanhoIdealACS(df$VTCC,N_ACS), 0)
erroTexto <- as.character(erro)
IAI <- paste0("\n Necessarias p/ erro de ", erroTexto, 
              "%: 1 parc/", IA, " ha.")

# Mostra resultados da ACS no Viewer
# Use o "Export" do Viewer para salvar em diferentes formatos
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NotaDeRodape <- paste0(IAS, IAI)
paramEstatisticosACS %>%
  kbl(caption = "Amostragem Casual Simples (ACS)", align = "r") %>%
  kable_classic(full_width = F) %>%
  footnote(general = NotaDeRodape,
           general_title = "Intensidade amostral",
           footnote_as_chunk = T)
# ----


# *********************************************************************
# 3. Amostragem Casual Estratificada (ACE)
# **************************************************************** ----
# Escolhe variável de estratificação
grupos = c("IDINV")

# Box-plot da variável de interesse por nível de estratificação
par(mfrow=c( length(grupos), length(variaveisDeInteresse) )) # 
attach(parcelas) # faz com que os colunas de parcelas sejam acessadas sem uso de parcelas$coluna, mas tem que usar deattach depois
for(i in grupos){ ## Bubble sort
  for(j in variaveisDeInteresse){
    plot( get(i) %>% factor , get(j), main = j, sub=i)  # Para cada item da idade do inventário (dados equivalem à parcelas$IDINV),
  }                                                     # transforme-os em fator (é assim pra plotar) e faça o plot
                                                        # de MHDOM e VTCC. O título é j (MHDOM ou VTCC) e o subtítulo é a IDINV
}
detach(parcelas)

# Estimativa da população para amostragem estratificada
popFromStrata = function(factorStrataList){
  popEstimates = foreach(i = factorStrataList, .combine = 'c') %do% { # Combina os dados em um vetor para cada um dos fatores de estratificação
    gpMeans = lapply(i, function(x) x[1,,drop=F]) %>% do.call(what = rbind) # Aplica a função X a cada elemento da lista de fatores de estratificação
    gpVars  = lapply(i, function(x) x[2,,drop=F]) %>% do.call(what = rbind) # rbind os empilha
    cols = 1:(ncol(gpMeans)-4)
    popMean   = apply(gpMeans[,cols], 2, 
                      function(x) sum(x*gpMeans$N) ) /N_ACS
    popVar    = apply(gpVars[,cols], 2, 
                      function(x) sum( x * (gpVars$N/N_ACS)^2 ) )
    popStdErr = sqrt(popVar)
    popCI     = calcCI(popStdErr, sum(gpMeans$n))
    popPars = data.frame(
      media = popMean,
      var   = popVar,
      dp    = popStdErr,
      ic    = popCI,
      erro  = 100 * popCI / popMean,
      n     = sum( sapply(i, function(x) mean(x$n)) )
    )
    return(list(popPars))
  }
  names(popEstimates) = names(factorStrataList)
  return(popEstimates)
}


if(!require(foreach))                         # Para loops inteligentes
  install.packages("foreach")
library(foreach)

if(!require(magrittr))
  install.packages("magrittr")
library(magrittr)

# Cria vetor de (True ou False) para marcar dados que estejam dentro
#      do intervalo de idades de interesse (entre 2 e 6)
rightAges = talhoes$IDINV > 2 & talhoes$IDINV < 6

# Resultados do inventário (estimação + inferência)
#             pelo método Amostragem Casual Estratificada (ACE)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
paramEstatisticosACE = foreach( # Executar linha por linha pode ser didático, mas também pode confundir. O código só realmente funciona com o foreach
  grp = grupos, .combine = 'rbind') %:%  # Para cada uma das idades (definidas por grupos) os dados serão empilhados em um data.frame (rbind). 
  foreach(
    niv = parcelas[,grp] %>% unique %>% # essa , antes do grp é para que a informação fique contida em um vetor numérico. Sem a vírgula, a informação é armazenada em um data.frame. Unique reduz as redundânicas.
      as.character %>% sort, .combine = 'rbind' # converter para string e organiza em ordem crescente. rbind empilha os resultados
  ) %do% {
    inGroup = talhoes[,grp] == niv # inGroup recebe TRUE e FALSE. Na primeira volta do foreach o TRUE será 3.7, a segunda volta será o 5.2
    popSize = round( # Calcula o tamanho da pop. 
      sum(talhoes[rightAges & !is.na(inGroup) & inGroup,]$AREA) / # Soma as áreas dos talhões onde as idades estão entre 2 e 6 (rightages), além de que a comparação das idades dos talhões (talhoes[,grp]) com o nível (niv) não tenha resultado em NA e onde inGroup esteja indicado como TRUE (talhoes[inGroup,]$AREA retorna as áreas onde a comparação de talhoes com niv deu como verdadeira)
        parcelaAreaMedia)
    inGroup = parcelas[,grp] == niv # inGroup classifica como verdadeira ou falsa a comparação da idade da parcela com a idade que niv está recebendo no momento (3.7 ou 5.2)
    tempPars = parcelas[inGroup, variaveisDeInteresse, drop=F] # Descarta as informações de interesse onde a compração da idade da parcela com a idade de niv são falsas. 
    inventory = calcPars(tempPars, popSize) # A função calcPars (mesma usada na ACS) recebe MHDOM e VTCC da idade indicada em niv como o df e N é a popSize, calculada acima
    inventory$grupo = grp # Indica o grupo utilizado na estratificação, no caso IDINV
    inventory$nivel = niv # indica qual o valor dos estratos (3.7 e 5.2)
    inventory$n     = nrow(tempPars) # Número de parcelas do estrato
    inventory$N     = popSize # Número máximo de amostra para o estrato
    return(inventory)
  }
paramEstatisticosACE %<>% base::split(f=paramEstatisticosACE$grupo) %>% # %<>% combina o pipe com a atribuição (->). Logo, paramEstatisticosACE irá receber o valor da função seguinte. ::base.split garante que a função split da base R seja executada mesmo que haja outro pacote com uma função split sendo executado. O split separa o resultado dos parâmeteros estatísticos com base no grupo, que é grp, logo, "IDINV"
  lapply(function(x) split(x, x$nivel)) # Agora separa por nível, que é 3.7 ou 5.2
globalparamEstatisticosACE = popFromStrata(paramEstatisticosACE) # popFromStrata recebe o data.frame paramEstatisticosACE

# Função para cálculo da intensidade amostral recomendada
# (ha por parcela) da ACE que garante ~10% de erro (altere se desejar)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
erro <- 10
tamanhoIdealACE = function(y, g, Nh, errDesired=erro/100){
  vars = by(y, g, stats::var)
  Wh   = by(y, g, length) / length(y)
  Nh   = Nh[ names(Nh) %in% g ]
  B    = errDesired * mean(y)
  n    = sum( Nh^2 * vars / Wh ) / 
    ((B^2 * (sum(Nh)^2))/4 + sum(Nh * vars))
  return(n)
}

# Calcula o número possível de unidades amostrais (N) por estrato
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parcPorEstrato <- round(                                           # Nh
  by(st_area(st_as_sf(talhoesComGeo)), talhoes$IDINV, sum) /
    (mean(parcelas$AREAPARCEL)), 0)
IA <- round(AreaTotal/tamanhoIdealACE(y = parcelas$VTCC,
                                      g = parcelas$IDINV, 
                                      Nh = parcPorEstrato), 0)

# Intensidade amostral usada por estrato
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IAE <- round(by(st_area(st_as_sf(talhoesComGeo))/10000, talhoes$IDINV, sum) /
               (parcelas %>% count(IDINV))[2])[1]
IAS <- paste0(" usada: 1 parc / ", as.character(IAE) , " ha.")

# Intensidade amostral que seria necessária para ACE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
erroTexto <- as.character(erro)
IAI <- paste0("\n Necessárias p/ erro de ", erroTexto, 
              "%: 1 parc / ", IA, " ha.")

NotaDeRodape <- paste0(IAS, IAI)
globalparamEstatisticosACE[[grupos]] %>% t %>%  # transpõe o data frame
  kbl(caption = paste0("Amostragem Casual Estratificada (ACE)"),
      align = "r") %>%
  kable_classic(full_width = F) %>%
  footnote(general = NotaDeRodape,
           general_title = "Intensidade amostral",
           footnote_as_chunk = T)

# Gráfico de número de amostras necessárias por erro amostral aceitável
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
prjNome <- 'PRJ_Modelo'
dirNome <- paste0('C:/LiDAR/', prjNome)
dir.create(dirNome, showWarnings = F)
dirNome <- paste0(dirNome, '/GRAPH/')
dir.create(dirNome, showWarnings = F)
arqNome <- paste0(dirNome, prjNome, '_IntAmostral_ACSvsACE.png')

erro = 1:20
scsPlotn <- tamanhoIdealACS(parcelas$VTCC, N_ACS,
                            errDesired = erro/100)
cssPlotn <- tamanhoIdealACE(parcelas$VTCC,  parcelas$IDINV, 
                            parcPorEstrato, errDesired = erro/100)
# dsPlotn = dubleSamplePlotNumber(plotMetrics$PC_VOL_TOT,
#                                 plotMetrics$avgCrownHeight,
#                                 cropMetrics$avgCrownHeight, 300, erro/100)

png( arqNome, 30, 20, 'cm', res = 200)           # abre "impressão" PNG
# jpeg(arqNome, 30, 20, 'cm', res = 200)           # abre "impressão" JPG

plot( scsPlotn ~ erro, type='l', col='red', lwd=2, 
      ylim=c(0,200), ylab='Número de parcelas', xlab='Erro amostral (%)',
      axes=F)
lines(erro, cssPlotn, col='green', lwd=2)
# lines(erro, dsPlotn, col='blue', lwd=2)
box()
axis(1)
axis(2, at = c(25, 50, 75, 100, 125, 150, 175, 200))
abline(v=10, col='black', lwd=2, lty=2)
lines(c(0,15), rep(100, 2), col='black', lty=2, lwd=2)
# lines(c(0,5), rep(dsPlotn[5], 2), col='blue', lty=2, lwd=2)
# lines(c(0,5), rep(cssPlotn[5], 2), col='green', lty=2, lwd=2)
legend('topright', col=rainbow(3), lwd=2,
       legend = c('ACS', 'ACE'))
# legend = c('ACS', 'ACE', 'AD'))

dev.off()                                           # fecha "impressão"


# Salva resultados em planilha Excel
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
prjNome <- 'PRJ_Modelo'
dirNome <- paste0('C:/LiDAR/', prjNome)
dir.create(dirNome, showWarnings = F)
dirNome <- paste0(dirNome, '/RSLTS/')
dir.create(dirNome, showWarnings = F)

arqNome <- paste0(dirNome, prjNome, '_Results.xlsx')
export(list(ACS = paramEstatisticosACS,
            ACE37 = paramEstatisticosACE$IDINV$'3.7',
            ACE52 = paramEstatisticosACE$IDINV$'5.2'),
       file = arqNome)

# save.image(file='Modelo_inventario.RData')       # faz backup dos dados
# load('Modelo_inventario.RData')                       # recupera backup
#  ----