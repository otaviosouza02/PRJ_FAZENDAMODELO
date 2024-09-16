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
popFromStrata = function(factorStrataList){ # factorStrataList apresenta as estimativas de VTCC e MHDOM para cada uma das idades e o resultado da análise de confiança para cada uma
  popEstimates = foreach(i = paramEstatisticosACE, .combine = 'c') %do% { # Combina os dados em um vetor para cada um dos fatores de estratificação
    gpMeans = lapply(i, function(x) x[1,,drop=F]) %>% do.call(what = rbind) # a função lapply seleciona a primeira linha de cada um dos data.frames(paramEstatisticosACE$IDINV$`3.7 ou 5.2`). do.call(what = 'rbind) unifica a resposta em um único data.frame, sem o argumento a respota seria dada em dois df diferentes, sendo um para cada idade
    gpVars  = lapply(i, function(x) x[2,,drop=F]) %>% do.call(what = rbind) # seleciona a segunda linha (variâncias) para cada idade
    cols = 1:(ncol(gpMeans)-4) # cols é um vetor onde o número de casas é resultado do número de colunas de gpMeans - 4. Como gpMeans tem 6 colunas, cols é um vetor de duas casas
    popMean   = apply(gpMeans[,cols], 2, # [,cols] faz com que sejam selecionados os índices 1 e 2 de gpMeans, que no caso são as infos de MHDOM e VTCC para cada uma das idades. , 2 indica que a função a seguir será aplicada nas colunas do data.frame gpMeans. A resposta é a média ponderada de MHDOM e VTCC da população, ou seja, unificou as informações e já não mais as separa por estrato
                      function(x) sum(x*gpMeans$N) ) /N_ACS # Faz a média ponderada usando o MHDOM ou VTCC multiplicado pelo N (número máxímo de amostras para cada estrato). O resultado é dividido pelo número máximo de amostra da população domo um todo, unificando as idades. Retorna a média das estimativas de VTCC e MHDOM da população
    popVar    = apply(gpVars[,cols], 2, 
                      function(x) sum( x * (gpVars$N/N_ACS)^2 ) ) # Mesmo procedimento, mas aqui retorna a variância de MHDOM e VTCC na população
    popStdErr = sqrt(popVar) # Calcula o desvio padrão de MHDOM e VTCC da população (não é para cada idade)
    popCI     = calcCI(popStdErr, sum(gpMeans$n)) # calcCI (calcula o IC para 95% de confiança) recebe o desvio padrão e o tamanho da amostra (n é o tamanho da amostra de 3.7 somado com o tamanho da amostra de 5.2)
    popPars = data.frame(
      media = popMean, # Coluna média do df recebe a média das estimativas de MHDOM e VTCC da população
      var   = popVar, # Coluna var recebe a variância da população para cada variável de interesse (MHDOM e VTCC)
      dp    = popStdErr, # Coluna dp recebe o desvio padrão da população para cada variável de interesse (MHDOM e VTCC)
      ic    = popCI, # Coluna ic recebe o intervalo de confiança da população para cada variável de interesse (MHDOM e VTCC)
      erro  = 100 * popCI / popMean, # # Coluna erro recebe o erro (%) da população para cada estimativa das variáveis de interesse (MHDOM e VTCC)
      n     = sum( sapply(i, function(x) mean(x$n)) ) # Coluna n recebe o tamanho da amostra da população
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
globalparamEstatisticosACE = popFromStrata(paramEstatisticosACE) # popFromStrata recebe o data.frame paramEstatisticosACE e retorna as estatísticas de confiança da média ponderada das estimativas de MHDOM e VTCC



# Função para cálculo da intensidade amostral recomendada
# (ha por parcela) da ACE que garante ~10% de erro (altere se desejar)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
erro <- 10
tamanhoIdealACE = function(y, g, Nh, errDesired=erro/100){ # y contém os VTCC das parcelas de campo, g contém as IDINV das parcelas de campo e Nh apresenta o número máximo de parcelas amostrais para cada uma das idades
  vars = by(y, g, stats::var) # BY() Executa uma função em subconjuntos de um objeto (no caso, data.frame). Sintaxe: by(dado, subconjunto, função). var calcula a variância. Logo, vars recebe a variância dos VTCC das parcelas de campo para cada classe de idade
  Wh   = by(y, g, length) / length(y) # Seleciona a quantidade de parcelas existente para cada uma das idades e divide pelo total das parcelas. Wh é uma representação percentual do quanto cada grupo de parcelas (separadas por idade) representam do total das parcelas de campo.
  Nh   = Nh[ names(Nh) %in% g ] # A operação entre colchetes é uma comparação. Nela é verificado se as idades contidas na variável parcPorEstrato, que agrupa as idades e o número máximo de parcelas amostrais de cada talhão, de fato existem no data.frame "parcelas", retornando True ou False. Caso True, Nh receberá o número máximo de parcelas amostrais para cada idade
  B    = errDesired * mean(y) # B recebe 10% do valor médio das VTCC e não segrega por idade. Esse valor é tido como o erro aceitável em m³ por ha
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
IAE <- round(by(st_area(st_as_sf(talhoesComGeo))/10000, talhoes$IDINV, sum) / # Agrupa os talhões por idades e soma as suas áreas em ha. O resultado é divido pelo número de parcelas existente para cada idade
               (parcelas %>% count(IDINV))[2])[1] # IAE recebe a intensidade amostral usaada em cada estrato

                                    ##############################
                                    ## OTIMIZAR TODA ESSA PARTE ##
                                    ##############################

IAS <- paste0(" Em 3.7 anos, usada: 1 parc / ", as.character(IAE[nrow(IAE) - 1, nrow(IAE) - 1]) , " ha.")
IAS1 <- paste0(" Em 5.2 anos, usada: 1 parc / ", as.character(IAE[nrow(IAE), nrow(IAE) - 1]) , " ha.")

# Intensidade amostral que seria necessária para ACE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
erroTexto <- as.character(erro)
IAI <- paste0("\n Necessárias p/ erro de ", erroTexto, 
              "%: 1 parc / ", IA, " ha.")

# Erro está até acima de 10%, então eu não poderia diminuir a minha intensidade amostral como mostra a tabela

NotaDeRodape <- paste0(IAS, IAS1, IAI)
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