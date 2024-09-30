# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PRJ_Modelo_Inv2: Inventário da Fazenda Modelo   ~~~~~~~~~~~~~~~~~~~~~
#
# Autor: Luiz Carlos Estraviz Rodriguez
#        Otávio Magalhães Silva Souza
#        Departamento de Ciências Florestais
#        ESALQ/USP - 22/Set/2024
#
#   - Estimativas de inventário com amostragem em duas fases
#        Amostragem Dupla Simples (ADS)                             
#        Amostragem Dupla Estratificada (ADE)                             
#
# Linguagem de programação:
#       R (v 4.4.1)
#       package: forestinventory (v 1.0.0 2021-01-08 - Andreas Hill)
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls(all=TRUE))                                   # Limpa memória
gc()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lê pacote forestinventory
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(!require(tidyverse))
  install.packages(tidyverse)
library(tidyverse)

# Leitura da versão mais atual do pacote rio 
#           para importação de planilhas Excel
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!require("remotes")){install.packages("remotes")}
if (!require(rio))      {remotes::install_github("gesistsa/rio")}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lê pacote forestinventory
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(!require(forestinventory))  # Para melhor manipulação de dados e funções
  install.packages("forestinventory")
library(forestinventory)

# Define local e nome da planilha para leitura dos dados
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
prjNome  <- "PRJ_Modelo"
dirNome  <- paste0('C:/LiDAR/', prjNome, '/DADOS/')
arqNome  <- paste0(dirNome, prjNome, '_metrics.csv')
grid     <- import(arqNome)

head(grid)

X <- tibble(grid) %>% select(fase, areacell, MHDOM, idade, zmean, 
                                 zq45, zq75, zq95, VTCC)

head(X)

X$boundaryweights <- X$areacell / 400
X <- as.data.frame(X) # Tibble não é uma função nativa do R, é uma função chamada pelo tidyverse. Dessa forma, o formato da tabela que é gerada não é reconhecido pela função twophase() do package forestinventory. O forestinventory reconhece data frames, já que são nativos do R.

# Análise de regressão linear para verificar a correlação
# entre o p95 e a idade do inventário com o VTCC
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
m <- lm(VTCC ~ zq95 + idade, data = X)    # Análise de Regressão Linear
summary(m)                          # Mostra os resultados da regressão
VTCCparcelas <- X$VTCC[!is.na(X$VTCC)] # VTCCparcelas recebe os valores não nulos de VTCC contidos em X, ou seja, recebe os valores de VTCC das parcelas de campo
plot(VTCCparcelas, predict(m))              # Gráfico de observado vs predito
abline(0,1)

# Dupla Amostragem Casual
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reg2p_nex <- twophase(formula = VTCC ~ zq95 + idade, # formula relaciona os valores de VTCC com zq95 e IDINV (análise de regressão)
                      data = X, #  Base de dados utilizada
                      boundary_weights = "boundaryweights",
                      phase_id = list(phase.col = "fase", terrgrid.id = 2)) # phase_id recebe uma lista em que a coluna a ser analisada é a "Inventario" e o identificador da segunda fase é 2
summary(reg2p_nex) # Dá os resultados da Dupla Amostragem
confint(reg2p_nex) # Estatística de confiança

# Dupla Amostragem Estratificada
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reg2p_nex_est = twophase(
  formula = VTCC ~ zq95 + idade,
  data = X,
  phase_id =list(phase.col = "fase", terrgrid.id = 2),
  boundary_weights = "boundaryweights",
  small_area = list(sa.col = "idade", areas = c("3.7", "5.2"), unbiased = FALSE)) 
summary(reg2p_nex_est)
confint(reg2p_nex_est)

# sae.table<- estTable(est.list = list(reg2p_nex, reg2p_nex_est), add.ci=TRUE,
#                      vartypes = c("g_variance"))
# 
# sae.table.df<- as.data.frame(sae.table)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Amostragem simples em fase única - parâmetros do pacote
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Função:
# twophase(
#         formula    = Y ~ métricas, Y é a variável de interesse,
#         data       = dataframe ou vetor com a variável Y,
#         phase_id   = list (
#                         phase.col   = nome coluna id da fase,
#                         terrgrid.id = valor numérico do id da fase),
#         small_area = list(
#                        sa.col = nome coluna de estratificação,
#                        areas  = vetor c("", "", "") de estratos),
#         boundary_weights = coluna com pesos para ajustes de borda,
#         cluster  = nome da coluna, se houver cluster sampling)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Amostragem Dupla Simples (ADS)  substituir grisons por "completo"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----

# AreaTotal <- talhoes$AREA %>% sum             # Área total inventariada
# ADS <- twophase(formula  = tvol ~ mean + stddev + max + q75,
#                 data     = grid,
#                 phase_id = list(phase.col = "fase", 
#                                 terrgrid.id = 2),
#                 boundary_weights = "cellweight")
# 
# summary(ADS)
# confint(ADS)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                      >>>>>>>>>>>>>>>>>>  Adequar daqui pra frente !!!
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Número Total de Unidades Amostrais (N) na população ~~~~~~~~~~~~~~~~~
# erro         <- 0.05                                    # Erro desejado
# tamMedioParc <- mean(parcelas$AREAPARCEL) / 10000               # em ha
# N            <- round(AreaTotal / tamMedioParc , 0)

# Função para cálculo da intensidade amostral desejável da ACS
#                           para garantir um certo erro mínimo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# tamanhoIdealACS = function(x, N, erro){
#   media = x$estimation$estimate
#   var   = x$estimation$variance
#   n     = x$estimation$n2
#   t     = qt(1 - erro/2, n -1)
#   B     = erro * media
#   ndsej = round((N * var * t^2) / (N * B^2 + var * t^2), 0)
#   
#   lista = cbind(media, B, ndsej) %>% as.data.frame
#   colnames(lista) = c('media', 'bound', 'ndsej')
#   return(lista)
# }
# nACS <- tamanhoIdealACS(ACS, N, erro)

# Tabela com resultados do inventário (estimação + inferência) por ACS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                  PRECISA ACERTAR ESTE BLOCO AINDA !!!!!!!!!!!!!!!!!!!
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IAS <- paste0(" usada: 1 parc/", 
#               round( AreaTotal / ACS$estimation$n2, 0), " ha.")
# IAI <- paste0("\n Necessárias p/ erro de ", erro*100, "%: 1 parc/",
#               round(AreaTotal / nACS$ndsej, 0), " ha.")
# NotaDeRodape <- paste0(IAS, IAI)
# 
# quadroACS <- as.data.frame(
#   tibble(LimInf     = confint(ACS)$ci$ci_lower_op, 
#          Estimativa = ACS$estimation$estimate, 
#          LimSup     = confint(ACS)$ci$ci_upper_op, 
#          Variância  = ACS$estimation$variance, 
#          n          = ACS$estimation$n2))
# 
# quadroACS %>%
#   kbl(caption = paste0("Amostragem Casual Simples em ~", 
#       round(AreaTotal, 0), " ha."), align = "r") %>%
#   kable_classic(full_width = F) %>%
#   footnote(general       = NotaDeRodape,
#            general_title = "Intensidade amostral",
#            footnote_as_chunk = T)
# ----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Amostragem Dupla Estratificada (ADE)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----

#   ....    terminar este bloco, adequando o ADS ao para ADE

# ----