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

# Leitura da versão mais atual do pacote rio 
#           para importação de planilhas Excel
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!require("remotes")){install.packages("remotes")}
if (!require(rio))      {remotes::install_github("gesistsa/rio")}
library(rio)

# Leitura da versão mais atual do pacote Tidyverse para
# melhor agilidade e organização do script
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(!require(tidyverse))  # Para melhor manipulação de dados e funções
  install.packages("tidyverse")
library(tidyverse)

# Leitura da versão mais atual do pacote Tidyverse para
# melhor agilidade e organização do script
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(!require(kableExtra))  # Para melhor manipulação de dados e funções
  install.packages("kableExtra")
library(kableExtra)


# Define local e nome da planilha para leitura dos dados
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
prjNome  <- 'PRJ_Modelo'
dirNome  <- paste0('C:/LiDAR/', prjNome, '/DADOS/')
arqNome  <- paste0(dirNome, prjNome, '.xlsx')
talhoes  <- import(arqNome, which = "talhoes")
parcelas <- import(arqNome, which = "parcelas")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lê pacote forestinventory
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(!require(forestinventory))  # Para melhor manipulação de dados e funções
  install.packages("forestinventory")
library(forestinventory)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Amostragem simples em fase única - parâmetros do pacote
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Função:
# onephase(
#         formula  = Y ~ 1, Y é a variável medida no campo,
#         data     = dataframe ou vetor com a variável Y,
#         phase_id = list (
#                         phase.col   = nome coluna id da fase,
#                         terrgrid.id = valor numérico do id da fase),
#         area     = list(
#                        sa.col = nome coluna de estratificação,
#                        areas  = vetor c("", "", "") de estratos),
#         cluster  = nome da coluna, se houver cluster sampling)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Amostragem Casual Simples (ACS)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
AreaTotal <- talhoes$AREA %>% sum             # Área total inventariada
# ACS <- onephase(formula  = MHDOM ~ 1,
ACS <- onephase(formula  = VTCC ~ 1,
                data     = parcelas,
                phase_id = list(phase.col = "FASE", terrgrid.id = 1))
summary(ACS)
confint(ACS)

parcelas$VTCC

# Número Total de Unidades Amostrais (N) na população ~~~~~~~~~~~~~~~~~
erro         <- 0.05                                    # Erro desejado
tamMedioParc <- mean(parcelas$AREAPARCEL) / 10000               # em ha
N            <- round(AreaTotal / tamMedioParc , 0)

# Função para cálculo da intensidade amostral desejável da ACS
#                           para garantir um certo erro mínimo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tamanhoIdealACS = function(x, N, erro, alpha = 0.05){
  media = ACS$estimation$estimate
  var   = ACS$estimation$variance
  n     = ACS$estimation$n2
  t     = qt(1 - alpha/2, n - 1)
  B     = erro * media
  ndsej = round((N * var * t^2) / (N * (erro * media)^2 + var * t^2), 0)
  
  lista = cbind(media, B, ndsej) %>% as.data.frame
  colnames(lista) = c('media', 'bound', 'ndsej')
  return(lista)
}
nACS <- tamanhoIdealACS(ACS, N, erro)

#n = (N * (Z**2) * (sigma**2)) / ((N - 1) * (E**2) + (Z**2) * (sigma**2))

# Tabela com resultados do inventário (estimação + inferência) por ACS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IAS <- paste0(" usada: 1 parc/", 
              round( AreaTotal / ACS$estimation$n2, 0), " ha.")
IAI <- paste0("\n Necessárias p/ erro de ", erro*100, "%: 1 parc/",
              round(AreaTotal / nACS$ndsej, 0), " ha.")
NotaDeRodape <- paste0(IAS, IAI)

quadroACS <- as.data.frame(
  tibble(LimInf95   = confint(ACS)$ci$ci_lower_op, 
         Estimativa = ACS$estimation$estimate, 
         LimSup95   = confint(ACS)$ci$ci_upper_op, 
         Variância  = ACS$estimation$variance, 
         n          = ACS$estimation$n2))

quadroACS %>%
  kbl(caption = paste0("Amostragem Casual Simples em ~", 
      round(AreaTotal, 0), " ha."), align = "r") %>%
  kable_classic(full_width = F) %>%
  footnote(general       = NotaDeRodape,
           general_title = "Intensidade amostral",
           footnote_as_chunk = T)
# ----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Amostragem Casual Estratificada (ACE)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
AreaTotal <- talhoes$AREA %>% sum             # Área total inventariada
# ACE <- onephase(formula  = MHDOM ~ 1,
ACE <- onephase(formula  = VTCC ~ 1,
                data     = parcelas,
                phase_id = list(phase.col = "FASE", terrgrid.id = 1),
                area     = list(sa.col = "IDINV", areas = c(3.7, 5.2)))
summary(ACE)
confint(ACE)

head(parcelas)

# Número Total de Unidades Amostrais (N) por estrato na população ~~~~~
erro         <- 0.1                                    # Erro desejado
tamMedioParc <- mean(parcelas$AREAPARCEL) / 10000               # em ha
N            <- round(AreaTotal / tamMedioParc, 0)
Nh           <- round(tapply(talhoes$AREA, talhoes$IDINV, FUN=sum) /
                        tamMedioParc, 0)

# Função para cálculo da intensidade amostral desejável da ACE
# para garantir um certo erro mínimo.
#                               Shiver&Borders (1996, pág 129 eq. 5.10)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tamanhoIdealACE = function(x, N, Nh, erro){
  media = sum( Nh * x$estimation$estimate ) / N
  var   = x$estimation$variance
  dh    = sqrt(var)
  wh    = (Nh*dh) / sum(Nh*dh)    # desconsiderada diferença nos custos
  B     = erro * media
  ndsej = round(sum( (Nh^2 * var) / wh ) / 
                   ( (N^2  * B^2) / 4 + sum(Nh * var) ), 0)
  
  lista = cbind(media, B, ndsej) %>% as.data.frame
  colnames(lista) = c('media', 'bound', 'ndsej')
  return(lista)
}
nACE <- tamanhoIdealACE(ACE, N, Nh, erro)

# Tabela com resultados do inventário (estimação + inferência) por ACE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IAS <- paste0(" usada: 1 parc/", 
              round( AreaTotal / sum(ACE$estimation$n2), 0), " ha.")
IAI <- paste0("\n Necessárias p/ erro de ", erro*100, "%: 1 parc/",
              round(AreaTotal / nACE$ndsej, 0), " ha.")
NotaDeRodape <- paste0(IAS, IAI)

quadroACE <- as.data.frame(
  tibble(
    Estrato    = ACE$estimation$area,
    LimInf95   = confint(ACE)$ci$ci_lower_op, 
    Estimativa = ACE$estimation$estimate,
    LimSup95   = confint(ACE)$ci$ci_upper_op,
    Variância  = ACE$estimation$variance,
    n          = ACE$estimation$n2))

quadroACE %>%
  kbl(caption = paste0("Amostragem Casual Estratificada em ~", 
                       round(AreaTotal, 0), " ha."), align = "r") %>%
  kable_classic(full_width = F) %>%
  footnote(general       = NotaDeRodape,
           general_title = "Intensidade amostral",
           footnote_as_chunk = T)
# ----
