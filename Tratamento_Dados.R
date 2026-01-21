# ============================================================
# Projeto: TCC – ELSI Brasil
# Autor: Brenno Pacheco
# Script: Tratamento de Dados
# ============================================================
# Limpar ambiente ---------------------------------------------------------
rm(list = ls())
gc()

# Instalar dependência ----------------------------------------------------
# (DESCOMENTAR APENAS NA PRIMEIRA EXECUÇÃO
#  ou ao enviar o código para outro computador)

# install.packages("devtools")
# install.packages(c("mldr", "ROCR"), dependencies = TRUE)
# devtools::install_github("rivolli/utiml")
# Carregamento dos pacotes ----------------------------------------------------

library(devtools)
library(mldr)
library(ROCR)
library(utiml)

# Verificação de ferramenta de compilação (Windows)
Sys.which("make")

 
# Diretório de trabalho ----------------------------------------------------

setwd("C:/Users/brenn/Documents/TCC - BRUNO/Código/R")

# Função auxiliar para criação de dummies preservando NA ------------------
make_dummy <- function(x, value) {
  ifelse(is.na(x), NA, ifelse(x == value, 1, 0))
}


# Importar banco de dados ELSI --------------------------------------------

dados <- read.csv(
  file = "ELSI_Variaveis_Selecionadas (nomes_identificadores).csv",
  header = TRUE,
  sep = ";",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "-99", "-1")
)

# Sexo: 1 = homem | 0 = mulher (padrão do ELSI utilizado neste estudo)

# -----------------------------------------
# Variavéis PREDITORAS -----------------------------------------
# -----------------------------------------
# Escolaridade ------------------------------------------------------------

# Escolaridade (ordinal)
# 0 = Sem instrução / Fundamental incompleto
# 1 = Fundamental completo
# 2 = Médio completo
# 3 = Superior ou mais
# 99 / inválidos -> NA

# Garantir numérico
dados$Escolaridade <- as.numeric(dados$Escolaridade)

# Recodificação ordinal
dados$Escolaridade[dados$Escolaridade %in% 1:4]   <- 0
dados$Escolaridade[dados$Escolaridade %in% 5:9]   <- 1
dados$Escolaridade[dados$Escolaridade %in% 10:13] <- 2
dados$Escolaridade[dados$Escolaridade %in% 14:18] <- 3

# Tratar códigos inválidos como NA
dados$Escolaridade[!dados$Escolaridade %in% 0:3] <- NA




# Raça --------------------------------------------------------------------
# Criar colunas dummy para raça/cor (preservando NA)
# 1 = Branca | 2 = Preta | 3 = Parda | 4 = Amarela | 5 = Indígena
# 9 = ignorado (vira 0 em todas). NA permanece NA.

raca <- suppressWarnings(as.numeric(dados$Raca_Cor))

dados$Raca_Branca   <- ifelse(is.na(raca), NA, ifelse(raca == 1, 1, 0))
dados$Raca_Preta    <- ifelse(is.na(raca), NA, ifelse(raca == 2, 1, 0))
dados$Raca_Parda    <- ifelse(is.na(raca), NA, ifelse(raca == 3, 1, 0))
dados$Raca_Amarela  <- ifelse(is.na(raca), NA, ifelse(raca == 4, 1, 0))
dados$Raca_Indigena <- ifelse(is.na(raca), NA, ifelse(raca == 5, 1, 0))

# Código 9 = ignorado (0 em todas as categorias)
dados$Raca_Branca[raca == 9]   <- 0
dados$Raca_Preta[raca == 9]    <- 0
dados$Raca_Parda[raca == 9]    <- 0
dados$Raca_Amarela[raca == 9]  <- 0
dados$Raca_Indigena[raca == 9] <- 0

# Excluir coluna original
dados$Raca_Cor <- NULL


# Estado Civil ------------------------------------------------------------
# Criar dummies preservando NA
# 1 = Solteiro | 2 = Casado | 3 = Divorciado | 4 = Viúvo
# 9 / 99 = ignorado (mantido como NA)

ec <- suppressWarnings(as.numeric(dados$Estado_Civil))

dados$EC_Solteiro   <- ifelse(is.na(ec), NA, ifelse(ec == 1, 1, 0))
dados$EC_Casado     <- ifelse(is.na(ec), NA, ifelse(ec == 2, 1, 0))
dados$EC_Divorciado <- ifelse(is.na(ec), NA, ifelse(ec == 3, 1, 0))
dados$EC_Viuvo      <- ifelse(is.na(ec), NA, ifelse(ec == 4, 1, 0))

# Excluir coluna original
dados$Estado_Civil <- NULL



# Qtd_Filhos --------------------------------------------------------------
# 0  = não tem filhos
# >=1 = tem filhos
# 99 = não respondeu (NA)

filhos <- suppressWarnings(as.numeric(dados$Qtd_Filhos))
filhos[filhos == 99] <- NA

dados$Qtd_Filhos <- ifelse(
  is.na(filhos), NA,
  ifelse(filhos >= 1, 1, 0)
)


# Escolaridade da Mãe -----------------------------------------------------

# 0 = Sem instrução / Fundamental incompleto  (1–2)
# 1 = Fundamental completo                    (3–4)
# 2 = Médio completo                          (5)
# 3 = Superior ou mais                        (6)
# 8 / 9 / inválidos -> NA

# Garantir numérico (tratando vírgula, se existir)
dados$Escolaridade_Mae <- as.numeric(gsub(",", ".", dados$Escolaridade_Mae))

# Recodificação ordinal
dados$Escolaridade_Mae[dados$Escolaridade_Mae %in% 1:2] <- 0
dados$Escolaridade_Mae[dados$Escolaridade_Mae %in% 3:4] <- 1
dados$Escolaridade_Mae[dados$Escolaridade_Mae == 5]     <- 2
dados$Escolaridade_Mae[dados$Escolaridade_Mae == 6]     <- 3

# 8 / 9 e qualquer outro valor fora de 0:3 vira NA
dados$Escolaridade_Mae[!dados$Escolaridade_Mae %in% 0:3] <- NA



# Renda Domiciliar ---------------------------------------------------------
# d28:
# 1 a 20 = faixas de renda (conforme questionário)
# 99 = Não sabe/não respondeu -> NA

if (exists("dados2") && "d28" %in% names(dados2)) {
  
  dados2$Renda_Domiciliar <- suppressWarnings(as.numeric(dados2$d28))
  dados2$Renda_Domiciliar[dados2$Renda_Domiciliar == 99] <- NA
  
  # (opcional, recomendado) garante que só 1..20 permaneça válido
  dados2$Renda_Domiciliar[!(dados2$Renda_Domiciliar %in% 1:20) & !is.na(dados2$Renda_Domiciliar)] <- NA
  
  # checagem rápida
  table(dados2$Renda_Domiciliar, useNA = "ifany")
  
} else if (exists("dados") && "d28" %in% names(dados)) {
  
  dados$Renda_Domiciliar <- suppressWarnings(as.numeric(dados$d28))
  dados$Renda_Domiciliar[dados$Renda_Domiciliar == 99] <- NA
  dados$Renda_Domiciliar[!(dados$Renda_Domiciliar %in% 1:20) & !is.na(dados$Renda_Domiciliar)] <- NA
  
  table(dados$Renda_Domiciliar, useNA = "ifany")
}

# Fuma_Atualmente ---------------------------------------------------------
# 1 = fuma atualmente
# 0 = não fuma atualmente
# 8 = não se aplica (considerado como não fuma)
# 9 = não sabe/não respondeu -> NA

fuma <- suppressWarnings(
  as.numeric(gsub(",", ".", trimws(as.character(dados$Fuma_Atualmente))))
)

fuma[fuma == 9] <- NA

dados$Fuma_Atualmente <- ifelse(
  is.na(fuma), NA,
  ifelse(fuma %in% c(1, 2), 1, 0)
)

rm(fuma)


# Fumou_Passado -----------------------------------------------------------
# 1 = já fumou
# 0 = nunca fumou
# 8 = não se aplica (considerado como nunca fumou)
# 9 = não sabe/não respondeu -> NA

fumou <- suppressWarnings(
  as.numeric(gsub(",", ".", trimws(as.character(dados$Fumou_Passado))))
)

fumou[fumou == 9] <- NA

dados$Fumou_Passado <- ifelse(
  is.na(fumou), NA,
  ifelse(fumou %in% c(1, 2), 1, 0)
)

rm(fumou)


# Alcool_Freq -------------------------------------------------------------
# 0 = não consome bebida alcoólica
# 1 = consome bebida alcoólica
# 9 = NA (não respondeu)

alcool <- suppressWarnings(as.numeric(dados$Alcool_Freq))
alcool[alcool == 9] <- NA

dados$Alcool_Freq <- ifelse(
  is.na(alcool), NA,
  ifelse(alcool %in% c(2, 3), 1, 0)
)

rm(alcool)


# Ativ_Fisica_Vigorosa ----------------------------------------------------
# 1, 2, 3 -> 1 (pratica atividade física vigorosa)
# 4       -> 0 (não pratica)
# 9       -> NA (não respondeu)

atv_vig <- suppressWarnings(as.numeric(dados$Ativ_Fisica_Vigorosa))
atv_vig[atv_vig == 9] <- NA

dados$Ativ_Fisica_Vigorosa <- ifelse(
  is.na(atv_vig), NA,
  ifelse(atv_vig %in% c(1, 2, 3), 1, 0)
)

rm(atv_vig)


# Dias_Caminhada ----------------------------------------------------------
# Recodificação:
# 0       -> 0 (não caminhou)
# 1–3     -> 1 (caminhou pouco)
# 4–7     -> 2 (caminhou frequentemente)
# 9       -> NA (não respondeu)

caminhada <- suppressWarnings(as.numeric(dados$Dias_Caminhada))
caminhada[caminhada == 9] <- NA

dados$Dias_Caminhada <- ifelse(
  is.na(caminhada), NA,
  ifelse(caminhada == 0, 0,
         ifelse(caminhada %in% 1:3, 1,
                ifelse(caminhada %in% 4:7, 2, NA)))
)

rm(caminhada)


# Qualidade_Sono ----------------------------------------------------------
# Recodificação (escala invertida):
# 1 (Muito boa)  -> 4
# 2 (Boa)        -> 3
# 3 (Regular)    -> 2
# 4 (Ruim)       -> 1
# 5 (Muito ruim) -> 0
# 9              -> NA

sono <- suppressWarnings(
  as.numeric(gsub(",", ".", trimws(as.character(dados$Qualidade_Sono))))
)

sono[sono == 9] <- NA

dados$Qualidade_Sono <- ifelse(
  is.na(sono), NA,
  ifelse(sono %in% 1:5, 5 - sono, NA)
)

rm(sono)




# Dificuldade_Dormir ------------------------------------------------------
# Recodificação:
# 1 (frequente)        -> 0
# 2 (às vezes)         -> 1
# 3 (nunca/raramente)  -> 2
# 9                    -> NA

dormir <- suppressWarnings(
  as.numeric(gsub(",", ".", trimws(as.character(dados$Dificuldade_Dormir))))
)

dormir[dormir == 9] <- NA

dados$Dificuldade_Dormir <- ifelse(
  is.na(dormir), NA,
  ifelse(dormir %in% 1:3, dormir - 1, NA)
)

rm(dormir)


# Vegetais_Dias_Semana ----------------------------------------------------
# Recodificação:
# 0       -> 0 (não consome)
# 1–3     -> 1 (consumo baixo)
# 4–7     -> 2 (consumo frequente)
# 9       -> NA (não respondeu)

verduras <- suppressWarnings(as.numeric(dados$Vegetais_Dias_Semana))
verduras[verduras == 9] <- NA

dados$Vegetais_Dias_Semana <- ifelse(
  is.na(verduras), NA,
  ifelse(verduras == 0, 0,
         ifelse(verduras %in% 1:3, 1,
                ifelse(verduras %in% 4:7, 2, NA)))
)

rm(verduras)


# Frutas_Dias_Semana ------------------------------------------------------
# Recodificação:
# 0       -> 0 (não consome frutas)
# 1–3     -> 1 (consumo baixo)
# 4–7     -> 2 (consumo frequente)
# 9       -> NA (não respondeu)

frutas <- suppressWarnings(as.numeric(dados$Frutas_Dias_Semana))
frutas[frutas == 9] <- NA

dados$Frutas_Dias_Semana <- ifelse(
  is.na(frutas), NA,
  ifelse(frutas == 0, 0,
         ifelse(frutas %in% 1:3, 1,
                ifelse(frutas %in% 4:7, 2, NA)))
)

rm(frutas)



# IMC ------------------------------------------------------
# Cálculo do Índice de Massa Corporal
# IMC = peso (kg) / (altura (m))²
#
# Tratamentos:
# - 666 / 777 / 888 / 999 -> NA
# - valores escalados (ex.: 70650002 -> 70.650002)
# - exclusão de valores inválidos ou fora de faixa plausível

# Função auxiliar para limpeza de medidas antropométricas
clean_measure <- function(x,
                          special = c(666, 777, 888, 999),
                          scale = 1e6,
                          huge_threshold = 1000,
                          min_val = -Inf,
                          max_val = Inf) {
  
  # Converter texto para numérico (padrão PT-BR)
  if (is.character(x)) {
    x <- trimws(x)
    x <- gsub("\\.", "", x)
    x <- gsub(",", ".", x)
  }
  
  x <- suppressWarnings(as.numeric(x))
  
  # Códigos especiais -> NA
  x[x %in% special] <- NA
  
  # Corrigir valores gigantes (escala 1e6)
  idx_huge <- !is.na(x) & x > huge_threshold
  x[idx_huge] <- x[idx_huge] / scale
  
  # Remover inválidos e fora da faixa plausível
  x[!is.na(x) & (x <= 0 | x < min_val | x > max_val)] <- NA
  
  x
}

# Limpeza de peso e altura
peso   <- clean_measure(dados$Peso_Medio_Kg,   min_val = 20,  max_val = 300)
altura <- clean_measure(dados$Altura_Media_Cm, min_val = 100, max_val = 250)

# Cálculo do IMC (somente quando peso e altura são válidos)
dados$IMC <- ifelse(
  !is.na(peso) & !is.na(altura),
  peso / ((altura / 100) ^ 2),
  NA
)

# Remover variáveis auxiliares
rm(peso, altura)

# Excluir colunas originais
dados$Peso_Medio_Kg   <- NULL
dados$Altura_Media_Cm <- NULL

# IMC – Classificação (Binning) ------------------------------------------
# Critério OMS (adultos):
# 1 = IMC ideal (18.5 a 24.9)
# 0 = IMC fora do ideal
# NA = IMC ausente

dados$IMC_Classificacao <- ifelse(
  is.na(dados$IMC), NA,
  ifelse(dados$IMC >= 18.5 & dados$IMC <= 24.9, 1, 0)
)



# Pressão Arterial --------------------------------------------------------
# Variáveis clínicas (mmHg) usadas como preditoras no modelo de multimorbidade
#
# Limpeza:
# - converter para numérico (trata vírgula decimal)
# - 777 = erro de aferição -> NA
# - 999 = não fez / não respondeu -> NA
#
# Feature engineering (binning clínico simplificado):
# PA_Classificacao:
# 0 = Normal
#     Sistólica < 120 E Diastólica < 80
# 1 = Pré-Hipertensão / Limítrofe
#     Sistólica 120–139 OU Diastólica 80–89
# 2 = Hipertensão
#     Sistólica ≥ 140 OU Diastólica ≥ 90
# NA = ausente em qualquer uma das medidas

# 1) Conversão para numérico (padrão PT-BR: vírgula -> ponto)
pa_sis <- suppressWarnings(as.numeric(gsub(",", ".", dados$PA_Sistolica)))
pa_dia <- suppressWarnings(as.numeric(gsub(",", ".", dados$PA_Diastolica)))

# 2) Códigos especiais -> NA
pa_sis[pa_sis %in% c(777, 999)] <- NA
pa_dia[pa_dia %in% c(777, 999)] <- NA  # (se 777 aparecer, também tratamos)
# OBS: se no seu dicionário diastólica não tem 777, não tem problema tratar igual.

# 3) Salvar de volta no dataset (PA contínua, já limpa)
dados$PA_Sistolica  <- pa_sis
dados$PA_Diastolica <- pa_dia

# 4) Classificação categórica (0/1/2)
dados$PA_Classificacao <- ifelse(
  is.na(pa_sis) | is.na(pa_dia), NA,
  ifelse(pa_sis >= 140 | pa_dia >= 90, 2,
         ifelse(pa_sis >= 120 | pa_dia >= 80, 1, 0))
)

# 5) Limpar variáveis auxiliares
rm(pa_sis, pa_dia)




# RCQ (Razão Cintura–Quadril) --------------------------------------------
# Variáveis:
# - Cintura_Media_Cm (cm)  | 888 = recusou, 999 = não fez
# - Quadril_Medio_Cm (cm)  | 888 = recusou, 999 = não fez
#
# Tratamento:
# - converter para numérico (trata vírgula decimal)
# - 888/999 -> NA
# - remover valores inválidos/fora de faixa plausível
#
# Feature:
# - RCQ = cintura / quadril (contínua)
# - Risco_RCQ (0/1) por sexo:
#   Homens  (Sexo == 1): risco se RCQ > 0.90
#   Mulheres(Sexo == 0): risco se RCQ > 0.80

# 1) Conversão para numérico (padrão PT-BR)
cintura <- suppressWarnings(as.numeric(gsub(",", ".", dados$Cintura_Media_Cm)))
quadril <- suppressWarnings(as.numeric(gsub(",", ".", dados$Quadril_Medio_Cm)))

# 2) Códigos especiais -> NA
cintura[cintura %in% c(888, 999)] <- NA
quadril[quadril %in% c(888, 999)] <- NA

# 3) Faixas plausíveis (ajuste se desejar)
# Cintura e quadril em adultos/idosos: valores muito baixos/altos provavelmente são erro
cintura[!is.na(cintura) & (cintura < 40 | cintura > 200)] <- NA
quadril[!is.na(quadril) & (quadril < 50 | quadril > 220)] <- NA

# 4) RCQ contínuo (só calcula se ambos existirem)
dados$RCQ <- ifelse(!is.na(cintura) & !is.na(quadril),
                    cintura / quadril,
                    NA)

# (Opcional, recomendado) remover RCQ impossível
dados$RCQ[!is.na(dados$RCQ) & (dados$RCQ < 0.5 | dados$RCQ > 1.5)] <- NA

# 5) Risco_RCQ binário por sexo
sexo <- suppressWarnings(as.numeric(dados$Sexo))

dados$Risco_RCQ <- ifelse(
  is.na(sexo) | is.na(dados$RCQ), NA,
  ifelse(sexo == 1, ifelse(dados$RCQ > 0.90, 1, 0),
         ifelse(sexo == 0, ifelse(dados$RCQ > 0.80, 1, 0), NA))
)

# 6) Remover variáveis auxiliares
rm(cintura, quadril, sexo)

# 7) Excluir colunas originais (se você não precisar mais delas)
dados$Cintura_Media_Cm <- NULL
dados$Quadril_Medio_Cm <- NULL


# -----------------------------------------
# Variavéis DESFECHO -----------------------------------------

# -----------------------------------------
# Hipertensão -------------------------------------------------------------
# n28:
# 1 = Sim (hipertensão crônica)
# 2 = Sim, apenas durante a gravidez -> NA (excluído do rótulo)
# 0 = Não
# 9 = Não sabe/não respondeu -> NA

hipert <- suppressWarnings(
  as.numeric(gsub(",", ".", trimws(as.character(dados$Hipertensao))))
)

hipert[hipert %in% c(2, 9)] <- NA

dados$Hipertensao <- ifelse(
  is.na(hipert), NA,
  ifelse(hipert == 1, 1, 0)
)

rm(hipert)


# Diabetes ---------------------------------------------------------------
# Diabetes:
# 1 = Sim
# 2 = Sim, apenas durante a gravidez -> NA
# 0 = Não
# 9 = Não sabe/não respondeu -> NA

diab <- suppressWarnings(
  as.numeric(gsub(",", ".", trimws(as.character(dados$Diabetes))))
)

diab[diab %in% c(2, 9)] <- NA

dados$Diabetes <- ifelse(
  is.na(diab), NA,
  ifelse(diab == 1, 1, 0)
)

rm(diab)

# Colesterol_Alto ---------------------------------------------------------
# n44:
# 1 = Sim (colesterol alto)
# 0 = Não
# 9 = Não sabe/não respondeu -> NA

colest <- suppressWarnings(
  as.numeric(gsub(",", ".", trimws(as.character(dados$Colesterol_Alto))))
)

colest[colest == 9] <- NA

dados$Colesterol_Alto <- ifelse(
  is.na(colest), NA,
  ifelse(colest == 1, 1, 0)
)

rm(colest)

# Problema_cronico_coluna -----------------------------------------------
# n58:
# 1 = Sim
# 0 = Não
# 9 = Não sabe/não respondeu -> NA

# Renomear a coluna (se ainda estiver com nome antigo)
names(dados)[names(dados) == "Problema.crônico.de.coluna"] <- "Problema_cronico_coluna"

# Tratamento e binarização
coluna <- suppressWarnings(
  as.numeric(gsub(",", ".", trimws(as.character(dados$Problema_cronico_coluna))))
)

coluna[coluna == 9] <- NA

dados$Problema_cronico_coluna <- ifelse(
  is.na(coluna), NA,
  ifelse(coluna == 1, 1, 0)
)

rm(coluna)


# Depressao --------------------------------------------------------------
# n59:
# 1 = Sim
# 0 = Não
# 9 = Não sabe/não respondeu -> NA

depress <- suppressWarnings(
  as.numeric(gsub(",", ".", trimws(as.character(dados$Depressao))))
)

depress[depress == 9] <- NA

dados$Depressao <- ifelse(
  is.na(depress), NA,
  ifelse(depress == 1, 1, 0)
)

rm(depress)

# Artrite ------------------------------------------------------------------
# n56:
# 1 = Sim (tem artrite ou reumatismo)
# 0 = Não
# 9 = Não sabe/não respondeu -> NA

artrite <- suppressWarnings(
  as.numeric(gsub(",", ".", trimws(as.character(dados$Artrite))))
)

artrite[artrite == 9] <- NA

dados$Artrite <- ifelse(
  is.na(artrite), NA,
  ifelse(artrite == 1, 1, 0)
)

rm(artrite)

# Osteoporose ---------------------------------------------------------
# n57:
# 1 = Sim (osteoporose)
# 0 = Não
# 9 = Não sabe/não respondeu -> NA

osteo <- suppressWarnings(
  as.numeric(gsub(",", ".", trimws(as.character(dados$Osteoporose))))
)

osteo[osteo == 9] <- NA

dados$Osteoporose <- ifelse(
  is.na(osteo), NA,
  ifelse(osteo == 1, 1, 0)
)

rm(osteo)



# Doenças que NÃO SERÃO UTILIZADAS ----------------------------------------
# # Infarto ---------------------------------------------------------------
# # n46:
# # 1 = Sim (teve infarto)
# # 0 = Não
# # 9 = Não sabe/não respondeu -> NA
# 
# inf <- suppressWarnings(
#   as.numeric(gsub(",", ".", trimws(as.character(dados$Infarto))))
# )
# 
# inf[inf == 9] <- NA
# 
# dados$Infarto <- ifelse(
#   is.na(inf), NA,
#   ifelse(inf == 1, 1, 0)
# )
# 
# rm(inf)
# 
# # Angina ---------------------------------------------------------------
# # n48:
# # 1 = Sim (tem angina)
# # 0 = Não
# # 9 = Não sabe/não respondeu -> NA
# 
# ang <- suppressWarnings(
#   as.numeric(gsub(",", ".", trimws(as.character(dados$Angina))))
# )
# 
# ang[ang == 9] <- NA
# 
# dados$Angina <- ifelse(
#   is.na(ang), NA,
#   ifelse(ang == 1, 1, 0)
# )
# 
# rm(ang)
# 
# # Insuficiencia_Cardiaca -----------------------------------------------
# # n50:
# # 1 = Sim (insuficiência cardíaca)
# # 0 = Não
# # 9 = Não sabe/não respondeu -> NA
# 
# ic <- suppressWarnings(
#   as.numeric(gsub(",", ".", trimws(as.character(dados$Insuficiencia_Cardiaca))))
# )
# 
# ic[ic == 9] <- NA
# 
# dados$Insuficiencia_Cardiaca <- ifelse(
#   is.na(ic), NA,
#   ifelse(ic == 1, 1, 0)
# )
# 
# rm(ic)
# 
# # AVC_Derrame ------------------------------------------------------------
# # n52:
# # 1 = Sim (teve AVC/derrame)
# # 0 = Não
# # 9 = Não sabe/não respondeu -> NA
# 
# avc <- suppressWarnings(
#   as.numeric(gsub(",", ".", trimws(as.character(dados$AVC_Derrame))))
# )
# 
# avc[avc == 9] <- NA
# 
# dados$AVC_Derrame <- ifelse(
#   is.na(avc), NA,
#   ifelse(avc == 1, 1, 0)
# )
# 
# rm(avc)
# 
# # Asma ------------------------------------------------------------
# # n54:
# # 1 = Sim (tem asma)
# # 0 = Não
# # 9 = Não sabe/não respondeu -> NA
# # Observação: bronquite já está excluída pelo questionário
# 
# asma <- suppressWarnings(
#   as.numeric(gsub(",", ".", trimws(as.character(dados$Asma))))
# )
# 
# asma[asma == 9] <- NA
# 
# dados$Asma <- ifelse(
#   is.na(asma), NA,
#   ifelse(asma == 1, 1, 0)
# )
# 
# rm(asma)
# 
# # Cancer ------------------------------------------------------------
# # n60:
# # 1 = Sim (tem ou teve câncer)
# # 0 = Não
# # 9 = Não sabe/não respondeu -> NA
# 
# cancer <- suppressWarnings(
#   as.numeric(gsub(",", ".", trimws(as.character(dados$Cancer))))
# )
# 
# cancer[cancer == 9] <- NA
# 
# dados$Cancer <- ifelse(
#   is.na(cancer), NA,
#   ifelse(cancer == 1, 1, 0)
# )
# 
# rm(cancer)
# 
# 
# # Angioplastia_Stent -----------------------------------------
# # n66:
# # 1 = Sim (já realizou procedimento)
# # 0 = Não
# # 9 = Não sabe/não respondeu -> NA
# 
# angio <- suppressWarnings(
#   as.numeric(gsub(",", ".", trimws(as.character(dados$Angioplastia_Stent))))
# )
# 
# angio[angio == 9] <- NA
# 
# dados$Angioplastia_Stent <- ifelse(
#   is.na(angio), NA,
#   ifelse(angio == 1, 1, 0)
# )
# 
# rm(angio)
# 
# 
# # Insuficiencia_Renal ------------------------------------------------------
# # n61:
# # 1 = Sim (tem insuficiência renal crônica)
# # 0 = Não
# # 9 = Não sabe/não respondeu -> NA
# 
# insuf_renal <- suppressWarnings(
#   as.numeric(gsub(",", ".", trimws(as.character(dados$Insuficiencia_Renal))))
# )
# 
# insuf_renal[insuf_renal == 9] <- NA
# 
# dados$Insuficiencia_Renal <- ifelse(
#   is.na(insuf_renal), NA,
#   ifelse(insuf_renal == 1, 1, 0)
# )
# 
# rm(insuf_renal)
# 
# 
# # DPOC_Pulmao --------------------------------------------------------------
# # n55:
# # 1 = Sim (tem enfisema / bronquite crônica / DPOC)
# # 0 = Não
# # 9 = Não sabe/não respondeu -> NA
# 
# dpoc <- suppressWarnings(
#   as.numeric(gsub(",", ".", trimws(as.character(dados$DPOC_Pulmao))))
# )
# 
# dpoc[dpoc == 9] <- NA
# 
# dados$DPOC_Pulmao <- ifelse(
#   is.na(dpoc), NA,
#   ifelse(dpoc == 1, 1, 0)
# )
# 
# rm(dpoc)
# 
# 
# 


# EXCLUIR DOENÇAS QUE NÃO SERÃO UTILIZADAS NO MODELO  -------------------------


dados$Infarto               <- NULL
dados$Angina                <- NULL
dados$Insuficiencia_Cardiaca<- NULL
dados$AVC_Derrame            <- NULL
dados$Asma                  <- NULL
dados$Cancer                <- NULL
dados$Angioplastia_Stent    <- NULL
dados$Insuficiencia_Renal   <- NULL
dados$DPOC_Pulmao            <- NULL
dados$Parkinson             <- NULL
dados$Alzheimer             <- NULL



# Salvar Dataset ----------------------------------------------------------
saveRDS(dados, "dados_tratados2.rds")

# Conferência -------------------------------------------------------------
dados2 <- readRDS("dados_tratados2.rds")
View(dados2)

# Exportar tabela ---------------------------------------------------------
write.csv(dados2, "dados_tratados2.csv", row.names = FALSE)

