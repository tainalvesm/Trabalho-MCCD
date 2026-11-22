library(readr)
library(tidyverse)
library(dplyr)


# Importando  o Dataset
dados_participantes <-  read.csv("dados/Participantes_AL.csv")
dados_participantes
dados_resultados <-  read.csv("dados/Resultados_AL.csv")
dados_resultados

#Limpeza dos dados
## Renomeando as variáveis que iremos usar:
dados_participantes <- dados_participantes %>%
  rename(faixa_etaria = TP_FAIXA_ETARIA,
         sexo = TP_SEXO,
         cor_raça = TP_COR_RACA,
         conclusao_em = TP_ST_CONCLUSAO,
         tipo_ensino = TP_ENSINO,
         pai_escolaridade = Q001,
         mae_escolaridade = Q002,
         ocupacao_pai = Q003,
         ocupacao_mae = Q004,
         qnt_moradores_casa = Q005,
         tem_renda = Q006,
         renda_familiar_mensal= Q007,
         servicos_domesticos_cntrdo = Q008,
         casa_banheiro = Q009,
         casa_quarto = Q010,
         tem_carro = Q011,
         tem_motocicleta = Q012,
         casa_geladeira = Q013,
         casa_freezer = Q014,
         casa_maquina_roupa = Q015,
         casa_microondas = Q016,
         casa_aspirador_po = Q017,
         casa_TV = Q018,
         casa_TV_assinatura = Q019,
         casa_internet_wifi = Q020,
         casa_comp_notebook = Q021,
         tem_telefone_celular = Q022,
         tipo_escola_freq_EM = Q023)

dados_participantes <- dados_participantes %>%
  rename(treineiro = IN_TREINEIRO)


dados_resultados <- dados_resultados %>%
  rename(dependencia_adminstrativa = TP_DEPENDENCIA_ADM_ESC,
         localizacao = TP_LOCALIZACAO_ESC,
         situacao_funcionamento = TP_SIT_FUNC_ESC)


dados_resultados <- dados_resultados %>%
  rename(municipio_prova = NO_MUNICIPIO_PROVA)

dados_participantes <- dados_participantes %>%
  rename(municipio_prova = NO_MUNICIPIO_PROVA)

##Transformar variáveis numéricas em categoricas e fatores:
dados_participantes <- dados_participantes %>%
  mutate(faixa_etaria = case_when(
    faixa_etaria == 1 ~ "Menor de 17 anos",
    faixa_etaria >= 2 & faixa_etaria <= 10 ~ "Entre 17 e 25 anos",
    faixa_etaria == 11 ~ "Entre 26 e 30 anos",
    faixa_etaria == 12 ~ "Entre 31 e 35 anos",
    faixa_etaria == 13 ~  "Entre 36 e 40 anos",
    faixa_etaria == 14 ~ "Entre 41 e 45 anos",
    faixa_etaria == 15 ~ "Entre 46 e 50 anos",
    faixa_etaria == 16 ~ "Entre 51 e 55 anos",
    faixa_etaria == 17 ~ "Entre 56 e 60 anos",
    faixa_etaria == 18 ~ "Entre 61 e 65 anos",
    faixa_etaria == 19 ~ "Entre 66 e 70 anos",
    faixa_etaria == 20 ~  " Maior de 70 anos"
  ))

dados_participantes <- dados_participantes %>%
  mutate(treineiros = case_when(
    treineiro == 1 ~ "Sim",
    treineiro == 0 ~ "Não"
  ))

dados_participantes <- dados_participantes %>%
  mutate(cor_raça = case_when(
    cor_raça == 0 ~ "Não declarado",
    cor_raça == 1 ~ "Branca",
    cor_raça == 2 ~ "Preta",
    cor_raça == 3 ~ "Parda",
    cor_raça ==4 ~  "Amarela",
    cor_raça == 5 ~ "Indígena",
    cor_raça == 6 ~  " Não dispõe da informação"
  ))

dados_participantes <- dados_participantes %>%
  mutate(conclusao_em = case_when(
    conclusao_em == 1 ~ "Já concluiu o Ensino Médio",
    conclusao_em == 2 ~ "Cursando o EM com conclusão em 2024",
    conclusao_em == 3 ~ "Cursando o EM com conclusão após 2024",
    conclusao_em == 4 ~ "Não concluiu e não está cursando o EM"
    
  ))

dados_participantes <- dados_participantes %>%
  mutate(tipo_ensino = case_when(
    tipo_ensino == 1 ~ "Ensino Regular",
    tipo_ensino == 2 ~  "Educação Especial - Modalidade Substitutiva"
  ))

dados_participantes <- dados_participantes %>%
  mutate(ocupacao_pai = case_when(
    ocupacao_pai == "A" ~ "Grupo 1",
    ocupacao_pai == "B" ~  "Grupo 2",
    ocupacao_pai == "C"  ~ "Grupo 3",
    ocupacao_pai == "D" ~ "Grupo 4",
    ocupacao_pai == "E" ~ "Grupo 5",
    ocupacao_pai == "F" ~ "Não sabe"
  ))


dados_participantes <- dados_participantes %>%
  mutate(ocupacao_mae = case_when(
    ocupacao_mae == "A" ~ "Grupo 1",
    ocupacao_mae == "B" ~  "Grupo 2",
    ocupacao_mae == "C"  ~ "Grupo 3",
    ocupacao_mae == "D" ~ "Grupo 4",
    ocupacao_mae == "E" ~ "Grupo 5",
    ocupacao_mae == "F" ~ "Não sabe"
  ))

dados_resultados <- dados_resultados %>%
  mutate(dependencia_adminstrativa = case_when(
    dependencia_adminstrativa == 1 ~"Federal",
    dependencia_adminstrativa == 2 ~ "Estadual",
    dependencia_adminstrativa == 3 ~ "Municipal",
    dependencia_adminstrativa == 4 ~ "Privada"
  ))

dados_resultados <- dados_resultados %>%
  mutate(localizacao = case_when(
    localizacao == 1 ~ "Urbana",
    localizacao == 2 ~ "Rural"
  ))

dados_resultados <- dados_resultados %>%
  mutate(situacao_funcionamento = case_when(
    situacao_funcionamento == 1 ~ "Em atividade",
    situacao_funcionamento == 2 ~ "Paralisada",
    situacao_funcionamento == 3 ~ "Extinta",
    situacao_funcionamento == 4 ~ "Escola extinta em anos anteriores"
  ))



# Consertando os nomes das cidades
dados_resultados %>% distinct(municipio_prova)
dados_participantes %>% distinct(municipio_prova)

dados_participantes$municipio_prova <- iconv(dados_participantes$municipio_prova,
                                             from = "latin1", to = "UTF-8")

dados_resultados$municipio_prova <- iconv(dados_resultados$municipio_prova,
                                          from = "latin1", to = "UTF-8")
#Valores Faltantes

#Verificando os dados faltantes em cada coluna
colSums(is.na(dados_resultados))
colSums(is.na(dados_participantes)) #valores faltantes apenas nos treineiros

#Removendo as linhas com os valores faltantes
dados_participantes %>% drop_na(faixa_etaria) %>% count(faixa_etaria)
dados_resultados %>% drop_na(dependencia_adminstrativa) %>% count(dependencia_adminstrativa)

#Verificando
colSums(is.na(dados_resultados))
colSums(is.na(dados_participantes))
