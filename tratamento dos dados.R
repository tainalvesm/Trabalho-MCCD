library(readr)
library(tidyverse)
library(dplyr)


# Importando  o Dataset.
dados_participantes <- read.csv("C:/Users/taina/Desktop/Participantes_AL.csv")
dados_participantes
dados_resultados <- read.csv("C:/Users/taina/Desktop/Resultados_AL.csv")
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
         tipo_escola_freq_EM = Q023
         )


dados_resultados <- dados_resultados %>%
  rename(dependencia_adminstrativa = TP_DEPENDENCIA_ADM_ESC,
         localizacao = TP_LOCALIZACAO_ESC,
         situacao_funcionamento = TP_SIT_FUNC_ESC)

##Transformar variáveis numéricas em categoricas e fatores:
dados_participantes <- dados_participantes %>%
  mutate(faixa_etaria = case_when(
    faixa_etaria == 1 ~ "Menor de 17 anos",
    faixa_etaria >= 2 & faixa_etaria < 10 ~ " Entre 18 e 26 anos",
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
  mutate(cor_raça = case_when(
    cor_raça == 0 ~ "Não declarado",
    cor_raça == 1 ~ "Branca",
    cor_raça == 2 ~ "Preta",
    cor_raça == 3 ~ "Parda",
    cor_raça ==4 ~  "Amarela",
    cor_raça == 5 ~ "Indígina",
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
