library(readr)
library(tidyverse)
library(dplyr)

#paleta
cor_principal  <- "#A6535D"
cor_secundaria <- "#668596"
cor_destaque   <- "#C98D91"
cor_areia      <- "#DCCDC0"
cor_texto      <- "#34383D"
cor_cinza      <- "#929292"

tema_relatorio <- theme_minimal() +
  theme(
    plot.title = element_text(
      size = 15,
      face = "bold",
      color = cor_texto,
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      size = 9,
      color = cor_cinza,
      margin = margin(b = 18)
    ),
    axis.title = element_text(
      size = 9,
      color = cor_texto
    ),
    axis.text = element_text(
      size = 9,
      color = cor_texto
    ),
    panel.grid.major.x = element_line(
      color = "#EAEAEA",
      linewidth = 0.4
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.margin = margin(18, 25, 18, 18)
  )

tema_barras_verticais <- tema_relatorio +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#EAEAEA", linewidth = 0.4)
  )

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
         cor_raca = TP_COR_RACA,
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
  mutate(
    faixa_etaria_cod = suppressWarnings(as.integer(trimws(as.character(faixa_etaria)))),
    faixa_etaria = case_when(
      faixa_etaria_cod == 1  ~ "Menor de 17 anos",
      faixa_etaria_cod == 2  ~ "17 anos",
      faixa_etaria_cod == 3  ~ "18 anos",
      faixa_etaria_cod == 4  ~ "19 anos",
      faixa_etaria_cod == 5  ~ "20 anos",
      faixa_etaria_cod == 6  ~ "21 anos",
      faixa_etaria_cod == 7  ~ "22 anos",
      faixa_etaria_cod == 8  ~ "23 anos",
      faixa_etaria_cod == 9  ~ "24 anos",
      faixa_etaria_cod == 10 ~ "25 anos",
      faixa_etaria_cod == 11 ~ "Entre 26 e 30 anos",
      faixa_etaria_cod == 12 ~ "Entre 31 e 35 anos",
      faixa_etaria_cod == 13 ~ "Entre 36 e 40 anos",
      faixa_etaria_cod == 14 ~ "Entre 41 e 45 anos",
      faixa_etaria_cod == 15 ~ "Entre 46 e 50 anos",
      faixa_etaria_cod == 16 ~ "Entre 51 e 55 anos",
      faixa_etaria_cod == 17 ~ "Entre 56 e 60 anos",
      faixa_etaria_cod == 18 ~ "Entre 61 e 65 anos",
      faixa_etaria_cod == 19 ~ "Entre 66 e 70 anos",
      faixa_etaria_cod == 20 ~ "Maior de 70 anos",
      TRUE ~ NA_character_
    )
  )

dados_participantes <- dados_participantes %>%
  mutate(treineiros = case_when(
    treineiro == 1 ~ "Sim",
    treineiro == 0 ~ "Não"
  ))

dados_participantes <- dados_participantes %>%
  mutate(cor_raca = case_when(
    cor_raca == 0 ~ "Não declarado",
    cor_raca == 1 ~ "Branca",
    cor_raca == 2 ~ "Preta",
    cor_raca == 3 ~ "Parda",
    cor_raca ==4 ~  "Amarela",
    cor_raca == 5 ~ "Indígena",
    cor_raca == 6 ~  " Não dispõe da informação"
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

dados_participantes %>%
  count(faixa_etaria, name = "n") %>%
  mutate(
    pct = round(100 * n / sum(n), 2),
    pct_acumulado = round(cumsum(pct), 2)
  )




# Faixa etária

niveis_y <- c(
  "Menor de 17 anos", "17 anos", "18 anos", "19 anos", "20 anos",
  "21 anos", "22 anos", "23 anos", "24 anos", "25 anos",
  "Entre 26 e 30 anos", "Entre 31 e 35 anos", "Entre 36 e 40 anos",
  "Entre 41 e 45 anos", "Entre 46 e 50 anos", "Entre 51 e 55 anos",
  "Entre 56 e 60 anos", "Entre 61 e 65 anos", "Entre 66 e 70 anos",
  "Maior de 70 anos"
)

grafico_faixa_etaria <- dados_participantes %>%
  filter(!is.na(faixa_etaria)) %>%
  mutate(faixa_etaria = factor(faixa_etaria, levels = niveis_y)) %>%
  count(faixa_etaria, .drop = FALSE, name = "n") %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ggplot(aes(x = n, y = faixa_etaria)) +
  geom_col(fill = cor_principal, width = 0.7) +
  geom_text(
    aes(label = ifelse(
      n > 0,
      sprintf("%s (%.1f%%)", format(n, big.mark = ".", decimal.mark = ","), pct),
      ""
    )),
    hjust = -0.15,
    size = 3.2,
    color = cor_texto
  ) +
  scale_x_continuous(
    limits = c(0, 26000), # leve folga para o rótulo de 21.628 (24.3%) não encostar na borda
    breaks = seq(0, 25000, by = 5000),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_y_discrete(drop = FALSE) +
  labs(
    title = "Distribuição por faixa etária",
    subtitle = "Inscritos do ENEM 2024 em Alagoas",
    x = "Número de inscritos",
    y = NULL
  ) +
  tema_relatorio

grafico_faixa_etaria

# Sexo
grafico_sexo <- dados_participantes %>%
  count(sexo, name = "n") %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ggplot(aes(x = sexo, y = n, fill = sexo)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(
    aes(label = sprintf("%s (%.1f%%)", format(n, big.mark = ".", decimal.mark = ","), pct)),
    vjust = -0.5,
    size = 3.5,
    color = cor_texto
  ) +
  scale_x_discrete(labels = c("F" = "Feminino (F)", "M" = "Masculino (M)")) +
  scale_fill_manual(values = c("F" = cor_principal, "M" = cor_secundaria)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Distribuição de inscritos do ENEM 2024 em Alagoas por sexo",
    x = NULL,
    y = "Número de inscritos"
  ) +
  tema_relatorio


# Raça
grafico_raca <- dados_participantes %>%
  filter(!is.na(cor_raca)) %>%
  count(cor_raca, name = "n") %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ggplot(aes(x = n, y = reorder(cor_raca, n))) +
  geom_col(fill = cor_principal, width = 0.7) +
  geom_text(
    aes(label = sprintf("%s (%.1f%%)", format(n, big.mark = ".", decimal.mark = ","), pct)),
    hjust = -0.15,
    size = 3.2,
    color = cor_texto
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "Distribuição por cor/raça",
    subtitle = "Inscritos do ENEM 2024 em Alagoas",
    x = "Número de inscritos",
    y = NULL
  ) +
  tema_relatorio

# Treineiros
dados_participantes <- dados_participantes %>%
  mutate(
    treineiros = case_when(
      treineiro == 0 ~ "Não treineiro",
      treineiro == 1 ~ "Treineiro",
      TRUE ~ NA_character_
    )
  )

grafico_treineiro <- dados_participantes %>%
  filter(!is.na(treineiros)) %>%
  count(treineiros, name = "n") %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ggplot(aes(x = treineiros, y = n, fill = treineiros)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(
    aes(label = sprintf("%s (%.1f%%)", format(n, big.mark = ".", decimal.mark = ","), pct)),
    vjust = -0.5,
    size = 3.2,
    color = cor_texto
  ) +
  scale_fill_manual(values = c(
    "Não treineiro" = cor_secundaria,
    "Treineiro" = cor_destaque
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Distribuição por condição de treineiro",
    subtitle = "Inscritos do ENEM 2024 em Alagoas",
    x = NULL,
    y = "Número de inscritos"
  ) +
  tema_barras_verticais

# Conclusão do Ensino Médio
grafico_conclusao <- dados_participantes %>%
  filter(!is.na(conclusao_em)) %>%
  count(conclusao_em, name = "n") %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ggplot(aes(x = n, y = reorder(conclusao_em, n))) +
  geom_col(fill = cor_principal, width = 0.7) +
  geom_text(
    aes(label = sprintf("%s (%.1f%%)", format(n, big.mark = ".", decimal.mark = ","), pct)),
    hjust = -0.15,
    size = 3.2,
    color = cor_texto
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    title = "Situação de conclusão do Ensino Médio",
    subtitle = "Inscritos do ENEM 2024 em Alagoas",
    x = "Número de inscritos",
    y = NULL
  ) +
  tema_relatorio