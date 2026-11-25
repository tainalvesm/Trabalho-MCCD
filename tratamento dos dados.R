library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)


#paleta
cor_principal  <- "#A6535D"
cor_secundaria <- "#668596"
cor_destaque   <- "#C98D91"
cor_areia      <- "#DCCDC0"
cor_texto      <- "#34383D"
cor_cinza      <- "#929292"
paleta_vermelha_5 <- colorRampPalette(c("#F2D9DC", cor_principal, "#5E2A31"))(5)
paleta_vermelha_4 <- colorRampPalette(c("#F2D9DC", cor_principal, "#5E2A31"))(4)

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

tema_empilhado <- tema_barras_verticais +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 9, color = cor_texto),
    legend.text = element_text(size = 8.5, color = cor_texto)
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
  mutate(
    estado_civil = case_when(
      TP_ESTADO_CIVIL == 0 ~ "Não informado",
      TP_ESTADO_CIVIL == 1 ~ "Solteiro(a)",
      TP_ESTADO_CIVIL == 2 ~ "Casado(a) / Mora com companheiro(a)",
      TP_ESTADO_CIVIL == 3 ~ "Divorciado(a) / Separado(a)",
      TP_ESTADO_CIVIL == 4 ~ "Viúvo(a)"
    )
  )

dados_participantes <- dados_participantes %>%
  mutate(
    renda_rotulo = case_when(
      renda_familiar_mensal == "A" ~ "Nenhuma renda",
      renda_familiar_mensal == "B" ~ "Até R$ 1.412,00",
      renda_familiar_mensal == "C" ~ "De R$ 1.412,01 até R$ 2.118,00",
      renda_familiar_mensal == "D" ~ "De R$ 2.118,01 até R$ 2.824,00",
      renda_familiar_mensal == "E" ~ "De R$ 2.824,01 até R$ 3.530,00",
      renda_familiar_mensal == "F" ~ "De R$ 3.530,01 até R$ 4.236,00",
      renda_familiar_mensal == "G" ~ "De R$ 4.236,01 até R$ 5.648,00",
      renda_familiar_mensal == "H" ~ "De R$ 5.648,01 até R$ 7.060,00",
      renda_familiar_mensal == "I" ~ "De R$ 7.060,01 até R$ 8.472,00",
      renda_familiar_mensal == "J" ~ "De R$ 8.472,01 até R$ 9.884,00",
      renda_familiar_mensal == "K" ~ "De R$ 9.884,01 até R$ 11.296,00",
      renda_familiar_mensal == "L" ~ "De R$ 11.296,01 até R$ 12.708,00",
      renda_familiar_mensal == "M" ~ "De R$ 12.708,01 até R$ 14.120,00",
      renda_familiar_mensal == "N" ~ "De R$ 14.120,01 até R$ 16.944,00",
      renda_familiar_mensal == "O" ~ "De R$ 16.944,01 até R$ 21.180,00",
      renda_familiar_mensal == "P" ~ "De R$ 21.180,01 até R$ 28.240,00",
      renda_familiar_mensal == "Q" ~ "Acima de R$ 28.240,00"
    ),
    renda_rotulo = factor(renda_rotulo, levels = unique(renda_rotulo[order(renda_familiar_mensal)]))
  )

dados_participantes <- dados_participantes %>%
  mutate(
    cor_raca = case_when(
      cor_raça == 0 ~ "Não declarado",
      cor_raça == 1 ~ "Branca",
      cor_raça == 2 ~ "Preta",
      cor_raça == 3 ~ "Parda",
      cor_raça == 4 ~ "Amarela",
      cor_raça == 5 ~ "Indígena",
      cor_raça == 6 ~ "Não dispõe da informação"
    )
  )

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

niveis_escolaridade <- c(
  "Nunca estudou",
  "Fundamental I incompleto",
  "Fundamental II incompleto",
  "Médio incompleto",
  "Médio completo",
  "Superior completo",
  "Pós-graduação completa",
  "Não sei"
)

rotular_escolaridade <- function(x) {
  factor(
    case_when(
      x == "A" ~ "Nunca estudou",
      x == "B" ~ "Fundamental I incompleto",
      x == "C" ~ "Fundamental II incompleto",
      x == "D" ~ "Médio incompleto",
      x == "E" ~ "Médio completo",
      x == "F" ~ "Superior completo",
      x == "G" ~ "Pós-graduação completa",
      x == "H" ~ "Não sei"
    ),
    levels = niveis_escolaridade
  )
}

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

dados_participantes <- dados_participantes %>%
  mutate(
    nacionalidade = case_when(
      TP_NACIONALIDADE == 0 ~ "Não informado",
      TP_NACIONALIDADE == 1 ~ "Brasileiro(a)",
      TP_NACIONALIDADE == 2 ~ "Brasileiro(a) Naturalizado(a)",
      TP_NACIONALIDADE == 3 ~ "Estrangeiro(a)",
      TP_NACIONALIDADE == 4 ~ "Brasileiro(a) Nato(a), nascido(a) no exterior"
    )
  )
dados_participantes <- dados_participantes %>%
  mutate(
    tipo_escola_rotulo = case_when(
      tipo_escola_freq_EM == "A" ~ "Somente escola pública",
      tipo_escola_freq_EM == "B" ~ "Parte pública / parte privada (sem bolsa)",
      tipo_escola_freq_EM == "C" ~ "Parte pública / parte privada (com bolsa)",
      tipo_escola_freq_EM == "D" ~ "Somente escola privada (sem bolsa)",
      tipo_escola_freq_EM == "E" ~ "Somente escola privada (com bolsa)",
      tipo_escola_freq_EM == "F" ~ "Não frequentou a escola"
    )
  )


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

#estado civil
tabela_estado_civil <- dados_participantes %>%
  count(estado_civil, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  arrange(desc(n))

grafico_estado_civil <- tabela_estado_civil %>%
  ggplot(aes(x = n, y = reorder(estado_civil, n))) +
  geom_col(fill = cor_principal, width = 0.7) +
  geom_text(
    aes(label = sprintf("%s (%.1f%%)", format(n, big.mark = ".", decimal.mark = ","), pct)),
    hjust = -0.15,
    size = 3.2,
    color = cor_texto
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    title = "Distribuição por estado civil",
    subtitle = "Inscritos do ENEM 2024 em Alagoas",
    x = "Número de inscritos",
    y = NULL
  ) +
  tema_relatorio

#nacionalidade
tabela_nacionalidade <- dados_participantes %>%
  count(nacionalidade, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 3)) %>%
  arrange(desc(n))

grafico_nacionalidade <- tabela_nacionalidade %>%
  ggplot(aes(x = n, y = reorder(nacionalidade, n))) +
  geom_col(fill = cor_principal, width = 0.7) +
  geom_text(
    aes(label = sprintf("%s (%.1f%%)", format(n, big.mark = ".", decimal.mark = ","), pct)),
    hjust = -0.15,
    size = 3.2,
    color = cor_texto
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    title = "Distribuição por nacionalidade",
    subtitle = "Inscritos do ENEM 2024 em Alagoas",
    x = "Número de inscritos",
    y = NULL
  ) +
  tema_relatorio

#renda
tabela_renda <- dados_participantes %>%
  count(renda_familiar_mensal, renda_rotulo, name = "n") %>%
  arrange(renda_familiar_mensal) %>%
  mutate(
    pct = round(100 * n / sum(n), 2),
    pct_acumulado = round(cumsum(pct), 2)
  )

grafico_renda <- tabela_renda %>%
  ggplot(aes(x = n, y = reorder(renda_rotulo, desc(renda_familiar_mensal)))) +
  geom_col(fill = cor_principal, width = 0.7) +
  geom_text(
    aes(label = sprintf("%s (%.1f%%)", format(n, big.mark = ".", decimal.mark = ","), pct)),
    hjust = -0.15,
    size = 3.2,
    color = cor_texto
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    title = "Distribuição por renda familiar mensal",
    subtitle = "Inscritos do ENEM 2024 em Alagoas",
    x = "Número de inscritos",
    y = NULL
  ) +
  tema_relatorio

##renda x moradores (com correlação de spearman)
mapa_renda_ord <- c(
  "A" = 1, "B" = 2, "C" = 3, "D" = 4, "E" = 5, "F" = 6,
  "G" = 7, "H" = 8, "I" = 9, "J" = 10, "K" = 11, "L" = 12,
  "M" = 13, "N" = 14, "O" = 15, "P" = 16, "Q" = 17
)

niveis_renda_grp <- c(
  "Sem renda",
  "Até 1,5 SM",
  "Mais de 1,5 a 3 SM",
  "Mais de 3 a 6 SM",
  "Mais de 6 SM"
)

niveis_moradores <- c(
  "1 morador",
  "2 moradores",
  "3 moradores",
  "4 moradores",
  "5 ou mais moradores"
)
dados_renda_moradores <- dados_participantes %>%
  mutate(
    renda_ord = mapa_renda_ord[as.character(renda_familiar_mensal)],
    moradores_num = suppressWarnings(as.numeric(as.character(qnt_moradores_casa))),
    renda_grp = case_when(
      renda_familiar_mensal == "A" ~ "Sem renda",
      renda_familiar_mensal %in% c("B", "C") ~ "Até 1,5 SM",
      renda_familiar_mensal %in% c("D", "E") ~ "Mais de 1,5 a 3 SM",
      renda_familiar_mensal %in% c("F", "G", "H") ~ "Mais de 3 a 6 SM",
      renda_familiar_mensal %in% c("I", "J", "K", "L", "M", "N", "O", "P", "Q") ~ "Mais de 6 SM"
    ),
    renda_grp = factor(renda_grp, levels = niveis_renda_grp),
    moradores_grp = case_when(
      moradores_num == 1 ~ "1 morador",
      moradores_num == 2 ~ "2 moradores",
      moradores_num == 3 ~ "3 moradores",
      moradores_num == 4 ~ "4 moradores",
      moradores_num >= 5 ~ "5 ou mais moradores"
    ),
    moradores_grp = factor(moradores_grp, levels = niveis_moradores)
  ) %>%
  filter(!is.na(renda_ord), !is.na(renda_grp), !is.na(moradores_grp))
teste_spearman <- cor.test(
  dados_renda_moradores$renda_ord,
  dados_renda_moradores$moradores_num,
  method = "spearman",
  exact = FALSE
)
tabela_composicao <- dados_renda_moradores %>%
  count(moradores_grp, renda_grp, name = "n") %>%
  group_by(moradores_grp) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

grafico_renda_moradores <- ggplot(tabela_composicao, aes(x = moradores_grp, y = pct, fill = renda_grp)) +
  geom_col(position = "fill", width = 0.65) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = paleta_vermelha_5) +
  labs(
    title = "Composição da renda familiar por quantidade de moradores",
    subtitle = "Inscritos do ENEM 2024 em Alagoas (faixas agrupadas em salários mínimos)",
    x = "Quantidade de moradores na casa",
    y = "Proporção dentro do grupo",
    fill = "Renda familiar"
  ) +
  tema_empilhado +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
#Com o teste de spearman foi detectada uma correlação desprezível (rho=0,057) entre renda familiar
#e quantidade de moradores com relevância estatística (p < 0,0001)

#tipo de escola no ensino medio
tabela_escola_geral <- dados_participantes %>%
  count(tipo_escola_rotulo, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  arrange(desc(n))

grafico_escola_geral <- tabela_escola_geral %>%
  ggplot(aes(x = n, y = reorder(tipo_escola_rotulo, n))) +
  geom_col(fill = cor_principal, width = 0.7) +
  geom_text(
    aes(label = sprintf("%s (%.1f%%)", format(n, big.mark = ".", decimal.mark = ","), pct)),
    hjust = -0.15,
    size = 3.2,
    color = cor_texto
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.28))) +
  labs(
    title = "Tipo de escola frequentada no Ensino Médio",
    subtitle = "Inscritos do ENEM 2024 em Alagoas",
    x = "Número de inscritos",
    y = NULL
  ) +
  tema_relatorio

#escolaridade pai/homem responsável
dados_participantes <- dados_participantes %>%
  mutate(
    pai_esc_rotulo = rotular_escolaridade(pai_escolaridade),
    mae_esc_rotulo = rotular_escolaridade(mae_escolaridade)
  )

# 2. Tabelas separadas no console
tabela_pai <- dados_participantes %>%
  count(pai_esc_rotulo, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 2))

tabela_mae <- dados_participantes %>%
  count(mae_esc_rotulo, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 2))

grafico_escolaridade_responsaveis <- dados_participantes %>%
  select(pai_esc_rotulo, mae_esc_rotulo) %>%
  pivot_longer(
    cols = everything(),
    names_to = "responsavel",
    values_to = "escolaridade"
  ) %>%
  mutate(
    responsavel = recode(
      responsavel,
      "pai_esc_rotulo" = "Pai / Responsável homem",
      "mae_esc_rotulo" = "Mãe / Responsável mulher"
    )
  ) %>%
  filter(!is.na(escolaridade)) %>%
  count(responsavel, escolaridade, name = "n") %>%
  group_by(responsavel) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = pct, y = escolaridade, fill = responsavel)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(
    aes(label = sprintf("%.1f%%", pct)),
    position = position_dodge(width = 0.75),
    hjust = -0.15,
    size = 3.2,
    color = cor_texto
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.16))) +
  scale_fill_manual(values = c(
    "Mãe / Responsável mulher" = cor_principal,
    "Pai / Responsável homem" = cor_secundaria
  )) +
  labs(
    title = "Escolaridade do pai e da mãe",
    subtitle = "Inscritos do ENEM 2024 em Alagoas",
    x = "Percentual de inscritos (%)",
    y = NULL,
    fill = NULL
  ) +
  tema_relatorio +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8.5, color = cor_texto)
  )

## CORRELAÇÃO EDUCAÇÃO MÃE X RENDA
dados_esc_renda <- dados_participantes %>%
  mutate(
    mae_ord = match(mae_escolaridade, c("A", "B", "C", "D", "E", "F", "G")),
    renda_ord = match(renda_familiar_mensal, LETTERS[1:17]),
    mae_grp = case_when(
      mae_escolaridade %in% c("A", "B", "C") ~ "Até Fund. I",
      mae_escolaridade == "D" ~ "Fund. II completo",
      mae_escolaridade == "E" ~ "Médio completo",
      mae_escolaridade %in% c("F", "G") ~ "Superior / Pós",
      mae_escolaridade == "H" ~ "Não sei"
    ),
    mae_grp = factor(mae_grp, levels = c("Até Fund. I", "Fund. II completo", "Médio completo", "Superior / Pós", "Não sei")),
    renda_grp = case_when(
      renda_familiar_mensal == "A" ~ "Sem renda",
      renda_familiar_mensal %in% c("B", "C") ~ "Até 1,5 SM",
      renda_familiar_mensal %in% c("D", "E") ~ "Mais de 1,5 a 3 SM",
      renda_familiar_mensal %in% c("F", "G", "H") ~ "Mais de 3 a 6 SM",
      renda_familiar_mensal %in% LETTERS[9:17] ~ "Mais de 6 SM"
    ),
    renda_grp = factor(renda_grp, levels = c("Sem renda", "Até 1,5 SM", "Mais de 1,5 a 3 SM", "Mais de 3 a 6 SM", "Mais de 6 SM"))
  )
validos_spearman <- dados_esc_renda %>% filter(!is.na(mae_ord), !is.na(renda_ord))
cor_mae_renda <- cor.test(validos_spearman$mae_ord, validos_spearman$renda_ord, method = "spearman", exact = FALSE)

grafico_mae_renda <- dados_esc_renda %>%
  filter(!is.na(mae_grp), !is.na(renda_grp)) %>%
  count(mae_grp, renda_grp, name = "n") %>%
  group_by(mae_grp) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = mae_grp, y = pct, fill = renda_grp)) +
  geom_col(position = "fill", width = 0.65) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = paleta_vermelha_5) +
  labs(
    title = "Renda familiar mensal segundo a escolaridade da mãe",
    subtitle = "Inscritos do ENEM 2024 em Alagoas",
    x = "Escolaridade da mãe / responsável mulher",
    y = "Proporção dentro do grupo",
    fill = "Renda familiar"
  ) +
  tema_empilhado +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

#CORRELAÇÃO ESCOLARIDADE PAI X RENDA
#df* = min(r-1,c-1) = 5 - ASSOCIAÇÃO MODERADA/FORTE
validos_pai <- dados_participantes %>%
  mutate(
    pai_ord = match(pai_escolaridade, c("A", "B", "C", "D", "E", "F", "G")),
    renda_ord = match(renda_familiar_mensal, LETTERS[1:17])
  ) %>%
  filter(!is.na(pai_ord), !is.na(renda_ord))

cor.test(validos_pai$pai_ord, validos_pai$renda_ord, method = "spearman", exact = FALSE)

#Escolaridade mãe x tipo de escola
grafico_mae_escola <- dados_participantes %>%
  mutate(
    mae_grp = case_when(
      mae_escolaridade %in% c("A", "B", "C") ~ "Até Fund. I",
      mae_escolaridade == "D" ~ "Fund. II completo",
      mae_escolaridade == "E" ~ "Médio completo",
      mae_escolaridade %in% c("F", "G") ~ "Superior / Pós",
      mae_escolaridade == "H" ~ "Não sei"
    ),
    mae_grp = factor(mae_grp, levels = c("Até Fund. I", "Fund. II completo", "Médio completo", "Superior / Pós", "Não sei")),
    escola_grp = case_when(
      tipo_escola_freq_EM == "A" ~ "Somente pública",
      tipo_escola_freq_EM %in% c("B", "C") ~ "Mista (pública/privada)",
      tipo_escola_freq_EM %in% c("D", "E") ~ "Somente privada",
      tipo_escola_freq_EM == "F" ~ "Não frequentou"
    ),
    escola_grp = factor(escola_grp, levels = c("Somente pública", "Mista (pública/privada)", "Somente privada", "Não frequentou"))
  ) %>%
  filter(!is.na(mae_grp), !is.na(escola_grp)) %>%
  count(mae_grp, escola_grp, name = "n") %>%
  group_by(mae_grp) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = mae_grp, y = pct, fill = escola_grp)) +
  geom_col(position = "fill", width = 0.65) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = paleta_vermelha_4) +
  labs(
    title = "Tipo de escola no Ensino Médio segundo a escolaridade da mãe",
    subtitle = "Inscritos do ENEM 2024 em Alagoas (V de Cramér = 0,2223)",
    x = "Escolaridade da mãe / responsável mulher",
    y = "Proporção dentro do grupo",
    fill = "Tipo de escola"
  ) +
  tema_empilhado +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

#PAI ocupação
dados_participantes <- dados_participantes %>%
  mutate(
    pai_ocup_rotulo = factor(
      case_when(
        ocupacao_pai == "A" ~ "Grupo 1: Agropecuária / Extrativismo",
        ocupacao_pai == "B" ~ "Grupo 2: Serviços gerais / Comércio / Doméstico",
        ocupacao_pai == "C" ~ "Grupo 3: Indústria / Construção / Transporte",
        ocupacao_pai == "D" ~ "Grupo 4: Técnico / Professor / Autônomo / Microemp.",
        ocupacao_pai == "E" ~ "Grupo 5: Nível superior / Direção / Alta patente",
        ocupacao_pai == "F" ~ "Não sei"
      ),
      levels = c(
        "Grupo 1: Agropecuária / Extrativismo",
        "Grupo 2: Serviços gerais / Comércio / Doméstico",
        "Grupo 3: Indústria / Construção / Transporte",
        "Grupo 4: Técnico / Professor / Autônomo / Microemp.",
        "Grupo 5: Nível superior / Direção / Alta patente",
        "Não sei"
      )
    )
  )
tabela_pai_ocup <- dados_participantes %>%
  count(ocupacao_pai, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 2))

grafico_pai_ocup <- tabela_pai_ocup %>%
  ggplot(aes(x = n, y = reorder(ocupacao_pai, n))) +
  geom_col(fill = cor_principal, width = 0.7) +
  geom_text(
    aes(label = sprintf("%s (%.1f%%)", format(n, big.mark = ".", decimal.mark = ","), pct)),
    hjust = -0.15,
    size = 3.2,
    color = cor_texto
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.28))) +
  labs(
    title = "Ocupação do pai / responsável homem",
    subtitle = "Inscritos do ENEM 2024 em Alagoas",
    x = "Número de inscritos",
    y = NULL
  ) +
  tema_relatorio


tab_ocup_renda <- table(dados_participantes$ocupacao_pai, dados_participantes$renda_familiar_mensal)
chi2_or <- chisq.test(tab_ocup_renda)$statistic
v_cramer_or <- sqrt(chi2_or / (sum(tab_ocup_renda) * (min(dim(tab_ocup_renda)) - 1)))
cat(sprintf("V de Cramér (Ocupação do pai x Renda familiar): %.4f\n", v_cramer_or))
#V de Cramer = 0,32 para um df = 5 indica associação forte entre ocupação do pai e renda familiar

#ocupação mãe
#V de Cramer = 0,31, mesmo df, associação forte entre ocupação da mãe e renda familiar
tabela_mae_ocup <- dados_participantes %>%
  count(ocupacao_mae, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 2))

grafico_mae_ocup <- tabela_mae_ocup %>%
  ggplot(aes(x = n, y = reorder(ocupacao_mae, n))) +
  geom_col(fill = cor_principal, width = 0.7) +
  geom_text(
    aes(label = sprintf("%s (%.1f%%)", format(n, big.mark = ".", decimal.mark = ","), pct)),
    hjust = -0.15,
    size = 3.2,
    color = cor_texto
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.28))) +
  labs(
    title = "Ocupação da mãe / responsável mulher",
    subtitle = "Inscritos do ENEM 2024 em Alagoas",
    x = "Número de inscritos",
    y = NULL
  ) +
  tema_relatorio

tab_mae_renda <- table(dados_participantes$ocupacao_mae, dados_participantes$renda_familiar_mensal)
v_cramer_mae <- sqrt(chisq.test(tab_mae_renda)$statistic / (sum(tab_mae_renda) * (min(dim(tab_mae_renda)) - 1)))
cat(sprintf("V de Cramér (Ocupação da mãe x Renda familiar): %.4f\n", v_cramer_mae))
#"Grupo 1: Agropecuária / Extrativismo",
#"Grupo 2: Serviços gerais / Comércio / Doméstico",
#"Grupo 3: Indústria / Construção / Transporte",
#"Grupo 4: Técnico / Professor / Autônomo / Microemp.",
#"Grupo 5: Nível superior / Direção / Alta patente"

