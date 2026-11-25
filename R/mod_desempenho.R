
desempenhoUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    div(
      class = "page-heading",
      span(class = "eyebrow", "RESULTADOS DO EXAME"),
      h1("Desempenho"),
      p("Explore as médias das áreas de conhecimento e da redação.")
    ),
    
    uiOutput(ns("medias")),
    
    tabsetPanel(
      type = "pills",
      tabPanel(
      "Por rede de ensino",
      fluidRow(
        column(12, div(class = "content-card chart-card",
          plotOutput(ns("desempenho_escola"), height = "420px")))
      )
      ),
      tabPanel(
      "Presença",
      fluidRow(
        column(12, div(class = "content-card chart-card",
          plotOutput(ns("presenca"), height = "400px")))
      )
      )
    )
  )
}


desempenhoServer <- function(id, resultados) {
  
  moduleServer(id, function(input, output, session) {
    
    notas <- c(
      "NU_NOTA_CN",
      "NU_NOTA_CH",
      "NU_NOTA_LC",
      "NU_NOTA_MT",
      "NU_NOTA_REDACAO"
    )
    
    nomes_notas <- c(
      NU_NOTA_CN = "Ciências da Natureza",
      NU_NOTA_CH = "Ciências Humanas",
      NU_NOTA_LC = "Linguagens",
      NU_NOTA_MT = "Matemática",
      NU_NOTA_REDACAO = "Redação"
    )
    
    
    output$medias <- renderUI({
      
      dados <- resultados()
      
      medias <- sapply(
        notas,
        function(coluna) {
          if (coluna %in% names(dados)) {
            mean(dados[[coluna]], na.rm = TRUE)
          } else {
            NA_real_
          }
        }
      )
      
      formatar <- function(x) {
        
        if (is.na(x) || is.nan(x) || is.infinite(x)) {
          return("—")
        }
        
        format(
          round(x, 1),
          decimal.mark = ",",
          nsmall = 1
        )
      }
      
      div(
        class = "metric-grid",
        
        lapply(seq_along(notas), function(i) {
          
          div(
            class = "metric-card",
            span(nomes_notas[[notas[i]]]),
            strong(formatar(medias[i])),
            tags$small("Nota média")
          )
        })
      )
    })
    
    
    output$desempenho_escola <- renderPlot({
      
      dados <- resultados()
      
      colunas <- c(
        "NU_NOTA_CN",
        "NU_NOTA_CH",
        "NU_NOTA_LC",
        "NU_NOTA_MT",
        "NU_NOTA_REDACAO",
        "dependencia_adminstrativa"
      )
      
      validate(
        need(
          all(colunas %in% names(dados)),
          "Não foram encontradas todas as variáveis necessárias para esta comparação."
        )
      )
      
      tabela <- dados %>%
        filter(!is.na(dependencia_adminstrativa)) %>%
        group_by(dependencia_adminstrativa) %>%
        summarise(
          CN = mean(NU_NOTA_CN, na.rm = TRUE),
          CH = mean(NU_NOTA_CH, na.rm = TRUE),
          LC = mean(NU_NOTA_LC, na.rm = TRUE),
          MT = mean(NU_NOTA_MT, na.rm = TRUE),
          Redação = mean(NU_NOTA_REDACAO, na.rm = TRUE),
          .groups = "drop"
        )
      
      tabela_longa <- tabela %>%
        tidyr::pivot_longer(
          cols = c(CN, CH, LC, MT, Redação),
          names_to = "area",
          values_to = "media"
        )
      
      ggplot(
        tabela_longa,
        aes(
          x = area,
          y = media,
          fill = dependencia_adminstrativa
        )
      ) +
        geom_col(
          position = "dodge",
          width = 0.7
        ) +
        scale_fill_manual(
          values = c(
            "Federal" = cor_principal,
            "Estadual" = cor_secundaria,
            "Municipal" = cor_destaque,
            "Privada" = cor_areia
          )
        ) +
        labs(
          title = "Desempenho médio por dependência administrativa",
          subtitle = "Média das notas por área do conhecimento",
          x = NULL,
          y = "Nota média",
          fill = "Dependência"
        ) +
        tema_relatorio +
        theme(
          legend.position = "bottom"
        )
    })
    
    
    output$presenca <- renderPlot({
      
      dados <- resultados()
      
      colunas_presenca <- c(
        "TP_PRESENCA_CN",
        "TP_PRESENCA_CH",
        "TP_PRESENCA_LC",
        "TP_PRESENCA_MT"
      )
      
      validate(
        need(
          all(colunas_presenca %in% names(dados)),
          "As variáveis de presença não foram encontradas nesta versão da base."
        )
      )
      
      tabela <- bind_rows(
        lapply(colunas_presenca, function(coluna) {
          
          dados %>%
            count(
              situacao = .data[[coluna]],
              name = "n"
            ) %>%
            mutate(area = coluna)
        })
      ) %>%
        mutate(
          area = recode(
            area,
            TP_PRESENCA_CN = "Natureza",
            TP_PRESENCA_CH = "Humanas",
            TP_PRESENCA_LC = "Linguagens",
            TP_PRESENCA_MT = "Matemática"
          ),
          situacao = case_when(
            situacao == 0 ~ "Ausente",
            situacao == 1 ~ "Presente",
            situacao == 2 ~ "Eliminado",
            TRUE ~ "Não informado"
          )
        )
      
      ggplot(
        tabela,
        aes(
          x = area,
          y = n,
          fill = situacao
        )
      ) +
        geom_col(position = "fill") +
        scale_y_continuous(
          labels = scales::percent
        ) +
        scale_fill_manual(
          values = c(
            "Presente" = cor_secundaria,
            "Ausente" = cor_destaque,
            "Eliminado" = cor_principal,
            "Não informado" = cor_areia
          )
        ) +
        labs(
          title = "Situação de presença por área",
          subtitle = "Proporção de participantes em cada prova",
          x = NULL,
          y = "Proporção",
          fill = "Situação"
        ) +
        tema_relatorio +
        theme(
          legend.position = "bottom"
        )
    })
  })
}

