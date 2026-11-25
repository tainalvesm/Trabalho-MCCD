
perfilUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    div(
      class = "page-heading",
      span(class = "eyebrow", "DIMENSÃO DEMOGRÁFICA"),
      h1("Perfil dos participantes"),
      p("Quem são os inscritos no ENEM 2024 em Alagoas?")
    ),
    
    tabsetPanel(
      type = "pills",
      tabPanel(
      "Sexo e raça/cor",
      fluidRow(
        column(6, div(class = "content-card chart-card",
          plotOutput(ns("sexo"), height = "360px"))),
        column(6, div(class = "content-card chart-card",
          plotOutput(ns("raca"), height = "360px")))
      )
      ),
      tabPanel(
      "Faixa etária",
      fluidRow(
        column(12, div(class = "content-card chart-card",
          plotOutput(ns("faixa_etaria"), height = "520px")))
      )
      ),
      tabPanel(
      "Escolaridade",
      fluidRow(
        column(12, div(class = "content-card chart-card",
          plotOutput(ns("conclusao"), height = "340px")))
      ),
      fluidRow(
        column(12, div(class = "content-card chart-card",
          plotOutput(ns("treineiros"), height = "320px")))
      )
      )
    )
  )
}


perfilServer <- function(id, participantes) {
  
  moduleServer(id, function(input, output, session) {
    
    output$sexo <- renderPlot({
      
      dados <- participantes()
      
      tabela <- dados %>%
        filter(!is.na(sexo)) %>%
        count(sexo, name = "n") %>%
        mutate(
          percentual = 100 * n / sum(n),
          rotulo = paste0(
            format(n, big.mark = "."),
            " (",
            round(percentual, 1),
            "%)"
          )
        )
      
      ggplot(
        tabela,
        aes(x = sexo, y = n, fill = sexo)
      ) +
        geom_col(width = 0.5) +
        geom_text(
          aes(label = rotulo),
          vjust = -0.5,
          size = 3.5,
          color = cor_texto
        ) +
        scale_x_discrete(
          labels = c(
            "F" = "Feminino",
            "M" = "Masculino"
          )
        ) +
        scale_fill_manual(
          values = c(
            "F" = cor_principal,
            "M" = cor_secundaria
          )
        ) +
        scale_y_continuous(
          expand = expansion(mult = c(0, 0.17))
        ) +
        labs(
          title = "Distribuição por sexo",
          subtitle = "Inscritos no ENEM 2024",
          x = NULL,
          y = "Número de inscritos"
        ) +
        tema_relatorio
    })
    
    
    output$treineiros <- renderPlot({
      
      dados <- participantes()
      
      ggplot(
        dados %>%
          filter(!is.na(treineiros)) %>%
          count(treineiros) %>%
          mutate(
            percentual = 100 * n / sum(n),
            rotulo = paste0(
              format(n, big.mark = "."),
              " (",
              round(percentual, 1),
              "%)"
            )
          ),
        aes(x = treineiros, y = n, fill = treineiros)
      ) +
        geom_col(width = 0.5) +
        geom_text(
          aes(label = rotulo),
          vjust = -0.5,
          size = 3.3,
          color = cor_texto
        ) +
        scale_fill_manual(
          values = c(
            "Não treineiro" = cor_secundaria,
            "Treineiro" = cor_destaque
          )
        ) +
        scale_y_continuous(
          expand = expansion(mult = c(0, 0.17))
        ) +
        labs(
          title = "Condição de treineiro",
          subtitle = "Participantes que realizam o exame como treino",
          x = NULL,
          y = "Número de inscritos"
        ) +
        tema_relatorio
    })
    
    
    output$raca <- renderPlot({
      
      dados <- participantes()
      
      grafico_contagem(
        dados,
        "cor_raca",
        "Distribuição por raça/cor",
        "Autodeclaração dos participantes",
        cor_principal
      )
    })
    
    
    output$faixa_etaria <- renderPlot({
      
      dados <- participantes()
      
      ordem <- c(
        "Menor de 17 anos",
        "17 anos",
        "18 anos",
        "19 anos",
        "20 anos",
        "21 anos",
        "22 anos",
        "23 anos",
        "24 anos",
        "25 anos",
        "Entre 26 e 30 anos",
        "Entre 31 e 35 anos",
        "Entre 36 e 40 anos",
        "Entre 41 e 45 anos",
        "Entre 46 e 50 anos",
        "Entre 51 e 55 anos",
        "Entre 56 e 60 anos",
        "Entre 61 e 65 anos",
        "Entre 66 e 70 anos",
        "Maior de 70 anos"
      )
      
      tabela <- dados %>%
        filter(!is.na(faixa_etaria)) %>%
        count(faixa_etaria, name = "n") %>%
        mutate(
          faixa_etaria = factor(
            faixa_etaria,
            levels = ordem
          ),
          percentual = 100 * n / sum(n),
          rotulo = paste0(
            format(n, big.mark = "."),
            " (",
            round(percentual, 1),
            "%)"
          )
        )
      
      ggplot(
        tabela,
        aes(x = n, y = faixa_etaria)
      ) +
        geom_col(fill = cor_secundaria, width = 0.7) +
        geom_text(
          aes(label = rotulo),
          hjust = -0.12,
          size = 2.8,
          color = cor_texto
        ) +
        scale_x_continuous(
          expand = expansion(mult = c(0, 0.3))
        ) +
        labs(
          title = "Distribuição por faixa etária",
          subtitle = "Idade dos participantes",
          x = "Número de inscritos",
          y = NULL
        ) +
        tema_relatorio
    })
    
    
    output$conclusao <- renderPlot({
      
      dados <- participantes()
      
      grafico_contagem(
        dados,
        "conclusao_em",
        "Situação de conclusão do Ensino Médio",
        "Etapa de escolaridade dos participantes",
        cor_destaque
      )
    })
  })
}
