
economicoUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    div(
      class = "page-heading",
      span(class = "eyebrow", "QUESTIONÁRIO SOCIOECONÔMICO"),
      h1("Contexto familiar e econômico"),
      p("Características familiares, condições domiciliares e acesso à tecnologia.")
    ),
    
    h2(class = "section-title", "Escolaridade e ocupação dos responsáveis"),
    
    fluidRow(
      column(
        6,
        div(
          class = "content-card chart-card",
          plotOutput(ns("escolaridade_pai"), height = "320px")
        )
      ),
      
      column(
        6,
        div(
          class = "content-card chart-card",
          plotOutput(ns("escolaridade_mae"), height = "320px")
        )
      )
    ),
    
    fluidRow(
      column(
        6,
        div(
          class = "content-card chart-card",
          plotOutput(ns("ocupacao_pai"), height = "320px")
        )
      ),
      
      column(
        6,
        div(
          class = "content-card chart-card",
          plotOutput(ns("ocupacao_mae"), height = "320px")
        )
      )
    ),
    
    h2(class = "section-title", "Composição familiar"),
    
    fluidRow(
      column(
        12,
        div(
          class = "content-card chart-card",
          plotOutput(ns("moradores"), height = "320px")
        )
      )
    ),
    
    h2(class = "section-title", "Bens e acesso à tecnologia"),
    
    fluidRow(
      column(
        6,
        div(
          class = "content-card chart-card",
          plotOutput(ns("internet"), height = "300px")
        )
      ),
      
      column(
        6,
        div(
          class = "content-card chart-card",
          plotOutput(ns("computador"), height = "300px")
        )
      )
    ),
    
    fluidRow(
      column(
        6,
        div(
          class = "content-card chart-card",
          plotOutput(ns("celular"), height = "300px")
        )
      ),
      
      column(
        6,
        div(
          class = "content-card chart-card",
          plotOutput(ns("bens"), height = "300px")
        )
      )
    ),
    
    div(
      class = "data-note",
      "As alternativas do questionário são apresentadas conforme os códigos presentes na base. A interpretação textual depende da conferência do dicionário oficial do ENEM."
    )
  )
}


economicoServer <- function(id, participantes) {
  
  moduleServer(id, function(input, output, session) {
    
    output$escolaridade_pai <- renderPlot({
      
      grafico_contagem(
        participantes(),
        "pai_escolaridade",
        "Escolaridade do pai ou responsável",
        "Categorias registradas no questionário",
        cor_principal
      )
    })
    
    
    output$escolaridade_mae <- renderPlot({
      
      grafico_contagem(
        participantes(),
        "mae_escolaridade",
        "Escolaridade da mãe ou responsável",
        "Categorias registradas no questionário",
        cor_secundaria
      )
    })
    
    
    output$ocupacao_pai <- renderPlot({
      
      grafico_contagem(
        participantes(),
        "ocupacao_pai",
        "Ocupação do pai ou responsável",
        "Grupos ocupacionais do questionário",
        cor_destaque
      )
    })
    
    
    output$ocupacao_mae <- renderPlot({
      
      grafico_contagem(
        participantes(),
        "ocupacao_mae",
        "Ocupação da mãe ou responsável",
        "Grupos ocupacionais do questionário",
        cor_secundaria
      )
    })
    
    
    output$moradores <- renderPlot({
      
      grafico_contagem(
        participantes(),
        "qnt_moradores_casa",
        "Pessoas que moram na residência",
        "Quantidade de moradores informada",
        cor_principal
      )
    })
    
    
    output$internet <- renderPlot({
      
      grafico_contagem(
        participantes(),
        "casa_internet_wifi",
        "Acesso à internet ou Wi-Fi",
        "Respostas registradas no questionário",
        cor_secundaria
      )
    })
    
    
    output$computador <- renderPlot({
      
      grafico_contagem(
        participantes(),
        "casa_comp_notebook",
        "Computador ou notebook na residência",
        "Respostas registradas no questionário",
        cor_destaque
      )
    })
    
    
    output$celular <- renderPlot({
      
      grafico_contagem(
        participantes(),
        "tem_telefone_celular",
        "Telefone celular na residência",
        "Respostas registradas no questionário",
        cor_principal
      )
    })
    
    
    output$bens <- renderPlot({
      
      dados <- participantes()
      
      dados_bens <- bind_rows(
        lapply(
          c("tem_carro", "tem_motocicleta"),
          function(coluna) {
            dados %>%
              count(
                resposta = .data[[coluna]],
                name = "n"
              ) %>%
              mutate(
                bem = recode(
                  coluna,
                  tem_carro = "Carro",
                  tem_motocicleta = "Motocicleta"
                )
              )
          }
        )
      )
      
      ggplot(
        dados_bens,
        aes(
          x = resposta,
          y = n,
          fill = bem
        )
      ) +
        geom_col(position = "dodge") +
        scale_fill_manual(
          values = c(
            "Carro" = cor_principal,
            "Motocicleta" = cor_secundaria
          )
        ) +
        labs(
          title = "Carro e motocicleta na residência",
          subtitle = "Distribuição das respostas do questionário",
          x = "Código da resposta",
          y = "Número de participantes",
          fill = "Bem"
        ) +
        tema_relatorio +
        theme(
          legend.position = "bottom"
        )
    })
  })
}

