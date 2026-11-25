
economicoUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    div(
      class = "page-heading",
      span(class = "eyebrow", "QUESTIONÁRIO SOCIOECONÔMICO"),
      h1("Contexto familiar e econômico"),
      p("Características familiares, condições domiciliares e acesso à tecnologia.")
    ),
    
    tabsetPanel(
      type = "pills",
      tabPanel(
      "Responsáveis",
      fluidRow(
        column(12, div(class = "content-card chart-card",
          plotOutput(ns("escolaridade"), height = "380px")))
      ),
      fluidRow(
        column(12, div(class = "content-card chart-card",
          plotOutput(ns("ocupacao"), height = "360px")))
      )
      ),
      tabPanel(
      "Domicílio e renda",
      fluidRow(
        column(6, div(class = "content-card chart-card",
          plotOutput(ns("moradores"), height = "330px"))),
        column(6, div(class = "content-card chart-card",
          plotOutput(ns("renda"), height = "330px")))
      )
      ),
      tabPanel(
      "Tecnologia",
      fluidRow(
        column(6, div(class = "content-card chart-card",
          plotOutput(ns("internet"), height = "330px"))),
        column(6, div(class = "content-card chart-card",
          plotOutput(ns("computador"), height = "330px")))
      ),
      fluidRow(
        column(12, div(class = "content-card chart-card",
          plotOutput(ns("celular"), height = "330px")))
      )
      ),
      tabPanel(
      "Transporte",
      fluidRow(
        column(12, div(class = "content-card chart-card",
          plotOutput(ns("bens"), height = "360px")))
      )
      )
    ),
    
    div(
      class = "data-note",
      "Rótulos conforme o dicionário dos microdados do ENEM 2024. SM = salário mínimo de R$ 1.412,00. As faixas de renda foram agrupadas."
    )
  )
}


economicoServer <- function(id, participantes) {
  
  moduleServer(id, function(input, output, session) {
    
    output$escolaridade <- renderPlot({
      grafico_comparativo(
        participantes(),
        c("Pai" = "pai_escolaridade", "Mãe" = "mae_escolaridade"),
        "Escolaridade dos responsáveis"
      )
    })
    
    output$ocupacao <- renderPlot({
      grafico_comparativo(
        participantes(),
        c("Pai" = "ocupacao_pai", "Mãe" = "ocupacao_mae"),
        "Grupo ocupacional dos responsáveis"
      )
    })
    
    output$moradores <- renderPlot({
      grafico_contagem(
        participantes(), "qnt_moradores_casa",
        "Pessoas na residência", "Incluindo o participante", cor_principal
      )
    })
    
    output$renda <- renderPlot({
      grafico_contagem(
        participantes(), "renda_familiar_mensal",
        "Renda familiar mensal", "Em salários mínimos", cor_secundaria
      )
    })
    
    output$internet <- renderPlot({
      grafico_contagem(
        participantes(), "casa_internet_wifi",
        "Internet Wi-Fi em casa", "Respostas do questionário", cor_secundaria
      )
    })
    
    output$computador <- renderPlot({
      grafico_contagem(
        participantes(), "casa_comp_notebook",
        "Computadores em casa", "Quantidade informada", cor_destaque
      )
    })
    
    output$celular <- renderPlot({
      grafico_contagem(
        participantes(), "tem_telefone_celular",
        "Celulares entre os moradores", "Quantidade informada", cor_principal
      )
    })
    
    output$bens <- renderPlot({
      grafico_comparativo(
        participantes(),
        c("Carro" = "tem_carro", "Motocicleta" = "tem_motocicleta"),
        "Carro e motocicleta entre os moradores"
      )
    })
  })
}
