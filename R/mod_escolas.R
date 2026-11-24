
escolasUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    div(
      class = "page-heading",
      span(class = "eyebrow", "DIMENSÃO EDUCACIONAL"),
      h1("Escolas"),
      p("Características das escolas relacionadas aos participantes do ENEM.")
    ),
    
    fluidRow(
      column(
        6,
        div(
          class = "content-card chart-card",
          plotOutput(ns("dependencia"), height = "330px")
        )
      ),
      
      column(
        6,
        div(
          class = "content-card chart-card",
          plotOutput(ns("localizacao"), height = "330px")
        )
      )
    ),
    
    fluidRow(
      column(
        12,
        div(
          class = "content-card chart-card",
          plotOutput(ns("situacao"), height = "330px")
        )
      )
    )
  )
}


escolasServer <- function(id, resultados) {
  
  moduleServer(id, function(input, output, session) {
    
    output$dependencia <- renderPlot({
      
      grafico_contagem(
        resultados(),
        "dependencia_adminstrativa",
        "Participantes por dependência administrativa",
        "Tipo de administração da escola",
        cor_principal
      )
    })
    
    
    output$localizacao <- renderPlot({
      
      grafico_contagem(
        resultados(),
        "localizacao",
        "Localização das escolas",
        "Área urbana ou rural",
        cor_secundaria
      )
    })
    
    
    output$situacao <- renderPlot({
      
      grafico_contagem(
        resultados(),
        "situacao_funcionamento",
        "Situação de funcionamento das escolas",
        "Situação registrada na base de resultados",
        cor_destaque
      )
    })
  })
}

