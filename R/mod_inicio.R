inicioUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    div(
      class = "pagina-inicio",
      
      h2("Explore os dados do ENEM"),
      p("Uma visão geral dos resultados em Alagoas."),
      
      fluidRow(
        column(
          10,
          textInput(
            inputId = ns("municipio"),
            label = "Buscar município",
            placeholder = "Digite o nome do município..."
          )
        ),
        
        column(
          2,
          br(),
          actionButton(
            inputId = ns("buscar"),
            label = NULL,
            icon = icon("search")
          )
        )
      ),
      
      div(
        class = "mapa-placeholder",
        
        icon("map", "fa-3x"),
        
        h3("Mapa de Alagoas"),
        
        p("O mapa interativo será inserido aqui.")
      ),
      
      h3("Médias gerais do estado"),
      
      fluidRow(
        column(4, div(
          class = "cartao-indicador",
          p("Redação"),
          h3("--"),
          p("Média geral")
        )),
        
        column(4, div(
          class = "cartao-indicador",
          p("Matemática"),
          h3("--"),
          p("Média geral")
        )),
        
        column(4, div(
          class = "cartao-indicador",
          p("Linguagens"),
          h3("--"),
          p("Média geral")
        ))
      ),
      
      fluidRow(
        column(6, div(
          class = "cartao-indicador",
          p("Ciências Humanas"),
          h3("--"),
          p("Média geral")
        )),
        
        column(6, div(
          class = "cartao-indicador",
          p("Ciências da Natureza"),
          h3("--"),
          p("Média geral")
        ))
      ),
      
      h3("Explore as análises"),
      
      actionButton(
        ns("proporcao"),
        "Proporção",
        icon = icon("chart-pie")
      ),
      
      actionButton(
        ns("desempenho"),
        "Desempenho",
        icon = icon("chart-bar")
      ),
      
      actionButton(
        ns("outras"),
        "Outras análises",
        icon = icon("users")
      )
      
    )
  )
}


inicioServer <- function(id, navegar) {
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$buscar, {
      
      showNotification(
        "A busca será conectada aos dados posteriormente."
      )
      
    })
    
    
    observeEvent(input$proporcao, {
      navegar("proporcao")
    })
    
    observeEvent(input$desempenho, {
      navegar("desempenho")
    })
    
    observeEvent(input$outras, {
      navegar("menu_analises")
    })
    
  })
}