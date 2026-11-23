library(shiny)

source("R/mod_inicio.R")
source("R/mod_menu.R")


ui <- fluidPage(
  
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "style.css"
    )
  ),
  
  div(
    class = "cabecalho",
    
    h1("ENEM em dados | Alagoas"),
    
    actionButton(
      "inicio_global",
      "Início",
      icon = icon("home")
    )
  ),
  
  tabsetPanel(
    
    id = "paginas",
    type = "hidden",
    
    tabPanel(
      title = "Início",
      value = "inicio",
      inicioUI("inicio")
    ),
    
    tabPanel(
      title = "Outras análises",
      value = "menu_analises",
      menuUI("menu")
    ),
    
    tabPanel(
      title = "Proporção",
      value = "proporcao",
      h2("Análise de proporção"),
      p("Conteúdo desta página será desenvolvido posteriormente.")
    ),
    
    tabPanel(
      title = "Desempenho",
      value = "desempenho",
      h2("Análise de desempenho"),
      p("Conteúdo desta página será desenvolvido posteriormente.")
    ),
    
    tabPanel(
      title = "Perfil",
      value = "perfil",
      h2("Perfil dos participantes"),
      p("Conteúdo desta página será desenvolvido posteriormente.")
    ),
    
    tabPanel(
      title = "Escolas",
      value = "escolas",
      h2("Análise das escolas"),
      p("Conteúdo desta página será desenvolvido posteriormente.")
    ),
    
    tabPanel(
      title = "Econômico",
      value = "economico",
      h2("Análise socioeconômica"),
      p("Conteúdo desta página será desenvolvido posteriormente.")
    )
    
  )
)


server <- function(input, output, session) {
  
  navegar <- function(pagina) {
    
    updateTabsetPanel(
      session,
      "paginas",
      selected = pagina
    )
    
  }
  
  inicioServer("inicio", navegar)
  
  menuServer("menu", navegar)
  
  observeEvent(input$inicio_global, {
    navegar("inicio")
  })
  
}


shinyApp(ui = ui, server = server)