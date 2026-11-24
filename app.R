
library(shiny)
library(dplyr)
library(ggplot2)

source("R/dados.R")
source("R/mod_inicio.R")
source("R/mod_perfil.R")
source("R/mod_escolas.R")
source("R/mod_desempenho.R")
source("R/mod_economico.R")
source("R/mod_municipios.R")

bases <- carregar_dados()

municipios <- sort(unique(na.omit(bases$participantes$municipio_prova)))

ui <- fluidPage(
  
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    )
  ),
  
  div(
    class = "topbar",
    div(
      class = "brand",
      div(class = "brand-mark", "ENEM"),
      div(
        h2("ENEM em Alagoas"),
        span("Painel de exploração • Edição 2024")
      )
    ),
    div(class = "topbar-tag", "MICRODADOS 2024")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      div(
        class = "sidebar-custom",
        
        span(class = "eyebrow", "EXPLORAR DADOS"),
        
        selectizeInput(
          inputId = "municipio_global",
          label = "Buscar município",
          choices = c("Todos os municípios", municipios),
          selected = "Todos os municípios",
          options = list(
            placeholder = "Digite o município...",
            maxOptions = 300
          )
        ),
        
        div(class = "sidebar-divider"),
        
        span(class = "eyebrow", "NAVEGAÇÃO"),
        
        actionButton(
          "nav_inicio",
          "Visão geral",
          icon = icon("home"),
          class = "btn-nav"
        ),
        
        actionButton(
          "nav_perfil",
          "Perfil dos participantes",
          icon = icon("users"),
          class = "btn-nav"
        ),
        
        actionButton(
          "nav_escolas",
          "Escolas",
          icon = icon("graduation-cap"),
          class = "btn-nav"
        ),
        
        actionButton(
          "nav_desempenho",
          "Desempenho",
          icon = icon("bar-chart"),
          class = "btn-nav"
        ),
        
        actionButton(
          "nav_economico",
          "Contexto familiar e econômico",
          icon = icon("home"),
          class = "btn-nav"
        ),
        
        actionButton(
          "nav_municipios",
          "Análise municipal",
          icon = icon("map"),
          class = "btn-nav"
        ),
        
        div(
          class = "sidebar-foot",
          "Projeto acadêmico • ENEM 2024"
        )
      )
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "paginas",
        type = "hidden",
        
        tabPanel(
          title = "Visão geral",
          value = "inicio",
          inicioUI("inicio")
        ),
        
        tabPanel(
          title = "Perfil",
          value = "perfil",
          perfilUI("perfil")
        ),
        
        tabPanel(
          title = "Escolas",
          value = "escolas",
          escolasUI("escolas")
        ),
        
        tabPanel(
          title = "Desempenho",
          value = "desempenho",
          desempenhoUI("desempenho")
        ),
        
        tabPanel(
          title = "Econômico",
          value = "economico",
          economicoUI("economico")
        ),
        
        tabPanel(
          title = "Municípios",
          value = "municipios",
          municipiosUI("municipios")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  participantes_filtrados <- reactive({
    filtrar_municipio(
      bases$participantes,
      input$municipio_global
    )
  })
  
  resultados_filtrados <- reactive({
    filtrar_municipio(
      bases$resultados,
      input$municipio_global
    )
  })
  
  observeEvent(input$nav_inicio, {
    updateTabsetPanel(session, "paginas", selected = "inicio")
  })
  
  observeEvent(input$nav_perfil, {
    updateTabsetPanel(session, "paginas", selected = "perfil")
  })
  
  observeEvent(input$nav_escolas, {
    updateTabsetPanel(session, "paginas", selected = "escolas")
  })
  
  observeEvent(input$nav_desempenho, {
    updateTabsetPanel(session, "paginas", selected = "desempenho")
  })
  
  observeEvent(input$nav_economico, {
    updateTabsetPanel(session, "paginas", selected = "economico")
  })
  
  observeEvent(input$nav_municipios, {
    updateTabsetPanel(session, "paginas", selected = "municipios")
  })
  
  inicioServer(
    "inicio",
    participantes = participantes_filtrados,
    resultados = resultados_filtrados,
    municipio = reactive(input$municipio_global)
  )
  
  perfilServer(
    "perfil",
    participantes = participantes_filtrados
  )
  
  escolasServer(
    "escolas",
    resultados = resultados_filtrados
  )
  
  desempenhoServer(
    "desempenho",
    resultados = resultados_filtrados
  )
  
  economicoServer(
    "economico",
    participantes = participantes_filtrados
  )
  municipiosServer(
    "municipios",
    participantes = reactive(bases$participantes),
    resultados = reactive(bases$resultados),
    municipio = reactive(input$municipio_global)
  )
}

shinyApp(ui = ui, server = server)

