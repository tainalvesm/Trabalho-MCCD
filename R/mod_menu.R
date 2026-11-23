menuUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    div(
      class = "pagina-inicio",
      
      h2("Outras análises"),
      
      p("Explore diferentes características dos participantes do ENEM."),
      
      actionButton(
        ns("perfil"),
        "Perfil dos participantes",
        icon = icon("users")
      ),
      
      actionButton(
        ns("escolas"),
        "Escolas",
        icon = icon("graduation-cap")
      ),
      
      actionButton(
        ns("economico"),
        "Aspectos econômicos",
        icon = icon("money-bill")
      )
      
    )
  )
}


menuServer <- function(id, navegar) {
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$perfil, {
      navegar("perfil")
    })
    
    observeEvent(input$escolas, {
      navegar("escolas")
    })
    
    observeEvent(input$economico, {
      navegar("economico")
    })
    
  })
}