
inicioUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "page-heading",
      h2("Visão geral"),
      p("Panorama dos participantes do ENEM 2024 em Alagoas.")
    ),
    
    uiOutput(ns("resumo_municipio")),
    
    h3("Indicadores em destaque"),
    uiOutput(ns("indicadores"))
  )
}


inicioServer <- function(id, participantes, resultados, municipio) {
  moduleServer(id, function(input, output, session) {
    
    output$resumo_municipio <- renderUI({
      div(
        class = "contexto-municipio",
        h4(municipio()),
        p("Dados referentes ao município selecionado.")
      )
    })
    
    output$indicadores <- renderUI({
      
      dados_part <- participantes()
      dados_result <- resultados()
      
      total_participantes <- nrow(dados_part)
      
      media_redacao <- if ("NU_NOTA_REDACAO" %in% names(dados_result)) {
        mean(dados_result$NU_NOTA_REDACAO, na.rm = TRUE)
      } else {
        NA_real_
      }
      
      media_matematica <- if ("NU_NOTA_MT" %in% names(dados_result)) {
        mean(dados_result$NU_NOTA_MT, na.rm = TRUE)
      } else {
        NA_real_
      }
      
      media_linguagens <- if ("NU_NOTA_LC" %in% names(dados_result)) {
        mean(dados_result$NU_NOTA_LC, na.rm = TRUE)
      } else {
        NA_real_
      }
      
      media_humanas <- if ("NU_NOTA_CH" %in% names(dados_result)) {
        mean(dados_result$NU_NOTA_CH, na.rm = TRUE)
      } else {
        NA_real_
      }
      
      media_natureza <- if ("NU_NOTA_CN" %in% names(dados_result)) {
        mean(dados_result$NU_NOTA_CN, na.rm = TRUE)
      } else {
        NA_real_
      }
      
      formatar_nota <- function(nota) {
        if (is.na(nota) || is.nan(nota)) {
          return("—")
        }
        
        format(round(nota, 1), decimal.mark = ",", nsmall = 1)
      }
      
      div(
        class = "metric-grid",
        
        div(
          class = "metric-card",
          h2(format(total_participantes, big.mark = ".", decimal.mark = ",")),
          span("Participantes na base", class = "metric-description")
        ),
        
        div(
          class = "metric-card",
          h2(formatar_nota(media_redacao)),
          span("Média de redação", class = "metric-description")
        ),
        
        div(
          class = "metric-card",
          h2(formatar_nota(media_matematica)),
          span("Média de matemática", class = "metric-description")
        ),
        
        div(
          class = "metric-card",
          h2(formatar_nota(media_linguagens)),
          span("Média de linguagens", class = "metric-description")
        ),
        
        div(
          class = "metric-card",
          h2(formatar_nota(media_humanas)),
          span("Média de ciências humanas", class = "metric-description")
        ),
        
        div(
          class = "metric-card",
          h2(formatar_nota(media_natureza)),
          span("Média de ciências da natureza", class = "metric-description")
        )
      )
    })
  })
}

