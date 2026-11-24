
municipiosUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    div(
      class = "page-heading",
      span(class = "eyebrow", "DIMENSÃO TERRITORIAL"),
      h1("Análise municipal"),
      p("Compare a participação e o desempenho dos municípios de Alagoas.")
    ),
    
    fluidRow(
      column(
        6,
        div(
          class = "content-card chart-card",
          plotOutput(ns("ranking_inscritos"), height = "420px")
        )
      ),
      
      column(
        6,
        div(
          class = "content-card chart-card",
          plotOutput(ns("ranking_redacao"), height = "420px")
        )
      )
    ),
    
    div(
      class = "content-card",
      h3("Indicadores por município"),
      tableOutput(ns("tabela_municipios"))
    ),
    
    div(
      class = "content-card map-placeholder",
      icon("map-marked-alt", class = "map-icon"),
      h3("Mapa interativo de Alagoas"),
      p("Área reservada para a visualização geográfica dos indicadores municipais."),
      span("A integração depende da malha geográfica dos municípios.")
    )
  )
}


municipiosServer <- function(
    id,
    participantes,
    resultados,
    municipio
) {
  
  moduleServer(id, function(input, output, session) {
    
    resumo_municipal <- reactive({
      
      p <- participantes()
      r <- resultados()
      
      inscritos <- p %>%
        filter(!is.na(municipio_prova)) %>%
        count(municipio_prova, name = "inscritos")
      
      notas <- r %>%
        filter(!is.na(municipio_prova)) %>%
        group_by(municipio_prova) %>%
        summarise(
          media_redacao = mean(NU_NOTA_REDACAO, na.rm = TRUE),
          media_matematica = mean(NU_NOTA_MT, na.rm = TRUE),
          .groups = "drop"
        )
      
      resultado <- inscritos %>%
        left_join(notas, by = "municipio_prova") %>%
        mutate(
          media_redacao = ifelse(
            is.nan(media_redacao),
            NA,
            media_redacao
          ),
          media_matematica = ifelse(
            is.nan(media_matematica),
            NA,
            media_matematica
          )
        )
      
      resultado
    })
    
    
    output$ranking_inscritos <- renderPlot({
      
      dados <- resumo_municipal() %>%
        arrange(desc(inscritos)) %>%
        slice_head(n = 10)
      
      ggplot(
        dados,
        aes(
          x = inscritos,
          y = reorder(municipio_prova, inscritos)
        )
      ) +
        geom_col(fill = cor_secundaria, width = 0.7) +
        geom_text(
          aes(label = format(inscritos, big.mark = ".")),
          hjust = -0.15,
          size = 3,
          color = cor_texto
        ) +
        scale_x_continuous(
          expand = expansion(mult = c(0, 0.2))
        ) +
        labs(
          title = "Municípios com mais inscritos",
          subtitle = "10 maiores quantitativos",
          x = "Número de inscritos",
          y = NULL
        ) +
        tema_relatorio
    })
    
    
    output$ranking_redacao <- renderPlot({
      
      dados <- resumo_municipal() %>%
        filter(!is.na(media_redacao)) %>%
        arrange(desc(media_redacao)) %>%
        slice_head(n = 10)
      
      ggplot(
        dados,
        aes(
          x = media_redacao,
          y = reorder(municipio_prova, media_redacao)
        )
      ) +
        geom_col(fill = cor_principal, width = 0.7) +
        geom_text(
          aes(label = round(media_redacao, 1)),
          hjust = -0.15,
          size = 3,
          color = cor_texto
        ) +
        scale_x_continuous(
          expand = expansion(mult = c(0, 0.2))
        ) +
        labs(
          title = "Maiores médias de redação",
          subtitle = "10 maiores médias municipais",
          x = "Nota média",
          y = NULL
        ) +
        tema_relatorio
    })
    
    
    output$tabela_municipios <- renderTable({
      
      resumo_municipal() %>%
        arrange(desc(inscritos)) %>%
        mutate(
          `Média redação` = round(media_redacao, 1),
          `Média matemática` = round(media_matematica, 1)
        ) %>%
        select(
          Município = municipio_prova,
          Inscritos = inscritos,
          `Média redação`,
          `Média matemática`
        )
    }, striped = TRUE, bordered = FALSE, hover = TRUE)
  })
}

