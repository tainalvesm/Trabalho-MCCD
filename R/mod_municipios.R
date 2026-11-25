
# Malha dos municípios de Alagoas (baixada uma vez e salva em dados/)
carregar_malha <- function(arquivo = "dados/malha_al.rds") {
  
  if (file.exists(arquivo)) {
    return(readRDS(arquivo))
  }
  
  tryCatch({
    malha <- geobr::read_municipality(
      code_muni = "AL", year = 2022, showProgress = FALSE
    )
    malha <- sf::st_transform(malha, 4326)
    malha$code_muni <- as.integer(malha$code_muni)
    saveRDS(malha, arquivo)
    malha
  }, error = function(e) NULL)
}

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
      h3("Mapa interativo de Alagoas"),
      selectInput(
        ns("indicador"),
        "O que mostrar no mapa",
        choices = c(
          "Inscritos (município da prova)" = "inscritos",
          "Média geral" = "media_geral",
          "Média de redação" = "NU_NOTA_REDACAO",
          "Média de matemática" = "NU_NOTA_MT",
          "Média de linguagens" = "NU_NOTA_LC",
          "Média de ciências humanas" = "NU_NOTA_CH",
          "Média de ciências da natureza" = "NU_NOTA_CN"
        ),
        width = "320px"
      ),
      leaflet::leafletOutput(ns("mapa"), height = "480px"),
      div(
        class = "data-note",
        "Inscritos: município onde a prova foi aplicada. Notas: município da escola, apenas participantes com escola informada; municípios com menos de 10 notas aparecem sem dados."
      )
    ),
    
    div(
      class = "content-card",
      h3("Indicadores por município de aplicação da prova"),
      div(class = "tabela-rolagem", tableOutput(ns("tabela_municipios")))
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
    
    
    malha <- reactive({
      m <- carregar_malha()
      validate(need(!is.null(m), "Não foi possível baixar o mapa. Verifique a internet."))
      m
    })
    
    
    valores_mapa <- reactive({
      
      if (input$indicador == "inscritos") {
        return(
          participantes() %>%
            count(code_muni = as.integer(CO_MUNICIPIO_PROVA), name = "valor") %>%
            mutate(n = valor)
        )
      }
      
      r <- resultados()
      
      validate(need(
        "CO_MUNICIPIO_ESC" %in% names(r),
        "A base de resultados não tem a coluna CO_MUNICIPIO_ESC."
      ))
      
      r$nota <- if (input$indicador == "media_geral") {
        rowMeans(r[, c("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC",
                       "NU_NOTA_MT", "NU_NOTA_REDACAO")])
      } else {
        r[[input$indicador]]
      }
      
      r %>%
        filter(!is.na(CO_MUNICIPIO_ESC), !is.na(nota)) %>%
        group_by(code_muni = as.integer(CO_MUNICIPIO_ESC)) %>%
        summarise(valor = mean(nota), n = n(), .groups = "drop") %>%
        filter(n >= 10)
    })
    
    
    output$mapa <- leaflet::renderLeaflet({
      
      mapa <- malha() %>%
        left_join(valores_mapa(), by = "code_muni")
      
      validate(need(any(!is.na(mapa$valor)), "Sem dados para este indicador."))
      
      contagem <- input$indicador == "inscritos"
      
      paleta <- leaflet::colorNumeric(
        c("#F3E9E1", cor_destaque, cor_principal, "#5E2A31"),
        domain = if (contagem) log10(mapa$valor) else mapa$valor,
        na.color = "#E4E4E4"
      )
      
      cor <- paleta(if (contagem) log10(mapa$valor) else mapa$valor)
      
      texto <- ifelse(
        is.na(mapa$valor),
        "Sem dados",
        if (contagem) {
          paste(format(mapa$valor, big.mark = ".", decimal.mark = ",", trim = TRUE), "inscritos")
        } else {
          paste0(
            "Média: ", format(round(mapa$valor, 1), decimal.mark = ",", nsmall = 1, trim = TRUE),
            " (", mapa$n, " notas)"
          )
        }
      )
      
      rotulos <- lapply(
        paste0("<strong>", htmltools::htmlEscape(mapa$name_muni), "</strong><br>", texto),
        htmltools::HTML
      )
      
      desenho <- leaflet::leaflet(mapa) %>%
        leaflet::addPolygons(
          fillColor = cor,
          fillOpacity = 0.85,
          color = "white",
          weight = 1,
          label = rotulos,
          highlightOptions = leaflet::highlightOptions(
            weight = 3, color = cor_texto, bringToFront = TRUE
          )
        )
      
      # Destaca e aproxima o município escolhido na busca lateral
      codigo <- participantes() %>%
        filter(municipio_prova == municipio()) %>%
        pull(CO_MUNICIPIO_PROVA) %>%
        head(1)
      
      if (length(codigo) == 1) {
        escolhido <- mapa[mapa$code_muni == codigo, ]
        limites <- unname(sf::st_bbox(escolhido))
        
        desenho <- desenho %>%
          leaflet::addPolylines(data = escolhido, color = cor_secundaria, weight = 4, opacity = 1) %>%
          leaflet::setView(
            lng = mean(limites[c(1, 3)]),
            lat = mean(limites[c(2, 4)]),
            zoom = 10
          )
      }
      
      if (!contagem) {
        desenho <- desenho %>%
          leaflet::addLegend(
            "bottomright", pal = paleta, values = mapa$valor,
            title = "Nota média", na.label = "Sem dados", opacity = 0.9
          )
      }
      
      desenho
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
    }, striped = TRUE, bordered = FALSE, hover = TRUE,
    width = "100%", align = "lrrr", digits = 1)
  })
}

