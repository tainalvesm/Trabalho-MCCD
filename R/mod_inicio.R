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
    uiOutput(ns("indicadores")),
    
    div(
      class = "content-card",
      style = "margin-top: 20px;",
      h3("Mapa interativo por município"),
      p("Clique em um município para filtrar o painel (clique novamente para voltar a todos)."),
      leaflet::leafletOutput(ns("mapa_visao_geral"), height = "430px")
    )
  )
}

inicioServer <- function(id, participantes, participantes_todos, resultados, malha, municipio) {
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
      
      notas_cols <- c(
        "NU_NOTA_CN",
        "NU_NOTA_CH",
        "NU_NOTA_LC",
        "NU_NOTA_MT",
        "NU_NOTA_REDACAO"
      )
      
      medias <- sapply(notas_cols, function(col) {
        if (col %in% names(dados_result)) {
          mean(dados_result[[col]], na.rm = TRUE)
        } else {
          NA_real_
        }
      })
      
      media_geral <- if (all(is.na(medias))) {
        NA_real_
      } else {
        mean(medias, na.rm = TRUE)
      }
      
      formatar_nota <- function(nota) {
        if (is.na(nota) || is.nan(nota) || is.infinite(nota)) {
          return("—")
        }
        format(round(nota, 1), decimal.mark = ",", nsmall = 1)
      }
      
      div(
        style = "display: grid; grid-template-columns: 1fr 2.4fr; gap: 16px; align-items: stretch;",
        
      
        div(
          class = "metric-card",
          style = "display: flex; flex-direction: column; justify-content: center; align-items: center; text-align: center; padding: 24px 18px; border-top: 4px solid #A6535D;",
          h2(
            style = "margin: 0 0 6px 0; font-size: 2.5rem; font-weight: 700;",
            format(total_participantes, big.mark = ".", decimal.mark = ",")
          ),
          span(
            "Inscritos na base",
            class = "metric-description",
            style = "margin-bottom: 18px;"
          ),
          div(
            style = "font-size: 2.3rem; color: #A6535D; opacity: 0.85;",
            icon("users")
          )
        ),
        
      
        div(
          style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 16px;",
          
          div(
            class = "metric-card",
            style = "border: 2px solid #A6535D; background-color: #FDF8F8;",
            span(
              "DESTAQUE",
              style = "font-size: 0.68rem; font-weight: 700; letter-spacing: 0.06em; color: #A6535D; display: block; margin-bottom: 2px;"
            ),
            h2(formatar_nota(media_geral)),
            span("Média geral", class = "metric-description")
          ),
          
          div(
            class = "metric-card",
            h2(formatar_nota(medias[["NU_NOTA_REDACAO"]])),
            span("Média de redação", class = "metric-description")
          ),
          
          div(
            class = "metric-card",
            h2(formatar_nota(medias[["NU_NOTA_MT"]])),
            span("Média de matemática", class = "metric-description")
          ),
          
          div(
            class = "metric-card",
            h2(formatar_nota(medias[["NU_NOTA_LC"]])),
            span("Média de linguagens", class = "metric-description")
          ),
          
          div(
            class = "metric-card",
            h2(formatar_nota(medias[["NU_NOTA_CH"]])),
            span("Média de ciências humanas", class = "metric-description")
          ),
          
          div(
            class = "metric-card",
            h2(formatar_nota(medias[["NU_NOTA_CN"]])),
            span("Média de ciências da natureza", class = "metric-description")
          )
        )
      )
    })
    
    output$mapa_visao_geral <- leaflet::renderLeaflet({
      m <- malha()
      validate(need(!is.null(m), "Malha municipal indisponível no momento."))
      
      contagem <- participantes_todos() %>%
        filter(!is.na(municipio_prova)) %>%
        count(municipio_prova, name = "n_inscritos")
      
      mapa_dados <- m %>%
        mutate(nome_limpo = corrigir_acentos(name_muni)) %>%
        left_join(contagem, by = c("nome_limpo" = "municipio_prova"))
      
      paleta <- leaflet::colorNumeric(
        c("#F3E9E1", cor_destaque, cor_principal, "#5E2A31"),
        domain = log10(pmax(mapa_dados$n_inscritos, 1, na.rm = TRUE)),
        na.color = "#E4E4E4"
      )
      
      cores_poligonos <- ifelse(
        is.na(mapa_dados$n_inscritos),
        "#E4E4E4",
        paleta(log10(pmax(mapa_dados$n_inscritos, 1)))
      )
      
      rotulos <- lapply(
        paste0(
          "<strong>", htmltools::htmlEscape(mapa_dados$name_muni), "</strong><br>",
          ifelse(
            is.na(mapa_dados$n_inscritos),
            "Sem registros",
            paste(format(mapa_dados$n_inscritos, big.mark = ".", decimal.mark = ",", trim = TRUE), "inscritos")
          )
        ),
        htmltools::HTML
      )
      
      mapa_leaflet <- leaflet::leaflet(mapa_dados) %>%
        leaflet::addPolygons(
          layerId = ~nome_limpo,
          fillColor = cores_poligonos,
          fillOpacity = 0.85,
          color = "white",
          weight = 1,
          label = rotulos,
          highlightOptions = leaflet::highlightOptions(
            weight = 3,
            color = cor_texto,
            bringToFront = TRUE
          )
        )
      
      if (!is.null(municipio()) && municipio() != "Todos os municípios") {
        selecionado <- mapa_dados[mapa_dados$nome_limpo == municipio(), ]
        if (nrow(selecionado) > 0) {
          limites <- unname(sf::st_bbox(selecionado))
          mapa_leaflet <- mapa_leaflet %>%
            leaflet::addPolylines(
              data = selecionado,
              color = cor_secundaria,
              weight = 4,
              opacity = 1
            ) %>%
            leaflet::setView(
              lng = mean(limites[c(1, 3)]),
              lat = mean(limites[c(2, 4)]),
              zoom = 10
            )
        }
      }
      
      mapa_leaflet
    })
    
   
    observeEvent(input$mapa_visao_geral_shape_click, {
      clicado <- input$mapa_visao_geral_shape_click$id
      if (!is.null(clicado) && nzchar(clicado)) {
        novo_valor <- if (!is.null(municipio()) && municipio() == clicado) {
          "Todos os municípios"
        } else {
          clicado
        }
        updateSelectizeInput(
          session = session$rootScope(),
          inputId = "municipio_global",
          selected = novo_valor
        )
      }
    })
  })
}