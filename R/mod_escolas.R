escolasUI <- function(id) {
  
  ns <- NS(id)
  
  card_grafico <- function(titulo, subtitulo, output_id, btn_id, height = "290px") {
    plot_dom_id <- ns(output_id)
    nome_arquivo <- gsub("[^a-zA-Z0-9_-]", "_", tolower(titulo))
    
    div(
      class = "content-card chart-card",
      style = "overflow: hidden;",
      div(
        style = "margin-bottom: 6px;",
        h3(style = "margin: 0; font-size: 1.05rem; font-weight: 700; color: #34383D;", titulo),
        tags$p(style = "margin: 2px 0 0 0; font-size: 0.82rem; color: #929292;", subtitulo)
      ),
      div(
        style = "width: 100%; overflow: hidden;",
        plotly::plotlyOutput(plot_dom_id, width = "100%", height = height)
      ),
      div(
        style = "display: flex; justify-content: flex-end; align-items: center; gap: 6px; margin-top: 8px; padding-top: 6px; border-top: 1px solid #F0F0F0;",
        tags$button(
          type = "button",
          title = "Baixar gráfico como imagem",
          class = "btn btn-default btn-xs",
          style = "font-size: 0.78rem; color: #34383D; border-color: #E0E0E0; background: #FAFAFA; padding: 3px 8px;",
          onclick = sprintf(
            "var el = document.getElementById('%s'); if (el && window.Plotly) { Plotly.downloadImage(el, {format: 'png', width: 1000, height: 600, filename: '%s'}); }",
            plot_dom_id,
            nome_arquivo
          ),
          icon("camera")
        ),
        actionButton(
          ns(btn_id),
          label = "Expandir",
          icon = icon("expand"),
          class = "btn-default btn-xs",
          style = "font-size: 0.78rem; color: #34383D; border-color: #E0E0E0; background: #FAFAFA; padding: 3px 8px;"
        )
      )
    )
  }
  
  tagList(
    
    div(
      class = "page-heading",
      span(class = "eyebrow", "DIMENSÃO EDUCACIONAL"),
      h1("Escolas"),
      p("Características das escolas relacionadas aos participantes do ENEM.")
    ),
    
    fluidRow(
      column(
        4,
        div(
          class = "content-card metric-card",
          style = "min-height: 368px; display: flex; flex-direction: column; justify-content: center; align-items: center; text-align: center; border-top: 4px solid #A6535D;",
          uiOutput(ns("total_escolas_ui"))
        )
      ),
      column(
        8,
        card_grafico(
          "Dependência administrativa",
          "Proporção por tipo de administração",
          "dependencia",
          "expand_dependencia",
          "290px"
        )
      )
    ),
    
    fluidRow(
      column(
        6,
        card_grafico(
          "Situação de funcionamento",
          "Composição percentual na base de resultados",
          "situacao",
          "expand_situacao",
          "240px"
        )
      ),
      column(
        6,
        card_grafico(
          "Localização das escolas",
          "Proporção entre áreas urbana e rural",
          "localizacao",
          "expand_localizacao",
          "240px"
        )
      )
    )
  )
}


escolasServer <- function(id, resultados) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    fmt_int <- function(x) {
      format(as.integer(round(x)), big.mark = ".", decimal.mark = ",", trim = TRUE, scientific = FALSE)
    }
    
    fmt_pct <- function(x) {
      paste0(format(round(x, 1), decimal.mark = ",", nsmall = 1, trim = TRUE), "%")
    }
    
    output$total_escolas_ui <- renderUI({
      dados <- resultados()
      
      total <- if ("CO_ENTIDADE" %in% names(dados)) {
        dplyr::n_distinct(stats::na.omit(dados$CO_ENTIDADE))
      } else {
        sum(!is.na(dados$dependencia_adminstrativa))
      }
      
      tagList(
        div(
          style = "font-size: 2rem; color: #A6535D; margin-bottom: 10px;",
          icon("school")
        ),
        h2(
          style = "margin: 0 0 6px 0; font-size: 2.3rem; font-weight: 700; color: #34383D;",
          fmt_int(total)
        ),
        span(
          "Total de escolas",
          class = "metric-description",
          style = "font-size: 0.92rem;"
        )
      )
    })
    
   
    plot_dependencia <- reactive({
      dados <- resultados()
      
      tabela <- dados %>%
        filter(!is.na(dependencia_adminstrativa)) %>%
        count(categoria = dependencia_adminstrativa, name = "n") %>%
        mutate(
          percentual = 100 * n / sum(n),
          pct_fmt = fmt_pct(percentual),
          dica = paste0(
            "<b>", categoria, "</b><br>",
            "Registros: ", fmt_int(n), "<br>",
            "Proporção: ", pct_fmt
          )
        )
      
      validate(need(nrow(tabela) > 0, "Sem dados para este filtro."))
      
      cores <- c(
        "Estadual" = cor_principal,
        "Privada" = cor_secundaria,
        "Federal" = cor_destaque,
        "Municipal" = cor_areia
      )
      
      plotly::plot_ly(
        data = tabela,
        labels = ~categoria,
        values = ~n,
        type = "pie",
        hole = 0.56,
        sort = FALSE,
        text = ~pct_fmt,
        textinfo = "text",
        textposition = "outside",
        hoverinfo = "text",
        hovertext = ~dica,
        marker = list(
          colors = unname(cores[tabela$categoria]),
          line = list(color = "white", width = 2)
        )
      ) %>%
        plotly::layout(
          autosize = TRUE,
          margin = list(t = 16, r = 24, b = 16, l = 24),
          showlegend = TRUE,
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.08),
          hoverlabel = list(
            bgcolor = "white",
            bordercolor = cor_principal,
            font = list(color = cor_texto, size = 12)
          )
        ) %>%
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    })
    
   
    plot_situacao <- reactive({
      dados <- resultados()
      
      niveis_sit <- c(
        "Em atividade",
        "Paralisada",
        "Extinta",
        "Escola extinta em anos anteriores",
        "Não informado"
      )
      
      contagem_raw <- dados %>%
        mutate(
          cat_limpa = ifelse(
            is.na(situacao_funcionamento) | !nzchar(as.character(situacao_funcionamento)),
            "Não informado",
            as.character(situacao_funcionamento)
          )
        ) %>%
        count(cat_limpa, name = "n")
      
      tabela <- tibble::tibble(cat_limpa = niveis_sit) %>%
        left_join(contagem_raw, by = "cat_limpa") %>%
        mutate(
          n = ifelse(is.na(n), 0L, as.integer(n)),
          cat_limpa = factor(cat_limpa, levels = niveis_sit)
        )
      
      total_n <- sum(tabela$n)
      validate(need(total_n > 0, "Sem dados para este filtro."))
      
      tabela <- tabela %>%
        mutate(
          percentual = 100 * n / total_n,
          dica = paste0(
            "<b>", as.character(cat_limpa), "</b><br>",
            "Registros: ", fmt_int(n), "<br>",
            "Proporção: ", fmt_pct(percentual)
          )
        )
      
      cores_sit <- c(
        "Em atividade" = cor_secundaria,
        "Paralisada" = cor_destaque,
        "Extinta" = cor_principal,
        "Escola extinta em anos anteriores" = cor_areia,
        "Não informado" = cor_cinza
      )
      
      p <- ggplot(tabela, aes(x = percentual, y = "", fill = cat_limpa, text = dica)) +
        geom_col(position = "stack", width = 0.42, color = "white", linewidth = 0.6) +
        scale_fill_manual(values = cores_sit, drop = FALSE) +
        scale_x_continuous(
          limits = c(0, 100),
          breaks = seq(0, 100, by = 25),
          labels = function(x) paste0(x, "%"),
          expand = c(0, 0)
        ) +
        labs(x = "Proporção (%)", y = NULL, fill = NULL) +
        tema_relatorio +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "bottom"
        )
      
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(
          autosize = TRUE,
          margin = list(t = 10, r = 18, b = 65, l = 12),
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.38),
          hoverlabel = list(
            bgcolor = "white",
            bordercolor = cor_principal,
            font = list(color = cor_texto, size = 12)
          )
        ) %>%
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    })
    
    
    plot_localizacao <- reactive({
      dados <- resultados()
      
      tabela <- dados %>%
        filter(!is.na(localizacao)) %>%
        count(categoria = localizacao, name = "n") %>%
        mutate(
          percentual = 100 * n / sum(n),
          categoria_ord = reorder(categoria, percentual),
          dica = paste0(
            "<b>", categoria, "</b><br>",
            "Registros: ", fmt_int(n), "<br>",
            "Proporção: ", fmt_pct(percentual)
          )
        )
      
      validate(need(nrow(tabela) > 0, "Sem dados para este filtro."))
      
      cores_loc <- c(
        "Urbana" = cor_principal,
        "Rural" = cor_secundaria
      )
      
      p <- ggplot(tabela, aes(x = percentual, y = categoria_ord, color = categoria, text = dica)) +
        geom_segment(
          aes(x = 0, xend = percentual, y = categoria_ord, yend = categoria_ord),
          linewidth = 1.6,
          alpha = 0.85
        ) +
        geom_point(size = 5.5) +
        scale_color_manual(values = cores_loc) +
        scale_x_continuous(
          limits = c(0, 100),
          breaks = seq(0, 100, by = 25),
          labels = function(x) paste0(x, "%"),
          expand = expansion(mult = c(0.02, 0.08))
        ) +
        labs(x = "Proporção (%)", y = NULL) +
        tema_relatorio +
        theme(
          legend.position = "none",
          panel.grid.major.y = element_blank()
        )
      
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(
          autosize = TRUE,
          margin = list(t = 14, r = 20, b = 45, l = 10),
          hoverlabel = list(
            bgcolor = "white",
            bordercolor = cor_principal,
            font = list(color = cor_texto, size = 12)
          )
        ) %>%
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    })
    
    output$dependencia <- plotly::renderPlotly({ plot_dependencia() })
    output$situacao    <- plotly::renderPlotly({ plot_situacao() })
    output$localizacao <- plotly::renderPlotly({ plot_localizacao() })
    
    abrir_modal <- function(titulo, output_modal_id) {
      showModal(
        modalDialog(
          title = titulo,
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Fechar"),
          plotly::plotlyOutput(ns(output_modal_id), width = "100%", height = "520px")
        )
      )
    }
    
    output$modal_dependencia <- plotly::renderPlotly({ plot_dependencia() })
    output$modal_situacao    <- plotly::renderPlotly({ plot_situacao() })
    output$modal_localizacao <- plotly::renderPlotly({ plot_localizacao() })
    
    observeEvent(input$expand_dependencia, { abrir_modal("Dependência administrativa", "modal_dependencia") })
    observeEvent(input$expand_situacao,    { abrir_modal("Situação de funcionamento", "modal_situacao") })
    observeEvent(input$expand_localizacao, { abrir_modal("Localização das escolas", "modal_localizacao") })
  })
}