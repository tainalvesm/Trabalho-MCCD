desempenhoUI <- function(id) {
  ns <- NS(id)
  
  card_grafico <- function(titulo, subtitulo, output_id, btn_id, height = "360px") {
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
      span(class = "eyebrow", "RESULTADOS DO EXAME"),
      h1("Desempenho"),
      p("Explore as médias das áreas de conhecimento e da redação.")
    ),
    
    h3(style = "margin: 0 0 14px 0;", "Médias por área"),
    uiOutput(ns("cards_medias")),
    
    div(
      style = "margin-top: 18px;",
      tabsetPanel(
        id = ns("abas_desempenho"),
        type = "pills",
        
        tabPanel(
          "Por rede de ensino",
          div(
            style = "margin-top: 14px;",
            fluidRow(
              column(
                12,
                card_grafico(
                  "Desempenho médio por dependência administrativa",
                  "Média das notas por área do conhecimento",
                  "plot_rede",
                  "expand_rede",
                  "380px"
                )
              )
            )
          )
        ),
        
        tabPanel(
          "Presença",
          div(
            style = "margin-top: 14px;",
            fluidRow(
              column(
                12,
                card_grafico(
                  "Desempenho médio por condição de presença",
                  "Média das notas conforme presença nas provas",
                  "plot_presenca",
                  "expand_presenca",
                  "360px"
                )
              )
            )
          )
        ),
        
        tabPanel(
          "Comparação",
          div(
            style = "margin-top: 14px;",
            fluidRow(
              column(
                6,
                div(
                  class = "content-card",
                  selectInput(
                    ns("mun_esq"),
                    label = NULL,
                    choices = c("Selecione um município" = ""),
                    selected = ""
                  ),
                  uiOutput(ns("resumo_esq"))
                )
              ),
              column(
                6,
                div(
                  class = "content-card",
                  selectInput(
                    ns("mun_dir"),
                    label = NULL,
                    choices = c("Selecione um município" = ""),
                    selected = ""
                  ),
                  uiOutput(ns("resumo_dir"))
                )
              )
            ),
            fluidRow(
              column(
                12,
                card_grafico(
                  "Comparação de médias entre municípios",
                  "Média por área de conhecimento para os municípios selecionados",
                  "plot_comparacao",
                  "expand_comparacao",
                  "380px"
                )
              )
            )
          )
        )
      )
    )
  )
}

desempenhoServer <- function(id, resultados, participantes = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    cor_prim <- if (exists("cor_principal", envir = .GlobalEnv)) get("cor_principal", envir = .GlobalEnv) else "#A6535D"
    cor_sec  <- if (exists("cor_secundaria", envir = .GlobalEnv)) get("cor_secundaria", envir = .GlobalEnv) else "#668596"
    cor_dest <- if (exists("cor_destaque", envir = .GlobalEnv)) get("cor_destaque", envir = .GlobalEnv) else "#C98D91"
    cor_ar   <- if (exists("cor_areia", envir = .GlobalEnv)) get("cor_areia", envir = .GlobalEnv) else "#DCCDC0"
    cor_txt  <- if (exists("cor_texto", envir = .GlobalEnv)) get("cor_texto", envir = .GlobalEnv) else "#34383D"
    tema_base <- if (exists("tema_relatorio", envir = .GlobalEnv)) get("tema_relatorio", envir = .GlobalEnv) else ggplot2::theme_minimal()
    
    cols_areas <- c(
      "Ciências da Natureza" = "NU_NOTA_CN",
      "Ciências Humanas"     = "NU_NOTA_CH",
      "Linguagens"           = "NU_NOTA_LC",
      "Matemática"           = "NU_NOTA_MT",
      "Redação"              = "NU_NOTA_REDACAO"
    )
    
    fmt_nota <- function(x) {
      if (is.null(x) || length(x) == 0 || is.na(x) || is.nan(x) || is.infinite(x)) {
        return("—")
      }
      format(round(x, 1), decimal.mark = ",", nsmall = 1, trim = TRUE)
    }
    
    fmt_int <- function(x) {
      format(as.integer(round(x)), big.mark = ".", decimal.mark = ",", trim = TRUE, scientific = FALSE)
    }
    
    montar_plotly_grupo <- function(p) {
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(
          autosize = TRUE,
          margin = list(t = 14, r = 20, b = 60, l = 50),
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.22),
          hoverlabel = list(
            bgcolor = "white",
            bordercolor = cor_prim,
            font = list(color = cor_txt, size = 12)
          )
        ) %>%
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    }
    
    # Cards de médias 
    output$cards_medias <- renderUI({
      df <- resultados()
      
      valores <- sapply(cols_areas, function(col) {
        if (col %in% names(df)) mean(df[[col]], na.rm = TRUE) else NA_real_
      })
      
      media_geral <- if (all(is.na(valores))) NA_real_ else mean(valores, na.rm = TRUE)
      
      card_geral <- div(
        class = "metric-card",
        style = sprintf("border: 2px solid %s; background-color: #FDF8F8;", cor_prim),
        span(
          "DESTAQUE",
          style = sprintf(
            "font-size: 0.68rem; font-weight: 700; letter-spacing: 0.06em; color: %s; display: block; margin-bottom: 2px;",
            cor_prim
          )
        ),
        h2(style = "margin: 0 0 4px 0;", fmt_nota(media_geral)),
        span("Média geral", class = "metric-description")
      )
      
      bordas <- c(cor_sec, cor_prim, cor_sec, cor_prim, cor_sec)
      
      cards_areas <- lapply(seq_along(cols_areas), function(i) {
        nome_area <- names(cols_areas)[i]
        div(
          class = "metric-card",
          style = sprintf("border-top: 4px solid %s;", bordas[i]),
          span(nome_area, class = "metric-description", style = "display: block; font-weight: 600; margin-bottom: 4px;"),
          h2(style = "margin: 0 0 4px 0;", fmt_nota(valores[i])),
          span("Nota média", class = "metric-description")
        )
      })
      
      div(
        style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 16px;",
        c(list(card_geral), cards_areas)
      )
    })
    
    # rede de ensino
    plot_rede_obj <- reactive({
      df <- resultados()
      validate(need("dependencia_adminstrativa" %in% names(df), "Coluna de dependência administrativa indisponível."))
      
      df_limpo <- df %>%
        filter(!is.na(dependencia_adminstrativa), nzchar(as.character(dependencia_adminstrativa))) %>%
        mutate(rede = as.character(dependencia_adminstrativa))
      
      validate(need(nrow(df_limpo) > 0, "Sem dados por rede de ensino para este filtro."))
      
      long_df <- lapply(names(cols_areas), function(area_label) {
        col_nome <- cols_areas[[area_label]]
        if (!col_nome %in% names(df_limpo)) return(NULL)
        df_limpo %>%
          group_by(rede) %>%
          summarise(
            media = mean(.data[[col_nome]], na.rm = TRUE),
            n_val = sum(!is.na(.data[[col_nome]])),
            .groups = "drop"
          ) %>%
          mutate(area = area_label)
      }) %>% bind_rows()
      
      long_df <- long_df %>%
        filter(!is.na(media)) %>%
        mutate(
          area = factor(area, levels = names(cols_areas)),
          dica = paste0(
            "<b>", rede, "</b> — ", as.character(area), "<br>",
            "Nota média: <b>", fmt_nota(media), "</b><br>",
            "Avaliações: ", fmt_int(n_val)
          )
        )
      
      validate(need(nrow(long_df) > 0, "Sem médias disponíveis para este filtro."))
      
      cores_rede <- c(
        "Privada"   = cor_prim,
        "Estadual"  = cor_sec,
        "Federal"   = cor_dest,
        "Municipal" = cor_ar
      )
      
      p <- ggplot(long_df, aes(x = area, y = media, fill = rede, text = dica)) +
        geom_col(position = position_dodge(width = 0.72), width = 0.62) +
        scale_fill_manual(values = cores_rede) +
        labs(x = NULL, y = "Nota média", fill = NULL) +
        tema_base +
        theme(legend.position = "bottom")
      
      montar_plotly_grupo(p)
    })
    
    # presença
    plot_presenca_obj <- reactive({
      df <- resultados()
      col_pres <- intersect(c("presenca", "TP_PRESENCA_CN", "TP_PRESENCA_MT"), names(df))[1]
      validate(need(!is.na(col_pres), "Coluna de presença indisponível na base."))
      
      df_p <- df %>%
        filter(!is.na(.data[[col_pres]])) %>%
        mutate(grupo_pres = as.character(.data[[col_pres]]))
      
      long_p <- lapply(names(cols_areas), function(area_label) {
        col_nome <- cols_areas[[area_label]]
        if (!col_nome %in% names(df_p)) return(NULL)
        df_p %>%
          group_by(grupo_pres) %>%
          summarise(
            media = mean(.data[[col_nome]], na.rm = TRUE),
            n_val = sum(!is.na(.data[[col_nome]])),
            .groups = "drop"
          ) %>%
          mutate(area = area_label)
      }) %>% bind_rows()
      
      long_p <- long_p %>%
        filter(!is.na(media)) %>%
        mutate(
          area = factor(area, levels = names(cols_areas)),
          dica = paste0(
            "<b>", grupo_pres, "</b> — ", as.character(area), "<br>",
            "Nota média: <b>", fmt_nota(media), "</b><br>",
            "Registros: ", fmt_int(n_val)
          )
        )
      
      validate(need(nrow(long_p) > 0, "Sem dados de presença para este filtro."))
      
      p <- ggplot(long_p, aes(x = area, y = media, fill = grupo_pres, text = dica)) +
        geom_col(position = position_dodge(width = 0.72), width = 0.62) +
        scale_fill_manual(values = c(cor_prim, cor_sec, cor_dest, cor_ar)) +
        labs(x = NULL, y = "Nota média", fill = NULL) +
        tema_base +
        theme(legend.position = "bottom")
      
      montar_plotly_grupo(p)
    })
    
    # select
    observe({
      df <- resultados()
      col_mun <- intersect(c("municipio_prova", "NO_MUNICIPIO_PROVA", "municipio"), names(df))[1]
      if (!is.na(col_mun)) {
        mun_choices <- sort(unique(stats::na.omit(as.character(df[[col_mun]]))))
        mun_choices <- mun_choices[nzchar(mun_choices)]
        opcoes <- c("Selecione um município" = "", stats::setNames(mun_choices, mun_choices))
        
        sel_esq <- if (!is.null(input$mun_esq) && nzchar(input$mun_esq) && input$mun_esq %in% mun_choices) {
          input$mun_esq
        } else {
          ""
        }
        sel_dir <- if (!is.null(input$mun_dir) && nzchar(input$mun_dir) && input$mun_dir %in% mun_choices) {
          input$mun_dir
        } else {
          ""
        }
        
        updateSelectInput(session, "mun_esq", choices = opcoes, selected = sel_esq)
        updateSelectInput(session, "mun_dir", choices = opcoes, selected = sel_dir)
      }
    })
    
    montar_resumo_mun <- function(mun_nome) {
      if (is.null(mun_nome) || !nzchar(mun_nome)) {
        return(
          div(
            style = "margin-top: 8px; font-size: 0.88rem; color: #929292;",
            "Selecione um município para ver o resumo."
          )
        )
      }
      
      df <- resultados()
      col_mun <- intersect(c("municipio_prova", "NO_MUNICIPIO_PROVA", "municipio"), names(df))[1]
      validate(need(!is.na(col_mun), "Coluna de município indisponível."))
      
      sub <- df[as.character(df[[col_mun]]) == mun_nome, , drop = FALSE]
      n_part <- nrow(sub)
      
      n_esc <- if ("CO_ENTIDADE" %in% names(sub)) {
        dplyr::n_distinct(stats::na.omit(sub$CO_ENTIDADE))
      } else {
        n_part
      }
      
      dep_vec <- if ("dependencia_adminstrativa" %in% names(sub)) {
        tolower(as.character(sub$dependencia_adminstrativa))
      } else {
        character(0)
      }
      
      n_priv <- sum(grepl("privad", dep_vec), na.rm = TRUE)
      n_fed  <- sum(grepl("federal", dep_vec), na.rm = TRUE)
      n_pub  <- sum(grepl("estadual|municipal|p[uú]blic", dep_vec), na.rm = TRUE)
      
      div(
        style = "margin-top: 10px; font-size: 0.9rem; line-height: 1.6;",
        div(strong("Participantes: "), fmt_int(n_part)),
        div(strong("Escolas (total do município): "), fmt_int(n_esc)),
        tags$ul(
          style = "margin: 4px 0 0 18px; padding: 0;",
          tags$li(paste0("Privada: ", fmt_int(n_priv))),
          tags$li(paste0("Federal: ", fmt_int(n_fed))),
          tags$li(paste0("Pública: ", fmt_int(n_pub)))
        )
      )
    }
    
    output$resumo_esq <- renderUI({ montar_resumo_mun(input$mun_esq) })
    output$resumo_dir <- renderUI({ montar_resumo_mun(input$mun_dir) })
    
    #  comparação entre municípios
    plot_comparacao_obj <- reactive({
      muns_sel <- c(input$mun_esq, input$mun_dir)
      muns_sel <- unique(muns_sel[!is.null(muns_sel) & nzchar(muns_sel)])
      
      validate(need(length(muns_sel) > 0, "Selecione pelo menos um município para visualizar a comparação."))
      
      df <- resultados()
      col_mun <- intersect(c("municipio_prova", "NO_MUNICIPIO_PROVA", "municipio"), names(df))[1]
      validate(need(!is.na(col_mun), "Coluna de município indisponível."))
      
      sub <- df[as.character(df[[col_mun]]) %in% muns_sel, , drop = FALSE]
      sub$.mun <- factor(as.character(sub[[col_mun]]), levels = muns_sel)
      
      long_c <- lapply(names(cols_areas), function(area_label) {
        col_nome <- cols_areas[[area_label]]
        if (!col_nome %in% names(sub)) return(NULL)
        sub %>%
          group_by(.mun) %>%
          summarise(
            media = mean(.data[[col_nome]], na.rm = TRUE),
            n_val = sum(!is.na(.data[[col_nome]])),
            .groups = "drop"
          ) %>%
          mutate(area = area_label)
      }) %>% bind_rows()
      
      long_c <- long_c %>%
        filter(!is.na(media)) %>%
        mutate(
          area = factor(area, levels = names(cols_areas)),
          dica = paste0(
            "<b>", as.character(.mun), "</b> — ", as.character(area), "<br>",
            "Nota média: <b>", fmt_nota(media), "</b><br>",
            "Avaliações: ", fmt_int(n_val)
          )
        )
      
      validate(need(nrow(long_c) > 0, "Sem dados para os municípios selecionados."))
      
      p <- ggplot(long_c, aes(x = area, y = media, fill = .mun, text = dica)) +
        geom_col(position = position_dodge(width = 0.72), width = 0.62) +
        scale_fill_manual(values = c(cor_prim, cor_sec)) +
        labs(x = NULL, y = "Nota média", fill = NULL) +
        tema_base +
        theme(legend.position = "bottom")
      
      montar_plotly_grupo(p)
    })
    
    output$plot_rede       <- plotly::renderPlotly({ plot_rede_obj() })
    output$plot_presenca   <- plotly::renderPlotly({ plot_presenca_obj() })
    output$plot_comparacao <- plotly::renderPlotly({ plot_comparacao_obj() })
    
    abrir_modal <- function(titulo, output_modal_id) {
      showModal(
        modalDialog(
          title = titulo,
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Fechar"),
          plotly::plotlyOutput(ns(output_modal_id), width = "100%", height = "540px")
        )
      )
    }
    
    output$modal_rede       <- plotly::renderPlotly({ plot_rede_obj() })
    output$modal_presenca   <- plotly::renderPlotly({ plot_presenca_obj() })
    output$modal_comparacao <- plotly::renderPlotly({ plot_comparacao_obj() })
    
    observeEvent(input$expand_rede,       { abrir_modal("Desempenho médio por dependência administrativa", "modal_rede") })
    observeEvent(input$expand_presenca,   { abrir_modal("Desempenho médio por condição de presença", "modal_presenca") })
    observeEvent(input$expand_comparacao, { abrir_modal("Comparação de médias entre municípios", "modal_comparacao") })
  })
}