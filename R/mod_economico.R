socioeconomicoUI <- function(id) {
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
      span(class = "eyebrow", "QUESTIONÁRIO SOCIOECONÔMICO"),
      h1("Contexto familiar e econômico"),
      p("Características familiares, condições domiciliares e acesso à tecnologia.")
    ),
    
    tabsetPanel(
      id = ns("abas_socio"),
      type = "pills",
      
      tabPanel(
        "Responsáveis",
        div(
          style = "margin-top: 14px;",
          fluidRow(
            column(
              12,
              card_grafico(
                "Escolaridade dos responsáveis",
                "Percentual de participantes por nível de instrução",
                "plot_esc_resp",
                "expand_esc_resp",
                "380px"
              )
            )
          ),
          fluidRow(
            column(
              12,
              card_grafico(
                "Grupo ocupacional dos responsáveis",
                "Distribuição por faixa de ocupação",
                "plot_ocup_resp",
                "expand_ocup_resp",
                "420px"
              )
            )
          )
        )
      ),
      
      tabPanel(
        "Domicílio e renda",
        div(
          style = "margin-top: 14px;",
          fluidRow(
            column(
              6,
              card_grafico(
                "Renda familiar mensal",
                "Faixas de renda declaradas no questionário",
                "plot_renda",
                "expand_renda",
                "360px"
              )
            ),
            column(
              6,
              card_grafico(
                "Moradores no domicílio",
                "Quantidade de pessoas que residem na casa",
                "plot_moradores",
                "expand_moradores",
                "360px"
              )
            )
          )
        )
      ),
      
      tabPanel(
        "Tecnologia",
        div(
          style = "margin-top: 14px;",
          fluidRow(
            column(
              6,
              card_grafico(
                "Acesso à internet",
                "Disponibilidade de conexão no domicílio",
                "plot_internet",
                "expand_internet",
                "340px"
              )
            ),
            column(
              6,
              card_grafico(
                "Acesso a computador",
                "Disponibilidade de computador no domicílio",
                "plot_computador",
                "expand_computador",
                "340px"
              )
            )
          )
        )
      ),
      
      tabPanel(
        "Transporte",
        div(
          style = "margin-top: 14px;",
          fluidRow(
            column(6, card_grafico("Carro no domicílio", "Quantidade de carros na casa",
                                   "plot_transporte", "expand_transporte", "340px")),
            column(6, card_grafico("Motocicleta no domicílio", "Quantidade de motocicletas na casa",
                                   "plot_moto", "expand_moto", "340px"))
          )
        )
      )
    )
  )
}

socioeconomicoServer <- function(id, participantes) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Questionário lido direto do CSV original (não depende dos nomes usados em dados.R)
    base_q <- read.csv("dados/Participantes_AL.csv")
    
    esc <- c(A = "Nunca estudou", B = "Fundamental I incompleto", C = "Fundamental II incompleto",
             D = "Médio incompleto", E = "Médio completo", F = "Superior completo",
             G = "Pós-graduação completa", H = "Não sei")
    ocup <- c(A = "G1: campo, pesca, extrativismo", B = "G2: domésticos, comércio, apoio",
              C = "G3: indústria, construção, transporte", D = "G4: técnicos, professores, autônomos",
              E = "G5: nível superior, direção", F = "Não sei")
    renda <- c(A = "Nenhuma renda", B = "Até R$ 1.412", C = "R$ 1.412 a 2.118", D = "R$ 2.118 a 2.824",
               E = "R$ 2.824 a 3.530", F = "R$ 3.530 a 4.236", G = "R$ 4.236 a 5.648",
               H = "R$ 5.648 a 7.060", I = "R$ 7.060 a 8.472", J = "R$ 8.472 a 9.884",
               K = "R$ 9.884 a 11.296", L = "R$ 11.296 a 12.708", M = "R$ 12.708 a 14.120",
               N = "R$ 14.120 a 16.944", O = "R$ 16.944 a 21.180", P = "R$ 21.180 a 28.240",
               Q = "Acima de R$ 28.240")
    wifi <- c(A = "Não", B = "Sim")
    comp <- c(A = "Não", B = "Sim, um", C = "Sim, dois", D = "Sim, três", E = "Sim, quatro ou mais")
    carro <- c(A = "Não", B = "Sim, um", C = "Sim, dois", D = "Sim, três ou mais")
    moto <- c(A = "Não", B = "Sim, uma", C = "Sim, duas", D = "Sim, três ou mais")
    
    mapas <- list(Q001 = esc, Q002 = esc, Q003 = ocup, Q004 = ocup, Q007 = renda,
                  Q011 = carro, Q012 = moto, Q020 = wifi, Q021 = comp)
    for (v in names(mapas)) base_q[[v]] <- unname(mapas[[v]][as.character(base_q[[v]])])
    
    # acompanha o filtro de município do app
    mun_original <- as.character(base_q$NO_MUNICIPIO_PROVA)
    mun_convertido <- iconv(mun_original, from = "latin1", to = "UTF-8")
    dados_q <- reactive({
      alvo <- unique(as.character(participantes()$municipio_prova))
      base_q[mun_original %in% alvo | mun_convertido %in% alvo, ]
    })
    
    cor_prim <- if (exists("cor_principal", envir = .GlobalEnv)) get("cor_principal", envir = .GlobalEnv) else "#A6535D"
    cor_sec  <- if (exists("cor_secundaria", envir = .GlobalEnv)) get("cor_secundaria", envir = .GlobalEnv) else "#668596"
    cor_dest <- if (exists("cor_destaque", envir = .GlobalEnv)) get("cor_destaque", envir = .GlobalEnv) else "#C98D91"
    cor_ar   <- if (exists("cor_areia", envir = .GlobalEnv)) get("cor_areia", envir = .GlobalEnv) else "#DCCDC0"
    cor_txt  <- if (exists("cor_texto", envir = .GlobalEnv)) get("cor_texto", envir = .GlobalEnv) else "#34383D"
    tema_base <- if (exists("tema_relatorio", envir = .GlobalEnv)) get("tema_relatorio", envir = .GlobalEnv) else ggplot2::theme_minimal()
    
    fmt_int <- function(x) {
      format(as.integer(round(x)), big.mark = ".", decimal.mark = ",", trim = TRUE, scientific = FALSE)
    }
    
    fmt_pct <- function(x) {
      paste0(format(round(x, 1), decimal.mark = ",", nsmall = 1, trim = TRUE), "%")
    }
    
    montar_plotly <- function(p, mostrar_legenda = FALSE) {
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(
          autosize = TRUE,
          margin = list(t = 12, r = 20, b = if (mostrar_legenda) 60 else 45, l = 12),
          showlegend = mostrar_legenda,
          legend = if (mostrar_legenda) {
            list(orientation = "h", x = 0.5, xanchor = "center", y = -0.22)
          } else {
            NULL
          },
          hoverlabel = list(
            bgcolor = "white",
            bordercolor = cor_prim,
            font = list(color = cor_txt, size = 12)
          )
        ) %>%
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    }
    
    
    grafico_barras_pct <- function(dados, candidatos_col, cor_barra, ordem_niveis = NULL) {
      col_encontrada <- intersect(candidatos_col, names(dados))[1]
      validate(need(!is.na(col_encontrada), "Variável não encontrada na base de participantes."))
      
      tabela <- dados %>%
        filter(!is.na(.data[[col_encontrada]]), nzchar(as.character(.data[[col_encontrada]]))) %>%
        count(categoria = as.character(.data[[col_encontrada]]), name = "n")
      
      validate(need(nrow(tabela) > 0, "Sem dados disponíveis para este filtro."))
      
      total_n <- sum(tabela$n)
      
      if (!is.null(ordem_niveis)) {
        niveis_presentes <- intersect(ordem_niveis, tabela$categoria)
        extras <- setdiff(tabela$categoria, niveis_presentes)
        niveis_finais <- c(niveis_presentes, extras)
        tabela <- tabela %>%
          mutate(
            pct = 100 * n / total_n,
            cat_fator = factor(categoria, levels = rev(niveis_finais)),
            dica = paste0(
              "<b>", categoria, "</b><br>",
              "Participantes: ", fmt_int(n), "<br>",
              "Percentual: ", fmt_pct(pct)
            )
          )
      } else {
        tabela <- tabela %>%
          mutate(
            pct = 100 * n / total_n,
            cat_fator = reorder(categoria, pct),
            dica = paste0(
              "<b>", categoria, "</b><br>",
              "Participantes: ", fmt_int(n), "<br>",
              "Percentual: ", fmt_pct(pct)
            )
          )
      }
      
      p <- ggplot(tabela, aes(x = pct, y = cat_fator, text = dica)) +
        geom_col(fill = cor_barra, width = 0.68) +
        scale_x_continuous(
          labels = function(x) paste0(x, "%"),
          expand = expansion(mult = c(0, 0.08))
        ) +
        labs(x = "Percentual de participantes", y = NULL) +
        tema_base
      
      montar_plotly(p, mostrar_legenda = FALSE)
    }
    
    grafico_responsaveis_duplo <- function(dados, cols_mae, cols_pai, ordem_niveis = NULL) {
      col_m <- intersect(cols_mae, names(dados))[1]
      col_p <- intersect(cols_pai, names(dados))[1]
      validate(need(!is.na(col_m) && !is.na(col_p), "Colunas de responsáveis não encontradas na base."))
      
      df_m <- dados %>%
        filter(!is.na(.data[[col_m]]), nzchar(as.character(.data[[col_m]]))) %>%
        count(categoria = as.character(.data[[col_m]]), name = "n") %>%
        mutate(responsavel = "Mãe", pct = 100 * n / sum(n))
      
      df_p <- dados %>%
        filter(!is.na(.data[[col_p]]), nzchar(as.character(.data[[col_p]]))) %>%
        count(categoria = as.character(.data[[col_p]]), name = "n") %>%
        mutate(responsavel = "Pai", pct = 100 * n / sum(n))
      
      tabela <- bind_rows(df_m, df_p)
      validate(need(nrow(tabela) > 0, "Sem dados de responsáveis para este filtro."))
      
      if (!is.null(ordem_niveis)) {
        niveis_presentes <- intersect(ordem_niveis, unique(tabela$categoria))
        extras <- setdiff(unique(tabela$categoria), niveis_presentes)
        niveis_finais <- c(niveis_presentes, extras)
        tabela <- tabela %>%
          mutate(cat_fator = factor(categoria, levels = rev(niveis_finais)))
      } else {
        ordem_media <- tabela %>%
          group_by(categoria) %>%
          summarise(m_pct = mean(pct), .groups = "drop") %>%
          arrange(m_pct) %>%
          pull(categoria)
        tabela <- tabela %>%
          mutate(cat_fator = factor(categoria, levels = ordem_media))
      }
      
      tabela <- tabela %>%
        mutate(
          dica = paste0(
            "<b>", categoria, "</b> (", responsavel, ")<br>",
            "Participantes: ", fmt_int(n), "<br>",
            "Percentual: ", fmt_pct(pct)
          )
        )
      
      p <- ggplot(tabela, aes(x = pct, y = cat_fator, fill = responsavel, text = dica)) +
        geom_col(position = position_dodge(width = 0.72), width = 0.62) +
        scale_fill_manual(values = c("Mãe" = cor_sec, "Pai" = cor_prim)) +
        scale_x_continuous(
          labels = function(x) paste0(x, "%"),
          expand = expansion(mult = c(0, 0.08))
        ) +
        labs(x = "Percentual de participantes", y = NULL, fill = NULL) +
        tema_base +
        theme(legend.position = "bottom")
      
      montar_plotly(p, mostrar_legenda = TRUE)
    }
    
    plot_esc_resp_obj <- reactive({
      grafico_responsaveis_duplo(dados_q(), "Q002", "Q001", ordem_niveis = unname(esc))
    })
    
    plot_ocup_resp_obj <- reactive({
      grafico_responsaveis_duplo(dados_q(), "Q004", "Q003", ordem_niveis = unname(ocup))
    })
    
    plot_renda_obj <- reactive({
      grafico_barras_pct(dados_q(), "Q007", cor_prim, unname(renda))
    })
    
    plot_moradores_obj <- reactive({
      grafico_barras_pct(dados_q(), "Q005", cor_sec, as.character(1:20))
    })
    
    plot_internet_obj <- reactive({
      grafico_barras_pct(dados_q(), "Q020", cor_prim, unname(wifi))
    })
    
    plot_computador_obj <- reactive({
      grafico_barras_pct(dados_q(), "Q021", cor_sec, unname(comp))
    })
    
    plot_transporte_obj <- reactive({
      grafico_barras_pct(dados_q(), "Q011", cor_dest, unname(carro))
    })
    
    plot_moto_obj <- reactive({
      grafico_barras_pct(dados_q(), "Q012", cor_sec, unname(moto))
    })
    
    output$plot_esc_resp   <- plotly::renderPlotly({ plot_esc_resp_obj() })
    output$plot_ocup_resp  <- plotly::renderPlotly({ plot_ocup_resp_obj() })
    output$plot_renda      <- plotly::renderPlotly({ plot_renda_obj() })
    output$plot_moradores  <- plotly::renderPlotly({ plot_moradores_obj() })
    output$plot_internet   <- plotly::renderPlotly({ plot_internet_obj() })
    output$plot_computador <- plotly::renderPlotly({ plot_computador_obj() })
    output$plot_transporte <- plotly::renderPlotly({ plot_transporte_obj() })
    
    output$plot_moto  <- plotly::renderPlotly({ plot_moto_obj() })
    output$modal_moto <- plotly::renderPlotly({ plot_moto_obj() })
    
    abrir_modal <- function(titulo, output_modal_id) {
      showModal(
        modalDialog(
          title = titulo,
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Fechar"),
          plotly::plotlyOutput(ns(output_modal_id), width = "100%", height = "560px")
        )
      )
    }
    
    output$modal_esc_resp   <- plotly::renderPlotly({ plot_esc_resp_obj() })
    output$modal_ocup_resp  <- plotly::renderPlotly({ plot_ocup_resp_obj() })
    output$modal_renda      <- plotly::renderPlotly({ plot_renda_obj() })
    output$modal_moradores  <- plotly::renderPlotly({ plot_moradores_obj() })
    output$modal_internet   <- plotly::renderPlotly({ plot_internet_obj() })
    output$modal_computador <- plotly::renderPlotly({ plot_computador_obj() })
    output$modal_transporte <- plotly::renderPlotly({ plot_transporte_obj() })
    
    observeEvent(input$expand_esc_resp,   { abrir_modal("Escolaridade dos responsáveis", "modal_esc_resp") })
    observeEvent(input$expand_ocup_resp,  { abrir_modal("Grupo ocupacional dos responsáveis", "modal_ocup_resp") })
    observeEvent(input$expand_renda,      { abrir_modal("Renda familiar mensal", "modal_renda") })
    observeEvent(input$expand_moradores,  { abrir_modal("Moradores no domicílio", "modal_moradores") })
    observeEvent(input$expand_internet,   { abrir_modal("Acesso à internet", "modal_internet") })
    observeEvent(input$expand_computador, { abrir_modal("Acesso a computador", "modal_computador") })
    observeEvent(input$expand_transporte, { abrir_modal("Carro no domicílio", "modal_transporte") })
    observeEvent(input$expand_moto,       { abrir_modal("Motocicleta no domicílio", "modal_moto") })
  })
}