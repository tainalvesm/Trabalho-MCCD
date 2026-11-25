desempenhoUI <- function(id) {
  ns <- NS(id)
  
  card_grafico <- function(titulo, subtitulo, output_id, btn_id, height = "360px", extra_ui = NULL) {
    plot_dom_id <- ns(output_id)
    nome_arquivo <- gsub("[^a-zA-Z0-9_-]", "_", tolower(titulo))
    
    div(
      class = "content-card chart-card",
      style = "overflow: hidden;",
      div(
        style = "display: flex; justify-content: space-between; align-items: flex-start; gap: 12px; margin-bottom: 6px; flex-wrap: wrap;",
        div(
          h3(style = "margin: 0; font-size: 1.05rem; font-weight: 700; color: #34383D;", titulo),
          tags$p(style = "margin: 2px 0 0 0; font-size: 0.82rem; color: #929292;", subtitulo)
        ),
        extra_ui
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
                  "Presença e ausência nas provas",
                  "Distribuição de presentes, faltosos e eliminados (0 = Faltou, 1 = Presente, 2 = Eliminado)",
                  "plot_presenca",
                  "expand_presenca",
                  "370px",
                  extra_ui = radioButtons(
                    ns("modo_presenca"),
                    label = NULL,
                    inline = TRUE,
                    choices = c(
                      "Quantidade de participantes" = "qtd",
                      "Nota média (presentes)" = "media"
                    ),
                    selected = "qtd"
                  )
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
                  selectizeInput(
                    ns("mun_esq"),
                    label = "Município 1",
                    choices = c("Selecione um município" = ""),
                    selected = "",
                    options = list(placeholder = "Digite ou selecione um município...")
                  ),
                  uiOutput(ns("resumo_esq"))
                )
              ),
              column(
                6,
                div(
                  class = "content-card",
                  selectizeInput(
                    ns("mun_dir"),
                    label = "Município 2",
                    choices = c("Selecione um município" = ""),
                    selected = "",
                    options = list(placeholder = "Digite ou selecione um município...")
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

desempenhoServer <- function(id, resultados, participantes = NULL, resultados_todos = NULL, participantes_todos = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    cor_prim <- if (exists("cor_principal", envir = .GlobalEnv)) get("cor_principal", envir = .GlobalEnv) else "#A6535D"
    cor_sec  <- if (exists("cor_secundaria", envir = .GlobalEnv)) get("cor_secundaria", envir = .GlobalEnv) else "#668596"
    cor_dest <- if (exists("cor_destaque", envir = .GlobalEnv)) get("cor_destaque", envir = .GlobalEnv) else "#C98D91"
    cor_ar   <- if (exists("cor_areia", envir = .GlobalEnv)) get("cor_areia", envir = .GlobalEnv) else "#DCCDC0"
    cor_txt  <- if (exists("cor_texto", envir = .GlobalEnv)) get("cor_texto", envir = .GlobalEnv) else "#34383D"
    
    cols_areas <- c(
      "Ciências da Natureza" = "NU_NOTA_CN",
      "Ciências Humanas"     = "NU_NOTA_CH",
      "Linguagens"           = "NU_NOTA_LC",
      "Matemática"           = "NU_NOTA_MT",
      "Redação"              = "NU_NOTA_REDACAO"
    )
    
    achar_coluna <- function(df, candidatos) {
      if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) return(NA_character_)
      nomes_reais <- names(df)
      nomes_lower <- tolower(nomes_reais)
      for (cand in tolower(candidatos)) {
        idx <- match(cand, nomes_lower)
        if (!is.na(idx)) return(nomes_reais[idx])
      }
      NA_character_
    }
    
    get_df_completo <- reactive({
      if (!is.null(resultados_todos)) {
        return(resultados_todos())
      }
      if (exists("bases", inherits = TRUE)) {
        b <- get("bases", inherits = TRUE)
        if (is.list(b) && !is.null(b$resultados)) {
          return(b$resultados)
        }
      }
      resultados()
    })
    
    get_part_completo <- reactive({
      if (!is.null(participantes_todos)) {
        return(participantes_todos())
      }
      if (exists("bases", inherits = TRUE)) {
        b <- get("bases", inherits = TRUE)
        if (is.list(b) && !is.null(b$participantes)) {
          return(b$participantes)
        }
      }
      if (!is.null(participantes)) return(participantes())
      NULL
    })
    
    fmt_nota <- function(x) {
      if (is.null(x) || length(x) == 0 || is.na(x) || is.nan(x) || is.infinite(x)) {
        return("—")
      }
      format(round(x, 1), decimal.mark = ",", nsmall = 1, trim = TRUE)
    }
    
    fmt_int <- function(x) {
      if (is.null(x) || length(x) == 0 || is.na(x) || is.nan(x) || is.infinite(x)) {
        return("0")
      }
      format(as.integer(round(x)), big.mark = ".", decimal.mark = ",", trim = TRUE, scientific = FALSE)
    }
    
    construir_grafico_barras <- function(long_df, col_grupo, niveis_grupo, paleta_cores,
                                         y_col = "media", y_titulo = "Nota média",
                                         y_limite = c(0, 1000), tipo_valor = "nota") {
      areas_ord <- names(cols_areas)
      p <- plotly::plot_ly()
      
      for (grp in niveis_grupo) {
        sub_g <- long_df[long_df[[col_grupo]] == grp, , drop = FALSE]
        if (nrow(sub_g) == 0) next
        
        idx <- match(areas_ord, sub_g$area)
        y_vals <- sub_g[[y_col]][idx]
        n_vals <- if ("n_val" %in% names(sub_g)) sub_g$n_val[idx] else rep(NA_integer_, length(areas_ord))
        m_vals <- if ("media" %in% names(sub_g)) sub_g$media[idx] else rep(NA_real_, length(areas_ord))
        
        if (tipo_valor == "qtd") {
          dicas <- ifelse(
            is.na(y_vals),
            paste0("<b>", grp, "</b> — ", areas_ord, "<br>Participantes: 0"),
            ifelse(
              is.na(m_vals),
              paste0("<b>", grp, "</b> — ", areas_ord, "<br>Participantes: <b>", vapply(y_vals, fmt_int, character(1)), "</b>"),
              paste0(
                "<b>", grp, "</b> — ", areas_ord, "<br>",
                "Participantes: <b>", vapply(y_vals, fmt_int, character(1)), "</b><br>",
                "Nota média: ", vapply(m_vals, fmt_nota, character(1))
              )
            )
          )
        } else {
          dicas <- ifelse(
            is.na(y_vals),
            paste0("<b>", grp, "</b> — ", areas_ord, "<br>Sem notas válidas"),
            paste0(
              "<b>", grp, "</b> — ", areas_ord, "<br>",
              "Nota média: <b>", vapply(y_vals, fmt_nota, character(1)), "</b><br>",
              "Avaliações: ", vapply(n_vals, fmt_int, character(1))
            )
          )
        }
        
        cor_grp <- if (!is.null(paleta_cores) && grp %in% names(paleta_cores)) {
          unname(paleta_cores[[grp]])
        } else {
          cor_prim
        }
        
        p <- plotly::add_trace(
          p,
          x = areas_ord,
          y = y_vals,
          type = "bar",
          name = grp,
          text = dicas,
          hoverinfo = "text",
          textposition = "none",
          marker = list(color = cor_grp)
        )
      }
      
      plotly::layout(
        p,
        barmode = "group",
        bargap = 0.25,
        bargroupgap = 0.08,
        autosize = TRUE,
        xaxis = list(
          title = "",
          categoryorder = "array",
          categoryarray = areas_ord,
          tickfont = list(color = cor_txt, size = 12)
        ),
        yaxis = list(
          title = y_titulo,
          range = y_limite,
          gridcolor = "#EFEFEF",
          tickfont = list(color = cor_txt, size = 11)
        ),
        margin = list(t = 14, r = 20, b = 60, l = 60),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.20),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
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
      req(is.data.frame(df))
      
      valores <- sapply(cols_areas, function(col) {
        col_real <- achar_coluna(df, col)
        if (!is.na(col_real)) mean(suppressWarnings(as.numeric(df[[col_real]])), na.rm = TRUE) else NA_real_
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
    
    obter_vetor_rede <- function(df) {
      candidatos_dep <- c(
        "dependencia_adminstrativa",
        "dependencia_administrativa",
        "TP_DEPENDENCIA_ADM_ESC",
        "tp_dependencia_adm_esc",
        "dependencia",
        "rede_ensino",
        "rede"
      )
      col_dep <- achar_coluna(df, candidatos_dep)
      if (!is.na(col_dep)) {
        return(trimws(as.character(df[[col_dep]])))
      }
      
      df_part <- get_part_completo()
      if (!is.null(df_part) && is.data.frame(df_part)) {
        col_dep_p <- achar_coluna(df_part, candidatos_dep)
        if (!is.na(col_dep_p)) {
          k_df <- achar_coluna(df, c("NU_INSCRICAO", "NU_SEQUENCIAL", "id"))
          k_pt <- achar_coluna(df_part, c("NU_INSCRICAO", "NU_SEQUENCIAL", "id"))
          if (!is.na(k_df) && !is.na(k_pt)) {
            idx <- match(df[[k_df]], df_part[[k_pt]])
            return(trimws(as.character(df_part[[col_dep_p]][idx])))
          } else if (nrow(df) == nrow(df_part)) {
            return(trimws(as.character(df_part[[col_dep_p]])))
          }
        }
      }
      rep(NA_character_, nrow(df))
    }
    
    plot_rede_obj <- reactive({
      df <- resultados()
      req(is.data.frame(df))
      
      rede_vec <- obter_vetor_rede(df)
      mapa_cod_dep <- c("1" = "Federal", "2" = "Estadual", "3" = "Municipal", "4" = "Privada")
      rede_vec <- ifelse(rede_vec %in% names(mapa_cod_dep), mapa_cod_dep[rede_vec], rede_vec)
      
      validos <- !is.na(rede_vec) & nzchar(rede_vec) & !(tolower(rede_vec) %in% c("na", "nan", "null"))
      validate(need(any(validos), "Sem dados de rede de ensino disponíveis para este filtro."))
      
      df_limpo <- df[validos, , drop = FALSE]
      df_limpo$rede <- rede_vec[validos]
      
      long_df <- lapply(names(cols_areas), function(area_label) {
        col_nome <- achar_coluna(df_limpo, cols_areas[[area_label]])
        if (is.na(col_nome)) return(NULL)
        notas <- suppressWarnings(as.numeric(df_limpo[[col_nome]]))
        tapply_m <- tapply(notas, df_limpo$rede, function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE))
        tapply_n <- tapply(notas, df_limpo$rede, function(x) sum(!is.na(x)))
        data.frame(
          rede = names(tapply_m),
          media = as.numeric(tapply_m),
          n_val = as.integer(tapply_n),
          area = area_label,
          stringsAsFactors = FALSE
        )
      })
      long_df <- do.call(rbind, long_df)
      long_df <- long_df[!is.na(long_df$media) & !is.nan(long_df$media), , drop = FALSE]
      
      validate(need(nrow(long_df) > 0, "Sem médias válidas por rede de ensino para este filtro."))
      
      redes_presentes <- unique(long_df$rede)
      ordem_pref <- c("Privada", "Federal", "Estadual", "Municipal")
      redes_ord <- c(intersect(ordem_pref, redes_presentes), setdiff(redes_presentes, ordem_pref))
      
      cores_fixas <- c(
        "Privada"       = cor_prim,
        "Estadual"      = cor_sec,
        "Federal"       = cor_dest,
        "Municipal"     = cor_ar,
        "Não informada" = "#B0B5BA",
        "Não informado" = "#B0B5BA"
      )
      paleta_extra <- rep(c(cor_prim, cor_sec, cor_dest, cor_ar, "#7F8C8D", "#95A5A6"), length.out = length(redes_ord))
      names(paleta_extra) <- redes_ord
      for (nm in intersect(names(cores_fixas), redes_ord)) {
        paleta_extra[[nm]] <- cores_fixas[[nm]]
      }
      
      construir_grafico_barras(long_df, "rede", redes_ord, paleta_extra, y_col = "media", y_titulo = "Nota média", y_limite = c(0, 1000), tipo_valor = "nota")
    })
    
    plot_presenca_obj <- reactive({
      df <- resultados()
      req(is.data.frame(df))
      
      modo <- if (!is.null(input$modo_presenca)) input$modo_presenca else "qtd"
      col_pres_unica <- achar_coluna(df, c("presenca", "condicao_presenca", "status_presenca"))
      
      mapa_tp_area <- c(
        "Ciências da Natureza" = "TP_PRESENCA_CN",
        "Ciências Humanas"     = "TP_PRESENCA_CH",
        "Linguagens"           = "TP_PRESENCA_LC",
        "Matemática"           = "TP_PRESENCA_MT",
        "Redação"              = "TP_PRESENCA_LC"
      )
      
      mapa_cod_pres <- c(
        "0" = "0 - Faltou à prova",
        "1" = "1 - Presente na prova",
        "2" = "2 - Eliminado na prova"
      )
      
      long_p <- lapply(names(cols_areas), function(area_label) {
        col_nota <- achar_coluna(df, cols_areas[[area_label]])
        
        col_p_usar <- if (!is.na(col_pres_unica)) {
          col_pres_unica
        } else {
          cand_tp <- achar_coluna(df, mapa_tp_area[[area_label]])
          if (!is.na(cand_tp)) cand_tp else achar_coluna(df, c("TP_PRESENCA_CN", "TP_PRESENCA_CH", "TP_PRESENCA_LC", "TP_PRESENCA_MT"))
        }
        
        if (is.na(col_p_usar)) return(NULL)
        
        grp_vec <- trimws(as.character(df[[col_p_usar]]))
        grp_vec <- ifelse(grp_vec %in% names(mapa_cod_pres), mapa_cod_pres[grp_vec], grp_vec)
        
        notas <- if (!is.na(col_nota)) suppressWarnings(as.numeric(df[[col_nota]])) else rep(NA_real_, nrow(df))
        ok <- !is.na(grp_vec) & nzchar(grp_vec) & !(tolower(grp_vec) %in% c("na", "nan", "null"))
        if (!any(ok)) return(NULL)
        
        grps_unicos <- unique(grp_vec[ok])
        do.call(rbind, lapply(grps_unicos, function(g) {
          idx_g <- ok & (grp_vec == g)
          n_part <- sum(idx_g)
          n_notas <- sum(!is.na(notas[idx_g]))
          m_nota <- if (n_notas > 0) mean(notas[idx_g], na.rm = TRUE) else NA_real_
          data.frame(
            grupo_pres = g,
            qtd = as.integer(n_part),
            media = as.numeric(m_nota),
            n_val = as.integer(n_notas),
            area = area_label,
            stringsAsFactors = FALSE
          )
        }))
      })
      
      long_p <- do.call(rbind, long_p)
      validate(need(!is.null(long_p) && nrow(long_p) > 0, "Sem dados de presença disponíveis para este filtro."))
      
      ordem_pres <- c("1 - Presente na prova", "0 - Faltou à prova", "2 - Eliminado na prova")
      grupos_ord <- c(intersect(ordem_pres, unique(long_p$grupo_pres)), setdiff(unique(long_p$grupo_pres), ordem_pres))
      
      paleta_pres <- c(
        "1 - Presente na prova"  = cor_sec,
        "0 - Faltou à prova"     = cor_dest,
        "2 - Eliminado na prova" = cor_prim
      )
      for (g in grupos_ord) {
        if (!g %in% names(paleta_pres)) paleta_pres[[g]] <- "#7F8C8D"
      }
      
      if (modo == "media") {
        long_m <- long_p[!is.na(long_p$media) & !is.nan(long_p$media), , drop = FALSE]
        validate(need(nrow(long_m) > 0, "Sem notas médias válidas por condição de presença para este filtro."))
        construir_grafico_barras(long_m, "grupo_pres", grupos_ord, paleta_pres, y_col = "media", y_titulo = "Nota média", y_limite = c(0, 1000), tipo_valor = "nota")
      } else {
        max_qtd <- max(long_p$qtd, na.rm = TRUE)
        lim_y <- c(0, if (is.finite(max_qtd) && max_qtd > 0) max_qtd * 1.15 else 100)
        construir_grafico_barras(long_p, "grupo_pres", grupos_ord, paleta_pres, y_col = "qtd", y_titulo = "Participantes", y_limite = lim_y, tipo_valor = "qtd")
      }
    })
    
    observe({
      df_all <- get_df_completo()
      req(is.data.frame(df_all))
      
      col_mun <- achar_coluna(df_all, c("municipio_prova", "NO_MUNICIPIO_PROVA", "municipio"))
      if (!is.na(col_mun)) {
        mun_choices <- sort(unique(stats::na.omit(trimws(as.character(df_all[[col_mun]])))))
        mun_choices <- mun_choices[nzchar(mun_choices)]
        opcoes <- c("Selecione um município" = "", stats::setNames(mun_choices, mun_choices))
        
        sel_esq <- isolate(input$mun_esq)
        sel_esq <- if (!is.null(sel_esq) && nzchar(sel_esq) && sel_esq %in% mun_choices) sel_esq else ""
        
        sel_dir <- isolate(input$mun_dir)
        sel_dir <- if (!is.null(sel_dir) && nzchar(sel_dir) && sel_dir %in% mun_choices) sel_dir else ""
        
        updateSelectizeInput(session, "mun_esq", choices = opcoes, selected = sel_esq, server = FALSE)
        updateSelectizeInput(session, "mun_dir", choices = opcoes, selected = sel_dir, server = FALSE)
      }
    })
    
    montar_resumo_mun <- function(mun_nome, cor_destaque_mun) {
      if (is.null(mun_nome) || !nzchar(mun_nome)) {
        return(
          div(
            style = "margin-top: 10px; font-size: 0.96rem; color: #929292;",
            "Selecione um município acima para visualizar os indicadores."
          )
        )
      }
      
      df_all <- get_df_completo()
      req(is.data.frame(df_all))
      col_mun <- achar_coluna(df_all, c("municipio_prova", "NO_MUNICIPIO_PROVA", "municipio"))
      validate(need(!is.na(col_mun), "Coluna de município indisponível."))
      
      mask_mun <- !is.na(df_all[[col_mun]]) & trimws(as.character(df_all[[col_mun]])) == mun_nome
      sub <- df_all[mask_mun, , drop = FALSE]
      
      df_part <- get_part_completo()
      col_id_part_sub <- achar_coluna(sub, c("NU_INSCRICAO", "NU_SEQUENCIAL"))
      n_inscritos <- if (!is.na(col_id_part_sub)) {
        length(unique(stats::na.omit(sub[[col_id_part_sub]])))
      } else {
        nrow(sub)
      }
      
      if (!is.null(df_part) && is.data.frame(df_part)) {
        col_mun_p <- achar_coluna(df_part, c("municipio_prova", "NO_MUNICIPIO_PROVA", "municipio"))
        if (!is.na(col_mun_p)) {
          sub_p <- df_part[!is.na(df_part[[col_mun_p]]) & trimws(as.character(df_part[[col_mun_p]])) == mun_nome, , drop = FALSE]
          col_id_p <- achar_coluna(sub_p, c("NU_INSCRICAO", "NU_SEQUENCIAL"))
          if (!is.na(col_id_p)) {
            n_inscritos <- length(unique(stats::na.omit(sub_p[[col_id_p]])))
          } else {
            n_inscritos <- nrow(sub_p)
          }
        }
      }
      
      rede_sub <- obter_vetor_rede(sub)
      col_id_esc <- achar_coluna(sub, c("co_escola", "co_school", "CO_ESCOLA", "CO_ENTIDADE", "codigo_escola", "NO_ENTIDADE", "escola", "nome_escola"))
      
      if (!is.na(col_id_esc)) {
        id_esc_vec <- trimws(as.character(sub[[col_id_esc]]))
        ok_esc <- !is.na(id_esc_vec) & nzchar(id_esc_vec) & !(tolower(id_esc_vec) %in% c("na", "nan", "null", "não informada", "nao informada"))
        esc_unicas <- data.frame(
          id_esc = id_esc_vec[ok_esc],
          rede = tolower(rede_sub[ok_esc]),
          stringsAsFactors = FALSE
        )
        esc_unicas <- esc_unicas[!duplicated(esc_unicas$id_esc), , drop = FALSE]
        n_esc_total <- nrow(esc_unicas)
        dep_vec <- esc_unicas$rede
      } else {
        n_esc_total <- NA_integer_
        dep_vec <- character(0)
      }
      
      n_est  <- sum(grepl("estadual|^2$", dep_vec), na.rm = TRUE)
      n_fed  <- sum(grepl("federal|^1$", dep_vec), na.rm = TRUE)
      n_priv <- sum(grepl("privad|^4$", dep_vec), na.rm = TRUE)
      n_mun  <- sum(grepl("municipal|^3$", dep_vec), na.rm = TRUE)
      
      calc_media <- function(col_n) {
        col_r <- achar_coluna(sub, col_n)
        if (!is.na(col_r)) mean(suppressWarnings(as.numeric(sub[[col_r]])), na.rm = TRUE) else NA_real_
      }
      
      m_cn  <- calc_media("NU_NOTA_CN")
      m_ch  <- calc_media("NU_NOTA_CH")
      m_lc  <- calc_media("NU_NOTA_LC")
      m_mt  <- calc_media("NU_NOTA_MT")
      m_red <- calc_media("NU_NOTA_REDACAO")
      m_ger <- mean(c(m_cn, m_ch, m_lc, m_mt, m_red), na.rm = TRUE)
      
      txt_esc_total <- if (is.na(n_esc_total)) "—" else fmt_int(n_esc_total)
      
      div(
        style = sprintf("margin-top: 12px; border-top: 3px solid %s; padding-top: 12px; font-size: 1rem; color: #34383D;", cor_destaque_mun),
        
        div(
          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin-bottom: 12px;",
          div(
            style = "background: #F9FAFB; padding: 10px 12px; border-radius: 6px; border: 1px solid #ECECEC;",
            span("Número de inscritos", style = "display: block; font-size: 0.85rem; color: #666C73; margin-bottom: 2px;"),
            strong(fmt_int(n_inscritos), style = "font-size: 1.25rem; color: #22252A;")
          ),
          div(
            style = "background: #F9FAFB; padding: 10px 12px; border-radius: 6px; border: 1px solid #ECECEC;",
            span("Escolas identificadas", style = "display: block; font-size: 0.85rem; color: #666C73; margin-bottom: 2px;"),
            strong(txt_esc_total, style = "font-size: 1.25rem; color: #22252A;")
          )
        ),
        
        div(
          style = "margin-bottom: 12px;",
          span("Escolas por rede de ensino", style = "display: block; font-weight: 700; font-size: 0.92rem; color: #444950; margin-bottom: 6px;"),
          div(
            style = "display: grid; grid-template-columns: repeat(2, 1fr); gap: 6px 14px; font-size: 0.96rem;",
            div(span("Estadual: ", style = "color: #555;"), strong(if (is.na(n_esc_total)) "—" else fmt_int(n_est))),
            div(span("Federal: ", style = "color: #555;"), strong(if (is.na(n_esc_total)) "—" else fmt_int(n_fed))),
            div(span("Privada: ", style = "color: #555;"), strong(if (is.na(n_esc_total)) "—" else fmt_int(n_priv))),
            div(span("Municipal: ", style = "color: #555;"), strong(if (is.na(n_esc_total)) "—" else fmt_int(n_mun)))
          )
        ),
        
        div(
          style = "border-top: 1px dashed #E0E0E0; padding-top: 10px;",
          span("Notas médias por competência", style = "display: block; font-weight: 700; font-size: 0.92rem; color: #444950; margin-bottom: 6px;"),
          div(
            style = "display: flex; flex-direction: column; gap: 5px; font-size: 0.96rem;",
            div(style = "display: flex; justify-content: space-between; font-weight: 700; color: #22252A;", span("Média Geral:"), span(fmt_nota(m_ger))),
            div(style = "display: flex; justify-content: space-between;", span("Redação:"), strong(fmt_nota(m_red))),
            div(style = "display: flex; justify-content: space-between;", span("Ciências da Natureza:"), strong(fmt_nota(m_cn))),
            div(style = "display: flex; justify-content: space-between;", span("Matemática:"), strong(fmt_nota(m_mt))),
            div(style = "display: flex; justify-content: space-between;", span("Ciências Humanas:"), strong(fmt_nota(m_ch))),
            div(style = "display: flex; justify-content: space-between;", span("Linguagens:"), strong(fmt_nota(m_lc)))
          )
        )
      )
    }
    
    output$resumo_esq <- renderUI({ montar_resumo_mun(input$mun_esq, cor_prim) })
    output$resumo_dir <- renderUI({ montar_resumo_mun(input$mun_dir, cor_sec) })
    
    plot_comparacao_obj <- reactive({
      muns_sel <- c(input$mun_esq, input$mun_dir)
      muns_sel <- unique(muns_sel[!is.null(muns_sel) & nzchar(muns_sel)])
      
      validate(need(length(muns_sel) > 0, "Selecione pelo menos um município para visualizar a comparação."))
      
      df_all <- get_df_completo()
      req(is.data.frame(df_all))
      col_mun <- achar_coluna(df_all, c("municipio_prova", "NO_MUNICIPIO_PROVA", "municipio"))
      validate(need(!is.na(col_mun), "Coluna de município indisponível."))
      
      mun_vec <- trimws(as.character(df_all[[col_mun]]))
      sub <- df_all[!is.na(mun_vec) & mun_vec %in% muns_sel, , drop = FALSE]
      sub$.mun <- trimws(as.character(sub[[col_mun]]))
      
      long_c <- lapply(names(cols_areas), function(area_label) {
        col_nome <- achar_coluna(sub, cols_areas[[area_label]])
        if (is.na(col_nome)) return(NULL)
        notas <- suppressWarnings(as.numeric(sub[[col_nome]]))
        tapply_m <- tapply(notas, sub$.mun, function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE))
        tapply_n <- tapply(notas, sub$.mun, function(x) sum(!is.na(x)))
        data.frame(
          .mun = names(tapply_m),
          media = as.numeric(tapply_m),
          n_val = as.integer(tapply_n),
          area = area_label,
          stringsAsFactors = FALSE
        )
      })
      long_c <- do.call(rbind, long_c)
      long_c <- long_c[!is.na(long_c$media) & !is.nan(long_c$media), , drop = FALSE]
      
      validate(need(nrow(long_c) > 0, "Sem notas válidas para os municípios selecionados."))
      
      paleta_mun <- c(cor_prim, cor_sec)[seq_along(muns_sel)]
      names(paleta_mun) <- muns_sel
      
      construir_grafico_barras(long_c, ".mun", muns_sel, paleta_mun, y_col = "media", y_titulo = "Nota média", y_limite = c(0, 1000), tipo_valor = "nota")
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
    observeEvent(input$expand_presenca,   { abrir_modal("Presença e ausência nas provas", "modal_presenca") })
    observeEvent(input$expand_comparacao, { abrir_modal("Comparação de médias entre municípios", "modal_comparacao") })
  })
}