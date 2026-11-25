# Malha dos municípios de Alagoas (mantida aqui pois é usada no mod_inicio.R)
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
      p("Ranking de desempenho dos municípios de Alagoas no ENEM 2024.")
    ),
    
    div(
      class = "content-card",
      
      # Barra de filtros: seletor de área e busca digitável
      fluidRow(
        column(
          6,
          selectInput(
            inputId = ns("area_ranking"),
            label = "Ordenar ranking por área do conhecimento",
            choices = c(
              "Média geral" = "media_geral",
              "Redação" = "NU_NOTA_REDACAO",
              "Matemática" = "NU_NOTA_MT",
              "Linguagens" = "NU_NOTA_LC",
              "Ciências Humanas" = "NU_NOTA_CH",
              "Ciências da Natureza" = "NU_NOTA_CN"
            ),
            selected = "media_geral",
            width = "100%"
          )
        ),
        column(
          6,
          textInput(
            inputId = ns("busca_municipio"),
            label = "Buscar município",
            value = "",
            placeholder = "Digite o nome do município...",
            width = "100%"
          )
        )
      ),
      
      div(
        style = "margin-top: 8px;",
        uiOutput(ns("tabela_ranking_ui"))
      )
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
    
    normalizar_texto <- function(x) {
      x_utf8 <- enc2utf8(as.character(x))
      sem_acento <- iconv(x_utf8, from = "UTF-8", to = "ASCII//TRANSLIT")
      tolower(trimws(ifelse(is.na(sem_acento), x_utf8, sem_acento)))
    }
    
    fmt_nota <- function(x) {
      ifelse(
        is.na(x) | is.nan(x) | is.infinite(x),
        "—",
        format(round(x, 1), decimal.mark = ",", nsmall = 1, trim = TRUE)
      )
    }
    
    fmt_int <- function(x) {
      ifelse(
        is.na(x),
        "0",
        format(as.integer(round(x)), big.mark = ".", decimal.mark = ",", trim = TRUE, scientific = FALSE)
      )
    }
    
    ranking_base <- reactive({
      p <- participantes()
      r <- resultados()
      
      inscritos <- p %>%
        filter(!is.na(municipio_prova), nzchar(trimws(municipio_prova))) %>%
        count(municipio_prova, name = "inscritos")
      
      notas <- r %>%
        filter(!is.na(municipio_prova), nzchar(trimws(municipio_prova))) %>%
        group_by(municipio_prova) %>%
        summarise(
          NU_NOTA_CN = mean(suppressWarnings(as.numeric(NU_NOTA_CN)), na.rm = TRUE),
          NU_NOTA_CH = mean(suppressWarnings(as.numeric(NU_NOTA_CH)), na.rm = TRUE),
          NU_NOTA_LC = mean(suppressWarnings(as.numeric(NU_NOTA_LC)), na.rm = TRUE),
          NU_NOTA_MT = mean(suppressWarnings(as.numeric(NU_NOTA_MT)), na.rm = TRUE),
          NU_NOTA_REDACAO = mean(suppressWarnings(as.numeric(NU_NOTA_REDACAO)), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          across(starts_with("NU_NOTA_"), ~ ifelse(is.nan(.x), NA_real_, .x)),
          media_geral = rowMeans(
            cbind(NU_NOTA_CN, NU_NOTA_CH, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO),
            na.rm = TRUE
          ),
          media_geral = ifelse(is.nan(media_geral), NA_real_, media_geral)
        )
      
      inscritos %>%
        left_join(notas, by = "municipio_prova")
    })
    
    output$tabela_ranking_ui <- renderUI({
      df <- ranking_base()
      req(nrow(df) > 0)
      
      col_ord <- if (!is.null(input$area_ranking) && input$area_ranking %in% names(df)) {
        input$area_ranking
      } else {
        "media_geral"
      }
      
      # Ordena do maior para o menor conforme a área selecionada (padrão: média geral)
      df_ord <- df %>%
        arrange(desc(.data[[col_ord]]), desc(media_geral), desc(inscritos), municipio_prova) %>%
        mutate(posicao = row_number())
      
      # Filtro por busca digitada (mantém a posição original do município no ranking)
      termo <- if (!is.null(input$busca_municipio)) normalizar_texto(input$busca_municipio) else ""
      if (nzchar(termo)) {
        df_ord <- df_ord %>%
          filter(grepl(termo, normalizar_texto(municipio_prova), fixed = TRUE))
      }
      
      if (nrow(df_ord) == 0) {
        return(
          div(
            style = "padding: 24px; text-align: center; color: #929292; font-size: 0.95rem;",
            "Nenhum município encontrado para a busca informada."
          )
        )
      }
      
      estilo_th <- function(col_id, alinhamento = "right") {
        destaque <- identical(col_id, col_ord)
        sprintf(
          "text-align: %s; padding: 10px 12px; border-bottom: 2px solid %s; color: %s; background-color: %s; font-weight: 700; white-space: nowrap;",
          alinhamento,
          if (destaque) "#A6535D" else "#E5E7EB",
          if (destaque) "#A6535D" else "#34383D",
          if (destaque) "#FDF8F8" else "#F9FAFB"
        )
      }
      
      estilo_td <- function(col_id, alinhamento = "right", negrito = FALSE) {
        destaque <- identical(col_id, col_ord)
        sprintf(
          "text-align: %s; padding: 9px 12px; border-bottom: 1px solid #F0F0F0; background-color: %s; color: %s; font-weight: %s;",
          alinhamento,
          if (destaque) "#FDF8F8" else "transparent",
          if (destaque) "#A6535D" else "#34383D",
          if (destaque || negrito) "700" else "400"
        )
      }
      
      linhas <- lapply(seq_len(nrow(df_ord)), function(i) {
        row <- df_ord[i, ]
        pos <- row$posicao
        
        badge_pos <- if (pos <= 3) {
          cor_bg <- if (pos == 1) "#A6535D" else if (pos == 2) "#668596" else "#C98D91"
          tags$span(
            style = sprintf(
              "display: inline-block; min-width: 26px; padding: 2px 7px; border-radius: 12px; background: %s; color: #FFFFFF; font-weight: 700; font-size: 0.82rem; text-align: center;",
              cor_bg
            ),
            paste0(pos, "º")
          )
        } else {
          tags$span(
            style = "display: inline-block; min-width: 26px; color: #666C73; font-weight: 600; font-size: 0.88rem; text-align: center;",
            paste0(pos, "º")
          )
        }
        
        tags$tr(
          tags$td(style = "text-align: center; padding: 9px 10px; border-bottom: 1px solid #F0F0F0;", badge_pos),
          tags$td(style = "text-align: left; padding: 9px 12px; border-bottom: 1px solid #F0F0F0; font-weight: 600; color: #22252A;", row$municipio_prova),
          tags$td(style = "text-align: right; padding: 9px 12px; border-bottom: 1px solid #F0F0F0; color: #555;", fmt_int(row$inscritos)),
          tags$td(style = estilo_td("media_geral", "right", negrito = TRUE), fmt_nota(row$media_geral)),
          tags$td(style = estilo_td("NU_NOTA_REDACAO", "right"), fmt_nota(row$NU_NOTA_REDACAO)),
          tags$td(style = estilo_td("NU_NOTA_MT", "right"), fmt_nota(row$NU_NOTA_MT)),
          tags$td(style = estilo_td("NU_NOTA_LC", "right"), fmt_nota(row$NU_NOTA_LC)),
          tags$td(style = estilo_td("NU_NOTA_CH", "right"), fmt_nota(row$NU_NOTA_CH)),
          tags$td(style = estilo_td("NU_NOTA_CN", "right"), fmt_nota(row$NU_NOTA_CN))
        )
      })
      
      div(
        class = "tabela-rolagem",
        style = "max-height: 560px; overflow-y: auto; border: 1px solid #ECECEC; border-radius: 8px;",
        tags$table(
          style = "width: 100%; border-collapse: collapse; font-size: 0.92rem;",
          tags$thead(
            style = "position: sticky; top: 0; z-index: 2;",
            tags$tr(
              tags$th(style = estilo_th("posicao", "center"), "#"),
              tags$th(style = estilo_th("municipio_prova", "left"), "Município"),
              tags$th(style = estilo_th("inscritos", "right"), "Inscritos"),
              tags$th(style = estilo_th("media_geral", "right"), "Média geral"),
              tags$th(style = estilo_th("NU_NOTA_REDACAO", "right"), "Redação"),
              tags$th(style = estilo_th("NU_NOTA_MT", "right"), "Matemática"),
              tags$th(style = estilo_th("NU_NOTA_LC", "right"), "Linguagens"),
              tags$th(style = estilo_th("NU_NOTA_CH", "right"), "C. Humanas"),
              tags$th(style = estilo_th("NU_NOTA_CN", "right"), "C. Natureza")
            )
          ),
          tags$tbody(linhas)
        )
      )
    })
  })
}