perfilUI <- function(id) {
  
  ns <- NS(id)
  
  card_grafico <- function(titulo, subtitulo, output_id, btn_id, height = "340px") {
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
      span(class = "eyebrow", "DIMENSÃO DEMOGRÁFICA"),
      h1("Perfil dos participantes"),
      p("Quem são os inscritos no ENEM 2024 em Alagoas?")
    ),
    
    tabsetPanel(
      type = "pills",
      tabPanel(
        "Sexo e raça/cor",
        fluidRow(
          column(6, card_grafico("Distribuição por sexo", "Inscritos no ENEM 2024", "sexo", "expand_sexo", "340px")),
          column(6, card_grafico("Distribuição por raça/cor", "Autodeclaração dos participantes", "raca", "expand_raca", "340px"))
        )
      ),
      tabPanel(
        "Faixa etária",
        fluidRow(
          column(12, card_grafico("Distribuição por faixa etária", "Idade dos participantes", "faixa_etaria", "expand_faixa", "500px"))
        )
      ),
      tabPanel(
        "Escolaridade",
        fluidRow(
          column(12, card_grafico("Situação de conclusão do Ensino Médio", "Etapa de escolaridade dos participantes", "conclusao", "expand_conclusao", "320px"))
        ),
        fluidRow(
          column(12, card_grafico("Condição de treineiro", "Participantes que realizam o exame como treino", "treineiros", "expand_treineiros", "300px"))
        )
      )
    )
  )
}


perfilServer <- function(id, participantes) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    montar_plotly <- function(p) {
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(
          autosize = TRUE,
          margin = list(t = 12, r = 20, b = 45, l = 10),
          showlegend = FALSE,
          hoverlabel = list(
            bgcolor = "white",
            bordercolor = cor_principal,
            font = list(color = cor_texto, size = 12)
          )
        ) %>%
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    }
    
    plot_sexo <- reactive({
      dados <- participantes()
      
      tabela <- dados %>%
        filter(!is.na(sexo)) %>%
        count(sexo, name = "n") %>%
        mutate(
          sexo_nome = recode(sexo, "F" = "Feminino", "M" = "Masculino"),
          percentual = 100 * n / sum(n),
          dica = paste0(
            "<b>", sexo_nome, "</b><br>",
            "Inscritos: ", format(n, big.mark = ".", decimal.mark = ","), "<br>",
            "Percentual: ", format(round(percentual, 1), decimal.mark = ","), "%"
          )
        )
      
      p <- ggplot(tabela, aes(x = sexo_nome, y = n, fill = sexo, text = dica)) +
        geom_col(width = 0.48) +
        scale_fill_manual(values = c("F" = cor_principal, "M" = cor_secundaria)) +
        scale_y_continuous(
          labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
          expand = expansion(mult = c(0, 0.08))
        ) +
        labs(x = NULL, y = "Número de inscritos") +
        tema_relatorio
      
      montar_plotly(p)
    })
    
    plot_raca <- reactive({
      dados <- participantes()
      
      tabela <- dados %>%
        filter(!is.na(cor_raca)) %>%
        count(cor_raca, name = "n") %>%
        mutate(
          percentual = 100 * n / sum(n),
          dica = paste0(
            "<b>", cor_raca, "</b><br>",
            "Participantes: ", format(n, big.mark = ".", decimal.mark = ","), "<br>",
            "Percentual: ", format(round(percentual, 1), decimal.mark = ","), "%"
          )
        )
      
      p <- ggplot(tabela, aes(x = n, y = reorder(cor_raca, n), text = dica)) +
        geom_col(fill = cor_principal, width = 0.65) +
        scale_x_continuous(
          labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
          expand = expansion(mult = c(0, 0.08))
        ) +
        labs(x = "Número de participantes", y = NULL) +
        tema_relatorio
      
      montar_plotly(p)
    })
    
    plot_faixa_etaria <- reactive({
      dados <- participantes()
      
      ordem <- c(
        "Menor de 17 anos", "17 anos", "18 anos", "19 anos", "20 anos",
        "21 anos", "22 anos", "23 anos", "24 anos", "25 anos",
        "Entre 26 e 30 anos", "Entre 31 e 35 anos", "Entre 36 e 40 anos",
        "Entre 41 e 45 anos", "Entre 46 e 50 anos", "Entre 51 e 55 anos",
        "Entre 56 e 60 anos", "Entre 61 e 65 anos", "Entre 66 e 70 anos",
        "Maior de 70 anos"
      )
      
      tabela <- dados %>%
        filter(!is.na(faixa_etaria)) %>%
        count(faixa_etaria, name = "n") %>%
        mutate(
          faixa_etaria = factor(faixa_etaria, levels = ordem),
          percentual = 100 * n / sum(n),
          dica = paste0(
            "<b>", faixa_etaria, "</b><br>",
            "Inscritos: ", format(n, big.mark = ".", decimal.mark = ","), "<br>",
            "Percentual: ", format(round(percentual, 1), decimal.mark = ","), "%"
          )
        )
      
      p <- ggplot(tabela, aes(x = n, y = faixa_etaria, text = dica)) +
        geom_col(fill = cor_secundaria, width = 0.68) +
        scale_x_continuous(
          labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
          expand = expansion(mult = c(0, 0.08))
        ) +
        labs(x = "Número de inscritos", y = NULL) +
        tema_relatorio
      
      montar_plotly(p)
    })
    
    plot_conclusao <- reactive({
      dados <- participantes()
      
      tabela <- dados %>%
        filter(!is.na(conclusao_em)) %>%
        count(conclusao_em, name = "n") %>%
        mutate(
          percentual = 100 * n / sum(n),
          dica = paste0(
            "<b>", conclusao_em, "</b><br>",
            "Participantes: ", format(n, big.mark = ".", decimal.mark = ","), "<br>",
            "Percentual: ", format(round(percentual, 1), decimal.mark = ","), "%"
          )
        )
      
      p <- ggplot(tabela, aes(x = n, y = reorder(conclusao_em, n), text = dica)) +
        geom_col(fill = cor_destaque, width = 0.65) +
        scale_x_continuous(
          labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
          expand = expansion(mult = c(0, 0.08))
        ) +
        labs(x = "Número de participantes", y = NULL) +
        tema_relatorio
      
      montar_plotly(p)
    })
    
    plot_treineiros <- reactive({
      dados <- participantes()
      
      tabela <- dados %>%
        filter(!is.na(treineiros)) %>%
        count(treineiros, name = "n") %>%
        mutate(
          percentual = 100 * n / sum(n),
          dica = paste0(
            "<b>", treineiros, "</b><br>",
            "Inscritos: ", format(n, big.mark = ".", decimal.mark = ","), "<br>",
            "Percentual: ", format(round(percentual, 1), decimal.mark = ","), "%"
          )
        )
      
      p <- ggplot(tabela, aes(x = treineiros, y = n, fill = treineiros, text = dica)) +
        geom_col(width = 0.48) +
        scale_fill_manual(values = c("Não treineiro" = cor_secundaria, "Treineiro" = cor_destaque)) +
        scale_y_continuous(
          labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
          expand = expansion(mult = c(0, 0.08))
        ) +
        labs(x = NULL, y = "Número de inscritos") +
        tema_relatorio
      
      montar_plotly(p)
    })
    
    output$sexo         <- plotly::renderPlotly({ plot_sexo() })
    output$raca         <- plotly::renderPlotly({ plot_raca() })
    output$faixa_etaria <- plotly::renderPlotly({ plot_faixa_etaria() })
    output$conclusao    <- plotly::renderPlotly({ plot_conclusao() })
    output$treineiros   <- plotly::renderPlotly({ plot_treineiros() })
    
    abrir_modal <- function(titulo, output_modal_id) {
      showModal(
        modalDialog(
          title = titulo,
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Fechar"),
          plotly::plotlyOutput(ns(output_modal_id), width = "100%", height = "580px")
        )
      )
    }
    
    output$modal_sexo       <- plotly::renderPlotly({ plot_sexo() })
    output$modal_raca       <- plotly::renderPlotly({ plot_raca() })
    output$modal_faixa      <- plotly::renderPlotly({ plot_faixa_etaria() })
    output$modal_conclusao  <- plotly::renderPlotly({ plot_conclusao() })
    output$modal_treineiros <- plotly::renderPlotly({ plot_treineiros() })
    
    observeEvent(input$expand_sexo,       { abrir_modal("Distribuição por sexo", "modal_sexo") })
    observeEvent(input$expand_raca,       { abrir_modal("Distribuição por raça/cor", "modal_raca") })
    observeEvent(input$expand_faixa,      { abrir_modal("Distribuição por faixa etária", "modal_faixa") })
    observeEvent(input$expand_conclusao,  { abrir_modal("Situação de conclusão do Ensino Médio", "modal_conclusao") })
    observeEvent(input$expand_treineiros, { abrir_modal("Condição de treineiro", "modal_treineiros") })
  })
}