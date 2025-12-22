# app.R (versión reorganizada con llamadas a descarga/procesado y uso de leer_sepe_csv / leer_poblacion_csv / normalizar_ccaa)
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(shinyjs)
library(DT)

# Definir la lista de años al inicio (ajusta si hace falta)
anios <- 2010:2024
anios_titulos <- paste(min(anios), max(anios), sep = "–")

# ------------------ Cargar scripts auxiliares ------------------
# preprocessing.R: contiene funciones de descarga/procesado (descargar_datasets_sepe, descargar_y_procesar_poblacion)
# read_data.R: contiene leer_sepe_csv(), leer_poblacion_csv(), normalizar_ccaa()
if (file.exists("preprocessing.R")) source("preprocessing.R")
if (file.exists("read_data.R"))    source("read_data.R")

res <- descargar_datasets_sepe(anios = anios, dir_data = "data")
res <- descargar_y_procesar_poblacion(codigos_ine = 2855:2907, dir_data = "data", anio_min = 2010, anio_max = 2025)


# ------------------ UI ------------------
ui <- navbarPage(
  title = paste0("SEPE — Resumen (", anios_titulos, ")"),
  id = "main_nav",
  header = tagList(useShinyjs()),

  tabPanel("Portada",
           fluidPage(
             br(),
             fluidRow(
               column(3,
                      tags$div(class = "card",
                               tags$div(class = "card-body",
                                        tags$h5("Año más reciente", class = "card-title"),
                                        tags$h3(textOutput("card_year"), style = "margin-top:0;"),
                                        tags$p("Año usado para indicadores", class = "card-text")
                               )
                      )
               ),
               column(3,
                      tags$div(class = "card",
                               tags$div(class = "card-body",
                                        tags$h5("Paro total (último año)", class = "card-title"),
                                        tags$h3(textOutput("card_paro_total")),
                                        tags$p("Suma de Paro Registrado en todas las CCAA", class = "card-text")
                               )
                      )
               ),
               column(3,
                      tags$div(class = "card",
                               tags$div(class = "card-body",
                                        tags$h5("Contratos (último año)", class = "card-title"),
                                        tags$h3(textOutput("card_contratos_total")),
                                        tags$p("Suma de contratos en todas las CCAA", class = "card-text")
                               )
                      )
               ),
               column(3,
                      tags$div(class = "card",
                               tags$div(class = "card-body",
                                        tags$h5("Demandantes (último año)", class = "card-title"),
                                        tags$h3(textOutput("card_dtes_total")),
                                        tags$p("Suma de demandantes en todas las CCAA", class = "card-text")
                               )
                      )
               )
             ),
             br(),
             fluidRow(
               column(12,
                      tags$h4("Evolución anual — Paro total (suma en todas las CCAA)"),
                      plotOutput("plot_paro_total_anual", height = "420px")
               )
             )
           )
  ),

  tabPanel("Explorar (filtros)",
           sidebarLayout(
             sidebarPanel(
               h4("Filtros"),
               checkboxGroupInput("metricas_sel", "Métricas (selecciona 1 o varias):",
                                  choices = c("Población" = "poblacion",
                                              "Dtes Empleo" = "dtes",
                                              "Contratos" = "contratos",
                                              "Paro" = "paro"),
                                  selected = c("paro")),
               uiOutput("ui_ccaa_selector"),
               hr(),
               actionButton("btn_reset_ccaa", "Restablecer selección"),
               width = 3
             ),
             mainPanel(
               h4("Resultados"),
               tabsetPanel(
                 tabPanel("Gráfica",
                          p("Serie temporal por la(s) métrica(s) seleccionada(s). Si eliges 'Total' en CCAA, se mostrará la agregación total."),
                          plotOutput("plot_filtro_timeseries", height = "520px")
                 ),
                 tabPanel("Tabla",
                          DTOutput("tabla_filtrada")
                 )
               ),
               width = 9
             )
           )
  )
)

# ------------------ SERVER ------------------
server <- function(input, output, session) {

  # --- Funciones para leer y agregar usando tus utilidades (leer_sepe_csv / normalizar_ccaa / leer_poblacion_csv) ---
  # lee un fichero procesado con leer_sepe_csv y devuelve NULL si falla
  safe_leer_sepe <- function(ruta) {
    if (!file.exists(ruta)) return(NULL)
    if (exists("leer_sepe_csv") && is.function(leer_sepe_csv)) {
      tryCatch({
        df <- leer_sepe_csv(ruta)
        return(df)
      }, error = function(e) {
        message("leer_sepe_csv falló en ", ruta, " : ", e$message)
        return(NULL)
      })
    } else {
      # fallback: intento leer con read.csv sin procesado
      tryCatch({
        df <- read.csv(ruta, stringsAsFactors = FALSE, check.names = FALSE)
        return(df)
      }, error = function(e) {
        message("read.csv también falló en ", ruta, " : ", e$message)
        return(NULL)
      })
    }
  }

  # Leer poblacion con la función de tu script
  poblacion_df_server <- reactive({
    if (exists("leer_poblacion_csv") && is.function(leer_poblacion_csv)) {
      # Intentar leer los ficheros de población en data/poblacion* si existen
      pob_files <- list.files("data/poblacion", pattern = "_processed.csv$|\\.csv$", full.names = TRUE)
      if (length(pob_files) > 0) {
        # concatenar todos los ficheros procesados por leer_poblacion_csv
        dfs <- lapply(pob_files, function(p) {
          tryCatch(leer_poblacion_csv(p), error = function(e) {
            message("leer_poblacion_csv falló en ", p, " : ", e$message); NULL
          })
        })
        df_all <- bind_rows(dfs)
        return(df_all)
      } else {
        # si no hay ficheros, intentar una llamada general si existe función que descarga/crea el csv
        # si no, devolver tibble vacío
        return(tibble::tibble(anio = integer(), cod_mun = integer(), poblacion = numeric(), comunidad = character()))
      }
    } else {
      return(tibble::tibble(anio = integer(), cod_mun = integer(), poblacion = numeric(), comunidad = character()))
    }
  })

  # Agregador por CCAA que usa normalizar_ccaa() y selecciona la columna numérica adecuada
  agregador_ccaa_uso_leer <- function(df_raw, patrones_busqueda = c("paro", "contrat", "dtes", "demand", "total")) {
    if (is.null(df_raw) || !is.data.frame(df_raw) || nrow(df_raw) == 0) return(NULL)

    # normalizar nombre de la columna comunidad si existe la utilidad
    # intentamos detectar columna de comunidad con varios nombres posibles
    col_comun <- names(df_raw)[grepl("comun|comunidad|Comunidad", names(df_raw), ignore.case = TRUE)][1]
    if (!is.null(col_comun) && !is.na(col_comun)) {
      if (exists("normalizar_ccaa") && is.function(normalizar_ccaa)) {
        df_raw[[col_comun]] <- normalizar_ccaa(df_raw[[col_comun]])
      } else {
        # trim
        df_raw[[col_comun]] <- trimws(as.character(df_raw[[col_comun]]))
      }
      names(df_raw)[names(df_raw) == col_comun] <- "Comunidad"
    } else {
      warning("No encontrada columna de Comunidad en df_raw")
      return(NULL)
    }

    # detectar columna año/codigo mes
    col_anio <- names(df_raw)[grepl("anio|año|codigo.mes|codigo|mes", names(df_raw), ignore.case = TRUE)][1]
    if (!is.null(col_anio) && !is.na(col_anio)) {
      df_raw$anio <- suppressWarnings(as.integer(substr(as.character(df_raw[[col_anio]]), 1, 4)))
    } else {
      # intentar usar columna llamada "anio" si existe
      if ("anio" %in% names(df_raw)) {
        df_raw$anio <- suppressWarnings(as.integer(df_raw$anio))
      } else {
        df_raw$anio <- NA_integer_
      }
    }

    # elegir columna numérica candidata según patrones
    numeric_cols <- names(df_raw)[sapply(df_raw, is.numeric)]
    candidate_cols <- names(df_raw)[sapply(names(df_raw), function(n) any(vapply(patrones_busqueda, function(p) grepl(p, n, ignore.case = TRUE), logical(1)))) & names(df_raw) %in% numeric_cols]

    chosen <- NULL
    if ("total Paro Registrado" %in% names(df_raw)) {
      chosen <- "total Paro Registrado"
    } else if ("Total" %in% names(df_raw) && "Total" %in% numeric_cols) {
      chosen <- "Total"
    } else if (length(candidate_cols) > 0) {
      chosen <- candidate_cols[1]
    } else if (length(numeric_cols) > 0) {
      chosen <- numeric_cols[1]
    }

    if (is.null(chosen)) {
      warning("No se ha podido identificar columna numérica para agregación")
      return(NULL)
    }

    df_raw %>%
      filter(!is.na(anio)) %>%
      group_by(anio, comunidad = Comunidad) %>%
      summarise(valor = mean(.data[[chosen]], na.rm = TRUE), .groups = "drop")
  }

  # Datos agregados combinando Paro / Contratos / Dtes usando leer_sepe_csv()
  datos_agregados <- reactive({
    contratos_list <- list()
    paro_list <- list()
    dtes_list <- list()

    # iterar años y leer los archivos procesados si existen (misma convención de nombres que usas)
    for (ano in anios) {
      ruta_contratos <- file.path("data", "contratos", sprintf("Contratos_por_municipios_%s_csv_processed.csv", ano))
      ruta_paro      <- file.path("data", "paro", sprintf("Paro_por_municipios_%s_csv_processed.csv", ano))
      ruta_dtes      <- file.path("data", "dtes_empleo", sprintf("Dtes_empleo_por_municipios_%s_csv_processed.csv", ano))

      # usar safe_leer_sepe (que invoca leer_sepe_csv si existe)
      df_c <- if (file.exists(ruta_contratos)) safe_leer_sepe(ruta_contratos) else NULL
      df_p <- if (file.exists(ruta_paro)) safe_leer_sepe(ruta_paro) else NULL
      df_d <- if (file.exists(ruta_dtes)) safe_leer_sepe(ruta_dtes) else NULL

      if (!is.null(df_c)) {
        agg_c <- agregador_ccaa_uso_leer(df_c, patrones_busqueda = c("contrat", "contrato", "total"))
        if (!is.null(agg_c)) contratos_list[[as.character(ano)]] <- agg_c %>% rename(contratos_total = valor)
      }

      if (!is.null(df_p)) {
        agg_p <- agregador_ccaa_uso_leer(df_p, patrones_busqueda = c("paro", "total"))
        if (!is.null(agg_p)) paro_list[[as.character(ano)]] <- agg_p %>% rename(paro_total = valor)
      }

      if (!is.null(df_d)) {
        agg_d <- agregador_ccaa_uso_leer(df_d, patrones_busqueda = c("demand", "dtes", "demandant", "total"))
        if (!is.null(agg_d)) dtes_list[[as.character(ano)]] <- agg_d %>% rename(dtes_total = valor)
      }
    }

    contratos_ccaa <- if (length(contratos_list) > 0) bind_rows(contratos_list) else tibble::tibble(anio = integer(), comunidad = character(), contratos_total = numeric())
    paro_ccaa_local <- if (length(paro_list) > 0) bind_rows(paro_list) else tibble::tibble(anio = integer(), comunidad = character(), paro_total = numeric())
    dtes_ccaa <- if (length(dtes_list) > 0) bind_rows(dtes_list) else tibble::tibble(anio = integer(), comunidad = character(), dtes_total = numeric())

    df_merged <- paro_ccaa_local %>%
      full_join(contratos_ccaa, by = c("anio", "comunidad")) %>%
      full_join(dtes_ccaa, by = c("anio", "comunidad"))

    # normalizar factor y tipos
    if (nrow(df_merged) > 0) {
      df_merged$comunidad <- as.character(df_merged$comunidad)
      df_merged$anio <- as.integer(df_merged$anio)
    }

    df_merged
  })

  # ------------------ Portada: indicadores y gráfico ------------------
  resumen_global <- reactive({
    df <- datos_agregados()
    pob <- poblacion_df_server()

    años_df <- if (nrow(df) > 0) sort(unique(na.omit(df$anio))) else integer(0)
    años_pob <- if (nrow(pob) > 0) sort(unique(na.omit(pob$anio))) else integer(0)
    años_all <- sort(unique(c(años_df, años_pob)), decreasing = TRUE)
    ultimo_anio <- if (length(años_all) > 0) años_all[1] else NA_integer_

    paro_sum <- if (!is.na(ultimo_anio) && "paro_total" %in% names(df)) sum(df$paro_total[df$anio == ultimo_anio], na.rm = TRUE) else NA_real_
    contratos_sum <- if (!is.na(ultimo_anio) && "contratos_total" %in% names(df)) sum(df$contratos_total[df$anio == ultimo_anio], na.rm = TRUE) else NA_real_
    dtes_sum <- if (!is.na(ultimo_anio) && "dtes_total" %in% names(df)) sum(df$dtes_total[df$anio == ultimo_anio], na.rm = TRUE) else NA_real_
    pobl_sum <- if (!is.na(ultimo_anio) && nrow(pob) > 0 && "poblacion" %in% names(pob)) sum(pob$poblacion[pob$anio == ultimo_anio], na.rm = TRUE) else NA_real_

    list(ultimo_anio = ultimo_anio, paro_sum = paro_sum, contratos_sum = contratos_sum, dtes_sum = dtes_sum, pobl_sum = pobl_sum)
  })

  output$card_year <- renderText({
    rg <- resumen_global()
    if (is.na(rg$ultimo_anio)) "-" else as.character(rg$ultimo_anio)
  })
  output$card_paro_total <- renderText({
    rg <- resumen_global()
    if (is.na(rg$paro_sum)) "-" else format(round(rg$paro_sum, 0), big.mark = ".", decimal.mark = ",")
  })
  output$card_contratos_total <- renderText({
    rg <- resumen_global()
    if (is.na(rg$contratos_sum)) "-" else format(round(rg$contratos_sum, 0), big.mark = ".", decimal.mark = ",")
  })
  output$card_dtes_total <- renderText({
    rg <- resumen_global()
    if (is.na(rg$dtes_sum)) "-" else format(round(rg$dtes_sum, 0), big.mark = ".", decimal.mark = ",")
  })

  output$plot_paro_total_anual <- renderPlot({
    df <- datos_agregados()
    req(!is.null(df))
    if (!("paro_total" %in% names(df))) {
      plot.new(); title("No hay datos de 'paro' disponibles para trazar."); return()
    }
    df_sum <- df %>% group_by(anio) %>% summarise(paro_total = sum(paro_total, na.rm = TRUE), .groups = "drop") %>% filter(!is.na(anio)) %>% arrange(anio)
    validate(need(nrow(df_sum) > 0, "No hay datos de paro por año para mostrar."))
    ggplot(df_sum, aes(x = anio, y = paro_total)) +
      geom_point(size = 2) + geom_line(size = 0.8) +
      geom_text(aes(label = format(round(paro_total, 0), big.mark = ".", decimal.mark = ",")), vjust = -0.7, size = 3) +
      labs(x = "Año", y = "Paro total (suma)", title = "Paro total por año — Todas las CCAA") +
      scale_x_continuous(breaks = df_sum$anio) +
      scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      theme_minimal()
  })

  # ------------------ UI dinámica CCAA ------------------
  observe({
    df <- datos_agregados()
    ccaa_choices <- sort(unique(na.omit(df$comunidad)))
    if (length(ccaa_choices) == 0) ccaa_choices <- character(0)
    ccaa_with_total <- c("Total", ccaa_choices)
    selected_default <- ccaa_choices
    updateSelectizeInput(session, "ccaa_multi_sel", choices = ccaa_with_total, selected = selected_default, server = TRUE)
  })

  output$ui_ccaa_selector <- renderUI({
    df <- datos_agregados()
    ccaa_choices <- sort(unique(na.omit(df$comunidad)))
    ccaa_with_total <- c("Total", ccaa_choices)
    selectizeInput("ccaa_multi_sel", "Comunidad(es):", choices = ccaa_with_total,
                   selected = if (length(ccaa_choices)>0) ccaa_choices else NULL,
                   multiple = TRUE, options = list(placeholder = "Selecciona CCAA (o Total)"))
  })

  observeEvent(input$btn_reset_ccaa, {
    df <- datos_agregados()
    ccaa_choices <- sort(unique(na.omit(df$comunidad)))
    updateSelectizeInput(session, "ccaa_multi_sel", selected = ccaa_choices)
  })

  # ------------------ Filtrado según selección ------------------
  datos_filtrados <- reactive({
    df <- datos_agregados()
    pob <- poblacion_df_server()

    req(!is.null(df))
    sel_ccaa <- input$ccaa_multi_sel
    sel_metrics <- input$metricas_sel
    if (is.null(sel_metrics) || length(sel_metrics) == 0) return(NULL)
    if (is.null(sel_ccaa) || length(sel_ccaa) == 0) sel_ccaa <- sort(unique(na.omit(df$comunidad)))

    # construir dataset base (paro/contratos/dtes) pivotado
    df2 <- df %>% mutate(paro = ifelse("paro_total" %in% names(.), paro_total, NA_real_),
                         contratos = ifelse("contratos_total" %in% names(.), contratos_total, NA_real_),
                         dtes = ifelse("dtes_total" %in% names(.), dtes_total, NA_real_)) %>%
      select(anio, comunidad, paro, contratos, dtes) %>%
      pivot_longer(cols = c("paro","contratos","dtes"), names_to = "metric", values_to = "valor")

    # si "Total" seleccionado, construir filas agregadas por año
    if ("Total" %in% sel_ccaa) {
      agg_tot <- df %>% group_by(anio) %>% summarise(paro = sum(paro_total, na.rm = TRUE),
                                                    contratos = sum(contratos_total, na.rm = TRUE),
                                                    dtes = sum(dtes_total, na.rm = TRUE),
                                                    .groups = "drop") %>%
        mutate(comunidad = "Total") %>%
        pivot_longer(cols = c("paro","contratos","dtes"), names_to = "metric", values_to = "valor")
      df_long <- bind_rows(df2 %>% filter(comunidad %in% sel_ccaa & comunidad != "Total"), agg_tot)
    } else {
      df_long <- df2 %>% filter(comunidad %in% sel_ccaa)
    }

    # añadir población si se pidió y existe (agregada por comunidad y año)
    if ("poblacion" %in% sel_metrics && nrow(pob) > 0) {
      # asumimos que leer_poblacion_csv ya normalizó comunidad si procede
      pob_agg <- pob %>% group_by(anio, comunidad) %>% summarise(poblacion = sum(poblacion, na.rm = TRUE), .groups = "drop") %>%
        pivot_longer(cols = "poblacion", names_to = "metric", values_to = "valor")
      if ("Total" %in% sel_ccaa) {
        pob_total <- pob %>% group_by(anio) %>% summarise(poblacion = sum(poblacion, na.rm = TRUE), .groups = "drop") %>%
          mutate(comunidad = "Total") %>% pivot_longer(cols = "poblacion", names_to = "metric", values_to = "valor")
        pob_bind <- bind_rows(pob_agg, pob_total)
      } else {
        pob_bind <- pob_agg
      }
      df_long <- bind_rows(df_long, pob_bind)
    }

    # Filtrar por métricas solicitadas
    req_metrics <- c()
    if ("paro" %in% sel_metrics) req_metrics <- c(req_metrics, "paro")
    if ("contratos" %in% sel_metrics) req_metrics <- c(req_metrics, "contratos")
    if ("dtes" %in% sel_metrics) req_metrics <- c(req_metrics, "dtes")
    if ("poblacion" %in% sel_metrics) req_metrics <- c(req_metrics, "poblacion")

    res <- df_long %>% filter(metric %in% req_metrics)
    res %>% filter(!is.na(anio)) %>% arrange(metric, comunidad, anio)
  })

  # Plot filtrado
  output$plot_filtro_timeseries <- renderPlot({
    df <- datos_filtrados()
    req(!is.null(df) && nrow(df) > 0)
    ggplot(df, aes(x = anio, y = valor, color = comunidad, group = comunidad)) +
      geom_line(size = 0.9) + geom_point(size = 1.6) +
      facet_wrap(~metric, scales = "free_y", ncol = 1, labeller = as_labeller(c(paro = "Paro", contratos = "Contratos", dtes = "Demandantes", poblacion = "Población"))) +
      labs(x = "Año", y = NULL, color = "Comunidad", title = "Serie temporal — métricas seleccionadas") +
      theme_minimal(base_size = 12) + theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
  })

  output$tabla_filtrada <- renderDT({
    df <- datos_filtrados()
    if (is.null(df) || nrow(df) == 0) {
      datatable(data.frame(Mensaje = "No hay datos según la selección"), options = list(dom = 't'), rownames = FALSE)
    } else {
      df %>% arrange(metric, comunidad, desc(anio)) %>% datatable(options = list(pageLength = 25, searchHighlight = TRUE), rownames = FALSE)
    }
  })

  # Actualizar el título de la pestaña
  observe({
    shinyjs::runjs(paste0("document.title = 'SEPE — Resumen (", min(anios), "–", max(anios), ")';"))
  })
}

shinyApp(ui = ui, server = server)
