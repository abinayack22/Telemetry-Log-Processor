# ============================================================
#   Sensor Log Processor — Shiny Web Application
#   Corporate Light Theme Edition
# ============================================================

library(shiny)
library(dplyr)
library(writexl)
library(DT)
library(openxlsx)

# ── Helper: core processing logic (unchanged) ────────────────────────────────
process_sensor_data <- function(file_path, threshold_sec) {
  
  # 1. FILE LOAD
  df <- read.csv(file_path, header = FALSE,
                 fileEncoding = "latin1", stringsAsFactors = FALSE)
  
  # Replace symbol 
  df[] <- lapply(df, function(x) gsub("\u00e2", "\u27a4", x))   # â → ➜
  
  # 2. SPLIT
  split_data <- strsplit(df$V1, ";")
  df_split   <- as.data.frame(do.call(rbind, split_data),
                              stringsAsFactors = FALSE)
  df_14 <- df_split[, 1:min(14, ncol(df_split))]
  colnames(df_14) <- paste0("V", seq_len(ncol(df_14)))
  
  # 3. CLEAN OTHER DATA
  df_cleaned <- df[, 1:min(8, ncol(df))]
  colnames(df_cleaned) <- c("ID","SDM VALUE","SDM STATUS","AM INDEX VALUE",
                            "AM INDEX STATUS","FORWARD POWER VALUE",
                            "FORWARD POWER STATUS","dBm value")[
                              seq_len(ncol(df_cleaned))]
  if (ncol(df_cleaned) >= 8) {
    df_cleaned$`dBm value` <- gsub(";.*", "", df_cleaned$`dBm value`)
  }
  
  # 4. MERGE
  final_output <- cbind(df_14, df_cleaned)
  final_output <- final_output[, !(colnames(final_output) %in%
                                     c("V6","V7","ID"))]
  final_output <- final_output[-1, ]
  
  colnames(final_output)[1:5] <- c("Time And Date","Device",
                                   "Path","Event","DDM Status")
  
  # 5. DEVICE CLEAN
  final_output$Device <- paste(final_output$Device,
                               final_output$Path,
                               final_output$Event, sep = ";")
  final_output <- final_output[, !(colnames(final_output) %in%
                                     c("Path","Event"))]
  final_output$EVENT  <- sub(".*?(Reserv.*)", "\\1", final_output$Device)
  final_output$Device <- sub("(.*?)(Reserv.*)", "\\1", final_output$Device)
  final_output$Device <- sub(";$", "", final_output$Device)
  
  # 6. CLEAN STATUS
  final_output$`DDM Status` <- sub(".*?(DDM.*)", "\\1",
                                   final_output$`DDM Status`)
  
  # Rename columns
  names(final_output)[names(final_output) == "SDM VALUE"]           <- "DDM VALUE"
  names(final_output)[names(final_output) == "AM INDEX VALUE"]      <- "SDM VALUE"
  names(final_output)[names(final_output) == "FORWARD POWER VALUE"] <- "AM INDEX VALUE"
  names(final_output)[names(final_output) == "dBm value"]           <- "FORWARD POWER VALUE"
  
  # Reorder
  final_output <- final_output[, c("Time And Date","EVENT",
                                   setdiff(names(final_output),
                                           c("Time And Date","EVENT")))]
  
  
  # 8. DIRECTION
  final_output <- final_output %>%
    mutate(Direction = sub(".*?(Direction \\d+).*", "\\1", Device))
  final_output$Device <- sub("Direction \\d+", "", final_output$Device)
  final_output <- final_output %>%
    mutate(Direction_ID = case_when(
      Direction == "Direction 3" ~ 1,
      Direction == "Direction 4" ~ 2,
      TRUE ~ NA_real_
    ))
  
  # 9. TIME EXTRACTION
  final_output$Only_Time <- sub(" .*", "", final_output$`Time And Date`)
  final_output$Time      <- as.POSIXct(final_output$Only_Time,
                                       format = "%H:%M:%S")
  
  # 10. STATUS CLEAN
  final_output$Status <- ifelse(
    grepl("Alarm",   final_output$`DDM Status`, ignore.case = TRUE), "ALARM",
    ifelse(grepl("Normal",  final_output$`DDM Status`, ignore.case = TRUE), "NORMAL",
           ifelse(grepl("Warning", final_output$`DDM Status`, ignore.case = TRUE),
                  "WARNING", NA)))
  
  # 11. SORT
  final_output <- final_output %>% arrange(Direction_ID, Time)
  
  # 12. SEQUENTIAL ALARM FILTERING
  remove_index <- c()
  i <- 1
  n <- nrow(final_output)
  
  while (i <= n) {
    current_dir    <- final_output$Direction_ID[i]
    current_status <- final_output$Status[i]
    
    if (!is.na(current_status) && current_status == "ALARM") {
      seq_alarm_indices <- c()
      j <- i
      
      while (j <= n &&
             !is.na(final_output$Direction_ID[j]) &&
             final_output$Direction_ID[j] == current_dir) {
        
        s <- final_output$Status[j]
        
        if (!is.na(s) && s == "ALARM") {
          seq_alarm_indices <- c(seq_alarm_indices, j)
          j <- j + 1
          
        } else if (!is.na(s) && s == "WARNING") {
          j <- j + 1
          
        } else if (!is.na(s) && s == "NORMAL") {
          normal_index     <- j
          last_alarm_index <- tail(seq_alarm_indices, 1)
          
          diff_sec <- as.numeric(
            difftime(final_output$Time[normal_index],
                     final_output$Time[last_alarm_index],
                     units = "secs"))
          
          if (!is.na(diff_sec) && diff_sec <= threshold_sec) {
            remove_index <- c(remove_index, seq_alarm_indices, normal_index)
          }
          j <- j + 1
          break
          
        } else {
          break
        }
      }
      i <- j
    } else {
      i <- i + 1
    }
  }
  
  # 13. APPLY THRESHOLD REMOVAL
  final_cleaned <- if (length(remove_index) > 0)
    final_output[-remove_index, ] else final_output
  
  # 14. REMOVE REMAINING NORMAL ROWS
  final_cleaned <- final_cleaned[
    !grepl("NORMAL", final_cleaned$Status, ignore.case = TRUE), ]
  
  # 15. SELECT & REORDER 12 COLUMNS
  final_cleaned <- final_cleaned[, c(
    "Time And Date","EVENT","Device","Direction",
    "DDM Status","DDM VALUE",
    "SDM STATUS","SDM VALUE",
    "AM INDEX STATUS","AM INDEX VALUE",
    "FORWARD POWER STATUS","FORWARD POWER VALUE"
  )]
  
  list(data = final_cleaned, rows_removed = length(remove_index))
}

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"
    ),
    tags$style(HTML("

      /* ── Base Reset ── */
      *, *::before, *::after { box-sizing: border-box; }

      html, body {
        background-color: #F0F4F8;
        color: #1a2535;
        font-family: 'Inter', 'Segoe UI', Arial, sans-serif;
        font-size: 14px;
        margin: 0;
        padding: 0;
        min-height: 100vh;
      }

      /* ── Top Navigation Bar ── */
      .navbar-corp {
        background-color: #0056b3;
        padding: 0 32px;
        height: 56px;
        display: flex;
        align-items: center;
        justify-content: space-between;
        box-shadow: 0 2px 6px rgba(0,0,0,0.15);
      }
      .navbar-brand-wrap {
        display: flex;
        align-items: center;
        gap: 12px;
      }
      .navbar-logo {
        width: 28px; height: 28px;
        background: rgba(255,255,255,0.2);
        border-radius: 6px;
        display: flex; align-items: center; justify-content: center;
        font-size: 15px; color: #fff;
      }
      .navbar-title {
        font-size: 16px;
        font-weight: 700;
        color: #ffffff;
        letter-spacing: 0.2px;
      }
      .navbar-subtitle {
        font-size: 11px;
        color: rgba(255,255,255,0.65);
        letter-spacing: 0.3px;
      }
      .navbar-meta {
        font-size: 11px;
        color: rgba(255,255,255,0.6);
      }

      /* ── Page Body ── */
      .page-body {
        padding: 28px 32px 40px;
      }

      /* ── Section Label ── */
      .section-label {
        font-size: 10px;
        font-weight: 700;
        letter-spacing: 1.5px;
        text-transform: uppercase;
        color: #64748b;
        margin-bottom: 8px;
        padding-left: 2px;
      }

      /* ── White Panels ── */
      .panel {
        background: #ffffff;
        border: 1px solid #E2E8F0;
        border-radius: 10px;
        padding: 20px 22px;
        margin-bottom: 18px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.06);
      }
      .panel-title {
        font-size: 13px;
        font-weight: 600;
        color: #0056b3;
        margin-bottom: 14px;
        padding-bottom: 10px;
        border-bottom: 1px solid #EDF2F7;
        display: flex;
        align-items: center;
        gap: 7px;
      }
      .panel-title .icon {
        width: 22px; height: 22px;
        background: #EBF4FF;
        border-radius: 5px;
        display: inline-flex; align-items: center; justify-content: center;
        font-size: 12px;
      }

      /* ── Form Controls ── */
      .form-control,
      .form-group input[type=number],
      .form-group input[type=text],
      input.form-control {
        background: #ffffff !important;
        border: 1px solid #CBD5E0 !important;
        border-radius: 6px !important;
        color: #1a2535 !important;
        font-family: 'Inter', 'Segoe UI', sans-serif !important;
        font-size: 13px !important;
        padding: 8px 11px !important;
        transition: border-color .18s, box-shadow .18s;
        width: 100%;
      }
      .form-control:focus,
      input.form-control:focus {
        border-color: #0056b3 !important;
        box-shadow: 0 0 0 3px rgba(0,86,179,0.12) !important;
        outline: none !important;
      }
      label {
        color: #4a5568 !important;
        font-size: 12px !important;
        font-weight: 500 !important;
        letter-spacing: 0.2px;
        margin-bottom: 5px !important;
        display: block;
      }

      /* ── File Input Styling ── */
      .input-group-btn .btn-default,
      .btn-file {
        background: #0056b3 !important;
        border: 1px solid #0056b3 !important;
        color: #ffffff !important;
        font-family: 'Inter', sans-serif !important;
        font-size: 12px !important;
        font-weight: 500 !important;
        border-radius: 0 6px 6px 0 !important;
        padding: 8px 14px !important;
      }
      #file_input_progress { display: none; }

      /* ── Primary Button ── */
      .btn-primary-corp {
        background-color: #0056b3 !important;
        border: none !important;
        border-radius: 7px !important;
        color: #ffffff !important;
        font-family: 'Inter', sans-serif !important;
        font-size: 13px !important;
        font-weight: 600 !important;
        padding: 10px 0 !important;
        width: 100%;
        letter-spacing: 0.2px;
        cursor: pointer;
        transition: background-color .18s, box-shadow .18s;
        box-shadow: 0 2px 5px rgba(0,86,179,0.25);
      }
      .btn-primary-corp:hover {
        background-color: #004494 !important;
        box-shadow: 0 3px 9px rgba(0,86,179,0.35) !important;
      }
      .btn-primary-corp:active {
        background-color: #003a7a !important;
        transform: translateY(1px);
      }

      /* ── Download Button ── */
      .btn-download-corp {
        background-color: #ffffff !important;
        border: 1.5px solid #0056b3 !important;
        border-radius: 7px !important;
        color: #0056b3 !important;
        font-family: 'Inter', sans-serif !important;
        font-size: 13px !important;
        font-weight: 600 !important;
        padding: 9px 0 !important;
        width: 100%;
        letter-spacing: 0.2px;
        cursor: pointer;
        transition: background-color .18s, color .18s;
      }
      .btn-download-corp:hover {
        background-color: #0056b3 !important;
        color: #ffffff !important;
      }

      /* ── Divider ── */
      hr.panel-rule {
        border: none;
        border-top: 1px solid #EDF2F7;
        margin: 16px 0;
      }

      /* ── Threshold hint ── */
      .hint-text {
        font-size: 11px;
        color: #94a3b8;
        margin-top: 6px;
        line-height: 1.5;
      }

      /* ── Summary Stat Cards ── */
      .stats-grid {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 14px;
        margin-bottom: 18px;
      }
      .stat-card {
        background: #ffffff;
        border: 1px solid #E2E8F0;
        border-radius: 10px;
        padding: 16px 18px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.05);
      }
      .stat-card-label {
        font-size: 10px;
        font-weight: 700;
        letter-spacing: 1.2px;
        text-transform: uppercase;
        color: #94a3b8;
        margin-bottom: 6px;
      }
      .stat-card-value {
        font-size: 26px;
        font-weight: 700;
        color: #1a2535;
        line-height: 1;
      }
      .stat-card-sub {
        font-size: 11px;
        color: #94a3b8;
        margin-top: 4px;
      }
      .stat-card.blue   { border-top: 3px solid #0056b3; }
      .stat-card.red    { border-top: 3px solid #dc3545; }
      .stat-card.orange { border-top: 3px solid #e67e22; }
      .stat-card.gray   { border-top: 3px solid #94a3b8; }

      /* ── Alert Boxes ── */
      .alert-corp {
        border-radius: 7px;
        padding: 11px 15px;
        font-size: 13px;
        margin-bottom: 16px;
        display: flex;
        align-items: flex-start;
        gap: 10px;
        border: 1px solid;
      }
      .alert-corp.success {
        background: #f0fdf4;
        border-color: #86efac;
        color: #166534;
      }
      .alert-corp.error {
        background: #fef2f2;
        border-color: #fca5a5;
        color: #991b1b;
      }
      .alert-corp .alert-icon { font-size: 15px; line-height: 1.4; }

      /* ── DataTable ── */
      .dataTables_wrapper {
        font-family: 'Inter', 'Segoe UI', sans-serif !important;
        font-size: 12.5px !important;
        color: #1a2535 !important;
      }
      table.dataTable {
        border-collapse: collapse !important;
        width: 100% !important;
      }
      table.dataTable thead th,
      table.dataTable thead td {
        background-color: #F8FAFC !important;
        color: #374151 !important;
        font-size: 11px !important;
        font-weight: 700 !important;
        letter-spacing: 0.8px !important;
        text-transform: uppercase !important;
        border-bottom: 2px solid #E2E8F0 !important;
        border-top: 1px solid #E2E8F0 !important;
        padding: 10px 14px !important;
      }
      table.dataTable tbody tr:nth-child(odd)  td { background: #ffffff !important; }
      table.dataTable tbody tr:nth-child(even) td { background: #F8FAFC !important; }
      table.dataTable tbody tr:hover td {
        background: #EBF4FF !important;
      }
      table.dataTable tbody td {
        border-bottom: 1px solid #EDF2F7 !important;
        padding: 9px 14px !important;
        color: #374151 !important;
        vertical-align: middle !important;
      }
      .dataTables_info {
        color: #94a3b8 !important;
        font-size: 12px !important;
      }
      .dataTables_length label,
      .dataTables_filter label {
        color: #64748b !important;
        font-size: 12px !important;
        font-weight: 400 !important;
        letter-spacing: 0 !important;
      }
      .dataTables_length select,
      .dataTables_filter input {
        background: #ffffff !important;
        border: 1px solid #CBD5E0 !important;
        border-radius: 5px !important;
        color: #374151 !important;
        padding: 4px 8px !important;
        font-size: 12px !important;
        font-family: 'Inter', sans-serif !important;
      }
      .dataTables_filter input:focus {
        border-color: #0056b3 !important;
        box-shadow: 0 0 0 2px rgba(0,86,179,0.1) !important;
        outline: none !important;
      }
      .dataTables_paginate .paginate_button {
        color: #64748b !important;
        border-radius: 5px !important;
        font-size: 12px !important;
        padding: 3px 9px !important;
        border: none !important;
        background: transparent !important;
      }
      .dataTables_paginate .paginate_button:hover {
        background: #EBF4FF !important;
        color: #0056b3 !important;
        border: none !important;
      }
      .dataTables_paginate .paginate_button.current,
      .dataTables_paginate .paginate_button.current:hover {
        background: #0056b3 !important;
        color: #ffffff !important;
        border: none !important;
        box-shadow: none !important;
      }
      .dataTables_paginate .paginate_button.disabled,
      .dataTables_paginate .paginate_button.disabled:hover {
        color: #cbd5e0 !important;
        background: transparent !important;
      }

      /* ── Scrollbar ── */
      ::-webkit-scrollbar { width: 6px; height: 6px; }
      ::-webkit-scrollbar-track { background: #F0F4F8; }
      ::-webkit-scrollbar-thumb { background: #CBD5E0; border-radius: 4px; }
      ::-webkit-scrollbar-thumb:hover { background: #94a3b8; }

    "))
  ),
  
  # ── Navigation Bar ──
  div(class = "navbar-corp",
      div(class = "navbar-brand-wrap",
          div(class = "navbar-logo", "📡"),
          div(
            div(class = "navbar-title", "Sensor Log Processor"),
            div(class = "navbar-subtitle", "Sequential Alarm Filtering System")
          )
      ),
      div(class = "navbar-meta",
          "Direction-Aware  ·  DDM / SDM / AM Index  ·  Excel Export")
  ),
  
  # ── Page Body ──
  div(class = "page-body",
      fluidRow(
        
        # ── LEFT SIDEBAR ────────────────────────────────────────
        column(3,
               
               div(class = "section-label", "Configuration"),
               
               div(class = "panel",
                   div(class = "panel-title",
                       span(class = "icon", "📂"), "Upload File"),
                   fileInput("file_input", "Select CSV File",
                             accept      = ".csv",
                             placeholder = "No file selected",
                             buttonLabel = "Browse…")
               ),
               
               div(class = "panel",
                   div(class = "panel-title",
                       span(class = "icon", "⏱"), "Threshold Setting"),
                   numericInput("threshold",
                                "Time Threshold (seconds)",
                                value = 2, min = 0, step = 0.5),
                   div(class = "hint-text",
                       "Alarm–Normal event pairs that resolve within this window will be removed from the output.")
               ),
               
               div(class = "panel",
                   div(class = "panel-title",
                       span(class = "icon", "▶"), "Actions"),
                   actionButton("process_btn",
                                "Process Data",
                                class = "btn-primary-corp"),
                   hr(class = "panel-rule"),
                   downloadButton("download_btn",
                                  "Export to Excel (.xlsx)",
                                  class = "btn-download-corp")
               )
        ),
        
        # ── MAIN CONTENT AREA ───────────────────────────────────
        column(9,
               
               uiOutput("stats_ui"),
               uiOutput("status_msg"),
               
               div(class = "panel",
                   div(class = "panel-title",
                       span(class = "icon", "📋"), "Cleaned Data Preview"),
                   DTOutput("data_table")
               )
        )
      )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  result <- reactiveVal(NULL)
  
  observeEvent(input$process_btn, {
    req(input$file_input)
    result(NULL)
    tryCatch({
      res <- process_sensor_data(
        file_path     = input$file_input$datapath,
        threshold_sec = input$threshold
      )
      result(res)
    }, error = function(e) {
      result(list(error = conditionMessage(e)))
    })
  })
  
  # ── Summary stat cards ──
  output$stats_ui <- renderUI({
    res <- result()
    if (is.null(res) || !is.null(res$error)) return(NULL)
    d <- res$data
    n_alarm   <- sum(grepl("ALARM",   d$`DDM Status`, ignore.case = TRUE))
    n_warning <- sum(grepl("Warning", d$`DDM Status`, ignore.case = TRUE))
    
    tagList(
      div(class = "section-label", "Summary"),
      div(class = "stats-grid",
          div(class = "stat-card blue",
              div(class = "stat-card-label", "Final Rows"),
              div(class = "stat-card-value", nrow(d)),
              div(class = "stat-card-sub",   "After all filters")
          ),
          div(class = "stat-card red",
              div(class = "stat-card-label", "Alarms"),
              div(class = "stat-card-value", n_alarm),
              div(class = "stat-card-sub",   "DDM alarm events")
          ),
          div(class = "stat-card orange",
              div(class = "stat-card-label", "Warnings"),
              div(class = "stat-card-value", n_warning),
              div(class = "stat-card-sub",   "DDM warning events")
          ),
          div(class = "stat-card gray",
              div(class = "stat-card-label", "Rows Removed"),
              div(class = "stat-card-value", res$rows_removed),
              div(class = "stat-card-sub",
                  paste0("≤ ", input$threshold, "s threshold"))
          )
      )
    )
  })
  
  # ── Status message ──
  output$status_msg <- renderUI({
    res <- result()
    if (is.null(res)) return(NULL)
    if (!is.null(res$error)) {
      div(class = "alert-corp error",
          span(class = "alert-icon", "✖"),
          span(paste("Processing failed:", res$error))
      )
    } else {
      div(class = "alert-corp success",
          span(class = "alert-icon", "✔"),
          span(sprintf(
            "Processing complete — %d rows in final output. %d rows removed by threshold.",
            nrow(res$data), res$rows_removed))
      )
    }
  })
  
  # ── Data table ──
  output$data_table <- renderDT({
    res <- result()
    if (is.null(res) || !is.null(res$error)) return(NULL)
    datatable(
      res$data,
      options  = list(
        pageLength = 15,
        scrollX    = TRUE,
        dom        = "lfrtip",
        language   = list(search = "Search:"),
        stripeClasses = c("odd-row", "even-row")
      ),
      rownames = FALSE,
      class    = "stripe hover compact"
    )
  })
  
  # ── Download ──
  output$download_btn <- downloadHandler(
    filename = function() {
      paste0("sensor_log_output_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      
      res <- result()
      req(!is.null(res), is.null(res$error))
      
      data <- res$data
      
      wb <- createWorkbook()
      addWorksheet(wb, "Sensor Data")
      
      writeData(wb, "Sensor Data", data)
      
      # Create style for ALARM
      alarm_style <- createStyle(fontColour = "#FF0000", textDecoration = "bold")
      
      # Apply style to rows where ALARM appears
      for (col in colnames(data)) {
        rows <- which(grepl("ALARM", data[[col]], ignore.case = TRUE))
        
        if (length(rows) > 0) {
          addStyle(
            wb,
            sheet = "Sensor Data",
            style = alarm_style,
            rows = rows + 1,  # +1 because header row
            cols = which(colnames(data) == col),
            gridExpand = TRUE
          )
        }
      }
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

# ── Run ──────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server) 