# app.R

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(readr)

# Function to process standards CSV
process_standards_csv <- function(file) {
  data <- read_csv(file, col_names = FALSE, skip = 1)
  colnames(data) <- c("Standard ID", "Unnamed", "Standard", "Repl", "Calculated", 
                      "Difference", "CV", "Flag", "Analytical", "Net Abs")
  
  cleaned_data <- data %>%
    filter(!is.na(`Standard ID`) & `Standard ID` != "Standard ID" & Repl != "Mean:") %>%
    select(`Standard ID`, Standard, Analytical) %>%
    rename(Concentration = Standard, Absorbance = Analytical) %>%
    mutate(
      Concentration = as.numeric(Concentration),
      Absorbance = as.numeric(Absorbance)
    ) %>%
    filter(!is.na(Concentration) & !is.na(Absorbance)) %>%
    arrange(Concentration)
  
  return(cleaned_data)
}

# Function to process unknown samples CSV
process_unknowns_csv <- function(file) {
  data <- read_csv(file, col_names = FALSE, skip = 1)
  colnames(data) <- c("Sample ID", "Unnamed", "Repl", "Analytical", "Net Abs", 
                      "Conc", "Mean", "CV", "Flag")
  
  cleaned_data <- data %>%
    filter(!is.na(`Sample ID`) & `Sample ID` != "Sample ID" & Repl != "Mean:") %>%
    select(`Sample ID`, Analytical) %>%
    rename(Absorbance = Analytical) %>%
    mutate(Absorbance = as.numeric(Absorbance)) %>%
    filter(!is.na(Absorbance)) %>%
    arrange(`Sample ID`)
  
  return(cleaned_data)
}

ui <- fluidPage(
  titlePanel("Bradford Protein Assay Analyzer"),
  tabsetPanel(
    tabPanel("CSV Upload",
             sidebarLayout(
               sidebarPanel(
                 fileInput("standards_file", "Upload Standards CSV",
                           accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                 actionButton("process_standards", "Process Standards Data"),
                 br(), br(),
                 fileInput("unknowns_file", "Upload Unknown Samples CSV",
                           accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                 actionButton("process_unknowns", "Process Unknown Samples Data"),
                 br(), br(),
                 numericInput("sample_volume_csv", "Volume of Sample Added to Assay (µL):", 
                              value = 2, min = 1, max = 5, step = 1),
                 numericInput("dilution_factor_csv", "Dilution Factor (e.g., 1 for no dilution, 2 for 1:2):", 
                              value = 1, min = 1, step = 1),
                 actionButton("analyze_unknowns_csv", "Analyze Unknown Samples"),
                 br(), br(),
                 downloadButton("download_unknowns_csv", "Download Results (CSV)")
               ),
               mainPanel(
                 fluidRow(
                   column(6,
                          h4("Processed Standards Data"),
                          DTOutput("standards_table", height = "300px", width = "100%")
                   ),
                   column(6,
                          h4("Standard Curve"),
                          plotOutput("std_curve_plot_csv", height = "300px", width = "100%")
                   )
                 ),
                 fluidRow(
                   column(12,
                          h4("Standard Curve Information"),
                          verbatimTextOutput("std_curve_info_csv")
                   )
                 ),
                 fluidRow(
                   column(6,
                          h4("Processed Unknown Samples Data"),
                          DTOutput("unknowns_table", height = "300px", width = "100%")
                   ),
                   column(6,
                          h4("Analysis Results"),
                          DTOutput("unknown_results_csv", height = "300px", width = "100%")
                   )
                 )
               )
             )
    ),
    tabPanel("Manual Input - Standard Curve",
             fluidRow(
               column(12,
                      h4("Standard Curve Data Entry"),
                      numericInput("num_std_conc", "Number of Standard Concentrations:", 
                                   value = 5, min = 2, max = 10),
                      selectInput("std_replicates", "Number of Replicates:", 
                                  choices = 2:4, selected = 3),
                      uiOutput("std_conc_inputs"),
                      actionButton("generate_curve_manual", "Generate Standard Curve")
               )
             ),
             fluidRow(
               column(6,
                      br(),
                      plotOutput("std_curve_plot_manual", height = "300px", width = "100%")
               ),
               column(6,
                      h4("Standard Curve Information"),
                      verbatimTextOutput("std_curve_info_manual")
               )
             )
    ),
    tabPanel("Manual Input - Unknown Samples",
             fluidRow(
               column(12,
                      h4("Unknown Samples Data Entry"),
                      numericInput("num_unknowns", "Number of Unknown Samples:", 
                                   value = 3, min = 1, max = 20),
                      selectInput("unknown_replicates", "Number of Replicates:", 
                                  choices = 2:4, selected = 3),
                      numericInput("sample_volume_manual", "Volume of Sample Added to Assay (µL):", 
                                   value = 2, min = 1, max = 5, step = 1),
                      uiOutput("unknown_inputs"),
                      numericInput("dilution_factor_manual", "Dilution Factor (e.g., 1 for no dilution, 2 for 1:2):", 
                                   value = 1, min = 1, step = 1),
                      actionButton("analyze_unknowns_manual", "Analyze Unknown Samples"),
                      br(), br(),
                      downloadButton("download_unknowns_manual", "Download Results (CSV)")
               )
             ),
             fluidRow(
               column(12,
                      br(),
                      DTOutput("unknown_results_manual", height = "300px", width = "100%")
               )
             )
    ),
    tabPanel("Protocols",
             fluidRow(
               column(12,
                      h4("Protocol for Standard Preparation"),
                      p("1. Prepare a BSA (Bovine Serum Albumin) stock solution of 1 mg/ml in Urea-Thiourea-Tris (UTU) buffer, 
         or in protease-free MilliQ water."),
                      p("2. Prepare a series of BSA standards in 1.5 ml Eppendorf tubes as follows:"),
                      tableOutput("standards_protocol_table"),
                      p("3. Add 200 µl of Bio-Rad Bradford reagent concentrate to each tube."),
                      p("4. Adjust the final volume to 1 ml with MilliQ water."),
                      p("5. Mix the solution by pipetting gently to ensure complete mixing."),
                      p("6. Allow the mixture to stand for 5 minutes at room temperature to allow full color development."),
                      p("7. Measure the absorbance at 595 nm. It is important to prepare the standard curve within 30 minutes after mixing."),
                      p("8. If you cannot complete the standard curve preparation within 30 minutes, prepare new standards to ensure accuracy."),
                      br(),
                      h4("Protocol for Measuring Protein Concentration of Unknown Samples"),
                      p("1. Add 1-10 µl of your protein sample to a tube."),
                      p("2. Add 200 µl of Bio-Rad Bradford reagent concentrate to the tube."),
                      p("3. Adjust the final volume to 1 ml with MilliQ water."),
                      p("4. Mix the solution by pipetting gently to ensure complete mixing."),
                      p("5. Measure the absorbance at 595 nm after allowing the mixture to stand for 5 minutes at room temperature."),
                      p("6. Use the standard curve to determine the protein concentration of your unknown samples."),
                      p("7. Ensure all measurements and dilutions are performed consistently to improve the accuracy and reproducibility of your results.")
               )
               
             )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values
  rv <- reactiveValues(standards_data = NULL, unknowns_data = NULL,
                       standard_fit = NULL, unknowns_results = NULL,
                       blank_absorbance = NULL)
  
  rv_manual <- reactiveValues(standard_fit = NULL, unknowns_results = NULL, blank_absorbance = NULL)
  
  # CSV Upload - Process Standards Data
  observeEvent(input$process_standards, {
    req(input$standards_file)
    tryCatch({
      rv$standards_data <- process_standards_csv(input$standards_file$datapath)
      output$standards_table <- renderDT({
        datatable(rv$standards_data, options = list(pageLength = 20, autoWidth = TRUE))
      })
      
      # Generate standard curve
      standards_avg <- rv$standards_data %>%
        group_by(Concentration) %>%
        summarise(Absorbance = mean(Absorbance, na.rm = TRUE), .groups = 'drop')
      
      blank_absorbance <- standards_avg$Absorbance[standards_avg$Concentration == 0]
      if (length(blank_absorbance) == 0) {
        showModal(modalDialog(
          title = "Error",
          "No 0 µg/ml standard found. Please ensure your data includes a blank.",
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      }
      
      standards_avg$Absorbance[standards_avg$Concentration != 0] <- 
        standards_avg$Absorbance[standards_avg$Concentration != 0] - blank_absorbance
      
      fit <- lm(Absorbance ~ Concentration, data = standards_avg)
      
      # Plot standard curve
      output$std_curve_plot_csv <- renderPlot({
        ggplot(standards_avg, aes(x = Concentration, y = Absorbance)) +
          geom_point() +
          geom_smooth(method = "lm", se = TRUE, color = "blue") +
          theme_minimal() +
          theme(text = element_text(size = 12)) + # Increase font size
          labs(title = "Standard Curve (Blank Corrected)",
               x = "Concentration (µg/ml)",
               y = "Absorbance (Blank Corrected)")
      })
      
      # Display regression info
      output$std_curve_info_csv <- renderPrint({
        cat("Linear Regression Equation:\n")
        cat(paste0("Absorbance = ", round(coef(fit)[2],4), " * Concentration + ", round(coef(fit)[1],4), "\n"))
        cat(paste0("R-squared: ", round(summary(fit)$r.squared,4)))
      })
      
      # Store the fit and blank absorbance for use in unknown samples analysis
      rv$standard_fit <- fit
      rv$blank_absorbance <- blank_absorbance
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Error processing standards data:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
  
  # CSV Upload - Process Unknown Samples Data
  observeEvent(input$process_unknowns, {
    req(input$unknowns_file)
    tryCatch({
      rv$unknowns_data <- process_unknowns_csv(input$unknowns_file$datapath)
      output$unknowns_table <- renderDT({
        datatable(rv$unknowns_data, options = list(pageLength = 20, autoWidth = TRUE))
      })
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Error processing unknown samples data:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
  
  # CSV Upload - Analyze Unknown Samples
  observeEvent(input$analyze_unknowns_csv, {
    req(rv$standard_fit, rv$blank_absorbance, rv$unknowns_data)
    tryCatch({
      # Analyze unknown samples
      unknowns_data <- rv$unknowns_data
      unknowns_data$Absorbance <- unknowns_data$Absorbance - rv$blank_absorbance
      unknowns_data$C_assay_ug_ml <- (unknowns_data$Absorbance - coef(rv$standard_fit)[1]) / coef(rv$standard_fit)[2]
      unknowns_data$C_sample_ug_ul <- unknowns_data$C_assay_ug_ml / input$sample_volume_csv * input$dilution_factor_csv
      
      unknowns_data$C_assay_ug_ml <- round(unknowns_data$C_assay_ug_ml, 4)
      unknowns_data$C_sample_ug_ul <- round(unknowns_data$C_sample_ug_ul, 4)
      
      rv$unknowns_results <- unknowns_data
      
      output$unknown_results_csv <- renderDT({
        datatable(unknowns_data, 
                  colnames = c("Sample ID", "Absorbance", "C_assay (µg/ml)", "Protein Concentration (µg/µl)"),
                  options = list(pageLength = 20, autoWidth = TRUE))
      })
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Error analyzing unknown samples:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
  
  # Manual Input - Dynamic UI for standard concentrations
  output$std_conc_inputs <- renderUI({
    num <- input$num_std_conc
    lapply(1:num, function(i) {
      fluidRow(
        column(4, numericInput(paste0("std_conc_",i), 
                               paste("Concentration", i, "(µg/ml):"), 
                               value = ifelse(i == 1, 0, (i-1)*2), min = 0)),
        column(8, textInput(paste0("std_abs_",i), 
                            paste("Absorbance Replicates (comma-separated):"), 
                            value = ifelse(i == 1, "0.05,0.04,0.05", "0.1,0.12,0.11")))
      )
    })
  })
  
  # Manual Input - Dynamic UI for unknown samples
  output$unknown_inputs <- renderUI({
    num <- input$num_unknowns
    lapply(1:num, function(i) {
      fluidRow(
        column(4, textInput(paste0("unknown_id_",i), 
                            paste("Sample ID", i, ":"), 
                            value = paste("Sample", i))),
        column(8, textInput(paste0("unknown_abs_",i), 
                            paste("Absorbance Replicates (comma-separated):"), 
                            value = "0.15,0.14,0.16"))
      )
    })
  })
  
  # Function to parse comma-separated values
  parse_replicates <- function(input_str) {
    vals <- unlist(strsplit(input_str, split = ","))
    as.numeric(trimws(vals))
  }
  
  # Manual Input - Generate Standard Curve
  observeEvent(input$generate_curve_manual, {
    num <- input$num_std_conc
    rep <- input$std_replicates
    std_data <- data.frame()
    
    for(i in 1:num){
      conc <- input[[paste0("std_conc_",i)]]
      abs_input <- input[[paste0("std_abs_",i)]]
      abs_vals <- parse_replicates(abs_input)
      
      if(length(abs_vals) != rep){
        showModal(modalDialog(
          title = "Input Error",
          paste("Standard Concentration", conc, "µg/ml does not have", rep, "replicates."),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      }
      
      temp_df <- data.frame(Concentration = conc, Absorbance = mean(abs_vals))
      std_data <- rbind(std_data, temp_df)
    }
    
    blank_absorbance <- std_data$Absorbance[std_data$Concentration == 0]
    std_data$Absorbance[std_data$Concentration != 0] <- 
      std_data$Absorbance[std_data$Concentration != 0] - blank_absorbance
    
    fit <- lm(Absorbance ~ Concentration, data = std_data)
    summary_fit <- summary(fit)
    
    rv_manual$standard_fit <- fit
    rv_manual$blank_absorbance <- blank_absorbance
    
    output$std_curve_plot_manual <- renderPlot({
      ggplot(std_data, aes(x = Concentration, y = Absorbance)) +
        geom_point() +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        theme_minimal() +
        theme(text = element_text(size = 12)) + # Increase font size
        labs(title = "Standard Curve (Blank Corrected)",
             x = "Concentration (µg/ml)",
             y = "Absorbance (Blank Corrected)")
    })
    
    output$std_curve_info_manual <- renderPrint({
      cat("Linear Regression Equation:\n")
      cat(paste0("Absorbance = ", round(coef(fit)[2],4), " * Concentration + ", round(coef(fit)[1],4), "\n"))
      cat(paste0("R-squared: ", round(summary_fit$r.squared,4)))
    })
  })
  
  # Manual Input - Analyze Unknown Samples
  observeEvent(input$analyze_unknowns_manual, {
    req(rv_manual$standard_fit, rv_manual$blank_absorbance)
    
    num <- input$num_unknowns
    rep <- input$unknown_replicates
    unknowns <- data.frame()
    
    for(i in 1:num){
      id <- input[[paste0("unknown_id_",i)]]
      abs_input <- input[[paste0("unknown_abs_",i)]]
      abs_vals <- parse_replicates(abs_input)
      
      if(length(abs_vals) != rep){
        showModal(modalDialog(
          title = "Input Error",
          paste("Unknown Sample", id, "does not have", rep, "replicates."),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      }
      
      avg_abs <- mean(abs_vals, na.rm = TRUE)
      avg_abs_corrected <- avg_abs - rv_manual$blank_absorbance
      
      unknowns <- rbind(unknowns, data.frame(ID = id, Absorbance = avg_abs_corrected))
    }
    
    fit <- rv_manual$standard_fit
    intercept <- coef(fit)[1]
    slope <- coef(fit)[2]
    
    unknowns$C_assay_ug_ml <- (unknowns$Absorbance - intercept) / slope
    unknowns$C_sample_ug_ul <- unknowns$C_assay_ug_ml / input$sample_volume_manual * input$dilution_factor_manual
    
    unknowns$C_assay_ug_ml <- round(unknowns$C_assay_ug_ml, 4)
    unknowns$C_sample_ug_ul <- round(unknowns$C_sample_ug_ul, 4)
    
    rv_manual$unknowns_results <- unknowns
    
    output$unknown_results_manual <- renderDT({
      datatable(unknowns, 
                colnames = c("Sample ID", "Absorbance", "C_assay (µg/ml)", "Protein Concentration (µg/µl)"),
                options = list(pageLength = 20, autoWidth = TRUE))
    })
  })
  
  # Download Handler for CSV Upload Results
  output$download_unknowns_csv <- downloadHandler(
    filename = function() {
      paste("unknown_samples_results_csv-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      req(rv$unknowns_results)
      write.csv(rv$unknowns_results, file, row.names = FALSE)
    }
  )
  
  # Download Handler for Manual Input Results
  output$download_unknowns_manual <- downloadHandler(
    filename = function() {
      paste("unknown_samples_results_manual-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      req(rv_manual$unknowns_results)
      write.csv(rv_manual$unknowns_results, file, row.names = FALSE)
    }
  )
  
  # Protocol table
  output$standards_protocol_table <- renderTable({
    data.frame(
      "BSA Concentration (µg)" = c(0, 2, 4, 6, 8, 10),
      "BSA Volume (µl)" = c(0, 2, 4, 6, 8, 10),
      "Water Volume (µl)" = c(800, 798, 796, 794, 792, 790),
      check.names = FALSE
    )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
