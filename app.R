# app.R
library(shiny)
library(readxl)
library(dplyr)
library(janitor)
library(DT)
library(ggplot2)
library(forcats)
library(viridis)


df <- readRDS("cleaned_data.rds")
df <- df %>% mutate(across(where(is.character), as.factor))
cat_vars <- names(df)[sapply(df, is.factor)]
num_vars <- names(df)[sapply(df, is.numeric)]
all_vars <- c(cat_vars, num_vars)
var_type <- c(rep("Categorical", length(cat_vars)), rep("Numeric", length(num_vars)))

choices_display <- setNames(all_vars, paste0(all_vars, " (", var_type, ")"))

ui <- fluidPage(
  titlePanel("DATA2902 Survey Visualisations and Hypothesis Tests"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "var1",
        "Choose Variable 1",
        choices = choices_display,
        selected = if (length(all_vars) >= 1) all_vars[1] else NULL,
        options = list(
          render = I('{
               option: function(item, escape) {
                 var label = item.label || item.text || item.value;
                 var color = label.indexOf("Categorical") !== -1 ? "blue" : "green";
                 return "<div style=\\"color:" + color + "\\">" + escape(label) + "</div>";
               }
             }')
        )
      ),
      selectizeInput(
        "var2",
        "Choose Variable 2",
        choices = choices_display,
        selected = if (length(all_vars) >= 2) all_vars[2] else NULL,
        options = list(
          render = I('{
               option: function(item, escape) {
                 var label = item.label || item.text || item.value;
                 var color = label.indexOf("Categorical") !== -1 ? "blue" : "green";
                 return "<div style=\\"color:" + color + "\\">" + escape(label) + "</div>";
               }
             }')
        )
      ),
      hr(),
      actionButton("run_tests", "Run Tests"),
      width = 3,
      uiOutput("level_selector")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Variable Information", verbatimTextOutput("var_info")),
        tabPanel("Graph", plotOutput("var_plot", height = "500px")),
        tabPanel("Hypothesis Test", verbatimTextOutput("hypothesis_output"))
      ),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  


  selected_vars <- eventReactive(input$run_tests, {
    if (is.null(input$var1) || input$var1 == "" || is.null(input$var2) || input$var2 == "") {
      return(NULL)
    }
    list(var1 = input$var1, var2 = input$var2)
  }, ignoreNULL = FALSE)
  
  output$var_info <- renderPrint({
    sv <- selected_vars()
    req(sv)
    req(sv$var1, sv$var2)

      
      for (v in c(sv$var1, sv$var2)) {
        cat("=====================================\n")
        cat("Variable:", v, "\n")
        
        dict_row <- data_dictionary[data_dictionary$Variable == v, ]
        if (nrow(dict_row) > 0) {
          cat("Full Name:", dict_row$Full_Name, "\n")
          cat("Description:", dict_row$Description, "\n")
          cat("Cleaning Steps:", dict_row$Cleaning_Steps, "\n\n")
        }
      
        total_before <- nrow(df)
        total_after <- sum(!is.na(df[[v]]))
        cat("Data points before cleaning:", total_before, "\n")
        cat("Data points after cleaning:", total_after, "\n\n")
        
        if (v %in% num_vars) {
          vals <- df[[v]][!is.na(df[[v]])]
          cat("Type: Numeric\n")
          cat("Range:", range(vals), "\n")
          cat("Min:", min(vals), "\n")
          cat("Max:", max(vals), "\n")
          cat("Mean:", mean(vals), "\n")
          cat("Median:", median(vals), "\n\n")
        } else if (v %in% cat_vars) {
          vals <- df[[v]][!is.na(df[[v]])]
          cat("Type: Categorical\n")
          cat("Categories:\n")
          print(levels(vals))
          cat("\n")
        }
      }
    })
  
  output$level_selector <- renderUI({
    sv <- selected_vars()
    req(sv)
    var1 <- sv$var1
    var2 <- sv$var2
    
    if ((var1 %in% num_vars && var2 %in% cat_vars) ||
        (var2 %in% num_vars && var1 %in% cat_vars)) {
      
      cat_var <- if (var1 %in% cat_vars) var1 else var2
      lvls <- levels(df[[cat_var]])
      
      if (length(lvls) > 2) {
        tagList(
          selectInput("level_choice", "Choose two levels to compare:",
                      choices = lvls, multiple = TRUE, selected = lvls[1:2])
        )
      }
    }
  })
  
  output$var_plot <- renderPlot({
    sv <- selected_vars()
    req(sv)
    req(sv$var1, sv$var2)
    var1 <- sv$var1
    var2 <- sv$var2
    
    if (identical(var1, var2)) {
      plot.new()
      text(0.5, 0.5, "Please choose two different variables to plot.", cex = 1.2)
      return()
    }
    
    df_nona <- df %>% filter(!is.na(.data[[var1]]), !is.na(.data[[var2]]))
  
    
    library(ggplot2)
    
    pastel_blue_palette <- c(
      "#a1c9f4", "#c6dbf7", "#d7e8fb", "#a8d0ff", # soft blues
      "#7ea9e1", "#b0dfff", "#adc6ff", "#d0d9ff",
      "#8ca6ff", "#b2bfff", "#9db8ff", "#c0c9ff"
    )
    
    if (var1 %in% cat_vars && var2 %in% cat_vars) {
      ggplot(df_nona, aes_string(x = var1, fill = var2)) +
        geom_bar(position = "dodge") +
        scale_fill_manual(values = pastel_blue_palette) +
        labs(x = var1, fill = var2) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (var1 %in% cat_vars && var2 %in% num_vars) {
      ggplot(df_nona, aes_string(x = var1, y = var2, fill = var1)) +
        geom_boxplot() +
        scale_fill_manual(values = pastel_blue_palette) +
        labs(x = var1, y = var2) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (var1 %in% num_vars && var2 %in% cat_vars) {
      ggplot(df_nona, aes_string(x = var2, y = var1, color = var2)) +
        geom_point(alpha = 0.7) +
        scale_color_manual(values = pastel_blue_palette) +
        labs(x = var2, y = var1) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if ((var1 %in% num_vars) && (var2 %in% num_vars)) {
      ggplot(df_nona, aes_string(x = var1, y = var2)) +
        geom_point(alpha = 0.7, color = "#a1c9f4") +
        labs(x = var1, y = var2) +
        theme_minimal()
    }} )
    
    
  
  output$hypothesis_output <- renderPrint({
    sv <- selected_vars()
    req(sv)
    var1 <- sv$var1
    var2 <- sv$var2
    
    if (identical(var1, var2)) return()
    
    df_nona <- df %>% filter(!is.na(.data[[var1]]), !is.na(.data[[var2]]))
  
    if (var1 %in% cat_vars && var2 %in% cat_vars) {
      tbl <- table(df_nona[[var1]], df_nona[[var2]])
      print(chisq.test(tbl))
    }
  
    else if ((var1 %in% num_vars && var2 %in% cat_vars) || (var2 %in% num_vars && var1 %in% cat_vars)) {
      numeric_var <- ifelse(var1 %in% num_vars, var1, var2)
      cat_var <- ifelse(var1 %in% cat_vars, var1, var2)
      groups <- factor(df_nona[[cat_var]])
      vals <- df_nona[[numeric_var]]
      n_levels <- length(levels(groups))
      
      if (n_levels == 2) {
        print(t.test(vals ~ groups))
      } else if (!is.null(input$level_choice) && length(input$level_choice) == 2) {
        df_sub <- df_nona %>% filter(.data[[cat_var]] %in% input$level_choice)
        groups_sub <- factor(df_sub[[cat_var]])
        vals_sub <- df_sub[[numeric_var]]
        print(t.test(vals_sub ~ groups_sub))
      }
    }
    
    # Numeric vs Numeric -> just print summary
    else if (var1 %in% num_vars && var2 %in% num_vars) {
      cat("Summary of", var1, ":\n")
      print(summary(df_nona[[var1]]))
      cat("\nSummary of", var2, ":\n")
      print(summary(df_nona[[var2]]))
    }
  })
  

  

  
  
  }
  

shinyApp(ui, server)


