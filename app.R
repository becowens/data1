options(repos = c(CRAN = "https://cran.rstudio.com/"))
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")



library(shiny)
library(readxl)
library(dplyr)
library(DT)
library(ggplot2)
library(forcats)


df <- readRDS("clean_data.rds")
df <- df %>% mutate(across(where(is.character), as.factor))
cat_vars <- names(df)[sapply(df, is.factor)]
num_vars <- names(df)[sapply(df, is.numeric)]
all_vars <- c(cat_vars, num_vars)
var_type <- c(rep("Categorical", length(cat_vars)), rep("Numeric", length(num_vars)))


choices_named <- data_dictionary$Variable
names(choices_named) <- data_dictionary$Short_Name

choices_display <- setNames(choices_named, paste0(names(choices_named), " (", var_type[match(choices_named, all_vars)], ")"))

ui <- fluidPage(
  titlePanel("DATA2902 Survey Visualisations and Hypothesis Tests"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "var1",
        "Choose Variable 1",
        choices = c(" " = "", choices_display),
        selected = "", 
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
        choices = c(" " = "", choices_display),
        selected = "", 
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
      uiOutput("level_selector"),
      uiOutput("graph_level_selector"),
      numericInput("alpha", "Significance level (alpha)", value = 0.05, min = 0.001, max = 0.1, step = 0.005),
      actionButton("run_tests", "Run Tests"),
      width = 3,
      checkboxInput("show_warnings", "Show warnings", value = FALSE),
      
      
      
      verbatimTextOutput("warnings_output")
      
      
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
  
  
  output$graph_level_selector <- renderUI({
    sv <- selected_vars()
    req(sv)
    var1 <- sv$var1
    var2 <- sv$var2
    
    cat_vars_selected <- c(var1, var2)[c(var1, var2) %in% cat_vars]
    
    if (length(cat_vars_selected) > 0) {
      checkbox_inputs <- lapply(cat_vars_selected, function(cv) {
        lvls <- levels(df[[cv]])
        checkboxGroupInput(
          inputId = paste0("graph_levels_", cv),
          label = paste("Select levels to include for", data_dictionary$Short_Name[data_dictionary$Variable == cv]),
          choices = lvls,
          selected = lvls  
        )
      })
      do.call(tagList, checkbox_inputs)
    }
  })
  
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
          selectInput(
            "level_choice",
            "Choose exactly two levels to compare:",
            choices = lvls,
            multiple = TRUE,
            selected = NULL   
          ),
          uiOutput("level_warning") 
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
    
    df_nona_unfilered <- df %>% filter(!is.na(.data[[var1]]), !is.na(.data[[var2]]))
    
    
    df_nona<- df_nona_unfilered
    
    cat_vars_selected <- c(var1, var2)[c(var1, var2) %in% cat_vars]
    for (cv in cat_vars_selected) {
      selected_lvls <- input[[paste0("graph_levels_", cv)]]
      if (!is.null(selected_lvls)) {
        df_nona <- df_nona %>% filter(.data[[cv]] %in% selected_lvls)
      }
    }
  
  
    
    pastel_blue_palette <- c(
      "#a1c9f4", "#c6dbf7", "#d7e8fb", "#a8d0ff",
      "#7ea9e1", "#b0dfff", "#adc6ff", "#d0d9ff",
      "#8ca6ff", "#b2bfff", "#9db8ff", "#c0c9ff"
    )
    
    x_label <- data_dictionary$Short_Name[data_dictionary$Variable == var1]
    y_label <- data_dictionary$Short_Name[data_dictionary$Variable == var2]
    
    if (var1 %in% cat_vars && var2 %in% cat_vars) {
      ggplot(df_nona, aes_string(x = var1, fill = var2)) +
        geom_bar(position = "dodge") +
        scale_fill_manual(name = y_label, values = pastel_blue_palette) +
        labs(x = x_label, y = y_label, title = paste(x_label, "vs", y_label)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (var1 %in% cat_vars && var2 %in% num_vars) {
      df_nona[[var1]] <- factor(df_nona[[var1]])
      ggplot(df_nona, aes_string(x = var1, y = var2, fill = var1)) +
        geom_boxplot() +
        scale_fill_manual(name = x_label, values = pastel_blue_palette) +
        labs(x = x_label, y = y_label, title = paste(x_label, "vs", y_label)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))}
      
      else if (var2 %in% cat_vars && var1 %in% num_vars) {
        df_nona[[var2]] <- factor(df_nona[[var2]])
        ggplot(df_nona, aes_string(x = var2, y = var1, fill = var2)) +
          geom_boxplot() +
          scale_fill_manual(name = y_label, values = pastel_blue_palette) +
          labs(x = y_label, y = x_label, title = paste(y_label, "vs", x_label)) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (var1 %in% num_vars && var2 %in% cat_vars) {
      ggplot(df_nona, aes_string(x = var2, y = var1, color = var2)) +
        geom_point(alpha = 0.7) +
        scale_color_manual(name = y_label, values = pastel_blue_palette) +
        labs(x = y_label, y = x_label, title = paste(x_label, "vs", y_label)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (var1 %in% num_vars && var2 %in% num_vars) {
      ggplot(df_nona, aes_string(x = var1, y = var2)) +
        geom_point(alpha = 0.7, color = "#a1c9f4") +
        labs(x = x_label, y = y_label, title = paste(x_label, "vs", y_label)) +
        theme_minimal()}
  
    })
    
  
  test_warnings <- reactiveVal(NULL)
  
    
  
  output$hypothesis_output <- renderPrint({
    test_warnings(NULL) 
    sv <- selected_vars()
    req(sv)
    var1 <- sv$var1
    var2 <- sv$var2
    
    if (identical(var1, var2)) return()
    
    df_nona <- df %>% filter(!is.na(.data[[var1]]), !is.na(.data[[var2]]))
    
    if (var1 %in% cat_vars && var2 %in% cat_vars) {
      tbl <- table(df_nona[[var1]], df_nona[[var2]])
      chi_result <- withCallingHandlers(
        chisq.test(tbl),
        warning = function(w) {
          test_warnings(paste("Chi-square warning:", conditionMessage(w)))
          invokeRestart("muffleWarning")  
        }
      )
      print(chi_result)
      alpha <- input$alpha
      if (chi_result$p.value < alpha) {
        cat("\nThe result is significant at alpha =", alpha, "\n")
      } else {
        cat("\nThe result is not significant at alpha =", alpha, "\n")
      }
    
    } else if ((var1 %in% num_vars && var2 %in% cat_vars) || 
               (var2 %in% num_vars && var1 %in% cat_vars)) {
      
      numeric_var <- ifelse(var1 %in% num_vars, var1, var2)
      cat_var <- ifelse(var1 %in% cat_vars, var1, var2)
      groups <- factor(df_nona[[cat_var]])
      vals <- df_nona[[numeric_var]]
      n_levels <- length(levels(groups))
      
      if (n_levels == 2) {
        t_result <- withCallingHandlers(
          t.test(vals ~ groups),
          warning = function(w) {
            test_warnings(paste("t-test warning:", conditionMessage(w)))
            invokeRestart("muffleWarning")
          }
        )
        print(t_result)
        alpha <- input$alpha
        if (t_result$p.value < alpha) {
          cat("\nThe difference between the two groups is significant at alpha =", alpha, "\n")
        } else {
          cat("\nThe difference between the two groups is not significant at alpha =", alpha, "\n")
        }
        
      } else {
        if (is.null(input$level_choice) || length(input$level_choice) != 2) {
          cat("Please select exactly two levels to run the t-test.\n")
          return()
        }
        
        df_sub <- df_nona %>% filter(.data[[cat_var]] %in% input$level_choice)
        groups_sub <- factor(df_sub[[cat_var]])
        vals_sub <- df_sub[[numeric_var]]
        
        t_result <- withCallingHandlers(
          t.test(vals_sub ~ groups_sub),
          warning = function(w) {
            test_warnings(paste("t-test warning:", conditionMessage(w)))
            invokeRestart("muffleWarning")
          }
        )
        print(t_result)
        alpha <- input$alpha
        if (t_result$p.value < alpha) {
          cat("\nThe difference between the two selected groups is significant at alpha =", alpha, "\n")
        } else {
          cat("\nThe difference between the two selected groups is not significant at alpha =", alpha, "\n")
        }
      }
      
    } else if (var1 %in% num_vars && var2 %in% num_vars) {
      cat("Summary of", var1, ":\n")
      print(summary(df_nona[[var1]]))
      cat("\nSummary of", var2, ":\n")
      print(summary(df_nona[[var2]]))
      
      cat("No hypothesis test for numeric vs numeric.\n")
      cor_val <- cor(df_nona[[var1]], df_nona[[var2]], use = "complete.obs")
      cat("R² =", round(cor_val^2, 3), "\n")
    }
  })
  
  
  output$warnings_output <- renderPrint({
    if (!input$show_warnings) return(invisible())
    
    w <- test_warnings()
    if (is.null(w)) {
      cat("No warnings detected.")
    } else {
      cat(w)
    }
  })
  
  output$level_warning <- renderUI({
    req(input$level_choice)
    if (length(input$level_choice) != 2) {
      div(style = "color: red; font-weight: bold;",
          "Please select exactly two levels to run the test.")
    }
  })
  

  

  
  
  }
  

shinyApp(ui, server)


