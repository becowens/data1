library(shiny)
library(readxl)
library(dplyr)
library(janitor)
library(DT)
library(ggplot2)

df <- readRDS("cleaned_data.rds") 

all_vars <- c(cat_vars, num_vars)
var_type <- c(rep("Categorical", length(cat_vars)), rep("Numeric", length(num_vars)))

ui <- fluidPage(
  titlePanel("DATA2902 Survey Visualisations and Hypothesis Tests"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "var1", 
        "Choose Variable 1", 
        choices = setNames(all_vars, paste0(all_vars, " (", var_type, ")")),
        options = list(
          render = I(
            '{
               option: function(item, escape) {
                 var color = item.label.includes("Categorical") ? "blue" : "green";
                 return "<div style=\"color:" + color + "\">" + escape(item.label) + "</div>";
               }
             }'
          )
        )
      ),
      selectizeInput(
        "var2", 
        "Choose Variable 2", 
        choices = setNames(all_vars, paste0(all_vars, " (", var_type, ")")),
        options = list(
          render = I(
            '{
               option: function(item, escape) {
                 var color = item.label.includes("Categorical") ? "blue" : "green";
                 return "<div style=\"color:" + color + "\">" + escape(item.label) + "</div>";
               }
             }'
          )
        )
      ),
      hr(),
      actionButton("run_tests", "Run Tests")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Variable Information", verbatimTextOutput("var_info")),
        tabPanel("Graph", plotOutput("var_plot")),
        tabPanel("Hypothesis Test", verbatimTextOutput("hypothesis_output"))
      )
    )
  )
)





server <- function(input, output, session) {
  
  output$cat_level_selector <- renderUI({
    req(input$cat_var)
    levs <- levels(df[[input$cat_var]])
    if(length(levs) > 2) {
      selectizeInput("selected_levels", "Select two variables",
                     choices = levs, selected = levs[1:2], multiple = TRUE)
    }
  })
  

  
  
  output$cat_plot <- renderPlot({
    req(input$cat_var1, input$cat_var2)
    
    df_nona  <- df %>%
      filter(!is.na(.data[[input$num_var]]), !is.na(.data[[input$cat_var]]))
    ggplot(df_nona, aes_string(x = input$cat_var1, fill = input$cat_var2)) +
      geom_bar(position = "dodge") +
      labs(x = input$cat_var1, fill = input$cat_var2) +
      theme_minimal()
  })
  
  
  
  
  output$chi_sq_test <- renderPrint({
    req(input$cat_var1, input$cat_var2)
    tbl <- table(df[[input$cat_var1]], df[[input$cat_var2]])
    test <- chisq.test(tbl)
    print(test)
  })
  
  output$chi_assumptions <- renderPrint({
    req(input$cat_var1, input$cat_var2)
    tbl <- table(df[[input$cat_var1]], df[[input$cat_var2]])
    expected <- chisq.test(tbl)$expected
    })
  
  
  output$num_cat_plot <- renderPlot({
    req(input$num_var, input$cat_var)
    plot_data <- df
    if(!is.null(input$selected_levels)){
      plot_data <- plot_data %>% filter(.data[[input$cat_var]] %in% input$selected_levels)
    }
    
    ggplot(plot_data, aes_string(x = input$cat_var, y = input$num_var)) +
      geom_boxplot() +
      geom_jitter(width = 0.15, alpha = 0.5) +
      labs(x = input$cat_var, y = input$num_var) +
      theme_minimal()
  })
  
  
  
  
  output$t_test_results <- renderPrint({
    req(input$num_var, input$cat_var)
    
    df_sub <- df
    cat_var <- input$cat_var
    num_var <- input$num_var
    
    if(!is.null(input$selected_levels)) {
      df_sub <- df_sub %>% filter(.data[[cat_var]] %in% input$selected_levels)
    }
    
    groups <- factor(df_sub[[cat_var]])
    values <- df_sub[[num_var]]
    
    n_levels <- length(levels(groups))
    
    if(n_levels == 2){
      test_res <- t.test(values ~ groups)
      cat("Two-sample t-test results:\n")
      print(test_res)
    } else if(n_levels > 2) {
      cat("Group normality checks (Shapiro-Wilk test):\n")
      shapiro_results <- tapply(values, groups, function(x) shakapiro.test(x)$p.value)
      print(shapiro_results)
      
      cat("\nANOVA test results:\n")
      anova_res <- aov(values ~ groups)
      print(summary(anova_res))
      
    } 
    }
)
  
  
  
  
  
  }
  


  
  
  
  



shinyApp(ui, server)
