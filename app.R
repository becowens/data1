library(shiny)
library(readxl)
library(dplyr)
library(janitor)
library(DT)
library(ggplot2)

df <- readRDS("cleaned_data.rds") 


ui <- fluidPage(
  selectInput("cat_var", "Choose categorical variable", choices = cat_vars),
  uiOutput("cat_level_selector"),
  selectInput("cat_var1", "Choose first categorical variable", choices = cat_vars),
  selectInput("cat_var2", "Choose second categorical variable", choices = cat_vars),
  plotOutput("cat_plot")
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
    ggplot(df, aes_string(x = input$cat_var1, fill = input$cat_var2)) +
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
  }
  
  
  
  
  



shinyApp(ui, server)
