library(shiny)
library(readxl)
library(dplyr)
library(janitor)
library(DT)

finaldf <- df_cleaned

cat_vars <- names(df)[sapply(df, is.factor)]
num_vars <- names(df)[sapply(df, is.numeric)]

server <- function(input, output, session) {
  output$cat_plot <- renderPlot({
    req(input$cat_var1, input$cat_var2)
    ggplot(df, aes_string(x = input$cat_var1, fill = input$cat_var2)) +
      geom_bar(position = "dodge") +
      labs(x = input$cat_var1, fill = input$cat_var2) +
      theme_minimal()
  })}


shinyApp(ui, server)
