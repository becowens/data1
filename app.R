# app.R
library(shiny)
library(readxl)
library(dplyr)
library(janitor)
library(DT)
library(ggplot2)
library(forcats)

# ---- Load data ----
# Make sure cleaned_data.rds is in the app working directory
df <- readRDS("cleaned_data.rds")

# Convert character columns to factors (common source of surprises)
df <- df %>% mutate(across(where(is.character), as.factor))

# Identify categorical and numeric variables
cat_vars <- names(df)[sapply(df, is.factor)]
num_vars <- names(df)[sapply(df, is.numeric)]

# If there are no categorical or no numeric variables, handle gracefully
all_vars <- c(cat_vars, num_vars)
var_type <- c(rep("Categorical", length(cat_vars)), rep("Numeric", length(num_vars)))

# Create named choices such that the display includes type but the value is the true var name
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
      width = 3
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
  
  # Helper: safe console printing (both to R console and to Shiny log)
  log_msg <- function(...) {
    txt <- paste0(..., collapse = "")
    cat(txt, "\n")
    message(txt)  # also appears in Shiny log
    flush.console()
  }
  
  # Debug: print when button clicked
  observeEvent(input$run_tests, {
    log_msg("run_tests button clicked")
    log_msg("Selected var1: ", input$var1)
    log_msg("Selected var2: ", input$var2)
  })
  
  # Reactive: returns a list of selected vars when run_tests is clicked
  selected_vars <- eventReactive(input$run_tests, {
    # Protect against missing selections
    if (is.null(input$var1) || input$var1 == "" || is.null(input$var2) || input$var2 == "") {
      return(NULL)
    }
    list(var1 = input$var1, var2 = input$var2)
  }, ignoreNULL = FALSE) # allow initial display if defaults provided
  
  # Variable info
  output$var_info <- renderPrint({
    sv <- selected_vars()
    req(sv)
    req(sv$var1, sv$var2)
    var1 <- sv$var1
    var2 <- sv$var2
    
    # Basic checks
    if (!(var1 %in% names(df))) {
      cat("Variable 1 not found in data:", var1, "\n")
      return()
    }
    if (!(var2 %in% names(df))) {
      cat("Variable 2 not found in data:", var2, "\n")
      return()
    }
    
    cat("Variable 1:", var1, "\n")
    print(summary(df[[var1]]))
    cat("\nClass:", class(df[[var1]]), "; Missing values:", sum(is.na(df[[var1]])), "\n\n")
    
    cat("Variable 2:", var2, "\n")
    print(summary(df[[var2]]))
    cat("\nClass:", class(df[[var2]]), "; Missing values:", sum(is.na(df[[var2]])), "\n")
  })
  
  # Plot output
  output$var_plot <- renderPlot({
    sv <- selected_vars()
    req(sv)
    req(sv$var1, sv$var2)
    var1 <- sv$var1
    var2 <- sv$var2
    
    # Prevent plotting if same variable chosen twice (still possible)
    if (identical(var1, var2)) {
      plot.new()
      text(0.5, 0.5, "Please choose two different variables to plot.", cex = 1.2)
      return()
    }
    
    df_nona <- df %>% filter(!is.na(.data[[var1]]), !is.na(.data[[var2]]))
    
    log_msg("Plotting variables: ", var1, " and ", var2)
    log_msg("Number of rows after NA removal: ", nrow(df_nona))
    
    # Choose plot based on types
    if (var1 %in% cat_vars && var2 %in% cat_vars) {
      log_msg("Plot type: categorical vs categorical (dodge bar)")
      ggplot(df_nona, aes_string(x = var1, fill = var2)) +
        geom_bar(position = "dodge") +
        labs(x = var1, fill = var2) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (var1 %in% cat_vars && var2 %in% num_vars) {
      log_msg("Plot type: categorical vs numeric (boxplot)")
      ggplot(df_nona, aes_string(x = var1, y = var2)) +
        geom_boxplot() +
        labs(x = var1, y = var2) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (var1 %in% num_vars && var2 %in% cat_vars) {
      log_msg("Plot type: numeric vs categorical (boxplot)")
      ggplot(df_nona, aes_string(x = var2, y = var1)) +
        geom_boxplot() +
        labs(x = var2, y = var1) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if ((var1 %in% num_vars) && (var2 %in% num_vars)) {
      log_msg("Plot type: numeric vs numeric (scatterplot)")
      ggplot(df_nona, aes_string(x = var1, y = var2)) +
        geom_point(alpha = 0.7) +
        labs(x = var1, y = var2) +
        theme_minimal()
    } else {
      plot.new()
      text(0.5, 0.5, "Unsupported variable types for plotting.", cex = 1.2)
    }
  })
  
  # Hypothesis test output
  output$hypothesis_output <- renderPrint({
    sv <- selected_vars()
    req(sv)
    req(sv$var1, sv$var2)
    var1 <- sv$var1
    var2 <- sv$var2
    
    if (identical(var1, var2)) {
      cat("Cannot run a hypothesis test on the same variable twice. Choose two different variables.\n")
      return()
    }
    
    df_nona <- df %>% filter(!is.na(.data[[var1]]), !is.na(.data[[var2]]))
    if (nrow(df_nona) == 0) {
      cat("No observations after removing missing data - cannot run tests.\n")
      return()
    }
    
    log_msg("Performing hypothesis test on variables: ", var1, " and ", var2)
    
    # Categorical vs Categorical
    if (var1 %in% cat_vars && var2 %in% cat_vars) {
      log_msg("Running Chi-square test")
      tbl <- table(df_nona[[var1]], df_nona[[var2]])
      # Use tryCatch to safely run chisq.test and fall back to simulate.p.value if needed
      chi_result <- tryCatch({
        chisq.test(tbl)
      }, warning = function(w) {
        # sometimes warning about Chi-square approximation; attempt with simulate.p.value=TRUE
        log_msg("Chi-square warning: ", conditionMessage(w), " — retrying with simulate.p.value = TRUE")
        chisq.test(tbl, simulate.p.value = TRUE)
      }, error = function(e) {
        log_msg("Chi-square error: ", conditionMessage(e))
        return(NULL)
      })
      if (is.null(chi_result)) {
        cat("Chi-square test could not be computed.\n")
      } else {
        print(chi_result)
        expected <- chi_result$expected
        if (any(expected < 5)) {
          cat("\nWarning: Some expected counts < 5 - Chi-square approximation may be invalid. Consider Fisher's exact test (if table is small) or combining categories.\n")
        }
      }
    }
    
    # Categorical vs Numeric
    else if (var1 %in% cat_vars && var2 %in% num_vars) {
      log_msg("Running t-test / ANOVA for categorical (var1) vs numeric (var2)")
      groups <- factor(df_nona[[var1]])
      vals <- df_nona[[var2]]
      n_levels <- length(levels(groups))
      cat("Groups (levels):", paste(levels(groups), collapse = ", "), "\n")
      if (n_levels == 2) {
        tt <- tryCatch(t.test(vals ~ groups), error = function(e) e)
        print(tt)
        cat("\nShapiro-Wilk p-values by group:\n")
        sw <- tryCatch(tapply(vals, groups, function(x) if (length(x) >= 3) shapiro.test(x)$p.value else NA), error = function(e) NA)
        print(sw)
      } else if (n_levels > 2) {
        cat("Shapiro-Wilk p-values by group:\n")
        sw <- tryCatch(tapply(vals, groups, function(x) if (length(x) >= 3) shapiro.test(x)$p.value else NA), error = function(e) NA)
        print(sw)
        cat("\nANOVA test:\n")
        print(summary(aov(vals ~ groups)))
      } else {
        cat("Categorical variable has fewer than 2 levels - cannot run test.\n")
      }
    }
    
    # Numeric vs Categorical (same as above swapped)
    else if (var1 %in% num_vars && var2 %in% cat_vars) {
      log_msg("Running t-test / ANOVA for numeric (var1) vs categorical (var2)")
      groups <- factor(df_nona[[var2]])
      vals <- df_nona[[var1]]
      n_levels <- length(levels(groups))
      cat("Groups (levels):", paste(levels(groups), collapse = ", "), "\n")
      if (n_levels == 2) {
        tt <- tryCatch(t.test(vals ~ groups), error = function(e) e)
        print(tt)
        cat("\nShapiro-Wilk p-values by group:\n")
        sw <- tryCatch(tapply(vals, groups, function(x) if (length(x) >= 3) shapiro.test(x)$p.value else NA), error = function(e) NA)
        print(sw)
      } else if (n_levels > 2) {
        cat("Shapiro-Wilk p-values by group:\n")
        sw <- tryCatch(tapply(vals, groups, function(x) if (length(x) >= 3) shapiro.test(x)$p.value else NA), error = function(e) NA)
        print(sw)
        cat("\nANOVA test:\n")
        print(summary(aov(vals ~ groups)))
      } else {
        cat("Categorical variable has fewer than 2 levels - cannot run test.\n")
      }
    }
    
    # Numeric vs Numeric
    else if ((var1 %in% num_vars) && (var2 %in% num_vars)) {
      log_msg("Running Pearson correlation test")
      # check at least 3 observations
      if (nrow(df_nona) < 3) {
        cat("Not enough observations to compute correlation test (need at least 3).\n")
      } else {
        print(cor.test(df_nona[[var1]], df_nona[[var2]], method = "pearson"))
      }
    } else {
      cat("Invalid variable combination for hypothesis test\n")
    }
  })
  
}

shinyApp(ui, server)


