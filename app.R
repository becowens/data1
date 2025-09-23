library(shiny)
library(readxl)
library(dplyr)
library(janitor)
library(DT)

# function to clean short video app usage times
clean_video_time <- function(x) {
  if (is.na(x)) return(NA)
  x <- tolower(trimws(x))
  
  # zero cases
  if (x %in% c("never", "none", "nil", "no", "0.0", "0")) return(0)
  
  # combined times (e.g., "1h30m")
  if (grepl("^[0-9]+h[0-9]+", x)) {
    parts <- unlist(strsplit(gsub("hr|min", "", x), "h"))
    hr <- suppressWarnings(as.numeric(parts[1]))
    mins <- ifelse(length(parts) > 1, suppressWarnings(as.numeric(parts[2])), 0)
    return(hr + mins/60)
  }
  
  # ranges
  if (grepl("-", x) | grepl("to", x)) {
    parts <- unlist(strsplit(x, "-|to"))
    nums <- suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", parts)))
    nums <- nums[!is.na(nums)]
    if (length(nums) > 0) return(mean(nums))
  }
  
  # plus sign (e.g., "5+")
  if (grepl("\\+$", x)) {
    num <- suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", x)))
    return(num)
  }
  
  # hours
  if (grepl("hour|hr|hrs|h", x)) {
    num <- suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", x)))
    return(num)
  }
  
  # minutes
  if (grepl("min|m", x)) {
    num <- suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", x)))
    return(num / 60)
  }
  
  # plain numbers
  if (grepl("^[0-9]+(\\.[0-9]+)?$", x)) {
    val <- as.numeric(x)
    return(ifelse(val > 24, val/60, val))
  }
  
  return(NA)
}

ui <- fluidPage(
  titlePanel("Data Cleaning Demo"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel File", 
                accept = c(".xlsx", ".xls"))
    ),
    mainPanel(
      DTOutput("cleaned_table")
    )
  )
)

server <- function(input, output, session) {
  
  cleaned_data <- reactive({
    req(input$file)  # wait for upload
    
    df <- read_excel(input$file$datapath) %>%
      janitor::clean_names()
    
    cleaned <- df %>%
      mutate(
        sleep_schedule_consistency = as.numeric(how_consistent_would_you_rate_your_sleep_schedule),
        time_short_video_apps_per_day = sapply(how_much_time_do_you_spend_on_short_video_apps_like_tiktok_or_reels_every_day,
                                               clean_video_time),
        anxiety_frequency = as.numeric(how_often_would_you_say_you_feel_anxious_on_a_daily_basis),
        what_os = what_computer_os_operating_system_are_you_currently_using,
        assignment_raw = do_you_submit_assignments_on_time,
        submission_hour = suppressWarnings(as.numeric(substr(timestamp, 12, 13))),
        submission_hour = ifelse(is.na(submission_hour), 0, submission_hour),
        day_night = ifelse(submission_hour >= 7 & submission_hour < 19, "Day", "Night"),
        os_group = ifelse(tolower(trimws(what_os)) %in% c("windows", "linux"), "Windows/Linux", "MacOS"),
        assignment_timely_bin = ifelse(is.na(assignment_raw), NA,
                                       ifelse(assignment_raw %in% c("Always", "Usually"),
                                              "Timely", "Not timely"))
      ) %>%
      select(
        day_night,
        sleep_schedule_consistency,
        time_short_video_apps_per_day,
        anxiety_frequency,
        os_group,
        assignment_timely_bin,
        assignment_raw,
        submission_hour
      ) %>%
      filter(!is.na(sleep_schedule_consistency),
             !is.na(time_short_video_apps_per_day),
             !is.na(anxiety_frequency))
    
    # remove outliers with z-score > 3
    z_scores <- scale(cleaned$time_short_video_apps_per_day)
    cleaned <- cleaned[abs(z_scores) <= 3, ]
    
    return(cleaned)
  })
  
  output$cleaned_table <- renderDT({
    cleaned_data()
  })
}

shinyApp(ui, server)

