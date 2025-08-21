# app.R — Habit & Writing Pod Planner (writing + general habits)

# Packages ---------------------------------------------------------------
library(shiny)
library(DT)
library(dplyr)
library(lubridate)
library(ggplot2)
library(shinyjs)

# Helpers ---------------------------------------------------------------
weekday_choices <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")

load_csv_safe <- function(path, cols) {
  if (!file.exists(path)) return(cols[0,])
  df <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) cols[0,])
  # make sure columns exist
  missing <- setdiff(names(cols), names(df))
  for (m in missing) df[[m]] <- cols[[m]]
  df <- df[names(cols)]
  df
}

save_csv_safe <- function(df, path) {
  tryCatch(write.csv(df, path, row.names = FALSE), error = function(e) NULL)
}

as_minutes <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x) | x <= 0, 25, x) # default 25 mins if bad input
}

in_today <- function(days_str, today_label) {
  if (is.na(days_str) || days_str == "") return(TRUE) # empty = daily
  days <- trimws(strsplit(days_str, ",")[[1]])
  today_label %in% days
}

# Data skeletons --------------------------------------------------------
HABIT_COLS <- data.frame(
  id = character(),
  name = character(),
  category = character(),  # Writing, Health, Learning, Other
  cue = character(),
  craving = character(),
  response = character(),
  reward = character(),
  frequency = character(), # Daily or Custom
  days = character(),      # e.g., "Mon,Wed,Fri"
  pref_time = character(), # "HH:MM"
  duration_min = integer(),
  goal_short = character(),
  goal_medium = character(),
  goal_long = character(),
  stringsAsFactors = FALSE
)

LOG_COLS <- data.frame(
  date = as.Date(character()),
  habit = character(),
  minutes = integer(),
  note = character(),
  stringsAsFactors = FALSE
)

# UI -------------------------------------------------------------------
ui <- navbarPage(
  title = "Habit & Writing Pod Planner",
  id = "top",
  header = tagList(useShinyjs()),
  
  tabPanel("Plan",
           fluidRow(
             column(4,
                    h3("Create / update a habit"),
                    textInput("habit_name", "Habit name", value = "Writing"),
                    selectInput("habit_cat", "Category", c("Writing","Health","Learning","Other"), selected = "Writing"),
                    selectInput("habit_freq", "Frequency", c("Daily","Custom days"), selected = "Daily"),
                    checkboxGroupInput("habit_days", "If Custom, pick days", choices = weekday_choices, inline = TRUE),
                    textInput("habit_time", "Preferred start time (HH:MM)", value = "09:30"),
                    numericInput("habit_dur", "Default duration (minutes)", value = 90, min = 10, max = 240, step = 5),
                    tags$hr(),
                    h4("Habit loop"),
                    textAreaInput("habit_cue", "Cue (when/where/after what)", rows = 2, value = "After coffee + before email; phone on airplane mode; noise-cancelling on."),
                    textAreaInput("habit_craving", "Craving (why it matters)", rows = 2, value = "Feel progress; reduce anxiety; move manuscripts toward submission."),
                    textAreaInput("habit_response", "Response (action)", rows = 2, value = "Open the target doc; start 50-min deep block; write before editing."),
                    textAreaInput("habit_reward", "Reward (instant & weekly)", rows = 2, value = "✅ log streak + espresso; Friday: treat after 5 sessions."),
                    tags$hr(),
                    h4("Goals (vision)"),
                    textAreaInput("goal_short", "Short-term (≤30 days)", rows = 2, value = "Draft Results section + submit one figure for coauthor review."),
                    textAreaInput("goal_medium", "Medium-term (~6 months)", rows = 2, value = "Submit CRC proteomics paper + preprint of zebrafish review."),
                    textAreaInput("goal_long", "Long-term (~12 months)", rows = 2, value = "2 papers accepted; ProteoImporter v1.0 released."),
                    actionButton("add_habit", "Add / Update habit", class = "btn-primary")
             ),
             column(8,
                    h3("Your habits"),
                    DTOutput("habits_table"),
                    tags$small("Tip: double-click a cell to edit inline. Edits are auto-saved.")
             )
           )
  ),
  
  tabPanel("Today",
           fluidRow(
             column(5,
                    h3("Today's checklist"),
                    uiOutput("today_ui"),
                    actionButton("save_today", "Save checklist to log", class = "btn-success")
             ),
             column(7,
                    h3("Quick log"),
                    selectInput("ql_habit", "Habit", choices = NULL),
                    numericInput("ql_minutes", "Minutes", value = 25, min = 5, max = 240, step = 5),
                    textInput("ql_note", "Note (optional)", value = ""),
                    actionButton("ql_add", "Add to log"),
                    tags$hr(),
                    DTOutput("log_table")
             )
           )
  ),
  
  tabPanel("Timer",
           fluidRow(
             column(4,
                    h3("Focus timer (Pomodoro / 50-10)"),
                    selectInput("timer_habit", "Habit", choices = NULL),
                    numericInput("timer_minutes", "Focus minutes", value = 50, min = 5, max = 180, step = 5),
                    textInput("timer_note", "Note (optional)", value = "Deep block"),
                    actionButton("start_timer", "Start", class = "btn-primary"),
                    actionButton("stop_timer", "Stop", class = "btn-warning ml-2")
             ),
             column(8,
                    tags$style("#countdown {font-size: 72px; font-weight: 700; letter-spacing: 1px;}"),
                    br(), br(),
                    div(id = "countdown", textOutput("countdown"))
             )
           )
  ),
  
  tabPanel("Analytics",
           fluidRow(
             column(6,
                    h3("Minutes per habit (last 30 days)"),
                    plotOutput("minutes_plot", height = 320)
             ),
             column(6,
                    h3("Streaks"),
                    verbatimTextOutput("streak_text"),
                    tags$hr(),
                    downloadButton("dl_log", "Download log CSV")
             )
           )
  ),
  
  tabPanel("Pod",
           fluidRow(
             column(5,
                    h3("Writing pod preferences"),
                    checkboxGroupInput("pod_days", "Best day(s)", choices = weekday_choices, selected = c("Tue","Thu"), inline = TRUE),
                    selectInput("pod_time", "Time of day", c("Morning [9–12]","Mid-day [11–13]","Afternoon [13–16]","Evening [16–21]"), selected = "Morning [9–12]"),
                    selectInput("pod_freq", "Meet...", c("Weekly","Every two weeks","Monthly"), selected = "Weekly"),
                    selectInput("pod_dur", "Duration", c("1 hour","1.5 hours","2 hours"), selected = "1.5 hours"),
                    selectInput("pod_mode", "Mode", c("in-person","virtually","no preference"), selected = "no preference"),
                    textInput("pod_name", "Your name", value = "Ehsan Zangene"),
                    textInput("pod_email", "Your email", value = "ehsan.zangene@helsinki.fi"),
                    textInput("pod_group", "Group name suggestion", value = "Inkipoddies")
             ),
             column(7,
                    h3("Copy/paste into the Microsoft Form"),
                    verbatimTextOutput("pod_summary"),
                    tags$small("This is a convenience summary only; submit the real form separately.")
             )
           )
  ),
  
  tabPanel("Help",
           fluidRow(
             column(12,
                    h3("How to use"),
                    tags$ol(
                      tags$li("In 'Plan', add your Writing habit (and any others). Use the habit loop boxes to define cue → craving → response → reward."),
                      tags$li("In 'Today', tick off planned blocks or log a quick session."),
                      tags$li("Use 'Timer' to run a 25–50 min deep block; it auto-logs on completion."),
                      tags$li("Check 'Analytics' weekly; aim for 5+ writing blocks per week (≥250 min)."),
                      tags$li("Use 'Pod' to prepare answers before filling the Microsoft Form.")
                    ),
                    tags$hr(),
                    HTML("<b>Data</b>: 'habits.csv' and 'log.csv' are saved in the app folder for persistence.")
             )
           )
  )
)

# Server ----------------------------------------------------------------
server <- function(input, output, session) {
  # Reactive store
  rv <- reactiveValues(
    habits = HABIT_COLS[0,],
    log = LOG_COLS[0,],
    running = FALSE,
    time_left = 0L
  )
  
  # Load saved data
  rv$habits <- load_csv_safe("habits.csv", HABIT_COLS)
  rv$log <- load_csv_safe("log.csv", LOG_COLS)
  
  # Update habit choices everywhere
  observe({
    choices <- if (nrow(rv$habits) == 0) character() else rv$habits$name
    updateSelectInput(session, "ql_habit", choices = choices, selected = ifelse("Writing" %in% choices, "Writing", choices[1]))
    updateSelectInput(session, "timer_habit", choices = choices, selected = ifelse("Writing" %in% choices, "Writing", choices[1]))
  })
  
  # Add / update habit
  observeEvent(input$add_habit, {
    req(input$habit_name)
    
    new <- data.frame(
      id = if (nrow(rv$habits) == 0) "h1" else paste0("h", max(1L, max(as.integer(gsub("h", "", rv$habits$id)), na.rm = TRUE) + 1L)),
      name = input$habit_name,
      category = input$habit_cat,
      cue = input$habit_cue,
      craving = input$habit_craving,
      response = input$habit_response,
      reward = input$habit_reward,
      frequency = input$habit_freq,
      days = if (identical(input$habit_freq, "Custom days")) paste(input$habit_days, collapse = ",") else "",
      pref_time = input$habit_time,
      duration_min = as.integer(input$habit_dur),
      goal_short = input$goal_short,
      goal_medium = input$goal_medium,
      goal_long = input$goal_long,
      stringsAsFactors = FALSE
    )
    
    # If habit name exists, update; else append
    if (nrow(rv$habits) && input$habit_name %in% rv$habits$name) {
      rv$habits[rv$habits$name == input$habit_name, ] <- new
    } else {
      rv$habits <- bind_rows(rv$habits, new)
    }
    save_csv_safe(rv$habits, "habits.csv")
    showNotification("Habit saved.", type = "message")
  })
  
  # Editable habits table
  output$habits_table <- renderDT({
    datatable(rv$habits, selection = "single", editable = TRUE, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  observeEvent(input$habits_table_cell_edit, {
    info <- input$habits_table_cell_edit
    i <- info$row; j <- info$col; v <- info$value
    rv$habits[i, j] <- v
    save_csv_safe(rv$habits, "habits.csv")
  })
  
  # Today's checklist UI
  output$today_ui <- renderUI({
    if (nrow(rv$habits) == 0) return(helpText("No habits yet. Add one on the Plan tab."))
    today_label <- wday(Sys.Date(), label = TRUE, abbr = TRUE) |> as.character()
    today_label <- substr(today_label, 1, 1) |> toupper() |> paste0(substr(today_label,2,3)) # e.g., "Mon"
    
    todays <- rv$habits %>% rowwise() %>% mutate(show = ifelse(frequency == "Daily", TRUE, in_today(days, today_label))) %>% ungroup() %>% filter(show)
    if (nrow(todays) == 0) return(helpText("No scheduled habits today. Enjoy a rest day ✨"))
    
    lapply(seq_len(nrow(todays)), function(i) {
      h <- todays[i,]
      wellPanel(
        strong(h$name), span(paste0(" (", h$duration_min, " min @ ", h$pref_time, ")")), br(),
        tags$ul(
          tags$li(strong("Cue:"), h$cue),
          tags$li(strong("Response:"), h$response),
          tags$li(strong("Reward:"), h$reward)
        ),
        checkboxInput(paste0("done_", h$id), "Completed", value = FALSE)
      )
    })
  })
  
  # Quick log add
  observeEvent(input$ql_add, {
    req(input$ql_habit)
    new <- data.frame(date = Sys.Date(), habit = input$ql_habit, minutes = as.integer(as_minutes(input$ql_minutes)), note = input$ql_note, stringsAsFactors = FALSE)
    rv$log <- bind_rows(rv$log, new)
    save_csv_safe(rv$log, "log.csv")
    showNotification("Logged.", type = "message")
  })
  
  # Save checklist to log
  observeEvent(input$save_today, {
    if (nrow(rv$habits) == 0) return(NULL)
    todays <- isolate(rv$habits)
    if (nrow(todays) == 0) return(NULL)
    
    for (i in seq_len(nrow(todays))) {
      h <- todays[i,]
      input_id <- paste0("done_", h$id)
      if (!is.null(input[[input_id]]) && isTRUE(input[[input_id]])) {
        new <- data.frame(date = Sys.Date(), habit = h$name, minutes = as.integer(h$duration_min), note = "Checklist", stringsAsFactors = FALSE)
        rv$log <- bind_rows(rv$log, new)
      }
    }
    save_csv_safe(rv$log, "log.csv")
    showNotification("Checklist saved to log.", type = "message")
  })
  
  # Timer logic
  observeEvent(input$start_timer, {
    req(input$timer_habit)
    rv$time_left <- as.integer(as_minutes(input$timer_minutes)) * 60L
    rv$running <- TRUE
  })
  
  observeEvent(input$stop_timer, { rv$running <- FALSE })
  
  observe({
    if (!rv$running) return()
    invalidateLater(1000)
    rv$time_left <- rv$time_left - 1L
    if (rv$time_left <= 0L) {
      rv$running <- FALSE
      # auto-log
      new <- data.frame(date = Sys.Date(), habit = input$timer_habit, minutes = as.integer(as_minutes(input$timer_minutes)), note = input$timer_note, stringsAsFactors = FALSE)
      rv$log <- bind_rows(rv$log, new)
      save_csv_safe(rv$log, "log.csv")
      showNotification("⏱️ Session complete & logged!", type = "message")
    }
  })
  
  output$countdown <- renderText({
    mins <- floor(rv$time_left / 60)
    secs <- rv$time_left %% 60
    sprintf("%02d:%02d", pmax(mins,0), pmax(secs,0))
  })
  
  # Log + analytics
  output$log_table <- renderDT({
    datatable(rv$log %>% arrange(desc(date)), options = list(pageLength = 7))
  })
  
  output$minutes_plot <- renderPlot({
    if (nrow(rv$log) == 0) return()
    rv$log %>% filter(date >= Sys.Date() - 30) %>%
      group_by(habit) %>% summarise(minutes = sum(minutes, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = reorder(habit, minutes), y = minutes)) +
      geom_col() + coord_flip() +
      labs(x = NULL, y = "Minutes (last 30 days)") +
      theme_minimal(base_size = 13)
  })
  
  output$streak_text <- renderText({
    if (nrow(rv$log) == 0) return("No sessions yet. Your streak starts today ✨")
    df <- rv$log %>% mutate(d = as.Date(date)) %>% arrange(desc(d)) %>% distinct(d)
    # compute writing streak specifically and overall
    wr <- rv$log %>% filter(grepl("Writing", habit, ignore.case = TRUE)) %>% mutate(d = as.Date(date)) %>% arrange(desc(d)) %>% distinct(d)
    streak_of <- function(days) {
      if (nrow(days) == 0) return(0L)
      s <- 0L; cur <- Sys.Date()
      while (cur %in% days$d) { s <- s + 1L; cur <- cur - 1 }
      s
    }
    paste0(
      "Overall daily streak: ", streak_of(df), " day(s)\n",
      "Writing streak: ", streak_of(wr), " day(s)\n",
      "Sessions this week: ", nrow(rv$log %>% filter(date >= floor_date(Sys.Date(), 'week')))
    )
  })
  
  output$dl_log <- downloadHandler(
    filename = function() paste0("habit_log_", Sys.Date(), ".csv"),
    content = function(file) write.csv(rv$log, file, row.names = FALSE)
  )
  
  # Pod summary (for Microsoft Form)
  output$pod_summary <- renderText({
    paste0(
      "1. I would like to work with a writing pod: YES\n",
      "2. Best day(s): ", paste(input$pod_days, collapse = ", "), "\n",
      "3. Best time of day: ", input$pod_time, "\n",
      "4. Specific time slot: ", ifelse(input$pod_time == "Morning [9–12]", "09:30–11:00", ifelse(input$pod_time == "Afternoon [13–16]", "13:30–15:00", "11:00–12:30")), " (EET/EEST)\n",
      "5. I would like to meet: ", input$pod_freq, "\n",
      "6. I would like to meet for: ", input$pod_dur, "\n",
      "7. I prefer to meet: ", input$pod_mode, "\n",
      "8. Name: ", input$pod_name, "\n",
      "9. Email: ", input$pod_email, "\n",
      "10. Writing Pod group name: ", input$pod_group, "\n"
    )
  })
}

shinyApp(ui, server)
