# EcoScore - online multi-user prototype
#
# Based on the working v2 offline scoring workflow.
# The user interface remains intentionally close to v2.
# Scores are stored centrally in PostgreSQL/Supabase instead of scores.csv.
#
# Required environment variables on the server:
#   DB_HOST
#   DB_PORT
#   DB_NAME
#   DB_USER
#   DB_PASSWORD
#   DB_SSLMODE   (usually require)
#
# Never put the database password in this file or in GitHub.

library(shiny)
library(DT)
library(dplyr)
library(DBI)
library(RPostgres)

# =========================================================
# MASTER DATA
# =========================================================

pressures <- read.csv("pressures.csv", stringsAsFactors = FALSE, check.names = FALSE)
components <- read.csv("components.csv", stringsAsFactors = FALSE, check.names = FALSE)
subregions <- read.csv("subregions.csv", stringsAsFactors = FALSE, check.names = FALSE)

# =========================================================
# SCORING DEFINITIONS - same values/descriptions as v2
# =========================================================

extent_levels <- data.frame(
  label = c("Exogenous", "Trace", "Site", "Local", "Widespread patchy", "Widespread even"),
  score = c(0, 1, 3, 37, 67, 100),
  description = c(
    "The activity occurs outside of the area occupied by the ecosystem component, but one or more pressures reach the ecosystem component through dispersal.",
    "The activity overlaps with the ecosystem component by up to 1% of the occupied area.",
    "The activity overlaps with the ecosystem component by between 1 and 5% of the occupied area.",
    "The activity overlaps with the ecosystem component by between 5 and 50% of the occupied area.",
    "The activity overlaps with the ecosystem component by between 50 and 100% of the occupied area, but distribution is patchy.",
    "The activity overlaps with the ecosystem component by between 50 and 100% of the occupied area and is evenly distributed."
  ), stringsAsFactors = FALSE
)

dispersal_levels <- data.frame(
  label = c("None", "Moderate", "High"),
  score = c(0, 5, 15),
  description = c(
    "The pressure does not disperse in the environment.",
    "The pressure disperses, but stays within the local environment.",
    "The pressure disperses widely and beyond the local environment."
  ), stringsAsFactors = FALSE
)

hazard_levels <- data.frame(
  label = c("Negligible", "Sublethal low", "Sublethal high", "Lethal low", "Lethal high"),
  score = c(0.001, 0.1, 1, 10, 100),
  description = c(
    "No hazard. Similar to undisturbed or pristine.",
    "Potentially unsustainable at low recovery potential.",
    "Potentially unsustainable at moderate recovery potential.",
    "Potentially unsustainable at high recovery potential.",
    "Likely to cause serious or irreversible harm."
  ), stringsAsFactors = FALSE
)

magnitude_levels <- data.frame(
  label = c("Very low (VL)", "Low (L)", "Medium (M)", "High (H)", "Maximum (VH)"),
  score = c(0.1, 1, 10, 50, 100),
  description = c(
    "Traces of the pressure with no adverse effects. Not different from pristine.",
    "Adverse effects expected but ≤ 1% of maximum.",
    "Adverse effects ≥ 1% but ≤ 10% of maximum.",
    "Adverse effects ≥ 10% but ≤ 50% of maximum.",
    "Causing highest adverse effects known for that pressure."
  ), stringsAsFactors = FALSE
)

behaviour_levels <- data.frame(
  label = c("Very low", "Low", "Moderate", "High"),
  score = c(0.005, 0.05, 0.5, 1),
  stringsAsFactors = FALSE
)

resilience_levels <- data.frame(
  label = c("None", "Low", "Moderate", "High"),
  score = c(100, 55, 6, 1),
  description = c(
    "The population/stock has no ability to recover and is expected to go locally extinct. Recovery predicted to take 100+ years.",
    "The population will take between 10 and 100 years to recover.",
    "The population will take between 2 and 10 years to recover.",
    "The population will take between 0 and 2 years to recover."
  ), stringsAsFactors = FALSE
)

categories <- c("Extent", "Dispersal", "Frequency", "Hazard", "Magnitude", "Behaviour", "Resilience")

# =========================================================
# DATABASE CONNECTION
# =========================================================

required_env <- c("DB_HOST", "DB_PORT", "DB_NAME", "DB_USER", "DB_PASSWORD")
missing_env <- required_env[!nzchar(Sys.getenv(required_env, unset = ""))]

if (length(missing_env) > 0) {
  stop(paste("Missing database environment variables:", paste(missing_env, collapse = ", ")))
}

db_con <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  port = as.integer(Sys.getenv("DB_PORT", "5432")),
  dbname = Sys.getenv("DB_NAME", "postgres"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  sslmode = Sys.getenv("DB_SSLMODE", "require")
)

onStop(function() {
  if (DBI::dbIsValid(db_con)) dbDisconnect(db_con)
})

# =========================================================
# DATABASE HELPERS
# =========================================================

read_scores <- function(con) {
  dbGetQuery(con, paste(
    "SELECT row_id, subregion, pressure, component,",
    "extent, extent_score, dispersal, dispersal_score,",
    "frequency, frequency_na, hazard, hazard_score,",
    "magnitude, magnitude_score, behaviour, behaviour_score,",
    "resilience, resilience_score, comments, updated_by, updated_at",
    "FROM assessment_scores",
    "ORDER BY subregion, pressure, component"
  ))
}

# Seed every assessment unit from the master CSV lists. ON CONFLICT makes
# repeated app starts harmless and supports multiple Shiny processes.
seed_scores <- function(con) {
  combinations <- expand.grid(
    subregion = subregions$subregion,
    pressure = pressures$pressure,
    component = components$component,
    stringsAsFactors = FALSE
  )

  # Write the complete seed set to a temporary PostgreSQL table in one
  # efficient batch operation, then insert the missing combinations into
  # the shared assessment table. This avoids thousands of network round trips
  # when the app starts.
  temp_name <- paste0("assessment_seed_", as.integer(Sys.getpid()))

  dbWriteTable(
    con,
    name = temp_name,
    value = combinations,
    temporary = TRUE,
    overwrite = TRUE,
    row.names = FALSE
  )

  dbExecute(con, paste0(
    "INSERT INTO assessment_scores (subregion, pressure, component) ",
    "SELECT subregion, pressure, component FROM \"", temp_name, "\" ",
    "ON CONFLICT (subregion, pressure, component) DO NOTHING"
  ))
}

seed_scores(db_con)

# =========================================================
# UI
# =========================================================

ui <- fluidPage(
  tags$head(tags$style(HTML("\n    .sticky-panel {\n      position: sticky;\n      top: 0;\n      background-color: white;\n      z-index: 1000;\n      padding-bottom: 10px;\n      border-bottom: 2px solid #dddddd;\n    }\n    .user-info {\n      margin-bottom: 10px;\n      color: #555555;\n    }\n  "))),

  titlePanel("EcoScore - Online Assessment"),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      div(class = "sticky-panel", uiOutput("main_ui"))
    ),
    mainPanel(
      DTOutput("table")
    )
  )
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session) {

  # Posit Connect supplies the signed-in user's email through session$user.
  # During local testing there may be no user identity, so use a local label.
  current_user <- reactive({
    if (is.null(session$user) || !nzchar(session$user)) "local_user" else session$user
  })

  scoring_started <- reactiveVal(FALSE)
  chosen_categories <- reactiveVal(character(0))
  rv <- reactiveVal(read_scores(db_con))
  advance_after_save <- reactiveVal(FALSE)
  last_saved_component <- reactiveVal(NULL)

  # -------------------------------------------------------
  # Main UI: category selection first, scoring interface second
  # -------------------------------------------------------

  output$main_ui <- renderUI({
    if (!scoring_started()) {
      return(tagList(
        div(class = "user-info", paste("User:", current_user())),
        h4("Choose categories to score"),
        checkboxGroupInput(
          "selected_categories",
          NULL,
          choices = categories,
          selected = character(0)
        ),
        actionButton("start_scoring", "OK - Start scoring")
      ))
    }

    selected <- chosen_categories()

    tagList(
      div(class = "user-info", paste("User:", current_user())),
      actionButton("change_categories", "← Change categories"),
      actionButton("refresh_data", "Refresh shared data"),
      br(), br(),

      selectInput(
        "selected_subregion",
        "Select subregion",
        choices = sort(unique(subregions$subregion))
      ),

      checkboxInput(
        "show_missing",
        "Show only incomplete combinations",
        value = TRUE
      ),

      selectInput("selected_pressure", "Select pressure", choices = NULL),
      selectInput("selected_component", "Select ecosystem component", choices = NULL),
      hr(),

      if ("Extent" %in% selected) tagList(
        h3("Extent"),
        radioButtons("extent", NULL, choices = extent_levels$label, selected = character(0)),
        verbatimTextOutput("extent_description"),
        checkboxInput("apply_extent_all", "Apply Extent to all subregions", value = FALSE),
        hr()
      ),

      if ("Dispersal" %in% selected) tagList(
        h3("Dispersal"),
        radioButtons("dispersal", NULL, choices = dispersal_levels$label, selected = character(0)),
        verbatimTextOutput("dispersal_description"),
        checkboxInput("apply_dispersal_all", "Apply Dispersal to all subregions", value = FALSE),
        hr()
      ),

      if ("Frequency" %in% selected) tagList(
        h3("Frequency"),
        checkboxInput("frequency_na", "Permanent pressure / NA", value = FALSE),
        numericInput("frequency", "Days per year", value = NULL, min = 0, max = 365),
        checkboxInput("apply_frequency_all", "Apply Frequency to all subregions", value = FALSE),
        hr()
      ),

      if ("Hazard" %in% selected) tagList(
        h3("Hazard"),
        radioButtons("hazard", NULL, choices = hazard_levels$label, selected = character(0)),
        verbatimTextOutput("hazard_description"),
        checkboxInput("apply_hazard_all", "Apply Hazard to all subregions", value = FALSE),
        hr()
      ),

      if ("Magnitude" %in% selected) tagList(
        h3("Magnitude"),
        radioButtons("magnitude", NULL, choices = magnitude_levels$label, selected = character(0)),
        verbatimTextOutput("magnitude_description"),
        checkboxInput("apply_magnitude_all", "Apply Magnitude to all subregions", value = FALSE),
        hr()
      ),

      if ("Behaviour" %in% selected) tagList(
        h3("Behaviour"),
        if (file.exists("www/behaviour_flowchart.png")) {
          tags$img(src = "behaviour_flowchart.png", width = "100%")
        },
        radioButtons("behaviour", NULL, choices = behaviour_levels$label, selected = character(0)),
        checkboxInput("apply_behaviour_all", "Apply Behaviour to all subregions", value = FALSE),
        hr()
      ),

      if ("Resilience" %in% selected) tagList(
        h3("Resilience"),
        radioButtons("resilience", NULL, choices = resilience_levels$label, selected = character(0)),
        verbatimTextOutput("resilience_description"),
        checkboxInput("apply_resilience_all", "Apply Resilience to all subregions", value = FALSE),
        hr()
      ),

      textAreaInput("comments", "Comments", rows = 4),
      actionButton("save_score", "Save score")
    )
  })

  observeEvent(input$start_scoring, {
    req(length(input$selected_categories) > 0)
    chosen_categories(input$selected_categories)
    scoring_started(TRUE)
  })

  observeEvent(input$change_categories, {
    scoring_started(FALSE)
    chosen_categories(character(0))
  })

  observeEvent(input$refresh_data, {
    req(scoring_started())
    rv(read_scores(db_con))
    showNotification("Shared data refreshed.", type = "message")
  })

  # -------------------------------------------------------
  # Filter table according to the currently selected categories
  # -------------------------------------------------------

  filtered_data <- reactive({
    req(scoring_started())
    req(input$selected_subregion)

    df <- rv() %>% filter(subregion == input$selected_subregion)
    selected <- chosen_categories()

    if (isTRUE(input$show_missing) && length(selected) > 0) {
      incomplete <- rep(FALSE, nrow(df))

      if ("Extent" %in% selected) incomplete <- incomplete | is.na(df$extent)
      if ("Dispersal" %in% selected) incomplete <- incomplete | is.na(df$dispersal)
      if ("Frequency" %in% selected) incomplete <- incomplete | (is.na(df$frequency) & !df$frequency_na)
      if ("Hazard" %in% selected) incomplete <- incomplete | is.na(df$hazard)
      if ("Magnitude" %in% selected) incomplete <- incomplete | is.na(df$magnitude)
      if ("Behaviour" %in% selected) incomplete <- incomplete | is.na(df$behaviour)
      if ("Resilience" %in% selected) incomplete <- incomplete | is.na(df$resilience)

      df <- df[incomplete, , drop = FALSE]
    }

    df
  })

  observe({
    req(scoring_started())
    df <- filtered_data()
    pressure_choices <- sort(unique(df$pressure))

    # Keep the currently selected pressure when the shared data are
    # refreshed (including after Save). Only clear it if it is no longer
    # available in the current filtered set.
    current_pressure <- input$selected_pressure
    selected_pressure <- if (!is.null(current_pressure) &&
                              length(current_pressure) > 0 &&
                              current_pressure %in% pressure_choices) {
      current_pressure
    } else {
      character(0)
    }

    updateSelectInput(
      session, "selected_pressure",
      choices = pressure_choices,
      selected = selected_pressure
    )
  })

  observe({
    req(scoring_started(), input$selected_pressure)
    df <- filtered_data() %>% filter(pressure == input$selected_pressure)
    component_choices <- sort(unique(df$component))

    if (length(component_choices) == 0) {
      updateSelectInput(
        session, "selected_component",
        choices = character(0), selected = character(0)
      )
      return()
    }

    # After saving, the current component normally disappears from the
    # incomplete list. In that case select the first remaining component
    # automatically, while keeping the selected pressure.
    current_component <- input$selected_component

    if (isTRUE(advance_after_save())) {
      # Prefer the first incomplete component after the one just saved.
      # If the saved component was the last one in the filtered sequence,
      # wrap around to the first remaining incomplete component.
      saved_component <- last_saved_component()
      if (!is.null(saved_component) &&
          length(saved_component) > 0 &&
          saved_component %in% component_choices) {
        after_saved <- component_choices[component_choices > saved_component]
        selected_component <- if (length(after_saved) > 0) {
          after_saved[1]
        } else {
          component_choices[1]
        }
      } else {
        selected_component <- component_choices[1]
      }
      advance_after_save(FALSE)
      last_saved_component(NULL)
    } else if (!is.null(current_component) &&
               length(current_component) > 0 &&
               current_component %in% component_choices) {
      selected_component <- current_component
    } else {
      selected_component <- character(0)
    }

    updateSelectInput(
      session, "selected_component",
      choices = component_choices,
      selected = selected_component
    )
  })

  # -------------------------------------------------------
  # Load current database values for exactly one assessment unit
  # -------------------------------------------------------

  observe({
    req(scoring_started(), input$selected_subregion, input$selected_pressure, input$selected_component)

    # Always read this exact row from the database rather than relying only
    # on the session cache. This helps concurrent users see recent changes.
    row <- dbGetQuery(
      db_con,
      paste(
        "SELECT * FROM assessment_scores",
        "WHERE subregion = $1 AND pressure = $2 AND component = $3"
      ),
      params = list(input$selected_subregion, input$selected_pressure, input$selected_component)
    )

    if (nrow(row) != 1) return()

    if ("Extent" %in% chosen_categories()) {
      updateRadioButtons(session, "extent", selected = if (is.na(row$extent)) character(0) else row$extent)
    }
    if ("Dispersal" %in% chosen_categories()) {
      updateRadioButtons(session, "dispersal", selected = if (is.na(row$dispersal)) character(0) else row$dispersal)
    }
    if ("Frequency" %in% chosen_categories()) {
      updateCheckboxInput(session, "frequency_na", value = isTRUE(row$frequency_na))
      updateNumericInput(session, "frequency", value = if (is.na(row$frequency)) NULL else row$frequency)
    }
    if ("Hazard" %in% chosen_categories()) {
      updateRadioButtons(session, "hazard", selected = if (is.na(row$hazard)) character(0) else row$hazard)
    }
    if ("Magnitude" %in% chosen_categories()) {
      updateRadioButtons(session, "magnitude", selected = if (is.na(row$magnitude)) character(0) else row$magnitude)
    }
    if ("Behaviour" %in% chosen_categories()) {
      updateRadioButtons(session, "behaviour", selected = if (is.na(row$behaviour)) character(0) else row$behaviour)
    }
    if ("Resilience" %in% chosen_categories()) {
      updateRadioButtons(session, "resilience", selected = if (is.na(row$resilience)) character(0) else row$resilience)
    }

    updateTextAreaInput(session, "comments", value = if (is.na(row$comments)) "" else row$comments)
  })

  # -------------------------------------------------------
  # Descriptions
  # -------------------------------------------------------

  output$extent_description <- renderText({
    req(input$extent)
    x <- extent_levels %>% filter(label == input$extent)
    paste0("Score: ", x$score, "\n\n", x$description)
  })

  output$dispersal_description <- renderText({
    req(input$dispersal)
    x <- dispersal_levels %>% filter(label == input$dispersal)
    paste0("Score: ", x$score, "\n\n", x$description)
  })

  output$hazard_description <- renderText({
    req(input$hazard)
    x <- hazard_levels %>% filter(label == input$hazard)
    paste0("Score: ", x$score, "\n\n", x$description)
  })

  output$magnitude_description <- renderText({
    req(input$magnitude)
    x <- magnitude_levels %>% filter(label == input$magnitude)
    paste0("Score: ", x$score, "\n\n", x$description)
  })

  output$resilience_description <- renderText({
    req(input$resilience)
    x <- resilience_levels %>% filter(label == input$resilience)
    paste0("Score: ", x$score, "\n\n", x$description)
  })

  # -------------------------------------------------------
  # SAVE
  # -------------------------------------------------------

  observeEvent(input$save_score, {
    req(scoring_started(), input$selected_subregion, input$selected_pressure, input$selected_component)

    selected <- chosen_categories()

    # Build a list of SQL column/value pairs for the exact selected row.
    # The column names below are fixed in the application; values are passed
    # through DBI parameters, so user-entered text is not interpolated into SQL.
    exact_updates <- list()

    add_exact <- function(column, value) {
      if (!is.null(value) && length(value) > 0) exact_updates[[column]] <<- value
    }

    if ("Extent" %in% selected && length(input$extent) > 0) {
      add_exact("extent", input$extent)
      add_exact("extent_score", extent_levels$score[extent_levels$label == input$extent])
    }

    if ("Dispersal" %in% selected && length(input$dispersal) > 0) {
      add_exact("dispersal", input$dispersal)
      add_exact("dispersal_score", dispersal_levels$score[dispersal_levels$label == input$dispersal])
    }

    if ("Frequency" %in% selected && (isTRUE(input$frequency_na) || !is.null(input$frequency))) {
      add_exact("frequency_na", isTRUE(input$frequency_na))
      add_exact("frequency", if (isTRUE(input$frequency_na)) NA_real_ else as.numeric(input$frequency))
    }

    if ("Hazard" %in% selected && length(input$hazard) > 0) {
      add_exact("hazard", input$hazard)
      add_exact("hazard_score", hazard_levels$score[hazard_levels$label == input$hazard])
    }

    if ("Magnitude" %in% selected && length(input$magnitude) > 0) {
      add_exact("magnitude", input$magnitude)
      add_exact("magnitude_score", magnitude_levels$score[magnitude_levels$label == input$magnitude])
    }

    if ("Behaviour" %in% selected && length(input$behaviour) > 0) {
      add_exact("behaviour", input$behaviour)
      add_exact("behaviour_score", behaviour_levels$score[behaviour_levels$label == input$behaviour])
    }

    if ("Resilience" %in% selected && length(input$resilience) > 0) {
      add_exact("resilience", input$resilience)
      add_exact("resilience_score", resilience_levels$score[resilience_levels$label == input$resilience])
    }

    # Comments retain the v2 behaviour: an empty comment does not erase a
    # previously saved comment.
    if (!is.null(input$comments) && nchar(input$comments) > 0) {
      add_exact("comments", input$comments)
    }

    # -----------------------------------------------------
    # Transaction: all changes made by this Save operation
    # succeed or fail together.
    # -----------------------------------------------------

    if (length(exact_updates) == 0) {
      showNotification("No score or comment was entered.", type = "warning")
      return()
    }

    dbWithTransaction(db_con, {
      for (category in selected) {

        value_column <- switch(
          category,
          Extent = "extent",
          Dispersal = "dispersal",
          Frequency = "frequency",
          Hazard = "hazard",
          Magnitude = "magnitude",
          Behaviour = "behaviour",
          Resilience = "resilience"
        )

        apply_all <- switch(
          category,
          Extent = isTRUE(input$apply_extent_all),
          Dispersal = isTRUE(input$apply_dispersal_all),
          Frequency = isTRUE(input$apply_frequency_all),
          Hazard = isTRUE(input$apply_hazard_all),
          Magnitude = isTRUE(input$apply_magnitude_all),
          Behaviour = isTRUE(input$apply_behaviour_all),
          Resilience = isTRUE(input$apply_resilience_all)
        )

        if (category == "Frequency") {
          if (!isTRUE(input$frequency_na) && is.null(input$frequency)) next
          frequency_value <- if (isTRUE(input$frequency_na)) NA_real_ else as.numeric(input$frequency)

          if (apply_all) {
            dbExecute(db_con,
              paste(
                "UPDATE assessment_scores SET frequency = $1, frequency_na = $2,",
                "updated_by = $3, updated_at = now()",
                "WHERE pressure = $4 AND component = $5"
              ),
              params = list(frequency_value, isTRUE(input$frequency_na), current_user(), input$selected_pressure, input$selected_component)
            )
          } else {
            dbExecute(db_con,
              paste(
                "UPDATE assessment_scores SET frequency = $1, frequency_na = $2,",
                "updated_by = $3, updated_at = now()",
                "WHERE subregion = $4 AND pressure = $5 AND component = $6"
              ),
              params = list(frequency_value, isTRUE(input$frequency_na), current_user(), input$selected_subregion, input$selected_pressure, input$selected_component)
            )
          }
          next
        }

        # The remaining categories all have a label and a numeric score.
        if (category == "Extent") {
          label <- input$extent
          score <- extent_levels$score[extent_levels$label == label]
          score_column <- "extent_score"
        } else if (category == "Dispersal") {
          label <- input$dispersal
          score <- dispersal_levels$score[dispersal_levels$label == label]
          score_column <- "dispersal_score"
        } else if (category == "Hazard") {
          label <- input$hazard
          score <- hazard_levels$score[hazard_levels$label == label]
          score_column <- "hazard_score"
        } else if (category == "Magnitude") {
          label <- input$magnitude
          score <- magnitude_levels$score[magnitude_levels$label == label]
          score_column <- "magnitude_score"
        } else if (category == "Behaviour") {
          label <- input$behaviour
          score <- behaviour_levels$score[behaviour_levels$label == label]
          score_column <- "behaviour_score"
        } else if (category == "Resilience") {
          label <- input$resilience
          score <- resilience_levels$score[resilience_levels$label == label]
          score_column <- "resilience_score"
        }

        if (is.null(label) || length(label) == 0) next

        if (apply_all) {
          dbExecute(db_con,
            paste0(
              "UPDATE assessment_scores SET ", value_column, " = $1, ",
              score_column, " = $2, updated_by = $3, updated_at = now() ",
              "WHERE pressure = $4 AND component = $5"
            ),
            params = list(label, score, current_user(), input$selected_pressure, input$selected_component)
          )
        } else {
          dbExecute(db_con,
            paste0(
              "UPDATE assessment_scores SET ", value_column, " = $1, ",
              score_column, " = $2, updated_by = $3, updated_at = now() ",
              "WHERE subregion = $4 AND pressure = $5 AND component = $6"
            ),
            params = list(label, score, current_user(), input$selected_subregion, input$selected_pressure, input$selected_component)
          )
        }
      }

      # Comments always belong to the exact selected assessment unit.
      if (!is.null(input$comments) && nchar(input$comments) > 0) {
        dbExecute(db_con,
          paste(
            "UPDATE assessment_scores SET comments = $1, updated_by = $2, updated_at = now()",
            "WHERE subregion = $3 AND pressure = $4 AND component = $5"
          ),
          params = list(input$comments, current_user(), input$selected_subregion, input$selected_pressure, input$selected_component)
        )
      }
    })

    # Refresh this session from the shared database. Mark the navigation
    # state so that the pressure is retained and the next incomplete
    # ecosystem component is selected automatically.
    last_saved_component(input$selected_component)
    advance_after_save(TRUE)
    rv(read_scores(db_con))

    showNotification(paste("Saved by", current_user()), type = "message")
  })

  # -------------------------------------------------------
  # TABLE
  # -------------------------------------------------------

  output$table <- renderDT({
    datatable(
      filtered_data(),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
}

shinyApp(ui, server)
