# server.R

library(shiny)
library(shinyBS)
library(bslib)
library(shinyjs)
library(shinyWidgets) # For pickerInput

# Define the server logic
function(input, output, session) {
  
  # ========= Navigation Logic =========
  observeEvent(input$FromLandingTo1A_Button, {
    updateTabsetPanel(session, "main_tabs", selected = "Step_1A_Screeners")
  })
  observeEvent(input$From1ATo1B_Button, {
    updateTabsetPanel(session, "main_tabs", selected = "rp1_panel")
  })
  observeEvent(input$to_rp2_button, {
    updateTabsetPanel(session, "main_tabs", selected = "sa_panel_5")
  })
  observeEvent(input$finish_v5_button, {
    updateTabsetPanel(session, "main_tabs", selected = "sa_panel_3")
  })
  observeEvent(input$finish_v3_button, {
    showModal(modalDialog(
      title = "Survey Complete",
      "Thank you for completing the survey. Your responses are valuable to our research.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # ======== Answer Enforcement Logic ========
  
  observe({
    required_inputs_s1 <- c("gender", "age_bracket", "household_children", "household_pets",
                            "drivers_licence", "next_mode_access_time", "pt_quality", 
                            "at_ability", "commute_time", "education", "Income")
    if (!is.null(input$drivers_licence) && input$drivers_licence == 'No') {
      required_inputs_s1 <- c(required_inputs_s1, "licence_intent")
    }
    all_filled <- all(sapply(required_inputs_s1, function(id) !is.null(input[[id]]) && input[[id]] != ""))
    if (all_filled) { shinyjs::enable("From1ATo1B_Button") } else { shinyjs::disable("From1ATo1B_Button") }
  })
  
  observe({
    req(input$num_cars) 
    required_inputs_s2 <- c("num_cars", "num_cars_intend")
    num_cars_val <- parse_num_cars(input$num_cars)
    if (num_cars_val > 0) {
      vehicle_inputs_to_check <- c("_body_type", "_ownership", "_fuel", "_age_year",
                                   "_parking", "_primary_use", "_freq_use", "_cost_rp")
      for (i in 1:num_cars_val) {
        required_inputs_s2 <- c(required_inputs_s2, paste0("car", i, vehicle_inputs_to_check))
      }
    } else if (input$num_cars == "0") {
      req(input$drivers_licence) 
      if (input$drivers_licence == 'Yes') {
        required_inputs_s2 <- c(required_inputs_s2, "car0_Commute_licence", "car0_cost_rp")
      } else {
        required_inputs_s2 <- c(required_inputs_s2, "car0_Commute_no_licence", "car0_no_licence_cost_rp")
      }
    }
    all_filled <- all(sapply(required_inputs_s2, function(id) !is.null(input[[id]]) && input[[id]] != ""))
    if (all_filled) { shinyjs::enable("to_rp2_button") } else { shinyjs::disable("to_rp2_button") }
  })
  
  shinyjs::disable("From1ATo1B_Button")
  shinyjs::disable("to_rp2_button")
  
  output$vehicle_details_header <- renderUI({
    req(input$num_cars)
    num_cars_val <- parse_num_cars(input$num_cars)
    if (num_cars_val > 0) {
      h4(strong(paste0("Step 2: Please provide details for your ", num_cars_val, " vehicle(s) below.")))
    }
  })
  
  
  # ============================================================================== #
  # == CHOICE EXPERIMENT FRAMEWORK: CORE PROFILES, PORTFOLIOS, AND PACKAGES
  # ============================================================================== #
  
  `%||%` <- function(a, b) if (is.null(a)) b else a
  TRIP_PURPOSES_V2 <- c("Work Trips", "Leisure Trips") 
  parse_num_cars <- function(num_cars_input) { if (is.null(num_cars_input) || num_cars_input == '' || num_cars_input == '0') return(0); as.integer(gsub("\\+", "", num_cars_input)) }
  parse_cost_bracket <- function(bracket_string) {
    if (is.null(bracket_string) || grepl("Don't know", bracket_string)) return(NA_real_); numbers <- as.numeric(unlist(regmatches(bracket_string, gregexpr("[0-9,]+", bracket_string)))); if (length(numbers) == 0) return(0); if (grepl("Under|£0 -", bracket_string)) return(mean(c(0, numbers[1]))); if (grepl("Over|More than", bracket_string)) return(numbers[1] * 1.2); if (length(numbers) == 1) return(numbers[1]); return(mean(numbers))
  }
  
  attribute_descriptions <- c(
    "Fixed monthly cost" = "The approximate total monthly cost for the vehicle or service.",
    "Car club: Walk time" = "Time to walk to the nearest available shared car.",
    "Car club: Booking" = "How far in advance a shared car must be booked.",
    "Frequency of public transport" = "The typical wait time between buses or trains on main routes.",
    "Extras" = "Additional benefits included with the package."
  )
  
  create_user_profile <- function(input) {
    total_work_trips <- 0; total_leisure_trips <- 0
    num_cars_parsed <- parse_num_cars(input$num_cars)
    if (num_cars_parsed > 0) {
      for (i in 1:num_cars_parsed) {
        total_work_trips <- total_work_trips + (input[[paste0("car", i, "_work_trips_rp")]] %||% 0)
        total_leisure_trips <- total_leisure_trips + (input[[paste0("car", i, "_leisure_trips_rp")]] %||% 0)
      }
    } else {
      if (input$drivers_licence == 'Yes') {
        total_work_trips <- input$car0_work_trips_rp %||% 0; total_leisure_trips <- input$car0_leisure_trips_rp %||% 0
      } else {
        total_work_trips <- input$car0_no_licence_work_trips_rp %||% 0; total_leisure_trips <- input$car0_no_licence_leisure_trips_rp %||% 0
      }
    }
    list(
      has_licence = input$drivers_licence == 'Yes', licence_intent = !is.null(input$licence_intent) && input$licence_intent == 'Yes',
      num_cars = num_cars_parsed, is_zero_car = input$num_cars == '0',
      has_children = !is.null(input$household_children) && input$household_children != "0",
      has_mobility = input$at_ability != 'No, neither', 
      has_pt_access = input$next_mode_access_time %in% c("Less than 2 minutes", "2-5 minutes", "6-10 minutes", "11-15 minutes"),
      pt_quality_perception = input$pt_quality %||% "Acceptable",
      commute_time_mins = switch(input$commute_time, "Under 15 minutes"=10, "15-30 minutes"=22, "31-45 minutes"=38, "46-60 minutes"=53, "More than 60 minutes"=75, 0),
      total_work_trips = total_work_trips, 
      total_leisure_trips = total_leisure_trips
    )
  }
  
  create_current_portfolio <- function(profile, input) {
    commute_mode <- if (profile$has_licence) input$car0_Commute_licence else input$car0_Commute_no_licence
    cost_input <- if (profile$has_licence) input$car0_cost_rp else input$car0_no_licence_cost_rp
    total_cost <- parse_cost_bracket(cost_input)
    list(
      id = "current", title = "Your Current Main Travel",
      description = paste("You primarily travel by:", commute_mode),
      attributes = list(
        "Fixed monthly cost" = if(is.na(total_cost)) "Not provided" else paste0("£", round(total_cost)),
        "Car club: Walk time" = "No membership", "Car club: Booking" = "No membership", 
        "Frequency of public transport" = switch(profile$pt_quality_perception, "Very good"="Every 10 mins", "Good"="Every 15-20 mins", "Acceptable"="Every 15-20 mins", "Poor"="Every 30+ mins", "Very poor"="Every 30+ mins", "I do not know"="Varies"),
        "Extras" = "None"
      )
    )
  }
  
  generate_policy_packages <- function() {
    list(
      package1 = list(
        id = "package1", title = "Shared Mobility Package A",
        description = "You would use a combination of public transport and car-sharing when needed. No vehicle ownership costs (insurance, depreciation, repairs).",
        attributes = list("Fixed monthly cost" = "£60", "Car club: Walk time" = "10 mins", "Car club: Booking" = "1 day ahead", "Frequency of public transport" = "Every 20 mins", "Extras" = "Access to discounted Beryl bike hire")
      ),
      package2 = list(
        id = "package2", title = "Shared Mobility Package B",
        description = "You would use a combination of public transport and car-sharing when needed. No vehicle ownership costs (insurance, depreciation, repairs).",
        attributes = list("Fixed monthly cost" = "£80", "Car club: Walk time" = "5 mins", "Car club: Booking" = "Same day", "Frequency of public transport" = "Every 10 mins", "Extras" = "Taxi credit (£20/mo)")
      ),
      package3 = list(
        id = "package3", title = "Shared Mobility Package C",
        description = "You would use a combination of public transport and car-sharing when needed. No vehicle ownership costs (insurance, depreciation, repairs).",
        attributes = list("Fixed monthly cost" = "£40", "Car club: Walk time" = "15 mins", "Car club: Booking" = "2 days ahead", "Frequency of public transport" = "Every 30 mins", "Extras" = "None")
      )
    )
  }
  
  get_ordinal <- function(n) {
    if (n %in% 11:13) return(paste0(n, "th"))
    last_digit <- n %% 10
    suffix <- switch(as.character(last_digit), "1"="st", "2"="nd", "3"="rd", "th")
    paste0(n, suffix)
  }
  
  create_single_vehicle_portfolio <- function(vehicle_index, input) {
    cost <- parse_cost_bracket(input[[paste0("car", vehicle_index, "_cost_rp")]])
    list(
      id = paste0("owned_", vehicle_index), title = paste0("Your ", get_ordinal(vehicle_index), " Vehicle"),
      description = paste("A", input[[paste0("car", vehicle_index, "_age_year")]], input[[paste0("car", vehicle_index, "_fuel")]], input[[paste0("car", vehicle_index, "_body_type")]],
                          "that is normally parked", tolower(input[[paste0("car", vehicle_index, "_parking")]])),
      attributes = list(
        "Fixed monthly cost" = if(is.na(cost)) "Not provided" else paste0("£", round(cost)),
        "Car club: Walk time" = "No membership", "Car club: Booking" = "No membership", 
        "Frequency of public transport" = "Car use only.",
        "Extras" = "N/A"
      )
    )
  }
  
  
  # ============================================================================== #
  # == STEP 3 (sa_panel_5): YOUR FUTURE TRAVEL
  # ============================================================================== #
  
  current_task_index_v5 <- reactiveVal(1)
  observeEvent(input$next_task_button_v5, { current_task_index_v5(current_task_index_v5() + 1) })
  observeEvent(input$prev_task_button_v5, { current_task_index_v5(current_task_index_v5() - 1) })
  observe({ input$num_cars; current_task_index_v5(1) })
  
  output$sa_ui_placeholder5 <- renderUI({
    req(input$num_cars != "", input$pt_quality != "")
    profile <- create_user_profile(input)
    policy_packages <- generate_policy_packages()
    
    num_tasks <- if (profile$is_zero_car) 1 else profile$num_cars
    task_idx <- current_task_index_v5()
    
    if (task_idx > num_tasks) {
      return(tagList(
        h3("Section Complete"),
        p("You have finished this section. Please click 'Continue' below.", style="text-align:center; margin:30px;"),
        div(style="text-align:center;", actionButton("finish_v5_button", "Continue", class="btn-primary btn-lg"))
      ))
    }
    
    # Text now passed directly to table function
    decision_text <- if (profile$is_zero_car) {
      h4("Decision for your current travel", style="margin:0;")
    } else {
      h4(paste0("Decision for your ", get_ordinal(task_idx), " vehicle"), style="margin:0;")
    }
    
    current_side <- if(profile$is_zero_car) create_current_portfolio(profile, input) else create_single_vehicle_portfolio(task_idx, input)
    pkg_idx <- ((task_idx - 1) %% length(policy_packages)) + 1
    package_side <- policy_packages[[pkg_idx]]
    
    
    css_styles_v5 <- HTML("
      .survey-content-wrapper-v5 { max-width: 1000px; margin: 0 auto; }
      .reconsideration-banner { padding: 15px; background-color: #f8f9fa; border-left: 5px solid #6c757d; margin-bottom: 20px; border-radius: 4px; }
      .reconsideration-banner h4 { margin: 0 0 5px 0; }
      .comparison-table-v5 th, .comparison-table-v5 td { padding: 12px; vertical-align: middle; }
      .comparison-table-v5 thead th { font-size: 1.1em; text-align: center; color: white; border: none !important; }
      .current-col-header { background-color: #28a745 !important; }
      .package-col-header { background-color: #007bff !important; }
      .attr-label { font-weight: bold; }
      .allocation-section { margin-top: 20px; padding: 25px; background-color: #f8f9fa; border-radius: 8px;}
      .trip-allocator-row { padding-top: 15px; border-top: 1px solid #e9ecef; margin-top: 15px;}
      .trip-allocator-row:first-child { border-top: none; margin-top: 0; }
      .allocator-header {text-align: center;}
      .remaining-trips {font-weight: bold; text-align: center; margin-top: 10px; padding: 8px; border-radius: 4px;}
    ")
    
    # REVISED: Table helper now includes decision_text and equal column widths
    create_comparison_table_v5 <- function(current, package, decision_text) {
      attr_names <- names(package$attributes)
      table_rows <- lapply(attr_names, function(attr_name) {
        tags$tr(
          tags$td(
            class = "attr-label", attr_name,
            tags$br(),
            tags$small(style="font-weight:normal; color: #555;", attribute_descriptions[[attr_name]])
          ),
          tags$td(current$attributes[[attr_name]] %||% "N/A"),
          tags$td(package$attributes[[attr_name]] %||% "N/A")
        )
      })
      tags$table(class = "table table-bordered comparison-table-v5",
                 tags$thead(tags$tr(
                   tags$th("Feature", style="width:30%"),
                   tags$th(icon("home"), current$title, class="current-col-header", style="width:35%"),
                   tags$th(icon("star"), package$title, class="package-col-header", style="width:35%")
                 )),
                 tags$tbody(
                   tags$tr(
                     tags$td(colspan=3, style="background-color:#f8f9fa; text-align:center;", decision_text)
                   ),
                   tags$tr(
                     tags$td(class="attr-label", "Description:"),
                     tags$td(em(current$description)),
                     tags$td(em(package$description))
                   ),
                   table_rows
                 )
      )
    }
    
    create_trip_allocator_ui <- function(task_idx, purpose, total_trips, mode_A_title, mode_B_title) {
      clean_purpose <- gsub(" ", "_", purpose)
      id_A <- paste0("trips_v5_task", task_idx, "_", clean_purpose, "_A")
      id_B <- paste0("trips_v5_task", task_idx, "_", clean_purpose, "_B")
      id_rem <- paste0("rem_trips_v5_task", task_idx, "_", clean_purpose)
      output[[id_rem]] <- renderUI({
        trips_A <- input[[id_A]] %||% 0; trips_B <- input[[id_B]] %||% 0
        remaining <- total_trips - trips_A - trips_B
        color <- if (remaining == 0) "#d4edda" else if (remaining < 0) "#f8d7da" else "#fff3cd"
        borderColor <- if (remaining == 0) "#c3e6cb" else if (remaining < 0) "#f5c6cb" else "#ffeeba"
        div(class = "remaining-trips", style = paste0("background-color:", color, "; border: 1px solid ", borderColor), paste(remaining, "trips remaining to be allocated."))
      })
      div(class="trip-allocator-row",
          h5(class="allocator-header", paste("Allocate your", total_trips, "Monthly", purpose)),
          fluidRow(
            column(6, strong(mode_A_title), numericInput(id_A, label=NULL, value=0, min=0, max=total_trips, step=1, width="100%")),
            column(6, strong(mode_B_title), numericInput(id_B, label=NULL, value=0, min=0, max=total_trips, step=1, width="100%"))
          ), uiOutput(id_rem)
      )
    }
    
    navigation_ui <- div(
      style = "margin-top: 30px; display: flex; justify-content: space-between; align-items: center;",
      if (task_idx > 1) { actionButton("prev_task_button_v5", "Back", class = "btn-default btn-lg") } else { div() },
      tags$strong(paste0("Task ", task_idx, " of ", num_tasks)),
      if (task_idx < num_tasks) { 
        actionButton("next_task_button_v5", "Continue to Next Vehicle", class = "btn-primary btn-lg") 
      } else { 
        actionButton("finish_v5_button", "Continue", class = "btn-primary btn-lg") 
      }
    )
    
    tagList(
      tags$head(tags$style(css_styles_v5)),
      div(class = "survey-content-wrapper-v5",
          div(class="reconsideration-banner",
              h4("Please consider how you would travel"),
              p(class="lead", "Let's imagine you are reviewing your household's transport. The options below present different paths you could take. Please show how you would share your monthly trips if the new package were available.", style="margin:0;")
          ),
          create_comparison_table_v5(current_side, package_side, decision_text),
          div(class="allocation-section",
              h4("How many trips per month would you make by each of these alternatives for the following purposes?", style="text-align:center; margin-top:0; margin-bottom: 20px;"),
              create_trip_allocator_ui(task_idx, "Work Trips", profile$total_work_trips, current_side$title, package_side$title),
              create_trip_allocator_ui(task_idx, "Leisure Trips", profile$total_leisure_trips, current_side$title, package_side$title)
          ),
          navigation_ui
      )
    )
  })
  
  
  # ============================================================================== #
  # == STEP 4 (sa_panel_3): YOUR TRAVEL PREFERENCE
  # ============================================================================== #
  
  current_task_index_v3 <- reactiveVal(1)
  observeEvent(input$next_task_button_v3, { current_task_index_v3(current_task_index_v3() + 1) })
  observeEvent(input$prev_task_button_v3, { current_task_index_v3(current_task_index_v3() - 1) })
  observe({ input$num_cars; current_task_index_v3(1) })
  
  output$choice_summary_v3 <- renderText({
    task_idx <- current_task_index_v3()
    choice_input_id <- paste0("package_choice_v3_task", task_idx)
    req(input[[choice_input_id]])
    profile <- create_user_profile(input)
    switch(input[[choice_input_id]],
           "current" = { if (profile$is_zero_car) "SUMMARY: You have chosen to continue with your current main travel mode." else paste0("SUMMARY: You have chosen to keep your ", get_ordinal(task_idx), " vehicle.") },
           "package" = {
             policy_packages <- generate_policy_packages()
             pkg_idx <- ((task_idx - 1) %% length(policy_packages)) + 1
             current_package <- policy_packages[[pkg_idx]]
             paste("SUMMARY: You have chosen to swap for the", current_package$title, ".")
           }
    )
  })
  
  output$sa_ui_placeholder3 <- renderUI({
    req(input$num_cars != "", input$pt_quality != "")
    profile <- create_user_profile(input)
    policy_packages <- generate_policy_packages()
    
    num_tasks <- if (profile$is_zero_car) length(policy_packages) else profile$num_cars
    task_idx <- current_task_index_v3()
    
    if (task_idx > num_tasks) {
      return(tagList(
        p("You have completed this section. Please click 'Finish Survey' below.", style="text-align:center; margin:30px;"),
        div(style="text-align:center;", actionButton("finish_v3_button", "Finish Survey", class="btn-success btn-lg"))
      ))
    }
    
    if (profile$is_zero_car) {
      current_side <- create_current_portfolio(profile, input)
      package_side <- policy_packages[[task_idx]]
    } else {
      current_side <- create_single_vehicle_portfolio(task_idx, input)
      pkg_idx <- ((task_idx - 1) %% length(policy_packages)) + 1
      package_side <- policy_packages[[pkg_idx]]
    }
    
    create_choice_card_v3 <- function(title, description, cost_text, type) {
      tags$div(class = paste("choice-card-v3", type),
               icon(if(type == "current") "home" else "star", class = "fa-2x choice-icon"),
               tags$div(class = "choice-content", h5(title), p(description, class = "small"), strong(cost_text))
      )
    }
    
    css_styles_v3 <- HTML("
      .package-choice-wrapper { max-width: 950px; margin: auto; }
      .reconsideration-banner { padding: 15px; background-color: #f8f9fa; border-left: 5px solid #6c757d; margin-bottom: 20px; border-radius: 4px; }
      .reconsideration-banner h4 { margin: 0 0 5px 0; }
      .summary-box-v4 { margin-top: 20px; margin-bottom: 30px; padding: 15px; background-color: #e3f2fd; border-left: 5px solid #007bff; border-radius: 0; font-weight: bold; text-align: center; }
      .package-table { border: 1px solid #dee2e6; margin-bottom: 10px; }
      .package-table th, .package-table td { padding: 12px; vertical-align: middle; }
      .package-table thead th { font-size: 1.1em; text-align: center; color: white; border: none !important; }
      .package-table tbody tr:nth-child(even) { background-color: #f8f9fa; }
      .current-col-header { background-color: #28a745 !important; }
      .package-col-header { background-color: #007bff !important; }
      .attr-label { font-weight: bold; }
      .choice-grid-wrapper-v3 .shiny-options-group { display: grid !important; grid-template-columns: 1fr 1fr !important; gap: 20px !important; }
      .choice-grid-wrapper-v3 .radio, .choice-grid-wrapper-v3 label { margin: 0 !important; width: 100% !important; display: block !important; }
      .choice-grid-wrapper-v3 input[type=radio] { display: none; }
      .choice-card-v3 { display: flex; flex-direction: column; text-align: center; height: 100%; min-height: 220px; padding: 20px; border: 3px solid #dee2e6; border-radius: 8px; cursor: pointer; transition: all 0.2s ease-in-out; background-color: white; }
      .choice-card-v3:hover { border-color: #007bff; }
      .choice-icon { margin-bottom: 10px; color: #495047; font-size: 2.2em; }
      .choice-content h5 { font-size: 1.1em; margin-bottom: 5px; font-weight: bold; }
      .choice-content p { color: #6c757d; min-height: 55px; }
      .choice-content strong { font-size: 1.1em; }
      .choice-grid-wrapper-v3 label input[type=radio]:checked + .choice-card-v3, .choice-grid-wrapper-v3 label input[type=radio]:checked ~ .choice-card-v3, .choice-grid-wrapper-v3 label input[type=radio]:checked + span .choice-card-v3, .choice-grid-wrapper-v3 label input[type=radio]:checked ~ span .choice-card-v3 { border-color: #007bff !important; box-shadow: 0 4px 12px rgba(0,123,255,.3); transform: translateY(-2px); }
    ")
    
    # REVISED: Table helper now includes descriptions and equal column widths
    create_package_table <- function(current, package) {
      attr_names <- names(package$attributes)
      table_rows <- lapply(attr_names, function(attr_name) {
        tags$tr(
          tags$td(
            class = "attr-label", attr_name,
            tags$br(),
            tags$small(style="font-weight:normal; color: #555;", attribute_descriptions[[attr_name]])
          ),
          tags$td(current$attributes[[attr_name]] %||% "N/A"),
          tags$td(package$attributes[[attr_name]] %||% "N/A")
        )
      })
      tags$table(class = "table table-bordered package-table",
                 tags$thead(tags$tr(
                   tags$th("Feature", style="width:30%"),
                   tags$th(icon("home"), current$title, class="current-col-header", style="width:35%"),
                   tags$th(icon("star"), package$title, class="package-col-header", style="width:35%")
                 )),
                 tags$tbody(
                   tags$tr(
                     tags$td(class="attr-label", "Description:"),
                     tags$td(em(current$description)),
                     tags$td(em(package$description))
                   ),
                   table_rows
                 )
      )
    }
    
    navigation_ui <- div(style = "margin-top: 30px; text-align: center;",
                         tags$strong(paste0("Decision ", task_idx, " of ", num_tasks)),
                         hr(),
                         div(style = "display: flex; justify-content: space-between; align-items: center;",
                             if (task_idx > 1) { actionButton("prev_task_button_v3", "Back", class="btn-default btn-lg") } else { div() },
                             if (task_idx < num_tasks) { 
                               actionButton("next_task_button_v3", "Decision for Next Vehicle", class = "btn-primary btn-lg") 
                             } else { 
                               actionButton("finish_v3_button", "Finish Survey", class = "btn-success btn-lg") 
                             }
                         )
    )
    
    tagList(
      tags$head(tags$style(css_styles_v3)),
      div(class = "package-choice-wrapper",
          # REVISED: Header section is replaced with this banner
          div(class="reconsideration-banner",
              h4("Imagine that you had to pick one of these two alternatives."),
              p(class="lead", "Which option would you prefer for your household?", style="margin:0;")
          ),
          create_package_table(current_side, package_side),
          div(class="summary-box-v4", textOutput("choice_summary_v3", inline=TRUE)),
          div(class="choice-grid-wrapper-v3",
              radioButtons(
                inputId = paste0("package_choice_v3_task", task_idx),
                label = h4("Which would you choose?", style="text-align:center;"),
                choiceNames = list(
                  create_choice_card_v3(current_side$title, current_side$description, current_side$attributes[["Fixed monthly cost"]], "current"),
                  create_choice_card_v3(package_side$title, package_side$description, package_side$attributes[["Fixed monthly cost"]], "package")
                ),
                choiceValues = list("current", "package"),
                selected = input[[paste0("package_choice_v3_task", task_idx)]] %||% character(0),
                width = "100%"
              )
          ),
          navigation_ui
      )
    )
  })
  
}