# server.R (Simplified Version)

library(shiny)
library(shinyBS)
library(bslib)
library(shinyjs)

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
    updateTabsetPanel(session, "main_tabs", selected = "sa_panel_3")
  })
  observeEvent(input$to_sa_button4, {
    updateTabsetPanel(session, "main_tabs", selected = "sa_panel_5")
  })
  
  
  # ========= Version 1: Preamble & Core Logic =========
  
  # ============================================================================== #
  # == CHOICE EXPERIMENT FRAMEWORK: CORE PROFILES, PORTFOLIOS, AND PACKAGES
  # ============================================================================== #
  
  # --- Constants and Helpers ---
  `%||%` <- function(a, b) if (is.null(a)) b else a
  TRIP_PURPOSES_V2 <- c("Work Trips", "Leisure Trips") # Still needed for sa_panel_5
  
  parse_num_cars <- function(num_cars_input) { if (is.null(num_cars_input) || num_cars_input == '0') return(0); as.integer(gsub("\\+", "", num_cars_input)) }
  parse_cost_bracket <- function(bracket_string) {
    if (is.null(bracket_string) || grepl("Don't know", bracket_string)) return(NA_real_); numbers <- as.numeric(unlist(regmatches(bracket_string, gregexpr("[0-9,]+", bracket_string)))); if (length(numbers) == 0) return(0); if (grepl("Under|£0 -", bracket_string)) return(mean(c(0, numbers[1]))); if (grepl("Over|More than", bracket_string)) return(numbers[1] * 1.2); if (length(numbers) == 1) return(numbers[1]); return(mean(numbers))
  }
  
  # --- 1. User Profile Generation ---
  create_user_profile <- function(input) {
    total_work_trips <- 0; total_leisure_trips <- 0
    if (parse_num_cars(input$num_cars) > 0) {
      for (i in 1:parse_num_cars(input$num_cars)) {
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
      num_cars = parse_num_cars(input$num_cars), is_zero_car = input$num_cars == '0',
      has_children = !is.null(input$household_children) && input$household_children != "0",
      has_mobility = input$at_ability != 'No, neither', 
      has_pt_access = input$pt_access == 'Yes',
      pt_quality_perception = input$pt_quality %||% "Acceptable",
      commute_distance_miles = switch(input$commute_distance, "Under 1 mile"=0.5, "1-3 miles"=2, "3-5 miles"=4, "5-10 miles"=7.5, "More than 10 miles"=15, 0),
      total_work_trips = total_work_trips, total_leisure_trips = total_leisure_trips
    )
  }
  
  # --- 2. Portfolio/Package Definition Functions ---
  
  # THIS FUNCTION IS NOW ONLY USED FOR THE ZERO-CAR CASE
  create_current_portfolio <- function(profile, input) {
    if (!profile$is_zero_car) return(NULL) # Guard clause
    
    commute_mode <- if (profile$has_licence) input$car0_Commute_licence else input$car0_Commute_no_licence
    cost_input <- if (profile$has_licence) input$car0_cost_rp else input$car0_no_licence_cost_rp
    total_cost <- parse_cost_bracket(cost_input)
    
    list(
      id = "current",
      title = "Your Current Main Travel",
      description = paste("You primarily travel by:", commute_mode),
      attributes = list(
        "Fixed monthly cost" = if(is.na(total_cost)) "Not provided" else paste0("£", round(total_cost)),
        "Car club: Walk time" = "N/A",
        "Car club: Booking" = "N/A",
        "PT frequency" = switch(profile$pt_quality_perception, 
                                "Very good"="Every 10 mins", 
                                "Good"="Every 15-20 mins", 
                                "Acceptable"="Every 15-20 mins", 
                                "Poor"="Every 30+ mins",
                                "Very poor"="Every 30+ mins"),
        "Trip coverage" = "Current travel patterns",
        "Extras" = "None"
      )
    )
  }
  
  generate_policy_packages <- function() {
    list(
      package1 = list(
        id = "package1",
        title = "Shared Mobility Package A",
        description = "Access to shared cars when needed, plus unlimited public transport. No vehicle ownership costs (insurance, depreciation, repairs).",
        attributes = list(
          "Fixed monthly cost" = "£60", # These would be varied by an experimental design
          "Car club: Walk time" = "10 mins",
          "Car club: Booking" = "1 day ahead",
          "Frequency of public transport" = "Every 20 mins",
          "Extras" = "Access to discounted Beryl bike hire"
        )
      ),
      package2 = list(
        id = "package2",
        title = "Shared Mobility Package B",
        description = "Access to shared cars when needed, plus unlimited public transport. No vehicle ownership costs (insurance, depreciation, repairs).",
        attributes = list(
          "Fixed monthly cost" = "£80",
          "Car club: Walk time" = "5 mins",
          "Car club: Booking" = "Same day",
          "Frequency of public transport" = "Every 10 mins",
          "Extras" = "Taxi credit (£20/mo)"
        )
      ),
      package3 = list(
        id = "package3",
        title = "Shared Mobility Package C",
        description = "Access to shared cars when needed, plus unlimited public transport. No vehicle ownership costs (insurance, depreciation, repairs).",
        attributes = list(
          "Fixed monthly cost" = "£40",
          "Car club: Walk time" = "15 mins",
          "Car club: Booking" = "2 days ahead",
          "Frequency of public transport" = "Every 30 mins",
          "Extras" = "None"
        )
      )
    )
  }
  
  # --- 3. Legacy Functions (for sa_panel_4 & sa_panel_5 compatibility) ---
  
  create_status_quo_mode <- function(profile, input) {
    if (profile$is_zero_car) {
      cost_input <- if (profile$has_licence) input$car0_cost_rp else input$car0_no_licence_cost_rp
      cost_val <- parse_cost_bracket(cost_input)
      list(id = "status_quo", title = "Your Current Main Travel", icon = "walking", type = "current", 
           description = paste("You primarily travel by:", if (profile$has_licence) input$car0_Commute_licence else input$car0_Commute_no_licence), 
           access = "Your current level of access", availability = "Your current travel reliability", 
           cost_val = cost_val, cost_text = ifelse(is.na(cost_val), "Not Provided", paste0("£", round(cost_val))))
    } else { NULL }
  }
  create_owned_vehicle_mode <- function(vehicle_index, input) {
    cost_input_id <- paste0("car", vehicle_index, "_cost_rp"); cost_val <- parse_cost_bracket(input[[cost_input_id]])
    list(
      id = paste0("owned_", vehicle_index), title = "Your Currently Owned Vehicle", icon = "car-side", type = "current", 
      description = paste("A", input[[paste0("car", vehicle_index, "_age_year")]], input[[paste0("car", vehicle_index, "_fuel")]], input[[paste0("car", vehicle_index, "_body_type")]], "that is normally parked", tolower(input[[paste0("car", vehicle_index, "_parking")]])),
      access = "Immediate, 24/7 at your home", availability = "Very reliable (available for >99 in 100 trips)", 
      vehicle_index = vehicle_index, cost_val = cost_val, cost_text = ifelse(is.na(cost_val), "Not Provided", paste0("£", round(cost_val)))
    )
  }
  create_alternative_modes <- function(profile) {
    modes <- list()
    if (profile$has_licence || profile$licence_intent) {
      modes$car_club_standard <- list(id = "car_club_standard", title = "Membership to a Neighbourhood Car Club", icon = "car-side", type = "alternative", description = "Provides access to a range of small-medium sized cars parked in your local area.", access = "5-10 minute walk to a vehicle", availability = "Reliable (available for ~95 in 100 trips)", cost_val = 60, cost_text = "£60")
    }
    if (profile$has_pt_access) {
      modes$public_transport <- list(id = "public_transport", title = "Yorkshire Pass for Public Transport", icon = "bus-alt", type = "alternative", description = "A monthly pass providing unlimited travel on all local buses, trams, and trains.", access = "Within a 5 minute walk", availability = "Reliable (available for ~95 in 100 trips)", cost_val = 75, cost_text = "£75")
    }
    return(modes)
  }
  get_modes_for_choice <- function(profile, input) {
    current_modes <- list()
    if (profile$is_zero_car) { 
      sq <- create_status_quo_mode(profile, input); if (!is.null(sq)) current_modes$status_quo <- sq
    } else { 
      for (i in seq_len(min(profile$num_cars, 4))) { 
        current_modes[[paste0("owned_", i)]] <- create_owned_vehicle_mode(i, input) 
      }
    }
    return(list(current = current_modes, alternatives = create_alternative_modes(profile)))
  }
  
  
  # ============================================================================== #
  # == VERSION 1 (sa_panel_3): PACKAGE CHOICE EXPERIMENT
  # ============================================================================== #
  
  # --- V3: Helper function for ordinals (copied from V4) ---
  get_ordinal <- function(n) {
    if (n %in% 11:13) return(paste0(n, "th"))
    last_digit <- n %% 10
    suffix <- switch(as.character(last_digit), "1"="st", "2"="nd", "3"="rd", "th")
    paste0(n, suffix)
  }
  
  # --- V3: NEW Helper function to create a portfolio for a single car ---
  create_single_vehicle_portfolio <- function(vehicle_index, input) {
    cost <- parse_cost_bracket(input[[paste0("car", vehicle_index, "_cost_rp")]])
    list(
      id = paste0("owned_", vehicle_index),
      title = paste0("Your ", get_ordinal(vehicle_index), " Vehicle"),
      description = paste("A", input[[paste0("car", vehicle_index, "_age_year")]], input[[paste0("car", vehicle_index, "_fuel")]], input[[paste0("car", vehicle_index, "_body_type")]],
                          "that is normally parked", tolower(input[[paste0("car", vehicle_index, "_parking")]])),
      attributes = list(
        "Fixed monthly cost" = if(is.na(cost)) "Not provided" else paste0("£", round(cost)),
        "Car club: Walk time" = "N/A",
        "Car club: Booking" = "N/A",
        "Frequency of public transport" = "N/A",
        "Extras" = "N/A"
      )
    )
  }
  
  # --- V3: Wizard State & Navigation ---
  current_task_index_v3 <- reactiveVal(1)
  observeEvent(input$next_task_button_v3, { current_task_index_v3(current_task_index_v3() + 1) })
  observeEvent(input$prev_task_button_v3, { current_task_index_v3(current_task_index_v3() - 1) })
  observe({ input$num_cars; current_task_index_v3(1) })
  
  # --- V3: Summary Box Logic ---
  output$choice_summary_v3 <- renderText({
    task_idx <- current_task_index_v3()
    choice_input_id <- paste0("package_choice_v3_task", task_idx)
    req(input[[choice_input_id]])
    
    profile <- create_user_profile(input)
    
    switch(input[[choice_input_id]],
           "current" = {
             if (profile$is_zero_car) {
               "SUMMARY: You have chosen to continue with your current main travel mode."
             } else {
               paste0("SUMMARY: You have chosen to keep your ", get_ordinal(task_idx), " vehicle.")
             }
           },
           "package" = {
             policy_packages <- generate_policy_packages()
             # Use modulo to cycle through packages if there are more cars than packages
             pkg_idx <- ((task_idx - 1) %% length(policy_packages)) + 1
             current_package <- policy_packages[[pkg_idx]]
             paste("SUMMARY: You have chosen to swap for the", current_package$title, ".")
           }
    )
  })
  
  # --- V3: UI Generation ---
  output$sa_ui_placeholder3 <- renderUI({
    req(input$num_cars, input$pt_quality)
    profile <- create_user_profile(input)
    policy_packages <- generate_policy_packages()
    
    # Determine the number of tasks: num_cars for owners, num_packages for non-owners
    num_tasks <- if (profile$is_zero_car) length(policy_packages) else profile$num_cars
    
    task_idx <- current_task_index_v3()
    
    if (task_idx > num_tasks) {
      return(tagList(
        p("You have completed this section. Please click 'Continue' below.", style="text-align:center; margin:30px;"),
        div(style="text-align:center;", actionButton("finish_v3_button", "Continue", class="btn-primary btn-lg"))
      ))
    }
    
    # --- Determine the two options to compare for this specific task ---
    if (profile$is_zero_car) {
      # Zero-car users compare their current travel mode against each policy package in turn
      current_side <- create_current_portfolio(profile, input)
      package_side <- policy_packages[[task_idx]]
      page_title <- "Future Mobility Choice"
      page_subtitle <- p("Please compare your current household travel situation with the following hypothetical future package.")
      
    } else {
      # Car owners compare each of their vehicles against a cycling policy package
      current_side <- create_single_vehicle_portfolio(task_idx, input)
      pkg_idx <- ((task_idx - 1) %% length(policy_packages)) + 1
      package_side <- policy_packages[[pkg_idx]]
      page_title <- "Household Vehicle Choice"
      page_subtitle <- h4(paste0("Decision for your ", get_ordinal(task_idx), " vehicle"))
    }
    
    
    # Helper function for generating choice cards
    create_choice_card_v3 <- function(title, description, cost_text, type) {
      tags$div(
        class = paste("choice-card-v3", type),
        icon(if(type == "current") "home" else "star", class = "fa-2x choice-icon"),
        tags$div(
          class = "choice-content",
          h5(title),
          p(description, class = "small"),
          strong(cost_text)
        )
      )
    }
    
    # CSS to match the styling of Version 4
    css_styles_v3 <- HTML("
      .package-choice-wrapper { max-width: 950px; margin: auto; }
      
      /* Banners (from V4) */
      .reconsideration-banner { padding: 15px; background-color: #f8f9fa; border-left: 5px solid #6c757d; margin-bottom: 20px; border-radius: 4px; }
      .reconsideration-banner h5 { margin: 0 0 5px 0; }
      .summary-box-v4 { margin-top: 20px; margin-bottom: 30px; padding: 15px; background-color: #e3f2fd; border-left: 5px solid #007bff; border-radius: 0; font-weight: bold; text-align: center; }
      
      /* Table styling (from V4) */
      .package-table { border: 1px solid #dee2e6; margin-bottom: 10px; }
      .package-table th, .package-table td { padding: 12px; vertical-align: middle; }
      .package-table thead th { font-size: 1.1em; text-align: center; color: white; border: none !important; }
      .package-table tbody tr:nth-child(even) { background-color: #f8f9fa; }
      .current-col-header { background-color: #28a745 !important; }
      .package-col-header { background-color: #007bff !important; }
      .attr-label { font-weight: bold; }
      
      /* Choice Grid Layout */
      .choice-grid-wrapper-v3 .shiny-options-group { 
        display: grid !important; 
        grid-template-columns: 1fr 1fr !important; 
        gap: 20px !important; 
      }
      .choice-grid-wrapper-v3 .radio, .choice-grid-wrapper-v3 label { 
        margin: 0 !important; 
        width: 100% !important; 
        display: block !important; 
      }
      .choice-grid-wrapper-v3 input[type=radio] { display: none; }

      /* Card Styling */
      .choice-card-v3 { 
        display: flex; flex-direction: column; text-align: center; 
        height: 100%; min-height: 220px; padding: 20px; 
        border: 3px solid #dee2e6; border-radius: 8px; 
        cursor: pointer; transition: all 0.2s ease-in-out; 
        background-color: white;
      }
      .choice-card-v3:hover { border-color: #007bff; }
      .choice-icon { margin-bottom: 10px; color: #495047; font-size: 2.2em; }
      .choice-content h5 { font-size: 1.1em; margin-bottom: 5px; font-weight: bold; }
      .choice-content p { color: #6c757d; min-height: 55px; }
      .choice-content strong { font-size: 1.1em; }
      
      /* Robust selector for the :checked state - NO green border by default */
      .choice-grid-wrapper-v3 label input[type=radio]:checked + .choice-card-v3,
      .choice-grid-wrapper-v3 label input[type=radio]:checked ~ .choice-card-v3,
      .choice-grid-wrapper-v3 label input[type=radio]:checked + span .choice-card-v3,
      .choice-grid-wrapper-v3 label input[type=radio]:checked ~ span .choice-card-v3 {
        border-color: #007bff !important;
        box-shadow: 0 4px 12px rgba(0,123,255,.3);
        transform: translateY(-2px);
      }
    ")
    
    # UI Helper for Dynamic Comparison Table
    create_package_table <- function(current, package) {
      attr_names <- names(package$attributes)
      table_rows <- lapply(attr_names, function(attr_name) {
        tags$tr(
          tags$td(class = "attr-label", attr_name),
          tags$td(current$attributes[[attr_name]] %||% "N/A"),
          tags$td(package$attributes[[attr_name]] %||% "N/A")
        )
      })
      
      tags$table(class = "table table-bordered package-table",
                 tags$thead(
                   tags$tr(
                     tags$th("Feature", style="width:25%"),
                     tags$th(icon("home"), current$title, class="current-col-header"),
                     tags$th(icon("star"), package$title, class="package-col-header")
                   )
                 ),
                 tags$tbody(
                   tags$tr(
                     tags$td(class="attr-label", "Description of your travel options:"),
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
                             if (task_idx < num_tasks) { actionButton("next_task_button_v3", "Next", class = "btn-primary btn-lg") } else { actionButton("finish_v3_button", "Finish and Continue", class = "btn-success btn-lg") }
                         )
    )
    
    tagList(
      tags$head(tags$style(css_styles_v3)),
      div(class = "package-choice-wrapper",
          h3(page_title),
          hr(),
          div(class="reconsideration-banner",
              h5("Please consider you would travel"),
              p("Let's imagine you are reviewing your household's transport for the next few years. The options below present different paths you could take.", style="margin:0;")
          ),
          page_subtitle,
          create_package_table(current_side, package_side),
          div(class="summary-box-v4", textOutput("choice_summary_v3", inline=TRUE)),
          div(class="choice-grid-wrapper-v3",
              radioButtons(
                inputId = paste0("package_choice_v3_task", task_idx),
                label = h4("Which would you choose?", style="text-align:center;"),
                choiceNames = list(
                  create_choice_card_v3(
                    current_side$title, 
                    current_side$description, 
                    current_side$attributes[["Fixed monthly cost"]], 
                    "current"
                  ),
                  create_choice_card_v3(
                    package_side$title, 
                    package_side$description, 
                    package_side$attributes[["Fixed monthly cost"]], 
                    "package"
                  )
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
  
  observeEvent(input$finish_v3_button, {
    updateTabsetPanel(session, "main_tabs", selected = "sa_panel_4")
  })
  
  
  
  # ============================================================================== #
  # ========= Version 2: Ternary Choice Task (Keep/Give Up/Swap) =========
  # ============================================================================== #
  
  
  # --- V4: Wizard State & Navigation ---
  current_task_index_v4 <- reactiveVal(1)
  observeEvent(input$next_task_button_v4, {
    current_task_index_v4(current_task_index_v4() + 1)
  })
  observeEvent(input$prev_task_button_v4, {
    current_task_index_v4(current_task_index_v4() - 1)
  })
  observe({ # Reset wizard if core inputs change
    input$num_cars; input$drivers_licence; current_task_index_v4(1)
  })
  
  # --- V4: Helper function for ordinals ---
  get_ordinal <- function(n) {
    if (n %in% 11:13) return(paste0(n, "th"))
    last_digit <- n %% 10
    suffix <- switch(as.character(last_digit), "1"="st", "2"="nd", "3"="rd", "th")
    paste0(n, suffix)
  }
  
  # --- V4: Dynamic Cost Calculation for the "Change/Swap" Car for OWNERS ---
  calculate_swap_car_cost <- reactive({
    task_idx <- current_task_index_v4()
    # Base cost
    cost <- 200
    # Adjustments based on selections
    cost <- cost + switch(input[[paste0("swap_car_fuel_", task_idx)]] %||% "Petrol", "Petrol" = 0, "Diesel" = 20, "Fully Electric" = 150, "Plug-in Hybrid" = 100, "Other Hybrid" = 60)
    cost <- cost + switch(input[[paste0("swap_car_age_", task_idx)]] %||% "2018 - 2021", "2022 or newer" = 80, "2018 - 2021" = 0, "2013 - 2017" = -50, "2008 - 2012" = -90, "2007 or older" = -120)
    cost <- cost + switch(input[[paste0("swap_car_body_", task_idx)]] %||% "Hatchback", "Hatchback" = 0, "Saloon" = 10, "Estate" = 25, "SUV / 4x4" = 50, "MPV" = 40, "Coupe / Sports car" = 70, "Van" = 30, "Motorbike / Scooter" = -100)
    round(cost)
  })
  
  # --- V4: Dynamic Cost Calculation for the "Add" Car for NON-OWNERS ---
  calculate_add_car_cost <- reactive({
    # Base cost
    cost <- 200
    # Adjustments based on selections (using unique IDs for non-owners)
    cost <- cost + switch(input[["add_car_fuel_zerocar"]] %||% "Petrol", "Petrol" = 0, "Diesel" = 20, "Fully Electric" = 150, "Plug-in Hybrid" = 100, "Other Hybrid" = 60)
    cost <- cost + switch(input[["add_car_age_zerocar"]] %||% "2018 - 2021", "2022 or newer" = 80, "2018 - 2021" = 0, "2013 - 2017" = -50, "2008 - 2012" = -90, "2007 or older" = -120)
    cost <- cost + switch(input[["add_car_body_zerocar"]] %||% "Hatchback", "Hatchback" = 0, "Saloon" = 10, "Estate" = 25, "SUV / 4x4" = 50, "MPV" = 40, "Coupe / Sports car" = 70, "Van" = 30, "Motorbike / Scooter" = -100)
    round(cost)
  })
  
  
  # --- V4: Summary Box Logic ---
  output$choice_summary_v4 <- renderText({ # For Car Owners
    task_idx <- current_task_index_v4()
    choice_input_id <- paste0("choice_v4_task", task_idx)
    req(input[[choice_input_id]])
    
    profile <- create_user_profile(input)
    modes <- get_modes_for_choice(profile, input)
    alt_idx <- ((task_idx - 1) %% length(modes$alternatives)) + 1
    alternative_mode <- modes$alternatives[[alt_idx]]
    
    switch(input[[choice_input_id]],
           "keep" = paste0("SUMMARY: You have chosen to keep your ", get_ordinal(task_idx), " vehicle."),
           "give_up" = paste("SUMMARY: You have chosen to swap for the", alternative_mode$title, "."),
           "change" = {
             swap_cost <- calculate_swap_car_cost()
             paste0("SUMMARY: You have chosen to change this vehicle for a different one with an estimated monthly cost of £", swap_cost, ".")
           }
    )
  })
  
  output$choice_summary_v4_zerocar <- renderText({ # For Zero-Car Households
    choice_input_id <- "choice_v4_task_zerocar"
    req(input[[choice_input_id]])
    
    profile <- create_user_profile(input)
    modes <- get_modes_for_choice(profile, input)
    # Just pick the first alternative for the summary text
    alternative_mode <- modes$alternatives[[1]] 
    
    switch(input[[choice_input_id]],
           "keep" = "SUMMARY: You have chosen to continue with your current travel patterns.",
           "swap" = paste("SUMMARY: You have chosen to swap your main travel mode for the", alternative_mode$title, "."),
           "add" = {
             add_cost <- calculate_add_car_cost()
             paste0("SUMMARY: You have chosen to add a new vehicle to your household with an estimated monthly cost of £", add_cost, ".")
           }
    )
  })
  
  # --- V4: Main UI Rendering ---
  output$sa_ui_placeholder4 <- renderUI({
    req(input$num_cars, input$drivers_licence)
    profile <- create_user_profile(input)
    modes <- get_modes_for_choice(profile, input)
    
    # --- UI Helper: Choice Cards (reusable for both groups) ---
    create_choice_card_v4 <- function(title, description, cost_text, icon_name, type) {
      tags$div(
        class = paste("choice-card-v4", type),
        icon(icon_name, class = "fa-2x choice-icon"),
        tags$div(
          class = "choice-content",
          h5(title),
          p(description, class = "small"),
          strong(cost_text)
        )
      )
    }
    
    # --- UI Helper: Comparison Table (reusable for both groups) ---
    create_comparison_table_v4 <- function(current_v, alt_m, header_text) {
      tags$table(class = "table table-bordered",
                 tags$thead(tags$tr(
                   tags$th("Attribute", style = "width:20%;"),
                   tags$th(icon(current_v$icon), header_text, class = "owned-col-header"),
                   tags$th(icon(alt_m$icon), alt_m$title, class = "alt-col-header")
                 )),
                 tags$tbody(
                   tags$tr(tags$td(strong("Description")), tags$td(current_v$description), tags$td(alt_m$description)),
                   tags$tr(tags$td(strong("Access")), tags$td(current_v$access), tags$td(alt_m$access)),
                   tags$tr(tags$td(strong("Availability")), tags$td(current_v$availability), tags$td(alt_m$availability)),
                   tags$tr(tags$td(strong("Monthly Cost")), tags$td(strong(current_v$cost_text)), tags$td(strong(alt_m$cost_text)))
                 )
      )
    }
    
    # --- V4: CSS Styling Overhaul ---
    css_styles_v4 <- HTML("
      .survey-content-wrapper-v4 { max-width: 1100px; margin: 0 auto; }
      
      /* Table Headers */
      .owned-col-header { background-color: #28a745 !important; color: white; }
      .alt-col-header { background-color: #007bff !important; color: white; }
      .table-bordered th { font-weight: bold; }

      /* Banners */
      .reconsideration-banner { padding: 15px; background-color: #f8f9fa; border-left: 5px solid #6c757d; margin-bottom: 20px; border-radius: 4px; }
      .reconsideration-banner h5 { margin: 0 0 5px 0; }
      .summary-box-v4 { margin-top: 0px; margin-bottom: 30px; padding: 15px; background-color: #e3f2fd; border-left: 5px solid #007bff; border-radius: 0; font-weight: bold; text-align: center; }

      /* Choice Grid and Cards */
      .choice-grid-wrapper .shiny-options-group { display: grid !important; grid-template-columns: repeat(3, 1fr) !important; gap: 20px !important; }
      .choice-grid-wrapper .radio { margin: 0 !important; }
      .choice-grid-wrapper label { margin: 0 !important; width: 100% !important; display: block !important; }
      .choice-card-v4 { 
        display: flex; flex-direction: column; text-align: center; 
        height: 100%; min-height: 220px; padding: 20px; 
        border: 3px solid #dee2e6; border-radius: 8px; 
        cursor: pointer; transition: all 0.2s ease-in-out; 
        background-color: white;
      }
      .choice-card-v4:hover { border-color: #007bff; }
      .choice-icon { margin-bottom: 10px; color: #495057; font-size: 2.2em; }
      .choice-content h5 { font-size: 1.1em; margin-bottom: 5px; font-weight: bold; }
      .choice-content p { color: #6c757d; min-height: 55px; }
      .choice-content strong { font-size: 1.1em; }
      input[type=radio] { display: none; }
      input[type=radio]:checked + div.choice-card-v4 { border-color: #007bff; box-shadow: 0 4px 12px rgba(0,123,255,.3); transform: translateY(-2px); }
      .choice-card-v4.keep { border-color: #28a745; }
      .choice-card-v4.give-up, .choice-card-v4.swap { border-color: #ffc107; }
      .choice-card-v4.change, .choice-card-v4.add { border-color: #6c757d; }

      /* Configurator Panel */
      .configurator-panel { padding: 20px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px; margin-top: 20px; }
    ")
    
    if (profile$is_zero_car) {
      # ============== UI FOR ZERO-CAR HOUSEHOLDS ==============
      if (length(modes$alternatives) == 0) {
        return(p("No alternatives to show."))
      }
      
      status_quo_mode <- modes$current$status_quo
      alternative_mode <- modes$alternatives[[1]] 
      
      choice_names_zc <- list(
        create_choice_card_v4("Continue As You Are", status_quo_mode$description, paste("Current Spend:", status_quo_mode$cost_text), status_quo_mode$icon, "keep"),
        create_choice_card_v4(paste("Swap for", alternative_mode$title), alternative_mode$description, paste("New Cost:", alternative_mode$cost_text), alternative_mode$icon, "swap"),
        create_choice_card_v4("Add a Vehicle", "Add a new, configurable vehicle to your household.", textOutput("add_car_cost_display_zerocar", inline=TRUE), "car", "add")
      )
      choice_values_zc <- c("keep", "swap", "add")
      
      output$add_car_cost_display_zerocar <- renderText({ paste("New Monthly Cost: £", calculate_add_car_cost()) })
      
      banner_ui <- div(class="reconsideration-banner",
                       h5("Time to Consider Your Options"),
                       p("Let's imagine your household is planning its transport for the next few years. The options below present different paths your household could take.", style="margin:0;")
      )
      
      tagList(
        tags$head(tags$style(css_styles_v4)),
        div(class = "survey-content-wrapper-v4",
            h3("Household Mobility Choice"),
            hr(),
            banner_ui,
            h4("Your Household's Travel Options"),
            p("The table below compares your current main way of travelling with a new alternative. Based on this, please select one of the three actions for your household."),
            create_comparison_table_v4(status_quo_mode, alternative_mode, header_text = "Your Current Main Travel"),
            div(class="summary-box-v4", textOutput("choice_summary_v4_zerocar", inline=TRUE)),
            h5("What would you like to do?", style="text-align: center; margin-bottom: 20px;"),
            div(class = "choice-grid-wrapper",
                radioButtons(inputId = "choice_v4_task_zerocar", label = NULL, choiceNames = choice_names_zc, choiceValues = choice_values_zc, selected = input[["choice_v4_task_zerocar"]] %||% character(0), width = "100%")),
            conditionalPanel(
              condition = "input.choice_v4_task_zerocar == 'add'",
              div(class="configurator-panel",
                  h4("Configure Your New Vehicle", style="text-align:center; margin-top:0;"),
                  p("Your selections below will estimate the monthly cost for this new vehicle.", style="text-align:center;"),
                  hr(),
                  fluidRow(
                    column(6, selectInput("add_car_body_zerocar", "Vehicle Body Type:", choices = c("Hatchback", "Saloon", "Estate", "SUV / 4x4", "MPV", "Coupe / Sports car", "Van", "Motorbike / Scooter"))),
                    column(6, selectInput("add_car_fuel_zerocar", "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid", "Other Hybrid")))
                  ),
                  fluidRow(
                    column(6, selectInput("add_car_age_zerocar", "Vehicle Age (Registration Year):", choices = c("2022 or newer", "2018 - 2021", "2013 - 2017", "2008 - 2012", "2007 or older"))),
                    column(6, selectInput("add_car_ownership_zerocar", "How would this vehicle be owned?", choices = c("Owned outright (no finance)", "Owned with a loan/HP", "Personal Contract Purchase (PCP) or Lease")))
                  )
              )
            ),
            div(style="text-align:center; margin-top: 30px;", actionButton("to_sa_button4", "Finish Ownership Section", class = "btn-success btn-lg"))
        )
      )
      
    } else {
      # ============== UI FOR CAR-OWNING HOUSEHOLDS ==============
      task_idx <- current_task_index_v4()
      num_tasks <- profile$num_cars
      
      if (task_idx > num_tasks || length(modes$alternatives) == 0) {
        return(tagList(
          p("You have completed this section. Please click continue."),
          div(style = "text-align: center; margin-top: 30px;", actionButton("to_sa_button4", "Finish and Continue", class = "btn-success btn-lg"))
        ))
      }
      
      current_vehicle <- modes$current[[paste0("owned_", task_idx)]]
      alt_idx <- ((task_idx - 1) %% length(modes$alternatives)) + 1
      alternative_mode <- modes$alternatives[[alt_idx]]
      vehicle_type <- input[[paste0("car", task_idx, "_body_type")]] %||% "Vehicle"
      
      choice_names <- list(
        create_choice_card_v4(paste0("Keep your ", get_ordinal(task_idx), " Vehicle"), paste("No change to your", tolower(vehicle_type)), paste("Monthly Cost:", current_vehicle$cost_text), current_vehicle$icon, "keep"),
        create_choice_card_v4(paste("Swap for", alternative_mode$title), "Relinquish your car and adopt this new mobility service instead.", paste("New Monthly Cost:", alternative_mode$cost_text), alternative_mode$icon, "give-up"),
        create_choice_card_v4("Change this Vehicle", "Swap your current car for a different one that you can configure below.", textOutput(paste0("swap_car_cost_display_", task_idx), inline=TRUE), "car-on", "change")
      )
      choice_values <- c("keep", "give_up", "change")
      
      output[[paste0("swap_car_cost_display_", task_idx)]] <- renderText({ paste("New Monthly Cost: £", calculate_swap_car_cost()) })
      
      navigation_ui <- div(
        style = "margin-top: 30px; display: flex; justify-content: space-between; align-items: center;",
        if (task_idx > 1) { actionButton("prev_task_button_v4", "Back", class = "btn-default btn-lg") } else { div() },
        tags$strong(paste0("Decision ", task_idx, " of ", num_tasks)),
        if (task_idx < num_tasks) { actionButton("next_task_button_v4", "Next", class = "btn-primary btn-lg") } else { actionButton("to_sa_button4", "Finish Ownership Section", class = "btn-success btn-lg") }
      )
      
      banner_ui <- div(class="reconsideration-banner",
                       h5("Please consider you would travel"),
                       p("Let's imagine you are reviewing your household's transport for the next few years. The options below present different paths you could take for your vehicle.", style="margin:0;")
      )
      
      tagList(
        tags$head(tags$style(css_styles_v4)),
        div(class = "survey-content-wrapper-v4",
            h3("Household Vehicle Choice"),
            hr(),
            banner_ui,
            h4(paste0("Decision for your ", get_ordinal(task_idx), " vehicle (a ", tolower(vehicle_type), ")")),
            p("The table below compares your current vehicle with a new alternative travel option. Based on this, please select one of the three actions for this specific vehicle."),
            create_comparison_table_v4(current_vehicle, alternative_mode, header_text = paste0("Your ", get_ordinal(task_idx), " Vehicle")),
            div(class = "summary-box-v4", textOutput("choice_summary_v4", inline = TRUE)),
            h5("What would you like to do with this vehicle?", style="text-align: center; margin-bottom: 20px;"),
            div(class = "choice-grid-wrapper",
                radioButtons(inputId = paste0("choice_v4_task", task_idx), label = NULL, choiceNames = choice_names, choiceValues = choice_values, selected = input[[paste0("choice_v4_task", task_idx)]] %||% character(0), width = "100%")),
            conditionalPanel(
              condition = paste0("input.choice_v4_task", task_idx, " == 'change'"),
              div(class="configurator-panel",
                  h4("Configure Your New Vehicle", style="text-align:center; margin-top:0;"),
                  p("Your selections below will estimate the monthly cost for this new vehicle.", style="text-align:center;"),
                  hr(),
                  fluidRow(
                    column(6, selectInput(paste0("swap_car_body_", task_idx), "Vehicle Body Type:", choices = c("Hatchback", "Saloon", "Estate", "SUV / 4x4", "MPV", "Coupe / Sports car", "Van", "Motorbike / Scooter"))),
                    column(6, selectInput(paste0("swap_car_fuel_", task_idx), "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid", "Other Hybrid")))),
                  fluidRow(
                    column(6, selectInput(paste0("swap_car_age_", task_idx), "Vehicle Age (Registration Year):", choices = c("2022 or newer", "2018 - 2021", "2013 - 2017", "2008 - 2012", "2007 or older"))),
                    column(6, selectInput(paste0("swap_car_ownership_", task_idx), "How would this vehicle be owned?", choices = c("Owned outright (no finance)", "Owned with a loan/HP", "Personal Contract Purchase (PCP) or Lease"))))
              )
            ),
            navigation_ui
        )
      )
    }
  })
  
  # ============================================================================== #
  # ========= VERSION 3: Menu Builder (REFACTORED) =========
  # ============================================================================== #
  
  # --- V5: Wizard State & Navigation ---
  current_task_index_v5 <- reactiveVal(1)
  observeEvent(input$next_task_button_v5, { current_task_index_v5(current_task_index_v5() + 1) })
  observeEvent(input$prev_task_button_v5, { current_task_index_v5(current_task_index_v5() - 1) })
  observe({ input$num_cars; input$drivers_licence; current_task_index_v5(1) })
  
  # --- V5: Core Logic to generate usage tasks based on previous ownership choices ---
  get_usage_tasks_v5 <- reactive({
    profile <- create_user_profile(input)
    all_modes <- get_modes_for_choice(profile, input)
    tasks <- list()
    
    if (profile$is_zero_car) {
      # --- Logic for Zero-Car Households ---
      choice <- input$choice_v4_task_zerocar %||% "keep"
      
      mode_A <- NULL
      mode_B <- NULL
      preamble <- ""
      
      if (choice == "keep") {
        mode_A <- all_modes$current$status_quo
        mode_B <- all_modes$alternatives[[1]] # Compare against first alternative
        preamble <- "You chose to continue with your current travel patterns. Now, please show how you would share your weekly trips if this new service were also available."
      } else if (choice == "swap") {
        mode_A <- all_modes$alternatives[[1]] # The one they swapped for
        mode_B <- all_modes$alternatives[[2]] %||% list(title="Other Travel", description="e.g. Walking, Cycling, Taxi", cost_text="Varies") # Compare against the next alternative
        preamble <- paste("You chose to swap your main travel mode for the", mode_A$title, ". Please show how you would share trips between these options.")
      } else if (choice == "add") {
        mode_A <- list(
          title = "Your New Vehicle", icon = "car", type = "owned",
          description = paste(input[["add_car_age_zerocar"]], input[["add_car_fuel_zerocar"]], input[["add_car_body_zerocar"]]),
          access = "Immediate, 24/7 at your home", availability = "Very reliable",
          cost_val = calculate_add_car_cost(), cost_text = paste0("£", calculate_add_car_cost())
        )
        mode_B <- all_modes$alternatives[[1]]
        preamble <- "You chose to add a new vehicle to your household. Please show how you would share your weekly trips between your new car and this alternative service."
      }
      tasks[[1]] <- list(mode_A = mode_A, mode_B = mode_B, preamble = preamble)
      
    } else {
      # --- Logic for Car-Owning Households ---
      for (i in 1:profile$num_cars) {
        choice <- input[[paste0("choice_v4_task", i)]] %||% "keep"
        original_vehicle <- all_modes$current[[paste0("owned_", i)]]
        # The alternative this vehicle was compared against in the ownership task
        alt_idx <- ((i - 1) %% length(all_modes$alternatives)) + 1
        compared_alt <- all_modes$alternatives[[alt_idx]]
        
        mode_A <- NULL
        mode_B <- NULL
        preamble <- ""
        
        if (choice == "keep") {
          mode_A <- original_vehicle
          mode_B <- compared_alt
          preamble <- paste0("For your ", get_ordinal(i), " vehicle, you chose to KEEP it. Please show how you would share trips between it and the ", compared_alt$title, ".")
        } else if (choice == "give_up") {
          mode_A <- compared_alt
          mode_B <- all_modes$alternatives[[2]] %||% list(title="Other Travel", description="e.g. Walking, Cycling, Taxi", cost_text="Varies")
          preamble <- paste0("For your ", get_ordinal(i), " vehicle, you chose to SWAP it for the ", mode_A$title, ". Please show how you would share trips between your new service and other options.")
        } else if (choice == "change") {
          mode_A <- list(
            title = paste0("Your New ", get_ordinal(i), " Vehicle"), icon = "car-on", type = "owned",
            description = paste(input[[paste0("swap_car_age_", i)]], input[[paste0("swap_car_fuel_", i)]], input[[paste0("swap_car_body_", i)]]),
            access = "Immediate, 24/7 at your home", availability = "Very reliable",
            cost_val = calculate_swap_car_cost(), cost_text = paste0("£", calculate_swap_car_cost())
          )
          mode_B <- compared_alt
          preamble <- paste0("For your ", get_ordinal(i), " vehicle, you chose to CHANGE it. Please show how you would share trips between your new car and the ", compared_alt$title, ".")
        }
        tasks[[i]] <- list(mode_A = mode_A, mode_B = mode_B, preamble = preamble, task_num = i)
      }
    }
    return(tasks)
  })
  
  # --- V5: UI Generation ---
  output$sa_ui_placeholder5 <- renderUI({
    tasks <- get_usage_tasks_v5()
    
    if (length(tasks) == 0) {
      return(tagList(
        p("Based on your previous answers, there are no new trip allocations required.", style = "text-align:center; font-style:italic; margin: 30px;"),
        div(style = "text-align:center;", actionButton("finish_v5_button", "Finish Survey", class = "btn-primary btn-lg"))
      ))
    }
    
    task_idx <- current_task_index_v5()
    num_tasks <- length(tasks)
    
    if (task_idx > num_tasks) {
      return(tagList(
        h3("Trip Allocation Complete"),
        p("You have finished allocating trips for your household. Thank you!", style = "text-align:center; margin: 30px;"),
        div(style = "text-align:center;", actionButton("finish_v5_button", "Finish Survey", class = "btn-success btn-lg"))
      ))
    }
    
    current_task <- tasks[[task_idx]]
    mode_A <- current_task$mode_A
    mode_B <- current_task$mode_B
    
    # --- V5: CSS Styling ---
    css_styles_v5 <- HTML("
      .survey-content-wrapper-v5 { max-width: 1000px; margin: 0 auto; }
      .owned-col-header, .owned-col-cell { background-color: #eafaf1 !important; }
      .alt-col-header, .alt-col-cell { background-color: #e3f2fd !important; }
      .allocation-section { margin-top: 20px; padding: 25px; background-color: #f8f9fa; border-radius: 8px;}
      .proportional-slider-row { padding-top: 15px; border-top: 1px solid #e9ecef; }
      .proportional-slider-row:first-child { border-top: none; }
      .slider-label { font-weight: bold; text-align: center; }
      .percent-display { font-size: 1.5em; font-weight: bold; text-align: center; }
      .mode-A-percent { color: #155724; }
      .mode-B-percent { color: #004085; }
    ")
    
    # --- V5: UI Helper for Comparison Table ---
    create_comparison_table_v5 <- function(mode_A, mode_B) {
      tags$table(class = "table table-bordered",
                 tags$thead(tags$tr(
                   tags$th("Attribute", style = "width:20%;"),
                   tags$th(icon(mode_A$icon %||% "car"), mode_A$title, class = "owned-col-header"),
                   tags$th(icon(mode_B$icon %||% "bus"), mode_B$title, class = "alt-col-header")
                 )),
                 tags$tbody(
                   tags$tr(tags$td(strong("Description")), tags$td(mode_A$description), tags$td(mode_B$description)),
                   tags$tr(tags$td(strong("Access")), tags$td(mode_A$access), tags$td(mode_B$access)),
                   tags$tr(tags$td(strong("Availability")), tags$td(mode_A$availability), tags$td(mode_B$availability)),
                   tags$tr(tags$td(strong("Monthly Cost")), tags$td(strong(mode_A$cost_text)), tags$td(strong(mode_B$cost_text)))
                 )
      )
    }
    
    # --- V5: UI Helper for Proportional Slider ---
    create_proportional_slider <- function(task_idx, purpose, mode_A_title, mode_B_title) {
      clean_purpose <- gsub("[^A-Za-z0-9]", "_", purpose)
      slider_id <- paste0("usage_slider_v5_task", task_idx, "_", clean_purpose)
      
      # Register outputs for the percentage displays
      output[[paste0(slider_id, "_A")]] <- renderText({ paste0(100 - (input[[slider_id]] %||% 50), "%") })
      output[[paste0(slider_id, "_B")]] <- renderText({ paste0(input[[slider_id]] %||% 50, "%") })
      
      div(class="proportional-slider-row",
          h5(purpose, style="text-align:center;"),
          fluidRow(
            column(3, class="slider-label", mode_A_title),
            column(6),
            column(3, class="slider-label", mode_B_title)
          ),
          fluidRow(
            column(3, class="percent-display mode-A-percent", textOutput(paste0(slider_id, "_A"), inline = TRUE)),
            column(6, sliderInput(slider_id, label=NULL, min=0, max=100, value=50, step=5, width="100%")),
            column(3, class="percent-display mode-B-percent", textOutput(paste0(slider_id, "_B"), inline = TRUE))
          )
      )
    }
    
    # --- V5: Main UI Structure ---
    navigation_ui <- div(
      style = "margin-top: 30px; display: flex; justify-content: space-between; align-items: center;",
      if (task_idx > 1) { actionButton("prev_task_button_v5", "Back", class = "btn-default btn-lg") } else { div() },
      tags$strong(paste0("Allocation ", task_idx, " of ", num_tasks)),
      if (task_idx < num_tasks) { actionButton("next_task_button_v5", "Next", class = "btn-primary btn-lg") } else { actionButton("finish_v5_button", "Finish Survey", class = "btn-success btn-lg") }
    )
    
    tagList(
      tags$head(tags$style(css_styles_v5)),
      div(class = "survey-content-wrapper-v5",
          h3("Trip Allocation"),
          hr(),
          p(class="lead", current_task$preamble),
          create_comparison_table_v5(mode_A, mode_B),
          div(class="allocation-section",
              h4("Allocate Weekly Trips", style="text-align:center; margin-top:0; margin-bottom: 20px;"),
              lapply(TRIP_PURPOSES_V2, function(purpose) {
                create_proportional_slider(task_idx, purpose, mode_A$title, mode_B$title)
              })
          ),
          navigation_ui
      )
    )
  })
  
  
  # --- V5: Final button observer ---
  observeEvent(input$finish_v5_button, {
    showModal(modalDialog(
      title = "Survey Complete",
      "Thank you for completing the survey. Your responses are valuable to our research.",
      easyClose = TRUE,
      footer = NULL
    ))
  })

  
  
  }