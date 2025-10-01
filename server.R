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
  # MODULAR CHOICE TASK SYSTEM - COMPLETE REFACTOR
  # ============================================================================== #
  
  # Constants
  TRIP_PURPOSES_V1 <- c( # The longer labels for the radio button version
    "How many trips to work would you take using this mode?",
    "Leisure/visiting family and friends, recreation, etc."
  )
  TRIP_PURPOSES_V2 <- c( # The more concise labels for the slider version
    "Work Trips",
    "Leisure Trips"
  )
  TRIP_LEVEL_MAP <- c("None" = 0, "For a few trips" = 0.25, "For around half" = 0.5, "For most trips" = 0.75, "For all trips" = 1)
  `%||%` <- function(a, b) if (is.null(a)) b else a
  
  # --- NEW HELPER FUNCTION: Maps slider values back to the old frequency labels for V1 ---
  map_numeric_to_frequency <- function(trip_count) {
    if (is.null(trip_count) || trip_count == 0) return("None")
    if (trip_count <= 2) return("For a few trips")
    if (trip_count <= 5) return("For around half")
    if (trip_count <= 8) return("For most trips")
    return("For all trips")
  }
  
  # ============================================================================== #
  # 1. SHARED LOGIC: USER PROFILE, MODE DEFINITIONS, AND TASK GENERATION
  # (Used by both Version 1 and Version 2)
  # ============================================================================== #
  
  # ... (All other shared functions are unchanged) ...
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
      num_cars = parse_num_cars(input$num_cars), has_children = !is.null(input$household_children) && input$household_children != "0",
      has_mobility = input$at_ability != 'No, neither', has_pt_access = input$pt_access == 'Yes',
      commute_distance_miles = switch(input$commute_distance, "Under 1 mile"=0.5, "1-3 miles"=2, "3-5 miles"=4, "5-10 miles"=7.5, "More than 10 miles"=15, 0),
      total_work_trips = total_work_trips, total_leisure_trips = total_leisure_trips,
      is_zero_car = input$num_cars == '0', segment = determine_user_segment(input)
    )
  }
  determine_user_segment <- function(input) {
    has_licence <- input$drivers_licence == 'Yes'; licence_intent <- !is.null(input$licence_intent) && input$licence_intent == 'Yes'
    has_mobility <- input$at_ability != 'No, neither'; has_vehicles <- input$num_cars != '0'
    has_children <- !is.null(input$household_children) && input$household_children != "0"
    base_segment <- 'unknown'
    if (has_vehicles) {
      if (has_licence && has_mobility) base_segment <- 'car_owner_full_mobility'
      else if (has_licence && !has_mobility) base_segment <- 'car_owner_no_mobility'
      else base_segment <- 'car_owner_complex'
    } else {
      if (!has_licence && !licence_intent && !has_mobility) base_segment <- 'zero_car_drt_only'
      else if (!has_licence && !licence_intent && has_mobility) base_segment <- 'zero_car_active_pt'
      else if ((has_licence || licence_intent) && !has_mobility) base_segment <- 'zero_car_club_pt'
      else if ((has_licence || licence_intent) && has_mobility) base_segment <- 'zero_car_full_options'
    }
    if (has_children) paste0(base_segment, "_family") else base_segment
  }
  parse_num_cars <- function(num_cars_input) { if (is.null(num_cars_input) || num_cars_input == '0') return(0); as.integer(gsub("\\+", "", num_cars_input)) }
  parse_cost_bracket <- function(bracket_string) {
    if (is.null(bracket_string) || grepl("Don't know", bracket_string)) return(NA); numbers <- as.numeric(unlist(regmatches(bracket_string, gregexpr("[0-9,]+", bracket_string)))); if (length(numbers) == 0) return(0); if (grepl("Under|£0 -", bracket_string)) return(mean(c(0, numbers))); if (grepl("Over|More than", bracket_string)) return(numbers * 1.2); if (length(numbers) == 1) return(numbers); return(mean(numbers))
  }
  
  create_status_quo_mode <- function(profile, input) {
    if (profile$is_zero_car) {
      cost_input <- if (profile$has_licence) input$car0_cost_rp else input$car0_no_licence_cost_rp
      current_mode_desc <- if (profile$has_licence) input$car0_Commute_licence else input$car0_Commute_no_licence
      
      # --- FIX: Remove the rp_trips element as it's no longer text-based ---
      list(id = "status_quo", title = "Your current main mode of travel", icon = "walking", type = "current", 
           description = paste("You primarily travel by:", current_mode_desc), 
           access = "Your current level of access", availability = "Your current travel reliability", 
           cost_val = parse_cost_bracket(cost_input), cost_text = paste(cost_input, "(your reported spend)"))
    } else { NULL }
  }
  create_owned_vehicle_mode <- function(vehicle_index, input) {
    cost_input_id <- paste0("car", vehicle_index, "_cost_rp"); cost_val_numeric <- parse_cost_bracket(input[[cost_input_id]]); cost_text <- if (is.na(cost_val_numeric)) "Not Provided" else paste0("£", round(cost_val_numeric))
    body_type <- input[[paste0("car", vehicle_index, "_body_type")]] %||% "Not provided"; fuel <- input[[paste0("car", vehicle_index, "_fuel")]] %||% "Not provided"; age_year <- input[[paste0("car", vehicle_index, "_age_year")]] %||% "Not provided"; parking <- input[[paste0("car", vehicle_index, "_parking")]] %||% "Not provided"
    description <- paste0("A ", age_year, " ", fuel, " ", body_type, " that is normally parked ", tolower(parking), ".")
    list(id = paste0("owned_", vehicle_index), title = "Your currently owned vehicle", icon = "car-side", type = "current", description = description, access = "Immediate, 24/7 at your home", availability = "Very reliable (available for >99 in 100 trips)", vehicle_index = vehicle_index, cost_val = cost_val_numeric, cost_text = cost_text)
  }
  create_alternative_modes <- function(profile) {
    modes <- list()
    if (profile$has_licence || profile$licence_intent) {
      if (profile$has_children) {
        modes$car_club_family <- list(id = "car_club_family", title = "Membership to a Family Car Club", icon = "car", type = "alternative", description = "Provides access to larger vehicles (MPVs/SUVs) with guaranteed child seat (ISOFIX) points.", access = "5-10 minute walk to a vehicle", availability = "Reliable (available for ~95 in 100 trips)", cost_val = 75, cost_text = "£75")
      } else {
        modes$car_club_standard <- list(id = "car_club_standard", title = "Membership to a Neighbourhood Car Club", icon = "car-side", type = "alternative", description = "Provides access to a range of small-medium sized cars parked in your local area.", access = "5-10 minute walk to a vehicle", availability = "Reliable (available for ~95 in 100 trips)", cost_val = 60, cost_text = "£60")
      }
    }
    if (profile$has_pt_access) { modes$public_transport <- list(id = "public_transport", title = "Yorkshire Pass for Public Transport", icon = "bus-alt", type = "alternative", description = "A monthly pass providing unlimited travel on all local buses, trams, and trains within West Yorkshire.", access = "Within a 5 minute walk", availability = "Reliable (available for ~95 in 100 trips)", cost_val = 75, cost_text = "£75") }
    if (profile$has_mobility && profile$commute_distance_miles > 0 && profile$commute_distance_miles <= 5) { modes$active_travel <- list(id = "active_travel", title = "Active Travel (Walk/Cycle)", icon = "bicycle", type = "alternative",  description = "Assumes safe, dedicated, and well-lit walking and cycling routes are available for your key journeys.", access = "Immediate, from your front door", availability = "Very reliable (weather dependent)", cost_val = 15, cost_text = "£15 (for maintenance)") }
    if (!profile$has_licence && !profile$licence_intent && !profile$has_mobility) { modes$drt <- list(id = "drt", title = "Demand Responsive Transport", icon = "shuttle-van", type = "alternative", description = "An on-demand, accessible shuttle bus service for door-to-door journeys, booked via an app.", access = "Booked via phone/app", availability = "Reliable (with advance booking)", cost_val = 45, cost_text = "£45") }
    return(modes)
  }
  get_modes_for_choice <- function(profile, input) {
    current_modes <- list(); if (profile$is_zero_car) { status_quo <- create_status_quo_mode(profile, input); if (!is.null(status_quo)) current_modes$status_quo <- status_quo } else { for (i in seq_len(min(profile$num_cars, 4))) { current_modes[[paste0("owned_", i)]] <- create_owned_vehicle_mode(i, input) } }; return(list(current = current_modes, alternatives = create_alternative_modes(profile)))
  }
  get_choice_tasks <- function(profile, modes) {
    tasks <- list(); if (profile$is_zero_car) { status_quo <- modes$current$status_quo; for (alt_id in names(modes$alternatives)) { tasks[[alt_id]] <- list(current = list(status_quo), alternative = modes$alternatives[[alt_id]]) } } else { for (alt_id in names(modes$alternatives)) { tasks[[alt_id]] <- list(current = modes$current, alternative = modes$alternatives[[alt_id]]) } }; return(tasks)
  }
  get_scenario_text <- function(profile, tasks) {
    alt_ids <- sapply(tasks, function(t) t$alternative$id); base_text <- "A new 'Leeds Travel' app provides seamless, integrated booking and payment for all transport services."
    service_descs <- c(); if (any(grepl("car_club", alt_ids))) service_descs <- c(service_descs, "neighbourhood car clubs"); if ("public_transport" %in% alt_ids) service_descs <- c(service_descs, "24/7 public transport"); if ("active_travel" %in% alt_ids) service_descs <- c(service_descs, "priority lanes for cycling and walking"); if ("drt" %in% alt_ids) service_descs <- c(service_descs, "on-demand shuttle services")
    if (length(service_descs) > 0) { services_text <- paste(service_descs, collapse = ", ", sep=""); services_text <- sub(",([^,]*)$", " and\\1", services_text); return(paste(base_text, "This includes", services_text, "across the city.")) }; return(base_text)
  }
  
  # == VERSION 1: RADIO BUTTON WIZARD (for sa_panel_3)
  
  
  # --- V1: Wizard State & Navigation ---
  current_task_index_v1 <- reactiveVal(1)
  observeEvent(input$next_task_button_v1, { current_task_index_v1(current_task_index_v1() + 1) })
  observeEvent(input$prev_task_button_v1, { current_task_index_v1(current_task_index_v1() - 1) })
  observe({ input$num_cars; input$at_ability; input$drivers_licence; input$licence_intent; input$household_children; current_task_index_v1(1) })
  
  # --- V1: UI Generation ---
  create_task_table_v1 <- function(task_num, current_modes, alternative_mode, trip_purposes, input) {
    tags$table( class = "table",
                tags$thead(tags$tr(tags$th("Attribute", style = "width:15%;"), lapply(current_modes, function(m) { tags$th(class = "owned-col-header", style = "position:relative;", tags$div(style = "display:flex; align-items:center; justify-content:center;", icon(m$icon), tags$strong(m$title, style = "font-size:0.9em; margin-left:6px;")), tags$div(style = "position:absolute; top:2px; right:6px; background:#ffd700; color:#333; padding:1px 6px; border-radius:3px; font-size:0.65em; font-weight:700;", "OWNED")) }), tags$th(class = if (alternative_mode$id == "public_transport") "pt-col-header" else "alt-col-header", tags$div(style = "display:flex; align-items:center; justify-content:center;", icon(alternative_mode$icon), tags$strong(alternative_mode$title, style = "font-size:0.9em; margin-left:6px;"))))),
                tags$tbody(
                  tags$tr(tags$th(div(strong("Description"), tags$small(style="font-weight:normal;", "A summary of each travel option."))), lapply(current_modes, function(m) tags$td(class = "owned-col-cell", m$description)), tags$td(class = if(alternative_mode$id == "public_transport") "pt-col-cell" else "alt-col-cell", alternative_mode$description)),
                  tags$tr(tags$th(div(strong("Access"), tags$small(style="font-weight:normal;", "Time required to begin your journey."))), lapply(current_modes, function(m) tags$td(class = "owned-col-cell", m$access)), tags$td(class = if(alternative_mode$id == "public_transport") "pt-col-cell" else "alt-col-cell", alternative_mode$access)),
                  tags$tr(tags$th(div(strong("Availability"), tags$small(style="font-weight:normal;", "Likelihood the service is ready when needed."))), lapply(current_modes, function(m) tags$td(class = "owned-col-cell", m$availability)), tags$td(class = if(alternative_mode$id == "public_transport") "pt-col-cell" else "alt-col-cell", alternative_mode$availability)),
                  tags$tr(tags$th("Fixed Monthly Cost"), lapply(current_modes, function(m) tags$td(class = "owned-col-cell", tags$strong(m$cost_text))), tags$td(class = if(alternative_mode$id == "public_transport") "pt-col-cell" else "alt-col-cell", tags$strong(alternative_mode$cost_text))),
                  lapply(seq_along(trip_purposes), function(p_idx) {
                    purpose <- trip_purposes[[p_idx]]; clean_purpose <- gsub("[^A-Za-z0-9]", "_", purpose)
                    tags$tr(tags$th(purpose, style = "font-weight:normal; font-style:italic;"),
                            lapply(current_modes, function(m) {
                              input_id <- paste0("task", task_num, "_v1_", m$id, "_trips_", clean_purpose)
                              
                              # --- FIX: Convert numeric slider input back to text label for radio button default ---
                              default_sel <- if (m$type == "current") {
                                rp_input_id <- if (length(current_modes) == 1 && m$id == "status_quo") {
                                  # Zero-car household case
                                  if (input$drivers_licence == 'Yes') {
                                    if (p_idx == 1) "car0_work_trips_rp" else "car0_leisure_trips_rp"
                                  } else {
                                    if (p_idx == 1) "car0_no_licence_work_trips_rp" else "car0_no_licence_leisure_trips_rp"
                                  }
                                } else {
                                  # Car-owner case
                                  paste0("car", m$vehicle_index, "_", if(p_idx == 1) "work" else "leisure", "_trips_rp")
                                }
                                map_numeric_to_frequency(input[[rp_input_id]])
                              } else { "None" }
                              
                              tags$td(class = "owned-col-cell", radioButtons(input_id, NULL, names(TRIP_LEVEL_MAP), selected = default_sel %||% "None"))
                            }),
                            tags$td(class = if(alternative_mode$id == "public_transport") "pt-col-cell" else "alt-col-cell", radioButtons(paste0("task", task_num, "_v1_", alternative_mode$id, "_trips_", clean_purpose), NULL, names(TRIP_LEVEL_MAP), selected = "None")))
                  })
                )
    )
  }
  output$sa_ui_placeholder3 <- renderUI({
    req(input$num_cars, input$drivers_licence, input$at_ability, input$household_children, input$commute_distance)
    profile <- create_user_profile(input); modes <- get_modes_for_choice(profile, input); tasks <- get_choice_tasks(profile, modes)
    if (length(tasks) == 0) { return(tagList(p("Based on your previous answers, there are no new transport alternatives to show.", style = "text-align:center; font-style:italic; margin: 30px;"), div(style = "text-align:center;", actionButton("finish_v1_button", "Continue", class = "btn-primary btn-lg")))) }
    task_idx <- current_task_index_v1(); num_tasks <- length(tasks); current_task <- tasks[[task_idx]]
    
    css_styles_v1 <- HTML(".survey-content-wrapper { max-width: 1100px; margin: 0 auto; } .table { table-layout:fixed; border-collapse: separate; border-spacing: 0; } .table th, .table td { padding:8px; word-break:break-word; vertical-align: middle; border: 1px solid #ddd; border-top-width: 0; border-left-width: 0; } .table th:first-child, .table td:first-child { border-left-width: 1px; } .table tr:first-child th, .table tr:first-child td { border-top-width: 1px; } .owned-col-header { background: linear-gradient(135deg,#28a745 0%,#1e7e34 100%) !important; color:white; text-align:center; } .owned-col-cell { background-color:#eafaf1; } .alt-col-header { background: linear-gradient(135deg,#007bff 0%,#0056b3 100%) !important; color:white; text-align:center; } .alt-col-cell { background-color:#f8f9fa; } .pt-col-header { background: linear-gradient(135deg, #ffc107 0%, #d39e00 100%) !important; color: #212529; text-align:center; } .pt-col-cell { background-color: #fff9e6; } .combined-info-box { padding:14px; background: linear-gradient(135deg,#e3f2fd 0%,#f8f9fa 100%); border-left:5px solid #007bff; border-radius:8px; box-shadow:0 2px 8px rgba(0,0,0,0.06); } .summary-pill { display:flex; justify-content:space-between; align-items:center; padding:10px 12px; border-radius:8px; margin-bottom:10px; box-shadow:0 1px 3px rgba(0,0,0,0.06); } .summary-pill .label { font-weight:700; color:#495057; font-size:0.85em; } .summary-pill .value { font-size:14px; font-weight:700; }")
    scenario_box <- div(style = "margin-top:12px; font-size:0.9em; color:#333; padding:10px; border-radius:6px; background: linear-gradient(90deg, #ffffff, #f7f9fb); border:1px solid #e1e7ef;", h5(tags$u("Imagine that in the next 3 years:")), p(get_scenario_text(profile, tasks)))
    instructions_content <- div(style = "flex:1; min-width:320px;", h5(tags$u("Instructions")), tags$ul(style="font-size: 1.05em; padding-left: 20px;", tags$li("The table below compares your current travel with a new alternative."), tags$li("Your task is to show how you would divide your household's trips between them."), tags$li("As you make selections, the summary box will update live."), tags$li("Use the following guide for trip allocation:"), tags$ul(style="padding-left: 20px; font-style: italic; color: #555;", tags$li(strong("For a few trips"), " means roughly 1-2 trips per week."), tags$li(strong("For around half"), " means roughly 3-5 trips per week."), tags$li(strong("For most trips"), " means the majority of trips for that purpose.")), tags$li("Once you are happy with your trip allocation, please press the 'Next' button.")), scenario_box)
    summary_pills <- div(style = "width:340px; flex-shrink:0;", div(class = "summary-pill", style="background: linear-gradient(90deg,#e9f7ef,#fff); border:1px solid #d4edda;", div(class = "label", "Work Trip Allocation"), div(class = "value", textOutput("trips_work_summary_v1", inline = TRUE))), div(class = "summary-pill", style="background: linear-gradient(90deg,#fff4e6,#fff); border:1px solid #ffeeba;", div(class = "label", "Leisure Trip Allocation"), div(class = "value", textOutput("trips_leisure_summary_v1", inline = TRUE))), div(class = "summary-pill", style="background: linear-gradient(90deg,#fff,#fff); border:1px solid #dee2e6;", div(class = "label", "Est. Monthly Cost"), div(class = "value", textOutput("cost_summary_val_v1", inline = TRUE))))
    table_content <- create_task_table_v1(task_idx, current_task$current, current_task$alternative, TRIP_PURPOSES_V1, input)
    navigation_ui <- div(style = "margin-top: 20px; display: flex; justify-content: space-between; align-items: center;", if (task_idx > 1) { actionButton("prev_task_button_v1", "Back", class = "btn-default btn-lg") } else { div() }, tags$strong(paste0("Task ", task_idx, " of ", num_tasks)), if (task_idx < num_tasks) { actionButton("next_task_button_v1", "Next", class = "btn-primary btn-lg") } else { actionButton("finish_v1_button", "Finish and Continue", class = "btn-success btn-lg") })
    tagList(tags$style(css_styles_v1), div(class = "survey-content-wrapper", div(class = "combined-info-box", style = "margin-bottom: 20px; display:flex; gap:20px; flex-wrap:wrap; align-items:flex-start;", instructions_content, summary_pills), table_content, navigation_ui))
  })
  # --- V1: Calculation Logic ---
  compute_estimates_v1 <- reactive({
    req(input$num_cars, current_task_index_v1(), input$household_children, input$commute_distance); req(input$drivers_licence, input$at_ability)
    profile <- create_user_profile(input); modes <- get_modes_for_choice(profile, input); tasks <- get_choice_tasks(profile, modes); if (length(tasks) == 0) return(list(est_cost = "N/A", work_text = "N/A", leisure_text = "N/A"))
    task_idx <- current_task_index_v1(); current_task <- tasks[[task_idx]]; modes_in_this_task <- c(current_task$current, list(current_task$alternative)); all_mode_props <- list(); all_mode_costs <- list(); mode_names <- list()
    for (mode in modes_in_this_task) {
      mode_id <- mode$id
      props <- sapply(TRIP_PURPOSES_V1, function(purpose) { clean_purpose <- gsub("[^A-Za-z0-9]", "_", purpose); input_id <- paste0("task", task_idx, "_v1_", mode_id, "_trips_", clean_purpose); sel <- input[[input_id]]; if (!is.null(sel) && sel %in% names(TRIP_LEVEL_MAP)) TRIP_LEVEL_MAP[[sel]] else 0 })
      all_mode_props[[mode_id]] <- props; all_mode_costs[[mode_id]] <- mode$cost_val %||% 0; mode_names[[mode_id]] <- if (mode$type == "current" && profile$is_zero_car) mode$title else if (mode$type == "current") paste("Vehicle", mode$vehicle_index) else mode$title
    }
    if (length(all_mode_props) == 0) return(list(est_cost = "£0", work_text = "Awaiting input...", leisure_text = "Awaiting input..."))
    mode_props_matrix <- do.call(cbind, all_mode_props); mode_avgs <- rowMeans(mode_props_matrix, na.rm = TRUE); mode_costs <- unlist(all_mode_costs[colnames(mode_props_matrix)]); est_monthly_cost <- sum(mode_costs * mode_avgs, na.rm = TRUE)
    make_breakdown_text <- function(purpose_idx) {
      props_vec <- mode_props_matrix[purpose_idx, ]; active_mask <- props_vec > 0.001; if (!any(active_mask)) return("No trips selected"); active_names <- unlist(mode_names)[active_mask]; active_vals <- props_vec[active_mask]; total_prop <- sum(active_vals); if (total_prop < 0.001) return("No trips selected"); pct <- round(100 * active_vals / total_prop); final_names <- active_names[pct > 0]; final_pcts <- pct[pct > 0]; if(length(final_names) == 0) return("No trips selected"); paste0(paste0(final_names, ": ", final_pcts, "%"), collapse = " | ")
    }
    list(est_cost = paste0("£", formatC(round(est_monthly_cost), format = "f", big.mark = ",", digits = 0)), work_text = make_breakdown_text(1), leisure_text = make_breakdown_text(2))
  })
  # --- V1: Render Outputs ---
  output$cost_summary_val_v1 <- renderText({ compute_estimates_v1()$est_cost }); output$trips_work_summary_v1 <- renderText({ compute_estimates_v1()$work_text }); output$trips_leisure_summary_v1 <- renderText({ compute_estimates_v1()$leisure_text })
  observeEvent(input$finish_v1_button, {})
  
  
  # ========= Version 2: Ternary Choice Task (Keep/Give Up/Swap) =========
  
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
           "give_up" = paste("SUMMARY: You have chosen to give up this vehicle for the", alternative_mode$title, "."),
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
    
    css_styles_v4 <- HTML("
      .survey-content-wrapper-v4 { max-width: 1100px; margin: 0 auto; }
      .owned-col-header { background-color: #eafaf1 !important; }
      .alt-col-header { background-color: #f8f9fa !important; }
      .choice-grid-wrapper .shiny-options-group { display: grid !important; grid-template-columns: repeat(3, 1fr) !important; gap: 20px !important; }
      .choice-grid-wrapper .radio { margin: 0 !important; }
      .choice-grid-wrapper label { margin: 0 !important; width: 100% !important; display: block !important; }
      .choice-card-v4 { display: flex; flex-direction: column; text-align: center; height: 100%; min-height: 200px; padding: 15px; border: 2px solid #ddd; border-radius: 8px; cursor: pointer; transition: all 0.2s ease-in-out; }
      .choice-card-v4:hover { border-color: #007bff; background-color: #f8f9fa; }
      .choice-icon { margin-bottom: 10px; color: #495057; }
      .choice-content h5 { font-size: 1.1em; margin-bottom: 5px; }
      .choice-content p { color: #6c757d; min-height: 55px; }
      input[type=radio] { display: none; }
      input[type=radio]:checked + div.choice-card-v4 { border-width: 3px; border-color: #007bff; box-shadow: 0 0 10px rgba(0,123,255,.5); }
      .choice-card-v4.keep { border-top: 5px solid #28a745; }
      .choice-card-v4.give-up, .choice-card-v4.swap { border-top: 5px solid #ffc107; }
      .choice-card-v4.change, .choice-card-v4.add { border-top: 5px solid #6c757d; }
      .summary-box-v4 { margin-top: 20px; padding: 15px; background-color: #e3f2fd; border-left: 5px solid #007bff; border-radius: 4px; font-weight: bold; text-align: center; }
      .configurator-panel { padding: 20px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px; margin-top: 20px; }
    ")
    
    if (profile$is_zero_car) {
      # ============== UI FOR ZERO-CAR HOUSEHOLDS ==============
      if (length(modes$alternatives) == 0) {
        return(p("No alternatives to show."))
      }
      
      status_quo_mode <- modes$current$status_quo
      alternative_mode <- modes$alternatives[[1]] # Compare against the first available alternative
      
      # Define choices for zero-car households
      choice_names_zc <- list(
        create_choice_card_v4("Continue As You Are", status_quo_mode$description, paste("Current Spend:", status_quo_mode$cost_text), status_quo_mode$icon, "keep"),
        create_choice_card_v4(paste("Swap for", alternative_mode$title), alternative_mode$description, paste("New Cost:", alternative_mode$cost_text), alternative_mode$icon, "swap"),
        create_choice_card_v4("Add a Vehicle", "Add a new, configurable vehicle to your household.", textOutput("add_car_cost_display_zerocar", inline=TRUE), "car", "add")
      )
      choice_values_zc <- c("keep", "swap", "add")
      
      # Live update for the cost display in the "Add" card
      output$add_car_cost_display_zerocar <- renderText({
        paste("New Monthly Cost: £", calculate_add_car_cost())
      })
      
      tagList(
        tags$head(tags$style(css_styles_v4)),
        div(class = "survey-content-wrapper-v4",
            h3("Household Mobility Choice"),
            hr(),
            h4("Your Household's Travel Options"),
            p("The table below compares your current main way of travelling with a new alternative. Based on this, please select one of the three actions for your household."),
            create_comparison_table_v4(status_quo_mode, alternative_mode, header_text = "Your Current Main Travel"),
            div(class="summary-box-v4", textOutput("choice_summary_v4_zerocar", inline=TRUE)),
            hr(style="margin-top:30px;"),
            h5("What would you like to do?", style="text-align: center; margin-bottom: 20px;"),
            div(class = "choice-grid-wrapper",
                radioButtons(
                  inputId = "choice_v4_task_zerocar",
                  label = NULL,
                  choiceNames = choice_names_zc,
                  choiceValues = choice_values_zc,
                  selected = input[["choice_v4_task_zerocar"]] %||% character(0),
                  width = "100%"
                )
            ),
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
      
      tagList(
        tags$head(tags$style(css_styles_v4)),
        div(class = "survey-content-wrapper-v4",
            h3("Household Vehicle Choice"),
            hr(),
            h4(paste0("Decision for your ", get_ordinal(task_idx), " vehicle (a ", tolower(vehicle_type), ")")),
            p("The table below compares your current vehicle with a new alternative travel option. Based on this, please select one of the three actions for this specific vehicle."),
            create_comparison_table_v4(current_vehicle, alternative_mode, header_text = paste0("Your ", get_ordinal(task_idx), " Vehicle")),
            div(class = "summary-box-v4", textOutput("choice_summary_v4", inline = TRUE)),
            hr(style="margin-top:30px;"),
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
  
  # ========= VERSION 3: Menu Builder (REFACTORED) =========
  # ============================================================================== #
  # == VERSION 3: USAGE ALLOCATION WIZARD (for sa_panel_5)
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