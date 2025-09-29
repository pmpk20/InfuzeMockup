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
  
  # --- V4: Summary Box Logic ---
  output$choice_summary_v4 <- renderText({
    task_idx <- current_task_index_v4()
    choice_input_id <- paste0("choice_v4_task", task_idx)
    req(input[[choice_input_id]])
    
    profile <- create_user_profile(input)
    modes <- get_modes_for_choice(profile, input)
    alt_idx <- ((task_idx - 1) %% length(modes$alternatives)) + 1
    alternative_mode <- modes$alternatives[[alt_idx]]
    
    switch(input[[choice_input_id]],
           "keep" = "SUMMARY: You have chosen to keep your current vehicle.",
           "give_up" = paste("SUMMARY: You have chosen to give up this vehicle for the", alternative_mode$title, ".")
    )
  })
  
  # --- V4: Main UI Rendering ---
  output$sa_ui_placeholder4 <- renderUI({
    req(input$num_cars, input$drivers_licence)
    profile <- create_user_profile(input)
    
    if (profile$is_zero_car) {
      return(tagList(
        h3("Household Vehicle Choice"),
        p("As your household does not currently own a vehicle, this section is not applicable. Please proceed to the next section."),
        div(style = "text-align: center; margin-top: 30px;", actionButton("to_sa_button4", "Continue", class = "btn-primary btn-lg"))
      ))
    }
    
    modes <- get_modes_for_choice(profile, input)
    num_tasks <- profile$num_cars
    task_idx <- current_task_index_v4()
    
    if (task_idx > num_tasks || length(modes$alternatives) == 0) {
      return(tagList(
        p("You have completed this section. Please click continue."),
        div(style = "text-align: center; margin-top: 30px;", actionButton("to_sa_button4", "Continue", class = "btn-success btn-lg"))
      ))
    }
    
    # --- Define Task Components ---
    current_vehicle <- modes$current[[paste0("owned_", task_idx)]]
    alt_idx <- ((task_idx - 1) %% length(modes$alternatives)) + 1
    alternative_mode <- modes$alternatives[[alt_idx]]
    
    # --- UI Helper: Comparison Table ---
    create_comparison_table_v4 <- function(current_v, alt_m) {
      tags$table(class = "table table-bordered",
                 tags$thead(tags$tr(
                   tags$th("Attribute", style = "width:20%;"),
                   tags$th(icon("car-side"), "Your Current Vehicle", class = "owned-col-header"),
                   tags$th(icon(alt_m$icon), alt_m$title, class = "alt-col-header")
                 )),
                 tags$tbody(
                   tags$tr(tags$td(strong("Description")), tags$td(current_v$description), tags$td(alt_m$description)),
                   tags$tr(tags$td(strong("Access")), tags$td(current_v$access), tags$td(alt_m$access)),
                   tags$tr(tags$td(strong("Availability")), tags$td(current_v$availability), tags$td(alt_m$availability)),
                   tags$tr(tags$td(strong("Monthly Cost")), tags$td(current_v$cost_text), tags$td(alt_m$cost_text))
                 )
      )
    }
    
    # --- UI Helper: Choice Cards ---
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
    
    choice_names <- list(
      create_choice_card_v4("Keep this Vehicle", "No change to this vehicle.", paste("Monthly Cost:", current_vehicle$cost_text), current_vehicle$icon, "keep"),
      create_choice_card_v4(paste("Give up for", alternative_mode$title), alternative_mode$description, paste("New Cost:", alternative_mode$cost_text), alternative_mode$icon, "give-up")
    )
    choice_values <- c("keep", "give_up")
    
    # --- Main Layout ---
    css_styles_v4 <- HTML("
    .survey-content-wrapper-v4 { max-width: 1000px; margin: 0 auto; }
    .owned-col-header { background-color: #eafaf1 !important; }
    .alt-col-header { background-color: #f8f9fa !important; }
    
    /* Force radio buttons into 2-column grid */
    .choice-grid-wrapper .shiny-options-group {
      display: grid !important;
      grid-template-columns: 1fr 1fr !important;
      gap: 20px !important;
      margin: 0 !important;
    }
    
    .choice-grid-wrapper .radio {
      margin: 0 !important;
      width: 100% !important;
    }
    
    .choice-grid-wrapper label {
      margin: 0 !important;
      width: 100% !important;
      display: block !important;
    }
    
    .choice-card-v4 { 
      display: flex; 
      flex-direction: column; 
      text-align: center; 
      height: 100%; 
      min-height: 200px;
      padding: 15px; 
      border: 2px solid #ddd; 
      border-radius: 8px; 
      cursor: pointer; 
      transition: all 0.2s ease-in-out; 
    }
    .choice-card-v4:hover { border-color: #007bff; background-color: #f8f9fa; }
    .choice-icon { margin-bottom: 10px; color: #495057; }
    .choice-content h5 { font-size: 1.1em; margin-bottom: 5px; }
    .choice-content p { color: #6c757d; min-height: 40px; }
    input[type=radio] { display: none; }
    input[type=radio]:checked + div.choice-card-v4 { border-width: 3px; border-color: #007bff; box-shadow: 0 0 10px rgba(0,123,255,.5); }
    .choice-card-v4.keep { border-top: 5px solid #28a745; }
    .choice-card-v4.give-up { border-top: 5px solid #dc3545; }
    .summary-box-v4 {
      margin-top: 20px;
      padding: 15px;
      background-color: #e3f2fd;
      border-left: 5px solid #007bff;
      border-radius: 4px;
      font-weight: bold;
      text-align: center;
    }
  ")
    
    navigation_ui <- div(
      style = "margin-top: 30px; display: flex; justify-content: space-between; align-items: center;",
      if (task_idx > 1) { actionButton("prev_task_button_v4", "Back", class = "btn-default btn-lg") } else { div() },
      tags$strong(paste0("Decision ", task_idx, " of ", num_tasks)),
      if (task_idx < num_tasks) { actionButton("next_task_button_v4", "Next", class = "btn-primary btn-lg") } else { actionButton("to_sa_button4", "Finish and Continue", class = "btn-success btn-lg") }
    )
    
    tagList(
      tags$head(tags$style(css_styles_v4)),
      div(class = "survey-content-wrapper-v4",
          h3("Household Vehicle Choice"),
          hr(),
          h4(paste0("Decision for Vehicle ", task_idx, ": Your ", input[[paste0("car",task_idx,"_body_type")]])),
          p("The table below compares your current vehicle with a new alternative travel option. Based on this, please select one of the two actions for this specific vehicle."),
          create_comparison_table_v4(current_vehicle, alternative_mode),
          
          div(class = "summary-box-v4",
              textOutput("choice_summary_v4", inline = TRUE)
          ),
          
          hr(style="margin-top:30px;"),
          
          h5("What would you like to do with this vehicle?", style="text-align: center; margin-bottom: 20px;"),
          
          div(class = "choice-grid-wrapper",
              radioButtons(
                inputId = paste0("choice_v4_task", task_idx),
                label = NULL,
                choiceNames = choice_names,
                choiceValues = choice_values,
                selected = input[[paste0("choice_v4_task", task_idx)]] %||% character(0),
                width = "100%"
              )
          ),
          
          navigation_ui
      )
    )
  })
  
  
  # ========= VERSION 3: Menu Builder (REFACTORED) =========
  # ============================================================================== #
  # ============================================================================== #
  # == VERSION 2: SLIDER ALLOCATION WIZARD (for sa_panel_5)
  # ============================================================================== #
  # ============================================================================== #
  
  # --- V2: Wizard State & Navigation ---
  current_task_index_v2 <- reactiveVal(1)
  observeEvent(input$next_task_button_v2, { current_task_index_v2(current_task_index_v2() + 1) })
  observeEvent(input$prev_task_button_v2, { current_task_index_v2(current_task_index_v2() - 1) })
  observe({ input$num_cars; input$at_ability; input$drivers_licence; input$licence_intent; input$household_children; current_task_index_v2(1) })
  
  # --- V2: UI Generation ---
  create_task_table_v2 <- function(task_num, current_modes, alternative_mode, trip_purposes, input) {
    num_current_modes <- length(current_modes); total_cols <- num_current_modes + 1
    current_mode_label <- if (num_current_modes == 1) { current_modes[[1]]$title } else { "Your Household Vehicles" }
    tags$table(class = "table",
               tags$thead(tags$tr(tags$th("Attribute", style = "width:20%;"), lapply(current_modes, function(m) { tags$th(class = "owned-col-header", style = "position:relative;", tags$div(style = "display:flex; align-items:center; justify-content:center;", icon(m$icon), tags$strong(m$title, style = "font-size:0.9em; margin-left:6px;")), tags$div(style = "position:absolute; top:2px; right:6px; background:#ffd700; color:#333; padding:1px 6px; border-radius:3px; font-size:0.65em; font-weight:700;", "OWNED")) }), tags$th(class = if (alternative_mode$id == "public_transport") "pt-col-header" else "alt-col-header", tags$div(style = "display:flex; align-items:center; justify-content:center;", icon(alternative_mode$icon), tags$strong(alternative_mode$title, style = "font-size:0.9em; margin-left:6px;"))))),
               tags$tbody(
                 tags$tr(tags$th(div(strong("Description"), tags$small(style="font-weight:normal;", "A summary of each travel option."))), lapply(current_modes, function(m) tags$td(class = "owned-col-cell", m$description)), tags$td(class = if(alternative_mode$id == "public_transport") "pt-col-cell" else "alt-col-cell", alternative_mode$description)),
                 tags$tr(tags$th(div(strong("Access"), tags$small(style="font-weight:normal;", "Time required to begin your journey."))), lapply(current_modes, function(m) tags$td(class = "owned-col-cell", m$access)), tags$td(class = if(alternative_mode$id == "public_transport") "pt-col-cell" else "alt-col-cell", alternative_mode$access)),
                 tags$tr(tags$th(div(strong("Availability"), tags$small(style="font-weight:normal;", "Likelihood the service is ready when needed."))), lapply(current_modes, function(m) tags$td(class = "owned-col-cell", m$availability)), tags$td(class = if(alternative_mode$id == "public_transport") "pt-col-cell" else "alt-col-cell", alternative_mode$availability)),
                 tags$tr(tags$th("Fixed Monthly Cost"), lapply(current_modes, function(m) tags$td(class = "owned-col-cell", tags$strong(m$cost_text))), tags$td(class = if(alternative_mode$id == "public_transport") "pt-col-cell" else "alt-col-cell", tags$strong(alternative_mode$cost_text))),
                 lapply(seq_along(trip_purposes), function(p_idx) {
                   purpose <- trip_purposes[[p_idx]]; clean_purpose <- gsub("[^A-Za-z0-9]", "_", purpose); slider_input_id <- paste0("task", task_num, "_share_v2_", clean_purpose)
                   tagList(
                     tags$tr(class = "allocation-header-row", tags$th(colspan = total_cols + 1, style = "text-align: center; background-color: #f8f9fa; border-top: 2px solid #dee2e6 !important;", tags$strong(paste(purpose, "Allocation")))),
                     tags$tr(tags$td(colspan = total_cols + 1, style = "padding: 10px 20px;", div(style = "display:flex; align-items:center; gap:15px; width:100%;", tags$span(style = "font-weight:500; text-align:right; flex-basis:150px; flex-shrink:0;", paste("100%", current_mode_label)), div(style = "flex-grow:1;", sliderInput(slider_input_id, label = NULL, min = 0, max = 100, value = 0, step = 5, post = "%")), tags$span(style = "font-weight:500; text-align:left; flex-basis:150px; flex-shrink:0;", paste("100%", alternative_mode$title)))))
                   )
                 })
               )
    )
  }
  output$sa_ui_placeholder5 <- renderUI({
    req(input$num_cars, input$drivers_licence, input$at_ability, input$household_children, input$commute_distance)
    profile <- create_user_profile(input); modes <- get_modes_for_choice(profile, input); tasks <- get_choice_tasks(profile, modes)
    if (length(tasks) == 0) { return(tagList(p("Based on your previous answers, there are no new transport alternatives to show.", style = "text-align:center; font-style:italic; margin: 30px;"), div(style = "text-align:center;", actionButton("finish_v2_button", "Continue", class = "btn-primary btn-lg")))) }
    task_idx <- current_task_index_v2(); num_tasks <- length(tasks); current_task <- tasks[[task_idx]]
    
    css_styles_v2 <- HTML(".survey-content-wrapper { max-width: 1100px; margin: 0 auto; } .table { table-layout:fixed; border-collapse: separate; border-spacing: 0; } .table th, .table td { padding:8px; word-break:break-word; vertical-align: middle; border: 1px solid #ddd; border-top-width: 0; border-left-width: 0; } .table th:first-child, .table td:first-child { border-left-width: 1px; } .table tr:first-child th, .table tr:first-child td { border-top-width: 1px; } .owned-col-header { background: linear-gradient(135deg,#28a745 0%,#1e7e34 100%) !important; color:white; text-align:center; } .owned-col-cell { background-color:#eafaf1; } .alt-col-header { background: linear-gradient(135deg,#007bff 0%,#0056b3 100%) !important; color:white; text-align:center; } .alt-col-cell { background-color:#f8f9fa; } .pt-col-header { background: linear-gradient(135deg, #ffc107 0%, #d39e00 100%) !important; color: #212529; text-align:center; } .pt-col-cell { background-color: #fff9e6; } .combined-info-box { padding:14px; background: linear-gradient(135deg,#e3f2fd 0%,#f8f9fa 100%); border-left:5px solid #007bff; border-radius:8px; box-shadow:0 2px 8px rgba(0,0,0,0.06); } .summary-pill { display:flex; justify-content:space-between; align-items:center; padding:10px 12px; border-radius:8px; margin-bottom:10px; box-shadow:0 1px 3px rgba(0,0,0,0.06); } .summary-pill .label { font-weight:700; color:#495057; font-size:0.85em; } .summary-pill .value { font-size:14px; font-weight:700; } .allocation-header-row th { border-bottom-width: 0 !important; }")
    scenario_box <- div(style = "margin-top:12px; font-size:0.9em; color:#333; padding:10px; border-radius:6px; background: linear-gradient(90deg, #ffffff, #f7f9fb); border:1px solid #e1e7ef;", h5(tags$u("Imagine that in the next 3 years:")), p(get_scenario_text(profile, tasks)))
    instructions_content <- div(style = "flex:1; min-width:320px;", h5(tags$u("Instructions")), tags$ul(style="font-size: 1.05em; padding-left: 20px;", tags$li("The table below compares your current travel with a new alternative."), tags$li("Use the slider to show how you would allocate your trips for each purpose."), tags$li("As you make selections, the summary box will update live."), tags$li("Once you are happy with your allocation, please press the 'Next' button.")), scenario_box)
    summary_pills <- div(style = "width:340px; flex-shrink:0;", div(class = "summary-pill", style="background: linear-gradient(90deg,#e9f7ef,#fff); border:1px solid #d4edda;", div(class = "label", "Work Trip Allocation"), div(class = "value", textOutput("trips_work_summary_v2", inline = TRUE))), div(class = "summary-pill", style="background: linear-gradient(90deg,#fff4e6,#fff); border:1px solid #ffeeba;", div(class = "label", "Leisure Trip Allocation"), div(class = "value", textOutput("trips_leisure_summary_v2", inline = TRUE))), div(class = "summary-pill", style="background: linear-gradient(90deg,#fff,#fff); border:1px solid #dee2e6;", div(class = "label", "Est. Monthly Cost"), div(class = "value", textOutput("cost_summary_val_v2", inline = TRUE))))
    table_content <- create_task_table_v2(task_idx, current_task$current, current_task$alternative, TRIP_PURPOSES_V2, input)
    navigation_ui <- div(style = "margin-top: 20px; display: flex; justify-content: space-between; align-items: center;", if (task_idx > 1) { actionButton("prev_task_button_v2", "Back", class = "btn-default btn-lg") } else { div() }, tags$strong(paste0("Task ", task_idx, " of ", num_tasks)), if (task_idx < num_tasks) { actionButton("next_task_button_v2", "Next", class = "btn-primary btn-lg") } else { actionButton("finish_v2_button", "Finish and Continue", class = "btn-success btn-lg") })
    tagList(tags$style(css_styles_v2), div(class = "survey-content-wrapper", div(class = "combined-info-box", style = "margin-bottom: 20px; display:flex; gap:20px; flex-wrap:wrap; align-items:flex-start;", instructions_content, summary_pills), table_content, navigation_ui))
  })
  # --- V2: Calculation Logic ---
  compute_estimates_v2 <- reactive({
    req(input$num_cars, current_task_index_v2(), input$household_children, input$commute_distance); req(input$drivers_licence, input$at_ability)
    profile <- create_user_profile(input); modes <- get_modes_for_choice(profile, input); tasks <- get_choice_tasks(profile, modes); if (length(tasks) == 0) return(list(est_cost = "N/A", work_text = "N/A", leisure_text = "N/A"))
    task_idx <- current_task_index_v2(); current_task <- tasks[[task_idx]]; all_mode_props <- list(); all_mode_costs <- list(); mode_names <- list()
    shares_by_purpose <- sapply(seq_along(TRIP_PURPOSES_V2), function(p_idx) { clean_purpose <- gsub("[^A-Za-z0-9]", "_", TRIP_PURPOSES_V2[[p_idx]]); slider_id <- paste0("task", task_idx, "_share_v2_", clean_purpose); alt_share <- (input[[slider_id]] %||% 0) / 100; c(current = 1 - alt_share, alternative = alt_share) })
    num_current_modes <- length(current_task$current)
    for (mode in current_task$current) { all_mode_props[[mode$id]] <- shares_by_purpose['current', ] / num_current_modes; all_mode_costs[[mode$id]] <- mode$cost_val %||% 0; mode_names[[mode$id]] <- if (profile$is_zero_car) mode$title else paste("Vehicle", mode$vehicle_index) }
    alt_mode <- current_task$alternative; all_mode_props[[alt_mode$id]] <- shares_by_purpose['alternative', ]; all_mode_costs[[alt_mode$id]] <- alt_mode$cost_val %||% 0; mode_names[[alt_mode$id]] <- alt_mode$title
    if (length(all_mode_props) == 0) return(list(est_cost = "£0", work_text = "Awaiting input...", leisure_text = "Awaiting input..."))
    mode_props_matrix <- do.call(cbind, all_mode_props); mode_avgs <- rowMeans(mode_props_matrix, na.rm = TRUE); mode_costs <- unlist(all_mode_costs[colnames(mode_props_matrix)]); est_monthly_cost <- sum(mode_costs * mode_avgs, na.rm = TRUE)
    make_breakdown_text <- function(purpose_idx) {
      props_vec <- mode_props_matrix[purpose_idx, ]; active_mask <- props_vec > 0.001; if (!any(active_mask)) return("No trips selected"); active_names <- unlist(mode_names)[active_mask]; active_vals <- props_vec[active_mask]; total_prop <- sum(active_vals); if (total_prop < 0.001) return("No trips selected"); pct <- round(100 * active_vals / total_prop); final_names <- active_names[pct > 0]; final_pcts <- pct[pct > 0]; if(length(final_names) == 0) return("No trips selected"); paste0(paste0(final_names, ": ", final_pcts, "%"), collapse = " | ")
    }
    list(est_cost = paste0("£", formatC(round(est_monthly_cost), format = "f", big.mark = ",", digits = 0)), work_text = make_breakdown_text(1), leisure_text = make_breakdown_text(2))
  })
  # --- V2: Render Outputs ---
  output$cost_summary_val_v2 <- renderText({ compute_estimates_v2()$est_cost }); output$trips_work_summary_v2 <- renderText({ compute_estimates_v2()$work_text }); output$trips_leisure_summary_v2 <- renderText({ compute_estimates_v2()$leisure_text })
  observeEvent(input$finish_v2_button, {})
  
}