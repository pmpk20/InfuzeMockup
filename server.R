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
  
  # Define trip purposes for the choice task
  trip_purposes <- c(
    "How many trips to work would you take using this mode?",
    "Leisure/visiting family and friends, recreation, etc."
  )
  
  # Helper function to convert text cost brackets to numeric values
  parse_cost_bracket <- function(bracket_string) {
    if (is.null(bracket_string) || grepl("Don't know", bracket_string)) return(NA)
    numbers <- as.numeric(unlist(regmatches(bracket_string, gregexpr("[0-9,]+", bracket_string))))
    if (length(numbers) == 0) return(0)
    if (grepl("Under|£0 -", bracket_string)) return(mean(c(0, numbers)))
    if (grepl("Over|More than", bracket_string)) return(numbers * 1.2)
    if (length(numbers) == 1) return(numbers)
    return(mean(numbers))
  }
  
  # Reactive expression to build the list of modes for the choice task
  modes_reactive <- reactive({
    make_val <- function(id, fallback = "Not provided") {
      v <- input[[id]]
      if (is.null(v) || v == "") fallback else v
    }
    
    create_vehicle_desc_full <- function(i) {
      body_type <- make_val(paste0("car", i, "_body_type"))
      fuel      <- make_val(paste0("car", i, "_fuel"))
      age_year  <- make_val(paste0("car", i, "_age_year"))
      parking   <- make_val(paste0("car", i, "_parking"))
      paste0("Body Type: ", body_type, "; Fuel: ", fuel, "; Age: ", age_year, "; Parking: ", parking)
    }
    
    modes_list <- list()
    req(input$num_cars)
    num_cars <- as.integer(gsub("\\+", "", input$num_cars))
    if (is.na(num_cars)) num_cars <- 0
    num_cars <- min(num_cars, 4L)
    
    if (num_cars > 0) {
      for (i in seq_len(num_cars)) {
        cost_input_id <- paste0("car", i, "_cost_rp")
        req(input[[cost_input_id]])
        
        cost_val_numeric <- parse_cost_bracket(input[[cost_input_id]])
        cost_text <- if (is.na(cost_val_numeric)) "Not Provided" else paste0("£", round(cost_val_numeric))
        
        modes_list[[length(modes_list) + 1]] <- list(
          id = paste0("current_", i),
          title = "Your currently owned vehicle",
          icon = "car-side",
          is_current = TRUE,
          access = "Immediate, 24/7 at home",
          availability = "Very High (99% success rate)",
          vehicle_index = i,
          cost_val = cost_val_numeric,
          cost_text = cost_text,
          description = create_vehicle_desc_full(i)
        )
      }
    }
    
    modes_list[[length(modes_list) + 1]] <- list(
      id = "public_transport",
      title = "Yorkshire Pass",
      icon = "bus-alt",
      is_current = FALSE,
      description = "Monthly pass for all local public transport (bus, tram, train).",
      access = "Within a 5 min walk",
      availability = "High (95% success rate)",
      cost_val = 75,
      cost_text = "£75"
    )
    
    modes_list
  })
  
  # Dynamic UI for "Choices: Version 1"
  # Dynamic UI for "Choices: Version 1"
  output$sa_ui_placeholder3 <- renderUI({
    req(input$num_cars)
    
    # --- UI for Zero-Car Households ---
    if (input$num_cars == '0') {
      req(input$drivers_licence)
      
      if (input$drivers_licence == 'Yes') {
        req(input$car0_Commute_licence, input$car0_work_trips_rp, input$car0_leisure_trips_rp, input$car0_cost_rp)
        current_commute <- input$car0_Commute_licence
        rp_work_trips <- input$car0_work_trips_rp
        rp_leisure_trips <- input$car0_leisure_trips_rp
        rp_cost_text <- input$car0_cost_rp
      } else {
        req(input$car0_Commute_no_licence, input$car0_no_licence_work_trips_rp, input$car0_no_licence_leisure_trips_rp, input$car0_no_licence_cost_rp)
        current_commute <- input$car0_Commute_no_licence
        rp_work_trips <- input$car0_no_licence_work_trips_rp
        rp_leisure_trips <- input$car0_no_licence_leisure_trips_rp
        rp_cost_text <- input$car0_no_licence_cost_rp
      }
      
      status_quo_mode <- list(
        id = "status_quo", title = "Your Current Main Mode", icon = "walking", is_current = TRUE,
        description = paste("Primarily using:", current_commute), access = "Current Access", availability = "Current Availability",
        cost_text = paste(rp_cost_text, "(your reported spend)"),
        rp_trips = c(rp_work_trips, rp_leisure_trips)
      )
      
      if ((input$drivers_licence == 'Yes') || (!is.null(input$licence_intent) && input$licence_intent == 'Yes')) {
        alternatives <- list(list(id = "car_club", title = "Enhanced Car Club Membership", icon = "car-side", is_current = FALSE, description = "Full membership to a dedicated car club.", access = "5-10 min walk", availability = "High (98% success)", cost_val = 60, cost_text = "£60", rp_trips = c("None", "None")))
      } else {
        alternatives <- list(list(id = "public_transport", title = "Yorkshire Pass", icon = "bus-alt", is_current = FALSE, description = "A monthly pass for all local public transport.", access = "Within a 5 min walk", availability = "High (95% success)", cost_val = 75, cost_text = "£75", rp_trips = c("None", "None")))
      }
      
      create_binary_choice_table <-
        function(task_num, option_A, option_B) {
          modes_for_this_table <- list(option_A, option_B)
          div(style = "margin-bottom: 25px;", tags$table(
            class = "table",
            tags$thead(tags$tr(
              tags$th("Attribute", style = "width:15%;"),
              lapply(modes_for_this_table, function(m) {
                header_class <- if (m$is_current) "owned-col-header" else "alt-col-header"
                tags$th(
                  class = header_class,
                  tags$div(
                    style = "display:flex; align-items:center; justify-content:center;",
                    icon(m$icon),
                    tags$strong(m$title, style = "font-size:0.9em; margin-left:6px;")
                  )
                )
              })
            )),
            tags$tbody(
              tags$tr(tags$th("Description"), lapply(modes_for_this_table, function(m) { tags$td(class = if (m$is_current) "owned-col-cell" else "alt-col-cell", m$description) })),
              # --- 2. ATTRIBUTE FIDELITY ---
              tags$tr(tags$th(div(strong("Access"), tags$small(style="font-weight:normal;", "Walking time to access this mode"))), lapply(modes_for_this_table, function(m) { tags$td(class = if (m$is_current) "owned-col-cell" else "alt-col-cell", m$access) })),
              tags$tr(tags$th(div(strong("Availability"), tags$small(style="font-weight:normal;", "The chance the service is ready when you need it"))), lapply(modes_for_this_table, function(m) { tags$td(class = if (m$is_current) "owned-col-cell" else "alt-col-cell", m$availability) })),
              # --- 3. PLACEHOLDER ROW ---
              tags$tr(tags$th(div(strong("Personal Security"))), 
                      tags$td(class = "owned-col-cell", "High (private, familiar space)"),
                      tags$td(class = "alt-col-cell", "High (professionally managed, well-lit hubs)")
              ),
              tags$tr(tags$th("Fixed Monthly Cost"), lapply(modes_for_this_table, function(m) { tags$td(class = if (m$is_current) "owned-col-cell" else "alt-col-cell", tags$strong(m$cost_text)) })),
              lapply(seq_along(trip_purposes), function(p_idx) {
                purpose <- trip_purposes[[p_idx]]
                clean_purpose <- gsub("[^A-Za-z0-9]", "_", purpose)
                tags$tr(
                  tags$th(purpose, style = "font-weight:normal; font-style:italic;"),
                  lapply(modes_for_this_table, function(m) {
                    input_id <- paste0("zc_task", task_num, "_", m$id, "_trips_", clean_purpose)
                    default_sel <- m$rp_trips[[p_idx]]
                    tags$td(class = if (m$is_current) "owned-col-cell" else "alt-col-cell",
                            radioButtons(input_id, NULL, c("None", "For a few trips", "For around half", "For most trips", "For all trips"), selected = default_sel))
                  })
                )
              })
            )
          ))
        }
      
      tagList(
        tags$style(HTML(".survey-content-wrapper { max-width: 1100px; margin: 0 auto; } .sp-layout-container { display: flex; flex-wrap: wrap; align-items: flex-start; gap: 20px; } .sp-table-wrapper { flex: 2; min-width: 0; } .sp-summary-wrapper { flex: 1; } .sticky-summary { position: -webkit-sticky; position: sticky; top: 20px; z-index: 20; } .table { table-layout:fixed; border-collapse: separate; border-spacing: 0; } .table th, .table td { padding:8px; word-break:break-word; vertical-align: middle; border: 1px solid #ddd; border-top-width: 0; border-left-width: 0; } .table tbody td { text-align: left; } .table tbody td[style*='text-align:center'] { text-align: center !important; } .table th:first-child, .table td:first-child { border-left-width: 1px; } .table tr:first-child th, .table tr:first-child td { border-top-width: 1px; } .owned-col-header { background: linear-gradient(135deg,#28a745 0%,#1e7e34 100%) !important; color:white; text-align:center; position:relative; } .owned-col-cell { background-color:#eafaf1; } .alt-col-header { background: linear-gradient(135deg,#007bff 0%,#0056b3 100%) !important; color:white; text-align:center; } .alt-col-cell { background-color:#f8f9fa; } .pt-col-header { background: linear-gradient(135deg, #ffc107 0%, #d39e00 100%) !important; color: #212529; text-align:center; } .pt-col-cell { background-color: #fff9e6; } .combined-info-box { padding:14px; background: linear-gradient(135deg,#e3f2fd 0%,#f8f9fa 100%); border-left:5px solid #007bff; border-radius:8px; box-shadow:0 2px 8px rgba(0,0,0,0.06); } .summary-pill { display:flex; justify-content:space-between; align-items:center; padding:10px 12px; border-radius:8px; margin-bottom:10px; box-shadow:0 1px 3px rgba(0,0,0,0.06); } .summary-pill .label { font-weight:700; color:#495057; font-size:0.85em; } .summary-pill .value { font-size:14px; font-weight:700; } @media (max-width: 991px) { .sp-layout-container { flex-direction: column; } .sp-table-wrapper, .sp-summary-wrapper { flex: 1 1 100%; width: 100%; } .sticky-summary { position: relative; top: auto; } }")),
        div(class = "survey-content-wrapper",
            div(class = "sp-table-wrapper",
                div(class = "combined-info-box", style = "margin-bottom: 20px;",
                    # --- 1. SCENARIO TEXT UPDATED (ZERO-CAR) ---
                    div(class = "summary-pill", style = "background: linear-gradient(90deg,#e9ecef,#fff); border:1px solid #ced4da;", 
                        div(class = "label", "Imagine that in the next 3 years:"), 
                        div(class = "value", style="font-weight:normal; text-align:right;", "A new 'Leeds Travel' app provides seamless access to 24/7 public transport and local car clubs.")),
                    div(class = "summary-pill", style = "background: linear-gradient(90deg,#e9f7ef,#fff); border:1px solid #d4edda;", div(class = "label", "Work trips"), div(class = "value", textOutput("trips_work_summary", inline = TRUE))),
                    div(class = "summary-pill", style = "background: linear-gradient(90deg,#fff4e6,#fff); border:1px solid #ffeeba;", div(class = "label", "Leisure trips"), div(class = "value", textOutput("trips_leisure_summary", inline = TRUE))),
                    div(class = "summary-pill", style = "background: linear-gradient(90deg,#fff,#fff); border:1px solid #dee2e6;", div(class = "label", "Est. Monthly Cost"), div(class = "value", textOutput("cost_summary_val", inline = TRUE))),
                    div(style="margin-top: 15px; padding-top: 10px; border-top: 1px solid #ccc;", h5(tags$u("Instructions")),
                        p("Please read the following carefully:", style="font-size: 1.1em;"),
                        tags$ul(style="font-size: 1.1em; padding-left: 20px;",
                                tags$li("The table below compares your current travel with a new alternative. Your task is to show how you would divide your household's trips between them."),
                                tags$li("As you make selections, this summary box will update live to show the total cost of your choices."),
                                tags$li("Use the following guide for trip allocation: ", tags$ul(style="padding-left: 20px; font-style: italic; color: #555;", tags$li(strong("For a few trips"), " means roughly 1-2 trips per week."), tags$li(strong("For around half"), " means roughly 3-5 trips per week."), tags$li(strong("For most trips"), " means the majority of trips for that purpose."))),
                                tags$li("Once you are happy with your trip allocation for this scenario, please press the 'Continue' button.")
                        )
                    )
                ),
                lapply(seq_along(alternatives), function(i) { create_binary_choice_table(i, status_quo_mode, alternatives[[i]]) }),
                actionButton("to_sa_button4", "Continue", class = "btn-primary btn-lg", style = "margin-top:20px;")
            )
        )
      )
      # --- UI for Car-Owning Households ---
    } else {
      modes <- modes_reactive()
      if (is.null(modes) || length(modes) == 0) return(tags$div("No modes available"))
      num_modes <- length(modes)
      pt_mode <- modes[[num_modes]]
      owned_modes <- if (num_modes > 1) modes[1:(num_modes - 1)] else list()
      
      tagList(
        tags$style(HTML(".survey-content-wrapper { max-width: 1100px; margin: 0 auto; } .sp-layout-container { display: flex; flex-wrap: wrap; align-items: flex-start; gap: 20px; } .sp-table-wrapper { flex: 2; min-width: 0; } .sp-summary-wrapper { flex: 1; } .sticky-summary { position: -webkit-sticky; position: sticky; top: 20px; z-index: 20; } .table { table-layout:fixed; border-collapse: separate; border-spacing: 0; } .table th, .table td { padding:8px; word-break:break-word; vertical-align: middle; border: 1px solid #ddd; border-top-width: 0; border-left-width: 0; } .table tbody td { text-align: left; } .table tbody td[style*='text-align:center'] { text-align: center !important; } .table th:first-child, .table td:first-child { border-left-width: 1px; } .table tr:first-child th, .table tr:first-child td { border-top-width: 1px; } .owned-col-header { background: linear-gradient(135deg,#28a745 0%,#1e7e34 100%) !important; color:white; text-align:center; position:relative; } .owned-col-cell { background-color:#eafaf1; } .pt-col-header { background: linear-gradient(135deg, #ffc107 0%, #d39e00 100%) !important; color: #212529; text-align:center; } .pt-col-cell { background-color: #fff9e6; } .combined-info-box { padding:14px; background: linear-gradient(135deg,#e3f2fd 0%,#f8f9fa 100%); border-left:5px solid #007bff; border-radius:8px; box-shadow:0 2px 8px rgba(0,0,0,0.06); } .summary-pill { display:flex; justify-content:space-between; align-items:center; padding:10px 12px; border-radius:8px; margin-bottom:10px; box-shadow:0 1px 3px rgba(0,0,0,0.06); } .summary-pill .label { font-weight:700; color:#495057; font-size:0.85em; } .summary-pill .value { font-size:14px; font-weight:700; } @media (max-width: 991px) { .sp-layout-container { flex-direction: column; } .sp-table-wrapper, .sp-summary-wrapper { flex: 1 1 100%; width: 100%; } .sticky-summary { position: relative; top: auto; } }")),
        div(class = "survey-content-wrapper",
            div(class = "sp-table-wrapper",
                div(class = "combined-info-box", style = "margin-bottom: 20px;",
                    # --- 1. SCENARIO TEXT UPDATED (CAR-OWNER) ---
                    div(class = "summary-pill", style = "background: linear-gradient(90deg,#e9ecef,#fff); border:1px solid #ced4da;", 
                        div(class = "label", "Imagine that in the next 3 years:"), 
                        div(class = "value", style="font-weight:normal; text-align:right;", "A new 'Leeds Travel' app provides seamless access to 24/7 public transport and local car clubs.")),
                    div(class = "summary-pill", style = "background: linear-gradient(90deg,#e9f7ef,#fff); border:1px solid #d4edda;", div(class = "label", "Work trips"), div(class = "value", textOutput("trips_work_summary", inline = TRUE))),
                    div(class = "summary-pill", style = "background: linear-gradient(90deg,#fff4e6,#fff); border:1px solid #ffeeba;", div(class = "label", "Leisure trips"), div(class = "value", textOutput("trips_leisure_summary", inline = TRUE))),
                    div(class = "summary-pill", style = "background: linear-gradient(90deg,#fff,#fff); border:1px solid #dee2e6;", div(class = "label", "Est. Monthly Cost"), div(class = "value", textOutput("cost_summary_val", inline = TRUE))),
                    div(style="margin-top: 15px; padding-top: 10px; border-top: 1px solid #ccc;", h5(tags$u("Instructions")),
                        p("Please read the following carefully:", style="font-size: 1.1em;"),
                        tags$ul(style="font-size: 1.1em; padding-left: 20px;",
                                tags$li("The table below compares your current vehicle(s) with an alternative. Your task is to show how you would divide your household's trips across all available options."),
                                tags$li("As you make selections, this summary box will update live to show the total cost of your choices."),
                                tags$li("Use the following guide for trip allocation: ", tags$ul(style="padding-left: 20px; font-style: italic; color: #555;", tags$li(strong("For a few trips"), " means roughly 1-2 trips per week."), tags$li(strong("For around half"), " means roughly 3-5 trips per week."), tags$li(strong("For most trips"), " means the majority of trips for that purpose."))),
                                tags$li("Once you are happy with your trip allocation, please press the 'Continue' button.")
                        )
                    )
                ),
                tags$table(class = "table",
                           tags$thead(tags$tr(tags$th("Attribute", style = "width:15%;"), lapply(seq_along(owned_modes), function(ii) { tags$th(class = "owned-col-header", style = "position:relative;", tags$div(style = "display:flex; align-items:center; justify-content:center;", icon(owned_modes[[ii]]$icon), tags$strong("Your currently owned vehicle", style = "font-size:0.9em; margin-left:6px;")), tags$div(style = "position:absolute; top:2px; right:6px; background:#ffd700; color:#333; padding:1px 6px; border-radius:3px; font-size:0.65em; font-weight:700;", "OWNED"))}), tags$th(class = "pt-col-header", tags$div(style = "display:flex; align-items:center; justify-content:center;", icon(pt_mode$icon), tags$strong(pt_mode$title, style = "font-size:0.9em; margin-left:6px;"))))),
                           tags$tbody(
                             tags$tr(tags$th("Description"), lapply(owned_modes, function(m) tags$td(class = "owned-col-cell", m$description)), tags$td(class = "pt-col-cell", pt_mode$description)),
                             # --- 2. ATTRIBUTE FIDELITY ---
                             tags$tr(tags$th(div(strong("Access"), tags$small(style="font-weight:normal;", "Walking time to access this mode"))), lapply(owned_modes, function(m) tags$td(class = "owned-col-cell", m$access)), tags$td(class = "pt-col-cell", pt_mode$access)),
                             tags$tr(tags$th(div(strong("Availability"), tags$small(style="font-weight:normal;", "The chance the service is ready when you need it"))), lapply(owned_modes, function(m) tags$td(class = "owned-col-cell", m$availability)), tags$td(class = "pt-col-cell", pt_mode$availability)),
                             # --- 3. PLACEHOLDER ROW ---
                             tags$tr(tags$th(div(strong("Personal Security"))), 
                                     lapply(owned_modes, function(m) tags$td(class = "owned-col-cell", "Very High (private, familiar space)")),
                                     tags$td(class = "pt-col-cell", "High (professionally managed, well-lit hubs)")
                             ),
                             tags$tr(tags$th("Fixed Monthly Cost"), lapply(owned_modes, function(m) tags$td(class = "owned-col-cell", tags$strong(m$cost_text))), tags$td(class = "pt-col-cell", tags$strong(pt_mode$cost_text))),
                             lapply(seq_along(trip_purposes), function(p_idx) {
                               purpose <- trip_purposes[[p_idx]]
                               clean_purpose <- gsub("[^A-Za-z0-9]", "_", purpose)
                               tags$tr(tags$th(purpose, style = "font-weight:normal; font-style:italic;"),
                                       lapply(seq_along(owned_modes), function(ii) {
                                         rp_input_id <- paste0("car", ii, "_", if(p_idx == 1) "work" else "leisure", "_trips_rp")
                                         default_sel <- input[[rp_input_id]]
                                         inputId <- paste0("current_trips_v", ii, "_", clean_purpose)
                                         tags$td(class = "owned-col-cell", radioButtons(inputId, NULL, c("None","For a few trips","For around half","For most trips","For all trips"), selected = default_sel))
                                       }),
                                       tags$td(class = "pt-col-cell", radioButtons(paste0("pt_trips_", clean_purpose), NULL, c("None","For a few trips","For around half","For most trips","For all trips"), selected = if (grepl("work", purpose, ignore.case = TRUE)) "For a few trips" else "None"))
                               )
                             })
                           )
                ),
                actionButton("to_sa_button4", "Continue", class = "btn-primary btn-lg", style = "margin-top:20px;")
            )
        )
      )
    }
  })
  
  
  # ---- Cost/Trips Computation for Summary Box ----
  trip_level_map <- c("None" = 0, "For a few trips" = 0.25, "For around half" = 0.5, "For most trips" = 0.75, "For all trips" = 1)
  
  compute_estimates <- reactive({
    req(input$num_cars)
    
    # --- Logic for ZERO-CAR households ---
    if (input$num_cars == '0') {
      req(input$drivers_licence)
      
      rp_cost <- if (input$drivers_licence == 'Yes') {
        req(input$car0_cost_rp)
        parse_cost_bracket(input$car0_cost_rp)
      } else {
        req(input$car0_no_licence_cost_rp)
        parse_cost_bracket(input$car0_no_licence_cost_rp)
      }
      if (is.na(rp_cost)) rp_cost <- 0
      
      modes_in_choice <- if ((input$drivers_licence == 'Yes') || (!is.null(input$licence_intent) && input$licence_intent == 'Yes')) {
        list(list(id = "status_quo", name = "Current Mode", cost_val = rp_cost), list(id = "car_club", name = "Car Club", cost_val = 60))
      } else {
        list(list(id = "status_quo", name = "Current Mode", cost_val = rp_cost), list(id = "public_transport", name = "Yorkshire Pass", cost_val = 75))
      }
      
      all_props <- sapply(modes_in_choice, function(m) {
        sapply(trip_purposes, function(purpose) {
          clean_purpose <- gsub("[^A-Za-z0-9]", "_", purpose)
          input_id <- paste0("zc_task1_", m$id, "_trips_", clean_purpose)
          sel <- input[[input_id]]
          if (!is.null(sel) && sel %in% names(trip_level_map)) trip_level_map[[sel]] else 0
        })
      })
      
      mode_avgs <- colMeans(all_props, na.rm = TRUE)
      mode_costs <- sapply(modes_in_choice, `[[`, "cost_val")
      est_monthly_cost <- sum(mode_costs * mode_avgs, na.rm = TRUE)
      
      make_breakdown_text <- function(purpose_idx) {
        props_vec <- all_props[purpose_idx, ]
        names_vec <- sapply(modes_in_choice, `[[`, "name")
        active_mask <- props_vec > 0
        if (!any(active_mask)) return("No trips selected")
        active_names <- names_vec[active_mask]
        active_vals <- props_vec[active_mask]
        pct <- round(100 * active_vals / sum(active_vals))
        paste0(paste0(active_names, ": ", pct, "%"), collapse = " | ")
      }
      
      return(list(
        est_cost = paste0("£", formatC(round(est_monthly_cost), format = "f", big.mark = ",", digits = 0)),
        work_text = make_breakdown_text(1),
        leisure_text = make_breakdown_text(2)
      ))
      
      # --- Logic for CAR-OWNING households ---
    } else {
      modes_local <- modes_reactive()
      if (is.null(modes_local) || length(modes_local) < 1) {
        return(list(est_cost = "£0", work_text = "No modes configured", leisure_text = "No modes configured"))
      }
      
      num_modes <- length(modes_local)
      pt_mode <- modes_local[[num_modes]]
      owned_modes <- if (num_modes > 1) modes_local[1:(num_modes - 1)] else list()
      n_owned <- length(owned_modes)
      
      all_props <- sapply(modes_local, function(m) {
        sapply(trip_purposes, function(purpose) {
          clean_purpose <- gsub("[^A-Za-z0-9]", "_", purpose)
          input_id <- if (m$is_current) {
            paste0("current_trips_v", m$vehicle_index, "_", clean_purpose)
          } else {
            paste0("pt_trips_", clean_purpose)
          }
          sel <- input[[input_id]]
          if (!is.null(sel) && sel %in% names(trip_level_map)) trip_level_map[[sel]] else 0
        })
      })
      
      mode_avgs <- colMeans(all_props, na.rm = TRUE)
      mode_costs <- sapply(modes_local, function(m) if (is.na(m$cost_val)) 0 else m$cost_val)
      est_monthly_cost <- sum(mode_costs * mode_avgs, na.rm = TRUE)
      
      make_breakdown_text <- function(purpose_idx) {
        props_vec <- all_props[purpose_idx, ]
        names_vec <- sapply(seq_along(modes_local), function(i) {
          if (modes_local[[i]]$is_current) paste0("Vehicle ", modes_local[[i]]$vehicle_index) else "Public Transport"
        })
        active_mask <- props_vec > 0
        if (!any(active_mask)) return("No trips selected")
        active_names <- names_vec[active_mask]
        active_vals <- props_vec[active_mask]
        pct <- round(100 * active_vals / sum(active_vals))
        paste0(paste0(active_names, ": ", pct, "%"), collapse = " | ")
      }
      
      list(
        est_cost = paste0("£", formatC(round(est_monthly_cost), format = "f", big.mark = ",", digits = 0)),
        work_text = make_breakdown_text(1),
        leisure_text = make_breakdown_text(2)
      )
    }
  })
  
  # ---------- Render outputs for the summary box ----------
  output$cost_summary_val <- renderText({ compute_estimates()$est_cost })
  output$trips_work_summary <- renderText({ compute_estimates()$work_text })
  output$trips_leisure_summary <- renderText({ compute_estimates()$leisure_text })
  


  # ========= Version 2: Menu by sliders =========
  mode_data <- reactive({
    req(input$num_cars)
    
    create_vehicle_desc <- function(car_num) {
      fuel <- input[[paste0("car", car_num, "_fuel")]]
      type <- input[[paste0("car", car_num, "_type")]]
      if (is.null(fuel) || is.null(type)) return("Unknown Vehicle")
      paste0(tolower(fuel), " ", tolower(type))
    }
    
    modes <- list()
    num_cars <- as.integer(gsub("\\+", "", input$num_cars))
    if (is.na(num_cars)) num_cars <- 0
    
    # 1. Add current vehicles from RP data
    if (num_cars > 0) {
      for (i in 1:min(num_cars, 4)) {
        modes[[length(modes) + 1]] <- list(
          id = paste0("current_", i), 
          title = paste("Your", create_vehicle_desc(i)), 
          icon = "car-side", 
          is_current = TRUE,
          access = "Immediate, 24/7 at home", 
          availability = "Very High (99% success rate)", 
          cost_val = 350
        )
      }
    }
    
    # 2. Add alternative/configurable modes
    modes[[length(modes) + 1]] <- list(
      id = "alt_1", 
      title = "A New Configurable Car", 
      icon = "car-alt", 
      is_current = FALSE, 
      is_configurable = TRUE,
      access = "Immediate, 24/7 at home", 
      availability = "Highest (Brand New)", 
      cost_val = NULL
    )
    
    modes[[length(modes) + 1]] <- list(
      id = "car_sharing", 
      title = "Peer-to-Peer Car-Sharing Membership", 
      icon = "users", 
      is_current = FALSE,
      access = "Within a 10 min walk", 
      availability = "High (95% success rate)", 
      cost_val = 60
    )
    
    modes[[length(modes) + 1]] <- list(
      id = "car_club", 
      title = "Closed Loop Car-Club Membership", 
      icon = "users", 
      is_current = FALSE,
      access = "Within a 10 min walk", 
      availability = "High (95% success rate)", 
      cost_val = 60
    )
    
    modes[[length(modes) + 1]] <- list(
      id = "public_transport", 
      title = "Yorkshire Pass: Covers All Public Transport", 
      icon = "bus-alt", 
      is_current = FALSE,
      access = "Within a 5 min walk", 
      availability = "Medium (90% success rate)", 
      cost_val = 75
    )
    
    return(modes)
  })
  
  
  
  output$sa_ui_placeholder4 <- renderUI({
    modes <- mode_data()
    
    # --- Helper function to generate a single row ---
    create_allocation_row <- function(mode, initial_trips) {
      row_class <- if (mode$is_current) "allocation-row current-vehicle-row" else "allocation-row"
      
      cost_display <- if (is.null(mode$cost_val)) {
        tags$span(id="dynamic_cost_display", textOutput("dynamic_price_alt_1", inline = TRUE))
      } else { 
        paste0("£", mode$cost_val) 
      }
      
      title_cell_content <- if (!is.null(mode$is_configurable) && mode$is_configurable) {
        tags$div(
          tags$div(
            style = "display: flex; align-items: center; margin-bottom: 8px;", 
            icon(mode$icon, class = "fa-lg", style="margin-right: 10px;"), 
            tags$strong(mode$title)
          ),
          tags$div(
            style = "padding: 8px; border-radius: 6px; margin-top: 8px; background-color: #f8f9fa;",
            selectInput("sa_alt_1_replace_type", "Type:", c("Car", "Van", "Motorbike"), width = "100%"),
            selectInput("sa_alt_1_replace_fuel", "Fuel:", c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid"), width = "100%"),
            selectInput("sa_alt_1_replace_mileage", "Mileage:", c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+"), width = "100%")
          )
        )
      } else {
        tags$div(
          style = "display: flex; align-items: center;", 
          icon(mode$icon, class = "fa-lg", style="margin-right: 10px;"), 
          tags$strong(mode$title)
        )
      }
      
      tags$tr(
        class = row_class, 
        `data-cost` = if(!is.null(mode$cost_val)) mode$cost_val else "", 
        `data-dynamic-cost` = if(is.null(mode$cost_val)) "true" else "false",
        tags$td(style = "vertical-align: middle; width: 25%;", title_cell_content),
        tags$td(style = "text-align: center; vertical-align: middle; font-size: 0.85em; width: 15%;", mode$access),
        tags$td(style = "text-align: center; vertical-align: middle; font-size: 0.85em; width: 15%;", mode$availability),
        tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold; width: 10%;", cost_display),
        tags$td(
          style = "vertical-align: middle; width: 35%;",
          div(class = "slider-container",
              span(class="slider-label-left", "Never"),
              div(class = "allocation-cell",
                  sliderInput(
                    paste0("trips_", mode$id), 
                    NULL, 
                    min = 0, 
                    max = 20, 
                    value = initial_trips, 
                    step = 1, 
                    width = "100%"
                  ),
                  span(class = "trips-value-display", paste0(initial_trips, " trips"))
              ),
              span(class="slider-label-right", "Daily")
          )
        )
      )
    }
    
    # --- Initial trips calculation (more realistic) ---
    num_modes <- length(modes)
    initial_trips <- rep(0, num_modes)
    
    # Reasonable weekly trip assumptions
    if (num_modes > 0) {
      num_owned <- sum(vapply(modes, function(m) m$is_current, logical(1)))
      
      if (num_owned > 0) {
        # If they own cars, primary mode gets most trips
        for(i in 1:num_modes) {
          if(modes[[i]]$is_current) {
            initial_trips[i] <- max(1, round(10/num_owned))  # Split 10 trips among owned vehicles
          } else {
            # Alternative modes get fewer initial trips
            if(modes[[i]]$id == "public_transport") initial_trips[i] <- 3
            else if(modes[[i]]$id %in% c("car_sharing", "car_club")) initial_trips[i] <- 1
            else initial_trips[i] <- 0
          }
        }
      } else {
        # No owned vehicles - distribute more evenly
        initial_trips[1] <- 5  # First alternative gets more
        if(num_modes > 1) initial_trips[2] <- 4
        if(num_modes > 2) initial_trips[3] <- 3
        for(i in 4:num_modes) if(i <= num_modes) initial_trips[i] <- 1
      }
    }
    
    table_rows <- lapply(1:num_modes, function(i) create_allocation_row(modes[[i]], initial_trips[i]))
    slider_ids <- vapply(modes, function(m) paste0("trips_", m$id), character(1))
    
    # --- Main UI Layout ---
    tagList(
      tags$style(HTML("
      .table { max-width: 900px; margin: 0 auto; }
      .current-vehicle-row td:first-child { position: relative; }
      .current-vehicle-row td:first-child::before { 
        content: 'OWNED'; position: absolute; top: 2px; right: 2px; 
        background-color: #ffd700; color: #333; padding: 2px 6px; 
        border-radius: 0 0 0 4px; font-size: 0.7em; font-weight: bold; 
      }
      .current-vehicle-row td { background-color: #fffef7 !important; }
      .slider-container { position: relative; padding: 15px 0; }
      .slider-label-left, .slider-label-right { 
        position: absolute; top: 0; font-size: 0.75em; color: #666; 
      }
      .slider-label-left { left: 0; } 
      .slider-label-right { right: 0; }
      .allocation-cell { display: flex; align-items: center; }
      .allocation-cell .form-group { flex-grow: 1; margin: 0; }
      .trips-value-display { 
        font-weight: bold; font-size: 1em; margin-left: 12px; 
        width: 60px; text-align: right; color: #007bff;
      }
      .combined-info-box { 
        margin: 0 0 25px 0; padding: 18px; background-color: #f8f9fa; 
        border-left: 5px solid #007bff; border-radius: 4px; max-width: 900px; margin-left: auto; margin-right: auto;
      }
      .scenario-section { margin-bottom: 15px; }
      .summary-section h5 { margin-top: 12px; }
      .total-ok { color: #155724; }
      .total-error { color: #721c24; }
    ")),
    
    tags$script(HTML(paste0("
      (function() {
        const slider_ids = ", jsonlite::toJSON(slider_ids), ";
        
        function updateAllDisplays() {
          let total_trips = 0;
          let total_cost = 0;
          
          for (const id of slider_ids) {
            const slider = $('#' + id);
            if (!slider.length) continue;
            
            const val = Number(slider.val());
            total_trips += val;
            
            // Cost calculation
            const row = slider.closest('tr');
            let cost = 0;
            if (row.data('dynamic-cost') === true) {
              const cost_text = $('#dynamic_cost_display').text();
              cost = parseFloat(cost_text.replace(/[^0-9.-]+/g, '')) || 0;
            } else {
              cost = Number(row.data('cost')) || 0;
            }
            
            // Cost calculation: assume cost is monthly, convert to per-trip basis
            if (!isNaN(cost) && val > 0) {
              // Assume monthly cost covers ~40 trips, so cost per trip = monthly_cost/40
              // Weekly cost = trips_per_week * cost_per_trip * (52/12) to get monthly equivalent
              total_cost += cost * (val / 40) * (52/12);
            }
            
            slider.closest('.allocation-cell').find('.trips-value-display')
              .text(val + (val === 1 ? ' trip' : ' trips'));
          }
          
          const summary_span = $('#trips_summary_val');
          const cost_span = $('#cost_summary_val');
          
          summary_span.text(total_trips + (total_trips === 1 ? ' trip' : ' trips'));
          cost_span.text('£' + total_cost.toFixed(0));
          
          // Enable button if reasonable number of trips
          if (total_trips >= 5 && total_trips <= 50) {
            summary_span.removeClass('total-error').addClass('total-ok');
            shinyjs.enable('to_sa_button4');
          } else {
            summary_span.removeClass('total-ok').addClass('total-error');
            shinyjs.disable('to_sa_button4');
          }
        }
        
        $(document).on('shiny:inputchanged', function(event) {
          if (slider_ids.includes(event.name) || event.name.startsWith('sa_alt_1_replace')) {
            setTimeout(updateAllDisplays, 50);
          }
        });
        
        $(document).on('shiny:value', function(event) {
          if (event.target.id === 'sa_ui_placeholder4') {
            setTimeout(updateAllDisplays, 150);
          }
        });
      })();
    "))),
    
    # --- COMBINED: Instruction box with scenario and summary info ---
    div(class = "combined-info-box",
        tags$h4("In this scenario, how would your household travel in the next 3 years?"),
        
        fluidRow(
          column(width = 6,
                 div(class = "scenario-section",
                     h5("Leeds 2030 Scenario:"),
                     tags$table(class="table table-sm table-borderless", style="margin-bottom: 0;",
                                tags$tbody(
                                  tags$tr(tags$td(icon("bus"), strong("Mobility network:")), 
                                          tags$td("Bus/tram/train in Leeds runs 24/7 all week")),
                                  tags$tr(tags$td(icon("check-circle"), strong("Car club:")), 
                                          tags$td("A car-club is organised in your neighbourhood."))
                                )
                     )
                 )
          ),
          column(width = 6,
                 div(class = "summary-section",
                     h5("Your Travel Summary"),
                     div("Total Weekly Trips:", tags$span(id="trips_summary_val", "0 trips")),
                     div(style="margin-top: 8px; font-weight: bold;", "Est. Monthly Cost:", tags$span(id="cost_summary_val", "£0"))
                 )
          )
        )
    ),
    
    # --- Narrower table ---
    tags$div(style = "overflow-x: auto;",
             tags$table(class = "table choice-table table-bordered",
                        tags$thead(tags$tr(
                          tags$th("Mode Option"), 
                          tags$th("Access"), 
                          tags$th("Availability"), 
                          tags$th("Cost"), 
                          tags$th("Trips per Week")
                        )),
                        tags$tbody(table_rows)
             )
    ),
    
    tags$div(style = "margin-top: 25px; text-align: center;",
             actionButton("to_sa_button4", "Continue to Next Section", class = "btn btn-success btn-lg")
    )
    )
  })
  
  # ========= VERSION 3: Menu Builder (REFACTORED) =========
  
  
  # --- trip purposes used by the SA UI and compute_estimates ---
  if (!exists("trip_purposes")) {
    trip_purposes <- c(
      "How many trips to work would you take using this mode?",
      "Leisure/visiting family and friends, recreation, etc."
    )
  }
  
  
  # ========= MULTI-OWNED-VEHICLES: modes_reactive (server) =========
  modes_reactive <- reactive({
    make_val <- function(id, fallback = "Not provided") {
      v <- input[[id]]
      if (is.null(v) || v == "") fallback else v
    }
    
    # --- THIS IS THE CORRECTED FUNCTION ---
    create_vehicle_desc_full <- function(i) {
      body_type <- make_val(paste0("car", i, "_body_type"))
      fuel      <- make_val(paste0("car", i, "_fuel"))
      age_year  <- make_val(paste0("car", i, "_age_year"))
      parking   <- make_val(paste0("car", i, "_parking"))
      
      paste0(
        "Body Type: ", body_type, "; Fuel: ", fuel, "; ",
        "Age: ", age_year, "; Parking: ", parking
      )
    }
    # --- END OF CORRECTION ---
    
    modes_list <- list()
    num_cars <- as.integer(gsub("\\+", "", input$num_cars))
    if (is.na(num_cars)) num_cars <- 0
    num_cars <- min(num_cars, 4L)
    
    if (num_cars > 0) {
      for (i in seq_len(num_cars)) {
        # --- PIPING RP COST ---
        cost_input_id <- paste0("car", i, "_cost_rp")
        cost_val_numeric <- parse_cost_bracket(input[[cost_input_id]])
        cost_text <- if (is.na(cost_val_numeric)) "Not Provided" else paste0("£", round(cost_val_numeric))
        
        modes_list[[length(modes_list) + 1]] <- list(
          id = paste0("current_", i),
          title = "Your currently owned vehicle",
          icon = "car-side",
          is_current = TRUE,
          access = "Immediate, 24/7 at home",
          availability = "Very High (99% success rate)",
          vehicle_index = i,
          cost_val = cost_val_numeric,
          cost_text = cost_text, # For display
          description = create_vehicle_desc_full(i)
        )
      }
    }
    
    modes_list[[length(modes_list) + 1]] <- list(
      id = "public_transport",
      title = "Yorkshire Pass",
      icon = "bus-alt",
      is_current = FALSE,
      description = "Monthly pass for all local public transport (bus, tram, train).",
      access = "Within a 5 min walk",
      availability = "High (95% success rate)",
      cost_val = 75,
      cost_text = "£75"
    )
    
    modes_list
  })
  
  # helper: map radio/select levels to numeric prop
  trip_level_map <- c(
    "None" = 0,
    "For a few trips" = 0.25,
    "For around half" = 0.5,
    "For most trips" = 0.75,
    "For all trips" = 1
  )
  
  # compute initial PT share per purpose from existing inputs if present (0-1)
  initial_pt_share_by_purpose <- function() {
    modes_local <- NULL
    # try to access existing pt radioButtons if present; if not, fall back to defaults
    res <- numeric(length(trip_purposes))
    for (pj in seq_along(trip_purposes)) {
      clean_purpose <- gsub("[^A-Za-z0-9]", "_", trip_purposes[pj])
      sel <- NULL
      try(sel <- isolate(input[[paste0("pt_trips_", clean_purpose)]]), silent = TRUE)
      if (!is.null(sel) && sel %in% names(trip_level_map)) {
        res[pj] <- trip_level_map[[sel]]
      } else {
        # sensible defaults: some PT for work, little for leisure
        res[pj] <- if (grepl("work", trip_purposes[pj], ignore.case = TRUE)) 0.25 else 0
      }
    }
    res
  }
  
  # Render UI for sa_ui_placeholder5
  # ********** Drop-in replacement: sa_ui_placeholder5 with styled boxes **********
  output$sa_ui_placeholder5 <- renderUI({
    req(input$num_cars)
    modes <- modes_reactive()
    if (is.null(modes) || length(modes) == 0) return(tags$div("No modes available"))
    
    num_modes <- length(modes)
    pt_mode <- modes[[num_modes]]
    owned_modes <- if (num_modes > 1) modes[1:(num_modes - 1)] else list()
    n_owned <- length(owned_modes)
    
    # compute initial slider values (0-100) from existing inputs where possible
    initial_pt_props <- initial_pt_share_by_purpose()
    initial_vals <- round(100 * initial_pt_props)
    
    tagList(
      tags$style(HTML("
      /* layout */
      .sa5-layout { display:flex; gap:18px; align-items:flex-start; flex-wrap:wrap; }
      .sa5-box { flex: 1 1 320px; border-radius:8px; padding:14px; box-shadow: 0 2px 8px rgba(0,0,0,0.06); background: #fff; border:1px solid #e9ecef; min-width:260px; }
      .sa5-box .title { font-weight:700; margin-bottom:8px; display:flex; align-items:center; gap:8px; }
      .sa5-center { flex: 0 0 220px; display:flex; flex-direction:column; gap:10px; align-items:center; justify-content:center; }
      .sa5-slider-wrap { width:100%; padding:6px 8px; border-radius:6px; background:#f8f9fa; border:1px solid #e9ecef; }
      .sa5-vehicle-list { font-size:0.9em; color:#343a40; line-height:1.3; }
      @media (max-width:880px) { .sa5-layout { flex-direction:column; } .sa5-center { width:100%; } }

      /* Scenario box: same style as combined-info-box in sa_ui_placeholder3 */
      .combined-info-box { padding:14px; background: linear-gradient(135deg,#e3f2fd 0%,#f8f9fa 100%); border-left:5px solid #007bff; border-radius:8px; box-shadow:0 2px 8px rgba(0,0,0,0.06); }

      /* summary pill (same look) */
      .summary-pill { display:flex; justify-content:space-between; align-items:center; padding:10px 12px; border-radius:8px; margin-bottom:10px; box-shadow:0 1px 3px rgba(0,0,0,0.06); }
      .summary-pill .label { font-weight:700; color:#495057; font-size:0.85em; }
      .summary-pill .value { font-size:14px; font-weight:700; }

      /* Owned vehicles box colours (match owned-col styles) */
      .owned-style { background: linear-gradient(135deg,#eafaf1 0%,#e8f8ee 100%); border:1px solid #d4edda; }
      .owned-style .title { background: linear-gradient(135deg,#28a745 0%,#1e7e34 100%); color:white; padding:6px 8px; border-radius:6px; display:inline-flex; align-items:center; }

      /* PT box colours (match pt-col styles) */
      .pt-style { background: linear-gradient(135deg,#fff9e6 0%,#fff6e0 100%); border:1px solid #ffeeba; }
      .pt-style .title { background: linear-gradient(135deg,#ffc107 0%,#d39e00 100%); color:#212529; padding:6px 8px; border-radius:6px; display:inline-flex; align-items:center; }
    ")),
    
    # Top scenario+summary box (styled like combined-info-box)
    div(class = "sa5-box combined-info-box", style = "margin-bottom:12px;",
        div(class = "title", icon("info-circle"), "Scenario: Public transport runs 24/7"),
        div(style="display:flex; gap:12px; flex-wrap:wrap;",
            div(style="flex:1; min-width:160px;",
                div(class="summary-pill", style="background: linear-gradient(90deg,#e9f7ef,#fff); border:1px solid #d4edda;",
                    div(class="label", "Work trips"), div(class="value", textOutput("sa5_work_summary", inline = TRUE))
                )
            ),
            div(style="flex:1; min-width:160px;",
                div(class="summary-pill", style="background: linear-gradient(90deg,#fff4e6,#fff); border:1px solid #ffeeba;",
                    div(class="label", "Leisure trips"), div(class="value", textOutput("sa5_leisure_summary", inline = TRUE))
                )
            ),
            div(style="flex:1; min-width:160px;",
                div(class="summary-pill", style="background: linear-gradient(90deg,#fff,#fff); border:1px solid #dee2e6;",
                    div(class="label", "Est. monthly cost"), div(class="value", textOutput("sa5_cost_summary", inline = TRUE))
                )
            )
        ),
        hr(),
        p("Instructions: Please consider all your vehicles and the alternative travel option. Use the sliders to allocate the share of trips that would go to public transport for each trip purpose. The remainder is assigned to your currently owned vehicles (distributed equally).",
          style = "margin-top:8px; font-size:0.92em; color:#495057;")
    ),
    
    # main 3-column layout: Owned (left) - Sliders (centre) - PT (right)
    div(class = "sa5-layout",
        # LEFT: Owned vehicles box (styled)
        div(class = "sa5-box owned-style",
            div(class = "title", icon("car"), "Your currently owned vehicle(s)"),
            if (n_owned == 0) {
              p("No owned vehicles configured.", style = "color:#6c757d;")
            } else {
              tags$div(class = "sa5-vehicle-list",
                       lapply(seq_along(owned_modes), function(ii) {
                         m <- owned_modes[[ii]]
                         div(style = "margin-bottom:8px;",
                             tags$strong(paste0("Vehicle ", ii)), tags$br(),
                             tags$span(m$description)
                         )
                       })
              )
            }
        ),
        
        # CENTRE: sliders (unchanged functionality)
        div(class = "sa5-center",
            lapply(seq_along(trip_purposes), function(pj) {
              clean_purpose <- gsub("[^A-Za-z0-9]", "_", trip_purposes[pj])
              sliderId <- paste0("sa5_pt_share_", clean_purpose)
              div(class = "sa5-slider-wrap", style = "width:100%;",
                  tags$div(style="font-weight:700; margin-bottom:6px; text-align:center;", trip_purposes[pj]),
                  sliderInput(sliderId, label = NULL, min = 0, max = 100, value = initial_vals[pj], step = 1, post = "%")
              )
            })
        ),
        
        # RIGHT: Public Transport box (styled)
        div(class = "sa5-box pt-style",
            div(class = "title", icon("bus"), "Public transport"),
            tags$div(style = "font-size:0.95em; color:#343a40;", pt_mode$description),
            tags$div(style = "margin-top:8px; color:#6c757d;", 
                     tags$small("Access:"), " ", pt_mode$access, br(),
                     tags$small("Availability:"), " ", pt_mode$availability, br(),
                     tags$small("Monthly cost:"), " ", paste0("£", pt_mode$cost_val)
            )
        )
    )
    )
  })
  
  
  # Compute amounts for sa_ui_placeholder5 from sliders
  # Compute amounts for sa_ui_placeholder5 from sliders (reactive to sliders)
  compute_estimates_sa5 <- reactive({
    modes_local <- modes_reactive()
    if (is.null(modes_local) || length(modes_local) == 0) {
      return(list(cost = "£0", work_text = "No modes configured", leisure_text = "No modes configured"))
    }
    num_modes <- length(modes_local)
    pt_mode <- modes_local[[num_modes]]
    owned_modes <- if (num_modes > 1) modes_local[1:(num_modes - 1)] else list()
    n_owned <- length(owned_modes)
    
    # read slider values (0-1) — no isolate so reactive dependencies are registered
    pt_shares <- sapply(seq_along(trip_purposes), function(pj) {
      clean_purpose <- gsub("[^A-Za-z0-9]", "_", trip_purposes[pj])
      sid <- paste0("sa5_pt_share_", clean_purpose)
      val <- input[[sid]]
      if (is.null(val)) {
        # fallback defaults (used only if slider not present)
        if (grepl("work", trip_purposes[pj], ignore.case = TRUE)) return(0.25)
        return(0)
      }
      as.numeric(val) / 100
    })
    
    # For each purpose, owned total share = 1 - pt_share.
    # Distribute equally across owned vehicles (if any)
    owned_props <- if (n_owned > 0) matrix(0, nrow = n_owned, ncol = length(trip_purposes)) else matrix(numeric(0), nrow = 0, ncol = length(trip_purposes))
    if (n_owned > 0) {
      for (pj in seq_along(trip_purposes)) {
        remaining <- 1 - pt_shares[pj]
        if (remaining < 0) remaining <- 0
        per_vehicle <- remaining / n_owned
        owned_props[, pj] <- rep(per_vehicle, n_owned)
      }
    }
    
    # compute per-mode averages across purposes (for cost calc)
    mode_avgs <- numeric(0)
    mode_costs <- numeric(0)
    
    if (n_owned > 0) {
      for (ii in seq_len(n_owned)) {
        avg <- mean(owned_props[ii, ], na.rm = TRUE)
        mode_avgs <- c(mode_avgs, avg)
        cv <- owned_modes[[ii]]$cost_val
        if (is.null(cv)) cv <- 0
        mode_costs <- c(mode_costs, as.numeric(cv))
      }
    }
    
    # PT average across purposes
    pt_avg <- mean(pt_shares, na.rm = TRUE)
    mode_avgs <- c(mode_avgs, pt_avg)
    mode_costs <- c(mode_costs, as.numeric(pt_mode$cost_val))
    
    # monthly cost
    est_monthly_cost <- sum(mode_costs * mode_avgs, na.rm = TRUE)
    
    # per-purpose breakdown text helper (produces "Your vehicles: 82% | Public Transport: 18%")
    make_breakdown_text <- function(pt_share, owned_vec) {
      owned_total <- sum(owned_vec)
      labels <- character(0); vals <- numeric(0)
      if (owned_total > 0) { labels <- c(labels, "Your vehicles"); vals <- c(vals, owned_total) }
      if (!is.na(pt_share) && pt_share > 0) { labels <- c(labels, "Public Transport"); vals <- c(vals, pt_share) }
      if (length(vals) == 0) return("No trips selected")
      pct <- round(100 * vals / sum(vals))
      paste0(paste0(labels, ": ", pct, "%"), collapse = " | ")
    }
    
    # work and leisure text (assumes trip_purposes[1] = work, [2] = leisure)
    work_props_vec_owned <- if (n_owned > 0) owned_props[, 1] else numeric(0)
    leisure_props_vec_owned <- if (n_owned > 0) owned_props[, 2] else numeric(0)
    
    work_text <- make_breakdown_text(pt_shares[1], work_props_vec_owned)
    leisure_text <- make_breakdown_text(pt_shares[2], leisure_props_vec_owned)
    
    list(
      cost = paste0("£", formatC(round(est_monthly_cost), format = "f", big.mark = ",", digits = 0)),
      work_text = work_text,
      leisure_text = leisure_text
    )
  })
  
  # Outputs for sa_ui_placeholder5
  output$sa5_cost_summary <- renderText({
    compute_estimates_sa5()$cost
  })
  output$sa5_work_summary <- renderText({
    compute_estimates_sa5()$work_text
  })
  output$sa5_leisure_summary <- renderText({
    compute_estimates_sa5()$leisure_text
  })
  
  
  
 
}