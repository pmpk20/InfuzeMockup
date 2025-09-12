# server.R (Version 14.2 - With Collapsible DCE Panels)

library(shiny)
library(shinyBS) # Ensure shinyBS is loaded
library(bslib)   # Ensure bslib is loaded
library(shinyjs)


# Define the server logic
function(input, output, session) {
  
  
  # ========= Navigation Logic =========
  # This controls the flow from one tab to the next when buttons are clicked.
  observeEvent(input$FromLandingTo1A_Button, {
    updateTabsetPanel(session, "main_tabs", selected = "Step_1A_Screeners")
  })
  observeEvent(input$From1ATo1B_Button, {
    updateTabsetPanel(session, "main_tabs", selected = "rp1_panel")
  })
  observeEvent(input$to_demographics_yours_button, {
    updateTabsetPanel(session, "main_tabs", selected = "demographics_panel_yours")
  })
  observeEvent(input$to_demographics_life_button, {
    updateTabsetPanel(session, "main_tabs", selected = "demographics_panel_life")
  })
  observeEvent(input$to_rp1_button, {
    updateTabsetPanel(session, "main_tabs", selected = "rp1_panel")
  })
  observeEvent(input$to_rp2_button, {
    updateTabsetPanel(session, "main_tabs", selected = "sa_panel_3")
  })
  
  observeEvent(input$to_sa_button, {
    updateTabsetPanel(session, "main_tabs", selected = "sa_panel_2")
  })
  observeEvent(input$to_sa_button2, {
    updateTabsetPanel(session, "main_tabs", selected = "sa_panel_2")
  })
  observeEvent(input$to_sa_button3, {
    updateTabsetPanel(session, "main_tabs", selected = "sa_panel_3")
  })
  observeEvent(input$to_sa_button4, {
    updateTabsetPanel(session, "main_tabs", selected = "sa_panel_5")
  })
  observeEvent(input$to_sa_button5, {
    updateTabsetPanel(session, "main_tabs", selected = "sa_panel_5")
  })
  observeEvent(input$to_sa_button6, {
    updateTabsetPanel(session, "main_tabs", selected = "sa_panel_6")
  })
  
  # ===================================================================
  # ========= DYNAMIC HOUSEHOLD COMPOSITION LOGIC =====================
  # ===================================================================
  
  # A reactive counter to keep track of how many members have been added
  member_counter <- reactiveVal(0)
  
  # Define choice vectors once to avoid repetition
  age_choices <- c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
  vehicle_choices <- c("None", "ICE Car/Van", "EV Car/Van", "Motorbike/Scooter", "Bicycle")
  mode_choices <- c("Car (driver)", "Car (passenger)", "Public Transport", "Walk/Cycle", "Taxi/Ride-hail", "Other")
  purpose_choices <- c("Commute to work/education", "Shopping", "Leisure/Social", "Business trip", "Other")
  freq_choices <- c("Daily", "A few times a week", "Weekly", "Monthly", "Less often")
  
  # This observer listens for the "Add" button click
  observeEvent(input$add_member_btn, {
    
    # Increment the counter
    member_counter(member_counter() + 1)
    id <- member_counter()
    
    # Define the UI for one member's row.
    # Note the use of paste0() to create unique IDs for every input and the row itself.
    ui_to_insert <- div(
      id = paste0("member_row_", id),
      style = "margin-bottom: 20px;",
      wellPanel(
        style = "border-left: 5px solid #007bff;",
        fluidRow(
          column(11, h4(paste("Household Member", id))),
          column(1, actionButton(paste0("remove_member_btn_", id), "", icon = icon("trash-alt"), class = "btn-danger btn-sm"))
        ),
        hr(style="margin-top: 5px;"),
        fluidRow(
          column(2, textInput(paste0("member_name_", id), "Name")),
          column(2, selectInput(paste0("member_age_", id), "Age", choices = age_choices)),
          column(2, selectInput(paste0("member_vehicle_", id), "Owns/Accesses", choices = vehicle_choices)),
          column(2, selectInput(paste0("member_mode_", id), "Main Travel Mode", choices = mode_choices)),
          column(2, selectInput(paste0("member_purpose_", id), "Main Travel Purpose", choices = purpose_choices)),
          column(2, selectInput(paste0("member_freq_", id), "Travel Frequency", choices = freq_choices))
        )
      )
    )
    
    # Use insertUI to add the new row to the placeholder div
    insertUI(
      selector = "#household_member_placeholder",
      where = "beforeEnd", # Adds the new UI at the end of the container
      ui = ui_to_insert
    )
    
    # Create an observer for the NEW remove button that we just added.
    # This observer will remove its own row when clicked.
    observeEvent(input[[paste0("remove_member_btn_", id)]], {
      removeUI(selector = paste0("#member_row_", id))
    }, once = TRUE, ignoreInit = TRUE, ignoreNULL = TRUE)
    
  })
  
  
  # ========= COST CALCULATIONS (Move to main server scope) =========
  
  # Current monthly cost (X) - based on Step 2 choices
  current_monthly_cost <- reactive({
    req(input$num_cars)
    
    total_cost <- 0
    
    # Basic vehicle running costs (fuel + insurance + maintenance per month)
    vehicle_monthly_cost <- function(fuel_type, mileage_bracket) {
      base_costs <- list(
        "Petrol" = 150, "Diesel" = 160,
        "Fully Electric" = 80, "Plug-in Hybrid" = 120
      )
      mileage_multiplier <- switch(mileage_bracket,
                                   "0-2,000" = 0.6, "2,001-5,000" = 1.0,
                                   "5,001 - 10,000" = 1.5, "10,001+" = 2.2 )
      return(base_costs[[fuel_type]] * mileage_multiplier)
    }
    
    if (input$num_cars != "0") {
      num_vehicles <- switch(input$num_cars, "1"=1, "2"=2, "3"=3, "4+"=4)
      for(i in 1:num_vehicles){
        if(!is.null(input[[paste0("car", i, "_fuel")]])) {
          total_cost <- total_cost + vehicle_monthly_cost(input[[paste0("car", i, "_fuel")]], input[[paste0("car", i, "_mileage")]])
        }
      }
    }
    
    pt_cost <- switch(input$pt_spend,
                      "£0" = 0, "£1-£30" = 15, "£31-£60" = 45,
                      "£61-£100" = 80, "More than £100" = 120, 0 )
    total_cost <- total_cost + pt_cost
    
    if (!is.null(input$car_share_member) && input$car_share_member == "Yes") {
      total_cost <- total_cost + 20
    }
    
    return(round(total_cost, 2))
  })
  
  # New monthly cost (Z) - based on Step 4 adaptation choices
  new_monthly_cost <- reactive({
    req(input$num_cars, input$main_tabs) # Depend on the active tab
    
    total_cost <- 0
    
    vehicle_monthly_cost_new <- function(fuel_type, mileage_bracket) {
      base_costs <- list("Petrol" = 150*1.25, "Diesel" = 160*1.25, "Fully Electric" = 80, "Plug-in Hybrid" = 120*1.15)
      parking <- switch(mileage_bracket, "0-2,000"=5, "2,001-5,000"=10, "5,001-10,000"=15, "10,001+"=20, 0)
      multiplier <- switch(mileage_bracket, "0-2,000"=0.6, "2,001-5,000"=1.0, "5,001-10,000"=1.5, "10,001+"=2.2, 1)
      return((base_costs[[fuel_type]] * multiplier) + (parking * 1.20))
    }
    
    # Logic for the four-column SA page
    if (input$main_tabs == "sa_panel") {
      if (input$num_cars == "0") {
        if(!is.null(input$sa_0_car_decision)) {
          total_cost <- switch(input$sa_0_car_decision, "add_pt"=50, "add_carshare"=25, "no_changes"=0, 0)
        }
      } else {
        num_vehicles <- switch(input$num_cars, "1"=1, "2"=2, "3"=3, "4+"=4)
        for (i in 1:num_vehicles) {
          decision <- input[[paste0("sa_car", i, "_decision")]]
          if (!is.null(decision)) {
            cost_to_add <- switch(decision,
                                  "Keep this vehicle" = { vehicle_monthly_cost_new(input[[paste0("car", i, "_fuel")]], input[[paste0("car", i, "_mileage")]]) },
                                  "Replace this vehicle" = {
                                    req(input[[paste0("sa_car", i, "_replace_fuel")]], input[[paste0("sa_car", i, "_replace_mileage")]])
                                    vehicle_monthly_cost_new(input[[paste0("sa_car", i, "_replace_fuel")]], input[[paste0("sa_car", i, "_replace_mileage")]])
                                  },
                                  "Remove and use PT" = 50,
                                  "Remove and use Car-sharing" = 25,
                                  0
            )
            total_cost <- total_cost + cost_to_add
          }
        }
      }
    } 
    else if (input$main_tabs == "sa_panel_3") {
      if (input$num_cars == "0") {
        # This part would need to be built out if zero-car users have choices on this page
      } else {
        num_vehicles <- switch(input$num_cars, "1"=1, "2"=2, "3"=3, "4+"=4)
        for (i in 1:num_vehicles) {
          current_cost_new <- vehicle_monthly_cost_new(input[[paste0("car", i, "_fuel")]], input[[paste0("car", i, "_mileage")]])
          
          if (!is.null(input[[paste0("sa3_car", i, "_keep")]]) && input[[paste0("sa3_car", i, "_keep")]]) total_cost <- total_cost + current_cost_new
          if (!is.null(input[[paste0("sa3_car", i, "_replace_ev")]]) && input[[paste0("sa3_car", i, "_replace_ev")]]) total_cost <- total_cost + 210
          if (!is.null(input[[paste0("sa3_car", i, "_use_my_days")]]) && input[[paste0("sa3_car", i, "_use_my_days")]]) total_cost <- total_cost + 75
          if (!is.null(input[[paste0("sa3_car", i, "_use_p2p")]]) && input[[paste0("sa3_car", i, "_use_p2p")]]) total_cost <- total_cost + 25
          if (!is.null(input[[paste0("sa3_car", i, "_use_pt")]]) && input[[paste0("sa3_car", i, "_use_pt")]]) total_cost <- total_cost + 50
        }
      }
    }
    # Logic for the DCE page
    else if (input$main_tabs == "sa_panel_4") {
      if (input$num_cars != "0") {
        num_vehicles <- switch(input$num_cars, "1"=1, "2"=2, "3"=3, "4+"=4)
        for (i in 1:num_vehicles) {
          decision <- input[[paste0("sa_car", i, "_decision")]]
          if (!is.null(decision)) {
            current_cost_new <- vehicle_monthly_cost_new(input[[paste0("car", i, "_fuel")]], input[[paste0("car", i, "_mileage")]])
            cost_for_this_car <- switch(decision,
                                        "Status Quo" = current_cost_new,
                                        "Modernise" = 165,
                                        "Car-Lite" = current_cost_new + 25,
                                        "Car-Free" = 120,
                                        0
            )
            total_cost <- total_cost + cost_for_this_car
          }
        }
      }
    }
    else if (input$main_tabs == "sa_panel_5") {
      
      total_cost <- 0
      
      # Define service costs once
      service_costs <- list(
        "bus_pass" = 50,
        "car_club" = 25,
        "p2p" = 75
      )
      
      # --- Zero-Car Household Logic ---
      if (input$num_cars == "0") {
        selected_services <- req(input$sa5_0_car_services)
        if ("Leeds Travel Pass" %in% selected_services) total_cost <- total_cost + service_costs$bus_pass
        if ("INFUZE Standard Car Club" %in% selected_services) total_cost <- total_cost + service_costs$car_club
        if ("INFUZE Premium Peer-to-Peer" %in% selected_services) total_cost <- total_cost + service_costs$p2p
        
      } else {
        # --- Car-Owning Household Logic ---
        num_vehicles <- switch(input$num_cars, "1"=1, "2"=2, "3"=3, "4+"=4)
        
        # Calculate costs from vehicle decisions
        for (i in 1:num_vehicles) {
          decision <- input[[paste0("sa5_car", i, "_decision")]]
          req(decision)
          
          cost_to_add <- switch(decision,
                                "Keep" = {
                                  vehicle_monthly_cost_new(input[[paste0("car", i, "_fuel")]], input[[paste0("car", i, "_mileage")]])
                                },
                                "Replace" = {
                                  req(input[[paste0("sa5_car", i, "_replace_fuel")]], input[[paste0("sa5_car", i, "_replace_mileage")]])
                                  vehicle_monthly_cost_new(input[[paste0("sa5_car", i, "_replace_fuel")]], input[[paste0("sa5_car", i, "_replace_mileage")]])
                                },
                                "Dispose" = {
                                  0 # Assuming zero cost/revenue from disposal for simplicity
                                }
          )
          total_cost <- total_cost + cost_to_add
        }
        
        # Add costs from the single, household-level service menu
        selected_services <- input$sa5_household_services
        if (!is.null(selected_services)) {
          if ("Leeds Travel Pass" %in% selected_services) total_cost <- total_cost + service_costs$bus_pass
          if ("INFUZE Standard Car Club" %in% selected_services) total_cost <- total_cost + service_costs$car_club
          if ("INFUZE Premium Peer-to-Peer" %in% selected_services) total_cost <- total_cost + service_costs$p2p
        }
      }
      
      return(round(total_cost, 2))
    }
    
    # Add other 'else if' blocks for sa_panel_2, sa_panel_3 if needed
    
    return(round(total_cost, 2))
  })
  
  # Cost Output Renderers
  output$current_monthly_formatted <- renderText({ format(current_monthly_cost(), nsmall = 2) })
  output$current_annual_formatted <- renderText({ format(current_monthly_cost() * 12, nsmall = 2) })
  output$new_monthly_formatted <- renderText({ format(new_monthly_cost(), nsmall = 2) })
  output$new_annual_formatted <- renderText({ format(new_monthly_cost() * 12, nsmall = 2) })
  output$monthly_change <- renderText({
    new <- new_monthly_cost(); current <- current_monthly_cost()
    req(new, current); if(current==0) return("")
    change <- new - current; pct <- round((change/current)*100, 1)
    sign <- if(change >= 0) "+" else ""
    paste0(" (", sign, "£", format(abs(change), nsmall=2), ", ", sign, pct, "%)")
  })
  output$annual_change <- renderText({
    new <- new_monthly_cost()*12; current <- current_monthly_cost()*12
    req(new, current); if(current==0) return("")
    change <- new - current; pct <- round((change/current)*100, 1)
    sign <- if(change >= 0) "+" else ""
    paste0(" (", sign, "£", format(abs(change), nsmall=2), ", ", sign, pct, "%)")
  })
  
  
  # ========= Version 1: Preamble =========
  # Trip purposes at server scope (used by UI and computations)
  trip_purposes <- c("Work/school", "Leisure/visiting family and friends, recreation, etc.")
  
  # modes generator as a server-level reactive so other server code can access it
  modes_reactive <- reactive({
    create_vehicle_desc <- function(car_num) {
      fuel <- input[[paste0("car", car_num, "_fuel")]]
      type <- input[[paste0("car", car_num, "_type")]]
      if (is.null(fuel) || is.null(type)) return("Unknown Vehicle")
      paste0(tolower(fuel), " ", tolower(type))
    }
    
    modes_list <- list()
    num_cars <- as.integer(gsub("\\+", "", input$num_cars))
    if (is.na(num_cars)) num_cars <- 0
    
    if (num_cars > 0) {
      for (i in 1:min(num_cars, 4)) {
        modes_list[[length(modes_list) + 1]] <- list(
          id = paste0("current_", i), title = paste("Your currently owned vehicle: a", create_vehicle_desc(i)),
          icon = "car-side", is_current = TRUE,
          access = "Immediate, 24/7 at home", availability = "Very High (99% success rate)", cost_val = 350
        )
      }
    }
    
    modes_list <- append(modes_list, list(
      list(
        id = "alt_1", title = "ADDITIONAL Vehicle", icon = "car-alt", is_current = FALSE, is_configurable = TRUE,
        description = "You would own a new vehicle", access = "Immediate, 24/7 at home", availability = "Highest (Brand New)",
        vehicle_type = "Configurable", variable_cost = "N/A", cost_val = NULL
      ),
      list(
        id = "car_sharing", title = "Peer-to-Peer Car-Sharing", icon = "users", is_current = FALSE,
        description = "Rent your neighbours' cars when they aren't using them.",
        access = "Within a 10 min walk", availability = "Medium (90% success rate - owner can cancel)",
        vehicle_type = "Varies (hatchback, van, etc.)", variable_cost = "~£6/hr", cost_val = 30 # Lower fixed, higher variable
      ),
      list(
        id = "car_club", title = "Closed Loop Car-Club", icon = "users-cog", is_current = FALSE,
        description = "Access a dedicated fleet of professionally managed vehicles.",
        access = "Within a 10 min walk", availability = "High (98% success rate - dedicated fleet)",
        vehicle_type = "Standardised Fleet (e.g. VW Golf)", variable_cost = "~£8/hr", cost_val = 60 # Higher fixed, lower variable
      ),
      list(
        id = "public_transport", title = "Yorkshire Pass: Covers All Public Transport", icon = "bus-alt", is_current = FALSE,
        description = "Monthly pass for all local public transport (bus, tram, train).",
        access = "Within a 5 min walk", availability = "High (95% success rate)",
        vehicle_type = "Public Transport", variable_cost = "None (pass covers all)", cost_val = 75
      )
    ))
    
    modes_list
  })
  
  # modes_reactive <- reactive({
  #   create_vehicle_desc <- function(car_num) {
  #     fuel <- input[[paste0("car", car_num, "_fuel")]]
  #     type <- input[[paste0("car", car_num, "_type")]]
  #     if (is.null(fuel) || is.null(type)) return("Unknown Vehicle")
  #     paste0(tolower(fuel), " ", tolower(type))
  #   }
  #   
  #   modes_list <- list()
  #   num_cars <- as.integer(gsub("\\+", "", input$num_cars))
  #   if (is.na(num_cars)) num_cars <- 0
  #   
  #   # Current vehicles
  #   if (num_cars > 0) {
  #     for (i in 1:min(num_cars, 4)) {
  #       modes_list[[length(modes_list) + 1]] <- list(
  #         id = paste0("current_", i),
  #         title = paste("Your currently owned vehicle: a ", create_vehicle_desc(i)),
  #         icon = "car-side",
  #         is_current = TRUE,
  #         description = "You would still own this vehicle",
  #         access = "Immediate, 24/7 at home",
  #         availability = "Very High (99% success rate)",
  #         cost_val = 350
  #       )
  #     }
  #   }
  #   
  #   # Alternative modes
  #   modes_list <- append(modes_list, list(
  #     list(
  #       id = "alt_1",
  #       title = "Add a new vehicle:",
  #       icon = "car-alt",
  #       is_current = FALSE,
  #       is_configurable = TRUE,
  #       description = "You would own a new vehicle",
  #       access = "Immediate, 24/7 at home",
  #       availability = "Highest (Brand New)",
  #       cost_val = NULL
  #     ),
  #     list(
  #       id = "car_sharing",
  #       title = "Peer-to-Peer Car-Sharing Membership",
  #       icon = "users",
  #       is_current = FALSE,
  #       description = "A peer-to-peer neighbourhood sharing model would be operating",
  #       access = "Within a 10 min walk",
  #       availability = "High (95% success rate)",
  #       cost_val = 60
  #     ),
  #     list(
  #       id = "car_club",
  #       title = "Closed Loop Car-Club Membership",
  #       icon = "users",
  #       is_current = FALSE,
  #       description = "You would have a membership to Enterprise car-club",
  #       access = "Within a 10 min walk",
  #       availability = "High (95% success rate)",
  #       cost_val = 60
  #     ),
  #     list(
  #       id = "public_transport",
  #       title = "Yorkshire Pass: Covers All Public Transport",
  #       icon = "bus-alt",
  #       is_current = FALSE,
  #       description = "You would have a monthly pass covering all local public transport (bus, tram, train)?",
  #       access = "Within a 5 min walk",
  #       availability = "Medium (90% success rate)",
  #       cost_val = 75
  #     )
  #   ))
  #   
  #   modes_list
  # })
  
  
  # ========= Version 1: Render (Corrected Row-by-Row Build) =========
  # ========= Version 1: Render (With Enhanced Content & Styling) =========
  output$sa_ui_placeholder3 <- renderUI({
    req(input$num_cars)
    
    # --- MODIFIED: modes_reactive now includes richer attributes ---
    # We call it directly here to use the enhanced data.
    modes <- modes_reactive()
    if (is.null(modes) || length(modes) == 0) return(tags$div("No modes available"))
    
    # Separate modes for easier logic
    current_vehicles <- Filter(function(m) isTRUE(m$is_current), modes)
    alternatives <- Filter(function(m) !isTRUE(m$is_current), modes)
    new_car_config <- alternatives[[1]] 
    other_alts <- alternatives[-1]
    
    # --- UI LAYOUT ---
    tagList(
      # --- CSS for layout, sticky summary, and new PT colour ---
      tags$style(HTML("
        .sp-layout-container { display: flex; flex-wrap: wrap; align-items: flex-start; gap: 20px; }
        .sp-table-wrapper { flex: 2; min-width: 0; }
        .sp-summary-wrapper { flex: 1; }
        .sticky-summary { position: -webkit-sticky; position: sticky; top: 20px; z-index: 20; }
        .table { table-layout:fixed; border-collapse: separate; border-spacing: 0; }
        .table th, .table td { padding:8px 6px; word-break:break-word; vertical-align: middle; border: 1px solid #ddd; border-top-width: 0; border-left-width: 0; }
        .table th:first-child, .table td:first-child { border-left-width: 1px; }
        .table tr:first-child th, .table tr:first-child td { border-top-width: 1px; }
        
        /* Colouring for different column types */
        .owned-col-header { background: linear-gradient(135deg,#28a745 0%,#1e7e34 100%) !important; color:white; text-align:center; position:relative; }
        .owned-col-cell { background-color:#eafaf1; }
        .new-car-col-header { background: linear-gradient(135deg,#6c757d 0%,#343a40 100%) !important; color:white; text-align:center; }
        .alt-col-header { background: linear-gradient(135deg,#007bff 0%,#0056b3 100%) !important; color:white; text-align:center; }
        .alt-col-cell { background-color:#f8f9fa; }
        .pt-col-header { background: linear-gradient(135deg, #ffc107 0%, #d39e00 100%) !important; color: #212529; text-align:center; }
        .pt-col-cell { background-color: #fff9e6; }

        .combined-info-box { padding:14px; background: linear-gradient(135deg,#e3f2fd 0%,#f8f9fa 100%); border-left:5px solid #007bff; border-radius:8px; box-shadow:0 2px 8px rgba(0,0,0,0.06); }
        .summary-pill { display:flex; justify-content:space-between; align-items:center; padding:10px 12px; border-radius:8px; margin-bottom:10px; box-shadow:0 1px 3px rgba(0,0,0,0.06); }
        .summary-pill .label { font-weight:700; color:#495057; font-size:0.85em; }
        .summary-pill .value { font-size:14px; font-weight:700; }
        @media (max-width: 991px) {
          .sp-layout-container { flex-direction: column; }
          .sp-table-wrapper, .sp-summary-wrapper { flex: 1 1 100%; width: 100%; }
          .sticky-summary { position: relative; top: auto; }
        }
      ")),
      
      div(class = "sp-layout-container",
          div(class = "sp-table-wrapper",
              h4("Instructions: First, decide the future of your current vehicle(s). Then, allocate your household's weekly trips across the available options."),
              tags$table(class = "table",
                         
                         tags$thead(
                           tags$tr(
                             tags$th("Attribute", style = "width:15%;"),
                             lapply(seq_along(current_vehicles), function(i) {
                               tags$th(class = "owned-col-header",
                                       tags$div(style = "display:flex; align-items:center; justify-content:center;", icon(current_vehicles[[i]]$icon), tags$strong(current_vehicles[[i]]$title, style = "font-size:0.9em; margin-left:6px;")),
                                       tags$div(style = "position:absolute; top:2px; right:2px; background:#ffd700; color:#333; padding:1px 4px; border-radius:3px; font-size:0.6em; font-weight:700;", "OWNED")
                               )
                             }),
                             conditionalPanel(
                               condition = "input.v1_plan == 'Replace'",
                               tags$th(class="new-car-col-header", uiOutput("new_vehicle_header"))
                             ),
                             lapply(other_alts, function(alt) {
                               header_class <- if(alt$id == "public_transport") "pt-col-header" else "alt-col-header"
                               tags$th(class=header_class,
                                       tags$div(style = "display:flex; align-items:center; justify-content:center;", icon(alt$icon), tags$strong(alt$title, style = "font-size:0.9em; margin-left:6px;"))
                               )
                             })
                           )
                         ),
                         
                         tags$tbody(
                           tags$tr(
                             tags$th("Decision / Description"),
                             lapply(seq_along(current_vehicles), function(i) {
                               tags$td(class="owned-col-cell", radioButtons(paste0("v", i, "_plan"), NULL, choices = c("Keep", "Replace", "Dispose"), selected = "Keep"))
                             }),
                             conditionalPanel(
                               condition = "input.v1_plan == 'Replace'",
                               tags$td(class="alt-col-cell",
                                       tags$div(style = "padding:6px; border-radius:4px; background-color: rgba(0,0,0,0.04);",
                                                selectInput("new_car_type", "Type:", c("Car","Van","Motorbike"), width = "100%"),
                                                selectInput("new_car_fuel", "Fuel:", c("Petrol","Diesel","Fully Electric","Plug-in Hybrid"), width = "100%"),
                                                selectInput("new_car_mileage", "Mileage:", c("0-2,000","2,01-5,000","5,001 - 10,000","10,001+"), width = "100%")
                                       )
                               )
                             ),
                             lapply(other_alts, function(alt) {
                               cell_class <- if(alt$id == "public_transport") "pt-col-cell" else "alt-col-cell"
                               tags$td(class=cell_class, style="font-size:0.85em;", alt$description)
                             })
                           ),
                           # --- NEW ROW: Vehicle Type ---
                           tags$tr(
                             tags$th("Vehicle Type"),
                             lapply(current_vehicles, function(v) tags$td(class="owned-col-cell", style="text-align:center;", "Your own vehicle")),
                             conditionalPanel(condition = "input.v1_plan == 'Replace'", tags$td(class="alt-col-cell", style="text-align:center;", new_car_config$vehicle_type)),
                             lapply(other_alts, function(alt) {
                               cell_class <- if(alt$id == "public_transport") "pt-col-cell" else "alt-col-cell"
                               tags$td(class=cell_class, style="text-align:center; font-size:0.85em;", alt$vehicle_type)
                             })
                           ),
                           tags$tr(
                             tags$th("Access"),
                             lapply(current_vehicles, function(v) tags$td(class="owned-col-cell", style="text-align:center;", v$access)),
                             conditionalPanel(condition = "input.v1_plan == 'Replace'", tags$td(class="alt-col-cell", style="text-align:center;", new_car_config$access)),
                             lapply(other_alts, function(alt) {
                               cell_class <- if(alt$id == "public_transport") "pt-col-cell" else "alt-col-cell"
                               tags$td(class=cell_class, style="text-align:center;", alt$access)
                             })
                           ),
                           tags$tr(
                             tags$th("Availability"),
                             lapply(current_vehicles, function(v) tags$td(class="owned-col-cell", style="text-align:center;", v$availability)),
                             conditionalPanel(condition = "input.v1_plan == 'Replace'", tags$td(class="alt-col-cell", style="text-align:center;", new_car_config$availability)),
                             lapply(other_alts, function(alt) {
                               cell_class <- if(alt$id == "public_transport") "pt-col-cell" else "alt-col-cell"
                               tags$td(class=cell_class, style="text-align:center;", alt$availability)
                             })
                           ),
                           tags$tr(
                             tags$th("Fixed Monthly Cost"),
                             lapply(current_vehicles, function(v) tags$td(class="owned-col-cell", style="text-align:center;", tags$strong(paste0("£", v$cost_val)))),
                             conditionalPanel(condition = "input.v1_plan == 'Replace'", tags$td(class="alt-col-cell", style="text-align:center;", textOutput("dynamic_price_alt_1", inline = TRUE))),
                             lapply(other_alts, function(alt) {
                               cell_class <- if(alt$id == "public_transport") "pt-col-cell" else "alt-col-cell"
                               tags$td(class=cell_class, style="text-align:center;", tags$strong(paste0("£", alt$cost_val)))
                             })
                           ),
                           # --- NEW ROW: Variable Cost ---
                           tags$tr(
                             tags$th("Variable Cost (per hour)"),
                             lapply(current_vehicles, function(v) tags$td(class="owned-col-cell", style="text-align:center;", "N/A")),
                             conditionalPanel(condition = "input.v1_plan == 'Replace'", tags$td(class="alt-col-cell", style="text-align:center;", "N/A")),
                             lapply(other_alts, function(alt) {
                               cell_class <- if(alt$id == "public_transport") "pt-col-cell" else "alt-col-cell"
                               tags$td(class=cell_class, style="text-align:center; font-size:0.85em;", alt$variable_cost)
                             })
                           ),
                           lapply(trip_purposes, function(purpose) {
                             tags$tr(
                               tags$th(purpose, style="font-weight:normal; font-style:italic;"),
                               lapply(seq_along(current_vehicles), function(i) {
                                 default_val <- if (purpose == "Work/school") "For most trips" else "None"
                                 tags$td(class="owned-col-cell", radioButtons(paste0("v", i, "_trips_", gsub("[^A-Za-z0-9]", "_", purpose)), NULL, c("None","For a few trips","For around half","For most trips","For all trips"), selected = default_val))
                               }),
                               conditionalPanel(condition = "input.v1_plan == 'Replace'",
                                                tags$td(class="alt-col-cell", radioButtons(paste0("new_car_trips_", gsub("[^A-Za-z0-9]", "_", purpose)), NULL, c("None","For a few trips","For around half","For most trips","For all trips"), selected = "None"))
                               ),
                               lapply(other_alts, function(alt) {
                                 default_val <- if (purpose == "Work/school" && identical(alt$id, "public_transport")) "For a few trips" else "None"
                                 cell_class <- if(alt$id == "public_transport") "pt-col-cell" else "alt-col-cell"
                                 tags$td(class=cell_class, radioButtons(paste0(alt$id, "_trips_", gsub("[^A-Za-z0-9]", "_", purpose)), NULL, c("None","For a few trips","For around half","For most trips","For all trips"), selected = default_val))
                               })
                             )
                           })
                         )
              ),
              actionButton("to_sa_button4", "Continue", class = "btn-primary btn-lg", style="margin-top:20px;")
          ),
          
          div(class = "sp-summary-wrapper",
              div(class = "sticky-summary",
                  div(class = "combined-info-box",
                      h5("Your Travel Summary", style = "margin-top:0; margin-bottom:15px;"),
                      div(class = "summary-pill", style = "background: linear-gradient(90deg,#e9ecef,#fff); border:1px solid #ced4da;",
                          div(class = "label", "Scenario:"),
                          div(class = "value", style="font-size: 12px; text-align: right; font-weight: normal;", "A car-club is organised in your neighbourhood.")
                      ),
                      div(class = "summary-pill", style = "background: linear-gradient(90deg,#e9f7ef,#fff); border:1px solid #d4edda;",
                          div(class = "label", "Trips"),
                          div(class = "value", textOutput("trips_summary", inline = TRUE))
                      ),
                      div(class = "summary-pill", style = "background: linear-gradient(90deg,#fff4e6,#fff); border:1px solid #ffeeba;",
                          div(class = "label", "Est. Monthly Cost"),
                          div(class = "value", textOutput("cost_summary_val", inline = TRUE))
                      ),
                      tags$div(style = "font-size:0.8em; color:#6c757d; margin-top:6px;", "Summary updates live as you change selections.")
                  )
              )
          )
      )
    )
  })
  
  # --- Server-side logic for the dynamic header ---
  output$new_vehicle_header <- renderUI({
    plan <- if(is.null(input$v1_plan)) "Keep" else input$v1_plan
    
    header_text <- if(plan == 'Replace') {
      "Your REPLACEMENT Vehicle"
    } else { 
      "ADDITIONAL Vehicle" # Fallback text
    }
    
    tags$div(style = "display:flex; align-items:center; justify-content:center;",
             icon("car-alt", class = "fa-sm", style = "margin-right:6px;"),
             tags$strong(header_text, style = "font-size:0.9em;")
    )
  })
  
  # ---- Cost / trips computation & outputs (server-level) ----
  
  # numeric mapping for radio options to share of that purpose's trips
  trip_level_map <- c(
    "None" = 0,
    "For a few trips" = 0.25,
    "For around half" = 0.5,
    "For most trips" = 0.75,
    "For all trips" = 1
  )
  
  # helper to estimate monthly cost for a mode
  estimate_mode_cost <- function(mode) { # No longer needs index 'i'
    if (!is.null(mode$cost_val)) return(as.numeric(mode$cost_val))
    
    # --- UPDATED: Read the new, non-indexed input IDs ---
    type_in <- input[["new_car_type"]]
    fuel_in <- input[["new_car_fuel"]]
    mileage_in <- input[["new_car_mileage"]]
    
    # base monthly cost by vehicle type
    base_by_type <- c("Car" = 200, "Van" = 300, "Motorbike" = 60)
    base <- if (!is.null(type_in) && type_in %in% names(base_by_type)) base_by_type[type_in] else 150
    
    # fuel multipliers
    fuel_adj <- c("Petrol" = 1.00, "Diesel" = 1.05, "Fully Electric" = 0.60, "Plug-in Hybrid" = 0.80)
    mult <- if (!is.null(fuel_in) && fuel_in %in% names(fuel_adj)) fuel_adj[fuel_in] else 1.0
    
    # simple mileage adder
    mileage_add <- if (!is.null(mileage_in)) {
      if (grepl("0-2,000", mileage_in)) 0 else if (grepl("2,001-5,000", mileage_in)) 20 else if (grepl("5,001 - 10,000", mileage_in)) 50 else 100
    } else 0
    
    round(base * mult + mileage_add)
  }
  
  # reactive to compute estimates using modes_reactive() and trip_purposes
  compute_estimates <- reactive({
    modes_local <- modes_reactive()
    if (is.null(modes_local) || length(modes_local) == 0) {
      return(list(est_cost = "£0", breakdown = numeric(0), mode_names = character(0), dynamic_costs = numeric(0)))
    }
    
    # --- ROBUST LOGIC TO PREVENT VECTOR MISMATCH ---
    
    # Separate modes for easier handling
    current_vehicles <- Filter(function(m) isTRUE(m$is_current), modes_local)
    new_car_config <- Filter(function(m) isTRUE(m$is_configurable), modes_local)[[1]]
    other_alts <- Filter(function(m) !isTRUE(m$is_current) && !isTRUE(m$is_configurable), modes_local)
    
    # Initialise lists to store results for each mode
    props <- list()
    costs <- list()
    mode_titles <- list()
    
    # 1. Process Current Vehicles
    for (i in seq_along(current_vehicles)) {
      vehicle <- current_vehicles[[i]]
      plan <- input[[paste0("v", i, "_plan")]]
      
      # Always populate all lists to maintain consistent vector lengths
      if (!is.null(plan) && plan == "Keep") {
        vals <- sapply(trip_purposes, function(purpose) {
          id <- paste0("v", i, "_trips_", gsub("[^A-Za-z0-9]", "_", purpose))
          sel <- input[[id]]
          if (is.null(sel) || !sel %in% names(trip_level_map)) return(0)
          trip_level_map[[sel]]
        })
        props[[vehicle$id]] <- mean(vals, na.rm = TRUE)
        costs[[vehicle$id]] <- vehicle$cost_val
        mode_titles[[vehicle$id]] <- vehicle$title
      } else {
        # If not "Keep", trips and cost contribution are zero
        props[[vehicle$id]] <- 0 
        costs[[vehicle$id]] <- 0
        mode_titles[[vehicle$id]] <- vehicle$title
      }
    }
    
    # 2. Process New/Replacement Vehicle
    v1_plan <- input[["v1_plan"]] # Simplified for 1 vehicle case
    if (!is.null(v1_plan) && v1_plan == "Replace") {
      vals <- sapply(trip_purposes, function(purpose) {
        id <- paste0("new_car_trips_", gsub("[^A-Za-z0-9]", "_", purpose))
        sel <- input[[id]]
        if (is.null(sel) || !sel %in% names(trip_level_map)) return(0)
        trip_level_map[[sel]]
      })
      props[[new_car_config$id]] <- mean(vals, na.rm = TRUE)
      costs[[new_car_config$id]] <- estimate_mode_cost(new_car_config) 
      mode_titles[[new_car_config$id]] <- "New/Replacement Vehicle"
    } else {
      # If not visible/active, contribution is zero
      props[[new_car_config$id]] <- 0
      costs[[new_car_config$id]] <- 0
      mode_titles[[new_car_config$id]] <- "New/Replacement Vehicle"
    }
    
    # 3. Process Other Alternatives (PT, Car Club) - this was already correct
    for (alt in other_alts) {
      vals <- sapply(trip_purposes, function(purpose) {
        id <- paste0(alt$id, "_trips_", gsub("[^A-Za-z0-9]", "_", purpose))
        sel <- input[[id]]
        if (is.null(sel) || !sel %in% names(trip_level_map)) return(0)
        trip_level_map[[sel]]
      })
      props[[alt$id]] <- mean(vals, na.rm = TRUE)
      costs[[alt$id]] <- alt$cost_val
      mode_titles[[alt$id]] <- alt$title
    }
    
    # --- Final Calculation ---
    props_vec <- unlist(props)
    costs_vec <- unlist(costs)
    titles_vec <- unlist(mode_titles)
    
    est_monthly_cost <- sum(costs_vec * props_vec, na.rm = TRUE)
    
    total_prop <- sum(props_vec)
    breakdown_pct <- if (total_prop > 0) round(100 * props_vec / total_prop) else rep(0, length(props_vec))
    
    dynamic_costs <- sapply(modes_local, function(m) {
      if (isTRUE(m$is_configurable)) estimate_mode_cost(m) else m$cost_val
    })
    
    # Filter out modes with zero trips for the summary text
    active_modes_mask <- props_vec > 0
    
    list(
      est_cost = paste0("£", formatC(round(est_monthly_cost), format = "f", big.mark = ",", digits = 0)),
      breakdown = breakdown_pct[active_modes_mask], 
      mode_names = titles_vec[active_modes_mask],
      dynamic_costs = dynamic_costs
    )
  })
  
  # render dynamic per-column price outputs (for textOutput("dynamic_price_alt_<i>"))
  observe({
    est <- compute_estimates()
    modes_local <- modes_reactive()
    req(length(modes_local) == length(est$dynamic_costs) || length(est$dynamic_costs) >= 0)
    lapply(seq_along(modes_local), function(i) {
      out_name <- paste0("dynamic_price_alt_", i)
      output[[out_name]] <- renderText({
        cost <- est$dynamic_costs[i]
        if (is.null(cost) || is.na(cost)) return("")
        paste0("£", cost)
      })
    })
  })
  
  # render the cost summary
  output$cost_summary_val <- renderText({
    compute_estimates()$est_cost
  })
  
  # render the trips breakdown summary (human-readable)
  output$trips_summary <- renderText({
    est <- compute_estimates()
    if (length(est$mode_names) == 0) return("No modes configured")
    
    df <- data.frame(mode = est$mode_names, pct = est$breakdown, stringsAsFactors = FALSE)
    df <- df[df$pct > 0, , drop = FALSE]
    if (nrow(df) == 0) return("No trips selected")
    
    df <- df[order(-df$pct), ]
    paste0(paste0(df$mode[1:min(3, nrow(df))], ": ", df$pct[1:min(3, nrow(df))], "%"), collapse = " | ")
  })
  
  
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
  output$sa_ui_placeholder5 <- renderUI({
    req(input$num_cars)
    
    # Mode data generation (simplified)
    modes <- reactive({
      create_vehicle_desc <- function(car_num) {
        fuel <- input[[paste0("car", car_num, "_fuel")]]
        type <- input[[paste0("car", car_num, "_type")]]
        if (is.null(fuel) || is.null(type)) return("Unknown Vehicle")
        paste0(tolower(fuel), " ", tolower(type))
      }
      
      modes_list <- list()
      num_cars <- as.integer(gsub("\\+", "", input$num_cars))
      if (is.na(num_cars)) num_cars <- 0
      
      # Current vehicles
      if (num_cars > 0) {
        for (i in 1:min(num_cars, 4)) {
          modes_list[[length(modes_list) + 1]] <- list(
            id = paste0("current_", i), 
            title = paste("Your currently owned vehicle: a ", create_vehicle_desc(i)), 
            icon = "car-side", 
            is_current = TRUE,
            description = "You would still own this vehicle",
            access = "Immediate, 24/7 at home", 
            availability = "Very High (99% success rate)", 
            cost_val = 350
          )
        }
      }
      
      # Alternative modes
      modes_list <- append(modes_list, list(
        list(id = "alt_1", title = "Add a new vehicle:", icon = "car-alt", is_current = FALSE,  is_configurable = TRUE, 
             description = "You would own a new vehicle", access = "Immediate, 24/7 at home", 
             availability = "Highest (Brand New)", cost_val = NULL),
        
        list(id = "car_sharing", title = "Peer-to-Peer Car-Sharing Membership", icon = "users", is_current = FALSE,
             description = "A peer-to-peer neighbourhood sharing model would be operating", 
             access = "Within a 10 min walk", availability = "High (95% success rate)", cost_val = 60),
        
        list(id = "car_club", title = "Closed Loop Car-Club Membership", icon = "users", is_current = FALSE,
             description = "You would have a membership to Enterprise car-club", 
             access = "Within a 10 min walk", availability = "High (95% success rate)", cost_val = 60),
        
        list(id = "public_transport", title = "Yorkshire Pass: Covers All Public Transport", icon = "bus-alt", is_current = FALSE,
             description = "You would have a monthly pass covering all local public transport (bus, tram, train)?", 
             access = "Within a 5 min walk", availability = "Medium (90% success rate)", cost_val = 75)
      ))
      
      return(modes_list)
    })()
    
    # Initial trip allocation
    num_modes <- length(modes)
    initial_trips_commute <- rep(0, num_modes)
    initial_trips_leisure <- rep(0, num_modes)
    
    if (num_modes > 0) {
      num_owned <- sum(vapply(modes, function(m) m$is_current, logical(1)))
      
      if (num_owned > 0) {
        for(i in 1:num_modes) {
          if(modes[[i]]$is_current) {
            initial_trips_commute[i] <- max(1, round(8/num_owned))
            initial_trips_leisure[i] <- max(1, round(6/num_owned))
          } else {
            if(modes[[i]]$id == "public_transport") {
              initial_trips_commute[i] <- 2
              initial_trips_leisure[i] <- 2
            } else if(modes[[i]]$id %in% c("car_sharing", "car_club")) {
              initial_trips_commute[i] <- 1
              initial_trips_leisure[i] <- 1
            }
          }
        }
      } else {
        initial_trips_commute[1] <- 4
        initial_trips_leisure[1] <- 3
        if(num_modes > 1) {
          initial_trips_commute[2] <- 3
          initial_trips_leisure[2] <- 2
        }
      }
    }
    
    # Generate column headers
    mode_headers <- lapply(1:num_modes, function(i) {
      mode <- modes[[i]]
      header_style <- if (mode$is_current) {
        "background: linear-gradient(135deg, #28a745 0%, #1e7e34 100%); color: white; text-align: center; position: relative;"
      } else {
        "background: linear-gradient(135deg, #007bff 0%, #0056b3 100%); color: white; text-align: center;"
      }
      
      header_content <- if (!is.null(mode$is_configurable) && mode$is_configurable) {
        tagList(
          tags$div(style = "display: flex; align-items: center; justify-content: center; margin-bottom: 8px;", 
                   icon(mode$icon, class = "fa-sm", style="margin-right: 6px;"), 
                   tags$strong(mode$title, style = "font-size: 0.9em;")),
          tags$div(style = "padding: 6px; border-radius: 4px; background-color: rgba(255,255,255,0.1);",
                   selectInput(paste0("sa_alt_1_replace_type_", i), "Type:", 
                               c("Car", "Van", "Motorbike"), width = "100%"),
                   selectInput(paste0("sa_alt_1_replace_fuel_", i), "Fuel:", 
                               c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid"), width = "100%"),
                   selectInput(paste0("sa_alt_1_replace_mileage_", i), "Mileage:", 
                               c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+"), width = "100%"))
        )
      } else {
        tags$div(style = "display: flex; align-items: center; justify-content: center;", 
                 icon(mode$icon, class = "fa-sm", style="margin-right: 6px;"), 
                 tags$strong(mode$title, style = "font-size: 0.9em;"))
      }
      
      # Add OWNED badge for current vehicles
      if (mode$is_current) {
        header_content <- tagList(
          header_content,
          tags$div(style = "position: absolute; top: 2px; right: 2px; background-color: #ffd700; color: #333; padding: 1px 4px; border-radius: 3px; font-size: 0.6em; font-weight: bold;", "OWNED")
        )
      }
      
      tags$th(style = header_style, header_content)
    })
    
    # Generate data cells for each row
    create_data_row <- function(attribute, get_cell_content) {
      row_cells <- lapply(1:num_modes, function(i) {
        mode <- modes[[i]]
        cell_style <- if (mode$is_current) {
          "text-align: center; vertical-align: middle; background-color: #d4edda; font-size: 0.85em;"
        } else {
          "text-align: center; vertical-align: middle; background-color: #f8f9fa; font-size: 0.85em;"
        }
        tags$td(style = cell_style, get_cell_content(mode, i))
      })
      
      tags$tr(
        tags$th(style = "background-color: white; color: black; font-weight: bold; text-align: left; padding: 12px;", attribute),
        do.call(tagList, row_cells)
      )
    }
    
    # Create table structure
    table_content <- tagList(
      # Header row
      tags$tr(
        tags$th(style = "background-color: white; color: black; font-weight: bold; width: 15%;", "Attribute"),
        do.call(tagList, mode_headers)
      ),
      
      # Test row
      create_data_row("Description", function(mode, i) mode$description),
      
      # Access row
      create_data_row("Access", function(mode, i) mode$access),
      
      # Availability row  
      create_data_row("Availability", function(mode, i) mode$availability),
      
      # Fixed monthly cost row
      create_data_row("Fixed Monthly Cost", function(mode, i) {
        if (is.null(mode$cost_val)) {
          tags$span(id="dynamic_cost_display", textOutput(paste0("dynamic_price_alt_", i), inline = TRUE))
        } else { 
          tags$strong(paste0("£", mode$cost_val))
        }
      }),
      
      # Commute trips row
      create_data_row("Trips per Week (Commute)", function(mode, i) {
        tags$div(class = "slider-container", style = "padding: 8px 4px;",
                 sliderInput(paste0("commute_trips_", mode$id), NULL, 
                             min = 0, max = 20, value = initial_trips_commute[i], step = 1, 
                             width = "100%"),
                 tags$span(class = "trips-value-display", style = "font-size: 0.75em; color: #007bff; font-weight: bold;",
                           paste0(initial_trips_commute[i], " trips")))
      }),
      
      # Leisure trips row
      create_data_row("Trips per Week (Leisure)", function(mode, i) {
        tags$div(class = "slider-container", style = "padding: 8px 4px;",
                 sliderInput(paste0("leisure_trips_", mode$id), NULL, 
                             min = 0, max = 20, value = initial_trips_leisure[i], step = 1, 
                             width = "100%"),
                 tags$span(class = "trips-value-display", style = "font-size: 0.75em; color: #28a745; font-weight: bold;",
                           paste0(initial_trips_leisure[i], " trips")))
      })
    )
    
    # Collect slider IDs for JavaScript
    commute_slider_ids <- vapply(modes, function(m) paste0("commute_trips_", m$id), character(1))
    leisure_slider_ids <- vapply(modes, function(m) paste0("leisure_trips_", m$id), character(1))
    all_slider_ids <- c(commute_slider_ids, leisure_slider_ids)
    
    # Main UI layout
    tagList(
      tags$style(HTML("
    .table { max-width: 1200px; margin: 0 auto; table-layout: fixed; }
    .table th, .table td { padding: 8px 4px; }
    .slider-container { position: relative; }
    .trips-value-display { display: block; text-align: center; margin-top: 4px; }
    .combined-info-box { 
      margin: 0 0 25px 0; padding: 18px; background: linear-gradient(135deg, #e3f2fd 0%, #f8f9fa 100%); 
      border-left: 5px solid #007bff; border-radius: 8px; max-width: 1200px; margin-left: auto; margin-right: auto;
      box-shadow: 0 2px 8px rgba(0,0,0,0.08);
    }
    .scenario-section { margin-bottom: 15px; }
    .summary-section h5 { margin-top: 12px; }
    .summary-pill { 
      display: flex; justify-content: space-between; align-items: center;
      padding: 12px 16px; border-radius: 8px; margin-bottom: 10px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1); 
    }
    .summary-pill .label { font-weight: 700; color: #495057; }
    .summary-pill .value { font-size: 18px; font-weight: 800; }
    .total-ok { color: #155724; }
    .total-error { color: #721c24; }
    ")),
    
    # Updated summary box matching the second example
    div(class = "combined-info-box",
        h4("In this scenario, how would your household travel in the next 3 years?"),
        fluidRow(
          column(width = 6,
                 div(class = "scenario-section",
                     h5("Leeds 2030 Scenario:"),
                     tags$table(class = "table table-sm table-borderless", style = "margin-bottom: 0;",
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
                     div(class = "summary-pill", style = "background: linear-gradient(90deg,#e9f7ef,#fff); border: 1px solid #d4edda;",
                         div(class = "label", "Total Weekly Trips"),
                         div(class = "value", tags$span(id="trips_summary_val", "0 trips"))
                     ),
                     div(class = "summary-pill", style = "background: linear-gradient(90deg,#fff4e6,#fff); border: 1px solid #ffeeba;",
                         div(class = "label", "Est. Monthly Cost"),
                         div(class = "value", tags$span(id="cost_summary_val", "£0"))
                     ),
                     div(style = "font-size: 12px; color: #6c757d; text-align: center; margin-top: 6px;",
                         tags$strong("Live update:"), " changing sliders immediately updates trips and cost."
                     )
                 )
          )
        )
    ),
    
    # Transposed table
    tags$div(style = "overflow-x: auto;",
             tags$table(class = "table table-bordered",
                        tags$thead(),
                        tags$tbody(table_content)
             )
    ),
    
    # JavaScript for updating displays
    tags$script(HTML(paste0("
    (function() {
      const slider_ids = ", jsonlite::toJSON(all_slider_ids), ";
      
      function updateAllDisplays() {
        let total_trips = 0;
        let total_cost = 0;
        
        for (const id of slider_ids) {
          const slider = $('#' + id);
          if (!slider.length) continue;
          
          const val = Number(slider.val());
          total_trips += val;
          
          // Update individual trip display
          slider.closest('.slider-container').find('.trips-value-display')
            .text(val + (val === 1 ? ' trip' : ' trips'));
        }
        
        // Calculate estimated costs (simplified)
        total_cost = total_trips * 8.5; // Rough estimate
        
        const summary_span = $('#trips_summary_val');
        const cost_span = $('#cost_summary_val');
        
        summary_span.text(total_trips + (total_trips === 1 ? ' trip' : ' trips'));
        cost_span.text('£' + Math.round(total_cost));
        
        // Enable/disable button based on trip count
        if (total_trips >= 5 && total_trips <= 50) {
          summary_span.removeClass('total-error').addClass('total-ok');
          $('#to_sa_button4').prop('disabled', false);
        } else {
          summary_span.removeClass('total-ok').addClass('total-error');
          $('#to_sa_button4').prop('disabled', true);
        }
      }
      
      $(document).on('shiny:inputchanged', function(event) {
        if (slider_ids.includes(event.name)) {
          setTimeout(updateAllDisplays, 50);
        }
      });
      
      $(document).ready(function() {
        setTimeout(updateAllDisplays, 150);
      });
    })();
    "))),
    
    tags$div(style = "margin-top: 25px; text-align: center;",
             actionButton("to_sa_button5", "Finish", class = "btn btn-success btn-lg")
    )
    )
  })
  
  
 
}