# server.R (Version 14.2 - With Collapsible DCE Panels)

library(shiny)
library(shinyBS) # Ensure shinyBS is loaded
library(bslib)   # Ensure bslib is loaded

# Define the server logic
function(input, output, session) {
  
  
  # ========= Navigation Logic =========
  # This controls the flow from one tab to the next when buttons are clicked.
  observeEvent(input$FromLandingTo1A_Button, {
    updateTabsetPanel(session, "main_tabs", selected = "Step_1A_Screeners")
  })
  observeEvent(input$From1ATo1B_Button, {
    updateTabsetPanel(session, "main_tabs", selected = "Step_1B_Employment")
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
    updateTabsetPanel(session, "main_tabs", selected = "rp2_panel")
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
    updateTabsetPanel(session, "main_tabs", selected = "sa_panel_4")
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
  
  
  # ========= VERSION 1: Old CE =========
  output$sa_ui_placeholder2 <- renderUI({
    
    req(input$num_cars)
    if (input$num_cars != "0") {
      req(input$car1_type, input$car1_fuel, input$car1_mileage)
    }
    
    main_question <- h3("How would you adapt your household's travel options?")
    
    create_vehicle_adaptation_block <- function(car_number, vehicle_type, fuel_type, mileage_val) {
      decision_id <- paste0("sa_car", car_number, "_decision")
      replace_fuel_id <- paste0("sa_car", car_number, "_replace_fuel")
      replace_mileage_id <- paste0("sa_car", car_number, "_replace_mileage")
      
      calculate_vehicle_cost <- function(fuel_type, mileage_bracket) {
        base_costs <- list("Petrol"=150*1.25, "Diesel"=160*1.25, "Fully Electric"=80, "Plug-in Hybrid"=120*1.15)
        parking <- switch(mileage_bracket, "0-2,000"=5, "2,001-5,000"=10, "5,001 - 10,000"=15, "10,001+"=20, 0)
        multiplier <- switch(mileage_bracket, "0-2,000"=0.6, "2,001-5,000"=1.0, "5,001 - 10,000"=1.5, "10,001+"=2.2, 1)
        return(round((base_costs[[fuel_type]] * multiplier) + (parking * 1.20), 0))
      }
      
      local({
        car_num <- car_number
        output[[paste0("sa_car", car_num, "_replace_cost")]] <- renderText({
          req(input[[replace_fuel_id]], input[[replace_mileage_id]])
          cost <- calculate_vehicle_cost(input[[replace_fuel_id]], input[[replace_mileage_id]])
          paste0("£", cost, "/month")
        })
      })
      
      current_cost <- calculate_vehicle_cost(fuel_type, mileage_val)
      vehicle_desc <- paste0(tolower(fuel_type), " ", tolower(vehicle_type), " driven approximately ", mileage_val, " miles per year")
      
      bsCollapse(
        id = paste0("sa_collapse_", car_number),
        open = if (car_number == 1) paste0("sa_panel_", car_number) else NULL, 
        bsCollapsePanel(
          title = paste0("Decision for Vehicle ", car_number, ": ", vehicle_desc),
          value = paste0("sa_panel_", car_number),
          style = "primary",
          wellPanel(
            style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);",
            tags$table(
              class = "table table-bordered",
              style = "width: 100%; border-collapse: collapse; margin: 10px 0; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
              tags$thead(
                tags$tr(
                  # tags$td("Alternatives"),
                  tags$th(style = "width:20%; background: linear-gradient(135deg, #dc3545 0%, #bd2130 100%); color: white; text-align: center;", 
                          "Keep this vehicle"),
                  
                  tags$th(style = "width:20%; background: linear-gradient(135deg, #28a745 0%, #1e7e34 100%); color: white; text-align: center;", 
                          strong("Replace this vehicle")),
                  
                  tags$th(style = "width:20%; background: linear-gradient(135deg, #007bff 0%, #0056b3 100%); color: white; text-align: center;", 
                          strong("Remove and use bus")),
                  tags$th(style = "width:20%; background: linear-gradient(135deg, #007bff 0%, #0056b3 100%); color: white; text-align: center;", 
                          strong("Remove and use train")),
                  
                  tags$th(style = "width:20%; background: linear-gradient(135deg, #ffc107 0%, #d39e00 100%); color: black; text-align: center;", 
                          strong("Remove and use Car-sharing"))
                  
                )
              ),
              tags$tbody(
                tags$tr(
                  # tags$td("Cost"),
                  tags$td(style="text-align: center; background-color: #f8d7da; font-weight: bold;", 
                          paste0("£", current_cost, "/month")),
                  
                  tags$td(style="text-align: center; background-color: #d4edda; font-weight: bold;", 
                          textOutput(paste0("sa_car", car_number, "_replace_cost"), inline = TRUE)),
                  tags$td(style="text-align: center; background-color: #cce7ff; font-weight: bold;", "£50/month"),
                  tags$td(style="text-align: center; background-color: #cce7ff; font-weight: bold;", "£50/month"),
                  tags$td(style="text-align: center; background-color: #fff3cd; font-weight: bold;", "£25/month")
                  
                ),
                
                tags$tr(
                  # tags$td("Availability"),
                  tags$td(style="text-align: center; background-color: #f8d7da; font-weight: bold;", "90% available - 10% garage"),
                  tags$td(style="text-align: center; background-color: #d4edda; font-weight: bold;", "95% available - 5% garage"),
                  tags$td(style="text-align: center; background-color: #cce7ff; font-weight: bold;", "Bus: 90% of services within 5 minutes"),
                  tags$td(style="text-align: center; background-color: #cce7ff; font-weight: bold;", "Train: 90% of services within 5 minutes"),
                  tags$td(style="text-align: center; background-color: #fff3cd; font-weight: bold;", "Car-sharing: 99%")
                ),
                
                tags$tr(
                  # tags$td("Access Time"),
                  tags$td(style="text-align: center; background-color: #f8d7da; font-weight: bold;", "You can access immediately"),
                  tags$td(style="text-align: center; background-color: #d4edda; font-weight: bold;", "You can access immediately"),
                  tags$td(style="text-align: center; background-color: #cce7ff; font-weight: bold;", "10 minute walk to nearest stop"),
                  tags$td(style="text-align: center; background-color: #cce7ff; font-weight: bold;", "25 minute walk to nearest station"),
                  tags$td(style="text-align: center; background-color: #fff3cd; font-weight: bold;", "10 minute walk to nearest shared car")
                ),
                
                tags$tr(
                  # tags$td("Changes"),
                  tags$td(style="background-color: #f8d7da; text-align:center; vertical-align:middle;", p("Continue using this vehicle as is.")),
                  tags$td(style = "background-color: #d4edda;",
                          selectInput(paste0("sa_car", car_number, "_replace_type"), "Type:", choices = c("Car", "Van", "Motorbike")),
                          selectInput(replace_fuel_id, "Fuel:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
                          selectInput(replace_mileage_id, "Mileage:", choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+"), selected = mileage_val)
                  ),
                  tags$td(style="background-color: #cce7ff; text-align:center; vertical-align:middle;", p("Dispose of this vehicle and get a 'Leeds Bus Travel Pass'.")),
                  tags$td(style="background-color: #cce7ff; text-align:center; vertical-align:middle;", p("Dispose of this vehicle and get a 'Leeds Train Travel Pass'.")),
                  
                  tags$td(style="background-color: #fff3cd; text-align:center; vertical-align:middle;", p("Dispose of this vehicle and join 'INFUZE_TRIAL' car-sharing."))
                ),
                
                tags$tr(
                  # tags$td("Choice"),
                  tags$td(style = "text-align: center; background-color: #fce8e8;", radioButtons(decision_id, NULL, choices = "Keep this vehicle", selected = character(0))),
                  tags$td(style = "text-align: center; background-color: #e8f5e8;", radioButtons(decision_id, NULL, choices = "Replace this vehicle", selected = character(0))),
                  tags$td(style = "text-align: center; background-color: #e6f3ff;", radioButtons(decision_id, NULL, choices = "Remove and use bus", selected = character(0))),
                  tags$td(style = "text-align: center; background-color: #e6f3ff;", radioButtons(decision_id, NULL, choices = "Remove and use train", selected = character(0))),
                  tags$td(style = "text-align: center; background-color: #fff9e6;", radioButtons(decision_id, NULL, choices = "Remove and use Car-sharing", selected = character(0)))
                )
              )
            )
          )
        )
      )
    }
    
    create_zero_car_adaptation_block <- function() {
      bsCollapse(
        id = "sa_collapse_0",
        open = "sa_panel_0",
        bsCollapsePanel(
          title = "Decision for Your Household (No Currently Owned Vehicles)",
          value = "sa_panel_0",
          style = "info",
          wellPanel(
            style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);",
            tags$table(
              class = "table table-bordered",
              style = "width: 100%; border-collapse: collapse; margin: 10px 0; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
              tags$thead(
                tags$tr(
                  tags$th(style = "width:25%; background: black; color: white; text-align: center;", strong("Attribute")),
                  tags$th(style = "width:25%; background: linear-gradient(135deg, #6c757d 0%, #495057 100%); color: white; text-align: center;", strong("No Changes")),
                  tags$th(style = "width:25%; background: linear-gradient(135deg, #007bff 0%, #0056b3 100%); color: white; text-align: center;", strong("Add Public Transport")),
                  tags$th(style = "width:25%; background: linear-gradient(135deg, #ffc107 0%, #d39e00 100%); color: black; text-align: center;", strong("Add Car-sharing"))
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$th("Cost"),
                  tags$td(style="text-align: center; font-weight: bold;", "£0/month"),
                  tags$td(style="text-align: center; font-weight: bold;", "£50/month"),
                  tags$td(style="text-align: center; font-weight: bold;", "£25/month")
                ),
                tags$tr(
                  tags$th("Change"),
                  tags$td(style="text-align: center;", p("Continue current travel patterns.")),
                  tags$td(style="text-align: center;", p("Get a 'Leeds Travel Pass'.")),
                  tags$td(style="text-align: center;", p("Join 'INFUZE_TRIAL' car-sharing."))
                ),
                tags$tr(
                  tags$th("Availability"),
                  tags$td(style="text-align: center;", "95% available - 5% garage"),
                  tags$td(style="text-align: center;", "Car: 90% + Car-sharing: 99%"),
                  tags$td(style="text-align: center;", "Car-sharing: 99%")
                ),
                tags$tr(
                  tags$th("Accessibility"),
                  tags$td(style="text-align: center;", "Immediately"),
                  tags$td(style="text-align: center;", "10 minute walk to nearest shared car"),
                  tags$td(style="text-align: center;", "10 minute walk to nearest shared car")
                ),
                tags$tr(
                  tags$th("Your preference"),
                  tags$td(style="text-align: center;", radioButtons("sa_0_car_decision", NULL, choices = c("No changes" = "no_changes"), selected = character(0))),
                  tags$td(style="text-align: center;", radioButtons("sa_0_car_decision", NULL, choices = c("Add PT" = "add_pt"), selected = character(0))),
                  tags$td(style="text-align: center;", radioButtons("sa_0_car_decision", NULL, choices = c("Add Car-sharing" = "add_carshare"), selected = character(0)))
                )
              )
            )
          )
        )
      )
    }
    
    scenario_and_costs_table <- wellPanel(
      style = "background-color: white; border: 2px solid black;",
      tags$table(
        style = "width: 100%; border-collapse: collapse;",
        tags$thead(tags$tr(
          tags$th(style="border:1px solid #ddd; padding:8px; background-color:#f2f2f2; width:50%;", "Scenario Changes"),
          tags$th(style="border:1px solid #ddd; padding:8px; background-color:#f2f2f2; width:50%;", "Cost Estimates")
        )),
        tags$tbody(
          tags$tr(
            tags$td(style="border:1px solid #ddd; padding:8px; vertical-align:top;", strong("Fuel prices: "), "+60p per litre"),
            tags$td(style="border:1px solid #ddd; padding:8px; vertical-align:top;", strong("Current monthly cost: £"), textOutput("current_monthly_formatted", inline=TRUE))
          ),
          tags$tr(
            tags$td(style="border:1px solid #ddd; padding:8px; vertical-align:top;", strong("Parking permit costs: "), "+20%"),
            tags$td(style="border:1px solid #ddd; padding:8px; vertical-align:top;", strong("Current annual cost: £"), textOutput("current_annual_formatted", inline=TRUE))
          ),
          tags$tr(
            tags$td(style="border:1px solid #ddd; padding:8px; vertical-align:top;", strong("Public Transport: "), "A new 'Leeds Travel Pass' for £50/month"),
            tags$td(style="border:1px solid #ddd; padding:8px; vertical-align:top;", strong("Estimated new monthly cost: £"), textOutput("new_monthly_formatted", inline=TRUE), textOutput("monthly_change", inline=TRUE))
          ),
          tags$tr(
            tags$td(style="border:1px solid #ddd; padding:8px; vertical-align:top;", strong("Car Club: "), "New sharing models available"),
            tags$td(style="border:1px solid #ddd; padding:8px; vertical-align:top;", strong("Estimated new annual cost: £"), textOutput("new_annual_formatted", inline=TRUE), textOutput("annual_change", inline=TRUE))
          )
        )
      )
    )
    
    ui_to_render <- list(scenario_and_costs_table, main_question)
    
    if (input$num_cars == "0") {
      ui_to_render <- append(ui_to_render, list(create_zero_car_adaptation_block()))
    } else {
      num_vehicles <- switch(input$num_cars, "1"=1, "2"=2, "3"=3, "4+"=4)
      for (i in 1:num_vehicles) {
        if (!is.null(input[[paste0("car", i, "_type")]])) {
          ui_to_render <- append(ui_to_render, list(create_vehicle_adaptation_block(i, input[[paste0("car", i, "_type")]], input[[paste0("car", i, "_fuel")]], input[[paste0("car", i, "_mileage")]])))
        }
      }
    }
    
    ui_to_render <- append(ui_to_render, 
                           list(hr(), 
                                actionButton("to_sa_button3", "Next Version", 
                                             # actionButton("to_summary_button", "See summary", 
                                             class = "btn-success btn-lg")))
    # ui_to_render <- append(ui_to_render, list(hr(), cost_box, hr(), actionButton("to_summary_button", "I have decided, see summary", class = "btn-success btn-lg")))
    
    do.call(tagList, ui_to_render)
    
  })
    
  
  
  # ========= VERSION 2: New menu =========
  # Move the dynamic price output OUTSIDE of the renderUI
  output$dynamic_price_alt_1 <- renderText({
    calculate_new_car_cost()
  })
  
  # Improve the reactive to be more robust
  calculate_new_car_cost <- reactive({
    # Default cost if no selections made
    base_cost <- 300
    
    # Get the current selections for the new car configurator
    car_type <- input[["sa_alt_1_replace_type"]]
    fuel_type <- input[["sa_alt_1_replace_fuel"]]
    mileage <- input[["sa_alt_1_replace_mileage"]]
    
    # Return base cost if any selection is missing
    if (is.null(car_type) || is.null(fuel_type) || is.null(mileage)) {
      return(paste0("£", base_cost))
    }
    
    # Cost modifiers based on selections
    type_cost <- switch(car_type,
                        "Car" = 0,
                        "Van" = 50,
                        "Motorbike" = -100,
                        0)
    
    fuel_cost <- switch(fuel_type,
                        "Petrol" = 0,
                        "Diesel" = 20,
                        "Fully Electric" = 80,
                        "Plug-in Hybrid" = 60,
                        0)
    
    mileage_cost <- switch(mileage,
                           "0-2,000" = -50,
                           "2,001-5,000" = 0,
                           "5,001 - 10,000" = 40,
                           "10,001+" = 80,
                           0)
    
    # Calculate total cost
    total_cost <- base_cost + type_cost + fuel_cost + mileage_cost
    
    # Return formatted cost
    return(paste0("£", total_cost))
  })
  
  output$sa_ui_placeholder3 <- renderUI({
    
    req(input$num_cars)
    
    create_vehicle_desc <- function(car_num) {
      fuel <- input[[paste0("car", car_num, "_fuel")]]; type <- input[[paste0("car", car_num, "_type")]]
      if (is.null(fuel) || is.null(type)) return("Unknown Vehicle")
      paste0(tolower(fuel), " ", tolower(type))
    }
    
    # --- Define all available modes with numeric costs ---
    modes <- list()
    num_cars <- as.integer(gsub("\\+", "", input$num_cars))
    if (is.na(num_cars)) num_cars <- 0
    
    if (num_cars > 0) {
      for (i in 1:min(num_cars, 4)) {
        modes[[length(modes) + 1]] <- list(id = paste0("current_", i), title = paste("Your", create_vehicle_desc(i)), icon = "car-side", is_current = TRUE,
                                           access = "Immediate, 24/7 at home", availability = "Very High (99% success rate)", cost_val = 350)
      }
    }
    
    modes[[length(modes) + 1]] <- list(id = "alt_1", title = "A New Configurable Car", icon = "car-new", is_current = FALSE, is_configurable = TRUE,
                                       access = "Immediate, 24/7 at home", availability = "Highest (Brand New)", cost_val = NULL) # Cost is dynamic
    
    modes[[length(modes) + 1]] <- list(id = "car_club", title = "Car Club Membership", icon = "cars", is_current = FALSE,
                                       access = "Within a 10 min walk", availability = "High (95% success rate)", cost_val = 60)
    
    modes[[length(modes) + 1]] <- list(id = "public_transport", title = "Public Transport Pass", icon = "bus-alt", is_current = FALSE,
                                       access = "Within a 5 min walk", availability = "Medium (90% success rate)", cost_val = 75)
    
    # --- Function to generate a single row in the allocation table ---
    create_allocation_row <- function(mode, initial_share) {
      row_class <- if (mode$is_current) "allocation-row current-vehicle-row" else "allocation-row"
      
      # Handle cost display (dynamic vs. static)
      cost_display <- if (is.null(mode$cost_val)) {
        # The dynamic cost output is wrapped in a span with a specific ID for JS to find
        tags$span(id="dynamic_cost_display", textOutput("dynamic_price_alt_1", inline = TRUE))
      } else {
        paste0("£", mode$cost_val)
      }
      
      title_cell_content <- if (!is.null(mode$is_configurable) && mode$is_configurable) {
        tags$div(
          tags$div(style = "display: flex; align-items: center; margin-bottom: 8px;", icon(mode$icon, class = "fa-2x", style="margin-right:15px;"), tags$strong(mode$title)),
          tags$div(style = "padding: 8px; border-radius: 6px; margin-top: 8px; background-color: #f8f9fa;",
                   selectInput("sa_alt_1_replace_type", label = "Type:", choices = c("Car", "Van", "Motorbike"), width = "100%"),
                   selectInput("sa_alt_1_replace_fuel", label = "Fuel:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid"), width = "100%"),
                   selectInput("sa_alt_1_replace_mileage", label = "Mileage:", choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+"), width = "100%")
          )
        )
      } else {
        tags$div(style = "display: flex; align-items: center;", icon(mode$icon, class = "fa-2x", style="margin-right:15px;"), tags$strong(mode$title))
      }
      
      tags$tr(
        class = row_class,
        # Add data attributes for JS. Note the special case for the dynamic cost row.
        `data-cost` = if(!is.null(mode$cost_val)) mode$cost_val else "",
        `data-dynamic-cost` = if(is.null(mode$cost_val)) "true" else "false",
        tags$td(style = "vertical-align: middle;", title_cell_content),
        tags$td(style = "text-align: center; vertical-align: middle; font-size: 0.9em;", mode$access),
        tags$td(style = "text-align: center; vertical-align: middle; font-size: 0.9em;", mode$availability),
        tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold; font-size: 1.1em;", cost_display),
        tags$td(style = "vertical-align: middle; min-width: 350px;",
                div(class = "slider-container",
                    span(class="slider-label-left", "I would not use at all"),
                    div(class = "allocation-cell",
                        sliderInput(paste0("share_", mode$id), label = NULL, min = 0, max = 100, value = initial_share, step = 5, width = "100%"),
                        span(class = "share-value-display", paste0(initial_share, "%"))
                    ),
                    span(class="slider-label-right", "I would only use this")
                )
        )
      )
    }
    
    num_modes <- length(modes); initial_shares <- rep(0, num_modes)
    if (num_cars > 0) {
      owned_share <- floor(70 / num_cars); other_share <- floor(30 / (num_modes - num_cars))
      for(i in 1:num_modes) { initial_shares[i] <- if(modes[[i]]$is_current) owned_share else other_share }
    } else { initial_shares <- rep(floor(100 / num_modes), num_modes) }
    initial_shares[num_modes] <- 100 - sum(initial_shares[1:(num_modes-1)])
    
    table_rows <- lapply(1:num_modes, function(i) create_allocation_row(modes[[i]], initial_shares[i]))
    slider_ids <- vapply(modes, function(m) paste0("share_", m$id), character(1))
    
    tagList(
      tags$style(HTML("
      /* --- Existing styles from before --- */
      .current-vehicle-row td:first-child { position: relative; }
      .current-vehicle-row td:first-child::before { content: 'OWNED'; position: absolute; top: 0; right: 0; background-color: #ffd700; color: #333; padding: 2px 8px; border-radius: 0 0 0 4px; font-size: 0.7em; font-weight: bold; }
      .current-vehicle-row td { background-color: #fffef7 !important; }
      .slider-container { position: relative; padding: 20px 0; }
      .slider-label-left, .slider-label-right { position: absolute; top: 0; font-size: 0.8em; color: #666; }
      .slider-label-left { left: 0; } .slider-label-right { right: 0; }
      .allocation-cell { display: flex; align-items: center; }
      .allocation-cell .form-group { flex-grow: 1; margin: 0; }
      .share-value-display { font-weight: bold; font-size: 1.2em; margin-left: 15px; width: 50px; text-align: right; }
      /* --- New styles for the summary box --- */
      .summary-box { margin-top: 20px; border: 1px solid #dee2e6; border-radius: 8px; overflow: hidden; }
      .summary-row { padding: 12px 15px; font-size: 1.1em; font-weight: bold; display: flex; justify-content: space-between; align-items: center; }
      .summary-row.total-percent { background-color: #f8f9fa; border-bottom: 1px solid #dee2e6; transition: all 0.3s ease; }
      .summary-row.total-cost { background-color: #fff; font-size: 1.2em; }
      .total-ok { color: #155724; }
      .total-error { color: #721c24; }
    ")),
    
    tags$script(HTML(paste0("
      (function() {
        const slider_ids = ", jsonlite::toJSON(slider_ids), ";
        
        function updateAllDisplays() {
          let current_sum = 0;
          let total_cost = 0;
          
          for (const id of slider_ids) {
            const slider = $('#' + id);
            if (!slider.length) continue;
            
            const val = Number(slider.val());
            current_sum += val;
            
            // --- Cost Calculation Logic ---
            const row = slider.closest('tr');
            let cost = 0;
            if (row.data('dynamic-cost') === true) {
              const cost_text = $('#dynamic_cost_display').text(); // Find the specific output span
              cost = parseFloat(cost_text.replace(/[^0-9.-]+/g, '')) || 0;
            } else {
              cost = Number(row.data('cost')) || 0;
            }
            if (!isNaN(cost)) {
              total_cost += cost * (val / 100);
            }
            
            slider.closest('.allocation-cell').find('.share-value-display').text(val + '%');
          }
          
          const summary_div = $('#allocation_summary');
          const cost_div = $('#cost_summary');
          
          summary_div.find('span').text(current_sum + '%');
          if (current_sum === 100) {
            summary_div.removeClass('total-error').addClass('total-ok');
          } else {
            summary_div.removeClass('total-ok').addClass('total-error');
          }
          
          cost_div.find('span').text('£' + total_cost.toFixed(2));
        }
        
        const handleSliderChange = (event) => {
          // ... [The existing slider adjustment logic remains the same] ...
          const source_id = event.name;
          let total = slider_ids.reduce((sum, id) => sum + Number($('#' + id).val()), 0);
          let diff = total - 100;
          if (Math.abs(diff) < 0.1) { updateAllDisplays(); return; }
          const other_sliders = slider_ids.filter(id => id !== source_id).map(id => $('#' + id));
          let total_adjustable = other_sliders.reduce((sum, s) => sum + (diff > 0 ? Number(s.val()) : 100 - Number(s.val())), 0);
          if (total_adjustable > 0) {
            for (let s of other_sliders) {
              let current_val = Number(s.val());
              let capacity = (diff > 0) ? current_val : 100 - current_val;
              s.val(current_val - ((capacity / total_adjustable) * diff));
            }
          }
          let final_total = 0;
          for(const id of slider_ids.slice(0, -1)) {
              let val = Math.round(Number($('#' + id).val()));
              $('#' + id).val(val);
              final_total += val;
          }
          $('#' + slider_ids[slider_ids.length-1]).val(100 - final_total);
          // --- End of slider logic ---
          
          updateAllDisplays();
        };
        
        $(document).on('shiny:inputchanged', function(event) {
          if (slider_ids.includes(event.name) || event.name.startsWith('sa_alt_1_replace')) {
             handleSliderChange(event); // Re-calculate for config changes too
          }
        });

        $(document).on('shiny:value', function(event) {
           if (event.target.id === 'sa_ui_placeholder3') {
              setTimeout(updateAllDisplays, 150);
           }
        });

      })();
    "))),
    
    tags$div(style = "margin: 20px 0; padding: 15px; background-color: #f8f9fa; border-left: 4px solid #007bff; border-radius: 4px;",
             tags$h4("Allocate Your Household's Trips", style = "margin-top: 0; color: #2c3e50;"),
             tags$p("Based on the new scenario (higher fuel costs, new mobility services), please allocate your household's typical monthly trips across the available modes. Use the sliders to adjust the percentage for each mode. The total must equal 100%.")
    ),
    
    tags$div(style = "margin: 20px 0; overflow-x: auto;",
             tags$table(class = "table choice-table table-bordered",
                        tags$thead(tags$tr(
                          tags$th("Mode Option"), tags$th("Access"), tags$th("Availability"),
                          tags$th("Full Cost"), tags$th("Share of Trips (%)")
                        )),
                        tags$tbody(table_rows)
             )
    ),
    
    # --- New Two-Row Summary Box ---
    div(class="summary-box",
        div(id="allocation_summary", class="summary-row total-percent total-ok", 
            "Total Allocated:", tags$span("100%")
        ),
        div(id="cost_summary", class="summary-row total-cost", 
            "Your Estimated Monthly Cost:", tags$span("£...")
        )
    ),
    
    tags$div(style = "margin: 30px 0; text-align: center;",
             actionButton("to_sa_button4", "Continue to Next Section", class = "btn btn-success btn-lg")
    )
    )
  })
  
  
  
  
  # ========= VERSION 3: CONFIGURATOR=========
  output$sa_ui_placeholder4 <- renderUI({
    
    # Gatekeeper
    req(input$num_cars)
    if (input$num_cars != "0") {
      req(input$car1_type, input$car1_fuel, input$car1_mileage)
    }
    
    # --- Helper function with corrected unique checkbox IDs ---
    create_choice_card <- function(car_number, title, choice_value_suffix, cost_text, specs_list, border_class, is_popular = FALSE) {
      
      checkbox_id <- paste0("sa3_car", car_number, "_", choice_value_suffix)
      
      tags$label(
        `for` = checkbox_id,
        style = "cursor: pointer; display: block; margin: 0;",
        div(
          class = paste("configurator-card", border_class),
          fluidRow(style="display: flex; align-items: center;",
                   column(8,
                          if (is_popular) div(class = "popular-flag", "Most Popular!"),
                          div(class = "card-header", h4(title)),
                          div(class = "card-specs",
                              lapply(names(specs_list), function(name) {
                                div(class = "spec-item",
                                    fluidRow(
                                      column(6, span(class = "spec-label", name)),
                                      column(6, span(specs_list[[name]]))
                                    )
                                )
                              })
                          )
                   ),
                   column(4, align="right",
                          div(class = "card-price", cost_text),
                          p("per month", style="margin-top: -5px; color: #666; margin-bottom: 10px;"),
                          div(class = "card-selection",
                              checkboxInput(checkbox_id, label="Select this Option")
                          )
                   )
          )
        )
      )
    }
    
    # --- Live cost calculation reactive (updated to read unique checkbox IDs) ---
    sa3_live_cost <- reactive({
      live_total <- 0
      req(input$num_cars)
      if (input$num_cars != "0") {
        num_vehicles <- switch(input$num_cars, "1"=1, "2"=2, "3"=3, "4+"=4)
        for (i in 1:num_vehicles) {
          cost_func <- function(fuel, mileage) {
            base_costs <- list("Petrol"=150*1.25, "Diesel"=160*1.25, "Fully Electric"=80, "Plug-in Hybrid"=120*1.15)
            parking <- switch(mileage, "0-2,000"=5, "2,001-5,000"=10, "5,001-10,000"=15, "10,001+"=20, 0)
            multiplier <- switch(mileage, "0-2,000"=0.6, "2,001-5,000"=1.0, "5,001-10,000"=1.5, "10,001+"=2.2, 1)
            return(round((base_costs[[fuel]] * multiplier) + (parking * 1.20), 0))
          }
          current_cost_new <- cost_func(input[[paste0("car", i, "_fuel")]], input[[paste0("car", i, "_mileage")]])
          
          # Now reads the unique IDs
          if (!is.null(input[[paste0("sa3_car", i, "_keep")]]) && input[[paste0("sa3_car", i, "_keep")]]) live_total <- live_total + current_cost_new
          if (!is.null(input[[paste0("sa3_car", i, "_replace_ev")]]) && input[[paste0("sa3_car", i, "_replace_ev")]]) live_total <- live_total + 210
          if (!is.null(input[[paste0("sa3_car", i, "_use_my_days")]]) && input[[paste0("sa3_car", i, "_use_my_days")]]) live_total <- live_total + 75
          if (!is.null(input[[paste0("sa3_car", i, "_use_p2p")]]) && input[[paste0("sa3_car", i, "_use_p2p")]]) live_total <- live_total + 25
          if (!is.null(input[[paste0("sa3_car", i, "_use_pt")]]) && input[[paste0("sa3_car", i, "_use_pt")]]) live_total <- live_total + 50
        }
      }
      return(live_total)
    })
    
    # --- UI element to render the live cost table row ---
    output$sa3_live_cost_display <- renderUI({
      tags$tr(
        style = "border-top: 0px solid #ccc;",
        tags$td(icon("money-check-dollar"), strong(" Selected Portfolio Cost: ")),
        tags$td(style="font-weight:bold; font-size: 1.1em;", paste0(" £", format(sa3_live_cost(), nsmall = 2), " per month"))
      )
    })
    
    # --- CORRECTED Scenario Header Box ---
    scenario_header <- div(style="text-align: center; margin-bottom: 40px;",
                           h2("Imagine this scenario in Leeds..."),
                           p("Consider the following changes and then configure your new household travel plan below."),
                           wellPanel(
                             style = "background-color: #f8f9fa; border: 1px solid #dee2e6; display: inline-block; text-align: left; max-width: 800px; padding: 15px;",
                             tags$table(
                               class="table table-sm", style="margin-bottom: 15px;",
                               tags$tbody(
                                 tags$tr(
                                   tags$td(icon("gas-pump"), strong(" Fuel prices:")),
                                   tags$td("+60p per litre")
                                 ),
                                 tags$tr(
                                   tags$td(icon("parking"), strong(" Parking permit costs:")),
                                   tags$td("+20%")
                                 ),
                                 tags$tr(
                                   tags$td(icon("bus"), strong(" Public Transport:")),
                                   tags$td("A new 'Leeds Travel Pass' is available for £50/month")
                                 ),
                                 tags$tr(
                                   tags$td(icon("car"), strong(" Car Club:")),
                                   tags$td("New sharing models are available, see options below.")
                                 )
                               )
                             ),
                             hr(),
                             tags$table(
                               class="table table-sm", style="margin-bottom: 0;",
                               tags$tbody(
                                 uiOutput("sa3_live_cost_display")
                               )
                             ),
                             hr()
                           )
    )
    
    # --- Main UI Layout ---
    tagList(
      tags$style(HTML("
        /* CSS is unchanged */
        .configurator-card { background-color:#fff; border:1px solid #e9ecef; border-left-width:7px; border-radius:8px; padding:20px; margin-bottom:25px; box-shadow:0 4px 8px rgba(0,0,0,0.05); transition:all .2s ease-in-out; }
        .configurator-card:hover { transform: translateY(-3px); box-shadow: 0 8px 16px rgba(0,0,0,0.1); }
        .card-border-car { border-left-color: #007bff; } .card-border-car h4 { color: #007bff; }
        .card-border-shared { border-left-color: #28a745; } .card-border-shared h4 { color: #28a745; }
        .card-border-managed { border-left-color: #17a2b8; } .card-border-managed h4 { color: #17a2b8; }
        .card-border-pt { border-left-color: #6f42c1; } .card-border-pt h4 { color: #6f42c1; }
        .popular-flag { background-color:#ffc107; color:#343a40; padding:4px 12px; border-radius:5px; font-weight:bold; font-size:.9em; display:inline-block; margin-bottom:15px; }
        .card-price { font-size:1.8em; font-weight:bold; color:#333; margin-bottom:0; }
        .card-selection .checkbox { margin-top: 5px !important; }
        .checkbox input[type=checkbox] { transform: scale(1.5); }
        .card-header h4 { margin-top:0; }
        .card-specs { margin-top:15px; font-size:.95em; }
        .spec-item { margin-bottom:8px; }
        .spec-label { font-weight:bold; }
      ")),
      
      scenario_header,
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("What is your primary goal?"),
          p("Select the option that best describes your main intention for your currently owned vehicle(s)."),
          radioButtons("sa3_initial_decision", label=NULL,
                       choices=c("Keep my current vehicle(s)",
                                 "Replace my vehicle(s) with something else",
                                 "Dispose of my vehicle(s)"))
        ),
        
        mainPanel(
          width = 9,
          h3("Please select one or more packages to build your household's new travel portfolio"),
          if (input$num_cars != "0") {
            lapply(1:switch(input$num_cars, "1"=1, "2"=2, "3"=3, "4+"=4), function(i) {
              
              current_car_cost <- reactive({
                cost_func <- function(fuel, mileage) {
                  base_costs <- list("Petrol"=150*1.25, "Diesel"=160*1.25, "Fully Electric"=80, "Plug-in Hybrid"=120*1.15)
                  parking <- switch(mileage, "0-2,000"=5, "2,001-5,000"=10, "5,001-10,000"=15, "10,001+"=20, 0)
                  multiplier <- switch(mileage, "0-2,000"=0.6, "2,001-5,000"=1.0, "5,001-10,000"=1.5, "10,001+"=2.2, 1)
                  return(round((base_costs[[fuel]] * multiplier) + (parking * 1.20), 0))
                }
                cost_func(input[[paste0("car", i, "_fuel")]], input[[paste0("car", i, "_mileage")]])
              })
              
              tagList(
                h4(paste("Options related to your", input[[paste0("car", i, "_fuel")]], input[[paste0("car", i, "_type")]])),
                # RESTORED: Full spec lists and corrected checkbox IDs
                create_choice_card(i, paste("Keep Your Current", input[[paste0("car", i, "_fuel")]], "Car"), "keep", paste0("£", current_car_cost()), 
                                   specs_list = list("Travel Time" = "No change", "Reliability" = "High", "Availability" = "Immediate, 24/7"), 
                                   border_class = "card-border-car"),
                create_choice_card(i, "Replace with a new Electric Vehicle", "replace_ev", "£210", 
                                   specs_list = list("Travel Time" = "Similar to current", "Reliability" = "Very High", "Availability" = "Immediate, 24/7"), 
                                   border_class = "card-border-car"),
                create_choice_card(i, "'My Days' Dedicated Vehicle", "use_my_days", "£75", 
                                   specs_list = list("Provider" = "Enterprise", "Reliability" = "Very High (on your days)", "Availability" = "Scheduled days only"), 
                                   border_class = "card-border-managed", is_popular=TRUE),
                create_choice_card(i, "Closed Loop Peer-to-Peer Sharing", "use_p2p", "£25", 
                                   specs_list = list("Provider" = "Neighbourhood group", "Reliability" = "Medium (depends on group)", "Availability" = "By arrangement with group"), 
                                   border_class = "card-border-shared"),
                create_choice_card(i, "Public Transport Pass", "use_pt", "£50", 
                                   specs_list = list("Travel Time" = "Varies with schedule", "Reliability" = "Medium", "Availability" = "Scheduled"), 
                                   border_class = "card-border-pt")
              )
            })
          }
        )
      ),
      hr(),
      div(align="center",
          actionButton("to_sa_button5", "Continue", class="btn-primary btn-lg")
      )
    )
  })
  # This UI placeholder now just creates the box where the text will go.
  # ========= VERSION 4 (MENU BUILDER) - CORRECTED =========
  
  output$sa_ui_placeholder5 <- renderUI({
    
    # Gatekeeper
    req(input$num_cars)
    if (input$num_cars != "0") {
      req(input$car1_type, input$car1_fuel, input$car1_mileage)
    }
    
    # Cost calculation function
    cost_func <- function(fuel, mileage) {
      base_costs <- list("Petrol"=150*1.25, "Diesel"=160*1.25, "Fully Electric"=80, "Plug-in Hybrid"=120*1.15)
      parking <- switch(mileage, "0-2,000"=5, "2,001-5,000"=10, "5,001-10,000"=15, "10,001+"=20, 0)
      multiplier <- switch(mileage, "0-2,000"=0.6, "2,001-5,000"=1.0, "5,001-10,000"=1.5, "10,001+"=2.2, 1)
      return(round((base_costs[[fuel]] * multiplier) + (parking * 1.20), 0))
    }
    
    # Attribute option creator - shows cost-quality trade-offs
    create_attribute_option <- function(option_id, title, base_price, price_modifier, modifier_text, description, is_default = FALSE, is_premium = FALSE) {
      total_price <- base_price + price_modifier
      
      div(
        class = paste("attribute-option", if(is_default) "default-option", if(is_premium) "premium-option"),
        id = option_id,
        onclick = paste0("Shiny.setInputValue('", option_id, "_clicked', Math.random())"),
        style = "cursor: pointer; margin-bottom: 8px;",
        div(class = "attribute-content",
            fluidRow(
              column(7,
                     div(style = "display: flex; align-items: center;",
                         h6(title, style = "margin: 0; flex-grow: 1;"),
                         if(is_default) span(class = "default-badge", "Standard"),
                         if(is_premium) span(class = "premium-badge", "Premium")
                     ),
                     p(description, class = "attribute-description", style = "margin: 2px 0 0 0;")
              ),
              column(3, align = "center",
                     span(class = "price-modifier", 
                          if(price_modifier > 0) paste0("+£", price_modifier) else if(price_modifier < 0) paste0("-£", abs(price_modifier)) else "£0"),
                     br(),
                     span(class = "modifier-text", modifier_text, style = "font-size: 0.8em; color: #666;")
              ),
              column(2, align = "right",
                     span(class = "total-price", paste0("£", total_price)),
                     br(),
                     span(class = "select-text", "Select", style = "font-size: 0.8em; color: #007bff;")
              )
            )
        )
      )
    }
    
    
    # Replace existing create_service_section with this version
    create_service_section <- function(service_title, service_description, base_price, service_class, options_list) {
      # safe id from title
      id <- paste0("svc_", gsub("[^A-Za-z0-9]", "", tolower(service_title)))
      header_id <- paste0(id, "_header")
      body_id   <- paste0(id, "_body")
      
      tagList(
        # clickable header (wraps the fluidRow so whole header toggles)
        tags$a(
          class = "service-toggle",
          href = paste0("#", body_id),
          `data-toggle` = "collapse",
          `aria-expanded` = "false",
          `aria-controls` = body_id,
          style = "text-decoration: none; color: inherit; display: block;",
          div(id = header_id, class = paste("service-header", service_class),
              fluidRow(
                column(8,
                       h4(service_title, style = "margin: 0;"),
                       p(service_description, style = "margin: 5px 0 0 0; opacity: 0.9;")
                ),
                column(4, align = "right",
                       span("From ", style = "color: white; opacity: 0.8;"),
                       span(paste0("£", base_price), style = "font-size: 1.3em; font-weight: bold; color: white;"),
                       br(),
                       span("per month", style = "color: white; opacity: 0.8; font-size: 0.9em;")
                )
              )
          )
        ),
        # collapsed body (closed by default)
        div(id = body_id, class = "service-options collapse",
            options_list
        )
      )
    }
    
    
    # Basket display reactive  
    output$sa4_basket_display <- renderUI({
      div(class = "cost-basket",
          h5("Selected Portfolio", style = "margin-bottom: 10px;"),
          div(id = "basket-items", style = "min-height: 50px;",
              p("Select service attributes below", style = "color: #666; font-style: italic;")
          ),
          hr(style = "margin: 10px 0;"),
          div(class = "basket-total",
              span("Monthly Total: ", style = "font-weight: bold;"),
              span(id = "basket-total-amount", "£0", style = "font-weight: bold; font-size: 1.3em; color: #28a745;")
          )
      )
    })
    
    # Main UI with sidebar layout
    tagList(
      tags$style(HTML("
      .service-toggle { cursor: pointer; }
      .service-options.collapse { transition: height .25s ease; }
      .attribute-option { 
        background: #fff; 
        border: 1px solid #e9ecef; 
        border-radius: 6px; 
        padding: 12px; 
        transition: all 0.15s ease;
        position: relative;
      }
      .attribute-option:hover { 
        border-color: #007bff; 
        box-shadow: 0 2px 8px rgba(0,123,255,0.1); 
        transform: translateY(-1px);
      }
      .attribute-option.selected { 
        border-color: #28a745; 
        background-color: #f8fff9;
        box-shadow: 0 0 0 2px rgba(40,167,69,0.2);
      }
      .attribute-option.default-option { background-color: #f8f9fa; }
      .attribute-option.premium-option { background-color: #fff8f0; }
      
      /* Improved service header colours for better contrast */
      .service-header { 
        background: linear-gradient(135deg, #495057 0%, #343a40 100%); 
        color: white; 
        padding: 15px 20px; 
        border-radius: 8px; 
        margin: 25px 0 15px 0; 
      }
      .service-header.private { background: linear-gradient(135deg, #2196F3 0%, #1565C0 100%); }
      .service-header.carclub { background: linear-gradient(135deg, #4CAF50 0%, #2E7D32 100%); }
      .service-header.p2p { background: linear-gradient(135deg, #E91E63 0%, #AD1457 100%); }
      .service-header.public { background: linear-gradient(135deg, #00BCD4 0%, #0097A7 100%); }
      .service-header.micro { background: linear-gradient(135deg, #FF9800 0%, #E65100 100%); }
      
      .service-options { 
        background: #fafbfc; 
        padding: 15px; 
        border-radius: 0 0 8px 8px; 
        margin-bottom: 20px;
        border: 1px solid #e9ecef;
        border-top: none;
      }
      
      .default-badge { 
        background: #6c757d; color: white; padding: 2px 6px; border-radius: 10px; font-size: 0.7em; margin-left: 8px;
      }
      .premium-badge { 
        background: #ffc107; color: #000; padding: 2px 6px; border-radius: 10px; font-size: 0.7em; margin-left: 8px;
      }
      .price-modifier { 
        font-weight: bold; 
        font-size: 1.1em;
      }
      .total-price { 
        font-size: 1.2em; 
        font-weight: bold; 
        color: #28a745; 
      }
      .attribute-description { 
        color: #666; 
        font-size: 0.85em; 
      }
      .select-text { 
        font-weight: bold;
      }
      
      /* Sticky sidebar styling */
      .sticky-sidebar {
        position: sticky;
        top: 20px;
        height: fit-content;
        max-height: calc(100vh - 40px);
        overflow-y: auto;
      }
      
      .scenario-info {
        background-color: #f8f9fa; 
        border: 1px solid #dee2e6; 
        padding: 20px; 
        border-radius: 8px;
        margin-bottom: 20px;
      }
      
      .portfolio-basket {
        background-color: #fff3cd; 
        border: 1px solid #ffeaa7; 
        padding: 20px; 
        border-radius: 8px;
      }
      
      .main-content {
        padding-right: 15px;
      }
      
      @media (max-width: 768px) {
        .sticky-sidebar {
          position: static;
          margin-bottom: 20px;
        }
      }
    ")),
    
    div(class = "container-fluid", style = "max-width: 1200px; margin-top: 20px;",
        fluidRow(
          # Main content area (left side)
          column(8, class = "main-content",
                 div(style="text-align: center; margin-bottom: 30px;",
                     h2("Design Your Mobility Portfolio"),
                     p("Choose your preferred service levels across different transport options. Each service shows attribute trade-offs with associated costs.")
                 ),
                 
                 # PRIVATE VEHICLE OWNERSHIP
                 create_service_section(
                   "Private Vehicle Ownership", 
                   "Own and maintain your personal vehicle(s) with different specification levels",
                   if(input$num_cars != "0") cost_func(input$car1_fuel, input$car1_mileage) else 150,
                   "private",
                   tagList(
                     if (input$num_cars != "0") {
                       lapply(1:switch(input$num_cars, "1"=1, "2"=2, "3"=3, "4+"=4), function(i) {
                         base_cost <- cost_func(input[[paste0("car", i, "_fuel")]], input[[paste0("car", i, "_mileage")]])
                         tagList(
                           h6(paste("Vehicle", i, ":", input[[paste0("car", i, "_fuel")]], input[[paste0("car", i, "_type")]]), 
                              style = "margin: 15px 0 10px 0; color: #495057;"),
                           create_attribute_option(
                             paste0("keep_current_", i), "Keep Current Vehicle", base_cost, 0, "no change",
                             "Continue with existing vehicle under new scenario conditions", is_default = TRUE
                           ),
                           create_attribute_option(
                             paste0("upgrade_insurance_", i), "Enhanced Insurance & Breakdown", base_cost, 25, "premium cover",  
                             "Full comprehensive plus European breakdown and courtesy car"
                           ),
                           create_attribute_option(
                             paste0("replace_ev_", i), "Replace with Electric Vehicle", base_cost, 60, "new EV",
                             "Brand new electric vehicle with home charging installation"
                           )
                         )
                       })
                     }
                   )
                 ),
                 
                 # CAR CLUB SERVICES
                 create_service_section(
                   "Car Club Membership",
                   "Access to shared vehicles with different availability and booking flexibility levels", 
                   35,
                   "carclub",
                   tagList(
                     h6("Availability Level", style = "margin: 0 0 10px 0; color: #495057; font-weight: bold;"),
                     create_attribute_option(
                       "carclub_basic", "Basic Access", 35, 0, "standard",
                       "Vehicle available with 24-hour advance booking, neighbourhood pods", is_default = TRUE
                     ),
                     create_attribute_option(
                       "carclub_priority", "Priority Access", 35, 25, "faster booking",
                       "2-hour advance booking, wider vehicle selection, city-wide access"
                     ),
                     create_attribute_option(
                       "carclub_guaranteed", "Guaranteed Access", 35, 45, "on-demand",
                       "Immediate booking, guaranteed availability during peak hours", is_premium = TRUE
                     ),
                     
                     br(),
                     h6("Vehicle Type", style = "margin: 15px 0 10px 0; color: #495057; font-weight: bold;"),
                     create_attribute_option(
                       "carclub_standard", "Standard Fleet", 0, 0, "economy cars",
                       "Small city cars and compact vehicles, basic specification"
                     ),
                     create_attribute_option(
                       "carclub_premium", "Premium Fleet Access", 0, 20, "better vehicles", 
                       "Mid-size vehicles, SUVs, and premium models available"
                     ),
                     create_attribute_option(
                       "carclub_specialist", "Specialist Vehicles", 0, 15, "cargo/family",
                       "Access to vans, people carriers, and cargo vehicles when needed"
                     )
                   )
                 ),
                 
                 # PEER-TO-PEER SHARING
                 create_service_section(
                   "Peer-to-Peer Car Sharing",
                   "Share vehicles within your community with different coordination and reliability levels",
                   15, 
                   "p2p",
                   tagList(
                     h6("Coordination Level", style = "margin: 0 0 10px 0; color: #495057; font-weight: bold;"),
                     create_attribute_option(
                       "p2p_informal", "Informal Neighbourhood Group", 15, 0, "basic sharing",
                       "Arrange directly with neighbours, informal scheduling", is_default = TRUE  
                     ),
                     create_attribute_option(
                       "p2p_managed", "Managed Community Scheme", 15, 20, "coordinated",
                       "Professional coordination, booking app, maintenance included"
                     ),
                     create_attribute_option(
                       "p2p_guaranteed", "Guaranteed Community Access", 15, 35, "reliable access",
                       "Backup vehicles available, insurance included, priority booking", is_premium = TRUE
                     ),
                     
                     br(),
                     h6("Coverage Area", style = "margin: 15px 0 10px 0; color: #495057; font-weight: bold;"),
                     create_attribute_option(
                       "p2p_local", "Immediate Neighbourhood", 0, 0, "walking distance", 
                       "Vehicles within 200m, small local group of 8-12 households"
                     ),
                     create_attribute_option(
                       "p2p_extended", "Extended Area Network", 0, 10, "wider choice",
                       "Multiple neighbourhood groups, vehicles within 800m, larger fleet"
                     )
                   )
                 ),
                 
                 # PUBLIC TRANSPORT
                 create_service_section(
                   "Public Transport",
                   "Integrated transport passes with different coverage and flexibility levels",
                   50,
                   "public", 
                   tagList(
                     h6("Coverage Level", style = "margin: 0 0 10px 0; color: #495057; font-weight: bold;"),
                     create_attribute_option(
                       "pt_local", "Local Area Pass", 35, 0, "Leeds zone",
                       "Unlimited buses and local rail within Leeds boundary", is_default = TRUE
                     ),
                     create_attribute_option(
                       "pt_regional", "West Yorkshire Pass", 50, 15, "regional access",
                       "All buses, trains, and metros across West Yorkshire"
                     ),
                     create_attribute_option(
                       "pt_national", "National Rail Included", 50, 40, "long distance", 
                       "Regional pass plus discounted national rail travel", is_premium = TRUE
                     ),
                     
                     br(),
                     h6("Flexibility Options", style = "margin: 15px 0 10px 0; color: #495057; font-weight: bold;"),
                     create_attribute_option(
                       "pt_standard", "Standard Pass", 0, 0, "fixed monthly",
                       "Monthly pass, no refunds, standard terms"
                     ),
                     create_attribute_option(
                       "pt_flexible", "Flexible Pass", 0, 12, "pause/resume", 
                       "Can pause during holidays, partial refunds available"
                     )
                   )
                 ),
                 
                 # MICROMOBILITY & ACTIVE TRAVEL
                 create_service_section(
                   "Micromobility & Active Travel",
                   "E-bikes, scooters, and walking/cycling infrastructure access",
                   10,
                   "micro",
                   tagList(
                     h6("Service Level", style = "margin: 0 0 10px 0; color: #495057; font-weight: bold;"),
                     create_attribute_option(
                       "micro_basic", "Basic Bike Share", 10, 0, "standard access",
                       "Manual bikes, 30-minute journeys, docking stations", is_default = TRUE
                     ),
                     create_attribute_option(
                       "micro_electric", "E-bike & E-scooter Access", 10, 15, "powered options",
                       "Electric bikes and scooters, 45-minute journeys, more locations"
                     ),
                     create_attribute_option(
                       "micro_premium", "Premium Active Mobility", 10, 25, "unlimited access",
                       "All e-bike/scooter types, unlimited time, priority support", is_premium = TRUE
                     ),
                     
                     br(), 
                     h6("Additional Services", style = "margin: 15px 0 10px 0; color: #495057; font-weight: bold;"),
                     create_attribute_option(
                       "micro_storage", "Secure Cycle Storage", 0, 8, "safe parking",
                       "Guaranteed secure parking at home, work, and transport hubs"
                     ),
                     create_attribute_option(
                       "micro_maintenance", "Bike Maintenance Package", 0, 12, "full service",
                       "Annual service, repairs, and breakdown assistance for personal bikes"
                     )
                   )
                 ),
                 
                 br(),
                 div(align="center",
                     actionButton("to_sa_button6", "Continue with Portfolio", class="btn-primary btn-lg", style="margin: 20px 0;")
                 )
          ),
          
          # Sticky sidebar (right side)
          column(4, class = "sticky-sidebar",
                 # Scenario information box
                 div(class = "scenario-info",
                     h5("Leeds 2030 Scenario:", style = "margin-top: 0;"),
                     tags$table(
                       class="table table-sm table-borderless", style="margin-bottom: 0;",
                       tags$tbody(
                         tags$tr(tags$td(icon("gas-pump"), " Fuel prices:"), tags$td("+60p/litre")),
                         tags$tr(tags$td(icon("parking"), " Parking:"), tags$td("+20% permits")),  
                         tags$tr(tags$td(icon("bus"), " Public transport:"), tags$td("New integrated passes")),
                         tags$tr(tags$td(icon("share-alt"), " Mobility services:"), tags$td("Expanded car sharing"))
                       )
                     )
                 ),
                 
                 # Portfolio basket
                 div(class = "portfolio-basket",
                     uiOutput("sa4_basket_display")
                 )
          )
        )
    ),
    
    # JavaScript for selection management
    tags$script(HTML("
      // Allow only one selection per service category
      $(document).on('click', '.attribute-option', function() {
        var $option = $(this);
        var $serviceSection = $option.closest('.service-options');
        var $categoryHeader = $option.prevAll('h6:first');
        var wasSelected = $option.hasClass('selected');
        
        // For same category, deselect all others first
        $categoryHeader.nextUntil('h6, br').filter('.attribute-option').removeClass('selected');
        
        // If it wasn't selected before, select it now (allows deselection by clicking again)
        if (!wasSelected) {
          $option.addClass('selected');
        }
        
        updateBasket();
      });
      
      function updateBasket() {
        var selectedOptions = [];
        var totalCost = 0;
        
        $('.attribute-option.selected').each(function() {
          var $option = $(this);
          var title = $option.find('h6').text();
          if (!title) title = $option.find('h5').first().text(); // Fallback
          var priceText = $option.find('.total-price').text();
          var price = parseFloat(priceText.replace('£', ''));
          
          selectedOptions.push({title: title, price: price});
          totalCost += price;
        });
        
        // Update basket display
        var $basketItems = $('#basket-items');
        if (selectedOptions.length === 0) {
          $basketItems.html('<p style=\"color: #666; font-style: italic;\">Select service attributes below</p>');
        } else {
          var itemsHtml = selectedOptions.map(function(item) {
            return '<div style=\"padding: 2px 0; font-size: 0.9em; display: flex; justify-content: space-between;\"><span>• ' + 
                   item.title + '</span><span>£' + item.price + '</span></div>';
          }).join('');
          $basketItems.html(itemsHtml);
        }
        
        $('#basket-total-amount').text('£' + totalCost);
      }
    "))
    )
  })
  
  # This UI placeholder now just creates the box where the text will go.
  # ========= VERSION 5 (MENU BUILDER) - NEW =========
  
  output$sa_ui_placeholder6 <- renderUI({
    
    # Gatekeeper
    req(input$num_cars)
    
    # Current car details for display
    current_cars <- if(input$num_cars != "0") {
      cars_count <- switch(input$num_cars, "1"=1, "2"=2, "3"=3, "4+"=4)
      paste0("Currently own ", input$num_cars, " car", if(cars_count > 1) "s" else "")
    } else {
      "Currently car-free"
    }
    
    # Cost calculation function
    cost_func <- function(fuel, mileage) {
      base_costs <- list("Petrol"=150*1.25, "Diesel"=160*1.25, "Fully Electric"=80, "Plug-in Hybrid"=120*1.15)
      parking <- switch(mileage, "0-2,000"=5, "2,001-5,000"=10, "5,001-10,000"=15, "10,001+"=20, 0)
      multiplier <- switch(mileage, "0-2,000"=0.6, "2,001-5,000"=1.0, "5,001-10,000"=1.5, "10,001+"=2.2, 1)
      return(round((base_costs[[fuel]] * multiplier) + (parking * 1.20), 0))
    }
    
    # Service option creator
    create_service_option <- function(option_id, title, price, description, is_default = FALSE) {
      div(
        class = paste("service-option", if(is_default) "default-option"),
        id = option_id,
        onclick = paste0("Shiny.setInputValue('", option_id, "_clicked', Math.random())"),
        style = "cursor: pointer; margin-bottom: 8px;",
        fluidRow(
          column(8,
                 div(style = "display: flex; align-items: center;",
                     h6(title, style = "margin: 0; flex-grow: 1;"),
                     if(is_default) span(class = "default-badge", "Current")
                 ),
                 p(description, class = "service-description", style = "margin: 2px 0 0 0; color: #666; font-size: 0.85em;")
          ),
          column(4, align = "right",
                 span(class = "service-price", paste0("£", price, "/month"), style = "font-size: 1.1em; font-weight: bold; color: #28a745;")
          )
        )
      )
    }
    
    # Fleet choice section creator
    create_fleet_choice <- function(choice_title, choice_description, choice_id, service_options, is_current = FALSE) {
      tagList(
        div(class = paste("fleet-choice-header", if(is_current) "current-choice"),
            onclick = paste0("$('#", choice_id, "_services').toggle(); $(this).toggleClass('active');"),
            style = "cursor: pointer; padding: 20px; margin: 15px 0; border-radius: 8px; background: linear-gradient(135deg, #495057 0%, #343a40 100%); color: white;",
            fluidRow(
              column(10,
                     h4(choice_title, style = "margin: 0;"),
                     p(choice_description, style = "margin: 5px 0 0 0; opacity: 0.9;")
              ),
              column(2, align = "right",
                     if(is_current) span("CURRENT", style = "background: #ffc107; color: #000; padding: 4px 8px; border-radius: 12px; font-size: 0.8em; font-weight: bold;"),
                     br(),
                     span("Click to explore", style = "font-size: 0.8em; opacity: 0.8;")
              )
            )
        ),
        div(id = paste0(choice_id, "_services"), class = "fleet-services", style = "display: none; background: #f8f9fa; padding: 20px; border-radius: 0 0 8px 8px; margin-bottom: 20px;",
            service_options
        )
      )
    }
    
    # Calculate current ownership cost
    current_cost <- if(input$num_cars != "0") {
      total_cost <- 0
      cars_count <- switch(input$num_cars, "1"=1, "2"=2, "3"=3, "4+"=4)
      for(i in 1:cars_count) {
        if(!is.null(input[[paste0("car", i, "_fuel")]]) && !is.null(input[[paste0("car", i, "_mileage")]])) {
          total_cost <- total_cost + cost_func(input[[paste0("car", i, "_fuel")]], input[[paste0("car", i, "_mileage")]])
        }
      }
      total_cost
    } else {
      0
    }
    
    # Basket display
    output$fleet_basket_display <- renderUI({
      div(class = "fleet-basket",
          h5("Selected Fleet Strategy", style = "margin-bottom: 10px;"),
          div(id = "fleet-basket-items", style = "min-height: 40px;",
              p("Select your preferred fleet choice below", style = "color: #666; font-style: italic;")
          ),
          hr(style = "margin: 10px 0;"),
          div(class = "basket-total",
              span("Monthly Total: ", style = "font-weight: bold;"),
              span(id = "fleet-total-amount", "£0", style = "font-weight: bold; font-size: 1.3em; color: #28a745;")
          )
      )
    })
    
    # Main UI
    tagList(
      tags$style(HTML("
        .fleet-choice-header { transition: all 0.3s ease; }
        .fleet-choice-header:hover { transform: translateY(-2px); box-shadow: 0 4px 12px rgba(0,0,0,0.15); }
        .fleet-choice-header.active { background: linear-gradient(135deg, #28a745 0%, #20c997 100%) !important; }
        .fleet-choice-header.current-choice { background: linear-gradient(135deg, #007bff 0%, #0056b3 100%) !important; }
        
        .service-option { 
          background: #fff; 
          border: 1px solid #e9ecef; 
          border-radius: 6px; 
          padding: 12px; 
          transition: all 0.15s ease;
        }
        .service-option:hover { 
          border-color: #007bff; 
          box-shadow: 0 2px 8px rgba(0,123,255,0.1); 
          transform: translateY(-1px);
        }
        .service-option.selected { 
          border-color: #28a745; 
          background-color: #f8fff9;
          box-shadow: 0 0 0 2px rgba(40,167,69,0.2);
        }
        .service-option.default-option { background-color: #f8f9fa; }
        
        .default-badge { 
          background: #007bff; color: white; padding: 2px 6px; border-radius: 10px; font-size: 0.7em; margin-left: 8px;
        }
        
        .sticky-sidebar {
          position: sticky;
          top: 20px;
          height: fit-content;
          max-height: calc(100vh - 40px);
          overflow-y: auto;
        }
        
        .scenario-info {
          background-color: #f8f9fa; 
          border: 1px solid #dee2e6; 
          padding: 20px; 
          border-radius: 8px;
          margin-bottom: 20px;
        }
        
        .fleet-basket {
          background-color: #fff3cd; 
          border: 1px solid #ffeaa7; 
          padding: 20px; 
          border-radius: 8px;
        }
        
        @media (max-width: 768px) {
          .sticky-sidebar {
            position: static;
            margin-bottom: 20px;
          }
        }
      ")),
      
      div(class = "container-fluid", style = "max-width: 1200px; margin-top: 20px;",
          fluidRow(
            # Main content
            column(8,
                   div(style="text-align: center; margin-bottom: 30px;",
                       h2("Your Fleet Strategy for 2030"),
                       p(current_cars, style = "font-size: 1.1em; color: #495057;"),
                       p("Given the new scenario conditions, what would be your household's best car ownership strategy? Click each option to explore the services you'd combine with that fleet choice.")
                   ),
                   
                   # KEEP ALL CURRENT CARS
                   if(input$num_cars != "0") {
                     create_fleet_choice(
                       "Keep All Current Cars",
                       paste0("Maintain ownership of your existing ", input$num_cars, " car", if(as.numeric(input$num_cars) > 1) "s" else "", " with complementary services"),
                       "keep_all",
                       tagList(
                         h6("Your Current Fleet Cost:", style = "color: #495057; margin-bottom: 15px;"),
                         create_service_option("keep_basic", "Basic Ownership", current_cost, "Continue with current setup under new scenario conditions", is_default = TRUE),
                         create_service_option("keep_plus_carclub", "Ownership + Car Club", current_cost + 35, "Keep cars for regular use, add car club for city trips/backup"),
                         create_service_option("keep_plus_services", "Enhanced Ownership Package", current_cost + 45, "Premium insurance, breakdown cover, plus mobility services"),
                         
                         br(),
                         h6("Why keep all cars?", style = "color: #495057; margin: 15px 0 10px 0;"),
                         p("• Immediate availability when needed", style = "margin: 2px 0; font-size: 0.9em; color: #666;"),
                         p("• Personal space and belongings", style = "margin: 2px 0; font-size: 0.9em; color: #666;"),
                         p("• No booking or sharing hassles", style = "margin: 2px 0; font-size: 0.9em; color: #666;"),
                         p("• Flexibility for spontaneous trips", style = "margin: 2px 0; font-size: 0.9em; color: #666;")
                       ),
                       is_current = TRUE
                     )
                   },
                   
                   # REDUCE CARS
                   if(input$num_cars != "0" && as.numeric(input$num_cars) > 1) {
                     create_fleet_choice(
                       "Reduce to Fewer Cars",
                       "Give up one or more cars and replace with shared mobility services",
                       "reduce_cars",
                       tagList(
                         h6("Which car would you give up first?", style = "color: #495057; margin-bottom: 15px;"),
                         if(input$num_cars == "2") {
                           tagList(
                             create_service_option("reduce_keep_car1", paste("Keep Car 1:", input$car1_fuel, input$car1_type), 
                                                   cost_func(input$car1_fuel, input$car1_mileage) + 50, 
                                                   "Keep primary car, replace second with car club + public transport"),
                             if(!is.null(input$car2_fuel)) {
                               create_service_option("reduce_keep_car2", paste("Keep Car 2:", input$car2_fuel, input$car2_type), 
                                                     cost_func(input$car2_fuel, input$car2_mileage) + 50, 
                                                     "Keep second car, replace primary with shared services")
                             }
                           )
                         } else {
                           create_service_option("reduce_to_one", "Reduce to One Car", 
                                                 min(sapply(1:as.numeric(input$num_cars), function(i) {
                                                   if(!is.null(input[[paste0("car", i, "_fuel")]])) cost_func(input[[paste0("car", i, "_fuel")]], input[[paste0("car", i, "_mileage")]]) else 150
                                                 }), na.rm = TRUE) + 60, 
                                                 "Keep most efficient car, comprehensive mobility services for other needs")
                         },
                         
                         br(),
                         h6("What would replace the car(s)?", style = "color: #495057; margin: 15px 0 10px 0;"),
                         p("• Guaranteed car club access", style = "margin: 2px 0; font-size: 0.9em; color: #666;"),
                         p("• Enhanced public transport pass", style = "margin: 2px 0; font-size: 0.9em; color: #666;"),
                         p("• E-bike and micro-mobility options", style = "margin: 2px 0; font-size: 0.9em; color: #666;"),
                         p("• Taxi/ride-hailing budget included", style = "margin: 2px 0; font-size: 0.9em; color: #666;")
                       )
                     )
                   },
                   
                   # GO CAR-FREE
                   if(input$num_cars != "0") {
                     create_fleet_choice(
                       "Go Completely Car-Free",
                       "Give up all car ownership and rely entirely on shared mobility",
                       "go_carfree",
                       tagList(
                         h6("Complete mobility package:", style = "color: #495057; margin-bottom: 15px;"),
                         create_service_option("carfree_basic", "Essential Car-Free Package", 85, "Car club + public transport + basic e-bike access"),
                         create_service_option("carfree_premium", "Premium Car-Free Package", 120, "Guaranteed car access + comprehensive PT + full mobility services"),
                         create_service_option("carfree_luxury", "Car-Free with On-Demand", 180, "All services plus taxi/ride-hailing allowance for convenience"),
                         
                         br(),
                         h6("This works best if you:", style = "color: #495057; margin: 15px 0 10px 0;"),
                         p("• Live in well-connected urban area", style = "margin: 2px 0; font-size: 0.9em; color: #666;"),
                         p("• Have predictable travel patterns", style = "margin: 2px 0; font-size: 0.9em; color: #666;"),
                         p("• Don't need cars for work/cargo", style = "margin: 2px 0; font-size: 0.9em; color: #666;"),
                         p("• Value cost savings over convenience", style = "margin: 2px 0; font-size: 0.9em; color: #666;")
                       )
                     )
                   },
                   
                   # ADD ANOTHER CAR
                   create_fleet_choice(
                     "Add Another Car",
                     if(input$num_cars == "0") "Get your first car with supporting services" else "Expand fleet to meet growing needs",
                     "add_car",
                     tagList(
                       h6("What type of additional car?", style = "color: #495057; margin-bottom: 15px;"),
                       create_service_option("add_basic", "Basic Additional Car", 200, "Standard petrol/diesel car for extra capacity"),
                       create_service_option("add_electric", "Electric Vehicle", 280, "New EV with home charging installation"),
                       create_service_option("add_specialist", "Specialist Vehicle", 250, "Van, SUV, or other specific-purpose vehicle"),
                       
                       br(),
                       h6("You might need this if:", style = "color: #495057; margin: 15px 0 10px 0;"),
                       p("• Household size is growing", style = "margin: 2px 0; font-size: 0.9em; color: #666;"),
                       p("• Work requirements have changed", style = "margin: 2px 0; font-size: 0.9em; color: #666;"),
                       p("• Current car(s) insufficient for needs", style = "margin: 2px 0; font-size: 0.9em; color: #666;"),
                       p("• Want different car types for different uses", style = "margin: 2px 0; font-size: 0.9em; color: #666;")
                     )
                   ),
                   
                   br(),
                   div(align="center",
                       actionButton("continue_fleet_button", "Continue with Fleet Strategy", class="btn-primary btn-lg", style="margin: 20px 0;")
                   )
            ),
            
            # Sticky sidebar
            column(4, class = "sticky-sidebar",
                   # Scenario info
                   div(class = "scenario-info",
                       h5("Leeds 2030 Scenario:", style = "margin-top: 0;"),
                       tags$table(
                         class="table table-sm table-borderless", style="margin-bottom: 0;",
                         tags$tbody(
                           tags$tr(tags$td(icon("gas-pump"), " Fuel prices:"), tags$td("+60p/litre")),
                           tags$tr(tags$td(icon("parking"), " Parking:"), tags$td("+20% permits")),  
                           tags$tr(tags$td(icon("bus"), " Public transport:"), tags$td("New integrated passes")),
                           tags$tr(tags$td(icon("share-alt"), " Mobility services:"), tags$td("Expanded car sharing"))
                         )
                       )
                   ),
                   
                   # Fleet basket
                   div(class = "fleet-basket",
                       uiOutput("fleet_basket_display")
                   )
            )
          )
      ),
      
      # JavaScript for interaction
      tags$script(HTML("
        $(document).on('click', '.service-option', function() {
          var $option = $(this);
          var $section = $option.closest('.fleet-services');
          
          // Only allow one selection per fleet choice section
          $section.find('.service-option.selected').removeClass('selected');
          $option.addClass('selected');
          
          updateFleetBasket();
        });
        
        function updateFleetBasket() {
          var selectedOption = $('.service-option.selected');
          var $basketItems = $('#fleet-basket-items');
          
          if (selectedOption.length === 0) {
            $basketItems.html('<p style=\"color: #666; font-style: italic;\">Select your preferred fleet choice below</p>');
            $('#fleet-total-amount').text('£0');
          } else {
            var title = selectedOption.find('h6').text();
            var priceText = selectedOption.find('.service-price').text();
            var price = parseFloat(priceText.replace('£', '').replace('/month', ''));
            
            var itemHtml = '<div style=\"padding: 5px 0; font-size: 0.9em;\"><strong>' + title + '</strong></div>';
            $basketItems.html(itemHtml);
            $('#fleet-total-amount').text('£' + price);
          }
        }
      "))
    )
  })
 
}