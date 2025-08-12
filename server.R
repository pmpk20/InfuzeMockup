# server.R (Version 14.1 - Fully Functional Nested UI)

library(shiny)

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
    updateTabsetPanel(session, "main_tabs", selected = "sa_panel")
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
  observeEvent(input$to_summary_button, {
    updateTabsetPanel(session, "main_tabs", selected = "summary_panel")
  })
  observeEvent(input$to_Attitudes_button, {
    updateTabsetPanel(session, "main_tabs", selected = "AttitudesPage")
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
        "Petrol" = 150,
        "Diesel" = 160,
        "Fully Electric" = 80,
        "Plug-in Hybrid" = 120
      )
      
      mileage_multiplier <- switch(mileage_bracket,
                                   "0-2,000" = 0.6,
                                   "2,001-5,000" = 1.0,
                                   "5,001 - 10,000" = 1.5,
                                   "10,001+" = 2.2
      )
      
      return(base_costs[[fuel_type]] * mileage_multiplier)
    }
    
    # Add costs for each vehicle
    if (input$num_cars != "0") {
      if (!is.null(input$car1_fuel)) {
        total_cost <- total_cost + vehicle_monthly_cost(input$car1_fuel, input$car1_mileage)
      }
      
      if (input$num_cars %in% c("2", "3", "4+") && !is.null(input$car2_fuel)) {
        total_cost <- total_cost + vehicle_monthly_cost(input$car2_fuel, input$car2_mileage)
      }
      
      if (input$num_cars %in% c("3", "4+") && !is.null(input$car3_fuel)) {
        total_cost <- total_cost + vehicle_monthly_cost(input$car3_fuel, input$car3_mileage)
      }
      
      if (input$num_cars == "4+" && !is.null(input$car4_fuel)) {
        total_cost <- total_cost + vehicle_monthly_cost(input$car4_fuel, input$car4_mileage)
      }
    }
    
    # Add public transport costs
    pt_cost <- switch(input$pt_spend,
                      "£0" = 0,
                      "£1-£30" = 15,
                      "£31-£60" = 45,
                      "£61-£100" = 80,
                      "More than £100" = 120,
                      0
    )
    
    total_cost <- total_cost + pt_cost
    
    # Add car sharing costs (assume £20/month if member)
    if (!is.null(input$car_share_member) && input$car_share_member == "Yes") {
      total_cost <- total_cost + 20
    }
    
    return(round(total_cost, 2))
  })
  
  # New monthly cost (Z) - based on Step 4 adaptation choices
  # *** REVISED LOGIC FOR CLARITY AND ACCURACY ***
  new_monthly_cost <- reactive({
    req(input$num_cars)
    
    total_cost <- 0
    
    # Enhanced vehicle costs with scenario price increases
    vehicle_monthly_cost_new <- function(fuel_type, mileage_bracket) {
      # Base costs with fuel price increase (+60p/litre ≈ +25% for petrol/diesel)
      base_costs <- list(
        "Petrol" = 150 * 1.25,
        "Diesel" = 160 * 1.25,
        "Fully Electric" = 80, # No fuel price increase
        "Plug-in Hybrid" = 120 * 1.15 # Partial increase
      )
      
      # Parking permit increase (+20%)
      parking_increase <- switch(mileage_bracket,
                                 "0-2,000" = 5,
                                 "2,001-5,000" = 10,
                                 "5,001 - 10,000" = 15,
                                 "10,001+" = 20
      )
      
      mileage_multiplier <- switch(mileage_bracket,
                                   "0-2,000" = 0.6,
                                   "2,001-5,000" = 1.0,
                                   "5,001 - 10,000" = 1.5,
                                   "10,001+" = 2.2
      )
      
      return((base_costs[[fuel_type]] * mileage_multiplier) + (parking_increase * 1.20))
    }
    
    # Handle zero-car households
    if (input$num_cars == "0") {
      req(input$sa_0_car_decision)
      total_cost <- switch(input$sa_0_car_decision,
                           "add_pt" = 50,
                           "add_carshare" = 25,
                           "no_changes" = 0,
                           0
      )
    } else { # Handle households with 1+ cars
      # Iterate through each potential vehicle
      num_vehicles <- switch(input$num_cars, "1" = 1, "2" = 2, "3" = 3, "4+" = 4)
      
      for (i in 1:num_vehicles) {
        decision_id <- paste0("sa_car", i, "_decision")
        decision <- input[[decision_id]]
        
        if (!is.null(decision)) {
          cost_to_add <- switch(decision,
                                "Keep this vehicle" = {
                                  # NOTE: The slider `sa_carX_usage_change` is not tied to a mileage bracket.
                                  # This calculation assumes mileage does not change for a "kept" vehicle.
                                  # This is a potential area for future refinement.
                                  vehicle_monthly_cost_new(input[[paste0("car", i, "_fuel")]], input[[paste0("car", i, "_mileage")]])
                                },
                                "Replace this vehicle" = {
                                  replace_fuel <- input[[paste0("sa_car", i, "_replace_fuel")]]
                                  replace_mileage <- input[[paste0("sa_car", i, "_replace_mileage")]]
                                  req(replace_fuel, replace_mileage) # Ensure inputs are available
                                  vehicle_monthly_cost_new(replace_fuel, replace_mileage)
                                },
                                "Remove and use PT" = 50,
                                "Remove and use Car-sharing" = 25,
                                0 # Default case if no decision matches
          )
          total_cost <- total_cost + cost_to_add
        }
      }
    }
    
    # The new total cost is based *only* on the choices made in the SA experiment.
    # We no longer add previous PT/car-share costs on top.
    
    return(round(total_cost, 2))
  })
  
  # Add these output renderers to your server.R:
  output$current_monthly_formatted <- renderText({
    cost <- current_monthly_cost()
    req(cost)
    format(cost, big.mark = ",", nsmall = 2)
  })
  
  output$current_annual_formatted <- renderText({
    cost <- current_monthly_cost()
    req(cost)
    format(cost * 12, big.mark = ",", nsmall = 2)
  })
  
  output$new_monthly_formatted <- renderText({
    cost <- new_monthly_cost()
    req(cost)
    format(cost, big.mark = ",", nsmall = 2)
  })
  
  output$new_annual_formatted <- renderText({
    cost <- new_monthly_cost()
    req(cost)
    format(cost * 12, big.mark = ",", nsmall = 2)
  })
  
  output$monthly_change <- renderText({
    new_cost <- new_monthly_cost()
    current_cost <- current_monthly_cost()
    req(new_cost, current_cost)
    
    # Avoid division by zero
    if (current_cost == 0 && new_cost > 0) return(paste0("+£", format(new_cost, nsmall = 2), ", +Inf%"))
    if (current_cost == 0 && new_cost == 0) return("£0.00, 0.0%")
    
    change <- new_cost - current_cost
    percentage <- round((change / current_cost) * 100, 1)
    sign <- if(change >= 0) "+" else ""
    paste0(sign, "£", format(abs(change), big.mark = ",", nsmall = 2), ", ", sign, percentage, "%")
  })
  
  output$annual_change <- renderText({
    new_cost <- new_monthly_cost() * 12
    current_cost <- current_monthly_cost() * 12
    req(new_cost, current_cost)
    
    # Avoid division by zero
    if (current_cost == 0 && new_cost > 0) return(paste0("+£", format(new_cost, nsmall = 2), ", +Inf%"))
    if (current_cost == 0 && new_cost == 0) return("£0.00, 0.0%")
    
    change <- new_cost - current_cost
    percentage <- round((change / current_cost) * 100, 1)
    sign <- if(change >= 0) "+" else ""
    paste0(sign, "£", format(abs(change), big.mark = ",", nsmall = 2), ", ", sign, percentage, "%")
  })
  
  #  ========= SA VERSION LATEST (with shinyBS collapsible panels for ALL users) =========
  output$sa_ui_placeholder <- renderUI({
    
    # Gatekeeper
    req(input$num_cars)
    if (input$num_cars != "0") {
      req(input$car1_type, input$car1_fuel, input$car1_mileage)
    }
    
    # --- UI Components ---
    main_question <- h3("How would you adapt your household's travel options?")
    
    # --- Helper function for CAR-OWNING households ---
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
                  tags$th(style = "width:25%; background: linear-gradient(135deg, #dc3545 0%, #bd2130 100%); color: white; text-align: center;", "Keep this vehicle"),
                  tags$th(style = "width:25%; background: linear-gradient(135deg, #28a745 0%, #1e7e34 100%); color: white; text-align: center;", strong("Replace this vehicle")),
                  tags$th(style = "width:25%; background: linear-gradient(135deg, #007bff 0%, #0056b3 100%); color: white; text-align: center;", strong("Remove and use PT")),
                  tags$th(style = "width:25%; background: linear-gradient(135deg, #ffc107 0%, #d39e00 100%); color: black; text-align: center;", strong("Remove and use Car-sharing"))
                )
              ),
              tags$tbody(
                tags$tr( # Cost row
                  tags$td(style="text-align: center; background-color: #f8d7da; font-weight: bold;", paste0("£", current_cost, "/month")),
                  tags$td(style="text-align: center; background-color: #d4edda; font-weight: bold;", textOutput(paste0("sa_car", car_number, "_replace_cost"), inline = TRUE)),
                  tags$td(style="text-align: center; background-color: #cce7ff; font-weight: bold;", "£50/month"),
                  tags$td(style="text-align: center; background-color: #fff3cd; font-weight: bold;", "£25/month")
                ),
                tags$tr( # Input row
                  tags$td(style="background-color: #fce8e8; text-align:center; vertical-align:middle;", p("Continue using this vehicle as is.")),
                  tags$td(style = "background-color: #e8f5e8;",
                          selectInput(paste0("sa_car", car_number, "_replace_type"), "Type:", choices = c("Car", "Van", "Motorbike")),
                          selectInput(replace_fuel_id, "Fuel:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
                          selectInput(replace_mileage_id, "Mileage:", choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+"), selected = mileage_val)
                  ),
                  tags$td(style="background-color: #e6f3ff; text-align:center; vertical-align:middle;", p("Dispose of this vehicle and get a 'Leeds Travel Pass'.")),
                  tags$td(style="background-color: #fff9e6; text-align:center; vertical-align:middle;", p("Dispose of this vehicle and join 'INFUZE_TRIAL' car-sharing."))
                ),
                tags$tr( # Radio button row
                  tags$td(style = "text-align: center; background-color: #f8d7da;", radioButtons(decision_id, NULL, choices = "Keep this vehicle", selected = character(0))),
                  tags$td(style = "text-align: center; background-color: #d4edda;", radioButtons(decision_id, NULL, choices = "Replace this vehicle", selected = character(0))),
                  tags$td(style = "text-align: center; background-color: #cce7ff;", radioButtons(decision_id, NULL, choices = "Remove and use PT", selected = character(0))),
                  tags$td(style = "text-align: center; background-color: #fff3cd;", radioButtons(decision_id, NULL, choices = "Remove and use Car-sharing", selected = character(0)))
                )
              )
            )
          )
        )
      )
    }
    
    # --- NEW: Helper function for ZERO-CAR households ---
    create_zero_car_adaptation_block <- function() {
      bsCollapse(
        id = "sa_collapse_0",
        open = "sa_panel_0", # Always open by default for these users
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
    
    # --- Assemble the UI ---
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
            tags$td(style="border:1px solid #ddd; padding:8px; vertical-align:top;", strong("Estimated new monthly cost: £"), textOutput("new_monthly_formatted", inline=TRUE), " (", textOutput("monthly_change", inline=TRUE), ")")
          ),
          tags$tr(
            tags$td(style="border:1px solid #ddd; padding:8px; vertical-align:top;", strong("Car Club: "), "New sharing models available"),
            tags$td(style="border:1px solid #ddd; padding:8px; vertical-align:top;", strong("Estimated new annual cost: £"), textOutput("new_annual_formatted", inline=TRUE), " (", textOutput("annual_change", inline=TRUE), ")")
          )
        )
      )
    )
    
    ui_to_render <- list(scenario_and_costs_table, main_question)
    
    if (input$num_cars == "0") {
      # Call the new collapsible block function for zero-car users
      ui_to_render <- append(ui_to_render, list(create_zero_car_adaptation_block()))
    } else {
      # Loop and call the collapsible block function for each car
      num_vehicles <- switch(input$num_cars, "1"=1, "2"=2, "3"=3, "4+"=4)
      for (i in 1:num_vehicles) {
        if (!is.null(input[[paste0("car", i, "_type")]])) {
          ui_to_render <- append(ui_to_render, list(create_vehicle_adaptation_block(i, input[[paste0("car", i, "_type")]], input[[paste0("car", i, "_fuel")]], input[[paste0("car", i, "_mileage")]])))
        }
      }
    }
    
    ui_to_render <- append(ui_to_render, 
                           list(hr(), actionButton("to_sa_button2", "Continue", class = "btn-success btn-lg")))
    
    do.call(tagList, ui_to_render)
    
  })
  

  
  # ========= MOCKUP VERSION 2 =========
  output$sa_ui_placeholder2 <- renderUI({
    
    # --- UI Components ---
    intro_line <- h4("Please consider how you would react to the following hypothetical scenario:")
    
    main_question <- h3("How would you adapt your household's travel options?")
    
    # --- Helper function to create the nested UI for ONE vehicle ---
    # Replace the create_vehicle_adaptation_block function with this DCE version:
    
    create_vehicle_adaptation_block <- function(car_number, vehicle_type, fuel_type, mileage_val) {
      
      decision_id <- paste0("sa_car", car_number, "_decision")
      
      # Calculate costs for each alternative
      calculate_monthly_cost <- function(fuel, mileage) {
        base_costs <- list(
          "Petrol" = 150 * 1.25,
          "Diesel" = 160 * 1.25, 
          "Fully Electric" = 80,
          "Plug-in Hybrid" = 120 * 1.15
        )
        
        parking_increase <- switch(mileage,
                                   "0-2,000" = 5,
                                   "2,001-5,000" = 10,
                                   "5,001 - 10,000" = 15,
                                   "10,001+" = 20)
        
        mileage_multiplier <- switch(mileage,
                                     "0-2,000" = 0.6,
                                     "2,001-5,000" = 1.0,
                                     "5,001 - 10,000" = 1.5,
                                     "10,001+" = 2.2)
        
        return(round((base_costs[[fuel]] * mileage_multiplier) + (parking_increase * 1.20), 0))
      }
      
      # Calculate CO2 emissions (rough estimates)
      calculate_co2 <- function(fuel, mileage) {
        annual_miles <- switch(mileage,
                               "0-2,000" = 1000,
                               "2,001-5,000" = 3500,
                               "5,001 - 10,000" = 7500,
                               "10,001+" = 12000)
        
        co2_per_mile <- switch(fuel,
                               "Petrol" = 0.18,
                               "Diesel" = 0.16,
                               "Fully Electric" = 0.05,
                               "Plug-in Hybrid" = 0.08)
        
        return(round(annual_miles * co2_per_mile / 1000, 1)) # tonnes per year
      }
      
      # Current vehicle costs
      current_cost <- calculate_monthly_cost(fuel_type, mileage_val)
      # current_co2 <- calculate_co2(fuel_type, mileage_val)
      
      # Electric replacement costs
      electric_cost <- calculate_monthly_cost("Fully Electric", mileage_val)
      # electric_co2 <- calculate_co2("Fully Electric", mileage_val)
      
      # Car-sharing alternative (£5/hour, assume 10 hours/month average)
      carshare_cost <- 50 # £5 × 10 hours
      # carshare_co2 <- 0.3 # Much lower due to shared usage
      
      vehicle_description <- p(strong(paste0("Decision for vehicle ",
                                             car_number,
                                             ": ",
                                             tolower(fuel_type), " ", 
                                             tolower(vehicle_type), 
                                             " driven approximately ", 
                                             mileage_val, " miles per year.")))
      
      wellPanel(
        style = "background-color: white; border: 2px solid white;",

        # h4(vehicle_description),

        # h4(paste0("Vehicle ", car_number)),
        vehicle_description,

        div(style = "border: 1px solid white; padding: 15px; margin: 10px 0;",
            h5("Please choose your preferred option:", style = "margin-bottom: 20px;"),
        
        # DCE alternatives in a grid layout
            fluidRow(

              # **********************************************************************************************
              #### Alternative A: Keep current vehicle ####
              column(2,
                     wellPanel(
                       style = "text-align: center; background-color: #f8f9fa; border: 2px solid black;",

                       h6("A: Keep Current Vehicle", style = "font-weight: bold;"),

                       hr(style = "margin: 10px 0;"),

                       p("Vehicle: ", paste0("My current vehicle"),
                         style = "margin: 5px 0; font-size: 12px;"),

                       p("Time: ", paste0("search time to park"),
                         style = "margin: 5px 0; font-size: 12px;"),

                       p(paste0("Monthly cost: £", current_cost),
                         style = "margin: 5px 0; font-size: 12px;"),

                       radioButtons(width = "100%",
                         paste0(decision_id, "_choice"),
                         "",
                         choices = c("I would choose this option" = "keep"),
                         selected = character(0)
                       )
                     )
              ),


              # **********************************************************************************************
              #### Alternative B: New vehicle ####
              column(2,
                     wellPanel(
                       style = "text-align: center; background-color: #f8f9fa; border: 2px solid black;",

                       h6("B: Replace with Electric", style = "font-weight: bold;"),

                       hr(style = "margin: 10px 0;"),

                       p(paste0("Vehicle: Electric ", vehicle_type),
                         style = "margin: 5px 0; font-size: 12px;"),

                       p("Time: ", paste0("search time to park and charge"),
                         style = "margin: 5px 0; font-size: 12px;"),

                       p(paste0("Monthly cost: £", electric_cost),
                         style = "margin: 5px 0; font-size: 12px;"),

                       radioButtons(width = "100%",
                                    paste0(decision_id, "_choice"), "",
                                    choices = c("I would choose this option" = "replace_electric"),
                                    selected = character(0))
                     )
              ),

        
        # **********************************************************************************************
              #### Alternative C: PT #### 
              column(2,
                     wellPanel(
                       style = "text-align: center; background-color: #f8f9fa; border: 2px solid black;",

                       h6("C: Use Public Transport",
                          style = "font-weight: bold;"),

                       hr(style = "margin: 10px 0;"),
                       p("Service: Leeds Public Transport",
                         style = "margin: 5px 0; font-size: 12px;"),

                       p("Time: ", paste0("waiting for it to arrive"),
                         style = "margin: 5px 0; font-size: 12px;"),

                       p(paste0("Monthly cost: £",
                                carshare_cost, "*"),
                         style = "margin: 5px 0; font-size: 12px;"),

                       radioButtons(width = "100%",
                                    paste0(decision_id, "_choice"), "",
                                    choices = c("I would choose this option" = "carshare"),
                                    selected = character(0))
                     )
              ),
        
        
              # **********************************************************************************************
              #### Alternative D: Car sharing #### 
              column(2,
                     wellPanel(
                       style = "text-align: center; background-color: #f8f9fa; border: 2px solid black;",

                       h6("D: Switch to Car-Sharing",
                          style = "font-weight: bold;"),

                       hr(style = "margin: 10px 0;"),
                       p("Service: INFUZE_TRIAL",
                         style = "margin: 5px 0; font-size: 12px;"),

                       p("Time: ", paste0("wait for parking and to return the vehicle"),
                         style = "margin: 5px 0; font-size: 12px;"),

                       p(paste0("Monthly cost: £",
                                carshare_cost, "*"),
                         style = "margin: 5px 0; font-size: 12px;"),

                       radioButtons(width = "100%",
                                    paste0(decision_id, "_choice"), "",
                                    choices = c("I would choose this option" = "carshare"),
                                    selected = character(0))
                     )
              )
            ),

        ),
      
        
        # Conditional panels for additional details (simplified)
        conditionalPanel(
          condition = paste0("input.", decision_id, "_choice == 'replace_electric'"),
          wellPanel(style = "margin-top: 15px;",
                    h6("Additional Details for Electric Vehicle:"),
                    selectInput(paste0("sa_car", car_number, "_electric_mileage"), 
                                "Expected annual mileage with electric vehicle:",
                                choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+"),
                                selected = mileage_val))
        ),
        
        conditionalPanel(
          condition = paste0("input.", decision_id, "_choice == 'carshare'"),
          wellPanel(style = "margin-top: 15px;",
                    h6("Additional Details for Car-Sharing:"),
                    selectInput(paste0("sa_car", car_number, "_carshare_hours"), 
                                "Expected hours of car-sharing per month:",
                                choices = c("0-5 hours (£0-25)", "6-10 hours (£30-50)", 
                                            "11-15 hours (£55-75)", "16+ hours (£80+)")))
        )
      )
    }
    
    # --- Assemble the UI ---
    req(input$num_cars)
    
    # FIX: Define the zero_car_adaptation_ui object BEFORE it is used.
    zero_car_adaptation_ui <- conditionalPanel(
      # Simplified condition - show if they start with 0 cars OR if all their cars are removed
      condition = "input.num_cars == '0' || (input.num_cars == '1' && input.sa_car1_decision == 'Remove this vehicle') || (input.num_cars == '2' && input.sa_car1_decision == 'Remove this vehicle' && input.sa_car2_decision == 'Remove this vehicle') || (input.num_cars == '3' && input.sa_car1_decision == 'Remove this vehicle' && input.sa_car2_decision == 'Remove this vehicle' && input.sa_car3_decision == 'Remove this vehicle') || (input.num_cars == '4+' && input.sa_car1_decision == 'Remove this vehicle' && input.sa_car2_decision == 'Remove this vehicle' && input.sa_car3_decision == 'Remove this vehicle' && input.sa_car4_decision == 'Remove this vehicle')",
      hr(),
      h4("Decisions about your new travel portfolio:"),
      p("Since your household would not have an owned car in this scenario, what new travel options would you adopt?"),
      checkboxGroupInput("sa_0_car_add_options", "Select all that apply:",
                         choices = c("Sign up for 'INFUZE_TRIAL' Car Sharing", 
                                     "Get a 'Leeds Travel Pass' for Public Transport",
                                     "Acquire a new OWNED vehicle")),
      conditionalPanel(
        condition = "input.sa_0_car_add_options && input.sa_0_car_add_options.indexOf('Acquire a new OWNED vehicle') > -1",
        wellPanel(style = "background-color: #D5F5E3;",
                  h5("Details of the NEW vehicle:"),
                  selectInput("sa_0_car_new_type", "Vehicle Type:", choices = c("Car", "Van", "Motorbike")),
                  selectInput("sa_0_car_new_fuel", "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
                  selectInput("sa_0_car_new_mileage", "Estimated Annual Mileage (miles):", choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+"))
        )
      )
    )
    
    scenario_and_costs_table <- wellPanel(
      style = "background-color: white; border: 2px solid black;",
      h4("Scenario Details & Your Costs"),
      tags$table(
        style = "width: 100%; border-collapse: collapse;",
        tags$thead(
          tags$tr(
            tags$th(style = "border: 1px solid #ddd; padding: 8px; background-color: #f2f2f2; width: 50%;", "Scenario Changes"),
            tags$th(style = "border: 1px solid #ddd; padding: 8px; background-color: #f2f2f2; width: 50%;", "Cost Estimates")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(style = "border: 1px solid #ddd; padding: 8px; vertical-align: top;", 
                    tags$strong("Fuel prices: "), "+60p per litre"),
            tags$td(style = "border: 1px solid #ddd; padding: 8px; vertical-align: top;", 
                    tags$strong("Current monthly cost: £"), textOutput("current_monthly_formatted", inline = TRUE))
          ),
          tags$tr(
            tags$td(style = "border: 1px solid #ddd; padding: 8px; vertical-align: top;", 
                    tags$strong("Parking permit costs: "), "+20%"),
            tags$td(style = "border: 1px solid #ddd; padding: 8px; vertical-align: top;", 
                    tags$strong("Current annual cost: £"), textOutput("current_annual_formatted", inline = TRUE))
          ),
          tags$tr(
            tags$td(style = "border: 1px solid #ddd; padding: 8px; vertical-align: top;", 
                    tags$strong("Public Transport: "), "A new 'Leeds Travel Pass' for £50/month"),
            tags$td(style = "border: 1px solid #ddd; padding: 8px; vertical-align: top;", 
                    tags$strong("Estimated monthly cost: £"), textOutput("new_monthly_formatted", inline = TRUE),
                    " (", textOutput("monthly_change", inline = TRUE), ")")
          ),
          tags$tr(
            tags$td(style = "border: 1px solid #ddd; padding: 8px; vertical-align: top;", 
                    tags$strong("Car Club: "), "Membership to 'INFUZE_TRIAL' costs £5 per hour of use, with vehicles available within a 5-minute walk"),
            tags$td(style = "border: 1px solid #ddd; padding: 8px; vertical-align: top;", 
                    tags$strong("Estimated annual cost: £"), textOutput("new_annual_formatted", inline = TRUE),
                    " (", textOutput("annual_change", inline = TRUE), ")")
          )
        )
      )
    )
    # Then in your ui_to_render section, replace:
    # ui_to_render <- list(intro_line, scenario_box, main_question)
    # with:
    ui_to_render <- list(intro_line, scenario_and_costs_table, main_question)
    
    # Check if user starts with zero cars
    if (input$num_cars == "0") {
      # Show zero-car options immediately
      zero_car_ui <- list(
        hr(),
        h4("Decisions about your travel portfolio:"),
        p("Since your household does not currently own a vehicle, what new travel options would you adopt in this scenario?"),
        selectInput("sa_0_car_add_options", "Select all that apply:",
                    choices = c(
                      "No, no other changes",
                      "Sign up for 'INFUZE_TRIAL' Car Sharing",
                      "Get a 'Leeds Travel Pass' for Public Transport",
                      "Acquire a new OWNED vehicle")),
        conditionalPanel(
          condition = "input.sa_0_car_add_options && input.sa_0_car_add_options.indexOf('Acquire a new OWNED vehicle') > -1",
          wellPanel(style = "background-color: #D5F5E3;",
                    h5("Details of the NEW vehicle:"),
                    selectInput("sa_0_car_new_type", "Vehicle Type:", choices = c("Car", "Van", "Motorbike")),
                    selectInput("sa_0_car_new_fuel", "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
                    selectInput("sa_0_car_new_mileage", "Estimated Annual Mileage (miles):", choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+"))
          )
        )
      )
      ui_to_render <- append(ui_to_render, zero_car_ui)
    } else {
      # For households with vehicles
      ui_to_render <- append(ui_to_render, list(h4("")))
      
      # Vehicle 1
      if (!is.null(input$car1_type)) {
        ui_to_render <- append(ui_to_render, list(create_vehicle_adaptation_block(1, input$car1_type, input$car1_fuel, input$car1_mileage)))
      }
      
      # Vehicle 2
      if (input$num_cars %in% c("2", "3", "4+") && !is.null(input$car2_type)) {
        ui_to_render <- append(ui_to_render, list(create_vehicle_adaptation_block(2, input$car2_type, input$car2_fuel, input$car2_mileage)))
      }
      
      # Vehicle 3
      if (input$num_cars %in% c("3", "4+") && !is.null(input$car3_type)) {
        ui_to_render <- append(ui_to_render, list(create_vehicle_adaptation_block(3, input$car3_type, input$car3_fuel, input$car3_mileage)))
      }
      
      # Vehicle 4
      if (input$num_cars == "4+" && !is.null(input$car4_type)) {
        ui_to_render <- append(ui_to_render, list(create_vehicle_adaptation_block(4, input$car4_type, input$car4_fuel, input$car4_mileage)))
      }
      
      # Add the conditional panel for when all vehicles are removed
      zero_car_adaptation_ui <- conditionalPanel(
        condition = "(input.num_cars == '1' && input.sa_car1_decision == 'Remove this vehicle') || (input.num_cars == '2' && input.sa_car1_decision == 'Remove this vehicle' && input.sa_car2_decision == 'Remove this vehicle') || (input.num_cars == '3' && input.sa_car1_decision == 'Remove this vehicle' && input.sa_car2_decision == 'Remove this vehicle' && input.sa_car3_decision == 'Remove this vehicle') || (input.num_cars == '4+' && input.sa_car1_decision == 'Remove this vehicle' && input.sa_car2_decision == 'Remove this vehicle' && input.sa_car3_decision == 'Remove this vehicle' && input.sa_car4_decision == 'Remove this vehicle')",
        hr(),
        h4("Decisions about your new travel portfolio:"),
        p("Since your household would not have an owned car in this scenario, what new travel options would you adopt?"),
        checkboxGroupInput("sa_0_car_add_options", "Select all that apply:",
                           choices = c("Sign up for 'INFUZE_TRIAL' Car Sharing", 
                                       "Get a 'Leeds Travel Pass' for Public Transport",
                                       "Acquire a new OWNED vehicle")),
        conditionalPanel(
          condition = "input.sa_0_car_add_options && input.sa_0_car_add_options.indexOf('Acquire a new OWNED vehicle') > -1",
          wellPanel(style = "background-color: #D5F5E3;",
                    h5("Details of the NEW vehicle:"),
                    selectInput("sa_0_car_new_type", "Vehicle Type:", choices = c("Car", "Van", "Motorbike")),
                    selectInput("sa_0_car_new_fuel", "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
                    selectInput("sa_0_car_new_mileage", "Estimated Annual Mileage (miles):", choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+"))
          )
        )
      )
      
      ui_to_render <- append(ui_to_render, list(zero_car_adaptation_ui))
    }
    
    
    ui_to_render <- append(ui_to_render, 
                           list(hr(), 
                                actionButton("to_sa_button3", "Version 3", 
                                # actionButton("to_summary_button", "See summary", 
                                             class = "btn-success btn-lg")))
    # ui_to_render <- append(ui_to_render, list(hr(), cost_box, hr(), actionButton("to_summary_button", "I have decided, see summary", class = "btn-success btn-lg")))
    
    do.call(tagList, ui_to_render)
    
  })
  
  
  # ========= MOCKUP VERSION 3 (Configurator Style) - Experimental Trials =========
  output$sa_ui_placeholder3 <- renderUI({
    
    # Gatekeeper
    req(input$num_cars)
    if (input$num_cars != "0") {
      req(input$car1_type, input$car1_fuel, input$car1_mileage)
    }
    
    # --- Helper function (no changes) ---
    create_choice_card <- function(title, choice_value, cost_text, specs_list, border_class, is_popular = FALSE) {
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
                            radioButtons("sa3_decision", label=NULL, choices = setNames(choice_value, "Select this Option"), selected = character(0))
                        )
                 )
        )
      )
    }
    
    # --- Scenario Header Box (no changes) ---
    scenario_header <- div(style="text-align: center; margin-bottom: 40px;",
                           h2("Imagine this scenario in Leeds..."),
                           p("Consider the following changes and then configure your new household travel plan below."),
                           wellPanel(
                             style = "background-color: #f8f9fa; border: 1px solid #dee2e6; display: inline-block; text-align: left; max-width: 800px;",
                             tags$ul(
                               style = "list-style-type: none; padding-left: 0;",
                               tags$li(icon("gas-pump"), strong(" Fuel prices:"), " +60p per litre"),
                               tags$li(icon("parking"), strong(" Parking permit costs:"), " +20%"),
                               tags$li(icon("bus"), strong(" Public Transport:"), " A new 'Leeds Travel Pass' is available for £50/month"),
                               tags$li(icon("car"), strong(" Car Club:"), " New sharing models are available, see options below.")
                             )
                           ),
                           div(class = "ethics-note",
                               p("NOTE: Here I am testing what a product configurator looks like.")
                           ),
                           hr()
    )
    
    # --- Main UI Layout ---
    tagList(
      # --- CSS block with a new border class added ---
      tags$style(HTML("
      /* Main card container */
      .configurator-card {
        background-color: #ffffff; border: 1px solid #e9ecef; border-left-width: 7px;
        border-radius: 8px; padding: 20px; margin-bottom: 25px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.05); transition: all 0.2s ease-in-out;
      }
      .configurator-card:hover { transform: translateY(-3px); box-shadow: 0 8px 16px rgba(0,0,0,0.1); }
      
      /* Classes for the colored borders and matching titles */
      .card-border-car { border-left-color: #007bff; } 
      .card-border-car h4 { color: #007bff; }
      .card-border-shared { border-left-color: #28a745; } 
      .card-border-shared h4 { color: #28a745; }
      .card-border-managed { border-left-color: #17a2b8; } /* New Teal color */
      .card-border-managed h4 { color: #17a2b8; }
      .card-border-pt { border-left-color: #6f42c1; } 
      .card-border-pt h4 { color: #6f42c1; }

      /* The banner style */
      .popular-flag {
        background-color: #ffc107; color: #343a40; padding: 4px 12px;
        border-radius: 5px; font-weight: bold; font-size: 0.9em;
        display: inline-block; margin-bottom: 15px;
      }
      
      /* Price and Selection styling */
      .card-price { font-size: 1.8em; font-weight: bold; color: #333; margin-bottom: 0; }
      .card-selection .form-group { margin-bottom: 0 !important; }

      /* Other styles */
      .card-header h4 { margin-top: 0; }
      .card-specs { margin-top: 15px; font-size: 0.95em; }
      .spec-item { margin-bottom: 8px; }
      .spec-label { font-weight: bold; }
    ")),
    
    scenario_header,
    
    sidebarLayout(
      # --- Filter Panel (no changes) ---
      sidebarPanel(
        width = 3,
        h4("What's important to you?"),
        p("Select your needs to see how each option compares (filtering is not yet active in this prototype)."),
        hr(),
        checkboxGroupInput("sa3_motivations", 
                           label = "Motivations:",
                           choices = c("I need immediate access 24/7", "I have mobility needs", 
                                       "Minimising cost is my top priority", "I want to reduce my carbon footprint")),
        hr(),
        radioButtons("sa3_seats", label = "Number of seats required:", choices = c("1-2", "3-4", "5+"))
      ),
      
      # --- Choice Cards Panel with new options ---
      mainPanel(
        width = 9,
        {
          choice_cards <- list()
          
          if (input$num_cars != "0") {
            current_car_cost <- reactive({
              base_costs <- list("Petrol"=150*1.25, "Diesel"=160*1.25, "Fully Electric"=80, "Plug-in Hybrid"=120*1.15)
              parking <- switch(input$car1_mileage, "0-2,000"=5, "2,001-5,000"=10, "5,001 - 10,000"=15, "10,001+"=20, 0)
              multiplier <- switch(input$car1_mileage, "0-2,000"=0.6, "2,001-5,000"=1.0, "5,001 - 10,000"=1.5, "10,001+"=2.2, 1)
              round((base_costs[[input$car1_fuel]] * multiplier) + (parking * 1.20), 0)
            })
            
            choice_cards <- append(choice_cards, list(
              create_choice_card(
                title = paste("Keep Your Current", input$car1_fuel, "Car"), choice_value = "keep_car",
                cost_text = paste0("£", current_car_cost()),
                specs_list = list("Travel Time" = "No change", "Reliability" = "High", "Availability" = "Immediate, 24/7", "Environmental Impact" = "High"),
                border_class = "card-border-car"
              )
            ))
          }
          
          # Append the rest of the cards
          choice_cards <- append(choice_cards, list(
            create_choice_card(
              title = "Replace with a new Electric Vehicle", choice_value = "replace_ev",
              cost_text = "£210", 
              specs_list = list("Travel Time" = "Similar to current", "Reliability" = "Very High", "Availability" = "Immediate, 24/7", "Environmental Impact" = "Very Low"),
              border_class = "card-border-car"
            ),
            # --- NEW CARD 1: My Days ---
            create_choice_card(
              title = "'My Days' Dedicated Vehicle", choice_value = "use_my_days",
              cost_text = "£75", # Placeholder cost
              specs_list = list("Provider" = "Enterprise", "Reliability" = "Very High (on your days)", "Availability" = "Scheduled days only", "Environmental Impact" = "Low"),
              border_class = "card-border-managed", is_popular = TRUE
            ),
            # --- NEW CARD 2: Peer-to-Peer ---
            create_choice_card(
              title = "Closed Loop Peer-to-Peer Sharing", choice_value = "use_p2p",
              cost_text = "£25", # Placeholder cost
              specs_list = list("Provider" = "Neighbourhood group", "Reliability" = "Medium (depends on group)", "Availability" = "By arrangement with group", "Environmental Impact" = "Low"),
              border_class = "card-border-shared"
            ),
            create_choice_card(
              title = "Switch to Public Transport", choice_value = "use_pt",
              cost_text = "£50", 
              specs_list = list("Travel Time" = "Varies with schedule", "Reliability" = "Medium", "Availability" = "Scheduled", "Environmental Impact" = "Very Low"),
              border_class = "card-border-pt"
            )
          ))
          
          choice_cards
        }
      )
    ),
    hr(),
    div(align="center",
        actionButton("to_sa_button4", "Continue", class="btn-primary btn-lg")
    )
    )
  })
  
  
  # ========= SA VERSION NEW (DCE Format) =========
  output$sa_ui_placeholder4 <- renderUI({
    
    # Gatekeeper
    req(input$num_cars)
    if (input$num_cars != "0") {
      req(input$car1_type, input$car1_fuel, input$car1_mileage)
    }
    # --- Helper function to create the DCE choice task for ONE vehicle ---
    create_vehicle_adaptation_block <- function(car_number, vehicle_type, fuel_type, mileage_val) {
      
      decision_id <- paste0("sa_car", car_number, "_decision")
      
      calculate_vehicle_cost <- function(fuel, mileage) {
        base_costs <- list("Petrol"=150*1.25, "Diesel"=160*1.25, "Fully Electric"=80, "Plug-in Hybrid"=120*1.15)
        parking <- switch(mileage, "0-2,000"=5, "2,001-5,000"=10, "5,001-10,000"=15, "10,001+"=20, 0)
        multiplier <- switch(mileage, "0-2,000"=0.6, "2,001-5,000"=1.0, "5,001-10,000"=1.5, "10,001+"=2.2, 1)
        return(round((base_costs[[fuel]] * multiplier) + (parking * 1.20), 0))
      }
      
      current_cost <- calculate_vehicle_cost(fuel_type, mileage_val)
      vehicle_desc_short <- paste(fuel_type, vehicle_type)
      vehicle_desc <- paste0(tolower(fuel_type), " ", tolower(vehicle_type), " driven approximately ", mileage_val, " miles per year")
      
      bsCollapse(
        id = paste0("sa_collapse_", car_number),
        open = if (car_number == 1) paste0("sa_panel_", car_number) else NULL, 
        bsCollapsePanel(
          title = paste0("Choice Task for Vehicle ", car_number, ": Your ", vehicle_desc),
          value = paste0("sa_panel_", car_number),
          style = "primary",
          tags$table(
            class = "table dce-table",
            tags$thead(
              tags$tr(
                tags$th("Attribute", class = "col-attribute"),
                tags$th("Package A: Status Quo", class = "col-status-quo"),
                tags$th("Package B: Modernise", class = "col-modernise"),
                tags$th("Package C: Go Car-Lite", class = "col-carlite"),
                tags$th("Package D: Go Car-Free", class = "col-carfree")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("Owned Vehicle"),
                tags$td(class="col-status-quo", paste("Keep your", vehicle_desc_short)),
                tags$td(class="col-modernise", "Replace with a Small EV"),
                tags$td(class="col-carlite", paste("Keep your", vehicle_desc_short), " and add car-sharing"),
                tags$td(class="col-carfree", paste("Replace your", vehicle_desc_short), " with car-sharing")
              ),
              tags$tr(
                tags$td("Availability"),
                tags$td(class="col-status-quo", "90% available - 10% garage"),
                tags$td(class="col-modernise", "95% available - 5% garage"),
                tags$td(class="col-carlite", "Car: 90% + Car-sharing: 99%"),
                tags$td(class="col-carfree", "Car-sharing: 99%")
              ),
              tags$tr(
                tags$td("Access Time"),
                tags$td(class="col-status-quo", "Immediately"),
                tags$td(class="col-modernise", "Immediately"),
                tags$td(class="col-carlite", "10 minute walk to nearest shared car"),
                tags$td(class="col-carfree", "10 minute walk to nearest shared car")
              ),
              tags$tr(
                tags$td("Est. Monthly Cost"),
                tags$td(class="col-status-quo", style="font-weight:bold;", paste0("£", current_cost)),
                tags$td(class="col-modernise", style="font-weight:bold;", "£165"),
                tags$td(class="col-carlite", style="font-weight:bold;", paste0("£", current_cost + 25)),
                tags$td(class="col-carfree", style="font-weight:bold;", "£120")
              ),
              tags$tr(
                tags$td("Your Choice"),
                tags$td(class="col-status-quo", radioButtons(decision_id, NULL, inline = TRUE, choices = "Status Quo", selected = character(0))),
                tags$td(class="col-modernise", radioButtons(decision_id, NULL, inline = TRUE, choices = "Modernise", selected = character(0))),
                tags$td(class="col-carlite", radioButtons(decision_id, NULL, inline = TRUE, choices = "Car-Lite", selected = character(0))),
                tags$td(class="col-carfree", radioButtons(decision_id, NULL, inline = TRUE, choices = "Car-Free", selected = character(0)))
              )
            )
          )
        )
      )
    }
    
    # --- Assemble the UI ---
    tagList(
      tags$style(HTML("
      .dce-table { border-collapse: separate; border-spacing: 0; width: 100%; }
      .dce-table th, .dce-table td { text-align: center; vertical-align: middle !important; padding: 12px 8px !important; border-top: 1px solid #ddd; }
      .dce-table thead th { color: white; font-size: 1.1em; border: none; }
      .dce-table tbody td:first-child { text-align: left; font-weight: bold; background-color: #f8f9fa; }
      .dce-table .radio { justify-content: center; display: flex; }
      .radio .form-check-input { transform: scale(1.5); }
      .col-attribute { background-color: #dc3545 !important; }
      .col-status-quo { background-color: #007bff !important; }
      .col-modernise { background-color: #28a745 !important; }
      .col-carlite { background-color: #ffc107 !important; }
      .col-carfree { background-color: #fd7e14 !important; color: #343a40 !important; }
      .dce-table td.col-status-quo { background-color: #e6f0ff !important; }
      .dce-table td.col-modernise { background-color: #eaf6ec !important; }
      .dce-table td.col-carlite { background-color: #fff9e8 !important; }
      .dce-table td.col-carfree { background-color: #fff2e8 !important; }
    ")),
    
    # This is the cost/scenario box that is now fed by your main reactive logic
    wellPanel(
      style = "background-color: white; border: 2px solid black;",
      tags$table(style = "width: 100%; border-collapse: collapse;",
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
                     tags$td(style="border:1px solid #ddd; padding:8px; vertical-align:top;", strong("Estimated new monthly cost: £"), textOutput("new_monthly_formatted", inline=TRUE), " (", textOutput("monthly_change", inline=TRUE), ")")
                   ),
                   tags$tr(
                     tags$td(style="border:1px solid #ddd; padding:8px; vertical-align:top;", strong("Car Club: "), "New sharing models available"),
                     tags$td(style="border:1px solid #ddd; padding:8px; vertical-align:top;", strong("Estimated new annual cost: £"), textOutput("new_annual_formatted", inline=TRUE), " (", textOutput("annual_change", inline=TRUE), ")")
                   )
                 )
      )
    ),
    
    h3("Please choose the package you prefer most for each of your vehicles"),
    
    if (input$num_cars == "0") {
      p("As a zero-car household, a different set of choices will be presented here.")
    } else {
      lapply(1:switch(input$num_cars, "1"=1, "2"=2, "3"=3, "4+"=4), function(i) {
        if (!is.null(input[[paste0("car", i, "_type")]])) {
          create_vehicle_adaptation_block(i, input[[paste0("car", i, "_type")]], 
                                          input[[paste0("car", i, "_fuel")]], 
                                          input[[paste0("car", i, "_mileage")]])
        }
      })
    },
    
    hr(), 
    actionButton("to_sa_button2", "Continue", class = "btn-success btn-lg")
    )
    
  })
  
  
  # **********************************************************************************************
  #### REVISED SUMMARY OUTPUT LOGIC #### 
  
  # FIX: Define the reactive expression in the main server scope
  summary_text <- eventReactive(input$to_summary_button, {
    
    # Use req() to ensure necessary inputs exist before trying to create the summary
    req(input$num_cars, input$household_size)
    
    # Initial RP section
    rp_summary <- paste(
      "--- REVEALED PREFERENCES ---\n",
      "Household Size:", input$household_size, "\n",
      "Owned Vehicles:", input$num_cars, "\n"
    )
    if (input$num_cars != "0") {
      rp_summary <- paste(rp_summary,
                          "  - Main Vehicle: ", input$car1_type, "|", input$car1_fuel, "|", input$car1_mileage, "miles/year\n"
      )
    }
    if (input$num_cars %in% c("2", "3", "4+")) {
      rp_summary <- paste(rp_summary,
                          "  - Second Vehicle: ", input$car2_type, "|", input$car2_fuel, "|", input$car2_mileage, "miles/year\n"
      )
    }
    # Add cars 3 & 4 if they exist in the UI
    if (input$num_cars %in% c("3", "4+")) {
      rp_summary <- paste(rp_summary,
                          "  - Third Vehicle: ", input$car3_type, "|", input$car3_fuel, "|", input$car3_mileage, "miles/year\n"
      )
    }
    if (input$num_cars == "4+") {
      rp_summary <- paste(rp_summary,
                          "  - Fourth Vehicle: ", input$car4_type, "|", input$car4_fuel, "|", input$car4_mileage, "miles/year\n"
      )
    }
    rp_summary <- paste(rp_summary,
                        "Car Share Member:", input$car_share_member, "\n",
                        "PT Spend:", input$pt_spend, "\n"
    )
    
    # SA section
    sa_summary <- "\n--- STATED ADAPTATION CHOICES ---\n"
    
    # Add a check to ensure the SA inputs exist before trying to access them
    if (!is.null(input$sa_car1_decision)) {
      if (input$num_cars != "0") {
        sa_summary <- paste(sa_summary,
                            "Decision for Main Vehicle:", input$sa_car1_decision, "\n",
                            "  - Mileage Change:", if(input$sa_car1_decision == "Keep this vehicle") input$sa_car1_mileage_change else "N/A", "\n"
        )
      }
    }
    if (!is.null(input$sa_car2_decision)) {
      if (input$num_cars %in% c("2", "3", "4+")) {
        sa_summary <- paste(sa_summary,
                            "Decision for Second Vehicle:", input$sa_car2_decision, "\n",
                            "  - Mileage Change:", if(input$sa_car2_decision == "Keep this vehicle") input$sa_car2_mileage_change else "N/A", "\n"
        )
      }
    }
    
    sa_summary <- paste(sa_summary, "\nNew Options Adopted:", if(!is.null(input$sa_add_options)) paste(input$sa_add_options, collapse=", ") else "None", "\n")
    
    if (!is.null(input$sa_add_options) && 'Acquire a new OWNED vehicle' %in% input$sa_add_options) {
      sa_summary <- paste(sa_summary,
                          "  - New Owned Vehicle Details:", input$sa_new_car_type, "|", input$sa_new_car_fuel, "|", input$sa_new_car_mileage, "miles/year\n"
      )
    }
    
    paste(rp_summary, sa_summary)
  })
  
  # This UI placeholder now just creates the box where the text will go.
  output$summary_ui_placeholder <- renderUI({
    tagList(
      h3("Summary of Your Choices"),
      p("This is the data that would be recorded for analysis. The combination of your Stated Adaptation choices forms your 'adaptation bundle'."),
      verbatimTextOutput("final_summary_text"),
      actionButton("to_Attitudes_button", 
                   "Next page", 
                   class = "btn-success btn-lg")
    )
  })
  
  # This renderer now correctly finds summary_text() in the main server scope.
  output$final_summary_text <- renderText({
    summary_text()
  })
  
}