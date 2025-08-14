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
    updateTabsetPanel(session, "main_tabs", selected = "sa_panel_5")
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
  
  #  ========= VERSION 1 =========
  output$sa_ui_placeholder <- renderUI({
    
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
                  tags$th(style = "width:25%; background: linear-gradient(135deg, #dc3545 0%, #bd2130 100%); color: white; text-align: center;", 
                          "Keep this vehicle"),
                  
                  tags$th(style = "width:25%; background: linear-gradient(135deg, #28a745 0%, #1e7e34 100%); color: white; text-align: center;", 
                          strong("Replace this vehicle")),
                  
                  tags$th(style = "width:25%; background: linear-gradient(135deg, #007bff 0%, #0056b3 100%); color: white; text-align: center;", 
                          strong("Remove and use PT")),
                  
                  tags$th(style = "width:25%; background: linear-gradient(135deg, #ffc107 0%, #d39e00 100%); color: black; text-align: center;", 
                          strong("Remove and use Car-sharing"))
                  
                )
              ),
              tags$tbody(
                tags$tr(
                  # tags$td("Cost"),
                  tags$td(style="text-align: center; background-color: #f8d7da; font-weight: bold;", 
                          paste0("£", current_cost, "/month")),
                  
                  tags$td(style="text-align: center; background-color: #d4edda; font-weight: bold;", 
                          textOutput(paste0("sa_car", car_number, "_replace_cost"))),
                  tags$td(style="text-align: center; background-color: #cce7ff; font-weight: bold;", "£50/month"),
                  tags$td(style="text-align: center; background-color: #fff3cd; font-weight: bold;", "£25/month")
                  
                ),
                
                tags$tr(
                  # tags$td("Availability"),
                  tags$td(style="text-align: center; background-color: #f8d7da; font-weight: bold;", "90% available - 10% garage"),
                  tags$td(style="text-align: center; background-color: #d4edda; font-weight: bold;", "95% available - 5% garage"),
                  tags$td(style="text-align: center; background-color: #cce7ff; font-weight: bold;", "Car: 90% + Car-sharing: 99%"),
                  tags$td(style="text-align: center; background-color: #fff3cd; font-weight: bold;", "Car-sharing: 99%")
                ),
                tags$tr(
                  # tags$td("Access Time"),
                  tags$td(style="text-align: center; background-color: #f8d7da; font-weight: bold;", "You can access immediately"),
                  tags$td(style="text-align: center; background-color: #d4edda; font-weight: bold;", "You can access immediately"),
                  tags$td(style="text-align: center; background-color: #cce7ff; font-weight: bold;", "10 minute walk to nearest shared car"),
                  tags$td(style="text-align: center; background-color: #fff3cd; font-weight: bold;", "10 minute walk to nearest shared car")
                ),
                
                tags$tr(
                  tags$td(style="background-color: #f8d7da; text-align:center; vertical-align:middle;", p("Continue using this vehicle as is.")),
                  tags$td(style = "background-color: #d4edda;",
                          selectInput(paste0("sa_car", car_number, "_replace_type"), "Type:", choices = c("Car", "Van", "Motorbike")),
                          selectInput(replace_fuel_id, "Fuel:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
                          selectInput(replace_mileage_id, "Mileage:", choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+"), selected = mileage_val)
                  ),
                  tags$td(style="background-color: #cce7ff; text-align:center; vertical-align:middle;", p("Dispose of this vehicle and get a 'Leeds Travel Pass'.")),
                  tags$td(style="background-color: #fff3cd; text-align:center; vertical-align:middle;", p("Dispose of this vehicle and join 'INFUZE_TRIAL' car-sharing."))
                ),
                tags$tr(
                  tags$td(style = "text-align: center; background-color: #fce8e8;", radioButtons(decision_id, NULL, choices = "Keep this vehicle", selected = character(0))),
                  tags$td(style = "text-align: center; background-color: #e8f5e8;", radioButtons(decision_id, NULL, choices = "Replace this vehicle", selected = character(0))),
                  tags$td(style = "text-align: center; background-color: #e6f3ff;", radioButtons(decision_id, NULL, choices = "Remove and use PT", selected = character(0))),
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
                           list(hr(), actionButton("to_sa_button2", "Continue", class = "btn-success btn-lg")))
    
    do.call(tagList, ui_to_render)
    
  })
  
  # ========= VERSION 2 =========
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
                                actionButton("to_sa_button3", "Version 3", 
                                             # actionButton("to_summary_button", "See summary", 
                                             class = "btn-success btn-lg")))
    # ui_to_render <- append(ui_to_render, list(hr(), cost_box, hr(), actionButton("to_summary_button", "I have decided, see summary", class = "btn-success btn-lg")))
    
    do.call(tagList, ui_to_render)
    
  })
    

  
  # ========= VERSION 3 =========
  output$sa_ui_placeholder3 <- renderUI({
    
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
          actionButton("to_sa_button4", "Continue", class="btn-primary btn-lg")
      )
    )
  })
  
  
  
  # ========= VERSION 4 =========
  output$sa_ui_placeholder4 <- renderUI({
    
    req(input$num_cars)
    if (input$num_cars != "0") {
      req(input$car1_type, input$car1_fuel, input$car1_mileage)
    }
    
    # --- Helper function for CAR-OWNING households ---
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
                tags$td(class="col-carfree", "Dispose of vehicle and use car-sharing")
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
    
    # --- NEW: Helper function for ZERO-CAR households ---
    create_zero_car_dce_block <- function() {
      bsCollapse(
        id = "sa_collapse_0", open = "sa_panel_0",
        bsCollapsePanel(
          title = "Choice Task for Your Household", value = "sa_panel_0", style = "info",
          tags$table(
            class = "table dce-table",
            tags$thead(
              tags$tr(
                tags$th("Attribute", class = "col-attribute"),
                tags$th("Option A: No Change", class = "col-status-quo"),
                tags$th("Option B: Add Public Transport", class = "col-modernise"), # Re-using colors
                tags$th("Option C: Add Car Sharing", class = "col-carlite")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("Primary Mode"),
                tags$td(class="col-status-quo", "Current travel patterns"),
                tags$td(class="col-modernise", "Leeds Travel Pass"),
                tags$td(class="col-carlite", "INFUZE_TRIAL Car Sharing")
              ),
              tags$tr(
                tags$td("Availability"),
                tags$td(class="col-status-quo", "N/A"),
                tags$td(class="col-modernise", "Scheduled"),
                tags$td(class="col-carlite", "99%")
              ),
              tags$tr(
                tags$td("Access Time"),
                tags$td(class="col-status-quo", "N/A"),
                tags$td(class="col-modernise", "5-minute walk to stop"),
                tags$td(class="col-carlite", "5-minute walk to vehicle")
              ),
              tags$tr(
                tags$td("Est. Monthly Cost"),
                tags$td(class="col-status-quo", style="font-weight:bold;", "£0"),
                tags$td(class="col-modernise", style="font-weight:bold;", "£50"),
                tags$td(class="col-carlite", style="font-weight:bold;", "£25")
              ),
              tags$tr(
                tags$td("Your Choice"),
                tags$td(class="col-status-quo", radioButtons("sa4_0_car_decision", NULL, inline = TRUE, choices = "No changes", selected = character(0))),
                tags$td(class="col-modernise", radioButtons("sa4_0_car_decision", NULL, inline = TRUE, choices = "Add PT", selected = character(0))),
                tags$td(class="col-carlite", radioButtons("sa4_0_car_decision", NULL, inline = TRUE, choices = "Add Car-sharing", selected = character(0)))
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
                       tags$td(style="border:1px solid #ddd; padding:8px; vertical-align:top;", strong("Estimated new monthly cost: £"), textOutput("new_monthly_formatted", inline=TRUE), textOutput("monthly_change", inline=TRUE))
                     ),
                     tags$tr(
                       tags$td(style="border:1px solid #ddd; padding:8px; vertical-align:top;", strong("Car Club: "), "New sharing models available"),
                       tags$td(style="border:1px solid #ddd; padding:8px; vertical-align:top;", strong("Estimated new annual cost: £"), textOutput("new_annual_formatted", inline=TRUE), textOutput("annual_change", inline=TRUE))
                     )
                   )
        )
      ),
      
      h3("Please choose the package you prefer most for each of your vehicles"),
      
      if (input$num_cars == "0") {
        create_zero_car_dce_block() # Call the new zero-car function
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
      actionButton("to_summary_button", "Continue", class = "btn-success btn-lg")
    )
  })
    
  
  
  
  # This UI placeholder now just creates the box where the text will go.
  # ========= VERSION 5 (MENU BUILDER) - CORRECTED =========
  function(input, output, session) {
    
    # single source of truth: costs and labels
    costs <- list(
      petrol_car = 280,
      electric_car = 420,
      car_free = 0,
      pt_none = 0,
      pt_offpeak = 35,
      pt_unlimited = 75,
      cs_payg = 25,
      cs_subscription = 85,
      bs_monthly = 15
    )
    
    labels <- list(
      petrol_car = "Keep my current Petrol Car",
      electric_car = "Replace with Small Electric Vehicle",
      car_free = "Go Car-Free (rely on other services)",
      pt_none = "None (pay-as-you-go)",
      pt_offpeak = "Off-Peak Travel Pass",
      pt_unlimited = "Full 'Leeds Travel Pass' (unlimited)",
      cs_payg = "Pay-as-you-go Access (occasional use)",
      cs_subscription = "Monthly Subscription (regular use)",
      bs_monthly = "Monthly Bike Share Pass"
    )
    
    # helper: collect all selected item codes (vector)
    selected_items <- reactive({
      sel <- c(input$owned_vehicle, input$public_transport, input$car_sharing, input$bike_sharing)
      sel <- unlist(sel, use.names = FALSE)
      sel <- sel[!is.na(sel) & nzchar(sel)]
      unique(sel)
    })
    
    # reactive total
    total_monthly_cost <- reactive({
      s <- selected_items()
      if (length(s) == 0) return(0)
      sum(unlist(costs[s]), na.rm = TRUE)
    })
    
    # render cost breakdown as HTML (individual lines + total)
    output$cost_breakdown <- renderUI({
      s <- selected_items()
      if (length(s) == 0) {
        tags$div(tags$p("No services selected."), tags$p(tags$strong("Total: £0")))
      } else {
        # lines with label and cost, aligned left within the box
        lines <- lapply(s, function(code) {
          tags$div(style = "display:flex; justify-content:space-between; padding:2px 0;",
                   tags$span(labels[[code]]),
                   tags$span(style = "font-weight:700;", paste0("£", costs[[code]]))
          )
        })
        # total line
        total_line <- tags$div(style = "border-top:1px solid #d4e6d4; margin-top:6px; padding-top:6px; display:flex; justify-content:space-between;",
                               tags$span(tags$strong("Total")),
                               tags$span(tags$strong(paste0("£", total_monthly_cost())))
        )
        tagList(lines, total_line)
      }
    })
    
    # textual package summary
    output$package_summary <- renderText({
      s <- selected_items()
      if (length(s) == 0) return("No selections.")
      lines <- sapply(s, function(code) paste0(labels[[code]], " — £", costs[[code]]))
      paste(lines, collapse = "\n")
    })
    
    # log on confirm
    observeEvent(input$confirm_choice, {
      data <- list(
        timestamp = as.character(Sys.time()),
        selections = selected_items(),
        total_cost = total_monthly_cost(),
        session_id = session$token
      )
      cat("Choice recorded:\n")
      cat(toJSON(data, pretty = TRUE, auto_unbox = TRUE), "\n")
      # replace with persistent save if required
    })
    
  }
  
  # This renderer now correctly finds summary_text() in the main server scope.
  # output$final_summary_text <- renderText({
  #   summary_text()
  # })
  
}