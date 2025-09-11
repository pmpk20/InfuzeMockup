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
  
  
  # ========= Version 1: Discrete CE [old] =========
  output$sa_ui_placeholder2 <- renderUI({
    
    req(input$num_cars)
    if (input$num_cars != "0") {
      req(input$car1_type, input$car1_fuel, input$car1_mileage)
    }
    
    main_question <- h4("In this scenario, how would your household adapt travel over the next 3 years?")
    
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
    
  
  
  # ========= Version 2: CE by stage =========

  # ---- Render UI for placeholder 3 (defensive, fixes length-one text error) ----
  output$sa_ui_placeholder3 <- renderUI({
    req(input$num_cars)
    
    # ---------- helper: safe extractor ----------
    safe_val <- function(x, fallback = "") {
      if (is.null(x)) return(fallback)
      x_chr <- as.character(x)
      if (length(x_chr) == 0) return(fallback)
      x_chr[1]
    }
    
    # ---------- helper functions (trip-aware) ----------
    calculate_vehicle_cost <- function(fuel_type, mileage_val, weekly_trips = 0) {
      fuel_type <- safe_val(fuel_type, "Petrol")
      mileage_val <- safe_val(mileage_val, "2,001-5,000")
      
      base_costs <- list("Petrol" = 150 * 1.25,
                         "Diesel" = 160 * 1.25,
                         "Fully Electric" = 80,
                         "Plug-in Hybrid" = 120 * 1.15)
      base <- if (!is.null(base_costs[[fuel_type]])) base_costs[[fuel_type]] else 150
      
      parking <- switch(mileage_val,
                        "0-2,000" = 5, "2,001-5,000" = 10,
                        "5,001 - 10,000" = 15, "10,001+" = 20, 10)
      multiplier <- switch(mileage_val,
                           "0-2,000" = 0.6, "2,001-5,000" = 1.0,
                           "5,001 - 10,000" = 1.5, "10,001+" = 2.2, 1.0)
      fixed_monthly <- round((base * multiplier) + (parking * 1.20), 0)
      
      monthly_trips <- as.numeric(ifelse(is.null(weekly_trips), 0, weekly_trips)) * 4.345
      per_trip_vars <- list("Petrol" = 0.45, "Diesel" = 0.40, "Fully Electric" = 0.20, "Plug-in Hybrid" = 0.30)
      per_trip <- if (!is.null(per_trip_vars[[fuel_type]])) per_trip_vars[[fuel_type]] else 0.40
      variable_monthly <- round(per_trip * monthly_trips, 2)
      
      list(fixed = fixed_monthly, variable = variable_monthly, total = round(fixed_monthly + variable_monthly, 0))
    }
    
    calculate_carlite_cost_numeric <- function(weekly_trips = NULL) {
      avail_choice <- safe_val(input[["sa_carlite_availability"]], "pt_90_on_time")
      mult_avail <- switch(avail_choice, "pt_90_on_time" = 0.95, "95_garage" = 1.0, "80_shared" = 1.15, "custom" = 1.0, 1.0)
      access_choice <- safe_val(input[["sa_carlite_access"]], "10_min")
      mult_access <- switch(access_choice, "at_home" = 1.0, "5_min" = 1.05, "10_min" = 1.10, 1.0)
      pt_choice <- safe_val(input[["sa_carlite_ptpass"]], "no")
      pt_cost <- if (pt_choice == "yes" && avail_choice == "pt_90_on_time") 50 else 0
      
      first_vehicle_cost <- tryCatch(calculate_vehicle_cost(safe_val(input$car1_fuel), safe_val(input$car1_mileage), 0)$fixed,
                                     error = function(e) 100)
      fixed_base <- round(first_vehicle_cost * 0.7 + 50)
      fixed_monthly <- round(fixed_base * mult_avail * mult_access + pt_cost)
      
      weekly <- if (is.null(weekly_trips)) safe_val(input[["sa_carlite_trips"]], 0) else weekly_trips
      monthly_trips <- as.numeric(ifelse(is.null(weekly) || identical(weekly, ""), 0, weekly)) * 4.345
      per_trip_carlite <- 1.50
      variable_monthly <- round(per_trip_carlite * monthly_trips, 2)
      
      list(fixed = fixed_monthly, variable = variable_monthly, total = round(fixed_monthly + variable_monthly, 0))
    }
    
    calculate_carfree_cost_numeric <- function(weekly_trips = NULL) {
      avail_choice <- safe_val(input[["sa_carfree_availability"]], "99_avail")
      mult_avail <- switch(avail_choice, "99_avail" = 0.95, "90_avail" = 1.0, "75_avail" = 1.2, "custom" = 1.0, 1.0)
      access_choice <- safe_val(input[["sa_carfree_access"]], "5_min")
      mult_access <- switch(access_choice, "at_home" = 1.0, "5_min" = 1.05, "10_min" = 1.10, 1.0)
      pt_choice <- safe_val(input[["sa_carfree_ptpass"]], "no")
      pt_cost <- if (pt_choice == "yes" && avail_choice == "pt_90_on_time") 50 else 0
      
      carshare_base <- 80
      fixed_monthly <- round(carshare_base * mult_avail * mult_access + pt_cost)
      
      weekly <- if (is.null(weekly_trips)) safe_val(input[["sa_carfree_trips"]], 0) else weekly_trips
      monthly_trips <- as.numeric(ifelse(is.null(weekly) || identical(weekly, ""), 0, weekly)) * 4.345
      per_trip_pt <- 2.20
      variable_monthly <- round(per_trip_pt * monthly_trips, 2)
      
      list(fixed = fixed_monthly, variable = variable_monthly, total = round(fixed_monthly + variable_monthly, 0))
    }
    
    # ---------- reactive outputs (trip-aware) ----------
    local({
      output[["sa_carlite_monthly_cost"]] <- renderText({
        vals <- calculate_carlite_cost_numeric()
        paste0("£", formatC(vals$total, digits = 0, format = "f"))
      })
      
      output[["sa_carfree_monthly_cost"]] <- renderText({
        vals <- calculate_carfree_cost_numeric()
        paste0("£", formatC(vals$total, digits = 0, format = "f"))
      })
      
      output[["trips_summary_val2"]] <- renderText({
        num_vehicles <- switch(safe_val(input$num_cars, "0"), "0" = 0, "1" = 1, "2" = 2, "3" = 3, "4+" = 4, 0)
        total_trips <- 0L
        
        if (num_vehicles == 0) {
          # sum reported weekly trips across modes (sliders)
          total_trips <- 0L
          total_trips <- total_trips + as.integer(safe_val(input[["sa_0_trips_none"]], 0))
          total_trips <- total_trips + as.integer(safe_val(input[["sa_0_trips_pt"]], 0))
          total_trips <- total_trips + as.integer(safe_val(input[["sa_0_trips_carshare"]], 0))
          paste0(total_trips, " trips")
          
        } else {
          for (i in seq_len(num_vehicles)) {
            t <- input[[paste0("sa_car", i, "_trips_keep")]]
            if (!is.null(t) && length(t) >= 1) total_trips <- total_trips + as.integer(t[1])
          }
          if (!is.null(input[["sa_carlite_trips"]])) total_trips <- total_trips + as.integer(input[["sa_carlite_trips"]])
          if (!is.null(input[["sa_carfree_trips"]])) total_trips <- total_trips + as.integer(input[["sa_carfree_trips"]])
        }
        
        paste0(total_trips, " trips")
      })
      
      output[["cost_summary_val2"]] <- renderText({
        num_vehicles <- switch(safe_val(input$num_cars, "0"), "0" = 0, "1" = 1, "2" = 2, "3" = 3, "4+" = 4, 0)
        total_cost <- 0
        
        if (num_vehicles == 0) {
          choice <- safe_val(input[["sa_0_car_decision"]], "no_changes")
          
          # cost when they "increase alternatives" => sum of car-lite + car-free totals (as before)
          if (choice == "increase_alts") {
            total_cost <- calculate_carlite_cost_numeric(as.numeric(safe_val(input[["sa_0_trips_pt"]], 0)))$total +
              calculate_carfree_cost_numeric(as.numeric(safe_val(input[["sa_0_trips_carshare"]], 0)))$total
          } else if (choice == "get_car") {
            # naive estimate of monthly cost of acquiring a small petrol car
            # estimate weekly car trips as the maximum of current mode trips (simple heuristic)
            est_weekly_trips <- max(as.numeric(safe_val(input[["sa_0_trips_none"]], 0)),
                                    as.numeric(safe_val(input[["sa_0_trips_pt"]], 0)),
                                    as.numeric(safe_val(input[["sa_0_trips_carshare"]], 0)),
                                    1)
            # use petrol + mid mileage as default; change if you prefer another baseline
            vc <- calculate_vehicle_cost("Petrol", "2,001-5,000", est_weekly_trips)
            total_cost <- vc$total
          } else {
            # no_changes: aggregate current alternatives' nominal cost (car-free assumed 0 ownership)
            total_cost <- calculate_carlite_cost_numeric(as.numeric(safe_val(input[["sa_0_trips_pt"]], 0)))$total +
              calculate_carfree_cost_numeric(as.numeric(safe_val(input[["sa_0_trips_carshare"]], 0)))$total
            # If you prefer 'no_changes' to show zero, set total_cost <- 0 instead.
          }
          
          paste0("£", formatC(total_cost, format = "f", big.mark = ",", digits = 0))
        } else {
          for (i in seq_len(num_vehicles)) {
            ft <- safe_val(input[[paste0("car", i, "_fuel")]], "Petrol")
            mi <- safe_val(input[[paste0("car", i, "_mileage")]], "2,001-5,000")
            weekly_trips_i <- as.numeric(safe_val(input[[paste0("sa_car", i, "_trips_keep")]], 0))
            vehicle_vals <- calculate_vehicle_cost(ft, mi, weekly_trips_i)
            total_cost <- total_cost + vehicle_vals$total
          }
          total_cost <- total_cost + calculate_carlite_cost_numeric(as.numeric(safe_val(input[["sa_carlite_trips"]], 0)))$total
          total_cost <- total_cost + calculate_carfree_cost_numeric(as.numeric(safe_val(input[["sa_carfree_trips"]], 0)))$total
        }
        
        paste0("£", formatC(as.numeric(total_cost), format = "f", big.mark = ",", digits = 0))
      })
    })
    
    # ---------- CSS and scenario summary (unchanged structure) ----------
    css_styles <- tags$style(HTML("
    .combined-info-box { margin: 0 0 18px 0; padding: 14px; background-color: #f8f9fa;
                         border-left: 5px solid #007bff; border-radius: 4px; }
    .summary-pill { display: flex; justify-content: space-between; align-items: center;
                    padding: 10px 12px; border-radius: 8px; margin-bottom: 8px;
                    box-shadow: 0 1px 3px rgba(0,0,0,0.06); }
    .summary-pill .label { font-weight: 700; color: #495057; }
    .summary-pill .value { font-size: 18px; font-weight: 800; }
    .summary-note { font-size: 12px; color: #6c757d; text-align: center; margin-top: 6px; }
  "))
    
    # small helper to safely build vehicle header text (guaranteed length-1)
    build_vehicle_header <- function(i) {
      fuel <- safe_val(input[[paste0("car", i, "_fuel")]])
      type <- safe_val(input[[paste0("car", i, "_type")]])
      mileage <- safe_val(input[[paste0("car", i, "_mileage")]], "")
      desc <- paste0(trimws(paste(fuel, type)), if (nzchar(mileage)) paste0(" (", mileage, ")") else "")
      paste0("Vehicle ", i, ": ", desc)
    }
    
    # scenario/context box (unchanged)
    scenario_context_box <- div(
      class = "combined-info-box",
      h4("In this scenario, how would your household adapt travel over the next 3 years?"),
      fluidRow(
        column(width = 6,
               div(class = "scenario-section",
                   h5("Leeds 2030 Scenario:"),
                   tags$table(class = "table table-sm table-borderless", style = "margin-bottom: 0;",
                              tags$tbody(
                                tags$tr(tags$td(icon("bus"), strong("Mobility network:")), tags$td("Bus/tram/train in Leeds runs 24/7 all week")),
                                tags$tr(tags$td(icon("check-circle"), strong("Car club:")), tags$td("A car-club is organised in your neighbourhood."))
                              )
                   )
               )
        ),
        column(width = 6,
               div(class = "summary-section",
                   h5("Your Travel Summary"),
                   div(class = "summary-pill", style = "background: linear-gradient(90deg,#e9f7ef,#fff); border: 1px solid #d4edda;",
                       div(class = "label", "Total Weekly Trips"),
                       div(class = "value", textOutput("trips_summary_val2", inline = TRUE))
                   ),
                   div(class = "summary-pill", style = "background: linear-gradient(90deg,#fff4e6,#fff); border: 1px solid #ffeeba;",
                       div(class = "label", "Est. Monthly Cost"),
                       div(class = "value", textOutput("cost_summary_val2", inline = TRUE))
                   ),
                   div(class = "summary-note",
                       tags$strong("Live update:"), " changing sliders or choices immediately updates trips and cost."
                   )
               )
        )
      )
    )
    
    # create tables (zero-car or multi-vehicle) similar to previous block
    create_multi_vehicle_table <- function() {
      num_vehicles <- switch(safe_val(input$num_cars, "1"), "1" = 1, "2" = 2, "3" = 3, "4+" = 4, 1)
      attr_width <- 12
      vehicle_width <- round((88 - attr_width) / (num_vehicles + 2), 2)
      
      vehicle_headers <- lapply(seq_len(num_vehicles), function(i) {
        tags$th(style = paste0("width: ", vehicle_width, "%; background: linear-gradient(135deg, #dc3545 0%, #bd2130 100%); color: white; text-align: center;"),
                build_vehicle_header(i))
      })
      
      vehicle_access_cells <- lapply(seq_len(num_vehicles), function(i) {
        tags$td(style = "text-align: center; background-color: #f8d7da;", "At home")
      })
      vehicle_avail_cells <- lapply(seq_len(num_vehicles), function(i) {
        tags$td(style = "text-align: center; background-color: #f8d7da;", "90% (10% garage)")
      })
      vehicle_pt_cells <- lapply(seq_len(num_vehicles), function(i) {
        tags$td(style = "text-align: center; background-color: #f8d7da;", "—")
      })
      vehicle_cost_cells <- lapply(seq_len(num_vehicles), function(i) {
        # show fixed (ownership) cost; safe numeric conversion
        val <- calculate_vehicle_cost(safe_val(input[[paste0("car", i, "_fuel")]]),
                                      safe_val(input[[paste0("car", i, "_mileage")]]), 0)$fixed
        tags$td(style = "text-align: center; background-color: #f8d7da; font-weight: bold;", paste0("£", as.integer(val)))
      })
      vehicle_trip_cells <- lapply(seq_len(num_vehicles), function(i) {
        tags$td(style = "text-align: center; background-color: #f8d7da; padding: 10px;",
                div(class = "slider-container",
                    sliderInput(paste0("sa_car", i, "_trips_keep"), NULL, min = 0, max = 21, value = 10, step = 1)))
      })
      
      wellPanel(
        tags$table(
          class = "table table-bordered",
          style = "width: 100%; margin: 15px 0; border-collapse: separate; border-spacing: 0;",
          tags$thead(
            tags$tr(
              tags$th(style = paste0("width: ", attr_width, "%; background: linear-gradient(135deg, #343a40 0%, #212529 100%); color: white; text-align: center; font-weight: bold;"), "Attribute"),
              do.call(tagList, vehicle_headers),
              tags$th(style = paste0("width: ", vehicle_width, "%; background: linear-gradient(135deg, #007bff 0%, #0056b3 100%); color: white; text-align: center;"), "Car-lite"),
              tags$th(style = paste0("width: ", vehicle_width, "%; background: linear-gradient(135deg, #ffc107 0%, #d39e00 100%); color: black; text-align: center;"), "Car-free")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(style = "background-color: #f8f9fa; font-weight: bold;", "Access Time"),
              do.call(tagList, vehicle_access_cells),
              tags$td(style = "text-align: center; background-color: #cce7ff;", div(style = "padding:6px;", selectInput("sa_carlite_access", NULL,
                                                                                                                        choices = c("At home" = "at_home", "5 min walk" = "5_min", "10 min walk" = "10_min"),
                                                                                                                        selected = "10_min", width = "100%"))),
              tags$td(style = "text-align: center; background-color: #fff3cd;", div(style = "padding:6px;", selectInput("sa_carfree_access", NULL,
                                                                                                                        choices = c("At home" = "at_home", "5 min walk" = "5_min", "10 min walk" = "10_min"),
                                                                                                                        selected = "5_min", width = "100%")))
              
            ),
            tags$tr(
              tags$td(style = "background-color: #f8f9fa; font-weight: bold;", "Availability"),
              do.call(tagList, vehicle_avail_cells),
              tags$td(style = "text-align: center; background-color: #cce7ff;", div(style = "padding:6px;", selectInput("sa_carlite_availability", NULL,
                                                                                                                        choices = c("PT: 90% on-time" = "pt_90_on_time",
                                                                                                                                    "95% (5% garage)" = "95_garage",
                                                                                                                                    "80% (shared schedule)" = "80_shared",
                                                                                                                                    "Custom (specify later)" = "custom"),
                                                                                                                        selected = "pt_90_on_time", width = "100%"))),
              tags$td(style = "text-align: center; background-color: #fff3cd;", div(style = "padding:6px;", selectInput("sa_carfree_availability", NULL,
                                                                                                                        choices = c("99% available" = "99_avail",
                                                                                                                                    "90% available" = "90_avail",
                                                                                                                                    "75% available" = "75_avail",
                                                                                                                                    "Custom (specify later)" = "custom"),
                                                                                                                        selected = "99_avail", width = "100%")))
              
            ),
            tags$tr(
              tags$td(style = "background-color: #f8f9fa; font-weight: bold;", "PT pass"),
              do.call(tagList, vehicle_pt_cells),
              tags$td(style = "text-align: center; background-color: #cce7ff;", div(style = "padding:6px;", selectInput("sa_carlite_ptpass", NULL,
                                                                                                                        choices = c("No" = "no", "Yes" = "yes"),
                                                                                                                        selected = "no", width = "100%"))),
              tags$td(style = "text-align: center; background-color: #fff3cd;", div(style = "padding:6px;", selectInput("sa_carfree_ptpass", NULL,
                                                                                                                        choices = c("No" = "no", "Yes" = "yes"),
                                                                                                                        selected = "no", width = "100%")))
              
            ),
            tags$tr(
              tags$td(style = "background-color: #f8f9fa; font-weight: bold;", "Monthly Cost"),
              do.call(tagList, vehicle_cost_cells),
              tags$td(style = "text-align: center; background-color: #cce7ff; font-weight: bold; vertical-align: middle;", textOutput("sa_carlite_monthly_cost", inline = TRUE)),
              tags$td(style = "text-align: center; background-color: #fff3cd; font-weight: bold; vertical-align: middle;", textOutput("sa_carfree_monthly_cost", inline = TRUE))
            ),
            tags$tr(
              tags$td(style = "background-color: #f8f9fa; font-weight: bold; vertical-align: middle;", "Weekly Trips"),
              do.call(tagList, vehicle_trip_cells),
              tags$td(style = "text-align: center; background-color: #cce7ff; padding: 10px;", div(class = "slider-container", sliderInput("sa_carlite_trips", NULL, min = 0, max = 21, value = 5, step = 1))),
              tags$td(style = "text-align: center; background-color: #fff3cd; padding: 10px;", div(class = "slider-container", sliderInput("sa_carfree_trips", NULL, min = 0, max = 21, value = 2, step = 1)))
            )
          )
        )
      )
    }
    
    create_zero_car_adaptation_block <- function() {
      local({
        safe_val_local <- function(x, fallback = "") {
          if (is.null(x)) return(fallback)
          x_chr <- as.character(x)
          if (length(x_chr) == 0) return(fallback)
          x_chr[1]
        }
        
        # helpers (reuse logic you already have)
        get_weekly_trips <- function(mode) {
          sa_id <- switch(mode,
                          "none" = "sa_0_trips_none",
                          "pt" = "sa_0_trips_pt",
                          "carshare" = "sa_0_trips_carshare")
          defaults <- list(none = 3, pt = 8, carshare = 2)
          if (!is.null(input[[sa_id]])) return(as.numeric(safe_val_local(input[[sa_id]], defaults[[mode]])))
          # fallback: guess from simple commute indicator if present (car0_* inputs)
          commute <- safe_val_local(input[["car0_Commute"]], "")
          if (mode == "none" && commute == "Active travel (walking / cycling)") return(6)
          if (mode == "pt"   && commute == "Public transport") return(12)
          if (mode == "carshare" && commute == "Car-sharing") return(4)
          return(defaults[[mode]])
        }
        
        get_pt_access <- function() {
          if (!is.null(input[["sa_0_access_pt"]])) return(safe_val_local(input[["sa_0_access_pt"]]))
          commute <- safe_val_local(input[["car0_Commute"]], "")
          if (commute == "Public transport") return("5_min")
          return("10_min")
        }
        
        get_carshare_access <- function() {
          if (!is.null(input[["sa_0_access_carshare"]])) return(safe_val_local(input[["sa_0_access_carshare"]]))
          near <- safe_val_local(input[["car0_CarClubNear"]], "")
          if (near == "Yes") return("5_min")
          return("10_min")
        }
        
        get_carshare_availability <- function() {
          if (!is.null(input[["sa_0_availability_carshare"]])) return(safe_val_local(input[["sa_0_availability_carshare"]]))
          near <- safe_val_local(input[["car0_CarClubNear"]], "")
          if (near == "Yes") return("99_avail")
          return("none")
        }
        
        get_pt_pass_flag <- function() {
          if (!is.null(input[["sa_0_ptpass_pt"]])) return(safe_val_local(input[["sa_0_ptpass_pt"]]))
          pay <- safe_val_local(input[["car0_PayForPT"]], "")
          if (grepl("Weekly|Monthly", pay, ignore.case = TRUE)) return("yes")
          return("no")
        }
        
        get_carclub_member_flag <- function() {
          if (!is.null(input[["sa_0_ptpass_carshare"]])) return(safe_val_local(input[["sa_0_ptpass_carshare"]]))
          member <- safe_val_local(input[["car0_CarClubMember"]], "")
          if (member == "Yes") return("yes")
          return("no")
        }
        
        # monthly cost outputs (use your existing helpers if present)
        output[["sa_0_monthly_pt"]] <- renderText({
          vals <- NULL
          weekly_pt <- get_weekly_trips("pt")
          if (exists("calculate_carlite_cost_numeric", where = globalenv())) {
            vals <- tryCatch(calculate_carlite_cost_numeric(weekly_pt), error = function(e) NULL)
          }
          if (!is.null(vals) && is.list(vals)) {
            paste0("£", formatC(vals$total, format = "f", digits = 0))
          } else {
            # fallback
            acc <- get_pt_access()
            mult_acc <- switch(acc, "at_home" = 1.0, "5_min" = 1.05, "10_min" = 1.10, 1.0)
            pt_cost_pt <- if (get_pt_pass_flag() == "yes") 50 else 0
            paste0("£", round(50 * mult_acc + pt_cost_pt))
          }
        })
        
        output[["sa_0_monthly_carshare"]] <- renderText({
          weekly_cs <- get_weekly_trips("carshare")
          vals <- NULL
          if (exists("calculate_carfree_cost_numeric", where = globalenv())) {
            vals <- tryCatch(calculate_carfree_cost_numeric(weekly_cs), error = function(e) NULL)
          }
          member_flag <- get_carclub_member_flag()
          member_discount <- if (member_flag == "yes") 0.8 else 1.0
          if (!is.null(vals) && is.list(vals)) {
            total <- round(vals$total * member_discount, 0)
            paste0("£", formatC(total, format = "f", digits = 0))
          } else {
            carshare_base <- 25
            acc <- get_carshare_access()
            mult_acc <- switch(acc, "at_home" = 1.0, "5_min" = 1.05, "10_min" = 1.10, 1.0)
            avail <- get_carshare_availability()
            mult_avail <- switch(avail, "99_avail" = 0.95, "90_avail" = 1.0, "75_avail" = 1.2, "none" = 2.0, 1.0)
            paste0("£", round(carshare_base * mult_avail * mult_acc * member_discount))
          }
        })
        
        # dynamic labels for the radio (keep values stable)
        pt_has_pass  <- (get_pt_pass_flag() == "yes")
        cs_member    <- (get_carclub_member_flag() == "yes")
        pt_label     <- if (pt_has_pass) "Increase PT use" else "Add PT"
        cs_label     <- if (cs_member) "Use car-sharing more" else "Join car-sharing"
        
        # UI: full-width table mirroring multi-vehicle style but with 3 mode columns
        wellPanel(
          tags$table(
            class = "table table-bordered",
            style = "width: 100%; margin: 15px 0; border-collapse: separate; border-spacing: 0;",
            tags$thead(
              tags$tr(
                tags$th(style = "width: 15%; background: linear-gradient(135deg, #343a40 0%, #212529 100%); color: white; text-align: center; font-weight: bold;", "Attribute"),
                tags$th(style = "width: 28.33%; background: linear-gradient(135deg, #20c997 0%, #17a2b8 100%); color: white; text-align: center;", "Active travel (walk / cycle)"),
                tags$th(style = "width: 28.33%; background: linear-gradient(135deg, #007bff 0%, #0056b3 100%); color: white; text-align: center;", "Public transport"),
                tags$th(style = "width: 28.33%; background: linear-gradient(135deg, #ffc107 0%, #d39e00 100%); color: black; text-align: center;", "Car-sharing / taxis")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td(style = "background-color: #f8f9fa; font-weight: bold;", "Access Time"),
                tags$td(style = "text-align: center; background-color: #e9f7ef;", "At home / short walk"),
                tags$td(style = "text-align: center; background-color: #cce7ff;", div(style = "padding:6px;", 
                                                                                      if (!is.null(input[["sa_0_access_pt"]])) selectInput("sa_0_access_pt", NULL,
                                                                                                                                           choices = c("At home" = "at_home", "5 min walk" = "5_min", "10 min walk" = "10_min"),
                                                                                                                                           selected = input[["sa_0_access_pt"]], width = "100%") else
                                                                                                                                             tags$div(get_pt_access())
                )),
                tags$td(style = "text-align: center; background-color: #fff3cd;", div(style = "padding:6px;",
                                                                                      if (!is.null(input[["sa_0_access_carshare"]])) selectInput("sa_0_access_carshare", NULL,
                                                                                                                                                 choices = c("At home" = "at_home", "5 min walk" = "5_min", "10 min walk" = "10_min"),
                                                                                                                                                 selected = input[["sa_0_access_carshare"]], width = "100%") else
                                                                                                                                                   tags$div(get_carshare_access())
                ))
              ),
              tags$tr(
                tags$td(style = "background-color: #f8f9fa; font-weight: bold;", "Availability"),
                tags$td(style = "text-align: center; background-color: #e9f7ef;", "High (local routes)"),
                tags$td(style = "text-align: center; background-color: #cce7ff;", "PT: 90% on-time"),
                tags$td(style = "text-align: center; background-color: #fff3cd;", div(style = "padding:6px;",
                                                                                      if (!is.null(input[["sa_0_availability_carshare"]])) selectInput("sa_0_availability_carshare", NULL,
                                                                                                                                                       choices = c("99% available" = "99_avail", "90% available" = "90_avail", "75% available" = "75_avail", "Custom (specify later)" = "custom"),
                                                                                                                                                       selected = input[["sa_0_availability_carshare"]], width = "100%") else
                                                                                                                                                         tags$div(get_carshare_availability())
                ))
              ),
              tags$tr(
                tags$td(style = "background-color: #f8f9fa; font-weight: bold;", "Season pass / membership"),
                tags$td(style = "text-align: center; background-color: #e9f7ef;", "—"),
                tags$td(style = "text-align: center; background-color: #cce7ff;", div(style = "padding:6px;", 
                                                                                      if (!is.null(input[["sa_0_ptpass_pt"]])) selectInput("sa_0_ptpass_pt", NULL, choices = c("No" = "no", "Yes" = "yes"), selected = input[["sa_0_ptpass_pt"]], width = "100%") else
                                                                                        tags$div(get_pt_pass_flag())
                )),
                tags$td(style = "text-align: center; background: #fff3cd;", div(style = "padding:6px;",
                                                                                if (!is.null(input[["sa_0_ptpass_carshare"]])) selectInput("sa_0_ptpass_carshare", NULL, choices = c("No" = "no", "Yes" = "yes"), selected = input[["sa_0_ptpass_carshare"]], width = "100%") else
                                                                                  tags$div(get_carclub_member_flag())
                ))
              ),
              tags$tr(
                tags$td(style = "background-color: #f8f9fa; font-weight: bold;", "Estimated Monthly Cost"),
                tags$td(style = "text-align: center; background-color: #e9f7ef; font-weight: bold;", "£0"),
                tags$td(style = "text-align: center; background-color: #cce7ff; font-weight: bold;", textOutput("sa_0_monthly_pt", inline = TRUE)),
                tags$td(style = "text-align: center; background-color: #fff3cd; font-weight: bold;", textOutput("sa_0_monthly_carshare", inline = TRUE))
              ),
              tags$tr(
                tags$td(style = "background-color: #f8f9fa; font-weight: bold; vertical-align: middle;", "Weekly Trips"),
                tags$td(style = "text-align: center; background-color: #e9f7ef; padding: 10px;", div(class = "slider-container", sliderInput("sa_0_trips_none", NULL, min = 0, max = 40, value = get_weekly_trips("none"), step = 1))),
                tags$td(style = "text-align: center; background-color: #cce7ff; padding: 10px;", div(class = "slider-container", sliderInput("sa_0_trips_pt", NULL, min = 0, max = 40, value = get_weekly_trips("pt"), step = 1))),
                tags$td(style = "text-align: center; background-color: #fff3cd; padding: 10px;", div(class = "slider-container", sliderInput("sa_0_trips_carshare", NULL, min = 0, max = 40, value = get_weekly_trips("carshare"), step = 1)))
              ),
              tags$tr(
                tags$td(style = "background-color: #f8f9fa; font-weight: bold;", "Your Choice"),
                tags$td(colspan = 3, style = "text-align: center; background-color: #f0f1f2;",
                        # values: keep simple and explicit
                        radioButtons("sa_0_car_decision", NULL,
                                     choices = c("No change" = "no_changes", "Get a car" = "get_car", "Increase use of alternatives" = "increase_alts"),
                                     selected = if (!is.null(input[["num_cars_intend"]]) && input[["num_cars_intend"]] == "Add 1 vehicle") "get_car" else character(0),
                                     inline = TRUE))
              )
            )
          )
        ) # end wellPanel
      })
    }
    
  
  
    
    # assemble UI
    ui_elements <- list(css_styles, scenario_context_box)
    
    if (safe_val(input$num_cars, "0") == "0") {
      ui_elements <- append(ui_elements, list(create_zero_car_adaptation_block()))
    } else {
      ui_elements <- append(ui_elements, list(create_multi_vehicle_table()))
    }
    
    ui_elements <- append(ui_elements, list(hr(), actionButton("to_sa_button4", "Continue", class = "btn-success btn-lg")))
    
    do.call(tagList, ui_elements)
  })
  
  
  
  # ========= Version 3: Menu by sliders =========
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
        content: 'OWNED'; position: absolute; top: 0; right: 0; 
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
  
  # ========= VERSION 4: Menu Builder (REFACTORED) =========
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
             actionButton("to_sa_button5", "Continue to Next Section", class = "btn btn-success btn-lg")
    )
    )
  })
  
  # This UI placeholder now just creates the box where the text will go.
  # ========= VERSION 5 Different Menu =========
  
  output$sa_ui_placeholder6 <- renderUI({
    
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
                     hr(),
                     h3("What will your household transport look like in 5 years time?"),
                     p("Choose your preferred service levels across different transport options. Each service shows attribute trade-offs with associated costs.")
                 ),
                 
                 # PRIVATE VEHICLE OWNERSHIP - Revised Structure
                 create_service_section(
                   "Private Vehicle Ownership", 
                   paste0("Current fleet: ", input$num_cars, " vehicle", if(input$num_cars != "1") "s"),
                   if(input$num_cars != "0") {
                     # Calculate total cost of all current vehicles
                     total_current_cost <- 0
                     for(i in 1:switch(input$num_cars, "1"=1, "2"=2, "3"=3, "4+"=4)) {
                       total_current_cost <- total_current_cost + cost_func(input[[paste0("car", i, "_fuel")]], input[[paste0("car", i, "_mileage")]])
                     }
                     total_current_cost
                   } else {
                     0
                   },
                   "private",
                   tagList(
                     # Show all currently owned vehicles first
                     if (input$num_cars != "0") {
                       tagList(
                         h6("Current Vehicle Fleet", style = "margin: 0 0 10px 0; color: #495057; font-weight: bold;"),
                         lapply(1:switch(input$num_cars, "1"=1, "2"=2, "3"=3, "4+"=4), function(i) {
                           vehicle_cost <- cost_func(input[[paste0("car", i, "_fuel")]], input[[paste0("car", i, "_mileage")]])
                           create_attribute_option(
                             paste0("keep_vehicle_", i), 
                             paste("Vehicle", i, ":", input[[paste0("car", i, "_fuel")]], input[[paste0("car", i, "_type")]]), 
                             vehicle_cost, 0, "no change",
                             paste0("Continue with ", input[[paste0("car", i, "_mileage")]], " miles/year, current insurance level"),
                             is_default = TRUE
                           )
                         }),
                         
                         br(),
                         h6("Fleet Modifications", style = "margin: 15px 0 10px 0; color: #495057; font-weight: bold;")
                       )
                     },
                     
                     # Vehicle addition options (for all household types)
                     if (input$num_cars == "0") {
                       tagList(
                         h6("First Vehicle Acquisition", style = "margin: 0 0 10px 0; color: #495057; font-weight: bold;"),
                         create_attribute_option(
                           "add_used_petrol", "Used Petrol Car (5-8 years)", 0, 180, "first car",
                           "£8-12k purchase, comprehensive insurance, 8,000 miles/year typical"
                         ),
                         create_attribute_option(
                           "add_new_petrol", "New Petrol Car", 0, 285, "brand new",
                           "£18-25k purchase, full warranty, comprehensive insurance included"
                         ),
                         create_attribute_option(
                           "add_used_ev", "Used Electric Vehicle", 0, 195, "second-hand EV",
                           "£12-18k purchase, home charging installation, lower running costs"
                         ),
                         create_attribute_option(
                           "add_new_ev", "New Electric Vehicle", 0, 320, "new EV",
                           "£25-35k purchase, home charging, government grants, full warranty"
                         )
                       )
                     } else {
                       tagList(
                         # Replacement options
                         create_attribute_option(
                           "replace_oldest_ev", "Replace Oldest with Electric", 0, 45, "EV upgrade",
                           "Trade oldest vehicle for 2-3 year old EV, install home charging"
                         ),

                         # Addition options (if not at maximum)
                         if (input$num_cars %in% c("1", "2", "3")) {
                           create_attribute_option(
                             "add_second_car", paste("Add Additional Vehicle (+1 car)"), 0, 180, "expand fleet",
                             "Used car for additional household member or specific use (work van, small car etc.)"
                           )
                         },
                         
                         # Removal options (if have multiple cars)  
                         if (input$num_cars %in% c("2", "3", "4+")) {
                           tagList(
                             create_attribute_option(
                               "remove_one_car", paste("Remove One Vehicle (-1 car)"), 0, -150, "downsize fleet",
                               "Sell least essential vehicle, reduce insurance/tax costs, rely more on alternatives"
                             ),
                             if (input$num_cars %in% c("3", "4+")) {
                               create_attribute_option(
                                 "remove_two_cars", paste("Remove Two Vehicles (-2 cars)"), 0, -280, "major downsize",
                                 "Keep only most essential vehicle, major cost reduction, increase alternative transport use"
                               )
                             }
                           )
                         }
                       )
                     }
                   )
                 ),
                 
                 # CAR CLUB - Based on actual Leeds operators
                 create_service_section(
                   "Car Club Membership",
                   "Access to shared vehicles through Enterprise Car Club (Leeds city centre locations)", 
                   20,  # More realistic: £10 annual + usage costs
                   "carclub",
                   tagList(
                     h6("Membership Level", style = "margin: 0 0 10px 0; color: #495057; font-weight: bold;"),
                     create_attribute_option(
                       "carclub_basic", "Basic Annual (£10/year)", 20, 0, "£7.50/hour", 
                       "Access to cars in Wellington Place, Leeds General Infirmary, University - must book 1hr ahead", is_default = TRUE
                     ),
                     create_attribute_option(
                       "carclub_premium", "Plus Annual (£60/year)", 20, 50, "£6.50/hour",
                       "Reduced hourly rates, priority booking, access to larger vehicles and vans"
                     ),
                     
                     br(),
                     h6("Usage Frequency", style = "margin: 15px 0 10px 0; color: #495057; font-weight: bold;"),
                     create_attribute_option(
                       "carclub_occasional", "Occasional Use", 0, 15, "2-4 hrs/month",
                       "Mainly for shopping trips, airport runs, weekend outings - £15-30/month typical"
                     ),
                     create_attribute_option(
                       "carclub_regular", "Regular Use", 0, 45, "8-12 hrs/month", 
                       "Regular errands, commuting backup, leisure trips - £50-80/month typical"
                     ),
                     create_attribute_option(
                       "carclub_heavy", "Heavy Use", 0, 90, "20+ hrs/month",
                       "Primary transport solution replacing second car - £120+ monthly"
                     )
                   )
                 ),
                 
                 # PEER-TO-PEER - Realistic UK context  
                 create_service_section(
                   "Community Car Sharing",
                   "Share vehicles with neighbours through informal arrangements or platforms",
                   0,
                   "p2p",
                   tagList(
                     h6("Organisation Level", style = "margin: 0 0 10px 0; color: #495057; font-weight: bold;"),
                     create_attribute_option(
                       "p2p_informal", "Informal Neighbour Arrangement", 0, 0, "direct contact",
                       "Share costs directly with 2-3 nearby households, WhatsApp coordination", is_default = TRUE  
                     ),
                     create_attribute_option(
                       "p2p_hiyacar", "Peer-to-Peer Platform (Turo/HiyaCar)", 8, 12, "app booking",
                       "£8/month insurance, book local private cars through app, £20-40/day typical"
                     ),
                     create_attribute_option(
                       "p2p_community", "Organised Community Scheme", 15, 25, "managed sharing",
                       "Community organisation manages 4-6 shared vehicles, maintenance included"
                     ),
                     
                     br(),
                     h6("Vehicle Availability", style = "margin: 15px 0 10px 0; color: #495057; font-weight: bold;"),
                     create_attribute_option(
                       "p2p_limited", "One Shared Vehicle", 0, 0, "weekend priority", 
                       "Access to one vehicle shared among 4-5 households, mainly weekend availability"
                     ),
                     create_attribute_option(
                       "p2p_multiple", "Multiple Vehicle Options", 0, 15, "better access",
                       "Choice of 2-3 different vehicles, improved weekday availability"
                     )
                   )
                 ),
                 
                 # PUBLIC TRANSPORT - Real West Yorkshire pricing
                 create_service_section(
                   "Public Transport Access",
                   "Bus and rail travel across West Yorkshire using MCard system", 
                   35,
                   "public", 
                   tagList(
                     h6("Geographic Coverage", style = "margin: 0 0 10px 0; color: #495057; font-weight: bold;"),
                     create_attribute_option(
                       "pt_leeds", "Leeds City Zone", 35, 0, "£2 max single",
                       "All Leeds buses, local rail to Harrogate/York lines - £35/month unlimited", is_default = TRUE
                     ),
                     create_attribute_option(
                       "pt_wyca", "West Yorkshire Wide", 50, 15, "cross-county",
                       "All buses/Metro across Leeds, Bradford, Wakefield, Halifax, Huddersfield"
                     ),
                     create_attribute_option(
                       "pt_northern", "Plus Northern Rail", 50, 35, "regional trains", 
                       "WYCA pass plus discounted rail to Manchester, Sheffield, York"
                     ),
                     
                     br(),
                     h6("Service Reliability", style = "margin: 15px 0 10px 0; color: #495057; font-weight: bold;"),
                     create_attribute_option(
                       "pt_current", "Current Service Levels", 0, 0, "existing routes",
                       "Current bus frequencies, existing rail timetables, no service improvements"
                     ),
                     create_attribute_option(
                       "pt_improved", "Enhanced Service Package", 0, 20, "better frequency", 
                       "Bus rapid transit routes, increased frequencies on key corridors, real-time info"
                     )
                   )
                 ),
                 
                 # MICROMOBILITY & ACTIVE TRAVEL
                 create_service_section(
                   "Micromobility & Active Travel",
                   "Bike share, e-scooters, and cycling infrastructure in Leeds",
                   0,
                   "micro",
                   tagList(
                     h6("Bike Share Access", style = "margin: 0 0 10px 0; color: #495057; font-weight: bold;"),
                     create_attribute_option(
                       "micro_none", "No Bike Share", 0, 0, "own bike only",
                       "Rely on personal bicycle, no shared mobility subscriptions", is_default = TRUE
                     ),
                     create_attribute_option(
                       "micro_donkey", "Donkey Republic Membership", 0, 12, "£1/30min unlock",
                       "Access to docked e-bikes around Leeds city centre and university areas"
                     ),
                     create_attribute_option(
                       "micro_multiple", "Multiple Operator Access", 0, 25, "all platforms",
                       "Access to all available bike share and e-scooter operators in Leeds"
                     ),
                     
                     br(), 
                     h6("Infrastructure Investment", style = "margin: 15px 0 10px 0; color: #495057; font-weight: bold;"),
                     create_attribute_option(
                       "micro_current", "Current Infrastructure", 0, 0, "existing paths",
                       "Use existing cycle lanes and paths, current storage arrangements"
                     ),
                     create_attribute_option(
                       "micro_improved", "Enhanced Cycling Package", 0, 18, "better access",
                       "Secure parking at home/work, priority cycle lane network, maintenance support"
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
 
}