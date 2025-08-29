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
  
  
  # --- DATA PREPARATION: Defines the set of available modes ---
  mode_data <- reactive({
    req(input$num_cars)
    
    create_vehicle_desc <- function(car_num) {
      fuel <- input[[paste0("car", car_num, "_fuel")]]; type <- input[[paste0("car", car_num, "_type")]]
      if (is.null(fuel) || is.null(type)) return("Unknown Vehicle")
      paste0(tolower(fuel), " ", tolower(type))
    }
    
    modes <- list()
    num_cars <- as.integer(gsub("\\+", "", input$num_cars))
    if (is.na(num_cars)) num_cars <- 0
    
    if (num_cars > 0) {
      for (i in 1:min(num_cars, 4)) {
        modes[[length(modes) + 1]] <- list(id = paste0("current_", i), title = paste("Your", create_vehicle_desc(i)), icon = "car-side", is_current = TRUE,
                                           access = "Immediate, 24/7 at home", availability = "Very High (99% success rate)", cost_val = 350)
      }
    }
    
    modes[[length(modes) + 1]] <- list(id = "alt_1", title = "A New Configurable Car", icon = "car-alt", is_current = FALSE, is_configurable = TRUE,
                                       access = "Immediate, 24/7 at home", availability = "Highest (Brand New)", cost_val = NULL)
    # DIFFERENTIATED CAR SHARING
    modes[[length(modes) + 1]] <- list(id = "p2p_sharing", title = "Peer-to-Peer Car-Sharing", icon = "user-friends", is_current = FALSE,
                                       access = "Within a 10 min walk", availability = "High (95% success rate)", cost_val = 60)
    modes[[length(modes) + 1]] <- list(id = "car_club", title = "Professional Car Club", icon = "car-building", is_current = FALSE,
                                       access = "Within a 10 min walk", availability = "High (95% success rate)", cost_val = 85)
    modes[[length(modes) + 1]] <- list(id = "public_transport", title = "Yorkshire Pass (Public Transport)", icon = "bus-alt", is_current = FALSE,
                                       access = "Within a 5 min walk", availability = "Medium (90% success rate)", cost_val = 75)
    
    return(modes)
  })
  
  
  output$sa_ui_placeholder3 <- renderUI({
    
    modes <- mode_data()
    trip_purposes <- c("commute", "leisure", "other")
    trip_choices <- 0:10
    
    # --- Helper function to generate a single mode row with 3 dropdowns ---
    create_allocation_row <- function(mode, is_first_owned) {
      row_class <- if (mode$is_current) "allocation-row current-vehicle-row" else "allocation-row"
      cost_display <- if (is.null(mode$cost_val)) {
        tags$span(id="dynamic_cost_display", textOutput("dynamic_price_alt_1", inline = TRUE))
      } else { paste0("£", mode$cost_val) }
      title_cell_content <- if (!is.null(mode$is_configurable) && mode$is_configurable) {
        tags$div(
          tags$div(style = "display: flex; align-items: center; margin-bottom: 8px;", icon(mode$icon, class = "fa-2x", style="margin-right:15px;"), tags$strong(mode$title)),
          tags$div(style = "padding: 8px; border-radius: 6px; margin-top: 8px; background-color: #f8f9fa;",
                   selectInput("sa_alt_1_replace_type", "Type:", c("Car", "Van", "Motorbike"), width = "100%"),
                   selectInput("sa_alt_1_replace_fuel", "Fuel:", c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid"), width = "100%"),
                   selectInput("sa_alt_1_replace_mileage", "Mileage:", c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+"), width = "100%")
          )
        )
      } else {
        tags$div(style = "display: flex; align-items: center;", icon(mode$icon, class = "fa-2x", style="margin-right:15px;"), tags$strong(mode$title))
      }
      
      # Define initial state: first owned car gets all trips, others get 0
      initial_values <- c(0, 0, 0)
      
      
      tags$tr( class = row_class, `data-cost` = if(!is.null(mode$cost_val)) mode$cost_val else "", `data-dynamic-cost` = if(is.null(mode$cost_val)) "true" else "false",
               tags$td(style = "vertical-align: middle;", title_cell_content),
               tags$td(style = "text-align: center; vertical-align: middle;", mode$access),
               tags$td(style = "text-align: center; vertical-align: middle;", mode$availability),
               tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold;", cost_display),
               
               # --- NEW: Three dropdowns, one for each purpose ---
               tags$td(selectInput(paste0("share_commute_", mode$id), NULL, choices=trip_choices, selected=initial_values[1], width="80px")),
               tags$td(selectInput(paste0("share_leisure_", mode$id), NULL, choices=trip_choices, selected=initial_values[2], width="80px")),
               tags$td(selectInput(paste0("share_other_", mode$id), NULL, choices=trip_choices, selected=initial_values[3], width="80px"))
      )
    }
    
    
    # Generate table rows
    is_first_owned_car <- TRUE
    # Generate table rows
    table_rows <- lapply(modes, function(mode) {
      create_allocation_row(mode)
    })
    # Generate list of all dropdown IDs for JS
    dropdown_ids <- as.vector(sapply(trip_purposes, function(p) paste0("share_", p, "_", sapply(modes, `[[`, "id"))))
    
    
    
    # --- Initial share calculation (unchanged) ---
    num_modes <- length(modes); initial_shares <- rep(0, num_modes)
    num_owned <- sum(vapply(modes, function(m) m$is_current, logical(1)))
    if (num_owned > 0) {
      owned_share <- floor(70 / num_owned); other_share <- floor(30 / (num_modes - num_owned))
      for(i in 1:num_modes) { initial_shares[i] <- if(modes[[i]]$is_current) owned_share else other_share }
    } else { initial_shares <- rep(floor(100 / num_modes), num_modes) }
    initial_shares[num_modes] <- 100 - sum(initial_shares[1:(num_modes-1)])
    
    table_rows <- lapply(1:num_modes, function(i) create_allocation_row(modes[[i]], initial_shares[i]))
    slider_ids <- vapply(modes, function(m) paste0("share_", m$id), character(1))
    
    # --- Main UI Layout ---
    tagList(
      tags$style(HTML("
        .allocation-dropdowns td { text-align: center; vertical-align: middle; }
        /* Add specific styles for the new summary rows */
        .summary-row.purpose-total { font-size: 1em; padding: 8px 15px; background-color: #f8f9fa; }
        .summary-row.purpose-total:not(:last-child) { border-bottom: 1px solid #e9ecef; }
        .main-summary-cost { font-size: 1.2em; padding: 12px 15px; }
      ")),
      
      tags$script(HTML(paste0("
        (function() {
          const purposes = ['commute', 'leisure', 'other'];
          const dropdown_ids = ", jsonlite::toJSON(dropdown_ids), ";
          
          function updateAllDisplays() {
            let all_purposes_valid = true;
            let total_cost = 0;
            
            // 1. Validate each purpose column
            for (const purpose of purposes) {
              let purpose_sum = 0;
              $('select[id^=\"share_' + purpose + '\"]').each(function() {
                purpose_sum += Number($(this).val());
              });
              
              const summary_span = $('#' + purpose + '_summary_val');
              summary_span.text(purpose_sum + ' / 10');
              
              if (purpose_sum === 10) {
                summary_span.removeClass('total-error').addClass('total-ok');
              } else {
                summary_span.removeClass('total-ok').addClass('total-error');
                all_purposes_valid = false;
              }
            }
            
            // 2. Calculate total weighted cost
            $('.allocation-row').each(function() {
              const row = $(this);
              let total_trips_for_mode = 0;
              for (const purpose of purposes) {
                total_trips_for_mode += Number(row.find('select[id^=\"share_' + purpose + '\"]').val());
              }
              
              // Total trips is 30 (10 for each of 3 purposes)
              const share_fraction = total_trips_for_mode / 30.0;
              
              let cost = (row.data('dynamic-cost') === true)
                ? parseFloat($('#dynamic_cost_display').text().replace(/[^0-9.-]+/g, '')) || 0
                : Number(row.data('cost')) || 0;
              
              if (!isNaN(cost)) { total_cost += cost * share_fraction; }
            });
            
            $('#cost_summary_val').text('£' + total_cost.toFixed(2));
            
            // 3. Enable/disable continue button
            if (all_purposes_valid) {
              shinyjs.enable('to_sa_button4');
            } else {
              shinyjs.disable('to_sa_button4');
            }
          }
          
          // Listen for changes on any dropdown or the configurable car inputs
          $(document).on('shiny:inputchanged', function(event) {
            if (dropdown_ids.includes(event.name) || event.name.startsWith('sa_alt_1_replace')) {
               updateAllDisplays();
            }
          });

          // Run on initial load
          $(document).on('shiny:value', function(event) {
             if (event.target.id === 'sa_ui_placeholder3') {
                setTimeout(updateAllDisplays, 150);
             }
          });

        })();
      "))),
      
      # --- NEW: Full-width instruction box, now outside the two-column layout ---
      div(style = "margin: 0 0 30px 0; padding: 20px; background-color: #f8f9fa; border-left: 5px solid #007bff; border-radius: 4px;",
          tags$h4("Allocate Your Household's Trips by Purpose"),
          tags$p("Based on the scenario, for every 10 trips of a certain type, how would you allocate them across the modes? Each column must sum to 10.")
      ),
      
      # --- Two-column page structure ---
      fluidRow(
        
        # --- LEFT COLUMN: Main Content (now just the table and button) ---
        column(width = 8,
               tags$div(style = "overflow-x: auto;",
                        tags$table(class = "table choice-table table-bordered allocation-dropdowns",
                                   tags$thead(tags$tr(
                                     tags$th("Mode Option", style="min-width: 250px;"), tags$th("Access"), tags$th("Availability"), tags$th("Full Cost"),
                                     tags$th("Commute Trips (out of 10)"), tags$th("Leisure Trips (out of 10)"), tags$th("Other Trips (out of 10)")
                                   )),
                                   tags$tbody(table_rows)
                        )
               ),
               tags$div(style = "margin-top: 30px; text-align: center;",
                        actionButton("to_sa_button4", "Continue to Next Section", class = "btn btn-success btn-lg")
               )
        ),
        
        # --- RIGHT COLUMN: Sticky Sidebar (now contains scenario and summary) ---
        column(width = 4,
               div(class="sticky-sidebar",
                   
                   # MOVED: Scenario Box is now the first item in the sidebar
                   div(class = "scenario-info",
                       h5("Leeds 2030 Scenario:", style = "margin-top: 0"),
                       h5("Here I am listing plausible options for our scenario:", style = "margin-top: 0"),
                       tags$table( class="table table-sm table-borderless", style="margin-bottom: 0;",
                                   tags$tbody(
                                     tags$tr(tags$td(strong(" OLD ideas "))),
                                     
                                     tags$tr(tags$td(icon("gas-pump"), " Fuel prices:"), tags$td("+60p/litre")),
                                     tags$tr(tags$td(icon("parking"), " Parking:"), tags$td("+20% permits")),
                                     tags$tr(tags$td(icon("bus"), " Public transport:"), tags$td("New integrated passes")),
                                    
                                     tags$tr(tags$td(strong(" NEW ideas "))),
                                     
                                     tags$tr(tags$td(icon("bus"), 
                                                     strong("Mobility network:")), 
                                             tags$td("You can now plan and pay for all public transport (bus, train, tram) in West Yorkshire in a single app. ")),
                                     
                                     tags$tr(tags$td(icon("check-circle"), 
                                                     strong("E-bike hire:")), 
                                             tags$td("You can easily hire (bikes, e-bikes, scooters) in West Yorkshire.")),
                                     
                                     tags$tr(tags$td(icon("pound-sign"), 
                                                     strong("City Mobility Charge:")), 
                                             tags$td(" A new charge of £50 per month is applied to each privately owned car in your household.")),
                                     
                                     tags$tr(tags$td(icon("map-marked-alt"), 
                                                     strong("City Access:")), 
                                             tags$td(" The City Centre is now a restricted zone, charging most petrol/diesel cars for entry."))
                                     )
                       )
                   ),
                   
                   # The summary box remains as the second item
                   h4("Your Portfolio Summary", style="text-align:center; margin-bottom:15px"),
                   
                   div(class="summary-box",
                       # --- NEW: Per-purpose validation rows ---
                       div(class="summary-row purpose-total", "Commute Trips Allocated:", tags$span(id="commute_summary_val", "10 / 10")),
                       div(class="summary-row purpose-total", "Leisure Trips Allocated:", tags$span(id="leisure_summary_val", "10 / 10")),
                       div(class="summary-row purpose-total", "Other Trips Allocated:", tags$span(id="other_summary_val", "10 / 10")),
                       # --- Main cost summary ---
                       div(class="summary-row main-summary-cost", "Estimated Monthly Cost:", tags$span(id="cost_summary_val", "£..."))
                   )
                   
                   # div(class="summary-box",
                   #     div(id="allocation_summary", class="summary-row total-percent total-ok", "Total Allocated:", tags$span("100%")),
                   #     div(id="cost_summary", class="summary-row total-cost", "Estimated Monthly Cost:", tags$span("£..."))
                   # )
               )
        )
      )
    )
  })
  
  
  
  
  # ========= VERSION 3: CONFIGURATOR=========
  mode_data <- reactive({
    req(input$num_cars)
    
    create_vehicle_desc <- function(car_num) {
      fuel <- input[[paste0("car", car_num, "_fuel")]]; type <- input[[paste0("car", car_num, "_type")]]
      if (is.null(fuel) || is.null(type)) return("Unknown Vehicle")
      paste0(tolower(fuel), " ", tolower(type))
    }
    
    modes <- list()
    num_cars <- as.integer(gsub("\\+", "", input$num_cars))
    if (is.na(num_cars)) num_cars <- 0
    
    # 1. Add current vehicles from RP data
    if (num_cars > 0) {
      for (i in 1:min(num_cars, 4)) {
        modes[[length(modes) + 1]] <- list(id = paste0("current_", i), title = paste("Your", create_vehicle_desc(i)), icon = "car-side", is_current = TRUE,
                                           access = "Immediate, 24/7 at home", availability = "Very High (99% success rate)", cost_val = 350)
      }
    }
    
    # 2. Add alternative/configurable modes
    modes[[length(modes) + 1]] <- list(id = "alt_1", title = "A New Configurable Car", icon = "car-alt", is_current = FALSE, is_configurable = TRUE,
                                       access = "Immediate, 24/7 at home", availability = "Highest (Brand New)", cost_val = NULL)
    modes[[length(modes) + 1]] <- list(id = "car_sharing", title = "Peer-to-Peer Car-Sharing Membership", icon = "users", is_current = FALSE,
                                       access = "Within a 10 min walk", availability = "High (95% success rate)", cost_val = 60)
    modes[[length(modes) + 1]] <- list(id = "car_club", title = "Closed Loop Car-Club Membership", icon = "users", is_current = FALSE,
                                       access = "Within a 10 min walk", availability = "High (95% success rate)", cost_val = 60)
    modes[[length(modes) + 1]] <- list(id = "public_transport", title = "Yorkshire Pass: Covers All Public Transport", icon = "bus-alt", is_current = FALSE,
                                       access = "Within a 5 min walk", availability = "Medium (90% success rate)", cost_val = 75)
    
    return(modes)
  })
  
  
  output$sa_ui_placeholder4 <- renderUI({
    
    modes <- mode_data() # Get data from the reactive
    
    # --- Helper function to generate a single row (unchanged) ---
    create_allocation_row <- function(mode, initial_share) {
      row_class <- if (mode$is_current) "allocation-row current-vehicle-row" else "allocation-row"
      cost_display <- if (is.null(mode$cost_val)) {
        tags$span(id="dynamic_cost_display", textOutput("dynamic_price_alt_1", inline = TRUE))
      } else { paste0("£", mode$cost_val) }
      title_cell_content <- if (!is.null(mode$is_configurable) && mode$is_configurable) {
        tags$div(
          tags$div(style = "display: flex; align-items: center; margin-bottom: 8px;", icon(mode$icon, class = "fa-2x", style="margin-right:15px;"), tags$strong(mode$title)),
          tags$div(style = "padding: 8px; border-radius: 6px; margin-top: 8px; background-color: #f8f9fa;",
                   selectInput("sa_alt_1_replace_type", "Type:", c("Car", "Van", "Motorbike"), width = "100%"),
                   selectInput("sa_alt_1_replace_fuel", "Fuel:", c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid"), width = "100%"),
                   selectInput("sa_alt_1_replace_mileage", "Mileage:", c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+"), width = "100%")
          )
        )
      } else {
        tags$div(style = "display: flex; align-items: center;", icon(mode$icon, class = "fa-2x", style="margin-right:15px;"), tags$strong(mode$title))
      }
      tags$tr( class = row_class, `data-cost` = if(!is.null(mode$cost_val)) mode$cost_val else "", `data-dynamic-cost` = if(is.null(mode$cost_val)) "true" else "false",
               tags$td(style = "vertical-align: middle;", title_cell_content),
               tags$td(style = "text-align: center; vertical-align: middle; font-size: 0.9em;", mode$access),
               tags$td(style = "text-align: center; vertical-align: middle; font-size: 0.9em;", mode$availability),
               tags$td(style = "text-align: center; vertical-align: middle; font-weight: bold; font-size: 1.1em;", cost_display),
               tags$td(style = "vertical-align: middle; min-width: 350px;",
                       div(class = "slider-container",
                           span(class="slider-label-left", "I would not use at all"),
                           div(class = "allocation-cell",
                               sliderInput(paste0("share_", mode$id), NULL, min=0, max=100, value=initial_share, step=5, width="100%"),
                               span(class = "share-value-display", paste0(initial_share, "%"))
                           ),
                           span(class="slider-label-right", "I would only use this")
                       )
               )
      )
    }
    
    # --- Initial share calculation (unchanged) ---
    num_modes <- length(modes); initial_shares <- rep(0, num_modes)
    num_owned <- sum(vapply(modes, function(m) m$is_current, logical(1)))
    if (num_owned > 0) {
      owned_share <- floor(70 / num_owned); other_share <- floor(30 / (num_modes - num_owned))
      for(i in 1:num_modes) { initial_shares[i] <- if(modes[[i]]$is_current) owned_share else other_share }
    } else { initial_shares <- rep(floor(100 / num_modes), num_modes) }
    initial_shares[num_modes] <- 100 - sum(initial_shares[1:(num_modes-1)])
    
    table_rows <- lapply(1:num_modes, function(i) create_allocation_row(modes[[i]], initial_shares[i]))
    slider_ids <- vapply(modes, function(m) paste0("share_", m$id), character(1))
    
    # --- Main UI Layout ---
    tagList(
      tags$style(HTML("
        /* Your existing styles are fine. No changes needed. */
        .sticky-sidebar { position: sticky; top: 20px; }
        .scenario-info { background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 20px; border-radius: 8px; margin-bottom: 20px; }
        .current-vehicle-row td:first-child { position: relative; }
        .current-vehicle-row td:first-child::before { content: 'OWNED'; position: absolute; top: 0; right: 0; background-color: #ffd700; color: #333; padding: 2px 8px; border-radius: 0 0 0 4px; font-size: 0.7em; font-weight: bold; }
        .current-vehicle-row td { background-color: #fffef7 !important; }
        .slider-container { position: relative; padding: 20px 0; }
        .slider-label-left, .slider-label-right { position: absolute; top: 0; font-size: 0.8em; color: #666; }
        .slider-label-left { left: 0; } .slider-label-right { right: 0; }
        .allocation-cell { display: flex; align-items: center; }
        .allocation-cell .form-group { flex-grow: 1; margin: 0; }
        .share-value-display { font-weight: bold; font-size: 1.2em; margin-left: 15px; width: 50px; text-align: right; }
        .summary-box { border: 1px solid #dee2e6; border-radius: 8px; overflow: hidden; background-color: #fff; }
        .summary-row { padding: 12px 15px; font-size: 1.1em; font-weight: bold; display: flex; justify-content: space-between; align-items: center; }
        .summary-row.total-percent { background-color: #f8f9fa; border-bottom: 1px solid #dee2e6; transition: all 0.3s ease; }
        .summary-row.total-cost { font-size: 1.2em; }
        .total-ok { color: #155724; }
        .total-error { color: #721c24; }
      ")),
      
      # JavaScript is unchanged from the previous version
      # --- Full JavaScript Block ---
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
            cost_div.find('span').text('£' + total_cost.toFixed(2));

            // Enable/disable button based on validity
            if (current_sum === 100) {
              summary_div.removeClass('total-error').addClass('total-ok');
              shinyjs.enable('to_sa_button4');
            } else {
              summary_div.removeClass('total-ok').addClass('total-error');
              shinyjs.disable('to_sa_button4');
            }
          }
          
          const handleSliderChange = (event) => {
            const source_id = event.name;
            let total = slider_ids.reduce((sum, id) => {
              const slider = $('#' + id);
              return slider.length ? sum + Number(slider.val()) : sum;
            }, 0);

            let diff = total - 100;
            if (Math.abs(diff) < 0.1) { 
              updateAllDisplays(); 
              return; 
            }

            const other_sliders = slider_ids.filter(id => id !== source_id).map(id => $('#' + id));
            let total_adjustable = other_sliders.reduce((sum, s) => {
                if (!s.length) return sum;
                return sum + (diff > 0 ? Number(s.val()) : 100 - Number(s.val()));
            }, 0);

            if (total_adjustable > 0) {
              for (let s of other_sliders) {
                if (!s.length) continue;
                let current_val = Number(s.val());
                let capacity = (diff > 0) ? current_val : 100 - current_val;
                s.val(current_val - ((capacity / total_adjustable) * diff));
              }
            }
            
            let final_total = 0;
            for(const id of slider_ids.slice(0, -1)) { // All but the last
                const slider = $('#' + id);
                if (!slider.length) continue;
                let val = Math.round(Number(slider.val()));
                slider.val(val);
                final_total += val;
            }
            const last_slider = $('#' + slider_ids[slider_ids.length-1]);
            if(last_slider.length) {
              last_slider.val(100 - final_total);
            }
            
            updateAllDisplays();
          };
          
          $(document).on('shiny:inputchanged', function(event) {
            if (slider_ids.includes(event.name) || event.name.startsWith('sa_alt_1_replace')) {
               handleSliderChange(event); // Re-calculate for config changes too
            }
          });

          $(document).on('shiny:value', function(event) {
             if (event.target.id === 'sa_ui_placeholder3') {
                // Use a small delay to ensure all shiny outputs (like dynamic cost) are rendered first
                setTimeout(updateAllDisplays, 150);
             }
          });

        })();
      "))),
      
      # --- NEW: Full-width instruction box, now outside the two-column layout ---
      div(style = "margin: 0 0 30px 0; padding: 20px; background-color: #f8f9fa; border-left: 5px solid #007bff; border-radius: 4px;",
          tags$h4("Allocate Your Household's Trips", style = "margin-top: 0; color: #2c3e50;"),
          tags$p("Please read the scenario on the right, then use the sliders in the table to allocate your household's typical monthly trips across the available modes. The total must equal 100%.", style="margin-bottom:0;")
      ),
      
      # --- Two-column page structure ---
      fluidRow(
        
        # --- LEFT COLUMN: Main Content (now just the table and button) ---
        column(width = 8,
               tags$div(style = "overflow-x: auto;",
                        tags$table(class = "table choice-table table-bordered",
                                   tags$thead(tags$tr(tags$th("Mode Option"), tags$th("Access"), tags$th("Availability"), tags$th("Full Cost"), tags$th("Share of Trips (%)"))),
                                   tags$tbody(table_rows)
                        )
               ),
               
               tags$div(style = "margin-top: 30px; text-align: center;",
                        actionButton("to_sa_button4", "Continue to Next Section", class = "btn btn-success btn-lg")
               )
        ),
        
        # --- RIGHT COLUMN: Sticky Sidebar (now contains scenario and summary) ---
        column(width = 4,
               div(class="sticky-sidebar",
                   
                   # MOVED: Scenario Box is now the first item in the sidebar
                   div(class = "scenario-info",
                       h5("Leeds 2030 Scenario:", style = "margin-top: 0"),
                       h5("Here I am listing plausible options for our scenario:", style = "margin-top: 0"),
                       tags$table( class="table table-sm table-borderless", style="margin-bottom: 0;",
                                   tags$tbody(
                                     tags$tr(tags$td(strong(" OLD ideas "))),
                                     
                                     tags$tr(tags$td(icon("gas-pump"), " Fuel prices:"), tags$td("+60p/litre")),
                                     tags$tr(tags$td(icon("parking"), " Parking:"), tags$td("+20% permits")),
                                     tags$tr(tags$td(icon("bus"), " Public transport:"), tags$td("New integrated passes")),
                                     
                                     tags$tr(tags$td(strong(" NEW ideas "))),
                                     
                                     tags$tr(tags$td(icon("bus"), 
                                                     strong("Mobility network:")), 
                                             tags$td("You can now plan and pay for all public transport (bus, train, tram) in West Yorkshire in a single app. ")),
                                     
                                     tags$tr(tags$td(icon("check-circle"), 
                                                     strong("E-bike hire:")), 
                                             tags$td("You can easily hire (bikes, e-bikes, scooters) in West Yorkshire.")),
                                     
                                     tags$tr(tags$td(icon("pound-sign"), 
                                                     strong("City Mobility Charge:")), 
                                             tags$td(" A new charge of £50 per month is applied to each privately owned car in your household.")),
                                     
                                     tags$tr(tags$td(icon("map-marked-alt"), 
                                                     strong("City Access:")), 
                                             tags$td(" The City Centre is now a restricted zone, charging most petrol/diesel cars for entry."))
                                   )
                       )
                   ),
                   
                   # The summary box remains as the second item
                   h4("Your Portfolio Summary", style="text-align:center; margin-bottom:15px"),
                   div(class="summary-box",
                       div(id="allocation_summary", class="summary-row total-percent total-ok", "Total Allocated:", tags$span("100%")),
                       div(id="cost_summary", class="summary-row total-cost", "Estimated Monthly Cost:", tags$span("£..."))
                   )
               )
        )
      )
    )
  })
  
  
  
  
  
  
  # output$sa_ui_placeholder4 <- renderUI({
  #   
  #   # Gatekeeper
  #   req(input$num_cars)
  #   if (input$num_cars != "0") {
  #     req(input$car1_type, input$car1_fuel, input$car1_mileage)
  #   }
  #   
  #   # --- Helper function with corrected unique checkbox IDs ---
  #   create_choice_card <- function(car_number, title, choice_value_suffix, cost_text, specs_list, border_class, is_popular = FALSE) {
  #     
  #     checkbox_id <- paste0("sa3_car", car_number, "_", choice_value_suffix)
  #     
  #     tags$label(
  #       `for` = checkbox_id,
  #       style = "cursor: pointer; display: block; margin: 0;",
  #       div(
  #         class = paste("configurator-card", border_class),
  #         fluidRow(style="display: flex; align-items: center;",
  #                  column(8,
  #                         if (is_popular) div(class = "popular-flag", "Most Popular!"),
  #                         div(class = "card-header", h4(title)),
  #                         div(class = "card-specs",
  #                             lapply(names(specs_list), function(name) {
  #                               div(class = "spec-item",
  #                                   fluidRow(
  #                                     column(6, span(class = "spec-label", name)),
  #                                     column(6, span(specs_list[[name]]))
  #                                   )
  #                               )
  #                             })
  #                         )
  #                  ),
  #                  column(4, align="right",
  #                         div(class = "card-price", cost_text),
  #                         p("per month", style="margin-top: -5px; color: #666; margin-bottom: 10px;"),
  #                         div(class = "card-selection",
  #                             checkboxInput(checkbox_id, label="Select this Option")
  #                         )
  #                  )
  #         )
  #       )
  #     )
  #   }
  #   
  #   # --- Live cost calculation reactive (updated to read unique checkbox IDs) ---
  #   sa3_live_cost <- reactive({
  #     live_total <- 0
  #     req(input$num_cars)
  #     if (input$num_cars != "0") {
  #       num_vehicles <- switch(input$num_cars, "1"=1, "2"=2, "3"=3, "4+"=4)
  #       for (i in 1:num_vehicles) {
  #         cost_func <- function(fuel, mileage) {
  #           base_costs <- list("Petrol"=150*1.25, "Diesel"=160*1.25, "Fully Electric"=80, "Plug-in Hybrid"=120*1.15)
  #           parking <- switch(mileage, "0-2,000"=5, "2,001-5,000"=10, "5,001-10,000"=15, "10,001+"=20, 0)
  #           multiplier <- switch(mileage, "0-2,000"=0.6, "2,001-5,000"=1.0, "5,001-10,000"=1.5, "10,001+"=2.2, 1)
  #           return(round((base_costs[[fuel]] * multiplier) + (parking * 1.20), 0))
  #         }
  #         current_cost_new <- cost_func(input[[paste0("car", i, "_fuel")]], input[[paste0("car", i, "_mileage")]])
  #         
  #         # Now reads the unique IDs
  #         if (!is.null(input[[paste0("sa3_car", i, "_keep")]]) && input[[paste0("sa3_car", i, "_keep")]]) live_total <- live_total + current_cost_new
  #         if (!is.null(input[[paste0("sa3_car", i, "_replace_ev")]]) && input[[paste0("sa3_car", i, "_replace_ev")]]) live_total <- live_total + 210
  #         if (!is.null(input[[paste0("sa3_car", i, "_use_my_days")]]) && input[[paste0("sa3_car", i, "_use_my_days")]]) live_total <- live_total + 75
  #         if (!is.null(input[[paste0("sa3_car", i, "_use_p2p")]]) && input[[paste0("sa3_car", i, "_use_p2p")]]) live_total <- live_total + 25
  #         if (!is.null(input[[paste0("sa3_car", i, "_use_pt")]]) && input[[paste0("sa3_car", i, "_use_pt")]]) live_total <- live_total + 50
  #       }
  #     }
  #     return(live_total)
  #   })
  #   
  #   # --- UI element to render the live cost table row ---
  #   output$sa3_live_cost_display <- renderUI({
  #     tags$tr(
  #       style = "border-top: 0px solid #ccc;",
  #       tags$td(icon("money-check-dollar"), strong(" Selected Portfolio Cost: ")),
  #       tags$td(style="font-weight:bold; font-size: 1.1em;", paste0(" £", format(sa3_live_cost(), nsmall = 2), " per month"))
  #     )
  #   })
  #   
  #   # --- CORRECTED Scenario Header Box ---
  #   scenario_header <- div(style="text-align: center; margin-bottom: 40px;",
  #                          h2("Imagine this scenario in Leeds..."),
  #                          p("Consider the following changes and then configure your new household travel plan below."),
  #                          wellPanel(
  #                            style = "background-color: #f8f9fa; border: 1px solid #dee2e6; display: inline-block; text-align: left; max-width: 800px; padding: 15px;",
  #                            tags$table(
  #                              class="table table-sm", style="margin-bottom: 15px;",
  #                              tags$tbody(
  #                                tags$tr(
  #                                  tags$td(icon("gas-pump"), strong(" Fuel prices:")),
  #                                  tags$td("+60p per litre")
  #                                ),
  #                                tags$tr(
  #                                  tags$td(icon("parking"), strong(" Parking permit costs:")),
  #                                  tags$td("+20%")
  #                                ),
  #                                tags$tr(
  #                                  tags$td(icon("bus"), strong(" Public Transport:")),
  #                                  tags$td("A new 'Leeds Travel Pass' is available for £50/month")
  #                                ),
  #                                tags$tr(
  #                                  tags$td(icon("car"), strong(" Car Club:")),
  #                                  tags$td("New sharing models are available, see options below.")
  #                                )
  #                              )
  #                            ),
  #                            hr(),
  #                            tags$table(
  #                              class="table table-sm", style="margin-bottom: 0;",
  #                              tags$tbody(
  #                                uiOutput("sa3_live_cost_display")
  #                              )
  #                            ),
  #                            hr()
  #                          )
  #   )
  #   
  #   # --- Main UI Layout ---
  #   tagList(
  #     tags$style(HTML("
  #       /* CSS is unchanged */
  #       .configurator-card { background-color:#fff; border:1px solid #e9ecef; border-left-width:7px; border-radius:8px; padding:20px; margin-bottom:25px; box-shadow:0 4px 8px rgba(0,0,0,0.05); transition:all .2s ease-in-out; }
  #       .configurator-card:hover { transform: translateY(-3px); box-shadow: 0 8px 16px rgba(0,0,0,0.1); }
  #       .card-border-car { border-left-color: #007bff; } .card-border-car h4 { color: #007bff; }
  #       .card-border-shared { border-left-color: #28a745; } .card-border-shared h4 { color: #28a745; }
  #       .card-border-managed { border-left-color: #17a2b8; } .card-border-managed h4 { color: #17a2b8; }
  #       .card-border-pt { border-left-color: #6f42c1; } .card-border-pt h4 { color: #6f42c1; }
  #       .popular-flag { background-color:#ffc107; color:#343a40; padding:4px 12px; border-radius:5px; font-weight:bold; font-size:.9em; display:inline-block; margin-bottom:15px; }
  #       .card-price { font-size:1.8em; font-weight:bold; color:#333; margin-bottom:0; }
  #       .card-selection .checkbox { margin-top: 5px !important; }
  #       .checkbox input[type=checkbox] { transform: scale(1.5); }
  #       .card-header h4 { margin-top:0; }
  #       .card-specs { margin-top:15px; font-size:.95em; }
  #       .spec-item { margin-bottom:8px; }
  #       .spec-label { font-weight:bold; }
  #     ")),
  #     
  #     scenario_header,
  #     
  #     sidebarLayout(
  #       sidebarPanel(
  #         width = 3,
  #         h4("What is your primary goal?"),
  #         p("Select the option that best describes your main intention for your currently owned vehicle(s)."),
  #         radioButtons("sa3_initial_decision", label=NULL,
  #                      choices=c("Keep my current vehicle(s)",
  #                                "Replace my vehicle(s) with something else",
  #                                "Dispose of my vehicle(s)"))
  #       ),
  #       
  #       mainPanel(
  #         width = 9,
  #         h3("Please select one or more packages to build your household's new travel portfolio"),
  #         if (input$num_cars != "0") {
  #           lapply(1:switch(input$num_cars, "1"=1, "2"=2, "3"=3, "4+"=4), function(i) {
  #             
  #             current_car_cost <- reactive({
  #               cost_func <- function(fuel, mileage) {
  #                 base_costs <- list("Petrol"=150*1.25, "Diesel"=160*1.25, "Fully Electric"=80, "Plug-in Hybrid"=120*1.15)
  #                 parking <- switch(mileage, "0-2,000"=5, "2,001-5,000"=10, "5,001-10,000"=15, "10,001+"=20, 0)
  #                 multiplier <- switch(mileage, "0-2,000"=0.6, "2,001-5,000"=1.0, "5,001-10,000"=1.5, "10,001+"=2.2, 1)
  #                 return(round((base_costs[[fuel]] * multiplier) + (parking * 1.20), 0))
  #               }
  #               cost_func(input[[paste0("car", i, "_fuel")]], input[[paste0("car", i, "_mileage")]])
  #             })
  #             
  #             tagList(
  #               h4(paste("Options related to your", input[[paste0("car", i, "_fuel")]], input[[paste0("car", i, "_type")]])),
  #               # RESTORED: Full spec lists and corrected checkbox IDs
  #               create_choice_card(i, paste("Keep Your Current", input[[paste0("car", i, "_fuel")]], "Car"), "keep", paste0("£", current_car_cost()), 
  #                                  specs_list = list("Travel Time" = "No change", "Reliability" = "High", "Availability" = "Immediate, 24/7"), 
  #                                  border_class = "card-border-car"),
  #               create_choice_card(i, "Replace with a new Electric Vehicle", "replace_ev", "£210", 
  #                                  specs_list = list("Travel Time" = "Similar to current", "Reliability" = "Very High", "Availability" = "Immediate, 24/7"), 
  #                                  border_class = "card-border-car"),
  #               create_choice_card(i, "'My Days' Dedicated Vehicle", "use_my_days", "£75", 
  #                                  specs_list = list("Provider" = "Enterprise", "Reliability" = "Very High (on your days)", "Availability" = "Scheduled days only"), 
  #                                  border_class = "card-border-managed", is_popular=TRUE),
  #               create_choice_card(i, "Closed Loop Peer-to-Peer Sharing", "use_p2p", "£25", 
  #                                  specs_list = list("Provider" = "Neighbourhood group", "Reliability" = "Medium (depends on group)", "Availability" = "By arrangement with group"), 
  #                                  border_class = "card-border-shared"),
  #               create_choice_card(i, "Public Transport Pass", "use_pt", "£50", 
  #                                  specs_list = list("Travel Time" = "Varies with schedule", "Reliability" = "Medium", "Availability" = "Scheduled"), 
  #                                  border_class = "card-border-pt")
  #             )
  #           })
  #         }
  #       )
  #     ),
  #     hr(),
  #     div(align="center",
  #         actionButton("to_sa_button5", "Continue", class="btn-primary btn-lg")
  #     )
  #   )
  # })
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
                     actionButton("to_sa_button5", "Continue with Portfolio", class="btn-primary btn-lg", style="margin: 20px 0;")
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

  # ========= VERSION 4: Menu Builder (REFACTORED) =========
  
  output$sa_ui_placeholder5 <- renderUI({
    
    # --- PREREQUISITES ---
    req(input$num_cars)
    if (input$num_cars != "0") {
      req(input$car1_type, input$car1_fuel, input$car1_mileage)
    }
    
    "%||%" <- function(a, b) if (!is.null(a)) a else b
    
    cost_func <- function(fuel, mileage, veh_type = "Car") {
      base_costs <- list("Petrol" = 150, "Diesel" = 160, "Fully Electric" = 80, "Plug-in Hybrid" = 120)
      mnorm <- gsub("[^0-9-]", "", mileage)
      mileage_multiplier <- switch(mnorm, "0-2000" = 0.6, "2001-5000" = 1.0, "5001-10000" = 1.5, "10001+" = 2.2, 1.0)
      type_multiplier <- switch(veh_type, "Car" = 1.0, "Van" = 1.35, "Motorbike" = 0.55, 1.0)
      parking <- switch(mnorm, "0-2000" = 5, "2001-5000" = 10, "5001-10000" = 15, "10001+" = 20, 0)
      total_base <- (base_costs[[fuel]] %||% 150) * type_multiplier
      fixed_cost <- total_base * 0.4 
      variable_cost <- (total_base * 0.6 * mileage_multiplier) + (parking * 1.2)
      return(list(fixed = round(fixed_cost), variable = round(variable_cost), total = round(fixed_cost + variable_cost)))
    }
    
    n_veh <- switch(input$num_cars, "0" = 0, "1" = 1, "2" = 2, "3" = 3, "4+" = 4)
    
    per_car_costs_list <- lapply(seq_len(n_veh), function(i) {
      fuel_i <- input[[paste0("car", i, "_fuel")]] %||% "Petrol"; type_i <- input[[paste0("car", i, "_type")]] %||% "Car"
      mileage_i <- input[[paste0("car", i, "_mileage")]] %||% "5,001 - 10,000"
      cost_func(fuel_i, mileage_i, type_i)
    })
    per_car_descs <- sapply(seq_len(n_veh), function(i) {
      paste0("Your ", tolower(input[[paste0("car", i, "_fuel")]] %||% "petrol"), " ", 
             tolower(input[[paste0("car", i, "_type")]] %||% "car"), ".")
    })
    per_car_fixed_costs <- if(n_veh > 0) sapply(per_car_costs_list, `[[`, "fixed") else numeric(0)
    per_car_variable_costs <- if(n_veh > 0) sapply(per_car_costs_list, `[[`, "variable") else numeric(0)
    per_car_total_costs <- if(n_veh > 0) sapply(per_car_costs_list, `[[`, "total") else numeric(0)
    
    total_current_cost <- if (n_veh > 0) sum(per_car_total_costs) else 0
    scenario_budget <- max(300, total_current_cost * 1.2)
    
    panels <- if (n_veh > 0) {
      lapply(seq_len(n_veh), function(i) {
        bsCollapsePanel(
          title = paste0("Vehicle ", i, " — £", per_car_total_costs[i], "/m"),
          value = paste0("vehicle_panel_", i),
          tags$div( style = "padding:8px 4px;",
                    tags$p(per_car_descs[i], style = "margin:0 0 8px 0; color:#444;"),
                    fluidRow(
                      column(4, radioButtons(inputId = paste0("car", i, "_keep"), label = "Decision:", choices = list("Keep" = 1, "Remove" = 0), selected = 1)),
                      column(5, radioButtons(inputId = paste0("car", i, "_usage"), label = "Change weekly trips?", choices = list("Halve (-50%)" = 0.5, "Reduce (-25%)" = 0.75, "Same" = 1.0, "Increase (+25%)" = 1.25), selected = 1.0)),
                      column(3, style = "text-align:right;", span(style = "font-weight:bold; color:#007bff;", paste0("£", per_car_total_costs[i], "/month")), br(), span(style = "font-size:0.85em; color:#666;", paste0("removal ⇒ −£", per_car_total_costs[i])))
                    )
          )
        )
      })
    } else { list() }
    
    collapse_ui <- if (n_veh == 0) {
      tags$div(style = "margin-top:12px; color:#666;", "You do not list any household vehicles.")
    } else {
      do.call(bsCollapse, c(list(id = "vehicles_collapse", open = paste0("vehicle_panel_", seq_len(n_veh)), multiple = TRUE), panels))
    }
    
    perCarFixedCostsJS <- if(n_veh > 0) paste0("[", paste(per_car_fixed_costs, collapse = ","), "]") else "[]"
    perCarVariableCostsJS <- if(n_veh > 0) paste0("[", paste(per_car_variable_costs, collapse = ","), "]") else "[]"
    
    # --- MAIN UI LAYOUT ---
    tagList(
      div(class = "budget-overview",
          style = "background: linear-gradient(135deg, #6c757d 0%, #495057 100%); color: white; padding: 20px; border-radius: 8px; margin-bottom: 18px; max-width: 900px; margin-left: auto; margin-right: auto;",
          fluidRow(
            column(4,
                   h4("2030 Transport Budget"),
                   p("Current:", style="opacity:0.8;"), h5(paste0("£", total_current_cost, "/month")),
                   p("Budget:", style="opacity:0.8;"), h3(paste0("£", round(scenario_budget), "/month"), style="color: #ffc107;"))
            ,
            column(4,
                   div(style = "text-align: center; margin-top: 10px;",
                       span("Allocated:"), br(),
                       span(id = "allocated_budget", "£0", style = "font-size: 1.5em; font-weight: bold; color: #17a2b8;"), br(),
                       span("Remaining:"), br(),
                       span(id = "remaining_budget", paste0("£", round(scenario_budget)), style = "font-size: 1.8em; font-weight: bold; color: #28a745;")
                   )
            ),
            column(4,
                   div(style = "text-align: center; padding-top: 20px;",
                       div(id = "portfolio_summary", "Select options below", style = "font-size: 0.9em; opacity: 0.8; margin-bottom: 15px; min-height: 40px;"),
                       actionButton("continue_budget", "Continue with Portfolio", class = "btn-warning", disabled = TRUE, style = "font-weight: bold;")
                   )
            )
          )
      ),
      
      div(style = "max-width: 900px; margin: 0 auto;",
          fluidRow(
            column(6,
                   div(class = "budget-section", style = "border: 2px solid #007bff; border-radius: 8px; padding: 12px; margin-bottom: 15px; background: #f8f9ff;",
                       h6("Vehicle Ownership", style = "color: #007bff; font-weight: bold;"),
                       div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:8px;",
                           div("Manage each vehicle below."),
                           actionButton("remove_all_vehicles", "Remove all", class = "btn-danger btn-sm")
                       ),
                       collapse_ui,
                       tags$div(style = "margin-top:12px; border-top:1px dashed #e0e6f0; padding-top:10px;",
                                checkboxInput("add_vehicle_active", "Add vehicle state", value = FALSE, width = '0px'),
                                actionButton("add_vehicle_btn", "Add a Vehicle", class = "btn-success btn-sm"),
                                tags$div(id = "add_vehicle_panel", style = "display: none; padding: 12px; border-radius: 6px; margin-top: 10px; background-color: #e9ecef;",
                                         div(style="display:flex; justify-content:space-between; align-items:center;",
                                             h6("Configure New Vehicle", style="margin:0;"),
                                             span(id="new_vehicle_cost_display", style="font-weight:bold; color:#28a745;")
                                         ),
                                         selectInput("new_veh_type", "Type:", c("Car", "Van", "Motorbike"), width = "100%"),
                                         selectInput("new_veh_fuel", "Fuel:", c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid"), width = "100%"),
                                         selectInput("new_veh_mileage", "Mileage:", c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+"), width = "100%"),
                                         actionButton("cancel_add_vehicle_btn", "Cancel Addition", class = "btn-secondary btn-xs")
                                )
                       )
                   )
            ),
            column(6,
                   div(class = "budget-section", style = "border: 2px solid #4CAF50; border-radius: 8px; padding: 12px; margin-bottom: 15px; background: #f8fff8;",
                       h6("Car-Sharing Membership", style = "color: #4CAF50; font-weight: bold;"),
                       radioButtons("carclub_access", "Access Distance:", inline = TRUE, choices = list("None" = 0, "On-street" = 20, "5min walk" = 35, "At home" = 55), selected = 0),
                       radioButtons("carclub_hours", "Availability:", inline = TRUE, choices = list("None" = 0, "9-5" = 5, "Extended" = 15, "24/7" = 25), selected = 0),
                       radioButtons("carclub_usage", "Usage Level:", inline = TRUE, choices = list("None" = 0, "Light" = 8, "Regular" = 20, "Heavy" = 45), selected = 0),
                       div(id = "carclub_cost_display", style = "margin-top: 10px; padding: 8px; background: white; border-radius: 4px; text-align: center; font-weight: bold; color: #4CAF50;", "£0/month")
                   )
            )
          ),
          
          fluidRow(
            column(6,
                   div(class = "budget-section", style = "border: 2px solid #00BCD4; border-radius: 8px; padding: 15px; margin-bottom: 15px; background: #f0fdff;",
                       h6("Public Transport", style = "color: #00BCD4; font-weight: bold;"),
                       radioButtons("pt_package", "Choose your pass:",
                                    choices = list(
                                      "Pay Per Trip" = 0,
                                      "Leeds City Pass (£35/month)" = 35,
                                      "West Yorkshire Pass (£65/month)" = 65
                                    ), selected = 0)
                   )
            ),
            column(6,
                   div(class = "budget-section", style = "border: 2px solid #FF9800; border-radius: 8px; padding: 15px; margin-bottom: 15px; background: #fffaf0;",
                       h6("Bike Share & Active", style = "color: #FF9800; font-weight: bold;"),
                       radioButtons("micro_package", "Choose your package:",
                                    choices = list(
                                      "None (use own equipment)" = 0,
                                      "Basic Bike Share (£15/month)" = 15,
                                      "Enhanced Package (£30/month, e-bikes & storage)" = 30
                                    ), selected = 0)
                   )
            )
          )
      ),
      
      tags$script(HTML(sprintf('
        var scenarioBudget = %d;
        var perCarFixedCosts = %s;
        var perCarVariableCosts = %s;

        const costData = {
            baseCosts: {"Petrol": 150, "Diesel": 160, "Fully Electric": 80, "Plug-in Hybrid": 120},
            mileageMultipliers: {"0-2,000": 0.6, "2,001-5,000": 1.0, "5,001-10,000": 1.5, "10,001+": 2.2},
            typeMultipliers: {"Car": 1.0, "Van": 1.35, "Motorbike": 0.55},
            parkingCosts: {"0-2,000": 5, "2,001-5,000": 10, "5,001-10,000": 15, "10,001+": 20}
        };

        function calculateNewVehicleCost() {
            const fuel = $("#new_veh_fuel").val() || "Petrol";
            const mileage = $("#new_veh_mileage").val() || "5,001-10,000";
            const type = $("#new_veh_type").val() || "Car";

            const totalBase = (costData.baseCosts[fuel] || 150) * (costData.typeMultipliers[type] || 1.0);
            const variableCost = (totalBase * 0.6 * (costData.mileageMultipliers[mileage] || 1.0)) + ((costData.parkingCosts[mileage] || 0) * 1.2);
            const fixedCost = totalBase * 0.4;
            return Math.round(fixedCost + variableCost);
        }

        function updateBudgetDisplay() {
          var vehicleCost = 0;
          var removedTotal = 0;

          for (var i = 0; i < perCarFixedCosts.length; i++) {
            var keepVal = parseInt($("input[name=car" + (i+1) + "_keep]:checked").val());
            var usageMultiplier = parseFloat($("input[name=car" + (i+1) + "_usage]:checked").val());

            if (keepVal === 1) {
              vehicleCost += perCarFixedCosts[i] + (perCarVariableCosts[i] * usageMultiplier);
            } else {
              removedTotal += perCarFixedCosts[i] + perCarVariableCosts[i];
            }
          }

          if ($("#add_vehicle_active").is(":checked")) {
            const newCost = calculateNewVehicleCost();
            vehicleCost += newCost;
            $("#new_vehicle_cost_display").text("Cost: £" + newCost + "/month");
          }

          var carclubCost = parseInt($("input[name=carclub_access]:checked").val()) + parseInt($("input[name=carclub_hours]:checked").val()) + parseInt($("input[name=carclub_usage]:checked").val());
          var ptCost = parseInt($("input[name=pt_package]:checked").val());
          var microCost = parseInt($("input[name=micro_package]:checked").val());
          
          $("#carclub_cost_display").text("£" + carclubCost + "/month");

          var totalAllocated = Math.round(vehicleCost + carclubCost + ptCost + microCost);
          var remaining = scenarioBudget - totalAllocated;

          $("#allocated_budget").text("£" + totalAllocated);
          $("#remaining_budget").text("£" + remaining);
          
          if (remaining < 0) {
            $("#remaining_budget").css("color", "#dc3545");
            $("#continue_budget").prop("disabled", true);
          } else if (totalAllocated === 0) {
            $("#remaining_budget").css("color", "#6c757d");
            $("#continue_budget").prop("disabled", true);
          } else {
            $("#remaining_budget").css("color", "#28a745");
            $("#continue_budget").prop("disabled", false);
          }
          
          var summary = totalAllocated === 0 ? "Select options below" : "£" + totalAllocated + " monthly";
          if (removedTotal > 0) summary += " — removed £" + Math.round(removedTotal);
          $("#portfolio_summary").text(summary);
        }

        $(document).on("click", "#add_vehicle_btn", function() {
          $("#add_vehicle_panel").slideDown();
          $(this).hide();
          $("#add_vehicle_active").prop("checked", true).trigger("change");
        });

        $(document).on("click", "#cancel_add_vehicle_btn", function() {
          $("#add_vehicle_panel").slideUp();
          $("#add_vehicle_btn").show();
          $("#add_vehicle_active").prop("checked", false).trigger("change");
        });
        
        $(document).on("click", "#remove_all_vehicles", function(e) {
          for (var i = 0; i < perCarFixedCosts.length; i++) {
            var sel = $("input[name=\\"car" + (i+1) + "_keep\\"][value=\\"0\\"]");
            if (sel.length) sel.prop("checked", true);
          }
          $("input[name=\\"car1_keep\\"]").trigger("change");
        });
        
        $(document).on("change", "input, select", updateBudgetDisplay);
        
        setTimeout(updateBudgetDisplay, 200);
    ', 
    as.integer(round(scenario_budget)), 
    perCarFixedCostsJS,
    perCarVariableCostsJS
      )))
    )
  })
  
  
    
  # This UI placeholder now just creates the box where the text will go.
  # ========= VERSION 5 (MENU BUILDER) - NEW =========
  
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
                     h3("What will your household transport look like in 2030?"),
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