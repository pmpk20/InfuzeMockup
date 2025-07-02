# server.R (Version 14.1 - Fully Functional Nested UI)

library(shiny)

# Define the server logic
function(input, output, session) {
  
  # ========= Navigation Logic =========
  # This controls the flow from one tab to the next when buttons are clicked.
  observeEvent(input$to_demographics_button, {
    updateTabsetPanel(session, "main_tabs", selected = "demographics_panel")
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
  observeEvent(input$to_summary_button, {
    updateTabsetPanel(session, "main_tabs", selected = "summary_panel")
  })
  
  # ========= DYNAMIC UI FOR STATED ADAPTATION SECTION =========
  output$sa_ui_placeholder <- renderUI({
    
    # --- UI Components ---
    intro_line <- h4("Please consider how you would react to the following hypothetical scenario:")
    
    scenario_box <- wellPanel(
      style = "background-color: white; border: 2px solid black;",
      h4("Scenario Details"),
      tags$ul(
        tags$li("Fuel prices: ", tags$u("+60p per litre")),
        tags$li("Parking permit costs: ", tags$u("+20%")),
        tags$li("Public Transport: A new 'Leeds Travel Pass' is available for", tags$u("£50/month")),
        tags$li("Car Club: Membership to the 'INFUZE_TRIAL' costs", tags$u("£5 per hour"), " of use, with vehicles available within a 5-minute walk.")
      )
    )
    
    main_question <- h3("How would you adapt your household's travel options?")
    
    # --- Helper function to create the nested UI for ONE vehicle ---
    create_vehicle_adaptation_block <- function(car_number, vehicle_type, fuel_type, mileage_val) {
      
      # --- CREATE UNIQUE INPUT IDs FOR EVERYTHING ---
      decision_id <- paste0("sa_car", car_number, "_decision")
      # Keep block IDs
      keep_mileage_id <- paste0("sa_car", car_number, "_keep_mileage")
      keep_add_options_id <- paste0("sa_car", car_number, "_keep_add_options")
      
      vehicle_description <- p("You previously said this vehicle was a: ", 
                               strong(paste0(tolower(fuel_type), " ", tolower(vehicle_type), 
                                             " driven approximately ", mileage_val, " miles per year.")))
      
      wellPanel(
        h4(paste0("Decision for Vehicle ", car_number)),
        vehicle_description,
        
        radioButtons(inputId = decision_id,
                     label = "In this scenario, what is your main decision for this vehicle?",
                     choices = c("Keep this vehicle", "Replace this vehicle", "Remove this vehicle")),
        
        # --- CONDITIONAL PANELS FOR EACH PRIMARY DECISION ---
        
        conditionalPanel(
          condition = paste0("input.", decision_id, " == 'Keep this vehicle'"),
          selectInput(
            inputId = keep_mileage_id,
            label = "What would be your annual mileage in this vehicle?",
            choices = c(
              "0-2,000",
              "2,001-5,000",
              "5,001 - 10,000",
              "10,001+"
            ), selected = mileage_val
          ), 
          selectInput(
            inputId = keep_add_options_id,
            label = "Would you add any new travel options?",
            choices = c(
              "No, no other changes",
              "Add 'INFUZE_TRIAL' Car Sharing",
              "Add a 'Leeds Travel Pass'",
              "Add an ADDITIONAL owned vehicle"
            )
          )
        ),
        
        conditionalPanel(
          condition = paste0("input.", decision_id, " == 'Replace this vehicle'"),
          wellPanel(style = "background-color: #EBF5FB;",
                    h5("Details of the REPLACEMENT vehicle:"),
                    selectInput(paste0("sa_car", car_number, "_replace_type"), "Vehicle Type:", choices = c("Car", "Van", "Motorbike")),
                    selectInput(paste0("sa_car", car_number, "_replace_fuel"), "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
                    selectInput(paste0("sa_car", car_number, "_replace_mileage"), "What would be your new annual mileage?", choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+")),
                    selectInput(
                      inputId = keep_add_options_id,
                      label = "Would you add any new travel options?",
                      choices = c(
                        "No, no other changes",
                        "Add 'INFUZE_TRIAL' Car Sharing",
                        "Add a 'Leeds Travel Pass'",
                        "Add an ADDITIONAL owned vehicle"
                      )
                    )
          )
        ),
        
        conditionalPanel(
          condition = paste0("input.", decision_id, " == 'Remove this vehicle'"),
          wellPanel(style = "background-color: #FDEDEC;",
                    selectInput(
                      inputId = keep_add_options_id,
                      label = "Would you add any new travel options?",
                      choices = c(
                        "No, no other changes",
                        "Add 'INFUZE_TRIAL' Car Sharing",
                        "Add a 'Leeds Travel Pass'",
                        "Add an ADDITIONAL owned vehicle"
                      )
                    )
          )
        ),
        
        conditionalPanel(
          condition = paste0("input.", keep_add_options_id, " == 'Add an ADDITIONAL owned vehicle'"),
          wellPanel(style = "background-color: #D5F5E3;",
                    h5("Details of the ADDITIONAL vehicle:"),
                    selectInput(paste0("sa_car", car_number, "_add_type"), "Vehicle Type:", choices = c("Car", "Van", "Motorbike")),
                    selectInput(paste0("sa_car", car_number, "_add_fuel"), "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
                    selectInput(paste0("sa_car", car_number, "_add_mileage"), "Estimated Annual Mileage (miles):", choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+"))
          )
        )
      )
    }
    
    # --- Assemble the UI ---
    req(input$num_cars)
    
    # FIX: Define the zero_car_adaptation_ui object BEFORE it is used.
    # zero_car_adaptation_ui <- conditionalPanel(
    #   condition = "input.num_cars == '0' || (input.num_cars == '1' && input.sa_car1_decision == 'Remove this vehicle') || (input.num_cars == '2' && input.sa_car1_decision == 'Remove this vehicle' && input.sa_car2_decision == 'Remove this vehicle')",
    #   hr(),
    #   h4("Decisions about your new travel portfolio:"),
    #   p("Since your household would not have an owned car in this scenario, what new travel options would you adopt?"),
    #   checkboxGroupInput("sa_0_car_add_options", "Select all that apply:",
    #                      choices = c("Sign up for 'INFUZE_TRIAL' Car Sharing", 
    #                                  "Get a 'Leeds Travel Pass' for Public Transport",
    #                                  "Acquire a new OWNED vehicle")),
    #   conditionalPanel(
    #     # The condition needs to check the checkboxGroupInput correctly with JavaScript
    #     condition = "input.sa_0_car_add_options.indexOf('Acquire a new OWNED vehicle') > -1",
    #     wellPanel(style = "background-color: #D5F5E3;",
    #               h5("Details of the NEW vehicle:"),
    #               selectInput("sa_0_car_new_type", "Vehicle Type:", choices = c("Car", "Van", "Motorbike")),
    #               selectInput("sa_0_car_new_fuel", "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
    #               selectInput("sa_0_car_new_mileage", "Estimated Annual Mileage (miles):", choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+"))
    #     )
    #   )
    # )
    
    ui_to_render <- list(intro_line, scenario_box, main_question)
    
    
    if (input$num_cars != "0") {
      ui_to_render <- append(ui_to_render, list(h4("Decisions about your OWNED vehicles:")))
      
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
    }
    
    # Now this append call will work because zero_car_adaptation_ui exists.
    # ui_to_render <- append(ui_to_render, list(zero_car_adaptation_ui))
    

    
    # ========= COST CALCULATIONS =========
    
    # Current monthly cost (X) - based on Step 2 choices
    current_monthly_cost <- reactive({
      req(input$num_cars)
      
      total_cost <- 0
      
      # Basic vehicle running costs (fuel + insurance + maintenance per month)
      vehicle_monthly_cost <- function(fuel_type, mileage_bracket) {
        base_costs <- list(
          # "Petrol" = 150,
          # "Diesel" = 160, 
          # "Fully Electric" = 80,
          # "Plug-in Hybrid" = 120
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
      
      # Process each vehicle based on SA decisions
      if (input$num_cars != "0") {
        # Vehicle 1
        if (!is.null(input$sa_car1_decision)) {
          if (input$sa_car1_decision == "Keep this vehicle" && !is.null(input$sa_car1_keep_mileage)) {
            total_cost <- total_cost + vehicle_monthly_cost_new(input$car1_fuel, input$sa_car1_keep_mileage)
          } else if (input$sa_car1_decision == "Replace this vehicle") {
            if (!is.null(input$sa_car1_replace_fuel)) {
              total_cost <- total_cost + vehicle_monthly_cost_new(input$sa_car1_replace_fuel, input$sa_car1_replace_mileage)
            }
          }
          # If "Remove this vehicle", add nothing
        }
        
        # Vehicle 2 (similar logic)
        if (input$num_cars %in% c("2", "3", "4+") && !is.null(input$sa_car2_decision)) {
          if (input$sa_car2_decision == "Keep this vehicle" && !is.null(input$sa_car2_keep_mileage)) {
            total_cost <- total_cost + vehicle_monthly_cost_new(input$car2_fuel, input$sa_car2_keep_mileage)
          } else if (input$sa_car2_decision == "Replace this vehicle") {
            if (!is.null(input$sa_car2_replace_fuel)) {
              total_cost <- total_cost + vehicle_monthly_cost_new(input$sa_car2_replace_fuel, input$sa_car2_replace_mileage)
            }
          }
        }
        
        # Vehicle 3
        if (input$num_cars %in% c("3", "4+") && !is.null(input$sa_car3_decision)) {
          if (input$sa_car3_decision == "Keep this vehicle" && !is.null(input$sa_car3_keep_mileage)) {
            total_cost <- total_cost + vehicle_monthly_cost_new(input$car3_fuel, input$sa_car3_keep_mileage)
          } else if (input$sa_car3_decision == "Replace this vehicle") {
            if (!is.null(input$sa_car3_replace_fuel)) {
              total_cost <- total_cost + vehicle_monthly_cost_new(input$sa_car3_replace_fuel, input$sa_car3_replace_mileage)
            }
          }
        }
        
        # Vehicle 4
        if (input$num_cars == "4+" && !is.null(input$sa_car4_decision)) {
          if (input$sa_car4_decision == "Keep this vehicle" && !is.null(input$sa_car4_keep_mileage)) {
            total_cost <- total_cost + vehicle_monthly_cost_new(input$car4_fuel, input$sa_car4_keep_mileage)
          } else if (input$sa_car4_decision == "Replace this vehicle") {
            if (!is.null(input$sa_car4_replace_fuel)) {
              total_cost <- total_cost + vehicle_monthly_cost_new(input$sa_car4_replace_fuel, input$sa_car4_replace_mileage)
            }
          }
        }
      }
      
      # Add new travel options costs
      # Check various "add options" inputs
      add_options_sources <- c(
        input$sa_car1_keep_add_options,
        input$sa_car2_keep_add_options,
        input$sa_0_car_add_options
      )
      
      if (any(grepl("Leeds Travel Pass", add_options_sources, ignore.case = TRUE))) {
        total_cost <- total_cost + 50
      }
      
      if (any(grepl("INFUZE_TRIAL", add_options_sources, ignore.case = TRUE))) {
        total_cost <- total_cost + 25 # Assume £25/month average usage
      }
      
      # Additional owned vehicles
      if (any(grepl("ADDITIONAL owned vehicle", add_options_sources, ignore.case = TRUE))) {
        # Add cost for additional vehicle (use car1 add details if available)
        if (!is.null(input$sa_car1_add_fuel)) {
          total_cost <- total_cost + vehicle_monthly_cost_new(input$sa_car1_add_fuel, input$sa_car1_add_mileage)
        }
      }
      
      return(round(total_cost, 2))
    })
    
    # Update the cost_box in your sa_ui_placeholder to use these reactive values:
    cost_box <- wellPanel(
      style = "background-color: white; border: 2px solid blue;",
      h4("Your Costs"),
      p(strong("Based on your current travel, we estimate:")),
      tags$ul(
        tags$li("Monthly cost: £", textOutput("current_monthly_formatted", inline = TRUE)),
        tags$li("Annual cost: £", textOutput("current_annual_formatted", inline = TRUE))
      ),
      
      p(strong("After adapting to the scenario above, we estimate:")),
      
      tags$ul(
        tags$li(
          "Monthly cost: £",
          textOutput("new_monthly_formatted", inline = TRUE),
          " (",
          textOutput("monthly_change", inline = TRUE),
          ")"
        ),
        tags$li(
          "Annual cost: £",
          textOutput("new_annual_formatted", inline = TRUE),
          " (",
          textOutput("annual_change", inline = TRUE),
          ")"
        )
      )
    )
    
    # Add these output renderers to your server.R:
    output$current_monthly_formatted <- renderText({
      format(current_monthly_cost(), big.mark = ",", nsmall = 2)
    })
    
    output$current_annual_formatted <- renderText({
      format(current_monthly_cost() * 12, big.mark = ",", nsmall = 2)
    })
    
    output$new_monthly_formatted <- renderText({
      format(new_monthly_cost(), big.mark = ",", nsmall = 2)
    })
    
    output$new_annual_formatted <- renderText({
      format(new_monthly_cost() * 12, big.mark = ",", nsmall = 2)
    })
    
    output$monthly_change <- renderText({
      change <- new_monthly_cost() - current_monthly_cost()
      percentage <- round((change / current_monthly_cost()) * 100, 1)
      sign <- if(change >= 0) "+" else ""
      paste0(sign, "£", format(abs(change), big.mark = ",", nsmall = 2), ", ", sign, percentage, "%")
    })
    
    output$annual_change <- renderText({
      change <- (new_monthly_cost() * 12) - (current_monthly_cost() * 12)
      percentage <- round((change / (current_monthly_cost() * 12)) * 100, 1)
      sign <- if(change >= 0) "+" else ""
      paste0(sign, "£", format(abs(change), big.mark = ",", nsmall = 2), ", ", sign, percentage, "%")
    })
    
    
    ui_to_render <- append(ui_to_render, list(hr(), cost_box, hr(), actionButton("to_summary_button", "I have decided, see summary", class = "btn-success btn-lg")))
    
    do.call(tagList, ui_to_render)
  })
  
  # ========= REVISED SUMMARY OUTPUT LOGIC =========
  
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
      verbatimTextOutput("final_summary_text")
    )
  })
  
  # This renderer now correctly finds summary_text() in the main server scope.
  output$final_summary_text <- renderText({
    summary_text()
  })
  
}