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
  
  
  # ========= COST CALCULATIONS (Move to main server scope) =========
  
  # Current monthly cost (X) - based on Step 2 choices
  current_monthly_cost <- reactive({
    req(input$num_cars)
    
    total_cost <- 0
    
    # Basic vehicle running costs (fuel + insurance + maintenance per month)
    vehicle_monthly_cost <- function(fuel_type, mileage_bracket) {
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
    
    # Handle zero-car households that acquire new vehicles
    if (input$num_cars == "0") {
      if (!is.null(input$sa_0_car_add_options) && 
          grepl("Acquire a new OWNED vehicle", input$sa_0_car_add_options, ignore.case = TRUE)) {
        if (!is.null(input$sa_0_car_new_fuel) && !is.null(input$sa_0_car_new_mileage)) {
          total_cost <- total_cost + vehicle_monthly_cost_new(input$sa_0_car_new_fuel, input$sa_0_car_new_mileage)
        }
      }
    }
    
    # Process each vehicle based on SA decisions
    if (input$num_cars != "0") {
      # Vehicle 1
      if (!is.null(input$sa_car1_decision)) {
        # if (input$sa_car1_decision == "Keep this vehicle" && !is.null(input$sa_car1_keep_mileage)) {
        #   total_cost <- total_cost + vehicle_monthly_cost_new(input$car1_fuel, input$sa_car1_keep_mileage)
        if (input$sa_car1_decision == "Keep this vehicle") {
            total_cost <- total_cost + vehicle_monthly_cost_new(input$car1_fuel, input$car1_mileage)
          
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
    
    return(round(total_cost, 2))
  })
  
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
  
  
  # ========= SA VERSION LATEST =========
  output$sa_ui_placeholder <- renderUI({
    
    tags$style(HTML("
  /* Make radio circles and labels larger */
  .radio .form-check-input {
    transform: scale(1.5);
    margin-right: 8px;
  }
  .radio .form-check-label {
    font-size: 1.1em;
    padding: 4px 0;
    cursor: pointer;
  }

  /* Make all .btn elements larger */
  .btn {
    padding: 0.75rem 1.25rem !important;
    font-size: 1.25rem !important;
  }
"))
    
    # --- UI Components ---
    # intro_line <- h4("Please consider how you would react to the following hypothetical scenario:")
  
    
    main_question <- h3("How would you adapt your household's travel options?")
    
    # --- Helper function to create the nested UI for ONE vehicle ---
    # Replace the create_vehicle_adaptation_block function with this DCE version:
    
    create_vehicle_adaptation_block <- function(car_number, vehicle_type, fuel_type, mileage_val) {
      
      decision_id <- paste0("sa_car", car_number, "_decision")
      replace_type_id <- paste0("sa_car", car_number, "_replace_type")
      replace_fuel_id <- paste0("sa_car", car_number, "_replace_fuel")
      replace_mileage_id <- paste0("sa_car", car_number, "_replace_mileage")
      remove_options_id <- paste0("sa_car", car_number, "_remove_options")
      
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
      
      
      
      # Vehicle cost calculation function (same as in your existing code)
      calculate_vehicle_cost <- function(fuel_type, mileage_bracket) {
        base_costs <- list(
          "Petrol" = 150 * 1.25,
          "Diesel" = 160 * 1.25,
          "Fully Electric" = 80,
          "Plug-in Hybrid" = 120 * 1.15
        )
        
        parking_increase <- switch(mileage_bracket,
                                   "0-2,000" = 5,
                                   "2,001-5,000" = 10,
                                   "5,001 - 10,000" = 15,
                                   "10,001+" = 20)
        
        mileage_multiplier <- switch(mileage_bracket,
                                     "0-2,000" = 0.6,
                                     "2,001-5,000" = 1.0,
                                     "5,001 - 10,000" = 1.5,
                                     "10,001+" = 2.2)
        
        return(round((base_costs[[fuel_type]] * mileage_multiplier) + (parking_increase * 1.20), 0))
      }
      
      # Reactive outputs for Car 1 costs
      output$sa_car1_replace_cost <- renderText({
        if (!is.null(input$sa_car1_replace_fuel) && !is.null(input$sa_car1_replace_mileage)) {
          cost <- calculate_vehicle_cost(input$sa_car1_replace_fuel, input$sa_car1_replace_mileage)
          paste0("£", cost, "/month")
        } else {
          "£[select options]"
        }
      })
      
      output$sa_car1_remove_cost <- renderText({
        if (!is.null(input$sa_car1_remove_options)) {
          cost <- switch(input$sa_car1_remove_options,
                         "no_changes" = 0,
                         "add_carshare" = 25,  # Assume £25/month average usage
                         "add_pt" = 50,        # Leeds Travel Pass
                         "add_vehicle" = {
                           if (!is.null(input$sa_car1_add_fuel) && !is.null(input$sa_car1_add_mileage)) {
                             calculate_vehicle_cost(input$sa_car1_add_fuel, input$sa_car1_add_mileage)
                           } else {
                             0
                           }
                         },
                         0)
          paste0("£", cost, "/month")
        } else {
          "£0/month"
        }
      })
      
      # Reactive outputs for Car 2 costs (if you have 2+ cars)
      output$sa_car2_replace_cost <- renderText({
        if (!is.null(input$sa_car2_replace_fuel) && !is.null(input$sa_car2_replace_mileage)) {
          cost <- calculate_vehicle_cost(input$sa_car2_replace_fuel, input$sa_car2_replace_mileage)
          paste0("£", cost, "/month")
        } else {
          "£[select options]"
        }
      })
      
      output$sa_car2_remove_cost <- renderText({
        if (!is.null(input$sa_car2_remove_options)) {
          cost <- switch(input$sa_car2_remove_options,
                         "no_changes" = 0,
                         "add_carshare" = 25,
                         "add_pt" = 50,
                         "add_vehicle" = {
                           if (!is.null(input$sa_car2_add_fuel) && !is.null(input$sa_car2_add_mileage)) {
                             calculate_vehicle_cost(input$sa_car2_add_fuel, input$sa_car2_add_mileage)
                           } else {
                             0
                           }
                         },
                         0)
          paste0("£", cost, "/month")
        } else {
          "£0/month"
        }
      })
      
      
    
      
      # Current vehicle costs
      current_cost <- calculate_monthly_cost(fuel_type, mileage_val)
      # Default replacement cost (will be updated reactively)
      default_replace_cost <- calculate_monthly_cost("Petrol", mileage_val)
      
      # Vehicle description for labels
      vehicle_desc <- paste0(tolower(fuel_type), " ", tolower(vehicle_type), 
                             " driven approximately ", mileage_val, " miles per year")
      
      wellPanel(
        style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);",
        
        # h5(paste0("Vehicle ", car_number, " Decisions"), style = "color: #495057; margin-bottom: 20px;"),
        
        tags$table(
          style = "width: 100%; border-collapse: collapse; margin: 10px 0; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
          class = "table table-bordered",
          tags$thead(
            tags$tr(
            #   tags$th(style = "border: 1px solid #dee2e6; padding: 12px; background: linear-gradient(135deg, #007bff 0%, #0056b3 100%); color: white; text-align: center;", 
            #           paste0("Keep this vehicle: ", vehicle_desc)),
              tags$th(
                style = "width:33%; border: 1px solid #dee2e6; padding: 12px; background: linear-gradient(135deg, #007bff 0%, #0056b3 100%); color: white; text-align: center;",
                HTML(paste0("Keep this vehicle:<br>", vehicle_desc))
              ),
              tags$th(style = "width:33%; border: 1px solid #dee2e6; padding: 12px; background: linear-gradient(135deg, #28a745 0%, #1e7e34 100%); color: white; text-align: center;", 
                      strong("Replace this vehicle with:")),
              tags$th(style = "width:33%; border: 1px solid #dee2e6; padding: 12px; background: linear-gradient(135deg, #dc3545 0%, #bd2130 100%); color: white; text-align: center;", 
                      strong("Remove this vehicle and:"))
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(style = "border: 1px solid #dee2e6; padding: 12px; background-color: #cce7ff; text-align: center;", 
                      div(style = "font-size: 16px; font-weight: bold; color: #0056b3;", 
                          paste0("£", current_cost, "/month"))),
              tags$td(style = "border: 1px solid #dee2e6; padding: 12px; background-color: #d4edda; text-align: center;", 
                      div(style = "font-size: 16px; font-weight: bold; color: #155724;",
                          textOutput(paste0("sa_car", car_number, "_replace_cost"), inline = TRUE))),
              tags$td(style = "border: 1px solid #dee2e6; padding: 12px; background-color: #f8d7da; text-align: center;", 
                      div(style = "font-size: 16px; font-weight: bold; color: #721c24;",
                          textOutput(paste0("sa_car", car_number, "_remove_cost"), inline = TRUE)))
            ),
            
            tags$tr(
              tags$td(style = "border: 1px solid #dee2e6; padding: 8px; background-color: #e6f3ff;", 
                      div(style = "text-align: center;",
                          p("Second attribute level here", style = "margin: 5px 0; font-size: 12px; color: #495057;"))),
              tags$td(style = "border: 1px solid #dee2e6; padding: 8px; background-color: #e8f5e8;", 
                      div(style = "text-align: center;",
                          p("Second attribute level here", style = "margin: 5px 0; font-size: 12px; color: #495057;"))),
              tags$td(style = "border: 1px solid #dee2e6; padding: 8px; background-color: #fce8e8;", 
                      div(style = "text-align: center;",
                          p("Second attribute level here", style = "margin: 5px 0; font-size: 12px; color: #495057;")))
            ),
            
            tags$tr(
              tags$td(style = "border: 1px solid #dee2e6; padding: 8px; background-color: #e6f3ff;", 
                      div(style = "text-align: center;",
                          p("Continue using your current vehicle", style = "margin: 5px 0; font-size: 12px; color: #495057;"))),
              tags$td(style = "border: 1px solid #dee2e6; padding: 8px; background-color: #e8f5e8;", 
                      div(
                        selectInput(replace_type_id, "Vehicle Type:", 
                                    choices = c("Car", "Van", "Motorbike"), 
                                    width = "100%"),
                        selectInput(replace_fuel_id, "Fuel Type:", 
                                    choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid"), 
                                    width = "100%"),
                        selectInput(replace_mileage_id, "Annual Mileage:", 
                                    choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+"), 
                                    selected = mileage_val, width = "100%")
                      )),
              tags$td(style = "border: 1px solid #dee2e6; padding: 8px; background-color: #fce8e8;", 
                      selectInput(remove_options_id, "Add new travel options:", 
                                  choices = c(
                                    "No other changes" = "no_changes",
                                    "Add 'INFUZE_TRIAL' Car Sharing" = "add_carshare",
                                    "Add a 'Leeds Travel Pass'" = "add_pt",
                                    "Add an ADDITIONAL owned vehicle" = "add_vehicle"
                                  ), width = "100%"),
                      
                      conditionalPanel(
                                condition = paste0("input.", remove_options_id, " == 'add_vehicle'"),
                                wellPanel(style = "background-color: #D5F5E3; margin-top: 15px;",
                                          h5("Details of the ADDITIONAL vehicle:"),
                                          div(
                                            selectInput(paste0("sa_car", car_number, "_add_type"), 
                                                        "Vehicle Type:", 
                                                        choices = c("Car", 
                                                                    "Van", 
                                                                    "Motorbike"), 
                                                        width = "100%"),
                                            selectInput(paste0("sa_car", car_number, "_add_fuel"), 
                                                        "Fuel Type:", 
                                                        choices = c("Petrol", 
                                                                    "Diesel", 
                                                                    "Fully Electric", 
                                                                    "Plug-in Hybrid"), 
                                                        width = "100%"),
                                            selectInput(paste0("sa_car", 
                                                               car_number, 
                                                               "_add_mileage"), 
                                                        "Annual Mileage:", 
                                                        choices = c("0-2,000", 
                                                                    "2,001-5,000", 
                                                                    "5,001 - 10,000", 
                                                                    "10,001+"), 
                                                        selected = mileage_val, width = "100%")
                                          )
                                          # fluidRow(
                                          #   column(4, selectInput(paste0("sa_car", car_number, "_add_type"), "Vehicle Type:",
                                          #                         choices = c("Car", "Van", "Motorbike"))),
                                          #   column(4, selectInput(paste0("sa_car", car_number, "_add_fuel"), "Main Fuel Type:",
                                          #                         choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid"))),
                                          #   column(4, selectInput(paste0("sa_car", car_number, "_add_mileage"), "Annual Mileage:",
                                          #                         choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+")))
                                          # )
                                )
                              )
                      )
            ),
            # NEW (reactive):
            tags$tr(
              tags$td(style = "border: 1px solid #dee2e6; padding: 8px; text-align: center; background-color: #cce7ff;",
                radioButtons(
                  inputId   = decision_id,
                  label     = NULL,
                  choices   = c(
                    "Keep this vehicle"    = "Keep this vehicle"
                  ),
                  inline    = TRUE,
                  selected = character(0)   # <-- no default
                )
              ),
              tags$td(style = "border: 1px solid #dee2e6; padding: 8px; text-align: center; background-color: #d4edda;",
                      radioButtons(
                        inputId   = decision_id,
                        label     = NULL,
                        choices   = c(
                          "Replace this vehicle" = "Replace this vehicle"
                        ),
                        inline    = TRUE,
                        selected = character(0)   # <-- no default
                      )
              ),
              tags$td(style = "border: 1px solid #dee2e6; padding: 8px; text-align: center; background-color: #f8d7da;",
                      radioButtons(
                        inputId   = decision_id,
                        label     = NULL,
                        choices   = c(
                          "Remove this vehicle"  = "Remove this vehicle"
                        ),
                        inline    = TRUE,
                         selected = character(0)   # <-- no default
                      )
              )
            )
            # tags$tr(
            #   tags$td(style = "border: 1px solid #dee2e6; padding: 8px; text-align: center; background-color: #cce7ff;",
            #           tags$input(type = "radio", name = decision_id, value = "Keep this vehicle", style = "transform: scale(1.2);")),
            #   tags$td(style = "border: 1px solid #dee2e6; padding: 8px; text-align: center; background-color: #d4edda;",
            #           tags$input(type = "radio", name = decision_id, value = "Replace this vehicle", style = "transform: scale(1.2);")),
            #   tags$td(style = "border: 1px solid #dee2e6; padding: 8px; text-align: center; background-color: #f8d7da;",
            #           tags$input(type = "radio", name = decision_id, value = "Remove this vehicle", style = "transform: scale(1.2);"))
            # )
          )
        ),
        
        # Conditional panel for additional vehicle details when "Add an ADDITIONAL owned vehicle" is selected
        # conditionalPanel(
        #   condition = paste0("input.", remove_options_id, " == 'add_vehicle'"),
        #   wellPanel(style = "background-color: #D5F5E3; margin-top: 15px;",
        #             h5("Details of the ADDITIONAL vehicle:"),
        #             fluidRow(
        #               column(4, selectInput(paste0("sa_car", car_number, "_add_type"), "Vehicle Type:", 
        #                                     choices = c("Car", "Van", "Motorbike"))),
        #               column(4, selectInput(paste0("sa_car", car_number, "_add_fuel"), "Main Fuel Type:", 
        #                                     choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid"))),
        #               column(4, selectInput(paste0("sa_car", car_number, "_add_mileage"), "Annual Mileage:", 
        #                                     choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+")))
        #             )
        #   )
        # )
      )
    }
    # --- Assemble the UI ---
    req(input$num_cars)
    
    # FIX: Define the zero_car_adaptation_ui object BEFORE it is used.
    zero_car_adaptation_ui <- conditionalPanel(
      # Simplified condition - show if they start with 0 cars OR if all their cars are removed
      condition = "input.num_cars == '0' || (input.num_cars == '1' && input.sa_car1_decision == 'Remove this vehicle') || 
      (input.num_cars == '2' && input.sa_car1_decision == 'Remove this vehicle' && input.sa_car2_decision == 'Remove this vehicle') || 
      (input.num_cars == '3' && input.sa_car1_decision == 'Remove this vehicle' && input.sa_car2_decision == 'Remove this vehicle' && input.sa_car3_decision == 'Remove this vehicle') || (input.num_cars == '4+' && input.sa_car1_decision == 'Remove this vehicle' && input.sa_car2_decision == 'Remove this vehicle' && input.sa_car3_decision == 'Remove this vehicle' && input.sa_car4_decision == 'Remove this vehicle')",
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
      # h4("Scenario Details & Your Costs"),
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
    ui_to_render <- list(scenario_and_costs_table, main_question)
    
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
      ## I'm adding 'TEST' here to break the logic as I couldn't work out how to remove it lol
      zero_car_adaptation_ui <- conditionalPanel(
        condition = "(input.num_cars == '1' && input.sa_car1_decision == ' TEST Remove this vehicle' && input.remove_options_id != 'add_vehicle') || 
        (input.num_cars == '2' && input.sa_car1_decision == 'TEST Remove this vehicle' && input.sa_car2_decision == 'Remove this vehicle' && input.remove_options_id != 'add_vehicle') || 
        (input.num_cars == '3' && input.sa_car1_decision == 'TEST Remove this vehicle' && input.sa_car2_decision == 'Remove this vehicle' && input.sa_car3_decision == 'Remove this vehicle' && input.remove_options_id != 'add_vehicle') || 
        (input.num_cars == '4+' && input.sa_car1_decision == 'TESTRemove this vehicle' && input.sa_car2_decision == 'Remove this vehicle' && input.sa_car3_decision == 'Remove this vehicle' && input.sa_car4_decision == 'Remove this vehicle' && input.remove_options_id != 'add_vehicle')",
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
                                actionButton("to_sa_button2", "Version 2", 
                                             class = "btn-success btn-lg")))
    # ui_to_render <- append(ui_to_render, list(hr(), cost_box, hr(), actionButton("to_summary_button", "I have decided, see summary", class = "btn-success btn-lg")))
    
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
  
  
  # ========= MOCKUP VERSION 3 =========
  output$sa_ui_placeholder3 <- renderUI({
    
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
            h5("In this scenario, what would you do with this vehicle?"),
            tags$table(
              style = "width: 100%; border-collapse: collapse; margin: 10px 0;",
              class = "table table-bordered",
              tags$thead(
                tags$tr(
                  tags$th(style = "border: 1px solid #ddd; padding: 8px; background-color: #f8f9fa;", "Option"),
                  tags$th(style = "border: 1px solid #ddd; padding: 8px; background-color: #f8f9fa;", "Monthly Cost"),
                  # tags$th(style = "border: 1px solid #ddd; padding: 8px; background-color: #f8f9fa;", "Description"),
                  tags$th(style = "border: 1px solid #ddd; padding: 8px; background-color: #f8f9fa; text-align: center;", "Your Choice")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Keep this vehicle"),
                  tags$td(style = "border: 1px solid #ddd; padding: 8px;", "£XXX"), # You'll need to calculate this
                  # tags$td(style = "border: 1px solid #ddd; padding: 8px;", 
                  #         paste("Continue using your", tolower(fuel_type), tolower(vehicle_type))),
                  tags$td(style = "border: 1px solid #ddd; padding: 8px; text-align: center;",
                          tags$input(type = "radio", 
                                     name = decision_id, 
                                     label = paste0("Monthly cost: ", "£XXX"),
                                     value = "Keep this vehicle"))
                ),
                tags$tr(
                  tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Replace this vehicle"),
                  tags$td(style = "border: 1px solid #ddd; padding: 8px;", "£YYY"), # You'll need to calculate this
                  # tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Get a different vehicle"),
                  tags$td(style = "border: 1px solid #ddd; padding: 8px; text-align: center;",
                          tags$input(type = "radio", name = decision_id, value = "Replace this vehicle"))
                ),
                tags$tr(
                  tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Remove this vehicle"),
                  tags$td(style = "border: 1px solid #ddd; padding: 8px;", "£0"),
                  # tags$td(style = "border: 1px solid #ddd; padding: 8px;", "No longer own this vehicle"),
                  tags$td(style = "border: 1px solid #ddd; padding: 8px; text-align: center;",
                          tags$input(type = "radio", name = decision_id, value = "Remove this vehicle"))
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
                                # actionButton("to_sa_button3", "Version 3", 
                                             actionButton("to_sa_button4", "OLD VERSION",
                                             class = "btn-success btn-lg")))
    # ui_to_render <- append(ui_to_render, list(hr(), cost_box, hr(), actionButton("to_summary_button", "I have decided, see summary", class = "btn-success btn-lg")))
    
    do.call(tagList, ui_to_render)
    
  })
  
  
  # ========= MOCKUP VERSION OLD =========
  output$sa_ui_placeholder4 <- renderUI({
    
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
    
    
    ui_to_render <- list(intro_line, scenario_box, main_question)
    
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
    
    
    # ========= OLD COSTS =========
    
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
      
      
      
      # Handle zero-car households that acquire new vehicles
      if (input$num_cars == "0") {
        if (!is.null(input$sa_0_car_add_options) && 
            grepl("Acquire a new OWNED vehicle", input$sa_0_car_add_options, ignore.case = TRUE)) {
          if (!is.null(input$sa_0_car_new_fuel) && !is.null(input$sa_0_car_new_mileage)) {
            total_cost <- total_cost + vehicle_monthly_cost_new(input$sa_0_car_new_fuel, input$sa_0_car_new_mileage)
          }
        }
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
    
    ui_to_render <- append(ui_to_render, list(hr(), 
                                              cost_box,
                                              hr(), 
                                              actionButton("to_summary_button", 
                                                           "I have decided, see summary", 
                                                           class = "btn-success btn-lg")))
    
    do.call(tagList, ui_to_render)
    
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