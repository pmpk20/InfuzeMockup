# server.R

library(shiny)

# Define the server logic
function(input, output, session) {
  
  # ========= Navigation Logic =========
  # When user clicks 'Continue' on RP tab, switch to the SA tab
  observeEvent(input$to_sa_button, {
    updateTabsetPanel(session, "main_tabs", selected = "sa_panel")
  })
  
  # When user clicks 'Lock In' on SA tab, switch to the Summary tab
  observeEvent(input$to_summary_button, {
    updateTabsetPanel(session, "main_tabs", selected = "summary_panel")
  })
  
  # ========= Dynamic UI for SA Section =========
  # This is the core of the branching logic.
  # It renders a set of questions based on the number of cars entered in the RP section.
  output$sa_adaptation_ui <- renderUI({
    
    # Ensure we have the number of cars before proceeding
    req(input$num_cars)
    
    # --- BRANCH A: 0-Car Households ---
    if (input$num_cars == 0) {
      tagList(
        h4("Your Adaptation Choices (0-Car Household):"),
        radioButtons("sa_0car_acquire_owned", "1. In this scenario, would you consider acquiring an OWNED car?",
                     choices = c("No, I would not get an owned car", "Yes, I would consider it")),
        # Show only if they say Yes to acquiring a car
        conditionalPanel(
          condition = "input.sa_0car_acquire_owned == 'Yes, I would consider it'",
          selectInput("sa_0car_new_type", "What type would you most likely get?",
                      choices = c("Small Petrol/Diesel", "Fully Electric", "Plug-in Hybrid"))
        ),
        hr(),
        radioButtons("sa_0car_adopt_cs", "2. Would you consider using the new 'LeedsGo' Car Sharing service?",
                     choices = c("No, I would not use it", "Yes, occasionally", "Yes, regularly"))
      )
      # --- BRANCH B: 1-Car Households ---
    } else if (input$num_cars == 1) {
      tagList(
        h4("Your Adaptation Choices (1-Car Household):"),
        radioButtons("sa_1car_decision", paste0("1. Regarding your current ", input$car1_fuel, " car:"),
                     choices = c("Keep, and use it similarly",
                                 "Keep, but aim to use it less",
                                 "Replace this car with a different OWNED vehicle",
                                 "Get rid of this car and NOT replace it with another OWNED vehicle")),
        hr(),
        radioButtons("sa_1car_adopt_cs", "2. Would you consider using the new 'LeedsGo' Car Sharing service?",
                     choices = c("No, I would not use it", "Yes, occasionally", "Yes, regularly")),
        # This question appears if they adopt car sharing, linking it to car shedding
        conditionalPanel(
          condition = "input.sa_1car_adopt_cs != 'No, I would not use it'",
          checkboxInput("sa_1car_cs_enables_shed", "Would using LeedsGo help you to get rid of your car, or decide not to replace it?", value = FALSE)
        )
      )
      # --- BRANCH C: 2+ Car Households ---
    } else {
      tagList(
        h4("Your Adaptation Choices (Multi-Car Household):"),
        # We use a grid-like layout with radio buttons for each car.
        h5("Decision for your MAIN car (", input$car1_fuel, "):"),
        radioButtons("sa_mcar_decision1", NULL,
                     choices = c("Keep, use similarly", "Keep, use less", "Replace", "Get Rid of"), inline = TRUE),
        h5("Decision for your SECOND car (", input$car2_fuel, "):"),
        radioButtons("sa_mcar_decision2", NULL,
                     choices = c("Keep, use similarly", "Keep, use less", "Replace", "Get Rid of"), inline = TRUE),
        hr(),
        selectInput("sa_mcar_total_fleet", "2. Overall, what would be the total number of OWNED cars in your household after these changes?",
                    choices = 0:input$num_cars, selected = input$num_cars),
        hr(),
        radioButtons("sa_mcar_adopt_cs", "3. Would you consider using the new 'LeedsGo' Car Sharing service?",
                     choices = c("No, I would not use it", "Yes, occasionally", "Yes, regularly"))
      )
    }
  })
  
  
  # ========= Summary Output Logic =========
  # This collects all the inputs and presents them as a clean summary.
  # This demonstrates what data we capture for modelling.
  output$summary_output <- renderPrint({
    
    # Create a list to hold the summary items
    summary_list <- list()
    
    # Part 1: Revealed Preferences
    summary_list$RP_Household_Cars <- input$num_cars
    if (input$num_cars > 0) {
      summary_list$RP_Car1_Fuel <- input$car1_fuel
      summary_list$RP_Car1_Mileage <- input$car1_vkm
    }
    if (input$num_cars > 1) {
      summary_list$RP_Car2_Fuel <- input$car2_fuel
      summary_list$RP_Car2_Mileage <- input$car2_vkm
    }
    summary_list$RP_Current_Car_Share_Member <- input$car_share_member
    
    # Part 2: Stated Adaptation Choices (Branching)
    summary_list$SA_Scenario_Presented <- "Fuel +60p, Parking x2, LeedsGo Car Share available"
    
    if(input$num_cars == 0) {
      summary_list$SA_Adaptation_Bundle <- paste(
        "Acquire Owned:", input$sa_0car_acquire_owned,
        "| Adopt Car Share:", input$sa_0car_adopt_cs
      )
    } else if (input$num_cars == 1) {
      summary_list$SA_Adaptation_Bundle <- paste(
        "Decision for Owned Car:", input$sa_1car_decision,
        "| Adopt Car Share:", input$sa_1car_adopt_cs,
        "| CS enables shedding:", input$sa_1car_cs_enables_shed
      )
    } else { # 2+ cars
      summary_list$SA_Adaptation_Bundle <- paste(
        "Decision Car 1:", input$sa_mcar_decision1,
        "| Decision Car 2:", input$sa_mcar_decision2,
        "| Final Owned Fleet Size:", input$sa_mcar_total_fleet,
        "| Adopt Car Share:", input$sa_mcar_adopt_cs
      )
    }
    
    # Print the list
    print(summary_list)
  })
}