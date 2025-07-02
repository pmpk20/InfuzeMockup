# ui.R

library(shiny)

# Define the user interface
fluidPage(
  titlePanel("INFUZE: Future Travel Choices in Leeds (MVP)"),
  
  # Use a tabset to guide the user through the experiment
  tabsetPanel(
    id = "main_tabs",
    
    # =========== Part 1: Revealed Preferences (RP) ===========
    tabPanel(
      "Step 1: Your Current Travel",
      value = "rp_panel",
      sidebarLayout(
        sidebarPanel(
          h4("About Your Household"),
          # Use conditional panels to show car questions only if cars are owned
          numericInput("num_cars", "1. How many cars/vans does your household own?", value = 1, min = 0, step = 1),
          hr(),
          h4("Car Sharing & Public Transport"),
          radioButtons("car_share_member", "2. Are you a member of a car club (e.g., Co-Wheels)?", choices = c("Yes", "No"), selected = "No"),
          radioButtons("car_share_aware", "3. Is a car sharing service available within a 10-15 min walk of your home?", choices = c("Yes", "No", "Don't know"), selected = "No"),
          selectInput("pt_spend", "4. Roughly, how much does your household spend on public transport per month?",
                      choices = c("£0", "£1-£30", "£31-£60", "£61-£100", "More than £100"))
        ),
        mainPanel(
          h3("Details of Your Vehicle(s)"),
          # This panel will only appear if the user owns 1 or more cars
          conditionalPanel(
            condition = "input.num_cars > 0",
            wellPanel(
              h5("Main Household Car"),
              selectInput("car1_fuel", "Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
              numericInput("car1_vkm", "Approx. Annual Mileage (miles):", value = 8000, min = 0)
            )
          ),
          # This panel will only appear if the user owns 2 or more cars
          conditionalPanel(
            condition = "input.num_cars > 1",
            wellPanel(
              h5("Second Household Car"),
              selectInput("car2_fuel", "Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
              numericInput("car2_vkm", "Approx. Annual Mileage (miles):", value = 4000, min = 0)
            )
          ),
          conditionalPanel(
            condition = "input.num_cars == 0",
            p("Since you do not own a car, we will move on to the next section.")
          ),
          hr(),
          actionButton("to_sa_button", "Continue to Future Scenario ->", class = "btn-primary btn-lg")
        )
      )
    ),
    
    # =========== Part 2: Stated Adaptation (SA) ===========
    tabPanel(
      "Step 2: Future Scenario",
      value = "sa_panel",
      h3("Adapting to a Future Travel Scenario in Leeds"),
      p("Now, please imagine the following changes happen over the next year. We're interested in how your household might realistically adapt."),
      # --- The Scenario Box ---
      wellPanel(
        h4("Scenario"),
        tags$ul(
          tags$li(strong("Fuel Prices:"), " Increase by 60p per litre."),
          tags$li(strong("Parking Costs:"), " Annual residential parking permit costs double."),
          tags$li(strong("New Service - 'LeedsGo' Car Sharing:"),
                  tags$ul(
                    tags$li("Guaranteed availability within a 5-minute walk from your home."),
                    tags$li("Costs £5 per hour (includes fuel for first 10 miles)."),
                    tags$li("Easy app-based booking and reservation.")
                  )
          )
        ),
        p(strong("Impact Note:"), "In this scenario, running a petrol/diesel car will be significantly more expensive, while the 'LeedsGo' service offers a new, convenient alternative.")
      ),
      
      # --- Dynamic UI Placeholders ---
      # These will be filled by the server based on the branching logic
      uiOutput("sa_adaptation_ui"),
      
      hr(),
      actionButton("to_summary_button", "Lock In Choices & See Summary", class = "btn-success btn-lg")
    ),
    
    # =========== Part 3: Summary of Choices ===========
    tabPanel(
      "Step 3: Summary",
      value = "summary_panel",
      h3("Summary of Your Revealed Preferences and Adapted Choices"),
      p("This is the data that would be recorded for analysis from your responses."),
      verbatimTextOutput("summary_output")
    )
  )
)