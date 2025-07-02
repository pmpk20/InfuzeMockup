# ui.R

library(shiny)

# Define the user interface
fluidPage(
  # Use a custom style to handle the rotated image
  tags$head(
    tags$style(HTML("
            .rotated-image {
                max-width: 80vh; /* Control size relative to viewport height */
                max-height: 80vw; /* Control size relative to viewport width */
                display: block;
                margin: auto;
                padding-top: 50px;
                padding-bottom: 50px;
            }
        "))
  ),
  
  titlePanel("INFUZE: Future Travel Choices in Leeds (MVP)"),
  
  tabsetPanel(
    id = "main_tabs",
    
    # =========== NEW: Landing Page ===========
    tabPanel(
      "Welcome",
      value = "welcome_panel",
      h3("Help us shape the future of travel in Leeds!"),
      p("This is a survey mockup that does not store any data :) "),
      # p("This short survey explores how households like yours might adapt their travel choices in response to new transport options and costs. Your anonymous responses are vital for the INFUZE project, which aims to make travel in Leeds greener and more accessible."),
      # Display the rotated image
      # NOTE: Place your image file named 'infuze_image.jpg' in a folder named 'www' inside your Shiny app directory.
      img(src = 'infuze_image.png', class = 'rotated-image', alt = "An artistic impression of a future Leeds streetscape with a tram, cyclists, and pedestrians."),
      hr(),
      p("TODO: Insert ethical approval information"),
      p("Please click the button below to begin."),
      div(style="text-align: center;",
          actionButton("to_rp_button", "Start Survey ->", class = "btn-primary btn-lg")
      )
    ),
    
    # =========== Part 1: Revealed Preferences (RP) ===========
    tabPanel(
      "Step 1: Your Current Travel",
      value = "rp_panel",
      sidebarLayout(
        sidebarPanel(
          h4("About Your Household"),
          # Changed to a dropdown (selectInput) as requested
          # This is the correct line for ui.R
          selectInput(
            "num_cars", 
            label = "1. How many cars or vans does your household own or have long-term access to?",
            choices = c("0", "1", "2", "3", "4+"),
            selected = "1"
          ),
          hr(),
          h4("Shared Mobility & Public Transport"),
          radioButtons("car_share_member", "2. Are you currently a member of a car club/car sharing service in Leeds (e.g., Enterprise Car Club)?",
                       choices = c("Yes", "No"), selected = "No"),
          # Rephrased question as requested
          radioButtons("car_share_aware", "3. Are you aware of a car sharing service available within a 10 minute walk of your home?",
                       choices = c("Yes", "No", "Don't know"), selected = "No"),
          # Rephrased question as requested
          selectInput("pt_spend", "4. Approximately, how much would you say that your household spends on public transport per month?",
                      choices = c("£0", "£1-£30", "£31-£60", "£61-£100", "More than £100"))
        ),
        mainPanel(
          h3("Details of Your Vehicle(s)"),
          p("Please provide details for the one or two most-used vehicles in your household."),
          # This panel will only appear if the user owns 1 or more cars
          conditionalPanel(
            condition = "input.num_cars != '0'",
            wellPanel(
              h5("Main (Most-Used) Household Vehicle"),
              # Added vehicle type dropdown
              selectInput("car1_type", "Vehicle Type:", choices = c("Car", "Van", "Motorbike")),
              selectInput("car1_fuel", "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
              # Changed mileage to a slider
              selectInput(
                "car1_mileage",
                "Approx. Annual Mileage (miles):",
                choices = c("0-2,000",
                  "2,001-5,000",
                  "5,001 - 10,000",
                  "10,000+"))
                # sliderInput("car1_mileage", "Approx. Annual Mileage (miles):", min = 0, max = 30000, value = 8000, step = 500)
            )
          ),
          # This panel will only appear if the user owns 2 or more cars
          conditionalPanel(
            # condition = "input.num_cars == '2'",
            condition = "input.num_cars == '2' || input.num_cars == '3' || input.num_cars == '4+'",
            wellPanel(
              h5("Second Most-Used Household Vehicle"),
              selectInput("car2_type", "Vehicle Type:", choices = c("Car", "Van", "Motorbike")),
              selectInput("car2_fuel", "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
              selectInput(
                "car2_mileage",
                "Approx. Annual Mileage (miles):",
                choices = c("0-2,000",
                  "2,001-5,000",
                  "5,001 - 10,000",
                  "10,000+"))
            )
          ),
          conditionalPanel(
            condition = "input.num_cars == '3' || input.num_cars == '4+'",
            wellPanel(
              h5("Third (Most-Used) Household Vehicle"),
              # Added vehicle type dropdown
              selectInput("car3_type", "Vehicle Type:", choices = c("Car", "Van", "Motorbike")),
              selectInput("car3_fuel", "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
              # Changed mileage to a slider
              selectInput(
                "car3_mileage",
                "Approx. Annual Mileage (miles):",
                choices = c("0-2,000",
                  "2,001-5,000",
                  "5,001 - 10,000",
                  "10,000+"))
            )
          ),
          conditionalPanel(
            condition = "input.num_cars == '4+'",
            wellPanel(
              h5("Fourth (Most-Used) Household Vehicle"),
              # Added vehicle type dropdown
              selectInput("car4_type", "Vehicle Type:", choices = c("Car", "Van", "Motorbike")),
              selectInput("car4_fuel", "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
              # Changed mileage to a slider
              selectInput(
                "car4_mileage",
                "Approx. Annual Mileage (miles):",
                choices = c("0-2,000",
                  "2,001-5,000",
                  "5,001 - 10,000",
                  "10,000+"))
              )
            ),
          conditionalPanel(
            condition = "input.num_cars == '0'",
            p("Since you do not own a car, we will move on to the next section when you are ready.")
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