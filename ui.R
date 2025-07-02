# ui.R (Version 3)

library(shiny)

# Define the user interface
fluidPage(
  # Use a custom style to handle the rotated image and highlighted text
  tags$head(
    tags$style(HTML("
      .rotated-image {
        max-width: 80vh; 
        max-height: 80vw;
        display: block;
        margin: auto;
        padding-top: 50px;
        padding-bottom: 50px;
      }
      .welcome-text {
        text-align: center;
      }
      .ethics-note {
        background-color: #FFFFB3; /* Light yellow background */
        border: 1px solid #E6E600;
        padding: 10px;
        border-radius: 5px;
        text-align: center;
        font-weight: bold;
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
      div(class = "welcome-text",
          h3("Help us shape the future of travel in Leeds!"),
          p("This is a survey mockup that does not store any data :) ")
      ),
      img(src = 'infuze_image.png', class = 'rotated-image', alt = "An artistic impression of a future Leeds streetscape with a tram, cyclists, and pedestrians."),
      hr(),
      # Highlighted ethics approval note
      div(class = "ethics-note",
          p("TODO: Insert full ethical approval information and participant consent details here.")
      ),
      hr(),
      div(style="text-align: center;",
          actionButton("to_demographics_button", "Start Survey ->", class = "btn-primary btn-lg")
      )
    ),
    
    # =========== NEW: Demographics Page ===========
    tabPanel(
      "Step 1: About You",
      value = "demographics_panel",
      h3("About You and Your Household"),
      p("These questions help us understand how travel choices differ between different groups of people. All responses are anonymous."),
      hr(),
      selectInput("gender", "Gender:", choices = c("Male", "Female", "In another way", "Prefer not to say")),
      selectInput("age_bracket", "Age:", choices = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")),
      selectInput("household_size", "Household Size (including yourself):", choices = c("1 - I live alone", "2", "3", "4", "5", "6+")),
      hr(),
      actionButton("to_rp1_button", "Continue ->", class = "btn-primary")
    ),
    
    # =========== Part 1a: Revealed Preferences (Vehicle Ownership) ===========
    tabPanel(
      "Step 2: Your Household's Vehicles",
      value = "rp1_panel",
      sidebarLayout(
        sidebarPanel(
          h4("Vehicle Ownership"),
          p("Please answer these questions about any vehicles that your household owns or has long-term access to."),
          selectInput(
            "num_cars", 
            label = "1. How many cars or vans does your household currently have?",
            choices = c("0", "1", "2", "3", "4+"),
            selected = "1"
          )
        ),
        mainPanel(
          h3("Details of Your Vehicle(s)"),
          p("Please provide details for up to your four most-used vehicles."),
          # --- Conditional Panels for each car ---
          conditionalPanel(
            condition = "input.num_cars != '0'",
            wellPanel(
              h5("Main (Most-Used) Household Vehicle"),
              selectInput("car1_type", "Vehicle Type:", choices = c("Car", "Van", "Motorbike")),
              selectInput("car1_fuel", "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
              selectInput("car1_mileage", "Approx. Annual Mileage (miles):", choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+"))
            )
          ),
          conditionalPanel(
            condition = "input.num_cars == '2' || input.num_cars == '3' || input.num_cars == '4+'",
            wellPanel(
              h5("Second Most-Used Household Vehicle"),
              selectInput("car2_type", "Vehicle Type:", choices = c("Car", "Van", "Motorbike")),
              selectInput("car2_fuel", "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
              selectInput("car2_mileage", "Approx. Annual Mileage (miles):", choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+"))
            )
          ),
          conditionalPanel(
            condition = "input.num_cars == '3' || input.num_cars == '4+'",
            wellPanel(
              h5("Third Most-Used Household Vehicle"),
              selectInput("car3_type", "Vehicle Type:", choices = c("Car", "Van", "Motorbike")),
              selectInput("car3_fuel", "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
              selectInput("car3_mileage", "Approx. Annual Mileage (miles):", choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+"))
            )
          ),
          conditionalPanel(
            condition = "input.num_cars == '4+'",
            wellPanel(
              h5("Fourth Most-Used Household Vehicle"),
              selectInput("car4_type", "Vehicle Type:", choices = c("Car", "Van", "Motorbike")),
              selectInput("car4_fuel", "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
              selectInput("car4_mileage", "Approx. Annual Mileage (miles):", choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+"))
            )
          ),
          conditionalPanel(
            condition = "input.num_cars == '0'",
            p("Since your household does not own a vehicle, we will move on to the next section.")
          ),
          hr(),
          actionButton("to_rp2_button", "Continue ->", class = "btn-primary")
        )
      )
    ),
    
    # =========== Part 1b: Revealed Preferences (Shared Mobility & PT) ===========
    tabPanel(
      "Step 3: Shared & Public Transport",
      value = "rp2_panel",
      h3("Your Household's Use of Other Transport"),
      hr(),
      radioButtons("car_share_member", "Are you currently a member of a car club/car sharing service in Leeds (e.g., Enterprise Car Club)?",
                   choices = c("Yes", "No"), selected = "No"),
      hr(),
      radioButtons("car_share_aware", "Are you aware of a car sharing service available within a 10 minute walk of your home?",
                   choices = c("Yes", "No", "Don't know"), selected = "No"),
      hr(),
      selectInput("pt_spend", "Approximately, how much would you say that your household spends on public transport per month?",
                  choices = c("£0", "£1-£30", "£31-£60", "£61-£100", "More than £100")),
      hr(),
      actionButton("to_sa_button", "Continue to Future Scenario ->", class = "btn-primary btn-lg")
    ),
    
    # =========== Part 2: Stated Adaptation (SA) - Placeholder ===========
    tabPanel("Step 4: Future Scenario", value = "sa_panel", uiOutput("sa_ui_placeholder")),
    
    # =========== Part 3: Summary - Placeholder ===========
    tabPanel("Step 5: Summary", value = "summary_panel", uiOutput("summary_ui_placeholder"))
  )
)