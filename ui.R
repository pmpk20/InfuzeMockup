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
          actionButton("to_demographics_you_button", "Start Survey ->", class = "btn-primary btn-lg")
      )
    ),
    
    # =========== STEP 1: ABOUT YOU ===========
    tabPanel(
      "Step 1: About You",
      value = "demographics_panel_you",
      h3("About You"),
      p("These questions help us understand how travel choices differ between different groups of people. All responses are anonymous."),
      hr(),
      selectInput("gender", "How would you describe your gender?", 
                  choices = c("Male", "Female", "In another way", "Prefer not to say")),
      
      selectInput("age_bracket", "How old are you?", 
                  choices = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")),

      selectInput("education", "Please select the highest level of education that you have completed:", 
                  choices = c(
                    "No qualifications",
                    "1 - 4 Levels / CSEs / GCSEs, NVQ Level 1",
                    "5 + O Levels / CSEs / GCSEs, NVQ Level 2, AS Levels, Higher Diploma, Diploma Apprenticeship",
                    "2 + A Levels, NVQ Level 3, BTEC National Diploma",
                    "Degree, Higher Degree, NVQ level 4-5, BTEC Higher Level, professional qualifications (e.g. teaching, nursing, accountancy)",
                    "Doctoral degree or equivalent.",
                    "Other qualifications (vocational/work related, foreign qualifications or level unknown)")),
                    
      selectInput("Income", "Please select the bracket that represents your total (before tax) monthly household income:", 
                  choices = c(
      "£0 to £1, 000",
      "£1, 001 to £1, 500",
      "£1, 501 to £2, 000",
      "£2, 001 to £2, 500",
      "£2, 501 to £3, 000",
      "£3, 001 to £3, 500",
      "£3, 501 to £4, 000",
      "£4, 001 to £6, 000",
      "£6, 001 or more",
      "Prefer not to say.")),
      
      selectInput("Urbanicity", "Would you say that the area you live in is mostly rural or urban?", 
                  choices = c("Rural", "Urban")),
      
      
      hr(),
      actionButton("to_demographics_yours_button", "Continue ->", class = "btn-primary")
    ),
    
    
    
    # =========== STEP 1B: ABOUT YOUR HOUSEHOLD ===========
    tabPanel(
      "Step 1b: About Your household",
      value = "demographics_panel_yours",
      h3("About Your Household"),
      p("These questions help us understand how travel choices differ between different groups of people. All responses are anonymous."),
      hr(),
     
      selectInput("household_size", "How many adults (i.e., over 18 years old) including yourself currently live in your household:", 
                  choices = c("1 - I live alone", "2", "3", "4", "5+")),
      
      selectInput("household_size_5", "How many children younger than 5 years old currently live in your household:", 
                  choices = c("0", "1", "2", "3", "4", "5+")),
      
      selectInput("household_size_12", "How many children between 5 and 12 years old) currently live in your household:", 
                  choices = c("0", "1", "2", "3", "4", "5+")),
      
      selectInput("household_size_17", "How many teenagers (i.e., 13-18 years old) currently live in your household:", 
                  choices = c("0", "1", "2", "3", "4", "5+")),
      
      selectInput("household_change_adults", 
                  "How many new adults have started living in your household in the last six months:", 
                  choices = c("0", "1", "2", "3", "4", "5+")),
      
      selectInput("household_change_children", 
                  "How many new children (<18yrs) have started living in your household in the last six months:", 
                  choices = c("0", "1", "2", "3", "4", "5+")),
      
      
      hr(),
      actionButton("to_demographics_life_button", "Continue ->", class = "btn-primary")
    ),
    
    
    # =========== STEP 1C: ABOUT YOUR LIFE ===========
    tabPanel(
      "Step 1b: About Your Life",
      value = "demographics_panel_life",
      h3("About Your Life"),
      p("These questions are about big changes in your life in the last six months. Your honest answer is appreciated"),
      h5("ALL WORDING IS PROVISIONAL"),
      hr(),
      
      selectInput("life_moved_job", 
                  "Have you changed jobs?", 
                  choices = c("Yes", "No")),
      
      selectInput("life_moved_job", 
                  "Have you started working in a new location?", 
                  choices = c("Yes", "No")),
      
      selectInput("life_moved_house", 
                  "Have you moved house?", 
                  choices = c("Yes", "No")),
      
      selectInput("life_crime", 
                  "Have you been a victim of crime in your local area?", 
                  choices = c("Yes", "No")),
      
      selectInput("life_health", 
                  "Has your health status worsened?", 
                  choices = c("Yes", "No")),
      
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
            label = "1. How many vehicles (cars, vans, motorbikes) does your household currently have?",
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