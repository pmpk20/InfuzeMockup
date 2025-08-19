# ui.R (Version 3)

library(shiny)
library(shinyBS)
library(shinydashboard)

# Define the frequency options
freq_choices <- c("Never", "Monthly", "Weekly", "Daily")
transport_modes <- c("Taxi", "Hire car", "Car share", "Borrow", "Use my own car")


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
      
      .github-callout {
  margin: 30px auto; /* Add space and center it */
  max-width: 700px;
  padding: 20px;
  background-color: #f8f9fa; /* A light grey background */
  border: 1px solid #dee2e6;
  border-left: 5px solid #333; /* A dark left border for emphasis */
  border-radius: 8px;
  text-align: center;
}
    "))
  ),
  
  titlePanel("INFUZE: Future Travel Choices in Leeds (MVP)"),
  
  tabsetPanel(
    id = "main_tabs",
    
    # =========== NEW: Landing Page ===========
    tabPanel(
      "Landing page + welcome",
      value = "welcome_panel",
      div(class = "welcome-text",
          h3("Help us shape the future of travel in Leeds!"),
          h4("This is a survey mockup that does not store any data :) ")
      ),
      
      # --- NEW: GitHub Callout Box ---
      div(class = "github-callout",
          h5("This is an open-source project"),
          p("You can explore, download, or contribute to the code on GitHub.", style = "margin-bottom: 15px;"),
          tags$a(href = "https://github.com/pmpk20/InfuzeMockup",
                 target = "_blank", # Opens in a new tab
                 rel = "noopener noreferrer", # Security best practice for external links
                 class = "btn btn-dark btn-lg", # Styles the link as a large, dark button
                 icon("github"), # Adds the GitHub icon
                 " View the Code on GitHub"
          )
      ),
      
      img(src = 'infuze_image.png', class = 'rotated-image', alt = "An artistic impression..."),
      hr(),
      
      # Highlighted ethics approval note
      div(class = "ethics-note",
          p("TODO: Insert full ethical approval information and participant consent details here.")
      ),
      hr(),
      div(style="text-align: center;",
          actionButton("FromLandingTo1A_Button", "Start Survey ->", class = "btn-primary btn-lg")
      )
    ),
    
    # =========== STEP 1A: ABOUT YOU ===========
    tabPanel(
      "Step 1A: Screeners",
      value = "Step_1A_Screeners",
      h3("About You"),
      p("These questions help us understand how travel choices differ between different groups of people. All responses are anonymous."),
      
      div(class = "ethics-note",
          p("TODO: Change survey logic to screen out <18, no licence, ")
      ),
      hr(),
      selectInput("gender", 
                  label = "How would you describe your gender?",
                  width = "50%",
                  choices = c("Male", "Female", "In another way", "Prefer not to say")),
      
      selectInput("age_bracket", 
                  "How old are you?", 
                  width = "50%",
                  choices = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")),

      selectInput("drivers_licence", 
                  "Do you currently hold a full valid UK driving licence?", 
                  width = "50%",
                  choices = c("Yes", "No")),
      
      selectInput("education", 
                  "Please select the highest level of education that you have completed:", 
                  width = "50%",
                  choices = c(
                    "No qualifications",
                    "1 - 4 Levels / CSEs / GCSEs, NVQ Level 1",
                    "5 + O Levels / CSEs / GCSEs, NVQ Level 2, AS Levels, Higher Diploma, Diploma Apprenticeship",
                    "2 + A Levels, NVQ Level 3, BTEC National Diploma",
                    "Degree, Higher Degree, NVQ level 4-5, BTEC Higher Level, professional qualifications (e.g. teaching, nursing, accountancy)",
                    "Doctoral degree or equivalent.",
                    "Other qualifications (vocational/work related, foreign qualifications or level unknown)")),
                    
      selectInput("Income", 
                  "Please select the bracket that represents your total (before tax) monthly household income:", 
                  width = "50%",
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

      
      
      hr(),
      actionButton("From1ATo1B_Button", "Continue ->", class = "btn-primary")
    ),
    
    
    
    # =========== STEP 1B: EMPLOYMENT ===========
    tabPanel(
      "Step 1b: Employment",
      value = "Step_1B_Employment",
      h3("About You"),
      p("These questions help us understand how travel choices differ between different groups of people. All responses are anonymous."),

      hr(),
      
      # Remove selected defaults first
      selectInput("1BEmployment_Occupation", "Which of these descriptions best fits your current occupation?", 
                  width = "50%",
                  choices = c(                    "Homemaker / housewife or house husband",
                                                  "Student / Full time education",
                              "Retired",
                              "Unemployed / on benefit",
                              "Factory / manual worker",
                              "Crafts / tradesperson / skilled worker",
                              "Office / clerical / administration",
                              "Middle management",
                              "Senior management",
                              "Professional")),
      
      selectInput("1BEmployment_WFH", 
                  "How often do your work from home?",
                  width = "50%",
                  selected = "Daily",
                  choices = c("Never",
                              "Less often than weekly",
                              "A few times a week",
                              "Daily")),
      
      div(class = "ethics-note",
          p("CHECK: Travel mode and time to work only appear if no WFH")
      ),
      
      conditionalPanel(
        condition = "input['1BEmployment_WFH'] != 'Daily'",
        selectInput("1BEmployment_TravelType", 
                    "How do you typically travel to your place of work?",
                    width = "50%",
                    choices = c("My own vehicle", "Car club / sharing", "Public transport", "Walk / bike / other active travel")),
        
        selectInput("1BEmployment_TravelTime", 
                    "How long does it take for you to travel to your place of work?",
                    width = "50%",
                    choices = c("More than 1 hour", "Between 30 - 60 minutes", "Between 10 - 30 minutes", "Fewer than 10 minutes"))
      ),
      
      hr(),
      actionButton("to_demographics_yours_button", "Continue ->", class = "btn-primary")
    ),
    
    
    
    # =========== STEP 1C: HOUSEHOLD ===========
    tabPanel(
      "Step 1C: About Your household",
      value = "demographics_panel_yours",
      h3("About Your Household"),
      p("These questions help us understand how travel choices differ between different groups of people. All responses are anonymous."),
      hr(),
      
      
      # --- NEW: Dynamic Household Composition Section ---
      h3("Household Composition"),
      p("Please add each member of your household, including yourself. This helps us understand travel patterns within families."),
      
      actionButton("add_member_btn", "Add Household Member", 
                   class = "btn-success", icon = icon("user-plus")),
      hr(),
      
      # This is the placeholder where the server will insert new member rows
      div(id = "household_member_placeholder"),
      
      hr(style = "border-top: 2px solid #ccc; margin-top: 30px;"),
      
      # --- Original Questions Moved Below ---
      h3("About Your Household (Overall)"),
      p("These questions are about your household as a whole."),
      
      selectInput("Urbanicity", 
                  "Would you say that the area you live in is mostly rural or urban?", 
                  width = "100%",
                  choices = c("Rural", "Urban")),
      
      selectInput("household_size", 
                  "How many adults (i.e., over 18 years old) including yourself currently live in your household:", 
                  width = "100%",
                  choices = c("1 - I live alone", "2", "3", "4", "5+")),
      
      
      
      selectInput("Urbanicity", 
                  "Would you say that the area you live in is mostly rural or urban?", 
                  width = "50%",
                  choices = c("Rural", "Urban")),
      
      selectInput("household_size", 
                  "How many adults (i.e., over 18 years old) including yourself currently live in your household:", 
                  width = "50%",
                  choices = c("1 - I live alone", "2", "3", "4", "5+")),
      
      selectInput("household_size_5", 
                  "How many children younger than 5 years old currently live in your household:", 
                  width = "50%",
                  choices = c("0", "1", "2", "3", "4", "5+")),
      
      selectInput("household_size_12", 
                  "How many children between 5 and 12 years old) currently live in your household:", 
                  width = "50%",
                  choices = c("0", "1", "2", "3", "4", "5+")),
      
      selectInput("household_size_17", 
                  "How many teenagers (i.e., 13-18 years old) currently live in your household:", 
                  width = "50%",
                  choices = c("0", "1", "2", "3", "4", "5+")),
      
      selectInput("household_change_adults", 
                  "How many new adults have started living in your household in the last six months:", 
                  width = "50%",
                  choices = c("0", "1", "2", "3", "4", "5+")),
      
      selectInput("household_change_children", 
                  "How many new children (<18yrs) have started living in your household in the last six months:", 
                  width = "50%",
                  choices = c("0", "1", "2", "3", "4", "5+")),
      
      selectInput("household_change_impairment", 
                  "How many people in your household have limited physical mobility?", 
                  width = "50%",
                  choices = c("0", "1", "2", "3", "4", "5+")),
      
      
      hr(),
      actionButton("to_demographics_life_button", "Continue ->", class = "btn-primary")
    ),
    
    
    # =========== STEP 1D: LIFE ===========
    tabPanel(
      "Step 1D: About Your Life",
      value = "demographics_panel_life",
      h3("About Your Life"),
      p("These questions are about big changes in your life in the last six months. Your honest answer is appreciated"),
      h5("ALL WORDING IS PROVISIONAL"),
      hr(),
      
      selectInput("life_moved_job", 
                  width = "50%",
                  "Have you changed jobs?", 
                  choices = c("Yes", "No")),
      
      selectInput("life_moved_job", 
                  width = "50%",
                  "Have you started working in a new location?", 
                  choices = c("Yes", "No")),
      
      selectInput("life_moved_house", 
                  width = "50%",
                  "Have you moved house?", 
                  choices = c("Yes", "No")),
      
      selectInput("life_crime", 
                  width = "50%",
                  "Have you been a victim of crime in your local area?", 
                  choices = c("Yes", "No")),
      
      selectInput("life_health", 
                  width = "50%",
                  "Has your health status worsened?", 
                  choices = c("Yes", "No")),
      
      selectInput("life_health", 
                  width = "50%",
                  "Has your mobility worsened?", 
                  choices = c("Yes", "No")),
      
      hr(),
      actionButton("to_rp1_button", "Continue ->", class = "btn-primary")
    ),
    
    
    
    # =========== Part 1a: Revealed Preferences (Vehicle Ownership) ===========
    tabPanel(
      "Step 2A: Your Household's Vehicles",
      value = "rp1_panel",
      sidebarLayout(
        sidebarPanel(
          h4("Vehicle Ownership"),
          p("Please answer these questions about any vehicles that your household owns or has long-term access to."),
          selectInput(
            "num_cars", 
            width = "50%",
            label = "1. How many vehicles (cars, vans, motorbikes) does your household currently have?",
            choices = c("0", "1", "2", "3", "4+"),
            selected = "1"
          )
        ),
        mainPanel(
          h3("Details of Your Vehicle(s)"),
          p("Please provide details for up to your four most-used vehicles. You can collapse each box by clicking on the header text."),
          
          # Vehicle 1
          conditionalPanel(
            condition = "input.num_cars != '0'",
            bsCollapse(
              id = "car1_collapse",
              open = "car1_panel",
              bsCollapsePanel(
                title = "Main (Most-Used) Household Vehicle",
                value = "car1_panel",
                selectInput("car1_type", "Vehicle Type:", choices = c("Car", "Van", "Motorbike")),
                selectInput("car1_fuel", "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
                selectInput("car1_mileage", "Approx. Annual Mileage (miles):", choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+")),
                selectInput("car1_age", "How long have you owned this vehicle?", choices = c("<1 year", "1-2 years", "More than 2 years"))
              )
            )
          ),
          
          # Vehicle 2
          conditionalPanel(
            condition = "input.num_cars == '2' || input.num_cars == '3' || input.num_cars == '4+'",
            bsCollapse(
              id = "car2_collapse",
              bsCollapsePanel(
                title = "Second Most-Used Household Vehicle",
                value = "car2_panel",
                selectInput("car2_type", "Vehicle Type:", choices = c("Car", "Van", "Motorbike")),
                selectInput("car2_fuel", "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
                selectInput("car2_mileage", "Approx. Annual Mileage (miles):", choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+")),
                selectInput("car2_age", "How long have you owned this vehicle?", choices = c("<1 year", "1-2 years", "More than 2 years"))
              )
            )
          ),
          
          # Vehicle 3
          conditionalPanel(
            condition = "input.num_cars == '3' || input.num_cars == '4+'",
            bsCollapse(
              id = "car3_collapse",
              bsCollapsePanel(
                title = "Third Most-Used Household Vehicle",
                value = "car3_panel",
                selectInput("car3_type", "Vehicle Type:", choices = c("Car", "Van", "Motorbike")),
                selectInput("car3_fuel", "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
                selectInput("car3_mileage", "Approx. Annual Mileage (miles):", choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+")),
                selectInput("car3_age", "How long have you owned this vehicle?", choices = c("<1 year", "1-2 years", "More than 2 years"))
              )
            )
          ),
          
          # Vehicle 4
          conditionalPanel(
            condition = "input.num_cars == '4+'",
            bsCollapse(
              id = "car4_collapse",
              bsCollapsePanel(
                title = "Fourth Most-Used Household Vehicle",
                value = "car4_panel",
                selectInput("car4_type", "Vehicle Type:", choices = c("Car", "Van", "Motorbike")),
                selectInput("car4_fuel", "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid")),
                selectInput("car4_mileage", "Approx. Annual Mileage (miles):", choices = c("0-2,000", "2,001-5,000", "5,001 - 10,000", "10,001+")),
                selectInput("car4_age", "How long have you owned this vehicle?", choices = c("<1 year", "1-2 years", "More than 2 years"))
              )
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
      "Step 2B: Shared & Public Transport",
      
      value = "rp2_panel",
      
      h3("Your Household's Use of Other Transport"),
      
      hr(),
      
      radioButtons("car_share_member", 
                   "Are you currently a member of a car club/car sharing service in Leeds (e.g., Enterprise Car Club)?",
                   width = "50%",
                   choices = c("Yes", "No"), selected = "No"),
      
      hr(),
      
      radioButtons("car_share_aware", 
                   "Are you aware of a car sharing service available within a 10 minute walk of your home?",
                   width = "50%",
                   choices = c("Yes", "No", "Don't know"), selected = "No"),
      
      hr(),
      
      selectInput("pt_spend", 
                  "Approximately, how much would you say that your household spends on public transport per month?",
                  width = "50%",
                  choices = c("£0", "£1-£30", "£31-£60", "£61-£100", "More than £100")),
      
      hr(),
      
      actionButton("to_sa_button", "Continue to Stated Adaptation ->", class = "btn-primary btn-lg")
    ),
    
    # =========== Part 2: Stated Adaptation (SA) - Placeholder ===========
    # tabPanel("Choices: Version 1 Simple replace", value = "sa_panel", uiOutput("sa_ui_placeholder")),
    
    # =========== Part 2: Stated Adaptation (SA) - Placeholder ===========
    tabPanel("Choices: Version 1: CE", value = "sa_panel_2", uiOutput("sa_ui_placeholder2")),
    
    # =========== Part 2: Stated Adaptation (SA) - OLD ===========
    tabPanel("Choices: Version 2: CE Packages", value = "sa_panel_3", uiOutput("sa_ui_placeholder3")),
    
    # =========== Part 2: Stated Adaptation (SA) - Placeholder ===========
    tabPanel("Choices: Version 3: Configurator", value = "sa_panel_4", uiOutput("sa_ui_placeholder4")),

    # =========== Part 3: MENU BASED ===========
    tabPanel("Choices: Version 4: Menu", value = "sa_panel_5", uiOutput("sa_ui_placeholder5")),
    
    # =========== Part 3B: MENU BASED ===========
    tabPanel("Choices: Version 5: Different Menu", value = "sa_panel_6", uiOutput("sa_ui_placeholder6")),
    
  )
)