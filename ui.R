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
    tabPanel("Choices: Version 1 Simple replace", value = "sa_panel", uiOutput("sa_ui_placeholder")),
    
    # =========== Part 2: Stated Adaptation (SA) - Placeholder ===========
    tabPanel("Choices: Version 1 Split PT", value = "sa_panel_2", uiOutput("sa_ui_placeholder2")),
    
    # =========== Part 2: Stated Adaptation (SA) - Placeholder ===========
    tabPanel("Choices: Version 3 Configurator", value = "sa_panel_3", uiOutput("sa_ui_placeholder3")),
    
    # =========== Part 2: Stated Adaptation (SA) - OLD ===========
    tabPanel("Choices: Version 4 Simple but keep not replace", value = "sa_panel_4", uiOutput("sa_ui_placeholder4")),
    
    # =========== Part 3: MENU BASED ===========
    tabPanel("Choices: Version 5 Menu based", value = "sa_panel_5", 
    
    
             fluidPage(
               title = "Mobility Choice Experiment",
               tags$head(
                 tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
                 tags$style(HTML("
      /* Category header */
      .menu-category {
        font-weight: 700;
        font-size: 1.0em;
        margin-top: 12px;
        background: linear-gradient(135deg, #007bff 0%, #0056b3 100%);
        color: white;
        padding: 8px 12px;
        border-radius: 4px;
      }

      /* Each choice row: label left, cost right */
      .menu-item {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 8px 10px;
        border-bottom: 1px solid #ececec;
      }
      .menu-item .label {
        margin-left: 6px; /* space after checkbox */
        font-weight: 500;
      }
      .menu-item .cost {
        white-space: nowrap;
        font-weight: 700;
      }

      /* Make the checkbox + custom content inline and tidy */
      .shiny-options-group .form-check {
        margin-bottom: 0;
        padding: 0;
      }
      .shiny-options-group .form-check input[type='checkbox'] {
        margin-left: 6px;
        margin-right: 6px;
      }

      /* Right column boxes */
      .scenario-box { background-color: #f8f9fa; border-left: 4px solid #007bff; margin-bottom: 18px; padding: 12px; }
      .cost-display {
        background-color: #e8f5e8;
        border: 2px solid #28a745;
        border-radius: 8px;
        padding: 12px;
        text-align: left;
        font-size: 1em;
        color: #155724;
        min-height: 120px;
      }
      .summary-box {
        background-color: #fff3cd;
        border: 1px solid #ffeaa7;
        border-radius: 5px;
        padding: 12px;
        min-height: 120px;
      }
      .confirm-row { text-align: center; margin-top: 12px; margin-bottom: 12px; }
    "))
               ),
    
    tabsetPanel(
      id = "main_tabs",
      tabPanel("Choices: Version 5 Menu based", value = "sa_panel_5",
               fluidPage(
                 # Scenario Description
                 wellPanel(
                   class = "scenario-box",
                   h3("Future Mobility Scenario - Leeds 2026"),
                   p(strong("Imagine the following changes have occurred:")),
                   tags$ul(
                     tags$li(strong("Push factors:"), "Fuel prices have increased by 40%, city centre parking now costs £15/day, and a new Clean Air Zone charges £12.50 for older petrol cars"),
                     tags$li(strong("Pull factors:"), "New comprehensive car sharing network launched, improved bike share system, and integrated public transport passes available")
                   ),
                   p(strong("Given these changes, what would your ideal monthly mobility package look like?"))
                 ),
                 
                 fluidRow(
                   column(8,
                          # Owned Vehicle (checkboxes now allow multiple choices)
                          div(class = "menu-category", tagList(icon("car"), " OWNED VEHICLE")),
                          checkboxGroupInput(
                            inputId = "owned_vehicle",
                            label = NULL,
                            choiceNames = list(
                              tags$div(class = "menu-item", tags$span(class = "label", "Keep my current Petrol Car"), tags$span(class = "cost", "£280")),
                              tags$div(class = "menu-item", tags$span(class = "label", "Replace with Small Electric Vehicle"), tags$span(class = "cost", "£420")),
                              tags$div(class = "menu-item", tags$span(class = "label", "Go Car-Free (rely on other services)"), tags$span(class = "cost", "£0"))
                            ),
                            choiceValues = c("petrol_car", "electric_car", "car_free"),
                            selected = c("petrol_car") # keep sensible default
                          ),
                          
                          # Public Transport (checkboxes)
                          div(class = "menu-category", tagList(icon("bus"), " PUBLIC TRANSPORT")),
                          checkboxGroupInput(
                            inputId = "public_transport",
                            label = NULL,
                            choiceNames = list(
                              tags$div(class = "menu-item", tags$span(class = "label", "None (pay-as-you-go)"), tags$span(class = "cost", "£0")),
                              tags$div(class = "menu-item", tags$span(class = "label", "Off-Peak Travel Pass"), tags$span(class = "cost", "£35")),
                              tags$div(class = "menu-item", tags$span(class = "label", "Full 'Leeds Travel Pass' (unlimited)"), tags$span(class = "cost", "£75"))
                            ),
                            choiceValues = c("pt_none", "pt_offpeak", "pt_unlimited"),
                            selected = c("pt_none")
                          ),
                          
                          # Car Sharing (optional)
                          div(class = "menu-category", tagList(icon("car-side"), " CAR SHARING (optional)")),
                          checkboxGroupInput(
                            inputId = "car_sharing",
                            label = NULL,
                            choiceNames = list(
                              tags$div(class = "menu-item", tags$span(class = "label", "Pay-as-you-go Access (occasional use)"), tags$span(class = "cost", "£25")),
                              tags$div(class = "menu-item", tags$span(class = "label", "Monthly Subscription (regular use)"), tags$span(class = "cost", "£85"))
                            ),
                            choiceValues = c("cs_payg", "cs_subscription"),
                            selected = NULL
                          ),
                          
                          # Bike Sharing (optional)
                          div(class = "menu-category", tagList(icon("bicycle"), " BIKE SHARING (optional)")),
                          checkboxGroupInput(
                            inputId = "bike_sharing",
                            label = NULL,
                            choiceNames = list(
                              tags$div(class = "menu-item", tags$span(class = "label", "Monthly Bike Share Pass"), tags$span(class = "cost", "£15"))
                            ),
                            choiceValues = c("bs_monthly"),
                            selected = NULL
                          )
                   ), # end left column
                   
                   column(4,
                          div(class = "cost-display",
                              h4("Total Estimated Monthly Cost"),
                              uiOutput("cost_breakdown")  # render HTML lines + total
                          ),
                          div(class = "summary-box",
                              h4("Your Chosen Package:"),
                              verbatimTextOutput("package_summary")
                          )
                   ) # end right column
                 ), # end fluidRow
                 
                 br(),
                 
                 div(class = "confirm-row",
                     actionButton("confirm_choice", "Confirm My Mobility Package", class = "btn-primary btn-lg")
                 ),
                 
                 br(),
                 
                 conditionalPanel(
                   condition = "input.confirm_choice > 0",
                   wellPanel(
                     style = "background-color: #d4edda; border-color: #c3e6cb;",
                     h4(style = "color: #155724;", "Thank you for your response!"),
                     p("Your mobility package choice has been recorded.")
                   )
                 )
               ) # end inner fluidPage
      ) # end tabPanel
    ) # end tabsetPanel
             ) # end fluidPage
    ),
    
    # =========== Part 5: Attitudes  - Placeholder ===========
    # Then in your tabPanel:
    tabPanel("Step 5: Attitudes", 
             value = "AttitudesPage",
             h3("About Your Life"),
             p("These questions are about big changes in your life in the last six months. Your honest answer is appreciated"),
             h5("ALL WORDING IS PROVISIONAL"),
             hr(),
             
             sliderInput(inputId = "SliderTest",
                         label = "Example sliding scale to show that we can do attitudes like this",
                         min = 0, max = 100, value = 50, ticks = FALSE,
                         width = "100%"),
             
             hr(),
             
             h4("How often do you do the following?"),
             div(class = "ethics-note",
                 p("CHECK: Just showing that we can do different response options here")
             ),
             # Header row
             div(style = "display: flex; margin-bottom: 10px;",
                 div(style = "width: 150px;"), 
                 lapply(freq_choices, function(x) {
                   div(style = "width: 100px; text-align: center; font-weight: bold;", x)
                 })
             ),
             
             # Data rows
             lapply(transport_modes, function(mode) {
               div(style = "display: flex; align-items: center; margin-bottom: 5px;",
                   div(style = "width: 150px;", strong(mode)),
                   div(style = "width: 400px;",
                       radioButtons(paste0("freq_", gsub(" ", "_", tolower(mode))), 
                                    label = NULL,
                                    choices = setNames(freq_choices, freq_choices),
                                    inline = TRUE,
                                    width = "100%")
                   )
               )
             }),
             
             hr(),
             actionButton("to_rp1_button", "Continue ->", class = "btn-primary")
    )
  )
)