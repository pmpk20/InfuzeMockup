# ui.R (Version 3)

library(shiny)
library(shinyBS)
library(shinydashboard)

# Define the frequency options
freq_choices <- c("Never", "Monthly", "Weekly", "Daily")
transport_modes <- c("Taxi", "Hire car", "Car club", "Borrow", "Use my own car")


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
      
      # --- NEW: Logos for Authority ---
      fluidRow(
        column(12, align = "center",
               # You would place your logo image files in the 'www' sub-directory
               # tags$img(src = 'UoL_logo.png', height = '80px', style = "margin-right: 20px;"),
               # tags$img(src = 'INFUZE_logo.png', height = '80px'),
               # h3("Help Shape the Future of Travel in Leeds"),
               img(src = 'INFUZE_Web_Logotype_For_Light_RGB_Clear.png', class = 'rotated-image', alt = "An artistic impression..."),
        )
      ),
      hr(),
      
      # --- REVISED: Introduction ---
      div(class = "welcome-text",
          p("Welcome to the INFUZE survey, a research project by the University of Leeds.", style = "font-size: 1.2em;"),
          p("Your views are vital for understanding how new transport options could work for the people of Leeds. 
            By taking part, you will help policymakers make better, more informed decisions about our city's transport future.", style = "max-width: 700px; margin: auto;")
      ),
      
      # --- NEW: Key Information Box ---
      div(style = "max-width: 700px; margin: 30px auto; padding: 20px; background-color: #f8f9fa; border-radius: 8px; border: 1px solid #dee2e6;",
          h4("Key Information", style="margin-top:0; text-align:center;"),
          tags$ul(style="list-style-type: none; padding-left: 0;",
                  
                  tags$li(style="display: flex; align-items: center; margin-bottom: 10px;", icon("clock", "fa-2x"), 
                          tags$span(style="margin-left: 15px; font-size: 1.1em;", "This survey takes approximately 10-15 minutes to complete.")),
                  
                  tags$li(style="display: flex; align-items: center; margin-bottom: 10px;", icon("user-shield", "fa-2x"), 
                          tags$span(style="margin-left: 15px; font-size: 1.1em;", "All your responses are completely anonymous and will be used for academic research purposes only.")),
                  
                  tags$li(style="display: flex; align-items: center;", icon("check-circle", "fa-2x"), 
                          tags$span(style="margin-left: 15px; font-size: 1.1em;", "Participation is voluntary, and you can withdraw at any time.")),
                  
                  tags$li(style="display: flex; align-items: center;", icon("github", "fa-2x"), 
                          tags$span(style="margin-left: 15px; font-size: 1.1em;", 
                                    tags$a(href = "https://github.com/pmpk20/InfuzeMockup",
                                           target = "_blank", rel = "noopener noreferrer", " All code is publicly available on Github"
                                    )))
          ),
      ),
      
      # Highlighted ethics approval note - now with better text
      div(class = "ethics-note",
          p("Insert ethical approval when available.
            Please click 'Start Survey' or click the Headers at the top to step through the survey")
      ),
      hr(),
      
      # The start button remains the same
      div(style="text-align: center;",
          actionButton("FromLandingTo1A_Button", "Start Survey", class = "btn-primary btn-lg")
      ),
      
      hr()
    ),
    
    # =========== STEP 1: Screeners ===========
    tabPanel(
      "Step 1: Screeners",
      value = "Step_1A_Screeners",
      h3("About You"),
      p("These questions help us understand how travel choices differ between different groups of people. All responses are anonymous."),
      hr(),
      
      selectInput("gender", "How would you describe your gender?", width = "50%",
                  choices = c("Male", "Female", "In another way", "Prefer not to say")),
      
      selectInput("age_bracket", "How old are you?", width = "50%",
                  choices = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")),
      
      selectInput("household_children", "How many children under the age of 16 live in your household?",
                  width = "50%", choices = c("0", "1", "2", "3 or more")),
      
      selectInput("drivers_licence", "Do you currently hold a full valid UK driving licence?", width = "50%",
                  choices = c("Yes", "No")),
      
      conditionalPanel(
        condition = "input.drivers_licence == 'No'",
        selectInput("licence_intent", "Do you intend to get a driving licence in the next 12 months?",
                    width = "40%",
                    choices = c("Yes", "No"), selected = character(0))
      ),
      
      # --- NEW ABILITY QUESTIONS ---
      selectInput("pt_access", "Is there a bus stop or train station within a 10-minute walk of your home?",
                  width = "50%", choices = c("Yes", "No", "I don't know")),
      
      selectInput("at_ability", "Are you physically able to walk or cycle for a 15-minute journey?",
                  width = "50%", choices = c("Yes, both", "Walking only", "Cycling only", "No, neither")),
      # --- END NEW QUESTIONS ---
      
      selectInput("education", "Please select the highest level of education that you have completed:", width = "50%",
                  choices = c("No qualifications", "1 - 4 Levels / CSEs / GCSEs, NVQ Level 1", "5 + O Levels / CSEs / GCSEs, NVQ Level 2, AS Levels, Higher Diploma, Diploma Apprenticeship", "2 + A Levels, NVQ Level 3, BTEC National Diploma", "Degree, Higher Degree, NVQ level 4-5, BTEC Higher Level, professional qualifications (e.g. teaching, nursing, accountancy)", "Doctoral degree or equivalent.", "Other qualifications (vocational/work related, foreign qualifications or level unknown)")),
      
      selectInput("Income", "Please select the bracket that represents your total (before tax) monthly household income:", width = "50%",
                  choices = c("£0 to £1,000", "£1,001 to £1,500", "£1,501 to £2,000", "£2,001 to £2,500", "£2,501 to £3,000", "£3,001 to £3,500", "£3,501 to £4,000", "£4,001 to £6,000", "£6,001 or more", "Prefer not to say.")),
      
      hr(),
      actionButton("From1ATo1B_Button", "Continue ->", class = "btn-primary")
    ),
    
    
    # =========== STEP 2: Your Household's Vehicles ===========
    tabPanel(
      "Step 2: Your Household's Vehicles",
      value = "rp1_panel",
      
      h3("Your Household's Vehicles"),
      p("Please answer these questions about any vehicles that your household currently owns or has long-term access to."),
      hr(),
      
      h4("Step 1: Current Vehicle Count"),
      selectInput("num_cars", "How many vehicles (cars, vans, motorbikes) does your household currently have?", 
                  choices = c("0", "1", "2", "3", "4+"), 
                  selected = "1", width = "400px"),
      hr(),
      
      h4("Step 2: Vehicle Details & Current Use"),
      
      conditionalPanel(
        condition = "input.num_cars != '0'",
        p("Please provide details for the vehicle(s) below, including how you currently use them.")
      ),
      
      # --- VEHICLE PANELS UPDATED WITH MONTHLY COST QUESTION ---
      
      # Vehicle 1
      conditionalPanel(condition = "input.num_cars != '0'", bsCollapse(id = "car1_collapse", open = "car1_panel", bsCollapsePanel(title = "Main (Most-Used) Household Vehicle", value = "car1_panel", 
                                                                                                                                  selectInput("car1_body_type", "Vehicle Body Type:", choices = c("Hatchback", "Saloon", "Estate", "SUV / 4x4", "MPV", "Coupe / Sports car", "Van", "Motorbike / Scooter")),
                                                                                                                                  selectInput("car1_fuel", "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid", "Other Hybrid")),
                                                                                                                                  selectInput("car1_age_year", "Vehicle Age (Registration Year):", choices = c("2022 or newer", "2018 - 2021", "2013 - 2017", "2008 - 2012", "2007 or older")),
                                                                                                                                  selectInput("car1_parking", "Where is this vehicle normally parked overnight?", choices = c("On a private driveway or in a garage", "On the street (designated bay)", "On the street (no designated bay)", "Private car park")),
                                                                                                                                  hr(style="border-top: 1px solid #eee;"),
                                                                                                                                  h5("Current Weekly Use & Cost of this Vehicle"),
                                                                                                                                  # --- NEW: Monthly cost question ---
                                                                                                                                  selectInput("car1_cost_rp", 
                                                                                                                                              "Please indicate your approximate monthly running cost for this vehicle (incl. fuel, insurance, tax, maintenance):",
                                                                                                                                              choices = c("Under £100", "£101 - £200", "£201 - £300", "£301 - £400", "Over £400", "Don't know / Prefer not to say")),
                                                                                                                                  
                                                                                                                                  radioButtons("car1_work_trips_rp", 
                                                                                                                                               "Please indicate roughly how many trips each week do you make with this vehicle to your place of work:", 
                                                                                                                                               choices = c("None","For a few trips","For around half","For most trips","For all trips"), 
                                                                                                                                               inline=TRUE),
                                                                                                                                  
                                                                                                                                  radioButtons("car1_leisure_trips_rp", 
                                                                                                                                               "Please indicate roughly how many trips each week do you make with this vehicle for leisure/recreational purposes:", 
                                                                                                                                               choices = c("None","For a few trips","For around half","For most trips","For all trips"),
                                                                                                                                               inline=TRUE)
      ))),
      
      # Vehicle 2
      conditionalPanel(condition = "input.num_cars == '2' || input.num_cars == '3' || input.num_cars == '4+'", bsCollapse(id = "car2_collapse", bsCollapsePanel(title = "Second Most-Used Household Vehicle", value = "car2_panel", 
                                                                                                                                                                selectInput("car2_body_type", "Vehicle Body Type:", choices = c("Hatchback", "Saloon", "Estate", "SUV / 4x4", "MPV", "Coupe / Sports car", "Van", "Motorbike / Scooter")),
                                                                                                                                                                selectInput("car2_fuel", "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid", "Other Hybrid")),
                                                                                                                                                                selectInput("car2_age_year", "Vehicle Age (Registration Year):", choices = c("2022 or newer", "2018 - 2021", "2013 - 2017", "2008 - 2012", "2007 or older")),
                                                                                                                                                                selectInput("car2_parking", "Where is this vehicle normally parked overnight?", choices = c("On a private driveway or in a garage", "On the street (designated bay)", "On the street (no designated bay)", "Private car park")),
                                                                                                                                                                hr(style="border-top: 1px solid #eee;"),
                                                                                                                                                                h5("Current Weekly Use & Cost of this Vehicle"),
                                                                                                                                                                selectInput("car2_cost_rp", 
                                                                                                                                                                            "Please indicate your approximate monthly running cost for this vehicle (incl. fuel, insurance, tax, maintenance):",
                                                                                                                                                                            choices = c("Under £100", "£101 - £200", "£201 - £300", "£301 - £400", "Over £400", "Don't know / Prefer not to say")),
                                                                                                                                                                
                                                                                                                                                                radioButtons("car2_work_trips_rp", 
                                                                                                                                                                             "Please indicate roughly how many trips each week do you make with this vehicle to your place of work:", 
                                                                                                                                                                             choices = c("None","For a few trips","For around half","For most trips","For all trips"), inline=TRUE),
                                                                                                                                                                
                                                                                                                                                                radioButtons("car2_leisure_trips_rp", 
                                                                                                                                                                             "Please indicate roughly how many trips each week do you make with this vehicle for leisure/recreational purposes:", 
                                                                                                                                                                             choices = c("None","For a few trips","For around half","For most trips","For all trips"), inline=TRUE)
      ))),
      
      # Vehicle 3
      conditionalPanel(condition = "input.num_cars == '3' || input.num_cars == '4+'", bsCollapse(id = "car3_collapse", bsCollapsePanel(title = "Third Most-Used Household Vehicle", value = "car3_panel", 
                                                                                                                                       selectInput("car3_body_type", "Vehicle Body Type:", choices = c("Hatchback", "Saloon", "Estate", "SUV / 4x4", "MPV", "Coupe / Sports car", "Van", "Motorbike / Scooter")),
                                                                                                                                       selectInput("car3_fuel", "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid", "Other Hybrid")),
                                                                                                                                       selectInput("car3_age_year", "Vehicle Age (Registration Year):", choices = c("2022 or newer", "2018 - 2021", "2013 - 2017", "2008 - 2012", "2007 or older")),
                                                                                                                                       selectInput("car3_parking", "Where is this vehicle normally parked overnight?", choices = c("On a private driveway or in a garage", "On the street (designated bay)", "On the street (no designated bay)", "Private car park")),
                                                                                                                                       hr(style="border-top: 1px solid #eee;"),
                                                                                                                                       h5("Current Weekly Use & Cost of this Vehicle"),
                                                                                                                                       selectInput("car3_cost_rp", 
                                                                                                                                                   "Please indicate your approximate monthly running cost for this vehicle (incl. fuel, insurance, tax, maintenance):",
                                                                                                                                                   choices = c("Under £100", "£101 - £200", "£201 - £300", "£301 - £400", "Over £400", "Don't know / Prefer not to say")),
                                                                                                                                       radioButtons("car3_work_trips_rp", 
                                                                                                                                                    "Please indicate roughly how many trips each week do you make with this vehicle to your place of work:", 
                                                                                                                                                    choices = c("None","For a few trips","For around half","For most trips","For all trips"), inline=TRUE),
                                                                                                                                       
                                                                                                                                       radioButtons("car3_leisure_trips_rp", 
                                                                                                                                                    "Please indicate roughly how many trips each week do you make with this vehicle for leisure/recreational purposes:", 
                                                                                                                                                    choices = c("None","For a few trips","For around half","For most trips","For all trips"), inline=TRUE)
      ))),
      
      # Vehicle 4
      conditionalPanel(condition = "input.num_cars == '4+'", bsCollapse(id = "car4_collapse", bsCollapsePanel(title = "Fourth Most-Used Household Vehicle", value = "car4_panel", 
                                                                                                              selectInput("car4_body_type", "Vehicle Body Type:", choices = c("Hatchback", "Saloon", "Estate", "SUV / 4x4", "MPV", "Coupe / Sports car", "Van", "Motorbike / Scooter")),
                                                                                                              selectInput("car4_fuel", "Main Fuel Type:", choices = c("Petrol", "Diesel", "Fully Electric", "Plug-in Hybrid", "Other Hybrid")),
                                                                                                              selectInput("car4_age_year", "Vehicle Age (Registration Year):", choices = c("2022 or newer", "2018 - 2021", "2013 - 2017", "2008 - 2012", "2007 or older")),
                                                                                                              selectInput("car4_parking", "Where is this vehicle normally parked overnight?", choices = c("On a private driveway or in a garage", "On the street (designated bay)", "On the street (no designated bay)", "Private car park")),
                                                                                                              hr(style="border-top: 1px solid #eee;"),
                                                                                                              h5("Current Weekly Use & Cost of this Vehicle"),
                                                                                                              selectInput("car4_cost_rp", 
                                                                                                                          "Please indicate your approximate monthly running cost for this vehicle (incl. fuel, insurance, tax, maintenance):",
                                                                                                                          choices = c("Under £100", "£101 - £200", "£201 - £300", "£301 - £400", "Over £400", "Don't know / Prefer not to say")),
                                                                                                              
                                                                                                              radioButtons("car4_work_trips_rp", 
                                                                                                                           "Please indicate roughly how many trips each week do you make with this vehicle to your place of work:", 
                                                                                                                           choices = c("None","For a few trips","For around half","For most trips","For all trips"), inline=TRUE),
                                                                                                              
                                                                                                              radioButtons("car4_leisure_trips_rp", 
                                                                                                                           "Please indicate roughly how many trips each week do you make with this vehicle for leisure/recreational purposes:", 
                                                                                                                           choices = c("None","For a few trips","For around half","For most trips","For all trips"), inline=TRUE)
      ))),
      
      conditionalPanel(
        condition = "input.num_cars == '0'",
        p("As your household does not have a vehicle, please answer the following about how you typically travel."),
        conditionalPanel(
          condition = "input.drivers_licence == 'Yes'",
          wellPanel(
            h5("Travel without a private car (with licence)"),
            selectInput("car0_Commute_licence", "For most of your journeys, how do you travel?", width = "400px",
                        choices = c("Public transport", "Car club/Car sharing", "Ride-hailing (e.g. Uber, taxi)", "Car passenger (friend, family, etc.)", "Cycling", "Walking", "Demand-responsive transport")),
            hr(style="border-top: 1px solid #eee;"),
            h5("Current Weekly Use & Cost"),
            # --- NEW: Monthly cost question for zero-car (licence) ---
            selectInput("car0_cost_rp", 
                        "Roughly, what is your household's total monthly spend on travel (e.g. bus passes, tickets, taxis)?", width = "400px",
                        choices = c("£0", "£1 - £30", "£31 - £60", "£61 - £100", "£101 - £150", "More than £150", "Don't know / Prefer not to say")),
            
            radioButtons("car0_work_trips_rp", 
                         "Please indicate roughly how many trips each week do you make with this vehicle to your place of work:", 
                         choices = c("None","For a few trips","For around half","For most trips","For all trips"), inline=TRUE),
            
            radioButtons("car0_leisure_trips_rp", 
                         "Please indicate roughly how many trips each week do you make with this vehicle for leisure/recreational purposes:", 
                         choices = c("None","For a few trips","For around half","For most trips","For all trips"), 
                         inline=TRUE)
          )
        ),
        conditionalPanel(
          condition = "input.drivers_licence == 'No'",
          wellPanel(
            h5("Travel without a private car (without licence)"),
            selectInput("car0_Commute_no_licence", "For most of your journeys, how do you travel?", width = "400px",
                        choices = c("Public transport", "Ride-hailing (e.g. Uber, taxi)", "Car passenger (friend, family, etc.)", "Cycling", "Walking", "Demand-responsive transport")),
            hr(style="border-top: 1px solid #eee;"),
            h5("Current Weekly Use & Cost"),
            # --- NEW: Monthly cost question for zero-car (no licence) ---
            selectInput("car0_no_licence_cost_rp", "Roughly, what is your household's total monthly spend on travel?",
                        choices = c("£0", "£1 - £30", "£31 - £60", "£61 - £100", "£101 - £150", "More than £150", "Don't know / Prefer not to say")),
            radioButtons("car0_no_licence_work_trips_rp", "Trips to work:", choices = c("None","For a few trips","For around half","For most trips","For all trips"), inline=TRUE),
            radioButtons("car0_no_licence_leisure_trips_rp", "Leisure trips:", choices = c("None","For a few trips","For around half","For most trips","For all trips"), inline=TRUE)
          )
        )
      ),
      hr(),
      
      h4("Step 3: Future Intentions"),
      selectInput("num_cars_intend", "Do you intend to acquire or dispose of a household vehicle in the next 12 months?",
                  choices = c("Dispose of one or more vehicles", "No change", "Acquire one or more vehicles"), selected = "No change", width = "400px"),
      hr(),
      actionButton("to_rp2_button", "Continue ->", class = "btn-primary")
    ),
    
    
    # =========== Choices: Version 1 ===========
    tabPanel("Choices: Version 1: Binary format", value = "sa_panel_3", uiOutput("sa_ui_placeholder3")),
    
    # =========== Choices: Version 2 ===========
    tabPanel("Choices: Version 2: Old format", value = "sa_panel_4", uiOutput("sa_ui_placeholder4")),

    # =========== Choices: Version 3 ===========
    tabPanel("Choices: Version 3: Horizontal format", value = "sa_panel_5", uiOutput("sa_ui_placeholder5")),
    
    # =========== END ===========
    # tabPanel("Choices: Version 4: Different Menu", value = "sa_panel_6", uiOutput("sa_ui_placeholder6")),
    
  )
)