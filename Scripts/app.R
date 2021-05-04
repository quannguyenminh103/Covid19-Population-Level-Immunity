source('libraries.R')
source("LoadDatabyDate.R")
source("getData.R")
source("contourMap.R")
source("dailyPlot.R")
source("immuStatePlot.R")
source("MakeMap.R")
## interface
ui <- fluidPage(
  titlePanel("Population Level Immunity"),
  fluidRow(
    sidebarLayout(sidebarPanel(
      wellPanel(tags$small(
        radioButtons("vaccinatedType", "Select Vaccination Type",
                     c("At Least One Dose", "Fully Vaccinated"), selected = "At Least One Dose", inline = T),
        radioButtons("ascertainment_bias", "Select Ascertainment Bias",
                     c(3,4,5), selected = 4, inline = T),
        tags$small(align = "left", 
                 class = "multicol",
                 checkboxGroupButtons(
                   inputId = "state",
                   label = "Choose a state",
                   choices = all_states,
                   selected = "United States",
                   individual = TRUE,
                   checkIcon = list(
                      yes = tags$i(class = "fa fa-circle", 
                      style = "color: steelblue"),
                      no = tags$i(class = "fa fa-circle-o", 
                                  style = "color: steelblue"))),
                      # Use two separate action links to allow user to select all or
                      # unselect all states quickly
                      span(
                        actionLink("selectall","Select All"),
                        "or",
                        actionLink("unselectall","Unselect All"),
                        style = "font-size: 1.7rem; font-weight: bold;"
                  )
        ),
      )),
      wellPanel(
        tags$small(paste0(
          "Note: The data includes the territories of the United States (excluding American Samoa).
          Due to the big data, it will take a few seconds to load plots for ALL states at once."
        ))),
      wellPanel(
        tags$small(
          strong("Data Sources:"), tags$br(),
            "- ", tags$a("COVID-19 case data collated by The New York Times, based on reports from state and local health agencies.", 
          href = 'https://github.com/nytimes/covid-19-data'), tags$br(),
            "- ", tags$a("Vaccination data collated by Our World in Data from United States Centers for Disease Control and Prevention.",
          href = 'https://github.com/owid/covid-19-data/tree/master/public/data/vaccinations'), tags$br(),
            "- Population data from United States Census Bureau.", tags$br(),
          strong("Codes:"), tags$br(),
          " - ",tags$a(href = 'https://github.com/quannguyenminh103/Covid19-Population-Level-Immunity', "Covid19-Population-Level-Immunity")
        ))
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Daily Immunity Level Plot", br(), plotlyOutput("plot2", width = 800, height = 700)),
                  tabPanel("Current Immunity Level Plot", br(), plotlyOutput("plot1", width = 800, height = 700)),
                  tabPanel("Map", leafletOutput("map1", width = 800, height = 700))),
      br(),
      fluidRow(column(10, wellPanel(align = 'center',
        tags$small('Population immunity is the estimated percentage of population expected
          to have (potentially, partial) immunity to Covid-19, where contours lines
          are equal to 100% x [1- (1-fraction_infected) x (1-fraction_immunized)].', #tags$br(), #tags$br(),
                   'These contours assume that immunization is independent of infection; if
          correlated then these will be over-estimates, if anti-correlated, then
          these will be under-estimates.', #tags$br(),# tags$br(),
                   'Note that the dashboard makes a homogeneity
          assumption and does not break down infection and/or immunization rates by
          age. The vaccination type includes data on individuals with at least one
          dose or those considered fully vaccinated (2 doses for mRNA vaccines).', #tags$br(), #tags$br(),
                   'The ascertainment bias is the ratio of actual cases to documented cases.', #tags$br(), #tags$br(),
                   'Note that in both cases we use cumulative infection and vaccination data
          through two weeks ago, given expected delays before the onset of
          protective immunity.'
        )))),
      fluidRow(column(10, wellPanel(tags$small(
        "Developers: ", tags$a(href = "https://www.linkedin.com/in/king-nguyen-103/", 'Quan M Nguyen'), ",", tags$a(href = 'http://sjbeckett.github.io/', 'Stephen J Beckett'), ", and ",
        tags$a(href = 'https://weitzgroup.biosci.gatech.edu/', "Joshua S Weitz."), "For more information contact Dr. Beckett (stephen.beckett@biology.gatech.edu) and
              Dr. Weitz (jsweitz@gatech.edu). This dashboard is powered and hosted by ", tags$a(href = "https://www.rstudio.com/", "RStudio."), "We acknowledge additional code contributions from ",
              tags$a(href = 'http://nickstrayer.me/', "Nick Strayer.")
      ), align = 'center')))
    )
  )
))

## back-end core
server <- function(input, output, session) {
  # Trigger updating of the checkbox groups whenever the select all or unselect
  # all buttons are pressed
  observe({
    updateCheckboxGroupButtons(session, inputId = "state", selected = all_states)
  }) %>% 
    bindEvent(input$selectall)
  
  observe({
    updateCheckboxGroupButtons(session, inputId = "state", selected = "United States")
  }) %>% 
    bindEvent(input$unselectall) 
  
  ## Current Immunity Level Plot with caching 
  output$plot1 <- renderPlotly({
    immuStatePlot(input$state, input$vaccinatedType, as.numeric(input$ascertainment_bias))
  }) %>% bindCache(input$state, input$vaccinatedType, input$ascertainment_bias)
  
  ## Daily Immunity Level Plot
  output$plot2 <- renderPlotly({
    dailyPlot(input$state, input$vaccinatedType, as.numeric(input$ascertainment_bias))
  }) %>% bindCache(input$state, input$vaccinatedType, input$ascertainment_bias)
  
  ## Map
  output$map1 <- renderLeaflet({
    MakeMap(input$vaccinatedType, input$ascertainment_bias)
  })
  
}
shinyApp(ui,server)
