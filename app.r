# Load packages
library(shiny)
library(tidyverse)
library(tidycensus)
library(leaflet)
library(sf)
library(DT)
library(shinythemes)

# Create county dataframe
counties <- 
  get_acs(geography = "county",
          variables = "B25064_001",
          key = "abac0e1ca2aa3d3ebb31d6d2fcdbaf52d3e25f7d")
county_list <- 
  gsub(pattern = ' County, ', x = counties$NAME, replacement = ";")
county_list[68:96] <- gsub(pattern = ', ', x = county_list[68:96], replacement = ";")
county_list[320] <- gsub(pattern = ', ', x = county_list[320], replacement = ";")
county_list[1114:1177] <- gsub(pattern = ', ', x = county_list[1114:1177], replacement = ";")
county_list[1217] <- gsub(pattern = ', ', x = county_list[1217], replacement = ";")
county_list[1598] <- gsub(pattern = ', ', x = county_list[1598], replacement = ";")
county_list[1764] <- gsub(pattern = ', ', x = county_list[1764], replacement = ";")
county_list[2916:2953] <- gsub(pattern = ', ', x = county_list[2916:2953], replacement = ";")
county_list[3143:3220] <- gsub(pattern = ', ', x = county_list[3143:3220], replacement = ";")
county_list <- as.data.frame(county_list) %>%
  separate(col = county_list, into = c("county", "state"), sep = ";")


# User interface
ui <- fluidPage(theme = shinytheme("flatly"),
                titlePanel("Rent Distributions Across All Counties in the United States"),
  sidebarPanel(
  selectizeInput(inputId = "state",
                 choices = state.name,
                 label = "Select state",
                 selected = "Washington"),
  uiOutput("secondSelection"),
  radioButtons(inputId = "level",
               label = "Level of precision",
               choices = c("county", "tract", "block group"),
               selected = "tract")
  ),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Rent Map",
                         plotOutput(outputId = "graph"),
                         htmlOutput(outputId = "text")),
                tabPanel("Rent Distribution",
                         plotOutput(outputId = "histo"),
                         htmlOutput(outputId = "densitytext")),
                tabPanel("Information",
                         tags$p("This app was created by Grayson White as a project for Math 241: Data 
                                Science at Reed College in spring 2020. The goal of this app is to
                                allow the public to explore the distribution of rent in any
                                county in the United States and better understand the rent in counties relevent to
                                their interests."),
                         tags$p(
                                "The data were obtained from the US Census Bureau with the",
                                tags$code("tidycensus"), "package in R.",
                                "For more information on the data used in
                                this app, you can visit the",
                                tags$a("tidycensus webpage.", href="https://walkerke.github.io/tidycensus/", target="_blank"))
                         )
                )
    ))

# Server function
server <- function(input, output, session){
  updateSelectizeInput(session = session, inputId = 'county')
  
  county_final <- reactive({
    county_list %>%
      filter(state == input$state)
  })
  
  output$secondSelection <- renderUI({
    selectizeInput(inputId = "county",
                   choices = county_final()$county,
                   label = "Select county",
                   selected = "San Juan")
  })
  
  tract <- reactive({
    get_acs(
      geography = input$level,
      variables = "B25064_001",
      state = input$state,
      county = input$county,
      geometry = TRUE,
      key = "abac0e1ca2aa3d3ebb31d6d2fcdbaf52d3e25f7d"
    )
  })

  output$graph <- renderPlot({
    ggplot(data = tract(),
           mapping = aes(geometry = geometry,
                         fill = estimate)) + 
      geom_sf() +
      coord_sf() +
      theme_void() +
      scale_fill_viridis_c() +
      labs(fill = "Median Gross Rent")
  })
  
  summary_stats <- reactive({ 
    get_acs(
      geography = "county",
      variables = "B25064_001",
      state = input$state,
      county = input$county,
      geometry = TRUE,
      key = "abac0e1ca2aa3d3ebb31d6d2fcdbaf52d3e25f7d"
    )
  })
  output$text <- renderUI({
    HTML(paste("The median rent in", input$county, "County is", summary_stats()$estimate, "dollars."))
  })
  
  output$histo <- renderPlot({
    ggplot(data = tract(),
           mapping = aes(x = estimate)) +
      geom_density(fill = "grey") +
      theme_minimal()
  })
  
  output$densitytext <- renderUI({
    HTML(paste("This is the distribution of rent in", input$county,
    "County, where each observation is the median rent of a", input$level, "in", input$county,
    "County."))
  })
}

# Creates app
shinyApp(ui = ui, server = server)
