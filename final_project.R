#final project

#load libraries
library(shiny)
library(tidyverse)
library(mosaic)
library(tidyverse)
library(glue)
library(lubridate)


trial_1 <- read_csv("/home/varadys/labwork_varadys/data_dump_ster/MTT_trial1.csv")

trial_2_24 <- read_csv("/home/varadys/labwork_varadys/data_dump_ster/MTT_rkl0424_round2.csv")

trial_2_4ht <- read_csv("/home/varadys/labwork_varadys/data_dump_ster/MTTassay_4ht_round2_r.csv")

trial_3 <- read_csv("/home/varadys/labwork_varadys/data_dump_ster/MTT_trial3.csv")

trial_4 <- read_csv("/home/varadys/labwork_varadys/data_dump_ster/MTT_trial4.csv")

#attempt at uploading multiple csvs
library(shiny)
library(data.table)

ui <- fluidPage(
  titlePanel("Multiple file uploads"),
  sidebarLayout(
    sidebarPanel(
      fileInput("csvs",
                label="Upload CSVs here",
                multiple = TRUE)
    ),
    mainPanel(
      textOutput("count")
    )
  )
)

server <- function(input, output) {
  mycsvs<-reactive({
    rbindlist(lapply(input$csvs$datapath, fread),
              use.names = TRUE, fill = TRUE)
  })
  output$count <- renderText(nrow(mycsvs()))
}

shinyApp(ui = ui, server = server)

# User interface trial 1
ui <- fluidPage(
  titlePanel(tags$div(a(href = "https://cran.r-project.org/web/packages/pdxTrees/index.html", "PDX Trees"), "Health based on Park")),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "trial", label = h3("Which MTT Assay Trial Do You Want to See Data From?"), 
                         choices = top_species$Park,
                         selected = top_species$Park),
      sliderInput("height_slider",
                  label = h3("Select Tree Height"),
                  # start = as_date("2015-01-01"), end = as_date("2015-12-31"),
                  min = min_height, max = max_height, value=c(min_height, max_height))
    ),
    mainPanel(plotOutput("tree_plot"), textOutput("avg_health")
    )
  )
)

# Server function
server <- function(input, output){
  
  pdx_trees_parks_reactive <- reactive({
    pdxTreesParks %>%
      filter(Tree_Height <= input$height_slider[2], Tree_Height >= input$height_slider[1]) %>%
      filter(Park %in% input$park_select)
  })
  
  
  
  
  
  condition_mode <- reactive({
    pdx_trees_parks_reactive() %>%
      select(Condition) %>%
      group_by(Condition) %>%
      summarise(n()) %>%
      top_n(1)
  })
  
  
  
  
  
  output$avg_health <- renderText({
    glue("The most common condition of trees between ", as.character(input$height_slider[1]), "ft and ", 
         as.character(input$height_slider[2]), "ft tall in the selected parks is ", condition_mode()$Condition[1], ". ", condition_mode()$`n()`[1] ," trees are in this condition")
    #How do I get the most common value in the condition column and its count?
  })
  
  
  output$tree_plot <- renderPlot({
    pdx_trees_parks_reactive() %>%
      ggplot(mapping = aes(x = Condition, fill = Park)) + 
      geom_bar(stat = "count") #+ 
    #theme(legend.position = "bottom")
  })
  
  
}

# Creates app
shinyApp(ui = ui, server = server)

#connecting to shiny.io to upload app
rsconnect::setAccountInfo(name='varadys',
                          token='02FF88D7BCFD1F981F643E15DA29E6B0',
                          secret='SpH7mizlZiwsig0s3OtVaQvEG3SVoAf4O0ETLcXx')

library(rsconnect)
rsconnect::deployApp('Home/labwork_varadys/shinnyrapp.R')