# Load libraries
library(shiny)
library(tidyverse)
library(mosaic)
library(tidyverse)
library(glue)
library(lubridate)
library(scales)
library(plotly)

mark_controls <- function(path) {
  #loading csv for trial
  #changing dmso control group to be distinguished as such in the SERM column of df instead of having its corresponding assay's molecule treatment listed as SERM. 
  read_csv(path) %>%
    mutate(SERM = ifelse(dmso_control == "yes", paste(SERM, "DMSO control"), SERM)) %>%
    mutate(SERM = ifelse(cells_only == "yes", paste(SERM, "cells only control"), SERM)) %>%
    mutate(SERM = ifelse(medium_only == "yes", paste(SERM, "medium only control"), SERM))
}

#using controls function and releveling concentrations to be ordered sequentially.
trial_1 <- mark_controls("/home/varadys/labwork_varadys/data_dump_ster/MTT_trial1.csv")
trial_1 <- trial_1 %>%
  mutate(concentration_nM = as_factor(concentration_nM))%>%
  mutate(concentration_nM = fct_relevel(concentration_nM, c('0', '3.125', '6.25', '12.5', '25', '50', '100', 'low','hi')))

trial_2 <- mark_controls("/home/varadys/labwork_varadys/data_dump_ster/MTT_rkl0424_round2.csv") %>%
  filter(!is.na(plate_number)) %>%
  mutate(concentration_nM = as_factor(concentration_nM))%>%
  mutate(concentration_nM = fct_relevel(concentration_nM, c('0', '3.125', '12.25', '50', '100', 'low', 'hi')))

#remove spurious NA columns created by excel
trial_2 <- trial_2[,colSums(is.na(trial_2))<nrow(trial_2)]

#trial 3 
trial_3 <- mark_controls("/home/varadys/labwork_varadys/data_dump_ster/MTTassay_4ht_round2_r.csv")
trial_3 <- trial_3 %>%
  mutate(concentration_nM = as_factor(concentration_nM))%>%
  mutate(concentration_nM = fct_relevel(concentration_nM, c('0', '3.125', '12.25', '50', '100', 'low', 'hi')))

trial_4 <- mark_controls("/home/varadys/labwork_varadys/data_dump_ster/MTT_trial3.csv")
trial_4 <- trial_4 %>%
  mutate(concentration = as_factor(concentration))%>%
  mutate(concentration = fct_relevel(concentration, c('0', '0.02', '0.1', '0.5', '1', '5', '10', '20', 'low', 'med', 'hi')))

trial_5 <- mark_controls("/home/varadys/labwork_varadys/data_dump_ster/MTT_trial4.csv")
trial_5 <- trial_5 %>%
  mutate(concentration = as_factor(concentration))%>%
  mutate(concentration = fct_relevel(concentration, c('0', '0.02', '0.1', '0.5', '1', '5', '10', '20', 'low', 'med', 'hi')))

# User interface
ui  <- navbarPage("Sophia's Thesis Data", fluid = TRUE, 
                  tabPanel("Background",
                           fluidPage( includeMarkdown(("/home/varadys/labwork_varadys/ShinyApps/project_motivation.md")))
                  ),
                  tabPanel("Trial 1",
                           fluidPage(title = "MTT Assay Trial 1",
                                     titlePanel("MTT Assay Trial 1"),
                                     sidebarLayout(
                                       tabsetPanel(
                                         tabPanel("Plate Replicate #",
                                                  sidebarPanel(
                                                    checkboxGroupInput(
                                                      inputId = "plate_select",
                                                      label = h3("Which Replicate Do You Want to See Data From?"), 
                                                      choices = unique(trial_1$plate_number),
                                                      selected = unique(trial_1$plate_number)
                                                    )
                                                  )),
                                         tabPanel("Molecule", 
                                                  sidebarPanel(
                                                    checkboxGroupInput(
                                                      inputId = "molecule_select",
                                                      label = h3("Which Molecules Tested Do You Want To See?"), 
                                                      choices = unique(trial_1$SERM),
                                                      selected = unique(trial_1$SERM)
                                                    )
                                                  )
                                         )),
                                       mainPanel(plotOutput("molecule_plot"), textOutput("molecule_selection"))
                                     ))),
                  
                  tabPanel("Trial 2",
                           fluidPage(title = "MTT Assay Trial 2",
                                     titlePanel("MTT Assay Trial 2"),
                                     sidebarLayout(
                                       tabsetPanel(
                                         tabPanel("Plate Replicate #",
                                                  sidebarPanel(
                                                    checkboxGroupInput(
                                                      inputId = "plate_select_2",
                                                      label = h3("Which Replicate Do You Want to See Data From?"),
                                                      choices = unique(trial_2$plate_number),
                                                      selected = unique(trial_2$plate_number)
                                                    )
                                                  )),
                                         tabPanel("Molecule",
                                                  sidebarPanel(
                                                    checkboxGroupInput(
                                                      inputId = "molecule_select_2",
                                                      label = h3("Which Molecules Tested Do You Want To See?"),
                                                      choices = unique(trial_2$SERM),
                                                      selected = unique(trial_2$SERM)
                                                    )
                                                  )
                                         )),
                                       mainPanel(plotOutput("molecule_plot_2"), textOutput("molecule_selection_2"))
                                     ))),
                  #trial 3
                  tabPanel("Trial 3",
                           fluidPage(title = "MTT Assay Trial 3",
                                     titlePanel("MTT Assay Trial 3"),
                                     sidebarLayout(
                                       tabsetPanel(
                                         tabPanel("Plate Replicate #",
                                                  sidebarPanel(
                                                    checkboxGroupInput(
                                                      inputId = "plate_select_3",
                                                      label = h3("Which Replicate Do You Want to See Data From?"),
                                                      choices = unique(trial_3$plate_number),
                                                      selected = unique(trial_3$plate_number)
                                                    )
                                                  )),
                                         tabPanel("Molecule",
                                                  sidebarPanel(
                                                    checkboxGroupInput(
                                                      inputId = "molecule_select_3",
                                                      label = h3("Which Molecules Tested Do You Want To See?"),
                                                      choices = unique(trial_3$SERM),
                                                      selected = unique(trial_3$SERM)
                                                    )
                                                  )
                                         )),
                                       mainPanel(plotOutput("molecule_plot_3"), textOutput("molecule_selection_3"))
                                     ))),
                  
                  tabPanel("Trial 4",
                           fluidPage(title = "MTT Assay Trial 4",
                                     titlePanel("MTT Assay Trial 4"),
                                     sidebarLayout(
                                       tabsetPanel(
                                         tabPanel("Plate Replicate #",
                                                  sidebarPanel(
                                                    checkboxGroupInput(
                                                      inputId = "plate_select_4",
                                                      label = h3("Which Replicate Do You Want to See Data From?"),
                                                      choices = unique(trial_4$plate_number),
                                                      selected = unique(trial_4$plate_number)
                                                    )
                                                  )),
                                         tabPanel("Molecule",
                                                  sidebarPanel(
                                                    checkboxGroupInput(
                                                      inputId = "molecule_select_4",
                                                      label = h3("Which Molecules Tested Do You Want To See?"),
                                                      choices = unique(trial_4$SERM),
                                                      selected = unique(trial_4$SERM)
                                                    )
                                                  )
                                         )),
                                       mainPanel(plotOutput("molecule_plot_4"), textOutput("molecule_selection_4"))
                                     ))),
                  
                  tabPanel("Trial 5",
                           fluidPage(title = "MTT Assay Trial 5",
                                     titlePanel("MTT Assay Trial 5"),
                                     sidebarLayout(
                                       tabsetPanel(
                                         tabPanel("Plate Replicate #",
                                                  sidebarPanel(
                                                    checkboxGroupInput(
                                                      inputId = "plate_select_5",
                                                      label = h3("Which Replicate Do You Want to See Data From?"),
                                                      choices = unique(trial_5$plate_number),
                                                      selected = unique(trial_5$plate_number)
                                                    )
                                                  )),
                                         tabPanel("Molecule",
                                                  sidebarPanel(
                                                    checkboxGroupInput(
                                                      inputId = "molecule_select_5",
                                                      label = h3("Which Molecules Tested Do You Want To See?"),
                                                      choices = unique(trial_5$SERM),
                                                      selected = unique(trial_5$SERM)
                                                    )
                                                  )
                                         )),
                                       mainPanel(plotOutput("molecule_plot_5"), textOutput("molecule_selection_5"))
                                     ))),
                  tabPanel("Explorer",
                           fluidPage(
                             titlePanel("A Plotly Based Data Explorer"),
                             div("Select a trial, then select the columns you wish to plot against each other"),
                             br(),
                             sidebarPanel(
                               selectInput('trial_select', 'Select Trial', c('Trial 1', 'Trial 2', 'Trial 3', 'Trial 4', 'Trial 5')),
                               uiOutput("explorerUIx"),
                               uiOutput("explorerUIy")
                             ),
                             mainPanel(
                               plotlyOutput('explorer')
                             )
                           )
                  ),
                  tabPanel("Takeaways",
                           titlePanel("Key Takeaways from this Project"),
                           div("The data collected for this thesis did not follow expected trends.However, this is a useful tool to look at various control groups and assays in conjunction. The ability to manipulate what groups you view, allows the user to explore the data and better hypothesize what could be causing the strange results. It is a useful tool for those who have read the thesis and are familiar with the data. The format however, could be applied to other data in the future, to selectively control what data the user wants to view. Throughout my thesis, R proved to be a valuable tool to analyze my data. Here, I was able to use it to visualize the data in both a more formally presented manner and an exploratory manner. Both of which are useful.")
                  )
)




# Server function 
server <- function(input, output){
  
  #trial 1 reactive inputs and outputs with ggplot
  trial1_reactive <- reactive({
    trial_1 %>%
      filter(plate_number %in% input$plate_select) %>%
      filter(SERM %in% input$molecule_select)
  }) 
  
  
  output$molecule_plot <- renderPlot({
    trial1_reactive() %>%
      ggplot(mapping = aes(x = concentration_nM, y = absorbance_550, color = SERM)) + 
      geom_point() +
      labs(title = "Molecule Concentration vs. Absorbance in MTT Assay Trial 1", x= "Molecule Concentration (nM)", y="Absorbance (550nm)")
  })
  
  #reactive text for trial 1 tab.
  max_row1 <- reactive({
    max_row <- trial_1[which.max(trial_1$absorbance_550),]
  })
  min_row1 <- reactive({
    max_row <- trial_1[which.min(trial_1$absorbance_550),]
  })
  output$molecule_selection <- renderText({
    glue("The maximum absorption occurs at a concentration of ",
         as.character(max_row1()$concentration_nM), " (nM) ",
         max_row1()$SERM, " with an absorbance at 550nm of ",
         max_row1()$absorbance_550,
         ". The minimum absorption occurs at a concentration of",
         as.character(min_row1()$concentration_nM), " (nM) ",
         min_row1()$SERM, " with an absorbance at 550nm of ",
         min_row1()$absorbance_550)
  })
  
  #trial 2 reactive inputs and outputs with ggplot 
  trial2_reactive <- reactive({
    trial_2 %>%
      filter(plate_number %in% input$plate_select_2) %>%
      filter(SERM %in% input$molecule_select_2)
  }) 
  
  
  output$molecule_plot_2 <- renderPlot({
    trial2_reactive() %>%
      ggplot(mapping = aes(x = concentration_nM, y = absorbance_550, color = SERM)) + 
      geom_point() +
      labs(title = "Molecule Concentration vs. Absorbance in MTT Assay Trial 1", x= "Molecule Concentration (nM)", y="Absorbance (550nm)") 
  })
  
  #reactive text for trial 2 tab. 
  max_row2 <- reactive({
    max_row <- trial_2[which.max(trial_2$absorbance_550),]
  })
  min_row2 <- reactive({
    max_row <- trial_2[which.min(trial_2$absorbance_550),]
  })
  output$molecule_selection2 <- renderText({
    glue("The maximum absorption occurs at a concentration of ",
         as.character(max_row2()$concentration_nM), " (nM) ",
         max_row2()$SERM, " with an absorbance at 550nm of ",
         max_row2()$absorbance_550,
         ". The minimum absorption occurs at a concentration of",
         as.character(min_row2()$concentration_nM), " (nM) ",
         min_row2()$SERM, " with an absorbance at 550nm of ",
         min_row2()$absorbance_550)
  })
  
  #trial 3 reactive inputs and outputs with ggplot
  trial3_reactive <- reactive({
    trial_3 %>%
      filter(plate_number %in% input$plate_select_3) %>%
      filter(SERM %in% input$molecule_select_3)
  }) 
  
  
  output$molecule_plot_3 <- renderPlot({
    trial3_reactive() %>%
      ggplot(mapping = aes(x = concentration_nM, y = absorbance_550, color = SERM)) + 
      geom_point() +
      labs(title = "Molecule Concentration vs. Absorbance in MTT Assay Trial 1", x= "Molecule Concentration (nM)", y="Absorbance (550nm)")
  })
  
  #reactive text for trial 3 tab. 
  max_row3 <- reactive({
    max_row <- trial_3[which.max(trial_1$absorbance_550),]
  })
  min_row1 <- reactive({
    max_row <- trial_3[which.min(trial_1$absorbance_550),]
  })
  output$molecule_selection <- renderText({
    glue("The maximum absorption occurs at a concentration of ",
         as.character(max_row1()$concentration_nM), " (nM) ",
         max_row3()$SERM, " with an absorbance at 550nm of ",
         max_row3()$absorbance_550,
         ". The minimum absorption occurs at a concentration of",
         as.character(min_row1()$concentration_nM), " (nM) ",
         min_row3()$SERM, " with an absorbance at 550nm of ",
         min_row3()$absorbance_550)
  })
  
  #trial 4 reactive inputs and outputs with ggplot
  trial4_reactive <- reactive({
    trial_4 %>%
      filter(plate_number %in% input$plate_select_4) %>%
      filter(SERM %in% input$molecule_select_4)
  }) 
  
  
  output$molecule_plot_4 <- renderPlot({
    trial4_reactive() %>%
      ggplot(mapping = aes(x = concentration, y = normalized, color = SERM)) + 
      geom_point() +
      labs(title = "Molecule Concentration vs. Absorbance in MTT Assay Trial 1", x= "Molecule Concentration (µM)", y="Absorbance (550nm)")
  })
  
  #reactive text for trial 4 tab.
  max_row4 <- reactive({
    max_row <- trial_4[which.max(trial_1$absorbance_550),]
  })
  min_row4 <- reactive({
    max_row <- trial_4[which.min(trial_1$absorbance_550),]
  })
  output$molecule_selection <- renderText({
    glue("The maximum absorption occurs at a concentration of ",
         as.character(max_row4()$concentration_nM), " (nM) ",
         max_row4()$SERM, " with an absorbance at 550nm of ",
         max_row4()$absorbance_550,
         ". The minimum absorption occurs at a concentration of",
         as.character(min_row4()$concentration_nM), " (nM) ",
         min_row4()$SERM, " with an absorbance at 550nm of ",
         min_row4()$absorbance_550)
  })
  
  #trial 5 reactive inputs and outputs with ggplot
  trial5_reactive <- reactive({
    trial_5 %>%
      filter(plate_number %in% input$plate_select_5) %>%
      filter(SERM %in% input$molecule_select_5)
  }) 
  
  
  output$molecule_plot_5 <- renderPlot({
    trial5_reactive() %>%
      ggplot(mapping = aes(x = concentration, y = normalized, color = SERM)) + 
      geom_point() +
      labs(title = "Molecule Concentration vs. Absorbance in MTT Assay Trial 1", x= "Molecule Concentration (µM)", y="Absorbance (550nm)")
  })
  
  #reactive text for trial 5 tab.
  max_row5 <- reactive({
    max_row <- trial_5[which.max(trial_1$absorbance_550),]
  })
  min_row5 <- reactive({
    max_row <- trial_5[which.min(trial_1$absorbance_550),]
  })
  output$molecule_selection <- renderText({
    glue("The maximum absorption occurs at a concentration of ",
         as.character(max_row5()$concentration_nM), " (nM) ",
         max_row5()$SERM, " with an absorbance at 550nm of ",
         max_row5()$absorbance_550,
         ". The minimum absorption occurs at a concentration of",
         as.character(min_row1()$concentration_nM), " (nM) ",
         min_row5()$SERM, " with an absorbance at 550nm of ",
         min_row5()$absorbance_550)
  })
  
  #explore page. making dynamic tab.
  make_selectors <- function(df, var_name) {
    selectInput(glue(var_name, 'col'), glue(var_name, ' Variable'), names(df), selected = names(df)[[2]])
  }
  output$explorerUIx <- renderUI({
    if (input$trial_select == "Trial 1") {
      return(make_selectors(trial_1, 'X'))
    }
    if (input$trial_select == "Trial 2") {
      return(make_selectors(trial_2, 'X'))
    }
    if (input$trial_select == "Trial 3") {
      return(make_selectors(trial_3, 'X'))
    }
    if (input$trial_select == "Trial 4") {
      return(make_selectors(trial_4, 'X'))
    }
    if (input$trial_select == "Trial 5") {
      return(make_selectors(trial_5, 'X'))
    }
  })
  output$explorerUIy <- renderUI({
    if (input$trial_select == "Trial 1") {
      return(make_selectors(trial_1, 'Y'))
    }
    if (input$trial_select == "Trial 2") {
      return(make_selectors(trial_2, 'Y'))
    }
    if (input$trial_select == "Trial 3") {
      return(make_selectors(trial_3, 'Y'))
    }
    if (input$trial_select == "Trial 4") {
      return(make_selectors(trial_4, 'Y'))
    }
    if (input$trial_select == "Trial 5") {
      return(make_selectors(trial_5, 'Y'))
      
    }
  })
  
  xlab <- reactive({
    list(
      title = input$Xcol
    )
  })
  ylab <- reactive({
    list(
      title = input$Ycol
    )
  })
  output$explorer <- renderPlotly(
    if (input$trial_select == "Trial 1") {
      explorer <- plot_ly( trial_1,
                           x = ~get(input$Xcol),
                           y = ~get(input$Ycol), 
                           type = 'scatter',
                           mode = 'markers') %>%
        layout(xaxis = xlab(), yaxis = ylab())
    } else if (input$trial_select == "Trial 2") {
      explorer <- plot_ly( trial_2,
                           x = ~get(input$Xcol),
                           y = ~get(input$Ycol), 
                           type = 'scatter',
                           mode = 'markers') %>%
        layout(xaxis = xlab(), yaxis = ylab())
    } else if (input$trial_select == "Trial 3") {
      explorer <- plot_ly( trial_3,
                           x = ~get(input$Xcol),
                           y = ~get(input$Ycol), 
                           type = 'scatter',
                           mode = 'markers') %>%
        layout(xaxis = xlab(), yaxis = ylab())      
    } else if (input$trial_select == "Trial 4") {
      explorer <- plot_ly( trial_4,
                           x = ~get(input$Xcol),
                           y = ~get(input$Ycol), 
                           type = 'scatter',
                           mode = 'markers') %>%
        layout(xaxis = xlab(), yaxis = ylab())      
    } else if (input$trial_select == "Trial 5") {
      explorer <- plot_ly( trial_5,
                           x = ~get(input$Xcol),
                           y = ~get(input$Ycol), 
                           type = 'scatter',
                           mode = 'markers') %>%
        layout(xaxis = xlab(), yaxis = ylab())      
    } else {
      explorer <- plot_ly( trial_2,
                           x = concentration_nM,
                           y = absorbance_550, 
                           type = 'scatter',
                           mode = 'markers') %>%
        layout(xaxis = xlab(), yaxis = ylab())
    }
    
  )
}




# Creates app
shinyApp(ui = ui, server = server)

#connecting to shiny.io to upload app
#rsconnect::setAccountInfo(name='varadys',
#                          token='02FF88D7BCFD1F981F643E15DA29E6B0',
#                          secret='SpH7mizlZiwsig0s3OtVaQvEG3SVoAf4O0ETLcXx')

#library(rsconnect)
#rsconnect::deployApp("/home/varadys/labwork_varadys/shinyrapp.R")

#getwd()
