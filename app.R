#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    http://graphs.chesno.org/

library(dplyr)
library(tidyr)
library(networkD3)
library(igraph)
library(stringr)
library(readr)

library(shiny)
library(shinythemes)

source("functions_shiny.R")

load("IA_x_y3.Rda")
load("list_initiators_acts.Rda")


# Define UI for application that draws a histogram
ui <- fluidPage(tags$head(includeHTML(("google-analytics.html"))),
                titlePanel("Як нардепи Ради-9 подають разом закони"),   


                
                 # tabPanel-1 ####
                 tabPanel(title = "Прийняті закони",
                          br(),
                          sidebarLayout(
                              sidebarPanel(
                                  sliderInput("bills_number",
                                              "Кількість спільноподаних законів",
                                              min = 1,
                                              max = 20,
                                              value = 10),
                                  checkboxGroupInput("factions",
                                                     label = "Обрати фракцію",
                                                     # 
                                                     choices = unique(list_initiators_acts$factions), 
                                                     selected = unique(list_initiators_acts$factions)), #,
                                  # 
                                  checkboxGroupInput("hidden_groups",
                                                     label = "Приховані групи",
                                                     #
                                                     choices = unique(list_initiators_acts$hidden_groups),
                                                     selected = unique(list_initiators_acts$hidden_groups)),
                                  
                                  checkboxGroupInput("komitet",
                                                     label = "За членством у комітетах",
                                                     #
                                                     choices = unique(list_initiators_acts$komitet),
                                                     selected = unique(list_initiators_acts$komitet))
                                  
                                  
                              ), # sidebarPanel
                              
                              mainPanel(
                                  tabsetPanel(type = "tabs",
                                              tabPanel("За фракціями",
                                                       br(),
                                                       forceNetworkOutput("network_bills")  # Change
                                              ),
                                              
                                              tabPanel("За комітетами",
                                                       br(),
                                                       forceNetworkOutput("network_bills_komitet")  # Change
                                              ),
                                              
                                              tabPanel("Неформальні групи",
                                                       br(),
                                                       forceNetworkOutput("network_bills_hidden_groups"))
                                  )
                              ) # mainPanel
                          ) # sidebarLayout
                 ), # tabPanel
                 tabPanel(title = "Методологія"),
                 
) # navbarPage 


# Define server logic required to draw a histogram
server <- function(input, output) {

    
    # output-1 ####
    output$network_bills <- renderForceNetwork({
        
        mps_force_network(
            filter(IA_x_y3, n >= input$bills_number),
            # Зв'язки
            
            filter(
                list_initiators_acts,
                list_initiators_acts$factions %in% input$factions & 
                    list_initiators_acts$komitet %in% input$komitet & 
                    list_initiators_acts$hidden_groups %in% input$hidden_groups
                
            )
        ) # Nodes
    })
    # output-2 ####
    output$network_bills_hidden_groups <- renderForceNetwork({
        
        mps_force_network_hidden_groups(
            filter(IA_x_y3, n >= input$bills_number),
            # Зв'язки
            
            filter(
                list_initiators_acts,
                list_initiators_acts$factions %in% input$factions & 
                    list_initiators_acts$komitet %in% input$komitet & 
                    list_initiators_acts$hidden_groups %in% input$hidden_groups
                
            )
        ) # Nodes
    })
    # output-3 Komitet ####
    output$network_bills_komitet <- renderForceNetwork({
        
        mps_force_network_komitet(
            filter(IA_x_y3, n >= input$bills_number),
            # Зв'язки
            
            filter(
                list_initiators_acts,
                list_initiators_acts$factions %in% input$factions & 
                    list_initiators_acts$komitet %in% input$komitet & 
                    list_initiators_acts$hidden_groups %in% input$hidden_groups
                
            )
        ) # Nodes
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
