#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    http://graphs.chesno.org/


library(tidyverse)

library(stringr)

library(igraph)
library(networkD3)
library(shiny)
library(shinythemes)

load("IA_x_y3_2021-06-22.Rda")
load("list_initiators_acts_2021-06-23.Rda")
source("functions_shiny.R")


# ---- Ui ####
# Define UI for application that draws a histogram
ui <- fluidPage(tags$head(includeHTML(("google-analytics.html"))),
                
                # Мало б побороти greying out
                tags$style(type="text/css",
                           ".recalculating {opacity: 1.0;}"
                ),
                
                
                titlePanel("Як нардепи Ради-9 подають разом закони"),   

                 # tabPanel-1 ####
                 tabPanel(title = "Прийняті закони",
                          br(),
                          sidebarLayout(
                              sidebarPanel(
                                  sliderInput("bills_number",
                                              "Кількість спільноподаних законів",
                                              min = 1,
                                              max = 21,
                                              value = 15),
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
                                  
                                  checkboxGroupInput("department_k", 
                                                     label = "За членством у комітетах",
                                                     #
                                                     choices = unique(list_initiators_acts$department_k),
                                                     selected = unique(list_initiators_acts$department_k))
                                  
                                  
                              ), # sidebarPanel
                              
                              # Tabs menu ####
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
                                                       forceNetworkOutput("network_bills_hidden_groups")
                                                       ),
                                              
                                              tabPanel("Методологія",
                                                       br(),
                                                       p("Неформальні групи ми взяли із серії публікацій Української Правди"),
                                                       
                                                       a(href = "https://www.pravda.com.ua/articles/2021/02/18/7283826/",
                                                         "Хмільний кардинал Ілля Павлюк: дружба із Зеленським і вплив на 'Слугу народу'",
                                                         target = "_blank"),
                                                       br(),
                                                       br(),
                                                       a(href = "https://www.pravda.com.ua/articles/2021/03/4/7285446/",
                                                         "Бе!команда. Скільки депутатів Коломойського залишилось у 'Слузі народу'",
                                                         target = "_blank")
                                              )
                                              
                                  ) # tabsetPanel
                              ) # mainPanel
                          ) # sidebarLayout
                 ) # tabPanel
) # navbarPage 


# Define server logic required to draw a histogram
server <- function(input, output) {

    
    # graph f-1 ####
    output$network_bills <- renderForceNetwork({
        
        mps_force_network(
            filter(IA_x_y3, n >= input$bills_number),
            # Зв'язки
            
            filter(
                list_initiators_acts,
                list_initiators_acts$factions %in% input$factions & 
                    list_initiators_acts$department_k %in% input$department_k & 
                    list_initiators_acts$hidden_groups %in% input$hidden_groups
                
            )
        ) # Nodes
    })
    
    # graph f-2 hidden ####
    output$network_bills_hidden_groups <- renderForceNetwork({
        
        mps_force_network_hidden_groups(
            filter(IA_x_y3, n >= input$bills_number),
            # Зв'язки
            
            filter(
                list_initiators_acts,
                list_initiators_acts$factions %in% input$factions & 
                    list_initiators_acts$department_k %in% input$department_k & 
                    list_initiators_acts$hidden_groups %in% input$hidden_groups
                
            )
        ) # Nodes
    })
    # graph f-3 department_k ####
    output$network_bills_komitet <- renderForceNetwork({
        
        mps_force_network_komitet(
            filter(IA_x_y3, n >= input$bills_number),
            # Зв'язки
            
            filter(
                list_initiators_acts,
                list_initiators_acts$factions %in% input$factions & 
                    list_initiators_acts$department_k %in% input$department_k & 
                    list_initiators_acts$hidden_groups %in% input$hidden_groups
                
            )
        ) # Nodes
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
