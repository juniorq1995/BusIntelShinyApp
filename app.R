library(shiny)
#install.packages("shinydashboard")
library(shinydashboard)
#devtools::install_github("dreamRs/shinyWidgets")
library(shinyWidgets)
#source('testScript.R') #move to a global.R file where all files can access the objects created by source call
#install.packages("highcharter")
library(highcharter)
#install.packages("devtools")
library(devtools)
#install.packages('DT')
library(DT)
#install.packages("plotly")
library(plotly)
#source("productionRequestDataParsePlot.R")

ui <- dashboardPage(
    dashboardHeader(title = "Dynamic sidebar"),
    dashboardSidebar(
        sidebarMenuOutput("menu")
    ),
    #Error is thrown in body
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "dashboard",
                fluidRow(
                    column(
                        width=9,
                        fluidRow(
                            valueBoxOutput("noRequests"),
                            
                            # Dynamic valueBoxes
                            valueBoxOutput("avgRequests"),
                            
                            valueBoxOutput("popularRequestType")
                        ),
                        fluidRow(
                            tabBox( id="tabChart",width=12,
                              tabPanel(
                                "Years",
                                #Example from insurance app
                                #highchartOutput("ay_plot")
                                plotlyOutput("dashboardPlotYears")
                              ),
                              tabPanel(
                                "Request Types",
                                plotlyOutput("dashboardPlotRequests")
                              )
                            )
                        )
                    ),
                    column(
                        width = 3,
                        box(
                            title="Data Filters",
                            width=12,
                            #radioButtons("dataType",label=NULL,choices=c("Dates","Requests"),inline=TRUE),
                            "Choose the types of requests to display",
                            pickerInput("Requests", choices = listOfTypes,options = list('actions-box' = TRUE),selected = listOfTypes,multiple=TRUE),
                            "Choose the years to display",
                            pickerInput("Years", choices = c("2013","2014","2015","2016","2017","2018"),options = list('actions-box' = TRUE),selected = c("2013","2014","2015","2016","2017","2018"),multiple=TRUE)
                    )
                    )
                )
            ),
            tabItem(
                tabName = "dataTable",
                fluidRow(
                    actionButton("show", "Access a new data table"),
                    box(width = 12, DT::dataTableOutput('tbl_b'))
                )
            )
        )
    )     
)

server <- function(input, output) {
    output$menu <- renderMenu({
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Data Table", tabName = "dataTable", icon = icon("th"))
        )
    })
    
    output$noRequests <- renderValueBox({
        valueBox(
            toString(format(length(typesRequests),big.mark=",",scientific=FALSE)), "Number of Requests", icon = icon("arrow-right"),
            width = 2,
            color = "blue"
        )
    })
    
    output$avgRequests <- renderValueBox({
        valueBox(
          toString(format(avgRequestsPerMonth,big.mark=",",scientific=FALSE)), "Avg. Requests/Month", icon = icon("calendar"),
            width = 2,
            color = "green"
        )
    })
    
    output$popularRequestType <- renderValueBox({
        valueBox(
          toString(data.frame(table(typesRequests))$typesRequests[which(summaryRequests$Freq == max(summaryRequests$Freq))]), "Most popular method of Request", icon = icon("sunglasses", lib = "glyphicon"),
            width = 2,
            color = "orange"
        )
    })
    
    
    #####################################################################################################
    # reactiveValues object for storing current data set.
    vals <- reactiveValues(data = NULL)
    #dataset <- NULL
    # Return the UI for a modal dialog with data selection input. If 'failed' is
    # TRUE, then display a message that the previous value was invalid.
    dataModal <- function(failed = FALSE) {
        modalDialog(
            title = "Connect to a Database",
            selectInput("driver","Select Driver",c("SQL Server" = "SQL Server","Other Driver" = "other driver")),
            textInput("server", "Choose server",
                      placeholder = 'Try "mtcars" or "abc"'
            ),
            textInput("port", "Choose a port",
                      placeholder = 'Try "1433"'
            ),
            textInput("database", "Choose database",
                      placeholder = 'Try "mtcars" or "abc"'
            ),
            textInput("table", "Choose a table",
                        placeholder = 'Try "flex22213"'
            ),
            textInput("UID", "Enter User ID",
                      placeholder = 'Try "mtcars" or "abc"'
            ),
            textInput("password", "Enter Password",
                      placeholder = 'Try "mtcars" or "abc"'
            ),
            if (failed)
                div(tags$b("Invalid name of data object", style = "color: red;")),

            footer = tagList(
                modalButton("Cancel"),
                actionButton("ok", "OK")
            )
        )
    }
    
    # Show modal when tab is clicked. Right now it is only triggered when button is clicked
    observeEvent(input$show, {
        showModal(dataModal())
    })

    # When OK button is pressed, attempt to load the data set. If successful,
    # remove the modal. If not show another modal, but this time with a failure
    # message.
    observeEvent(input$ok, {
        #Check that data object exists and is data frame.
        credentials <- paste('driver={',input$driver,'};server=',input$server,';database=',input$database,';uid=',input$UID,';pwd=',input$password,sep = "")
        library(odbc)
        conn <- odbcDriverConnect(credentials)
        dataset <- sqlFetch(conn,input$table,colnames = FALSE,rownames = FALSE)
        source("productionRequestDataParsePlot.R")
        #exists(deparse(substitute(dataset)))    
        if (!is.null(dataset) && is.data.frame(dataset)){
            vals$data <- dataset
            
            removeModal()
        } else {
            showModal(dataModal(failed = TRUE))
        }
    })

    #Display information about selected data
    # output$dataInfo <- renderPrint({
    #     if (is.null(vals$data))
    #         "No data selected"
    #     else
    #         #summary(vals$data)
    #         output$tbl_b = DT::renderDataTable(vals$data)
    # })
    output$tbl_b = DT::renderDataTable({
        if(is.null(vals$data))
            iris
        else
            datatable(
                vals$data,
                extensions = 'Buttons', options = list(
                    dom = 'Bfrtip',
                    buttons = 
                        list('copy', 'print', list(
                            extend = 'collection',
                            buttons = c('csv', 'excel', 'pdf'),
                            text = 'Download'
                        ))
                )
            )
        })
    
    output$dashboardPlotYears<-renderPlotly({
      #Outputs the date plot found in productionRequestDataParse.R
      ggplotly(d)
    })
    output$dashboardPlotRequests<-renderPlotly({
      #Outputs the date plot found in productionRequestDataParse.R
      ggplotly(dt2)
    })
}

shinyApp(ui, server)