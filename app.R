library(shiny)
#install.packages("shinydashboard")
library(shinydashboard)
#devtools::install_github("dreamRs/shinyWidgets")
library(shinyWidgets)
#source('testScript.R') #move to a global.R file where all files can access the objects created by source call

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
                            box(
                                width = 12,
                                #Example from insurance app
                                #highchartOutput("ay_plot")
                                plotOutput("dashboardPlot")
                            )
                        )
                    ),
                    column(
                        width = 3,
                        box(
                            title="Data Filters",
                            width=12,
                            radioButtons("dataType",label=NULL,choices=c("Dates","Requests"),inline=TRUE),
                            "Choose the years to display",
                            pickerInput("Years", choices = c("All Years","2013","2014","2015","2016","2017","2018"),selected = "All Years",multiple=TRUE)
                        
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
    
    output$dashboardPlot <- renderPlot({
        
    })
    
    output$noRequests <- renderValueBox({
        valueBox(
            paste0(NULL,"13,000,000"), "Number of Requests", icon = icon("arrow-right"),
            width = 2,
            color = "blue"
        )
    })
    
    output$avgRequests <- renderValueBox({
        valueBox(
            "400,000/month", "Avg. Requests/Month", icon = icon("calendar"),
            width = 2,
            color = "green"
        )
    })
    
    output$popularRequestType <- renderValueBox({
        valueBox(
            "TASC:Card", "Most popular method of Request", icon = icon("sunglasses", lib = "glyphicon"),
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
        dataset <- sqlFetch(conn,"flex22213",colnames = FALSE,rownames = FALSE)
        
        #exists(deparse(substitute(dataset)))    
        if (!is.null(dataset) && is.data.frame(dataset)) {
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
}


shinyApp(ui, server)