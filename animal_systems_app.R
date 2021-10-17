library(tidyverse)
library(lubridate)
library(plotly)
library(shiny)
library(here)


source(here::here("funcs", "zscore_outlier.R" ))
load(here::here("systems.Rdata"))




param_label = c("pH" = "pH",
                "Conductivity (microSiemens/cm)" = "Conductivity",
                "Water Temperature (Celsius)" = "Water_Temp",
                "Room Temperature (Celsius)" = "Room_Temp",
                "Biofilter Inflow Pressure (psi)" = "FBB_Pressure",
                "Finish Filter Inflow Pressure (psi)" = "FF_Pressure"
)
system_label = c("Zebrafish Main" = "zfish_main",
                 "Zebrafish Juveniles" = "zfish_juvenile",
                 "Axolotl Main" = "axo_main",
                 "ATK" = "atk",
                 "Davis Room Axolotl Main" = "davis_axo_main",
                 "Davis Room Axolotl Breeder" ="davis_axo_breeder",
                 "Zfish Backup 1" = "Zfish Backup 1",
                 "Zfish Backup 2" = "Zfish Backup 2",
                 "Zfish Quarantine" = "Zfish Quarantine",
                 "Davis Room Axolotl 2" = "Axo 2",
                 "Davis Room Axolotl 3" = "Axo 3"
                 )

ui <- fluidPage(
  
  titlePanel("Animal Housing Systems Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      p("Monitors connected to the animal housing systems collect and store sensor readings every 15-30 minutes. 
        This data is compiled on a regular basis, cleaned-up/organized, then uploaded to this app."),
      br(),
      
      #select box - choose system(s)
      #update: user can choose multiple systems
      selectInput(inputId = "system",
                  label = "Choose system",
                  choices = names(system_label),
                  multiple = TRUE,
                  selected = names(system_label[1:2])
      ),
      
      helpText("More than one systems can be displayed at a time."),
      
      #select box for parameters. choices by user selected system
      selectInput(inputId = "param", 
                  label = "Choose parameter", 
                  choices = NULL
      ),
      
      helpText("Some systems do not have sensors to detect a given parameter"),
      
      #date range input
      dateRangeInput('dateRange',
                     label = 'Choose date range',
                     start = Sys.Date() - 7, 
                     end = Sys.Date() + 1
      ),
      
      #checkbox for average
      checkboxInput("checkbox_avg", label = "Show average over date range", value = FALSE),
      br(),
      
      #remove outlier
      checkboxInput("checkbox_outlier", label = "Remove outliers", value = TRUE),
      helpText("Data points that are 3 standard deviations from the mean (over the date range)
               will be filtered out. Outliers tend to occur when the sensors are calibrated, which is done twice a month"),
      br(),
      
      #button to download 
      downloadButton("downloadData", "Download")
    ),
    
    #create area for plot and table
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotlyOutput("plot", width = "100%", height = "400px")),
                  tabPanel("Table", DT::dataTableOutput("table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  #user selects system(s)
  which_sys <- reactive({
    systems %>%
      keep(names(.) %in% system_label[input$system] )
  })
  
  #update select input for paramters
  #update: retrieve names of parameters 
  #update: all potential parameter choices can be seen when multiple systems are picked. 
  observeEvent(which_sys(),{
    req(input$system)
    choices <- which_sys() %>%
      map(names) %>%
      unlist()%>%
      unique()%>%
      .[.!="date"]
    updateSelectInput(session, "param", choices = choices)
  })
  
  #reactive - filter user-selected date range from selected system. 
  #update: filter list of system(s), then combine, add column for system name.
  which_date <- reactive({
    req(input$system)
    map(which_sys(), filter, date >= input$dateRange[1] & date <= input$dateRange[2])%>%
      bind_rows(.id = "system")
    
  })
  
  #if user selects outlier checkbox, uses zscore_outlier function to remove outliers
  outlier <- reactive({
    req(input$system)
    if(input$checkbox_outlier == FALSE) return(which_date())
    else {
      group_by(which_date(), system)%>%
        mutate(across(where(is.numeric), ~zscore_outlier(.x)))
    }
  })
  
  #create plot
  #update: line colors for different systems
  output$plot <- renderPlotly({
    req(input$system)
    req(input$param)
    #checks if any data within date range. If none, sends message to user.
    if(min(outlier()$date <= input$dateRange[1])){
      validate("No data for selected date range. Choose an earlier date.")
    }
    
    p = ggplot(outlier(), aes(x = .data$date, y = .data[[input$param]], group = system, 
                              text = paste("System:", system,
                                           "<br>", "Date:", 
                                           format(date, "%a %b-%d %H:%M"),
                                           "<br>", input$param, ":", .data[[input$param]]
                              ),
                              color = system)
              ) +
      #filter out NA from y-values. This prevents any breaks in geom_line
      geom_line(data = outlier()[!is.na(outlier()[[input$param]]),]) +
      #add axis labels
      labs(x = "Date", y = names(param_label[which(param_label==input$param)])) +
      #adjust x-scale breaks
      scale_x_datetime(breaks = scales::breaks_pretty(10)) +
      theme(axis.text.x = element_text(angle = -45, vjust = 0.5))
              
    
    #add smooth line when user clicks checkbox
    if(input$checkbox_avg)
      p = p + geom_smooth(aes(group = system))
    
    ggplotly(p, tooltip = "text")
  })
  
  #create table
  output$table <- DT::renderDT({
    t <- outlier()%>%
      arrange(desc(.data$date))
    t$date <- format(t$date, '%m-%d-%Y %H:%M')
    t
    
  })
  
  #download button
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$system, input$dateRange[1], "-", input$dateRange[2], ".csv", sep = "")
    },
    content = function(file) {
      write.csv(which_date(), file, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
