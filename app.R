library(shiny)
library(DT)
library(DBI)
library(RSQLite)
library(ggplot2)

# Database setup
conn <- dbConnect(RSQLite::SQLite(), "bus_commute.db")

dbExecute(conn, "
  CREATE TABLE IF NOT EXISTS commute (
    id INTEGER PRIMARY KEY, 
    date TEXT, 
    bus_route TEXT, 
    direction TEXT,
    scheduled_time TEXT, 
    get_on TEXT, 
    get_off TEXT, 
    duration INTEGER
  )")

dbDisconnect(conn)

# UI
ui <- fluidPage(
  titlePanel("Daily Bus Commute Logger"),
  sidebarLayout(
    sidebarPanel(
      h3("Input Details"),
      dateInput("date", "Date", value = Sys.Date(), format = "yyyy-mm-dd"),
      textInput("bus_route", "Bus Route", value = "431"),
      selectInput("direction", "Direction", choices = c("City Bound", "Home Bound")),
      uiOutput("scheduled_time_ui"),
      textInput("get_on", "Get On Time (HH:MM)", value = "07:20"),
      textInput("get_off", "Get Off Time (HH:MM)", value = "07:54"),
      actionButton("submit", "Submit"),
      downloadButton("download_csv", "Download CSV")
    ),
    mainPanel(
      h3("Travel Time Trend"),
      plotOutput("travel_time_plot"),
      hr(),
      DTOutput("commute_table")
    )
  ),
  tags$script("
    $(document).on('click', '.btn-delete', function() {
      var id = $(this).attr('id').split('_')[1];
      Shiny.setInputValue('delete_id', id);
    });
  ")
)

# Server
server <- function(input, output, session) {
  load_data <- reactiveVal()
  
  refresh_data <- function() {
    conn <- dbConnect(RSQLite::SQLite(), "bus_commute.db")
    data <- dbGetQuery(conn, "SELECT * FROM commute ORDER BY date DESC")
    dbDisconnect(conn)
    load_data(data)
  }
  
  refresh_data()
  
  observeEvent(input$submit, {
    req(input$get_on, input$get_off, input$direction)
    
    get_on_time <- as.POSIXct(paste(input$date, input$get_on), format="%Y-%m-%d %H:%M")
    get_off_time <- as.POSIXct(paste(input$date, input$get_off), format="%Y-%m-%d %H:%M")
    
    if (is.na(get_on_time) || is.na(get_off_time) || get_off_time <= get_on_time) {
      showNotification("Invalid time input", type = "error")
      return()
    }
    
    duration <- as.numeric(difftime(get_off_time, get_on_time, units = "mins"))
    
    conn <- dbConnect(RSQLite::SQLite(), "bus_commute.db")
    dbExecute(conn, "INSERT INTO commute (date, bus_route, direction, scheduled_time, get_on, get_off, duration) 
                     VALUES (?, ?, ?, ?, ?, ?, ?)",
              params = list(
                as.character(input$date), 
                input$bus_route, 
                input$direction,
                input$scheduled_time, 
                input$get_on, 
                input$get_off, 
                duration
              ))
    dbDisconnect(conn)
    showNotification("Commute logged successfully", type = "message")
    
    refresh_data()
  })
  
  output$commute_table <- renderDT({
    data <- load_data()
    if (nrow(data) == 0) return(NULL)
    data$Action <- paste0('<button class="btn-delete" id="delete_', data$id, '">Delete</button>')
    datatable(data, escape = FALSE, selection = "none", options = list(pageLength = 10))
  }, server = FALSE)
  
  observeEvent(input$delete_id, {
    id <- as.numeric(input$delete_id)
    
    conn <- dbConnect(RSQLite::SQLite(), "bus_commute.db")
    dbExecute(conn, "DELETE FROM commute WHERE id = ?", params = list(id))
    dbDisconnect(conn)
    
    showNotification("Record deleted successfully", type = "message")
    refresh_data()
  })
  
  output$scheduled_time_ui <- renderUI({
    if (input$bus_route == "431" && input$direction == "City Bound") {
      selectInput("scheduled_time", "Scheduled Time", choices = c("07:19", "07:54"))
    } else if (input$bus_route == "431" && input$direction == "Home Bound") {
      selectInput("scheduled_time", "Scheduled Time", choices = c("16:54", "17:25"))
    } else {
      textInput("scheduled_time", "Scheduled Time (HH:MM)", value = "07:20")
    }
  })
  
  output$travel_time_plot <- renderPlot({
    data <- load_data()
    if (nrow(data) == 0) return(NULL)
    
    data$date <- as.Date(data$date)
    
    ggplot(data, aes(x = date, y = duration)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(title = "Trend of Travel Time", x = "Date", y = "Duration (minutes)") +
      theme_minimal()
  })
  
  output$download_csv <- downloadHandler(
    filename = function() { "commute_data.csv" },
    content = function(file) {
      data <- load_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
