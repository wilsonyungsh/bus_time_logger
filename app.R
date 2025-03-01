library(shiny)
library(DT)
library(DBI)
library(RSQLite)

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
      actionButton("undo", "Undo")
    ),
    mainPanel(
      h3("Travel time trend"),
      hr(),
      plotOutput("travel_time_plot"),
      DTOutput("commute_table")
    )
  ),
  tags$script("
    $(document).on('click', '.btn-delete', function() {
      var id = $(this).attr('id').split('_')[1];
      Shiny.setInputValue('delete_id', id);
    });
    $(document).on('click', '.btn-update', function() {
      var id = $(this).attr('id').split('_')[1];
      Shiny.setInputValue('update_id', id);
    });
  ")
)

# Server
server <- function(input, output, session) {
  load_data <- reactiveVal()
  history <- reactiveValues(
    actions = list(),
    current_index = 0
  )
  
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
    
    # Save action to history
    history$actions <- c(history$actions, list(action = "insert", data = data.frame(
      date = input$date,
      bus_route = input$bus_route,
      direction = input$direction,
      scheduled_time = input$scheduled_time,
      get_on = input$get_on,
      get_off = input$get_off,
      duration = duration
    )))
    history$current_index <- length(history$actions)
    
    refresh_data()
  })
  
  output$commute_table <- renderDT({
    data <- load_data()
    data$Action <- paste0(
      '<button class="btn-delete" id="delete_', data$id, '">Delete</button> ',
      '<button class="btn-update" id="update_', data$id, '">Update</button>'
    )
    datatable(data, escape = FALSE, selection = "none", options = list(pageLength = 50))
  }, server = FALSE)
  
  observeEvent(input$delete_id, {
    id <- as.numeric(input$delete_id)
    conn <- dbConnect(RSQLite::SQLite(), "bus_commute.db")
    record <- dbGetQuery(conn, "SELECT * FROM commute WHERE id = ?", params = list(id))
    dbDisconnect(conn)
    
    # Save action to history
    history$actions <- c(history$actions, list(action = "delete", data = record))
    history$current_index <- length(history$actions)
    
    conn <- dbConnect(RSQLite::SQLite(), "bus_commute.db")
    dbExecute(conn, "DELETE FROM commute WHERE id = ?", params = list(id))
    dbDisconnect(conn)
    showNotification("Record deleted successfully", type = "message")
    refresh_data()
  })
  
  observeEvent(input$update_id, {
    id <- as.numeric(input$update_id)
    conn <- dbConnect(RSQLite::SQLite(), "bus_commute.db")
    record <- dbGetQuery(conn, "SELECT * FROM commute WHERE id = ?", params = list(id))
    dbDisconnect(conn)
    
    # Save action to history
    history$actions <- c(history$actions, list(action = "update_before", data = record))
    history$current_index <- length(history$actions)
    
    if (nrow(record) > 0) {
      updateTextInput(session, "scheduled_time", value = record$scheduled_time)
      updateTextInput(session, "get_on", value = record$get_on)
      updateTextInput(session, "get_off", value = record$get_off)
      updateTextInput(session, "bus_route", value = record$bus_route)
      updateSelectInput(session, "direction", selected = record$direction)
      updateDateInput(session, "date", value = as.Date(record$date))
    }
  })
  
  observeEvent(input$submit, {
    # Update record if updating
    if (!is.null(input$update_id)) {
      id <- as.numeric(input$update_id)
      get_on_time <- as.POSIXct(paste(input$date, input$get_on), format="%Y-%m-%d %H:%M")
      get_off_time <- as.POSIXct(paste(input$date, input$get_off), format="%Y-%m-%d %H:%M")
      
      if (is.na(get_on_time) || is.na(get_off_time) || get_off_time <= get_on_time) {
        showNotification("Invalid time input", type = "error")
        return()
      }
      
      duration <- as.numeric(difftime(get_off_time, get_on_time, units = "mins"))
      
      conn <- dbConnect(RSQLite::SQLite(), "bus_commute.db")
      dbExecute(conn, "UPDATE commute SET 
                        date = ?, 
                        bus_route = ?, 
                        direction = ?, 
                        scheduled_time = ?, 
                        get_on = ?, 
                        get_off = ?, 
                        duration = ?
                      WHERE id = ?",
                params = list(
                  as.character(input$date), 
                  input$bus_route, 
                  input$direction,
                  input$scheduled_time, 
                  input$get_on, 
                  input$get_off, 
                  duration,
                  id
                ))
      dbDisconnect(conn)
      
      # Save action to history
      history$actions <- c(history$actions, list(action = "update_after", data = data.frame(
        id = id,
        date = input$date,
        bus_route = input$bus_route,
        direction = input$direction,
        scheduled_time = input$scheduled_time,
        get_on = input$get_on,
        get_off = input$get_off,
        duration = duration
      )))
      history$current_index <- length(history$actions)
      
      showNotification("Record updated successfully", type = "message")
      refresh_data()
    }
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
  
  observeEvent(input$undo, {
    if (history$current_index > 0) {
      action <- history$actions[[history$current_index]]
      history$current_index <- history$current_index - 1
      
      if (action$action == "insert") {
        conn <- dbConnect(RSQLite::SQLite(), "bus_commute.db")
        dbExecute(conn, "DELETE FROM commute WHERE date = ? AND bus_route = ? AND direction = ? AND scheduled_time = ? AND get_on = ? AND get_off = ? AND duration = ?",
                  params = list(
                    action$data$date,
                    action$data$bus_route,
                    action$data$direction,
                    action$data$scheduled_time,
                    action$data$get_on,
                    action$data$get_off,
                    action$data$duration
                  ))
        dbDisconnect(conn)
        showNotification("Insertion undone", type = "message")
        
      } else if (action$action == "delete") {
        conn <- dbConnect(RSQLite::SQLite(), "bus_commute.db")
        dbExecute(conn, "INSERT INTO commute (date, bus_route, direction, scheduled_time, get_on, get_off, duration) 
                         VALUES (?, ?, ?, ?, ?, ?, ?)",
                  params = list(
                    action$data$date,
                    action$data$bus_route,
                    action$data$direction,
                    action$data$scheduled_time,
                    action$data$get_on,
                    action$data$get_off,
                    action$data$duration
                  ))
        dbDisconnect(conn)
        showNotification("Deletion undone", type = "message")
        
      } else if (action$action == "update_before") {
        conn <- dbConnect(RSQLite::SQLite(), "bus_commute.db")
        dbExecute(conn, "UPDATE commute SET 
                        date = ?, 
                        bus_route = ?, 
                        direction = ?, 
                        scheduled_time = ?, 
                        get_on = ?, 
                        get_off = ?, 
                        duration = ?
                      WHERE id = ?",
                  params = list(
                    action$data$date, 
                    action$data$bus_route, 
                    action$data$direction,
                    action$data$scheduled_time, 
                    action$data$get_on, 
                    action$data$get_off, 
                    action$data$duration,
                    action$data$id
                  ))
        dbDisconnect(conn)
        showNotification("Update undone", type = "message")
      }
      
      refresh_data()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
