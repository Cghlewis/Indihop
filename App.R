library(shiny)
library(ggplot2)
library(tidyverse)

outputDir <- "responses"

# Define the fields we want to save from the form
fields <- c("Name","Beer", "flavor", "aroma")

saveData <- function(input) {
  # put variables in a data frame
  data <- data.frame(matrix(nrow=1,ncol=0))
  for (x in fields) {
    var <- input[[x]]
    if (length(var) > 1 ) {
      # handles lists from checkboxGroup and multiple Select
      data[[x]] <- list(var)
    } else {
      # all other data types
      data[[x]] <- var
    }
  }
  data$submit_time <- date()
  
  # Create a unique file name
  fileName <- sprintf(
    "%s_%s.rds", 
    as.integer(Sys.time()), 
    digest::digest(data)
  )
  
  # Write the file to the local system
  saveRDS(
    object = data,
    file = file.path(outputDir, fileName)
  )
}

loadData <- function() {
  # read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  
  if (length(files) == 0) {
    # create empty data frame with correct columns
    field_list <- c(fields, "submit_time")
    data <- data.frame(matrix(ncol = length(field_list), nrow = 0))
    names(data) <- field_list
  } else {
    data <- lapply(files, function(x) readRDS(x)) 
    
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
  }
  
  data
}

resetForm <- function(session) {
  # reset values
  updateSelectInput(session,"Name", selected=character(0))
  updateSelectInput(session, "Beer", selected=character(0))
  updateNumericInput(session, "flavor", value = 1)
  updateNumericInput(session, "aroma", value = 1)
}

ui <- fluidPage(
  
  # App title ----
  titlePanel("IndiHop Ratings"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput("Name",
                  "Your Name",
                  c(" ","Crystal", "Josh", "Jacob", "Rena","Lacy", "Dre")
      ),
      selectInput("Beer",
                  "Name of Beer", 
                  c(" ","City Wide","High Life", "Ginger Wheat"
                  )
      ),
      numericInput("flavor", "Flavor Rating",  min = 1, max = 5, step = 1, value = 1),
      numericInput("aroma", "Aroma Rating",
                   min = 1, max = 5, step = 1, value = 1),
      actionButton("submit", "Submit"),
      actionButton("clear", "Clear Form"),
      downloadButton("downloadData", "Download")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput(outputId = "flavorPlot"),
      tags$hr(),
      dataTableOutput("responses")
    )
  )
)

server = function(input, output, session) {
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(input)
    resetForm(session)
  })
  
  observeEvent(input$clear, {
    resetForm(session)
  })
  
  
  # Show the previous responses in a reactive table ----
  output$responses <- renderDataTable({
    # update with current response when Submit or Delete are clicked
    input$submit 
    
    
    loadData()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      write.csv(loadData(), file, row.names = FALSE, quote= TRUE)
    }
  )
  
  output$flavorPlot <- renderPlot({
    input$submit
    
    data <- loadData()
    
    data%>%
      mutate(Score = rowSums(select(., flavor:aroma))) %>%
      group_by(Beer) %>%
      summarize(AvgScore = mean(Score))%>%
      ggplot(aes(x=reorder(Beer, -AvgScore), y=AvgScore)) +
      geom_bar(stat="identity", fill = "goldenrod2")+geom_text(aes(label=round(AvgScore,2)))+
      xlab("Beer")
    
  })
}

shinyApp(ui, server)