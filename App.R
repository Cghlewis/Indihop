library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)

#load responses into a response folder

outputDir <- "responses"

# Define the fields we want to save from the form
fields <- c("Name","Beer", "flavor", "aroma", "appearance","drinkability")

#save data
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

#load data
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

#reset form after submission
resetForm <- function(session) {
  # reset values
  updateSelectInput(session,"Name", selected=character(0))
  updateSelectInput(session, "Beer", selected=character(0))
  updateSliderInput(session, "flavor", value = 1)
  updateSliderInput(session, "aroma", value = 1)
  updateSliderInput(session, "appearance", value = 1)
  updateSliderInput(session, "drinkability", value = 1)
}


#ui
ui <- fluidPage(
  
  # App title ----
  titlePanel("IndiHop 2019 Beer Ratings"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput("Name",
                  "Your Name",
                  c(" ","Test","Crystal", "Josh", "Jacob", "Rena","Lacy", "Dre")
      ),
      selectInput("Beer",
                  "Name of Beer", 
                  c(" ","4 Hands City Wide Pale Ale","Civil Life Scottish Ale",
                    "2nd Shift Katy Brett Saison", "Logboat Shiphead"
                  )
      ),
      sliderInput("flavor", "Flavor Rating (Weight 30%)",  min = 1, max = 5, step = 1, value = 1),
      sliderInput("aroma", "Aroma Rating (Weight 20%)",
                   min = 1, max = 5, step = 1, value = 1),
      sliderInput("appearance", "Appearance Rating (Weight 10%)",
                   min = 1, max = 5, step = 1, value = 1),
      sliderInput(
        "drinkability", "Drinkability Rating (Weight 40%)",
        min = 1, max = 5, step = 1, value = 1
      ),
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

#server

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
    # update with current response when Submit is clicked
    input$submit
    
    loadData()
  }, options=list(pageLength=5, lengthMenu = c(5, 10, 15, 20, 30, 50)))
                  
                  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      write.csv(loadData(), file, row.names = FALSE, quote= TRUE)
    }
  )
 
  #make plot of output
  #bar chart of weighted score per person averaged across each beer and then only the top 3 displayed 
  #SD for each beer is also displayed (error bars)
  output$flavorPlot <- renderPlot({
    input$submit
    
    data <- loadData()
    
    data%>%
      rowwise%>%
      mutate(Score = (flavor*.3)+(aroma*.2)+(appearance*.1)+(drinkability*.4)) %>%
      group_by(Beer) %>%
      summarize(AvgScore = mean(Score), SD=sd(Score))%>%
      arrange(desc(AvgScore)) %>%
      slice(1:3) %>%
      ggplot(aes(x=reorder(Beer, -AvgScore), y=AvgScore)) +
      geom_bar(stat="identity", fill = "goldenrod2", color="black")+geom_text(aes(label=round(AvgScore,1)),
                                                                              position = position_nudge(y = -8))+
      xlab("Top 3 Beers")+ylab("Average Weighted Score")+geom_errorbar(aes(ymin=AvgScore-SD, ymax=AvgScore+SD), width=.2,
                                 position=position_dodge(.9))+
      theme(axis.text=element_text(face="bold"))+theme_classic()+ylim(0,6.5)
    
  })
}

shinyApp(ui, server)