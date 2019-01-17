
library(shiny)
#setwd("D:/vu-master/Project OBP/app")
arrival_instances <- readLines("arrInstances.txt")
crptw_instances <- readLines("crptwInstances.txt")

#werkt niet, vandaar functie hieronder geplakt 
#source("readfilesKatrin.R")

########## readfiles 
read_crptw = function(filepath){
  # read first line separately
  header = unname(read.table(filepath, nrows=1, sep="\n", stringsAsFactors = FALSE))[[1]][1]
  stacks = levels(unname(read.table(filepath, sep="\n", skip=1))[[1]])
  
  #extract dimensions of bay from first line
  dims = strsplit(strsplit(header, " ")[[1]][1], "_")[[1]][2]
  n_tiers = type.convert(substr(dims, 3, 4))
  n_stacks = type.convert(substr(dims, 1, 2))
  
  # create bay
  bay = matrix(nrow=n_tiers, ncol=n_stacks)
  
  # fill bay with containers
  for (i in 1:n_stacks){
    containers = as.numeric(strsplit(stacks[i], "\\D+")[[1]][-1])[-c(1,2,3)]
    if(length(containers)!=0){
      sequence = seq(1, length(containers), 2)
      for(j in 1:length(sequence)){
        bay[j,i] = containers[sequence[j]]
      }
    }
  }
  return(bay[nrow(bay):1,])
}

read_arr = function(filepath){
  arrivals = read.csv(filepath, sep=",")
  names(arrivals) = c("n", "arr", "dep")
  return(arrivals[order(arrivals$arr),])
}

################################################

#APP

# Define UI 
ui <- navbarPage("Container Relocation", fluid = TRUE, id = "inTabset",
                 navbarMenu("Bay selection",
                            #window 1.a
                            tabPanel("Select known instance",
                                     column (6, 
                                     numericInput("BayInputNumberStacks", h3("Give the number of stacks"), value = 5, min =5, max=10),
                                     numericInput("BayInputMaximumHeight", h3("Give the maximum stack height"), value = 3, min = 3, max = 6),
                                     numericInput("BayInstance", h3("Choose the instance"), value = 1),
                                     column(6,
                                     textOutput("dimensionsOfBay")),
                                     actionButton("action",label = "Show the bay"),
                                     textOutput("selectedBay"))
                                     ),
                            #window 1.b
                            tabPanel("Upload instance",
                                     fileInput("bayFile", h3("Upload bay")),
                                     textOutput("dimensionsOfUploadedBay")
                                     )
                            ),
                 navbarMenu("Arrival selection",
                            #window 2.a
                            tabPanel("Select known instance",
                                     numericInput("ArrivalAmount", h3("Give the number of arriving containers"), value = 8, min =8, max = 40),
                                     numericInput("ArrivalTimeIntervals", h3("Give the different number of intervals"), value = 4, min = 4, max =24),
                                     numericInput("ArrivalInstance", h3("Choose the instance"), value = 0, min =0, max =29),
                                     textOutput("chosenArrival"),
                                     actionButton("process",label = "Process")
                            ),
                            #window 2.b
                            tabPanel("Upload instance",
                                     fileInput("bayFile", h3("Upload arrivals")),
                                     actionButton("processB",label = "Process")
                            )
                 ),
                 # window 3
                 tabPanel("Forecast", value = "tab3",
                          textOutput("estNumMoves"),
                          radioButtons("arrOrDep", label = "What type of event is it?",
                                       choices = list("Departure" =1, 
                                       "Arrival" = 2), selected = 1
                                       ),
                          actionButton("jumpTo4", "Show relocation plan")
                          ),
                 # window 4
                 tabPanel("Relocation plan",value = "tab4"
                          )
)

#server
#Here is the server function for the Hello Shiny example.
server <- function(input, output, session) {
  # window 1.a give dimensions
  output$dimensionsOfBay <- renderText({ 
    paste("The dimensions of the bay are ", input$BayInputNumberStacks, " by ",input$BayInputMaximumHeight)
  })
  # window 1.b give dimensions of uploaded bay
  output$dimensionsOfUploadedBay <- renderText({ 
      a <- length(input$bayFile)
      header = unname(input$bayFile)[[1]][1]
      dims = strsplit(strsplit(header, " ")[[1]][1], "_")[[1]][2]
      paste("The dimensions of the bay are ",dims)
  })
  
  # window 1.a selecting instance. Needs to become a table
  # is BayInstance the wright input??? 
  observeEvent(input$action,{
      bay <- read_crptw(crptw_instances[input$BayInstance])
      output$selectedBay <- renderText(bay)
  })
  
  #window 2a
  output$chosenArrival <- renderText({
    paste("The chosen arrival instance is ",input$ArrivalAmount,input$ArrivalTimeIntervals, "container number", input$ArrivalInstance)
  })
  
  output$estNumMoves <- renderText({
    "The estimated number of moves is X (bayManager or forecast?)"
  })
  
  #jump from 2a to 3
  observeEvent(input$process, {updateTabsetPanel(session, "inTabset", selected = "tab3")})
  
  #jump from 2b to 3
  observeEvent(input$processB, {updateTabsetPanel(session, "inTabset", selected = "tab3")})
  
  #jump from 3 to 4
  observeEvent(input$jumpTo4, {updateTabsetPanel(session, "inTabset", selected = "tab4")})
}

shinyApp(ui = ui, server = server)
# Define server logic required to draw a histogram ----


