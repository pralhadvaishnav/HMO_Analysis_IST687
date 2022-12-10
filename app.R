#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
library(tidyverse)
library(caret)
library(kernlab)

hmo = read_csv(url("https://intro-datascience.s3.us-east-2.amazonaws.com/HMO_data.csv"))

cost_threshold = 5000
hmo$expensive <- hmo$cost
hmo<-mutate(hmo, expensive = factor(ifelse(cost > cost_threshold, "TRUE", "FALSE")))
hmo<-hmo[complete.cases(hmo),]


#DATA PARTITION
library(caret)
#MODEL
library(kernlab)
final_model <- glm(data = hmo,
                   formula = expensive ~ age + bmi + children + smoker + exercise + hypertension, 
                   family = "binomial")



our_model <- final_model
save(our_model, file = "our_model.rda")
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    fileInput("upload", label="HMO inout file", accept = c(".csv")),
    #Read the actual (solution) data
    fileInput("upload_Solution", label="HMO solution file", accept = c(".csv")),
    #get a number (how much of the dataframe to show)
    numericInput("n", "Number of Rows", value = 5, min = 1, step = 1),
    #a place to output a table (i.e., a dataframe)
    tableOutput("headForDF"),
    #output the results (for now, just simple text)
    verbatimTextOutput("txt_results", placeholder = TRUE)
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    #require an input file, then read a CSV file
    getTestData <- reactive({
        req(input$upload)
        read_csv(input$upload$name)
    })
    #require an the actual values for the prediction (i.e. solution file)
    getSolutionData <- reactive({
        req(input$upload_Solution)
        read_csv(input$upload_Solution$name)
    })
    
    #show the output of the model
    output$txt_results <- renderPrint({
        #load the data
        dataset <- getTestData()
        dataset_solution <- getSolutionData()
        #load and use the model on the new data
        use_model_to_predict(dataset, dataset_solution)
    })
    #show a few lines of the dataframe
    output$headForDF <- renderTable({
        df <- getTestData()
        head(df, input$n)
    })
}
#these libraries are needed, will be used with predict
library(caret); library(kernlab); library(e1071)
#load a model, do prediction and compute the confusion matrix
use_model_to_predict <- function(df, df_solution){
    #load the pre-built model, we named it ‘out_model.rda’)
    load(file="our_model.rda")
    #use the model with new data
    
    

    pred <- predict(our_model, df, type = "response")
    pred_expensive = factor(ifelse(pred > 0.2, 'TRUE', 'FALSE'))
    #show how the model performed
    df_solution$expensive<- as.factor(df_solution$expensive)
    confusionMatrix(data = pred_expensive, 
                    reference = df_solution$expensive, 
                    positive = 'TRUE')
    
}

# Run the application 
shinyApp(ui = ui, server = server)
