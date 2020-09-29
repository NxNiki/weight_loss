#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI
ui <- fluidPage(
    
    titlePanel("Developmental Weight Suppression Calculator"),
    
    fluidRow(
        verbatimTextOutput("instructions")
    ),
    
    fluidRow(
        
        column(6,
               tags$hr(),
               fileInput("file1", "Choose CSV File",
                         accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
               )
               #checkboxInput("header", "Header", TRUE)
               )
        
    ),
    
#    fluidRow(
#
#        column(6,
#               textInput("filepath", h3("output file path:"), 
#                         value = getwd()))
#    ),
#        
#    fluidRow(
#        column(6, offset = 0,
#               textInput("filename", h3("output file name:"), 
#                         value = "output.csv"))
#    ),
    

    fluidRow(
        
        
        column(3,
               #h3("submit"),
               actionButton("generateButton", "Submit"),
               br(),
               br(),
               downloadButton("downloadData", "Download"))
               #br(), 
               #submitButton("Submit"))
        
    ),
    
    mainPanel(
        tableOutput("writecsv")
    )
    
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
    output$instructions <- renderText({
        paste( "Instructions:",
               "1. Preparing your CSV file:", 
               "       a. Assign correct numeric labels to gender (1 = Male, 2 = Female)",
               "       b. Use the following variable names for the 5 required variables (names are case sensitive!):", 
               "           i. Highest premorbid BMI: “highest_premorbid_BMI”", 
               "           ii. Highest premorbid age (in years): “premorbid_age”", 
               "           iii. Current BMI: “current_BMI”", 
               "           iv. Current age (in years): “current_age”", 
               "           V. Sex: “sex”", 
               "       c. If not already in CSV format, export as a CSV file", 
               "2. Click “Choose CSV file” and upload your CSV file", 
               #"3. Click “Output file path” and enter your desired file path (can use default file path)", 
               #"4. Click “Output file name” and enter your desired name (can use default file name)", 
               "3. Press “submit” ", 
               "4. Press Download to download result. file name should end with .csv",
               "Note: If an individual is missing data for any of the above variables, the UI will not calculate a",
               "developmental weight suppression value for them, and they will be removed from the output dataset.",
               "Please also note that the output dataset will display the recoded ages for all individuals.",
               sep = '\n')
    })

    
    dataInput <- reactive({
        
        if(input$generateButton == 0){return()}
        
        inFile <- input$file1
        
        isolate({ 
            input$generateButton
            
            # read the file input form UI
            #upload_file = read.csv(inFile$datapath, header = input$header)
            upload_file = read.csv(inFile$datapath, header = T)
            
            #colnames(upload_file) = c('highest_premorbid_BMI', 'premorbid_age', 'current_BMI', 'current_age', 'sex')
            ###  your own calculations:
            
            ## Importing CDC data. Splitting into male and female reference data sets. Male = 1, Female = 2. 
            ref  <- read.csv(file="CDC_reference.csv", header = TRUE)
            refM <- ref[which(ref$cdc_sex==1), ] # CDC males only
            refF <- ref[which(ref$cdc_sex==2), ] # CDC females only
            
            library(foreign)
            ## Importing primary data. MAKE SURE SEX IS CODED APPROPRIATELY (MALE = 1, FEMALE = 2).
            #upload_file <- read.csv("test.csv" , header = TRUE)
            
            data <- upload_file[complete.cases(upload_file), ] # Removing any cases which have missing data
            
            n = dim(data)[1]
            data$sex <- as.numeric(data$sex)
            
            ## Recoding >20 years old, to 20 years old. 
            data$premorbid_age[data$premorbid_age>20]=20.0
            data$current_age[data$current_age>20]=20.0
            
            ## Splitting primary data into sex datasets.
            ind1  <- which(data$sex==1)
            dataM <- data[ind1, ] # Dataset of males
            n1    <- dim(dataM)[1]
            
            ind2  <- which(data$sex==2)
            dataF <- data[ind2, ] # Dataset of females
            n2    <- dim(dataF)[1]
            
            # If there are males in the dataset, then calculate zBMIs...
            if( dim(dataM)[1] > 0 ) {
                
                # Male, premorbid zBMI
                premorbid_zBMI <- NULL
                for (i in 1:n1){
                    
                    if(min(abs(refM$cdc_age_mos - dataM[i, ]$premorbid_age*12)) == 0.5){
                        p <- which.min(abs(refM$cdc_age_mos - dataM[i, ]$premorbid_age*12))
                        m <- p+1 # This chooses the larger of the two values, to yield a more conservative zBMI estimate.
                        premorbid_temp <- ((dataM$highest_premorbid_BMI[i] / refM[m, ]$mu )^refM[m, ]$lambda - 1)/(refM[m, ]$lambda * refM[m, ]$sigma)
                        
                    } else {
                        p <- which.min(abs(refM$cdc_age_mos - dataM[i, ]$premorbid_age*12))
                        
                        premorbid_temp <- ((dataM$highest_premorbid_BMI[i] / refM[p, ]$mu )^refM[p, ]$lambda - 1)/(refM[p, ]$lambda * refM[p, ]$sigma)
                        
                    }
                    
                    premorbid_zBMI <- c(premorbid_zBMI, premorbid_temp)
                    
                }
                
                # Male, current zBMI
                current_zBMI=NULL
                for (i in 1:n1){
                    
                    if(min(abs(refM$cdc_age_mos - dataM[i, ]$current_age*12)) == 0.5){
                        p <- which.min(abs(refM$cdc_age_mos - dataM[i, ]$current_age*12))
                        m <- p+1 # This chooses the larger of the two values, to yield a more conservative zBMI estimate.
                        current_temp <- ((dataM$current_BMI[i] /  refM[m, ]$mu )^refM[m, ]$lambda - 1)/(refM[m, ]$lambda * refM[m, ]$sigma)
                        
                    } else {
                        p <- which.min(abs(refM$cdc_age_mos - dataM[i, ]$current_age*12))
                        
                        current_temp <- ((dataM$current_BMI[i] /  refM[p, ]$mu )^refM[p, ]$lambda - 1)/(refM[p, ]$lambda * refM[p, ]$sigma)
                        
                    }
                    
                    current_zBMI <- c(current_zBMI, current_temp)
                    
                }
                
                # Combining and labeling current and premorbid zBMI into male zBMI dataset. 
                dataM <- cbind(dataM, premorbid_zBMI, current_zBMI)
                names(dataM$premorbid_zBMI) <- "premorbid_zBMI"
                names(dataM$current_zBMI) <- "current_zBMI" 
            }
            
            # If there are females in the dataset, then calculate zBMIs...
            if( dim(dataF)[1] > 0 ) { 
                # Female, premorbid zBMI.
                premorbid_zBMI=NULL
                for (i in 1:n2){
                    
                    if(min(abs(refF$cdc_age_mos - dataF[i, ]$premorbid_age*12)) == 0.5){
                        p <- which.min(abs(refF$cdc_age_mos - dataF[i, ]$premorbid_age*12))
                        m <- p+1 # This chooses the larger of the two values, to yield a more conservative zBMI estimate.
                        premorbid_temp <- ( (dataF$highest_premorbid_BMI[i] /  refF[m, ]$mu )^refF[m, ]$lambda - 1)/(refF[m, ]$lambda *refF[m, ]$sigma)
                        
                    } else {
                        p <- which.min(abs(refF$cdc_age_mos - dataF[i, ]$premorbid_age*12))
                        
                        premorbid_temp <- ((dataF$highest_premorbid_BMI[i] / refF[p, ]$mu )^refF[p, ]$lambda - 1)/(refF[p, ]$lambda * refF[p, ]$sigma)
                        
                    }
                    
                    premorbid_zBMI <- c(premorbid_zBMI, premorbid_temp)
                    
                }
                
                # Female, current zBMI.
                current_zBMI=NULL
                for (i in 1:n2){
                    
                    if(min(abs(refF$cdc_age_mos - dataF[i, ]$current_age*12)) == 0.5){
                        p <- which.min(abs(refF$cdc_age_mos - dataF[i, ]$current_age*12))
                        m <- p+1 # This chooses the larger of the two values, to yield a more conservative zBMI estimate.
                        current_temp <- ((dataF$current_BMI[i] /  refF[m, ]$mu )^refF[m, ]$lambda - 1)/(refF[m, ]$lambda * refF[m, ]$sigma)
                        
                    } else {
                        p <- which.min(abs(refF$cdc_age_mos - dataF[i, ]$current_age*12))
                        
                        current_temp <- ((dataF$current_BMI[i] / refF[p, ]$mu )^refF[p, ]$lambda - 1)/(refF[p, ]$lambda * refF[p, ]$sigma)
                        
                    }
                    
                    current_zBMI <- c(current_zBMI, current_temp)
                    
                }
                
                # Combining and labeling current and premorbid zBMI into male zBMI dataset. 
                
                dataF <- cbind(dataF, premorbid_zBMI, current_zBMI)
                names(dataF$premorbid_zBMI) <- "premorbid_zBMI"
                names(dataF$current_zBMI) <- "current_zBMI"
                
            }
            
            ## Combining male and female zBMI data subsets into final dataset. 
            if( dim(dataM)[1] == 0 ){ # If no males, then append females to final dataset...
                output_file <- dataF } else if(dim(dataF)[1] == 0){ # If no females, then append females to final dataset...
                    output_file <- dataM 
                } else { # If males and females, append all to final dataset...
                    
                    output_file <- rbind(dataM, dataF)
                }
            
            ## Calculating developmental WS, recoding negatives to 0. 
            output_file$DWS <- (output_file$premorbid_zBMI - output_file$current_zBMI)
            output_file$DWS[output_file$DWS<0]=0
            
            #####
        })
        #write.csv(output_file, paste0(input$filepath, .Platform$file.sep, input$filename))
        
        return(output_file)

    })
    
    #output$writecsv <- renderTable({dataInput()})
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("output", ".csv", sep = "")
      },
      
      content = function(file) {
        write.csv(dataInput(), file, row.names = FALSE)
      }
    )
   
}

# Run the application 
shinyApp(ui = ui, server = server)



#rsconnect::deployApp("C:/Users/Xin/OneDrive - Drexel University/Rshiny/devWS")

