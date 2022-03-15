#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI 
ui <- fluidPage(
    
    sidebarPanel(
        
        selectizeInput("select",
                       label = "Cylinder" ,
                       choices = unique(mtcars$cyl),
                       selected = 4
        ),
        

        
        
        
        downloadButton("report", "Generate report for selected"),
        br(),br()
    ),
    
    mainPanel(
        DT::dataTableOutput('myTable1')
    )


)

# Define server 
server <- function(input, output) {
    
    mtcars <- tibble::rownames_to_column(mtcars, "Name")
    v <- reactiveValues(data = NULL)
    
    observeEvent(input$select, {
        v$data <- mtcars %>% filter(cyl == input$select)
        write.csv(v$data, "mydata.csv")
    })
    
    
    output$myTable1 <- DT::renderDataTable({
        
        if (is.null(v$data)) return()
        DT::datatable(v$data) 
    })
    
    output$report <- downloadHandler(
        
        filename = function(){
            paste0(v$data$Name[input$myTable1_rows_selected],".doc")
        },
        content = function(file) {
            params = list(j=input$myTable1_rows_selected)
            p1 = here::here("download_report", "report.Rmd")
            p2 = here::here("download_report", "New Microsoft Word Document.docx")
            src = c(p1, p2)
            
            owd <- setwd(tempdir())
            on.exit(setwd(owd), add = TRUE)
            file.copy(from = src, to = c('report.Rmd', 'New Microsoft Word Document.docx'), overwrite = TRUE)
            # browser()
            # k <- (input$myTable1_rows_selected)
            # fs <- c()
            # for ( i in k) 
            # {
                # path <- paste0(v$data$Name[k],".doc")

                
                rmarkdown::render(here::here("download_report", "report.Rmd"), output_file = file,
                                  params = params,
                                  envir = new.env(parent = globalenv())
                )
            #     fs <- c(fs,path)
            #     
            # # }
            # zip(file,fs)
        }
    )
    
    
    # output$report <- downloadHandler(
    #     
    #     filename = function(){
    #         paste0(input$select,".doc")
    #     },
    #     content = function(file) {
    #         params = list(j = input$select)
    #         
    #         p1 = "C:\\Users\\mansari\\University of Mississippi Medical Center\\Pramod Anugu - Exam 4\\Weekly Reports\\Other Reports\\Blood Pressure\\report.Rmd"
    #         p2 = "C:\\Users\\mansari\\University of Mississippi Medical Center\\Pramod Anugu - Exam 4\\Weekly Reports\\Other Reports\\Blood Pressure\\CARDIA EXAM X.docx"
    #         src = c(p1, p2)
    #         
    #         owd <- setwd(tempdir())
    #         on.exit(setwd(owd), add = TRUE)
    #         file.copy(from = src, to = c('report.Rmd', 'CARDIA EXAM X.docx'), overwrite = TRUE)
    #         
    #         rmarkdown::render('report.Rmd', output_file = file,
    #                           params = params,
    #                           envir = new.env(parent = globalenv())
    #         )
    #         
    #         
    #         
    #     }
    # )


}

# Run the application 
shinyApp(ui = ui, server = server)
