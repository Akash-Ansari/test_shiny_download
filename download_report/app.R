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
        
        filename = ("Reports"),
        content = function(file) {
            k <- (input$myTable1_rows_selected)
            fs <- c()
            for ( i in k) 
            {
                path <- paste0(v$data$Name[i],".doc")

                
                rmarkdown::render(here::here("download_report", "report.Rmd"), output_file = path,
                                  params = list(j=i),
                                  envir = new.env(parent = globalenv())
                )
                fs <- c(fs,path)
                
            }
            zip(file,fs)
        }
    )


}

# Run the application 
shinyApp(ui = ui, server = server)
