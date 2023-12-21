library(shiny)
# library(shinythemes)
# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", href = "https://use.fontawesome.com/releases/v5.8.2/css/all.css"),
            tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.3.1/css/bootstrap.min.css"),
            tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/mdbootstrap/4.8.11/css/mdb.min.css")),
  navbarPage("MyApp", 
             # theme = shinytheme("flatly"),
             tabPanel("Test")),
  tags$footer(HTML("
                    <!-- Footer -->
                           <footer class='page-footer font-large indigo'>
                           <!-- Copyright -->
                           <div class='footer-copyright text-center py-3'>© 2018 Copyright:
                           <a href='https://mdbootstrap.com/education/bootstrap/'> MDBootstrap.com</a>
                           </div>
                           <!-- Copyright -->

                           </footer>
                           <!-- Footer -->")))
# Define server logic required to draw a histogram
server <- function(input, output,session) {}
# Run the application 
shinyApp(ui = ui, server = server)