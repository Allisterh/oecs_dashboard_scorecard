
# sourcetools::
source('./packages.R', local = T , echo = F)
source('./global.R')
source('./www/css.R')
options(dplyr.summarise.inform = FALSE)
light <- bs_theme()
dark <- bs_theme(bg = "black", fg = "white", primary = "purple")


ui <- 
       navbarPage(
                 titlePanel(title = span(img(src = "oecs-logo.png", height = 20), "OECS Development Monitor")),
                 id="main",
                 collapsible = T, position = "fixed-top",
                 selected = "Scorecard", 
                 # theme = bs_theme(bootswatch = "lumen"),
                 # theme=bs_theme(version = 4, bootswatch = "default"),
                 # checkboxInput("dark_mode", "Dark mode"),
                 footer = HTML("
                    <!-- Footer -->
                           <footer class='page-footer font-large indigo'>
                           <!-- Copyright -->
                           <div class='footer-copyright text-center py-3'>© 2023 Copyright:
                           <a href='https://www.oecs.org/en/'> OECS</a>
                           </div>
                           <!-- Copyright -->

                           </footer>
                           <!-- Footer -->"),
                 header = tagList(useShinydashboard(),
                                  use_gotop(),
                                  use_prompt(),
                                  # useShinyalert(),
                                  use_font("roboto", "www/css/oswald.css"),
                                  shinybusy::add_busy_spinner(spin = "self-building-square",
                                                   position = 'full-page'),
                                  use_cicerone() # include dependencies
                                  ),
                 tags$head(tags$style(css)),
                 
               
            
                 
                 tabPanel("Scorecard", icon = icon('bar-chart'),
                          fluidRow(
                            column(12,  class="overlay", height="100px",
                                   HTML("
                                                <div class='col-sm-5' style='border-right:solid 2px; margin-top:40px; margin-bottom:10px'>
                                                  <center>
                                                    <h1><b><img src='oecs-logo.png' style='border-radius:50%; margin-right:5px;' width='30px' height='30px'>OECS Scorecard </b></h1>
                                                    <h3> Where is the OECS today? </h3> <br>
                                                    <img src='flag-of-monserrat.gif' style='border-radius:50%; margin-right:5px; ' width='30px' height='30px'>
                                                    <img src='flag-of-dominica.gif' style='border-radius:50%; margin-right:5px;' width='30px' height='30px'>
                                                    <img src='flag-of-grenada.gif' style='border-radius:50%; margin-right:5px;' width='30px' height='30px'>
                                                    <img src='flag-of-stkitts_nevis.gif' style='border-radius:50%; margin-right:5px;' width='30px' height='30px'>
                                                    <img src='flag-of-Antigua.gif' style='border-radius:50%; margin-right:5px;' width='30px' height='30px'>
                                                    <img src='flag-of-St.-Lucia.gif' style='border-radius:50%; margin-right:5px;' width='30px' height='30px'>
                                                    <img src='flag-of-st.vincent-the-Grenadiness.gif' style='border-radius:50%; margin-right:5px;' width='30px' height='30px'>
                                                    <br>
                                                    <br>
                                                    <hr style='width:60%; margin:0 auto;border-top:2px solid;'>
                                                    <br>
                                                    <b style='display:inline-block;vertical-align:top'>Designed by:</b> <img src='logo_OECD_DEV_en-transparent.png' width='70px' height='40px'>
                                                  </center>
                                                </div> 
                                                <div class='col-sm-6' style='margin-left:50px'>
                                                  <br>
                                                  <br>
                                                  <b style='font-size:24px'>Welcome to the OECS Scorecard!</b><br><br>
                                                  This Scorecard tracks performance towards goals outlined in the Organisation of Eastern Caribbean State's (OECS) regional 
                                                  <a href='https://oecs.org/en/oecs-development-strategy'>Development Strategy</a> (ODS).
                                                  It is organized according to strategy's three pillars: Economy, Environment and Social, with a fourth category for indicators with limited data availablity.
                                                  Altogether, 40 indicators were selected to track progress towards the ODS's goals, with each of the member states providing indicator-specific targets that are 
                                                  tailored to their country's circumstances and ambitions. 
                                                  For each indicator we track progress from 2020, or latest available, until 2030 with a country-by-country breakdown available when clicking the <b>View more </b>button.
                                                </div>
                                               ")
                            ),
          #=========Start formmatable below the verbiage=======================
                            fluidRow(align="center", class="overlay2", style="margin:0px",
                                     
          # First insert the action button for data upload file   
                                     column(12,
                                            br(),
                                            column(4),
                                            column(4,
                                                   br(),
                                                   # could be removed given data process
                                    fileInput("newVals", "Upload new data",
                                                    multiple = FALSE, 
                                                    accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                                              ".csv"))%>% 
                                                
                                                  shinyhelper::helper(type = "inline",
                                                            title = "Inline Help",
                                                            content = c("Upload new data here!",
                                                                        "Only accepts CSV files!",
                                                                        "This is some <b>HTML</b>."),
                                                            size = "s"),
                                                   
                                                   # not used, but for some reason is integral to the app
                                                   # hidden using css
                                                   selectInput("growthChoice", label="Choose OECS growth rate",
                                                               choices = 1,
                                                               selected = 1)
                                            ), 
                                            column(4)
                                     ) # end of column 12
                            ), # end of fluid row
      #===============================================================
                            fluidRow(
                # shiny::helpText("Note: while the data view will show only",
                #                 "the specified number of observations, the",
                #                 "summary will be based on the full dataset."),
                              
                              column(12, align="center", style='padding-left: 25px; padding-right:25px;',
                                     br(),
                                     
                                     column(12,
                                            br(),
                                            column(4),
                                            column(4,
                                                   br(),
                                                   # could be removed given data process
                                                   actionBttn(
                                                     inputId = "bttn2",
                                                     label = "How to use the table!",
                                                     color = "royal",
                                                     style = "jelly",
                                                     icon = icon("info"),
                                                     block = TRUE
                                                   )
                                            )),
                                     
                                     
                                     fluidRow(align="center", 
                                              h2(style="font-weight:bold;","Economic indicators")),
                                     fluidRow(
                                       column(12, style='padding-right:50px;padding-left:50px; overflow-x:scroll;',
                                              br(),
                                              # actionButton(
                                              #   "btn_pop", 
                                              #   "Click here for popover"
                                              # ), 
                                                
                                              # bslib::popover(
                                              #     "Popover message",
                                              #     title = "Popover title"
                                              #   ),     
                                        shinytip::tip(shiny::plotOutput("fullAvgEcon", height=200),
                                                      "Average for OECS countries across all economic indicators.
                                                      Click the view more button to see country charts.", position = 'bottom',
                                                      animate = TRUE,
                                                      click = FALSE),
                                        
                                  HTML("<span style='font-size:10px'> Black line marks the starting point (2020)</span>"),
                                           
                                              br(),
                                              br(),
                                              formattableOutput("tableEcon") %>% 
                                                shinycssloaders::withSpinner(),
                                       )),
            #================================================================
                                     hr(),
                                     fluidRow(align="center",
                                              h2(style="font-weight:bold;", "Environmental indicators")),
                                     fluidRow(
                                       column(12, style='padding-right:50px;padding-left:50px; overflow-x:scroll;',
                                              br(),
                                              plotOutput("fullAvgEnvi", height=200) |> 
                                              
                                              prompter::add_prompt(
                                                message = "Gauge plot OECS average. For country charts click the view more button
                                                in the table",
                                                position = "bottom", type = "info", 
                                                size = "medium", rounded = TRUE,
                                                bounce = T
                                              ),
                                              
                                              HTML("<span style='font-size:10px'> Black line marks the starting point (2020)</span>"),
                                              br(),
                                              br(),
                                              htmlOutput("tableEnvi"),
                                       )),
              #=====================================================
                                     hr(),
                                     fluidRow(align="center", 
                                              h2(style="font-weight:bold;", "Social indicators")),
                                     fluidRow(
                                       column(12, style='padding-right:50px;padding-left:50px; overflow-x:scroll;',
                                              br(),
                                              plotOutput("fullAvgSocial", height=200) |> 
                                                
                                                prompter::add_prompt(
                                                  message = "Gauge plot OECS average. For country charts click the view more button
                                                in the table",
                                                  position = "bottom", type = "info", 
                                                  size = "medium", rounded = TRUE,
                                                  bounce = TRUE
                                                ),
                                              
                                              HTML("<span style='font-size:10px'> Black line marks the starting point (2020)</span>"),
                                              br(),
                                              br(),
                                              htmlOutput("tableSocial"),
                                       )),
              #=================================================================
                                     hr(),
                                     fluidRow(align="center",
                                              h2(style="font-weight:bold;", "Important indicators but with insufficient data")),
                                     column(12, style='padding-right:50px;padding-left:50px; overflow-x:scroll;',
                                            br(),
                                            br(),
                                            htmlOutput("tableInsuff"),
                                     ),
                                     hr(),
                                     br(), br()
                                     
                              )
                            )
                          )
                          
                 ), # end of tab 1
                 #======== A hidden tab reacts to changes on the server side=====
                 tabPanel(" ",
                          fluidRow(style="padding-top:50px",
                                   fluidRow(
                                     column(12,
                                            br(),
          # All of these are at the top of the page for the hidden tab=====
                                            
                                            column(1, align="center", 
                                                   style="padding: 15px 0px 0px 0px",
                                                   uiOutput("varSDG")),
                                            
                                            column(9, align="left", 
                                                   style="padding-left:0px",
                                                   column(12, 
                                                          uiOutput("varName"), 
                                                          uiOutput("goalTextTitle"),
                                                          uiOutput("sourceTextTitle")
                                                   )),
                                            
                                            column(2, align="center", 
                                                   br(),
                                                   uiOutput("returnButton")))
                                   ),
            #===========Here starts the charts starting with the line chart=====
                                   fluidRow(
                                     hr(),
                                     column(9, align="center", style='padding-left:20px',
                                            plotOutput("projectionPlot") |> 
                                              
                                              # tooltip("Tooltip message")
                                              shinyhelper::helper(type = "inline",
                                                     icon = 'info-circle',
                                                     title = "projectionPlot",
                                                     size = 'l',
                                                     buttonLabel = 'Close',
                                                     fade = TRUE,
                                                     content = c("<b>Dashed Lines represent forecast values!</b>."))
                                              
                                              

                                            ), #end column,
                                     
                # Here starts the gauge charts for each country by variable/indicator 
                                     column(3, align="left", style="padding-top: 50px",
               
                                            plotOutput("gaugeGoalAverage"),
                                            fluidRow(align="center",
                                                     uiOutput("gaugeLabelAverage"),
                                                     column(6,
                                                            htmlOutput("currentText"),
                                                            htmlOutput("startPointOECS")
                                                     ),
                                                     column(6,
                                                            htmlOutput("goalText"),
                                                            htmlOutput("growthVal")
                                                     ))
                                     ), # end of column 3 
                                   ), # end of the fluidrow
                                   #=======================================================
                                   fluidRow(
                                     column(12, 
                                            br(),
                                            fluidRow(
                                              column(3, align="center",
                                                     plotOutput("atgGauge"),
                                                     uiOutput("atgLabel"),
                                                     column(1),
                                                     column(5,
                                                            htmlOutput("atgCurrent"),
                                                            htmlOutput("startPointATG")
                                                     ),
                                                     column(5,
                                                            htmlOutput("atgGoal"),
                                                            htmlOutput("growthValATG")
                                                     ),
                                                     column(1),
                                                     br()),
                                              column(3, align="center",
                                                     plotOutput("dmaGauge", height=125),
                                                     uiOutput("dmaLabel"),
                                                     column(6,
                                                            htmlOutput("dmaCurrent"),
                                                            htmlOutput("startPointDMA")
                                                     ),
                                                     column(6,
                                                            htmlOutput("dmaGoal"),
                                                            htmlOutput("growthValDMA")
                                                     ),
                                                     br()),
                                              column(3, align="center",
                                                     plotOutput("msrGauge", height=125),
                                                     uiOutput("msrLabel"),
                                                     column(6,
                                                            htmlOutput("msrCurrent"),
                                                            htmlOutput("startPointMSR")
                                                     ),
                                                     column(6,
                                                            htmlOutput("msrGoal"),
                                                            htmlOutput("growthValMSR")
                                                     ),
                                                     
                                                     br()),
                                              column(3, align="center",
                                                     plotOutput("grdGauge", height = 125),
                                                     uiOutput("grdLabel"),
                                                     column(6,
                                                            htmlOutput("grdCurrent"),
                                                            htmlOutput("startPointGRD")
                                                     ),
                                                     column(6,
                                                            htmlOutput("grdGoal"),
                                                            htmlOutput("growthValGRD")
                                                     ),
                                                     
                                                     br())
                                            ),
                                            column(4, align="center",
                                                   plotOutput("lcaGauge", height=125),
                                                   uiOutput("lcaLabel"),
                                                   column(6,
                                                          htmlOutput("lcaCurrent"),
                                                          htmlOutput("startPointLCA")
                                                   ),
                                                   column(6,
                                                          htmlOutput("lcaGoal"),
                                                          htmlOutput("growthValLCA")
                                                   )
                                                   
                                            ),
                                            column(4, align="center",
                                                   plotOutput("knaGauge", height=125),
                                                   uiOutput("knaLabel"),
                                                   column(6,
                                                          htmlOutput("knaCurrent"),
                                                          htmlOutput("startPointKNA")
                                                   ),
                                                   column(6,
                                                          htmlOutput("knaGoal"),
                                                          htmlOutput("growthValKNA")
                                                   )
                                            ),
                                            column(4, align="center",
                                                   plotOutput("vctGauge", height=125),
                                                   uiOutput("vctLabel"),
                                                   column(6,
                                                          htmlOutput("vctCurrent"),
                                                          htmlOutput("startPointVCT")
                                                   ),
                                                   column(6,
                                                          htmlOutput("vctGoal"),
                                                          htmlOutput("growthValVCT")
                                                   )
                                            )
                                     )
                                   ), # end of fluidrow
                                   column(2,
                                          br(),
                                          column(6, 
                                                 align="left",
                                                 uiOutput("panelLeft2")
                                          ),
                                          br(),
                                          column(6,
                                                 align="center",
                                                 uiOutput("panelRight2")
                                          )
                                   ) #end of column for the next and previous buttons
                                   
                          ) # end of fluidrow next to hidden tab
        
                 ), #closes the hidden tab
      # Create a tab for the about us page
      tabPanel("About", icon = icon('question-circle'),
                 tags$h3("ABOUT THE DASHBOARD"),
               tags$br(), tags$br(),tags$br(),
               fluidRow(
                 column(12,  height="150px",
               
                HTML(
                  "<p><strong>The Organisation of Eastern Caribbean States (OECS)</strong> is an 
                  International Inter-governmental Organisation dedicated to 
                  regional integration in the Eastern Caribbean.  Regional 
                  Integration is when countries in close proximity who share 
                  physical or cultural characteristics with each other, come 
                  together to achieve common goals.  Some common goals would 
                  be to make trade easier with each other, share resources or solve problems together.  
                  For Example, the OECS is a Regional Grouping of 11 countries 
                  in the Eastern Caribbean. The OECS makes it easier for any 
                  citizen of the 7 Protocol Countries:  <strong>Antigua and Barbuda</strong>,  
                  <strong>Commonwealth of Dominica</strong>,  <strong>Grenada</strong>,  <strong>Montserrat</strong>,  
                  <strong>Saint Kitts and Nevis</strong>,<strong>Saint Lucia</strong> and <strong>Saint Vincent and the Grenadines</strong> 
                  to travel and live in any member state with a  valid form of 
                  ID like a Driver Licence, National Identification Card, 
                  Voters Registration Card, Social Security Card or Passport.
                  The vision of the organisation for 2020-2024 is 
                 <strong><q>A better quality of life for the people of the OECS</q></strong> and 
                  Mission Statement: <strong><q>To drive and support sustainable development 
                  through regional integration. collective action and development cooperation.</q></strong>
                  The Organisation of Eastern Caribbean States came into being on June 18th 1981, 
                  when seven Eastern Caribbean countries signed a treaty agreeing 
                  to cooperate and promote unity and solidarity among the Members. 
                  The Treaty became known as the Treaty of Basseterre, named in 
                  honour of the capital city of Saint Kitts and Nevis where it was signed.</p>"
                ),
                tags$br(),
                HTML("<strong><p>For more information on the OECS Development Strategy click the link below:</p><strong/>"),
                tags$a(href="https://oecs.org/en/oecs-development-strategy", "OECS Development Strategy!")
                 )),
                 tags$hr(),
               spsHr(
                 status = "info",
                 width = 1.5,
                 other_color = NULL,
                 type = "solid",
                 opacity = 1
               ),
               tags$br(),
                 fluidRow(
                   column(12,  height="100px",
                   includeHTML('www/about_us.html')),
                 # tags$a(href="https://www.eccb-centralbank.org/statistics/dashboard-datas/", "ECCB dashboard"),tags$br(),
                 
                 tags$br(),tags$br(),tags$br(),
                 
                 # tags$a(href="https://shiny.rstudio.com/", ""), tags$br(),tags$br(),
                 tags$div(
                 tags$h2("Hosted by:"),
                 HTML( "<h3>Eastern Caribbean Central Bank (ECCB)</h3>"),
                 tags$br(),tags$br(),tags$h2("Maintained by:"),
                 HTML( "<h3>Research Statistics Data Analytics Department ECCB</h3>"),
                 tags$br(),
                 # tags$a(href="mailto:allister.hodge@eccb-centralbank.org", "Email"), tags$br(),
                 tags$br()
               )))
          
) #close the nvabarpage and conclude the UI user interface



# Server side 
server <- function(input, output, session) {
  
  
# Start pop-up message
  
  shinyhelper::observe_helpers(withMathJax = TRUE)
  
  # showModal(modalDialog(
  #   title = "Welcome User",
  #   paste("Welcome to the OECS Development Scorecard dashboard!",
  #         "The dashboard presents data on the progress by OECS countries on meeting key development indicators",
  #         "for more information see the About Us tab."),
  #   easyClose = TRUE))
  
 
  # initialize then start the guide
   # guide$init()$start()

   
   
   # observe(session$setCurrentTheme(
   #   if (isTRUE(input$dark_mode)) dark else light
   # ))
  
  
  shinyalert(
    title = "Welcome",
    text = "Welcome to the OECS Development Scorecard dashboard!.
    The dashboard presents data on the progress by OECS countries on meeting key development indicators,
    for more information see the About Us tab.",
    size = "s", 
    closeOnEsc = FALSE,
    closeOnClickOutside = TRUE,
    html = FALSE,
    type = "success",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "Close",
    confirmButtonCol = "#93c83e",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  
  
  
  observeEvent(input$bttn2, {
    shinyalert(
      title = "How to use the table", type = "info",
      html = T,
      showCancelButton = TRUE,
      showConfirmButton = FALSE,
      cancelButtonText = "Close",
      confirmButtonCol = '#93c83e',
      immediate = T, 
      animation = TRUE,
      text = HTML("Click left on the <strong>View more button</strong> to see individual country data")
      
    )
  })
#=== collect & clean new data from github scraper===============================
  githubDatFull <- reactive({
    # !!!!!!! REPLACE THIS LINK WITH ECCB RUN SCRIPT !!!!!!!!!
    # new_dat <- read_csv("https://raw.githubusercontent.com/kate-chalmers/OECS-Scorecard-Scraper/main/data/updated_data.csv")
    new_dat <- read_rds('data/new_dat.RDS')
    return(new_dat)
  })
  
  githubDat <- reactive({
    
    new_dat <- githubDatFull()
    
    if(length(new_dat) > 0) {
      df_average <- new_dat %>%
        pivot_longer(!c(variable, iso3c), names_to="year") %>%
        mutate(year = parse_number(year)) %>%
        group_by(variable, year) %>%
        summarize(value = mean(value, na.rm=T)) %>%
        drop_na() %>%
        mutate(value = ifelse(!is.nan(value), round(value, digits=2), NA),
               value = ifelse(!is.nan(value),prettyNum(value, big.mark = ","), value)) %>%
        ungroup() %>%
        pivot_wider(names_from="year", values_from="value") 
      
    } else {
      df_average <- NULL
    }
    
    return(df_average)
    
  })
  
# frontpagebuilder is simply the formattable table and gauge charts 
# ================run front page builder function=========================
  
# here the observe function is waiting on the eventReactive to execute the code
  observe(frontPageTableBuilder() )
  
#======= start front page table builder function ===============================
  
  
# I don't get the sense of this by going over the the temporary dataframe only to
# go back and use the original
  frontPageTableBuilder <- eventReactive(c(input$growthChoice,input$newVals), {
    
    df_average <- githubDat() 

    if(!is.null(df_average)) {
      
      yearValues <- which(colnames(formattableListFin) %in% colnames(df_average))
      selector_list <- colnames(formattableListFin)[yearValues]
      
      avgVal <- nrow(df_average)
      
      formattableListFinTemp <- data.frame()
      for(i in 1:avgVal) {
        for(selector in selector_list) {
          
          temp_average <- df_average[i,]
          
          if(!is.na(temp_average[[selector]])) {
            temp_average <- temp_average[,colSums(is.na(temp_average))<nrow(temp_average)]
            
            yearsIncluded <- colnames(temp_average[-1])
            yearSelector <- as.character(max(as.numeric(yearsIncluded)))
            
            val <- df_average[[selector]][i]
            val2 <- df_average[[yearSelector]][i]
            val2 <- parse_number(val2)
            
            df <- formattableListFin %>%
              filter(Indicator == df_average$variable[i]) %>%
              mutate(!!selector := ifelse(!!selector == "-", NA, NA)) %>%
              mutate(!!selector := val) %>%
              mutate(val3 = parse_number(as.character(`Baseline target (5% growth)`))) %>%
              mutate(`Current score (0 to 100)` = ifelse(`Desired direction` == 1, round((val3/val2) * 100, digits=1),
                                                        round((val2/val3) * 100, digits=1))) %>%
              select(-val3)
            
            df <- df[c("Indicator", "gr", selector, "Current score (0 to 100)")]
            
            df <- df %>% pivot_longer(!c(Indicator, gr, `Current score (0 to 100)`), names_to="year") %>% mutate(year = as.numeric(year))
            
            formattableListFinTemp <- rbind(formattableListFinTemp, df)
          }
        }
      }
       
      formattableListFinTemp <- formattableListFinTemp %>% 
        group_by(Indicator) %>%
        tidyr::complete(year = 2021:2030) %>%
        pivot_wider(names_from="year", values_from="value")
      
      formattableListFinTemp <- formattableListFin %>%
        filter(Indicator %in% df_average$variable) %>%
        select(-`2021`:-`2030`) %>%
        rename("Starting score (0 to 100)" = "Target score (0 to 100)") %>%
        merge(., formattableListFinTemp, by = c("Indicator", "gr"))
      
      formattableListFin <- formattableListFin %>%
        mutate("Starting score (0 to 100)" = `Target score (0 to 100)`) %>%
        rename("Current score (0 to 100)" = "Target score (0 to 100)") %>%
        filter(!Indicator %in% df_average$variable) %>%
        rbind(., formattableListFinTemp) %>%
        replace(is.na(.), "-")
      
      formattableListFin <- formattableListFin[order(match(formattableListFin$Indicator,c(var.list2[[1]], 
                            var.list2[[2]], var.list2[[3]], var.list2[[4]]))),]
      
      formattableListFin <- formattableListFin %>% 
        select(-`2nd Target`) %>% 
        rename("Target" = "Baseline target (5% growth)") %>%
        mutate(Target = prettyNum(as.numeric(Target), big.mark = ",")) %>%
        relocate(`Starting score (0 to 100)`, .before="Current score (0 to 100)") %>%
        select(-`Starting score (0 to 100)`) %>%
        rename("Target score (0 to 100)" = "Current score (0 to 100)")

    } else {
      
      formattableListFin <- formattableListFin %>% 
        select(-`2nd Target`) %>% 
        rename("Target" = "Baseline target (5% growth)") %>%
        mutate(Target = base::prettyNum(as.numeric(Target), big.mark = ","))

    }

    
    # Select the broad indicators here i.e. economic, environmental, social and
    # others
    formattableEcon <- formattableListFin %>% filter(Indicator %in% var.list2[[1]] & gr == 0.05) %>% select(-gr)
    formattableEnvi <- formattableListFin %>% filter(Indicator %in% var.list2[[2]] & gr == 0.05) %>% select(-gr)
    formattableSocial <- formattableListFin %>% filter(Indicator %in% var.list2[[3]] & gr == 0.05) %>% select(-gr)
    formattableInsuff <- formattableListFin %>% filter(Indicator %in% var.list2[[4]] & gr == 0.05) %>% select(-gr)

    
#===== Build formattable =========================================================

# The buttons and image come directly from the formattableListFin data frame
#==========Economics =======================================================    
 
    output$tableEcon <- renderFormattable({
      p <- formattable(formattableEcon, align = c("c", "l", "l", rep("c", NCOL(formattableEcon) - 3)),
                       list(`Target score (0 to 100)` = improvement_formatter,
                            Indicator = styler, 
                            `<center>Data completeness (%)</center>` = color_bar("lightblue", fun = unit.scale),
                            "Desired direction" = formatter("span", 
                                  style = ~ style(color = ifelse(`Desired direction` == 0, "green", "red")),                                    
      ~ icontext(sapply(`Desired direction`, function(x) if (x == 1) "arrow-down" else if (x == 0) "arrow-up" else ""))))) 
      
      # as.htmlwidget() %>%
      # div() %>%
      # spk_add_deps()
      
      p
    })
    
#=======Environment============================================================
    output$tableEnvi <- renderUI({
      p <- formattable(formattableEnvi, align = c("c", "l", "l", rep("c", NCOL(formattableEnvi) - 3)),
                       list(`Target score (0 to 100)` = improvement_formatter,
                            `Indicator` = label_formatter,
                            `<center>Data completeness (%)</center>` = color_bar("lightblue", fun = unit.scale),
                            "Desired direction" = formatter("span", 
                                       style = ~ style(color = ifelse(`Desired direction` == 0, "green", "red")),                                    
                                      ~ icontext(sapply(`Desired direction`, function(x) if (x == 1) 
          "arrow-down" else if (x == 0) "arrow-up" else ""))))) %>%
        as.htmlwidget() %>%
        div() %>%
        spk_add_deps() 
      p
    })
    

#==Social Indicators========================================================
    
    # Social has headers, which are inserted into the center of each section
    row.val <- nrow(formattableSocial)
    formattableSocial <- rbind(formattableSocial, rep(" ", ncol(formattableSocial)), rep(" ", ncol(formattableSocial)), 
                               rep(" ", ncol(formattableSocial)), rep(" ", ncol(formattableSocial)),  
                               rep(" ", ncol(formattableSocial)))
    
    formattableSocial[row.val+1,round(ncol(formattableSocial)/2)-1] <- "<b>Labour</b>"
    formattableSocial[row.val+2,round(ncol(formattableSocial)/2)-1] <- "<b>Safety</b>"
    formattableSocial[row.val+3,round(ncol(formattableSocial)/2)-1] <- "<b>Health</b>"
    formattableSocial[row.val+4,round(ncol(formattableSocial)/2)-1] <- "<b>Education</b>"
    
    formattableSocial <- formattableSocial[c(row.val+1, 1, row.val+2, 2:3, row.val+3, 4:9, row.val+4, 10:row.val),]
    
    rownames(formattableSocial) <- c()
    
    output$tableSocial <- renderUI({
      p <- formattable(formattableSocial, align = c("c", "l", "l", rep("c", NCOL(formattableSocial) - 3)),
                       list(`Target score (0 to 100)` = improvement_formatter,
                            # `Current score (0 to 100)` = improvement_formatter,
                            `Indicator` = label_formatter,
                            `<center>Data completeness (%)</center>` = color_bar("lightblue", fun = unit.scale),
                            "Desired direction" = formatter("span", 
                                        style = ~ style(color = ifelse(`Desired direction` == 0, "green", "red")),                                    
                                      ~ icontext(sapply(`Desired direction`, function(x) if (x == 1) "arrow-down" 
                                        else if (x == 0) "arrow-up" else ""))))) %>%
        as.htmlwidget() %>%
        div() %>%
        spk_add_deps() 
      
      p
    })
    
    

#===Data with insufficient observations=========================================
    
    row.val2 <- nrow(formattableInsuff)
    formattableInsuff <- rbind(formattableInsuff, rep(" ", ncol(formattableInsuff)), 
                               rep(" ", ncol(formattableInsuff)), 
                               rep(" ", ncol(formattableInsuff)), rep(" ", 
                                  ncol(formattableInsuff)), 
                               rep(" ", ncol(formattableInsuff)))
    
    formattableInsuff <- formattableInsuff[order(
      match(formattableInsuff$Indicator,c(var.list2[[4]]))),]
    
    formattableInsuff[row.val2+1,round(ncol(formattableInsuff)/2)-1] <- "<b>Productivity</b>"
    formattableInsuff[row.val2+2,round(ncol(formattableInsuff)/2)-1] <- "<b>Labour</b>"
    formattableInsuff[row.val2+3,round(ncol(formattableInsuff)/2)-1] <- "<b>Poverty</b>"
    formattableInsuff[row.val2+4,round(ncol(formattableInsuff)/2)-1] <- "<b>Health</b>"
    formattableInsuff[row.val2+5,round(ncol(formattableInsuff)/2)-1] <- "<b>Education</b>"
    
    formattableInsuff <- formattableInsuff[c(row.val2+1, 1, row.val2+2, 2:5, 
                                             row.val2+3, 6, row.val2+4, 7:8, 
                                             row.val2+5, 9:row.val2),]
    rownames(formattableInsuff) <- c()
    
    formattableInsuff <- formattableInsuff %>% mutate(SDG = ifelse(is.na(SDG), " ", SDG))
    
    output$tableInsuff <- renderUI({
      p <- formattable(formattableInsuff, align = c("c", "l", "l", rep("c", NCOL(formattableInsuff) - 3)),
                       list(`Target score (0 to 100)` = improvement_formatter,
                            # `Current score (0 to 100)` = improvement_formatter,
                            `Indicator` = label_formatter,
                            `<center>Data completeness (%)</center>` = color_bar("lightblue", fun = unit.scale),
                            "Desired direction" = formatter("span", 
                      style = ~ style(color = ifelse(`Desired direction` == 0, "green", "red")),                                    
                     ~ icontext(sapply(`Desired direction`, function(x) if (x == 1) "arrow-down" 
                                       else if (x == 0) "arrow-up" else ""))))) %>%
        as.htmlwidget() %>%
        div() %>%
        spk_add_deps() 
      
      p
    })
    
#====================Average gauges===============================================
   
    
# Average value economic indicators
     avgValEcon <- formattableListFin %>% 
      filter(Indicator %in% var.list2[[1]] & gr == 0.05) %>% 
      summarize(avgVal = round(mean(`Target score (0 to 100)`), digits=1), iso3c="OECS") %>% 
      mutate(label = ifelse(avgVal <= 25, "#ff6f69", ifelse(avgVal >= 75, "#88d8b0", "#ffcc5c")))
    
# Average values environmental
    avgValEnvi <- formattableListFin %>% 
      filter(Indicator %in% var.list2[[2]] & gr == 0.05) %>% 
      summarize(avgVal = round(mean(`Target score (0 to 100)`), digits=1), iso3c="OECS") %>% 
      mutate(label = ifelse(avgVal <= 25, "#ff6f69", ifelse(avgVal >= 75, "#88d8b0", "#ffcc5c")))
    
# Average values Social indicators
    avgValSocial <- formattableListFin %>% 
      filter(Indicator %in% var.list2[[3]] & gr == 0.05) %>% 
      summarize(avgVal = round(mean(`Target score (0 to 100)`), digits=1), iso3c="OECS") %>% 
      mutate(label = ifelse(avgVal <= 25, "#ff6f69", ifelse(avgVal >= 75, "#88d8b0", "#ffcc5c")))
    
    # Econ average gauge======================
    output$fullAvgEcon <- renderPlot({
      
      prev_value <- progress_dat %>% 
        filter(variable %in% var.list2[[1]]) %>% 
        summarize(prev_pct=mean(prev_pct, na.rm=T)) %>% 
        pull(prev_pct)
      
      avgValEcon %>%
        ggplot(aes(x=iso3c, y=avgVal)) +
        ggforce::geom_link(
          aes(
            x = iso3c,
            xend = iso3c,
            y = 0,
            yend = 100,
          ),
          color="lightgrey",
          alpha=0.5,
          linewidth = 22
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c, 
            xend = iso3c,
            y = 0,
            yend = avgVal,
            color = label,
          ),
          linewidth = 22
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c, 
            xend = iso3c,
            y = prev_value-1,
            yend = prev_value,
          ),
          color="black",
          linewidth = 22
        ) +
        # annotate("richtext", label="Starting point (2020)", x=0.2, y=50, vjust=-8.75, hjust=-0.15, 
        #          size=5, family="Roboto Condensed",
        #          label.color=NA, fill=NA) +
        annotate("richtext", label="<b style='color:darkgrey;font-size:22px'>OECS Economic Average</b>", 
                 x=0.2, y=50, size=4, family="Roboto Condensed",
                 label.color=NA, fill=NA) +
        annotate("richtext", label=paste0("<b>", avgValEcon$avgVal, "</b>"), x=0.45, y=50, size=20,
                 label.color=NA, fill=NA) +
        scale_y_continuous(limits = c(0, 198), expand = c(0, 0)) + 
        coord_polar(theta = "y", start=4.7, clip = "off") +
        theme_void() +
        scale_color_manual(values = avgValEcon$label) +
        guides(color="none") +
        theme(
          text = element_text(size=18, family= "Roboto Condensed", face="bold"),
          plot.title = element_text(size=12, hjust=0.05),
          axis.title=element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          plot.title.position = "plot") 
      
    }, height=400)
    
    # Environment average gauge==================
    output$fullAvgEnvi <- renderPlot({
      
      prev_value <- progress_dat %>% 
        filter(variable %in% var.list2[[2]]) %>% 
        summarize(prev_pct=mean(prev_pct,na.rm=T)) %>% 
        pull(prev_pct)
      
      avgValEnvi %>%
        ggplot(aes(x=iso3c, y=avgVal)) +
        ggforce::geom_link(
          aes(
            x = iso3c,
            xend = iso3c,
            y = 0,
            yend = 100,
          ),
          color="lightgrey",
          alpha=0.5,
          linewidth = 22
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c, 
            xend = iso3c,
            y = 0,
            yend = avgVal,
            color = label,
          ),
          linewidth = 22
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c, 
            xend = iso3c,
            y = prev_value-1,
            yend = prev_value,
          ),
          color="black",
          linewidth = 22
        ) +
        annotate("richtext", label="<b style='color:darkgrey;font-size:22px'>OECS Environment Average</b>", 
                 x=0.2, y=50, size=4, family="Roboto Condensed",
                 label.color=NA, fill=NA) +
        annotate("richtext", label=paste0("<b>",avgValEnvi$avgVal,"</b>"), x=0.45, 
                 y=50, size=20, label.color=NA, fill=NA) +
        scale_y_continuous(limits = c(0, 198), expand = c(0, 0)) + 
        coord_polar(theta = "y", start=4.7, clip = "off") +
        theme_void() +
        scale_color_manual(values = avgValEnvi$label) +
        guides(color="none") +
        theme(
          text = element_text(size=18, family= "Roboto Condensed", face="bold"),
          plot.title = element_text(size=12, hjust=0.05),
          axis.title=element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          plot.title.position = "plot") 
      
      
    }, height=400)
    
    # Social average gauge============
    output$fullAvgSocial <- renderPlot({
      
      prev_value <- progress_dat %>% 
        filter(variable %in% var.list2[[3]]) %>% 
        summarize(prev_pct=mean(prev_pct,na.rm=T)) %>% 
        pull(prev_pct)
      
      avgValSocial %>%
        ggplot(aes(x=iso3c, y=avgVal)) +
        ggforce::geom_link(
          aes(
            x = iso3c,
            xend = iso3c,
            y = 0,
            yend = 100,
          ),
          color="lightgrey",
          alpha=0.5,
          linewidth = 22
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c, 
            xend = iso3c,
            y = 0,
            yend = avgVal,
            color = label,
          ),
          linewidth = 22
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c, 
            xend = iso3c,
            y = prev_value-1,
            yend = prev_value,
          ),
          color="black",
          linewidth = 22
        ) +
        annotate("richtext", label="<b style='color:darkgrey;font-size:22px'>OECS Social Average</b>",
                 x=0.2, y=50, size=4, family="Roboto Condensed",
                 label.color=NA, fill=NA) +
        annotate("richtext", label=paste0("<b>",avgValSocial$avgVal, "</b>"), 
                 x=0.45, y=50, size=20,
                 label.color=NA, fill=NA) +
        scale_y_continuous(limits = c(0, 198), expand = c(0, 0)) + 
        coord_polar(theta = "y", start=4.7, clip = "off") +
        theme_void() +
        scale_color_manual(values = avgValSocial$label) +
        guides(color="none") +
        theme(
          text = element_text(size=18, family= "Roboto Condensed", face="bold"),
          plot.title = element_text(size=12, hjust=0.05),
          axis.title=element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          plot.title.position = "plot") 
      
    }, height=400)
    
    print("End formattable")
    
  }) #End formattable
  


#=================== Rendering page============================================
  
  # Again here the the observe function is waiting on the pagebuilder eventReactive
  # to do something, in tis case switching between the next and previous buttons
  observe({ pageBuilderTabbed() })
  
  
  #=============Updates gauge page based on scrolling============================
  
  # first time the select_button is featured
  # the select_button is taking directly from the formattableListFin data frame column 22
  # the observeEvent is a eager responder since evey time the select_button changes it restimates the code
  # take the value of input$select_button, e.g. "button_1"
  # get the button number (1) and assign to selectedRow
  observeEvent(input$select_button, {
    # Could have done lapply or map
    # 1)
    var_list <- c(as.character(var.list2[[1]]), as.character(var.list2[[2]]), 
                  as.character(var.list2[[3]]), as.character(var.list2[[4]]))
    
    # 2)
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    
    # 3)

    var_name <- var_list[selectedRow]
    
    # var_list[2]
    # [1] "GDP per capita (current EC$)"
    # 4)
    # select variables
    selectedVar <- averageGaugeList %>% 
      filter(variable == var_name)
    
    # 5)
    # the labels for the data country
    label_list <- countryGaugeList %>% 
      filter(variable == var_name) %>% 
      select(iso3c, label)
    
    val <- pageBuilder(var_name, selectedVar, selectedRow, var.list, label_list)
    val
    
  })
  
  
  #=======Allows for scrolling between gauge pages within app=====================
  # in other words next and previous button in the app to forward between indicators 
  
  # since this is reactive it changes every time the user clicks the next or 
  # previous buttons
  pageBuilderTabbed <- eventReactive(c(input$to_next, input$to_prev), {
    
    var_list <- c(as.character(var.list2[[1]]), as.character(var.list2[[2]]), 
                  as.character(var.list2[[3]]), as.character(var.list2[[4]]))
    
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    
    if (!is.null(input$to_prev)) {
      prevClicks <- as.numeric(input$to_prev)
      selectedRow <- selectedRow - prevClicks 
    }
    if (!is.null(input$to_next)) {
      nextClicks <- as.numeric(input$to_next) #transforms input to numeric pages 
      selectedRow <- selectedRow + nextClicks
    }
    
    if(selectedRow < 1) {
      selectedRow <- 1 
      prevClicks <- prevClicks - 1
    } 
    if(selectedRow >= length(var_list)) {
      selectedRow <- length(var_list)
    } 
    
    var_name <- var_list[selectedRow]
    
    # the average gauge list pulls in data for the oecs average
    selectedVar <- averageGaugeList %>% 
      filter(variable == var_name)
    
    # the country gauge list pulls in data for the countries respectively
    label_list <- countryGaugeList %>% 
      filter(variable == var_name) %>% 
      select(iso3c, label)
    
    val <- pageBuilder(var_name, selectedVar, valueClick, var_list, label_list)
    val
    
  })
  
  
  #===Creates ggplot upon tab selection line charts for each variable============
  
  # The logic behind this is that the observeEvent is looking at when a click 
  # on the next or previous buttons are pressed to skip through the pages
  
  
  # This basically updates the line chart based on the indicator chosen!
  
  observeEvent(c(input$select_button, input$to_prev, input$to_next), {
    
    var_list <- c(as.character(var.list2[[1]]), as.character(var.list2[[2]]), 
                  as.character(var.list2[[3]]), as.character(var.list2[[4]]))
    
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    
    if (!is.null(input$to_prev)) {
      prevClicks <- as.numeric(input$to_prev)
      selectedRow <- selectedRow - prevClicks 
    }
    if (!is.null(input$to_next)) {
      nextClicks <- as.numeric(input$to_next)
      selectedRow <- selectedRow + nextClicks
    }
    
    if(selectedRow < 1) {
      selectedRow <- 1 
      prevClicks <- prevClicks - 1
    } 
    if(selectedRow >= length(var_list)) {
      selectedRow <- length(var_list)
    } 
    
    var_name <- var_list[selectedRow]
    
    # pulls data for the line charts in the pages
    available_country <- projectionValues %>%
      filter(variable == var_name & growthrate == "Current performance") %>%
      distinct(country) %>% 
      pull(country) # grab the columns for only the countries
    
    
    # filter the data for the countries
    filtered_df <- projectionValues %>%
      filter(gr == 0.05 & variable == var_name & 
               growthrate %in% c("GDP targeting rate")) %>%
      filter(!country == "Anguilla") %>%
      filter(country %in% available_country)
    
    filtered_temp <- projectionValues %>%
      filter(gr == 0.05 & variable == var_name & 
               growthrate %in% c("Country selected"))
    
    filtered_df <- filtered_df %>%
      anti_join(., filtered_temp[,c("variable", "country")], by = c("variable", "country")) %>%
      rbind(filtered_temp) 
    
    filtered_df <- projectionValues %>%
      filter(gr == 0.05 & variable == var_name & 
               growthrate %in% c("Current performance")) %>%
      rbind(filtered_df, .) %>%
      arrange(country, year) %>%
      mutate(growthrate = ifelse(!growthrate == "Current performance", 
                                 "GDP targeting rate", growthrate))
    
    new_dat <- githubDatFull()
    
    new_vals <- new_dat %>% 
      filter(variable == var_name) 
    
    if(nrow(new_vals) > 0) {
      
      new_vals_tidy <- new_vals %>%
        filter(!iso3c == "AIA") %>%
        pivot_longer(!c(variable,iso3c), names_to = "year", 
                     names_transform = list(year=as.numeric)) %>%
        mutate(gr = 0.05, growthrate = "Current performance",
               country = countrycode(iso3c, "iso3c", "country.name", 
                                     custom_match = c("OECS" = "OECS"))) %>%
        select(-iso3c)
      
      country_new <- c(new_vals_tidy$country, "OECS")
      
      df_average <- githubDat()
      
      average_tidy <- df_average %>% 
        filter(variable == var_name) %>%
        pivot_longer(!variable, names_to="year", names_transform = list(year=as.numeric)) %>%
        mutate(value = parse_number(value), gr=0.05, growthrate = "Current performance",
               country = "OECS")
      
      new_vals_tidy <- rbind(new_vals_tidy, average_tidy)
      
      dup_years <- unique(new_vals_tidy$year)
      
      current_dat <- filtered_df %>% 
        filter(!year %in% dup_years) %>% 
        filter(country %in% country_new & growthrate == "Current performance")
      
      filtered_df_temp <- filtered_df %>% filter(!growthrate == "Current performance") %>%
        filter(country %in% country_new) %>%
        group_by(country, growthrate) %>%
        filter(year == max(year))
      
      filtered_df_temp <- new_vals_tidy %>%
        mutate(growthrate = "GDP targeting rate") %>%
        rbind(., filtered_df_temp) %>%
        group_by(country) %>%
        complete(year = min(year):max(year), variable=variable, 
                 gr=gr, growthrate=growthrate) %>%
        mutate(value=zoo::na.approx(value)) %>%
        ungroup()
      
      filtered_df <- filtered_df %>% filter(!country %in% country_new) 
      
      filtered_df <- rbind(filtered_df, filtered_df_temp, current_dat, new_vals_tidy) %>%
        arrange(country, year, growthrate) %>% 
        drop_na()
    }
    
    df_region <- filtered_df %>% filter(country == "OECS")
    
    df_country <- filtered_df %>% filter(!country == "OECS") 
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
    } else if (var_name %in% units[[2]]) {
      unit_val <- ""
    } else {
      unit_val <- "%"
    }
    
    max_region <- df_region %>%
      filter(growthrate == "GDP targeting rate") %>%
      filter(year == max(year)) %>%
      mutate(country = ifelse(variable %in% units[[1]],
                              paste0("<b>OECS: ", unit_val, prettyNum(format(round(value, 2),nsmall=2), big.mark=","), "</b>"),
                              paste0("<b>OECS: ", prettyNum(format(round(value, 2),nsmall=2), big.mark=","), unit_val, "</b>")))
    
    min_year <- filtered_df %>% 
      filter(year == min(year)) %>% 
      pull(year)
    
    min_year <- min(min_year) - 1
    seg_year <- round(min_year + 5, -1)
    
    flags <- df_country %>%
      filter(growthrate == "Current performance") %>%
      group_by(country) %>%
      filter(year == max(year)) %>%
      ungroup() %>%
      mutate(iso2c = tolower(countrycode(country, "country.name", "iso2c"))) %>%
      mutate(country2 = ifelse(variable %in% units[[1]],
                               paste0("<b>", country, "<span style='color:black'>", unit_val, prettyNum(format(round(value, 2),nsmall=2), big.mark=","), "</span></b>"),
                               paste0("<b>", country, "<span style='color:black'>", prettyNum(format(round(value, 2),nsmall=2), big.mark=","), unit_val, "</span></b>")))
    
    flags2 <- df_country %>%
      rbind(., df_region) %>%
      filter(growthrate == "GDP targeting rate") %>%
      group_by(country) %>%
      filter(year == max(year)) %>%
      ungroup() %>%
      mutate(iso2c = tolower(countrycode(country, "country.name", "iso2c"))) %>%
      mutate(country2 = ifelse(variable %in% units[[1]],
                               paste0(unit_val, prettyNum(format(round(value, 2), nsmall=2), big.mark=",")),
                               paste0(prettyNum(format(round(value, 2),nsmall=2), big.mark=","), unit_val)),
             country2 = paste0(country, ": ", country2)) 
    
    if(var_name == "Renewable electricity output (% of total electricity output)") {
      flags2 <- flags2 %>%
        mutate(country2 = ifelse(iso2c == "kn", paste0(country2, " \n(Official goal: 100%)"), 
                                 ifelse(iso2c == "vc", paste0(country2, " \n(Official goal: 60%)"), country2)))
    }
    
    # temp <- filtered_df %>% drop_na() %>% filter(value == max(value)) %>% pull(value)
    # temp <- ceiling(max(temp))
    # temp <- round(temp, digits=-(nchar(temp) - 1))
    # temp2 <- temp/4
    # seg_df <- data.frame(value = seq(0, temp, by=temp2))
    # seg_df <- seg_df %>% filter(!value == 0)
    
    # Change the colors here allister
    group_colors <- c("Antigua & Barbuda" = "#F8766D",
                      "Dominica" = "#CD9600",
                      "Grenada" = "#7CAE00",
                      "Montserrat" = "#00BE67",
                      "St. Kitts & Nevis" = "#00BFC4",
                      "St. Lucia" = "#00A9FF",
                      "St. Vincent & Grenadines" = "#C77CFF",
                      "Anguilla" = "#FF61CC")
    
    region_name <- flags2 %>%
      filter(grepl("OECS", country)) %>%
      pull(country)
    
    pos <- which(names(group_colors) == "OECS")
    
    names(group_colors)[pos] <- region_name
    
    if(var_name == "Poverty headcount ratio at $5.50 a day (2011 PPP) (% of population)") {
      
      df_region <- df_region %>%
        mutate(temp = ifelse(year < 2008 & country == "OECS", 1, 0)) %>%
        filter(!temp == 1)
      
      temp <- df_country %>%
        filter(country=="St. Lucia" & growthrate=="Current performance") %>%
        complete(year = 1995:2016, country=country, growthrate=growthrate,
                 variable=variable, gr=gr) %>%
        mutate(value = zoo::na.approx(value))
      
      df_country <- df_country %>%
        filter(!country=="St. Lucia" | !growthrate=="Current performance") %>%
        rbind(temp, .) %>%
        arrange(country, year)
      
    }
    
    output$projectionPlot <- renderPlot({
      
      p <- filtered_df %>% 
        ggplot(aes(x = year, y = value)) +
        geom_line(data=filter(df_country, growthrate=="Current performance"), aes(color=country), alpha=0.8) +
        geom_line(data=filter(df_country, growthrate=="GDP targeting rate"), aes(color=country), alpha=0.8, linetype=2) +
        geom_line(data = filter(df_region, growthrate == "Current performance")) +
        geom_line(data= filter(df_region, growthrate=="GDP targeting rate"),linetype=2) +
        geom_text_repel(data=flags2, aes(label=country2, color=country, x=year-0.25),
                        family = "Roboto Condensed",
                        fontface = "bold",
                        max.overlaps = Inf,
                        size = 4.5,
                        direction = "y",
                        xlim = c(2031, NA),
                        hjust = 0,
                        segment.size = .5,
                        segment.alpha = 0.2,
                        segment.linetype = "dotted",
                        box.padding = .1,
                        segment.curvature = -0.5,
                        segment.ncp = 3,
                        segment.angle = 100,
                        segment.inflect   = TRUE
        )  +
        ggflags::geom_flag(data =flags, aes(country=iso2c, x=year, y=value*1.01), size = 6) +
        ggimage::geom_image(data=df_region %>% filter(growthrate=="Current performance") %>% 
                              filter(year==max(year)),
                            aes(image="./www/oecs-logo.png", x=year), asp=2.5, size=0.04, by="height") +
        labs(y = " ", x = " ") +
        ggtitle(var_name) +
        expand_limits(y = 0)+
        theme_light(base_family="Roboto Condensed") +
        coord_cartesian(clip = "off",
                        xlim = c(2000, 2036)) +
        scale_y_continuous(labels=scales::comma_format()) +
        scale_x_continuous(limits=c(1999,2032), breaks=c(2000, 2005, 2010, 2015, 2020, 2025, 2030)) +
        scale_color_manual(values=group_colors) +
        guides(color="none") +
        geom_segment(aes(x=2000,xend=2030,y=-Inf,yend=-Inf))+
        theme(plot.title.position = "plot",
              plot.title = element_text(size=12, face="italic"),
              plot.caption.position = "plot",
              legend.position = "bottom",
              text = element_text(size=18, family="Roboto Condensed"),
              legend.title = element_blank(),
              panel.background = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.border = element_blank(),
              axis.ticks = element_line(color="black", linewidth  =0.5),
              plot.caption = element_text(face="italic", size=6)
        )
      
      break_vals <- ggplot_build(p)$layout$panel_params[[1]][["y"]][["breaks"]]
      break_vals <- break_vals[!break_vals == 0]
      
      p + geom_segment(data=data.frame("value" = break_vals),
                       aes(y=value, x=1999, xend=2030, yend=value),
                       alpha=0.15, linewidth=0.2) 
      
      
    })
  }) # concludes observeEvent function
  
  

  #========================== Page builder function============================
  # this section does the gauge plot for the countries and  the OECS average
  # Definition of variables in the function
  # var_name - variable/indicator name
  # selectedVar= Variable/indicator selected
  # selectedRow = Row of the formattable selected
  
  pageBuilder <- function(var_name, selectedVar, selectedRow, var.list, 
                          label_list) {
    
    output$style_tag <- renderUI({
      if(var_name %in% var.list2[[4]])
        return(tags$head(tags$style(HTML('* {background-color:blue;}'))))
    })
    
    df_average <- githubDat() 
    
    new_avg <- df_average %>% 
      filter(variable == var_name)
    
    if(nrow(new_avg) > 0) {
      
      new_temp <- new_avg %>%
        pivot_longer(!variable, names_to="year", names_transform = list(year=as.numeric)) %>%
        filter(!is.na(value)) %>%
        filter(year == max(year))
      
      new_temp2 <- new_temp %>% select(variable, "current" = value)
      
      lower_val_temp <- oecs.long %>%
        filter(variable == var_name) %>%
        select(variable, lower_val_wanted) %>%
        distinct()
      
      
      # The average gauge dataframe matches data for the OECS average
      newAverageGaugeList <- averageGaugeList %>%
        filter(variable == var_name) %>%
        select(variable, goal) %>%
        merge(new_temp2, ., by="variable") %>%
        merge(., lower_val_temp) %>%
        mutate(current = parse_number(current), goal2 = parse_number(goal)) %>%
        mutate(gauge = ifelse(current < goal2 & lower_val_wanted == 0, current/goal2,
                              ifelse(current < goal2 & lower_val_wanted == 1, 1,
                                     ifelse(current > goal2 & lower_val_wanted == 0, 1, goal2/current)))) %>%
        mutate(gauge = format(round((gauge*100), 1), nsmall=1),
               current = prettyNum(round(current, 2), big.mark = ",")) %>%
        select(-lower_val_wanted, -goal2) 
      
      # cycle through the oecs data for each indicators
      averageGaugeList$gauge[averageGaugeList$variable == var_name] <- newAverageGaugeList$gauge
      
      # cycle through the oecs data for the current data
      averageGaugeList$current[averageGaugeList$variable == var_name] <- newAverageGaugeList$current
      
      selectedVar$current <- new_temp %>% pull(value)
      selectedVar$last_year <- new_temp %>% pull(year)
      
      # This selects indicator for which the OECS wants the values to be lower
      lower_val_temp <- oecs.long %>%
        filter(variable == var_name) %>%
        select(variable, lower_val_wanted) %>%
        distinct() %>% 
        pull(lower_val_wanted)
      
      gauge_temp <- ifelse(parse_number(selectedVar$current) < parse_number(selectedVar$goal) & 
                             lower_val_temp == 0, parse_number(selectedVar$current)/parse_number(selectedVar$goal),
                           ifelse(parse_number(selectedVar$current) < parse_number(selectedVar$goal) & 
                                    lower_val_temp == 1, 1,
                                  ifelse(parse_number(selectedVar$current) > parse_number(selectedVar$goal) & 
                                           lower_val_temp == 0, 1, parse_number(selectedVar$goal)/parse_number(selectedVar$current))))
      
      selectedVar$gauge <- ifelse(var_name == "Fish species, threatened", format(round((gauge_temp*100), 1), nsmall=1), 
                                  format(round((gauge_temp*100), 0)))
      
    } 
    
    
    prev_value <- five_year %>% 
      filter(variable == var_name & iso3c == "OECS") 
    
    #===============Gauge plot OECS=================================================
    avgGauge <- averageGaugeList %>% 
      filter(variable == var_name) %>% 
      mutate(iso3c = "OECS") %>% 
      select(iso3c, avgVal = "gauge") %>%
      mutate( avgVal = ifelse(as.numeric(avgVal) >= 100, 99, avgVal),
              label = ifelse(avgVal <= 25, "#ff6f69", ifelse(avgVal >= 75, "#88d8b0", "#ffcc5c")))
    
    # OECS average for particular year in this case 2022
    output$gaugeLabelAverage <- renderUI({
      HTML(paste0("<b style='font-size:20px'>OECS Average", "\n (", selectedVar$last_year, ")</b>"))
    })
    
    if(nrow(prev_value) < 1) {
      prev_value <- data.frame("variable" = var_name, "iso3c" = "OECS", prev_pct = 0)
    }
    
    output$gaugeGoalAverage <- renderPlot({
      
      avgGauge %>%
        mutate(avgVal = as.numeric(avgVal)) %>%
        ggplot(aes(x=iso3c, y=avgVal)) +
        ggforce::geom_link(
          aes(
            x = iso3c,
            xend = iso3c,
            y = 0,
            yend = 100,
          ),
          color="lightgrey",
          alpha=0.5,
          linewidth = 15
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c, 
            xend = iso3c,
            y = 0,
            yend = avgVal,
            color = label,
          ),
          linewidth = 15
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c, 
            xend = iso3c,
            y = prev_value$prev_pct,
            yend = prev_value$prev_pct+0.45,
          ),
          color="black",
          linewidth = 15
        ) +
        annotate("text", label=avgGauge$avgVal, x=0.45, y=50, size=20, vjust=0) +
        scale_y_continuous(limits = c(0, 198), expand = c(0, 0)) + 
        coord_polar(theta = "y", start=4.7, clip = "off") +
        theme_void() +
        scale_color_manual(values = avgGauge$label) +
        guides(color="none") +
        theme(
          text = element_text(size=18, family= "Roboto Condensed", face="bold"),
          plot.title = element_text(size=12, hjust=0.05),
          axis.title=element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          plot.title.position = "plot") 
      
    }, height=400)
    
    #============Calculate the Goal point for the goals ============================
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$goalText <- renderUI({
        HTML(paste0("<span style='font-size:16px'><i>Goal: </i>", unit_val, selectedVar$goal, "</span>"))
      })
      
    } else if (var_name %in% units[[2]]) {
      output$goalText <- renderUI({
        HTML(paste0("<span style='font-size:16px'><i>Goal: </i>", selectedVar$goal, "</span>"))
      })
      
    } else {
      unit_val <- "%"
      output$goalText <- renderUI({
        HTML(paste0("<span style='font-size:16px'><i>Goal: </i>", selectedVar$goal, unit_val, "</span>"))
      })
    }
    
    
    
    #=============Calculate the Current point for the goals ====================================================
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$currentText <- renderUI({
        HTML(paste0("<span style='font-size:16px'><i>Current: </i>", unit_val, selectedVar$current, "</span>"))
      })
      
    } else if (var_name %in% units[[2]]) {
      output$currentText <- renderUI({
        HTML(paste0("<span style='font-size:16px'><i>Current: </i>", selectedVar$current, "</span>"))
      })
      
    } else {
      unit_val <- "%"
      output$currentText <- renderUI({
        HTML(paste0("<span style='font-size:16px'><i>Current: </i>", selectedVar$current, unit_val, "</span>"))
      })
    }
    
    
    #======Calculate the Annual growth rate required to meet the goals==============
    
    cagr_avg <- averageGaugeList %>%
      filter(variable == var_name) %>%
      mutate(goal = parse_number(goal), current = parse_number(current),
             cagr = ((goal/current)^(1/10))-1,
             cagr = round(cagr*100, 2)) %>%
      pull(cagr)
    
    output$growthVal <- renderUI({
      HTML(paste0("<span style='font-size:16px'><i>Annual growth required: </i>", cagr_avg, "%</span>"))
    })
    
    
    #======Calculate the starting point for the goals ==============================
    start_oecs <- oecs.long %>%
      filter(variable == var_name & iso3c == "OECS") %>%
      filter(year == max(year)) %>%
      pull(value)
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$startPointOECS <- renderUI({
        HTML(paste0("<span style='font-size:16px'><i>Starting point: </i>$", prettyNum(round(start_oecs,2), big.mark = ","), "</span>"))
      })
    } else if (var_name %in% units[[2]]) {
      output$startPointOECS <- renderUI({
        HTML(paste0("<span style='font-size:16px'><i>Starting point: </i>", prettyNum(round(start_oecs,2), big.mark = ","), "</span>"))
      })
    } else {
      unit_val <- "%"
      output$startPointOECS <- renderUI({
        HTML(paste0("<span style='font-size:16px'><i>Starting point: </i>", prettyNum(round(start_oecs,2), big.mark = ","), "%</span>"))
      })
    }
    
    # Print the OECS target at the top of the page based on whether indicator is in 
    # dollar value, percentage or otherwise
    # Calculate the indicator under review
    output$varName <- renderUI({
      HTML("<h3 style='border-bottom:solid 2px;'><b>", var_name, "</b></h3>")
    })
    
    # Print the data source
    output$sourceTextTitle <- renderUI({
      sourceText <- indicator.tags %>% 
        filter(variable == var_name) %>% 
        pull(Source)
      
      HTML(paste0("<span style='font-size:16px'><b>Source:</b> ", sourceText, "</span>"))
    })
    
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      
      output$goalTextTitle <- renderUI({
        goalTexter <- indicator.tags %>% 
          filter(variable == var_name) %>% 
          pull(goal)
        
        goalTexter <- paste0("<span style='font-size:16px'><b>OECS average target:</b> ", unit_val, goalTexter, "</span>")
        HTML(goalTexter)
      })
    } else if (var_name %in% units[[2]]) {
      output$goalTextTitle <- renderUI({
        goalTexter <- indicator.tags %>% 
          filter(variable == var_name) %>% 
          pull(goal)
        HTML("<span style='font-size:16px'><b>OECS average target:</b> ", goalTexter, "</span>")
      })
    } else {
      unit_val <- "%"
      output$goalTextTitle <- renderUI({
        goalTexter <- indicator.tags %>% 
          filter(variable == var_name) %>% 
          pull(goal)
        
        goalTexter <- HTML("<span style='font-size:16px'><b>OECS average target:</b> ", goalTexter, unit_val, "</span>")
        HTML(goalTexter)
      })
    }
    
    # Print the icons on the top of each page for the indicator===================
    output$varSDG <- renderUI({
      image <- sdg.list2 %>%
        filter(variable == var_name) %>%
        pull(icon_image)
      
      HTML(paste(image))
    })
    
    
    #====================== Commence the gauge plots for each country for each respective target=======
    new_dat <- githubDatFull()
    
    new_vals <- new_dat %>% 
      filter(variable == var_name) 
    
    if(nrow(new_vals)>0) {
      
      new_temp <- new_vals %>%
        pivot_longer(!c(variable,iso3c), names_to="year", 
                     names_transform = list(year=as.numeric),
                     values_to="current") %>%
        group_by(iso3c) %>%
        filter(!is.na(current)) %>%
        filter(year == max(year)) %>%
        ungroup() 
      
      lower_val_temp <- oecs.long %>%
        filter(variable == var_name) %>%
        select(variable, lower_val_wanted) %>%
        distinct()
      
      newCountryGauge <- countryGaugeList %>%
        filter(variable == var_name) %>%
        select(iso3c, goal) %>%
        merge(new_temp, ., by="iso3c") %>%
        merge(., lower_val_temp) %>%
        mutate(current = as.numeric(current), goal2 = parse_number(goal)) %>%
        mutate(percent = ifelse(current < goal2 & lower_val_wanted == 0, current/goal2,
                                ifelse(current < goal2 & lower_val_wanted == 1, 1,
                                       ifelse(current > goal2 & lower_val_wanted == 0, 1, goal2/current)))) %>%
        mutate(percent = format(round((percent*100), 1), nsmall=1),
               current = prettyNum(round(current, 2), big.mark = ","),
               label = paste0(countrycode(iso3c, "iso3c", "country.name"), "\n (", year, ")")) %>%
        select(-year, -lower_val_wanted, -goal2) 
      
      new_dat_iso3c <- newCountryGauge$iso3c
      
      countryGaugeList <- countryGaugeList %>%
        select(-val) %>%
        filter(!variable == var_name | !iso3c %in% new_dat_iso3c) %>%
        rbind(newCountryGauge, .)
    }
    
    
    #===============Gauge plot Antigua and Barbuda=================================
    
    atgGauge <- countryGaugeList %>% 
      filter(iso3c == "ATG" & variable == var_name)
    
    output$atgLabel <- renderUI({ HTML(paste0("<b>", atgGauge %>% pull(label), "</b>")) })
    
    cagr_atg <- countryGaugeList %>%
      filter(variable == var_name & iso3c == "ATG") %>%
      mutate(goal = parse_number(goal), 
             current = parse_number(current),
             cagr = ((goal/current)^(1/10))-1,
             cagr = round(cagr*100, 2)) %>%
      pull(cagr)
    
    
    output$growthValATG <- renderUI({
      HTML(paste0("<i>Annual growth required: </i>", cagr_atg, "%"))
    })
    
    start_atg <- oecs.long %>%
      filter(variable == var_name & iso3c == "ATG") %>%
      filter(year == max(year)) %>%
      pull(value)
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$startPointATG <- renderUI({
        HTML(paste0("<i>Starting point: </i>$", prettyNum(round(start_atg,2), big.mark = ",")))
      })
    } else if (var_name %in% units[[2]]) {
      output$startPointATG <- renderUI({
        HTML(paste0("<i>Starting point: </i>", prettyNum(round(start_atg,2), big.mark = ",")))
      })
    } else {
      unit_val <- "%"
      output$startPointATG <- renderUI({
        HTML(paste0("<i>Starting point: </i>", prettyNum(round(start_atg,2), big.mark = ","), "%"))
      })
    }
    
    prev_valueATG <- five_year %>% 
      filter(variable == var_name & iso3c == "ATG") 
    
    avgGaugeATG <- countryGaugeList %>% 
      filter(variable == var_name & iso3c == "ATG") %>% 
      select(iso3c, avgVal = "percent") %>%
      mutate(avgVal = as.numeric(avgVal)) %>%
      mutate(label = ifelse(avgVal <= 25, "#ff6f69", ifelse(avgVal >= 75, "#88d8b0", "#ffcc5c")))
    
    if(nrow(prev_valueATG) < 1) {
      prev_valueATG <- data.frame("variable" = var_name, "iso3c" = "ATG", prev_pct = 0)
    }
    
    output$atgGauge <- renderPlot({
      
      avgGaugeATG %>%
        ggplot(aes(x=iso3c, y=avgVal)) +
        ggforce::geom_link(
          aes(
            x = iso3c,
            xend = iso3c,
            y = 0,
            yend = 100,
          ),
          color="lightgrey",
          alpha=0.5,
          linewidth = 13
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c, 
            xend = iso3c,
            y = 0,
            yend = avgVal,
            color = label,
          ),
          linewidth = 13
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c, 
            xend = iso3c,
            y = prev_valueATG$prev_pct-1,
            yend = prev_valueATG$prev_pct,
          ),
          color="black",
          linewidth = 13
        ) +
        annotate("richtext", label=paste0("<b>",avgGaugeATG$avgVal,"</b>"), 
                 x=0.45, y=50, size=9, vjust=0, label.color=NA,fill=NA) +
        scale_y_continuous(limits = c(0, 198), expand = c(0, 0)) + 
        coord_polar(theta = "y", start=4.7, clip = "off") +
        theme_void() +
        scale_color_manual(values = avgGaugeATG$label) +
        guides(color="none") +
        theme(
          text = element_text(size=18, family= "Roboto Condensed", face="bold"),
          plot.title = element_text(size=12, hjust=0.05),
          axis.title=element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          plot.title.position = "plot") 
      
    }, height=200)
    
    atg_val <- atgGauge %>% pull(goal)
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$atgGoal <- renderUI({
        HTML(paste0("<i>Goal: </i>", ifelse(atg_val == " ", "", unit_val), atgGauge %>% pull(goal)))
      })
    } else if (var_name %in% units[[2]]) {
      output$atgGoal <- renderUI({
        HTML(paste0("<i>Goal: </i>", atgGauge %>% pull(goal)))
      })
    } else {
      unit_val <- "%"
      output$atgGoal <- renderUI({
        HTML(paste0("<i>Goal: </i>", atgGauge %>% pull(goal), ifelse(atg_val == " ", "", unit_val)))
      })
    }
    
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$atgCurrent <- renderUI({
        HTML(paste0("<i>Current: </i>", ifelse(atg_val == " ", "", unit_val), atgGauge %>% pull(current)))
      })
    } else if (var_name %in% units[[2]]) {
      output$atgCurrent <- renderUI({
        HTML(paste0("<i>Current: </i>", atgGauge %>% pull(current)))
      })
    } else {
      unit_val <- "%"
      output$atgCurrent <- renderUI({
        HTML(paste0("<i>Current: </i>", atgGauge %>% 
                      pull(current), ifelse(atg_val == " ", "", unit_val)))
      })
    } 
    
    #===============Gauge plot Dominica=============================================
    # 
    dmaGauge <- countryGaugeList %>% filter(iso3c == "DMA" & variable == var_name)
    
    output$dmaLabel <- renderUI({  HTML(paste0("<b>", dmaGauge %>% pull(label), "</b>")) })
    
    cagr_dma <- countryGaugeList %>%
      filter(variable == var_name & iso3c == "DMA") %>%
      mutate(goal = parse_number(goal), current = parse_number(current),
             cagr = ((goal/current)^(1/10))-1,
             cagr = round(cagr*100, 2)) %>%
      pull(cagr)
    
    output$growthValDMA <- renderUI({
      HTML(paste0("<i>Annual growth required: </i>", cagr_dma, "%"))
    })
    
    start_dma <- oecs.long %>%
      filter(variable == var_name & iso3c == "DMA") %>%
      filter(year == max(year)) %>%
      pull(value)
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$startPointDMA <- renderUI({
        HTML(paste0("<i>Starting point: </i>$", prettyNum(round(start_dma,2), big.mark = ",")))
      })
    } else if (var_name %in% units[[2]]) {
      output$startPointDMA <- renderUI({
        HTML(paste0("<i>Starting point: </i>", prettyNum(round(start_dma,2), big.mark = ",")))
      })
    } else {
      unit_val <- "%"
      output$startPointDMA <- renderUI({
        HTML(paste0("<i>Starting point: </i>", prettyNum(round(start_dma,2), big.mark = ","), "%"))
      })
    }
    
    prev_valueDMA <- five_year %>% filter(variable == var_name & iso3c == "DMA")
    
    avgGaugeDMA <- countryGaugeList %>% 
      filter(variable == var_name & iso3c == "DMA") %>% 
      select(iso3c, avgVal = "percent") %>%
      mutate(avgVal = as.numeric(avgVal)) %>%
      mutate(label = ifelse(avgVal <= 25, "#ff6f69", ifelse(avgVal >= 75, "#88d8b0", "#ffcc5c")))
    
    if(nrow(prev_valueDMA) < 1) {
      prev_valueDMA <- data.frame("variable" = var_name, "iso3c" = "DMA", prev_pct = 0)
    }
    
    output$dmaGauge <- renderPlot({
      
      avgGaugeDMA %>%
        ggplot(aes(x=iso3c, y=avgVal)) +
        ggforce::geom_link(
          aes(
            x = iso3c,
            xend = iso3c,
            y = 0,
            yend = 100,
          ),
          color="lightgrey",
          alpha=0.5,
          linewidth = 13
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c, 
            xend = iso3c,
            y = 0,
            yend = avgVal,
            color = label,
          ),
          linewidth = 13
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c, 
            xend = iso3c,
            y = prev_valueDMA$prev_pct-1,
            yend = prev_valueDMA$prev_pct,
          ),
          color="black",
          linewidth = 13
        ) +
        annotate("richtext", label=paste0("<b>",avgGaugeDMA$avgVal,"</b>"), 
                 x=0.45, y=50, size=9, vjust=0, label.color=NA,fill=NA) +
        scale_y_continuous(limits = c(0, 198), expand = c(0, 0)) + 
        coord_polar(theta = "y", start=4.7, clip = "off") +
        theme_void() +
        scale_color_manual(values = avgGaugeDMA$label) +
        guides(color="none") +
        theme(
          text = element_text(size=18, family= "Roboto Condensed", face="bold"),
          plot.title = element_text(size=12, hjust=0.05),
          axis.title=element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          plot.title.position = "plot") 
      
    }, height=200)
    
    dma_val <- dmaGauge %>% 
      pull(goal)
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$dmaGoal <- renderUI({
        HTML(paste0("<i>Goal: </i>", ifelse(dma_val == " ", "", unit_val), dmaGauge %>% pull(goal)))
      })
    } else if (var_name %in% units[[2]]) {
      output$dmaGoal <- renderUI({
        HTML(paste0("<i>Goal: </i>", dmaGauge %>% pull(goal)))
      })
    } else {
      unit_val <- "%"
      output$dmaGoal <- renderUI({
        HTML(paste0("<i>Goal: </i>", dmaGauge %>% pull(goal), ifelse(dma_val == " ", "", unit_val)))
      })
    }
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$dmaCurrent <- renderUI({
        HTML(paste0("<i>Current: </i>", ifelse(dma_val == " ", "", unit_val), dmaGauge %>% pull(current)))
      })
    } else if (var_name %in% units[[2]]) {
      output$dmaCurrent <- renderUI({
        HTML(paste0("<i>Current: </i>", dmaGauge %>% pull(current)))
      })
    } else {
      unit_val <- "%"
      output$dmaCurrent <- renderUI({
        HTML(paste0("<i>Current: </i>", dmaGauge %>% pull(current), ifelse(dma_val == " ", "", unit_val)))
      })
    }
    #==============Gauge plot Anguilla==============================================
    # Left in Anguilla gauges, would need reformatting before adding back in ---------
    # aiaGauge <- countryGaugeList %>% filter(iso3c == "AIA" & variable == var_name)
    # 
    # output$aiaLabel <- renderUI({ HTML(paste0("<b>", aiaGauge %>% pull(label), "</b>")) })
    # 
    # output$aiaGauge <- flexdashboard::renderGauge({
    #   gauge(
    #     as.numeric(aiaGauge %>% pull(percent)), min = 0, max = 100,
    #     sectors = gaugeSectors(success = c(75, 100),
    #                            warning = c(25, 75),
    #                            danger = c(0, 25),
    #                            colors = c("#88d8b0", "#ffcc5c", "#ff6f69")))
    # })
    # 
    # output$aiaGoal <- renderUI({
    #   HTML(paste("<i>Goal:</i>", aiaGauge %>% pull(goal)))
    # })
    # 
    # output$aiaCurrent <- renderUI({
    #   HTML(paste("<i>Current:</i>", aiaGauge %>% pull(current)))
    # })
    
    #===============Gauge plot Grenada===============================================
    
    grdGauge <- countryGaugeList %>% 
      filter(iso3c == "GRD" & variable == var_name)
    
    output$grdLabel <- renderUI({ HTML(paste0("<b>", grdGauge %>% pull(label), "</b>")) })
    
    cagr_grd <- countryGaugeList %>%
      filter(variable == var_name & iso3c == "GRD") %>%
      mutate(goal = parse_number(goal), current = parse_number(current),
             cagr = ((goal/current)^(1/10))-1,
             cagr = round(cagr*100, 2)) %>%
      pull(cagr)
    output$growthValGRD <- renderUI({
      HTML(paste0("<i>Annual growth required: </i>", cagr_grd, "%"))
    })
    
    start_grd <- oecs.long %>%
      filter(variable == var_name & iso3c == "GRD") %>%
      filter(year == max(year)) %>%
      pull(value)
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$startPointGRD <- renderUI({
        HTML(paste0("<i>Starting point: </i>$", prettyNum(round(start_grd,2), big.mark = ",")))
      })
    } else if (var_name %in% units[[2]]) {
      output$startPointGRD <- renderUI({
        HTML(paste0("<i>Starting point: </i>", prettyNum(round(start_grd,2), big.mark = ",")))
      })
    } else {
      unit_val <- "%"
      output$startPointGRD <- renderUI({
        HTML(paste0("<i>Starting point: </i>", prettyNum(round(start_grd,2), big.mark = ","), "%"))
      })
    }
    
    prev_valueGRD <- five_year %>% filter(variable == var_name & iso3c == "GRD") 
    
    avgGaugeGRD <- countryGaugeList %>% 
      filter(variable == var_name & iso3c == "GRD") %>% 
      select(iso3c, avgVal = "percent") %>%
      mutate(avgVal = as.numeric(avgVal)) %>%
      mutate(label = ifelse(avgVal <= 25, "#ff6f69", ifelse(avgVal >= 75, "#88d8b0", "#ffcc5c")))
    
    if(nrow(prev_valueGRD) < 1) {
      prev_valueGRD <- data.frame("variable" = var_name, "iso3c" = "GRD", prev_pct = 0)
    }
    
    output$grdGauge <- renderPlot({
      
      avgGaugeGRD %>%
        ggplot(aes(x=iso3c, y=avgVal)) +
        ggforce::geom_link(
          aes(
            x = iso3c,
            xend = iso3c,
            y = 0,
            yend = 100,
          ),
          color="lightgrey",
          alpha=0.5,
          linewidth = 13
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c, 
            xend = iso3c,
            y = 0,
            yend = avgVal,
            color = label,
          ),
          linewidth = 13
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c, 
            xend = iso3c,
            y = prev_valueGRD$prev_pct-1,
            yend = prev_valueGRD$prev_pct,
          ),
          color="black",
          linewidth = 13
        ) +
        annotate("richtext", label=paste0("<b>",avgGaugeGRD$avgVal,"</b>"), 
                 x=0.45, y=50, size=9, vjust=0, label.color=NA,fill=NA) +
        scale_y_continuous(limits = c(0, 198), expand = c(0, 0)) + 
        coord_polar(theta = "y", start=4.7, clip = "off") +
        theme_void() +
        scale_color_manual(values = avgGaugeGRD$label) +
        guides(color="none") +
        theme(
          text = element_text(size=18, family= "Roboto Condensed", face="bold"),
          plot.title = element_text(size=12, hjust=0.05),
          axis.title=element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          plot.title.position = "plot") 
      
    }, height=200)
    
    grd_val <- grdGauge %>% pull(goal)
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$grdGoal <- renderUI({
        HTML(paste0("<i>Goal: </i>", ifelse(grd_val == " ", "", unit_val), grdGauge %>% pull(goal)))
      })
    } else if (var_name %in% units[[2]]) {
      output$grdGoal <- renderUI({
        HTML(paste0("<i>Goal: </i>", grdGauge %>% pull(goal)))
      })
    } else {
      unit_val <- "%"
      output$grdGoal <- renderUI({
        HTML(paste0("<i>Goal: </i>", grdGauge %>% pull(goal), ifelse(grd_val == " ", "", unit_val)))
      })
    }
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$grdCurrent <- renderUI({
        HTML(paste0("<i>Current: </i>", ifelse(grd_val == " ", "", unit_val), grdGauge %>% pull(current)))
      })
    } else if (var_name %in% units[[2]]) {
      output$grdCurrent <- renderUI({
        HTML(paste0("<i>Current: </i>", grdGauge %>% pull(current)))
      })
    } else {
      unit_val <- "%"
      output$grdCurrent <- renderUI({
        HTML(paste0("<i>Current: </i>", grdGauge %>% pull(current), ifelse(grd_val == " ", "", unit_val)))
      })
    }
    
    #===============Gauge plot Saint Lucia=============================================  
    
    lcaGauge <- countryGaugeList %>% filter(iso3c == "LCA" & variable == var_name)
    
    output$lcaLabel <- renderUI({ HTML(paste0("<b>", lcaGauge %>% 
                                                pull(label), "</b>")) })
    
    cagr_lca <- countryGaugeList %>%
      filter(variable == var_name & iso3c == "LCA") %>%
      mutate(goal = parse_number(goal), current = parse_number(current),
             cagr = ((goal/current)^(1/10))-1,
             cagr = round(cagr*100, 2)) %>%
      pull(cagr)
    output$growthValLCA <- renderUI({
      HTML(paste0("<i>Annual growth required: </i>", cagr_lca, "%"))
    })
    
    start_lca <- oecs.long %>%
      filter(variable == var_name & iso3c == "LCA") %>%
      filter(year == max(year)) %>%
      pull(value)
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$startPointLCA <- renderUI({
        HTML(paste0("<i>Starting point: </i>$", prettyNum(round(start_lca,2), big.mark = ",")))
      })
    } else if (var_name %in% units[[2]]) {
      output$startPointLCA <- renderUI({
        HTML(paste0("<i>Starting point: </i>", prettyNum(round(start_lca,2), big.mark = ",")))
      })
    } else {
      unit_val <- "%"
      output$startPointLCA <- renderUI({
        HTML(paste0("<i>Starting point: </i>", prettyNum(round(start_lca,2), big.mark = ","), "%"))
      })
    }
    
    prev_valueLCA <- five_year %>% filter(variable == var_name & iso3c == "LCA") 
    avgGaugeLCA <- countryGaugeList %>% 
      filter(variable == var_name & iso3c == "LCA") %>% 
      select(iso3c, avgVal = "percent") %>%
      mutate(avgVal = as.numeric(avgVal)) %>%
      mutate(label = ifelse(avgVal <= 25, "#ff6f69", ifelse(avgVal >= 75, "#88d8b0", "#ffcc5c")))
    
    if(nrow(prev_valueLCA) < 1) {
      prev_valueLCA <- data.frame("variable" = var_name, "iso3c" = "KNA", prev_pct = 0)
    }
    
    output$lcaGauge <- renderPlot({
      
      avgGaugeLCA %>%
        ggplot(aes(x=iso3c, y=avgVal)) +
        ggforce::geom_link(
          aes(
            x = iso3c,
            xend = iso3c,
            y = 0,
            yend = 100,
          ),
          color="lightgrey",
          alpha=0.5,
          linewidth = 13
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c, 
            xend = iso3c,
            y = 0,
            yend = avgVal,
            color = label,
          ),
          linewidth = 13
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c, 
            xend = iso3c,
            y = prev_valueLCA$prev_pct-1,
            yend = prev_valueLCA$prev_pct,
          ),
          color="black",
          linewidth = 13
        ) +
        annotate("richtext", label=paste0("<b>",avgGaugeLCA$avgVal,"</b>"),
                 x=0.45, y=50, size=9, vjust=0, label.color=NA,fill=NA) +
        scale_y_continuous(limits = c(0, 198), expand = c(0, 0)) + 
        coord_polar(theta = "y", start=4.7, clip = "off") +
        theme_void() +
        scale_color_manual(values = avgGaugeLCA$label) +
        guides(color="none") +
        theme(
          text = element_text(size=18, family= "Roboto Condensed", face="bold"),
          plot.title = element_text(size=12, hjust=0.05),
          axis.title=element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          plot.title.position = "plot") 
      
    }, height=200)
    
    lca_val <- lcaGauge %>% pull(goal)
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$lcaGoal <- renderUI({
        HTML(paste0("<i>Goal: </i>", ifelse(lca_val == " ", "", unit_val), lcaGauge %>% pull(goal)))
      })
    } else if (var_name %in% units[[2]]) {
      output$lcaGoal <- renderUI({
        HTML(paste0("<i>Goal: </i>", lcaGauge %>% pull(goal)))
      })
    } else {
      unit_val <- "%"
      output$lcaGoal <- renderUI({
        HTML(paste0("<i>Goal: </i>", lcaGauge %>% 
                      pull(goal), ifelse(lca_val == " ", "", unit_val)))
      })
    }
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$lcaCurrent <- renderUI({
        HTML(paste0("<i>Current: </i>", ifelse(lca_val == " ", "", unit_val), lcaGauge %>% pull(current)))
      })
    } else if (var_name %in% units[[2]]) {
      output$lcaCurrent <- renderUI({
        HTML(paste0("<i>Current: </i>", lcaGauge %>% pull(current)))
      })
    } else {
      unit_val <- "%"
      output$lcaCurrent <- renderUI({
        HTML(paste0("<i>Current: </i>", lcaGauge %>% 
                      pull(current), ifelse(lca_val == " ", "", unit_val)))
      })
    }
    
    #===============Gauge plot Saint Kitts and Nevis=========================================
    # 
    knaGauge <- countryGaugeList %>% filter(iso3c == "KNA" & variable == var_name)
    
    output$knaLabel <- renderUI({ HTML(paste0("<b>", knaGauge %>% pull(label), "</b>")) })
    
    cagr_kna <- countryGaugeList %>%
      filter(variable == var_name & iso3c == "KNA") %>%
      mutate(goal = parse_number(goal), current = parse_number(current),
             cagr = ((goal/current)^(1/10))-1,
             cagr = round(cagr*100, 2)) %>%
      pull(cagr)
    
    output$growthValKNA <- renderUI({
      HTML(paste0("<i>Annual growth required: </i>", cagr_kna, "%"))
    })
    
    start_kna <- oecs.long %>%
      filter(variable == var_name & iso3c == "KNA") %>%
      filter(year == max(year)) %>%
      pull(value)
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$startPointKNA <- renderUI({
        HTML(paste0("<i>Starting point: </i>$", prettyNum(round(start_kna,2), big.mark = ",")))
      })
    } else if (var_name %in% units[[2]]) {
      output$startPointKNA <- renderUI({
        HTML(paste0("<i>Starting point: </i>", prettyNum(round(start_kna,2), big.mark = ",")))
      })
    } else {
      unit_val <- "%"
      output$startPointKNA <- renderUI({
        HTML(paste0("<i>Starting point: </i>", prettyNum(round(start_kna,2), big.mark = ","), "%"))
      })
    }
    
    prev_valueKNA <- five_year %>% filter(variable == var_name & iso3c == "KNA") 
    avgGaugeKNA <- countryGaugeList %>% 
      filter(variable == var_name & iso3c == "KNA") %>% 
      select(iso3c, avgVal = "percent") %>%
      mutate(avgVal = as.numeric(avgVal)) %>%
      mutate(label = ifelse(avgVal <= 25, "#ff6f69", ifelse(avgVal >= 75, "#88d8b0", "#ffcc5c")))
    
    if(nrow(prev_valueKNA) < 1) {
      prev_valueKNA <- data.frame("variable" = var_name, "iso3c" = "KNA", prev_pct = 0)
    }
    
    output$knaGauge <- renderPlot({
      
      avgGaugeKNA %>%
        ggplot(aes(x=iso3c, y=avgVal)) +
        ggforce::geom_link(
          aes(
            x = iso3c,
            xend = iso3c,
            y = 0,
            yend = 100,
          ),
          color="lightgrey",
          alpha=0.5,
          linewidth = 13
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c,
            xend = iso3c,
            y = 0,
            yend = avgVal,
            color=label
          ),
          linewidth = 13
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c, 
            xend = iso3c,
            y = prev_valueKNA$prev_pct-1,
            yend = prev_valueKNA$prev_pct,
          ),
          color="black",
          linewidth = 13
        ) +
        annotate("richtext", label=paste0("<b>",avgGaugeKNA$avgVal,"</b>"), 
                 x=0.45, y=50, size=9, vjust=0, label.color=NA,fill=NA) +
        scale_y_continuous(limits = c(0, 198), expand = c(0, 0)) +
        coord_polar(theta = "y", start=4.7, clip = "off") +
        theme_void() +
        scale_color_manual(values = avgGaugeKNA$label) +
        guides(color="none") +
        theme(
          text = element_text(size=18, family= "Roboto Condensed", face="bold"),
          plot.title = element_text(size=12, hjust=0.05),
          axis.title=element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          plot.title.position = "plot")
      
    }, height=200)
    
    kna_val <- knaGauge %>% pull(goal)
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$knaGoal <- renderUI({
        HTML(paste0("<i>Goal: </i>", ifelse(kna_val == " ", "", unit_val), knaGauge %>% pull(goal)))
      })
    } else if (var_name %in% units[[2]]) {
      output$knaGoal <- renderUI({
        HTML(paste0("<i>Goal: </i>", knaGauge %>% pull(goal)))
      })
    } else {
      unit_val <- "%"
      output$knaGoal <- renderUI({
        HTML(paste0("<i>Goal: </i>", knaGauge %>% pull(goal), ifelse(kna_val == " ", "", unit_val)))
      })
    }
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$knaCurrent <- renderUI({
        HTML(paste0("<i>Current: </i>", ifelse(kna_val == " ", "", unit_val), knaGauge %>% pull(current)))
      })
    } else if (var_name %in% units[[2]]) {
      output$knaCurrent <- renderUI({
        HTML(paste0("<i>Current: </i>", knaGauge %>% pull(current)))
      })
    } else {
      unit_val <- "%"
      output$knaCurrent <- renderUI({
        HTML(paste0("<i>Current: </i>", knaGauge %>% pull(current), ifelse(kna_val == " ", "", unit_val)))
      })
    }
    
    #===============Gauge plot Saint Vincent===============================================
    
    vctGauge <- countryGaugeList %>% filter(iso3c == "VCT" & variable == var_name)
    
    output$vctLabel <- renderUI({ HTML(paste0("<b>", vctGauge %>% pull(label), "</b>")) })
    
    cagr_vct <- countryGaugeList %>%
      filter(variable == var_name & iso3c == "VCT") %>%
      mutate(goal = parse_number(goal), current = parse_number(current),
             cagr = ((goal/current)^(1/10))-1,
             cagr = round(cagr*100, 2)) %>%
      pull(cagr)
    
    output$growthValVCT <- renderUI({
      HTML(paste0("<i>Annual growth required: </i>", cagr_vct, "%"))
    })
    
    start_vct <- oecs.long %>%
      filter(variable == var_name & iso3c == "VCT") %>%
      filter(year == max(year)) %>%
      pull(value)
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$startPointVCT <- renderUI({
        HTML(paste0("<i>Starting point: </i>$", prettyNum(round(start_vct,2), big.mark = ",")))
      })
    } else if (var_name %in% units[[2]]) {
      output$startPointVCT <- renderUI({
        HTML(paste0("<i>Starting point: </i>", prettyNum(round(start_vct,2), big.mark = ",")))
      })
    } else {
      unit_val <- "%"
      output$startPointVCT <- renderUI({
        HTML(paste0("<i>Starting point: </i>", prettyNum(round(start_vct,2), big.mark = ","), "%"))
      })
    }
    
    prev_valueVCT <- five_year %>% filter(variable == var_name & iso3c == "VCT") 
    avgGaugeVCT <- countryGaugeList %>% 
      filter(variable == var_name & iso3c == "VCT") %>% 
      select(iso3c, avgVal = "percent") %>%
      mutate(avgVal = as.numeric(avgVal)) %>%
      mutate(label = ifelse(avgVal <= 25, "#ff6f69", ifelse(avgVal >= 75, "#88d8b0", "#ffcc5c")))
    
    if(nrow(prev_valueVCT) < 1) {
      prev_valueVCT <- data.frame("variable" = var_name, "iso3c" = "VCT", prev_pct = 0)
    }
    
    output$vctGauge <- renderPlot({
      
      avgGaugeVCT %>%
        ggplot(aes(x=iso3c, y=avgVal)) +
        ggforce::geom_link(
          aes(
            x = iso3c,
            xend = iso3c,
            y = 0,
            yend = 100,
          ),
          color="lightgrey",
          alpha=0.5,
          linewidth = 13
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c, 
            xend = iso3c,
            y = 0,
            yend = avgVal,
            color = label,
          ),
          linewidth = 13
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c, 
            xend = iso3c,
            y = prev_valueVCT$prev_pct-1,
            yend = prev_valueVCT$prev_pct,
          ),
          color="black",
          linewidth = 13
        ) +
        annotate("richtext", label=paste0("<b>",avgGaugeVCT$avgVal,"</b>"), 
                 x=0.45, y=50, size=9, vjust=0, label.color=NA,fill=NA) +
        scale_y_continuous(limits = c(0, 198), expand = c(0, 0)) + 
        coord_polar(theta = "y", start=4.7, clip = "off") +
        theme_void() +
        scale_color_manual(values = avgGaugeVCT$label) +
        guides(color="none") +
        theme(
          text = element_text(size=18, family= "Roboto Condensed", face="bold"),
          plot.title = element_text(size=12, hjust=0.05),
          axis.title=element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          plot.title.position = "plot") 
      
    }, height=200)
    
    vct_val <- vctGauge %>% pull(goal)
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$vctGoal <- renderUI({
        HTML(paste0("<i>Goal: </i>", ifelse(vct_val == " ", "", unit_val), vctGauge %>% pull(goal)))
      })
    } else if (var_name %in% units[[2]]) {
      output$vctGoal <- renderUI({
        HTML(paste0("<i>Goal: </i>", vctGauge %>% pull(goal)))
      })
    } else {
      unit_val <- "%"
      output$vctGoal <- renderUI({
        HTML(paste0("<i>Goal: </i>", vctGauge %>% pull(goal), ifelse(vct_val == " ", "", unit_val)))
      })
    }
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$vctCurrent <- renderUI({
        HTML(paste0("<i>Current: </i>", ifelse(vct_val == " ", "", unit_val), vctGauge %>% pull(current)))
      })
    } else if (var_name %in% units[[2]]) {
      output$vctCurrent <- renderUI({
        HTML(paste0("<i>Current: </i>", vctGauge %>% pull(current)))
      })
    } else {
      unit_val <- "%"
      output$vctCurrent <- renderUI({
        HTML(paste0("<i>Current: </i>", vctGauge %>% pull(current), ifelse(vct_val == " ", "", unit_val)))
      })
    }
    
    #===============Gauge plot Montserrat================================================
    msrGauge <- countryGaugeList %>% filter(iso3c == "MSR" & variable == var_name)
    
    output$msrLabel <- renderUI({ HTML(paste0("<b>", msrGauge %>% pull(label), "</b>")) })
    
    cagr_msr <- countryGaugeList %>%
      filter(variable == var_name & iso3c == "MSR") %>%
      mutate(goal = parse_number(goal), current = parse_number(current),
             cagr = ((goal/current)^(1/10))-1,
             cagr = round(cagr*100, 2)) %>%
      pull(cagr)
    output$growthValMSR <- renderUI({
      HTML(paste0("<i>Annual growth required: </i>", cagr_msr, "%"))
    })
    
    start_msr <- oecs.long %>%
      filter(variable == var_name & iso3c == "MSR") %>%
      filter(year == max(year)) %>%
      pull(value)
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$startPointMSR <- renderUI({
        HTML(paste0("<i>Starting point: </i>$", prettyNum(round(start_msr,2), big.mark = ",")))
      })
    } else if (var_name %in% units[[2]]) {
      output$startPointMSR <- renderUI({
        HTML(paste0("<i>Starting point: </i>", prettyNum(round(start_msr,2), big.mark = ",")))
      })
    } else {
      unit_val <- "%"
      output$startPointMSR <- renderUI({
        HTML(paste0("<i>Starting point: </i>", prettyNum(round(start_msr,2), big.mark = ","), "%"))
      })
    }
    
    prev_valueMSR <- five_year %>% filter(variable == var_name & iso3c == "MSR") 
    avgGaugeMSR <- countryGaugeList %>% 
      filter(variable == var_name & iso3c == "MSR") %>% 
      select(iso3c, avgVal = "percent") %>%
      mutate(avgVal = as.numeric(avgVal)) %>%
      mutate(label = ifelse(avgVal <= 25, "#ff6f69", ifelse(avgVal >= 75, "#88d8b0", "#ffcc5c")))
    
    if(nrow(prev_valueMSR) < 1) {
      prev_valueMSR <- data.frame("variable" = var_name, "iso3c" = "MSR", prev_pct = 0)
    }
    
    if(nrow(avgGaugeMSR) < 1) {
      avgGaugeMSR <- data.frame("avgVal" = 0, "iso3c" = "MSR", label = "transparent")
    }
    
    output$msrGauge <- renderPlot({
      
      avgGaugeMSR %>%
        ggplot(aes(x="MSR", y=avgVal)) +
        ggforce::geom_link(
          aes(
            x = iso3c,
            xend = iso3c,
            y = 0,
            yend = 100,
          ),
          color="lightgrey",
          alpha=0.5,
          linewidth = 13
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c, 
            xend = iso3c,
            y = 0,
            yend = avgVal,
            color = label,
          ),
          linewidth = 13
        ) +
        ggforce::geom_link(
          aes(
            x = iso3c, 
            xend = iso3c,
            y = prev_valueMSR$prev_pct-1,
            yend = prev_valueMSR$prev_pct,
          ),
          color="black",
          linewidth = 13
        ) +
        annotate("richtext", label=paste0("<b>",avgGaugeMSR$avgVal,"</b>"),
                 x=0.45, y=50, size=9, vjust=0, label.color=NA,fill=NA) +
        scale_y_continuous(limits = c(0, 198), expand = c(0, 0)) + 
        coord_polar(theta = "y", start=4.7, clip = "off") +
        theme_void() +
        scale_color_manual(values = avgGaugeMSR$label) +
        guides(color="none") +
        theme(
          text = element_text(size=18, family= "Roboto Condensed", face="bold"),
          plot.title = element_text(size=12, hjust=0.05),
          axis.title=element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          plot.title.position = "plot") 
      
    }, height=200)
    
    msr_val <- msrGauge %>% pull(current)
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$msrGoal <- renderUI({
        HTML(paste0("<i>Goal: </i>", ifelse(msr_val == " ", "", unit_val), msrGauge %>% pull(goal)))
      })
    } else if (var_name %in% units[[2]]) {
      output$msrGoal <- renderUI({
        HTML(paste0("<i>Goal: </i>", msrGauge %>% pull(goal)))
      })
    } else {
      unit_val <- "%"
      output$msrGoal <- renderUI({
        HTML(paste0("<i>Goal: </i>", msrGauge %>% pull(goal), ifelse(msr_val == " ", "", unit_val)))
      })
    }
    
    if (var_name %in% units[[1]]) {
      unit_val <- "$"
      output$msrCurrent <- renderUI({
        HTML(paste0("<i>Current: </i>", ifelse(msr_val == " ", "", unit_val), msrGauge %>% pull(current)))
      })
    } else if (var_name %in% units[[2]]) {
      output$msrCurrent <- renderUI({
        HTML(paste0("<i>Current: </i>", msrGauge %>% pull(current)))
      })
    } else {
      unit_val <- "%"
      output$msrCurrent <- renderUI({
        HTML(paste0("<i>Current: </i>", msrGauge %>% pull(current), ifelse(msr_val == " ", "", unit_val)))
      })
    }
    
  }

#================ Return button on gauge pages==================================
# I went in and changed the action button to be more readable
  
  # Return button on gauge pages
  observe({ output$returnButton <- renderUI({p( shinyWidgets::actionBttn(
    inputId = "go_back",
    label = "Click to return to Main page",
    style = "jelly", 
    color = "danger"
  )
    )}) })
  
  
 
  # Generate panels for scrolling between gauge pages
  observeEvent(c(input$select_button), {
    
    var_list <- c(as.character(var.list2[[1]]), as.character(var.list2[[2]]), 
                  as.character(var.list2[[3]]), as.character(var.list2[[4]]))
    
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    
    if(selectedRow < 1) {
      selectedRow <- 1 
    } 
    if(selectedRow >= length(var_list)) {
      selectedRow <- length(var_list)
    } 
    
    var_name <- var_list[selectedRow]
    position_val <- which(var_name == var_list)
    
    
    # these create the next and previous button by rendering the UI
    output$panelLeft2 <- renderUI({
      fluidRow(
        column(2,
               align="center",
               fixedPanel(
                 actionButton("to_prev", label = "Previous  Page",
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                 left = 10, bottom = 10)
        )
      )
    })
    
    # these create the next and previous button by rendering the UI
    output$panelRight2 <- renderUI({
      fluidRow(
        column(2,
               align="center",
               fixedPanel(
                 actionButton("to_next", label = "Next Page",
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                 right = 10, bottom = 10)
        )
      )
    })
    
  })
  
  # Toggling between front and gauge pages
  observeEvent(c(input$select_button), {updateNavbarPage(session, "main", " ")})
  
  observeEvent(input$go_back, {updateNavbarPage(session, "main", "Scorecard")})
  
} # end of server



# Run app 
shinyApp(ui = ui, server = server, options = list(width = "90%"))
