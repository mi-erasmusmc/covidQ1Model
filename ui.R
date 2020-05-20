addInfo <- function(item, infoId) {
  infoTag <- tags$small(class = "badge pull-right action-button",
                        style = "padding: 1px 6px 2px 6px; background-color: steelblue;",
                        type = "button", 
                        id = infoId,
                        "i")
  item$children[[1]]$children <- append(item$children[[1]]$children, list(infoTag))
  return(item)
}

ui <- shinydashboard::dashboardPage(skin = 'black',
                                    
                                    shinydashboard::dashboardHeader(title = "COVID-19 Predictor", 
                                                                    
                                                                    tags$li(div(img(src = 'logo.png',
                                                                                    title = "OHDSI PLP", height = "40px", width = "40px"),
                                                                                style = "padding-top:0px; padding-bottom:0px;"),
                                                                            class = "dropdown")
                                                                    
                                                                    
                                    ), 
                                    
                                    shinydashboard::dashboardSidebar(
                                      shinydashboard::sidebarMenu(
                                        addInfo(shinydashboard::menuItem("Calculate Risk", tabName = "Risk", icon = shiny::icon("heartbeat")), "RiskInfo"),
                                        addInfo(shinydashboard::menuItem("Evidence", tabName = "Evidence", icon = shiny::icon("search")), "EvidenceInfo"),
                                        addInfo(shinydashboard::menuItem("About", tabName = "About", icon = shiny::icon("info-circle")), "AboutInfo")
                                      )
                                    ),
                                    
                                    shinydashboard::dashboardBody(
                                      shinydashboard::tabItems(  
                                        
                                        shinydashboard::tabItem(tabName = "Risk",                              
                                                                sidebarPanel(
                                                                  shiny::p('Use this tool to calculate the risk of COVID outcomes: '),
                                                                  shiny::p(' '),
                                                                  shiny::sliderInput("age", "Age:",
                                                                              min = 18, max = 94,
                                                                              value = 50),
                                                                  shiny::selectInput("sex","Sex", choices = c("Male", "Female")),
                                                                  shiny::checkboxInput("cancer", "History of Cancer"),
                                                                  shiny::checkboxInput("copd", "History of COPD"),
                                                                  shiny::checkboxInput("diabetes", "History of Diabetes"),
                                                                  shiny::checkboxInput("hd", "History of Heart disease"),
                                                                  shiny::checkboxInput("hl", "History of Hyperlipidemia"),
                                                                  shiny::checkboxInput("hypertension", "History of Hypertension"),
                                                                  shiny::checkboxInput("kidney","History of Kidney Disease"),
                                                                  shiny::actionButton("calculate","Calculate Risk"),
                                                                  
                                                                  hr()
                                                                  
                                                                ),
                                                                
                                                                mainPanel(
                                                                  #shiny::tableOutput("data"),
                                                                  #shinydashboard::box(status = "warning", width = 12,
                                                                  #shiny::textOutput("risk"),
                                                                  #),
                                                                  # conditionalPanel('input.calculate', 
                                                                  # shinydashboard::box(width = 12,
                                                                  #                     title = tagList(shiny::icon("tachometer-alt"),"Predicted Risk"), status = "warning", solidHeader = TRUE,
                                                                  #                     shiny::textOutput("risk")
                                                                  # )),
                                                                  
                                                                  conditionalPanel('input.calculate', {
                                                                    shinydashboard::box(width = 12,
                                                                                        title = tagList(shiny::icon("bar-chart"),"Predicted Risk (%)"), status = "info", solidHeader = TRUE,
                                                                                        
                                                                    plotly::plotlyOutput("contributions"))}
                                                                    
                                                                    ),
                                                                  shinydashboard::box(
                                                                    status = "primary", solidHeader = TRUE,
                                                                    width = 12,
                                                                    shiny::HTML("<br>The evidence for this prediction model can be explored at: <a href=\"https://data.ohdsi.org/Covid19PredictingHospitalizationInFluPatients//\">Link</a>"),
                                                                    shiny::HTML("<br>These models should NOT considered as a replacement for advice from medical professionals.")
                                                                  )
                                                                )
                                        ), # end risk tab,
                                        
                                        shinydashboard::tabItem(tabName = "Evidence",
                                                                shiny::h2("Information"),
                                                                shiny::p("The Observational Health Data Sciences and Informatics (OHDSI) international community is hosting a COVID-19 virtual study-a-thon this week (March 26-29) to inform healthcare decision-making in response to the current global pandemic."),
                                                                shiny::a("Paper", href = 'https://', target='_blank'),
                                                                shiny::p(' '),
                                                                shiny::a("Result App", href = 'https://data.ohdsi.org/Covid19PredictingHospitalizationInFluPatients/', target='_blank')
                                        ),
                                        
                                        shinydashboard::tabItem(tabName = "About",
                                                                shiny::h2("About OHDSI"),
                                                                
                                                                shiny::p("Mission: Improving health by empowering a community to collaboratively generate evidence that promotes better health decisions and better care"),
                                                                shiny::p("Vision: Creating a world in which observational research produces a comprehensive understanding of health and disease"),
                                                                shiny::p("Values: Innovation, Reproducibility, Community, Collaboration, Openness, Beneficence"),
                                                                shiny::p(" "),
                                                                shiny::p("OHDSI is a multi-stakeholder, interdisciplinary collaborative to bring out the value of health data through large-scale analytics. All our solutions are open-source."),
                                                                shiny::p("OHDSI has established an international network of researchers and observational health databases with a central coordinating center housed at Columbia University."),
                                                                shiny::p(" "),
                                                                shiny::a("OHDSI website", href = 'https://www.ohdsi.org', target='_blank')
                                        )
                                        
                                      ) # end  tabs
                                      
                                      
                                    )
                                    
)
