# shinymanager::secure_app(
  fluidPage(
  titlePanel("Covid-19 Hospitalisation Risk"),
  
  sidebarPanel(
    shiny::numericInput("age", "Age", 50, min = 18, max = 94),
    shiny::selectInput("sex","Sex", choices = c("Male", "Female")),
    shiny::checkboxInput("cancer", "History of Cancer"),
    shiny::checkboxInput("copd", "History of COPD"),
    shiny::checkboxInput("diabetes", "History of Diabetes"),
    shiny::checkboxInput("hd", "History of Heart disease"),
    shiny::checkboxInput("hl", "History of Hyperlipidemia"),
    shiny::checkboxInput("hypertension", "History of Hypertension"),
    shiny::checkboxInput("kidney","History of Kidney Disease"),
    shiny::actionButton("calculate","Calculate Risk"),
    
    hr(),

  ),
  
  mainPanel(
  shiny::tableOutput("data"),
  shiny::textOutput("risk")
  )
)
# )
