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
    
    hr()

  ),
  
  mainPanel(
    shiny::tableOutput("data"),
    shiny::textOutput("risk"),
    shinydashboard::box(
      status = "primary", solidHeader = TRUE,
      width = 12,
      shiny::HTML("<br>The evidence for this prediction model can be explored at: <a href=\"https://data.ohdsi.org/Covid19PredictingSimpleModels/\">Link</a>"),
      shiny::HTML("<br>These models should NOT considered as a replacement for advice from medical professionals.")
    )
  )
)
