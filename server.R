function(input, output, session) {
  #event handler submit
  # observeEvent(input$submit,
  
  ageCalc <- function(age){
    if(age < 20)           { value = -7}
    if(between(age,20, 24)){ value = -4}
    if(between(age,25, 29)){ value = -2}
    if(between(age,30, 34)){ value = -2}
    if(between(age,35, 39)){ value = 3}
    if(between(age,40, 44)){ value = 3}
    if(between(age,45, 49)){ value = 6}
    if(between(age,50, 54)){ value = 9}
    if(between(age,55, 59)){ value = 13}
    if(between(age,60, 64)){ value = 15}
    if(between(age,65, 69)){ value = 19}
    if(between(age,70, 74)){ value = 20}
    if(between(age,75, 79)){ value = 23}
    if(between(age,80, 84)){ value = 24}
    if(between(age,85, 89)){ value = 27}
    if(between(age,90, 94)){ value = 25}
    
    return(value)
  }
          shiny::observeEvent(eventExpr = input$calculate, { 
            inputData <- data.frame(
                         Intercept = -50,
                         # age = ageCalc(input$age),
                         sex = ifelse(input$sex == "Male",3,0),
                         cancer = input$cancer * 2,
                         copd = input$copd * 6,
                         diabetes = input$diabetes * 3,
                         hd = input$hd * 4,
                         hl = input$hl * -3,
                         hypertension = input$hypertension * 3,
                         kidney = input$kidney * 2
                         )
            
            total <- rowSums(inputData)
            inputData$risk <- 1/(1+exp(-total/10)) *100
            inputData
               
          output$data <- shiny::renderTable({data = inputData})
          riskText <- paste0("The patient's risk is: ", round(x = inputData$risk, digits = 1), "%")
          output$risk <- shiny::renderText(riskText)
          }) 
               
}