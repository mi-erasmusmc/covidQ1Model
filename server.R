function(input, output, session) {

    ageCalc <- function(age, model){
      
      if(model == 'Hospitalization'){
        if(age < 20)           { value = -7}
        if(dplyr::between(age,20, 24)){ value = -4}
        if(dplyr::between(age,25, 29)){ value = -2}
        if(dplyr::between(age,30, 34)){ value = -2}
        if(dplyr::between(age,35, 39)){ value = 3}
        if(dplyr::between(age,40, 44)){ value = 3}
        if(dplyr::between(age,45, 49)){ value = 6}
        if(dplyr::between(age,50, 54)){ value = 9}
        if(dplyr::between(age,55, 59)){ value = 13}
        if(dplyr::between(age,60, 64)){ value = 15}
        if(dplyr::between(age,65, 69)){ value = 19}
        if(dplyr::between(age,70, 74)){ value = 20}
        if(dplyr::between(age,75, 79)){ value = 23}
        if(dplyr::between(age,80, 84)){ value = 24}
        if(dplyr::between(age,85, 89)){ value = 27}
        if(dplyr::between(age,90, 94)){ value = 25}
        return(value)
      }
      if(model == 'Intensive Care'){
        if(age < 20)           { value = -10}
        if(dplyr::between(age,20, 24)){ value = -2}
        if(dplyr::between(age,25, 29)){ value = -1}
        if(dplyr::between(age,30, 34)){ value = 0}
        if(dplyr::between(age,35, 39)){ value = 0}
        if(dplyr::between(age,40, 44)){ value = 3}
        if(dplyr::between(age,45, 49)){ value = 5}
        if(dplyr::between(age,50, 54)){ value = 10}
        if(dplyr::between(age,55, 59)){ value = 12}
        if(dplyr::between(age,60, 64)){ value = 16}
        if(dplyr::between(age,65, 69)){ value = 22}
        if(dplyr::between(age,70, 74)){ value = 21}
        if(dplyr::between(age,75, 79)){ value = 22}
        if(dplyr::between(age,80, 84)){ value = 21}
        if(dplyr::between(age,85, 89)){ value = 25}
        if(dplyr::between(age,90, 94)){ value = 21}
        return(value)
      }
      
      if(model == 'Death'){
        if(age < 20)           { value = -15}
        if(dplyr::between(age,20, 24)){ value = -8}
        if(dplyr::between(age,25, 29)){ value = -20}
        if(dplyr::between(age,30, 34)){ value = -5}
        if(dplyr::between(age,35, 39)){ value = 0}
        if(dplyr::between(age,40, 44)){ value = -6}
        if(dplyr::between(age,45, 49)){ value = 1}
        if(dplyr::between(age,50, 54)){ value = 15}
        if(dplyr::between(age,55, 59)){ value = 12}
        if(dplyr::between(age,60, 64)){ value = 16}
        if(dplyr::between(age,65, 69)){ value = 27}
        if(dplyr::between(age,70, 74)){ value = 31}
        if(dplyr::between(age,75, 79)){ value = 35}
        if(dplyr::between(age,80, 84)){ value = 40}
        if(dplyr::between(age,85, 89)){ value = 45}
        if(dplyr::between(age,90, 94)){ value = 30}
        return(value)
      }
      
    }
    
    inputData  <- shiny::reactiveValues(data = NULL, model = NULL)
    
    shiny::observeEvent(input$calculate, {
      
      inputData$model<- input$model
      
      if(input$model == 'Hospitalization'){
        inputData$data <- data.frame(
          Intercept = -50,
          age = ageCalc(input$age, 'Hospitalization'),
          sex = ifelse(input$sex == "Male",3,0),
          cancer = input$cancer * 2,
          copd = input$copd * 6,
          diabetes = input$diabetes * 3,
          hd = input$hd * 4,
          hl = input$hl * -3,
          hypertension = input$hypertension * 3,
          kidney = input$kidney * 2
        )
      } else if (input$model == 'Intensive Care'){
        inputData$data <- data.frame(
          Intercept = -56, # this is wrong!
          age = ageCalc(input$age, 'Intensive Care'),
          sex = ifelse(input$sex == "Male",4,0),
          cancer = input$cancer * 1,
          copd = input$copd * 6,
          diabetes = input$diabetes * 4,
          hd = input$hd * 4,
          hl = input$hl * -4,
          hypertension = input$hypertension * 5,
          kidney = input$kidney * 4
        )
      } else{
        inputData$data <- data.frame(
          Intercept = -66, # need to add this
          age = ageCalc(input$age, 'Death'),
          sex = ifelse(input$sex == "Male",4,0),
          cancer = input$cancer * 3,
          copd = input$copd * 4,
          diabetes = input$diabetes * 2,
          hd = input$hd * 2,
          hl = input$hl * -7,
          hypertension = input$hypertension * 3,
          kidney = input$kidney * 2
        )
      }
      
      
      total <- rowSums(inputData$data)
      inputData$data$risk <- 1/(1+exp(-total/10)) *100
    })
    
          
          output$data <- shiny::renderTable({data = inputData$data})
          riskText <- function(x1, model){
            if(!is.null(x1)){
              paste0("The patient's risk of ",model," is: ", round(x = x1, digits = 1), "%")}
            else{NULL}
            }
          output$risk <- shiny::renderText(riskText(inputData$data$risk, inputData$model))
          
          
          

          #contribution of risk
          output$contributions <- plotly::renderPlotly(plotly::plot_ly(x = as.double(inputData$data)[!names(inputData$data)%in%c('risk','Intercept')], 
                                                                       y = names(inputData$data)[!names(inputData$data)%in%c('risk','Intercept')], 
                                                              type = 'bar', orientation = 'h'))
          
          
          
 
          
          
          
          
          # helpers
          showInfoBox <- function(title, htmlFileName) {
            shiny::showModal(shiny::modalDialog(
              title = title,
              easyClose = TRUE,
              footer = NULL,
              size = "l",
              shiny::HTML(readChar(htmlFileName, file.info(htmlFileName)$size) )
            ))
          }
          
          observeEvent(input$RiskInfo, {
            showInfoBox("Risk", "html/Risk.html")
          })
          observeEvent(input$EvidenceInfo, {
            showInfoBox("Evidence", "html/Evidence.html")
          })
          observeEvent(input$AboutInfo, {
            showInfoBox("About", "html/About.html")
          })
}