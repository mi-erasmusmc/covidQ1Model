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
    
    inputData  <- shiny::reactiveValues(dataHosp = NULL, dataIntense = NULL, dataDeath = NULL)
    riskValues  <- shiny::reactiveValues(data = NULL)
    
    shiny::observeEvent(input$calculate, {
      
        inputData$dataHosp <- data.frame(
          Intercept = 20, #updated to make scale positive
          age = ageCalc(input$age, 'Hospitalization'),
          sex = ifelse(input$sex == "Male",3,0),
          cancer = input$cancer * 2,
          copd = input$copd * 6,
          diabetes = input$diabetes * 3,
          'heart disease' = input$hd * 4,
          hyperlipidemia = input$hl * -3,
          hypertension = input$hypertension * 3,
          kidney = input$kidney * 2
        )
        
        inputData$dataIntense  <- data.frame(
          Intercept = 4, #updated to make scale positive
          age = ageCalc(input$age, 'Intensive Care'),
          sex = ifelse(input$sex == "Male",4,0),
          cancer = input$cancer * 1,
          copd = input$copd * 6,
          diabetes = input$diabetes * 4,
          'heart disease' = input$hd * 4,
          hyperlipidemia = input$hl * -4,
          hypertension = input$hypertension * 5,
          kidney = input$kidney * 4
        )
        
        inputData$dataDeath <- data.frame(
          Intercept = 4, #updated to make scale positive
          age = ageCalc(input$age, 'Death'),
          sex = ifelse(input$sex == "Male",4,0),
          cancer = input$cancer * 3,
          copd = input$copd * 4,
          diabetes = input$diabetes * 2,
          'heart disease' = input$hd * 2,
          hyperlipidemia = input$hl * -7,
          hypertension = input$hypertension * 3,
          kidney = input$kidney * 2
        )
  
      totals <- unlist(lapply(inputData,  function(x){rowSums(x) - 70})) #subtract the 70 we used to make positive
      
      riskValues$data <- data.frame(names = c('Death', 'Hospitalization','Hospitalization with Intensive Care or Death'),
                                        values = 1/(1+exp(-totals/10)) *100, stringsAsFactors = F)
      #sort by values (descending) probably a better way to do this...
      riskValues$data$names <- factor(riskValues$data$names, 
                                      levels = unique(riskValues$data$names)[order(riskValues$data$values, 
                                                                                   decreasing = FALSE)])
      #moved this here because there is a warning if done in plotting
      riskValues$data$values <- round(riskValues$data$values, 1)

      
    })
    
          
          riskText <- function(x1, model){
            if(!is.null(x1)){
              paste0("The patient's risk of ",model," is: ", round(x = x1, digits = 1), "%")}
            else{NULL}
            }
          output$risk <- shiny::renderText(riskText(riskValues$data[1,2], riskValues$data[1,1]))
          
          
          

          #contribution of risk
          output$contributions <- plotly::renderPlotly(plotly::plot_ly(x = as.double(riskValues$data$values), 
                                                                       y = riskValues$data$names, 
                                                                       #weird warning but doesnt seem to impact the function
                                                                       text = riskValues$data$values, textposition = 'auto', insidetextfont = list(size=20, color = 'black'),
                                                                       #color = as.double(riskValues$data$values),
                                                                       #colors = c('TRUE'= "#0E8009", 'FALSE' = "#D30E1A"),
                                                                       type = 'bar', orientation = 'h', showlegend = F, ))
          
          
          
 
          
          
          
          
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
          
          # shiny::observeEvent(input$RiskInfo, {
          #   showInfoBox("Risk", "html/Risk.html")
          # })
          shiny::observeEvent(input$EvidenceInfo, {
            showInfoBox("Evidence", "html/Evidence.html")
          })
          shiny::observeEvent(input$AboutInfo, {
            showInfoBox("About", "html/About.html")
          })
}