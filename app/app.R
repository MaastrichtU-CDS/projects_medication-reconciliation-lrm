# Packages ----

library(shiny)
library(bslib)
library(rms)

# Model ----

model_2 <- function(age,
                    sex.male_1,
                    n_prescribed_medicines,
                    high_risk_medicines.yes_1,
                    health_literacy_composite.insufficient_1) {
  lp <- 
    3.9586 * -1 +
    0.0406 * ((age - 59.2184) / 18.5225) +
    0.3084 * ((n_prescribed_medicines - 5.9345) / 4.7231) +
    0.6339 * high_risk_medicines.yes_1 +  
    0.7827 * health_literacy_composite.insufficient_1
  
  pred_prob <- 1 / (1 + exp(-lp))
  
  return(pred_prob)
}

# UI ----

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  
  titlePanel(
    wellPanel(
      style = "background:#2c3e50",
      
      h3(strong("Predicting adjustment to medication policy after medication reconciliation in outpatient clinics", style = "color:#fff"))
    ),
  ), 
  
  br(),
  
  mainPanel(
    width = 12,
    
    wellPanel(
      h5(strong("Predictors")),
      
      numericInput("age", 
                   label = HTML("<b> Age </b> <br> Please enter a value between 0 and 100 years."), 
                   value = NA,
                   min = 0,
                   max = 100),
      
      radioButtons("sex.male_1", 
                   label = h6(strong("Sex")),
                   choices = list("Male" = 1,
                                  "Female" = 0),
                   selected = NA),
      
      numericInput("n_prescribed_medicines", 
                   label = HTML("<b> Number of prescribed medicines </b> <br> Please enter a value between 0 and 20."), 
                   value = NA,
                   min = 0,
                   max = 20,
                   step = 1),
      
      radioButtons("high_risk_medicines.yes_1", 
                   label = h6(strong("Use of high-risk medicines")),
                   choices = list("Yes" = 1, 
                                  "No" = 0), 
                   selected = NA),
      
      radioButtons("health_literacy_composite.insufficient_1", 
                   label = h6(strong("Health literacy")),
                   choices = list("Insufficient" = 1, 
                                  "Sufficient or patient is dependent on others for medication" = 0), 
                   selected = NA),
      
      submitButton("Submit")
    ),
    
    br(),
    
    wellPanel(
      style = "background:#DBE9FA",
      
      p(h5(strong("Estimated probability adjustment to medication policy after medication reconciliation"))),
      
      textOutput("pred_prob.text"),
      
      br()
    ),
    
    br(),
    
    wellPanel(
      p(h5(strong("Probability threshold"))),
      
      HTML("<p>We determine the probability threshold based on how many patients a pharmacy is willing to screen to find one patient needing an adjustment to their medication policy <a href='https://doi.org/10.1177/0272989X06295361'>(Vickers & Elkin, 2006)</a>. If the predicted probability of the patient is equal to or above the probability threshold, medication reconciliation is recommended. Please enter below how many patients you would be willing to screen to find one patient needing an adjustment to their medications. The one patient needing an adjustment to their medication policy is included in this number.</p>"),
      
      numericInput("exchange_rate.n_total", 
                   label = NULL, 
                   value = NA),
      
      submitButton("Submit")
    ),
    
    br(),
    
    wellPanel(
      style = "background:#DBE9FA",
      
      p(h5(strong("Recommendation"))),
      
      textOutput("recommendation"),
      
      br()
    ),
    
    br()
    
  ),
  
  HTML("<i> Please note further research is needed before implementing this model in clinical practice. </i> 
        <br> 
        <i> To enhance data protection, please download the source code <a href = https://github.com/MaastrichtU-CDS/projects_medication-reconciliation-lrm/tree/main/app> here</a> and run the app locally.</i> 
        <br>
        <br>
        <i> DISCLAIMER: THE AUTHORS WAIVE RESPONSIBILITY FOR ANY HARMS CAUSED BY THE USE OF THIS MODEL, SOFTWARE, OR WEBSITE. </i> <br>
        <br>")
)

# Server ----

server <- function(input, output) {
  
  pred_prob <- reactive({
    pred_prob <- model_2(age = input$age,
                         sex.male_1 = as.numeric(input$sex.male_1),
                         n_prescribed_medicines = input$n_prescribed_medicines,
                         high_risk_medicines.yes_1 = as.numeric(input$high_risk_medicines.yes_1),
                         health_literacy_composite.insufficient_1 = as.numeric(input$health_literacy_composite.insufficient_1)) 
    
    return(pred_prob)
  })
  
  output$pred_prob.text <- renderText({ 
    pred_probx100 <- round(pred_prob(), digits = 4) * 100
    
    pred_prob.text <- 
      if(isTruthy(pred_probx100) 
         &
         (input$age >= 0)
         & 
         (input$age <= 100)
         & 
         (input$n_prescribed_medicines >= 0)
         &
         (input$n_prescribed_medicines <= 20)){
        pred_prob.text <- paste0("The estimated probability this patient needs an adjustment to their medication policy is ", pred_probx100, "%.")
      } else if(isTruthy(pred_probx100)) {
        pred_prob.text <- paste("Out of range values were detected. Please review your submission.") 
      } else {
        pred_prob.text <- paste("Please complete the form above and press submit.")
      }
    
    return(pred_prob.text)
  })
  
  output$recommendation <- renderText({
    pt <- 1 / (input$exchange_rate.n_total)
    
    recommendation <- 
      if(is.na(pt)
         |
         (!isTruthy(input$age))
         |
         (input$age < 0)
         |
         (input$age > 100)
         |
         (!isTruthy(input$sex.male_1))
         |
         (!isTruthy(input$n_prescribed_medicines))
         |
         (input$n_prescribed_medicines < 0)
         |
         (input$n_prescribed_medicines > 20)
         |
         (!isTruthy(input$high_risk_medicines.yes_1))
         |
         (!isTruthy(input$health_literacy_composite.insufficient_1))) {
        paste("For a recommendation, please submit an answer to the question above.")
      } else if (input$exchange_rate.n_total <= 1) {
        paste("Please submit a number above one.")
      } else if(pred_prob() >= pt) {
        paste("It is recommended to provide medication reconciliation.")
      } else {
        paste("It is not recommended to provide medication reconciliation.")
      }
    
    return(recommendation)
  })
}

shinyApp(ui, server)

# Run ----

shinyApp(ui, server)
