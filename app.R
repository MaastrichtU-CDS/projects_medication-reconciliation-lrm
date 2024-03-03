# Packages ----

library(shiny)
library(shinythemes)
library(rms)

# Models ----

## Model 1 ----

model_1 <- function(age,
                    sex.male_1,
                    n_prescribed_medicines,
                    high_risk_medicines.yes_1,
                    eGFR.below_60_1,
                    out_specialty_current_MUMC.group_A_1,
                    out_specialty_current_MUMC.group_B_1,
                    n_specialty_MUMC,
                    health_literacy_composite.insufficient_1) {
  lp <- 
    5.1277 * -1 +
    0.1406 * ((age - 59.2184) / 18.5225) +
    0.4051 * -1 * sex.male_1 + 
    0.4206 * ((n_prescribed_medicines - 5.9345) / 4.7231) +
    0.4878 * high_risk_medicines.yes_1 +  
    0.3966 * -1 * eGFR.below_60_1 +
    1.6124 * out_specialty_current_MUMC.group_A_1 +
    1.6189 * out_specialty_current_MUMC.group_B_1 +
    0.2674 * -1 * ((n_specialty_MUMC - 2.8139) / 1.8980) +
    0.7364 * health_literacy_composite.insufficient_1
  
  pred_prob <- 1 / (1 + exp(-lp))
  
  return(pred_prob)
}

## Model 2 ----

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
  theme = shinytheme("flatly"),
  
  tags$head(tags$style(HTML("a {color:#3498db}
                             a:hover {color:#3498db}")
                       )
            ),
  
  tags$style(HTML(".tabbable > .nav > li > a {background-color:#dee2e6 ;  color:#b4bcc2}
                  .tabbable > .nav > li[class=active] > a {background-color:#dee2e6; color:black}"
                  )
             ),
  
  titlePanel(
    wellPanel(
      style = "background:#2c3e50",
      h3(strong("Predicting adjustment to medication policy after medication reconciliation in outpatient clinics", style = "color:#fff"))
    ),
  ),
  
  tabsetPanel(
    tabPanel(
      h4(strong("Model 1")),
      fluidRow(
        column(
          12,
          wellPanel(
            h5(strong("Predictors")),
            numericInput("input_model_1.age", 
                         label = h5(strong("Age")), 
                         value = NA,
                         min = 0,
                         max = 110),
            radioButtons("input_model_1.sex.male_1", 
                         label = h5(strong("Gender")),
                         choices = list("Male" = 1,
                                        "Female" = 0),
                         selected = NA),
            numericInput("input_model_1.n_prescribed_medicines", 
                         label = h5(strong("Number of prescribed medicines")), 
                         value = NA,
                         min = 0,
                         max = 20,
                         step = 1),
            radioButtons("input_model_1.high_risk_medicines.yes_1", 
                         label = h5(strong("Use of high-risk medicines")),
                         choices = list("Yes" = 1, 
                                        "No" = 0), 
                         selected = NA),
            radioButtons("input_model_1.eGFR.below_60_1", 
                         label = h5(strong("eGFR < 60")),
                         choices = list("Yes" = 1, 
                                        "No" = 0), 
                         selected = NA),
            radioButtons("input_model_1.out_specialty_current_MUMC", 
                         label = h5(strong("Outpatient clinic specialty")),
                         choices = list("Cardiology, internal medicine, gastroenterology, neurology, rheumatology" = 1, 
                                        "Surgery, orthopedics, traumatology, urology: oncology" = 2, 
                                        "Dermatology, otorhinolaryngology, urology: functional" = 3), 
                         selected = NA),
            numericInput("input_model_1.n_specialty_MUMC", 
                         label = h5(strong("Number of specialties involved in the past year")), 
                         value = NA,
                         min = 1,
                         max = 17),
            radioButtons("input_model_1.health_literacy_composite.insufficient_1", 
                         label = h5(strong("Health literacy")),
                         choices = list("Insufficient" = 1, 
                                        "Sufficient or patient is dependent on others for medication" = 0), 
                         selected = NA),
            submitButton("Submit")
          )
        )
      ),
      
      fluidRow(
        column(
          12,
          wellPanel(
            style = "background:#DBE9FA",
            p(h5(strong("Estimated probability adjustment to medication policy after medication reconciliation"))),
            textOutput("output_model_1.pred_prob.text"),
          )
        )
      )
      ),
      
    tabPanel(
      h4(strong("Model 2")),
      
      fluidRow(
        column(12,
               wellPanel(
                 h5(strong("Predictors")),
                 numericInput("input_model_2.age", 
                              label = h5(strong("Age")), 
                              value = NA,
                              min = 0,
                              max = 110),
                 numericInput("input_model_2.n_prescribed_medicines", 
                              label = h5(strong("Number of prescribed medicines")), 
                              value = NA,
                              min = 0,
                              max = 20,
                              step = 1),
                 radioButtons("input_model_2.high_risk_medicines.yes_1", 
                              label = h5(strong("Use of high-risk medicines")),
                              choices = list("Yes" = 1, 
                                             "No" = 0), 
                              selected = NA),
                 radioButtons("input_model_2.health_literacy_composite.insufficient_1", 
                              label = h5(strong("Health literacy")),
                              choices = list("Insufficient" = 1, 
                                             "Sufficient or patient is dependent on others for medication" = 0), 
                              selected = NA),
                 submitButton("Submit")
               )
        )
      ),
      
      fluidRow(
        column(
          12,
          wellPanel(
            style = "background:#DBE9FA",
            p(h5(strong("Estimated probability adjustment to medication policy after medication reconciliation"))),
            textOutput("output_model_2.pred_prob.text")
          )
        )
      )
    )
  ),
  HTML("<i> Please note further research is needed before implementing this model in clinical practice. </i> <br> 
        <i> To enhance data protection, please download the source code <a href = https://github.com/MaastrichtU-CDS/projects_medication-reconciliation-lrm/app> here</a> and run the app locally.</i> <br> <br>")
)

# Server ----

server <- function(input, output) {
  
  model_1.pred_prob <- reactive({
    model_1.pred_prob <- model_1(age = input$input_model_1.age,
                                 sex.male_1 = as.numeric(input$input_model_1.sex.male_1),
                                 n_prescribed_medicines = input$input_model_1.n_prescribed_medicines,
                                 high_risk_medicines.yes_1 = as.numeric(input$input_model_1.high_risk_medicines.yes_1),
                                 eGFR.below_60_1 = as.numeric(input$input_model_1.eGFR.below_60_1),
                                 out_specialty_current_MUMC.group_A_1 = ifelse(input$input_model_1.out_specialty_current_MUMC == "1",
                                                                               1,
                                                                               0),
                                 out_specialty_current_MUMC.group_B_1 = ifelse(input$input_model_1.out_specialty_current_MUMC == "2",
                                                                               1,
                                                                               0),
                                 n_specialty_MUMC = input$input_model_1.n_specialty_MUMC,
                                 health_literacy_composite.insufficient_1 = as.numeric(input$input_model_1.health_literacy_composite.insufficient_1)) 
    
    return(model_1.pred_prob)
  })
  
  output$output_model_1.pred_prob.text <- renderText({ 
    
    model_1.pred_probx100 <- round(model_1.pred_prob(), digits = 4) * 100
    
    if((!is.na(input$input_model_1.age) 
         & 
         ((input$input_model_1.age < 0) 
          | 
          (input$input_model_1.age > 110)))
       &
       (!is.na(input$input_model_1.n_prescribed_medicines) 
        & 
        ((input$input_model_1.n_prescribed_medicines < 0) 
         | 
         (input$input_model_1.n_prescribed_medicines > 20)))
       &
       (!is.na(input$input_model_1.n_specialty_MUMC) 
        & 
        ((input$input_model_1.n_specialty_MUMC < 1) 
         | 
         (input$input_model_1.n_specialty_MUMC > 17)))
       ){
      output_model_1.pred_prob.text <- paste0("The submitted values for age, the number of prescribed medicines and the number of involved specialties in the past year are out of range.")
      
      return(output_model_1.pred_prob.text)
      } else if((!is.na(input$input_model_1.age) 
                 & 
                 ((input$input_model_1.age < 0) 
                  | 
                  (input$input_model_1.age > 110)))
                &
                (!is.na(input$input_model_1.n_prescribed_medicines) 
                 & 
                 ((input$input_model_1.n_prescribed_medicines < 0) 
                  | 
                  (input$input_model_1.n_prescribed_medicines > 20)))
      ){
        output_model_1.pred_prob.text <- paste0("The submitted values for age and the number of prescribed medicines are out of range.")
        
        return(output_model_1.pred_prob.text)
      } else if((!is.na(input$input_model_1.age) 
                 & 
                 ((input$input_model_1.age < 0) 
                  | 
                  (input$input_model_1.age > 110)))
                &
                (!is.na(input$input_model_1.n_specialty_MUMC) 
                 & 
                 ((input$input_model_1.n_specialty_MUMC < 1) 
                  | 
                  (input$input_model_1.n_specialty_MUMC > 17)))
      ){
        output_model_1.pred_prob.text <- paste0("The submitted values for age and the number of specialties involved in the past year are out of range.")
        
        return(output_model_1.pred_prob.text)
      } else if((!is.na(input$input_model_1.n_prescribed_medicines) 
                 & 
                 ((input$input_model_1.n_prescribed_medicines < 0) 
                  | 
                  (input$input_model_1.n_prescribed_medicines > 20)))
                 &
                (!is.na(input$input_model_1.n_specialty_MUMC) 
                 & 
                 ((input$input_model_1.n_specialty_MUMC < 1) 
                  | 
                  (input$input_model_1.n_specialty_MUMC > 17)))
                ){
        output_model_1.pred_prob.text <- paste0("The submitted values for the number of prescribed medicines and the number of specialties involved in the past year are out of range.")
        
        return(output_model_1.pred_prob.text)
      } else if(!is.na(input$input_model_1.age) 
                & 
                ((input$input_model_1.age < 0) 
                 | 
                 (input$input_model_1.age > 110))) {
      output_model_1.pred_prob.text <- paste0("The submitted value for age is out of range. Please enter a value between 0 and 110.")
      
      return(output_model_1.pred_prob.text)  
    } else if(!is.na(input$input_model_1.n_prescribed_medicines) 
              & 
              ((input$input_model_1.n_prescribed_medicines < 0) 
               | 
               (input$input_model_1.n_prescribed_medicines > 20))) {
      output_model_1.pred_prob.text <- paste0("The submitted value for the number of prescribed medicines is out of range. Please enter a value between 0 and 20.")
      
      return(output_model_1.pred_prob.text)  
    } else if(!is.na(input$input_model_1.n_specialty_MUMC) 
              & 
              ((input$input_model_1.n_specialty_MUMC < 1) 
               | 
               (input$input_model_1.n_specialty_MUMC > 17))) {
    output_model_1.pred_prob.text <- paste0("The submitted value for the number of specialties involved in the past year is out of range. Please enter a value between 1 and 17.")
    
    return(output_model_1.pred_prob.text)  
    } else if(isTruthy(model_1.pred_probx100)) {
      output_model_1.pred_prob.text <- paste0("The estimated probability this patient needs a change in medication policy is ", model_1.pred_probx100, "%.")
      
      return(output_model_1.pred_prob.text)  
    } else{
      output_model_1.pred_prob.text <- paste("Please complete the form above and press submit. Note no missing values are allowed.")
      
      return(output_model_1.pred_prob.text)  
    }
  })
    
  model_2.pred_prob <- reactive({
    model_2.pred_prob <- model_2(age = input$input_model_2.age,
                                 n_prescribed_medicines = input$input_model_2.n_prescribed_medicines,
                                 high_risk_medicines.yes_1 = as.numeric(input$input_model_2.high_risk_medicines.yes_1),
                                 health_literacy_composite.insufficient_1 = as.numeric(input$input_model_2.health_literacy_composite.insufficient_1)) 
    
    return(model_2.pred_prob)
  })
  
  output$output_model_2.pred_prob.text <- renderText({ 
    
    model_2.pred_probx100 <- round(model_2.pred_prob(), digits = 4) * 100
    
    if((!is.na(input$input_model_2.age) 
       & 
       ((input$input_model_2.age < 0) 
        | 
        (input$input_model_2.age > 110)))
       &
       (!is.na(input$input_model_2.n_prescribed_medicines) 
        & 
        ((input$input_model_2.n_prescribed_medicines < 0) 
        | 
        (input$input_model_2.n_prescribed_medicines > 20)))
    ){
      output_model_2.pred_prob.text <- paste0("The submitted values for age and the number of prescribed medicines are out of range.")
      
      return(output_model_2.pred_prob.text)
    } else if(!is.na(input$input_model_2.age) 
              & 
              ((input$input_model_2.age < 0) 
               | 
               (input$input_model_2.age > 110))) {
      output_model_2.pred_prob.text <- paste0("The submitted value for age is out of range. Please enter a value between 0 and 110.")
      
      return(output_model_2.pred_prob.text)  
    } else if(!is.na(input$input_model_2.n_prescribed_medicines) 
              & 
              ((input$input_model_2.n_prescribed_medicines < 0) 
               | 
               (input$input_model_2.n_prescribed_medicines > 20))) {
      output_model_2.pred_prob.text <- paste0("The submitted value for the number of prescribed medicines is out of range. Please enter a value between 0 and 20.")
  
      return(output_model_2.pred_prob.text)  
    } else if(isTruthy(model_2.pred_probx100)) {
      output_model_2.pred_prob.text <- paste0("The estimated probability this patient needs a change in medication policy is ", model_2.pred_probx100, "%.")
      
      return(output_model_2.pred_prob.text)  
    } else{
      output_model_2.pred_prob.text <- paste("Please complete the form above and press submit. Note no missing values are allowed.")
      
      return(output_model_2.pred_prob.text)  
    }
  })
  
}

# Run ----

shinyApp(ui, server)
