library(shiny)
library(readr)


trauma <- list(
  "Domestic Violence:" = "dv",
  "Household Substance Use:" = "hsu",
  "Mental Illness" = "mi",
  "Parental Separation or Divorce" = "psod",
  "Incarceration" = "inc",
  "Emotional Abuse:" = "ea",
  "Physical Abuse" = "pa",
  "Sexual Abuse" = "sa",
  "Emotional Neglect" = "en",
  "Physical Neglect" = "pn"
)

snp_weights <- data.frame(
  SNP = c("rs6894288", "rs6866910", "rs6882423", "rs73125991", 
          "rs11951568", "rs73772260", "rs6888413"),
  Condition = c(1.43317, 1.59989, 1.60132, 1.64513, 1.67161,1.72634 ,1.74865 )
)



genetics_select <- selectInput("genetic_information", 
                               label = "Do you know patient's genetic information?",
                               choices = list("",
                                              "Yes, I do" = "y",
                                              "No, I don't" = "n"),
                               selected = NULL,
                               width = "100%")
trauma_select <- checkboxGroupInput("Trauma_information",
                             label = "Patient trauma information",
                             choices =)

demo_select_multi <- 
  selectInput("trauma select", 
              label = "ACE events (select all that apply)", 
              choices = trauma,
              selected = NULL,
              multiple = TRUE, 
  )


ui <- fluidPage(
  
  # Application title
  titlePanel("Patient PGS Risk"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      genetics_select,
      demo_select_multi,
      
      # FIXED conditionalPanel
      conditionalPanel(
        condition = "input.genetic_information == 'y'",
        tagList(
          selectInput("rs6894288", "rs6894288", choices = c("2" = "0", "1" = "1", "0" = "2")),
          selectInput("rs6866910", "rs6866910", choices = c("2" = "0", "1" = "1", "0" = "2")),
          selectInput("rs6882423", "rs6882423", choices = c("2" = "0", "1" = "1", "0" = "2")),
          selectInput("rs73125991", "rs73125991", choices = c("2" = "0", "1" = "1", "0" = "2")),
          selectInput("rs11951568", "rs11951568", choices = c("2" = "0", "1" = "1", "0" = "2")),
          selectInput("rs73772260", "rs73772260", choices = c("2" = "0", "1" = "1", "0" = "2")),
          selectInput("rs6888413", "rs6888413", choices = c("2" = "0", "1" = "1", "0" = "2")),
          plotOutput("snp_piechart")
        )
      )
    ),
    
    mainPanel(
      verbatimTextOutput("conditionalPanel"),
      verbatimTextOutput("genetic_output"), 
      verbatimTextOutput("snp_contributions_text"),
      verbatimTextOutput("total_risk_score")
    )
  )
)
# Server logic
server <- function(input, output) {
  
  # Print the choices selected for each SNP and calculate contributions
  output$selected_genetic_info <- renderPrint({
    req(input$genetic_information == "y")  # Make sure they selected 'Yes' for genetic info
    
    # Collect selected SNP values (the user's choices)
    selected_values <- sapply(snp_weights$SNP, function(snp) {
      input[[snp]]  # Directly return the selected value for each SNP input
    })
    
    # Return the SNP values in a readable format
    data.frame(SNP = snp_weights$SNP, Selected_Value = selected_values)
  })
  
  # Calculate contributions of each SNP and display
  output$total_risk_score <- renderPrint({
    req(input$genetic_information == "y")
    
    # Get the genotypes
    genotypes <- sapply(snp_weights$SNP, function(snp) {
      val <- input[[snp]]
      as.numeric(val)
    })
    
    # Calculate contributions
    contributions <- genotypes * snp_weights$Condition
    total <- sum(contributions, na.rm = TRUE)
    
    # Trauma adjustment
    has_trauma <- length(input$`trauma select`) > 0
    total_trauma <- length(input$ `trauma select`)
    adjusted_score <- if (has_trauma) total * 13 else total
    trauma_score <- if(has_trauma) total * (total_trauma * (13 /4)) else total
    
    # Load comparison data
    simulated_data <- read.csv("simulated_bpd_data.csv")
    
    # Z-score + percentile
    population_mean <- mean(simulated_data$new_risk, na.rm = TRUE)
    population_sd <- sd(simulated_data$new_risk, na.rm = TRUE)
    z <- (trauma_score - population_mean) / population_sd
    pct <- ecdf(simulated_data$new_risk)(trauma_score) * 100
    
    rec <- if (pct < 50) {
      "No Current risk of BPD!"
    }  else if (pct < 95) {
      "Moderate risk, evulate patient for BPD and other symptoms ."
    } else {
      "High Risk, assess patient for BPD & therapy"
    }
    
    # Print everything
    list(
      Adjusted_Risk_Score = round(trauma_score, 2),
      Z_Score = round(z, 2),
      Percentile = paste0(round(pct, 1), "%"),
      Recommendation = rec
    )
    
  })
}



# Run the application 
shinyApp(ui = ui, server = server)

