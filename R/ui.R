shinyUI(pageWithSidebar(
headerPanel("Sector Relative Reversal"),
 
sidebarPanel(
wellPanel(
p(strong("Sectors")),
checkboxInput(inputId = "sector_ALL", label = "All Sectors", value = TRUE),

checkboxInput(inputId = "sector_ConsumerDiscretionary", label = "Consumer Discretionary", value = TRUE),
checkboxInput(inputId = "sector_ConsumerStaples", label = "Consumer Staples", value = TRUE),
checkboxInput(inputId = "sector_Financials", label = "Financials", value = TRUE),
checkboxInput(inputId = "sector_Industrials", label = "Industrials", value = TRUE),
checkboxInput(inputId = "sector_Materials", label = "Materials", value = TRUE),
checkboxInput(inputId = "sector_Energy", label = "Energy", value = FALSE),
checkboxInput(inputId = "sector_InformationTechnology", label = "Information Technology", value = FALSE),
checkboxInput(inputId = "sector_TelecommunicationServices", label = "Telecommunication Services", value = FALSE),
checkboxInput(inputId = "sector_Utilities", label = "Utilities", value = FALSE),
checkboxInput(inputId = "sector_HealthCare", label = "Health Care", value = FALSE),
checkboxInput(inputId = "sector_Undefined", label = "Undefined", value = FALSE)

)# ,
 
#selectInput(inputId = "chart_type",
#label = "Chart type",
#choices = c("Candlestick" = "candlesticks",
#"Matchstick" = "matchsticks",
#"Bar" = "bars",
#"Line" = "line")
#),
 
#wellPanel(
#p(strong("Date range (back from present)")),
#sliderInput(inputId = "time_num",
#label = "Time number",
#min = 1, max = 24, step = 1, value = 6)# ,
 
#selectInput(inputId = "time_unit",
#label = "Time unit",
#choices = c("Days" = "days",
#"Weeks" = "weeks",
#"Months" = "months",
#"Years" = "years"),
#selected = "Months")
#)#,

#checkboxInput(inputId = "log_y", label = "log y axis", value = FALSE)
),

mainPanel(
conditionalPanel(condition = "input.sector_ALL",
br(),
plotOutput(outputId = "plot_ALL")),
  
conditionalPanel(condition = "input.sector_ConsumerDiscretionary",
br(),
plotOutput(outputId = "plot_ConsumerDiscretionary")),

conditionalPanel(condition = "input.sector_ConsumerStaples",
br(),
plotOutput(outputId = "plot_ConsumerStaples")),

conditionalPanel(condition = "input.sector_Energy",
br(),
plotOutput(outputId = "plot_Energy")),

conditionalPanel(condition = "input.sector_Financials",
                 br(),
                 plotOutput(outputId = "plot_Financials")),

conditionalPanel(condition = "input.sector_HealthCare",
                 br(),
                 plotOutput(outputId = "plot_HealthCare")),

conditionalPanel(condition = "input.sector_Industrials",
                 br(),
                 plotOutput(outputId = "plot_Industrials")),

conditionalPanel(condition = "input.sector_InformationTechnology",
                 br(),
                 plotOutput(outputId = "plot_InformationTechnology")),

conditionalPanel(condition = "input.sector_Materials",
                 br(),
                 plotOutput(outputId = "plot_Materials")),

conditionalPanel(condition = "input.sector_TelecommunicationServices",
                 br(),
                 plotOutput(outputId = "plot_TelecommunicationServices")),

conditionalPanel(condition = "input.sector_Undefined",
                 br(),
                 plotOutput(outputId = "plot_Undefined")),

conditionalPanel(condition = "input.sector_Utilities",
                 br(),
                 plotOutput(outputId = "plot_Utilities"))


)
))
