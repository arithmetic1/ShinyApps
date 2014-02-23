require(xts)
within = read.table('within.csv') 
dates = as.Date(rownames(within), '%Y-%m-%d')
within = xts(within, order.by=dates)
all_sectors = xts(rowSums(within), order.by=dates); # , as.Date(rownames(within)))

all_secotrs = all_sectors["2009/"]

shinyServer(function(input, output) {
 
# Make a chart for a symbol, with the settings from the inputs
make_chart <- function(symbol) {

  if (symbol == 'All.Sectors'){
    plot.xts(cumprod(1+all_sectors)-1, main='All Sectors')  
  }else{
    plot.xts(cumprod(1+within[,symbol])-1, main=symbol)
  }
  
#chartSeries(symbol_data,
#name = symbol,
#type = input$chart_type,
#subset = paste("last", input$time_num, input$time_unit),
# log.scale = input$log_y,
#theme = "white")
}
 
output$plot_ALL <- renderPlot({ make_chart("All.Sectors") })
output$plot_ConsumerDiscretionary <- renderPlot({ make_chart("Consumer.Discretionary") })
output$plot_ConsumerStaples <- renderPlot({ make_chart("Consumer.Staples") })
output$plot_Energy <- renderPlot({ make_chart("Energy") })
output$plot_Financials <- renderPlot({ make_chart("Financials") })
output$plot_HealthCare <- renderPlot({ make_chart("Health.Care") })
output$plot_Industrials <- renderPlot({ make_chart("Industrials") })
output$plot_InformationTechnology <- renderPlot({ make_chart("Information.Technology") })
output$plot_Materials <- renderPlot({ make_chart("Materials") })
output$plot_TelecommunicationServices <- renderPlot({ make_chart("Telecommunication.Services") })
output$plot_Undefined <- renderPlot({ make_chart("Undefined") })
output$plot_Utilities <- renderPlot({ make_chart("Utilities") })

})
