#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(quantmod)
library(plotly)
library(fpp3)
library(ggpubr)
library(ggplot2)
library(ggeasy)
library(tidyquant)
library(ggthemes)



ui <- fluidPage(
  
  h2(titlePanel("Major Companies Closing Stock Prices During COVID-19 (2020)")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selectCompany", label = h3("Select a Company:"), 
                  choices = list("American Airlines" = "American Airlines", "Amazon" = "Amazon", "Apple" = "Apple", "Disney" = "Disney",
                                 "Exxon Mobil" = "Exxon Mobil", "General Motors" = "General Motors",  
                                 "Home Depot" = "Home Depot","Lululemon" = "Lululemon","McDonald's" = "McDonald's", 
                                 "Starbucks" = "Starbucks", "Spotify" = "Spotify", "Tesla" = "Tesla","Zoom" = "Zoom"
                  ), 
                  selected = "American Airlines"),
      
      dateInput("date1", "From", value = "2020-01-01"),
      dateInput("date2", "To", value="2020-05-31"),
      h3(textOutput("summaryLabel1")),
      h4(paste("Maximum Closing Amount:")),
      h4(textOutput("Max"), style = "color:blue"),
      h4(paste("Minimum Closing Amount:")),
      h4(textOutput("Min"), style = "color:blue"),
      h4(paste("Maximum Difference in Closing Prices:")),
      h4(textOutput("DiffMax"), style = "color:blue"),
      h4(paste("Minimum Difference in Closing Prices:")),
      h4(textOutput("DiffMin"), style = "color:blue"),
      h4(paste("Average Difference in Closing Prices:")),
      h4(textOutput("DiffMean"), style = "color:blue")
    ),
    
    mainPanel(
      plotlyOutput("STOCKPlot"),
      plotlyOutput("STOCKDiffPlot"),
      plotlyOutput("STOCKReturn")
    )
  )
)



server <- function(input, output) {
  
 newdata<-reactive({
   start<-input$date1
   end<-input$date2
   STOCK.SPOT <- getSymbols("SPOT", src = "yahoo", 
                            from = start, to = end, auto.assign = FALSE)
   STOCK.SPOT <- STOCK.SPOT[,4]
   STOCK.DIS <- getSymbols("DIS", src = "yahoo", 
                           from = start, to = end, auto.assign = FALSE)
   STOCK.DIS <- STOCK.DIS[,4]
   STOCK.AMZN <- getSymbols("AMZN", src = "yahoo", 
                            from = start, to = end, auto.assign = FALSE)
   STOCK.AMZN <- STOCK.AMZN[,4]
   STOCK.AAPL <- getSymbols("AAPL", src = "yahoo", 
                            from = start, to = end, auto.assign = FALSE)
   STOCK.AAPL <- STOCK.AAPL[,4]
   STOCK.MCD <- getSymbols("MCD", src = "yahoo", 
                           from = start, to = end, auto.assign = FALSE)
   STOCK.MCD <- STOCK.MCD[,4]
   STOCK.SBUX <- getSymbols("SBUX", src = "yahoo", 
                            from = start, to = end, auto.assign = FALSE)
   STOCK.SBUX <- STOCK.SBUX[,4]
   STOCK.GM <- getSymbols("GM", src = "yahoo", 
                          from = start, to = end, auto.assign = FALSE)
   STOCK.GM <- STOCK.GM[,4]
   STOCK.AAL <- getSymbols("AAL", src = "yahoo", 
                           from = start, to = end, auto.assign = FALSE)
   STOCK.AAL <- STOCK.AAL[,4]
   STOCK.ZM <- getSymbols("ZM", src = "yahoo", 
                          from = start, to = end, auto.assign = FALSE)
   STOCK.ZM <- STOCK.ZM[,4]
   STOCK.XOM <- getSymbols("XOM", src = "yahoo", 
                           from = start, to = end, auto.assign = FALSE)
   STOCK.XOM <- STOCK.XOM[,4]
   STOCK.TSLA <- getSymbols("TSLA", src = "yahoo", 
                            from = start, to = end, auto.assign = FALSE)
   STOCK.TSLA <- STOCK.TSLA[,4]
   STOCK.HD <- getSymbols("HD", src = "yahoo", 
                          from = start, to = end, auto.assign = FALSE)
   STOCK.HD <- STOCK.HD[,4]
   STOCK.LULU <- getSymbols("LULU", src = "yahoo", 
                            from = start, to = end, auto.assign = FALSE)
   STOCK.LULU <- STOCK.LULU[,4]
   
   if(input$selectCompany == "American Airlines"){ x <- STOCK.AAL }
   if(input$selectCompany == "Amazon"){ x <- STOCK.AMZN }
   if(input$selectCompany == "Apple"){ x <- STOCK.AAPL }
   if(input$selectCompany == "Disney"){ x <- STOCK.DIS }
   if(input$selectCompany == "Exxon Mobil"){ x <- STOCK.XOM }
   if(input$selectCompany == "General Motors"){ x <- STOCK.GM }
   if(input$selectCompany == "Home Depot"){ x <- STOCK.HD }
   if(input$selectCompany == "Lululemon"){ x <- STOCK.LULU }
   if(input$selectCompany == "McDonald's"){ x <- STOCK.MCD }
   if(input$selectCompany == "Starbucks"){ x <- STOCK.SBUX }
   if(input$selectCompany == "Spotify"){ x <- STOCK.SPOT }
   if(input$selectCompany == "Tesla"){ x <- STOCK.TSLA }
   if(input$selectCompany == "Zoom"){ x <- STOCK.ZM }
   return(x)
 })
 
 
 output$STOCKPlot <- renderPlotly({
 
   

   
   newdata() %>% autoplot()+labs(title = paste("Plot of", input$selectCompany , "Closing Prices in 2020:" ),
                         x = "Date")+easy_center_title()+theme_economist_white()
   
   
 })
 output$STOCKDiffPlot <- renderPlotly({

   
   
   diff(newdata()) %>% autoplot()+labs(title = paste("Plot of", input$selectCompany , "Closing Prices Differences in 2020:" ),
                               x = "Date")+easy_center_title()+theme_economist_white()
   
 })
 output$STOCKReturn <- renderPlotly({

   monthlyReturn(newdata()) %>% autoplot()+labs(title = paste("Plot of", input$selectCompany , "Monthly Returns in 2020:" ),
                                        x = "Date")+easy_center_title()+theme_economist_white()
   
 })
 
 output$Max <- renderPrint({

   
   cat(newdata()[which.max(newdata())])
 })
 output$Min <- renderPrint({

   
   cat(newdata()[which.min(newdata())])
 })
 
 output$DiffMax <- renderPrint({

   
   diffSTOCK.x <- diff(newdata())
   cat(diffSTOCK.x[which.max(diffSTOCK.x)])
 })
 
 output$DiffMin <- renderPrint({

   
   diffSTOCK.x <- diff(newdata())
   cat(diffSTOCK.x[which.min(diffSTOCK.x)])
 })
 
 output$DiffMean <- renderPrint({

   diffSTOCK.x <- diff(newdata())
   cat(mean(diffSTOCK.x, na.rm = TRUE))
 })
 
 
 output$summaryLabel1 <- renderText({
   paste("Summary Statistics for", input$selectCompany,":")
 })
 
 
  
}



# Run the application 
shinyApp(ui = ui, server = server)
