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



ui <- fluidPage(

    h2(titlePanel("Major Companies Closing Stock Prices During COVID-19 (2020)")),

    sidebarLayout(
        sidebarPanel(
            selectInput("selectCompany", label = h3("Select a Company:"), 
                        choices = list("American Airlines" = "American Airlines", "Amazon" = "Amazon", "Disney" = "Disney",
                                       "Exxon Mobil" = "Exxon Mobil", "General Motors" = "General Motors",  
                                       "Home Depot" = "Home Depot","Lululemon" = "Lululemon","McDonald's" = "McDonald's", 
                                       "Starbucks" = "Starbucks", "Spotify" = "Spotify", "Tesla" = "Tesla","Zoom" = "Zoom"
                                        ), 
                        selected = "American Airlines"),
            
            dateRangeInput("dates", "Date Range of Stocks", start = ("2020-01-01"), end = ("2020-12-31"), min = NULL,
                     max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                     language = "en", separator = " to ", width = NULL),
 
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
           plotlyOutput("STOCKDiffPlot")
        )
    )
)



server <- function(input, output) {

    output$STOCKPlot <- renderPlotly({
        
        if(input$selectCompany == "American Airlines"){ x <- STOCK.AAL }
        if(input$selectCompany == "Amazon"){ x <- STOCK.AMZN }
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

        x %>% autoplot()+labs(title = paste("Plot of", input$selectCompany , "Closing Prices in 2020:" ),
                              x = "Date")+easy_center_title()
        
        
    })
    
    output$STOCKDiffPlot <- renderPlotly({
        
        if(input$selectCompany == "American Airlines"){ x <- STOCK.AAL }
        if(input$selectCompany == "Amazon"){ x <- STOCK.AMZN }
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
        
        
        diff(x) %>% autoplot()+labs(title = paste("Plot of", input$selectCompany , "Closing Prices Differences in 2020:" ),
                                    x = "Date")+easy_center_title()
        
    })
    
    output$Max <- renderPrint({
        
        if(input$selectCompany == "American Airlines"){ x <- STOCK.AAL }
        if(input$selectCompany == "Amazon"){ x <- STOCK.AMZN }
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
        
        cat(x[which.max(x)])
    })
    
    output$Min <- renderPrint({
        
        if(input$selectCompany == "American Airlines"){ x <- STOCK.AAL }
        if(input$selectCompany == "Amazon"){ x <- STOCK.AMZN }
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
        
        cat(x[which.min(x)])
    })

    output$DiffMax <- renderPrint({
        
        if(input$selectCompany == "American Airlines"){ x <- STOCK.AAL }
        if(input$selectCompany == "Amazon"){ x <- STOCK.AMZN }
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
        
        diffSTOCK.x <- diff(x)
        cat(diffSTOCK.x[which.max(diffSTOCK.x)])
    })
    
    output$DiffMin <- renderPrint({
        
        if(input$selectCompany == "American Airlines"){ x <- STOCK.AAL }
        if(input$selectCompany == "Amazon"){ x <- STOCK.AMZN }
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
        
        diffSTOCK.x <- diff(x)
        cat(diffSTOCK.x[which.min(diffSTOCK.x)])
    })
    
    output$DiffMean <- renderPrint({
        
        if(input$selectCompany == "American Airlines"){ x <- STOCK.AAL }
        if(input$selectCompany == "Amazon"){ x <- STOCK.AMZN }
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
        
        diffSTOCK.x <- diff(x)
        cat(mean(diffSTOCK.x, na.rm = TRUE))
    })
    
    
    output$summaryLabel1 <- renderText({
        paste("Summary Statistics for", input$selectCompany,":")
    })
    
    
    
    start <- as.Date("2020-01-01")
    end <- as.Date("2020-12-31")
    
    STOCK.SPOT <- getSymbols("SPOT", src = "yahoo", 
                             from = start, to = end, auto.assign = FALSE)
    STOCK.SPOT <- STOCK.SPOT[,4]
    STOCK.DIS <- getSymbols("DIS", src = "yahoo", 
                            from = start, to = end, auto.assign = FALSE)
    STOCK.DIS <- STOCK.DIS[,4]
    STOCK.AMZN <- getSymbols("AMZN", src = "yahoo", 
                             from = start, to = end, auto.assign = FALSE)
    STOCK.AMZN <- STOCK.AMZN[,4]
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
    
    
}



# Run the application 
shinyApp(ui = ui, server = server)
