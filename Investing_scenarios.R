#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(readr)
library(reshape2)
ui <- fluidPage(

    titlePanel("Investing Scenarios"),
    fluidRow(
        column(3, 
                    sliderInput("initial",
                        "Initial Amount",
                        min = 1,
                        max = 10000,
                        value = 1000,
                        step = 100,
                        sep = ",",
                        pre = "$"),
                    sliderInput("annuity",
                        "Annual Contribution",
                        min = 1,
                        max = 5000,
                        value = 200,
                        sep = ",",
                        pre = "$"),
                    sliderInput("growth",
                        "Annual Growth Rate (in %)",
                        min = 0,
                        max = 20,
                        value = 3)
                ),
        column(3,
                    sliderInput("yield",
                        "High Yield Annual Rate (in %)",
                        min = 0,
                        max = 20,
                        value = 2),
                    sliderInput("fixed",
                        "Fixed Income annual rate (in %)",
                        min = 0,
                        max = 20,
                        step = 0.1,
                        value = 4.5),
                    sliderInput("equityrate",
                        "US Equity annual rate (in %)",
                        min = 0,
                        max = 20,
                        value = 10)
                ),
        column(3,
                    
                    sliderInput("yieldvolatility",
                        "High Yield volatility (in %)",
                        min = 0,
                        max = 20,
                        step = 0.1,
                        value = 0.1,
                        round = FALSE),
                    sliderInput("incomevolatility",
                        "Fixed Income volatlity (in %)",
                        min = 0,
                        max = 20,
                        value = 4.5,
                        round = FALSE),
                    sliderInput("equityvolatility",
                        "US Equity volatlity (in %)",
                        min = 0,
                        max = 20,
                        value = 15)
            ),
        column(3, 
                    sliderInput("years",
                        "Years",
                        min = 0,
                        max = 50,
                        value = 20),
                    numericInput("random",
                         "Random seed",
                         value = 12345),
                    selectInput("facet", "Facet?", 
                        choices = c("Yes", "No"),
                        multiple = FALSE,
                        selected = "Yes")
            ),
        mainPanel(
           plotOutput("Timelines")
        )
    )
)

server <- function(input, output) {

    output$Timelines <- renderPlot({
        amt_yield <- input$initial
        rate_yield <- input$yield
        vol_yield <- input$yieldvolatility
        growth_yield <- input$growth
        #year <- input$years
        high_yield <- vector()
        set.seed(input$random)
        for (i in 0:input$years){ # calculates the investment amounts for high yield intreest rate
            if ( i == 0){
                #amt_yield = input$initial
                high_yield[i+1] <- input$initial
            } else {
                r_y <- rnorm(1, rate_yield, vol_yield)
                amt_yield <- amt_yield*(1+r_y/100)+input$annuity*(1+(growth_yield)/100)^(i-1)
                high_yield[i+1] <- amt_yield
            }}
        amt_bonds <- input$initial
        rate_bonds <- input$fixed
        vol_bonds <- input$incomevolatility
        growth_bonds <- input$growth
        #year <- input$years
        us_bonds <- vector()
        #set.seed(input$random)
        for ( i in 0:input$years){ # calculates the investment amounts for us bonds interest rate 
            if ( i == 0){
                #amt_bonds = input$initial
                us_bonds[i+1] <- input$initial
            } else {
                r_bonds <- rnorm(1, rate_bonds, vol_bonds)
                amt_bonds <- amt_bonds*(1+r_bonds/100)+input$annuity*(1+(growth_bonds)/100)^(i-1)
                us_bonds[i+1] <- amt_bonds
                
            }}
        amt_stocks <- input$initial
        rate_stocks <- input$equityrate
        vol_stocks <- input$equityvolatility
        #year <- input$years
        growth_stocks <- input$growth
        us_stocks <- vector()
        #set.seed(input$random)
        for (i in 0:input$years){ # calculates invesment amount for us stocks interest rate
            if ( i == 0){
                #amt_stocks = input$initial
                us_stocks[i+1] <- input$initial
            } else {
                r_s <- rnorm(1, rate_stocks, vol_stocks)
                amt_stocks <- amt_stocks*(1+r_s/100)+input$annuity*(1+(growth_stocks)/100)^(i-1)
                us_stocks[i+1] <- amt_stocks
            }}
        modalitites <- data.frame(year = 0:input$years, high_yield, us_bonds, us_stocks)
        modalitites[1, 2:4] <- input$initial
        modalitites <- melt(modalitites, id = "year")
        names(modalitites)[2] <- "index"
        if (input$facet == "No"){
            ggplot(modalitites, aes(year, value, colour = index))+geom_line()+geom_point()+theme_bw()+labs(y = "amount")+labs(title = "Three indices", tag = "Timelines")
        } else {
            ggplot(modalitites, aes(year, value, colour = index))+geom_line()+geom_point()+theme_bw()+labs(y = "amount")+facet_grid(~index)+geom_area(aes(y = value, fill=index),alpha = 0.4)+labs(title = "Three indices", tag = "Timelines")
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
options(shiny.launch.browser = .rs.invokeShinyWindowViewer)
shinyApp(ui = ui, server = server)
