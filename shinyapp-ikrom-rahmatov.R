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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Drawing Balls Experiment"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of repetitions:",
                        min = 1,
                        max = 5000,
                        value = 100,
                        sep = ","),
            sliderInput("bins",
                        "Threshold for choosing boxes:",
                        min = 0,
                        max = 1,
                        value = 0.5)
        ),

        mainPanel(
           plotOutput("Plot")
        )
    )
)


server <- function(input, output) {

    output$Plot <- renderPlot({
        repetitions <- 1000
        trial0 = 0
        trial1 = 0
        trial2 = 0
        trial3 = 0
        trial4 = 0
        for (i in 1:repetitions){
            num <- sum(str_count(drawn_balls[i,], "blue")) 
            if (num == 0){
                trial0 = trial0 + 1
            }
            else if (num == 1){
                trial1 = trial1 + 1
            }
            else if (num == 2){
                trial2 = trial2 + 1
            }
            else if (num == 3){
                trial3 = trial3 + 1
            }
            else {
                trial4 = trial4 + 1
            }
        }
        freq <- matrix(c(trial0, trial1, trial2, trial3, trial4)/repetitions, ncol = 5)
        colnames(freq) <- c(0, 1, 2, 3, 4)
        rownames(freq) <- c("frequencies")
        freq <- as.table(freq)
        freq
        dat <-  data.frame(matrix(nrow = 0,ncol = 5))
        for (i in 1:repetitions){
            num = i
            trial0 = 0
            trial1 = 0
            trial2 = 0
            trial3 = 0
            trial4 = 0
            for (i in 1:num){
                num <- sum(str_count(drawn_balls[i,], "blue"))
                if (num == 0){
                    trial0 = trial0 + 1
                }
                else if (num == 1){
                    trial1 = trial1 + 1
                }
                else if (num == 2){
                    trial2 = trial2 + 1
                }
                else if (num == 3){
                    trial3 = trial3 + 1
                }
                else {
                    trial4 = trial4 + 1
                }
            }
            freq <- c(trial0, trial1, trial2, trial3, trial4)/i
            dat <- rbind(dat, freq)
        }
        colnames(dat) <- c('zero', 'one', 'two', 'three', 'four')
        plot(dat[, 'zero'], main = "Relative frequencies of number of blue balls", ylab = 'freq'
             , xlab = 'reps', type = 'l', lwd = 2, col = 'red', las = 1, ylim = c(0, 1))
        lines(dat[, 'one'], col = 'gold')
        lines(dat[, 'two'], col = 'green')
        lines(dat[, 'three'], col = 'blue')
        lines(dat[, 'four'], col = 'purple')
        legend('topright', legend = c('0', '1', '2', '3', '4'), col = c('red', 'gold', 'green'
                                                                        , 'blue', 'purple'), lty = 1)
    })
}


shinyApp(ui = ui, server = server)
