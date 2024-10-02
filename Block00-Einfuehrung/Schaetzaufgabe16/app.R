library(shiny)
library(bslib)

custom_theme <- bs_theme(
  font_scale = .8
)

# Define UI for application that draws a histogram
ui <- page_fixed(
  theme = custom_theme, 
    card(
      card_header("Erst- vs. Sechstklässler", class = "bg-dark"),
      card_body(
          card(shinycssloaders::withSpinner(
               plotOutput("plot1", 
                         width = "300px",
                         height = "300px"
                         ),
               color = "#8cd000"),
               textOutput("overlap")
               ),
        layout_columns(
          actionButton("smaller_plot1",  icon("scale-balanced")),
          actionButton("larger_plot1",  icon("scale-unbalanced")))
      ))
)

# Define server logic required to draw a histogram
distribution_normal <- function(n,
            mean = 0,
            sd = 1,
            random = FALSE,
            ...){
    if (random) {
      stats::rnorm(n, mean, sd)
    }
    else {
      stats::qnorm(stats::ppoints(n), mean, sd, ...)
    }
  }

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    Erstklässler=distribution_normal(300, 121, 7.9)     
    Sechstklässler=distribution_normal(300, 121, 7.9) +
      max(1*input$larger_plot1 - 1*input$smaller_plot1, 0)
    
    # First distribution
    par(mfrow = c(2,1), mar=c(2,1,1.5,1))
    hist(Erstklässler, 
         breaks=30, 
         xlim=c(min(c(Erstklässler, Sechstklässler)),max(c(Erstklässler, Sechstklässler))), 
         ylab = "",
         xlab = "",
         col=rgb(1,0,0,0.5), 
         main="Erstklässler" )
    
    # Second with add=T to plot on top
    hist(Sechstklässler, 
         breaks=30, 
         xlim=c(min(c(Erstklässler, Sechstklässler)),max(c(Erstklässler, Sechstklässler))), 
         ylab = "",
         col=rgb(0,0,1,0.5),
         main="Sechstklässler" )
  })
  
  cohend <- reactive({
    (mean(distribution_normal(300, 121, 7.9)) - 
       mean(distribution_normal(300, 121, 7.9) +
              max(1*input$larger_plot1 - 1*input$smaller_plot1, 0)))/15
  })
  
  output$overlap <- renderText({
    paste0("Aktuelle Überlappung = ", round(2*pnorm(-abs(cohend())/2), 2)*100, "%")
  })
}   


# Run the application 
shinyApp(ui = ui, server = server)
