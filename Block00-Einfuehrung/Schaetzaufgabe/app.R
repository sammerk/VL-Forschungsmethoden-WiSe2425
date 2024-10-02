library(shiny)
library(bslib)
library(ggplot2)
library(tidyr)
library(dplyr)

custom_theme <- bs_theme(
  font_scale = .8
)

# Define UI for application that draws a histogram
ui <- page_fixed(
  theme = custom_theme, 
    card(
      card_header("Erst- vs. Drittklässler", class = "bg-dark"),
      card_body(
          card(shinycssloaders::withSpinner(
               plotOutput("plot1", 
                          width = "300px",
                          height = "300px"),
               color = "#8cd000")
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
  
  data1 <- reactive({
    tibble(`Erstklässler` = distribution_normal(300, 121, 7.9),
           `Zweitklässler` = distribution_normal(300, 121, 7.9) +
             max(1.6*input$larger_plot1 - 1.6*input$smaller_plot1, 0)
    ) %>% 
      gather(Klasse, `Körpergröße in cm`)
  })
  
  output$plot1 <- renderPlot({
      ggplot(data1(), aes(x = `Körpergröße in cm`)) +
      #geom_dotplot(binaxis = "x",
      #             stackdir = "up",
      #             dotsize = .7) + 
      geom_histogram() +
      facet_wrap(~Klasse, ncol = 1) +
      theme_minimal(base_size = 12) +
      ylab("") +
      ggtitle(paste0("Aktuelle Überlappung = ", round(2*pnorm(-abs(cohend())/2), 2)*100, "%"))
                    
  })
  
  cohend <- reactive({
    (data1() %>% 
       filter(Klasse == "Erstklässler") %>% 
       pull(`Körpergröße in cm`) %>% 
       mean(.) - 
       data1() %>% 
       filter(Klasse == "Zweitklässler") %>% 
       pull(`Körpergröße in cm`) %>% 
       mean(.))/15
  })
  
  output$debug <- renderPrint({
    paste0("Aktuelle Überlappung = ", round(2*pnorm(-abs(cohend())/2), 2)*100, "%")
  })
}   


# Run the application 
shinyApp(ui = ui, server = server)
