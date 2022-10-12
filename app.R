library(shiny)
library(plotly)
library(tidyverse)
library(geomtextpath)

ui <- fluidPage(

    titlePanel("Gaskostenbremse für Gasverbrauch 2023"),


    sidebarLayout(
        sidebarPanel(
            sliderInput("consumption_2022",
                        "Verbrauch 2022:",
                        min = 0,
                        max = 20000,
                        value = 10000
                        ),
            sliderInput("price_gas_2022",
                        "Alter Arbeitspreis 2022 (in Cent pro kWh)",
                        min = 0,
                        max = 60,
                        value = 7
            ),
            sliderInput("price_gas_2023",
                        "Neuer Arbeitspreis 2023 (in Cent pro kWh)",
                        min = 0,
                        max = 60,
                        value = 20
            )

        ),

        mainPanel(
          
          fluidRow(column = 2,
           plotOutput("plot_costs"),
           plotOutput("plot_prices")
           )
        )
    )
)

server <- function(input, output) {
  
  
  rebate <- 
    reactive({input$consumption_2022*max(input$price_gas_2023-12) * 0.8/100})
  
  consumption <- 0:20000
  
  cost_norebate <- reactive(consumption*input$price_gas_2023/100)
  cost_rebate <- reactive(pmax(0,cost_norebate()-rebate()))
  cost_price2022 <- reactive(consumption*input$price_gas_2022/100)
  
  prices_avg_rebate <- reactive(cost_rebate()/consumption*100)
  
  output$plot_costs <- 
    renderPlot({
      
      ggplot(mapping = aes(x= consumption))+
        geom_textline(mapping = aes(y = cost_norebate()),
                      color = "blue",
                      label = "Kosten 2023 zu neuem Preis"
                  )+
        geom_textline(mapping = aes(y = cost_rebate()),
                      label= paste("Kosten 2023 zu neuem Preis abzgl. maximal einer Pauschale von",
                                   round(rebate(),0),
                                   "€"
                                   )
                      )+
        geom_textline(mapping = aes(y = cost_price2022()),
                  label= "fiktive Kosten 2023 zu altem Preis",
                  color = "red"
                  )+
        labs(x = "Verbrauch 2023 (in kWh)",
             y = "Gesamtkosten (in €)"
             )+
        theme_minimal()
    })

  output$plot_prices <- 
    renderPlot({
      
      ggplot(mapping = aes(x= consumption))+
        geom_textline(mapping = aes(y = prices_avg_rebate()),
                      label = "durchschnittl. Kosten 2023")+
        geom_texthline(yintercept = input$price_gas_2023,
                       color = "blue",
                       label = "marginale Kosten 2023")+
        geom_texthline(yintercept = input$price_gas_2022,
                       color = "red",
                       label = "marginale Kosten 2022"
                   )+
        labs(x = "Verbrauch 2023 (in kWh)",
             y = "marginale/durchschnittl. Gaskosten (in Cent pro kWh)") +
        theme_minimal()
      
      
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
