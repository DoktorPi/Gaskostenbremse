library(shiny)
library(tidyverse)
library(geomtextpath)

ui <- fluidPage(

    titlePanel("Gaskostenbremse für Gasverbrauch 2023"),


    sidebarLayout(
        sidebarPanel(
            sliderInput("consumption_2022",
                        "Verbrauch 2022 (80% davon werden für Pauschale berücksichtigt):",
                        min = 0,
                        max = 20000,
                        value = 10000
                        ),
            sliderInput("price_rebate",
                        "Referenzpreis für Gaspauschale (in Cent pro kWh, Vorschlag Kommission: 12)",
                        min = 0,
                        max = 60,
                        value = 12),
            textOutput("max_rebate"),
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
          
           plotOutput("plot_costs"),
           plotOutput("plot_prices")
        )
    )
)

server <- function(input, output) {
  
  
  rebate <- 
    reactive({input$consumption_2022*max(input$price_gas_2023-input$price_rebate,0) * 0.8/100})
  
  output$max_rebate <- renderText({paste(" \n Maximale Pauschale:",round(rebate()), "€\n ")})
  
  consumption <- 0:20000
  
  cost_norebate <- reactive(consumption*input$price_gas_2023/100)
  cost_rebate <- reactive(pmax(0,cost_norebate()-rebate()))
  cost_price2022 <- reactive(consumption*input$price_gas_2022/100)
  
  cost_avg_rebate <- reactive(cost_rebate()/consumption*100)
  cost_marginal_2023_rebate <- reactive(100*c(0,diff(cost_rebate())))
  
  
  
  output$plot_costs <- 
    renderPlot({
      
      ggplot(mapping = aes(x= consumption))+
        geom_textline(mapping = aes(y = cost_norebate()),
                      color = "blue",
                      label = "Kosten 2023 zu neuem Preis",
                      hjust = 0.8
                  )+
        geom_textline(mapping = aes(y = cost_rebate()),
                      label= paste("Kosten 2023 zu neuem Preis abzgl. maximal einer Pauschale von",
                                   round(rebate(),0),
                                   "€"
                                   ),
                      hjust = 0.8
                      )+
        geom_textline(mapping = aes(y = cost_price2022()),
                  label= "fiktive Kosten 2023 zu altem Preis",
                  color = "red",
                  hjust = 0.8
                  )+
        labs(x = "Verbrauch 2023 (in kWh)",
             y = "Gesamtkosten (in €)"
             )+
        theme_minimal()
    })

  output$plot_prices <- 
    renderPlot({
      

      ggplot(mapping = aes(x= consumption))+
        geom_textline(mapping = aes(y = cost_avg_rebate()),
                      label = "durchschnittl. Kosten 2023",
                      hjust = 0.8
                      )+
        geom_textline(mapping = aes(y = cost_marginal_2023_rebate()),
                      color = "blue",
                      label = "marginale Kosten zu Preis in 2023 mit Pauschale",
                      hjust = 0.8
                      )+
        geom_texthline(yintercept = input$price_gas_2022,
                       color = "red",
                       label = "marginale Kosten zu Preis in 2022 ohne Pauschale",
                       hjust = 0.8
                   )+
        labs(x = "Verbrauch 2023 (in kWh)",
             y = "marginale/durchschnittl. Gaskosten (in Cent pro kWh)") +
        theme_minimal() 
      
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
