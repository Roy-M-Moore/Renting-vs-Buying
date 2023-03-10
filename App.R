library(shiny)

# Define UI
ui <- fluidPage(
  
  # App title 
  titlePanel("Renting vs Buying Calculator"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for input
    sidebarPanel(
      
      # Input:
      ## Property Value
      numericInput(inputId = "property_value",
                   label = "Property Value",
                   value = 300000,
                   step = 1000),
      
      ## Property Tax Rate
      sliderInput(inputId = "down_payment",
                  label = "Down Payment",
                  min = 0,
                  max = 100,
                  value = 20,
                  ticks = FALSE),
      
      ## Property Tax Rate
      sliderInput(inputId = "tax_rate",
                  label = "Property Tax Rate (OZB)",
                  min = 0,
                  max = 2,
                  value = 0.04,
                  step = 0.01,
                  ticks = FALSE),
      
      ## Maintenance Costs
      sliderInput(inputId = "maintenance_costs",
                  label = "Yearly Maintenance Costs",
                  min = 0,
                  max = 5,
                  value = 1.5,
                  step = 0.05,
                  ticks = FALSE),
      
      ## Mortgage Rate
      sliderInput(inputId = "mortgage_rate",
                  label = "Mortgage Rate",
                  min = 0,
                  max = 15,
                  value = 4,
                  step = 0.1,
                  ticks = FALSE),
      
      ## Real Property Price Appreciation 
      sliderInput(inputId = "price_appreciation",
                  label = "Inflation-Adjusted Property Price Appreciation*",
                  min = 0,
                  max = 15,
                  value = 1.3,
                  step = 0.1,
                  ticks = FALSE),
      
      ## Return on (non-home) Investments / Opportunity Cost
      sliderInput(inputId = "return_on_investments",
                  label = "Inflation-Adjusted Return on Investments*",
                  min = 0,
                  max = 15,
                  value = 4.5,
                  step = 0.1,
                  ticks = FALSE),
      ## Footnote
      p("* It is also possible to introduce the nominal (i.e., not inflation-adjusted)
        returns for both of these parameters. Just make sure to introduce either the
        real OR nominal expected returns for BOTH of them.")
      
    ),
    
    # Main panel
    mainPanel(
      
      # Output: Equivalent rent for given house value and inputs
      p("If you can find a similar property for",
        span(htmlOutput(outputId = "equivalent_rent", inline = TRUE), style = "color: red"),
        "or lower, you should rent!", style = "font-family: Helvetica; font-size: 16pt"),
      
      tabsetPanel(
        
        tabPanel("Parameter explanation",
          
          # Explanation of the parameters of the calculator
          headerPanel(""), # Add some white space
          p(strong("Down Payment:"), "Percentage of total property value that will be paid initially."),
          p(strong("Property Tax Rate (OZB):"),
            "In the Netherlands, property taxes (OZB) are calculated as a percentage
            of the home's value, known as the WOZ. You may check the property's
            WOZ value in the previous year's",
            a(href = "https://www.wozwaardeloket.nl", "here.", target = "_blank"),
            "Each municipality determines its own OZB tax rate. In general, it ranges
            between 0.1% and 0.3% of the property value. In Amsterdam, it is 0.0431%."),
          p(strong("Yearly Maintenance Costs:"), "Percentage of the property value that
            will be spent annually on repairs, renovations and insurance. Jord?? et al. (2019)
            estimate this value to be between 1% and 2% based on historical data. A rate of around 
            1.5% seems appropriate in most circumstances."),
          p(strong("Mortgage Rate:"), "Interest rate charged on a mortgage as a percentage.
            In the Netherlands, a quick Google search reveals that as of early 2023 it hovers slightly above 4%,
            depending on the duration of the mortgage."),
          p(strong("Inflation-Adjusted Property Price Appreciation:"),
            "Expected increase in value of the property after inflation as a percentage.
            The price appreciation of property has historically been substantially lower
            than the returns of other forms of investment, namely stocks. Dimson et al. (2018, p. 26)
            found a historical average real housing price increase of 1.3% p.a. across 11 countries. Jord?? et al.
            (2019), found a similar value of 1.15% across 16 countries."),
          p(strong("Inflation-Adjusted Return on Investments:"),
            "Expected return on investment on alternative assests as a percentage. Instead of
            paying for the down payment of a property, you could invest this money into other
            assets, such as equity. Hence, an estimate of the returns this investment would bring
            is necessary to calculate the opportunity cost of buying instead of renting.
            The annualized real returns of equities worldwide from 1900 to 2017 was estimated to be
            5.2% by Dimson et al. (2018, p. 11). Still, using a slightly more conservative estimate
            of real future returns for a 100% equity portfolio is encouraged."),
          
          # Final Remarks & Links
          p(strong("Final Remarks"), style = "font-family: Helvetica; font-size: 12pt"),
          p("This calculator has focused on the recurring costs of owning a home, which affect current
            and new homeowners alike. It is important to note that there are substaintial costs
            associated with the purchasing and transfer of a property. For instance, the
            transfer tax", em("(Overdrachtsbelasting)"), "sits at 2% of the purchase price.
            The impact of these costs on your decision to rent or buy will depend mainly
            on the amount of time you expect to stay in the newly purchased home."),
          p("The source code of this calculator can be found",
            a(href = "https://github.com/Roy-M-Moore/Renting-vs-Buying", "here.", target = "_blank"))),
          
          tabPanel("References",
          
          # References
          headerPanel(""), # Add some white space
          p("Dimson, E., Marsh, P. R., & Staunton, M. (2018).
            Credit Suisse Global Investment Returns Yearbook 2018:  Summary Edition.
            Credit Suisse Research Institute."),
          p("Jord??, ??., Knoll, K., Kuvshinov, D., Schularick, M., & Taylor, A. M. (2019).
            The rate of return on everything, 1870???2015. The Quarterly Journal of Economics, 134(3), 1225-1298.")),
          
          tabPanel("Disclaimer",
          
          # Disclaimer
          headerPanel(""), # Add some white space
          p("The creator of this calculator is not a tax accountant nor a financial advisor.
            Use the information presented here with due caution."))
      )
    )
  )
)



server <- function(input, output) {
  
  output$equivalent_rent <- renderText({
    
    # Calculate costs & opportunity costs over down payment
    round((input$property_value * (input$down_payment / 100) *
       ((input$tax_rate + input$maintenance_costs + input$return_on_investments - input$price_appreciation) / 100) / 12) +
    # Calculate costs & mortgage rate over principal
    (input$property_value * ((100 - input$down_payment) / 100) *
       ((input$tax_rate + input$maintenance_costs + input$mortgage_rate) / 100) / 12))
    
  })
    
}






shinyApp(ui = ui, server = server)