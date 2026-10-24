# 1. Install and load packages ----------------------------------------------
for (p in c("shiny", "readxl")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}
library(shiny)

# 2. Set the results folder -------------------------------------------------
RUN_FOLDER <- "results/run_20261024_113027"

# 3. Load the saved forecasts -----------------------------------------------
points <- as.data.frame(readxl::read_excel(
  file.path(RUN_FOLDER, "10_tuition_point_forecasts.xlsx")
))

ranges <- as.data.frame(readxl::read_excel(
  file.path(RUN_FOLDER, "10_garch_tuition_scenarios.xlsx")
))

# 4. Format displayed amounts ----------------------------------------------
money <- function(x) {
  paste0("LKR ", formatC(x, format = "f", digits = 0, big.mark = ","))
}

rate_text <- function(x) {
  formatC(x, format = "f", digits = 2)
}

# 5. Define colours and layout ---------------------------------------------
css <- "
  body {
    background: #0F1C44;
    color: #F2F4FF;
    font-family: Arial, sans-serif;
  }

  .card {
    max-width: 780px;
    margin: 35px auto;
    padding: 28px;
  }

  h2 {
    font-family: Georgia, serif;
    font-size: 32px;
    font-weight: bold;
    margin-top: 0;
    color: #F2F4FF;
  }

  .subtitle {
    color: #AAB8E8;
    line-height: 1.6;
    margin-bottom: 18px;
  }

  .control-label {
    color: #AAB8E8;
    font-size: 13px;
  }

  .form-control {
    background: #202D55;
    color: white;
    border: 1px solid #465378;
    border-radius: 9px;
  }

  .form-control:focus {
    border-color: #7585FF;
    box-shadow: 0 0 0 2px rgba(90,115,255,.25);
  }

  .radio-inline {
    position: relative;
    padding: 0;
    margin: 0 6px 8px 0 !important;
  }

  .radio-inline input[type='radio'] {
    position: absolute;
    opacity: 0;
  }

  .radio-inline span {
    display: inline-block;
    padding: 8px 15px;
    border: 1px solid #465378;
    border-radius: 25px;
    color: #F2F4FF;
    cursor: pointer;
  }

  .radio-inline input:checked + span {
    background: #5A73FF;
    border-color: #5A73FF;
    color: white;
    font-weight: bold;
  }

  .radio-inline input:focus-visible + span {
    outline: 2px solid #B9C3FF;
    outline-offset: 3px;
  }

  .payment-row {
    display: flex;
    align-items: flex-end;
    gap: 24px;
    flex-wrap: wrap;
    margin: 12px 0 30px;
  }

  .fee-box {
    width: 155px;
  }

  .fee-box .form-group {
    margin-bottom: 0;
  }

  .amount-label {
    font-size: 12px;
    color: #AAB8E8;
    margin-bottom: 5px;
  }

  .amount {
    font-size: 46px;
    font-weight: bold;
    line-height: 1.1;
    overflow-wrap: anywhere;
  }

  .range-caption {
    color: #AAB8E8;
    font-size: 12px;
    margin-bottom: 14px;
  }

  .range-track {
    position: relative;
    height: 24px;
    border-radius: 20px;
    background: #2A365C;
    margin-bottom: 16px;
  }

  .range-band {
    position: absolute;
    top: 0;
    height: 24px;
    border-radius: 20px;
    background: #4054A2;
  }

  .latest-marker {
    position: absolute;
    top: -6px;
    height: 36px;
    width: 3px;
    background: white;
    border-radius: 3px;
    transform: translateX(-50%);
    z-index: 2;
  }

  .forecast-marker {
    position: absolute;
    top: -8px;
    height: 40px;
    width: 9px;
    background: #5A73FF;
    border: 2px solid #E2E7FF;
    border-radius: 4px;
    transform: translateX(-50%);
    box-shadow: 0 0 8px rgba(117,133,255,.6);
    z-index: 3;
  }

  .range-legend {
    display: flex;
    align-items: center;
    flex-wrap: wrap;
    gap: 10px 18px;
    font-size: 12px;
    color: #BAC6EF;
    line-height: 1.8;
  }

  .range-legend strong {
    color: white;
  }

  .dot {
    display: inline-block;
    width: 8px;
    height: 8px;
    margin-right: 5px;
    border-radius: 50%;
  }

  @media (max-width: 550px) {
    .card {
      padding: 18px;
      margin: 15px auto;
    }
    h2 { font-size: 27px; }
    .amount { font-size: 34px; }
  }
"

# 6. Build the calculator ---------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$title("Tuition cost in rupees"),
    tags$style(HTML(css))
  ),
  
  div(
    class = "card",
    
    h2("What will my tuition cost in rupees?"),
    
    p(
      class = "subtitle",
      "Tested forecasts for paying overseas fees in USD, GBP or EUR.",
      br(),
      textOutput("as_of", inline = TRUE)
    ),
    
    # Currency selection
    radioButtons(
      "currency", NULL,
      choices = c("USD", "GBP", "EUR"),
      selected = "USD",
      inline = TRUE
    ),
    
    # Forecast horizon selection
    radioButtons(
      "horizon", NULL,
      choices = c("5 days ahead" = "5", "20 days ahead" = "20"),
      selected = "5",
      inline = TRUE
    ),
    
    # Fee input and forecast cost
    div(
      class = "payment-row",
      
      div(
        class = "fee-box",
        numericInput(
          "fee", "Tuition fee (USD)",
          value = 5000, min = 0, step = 100,
          width = "100%"
        )
      ),
      
      div(
        div(class = "amount-label", "Estimated tuition cost"),
        div(class = "amount", textOutput("cost", inline = TRUE))
      )
    ),
    
    # Range bar replaces the previous four tiles
    uiOutput("range_bar")
  )
)

# 7. Connect inputs to saved results ----------------------------------------
server <- function(input, output, session) {
  
  # Update the currency label without resetting the fee
  observeEvent(input$currency, {
    updateNumericInput(
      session, "fee",
      label = paste0("Tuition fee (", input$currency, ")")
    )
  })
  
  # Find matching point forecasts and GARCH bounds
  selected <- reactive({
    h <- as.integer(input$horizon)
    
    p <- points[
      points$currency == input$currency &
        points$reporting_days_ahead == h,
      , drop = FALSE
    ]
    
    g <- ranges[
      ranges$currency == input$currency & ranges$h == h,
      , drop = FALSE
    ]
    
    validate(need(
      nrow(p) == 1 && nrow(g) == 1,
      "A unique forecast is unavailable."
    ))
    
    # Validate numeric values before calculating positions
    values <- c(
      p$forecast_rate, p$latest_rate_cost_LKR,
      p$foreign_fee, g$lower95, g$upper95
    )
    
    validate(need(
      all(is.finite(values)) &&
        all(values > 0) &&
        g$upper95 > g$lower95,
      "Invalid forecast values."
    ))
    
    validate(need(
      as.Date(p$as_of) == as.Date(g$as_of),
      "Forecast origin dates do not match."
    ))
    
    list(
      point = p,
      low = g$lower95,
      high = g$upper95,
      latest = p$latest_rate_cost_LKR / p$foreign_fee,
      forecast = p$forecast_rate
    )
  })
  
  # Display the forecast origin date
  output$as_of <- renderText({
    paste0(
      "Figures are as of ",
      format(as.Date(selected()$point$as_of), "%d %B %Y"),
      "."
    )
  })
  
  # Calculate the fee in LKR
  output$cost <- renderText({
    req(!is.null(input$fee))
    
    validate(need(
      is.finite(input$fee) && input$fee >= 0,
      "Enter a valid non-negative fee."
    ))
    
    value <- input$fee * selected()$forecast
    validate(need(is.finite(value), "The fee is too large."))
    
    money(value)
  })
  
  # Draw a proportional range bar and labelled markers
  output$range_bar <- renderUI({
    s <- selected()
    
    # Include every displayed rate, with padding at both ends
    values <- c(s$low, s$high, s$latest, s$forecast)
    span <- max(values) - min(values)
    axis_min <- min(values) - span * 0.30
    axis_max <- max(values) + span * 0.30
    
    # Convert rates into percentage positions on the bar
    position <- function(x) {
      100 * (x - axis_min) / (axis_max - axis_min)
    }
    
    low_pos <- position(s$low)
    high_pos <- position(s$high)
    latest_pos <- position(s$latest)
    forecast_pos <- position(s$forecast)
    
    # Helper for legend entries
    legend_item <- function(label, value, colour) {
      span(
        span(class = "dot",
             style = paste0("background:", colour, ";")),
        paste0(label, " "),
        tags$strong(rate_text(value))
      )
    }
    
    tagList(
      div(
        class = "range-caption",
        "GARCH 95% scenario range • markers: latest and point forecast"
      ),
      
      div(
        class = "range-track",
        role = "img",
        `aria-label` = paste(
          "Low", rate_text(s$low),
          "Latest", rate_text(s$latest),
          "Forecast", rate_text(s$forecast),
          "High", rate_text(s$high)
        ),
        
        # Shaded GARCH interval
        div(
          class = "range-band",
          style = sprintf(
            "left:%.4f%%; width:%.4f%%;",
            low_pos, high_pos - low_pos
          )
        ),
        
        # White marker for the latest observed rate
        div(
          class = "latest-marker",
          title = paste("Latest", rate_text(s$latest)),
          style = sprintf("left:%.4f%%;", latest_pos)
        ),
        
        # Purple marker for the point-model forecast
        div(
          class = "forecast-marker",
          title = paste("Forecast", rate_text(s$forecast)),
          style = sprintf("left:%.4f%%;", forecast_pos)
        )
      ),
      
      # Labels below the bar
      div(
        class = "range-legend",
        legend_item("Low", s$low, "#4054A2"),
        legend_item("Latest", s$latest, "#FFFFFF"),
        legend_item("Forecast", s$forecast, "#5A73FF"),
        legend_item("High", s$high, "#4054A2"),
        span(paste("LKR per", input$currency))
      )
    )
  })
}

# 8. Launch the calculator --------------------------------------------------
shinyApp(ui, server)