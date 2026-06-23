library(shiny)
library(bolteR)

# Hit probability choices (40K dice notation → raw probability)
hit_choices <- c("2+" = 0.833, "3+" = 0.667, "4+" = 0.5, "5+" = 0.333, "6+" = 0.167)

ui <- fluidPage(

  titlePanel("bolteR — Warhammer 40K Dice Simulator"),

  tabsetPanel(

    # ── Tab 1: Wound Table ─────────────────────────────────────────────────────
    tabPanel(
      "Wound Table",
      br(),
      sidebarLayout(
        sidebarPanel(
          numericInput("dice", "Number of Dice (Attacks)", value = 24, min = 1, step = 1),
          numericInput("toughness", "Target Toughness", value = 4, min = 1, step = 1),
          selectInput("hit_prob", "To-Hit Roll", choices = hit_choices, selected = 0.667),
          checkboxInput("lethal_hits", "Lethal Hits", value = FALSE),
          checkboxInput("reroll_wounds", "Reroll Wounds", value = FALSE),
          hr(),
          helpText(
            "Strength profiles are automatically calculated from Toughness:",
            tags$ul(
              tags$li("Very Weak: floor(T / 2)"),
              tags$li("Weaker: T - 1"),
              tags$li("Equal: T"),
              tags$li("Stronger: T + 1"),
              tags$li("Very Strong: T \u00d7 2")
            )
          ),
          actionButton("go_wounds", "Roll!", class = "btn-primary")
        ),
        mainPanel(
          h4("Expected Wounds by Strength Profile"),
          tableOutput("wound_result")
        )
      )
    ),

    # ── Tab 2: Dice Distribution ───────────────────────────────────────────────
    tabPanel(
      "Dice Distribution",
      br(),
      sidebarLayout(
        sidebarPanel(
          numericInput("rolls", "Number of Dice per Roll", value = 20, min = 1, step = 1),
          numericInput("value", "Target Value or Better (on a D6)", value = 3, min = 1, max = 6, step = 1),
          checkboxInput("rerolls", "Allow Rerolls", value = FALSE),
          conditionalPanel(
            condition = "input.rerolls == true",
            sliderInput("reroll_up_to", "Reroll Dice Showing Up To",
                        min = 1, max = 5, value = 2, step = 1)
          ),
          actionButton("go_distro", "Simulate!", class = "btn-primary")
        ),
        mainPanel(
          h4("Simulation Summary"),
          tableOutput("distro_stats"),
          br(),
          h4("Distribution of Success Proportions (1 000 simulations)"),
          plotOutput("distro_hist")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # ── Tab 1: Wound Table ─────────────────────────────────────────────────────

  wound_data <- eventReactive(input$go_wounds, {
    toughness <- as.integer(input$toughness)
    strength  <- c(floor(toughness / 2), toughness - 1, toughness,
                   toughness + 1, toughness * 2)
    # Ensure minimum strength of 1
    strength <- pmax(strength, 1L)

    wound_table(
      dice          = as.integer(input$dice),
      strength      = strength,
      toughness     = toughness,
      lethal_hits   = input$lethal_hits,
      hit_prob      = as.numeric(input$hit_prob),
      reroll_wounds = input$reroll_wounds
    )
  })

  output$wound_result <- renderTable({
    wound_data()
  }, rownames = FALSE, striped = TRUE, hover = TRUE, bordered = TRUE)

  # ── Tab 2: Dice Distribution ───────────────────────────────────────────────

  distro_data <- eventReactive(input$go_distro, {
    reroll_val <- if (input$rerolls) seq_len(input$reroll_up_to) else 1:2

    dice_distro(
      sides     = 1:6,
      rolls     = as.integer(input$rolls),
      value     = as.integer(input$value),
      rerolls   = input$rerolls,
      reroll_val = reroll_val
    )
  })

  output$distro_stats <- renderTable({
    d <- distro_data()
    data.frame(
      Statistic = c("Mean success rate", "Min success rate", "Max success rate",
                    "Lower bound (\u22123 SD)", "Upper bound (+3 SD)",
                    "Avg successes per roll"),
      Value = c(
        round(d$avg_result,   3),
        round(d$min_result,   3),
        round(d$max_result,   3),
        round(d[[6]],         3),   # var_lower (unnamed in list)
        round(d[[7]],         3),   # var_upper (unnamed in list)
        round(d$avg_result * as.integer(input$rolls), 1)
      )
    )
  }, rownames = FALSE, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$distro_hist <- renderPlot({
    results <- distro_data()$results
    hist(
      results,
      main    = paste0("Distribution of Success Rates (", input$rolls, " dice, target ", input$value, "+)"),
      xlab    = "Proportion of Successful Rolls",
      ylab    = "Frequency",
      col     = "#4E79A7",
      border  = "white",
      breaks  = 30
    )
    abline(v = mean(results), col = "#E15759", lwd = 2, lty = 2)
    legend("topright", legend = "Mean", col = "#E15759", lwd = 2, lty = 2, bty = "n")
  })
}

shinyApp(ui, server)
