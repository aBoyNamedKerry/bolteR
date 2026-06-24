library(shiny)
library(bolteR)

# Hit probability choices (40K dice notation → raw probability)
hit_choices <- c("2+" = 5/6, "3+" = 4/6, "4+" = 3/6, "5+" = 2/6, "6+" = 1/6)

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
          selectInput("hit_prob", "To-Hit Roll", choices = hit_choices, selected = 4/6),
          checkboxInput("lethal_hits", "Lethal Hits", value = FALSE),
          checkboxInput("reroll_hits", "Reroll Hits", value = FALSE),
          checkboxInput("reroll_wounds", "Reroll Wounds", value = FALSE),
          hr(),
          helpText(
            "Strength profiles are automatically calculated from Toughness:",
            tags$ul(
              tags$li(tags$strong("Weak"), ": Strength is half toughness"),
              tags$li(tags$strong("Weaker"), ": Strength is less than toughness but not half"),
              tags$li(tags$strong("Equal"), ": Strength is equal to toughness"),
              tags$li(tags$strong("Stronger"), ": Strength is greather than toughness but not double"),
              tags$li(tags$strong("Strong"), ": Strength is twice toughness")
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
                        min = 1, max = 5, value = 1, step = 1)
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
      reroll_hits   = input$reroll_hits,
      reroll_wounds = input$reroll_wounds
    )
  })

  output$wound_result <- renderTable({
    wound_data()
  }, rownames = FALSE, striped = TRUE, hover = TRUE, bordered = TRUE)

  # ── Tab 2: Dice Distribution ───────────────────────────────────────────────

  distro_data <- eventReactive(input$go_distro, {
    reroll_val <- if (input$rerolls) seq_len(input$reroll_up_to) else 1:2

    main_result <- dice_distro(
      sides      = 1:6,
      rolls      = as.integer(input$rolls),
      value      = as.integer(input$value),
      rerolls    = input$rerolls,
      reroll_val = reroll_val
    )

    baseline_result <- if (input$rerolls) {
      dice_distro(
        sides      = 1:6,
        rolls      = as.integer(input$rolls),
        value      = as.integer(input$value),
        rerolls    = FALSE,
        reroll_val = 1:2
      )
    } else NULL

    list(main = main_result, baseline = baseline_result, has_rerolls = input$rerolls)
  })

  output$distro_stats <- renderTable({
    d    <- distro_data()
    main <- d$main
    n    <- as.integer(input$rolls)

    stats <- c("Mean success rate", "Min success rate", "Max success rate",
               "Lower bound (\u22123 SD)", "Upper bound (+3 SD)",
               "Avg successes per roll")

    main_vals <- c(
      round(main$avg_result,        3),
      round(main$min_result,        3),
      round(main$max_result,        3),
      round(main[[6]],              3),   # var_lower
      round(main[[7]],              3),   # var_upper
      round(main$avg_result * n,    1)
    )

    if (d$has_rerolls && !is.null(d$baseline)) {
      base      <- d$baseline
      base_vals <- c(
        round(base$avg_result,        3),
        round(base$min_result,        3),
        round(base$max_result,        3),
        round(base[[6]],              3),
        round(base[[7]],              3),
        round(base$avg_result * n,    1)
      )
      delta <- main_vals - base_vals
      tbl <- data.frame(
        Statistic           = stats,
        "Without Rerolls"   = base_vals,
        "With Rerolls"      = main_vals,
        Difference          = ifelse(delta >= 0, paste0("+", delta), as.character(delta)),
        check.names         = FALSE
      )
    } else {
      tbl <- data.frame(Statistic = stats, Value = main_vals)
    }

    tbl
  }, rownames = FALSE, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$distro_hist <- renderPlot({
    d       <- distro_data()
    results <- d$main$results
    title   <- paste0("Distribution of Success Rates (", input$rolls, " dice, target ", input$value, "+)")

    col_blue   <- rgb(0.306, 0.475, 0.655, 1)     # #4E79A7 — without rerolls
    col_orange <- rgb(0.851, 0.373, 0.008, 1)     # #D95F02 — with rerolls

    if (d$has_rerolls && !is.null(d$baseline)) {
      results_base <- d$baseline$results
      all_res      <- c(results, results_base)
      break_pts    <- seq(min(all_res), max(all_res), length.out = 32)

      h_with    <- hist(results,      breaks = break_pts, plot = FALSE)
      h_without <- hist(results_base, breaks = break_pts, plot = FALSE)

      y_max <- max(h_with$counts, h_without$counts) * 1.1

      # With rerolls behind — dark orange, full opacity
      plot(h_with,
           main   = title,
           xlab   = "Proportion of Successful Rolls",
           ylab   = "Frequency",
           col    = col_orange,
           border = "white",
           xlim   = range(all_res),
           ylim   = c(0, y_max))

      # Without rerolls in front — blue, lower opacity
      plot(h_without, add = TRUE,
           col    = adjustcolor(col_blue, alpha.f = 0.5),
           border = "white")

      abline(v = mean(results),      col = col_orange, lwd = 2, lty = 2)
      abline(v = mean(results_base), col = col_blue,   lwd = 2, lty = 2)

      legend("topright",
             legend = c("With rerolls", "Without rerolls"),
             fill   = c(col_orange, adjustcolor(col_blue, alpha.f = 0.5)),
             border = "white",
             bty    = "n")
    } else {
      hist(
        results,
        main   = title,
        xlab   = "Proportion of Successful Rolls",
        ylab   = "Frequency",
        col    = col_blue,
        border = "white",
        breaks = 30
      )
      abline(v = mean(results), col = col_orange, lwd = 2, lty = 2)
      legend("topright", legend = "Mean", col = col_orange, lwd = 2, lty = 2, bty = "n")
    }
  })
}

shinyApp(ui, server)
