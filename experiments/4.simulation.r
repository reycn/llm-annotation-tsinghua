if (!require("pacman")) install.packages("pacman")
pacman::p_load(scales, shiny, ggplot2, gridExtra)

nature_colors <- c("#014E8D", "#9B241B", "#999999")

ui <- fluidPage(
    titlePanel("Sample Accuracy Simulation"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                "nRounds",
                "Number of simulations (n)",
                min = 10,
                max = 1000,
                value = 300,
                step = 50
            ),
            sliderInput(
                "label_pred",
                "Prediction accuracy",
                min = 0.3,
                max = 0.7,
                value = 0.7,
                step = 0.05
            )
        ),
        mainPanel(
            plotOutput("plots", height = "600px"),
            verbatimTextOutput("popAcc")
        )
    )
)

server <- function(input, output, session) {
    sim_data <- reactive({
        set.seed(123)
        n <- 10000
        df <- data.frame(
            id = 1:n,
            text = paste("Text", 1:n),
            label_true = rbinom(n, 1, 0.5)
        )

        df$label_pred <- ifelse(runif(n) < input$label_pred, df$label_true, 1 - df$label_true)
        pop_accuracy <- mean(df$label_true == df$label_pred)

        nRounds <- input$nRounds
        sampled_accuracy <- numeric(nRounds)
        for (i in 1:nRounds) {
            sample_ids <- sample(df$id, size = 200, replace = TRUE)
            sample_df <- df[sample_ids, ]
            sampled_accuracy[i] <- mean(sample_df$label_true == sample_df$label_pred)
        }

        list(
            df_acc = data.frame(Round = 1:nRounds, Accuracy = sampled_accuracy),
            pop_accuracy = pop_accuracy
        )
    })

    output$plots <- renderPlot({
        data <- sim_data()
        df_acc <- data$df_acc
        pop_accuracy <- data$pop_accuracy

        p1 <- ggplot(df_acc, aes(x = Round, y = Accuracy)) +
            geom_line(color = nature_colors[[3]], alpha = 0.5) +
            geom_point(color = nature_colors[[1]], alpha = 0.3, size = 1) +
            geom_hline(yintercept = pop_accuracy, linetype = "dashed", color = "black") +
            geom_text(
                aes(
                    x = max(df_acc$Round) * 0.5,
                    y = pop_accuracy,
                    label = paste("Population Accuracy:", round(pop_accuracy * 100, 2), "%")
                ),
                vjust = -7,
                color = "black",
                size = 4
            ) +
            scale_y_continuous(limits = c(0.2, 0.8), labels = percent_format(accuracy = 1)) +
            labs(
                title = "A. Sample Accuracy by Simulation Round",
                x = "Round",
                y = "Accuracy"
            ) +
            theme_bw()

        p2 <- ggplot(df_acc, aes(x = Accuracy)) +
            geom_vline(xintercept = pop_accuracy, linetype = "dashed", color = "black", size = 1) +
            geom_histogram(bins = 20, fill = nature_colors[[1]], color = nature_colors[[1]], alpha = 0.8) +
            labs(
                title = "B. Sample Accuracy Distribution",
                x = "Accuracy",
                y = "Frequency"
            ) +
            theme_bw()

        grid.arrange(p1, p2, ncol = 2)
    })

    output$popAcc <- renderText({
        data <- sim_data()
        paste0("Population Accuracy: ", round(data$pop_accuracy * 100, 2), "%")
    })
}

shinyApp(ui, server)
