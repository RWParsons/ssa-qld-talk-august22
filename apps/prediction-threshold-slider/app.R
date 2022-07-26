library(shiny)
library(knitr)
library(kableExtra)

source("../src/utils.R", chdir = T)

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            sliderInput("cutpoint",
                        "Select cutpoint:",
                        min = 0,
                        max = 1,
                        value = 0.5),
            sliderInput("event_rate",
                        "Rate of the outcome:",
                        min = 0,
                        max = 0.5,
                        value = 0.1),
            sliderInput("auc",
                        "Model discrimination (AUC):",
                        min = 0.5,
                        max = 1,
                        value = 0.75)
        ),

        mainPanel(
           plotOutput("plot", height=320),
           tags$br(),
           tableOutput("confusion_matrix") 
        )
    )
)


server <- function(input, output) {
    
    df_preds <- reactive({
      set.seed(42)
      data <- get_sample(
        auc=input$auc, 
        n_samples=10000, 
        prevalence=input$event_rate
      )
      
      mod <- glm(actual ~ predicted, data=data, family=binomial())
      data$actual <- as.factor(data$actual)
      data$proba <- predict(mod, type="response")
      df_out <<- data
      data
    })
    
    output$plot <- renderPlot({
      df_preds() %>%
        ggplot(aes(x=proba, fill=actual)) + 
        geom_histogram() +
        geom_vline(xintercept=input$cutpoint) +
        theme_bw() +
        labs(
          x="Predicted probabilities",
          y="Number of observations",
          fill="Event"
        ) +
        scale_x_continuous(limits=c(0,1)) +
        theme(text = element_text(size = 13))
    })
    
    output$confusion_matrix <- renderText({
      
      
      df_preds() %>%
        group_by(actual, predicted=proba > input$cutpoint) %>%
        summarize(n=n()) %>% 
        ungroup() %>%
        mutate(predicted = factor(predicted, levels=c(FALSE, TRUE))) %>%
        pivot_wider(
          names_from=predicted, values_from=n, 
          names_expand=T, values_fill=0
        ) %>%
        mutate(actual=as.logical(as.numeric(actual)-1)) %>%
        rename(" "=actual) %>%
        kable("html") %>%
        kable_paper(full_width=FALSE, font_size=18) %>%
        column_spec(2:3, width = "6em") %>%
        column_spec(1, bold=T) %>%
        add_header_above(c(" "=1, "Predicted"=2)) %>%
        group_rows("Actual", 1, 2)
    })
}










# Run the application 
shinyApp(ui = ui, server = server)
