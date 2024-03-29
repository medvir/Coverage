library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Coverage"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a file", accept = c(".csv", ".tsv")),
      downloadButton("downloadPlot", "Download PDF")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    req(input$file)
    read.table(input$file$datapath, header = FALSE, sep = "\t")
  })
  
  output$plot <- renderPlot({
    horizontal <- sprintf("%.2f", sum(data()[, 3] > 0) / nrow(data()) * 100)
    vertical <- sprintf("%.2f", mean(data()[, 3]))
    total_rows <- nrow(data())
    
    title_text <- sprintf("%s (%d nt)", data()[1, 1], total_rows)
    
    plot <- ggplot(data(), aes(x = data()[,2], y = data()[,3])) +
      geom_line() +
      scale_y_log10(breaks = 10^(-10:10), minor_breaks = rep(1:9, 21) * (10^rep(-10:10, each = 9))) +
      theme_minimal() +
      labs(
        title = title_text,
        subtitle = paste("Horizontal coverage:", paste0(horizontal, "%"),
                         "| Mean vertical coverage:", paste0(vertical, "X")),
        x = "Genome position",
        y = "Coverage (reads)"
      ) +
      theme(plot.title = element_text(face = "bold"))
    
    print(plot)
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "pdf", width = 10, height = 6, units = "in")
    }
  )
}

shinyApp(ui, server)

