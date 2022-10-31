#' Function that creates a scatter plot with a smoother, that allows users to select dots and see their values using shiny.
#'
#' @param data input the data set that is to be used
#' @param x input the variable
#' @param y second input the variable
#' @param x1 input the variable, but add data$x1
#' @param x2 second input the variable, but add data$x2
#'
#' @return a shiny application that allows users to select dots in scatter plot and see data
#' @export
#' @import shiny
#' @import shinyWidgets
#' @import ggplot2
#' @import stats
#'

plotcorr <-function(data, x, y, x1, x2){

corr<- cor(x1, x2)
corr<- round(corr, digits = 3)


  ui <- fluidPage(
    plotOutput("plot", brush = "plot_brush"),
    tableOutput("data")
  )
  server <- function(input, output, session) {
    output$plot <- renderPlot({
      ggplot(data, aes({{x}}, {{y}})) + geom_point(
        color="blue",
        fill="#00C19A",
        shape=21,
        alpha=0.5,
        size=3,
        stroke = 2) + annotate("text", x=6, y=10, label= corr) + annotate("text", x= 5.75, y= 10, label = "Correlation:") + stat_smooth(method="lm", se=FALSE)
    },res = 96)

    output$data <- renderTable({
      brushedPoints(data, input$plot_brush )
    })

  }
  shinyApp(ui = ui, server = server)

}
