#' Easy mean forecasting with barely any code
#'
#' @param data1 input a numerical vector that you want to forecast
#' @param Y1    input the first year
#' @param Y2    input the last year
#'
#' @return      forecast for next year
#' @export
#'

easyMeanF <- function(data1, Y1, Y2){


  students.ts <- stats::ts(data1, start=c(Y1, 1), end=c(Y2, 1))

  # Use meanf() to forecast quarterly contributions in 2021
  students.fc <- forecast::meanf(students.ts, h=4)

  # Plot and summarize the forecasts
  ggplot2::autoplot(students.fc,xlab = "Year", ylab = "students")
  y<-summary(students.fc)

  utils::head(y$upper,1)

}
