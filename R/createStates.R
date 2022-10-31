#' This counts the single states in a  data frame
#'
#' @param data input your data set
#' @param s0 input upper limit for state 0
#' @param s1 input lower limit for state 1
#' @param s2 input upper limit for state 2
#' @param s3 input lower limit for state 3
#'
#' @return returns a small datafram with numbers of each state
#' @export
#'

createStates<- function(data,s0,s1,s2,s3){

  state0 <- length(which(data<s0+1))

  state1 <- length(which(data<s1+1))-state0

  state2 <- length(which(data<s2+1))-state1-state0

  state3 <- length(which(data>s3-1))

  singles <- data.frame(state0,state1,state2,state3)

  return(singles)



}
