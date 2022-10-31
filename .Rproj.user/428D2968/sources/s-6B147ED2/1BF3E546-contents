#' Creates a transition matrix diagram
#'
#' @param matrix1 input matrix output-ed from transcount function
#'
#' @return diagram
#' @export
#'

diagMat <- function(matrix1){
  matrix <-matrix1[0-5]
  M1<-round(matrix,3)
  statesNames<-c("low","med","high","very high")
  curves <- matrix(nrow=ncol(M1),ncol=ncol(M1),0)
  curves[3,1]<- curves[1,4]<- -0.35
  curves[2,4]<- curves[4,2]<- curves[3,4]<- curves[4,2]<-0.08
  curves[3,4]<- 0.35
  diagram::plotmat(M1, curve = curves, name = statesNames,
          lwd = 1, box.lwd = 2, cex.txt = 0.8,
          box.type = "square", box.prop = 0.5, arr.type = "triangle",
          arr.pos = 0.4, shadow.size = 0.01,
          main = "TRANSITION MATRIX", self.cex = 0.4 , self.shiftx = c(0.05, -.135, .01, 0.14),self.shifty = c(.07, .01, .07, -0.01))

}
