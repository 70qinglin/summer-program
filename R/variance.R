#' Calculates the variance of a sequence
#'
#' @param x the whole sequence
#'
#' @return a number as the variance of the sequence
#' @export
#'
#' @examples
#' variance(c(88,89,90,87,86))
#' variance(c(77,78,81,76,78,77,66,90))
#' variance(c(25,35,45,55,10))
#'
variance<-function(x){
  k<-0
  t<-0
  for(i in 1:length(x)){
    k<-k+x[i]
  }
  k<-k/length(x)
  for(i in 1:length(x)){
    t<-(x[i]-k)^2+t
  }
  t<-t/length(x)
  return(t)
}
