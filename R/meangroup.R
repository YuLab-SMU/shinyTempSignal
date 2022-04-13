#' Combining data from the same years
#'
#' @param d a data frame with "time" in the column name
#'
#' @return The processed data frame, data.frame
#' @export
#'
#' @examples
#' 
#' x <- c(1999, 2002  ,2005, 2000,2004 ,2004, 1999)
#' y <- c(1, 1.5, 2, 3 ,4 ,5 ,6)
#' d <- data.frame(time=x, score=y)
#' meangroup(d)
#' 
meangroup <- function(d){
  d <- d[order(d$time),]
  x <- d[, 1]
  y <- d[, 2]
  g<-grouping(d[,1])
  gp <- attr(g,"ends")
  Nyear <- c()
  Nscore <- c()
  for (i in 1:length(gp)) {
    if(i==1){
      Nyear[i] <- mean(x[1:gp[i]])
      Nscore[i] <- mean(y[1:gp[i]])
    }
    else{
      Nyear[i] <- mean(x[gp[i]])
      Nscore[i] <- mean(y[(gp[i-1]+1):gp[i]])
    }
  }
  dd <- data.frame(a = Nyear, b = Nscore)
  names(dd)<- c(names(d)[1], names(d)[2])
  return(dd)
}