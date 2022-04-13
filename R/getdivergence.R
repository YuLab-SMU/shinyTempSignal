#' Calculating the divergence of sequences
#'
#' @param tree A tree of sequences, phylo
#' @param date dates of numeric type, numeric
#' @param method one of "correlation", "rms", or "rsquared", character
#'
#' @return the divergence of sequences, data.frame
#' @export
#'
#' @examples
#'
#' data("MCC_FluA_H3_tree")
#' date <- dateType3(MCC_FluA_H3_tree, "(?<=/)\\d+$")
#' date <- dateNumeric(date, "yyyy")
#' getdivergence(MCC_FluA_H3_tree, date, "rms") 
#' 
getdivergence <- function(tree, date, method) {
  ## Root a Tree by Root-to-Tip Regression
  tree2 <- rtt(tree, date, objective=method)
  divergence <- data.frame(divergence=numeric(length(tree2$tip.label)))
  dataset <- cbind(tree2$edge, tree2$edge.length)
  dataset <- as.data.frame(dataset)
  names(dataset) <- c("from", "to", "length")
  for (i in 1:length(tree2$tip.label)) {
    edge <- nodepath(tree2, from=length(tree2$tip.label) + 1, to=i)
    for (j in 1:(length(edge) - 1)) {
      a <- edge[j]
      b <- edge[j + 1]
      ##The start and end of the node are matched, and the length is assigned
      len <- dataset[which(dataset$from == a & dataset$to == b), ]$length  
      divergence[i, 1] <- divergence[i, 1] + len
    }
  }
  return(divergence)
}