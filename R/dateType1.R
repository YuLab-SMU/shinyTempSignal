#' Method 1 for finding the date inside the label
#'
#' @param tree A tree of sequences, phylo
#' @param order Location of the date, character or numeric
#'
#' @return date, character
#' @export
#'
#' @examples
#' 
#' data("MCC_FluA_H3_tree")
#' dateType1(MCC_FluA_H3_tree, "last")
#' 
dateType1 <- function(tree, order) {
  date <- numeric(length(tree$tip.label))##count tip
  if (order == "last") {
    for (i in 1:length(tree$tip.label)) {
      result <- regexpr("\\d+(\\-?|\\d+)+(\\-?|\\d+)$", tree$tip.label[i])
      pos <- result[1]
      length <- attr(result, "match.length") - 1
      ##substr(x,start,stop),get the time
      time <- substr(tree$tip.label[i], pos, pos + length)
      ## save in date
      date[i] <- time 
    }}
  else{
    for (i in 1:length(tree$tip.label)) {
      order <- as.numeric(order)
      result <- gregexpr("\\d+(\\-?|\\d+)+(\\-?|\\d+)", tree$tip.label[i])
      pos <- result[[1]][order]
      length <- attr(result[[1]], "match.length")[order] - 1
      time <- substr(tree$tip.label[i], pos, pos + length)
      date[i] <- time
    }}
  return(date)
}