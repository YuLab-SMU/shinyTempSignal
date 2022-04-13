#' Method 2 for finding the date inside the label
#'
#' @param tree A tree of sequences, phylo
#' @param order Location of the date, character or numeric
#' @param prefix prefix for dates, character
#'
#' @return date, character
#' @export
#'
#' @examples
#' 
#' data("MCC_FluA_H3_tree")
#' dateType2(MCC_FluA_H3_tree, "last","/")
#' 
dateType2 <- function(tree, order, prefix) {
  date <- numeric(length(tree$tip.label))
  if (order == "first") {
    prefix2 <- paste0("^\\", prefix, "\\d+\\D?\\d+\\D?\\d+")
  }
  if (order == "last") {
    prefix2 <- paste0("\\", prefix, "\\d+\\D?\\d+\\D?\\d+$")
  }
  for (i in 1:length(tree$tip.label)) {
    result <- regexpr(prefix2, tree$tip.label[i])
    pos <- result[1]
    length <- attr(result, "match.length") - 1
    time <- substr(tree$tip.label[i], pos + 1, pos + length)
    date[i] <- time
  }
  return(date)
}