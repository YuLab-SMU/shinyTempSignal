#' Method 3 for finding the date inside the label
#'
#' @param tree A tree of sequences, phylo
#' @param pattern Canonical matching command, character
#'
#' @return date, character
#' @export
#'
#' @examples
#' 
#' data("MCC_FluA_H3_tree")
#' dateType3(MCC_FluA_H3_tree, "(?<=/)\\d+$")
#' 
dateType3 <- function(tree, pattern) {
  date <- numeric(length(tree$tip.label))
  for (i in 1:length(tree$tip.label)) {
    date[i] <- str_extract(tree$tip.label[i], pattern)
  }
  return(date)
}