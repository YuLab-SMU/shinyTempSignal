extract_date_match <- function(label, pattern) {
  matched <- regmatches(label, regexec(pattern, label, perl = TRUE))[[1]]
  if (length(matched) == 0) {
    return(NA_character_)
  }
  if (length(matched) > 1) {
    captured <- matched[-1]
    captured <- captured[!is.na(captured) & nzchar(captured)]
    if (length(captured) > 0) {
      return(captured[[1]])
    }
  }
  matched[[1]]
}

override_tip_dates <- function(labels, date, external_data = NULL) {
  if (is.null(external_data) || !all(c("label", "date") %in% colnames(external_data))) {
    return(date)
  }
  idx <- match(labels, external_data$label)
  external_date <- external_data$date[idx]
  replace <- !is.na(external_date)
  if (any(replace)) {
    date[replace] <- external_date[replace]
  }
  date
}

resolve_tip_dates_raw <- function(tree, pattern, external_data = NULL) {
  date <- vapply(tree$tip.label, extract_date_match, character(1), pattern = pattern)
  override_tip_dates(tree$tip.label, date, external_data = external_data)
}

resolve_tip_dates <- function(tree, pattern, format, external_data = NULL) {
  date <- resolve_tip_dates_raw(tree = tree, pattern = pattern, external_data = external_data)
  dateNumeric(date = date, format = format)
}

dateType3 <- function(tree, pattern) {
  resolve_tip_dates_raw(tree = tree, pattern = pattern)
}
## convert Date to decimal format
dateNumeric <- function(date, format) {
  if (format == "yy" | format == "yyyy") {
    date <- as.numeric(date)
  }
  if (format == "yyyy-MM-dd") {
    date <- Date2decimal(date)
  }
  if (format == "yyyy/MM/dd") {
    date <- as.Date(date, format = "%y/%m/%d")
    date <- Date2decimal(date)
  }
  if (format == "yyyy.MM.dd") {
    date <- as.Date(date, format = "%y.%m.%d")
    date <- Date2decimal(date)
  }
  if (format == "MM-dd-yyyy") {
    date <- as.Date(date, format = "%m-%d-%y")
    date <- Date2decimal(date)
  }
  if (format == "MM/dd/yyyy") {
    date <- as.Date(date, format = "%m/%d/%y")
    date <- Date2decimal(date)
  }
  if (format == "MM.dd.yyyy") {
    date <- as.Date(date, format = "%m.%d.%y")
    date <- Date2decimal(date)
  }
  return(date)
}

getdivergence <- function(tree) {
  ## Root a Tree by Root-to-Tip Regression
  # tree2 <- rtt(tree, date, objective=method)
  tree2 <- tree
  divergence <- data.frame(divergence = numeric(length(tree2$tip.label)))
  dataset <- cbind(tree2$edge, tree2$edge.length)
  dataset <- as.data.frame(dataset)
  names(dataset) <- c("from", "to", "length")
  for (i in 1:length(tree2$tip.label)) {
    edge <- nodepath(tree2, from = length(tree2$tip.label) + 1, to = i)
    for (j in 1:(length(edge) - 1)) {
      a <- edge[j]
      b <- edge[j + 1]
      ## The start and end of the node are matched, and the length is assigned
      len <- dataset[which(dataset$from == a & dataset$to == b), ]$length
      divergence[i, 1] <- divergence[i, 1] + len
    }
  }
  return(divergence)
}

get_new_divergence <- function(tree, node) {
  dataset <- cbind(tree$edge, tree$edge.length) |> as.data.frame()
  names(dataset) <- c("from", "to", "length")
  len <- 0
  if (is.null(node) | is.na(node)) {
    node <- length(tree$tip.label) + 1
  }
  a <- nodepath(tree, from = length(tree$tip.label) + 1, to = node)
  if (node == length(tree$tip.label) + 1) {
    return(0)
  } else {
    for (i in 1:(length(a) - 1)) {
      tem_len <- dataset[, 3][dataset[, 1] == a[i] & dataset[, 2] == a[i + 1]]
      len <- len + tem_len
    }
    return(len)
  }
}
