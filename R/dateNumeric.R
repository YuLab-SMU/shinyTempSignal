#' Convert dates according to date format
#'
#' @param date input a data extracted from labels, character
#' @param format input format of the date, character
#'
#' @return Returns a date of numeric type, numeric
#' @importFrom ggtree Date2decimal
#' @export
#'
#' @examples
#' 
#' dateNumeric(date="1999-12-07", format="yyyy-MM-dd")
#' 
dateNumeric <- function(date, format) {
  if (format == "yy" | format == "yyyy") {
    date <- as.numeric(date)
  }
  if (format == "yyyy-MM-dd") {
    date <- Date2decimal(date)
  }
  if (format == "yyyy/MM/dd") {
    date <- as.character.Date(as.Date(date, format="%Y/%m/%d"))
    date <- Date2decimal(date)
  }
  if (format == "yyyy.MM.dd") {
    date <- as.character.Date(as.Date(date, format="%Y.%m.%d"))
    date <- Date2decimal(date)
  }
  if (format == "MM-dd-yyyy") {
    date <- as.character.Date(as.Date(date, format="%m-%d-%Y"))
    date <- Date2decimal(date)
  }
  if (format == "MM/dd/yyyy") {
    date <- as.character.Date(as.Date(date, format="%m/%d/%Y"))
    date <- Date2decimal(date)
  }
  if (format == "MM.dd.yyyy") {
    date <- as.character.Date(as.Date(date, format="%m.%d.%Y"))
    date <- Date2decimal(date)
  }
  return(date)
}
