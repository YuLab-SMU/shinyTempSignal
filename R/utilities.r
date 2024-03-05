##' set default theme
##'
##' @title shinyTempSignal_set_theme
##' @param theme a ggplot theme
##' @return No value return
##' @export 
shinyTempSignal_set_theme <- function(theme) {
  if (inherits(theme, 'function')) {
    theme <- theme()
  }

  if (!inherits(theme, "theme")) stop("the input is not a ggplot theme")

  options(shinyTempSignal_theme = theme)
}

##' @importFrom ggprism theme_prism
shinyTempSignal_get_theme <- function() {
  getOption("shinyTempSignal_theme", 
          default = ggprism::theme_prism(base_size = 14, border = TRUE)
  )
}
