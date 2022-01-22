#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyjs useShinyjs
#' @noRd
app_ui <- function(request) {
  tagList(
    dashboardPage(skin="blue", 
                  dashboardHeader(title="Temporal Signal"),
                  dashboardSidebar(
                    useShinyjs(),
                    fileInput("file1", "choose a tree", accept="nwk"),  
                    sidebarMenu(menuItem("Sample Dates", tabName="Dates"),
                                menuItem("Root-to-tip", tabName="root")),   
                    title="Choose a way to parse dates:",
                    checkboxInput("type1", "Defined just by its order"),
                    checkboxInput("type2", 
                                  "Defined by a prefix and its order"),
                    checkboxInput("type3", 
                                  "Defined by regular expression (REGEX)"),
                    selectInput("format", 
                                "Dates format:", 
                                c("yy", "yyyy", "yyyy-MM-dd", "MM-dd-yyyy", 
                                  "yyyy/MM/dd", "yyyy.MM.dd", "MM/dd/yyyy", 
                                  "MM.dd.yyyy")),
                    selectInput("order1", 
                                "numerical_order:", 
                                c("1", "2", "3", "4", "5", 
                                  "6", "7", "8", "last")), 
                    textInput("prefix", "Date_prefix:"), 
                    selectInput("order2", 
                                "prefix_order:", c("first", "last")), 
                    textInput("REGEX", "regular expression:") 
                  ),
                  ## Design point to open the menu interface :tree Dates root
                  dashboardBody(
                    tabItems(
                      tabItem(tabName="Dates",
                              tableOutput("Sample")
                      ),
                      tabItem(tabName="root",
                              box(plotOutput("distplot1"), width=8),
                              box(width=4,
                                  sliderInput("height", 
                                              "height:", 0, 5000, 380),
                                  textInput("color3", 
                                            "color:", value="black"),
                                  sliderInput("size", 
                                              "size:", 0, 10, 1, step=0.1),
                                  checkboxInput("tip", "tiplab:", TRUE),
                                  sliderInput("tipsize", 
                                              "tiplab_size:", 
                                              0, 10, 3, step=0.1)),
                              mainPanel(textOutput("text1")),
                              column(width=6,
                                     plotOutput("distPlot2", height=500,
                                                click="plot2_click",
                                                brush=brushOpts(
                                                  id="plot2_brush"
                                                )
                                     ),
                                     box(plotOutput("distPlot3", 
                                                    height=500), width=10),
                              ),
                              box(width=4,
                                  selectInput("method", "method:", 
                                              c("rms", "rsquared", 
                                                "correlation")),
                                  numericInput("pvalue", "pvalue<:", 
                                               value = "0.05"),
                                  textInput("color2", 
                                            "line.color:", value = "blue"),
                                  actionButton("exclude_toggle", 
                                               "Toggle points"),
                                  actionButton("exclude_reset", "Reset"),
                                  checkboxInput("update_data", 
                                                "live updates", FALSE)),
                              tableOutput("Summary"),
                              box(plotOutput("distPlot4"), width=4),
                              box(plotOutput("distPlot5"), width=4),
                              box(plotOutput("distPlot6", 
                                             height=500), width=10),
                      )
                    )
                  )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'sTSpkg'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

