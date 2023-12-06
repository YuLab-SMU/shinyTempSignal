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
    dashboardPage(
      skin = "yellow",
      dashboardHeader(title = "shinyTempSignal"),
      dashboardSidebar(
        radioButtons(
          inputId = "filetype",
          "choose your file type",
          choiceNames = list("Newick", "NEXUS","Beast", "Phylip"),
          choiceValues = list("Newick", "NEXUS","Beast",  "Phylip")
        ),
        fileInput("treefile", "choose a tree file"),
        textInput("regression", "Regular expression to extract sampling date from tip labels"),
        selectInput(
          "format",
          "Date format",
          c(
            "yy",
            "yyyy",
            "yyyy-MM-dd",
            "MM-dd-yyyy",
            "yyyy/MM/dd",
            "yyyy.MM.dd",
            "MM/dd/yyyy",
            "MM.dd.yyyy"
          )
        ),
        #tags$h4("This file is ",tags$strong("not necessary"),"."),
        fileInput(
          "outdata",
          "External data (optional)"
        ),
        actionButton("fileinput", "submit"),
        sidebarMenu(
           menuItem("Tree structure exploration", tabName = "tree"),
          menuItem("Sample Dates", tabName = "Dates"),
          menuItem("Temporal signal analysis", tabName = "regression"),
          menuItem("Subtree Temporal signal", tabName = "node"),
         # menuItem("Time_Series_Analysis", tabName = "time"),
          menuItem("Phylogenetic signal", tabName = "out"),
          menuItem("Subtree phylogenetic signal", tabName = "out_data_regression")
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(
            tabName = "Dates",
            dataTableOutput("datetable"),
            downloadButton("download1", "download")
          ),
          tabItem(
            tabName = "node",
            dataTableOutput("dataframe"),
            downloadButton("download2.table", "download")
          ),
          tabItem(
            tabName = "out_data_regression",
            dataTableOutput("out_dataframe"),
            downloadButton("download3.table", "download")
          ),
          tabItem(
            tabName = "time",
            box(
              plotOutput("plot8"),
              width = 6,
              downloadButton("downloadplot8", "download")
            ),
            
            box(
              plotOutput("plot9"),
              width = 6,
              downloadButton("downloadplot9", "download")
            ),
            
            box(
              plotOutput("plot10"),
              width = 6,
              downloadButton("downloadplot10", "download")
            ),
            
            box(
              plotOutput("plot11"),
              width = 6,
              downloadButton("downloadplot11", "download"),
              selectInput("fmethod", "method:",
                          c("ETS", "ARIMA")),
              textInput("hstep",
                        "h:", value = "120")
            ),
          ),
          tabItem(
            tabName = "tree",
            
            
            box(
              plotOutput("plot1", click = "plotClick"),
              width = 8,
              downloadButton("downloadplot1", "download")
            ),
            
            
            box(
              width = 4,
              textInput("node", "subset_node"),
              sliderInput("height", "height:", 0, 5000, 380),
              textInput("color3",
                        "color:", value = "black"),
              sliderInput("size",
                          "size:", 0, 10, 1, step = 0.1),
              
              checkboxInput("tip", "tiplab:", FALSE),
              checkboxInput("tip_point", "tip_point:", FALSE),
              checkboxInput("geom_nodelab", "node number", TRUE),
              sliderInput("tipsize",
                          "tiplab_size:",
                          0, 10, 3, step = 0.1),
              
            )
            
          ),
          tabItem(
            tabName = "regression",
            box(
              width = 8,
              plotOutput("plot2"),
              downloadButton("downloadplot2", "download"),
              actionButton("delete", "autodel"),
              actionButton("reset", "Reset"),
              fluidRow(column(
                6, numericInput("pvalue", "pvalue<:",
                                value = "0.05")
              ),
              column(
                6, textInput("color2",
                             "line.color:",
                             value = "blue")
              ))
            ),
            
            box(
              width = 4,
              tableOutput("Summary"),
              downloadButton("download_dt2", "download")
            ),
            
            
            dataTableOutput("outliers")
          ),
          tabItem(
            tabName = "out",
            box(
              width = 8,
              plotOutput("plot3"),
              downloadButton("downloadplot3", "download")
            ),
            
            box(
              width = 4,
              selectInput("x_var", "please choose your x var", choices = NULL),
              selectInput("y_var", "please choose your Y var", choices = NULL),
              actionButton("regression_btn", "regression_analysis")
            ),
            box(dataTableOutput("data_table"), width = 12),
            box(dataTableOutput("outliers2"), width = 12)
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
