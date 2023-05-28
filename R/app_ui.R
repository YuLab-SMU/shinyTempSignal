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
      skin="yellow",
      dashboardHeader(title = "shinyTempSignal"),
      dashboardSidebar(
        radioButtons(inputId = "filetype",
                     "choose your file type",
                     choiceNames = list("Newick","beast","NEXUS","phylip"),
                     choiceValues = list("Newick","beast","NEXUS","phylip")),
        fileInput("treefile","choose your tree file (necessary)"),
        #tags$h4("This file is ",tags$strong("not necessary"),"."),
        fileInput("outdata","choose your corresponding external data(not necessary)"),
        textInput("regression","a regrular expression to match your date"),
        selectInput("format", 
                    "Dates format:", 
                    c("yy", "yyyy", "yyyy-MM-dd", "MM-dd-yyyy", 
                      "yyyy/MM/dd", "yyyy.MM.dd", "MM/dd/yyyy", 
                      "MM.dd.yyyy")),
        
        actionButton("fileinput","submit"),
        sidebarMenu(
          menuItem("Sample Dates", tabName="Dates"),
          menuItem("Subtree_regression_intergration",tabName = "node"),
          menuItem("tree-structure",tabName = "tree"),
          menuItem("Root-to-tip",tabName = "regression"),
          menuItem("out_data_analysis", tabName="out"),
          menuItem("subtree_outdata__regression_intergration",tabName = "out_data_regression")
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "Dates",dataTableOutput("datetable"),
                  downloadButton("download1","download")),
          tabItem(tabName = "node",dataTableOutput("dataframe"),
                  downloadButton("download2.table","download")),
          tabItem(tabName = "out_data_regression",dataTableOutput("out_dataframe"),
                  downloadButton("download3.table","download")),
          tabItem(tabName = "tree",
                  
                  
                  box(plotOutput("plot1",click = "plotClick"),width = 8),
                  
                  box(width = 4,
                      textInput("node","subset_node"),
                      sliderInput("height","height:", 0, 5000, 380),
                      textInput("color3", 
                                "color:", value="black"),
                      sliderInput("size", 
                                  "size:", 0, 10, 1, step=0.1),
                      
                      checkboxInput("tip", "tiplab:", FALSE),
                      sliderInput("tipsize", 
                                  "tiplab_size:", 
                                  0, 10, 3, step=0.1),
                      
                  )
                  
          ),
          tabItem(tabName = "regression",box(width = 8,plotOutput("plot2")),
                  box(width = 4,numericInput("pvalue", "pvalue<:", 
                                             value = "0.05"),
                      textInput("color2", 
                                "line.color:", value = "blue"),
                      actionButton("delete", "auto-delet"),
                      actionButton("reset", "Reset")),
                  dataTableOutput("outliers")),
          tabItem(tabName = "out",
                  box(width = 8,plotOutput("plot3")),
                  box(width = 4,
                      selectInput("x_var","please choose your x var",choices = NULL),
                      selectInput("y_var","please choose your Y var",choices = NULL),
                      actionButton("regression_btn","regression_analysis")),
                  box(dataTableOutput("data_table"),width = 12),
                  box(dataTableOutput("outliers2"),width = 12)
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

