#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard

#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets::colorPickr

app_ui <- function(request) {
  tagList(
    dashboardPage(
      skin = "yellow",
      dashboardHeader(title = "shinyTempSignal"),
      dashboardSidebar(
        radioButtons(
          inputId = "filetype",
          "choose your file type",
          choiceNames = list("Newick", "NEXUS","BEAST", "Phylip"),
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
          menuItem("Temporal signal", tabName = "regression"),
          # menuItem("Subtree Temporal signal", tabName = "node"),
         # menuItem("Time_Series_Analysis", tabName = "time"),
          menuItem("Phylogenetic signal", tabName = "out")
          # menuItem("Subtree phylogenetic signal", tabName = "out_data_regression")
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(
            tabName = "Dates",
            dataTableOutput("datetable"),
            downloadButton("download1", "download")
          ),
          # tabItem(
          #   tabName = "node",
          #   dataTableOutput("dataframe"),
          #   downloadButton("download2.table", "download")
          # ),
          # tabItem(
          #   tabName = "out_data_regression",
          #   dataTableOutput("out_dataframe"),
          #   downloadButton("download3.table", "download")
          # ),
          tabItem(
            tabName = "tree",
            
            
            box(
              plotOutput("plot1", click = "plotClick"),
              width = 8,
              height = 850              
            ),
                       
            box(
              width = 4,
              height = 850,
              sliderInput("height", "figure height:", 0, 5000, 835),
              fluidRow(
                column(width = 6,textInput("node", "subset_node")),
                column(width = 6,shinyWidgets::colorPickr("color3",label="line color:", selected  = "#020202"))),
              fluidRow(
              column(width = 6,shinyWidgets::colorPickr( "down_color",label="down_color",selected  =  "#6a73cf")),
              column(width = 6,shinyWidgets::colorPickr( "up_color",label="up_color",selected  ="#f26115" ))
              ),
              sliderInput("size","line size:", 0, 10, 1, step = 0.1),
              
            fluidRow(
              column(width = 4,checkboxInput("tip_point", "tip_point", FALSE)),
              column(width = 4,checkboxInput("tip", "tiplab", FALSE)),
              column(width = 4, checkboxInput("geom_nodelab", "node number", TRUE))),
              sliderInput("tipsize","tip_point_size:",
                          0, 10, 3, step = 0.1),
             fluidRow(column(width = 6,radioButtons(
          inputId = "layout",
          "tree layout",
          choiceNames = list("rectangular","roundrect", "slanted","circular", "daylight","ellipse","fan"),
          choiceValues = list("rectangular","roundrect", "slanted","circular", "daylight","ellipse","fan")
        )),
        column(width = 6,radioButtons(inputId = "line_type","tree line type",
        choiceNames = list("solid","twodash","longdash","dotted","dashed","dotted","blank"),
        choiceValues = list("solid","twodash","longdash","dotted","dashed","dotted","blank")
        ))
        ),
             
              
              radioButtons(
          inputId = "choose_analysis",
          "choose the point to display in the tree",
          choiceNames = list("only_tree","Temporal_signal", "Phylogenetic_signal"),
          choiceValues = list("only_tree","Temporal_signal", "Phylogenetic_signal")
        ),
              downloadButton("downloadplot1", "download(pdf)"),
              downloadButton("tree1", "download(newick)"),    
            )
            
          ),
          tabItem(
            tabName = "regression",
            box(
              width = 8,
              plotOutput("plot2"),
             fluidRow( 
              column(width = 3,downloadButton("downloadplot2", "download")),
              column(width = 3,actionButton("delete", "autodel")),
              column(width = 3,actionButton("reset", "Reset")),
              column(width = 3,textInput("temp_node","node",value = ""))),
              fluidRow(checkboxInput("plot_all", "plot whole tree regression", TRUE)),
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
            box(width = 8,plotOutput("multi_regression")),
            box(width = 4,
            textInput("multi_node","input multi nodes(comma-separated)"),
            actionButton("update_button","submit nodes")),
            box(width = 12,
            dataTableOutput("dataframe"),
            downloadButton("download2.table", "download")),
            
            dataTableOutput("outliers")
          ),
          tabItem(
            tabName = "out",
            box(
              width = 8,
              plotOutput("plot3"),
              downloadButton("downloadplot3", "download"),
              
              fluidRow(
                column(3,selectInput("x_var", "please choose your x var", choices = NULL),),
                column(3, selectInput("y_var", "please choose your Y var", choices = NULL),),
                column(3,actionButton("regression_btn", "update_regression_analysis")),
                column(3,checkboxInput("plot_all2", "plot whole tree regression", TRUE))
                ),
                fluidRow(
                column(width = 3,actionButton("delete2", "autodel")),
                column(width = 3,actionButton("reset2", "Reset")),
                column(width = 3,textInput("phylo_node","node",value = "")))
            ),
         
            box(
             width = 4,
              fluidRow(column(width = 12,tableOutput("Summary2")), downloadButton("download_dt_2", "download")),
              fluidRow(column(width = 12,verbatimTextOutput("correlation"))),
               radioButtons(
          inputId = "cortype",
          "choose your correltion type",
          choiceNames = list("PIC", "PGLS"),
          choiceValues = list("PIC", "PGLS")
        )
              
            ),
            box(width = 8,plotOutput("multi_regression2")),
            box(width = 4,textInput("multi_node2"," input multi nodes(comma-separated)"),
                          actionButton("update_button2","submit nodes")),
            box(
              width = 12,
              dataTableOutput("out_dataframe"),
              downloadButton("download3.table", "download")
            ),
           
            box(dataTableOutput("outliers2"), width = 12),
             box(dataTableOutput("data_table"), width = 12)
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