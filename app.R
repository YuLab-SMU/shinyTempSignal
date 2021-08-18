library(shiny)
library(shinydashboard)
library(shinyjs)

ui <- dashboardPage(skin = "blue",##set blue background
                    dashboardHeader(title = "Temporal Signal"),
                    
                    dashboardSidebar(
                      useShinyjs(),
                      fileInput("file1","choose a tree",accept = "nwk"),##choose a file ,Use it to prevent users from selecting invalid files. 
                      sidebarMenu(menuItem("Sample Dates",tabName = "Dates"),
                                  menuItem("Root-to-tip",tabName = "root")),##Create a dashboard sidebar menu and menu items.
                      title="Choose a way to parse dates:",
                      checkboxInput("type1","Defined just by its order"),
                      checkboxInput("type2","Defined by a prefix and its order"),
                      checkboxInput("type3","Defined by regular expression (REGEX)"),
                      selectInput("format","Dates format:",c("yy","yyyy","yyyy-MM-dd","MM-dd-yyyy","yyyy/MM/dd","yyyy.MM.dd","MM/dd/yyyy","MM.dd.yyyy")),
                      selectInput("order1","numerical_order:",c("1","2","3","4","5","6","7","8","last")),##select 1
                      textInput("prefix","Date_prefix:"),##select 2
                      selectInput("order2","prefix_order:",c("first","last")),##select 2
                      textInput("REGEX","regular expression:")##select 3
                      
                      
                    ),
                    ## Design point to open the menu interface :tree Dates root
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "Dates",
                                tableOutput("Sample")
                        ),
                        
                        tabItem(tabName = "root",
                                box(plotOutput("distplot1"),width=8),
                                box(width = 4,
                                    sliderInput("height","height:",0,5000,380),
                                    textInput("color3","color:",value = "black"),
                                    sliderInput("size","size:",0,10,1,step=0.1),
                                    checkboxInput("tip","tiplab:",TRUE),
                                    sliderInput("tipsize","tiplab_size:",0,10,3,step = 0.1)),
                                mainPanel(textOutput("text1")),
                                column(width = 6,
                                       plotOutput("distPlot2", height = 500,
                                                  click = "plot2_click",
                                                  brush = brushOpts(
                                                    id = "plot2_brush"
                                                  )
                                       ),
                                ),
                                box(width=4,
                                    selectInput("method","method:",
                                                c("rms","rsquared","correlation")),
                                    textInput("color1","point.color:",value = "red"),
                                    textInput("color2","line.color:",value = "blue"),
                                    actionButton("exclude_toggle", "Toggle points"),
                                    actionButton("exclude_reset", "Reset"),
                                    checkboxInput("update_data","live updates",F)),
                                tableOutput("Summary")
                        ))
                      
                      
                    )
                    
)



library(ggtree)
library(ggplot2)
library(ggpubr)
library(ape)
library(stringr)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux

date.type1<-function(tree,order)  ##input output
{
  date = numeric(length(tree$tip.label))##count tip
  if(order=="last"){
    for (i in 1:length(tree$tip.label)){
      result<-regexpr("\\d+(\\-?|\\d+)+(\\-?|\\d+)$",tree$tip.label[i])##this result[1] is location,result[2]is match length,result[3] is T or F.
      pos<-result[1]
      length<-attr(result,"match.length")-1
      time<-substr(tree$tip.label[i],pos,pos+length)##substr(x,start,stop),get the time
      date[i]<-time ## save in date
    }}
  else{
    for (i in 1:length(tree$tip.label)){
      order<-as.numeric(order)
      result<-gregexpr("\\d+(\\-?|\\d+)+(\\-?|\\d+)",tree$tip.label[i])
      pos<-result[[1]][order]
      length<-attr(result[[1]],"match.length")[order]-1
      time<-substr(tree$tip.label[i],pos,pos+length)
      date[i]<-time
    }}
  return(date)
}

date.type2<-function(tree,order,prefix)
{
  date = numeric(length(tree$tip.label))
  if(order=="first"){
    prefix2<-paste0("^\\",prefix,"\\d+\\D?\\d+\\D?\\d+")
  }
  if(order=="last"){
    prefix2<-paste0("\\",prefix,"\\d+\\D?\\d+\\D?\\d+$")
  }
  for (i in 1:length(tree$tip.label)){
    result<-regexpr(prefix2,tree$tip.label[i])
    pos<-result[1]
    length<-attr(result,"match.length")-1
    time<-substr(tree$tip.label[i],pos+1,pos+length)
    date[i]<-time
  }
  return(date)
}

date.type3<-function(tree,pattern)
{
  date = numeric(length(tree$tip.label))
  for (i in 1:length(tree$tip.label)){
    date[i]<-str_extract(tree$tip.label[i],pattern)
  }
  return(date)
}

date.numeric<-function(date,format) ## convert Date to decimal format
{
  if(format=="yy"|format=="yyyy"){
    date<-as.numeric(date)
  }
  if(format=="yyyy-MM-dd"){
    date<-Date2decimal(date)
  }
  if(format=="yyyy/MM/dd"){
    date<-as.Date(date,format = "%y/%m/%d")
    date<-Date2decimal(date)
  }
  if(format=="yyyy.MM.dd"){
    date<-as.Date(date,format = "%y.%m.%d")
    date<-Date2decimal(date)
  }
  if(format=="MM-dd-yyyy"){
    date<-as.Date(date,format = "%m-%d-%y")
    date<-Date2decimal(date)
  }
  if(format=="MM/dd/yyyy"){
    date<-as.Date(date,format = "%m/%d/%y")
    date<-Date2decimal(date)
  }
  if(format=="MM.dd.yyyy"){
    date<-as.Date(date,format = "%m.%d.%y")
    date<-Date2decimal(date)
  }
  return(date)
}

getdivergence<-function(tree,date,method)
{
  tree2<-rtt(tree,date,objective = method)## Root a Tree by Root-to-Tip Regression
  divergence<-data.frame(divergence=numeric(length(tree2$tip.label)))
  dataset<-cbind(tree2$edge,tree2$edge.length)
  dataset<-as.data.frame(dataset)
  names(dataset)<-c("from","to","length")
  for (i in 1:length(tree2$tip.label)) 
  {
    edge<-nodepath(tree2,from = length(tree2$tip.label)+1,to=i)
    for (j in 1:(length(edge)-1)) 
    {
      a<-edge[j]
      b<-edge[j+1]
      len<-dataset[which(dataset$from==a & dataset$to==b),]$length ##The start and end of the node are matched, and the length is assigned 
      divergence[i,1]=divergence[i,1]+len
    }
  }
  return(divergence)
}


# Define server logic required to draw a histogram
server <- function(input, output) { 
  
  observeEvent(input$type1,{toggle("order1")
  })
  observeEvent(input$type2,{toggle("order2")
  })
  observeEvent(input$type2,{toggle("prefix")
  })
  observeEvent(input$type3,{toggle("REGEX")
  })
  height <- reactive({
    return(input$height)
  })
  
  treeda<-reactive({
    inFile<-input$file1
    if(is.null(inFile))
      return(NULL)
    tree<-read.tree(inFile$datapath)
    return(tree)
  })
  
  vals <- reactiveValues(
    keeprows = rep(TRUE, 1)
  )
  
  tree.data<-reactiveValues(
    date=c(),
    divergence=c(),
    tip.label=c()
  )
  
  tree_data<-reactive({
    tree_data<-data.frame(date=tree.data$date,divergence=tree.data$divergence)
    row.names(tree_data)<-tree.data$tip.label
    return(tree_data)
  })
  
  observeEvent(input$file1,{
    tree<-treeda()
    vals$keeprows = rep(TRUE, length(tree$tip.label))
    print(vals$keeprows) 
  })
  
  observeEvent(input$plot2_click, {
    tree_data<-tree_data()
    res <- nearPoints(tree_data, input$plot2_click, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
    tree_data<-tree_data()
    res <- brushedPoints(tree_data, input$plot2_brush, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, length(vals$keeprows))
  })
  
  
  output$distplot1 <- renderPlot({    ##output tree plot by using ggtree
    tree<-treeda()
    p<-ggtree(tree,color=input$color3,size=input$size)
    if(input$tip){
      p1<-ggtree(tree,color=input$color3,size=input$size)+geom_tiplab(size=input$tipsize)
    if(input$update_data){
      tdf<-data.frame(Tiplabel=tree$tip.label,Group=vals$keeprows)
    p1<-p%<+%tdf+geom_tiplab(size=input$tipsize,aes(col=Group))
    }
      print(p1)
    }
    else{
      print(p)
    }
  },height=height)
  
  output$distPlot2 <- renderPlot({## Root-to-Tip Regression
    tree<-treeda()
    
    if(input$type1){
      date<-date.type1(tree,input$order1)
    }
    if(input$type2){
      date<-date.type2(tree,input$order2,input$prefix)
    }
    if(input$type3){
      date<-date.type3(tree,input$REGEX)
    }
    
    date<-date.numeric(date,input$format)
    divergence<-getdivergence(tree,date,input$method)
    tree.data$date=date
    tree.data$divergence=divergence
    tree.data$tip.label=tree$tip.label
    tree_data<-tree_data()
    keep    <- tree_data[ vals$keeprows, , drop = FALSE]
    exclude <- tree_data[!vals$keeprows, , drop = FALSE]
    p<-ggplot(keep, aes(x=date,y=divergence)) + geom_point(color=input$color1) +
      geom_text(aes(x=date, y=divergence, label = rownames(keep)))+
      geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25)+
      geom_text(data=exclude,aes(x=date, y=divergence, label = rownames(exclude)),alpha=0.25)
    if(input$update_data)
    {
      p2<-p+geom_smooth(data=keep,aes(x=date,y=divergence),method="lm",se=FALSE,colour=input$color2)
    }
    else
    {
      p2<-p+geom_smooth(data=tree_data,aes(x=date,y=divergence),method="lm",se=FALSE,colour=input$color2)
    }
   return(p2)
  })
  
  
  output$Summary<-renderTable({  
    tree<-treeda()
    if(input$update_data)
    {
      tree_data<-tree_data()
      tree_data_no<- tree_data[!vals$keeprows, , drop = FALSE]
      dele_label<-row.names(tree_data_no)
      print(tree_data_no)
      if(!is.null(dele_label)){
        tree<-drop.tip(tree,dele_label)}
    }
    date = numeric(length(tree$tip.label))
    if(input$type1){
      date<-date.type1(tree,input$order1)
    }
    if(input$type2){
      date<-date.type2(tree,input$order2,input$prefix)
    }
    if(input$type3){
      date<-date.type3(input$REGEX)
    }## according your choose to input date
    date<-date.numeric(date,input$format)
    divergence<-getdivergence(tree,date,input$method)
    print(divergence)
    df<-cbind(divergence,date)
    print(tree.data)
    if(input$format=="yy"|input$format=="yyyy"){
      range<-max(date)-min(date)
    }
    else{
      range<-max(date)-min(date)
      range<-range*365
    }
    ##make a summary and output
    summary<-data.frame(Dated.tips=c("Date range","Slope(rate)","X-Intercept","Correlation","R squared","RSE"),value=numeric(6))
    summary[1,2]<-range
    summary[4,2]<-as.numeric(cor(date,divergence))
    lm.rtt<-lm(df)
    summary[2,2]<-as.numeric(lm.rtt$coefficients[2])
    summary[3,2]<-as.numeric(abs(lm.rtt$coefficients[1]))/as.numeric(lm.rtt$coefficients[2])
    summary[5,2]<-summary(lm.rtt)$r.squared
    summary[6,2]<-summary(lm.rtt)[["sigma"]]
    print(summary)
  },
  digits = 5,width = 8)
  
  
  output$Sample<-renderTable({ ## output label and dates
    inFile<-input$file1
    if(is.null(inFile))
      return(NULL)
    tree<-read.tree(inFile$datapath)
    
    if(input$type1){
      date<-date.type1(tree,input$order1)
    }
    if(input$type2){
      date<-date.type2(tree,input$order2,input$prefix)
    }
    if(input$type3){
      date<-date.type3(tree,input$REGEX)
    }
    if(!is.character(date)){
      frame<-as.data.frame(tree$tip.label)
      print(frame)
    }
    else{
      frame<-cbind(tree$tip.label,date)
      colnames(frame)=c("tip.label","dates")
      print(frame)
    }
    
  })    
  
}

# Run the application 
shinyApp(ui = ui, server = server)