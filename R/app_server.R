#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import ggtree
#' @import ggplot2
#' @import stringr
#' @import Cairo
#' @import stats
#' @importFrom shinyjs toggle
#' @rawNamespace import(ggpubr, except = rotate)
#' @rawNamespace import(ape, except = rotate)
#' @importFrom ggtree rotate
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
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
    keeprows = rep(TRUE, 1),
    rightkeep=rep(TRUE,1),
    upkeep=rep(TRUE,1)
  )
  
  tree.data<-reactiveValues(
    date=c(),
    divergence=c(),
    tip.label=c(),
    Uplabelname=c(),
    dowmlabelname=c()
  )
  
  Group<-reactiveValues()
  abnormal<-reactiveValues()
  
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
    tree_data<-tree_data()
    all   <- tree_data
    all<-cbind(all,Group=vals$keeprows)
    all<-cbind(label=row.names(all),all)
    up   <- data.frame(label=tree.data$Uplabelname,abnormal=rep("up",length(tree.data$Uplabelname)))
    dowm <- data.frame(label=tree.data$dowmlabelname,abnormal=rep("dowm",length(tree.data$dowmlabelname)))
    d<-rbind(up,dowm)
    dd<-merge(all,d,by="label",all= T)
    p<-ggtree(tree,color=input$color3,size=input$size)
    p1<-p%<+%dd+geom_tippoint(aes(shape=abnormal),size=2.5,color="red")
    if(input$tip){
      p2<-p1+geom_tiplab(size=input$tipsize)
      if(input$update_data){
        p2<-p1+geom_tiplab(size=input$tipsize,aes(col=Group))
      }
      print(p2)
    }
    else{
      print(p1)
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
    m<-lm(divergence~date,tree_data)
    rst<-rstudent(m)
    UpValue<-tree_data[(0.5-abs(pt(rst,m$df.residual)-0.5))<input$pvalue/2&rst>0,]
    downValue<-tree_data[(0.5-abs(pt(rst,m$df.residual)-0.5))<input$pvalue/2&rst<0,]
    tree.data$Uplabelname<-row.names(UpValue)
    tree.data$dowmlabelname<-row.names(downValue)
    for (i in 1:(length(keep$date))) {
      if(keep$date[i]%in%UpValue$date||keep$date[i]%in%downValue$date)
      {
        vals$rightkeep[i]<-FALSE
        if(keep$date[i]%in%UpValue$date){
          vals$upkeep[i]<-TRUE
          vals$downkeep[i]<-FALSE
        }
        else{
          vals$upkeep[i]<-FALSE
          vals$downkeep[i]<-TRUE
        }
      }
      else{
        vals$rightkeep[i]<-TRUE
        vals$upkeep[i]<-FALSE
        vals$downkeep[i]<-FALSE
      }
    }
    Fupkeep   <-keep[ vals$upkeep, , drop = FALSE]
    Fdownkeep <-keep[ vals$downkeep, , drop = FALSE]
    Tkeep     <- keep[ vals$rightkeep, , drop = FALSE]
    p<-ggplot(Tkeep, aes(x=date,y=divergence)) + geom_point(color="black") +
      geom_text(aes(x=date, y=divergence, label = rownames(Tkeep)))+
      geom_point(data = Fupkeep, color = "red")+
      geom_text(data=Fupkeep,aes(x=date, y=divergence, label = rownames(Fupkeep)))+
      geom_point(data = Fdownkeep, color = "blue")+
      geom_text(data=Fdownkeep,aes(x=date, y=divergence, label = rownames(Fdownkeep)))+
      geom_point(data = exclude, color = "black", alpha = 0.25)+
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
