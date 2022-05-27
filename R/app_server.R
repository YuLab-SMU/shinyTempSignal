#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import ggtree
#' @import ggplot2
#' @import stringr
#' @import ggprism
#' @import Cairo
#' @importFrom shinyjs toggle
#' @rawNamespace import(ggpubr, except = rotate)
#' @rawNamespace import(ape, except = rotate)
#' @importFrom ggtree rotate
#' @importFrom treeio read.beast
#' @importFrom treeio read.codeml
#' @importFrom treeio merge_tree
#' @importFrom treeio rescale_tree
#' @importFrom TSA runs
#' @importFrom TSA acf
#' @importFrom forecast forecast
#' @importFrom aTSA adf.test
#' @importFrom stats cor
#' @importFrom stats lm
#' @importFrom stats qqnorm
#' @importFrom stats rstudent
#' @importFrom stats pt
#' @importFrom stats shapiro.test
#' @importFrom stats ts

#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  observeEvent(input$type1, {toggle("order1")
  })
  observeEvent(input$type2, {toggle("order2")
  })
  observeEvent(input$type2, {toggle("prefix")
  })
  observeEvent(input$type3, {toggle("REGEX")
  })
  observeEvent(input$type4, {toggle("file1")
  })
  observeEvent(input$type5, {toggle("file2")
  })
  observeEvent(input$type5, {toggle("file3")
  })
  observeEvent(input$type5, {toggle("file4")
  })
  height <- reactive({
    return(input$height)
  })
  
  treeda <- reactive({
    if (input$fileinput==1){
      inFile <- input$file1
      beast_file <- input$file2
      mlc_file <- input$file3
      rst_file <- input$file4
      if (!is.null(inFile)){
        tree <- read.tree(inFile$datapath)
        return(tree)
      }
      beast_tree <- read.beast(beast_file$datapath)
      codeml_tree <- read.codeml(rst_file$datapath, mlc_file$datapath)
      merged_tree <- merge_tree(beast_tree, codeml_tree)
      tree<-fortify(merged_tree)
      treea1 <- rescale_tree(merged_tree, input$distance)
      treea2 <- treea1@phylo
      return(treea2)
    }
    return(NULL)
  })
  
  vals <- reactiveValues(
    keeprows=rep(TRUE, 1),
    rightkeep=rep(TRUE, 1),
    upkeep=rep(TRUE, 1)
  )
  
  treeData <- reactiveValues(
    date=c(),
    divergence=c(),
    up_labelname=c(),
    down_labelname=c()
  )
  
  mySetTheme <- function()
  {
    mySetTheme <- theme_prism(base_size = 14, border = TRUE)+
      theme(panel.grid.major = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = "dotted"))+
      theme(panel.background =  element_rect(fill = "linen", 
                                             colour = "grey50"))
    return(mySetTheme)
  }
    
  category <- reactiveValues()
  abnormal <- reactiveValues()
  
  tree_data <- reactive({
    tree_data <- data.frame(date=treeData$date, divergence=treeData$divergence)
    row.names(tree_data) <- treeData$tip.label
    return(tree_data)
  })
  
  observeEvent(input$fileinput, {
    tree <- treeda()
    vals$keeprows <- rep(TRUE, length(tree$tip.label))
  }) 
  
  observeEvent(input$plot2_click, {
    tree_data <- tree_data()
    res <- nearPoints(tree_data, input$plot2_click, allRows=TRUE)
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
    tree_data <- tree_data()
    res <- brushedPoints(tree_data, input$plot2_brush, allRows=TRUE)
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, length(vals$keeprows))
  })
  
  ##output tree plot by using ggtree
  output$distplot1 <- renderPlot({    
    tree <- treeda()
    tree_data <- tree_data()
    all <- tree_data
    all <- cbind(all, category=vals$keeprows)
    all <- cbind(label=row.names(all), all)
    up <- data.frame(label=treeData$up_labelname, 
                     abnormal=rep("up", length(treeData$up_labelname)))
    dowm <- data.frame(label=treeData$down_labelname, 
                       abnormal=rep("dowm", length(treeData$down_labelname)))
    d <- rbind(up, dowm)
    dd <- merge(all, d, by="label", all=TRUE)
    p <- ggtree(tree, color=input$color3, size=input$size)
    p1 <- p %<+% dd + geom_tippoint(aes(shape=abnormal), size=2.5, color="red")
    if (input$tip) {
      p2 <- p1 + geom_tiplab(size=input$tipsize)
      if (input$update_data) {
        p2 <- p1 + geom_tiplab(size=input$tipsize, aes(col=category))
      }
      print(p2)
    }
    else{
      print(p1)
    }
  }, height=height)
  
  output$distPlot2 <- renderPlot({## Root-to-Tip Regression
    tree <- treeda()
    if (input$type1) {
      date <- dateType1(tree, input$order1)
    }
    if (input$type2) {
      date <- dateType2(tree, input$order2, input$prefix)
    }
    if (input$type3) {
      date <- dateType3(tree, input$REGEX)
    }
    date <- dateNumeric(date, input$format)
    divergence <- getdivergence(tree, date, input$method)
    treeData$date <- date
    treeData$divergence <- divergence
    treeData$tip.label <- tree$tip.label
    tree_data <- tree_data()
    keep <- tree_data[vals$keeprows, , drop = FALSE]
    exclude <- tree_data[!vals$keeprows, , drop = FALSE]
    m <- lm(divergence~date, tree_data)
    rst <- rstudent(m)
    upvalue <- tree_data[(0.5 - abs(pt(rst, m$df.residual) - 0.5)) 
                         < input$pvalue / 2 & rst > 0, ]
    downvalue <- tree_data[(0.5 - abs(pt(rst, m$df.residual) - 0.5)) 
                           < input$pvalue / 2 & rst < 0, ]
    treeData$up_labelname <- row.names(upvalue)
    treeData$down_labelname <- row.names(downvalue)
    u <- 1
    v <- 1
    for (i in 1:(length(keep$divergence))) {
      if ((isTRUE(keep$divergence[i] == upvalue$divergence[u]) &
           isTRUE(keep$date[i] == upvalue$date[u]))
          || (isTRUE(keep$divergence[i] == downvalue$divergence[v]) & 
              isTRUE(keep$date[i] == downvalue$date[v]))) {
        vals$rightkeep[i] <- FALSE
        if (isTRUE(keep$divergence[i] == upvalue$divergence[u]) &
            isTRUE(keep$date[i] == upvalue$date[u])) {
          vals$upkeep[i] <- TRUE
          vals$downkeep[i] <- FALSE
          u <- u+1
        }
        else{
          vals$upkeep[i] <- FALSE
          vals$downkeep[i] <- TRUE
          v <- v+1
        }
      }
      else{
        vals$rightkeep[i] <- TRUE
        vals$upkeep[i] <- FALSE
        vals$downkeep[i] <- FALSE
      }
    }
    wrong_upkeep <- keep[vals$upkeep, , drop = FALSE]
    wrong_downkeep <- keep[vals$downkeep, , drop = FALSE]
    true_keep     <- keep[vals$rightkeep, , drop = FALSE]
    p <- ggplot(true_keep,
                aes(x=date, y=divergence)) + geom_point(color="black") +
      geom_point(data = wrong_upkeep, color = "red") +
      geom_point(data = wrong_downkeep, color = "blue") +
      geom_point(data = exclude, color = "black", alpha = 0.25) 
    if (input$tip2){
      p <- p+geom_text(aes(x=date, y=divergence, label = rownames(true_keep)))+
        geom_text(data=wrong_upkeep, 
                  aes(x=date, y=divergence, label = rownames(wrong_upkeep)))+
        geom_text(data=wrong_downkeep, 
                  aes(x=date, y=divergence, label = rownames(wrong_downkeep)))+
        geom_text(data=exclude, aes(x=date, y=divergence, 
                                    label = rownames(exclude)), alpha=0.25)
    }
    if (input$update_data) {
      p2 <- p + geom_smooth(data=keep, aes(x=date, y=divergence), method="lm", 
                            se=FALSE, colour=input$color2)
    }
    else {
      p2 <- p+geom_smooth(data=tree_data, 
                          aes(x=date, y=divergence), method="lm",
                          se=FALSE, colour=input$color2)
    }
    p2 <- p2 + mySetTheme() 
    return(p2)
  })
  output$distPlot3 <- renderPlot({
    time <- NULL
    tree_data <- tree_data()
    keep <- tree_data[vals$keeprows, , drop = FALSE]
    x <- keep$date
    y <- keep$divergence
    fra <- data.frame(time=x, res=y)
    fra <- meangroup(fra)
    x <- fra$time
    y <- fra$res
    lm3 <- lm(y~x)
    residuals_3 <- rstudent(lm3)
    fra$res <- residuals_3
    p3 <- ggplot(fra,
                 aes(x=time, y=res)) + geom_point(color="blue") + 
      geom_line()+labs(title="residuals plot")+mySetTheme()
    return(p3)
  })
  output$distPlot4 <- renderPlot({
    time <- NULL
    lag <- NULL
    tree_data <- tree_data()
    keep <- tree_data[vals$keeprows, , drop = FALSE]
    x <- keep$date
    y <- keep$divergence 
    fra <- data.frame(time=x,div=y)
    fra <- fra[order(fra$time),]
    x <- fra$time
    y <- fra$div
    lm4 <- lm(y~x)
    residuals_4 <- rstudent(lm4)
    bacf <- TSA::acf(residuals_4, plot = FALSE)
    h <- stats::sd(abs(as.numeric(bacf$acf)))*2
    bacfdf <- with(bacf, data.frame(lag, acf))
    p4 <- ggplot(data=bacfdf, mapping = aes(x=lag, y=acf))+
      geom_segment(mapping = aes(xend = lag, yend = 0)
                   , color='black', size = 1,alpha=I(1/2))+
      geom_hline(yintercept = h, linetype = 2, color = 'darkblue')+
      geom_hline(yintercept = -h, linetype = 2, color = 'darkblue')+
      geom_hline(yintercept = 0, linetype = 1, color = 'black')+
      labs(title="ACF plot")+theme(plot.title = element_text(hjust = 0.5))+
      mySetTheme()
    print(p4)
  })
  output$distPlot5 <- renderPlot({
    time <- NULL
    tree_data <- tree_data()
    keep    <- tree_data[vals$keeprows, , drop = FALSE]
    x <- keep$date
    y <- keep$divergence
    fra <- data.frame(time=x, res=y)
    lm3 <- lm(y~x)
    residuals_3 <- rstudent(lm3)
    fra$res <- residuals_3
    p5 <-ggplot(fra,aes(sample=res))+
      stat_qq()+labs(title="Normal Q-Q plot")+mySetTheme()
    return(p5)
  })
  output$distPlot6 <- renderPlot({
    time <- NULL
    tree_data <- tree_data()
    keep <- tree_data[vals$keeprows, , drop = FALSE]
    x <- keep$date
    x <- round(x, 1)
    y <- keep$divergence
    fra <- data.frame(time=x,res=y)
    #为丢失的年份设定缺失值
    q <- seq(from=min(x), to=max(x), 0.1)
    for (i in q) {
      if(!i%in%fra$time){
        addfra <- data.frame(time=i, res=NA)
        fra <- rbind(fra, addfra)
      }
    }
    fra <- meangroup(fra)
    x <- fra$time
    y <- fra$res
    lm6 <- lm(y~x)
    rsdata <- rstudent(lm6)
    j <- 1
    for (i in 1:length(x)) {
      if(!is.na(fra$res[i])){
        fra$res[i] <- rsdata[j]
        j <- j+1
      }
    }
    dwelling <- ts(fra$res, start=min(x), frequency=10)
    #填补缺失值
    dwelling <- forecast::na.interp(dwelling)
    if (input$fmethod == "ARIMA") {
      p<- dwelling %>% forecast::auto.arima() %>% forecast::forecast(
        as.numeric(input$hstep)) %>% ggplot2::autoplot(
          xlab = "Year", ylab = "residuals"
        )+mySetTheme()
    }
    if (input$fmethod == "ETS") {
      p<- dwelling %>% forecast::ets() %>% forecast::forecast(
        as.numeric(input$hstep)) %>% ggplot2::autoplot(
          xlab = "Year", ylab = "residuals"
        )+mySetTheme()
    }
    print(p)
  })
  output$Summary <- renderTable({  
    tree <- treeda()
    if (input$update_data) {
      tree_data <- tree_data()
      tree_data_no <- tree_data[!vals$keeprows, , drop = FALSE]
      dele_label <- row.names(tree_data_no)
      if (!is.null(dele_label)) {
        tree <- drop.tip(tree, dele_label)
      }
    }
    if (input$type1) {
      date <- dateType1(tree, input$order1)
    }
    if (input$type2) {
      date <- dateType2(tree, input$order2, input$prefix)
    }
    if (input$type3) {
      date <- dateType3(tree, input$REGEX)
    }
    date <- dateNumeric(date, input$format)
    divergence <- getdivergence(tree, date, input$method)
    df <- cbind(divergence, date)
    if (input$format == "yy" | input$format == "yyyy") {
      range <- max(date) - min(date)
    }
    else{
      range <- max(date) - min(date)
      range <- range * 365
    }
    ##make a summary and output
    summary <- data.frame(Dated.tips=c("Date range", "Slope(rate)", 
                                       "X-Intercept", "Correlation", 
                                       "R squared", "RSE", "Shapiro-test", 
                                       "Runs-test"), value=numeric(8))
    summary[1, 2] <- range
    summary[4, 2] <- as.numeric(cor(date, divergence))
    lm.rtt <- lm(df)
    summary[2, 2] <- as.numeric(lm.rtt$coefficients[2])
    summary[3, 2] <- as.numeric(
      abs(lm.rtt$coefficients[1])) / as.numeric(lm.rtt$coefficients[2])
    summary[5, 2] <- summary(lm.rtt)$r.squared
    summary[6, 2] <- summary(lm.rtt)[["sigma"]]
    summary[7, 2] <- shapiro.test(rstudent(lm(df)))[2]
    summary[8, 2] <- runs(rstudent(lm(df)))
    print(summary)
  },
  digits = 5, width = 8)
  
  output$Sample <- renderTable({
    tree <- treeda()
    if (input$type1) {
      date <- dateType1(tree, input$order1)
    }
    if (input$type2) {
      date <- dateType2(tree, input$order2, input$prefix)
    }
    if (input$type3) {
      date <- dateType3(tree, input$REGEX)
    }
    if (!is.character(date)) {
      frame <- as.data.frame(tree$tip.label)
      print(frame)
    }
    else{
      frame <- cbind(tree$tip.label, date)
      colnames(frame) <- c("tip.label", "dates")
      print(frame)
    }
  })      
}
