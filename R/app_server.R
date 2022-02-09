#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import ggtree
#' @import ggplot2
#' @import stringr
#' @import Cairo
#' @importFrom shinyjs toggle
#' @rawNamespace import(ggpubr, except = rotate)
#' @rawNamespace import(ape, except = rotate)
#' @importFrom ggtree rotate
#' @importFrom TSA runs
#' @importFrom forecast forecast
#' @importFrom aTSA adf.test
#' @importFrom stats acf
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
  height <- reactive({
    return(input$height)
  })
  
  treeda <- reactive({
    inFile <- input$file1
    if (is.null(inFile))  
      return(NULL)
    tree <- read.tree(inFile$datapath)
    return(tree)
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
  
  category <- reactiveValues()
  abnormal <- reactiveValues()
  
  tree_data <- reactive({
    tree_data <- data.frame(date=treeData$date, divergence=treeData$divergence)
    row.names(tree_data) <- treeData$tip.label
    return(tree_data)
  })
  
  observeEvent(input$file1, {
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
    for (i in 1:(length(keep$date))) {
      if (keep$date[i] %in% upvalue$date || keep$date[i] %in% downvalue$date) {
        vals$rightkeep[i] <- FALSE
        if (keep$date[i] %in% upvalue$date) {
          vals$upkeep[i] <- TRUE
          vals$downkeep[i] <- FALSE
        }
        else{
          vals$upkeep[i] <- FALSE
          vals$downkeep[i] <- TRUE
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
      geom_text(aes(x=date, y=divergence, label = rownames(true_keep))) +
      geom_point(data = wrong_upkeep, color = "red") +
      geom_text(data=wrong_upkeep, 
                aes(x=date, y=divergence, label = rownames(wrong_upkeep))) +
      geom_point(data = wrong_downkeep, color = "blue") +
      geom_text(data=wrong_downkeep, 
                aes(x=date, y=divergence, label = rownames(wrong_downkeep))) +
      geom_point(data = exclude, color = "black", alpha = 0.25) +
      geom_text(data=exclude, aes(x=date, y=divergence, 
                                  label = rownames(exclude)), alpha=0.25)
    if (input$update_data) {
      p2 <- p + geom_smooth(data=keep, aes(x=date, y=divergence), method="lm", 
                            se=FALSE, colour=input$color2)
    }
    else {
      p2 <- p+geom_smooth(data=tree_data, 
                          aes(x=date, y=divergence), method="lm",
                          se=FALSE, colour=input$color2)
    }
    return(p2)
  })
  output$distPlot3 <- renderPlot({
    tree_data <- tree_data()
    keep <- tree_data[vals$keeprows, , drop = FALSE]
    x <- keep$date
    y <- keep$divergence
    lm3 <- lm(y~x)
    residuals_3 <- rstudent(lm3)
    p3 <- plot(y=residuals_3, x=x, type="l", xlab="time", ylab="residuals") + 
      graphics::points(y=residuals_3, x=x, col="blue")
    return(p3)
  })
  output$distPlot4 <- renderPlot({
    tree_data <- tree_data()
    keep <- tree_data[vals$keeprows, , drop = FALSE]
    x <- keep$date
    y <- keep$divergence
    lm4 <- lm(y~x)
    residuals_4 <- rstudent(lm4)
    p4 <- acf(residuals_4)
    print(p4)
  })
  output$distPlot5 <- renderPlot({
    tree_data <- tree_data()
    keep    <- tree_data[vals$keeprows, , drop = FALSE]
    x <- keep$date
    y <- keep$divergence
    lm5 <- lm(y~x)
    residuals_5 <- rstudent(lm5)
    return(qqnorm(residuals_5))
  })
  output$distPlot6 <- renderPlot({
    tree_data <- tree_data()
    keep <- tree_data[vals$keeprows, , drop = FALSE]
    x <- keep$date
    y <- keep$divergence
    fra <- data.frame(time=x,div=y)
    fra <- fra[order(fra$time),]
    x <- fra$time
    y <- fra$div
    lm6 <- lm(y~x)
    residuals <- rstudent(lm6)
    dwelling <- ts(residuals)
    print(dwelling)
    if (input$fmethod == "ARIMA") {
      p<- dwelling %>% forecast::auto.arima() %>% forecast::forecast(
        as.numeric(input$hstep)) %>% autoplot(
          xlab = "Year", ylab = "residuals"
        )
    }
    if (input$fmethod == "ETS") {
      
      p<- dwelling %>% forecast::ets() %>% forecast::forecast(
        as.numeric(input$hstep)) %>% autoplot(
          xlab = "Year", ylab = "residuals"
        )
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
    date <- numeric(length(tree$tip.label))
    if (input$type1) {
      date <- dateType1(tree, input$order1)
    }
    if (input$type2) {
      date <- dateType2(tree, input$order2, input$prefix)
    }
    if (input$type3) {
      date <- dateType3(input$REGEX)
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
    summary[7, 2] <- shapiro.test(rstudent(lm(df)))
    summary[8, 2] <- runs(rstudent(lm(df)))
    print(summary)
  },
  digits = 5, width = 8)
  
  output$Sample <- renderTable({
    inFile <- input$file1
    if (is.null(inFile)) 
      return(NULL)
    tree <- read.tree(inFile$datapath)
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
