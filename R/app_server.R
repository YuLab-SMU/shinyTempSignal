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
#' @import ggpmisc
#
#' @importFrom shinyjs toggle
#' @rawNamespace import(ggpubr, except = rotate)
#' @rawNamespace import(ape, except = rotate)
#' @importFrom ggtree rotate
#' @importFrom treeio read.beast
#' @importFrom treeio read.codeml
#' @importFrom treeio merge_tree
#' @importFrom treeio rescale_tree
#' @importFrom treeio tree_subset
#' @importFrom treeio read.phylip.tree
#' @importFrom forecast forecast
#' @importFrom stats cor
#' @importFrom stats lm
#' @importFrom stats qqnorm
#' @importFrom stats rstudent
#' @importFrom stats pt
#' @importFrom stats shapiro.test
#' @importFrom stats ts
#' @importFrom stats acf
#' @importFrom DescTools RunsTest
#' @importFrom stats na.omit
#' @importFrom stats as.formula
#' @importFrom ggpmisc stat_poly_eq
#' @importFrom utils read.csv
#' @noRd
app_server <- function( input, output, session )  {
  category <- ..eq.label.. <- ..rr.label.. <- NULL
  observeEvent(input$plotClick, {
    tree <- tree()
    p <- ggtree(tree)
    x <- as.numeric(input$plotClick$x)
    y <- as.numeric(input$plotClick$y)
    node <- click_node(x, y, p$data)
    updateTextInput(session,"node",value = node)
  })
  click_node <- function(x, y, tr) {
    sq_dx <- (x - tr$x)^2
    sq_dy <- (y - tr$y)^2
    i <- which.min(sq_dx + sq_dy)
    node <- tr$node[i]
    return(node)
  }
  #1.读入树文件
  tree <- reactive({
    infile <- input$treefile
    if (!is.null(infile)) {
      if (input$filetype=="Newick") {
        tree <- read.tree(infile$datapath) %>% as.phylo()
      } else if (input$filetype=="beast") {
        tree <- read.beast(infile$datapath) %>% as.phylo()
      } else if (input$filetype=="Nexus") {
        tree <- read.nexus(infile$datapath)
      } else if (input$filetype=="phylip") {
        tree <- read.phylip.tree(infile$datapath) %>% as.phylo()
      }
      
      if (input$node != "") {
        
        if (as.numeric(input$node)<length(as.phylo(tree)$tip.label)) {
          stop("it is a tip label")
        }else{
          tree <- tree_subset(tree,as.numeric(input$node),levels_back = 0)
        }
      }
      return(tree)
    } else {
      return(NULL)
    }
  })
  #全部在外面取出来不就好了
  data <- reactive({
    tree <- tree()
    if (!is.null(tree)){
      tree <- tree %>% as.phylo()
      date <-dateType3(tree = tree,pattern = input$regression)
      date <- dateNumeric(date = date,format = input$format)
      divergence <- getdivergence(tree = tree)
      df <- cbind(label=tree$tip.label,date=date,divergence=divergence)
      return(df)
    }else{
      return(NULL)
    }
    
  })
  date <- reactive({
    tree <- tree()
    if (!is.null(tree)){
      tree <- tree %>% as.phylo()
      date <-dateType3(tree = tree,pattern = input$regression)
      date <- dateNumeric(date = date,format = input$format)
      return(date)
    }else{
      return(NULL)
    }
  })
  label <-reactive({
    tree <- tree()
    if (!is.null(tree)){
      tree <- tree %>% as.phylo()
      label <-tree$tip.label
      return(label)
    }else{
      return(NULL)
    }
  }) 
  
  divergence <- reactive({
    tree <- tree()
    if (!is.null(tree)){
      divergence <- getdivergence(tree = tree)
      return(divergence)
    }else{
      return(NULL)
    }
  })
  height <- reactive({
    return(input$height)
  })
  estimate <- function(df,p){lm=lm(df$divergence~ df$date)
  rst <- rstudent(lm)
  down <- 0.5-abs(0.5-pt(rst,lm$df.residual))< p / 2&rst<0
  up <- 0.5-abs(0.5-pt(rst,lm$df.residual))< p/ 2&rst>0
  return(list(down=down,up=up))}
  
  
  
  
  up.table <- reactive({
    df <- data()
    if (!is.null(df)) {
      pd <- estimate(df,p=input$pvalue)
      up.table <- df[pd$up,,drop=F]
      up.table$category <- rep("up",nrow(up.table))
      return(up.table)
    }else{return(NULL)}
  })
  down.table <- reactive({
    df <- data()
    if (!is.null(df)) {
      pd <- estimate(df,p=input$pvalue)
      down.table <- df[pd$down,,drop=F]
      down.table$category <- rep("down",nrow(down.table))
      return(down.table)
    }else{return(NULL)}
  })
  
  keep.table <- reactive({
    df <- data()
    if (!is.null(df)) {
      pd <- estimate(df,p=input$pvalue)
      keep <- pd$up==pd$down
      keep.table <- df[keep,,drop=F]
      return(keep.table)
    }else{return(NULL)}
  })
  exclude.table <- reactive({
    df <- data()
    if (!is.null(df)) {
      pd <- estimate(df,p=input$pvalue)
      keep <- pd$up==pd$down
      exclude.table <- df[!keep,,drop=F]
      return(exclude.table)
    }else{return(NULL)}
  })
  
  
  output$plot1 <- renderPlot({
    tree <- tree()
    if (is.null(tree)) {
      return()
    }
    df <- data()
    up.table <- up.table()
    down.table <- down.table()
    d <- rbind(up.table,down.table)
    all <- merge(d,df,by='label',all = T)
    if (input$tip) {
      p <- ggtree(tree,color=input$color3, size=input$size) + geom_tiplab()+geom_nodelab(aes(label=node),hjust=-.3)
      p%<+%all+ geom_tippoint(aes(shape=category,color=category))+
        geom_tiplab(aes(color=category))
    }else{
      p <- ggtree(tree,color=input$color3, size=input$size)+geom_nodelab(aes(label=node),hjust=-.3)
      p}
    
    
  },height = height)
  #2.取出日期
  output$datetable <- renderDataTable({
    tree <- tree()
    if (is.null(tree)) {
      return()
    }
    tree <- tree %>% as.phylo()
    date <-dateType3(tree = tree,pattern = input$regression)
    divergence <- getdivergence(tree = tree)
    df <- cbind(label=tree$tip.label,date=date,divergence=divergence)
    print(df)
  })
  
  #3.取出divergence，回归分析
  output$plot2 <- renderPlot({
    df <- data()
    up.table <- up.table()
    down.table <- down.table()
    ggplot(df, aes(x = date, y = divergence)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
      geom_point(data = down.table, aes(x = date, y = divergence), color = 'blue') +
      geom_point(data = up.table,aes(x = date, y = divergence), color ="red")+
      # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
      theme_bw() +
      stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
  })
  output$outliers <- renderDataTable({
    up.table <- up.table()
    down.table <- down.table()
    rbind(up.table,down.table)
  })
  observeEvent(input$delete,{
    output$plot2 <- renderPlot({
      exclude.table <- exclude.table()
      keep.table <- keep.table()
      ggplot(keep.table, aes(x = date, y = divergence)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
        geom_point(data = exclude.table, aes(x = date, y = divergence), color = 'gray') +
        # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
        theme_bw() +
        stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
    })
    output$plot3 <- renderPlot({
      need.exclude.table <- need.exclude.table()
      keep.table <- need.keep.table()
      ggplot(keep.table, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
        geom_point(data = need.exclude.table, aes_string(x = input$x_var, y = input$y_var), color = 'gray') +
        # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
        theme_bw() +
        stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
    })
    
    print("here do delete")
    output$plot1 <- renderPlot({
      tree <- tree()
      up.table <- up.table()
      down.table <- down.table()
      to_drop <- c(down.table$label,up.table$label)
      tip_reduced <- drop.tip(tree, to_drop)
      if (input$tip) {
        ggtree(tip_reduced) + geom_tiplab()+geom_nodelab(aes(label=node),hjust = -.3)
      }else{
        ggtree(tip_reduced)+geom_nodelab(aes(label=node),hjust = -.3)
      }
      
    },height = height)
    
    
  })
  observeEvent(input$reset,{
    output$plot1 <- renderPlot({
      tree <- tree()
      if (is.null(tree)) {
        return()
      }
      df <- data()
      up.table <- up.table()
      down.table <- down.table()
      d <- rbind(up.table,down.table)
      all <- merge(d,df,by='label',all = T)
      if (input$tip) {
        p <- ggtree(tree,color=input$color3, size=input$size) + geom_tiplab()+geom_nodelab(aes(label=node),hjust=-.3)
        p%<+%all+ geom_tippoint(aes(shape=category,color=category))+
          geom_tiplab(aes(color=category))
      }else{
        p <- ggtree(tree,color=input$color3, size=input$size)+geom_nodelab(aes(label=node),hjust=-.3)
        p}
    },height = height)
    
    output$plot2 <- renderPlot({
      df <- data()
      up.table <- up.table()
      down.table <- down.table()
      ggplot(df, aes(x = date, y = divergence)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
        geom_point(data = down.table, aes(x = date, y = divergence), color = 'blue') +
        geom_point(data = up.table,aes(x = date, y = divergence), color ="red")+
        # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
        theme_bw() +
        stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
    })
    output$plot3 <- renderPlot({
      df1 <- merged_data()
      df <- df1[,c("label",input$x_var,input$y_var)]
      need.up.table <- need.up.table()
      need.down.table <- need.down.table()
      ggplot(df, aes_string(x = input$x_var, y =input$y_var)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
        geom_point(data = need.down.table, aes_string(x = input$x_var, y =input$y_var), color = 'blue') +
        geom_point(data = need.up.table,aes_string(x = input$x_var, y =input$y_var), color ="red")+
        # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
        theme_bw() +
        stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
    })
    print("here do reset")
    
  })
  
  output$dataframe <- renderDataTable({
    tree <- tree()%>%as.phylo()
    #1 set parameters needed
    a = length(tree$tip.label) + 1
    b = length(tree$tip.label) + tree$Nnode
    sub.tree <- list()
    date <- list()
    divergence <- list()
    df_td = list()
    model.results <- list()
    modele <- list()
    
    for (i in a:b) {
      f=i-a+1
      sub.tree[[f]] <- tree_subset(tree = tree,node = i,levels_back = 0)
      date[[f]] <- dateType3(sub.tree[[f]],pattern = input$regression) %>% dateNumeric(format = input$format) 
      divergence[[f]] <- getdivergence(tree = sub.tree[[f]])
      df_td[[f]] <- as.data.frame(cbind(date=date[[f]],divergence=divergence[[f]]))
      model.results[[f]] <- if (unique(is.na(divergence[[f]]))) {
        NA
      } else{
        
        m <- lm(divergence ~ date, data = df_td[[f]])
        rst <- rstudent(m)
        upval <- c((0.5 - abs(pt(
          rst, m$df.residual
        ) - 0.5))
        < input$pvalue/ 2 & rst > 0)
        downval <- c((0.5 - abs(pt(
          rst, m$df.residual
        ) - 0.5))
        < input$pvalue / 2 & rst < 0)
        modele <-
          summary(lm(divergence ~ date, data = df_td[[f]]))
        data.frame(
          node = i,
          r.squared = modele$r.squared,
          adj.r.squared = modele$adj.r.squared,
          pvalue = modele$coefficients[nrow(modele$coefficients), ncol(modele$coefficients)],
          slope = modele$coefficients[nrow(modele$coefficients), 1],
          intercept = modele$coefficients[1, 1],
          up = length(which(upval == T)),
          down = length(which(downval == T)),
          total_abnormal = length(which(upval == T)) + length(which(downval == T))
        )
      }}
    df <- na.omit(do.call(rbind, model.results))
    df[order(df$total_abnormal, decreasing = T), ]
  })
  data2 <- reactive({
    req(input$outdata)
    read.csv(input$outdata$datapath)
  })
  
  existing_data <-reactive({
    date <- date()
    divergence <- divergence()
    label <- label()
    df <- data.frame(label=label,divergence=divergence,date=date)
    
    return(df)
  })
  
  merged_data <- reactive({
    req(data2())
    # 将上传的表格与现有的表格合并
    if (!is.null(data2())) {
      existing_data <- existing_data()
      merged <- merge(existing_data, data2())
      merged
    } else {
      
    }
  })
  observeEvent(merged_data(), {
    updateSelectInput(session, "x_var", choices = colnames(merged_data()))
    updateSelectInput(session, "y_var", choices = colnames(merged_data()))
  })
  
  output$data_table <- renderDataTable({
    req(merged_data())
    merged_data()
  })
  regression <- reactive({
    req(input$regression_btn)
    lm(as.formula(paste(input$y_var, "~", input$x_var)), merged_data())
  })
  estimate2 <- function(lm,p){
    rst <- rstudent(lm)
    down <- 0.5-abs(0.5-pt(rst,lm$df.residual))< p / 2&rst<0
    up <- 0.5-abs(0.5-pt(rst,lm$df.residual))< p/ 2&rst>0
    return(list(down=down,up=up))}
  need.up.table <- reactive({
    df1 <- merged_data()
    df <- df1[,c("label",input$x_var,input$y_var)]
    if (!is.null(df)) {
      lm=regression()
      pd <- estimate2(lm=lm,p=input$pvalue)
      up.table <- df[pd$up,,drop=F]
      up.table$category <- rep("up",nrow(up.table))
      return(up.table)
    }else{return(NULL)}
  })
  
  need.down.table <- reactive({
    df1 <- merged_data()
    df <- df1[,c("label",input$x_var,input$y_var)]
    if (!is.null(df)) {
      lm=regression()
      pd <- estimate2(lm=lm,p=input$pvalue)
      up.table <- df[pd$down,,drop=F]
      up.table$category <- rep("down",nrow(up.table))
      return(up.table)
    }else{return(NULL)}
  })
  need.keep.table <- reactive({
    df1 <- merged_data()
    df <- df1[,c("label",input$x_var,input$y_var)]
    if (!is.null(df)) {
      lm=regression()
      pd <- estimate2(lm=lm,p=input$pvalue)
      keep <- pd$up==pd$down
      keep.table <- df[keep,,drop=F]
      return(keep.table)
    }else{return(NULL)}
  })
  need.exclude.table <- reactive({
    df1 <- merged_data()
    df <- df1[,c("label",input$x_var,input$y_var)]
    if (!is.null(df)) {
      lm=regression()
      pd <- estimate2(lm=lm,p=input$pvalue)
      keep <- pd$up==pd$down
      exclude.table <- df[!keep,,drop=F]
      return(exclude.table)
    }else{return(NULL)}
  })
  
  
  output$plot3 <- renderPlot({
    df1 <- merged_data()
    df <- df1[,c("label",input$x_var,input$y_var)]
    need.up.table <- need.up.table()
    need.down.table <- need.down.table()
    ggplot(df, aes_string(x = input$x_var, y =input$y_var)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
      geom_point(data = need.down.table, aes_string(x = input$x_var, y =input$y_var), color = 'blue') +
      geom_point(data = need.up.table,aes_string(x = input$x_var, y =input$y_var), color ="red")+
      # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
      theme_bw() +
      stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
  })
  
  output$outliers2 <- renderDataTable({
    need.up.table <- need.up.table()
    need.down.table <- need.down.table()
    rbind(need.up.table,need.down.table)
  })
  
}
