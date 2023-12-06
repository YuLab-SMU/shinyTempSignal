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
#' @importFrom ape extract.clade
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
#' @importFrom utils write.csv
#' @noRd

app_server <- function( input, output, session )  {
  # tree <- eventReactive(input$fileinput, {
  #     infile <- input$treefile
  #     if (!is.null(infile)) {
  #       if (input$filetype=="Newick") {
  #         tree <- read.tree(infile$datapath) %>% as.phylo()
  #       } else if (input$filetype=="beast") {
  #         tree <- read.beast(infile$datapath) %>% as.phylo()
  #       } else if (input$filetype=="Nexus") {
  #         tree <- read.nexus(infile$datapath)
  #       } else if (input$filetype=="phylip") {
  #         tree <- read.phylip.tree(infile$datapath) %>% as.phylo()
  #       }
        
  #       if (input$node != "") {
          
  #         if (as.numeric(input$node)<length(as.phylo(tree)$tip.label)) {
  #           stop("it is a tip label")
  #         }else{
  #           tree <- extract.clade(tree,node = as.numeric(input$node))#tree_subset(tree,as.numeric(input$node),levels_back = 0)
  #         }
  #       }
  #       return(tree)
  #     } else {
  #       return(NULL)
  #     }
  #   })
  mySetTheme <- function()
  {
    mySetTheme <- theme_prism(base_size = 14, border = TRUE) +
      theme(panel.grid.major = element_line(
        colour = "grey",
        size = 0.5,
        linetype = "dotted"
      )) +
      theme(panel.background =  element_rect(fill = "linen",
                                             colour = "grey50"))
    return(mySetTheme)
  }
  mySetTheme2 <- function()
  {
    mySetTheme <- 
    theme(panel.background =  element_rect(fill = "linen",
                                             colour = "grey50"))
    return(mySetTheme)
  }
  
  options(shiny.maxRequestSize = 4000*1024^2)
  category <- ..eq.label.. <- ..rr.label.. <- NULL
  observeEvent(input$plotClick, {
  if(!is.null(sub_tree())) {
    tree <- sub_tree()
  } else {
    tree <- tree()
  }
    p <- ggtree(tree,color=input$color3, size=input$size)+mySetTheme2()+theme(legend.position="none")
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
  sub_tree <- eventReactive(
    input$node, {
      if(input$node != "") {
        extract.clade(tree(), node = as.numeric(input$node))
      } else {
        tree()
      }
    }
  )
  #1.读入树文件
  tree <- eventReactive(input$fileinput, {
    req(!is.null(input$treefile))
    if (input$filetype=="Newick") {
      tree <- read.tree(input$treefile$datapath) %>% as.phylo()
    } else if (input$filetype=="beast") {
      tree <- read.beast(input$treefile$datapath) %>% as.phylo()
    } else if (input$filetype=="Nexus") {
      tree <- read.nexus(input$treefile$datapath)
    } else if (input$filetype=="phylip") {
      tree <- read.phylip.tree(input$treefile$datapath) %>% as.phylo()
    }
    tree
  })
  observeEvent(
    tree(), {
      tree <- tree()
      root_node <- length(tree$tip.label) + 1
      updateTextInput(session, "node", value = root_node)
    }
  )
  #全部在外面取出来不就好了
  data <- reactive({
    tree <- sub_tree()
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
    tree <- sub_tree()
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
    tree <- sub_tree()
    if (!is.null(tree)){
      tree <- tree %>% as.phylo()
      label <-tree$tip.label
      return(label)
    }else{
      return(NULL)
    }
  }) 
  
  divergence <- reactive({
    tree <- sub_tree()
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
    tree <- sub_tree()
    if (is.null(tree)) {
      return(NULL)
    }
    df <- data()
    up.table <- up.table()
    down.table <- down.table()
    d <- rbind(up.table,down.table)
    all <- merge(d,df,by='label',all = T)
    p <- ggtree(tree,color=input$color3, size=input$size)+mySetTheme2()+theme(legend.position="none") 
    if(input$geom_nodelab){p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
    if (input$tip) {
      
      p <- p+ geom_tiplab(size=input$tipsize)
      p <- p%<+%all+geom_tiplab(aes(color=category))+
        scale_color_manual(values = c("up" = "red", "down" = "blue"))
    }else if(input$tip_point){
      if(input$geom_nodelab){p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
      p <- p%<+%all+ geom_tippoint(aes(color=category),size=input$tipsize)+
        geom_tippoint(aes(color=category),size=input$tipsize)+
        scale_color_manual(values = c("up" = "red", "down" = "blue"))
    }
    plotd1(p)
    p
  },height = height
  )
  plotd1 <- reactiveVal()
  output$downloadplot1 <- downloadHandler(
    filename = function() {
      "tree_plot.pdf"  # 指定要保存的文件名
    },
    content = function(file) {
      p <- plotd1()
      ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
    }
  )
  
 
  
  #2.取出日期
  output$datetable <- renderDataTable({
    tree <- sub_tree()
    if (is.null(tree)) {
      return()
    }
    tree <- tree %>% as.phylo()
    date <-dateType3(tree = tree,pattern = input$regression)
    divergence <- getdivergence(tree = tree)
    df <- cbind(label=tree$tip.label,date=date,divergence=divergence)
    table1(df)
    df
  })
  table1 <- reactiveVal()
  output$download1 <- downloadHandler(
    filename = function(){
      "Sample-Dates.csv"
    },
    content = function(file){
      df <- table1()
      write.csv(df,file,row.names = FALSE)
    }
  )
  
  #3.取出divergence，回归分析
  output$plot2 <- renderPlot({
    tree <- sub_tree()
    if (is.null(tree)) {
      return()
    }
    df <- data()
    up.table <- up.table()
    down.table <- down.table()
    p <- ggplot(df, aes(x = date, y = divergence)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, formula = y ~ x,colour=input$color2) +
      geom_point(data = down.table, aes(x = date, y = divergence), color = 'blue') +
      geom_point(data = up.table,aes(x = date, y = divergence), color ="red")+
      # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
      mySetTheme() +
      stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
    plotd2(p)
    print(p)
  })
  plotd2 <- reactiveVal()
  output$downloadplot2 <- downloadHandler(
    filename = function() {
      "regression_plot.pdf"  # 指定要保存的文件名
    },
    content = function(file) {
      p <- plotd2()
      ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
    }
  )
  
  output$outliers <- renderDataTable({
    up.table <- up.table()
    down.table <- down.table()
    rbind(up.table,down.table)
  })
  observeEvent(input$delete,{
    
    output$Summary <- renderTable({  
      keep <- keep.table()
      date <- keep$date|> unlist()|>as.numeric()
      divergence <-  keep$divergence|>unlist()|>as.numeric()
      df <- cbind(divergence, date)|>as.data.frame()
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
                                         "R squared", "RSE"), value=numeric(6))
      summary[1, 2] <- range
      summary[4, 2] <- as.numeric(cor(date, divergence))
      lm.rtt <- lm(df)
      summary[2, 2] <- as.numeric(lm.rtt$coefficients[2])
      summary[3, 2] <- as.numeric(
        abs(lm.rtt$coefficients[1])) / as.numeric(lm.rtt$coefficients[2])
      summary[5, 2] <- summary(lm.rtt)$r.squared
      summary[6, 2] <- summary(lm.rtt)[["sigma"]]
      # summary[7, 2] <- shapiro.test(rstudent(lm(df)))[2]
      # summary[8, 2] <- DescTools::RunsTest(rstudent(lm(df)))$p.value
      table_dt(summary)
      print(summary)
    },
    digits = 5, width = 8)
    
    table_dt <- reactiveVal()
    output$download_dt2 <- downloadHandler(
      filename = function(){
        "summary(delete).csv"
      },
      content = function(file){
        df <- table_dt()
        write.csv(df,file,row.names = FALSE)
      }
    )
    output$plot2 <- renderPlot({
      exclude.table <- exclude.table()
      keep.table <- keep.table()
      p <- ggplot(keep.table, aes(x = date, y = divergence)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, formula = y ~ x,colour=input$color2) +
        geom_point(data = exclude.table, aes(x = date, y = divergence), color = 'gray') +
        # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
        mySetTheme() +
        stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
      plotd2(p)
      print(p)
    })
    plotd2 <- reactiveVal()
    
    output$downloadplot2 <- downloadHandler(
      filename = function() {
        "regression(deleted_plot.pdf"  # 指定要保存的文件名
      },
      content = function(file) {
        p <- plotd2()
        ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
      }
    )
    
    output$plot3 <- renderPlot({
      need.exclude.table <- need.exclude.table()
      keep.table <- need.keep.table()
      p <- ggplot(keep.table, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, formula = y ~ x,colour=input$color2) +
        geom_point(data = need.exclude.table, aes_string(x = input$x_var, y = input$y_var), color = 'gray') +
        # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
        mySetTheme() +
        stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
      plotd3(p)
      print(p)
    })
    plotd3 <- reactiveVal()
    
    output$downloadplot3 <- downloadHandler(
      filename = function() {
        "regression(deleted)_plot.pdf"  # 指定要保存的文件名
      },
      content = function(file) {
        p <- plotd3()
        ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
      }
    )
    
    print("here do delete")
    output$plot1 <- renderPlot({
      tree <- sub_tree()
      up.table <- up.table()
      down.table <- down.table()
      to_drop <- c(down.table$label,up.table$label)
      tip_reduced <- drop.tip(tree, to_drop)
      p <- ggtree(tip_reduced,color=input$color3, size=input$size)+mySetTheme2() 
      if(input$geom_nodelab){p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
      if (input$tip) {
        p <- p+ geom_tiplab(size=input$tipsize)
      }else if(input$tip_point){
        if(input$geom_nodelab){p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
        p <- p+ geom_tippoint(size=input$tipsize,color="gray")+
          geom_tippoint(size=input$tipsize)
        print(p)
      }else{
        p <- p
      }
      plotd1(p)
      print(p)
    },height = height)
    plotd1 <- reactiveVal()
    output$downloadplot1 <- downloadHandler(
      filename = function() {
        "tree_plot(delete).pdf"  # 指定要保存的文件名
      },
      content = function(file) {
        p <- plotd1()
        ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
      }
    )
    
    output$plot8 <- renderPlot({
      time <- NULL
      keep <- keep.table()
      x <- keep$date|> unlist()|>as.numeric()
      y <- keep$divergence|>unlist()|>as.numeric()
      fra <- data.frame(time=x, res=y)
      lm3 <- lm(y~x)
      residuals_3 <- rstudent(lm3)
      fra$res <- residuals_3
      p5 <-ggplot(fra,aes(sample=res))+
        stat_qq()+labs(title="Normal Q-Q plot")+mySetTheme()#+mySetTheme()
      plotd8(p5)
      return(p5)
      
    })
    plotd8 <- reactiveVal()
    
    output$downloadplot8 <- downloadHandler(
      filename = function() {
        "QQ_plot(delete).pdf"  # 指定要保存的文件名
      },
      content = function(file) {
        p <- plotd8()
        ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
      }
    )
    
    output$plot9 <- renderPlot({
      time <- NULL
      keep <- keep.table()
      x <- keep$date|> unlist()|>as.numeric()
      y <- keep$divergence|>unlist()|>as.numeric()
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
      plotd9(p3)
      return(p3)
    })
    plotd9 <- reactiveVal()
    
    output$downloadplot9 <- downloadHandler(
      filename = function() {
        "residual_plot(delete).pdf"  # 指定要保存的文件名
      },
      content = function(file) {
        p <- plotd9()
        ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
      }
    )
    output$plot10 <- renderPlot({
      time <- NULL
      lag <- NULL
      keep <- keep.table()
      x <- keep$date|> unlist()|>as.numeric()
      y <- keep$divergence|>unlist()|>as.numeric()
      fra <- data.frame(time=x,div=y)
      fra <- fra[order(fra$time),]
      x <- fra$time
      y <- fra$div
      lm4 <- lm(y~x)
      residuals_4 <- rstudent(lm4)
      bacf <- stats::acf(residuals_4)
      bacf$acf = bacf$acf[-1, , , drop = FALSE]
      bacf$lag = bacf$lag[-1, , , drop = FALSE]
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
      plotd10(p4)
      print(p4)
    })
    
    plotd10<- reactiveVal()
    
    output$downloadplot10 <- downloadHandler(
      filename = function() {
        "ACF_plot(delete).pdf"  # 指定要保存的文件名
      },
      content = function(file) {
        p <- plotd10()
        ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
      }
    )
    
    output$plot11 <- renderPlot({
      time <- NULL
      keep <- keep.table()
      x <- keep$date|> unlist()|>as.numeric()
      y <- keep$divergence|>unlist()|>as.numeric()
      fra <- data.frame(time=x, res=y)
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
      plotd11(p)
      print(p)
    })
    plotd11<- reactiveVal()
    
    output$downloadplot11 <- downloadHandler(
      filename = function() {
        "predict_plot(delete).pdf"  # 指定要保存的文件名
      },
      content = function(file) {
        p <- plotd11()
        ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
      }
    )
    
  })
  observeEvent(input$reset,{
    output$Summary <- renderTable({  
      date <- date()|> unlist()|>as.numeric()
      divergence <- divergence()|>unlist()|>as.numeric()
      df <- cbind(divergence, date)|>as.data.frame()
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
                                         "R squared", "RSE"), value=numeric(6))
      summary[1, 2] <- range
      summary[4, 2] <- as.numeric(cor(date, divergence))
      lm.rtt <- lm(df)
      summary[2, 2] <- as.numeric(lm.rtt$coefficients[2])
      summary[3, 2] <- as.numeric(
        abs(lm.rtt$coefficients[1])) / as.numeric(lm.rtt$coefficients[2])
      summary[5, 2] <- summary(lm.rtt)$r.squared
      summary[6, 2] <- summary(lm.rtt)[["sigma"]]
      # summary[7, 2] <- shapiro.test(rstudent(lm(df)))[2]
      # summary[8, 2] <- DescTools::RunsTest(rstudent(lm(df)))$p.value
      table_dt(summary)
      print(summary)
    },
    digits = 5, width = 8)
    
    table_dt <- reactiveVal()
    output$download_dt2 <- downloadHandler(
      filename = function(){
        "summary(reset).csv"
      },
      content = function(file){
        df <- table_dt()
        write.csv(df,file,row.names = FALSE)
      }
    )
    
    output$plot1 <- renderPlot({
      tree <- sub_tree()
      if (is.null(tree)) {
        return()
      }
      df <- data()
      up.table <- up.table()
      down.table <- down.table()
      d <- rbind(up.table,down.table)
      all <- merge(d,df,by='label',all = T)
      p <- ggtree(tree,color=input$color3, size=input$size)+mySetTheme2()+theme(legend.position="none")  
      if(input$geom_nodelab){p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
      if (input$tip) {
      
        p <- p%<+%all+geom_tiplab(aes(color=category),size=input$tipsize)+
          scale_color_manual(values = c("up" = "red", "down" = "blue"))
        print(p)
      }else if(input$tip_point){
        p <- p%<+%all+ geom_tippoint(aes(color=category),size=input$tipsize)+
          geom_tippoint(aes(color=category),size=input$tipsize)+
          scale_color_manual(values = c("up" = "red", "down" = "blue"))
        print(p)
      } else{
        print(p)}
      plotd1(p)
      print(p)
    },height = height)
    plotd1 <- reactiveVal()
    output$downloadplot1 <- downloadHandler(
      filename = function() {
        "tree_plot(reset).pdf"  # 指定要保存的文件名
      },
      content = function(file) {
        p <- plotd1()
        ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
      }
    )
    output$plot8 <- renderPlot({
      time <- NULL
      x <- date()|> unlist()|>as.numeric()
      y <- divergence()|>unlist()|>as.numeric()
      fra <- data.frame(time=x, res=y)
      lm3 <- lm(y~x)
      residuals_3 <- rstudent(lm3)
      fra$res <- residuals_3
      p5 <-ggplot(fra,aes(sample=res))+
        stat_qq()+labs(title="Normal Q-Q plot")+mySetTheme()#+mySetTheme()
      plotd8(p5)
      return(p5)
      
    })
    plotd8 <- reactiveVal()
    
    output$downloadplot8 <- downloadHandler(
      filename = function() {
        "QQ_plot(reset).pdf"  # 指定要保存的文件名
      },
      content = function(file) {
        p <- plotd8()
        ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
      }
    )
    
    output$plot9 <- renderPlot({
      time <- NULL
      x <- date()|> unlist()|>as.numeric()
      y <- divergence()|>unlist()|>as.numeric()
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
      plotd9(p3)
      return(p3)
    })
    plotd9 <- reactiveVal()
    
    output$downloadplot9 <- downloadHandler(
      filename = function() {
        "residual_plot(reset).pdf"  # 指定要保存的文件名
      },
      content = function(file) {
        p <- plotd9()
        ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
      }
    )
    output$plot10 <- renderPlot({
      time <- NULL
      lag <- NULL
      x <- date()|> unlist()|>as.numeric()
      y <- divergence()|>unlist()|>as.numeric()
      fra <- data.frame(time=x,div=y)
      fra <- fra[order(fra$time),]
      x <- fra$time
      y <- fra$div
      lm4 <- lm(y~x)
      residuals_4 <- rstudent(lm4)
      bacf <- stats::acf(residuals_4)
      bacf$acf = bacf$acf[-1, , , drop = FALSE]
      bacf$lag = bacf$lag[-1, , , drop = FALSE]
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
      plotd10(p4)
      print(p4)
    })
    plotd10<- reactiveVal()
    
    output$downloadplot10 <- downloadHandler(
      filename = function() {
        "ACF_plot(reset).pdf"  # 指定要保存的文件名
      },
      content = function(file) {
        p <- plotd10()
        ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
      }
    )
    
    
    output$plot11 <- renderPlot({
      time <- NULL
      x <- date()|> unlist()|>as.numeric()
      y <- divergence()|>unlist()|>as.numeric()
      fra <- data.frame(time=x, res=y)
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
      plotd11(p)
      print(p)
    })
    plotd11<- reactiveVal()
    
    output$downloadplot11 <- downloadHandler(
      filename = function() {
        "predict_plot(reset).pdf"  # 指定要保存的文件名
      },
      content = function(file) {
        p <- plotd11()
        ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
      }
    )
    
    output$plot2 <- renderPlot({
      df <- data()
      up.table <- up.table()
      down.table <- down.table()
      p <- ggplot(df, aes(x = date, y = divergence)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, formula = y ~ x,colour=input$color2) +
        geom_point(data = down.table, aes(x = date, y = divergence), color = 'blue') +
        geom_point(data = up.table,aes(x = date, y = divergence), color ="red")+
        # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
        mySetTheme() +
        stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
      plotd2(p)
      print(p)
    })
    plotd2 <- reactiveVal()
    output$downloadplot2 <- downloadHandler(
      filename = function() {
        "regression_plot(reset).pdf"  # 指定要保存的文件名
      },
      content = function(file) {
        p <- plotd2()
        ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
      }
    )
    output$plot3 <- renderPlot({
      df1 <- merged_data()
      df <- df1[,c("label",input$x_var,input$y_var)]
      need.up.table <- need.up.table()
      need.down.table <- need.down.table()
      p <- ggplot(df, aes_string(x = input$x_var, y =input$y_var)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, formula = y ~ x,colour=input$color2) +
        geom_point(data = need.down.table, aes_string(x = input$x_var, y =input$y_var), color = 'blue') +
        geom_point(data = need.up.table,aes_string(x = input$x_var, y =input$y_var), color ="red")+
        # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
        mySetTheme() +
        stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
      plotd3(p)
      print(p)
    })
    
    print("here do reset")
    
    plotd3 <- reactiveVal()
    
    output$downloadplot3 <- downloadHandler(
      filename = function() {
        "regression(external_data_(reset))_plot.pdf"  # 指定要保存的文件名
      },
      content = function(file) {
        p <- plotd3()
        ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
      }
    )
    
  })

  
  output$dataframe <- renderDataTable({
    tree <- sub_tree()
    if (is.null(tree)) {
      return(NULL)
    }
    tree <- tree%>%as.phylo()
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
      sub.tree[[f]] <- extract.clade(tree,node = i)
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
          tip.number=Ntip(sub.tree[[f]]),
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
    dd <- df[order(df$total_abnormal, decreasing = T), ]
    table2(dd)
    dd
  })
 table2 <- reactiveVal()
  
  output$ download2.table <- downloadHandler(
    filename = function(){
      "Subtree_regression_intergration.csv"
    },
    content = function(file){
      df <- table2()
      write.csv(df,file,row.names = FALSE)
    }
  )
  
  data2 <- reactive({
    req(input$outdata)
    read.csv(input$outdata$datapath)
  })
  
  existing_data <-reactive({
    date <- date() %>% as.numeric()
    divergence <- divergence()
    label <- label()
    df <- data.frame(label=label,divergence=divergence,date=date %>% as.numeric())
    
    return(df)
  })
  
  merged_data <- reactive({
    req(data2())
    # 将上传的表格与现有的表格合并
    if (!is.null(data2())) {
      existing_data <- existing_data()
      merged <- merge(existing_data, data2(),all.x=TRUE) %>%unique() %>% na.omit()
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
    p <- ggplot(df, aes_string(x = input$x_var, y =input$y_var)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, formula = y ~ x,colour=input$color2) +
      geom_point(data = need.down.table, aes_string(x = input$x_var, y =input$y_var), color = 'blue') +
      geom_point(data = need.up.table,aes_string(x = input$x_var, y =input$y_var), color ="red")+
      # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
      mySetTheme() +
      stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
    plotd3(p)
    print(p)
  })
  plotd3 <- reactiveVal()
  
  output$downloadplot3 <- downloadHandler(
    filename = function() {
      "regression(external_data)_plot.pdf"  # 指定要保存的文件名
    },
    content = function(file) {
      p <- plotd3()
      ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
    }
  )
  output$outliers2 <- renderDataTable({
    need.up.table <- need.up.table()
    need.down.table <- need.down.table()
    rbind(need.up.table,need.down.table)
  })
  
  output$out_dataframe <- renderDataTable({
    tree <- sub_tree()
    if (is.null(tree)) {
      return(NULL)
    }
    if (is.null(data2())) {
      return(NULL)
    }
    tree <- tree%>%as.phylo()
    data2 <- data2()
    a = length(tree$tip.label) + 1
    b = length(tree$tip.label) + tree$Nnode
    sub.tree <- list()
    divergence <- list()
    label <- list()
    df_td1 = list()
    df_td2=list()
    model.results <- list()
    modele <- list()
    for (i in a:b) {
      f=i-a+1
      sub.tree[[f]] <- extract.clade(tree,node = i)
      divergence[[f]] <- getdivergence(tree = sub.tree[[f]])
      label[[f]] <-sub.tree[[f]]$tip.label
      df_td1[[f]] <- data.frame(label=label[[f]],divergence=divergence[[f]])
      
      df_td2[[f]] <-if (unique(is.na(divergence[[f]]))) {
        NA
      }else{
        merge(data2,df_td1[[f]]) %>% unique()
      }
      pd <- unique(df_td2[[f]][1]) %>% unlist() %>% as.vector()
      model.results[[f]] <- if (is.na(pd[1])) {
        NA
      }else{
        m <- lm(as.formula(paste(input$y_var, "~", input$x_var)),df_td2[[f]])
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
          summary(lm(as.formula(paste(input$y_var, "~", input$x_var)),df_td2[[f]]))
        data.frame(
          node = i,
          tip.number=Ntip(sub.tree[[f]]),
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
    dd <- df[order(df$total_abnormal, decreasing = T), ]
    table3(dd)
    dd
  })
  table3 <- reactiveVal()
  output$download3.table <- downloadHandler(
    filename = function(){
      "out_data__regression_intergration.csv"
    },
    content = function(file){
      df <- table3()
      write.csv(df,file,row.names = FALSE)
    }
  )
  
  output$plot8 <- renderPlot({
    time <- NULL
    x <- date()|> unlist()|>as.numeric()
    y <- divergence()|>unlist()|>as.numeric()
    fra <- data.frame(time=x, res=y)
    lm3 <- lm(y~x)
    residuals_3 <- rstudent(lm3)
    fra$res <- residuals_3
    p5 <-ggplot(fra,aes(sample=res))+
      stat_qq()+labs(title="Normal Q-Q plot")+mySetTheme()#+mySetTheme()
    plotd8(p5)
    return(p5)
    
  })
  plotd8 <- reactiveVal()
  
  output$downloadplot8 <- downloadHandler(
    filename = function() {
      "QQ_plot.pdf"  # 指定要保存的文件名
    },
    content = function(file) {
      p <- plotd8()
      ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
    }
  )
  output$plot9 <- renderPlot({
    time <- NULL
    x <- date()|> unlist()|>as.numeric()
    y <- divergence()|>unlist()|>as.numeric()
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
    plotd9(p3)
    return(p3)
  })
  
  plotd9 <- reactiveVal()
  
  output$downloadplot9 <- downloadHandler(
    filename = function() {
      "residual_plot.pdf"  # 指定要保存的文件名
    },
    content = function(file) {
      p <- plotd9()
      ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
    }
  )
  
  
    output$plot10 <- renderPlot({
      time <- NULL
      lag <- NULL
      x <- date()|> unlist()|>as.numeric()
      y <- divergence()|>unlist()|>as.numeric()
      fra <- data.frame(time=x,div=y)
      fra <- fra[order(fra$time),]
      x <- fra$time
      y <- fra$div
      lm4 <- lm(y~x)
      residuals_4 <- rstudent(lm4)
      bacf <- stats::acf(residuals_4)
      bacf$acf = bacf$acf[-1, , , drop = FALSE]
      bacf$lag = bacf$lag[-1, , , drop = FALSE]
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
      plotd10(p4)
      print(p4)
    })
    plotd10<- reactiveVal()
    
    output$downloadplot10 <- downloadHandler(
      filename = function() {
        "ACF_plot.pdf"  # 指定要保存的文件名
      },
      content = function(file) {
        p <- plotd10()
        ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
      }
    )
    
    
    output$plot11 <- renderPlot({
      time <- NULL
      x <- date()|> unlist()|>as.numeric()
      y <- divergence()|>unlist()|>as.numeric()
      fra <- data.frame(time=x, res=y)
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
      plotd11(p)
      print(p)
    })
    
    plotd11<- reactiveVal()
    
    output$downloadplot11 <- downloadHandler(
      filename = function() {
        "predict_plot.pdf"  # 指定要保存的文件名
      },
      content = function(file) {
        p <- plotd11()
        ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
      }
    )
    
    
    output$Summary <- renderTable({  
      date <- date()|> unlist()|>as.numeric()
      divergence <- divergence()|>unlist()|>as.numeric()
      df <- cbind(divergence, date)|>as.data.frame()
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
                                         "R squared", "RSE"), value=numeric(6))
      summary[1, 2] <- range
      summary[4, 2] <- as.numeric(cor(date, divergence))
      lm.rtt <- lm(df)
      summary[2, 2] <- as.numeric(lm.rtt$coefficients[2])
      summary[3, 2] <- as.numeric(
        abs(lm.rtt$coefficients[1])) / as.numeric(lm.rtt$coefficients[2])
      summary[5, 2] <- summary(lm.rtt)$r.squared
      summary[6, 2] <- summary(lm.rtt)[["sigma"]]
      # summary[7, 2] <- shapiro.test(rstudent(lm(df)))[2]
      # summary[8, 2] <- DescTools::RunsTest(rstudent(lm(df)))$p.value
      table_dt(summary)
      print(summary)
    },
    digits = 5, width = 8)
    
    table_dt <- reactiveVal()
    output$download_dt2 <- downloadHandler(
      filename = function(){
        "summary.csv"
      },
      content = function(file){
        df <- table_dt()
        write.csv(df,file,row.names = FALSE)
      }
    )
    
}

