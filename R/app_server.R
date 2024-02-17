#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import ggtree
#' @import ggplot2

#
#' @importFrom shinyjs toggle
#' @importFrom ggtree rotate
#' @importFrom treeio read.beast
#' @importFrom treeio read.codeml
#' @importFrom treeio merge_tree
#' @importFrom treeio rescale_tree
#' @importFrom ape extract.clade
#' @importFrom ape pic
#' @importFrom ape as.phylo
#' @importFrom ape read.nexus
#' @importFrom ape drop.tip
#' @importFrom ape Ntip
#' @importFrom ape nodepath
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
#' @importFrom stats cor.test
#' @importFrom DescTools RunsTest
#' @importFrom stats na.omit
#' @importFrom stats as.formula
#' @importFrom ggpmisc stat_poly_eq
#' @importFrom utils read.csv
#' @importFrom utils write.csv
#' @importFrom yulab.utils str_extract
#' @importFrom nlme gls
#' @noRd
app_server <- function(input, output, session)  {
  update_group <- function(tree, data_all, nodes) {
    plot_data <- NULL
    for (node in nodes) {
      subtree <- extract.clade(tree, node = node)
      data_node <- data_all[data_all$label %in% subtree$tip.label, ]
      data_node$group <- paste("subtree", node, sep = "")
      plot_data <- rbind(plot_data, data_node)
    }
    return(plot_data)
  }

  observeEvent(input$update_button, {
    tree <- sub_tree()
    dt <- data.frame(label=label(),date=date(),divergence=divergence())
    nodes <- as.numeric(unlist(strsplit(input$multi_node, ",")))
    plot_data <- update_group(tree=tree, data_all=dt, nodes=nodes)
    output$multi_regression <- renderPlot(
  ggplot(plot_data, aes(x = date, y = divergence,color=group)) +
  geom_point()+
  geom_smooth(method = lm,se=F) +
  mySetTheme()+stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
  parse = TRUE)
    )
  })

observeEvent(input$update_button2,{
  tree2 <- sub_tree()
  dt2 <-merged_data()#this data with the updated divergence 
  nodes2 <-as.numeric(unlist(strsplit(input$multi_node2, ",")))
  plot_data2 <- update_group(tree=tree2, data_all=dt2, nodes=nodes2)  

  output$multi_regression2 <- renderPlot(
  ggplot(plot_data2, aes_string(x = input$x_var, y =input$y_var,color="group")) +
  geom_point()+
  geom_smooth(method = lm,se=F) +
  mySetTheme()+stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
  parse = TRUE)
    )
})

   observeEvent(input$regression_btn, {
  df1 <- merged_data()
  row.names(df1) <- df1[,"label"]
  tree <- sub_tree()
  df <- df1[, c("label", input$x_var, input$y_var)]

  x <- df1[, input$x_var]
  y <- df1[, input$y_var]

  names(x) <- names(y) <- df1[, "label"]
  pic.x <- pic(x, tree)
  pic.y <- pic(y, tree)
  pglsModel <- nlme::gls(as.formula(paste(input$y_var, "~", input$x_var)), correlation = corBrownian(phy = tree),
                 data = df1, method = "ML")
if (input$cortype=="PIC") {
    
  output$correlation <- renderPrint({
    print(cor.test(pic.x, pic.y))
  })} else if(input$cortype=="PGLS") {

     output$correlation <- renderPrint({
    
    print(summary(pglsModel))
  })
  }

})

<<<<<<< HEAD
 down_color = "#6a73cf"
    up_color ="#f26115"
=======

down_color = "#6a73cf"
up_color ="#f26115"

##' @importFrom ggprism theme_prism
>>>>>>> f57e04a5794460c7278e7b54b42af22e340b5a37
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
    p <- ggtree(tree,color=input$color3, size=input$size,layout = input$layout)+theme(legend.position="none")+mySetTheme2()
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
 observeEvent(input$temp_node,
  updateTextInput(session,"node",value=input$temp_node)
 )
 observeEvent(input$phylo_node,
  updateTextInput(session,"node",value=input$phylo_node)
 )
  sub_tree <- eventReactive(
    input$node, {
    # browser()
      if(input$node != "") {
        tree <- tree()
        check_node <- as.numeric(input$node)<length(as.phylo(tree)$tip.label)
        req(!check_node)
        extract.clade(tree(), node = as.numeric(input$node))
      } else {
        tree()
      }
    }
  )


  sub_divergence <- eventReactive(
    input$node,{
      tree <- tree()
      get_new_divergence(tree=tree,node=as.numeric(input$node))
    })
  # 1.读入树文件
  tree <- eventReactive(input$fileinput, {
    req(!is.null(input$treefile))
    if (input$filetype=="Newick") {
      tree <- read.tree(input$treefile$datapath) %>% as.phylo()
    } else if (input$filetype=="Beast") {
      tree <- read.beast(input$treefile$datapath) %>% as.phylo()
    } else if (input$filetype=="NEXUS") {
      tree <- read.nexus(input$treefile$datapath)
    } else if (input$filetype=="Phylip") {
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
  # 全部在外面取出来不就好了
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
divergence_all <- reactive({
     tree <- tree()
    if (!is.null(tree)){
      divergence <- getdivergence(tree = tree)
      return(divergence)
    }else{
      return(NULL)
    }
  })
label_all<-reactive({
     tree <- tree()
    if (!is.null(tree)){
      tree <- tree %>% as.phylo()
      label <-tree$tip.label
      return(label)
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

 observeEvent(input$reset,{
  tree <- tree()
  reset_root_node <- length(tree$tip.label)+1
  updateTextInput(session,"node",value = reset_root_node)
})
 observeEvent(input$reset2,{
  tree <- tree()
  reset_root_node <- length(tree$tip.label)+1
  updateTextInput(session,"node",value = reset_root_node)
})

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

observeEvent(input$reset2,{
    output$Summary2 <- renderTable({  
      df1 <- merged_data()
      df <- df1[,c("label",input$x_var,input$y_var)]
       lm.rtt <- lm(as.formula(paste(input$y_var, "~", input$x_var)), merged_data())
       summary <- data.frame(parameters=c( "Slope(rate)", 
                                         "X-Intercept", "Correlation", 
                                         "R squared", "RSE"), value=numeric(5))
       summary[1, 2] <- as.numeric(lm.rtt$coefficients[2])
        summary[2, 2] <- as.numeric(
       abs(lm.rtt$coefficients[1])) / as.numeric(lm.rtt$coefficients[2])
       summary[3, 2] <- as.numeric(cor(df[,input$x_var], df[,input$y_var]))
       summary[4, 2] <- summary(lm.rtt)$r.squared
       summary[5, 2] <- summary(lm.rtt)[["sigma"]]
       print(summary)
    })
  output$plot1 <- renderPlot({
    # browser()
    req(sub_tree())
  if (input$choose_analysis=="Temporal_signal") {
    tree <- sub_tree()
    tree_download(tree)
    if (is.null(tree)) {
      return(NULL)
    }
    df <- data()
    up.table <- up.table()
    down.table <- down.table()
    d <- rbind(up.table,down.table)
    all <- merge(d,df,by='label',all = T)
    p <- ggtree(tree,color=input$color3, size=input$size,layout = input$layout)+theme(legend.position="none")+mySetTheme2()
    if(input$geom_nodelab){p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
    if (input$tip) {
      
      p <- p+ geom_tiplab(size=input$tipsize)
      p <- p%<+%all+geom_tiplab(aes(color=category))+
        scale_color_manual(values = c("up" = up_color, "down" = down_color))
    }else if(input$tip_point){
      if(input$geom_nodelab){p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
      p <- p%<+%all+ geom_tippoint(aes(color=category),size=input$tipsize)+
        geom_tippoint(aes(color=category),size=input$tipsize)+
        scale_color_manual(values = c("up" = up_color, "down" = down_color))
    }
    plotd1(p)
    p
    } else if (input$choose_analysis=="Phylogenetic_signal") {
      tree <- sub_tree()
      tree_download(tree)
    if (is.null(tree)) {
      return(NULL)
    }

      df1 <- merged_data()
      df <- df1[,c("label",input$x_var,input$y_var)]
      up.table <- need.up.table()
      down.table <- need.down.table() 
    
    d <- rbind(up.table,down.table)
    all <- merge(d,df,by='label',all = T)
    p <- ggtree(tree,color=input$color3, size=input$size,layout = input$layout)+theme(legend.position="none")+mySetTheme2()
    if(input$geom_nodelab){p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
    if (input$tip) {
      
      p <- p+ geom_tiplab(size=input$tipsize)
      p <- p%<+%all+geom_tiplab(aes(color=category))+
        scale_color_manual(values = c("up" = up_color, "down" = down_color))
    }else if(input$tip_point){
      if(input$geom_nodelab){p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
      p <- p%<+%all+ geom_tippoint(aes(color=category),size=input$tipsize)+
        geom_tippoint(aes(color=category),size=input$tipsize)+
        scale_color_manual(values = c("up" = up_color, "down" = down_color))
    }
    plotd1(p)
    p

    }else if(input$choose_analysis=="only_tree"){
       tree <- sub_tree()
       tree_download(tree)
       tree_download(tree)
    if (is.null(tree)) {
      return(NULL)
    }

    p <- ggtree(tree,color=input$color3, size=input$size,layout = input$layout)+theme(legend.position="none")+mySetTheme2() 
    if(input$geom_nodelab){p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
    if (input$tip) {  
      p <- p+ geom_tiplab(size=input$tipsize)
    }else if(input$tip_point){
      if(input$geom_nodelab){
        p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
      p <- p+geom_tippoint(size=input$tipsize)
    }
    plotd1(p)
    p
    }

    
  },height = height
  )
  plotd1 <- reactiveVal()
  output$downloadplot1 <- downloadHandler(
    filename = function() {
      "tree_plot_reset.pdf"  # 指定要保存的文件名
    },
    content = function(file) {
      p <- plotd1()
      ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
    }
  )
 
tree_download <- reactiveVal()
output$tree1 <- downloadHandler(
  filename = function(){
    "tree.nwk"
  },
  content = function(file){
    nwk <- tree_download()
    write.tree(phy=nwk,file=file)
  }
)

  output$plot3 <- renderPlot({
      # browser()
    df1_1 <- merged_data2()
    df1_2 <- df1_1[,c("label",input$x_var,input$y_var)]
  sub_divergence <- sub_divergence()
  # x <- as.character(input$x_var)
  # y  <- as.character()
  p_all2 <- ggplot(df1_1, mapping = aes_string(x=input$x_var ,y=input$y_var)) +
      geom_point(colour="gray") +
      geom_smooth(method = "lm", se = FALSE, formula = y ~ x,colour="gray")
  #  browser()
  if(input$plot_all2){
    df1 <- merged_data()
    df <- df1[,c("label",input$x_var,input$y_var)]
    # df[,"divergence"] <- df[,"divergence"]+sub_divergence()
    need.up.table <- need.up.table()
   
    need.down.table <- need.down.table()

    p <- p_all2+
      geom_point(data=df, aes_string(x = input$x_var, y =input$y_var)) +
      geom_smooth(data=df, aes_string(x = input$x_var, y =input$y_var),method = "lm", se = FALSE, formula = y ~ x,colour=input$color2) +
      geom_point(data = need.down.table, aes_string(x = input$x_var, y =input$y_var), color = down_color) +
      geom_point(data = need.up.table,aes_string(x = input$x_var, y =input$y_var), color =up_color)+
      # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
      mySetTheme() +
      stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
    plotd3(p)
    print(p)
  }else {
      df1 <- merged_data()
    df <- df1[,c("label",input$x_var,input$y_var)]
    need.up.table <- need.up.table()
    need.down.table <- need.down.table()
    p <- ggplot(df, aes_string(x = input$x_var, y =input$y_var)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, formula = y ~ x,colour=input$color2) +
      geom_point(data = need.down.table, aes_string(x = input$x_var, y =input$y_var), color = down_color) +
      geom_point(data = need.up.table,aes_string(x = input$x_var, y =input$y_var), color =up_color)+
      # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
      mySetTheme() +
      stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
    plotd3(p)
    print(p)
  }
  })
  
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
    # browser()
    # req(sub_tree())
  if (input$choose_analysis=="Temporal_signal") {
    tree <- sub_tree()
    tree_download(tree)
    if (is.null(tree)) {
      return(NULL)
    }
    df <- data()
    up.table <- up.table()
    down.table <- down.table()
    d <- rbind(up.table,down.table)
    all <- merge(d,df,by='label',all = T)
    p <- ggtree(tree,color=input$color3, size=input$size,layout = input$layout)+theme(legend.position="none")+mySetTheme2() 
    if(input$geom_nodelab){p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
    if (input$tip) {
      
      p <- p+ geom_tiplab(size=input$tipsize)
      p <- p%<+%all+geom_tiplab(aes(color=category))+
        scale_color_manual(values = c("up" = up_color, "down" = down_color))
    }else if(input$tip_point){
      if(input$geom_nodelab){p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
      p <- p%<+%all+ geom_tippoint(aes(color=category),size=input$tipsize)+
        geom_tippoint(aes(color=category),size=input$tipsize)+
        scale_color_manual(values = c("up" = up_color, "down" = down_color))
    }
    plotd1(p)
    p
    } else if (input$choose_analysis=="Phylogenetic_signal") {
      tree <- sub_tree()
      tree_download(tree)
    if (is.null(tree)) {
      return(NULL)
    }

      df1 <- merged_data()
      df <- df1[,c("label",input$x_var,input$y_var)]
      up.table <- need.up.table()
      down.table <- need.down.table() 
    
    d <- rbind(up.table,down.table)
    all <- merge(d,df,by='label',all = T)
    p <- ggtree(tree,color=input$color3, size=input$size,layout = input$layout)+theme(legend.position="none") +mySetTheme2()
    if(input$geom_nodelab){p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
    if (input$tip) {
      
      p <- p+ geom_tiplab(size=input$tipsize)
      p <- p%<+%all+geom_tiplab(aes(color=category))+
        scale_color_manual(values = c("up" = up_color, "down" = down_color))
    }else if(input$tip_point){
      if(input$geom_nodelab){p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
      p <- p%<+%all+ geom_tippoint(aes(color=category),size=input$tipsize)+
        geom_tippoint(aes(color=category),size=input$tipsize)+
        scale_color_manual(values = c("up" = up_color, "down" = down_color))
    }
    plotd1(p)
    p

    }else if(input$choose_analysis=="only_tree"){
       tree <- sub_tree()
       tree_download(tree)
    if (is.null(tree)) {
      return(NULL)
    }

    p <- ggtree(tree,color=input$color3, size=input$size,layout = input$layout)+theme(legend.position="none") +mySetTheme2()
    if(input$geom_nodelab){p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
    if (input$tip) {  
      p <- p+ geom_tiplab(size=input$tipsize)
    }else if(input$tip_point){
      if(input$geom_nodelab){
        p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
      p <- p+geom_tippoint(size=input$tipsize)
    }
    plotd1(p)
    p
    }

    
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


tree_download <- reactiveVal()
output$tree1 <- downloadHandler(
  filename = function(){
    "tree.nwk"
  },
  content = function(file){
    nwk <- tree_download()
    write.tree(phy=nwk,file=file)
  }
)

  
  #2.取出日期
  output$datetable <- renderDataTable({
    # browser()
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
    data2_1 <- reactive({
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
  #3.取出divergence，回归分析
  output$plot2 <- renderPlot({
    # browser()
  df1 <- data2_1()
  sub_divergence <- sub_divergence()
  p_all <- ggplot(df1, aes(x = date, y = divergence)) +
      geom_point(colour="gray") +
      geom_smooth(method = "lm", se = FALSE, formula = y ~ x,colour="gray")
  if(input$plot_all){
    df <- data()
    up.table <- up.table()
    down.table <- down.table()
    p <-p_all+
      geom_point(data=df, aes(x = date, y = divergence+sub_divergence)) +
      geom_smooth(data=df, aes(x = date, y = divergence+sub_divergence),method = "lm", se = FALSE, formula = y ~ x,colour=input$color2) +
      stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)+
      geom_point(data = down.table, aes(x = date, y = divergence+sub_divergence), color = down_color) +
      geom_point(data = up.table,aes(x = date, y = divergence+sub_divergence), color =up_color)+
      # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
      mySetTheme()
      plotd2(p)
      p
  }else {
    df <- data()
    up.table <- up.table()
    down.table <- down.table()
     p <-ggplot(data=df, aes(x = date, y = divergence+sub_divergence))+
      geom_point() +
      geom_smooth(data=df, aes(x = date, y = divergence+sub_divergence),method = "lm", se = FALSE, formula = y ~ x,colour=input$color2) +
      stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)+
      geom_point(data = down.table, aes(x = date, y = divergence+sub_divergence), color = down_color) +
      geom_point(data = up.table,aes(x = date, y = divergence+sub_divergence), color =up_color)+
      # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
      mySetTheme() 
      plotd2(p)
      p
  }

  
  #  tree <- sub_tree()
  #   if (is.null(tree)) {
  #     return()
  #   }
  #   df <- data()
  #   up.table <- up.table()
  #   down.table <- down.table()
  #   p <- ggplot(df, aes(x = date, y = divergence)) +
  #     geom_point() +
  #     geom_smooth(method = "lm", se = FALSE, formula = y ~ x,colour=input$color2) +
  #     geom_point(data = down.table, aes(x = date, y = divergence), color = down_color) +
  #     geom_point(data = up.table,aes(x = date, y = divergence), color =up_color)+
  #     # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
  #     mySetTheme() +
  #     stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
  #   plotd2(p)
  #   print(p)
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
      sub_divergence <- sub_divergence()
      df1 <- data2_1()
      p_all <- ggplot(df1, aes(x = date, y = divergence)) +
      geom_point(colour="gray") +
      geom_smooth(method = "lm", se = FALSE, formula = y ~ x,colour="gray")
      exclude.table <- exclude.table()
      keep.table <- keep.table()
      if(input$plot_all){
           df <- data()
           up.table <- up.table()
           down.table <- down.table()
           p <-p_all+
             geom_point(data=keep.table, aes(x = date, y = divergence+sub_divergence)) +
             geom_smooth(data=keep.table, aes(x = date, y = divergence+sub_divergence),method = "lm", se = FALSE, formula = y ~ x,colour=input$color2) +
             stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)+
              geom_point(data = exclude.table, aes(x = date, y = divergence+sub_divergence), color = 'gray')+
             # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
             mySetTheme()
             plotd2(p)
             p
         }else {
       p <- ggplot(keep.table, aes(x = date, y = divergence+sub_divergence)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, formula = y ~ x,colour=input$color2) +
        geom_point(data = exclude.table, aes(x = date, y = divergence+sub_divergence), color = 'gray') +
        # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
        mySetTheme() +
        stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
      plotd2(p)
      print(p)
  }
# data_all <- reactive(
#   req(data2())
#   cbind(label_all(),divergence_all)|>as.data.frame()
# )
      # p <- ggplot(keep.table, aes(x = date, y = divergence)) +
      #   geom_point() +
      #   geom_smooth(method = "lm", se = FALSE, formula = y ~ x,colour=input$color2) +
      #   geom_point(data = exclude.table, aes(x = date, y = divergence), color = 'gray') +
      #   # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
      #   mySetTheme() +
      #   stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
      # plotd2(p)
      # print(p)
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
    
    

  # output$Summary2 <- renderTable({  
  #     date <- date()|> unlist()|>as.numeric()
  #     divergence <- divergence()|>unlist()|>as.numeric()
  #     df <- cbind(divergence, date)|>as.data.frame()
  #     if (input$format == "yy" | input$format == "yyyy") {
  #       range <- max(date) - min(date)
  #     }
  #     else{
  #       range <- max(date) - min(date)
  #       range <- range * 365
  #     }
  #     ##make a summary and output
  #     summary <- data.frame(Dated.tips=c("Date range", "Slope(rate)", 
  #                                        "X-Intercept", "Correlation", 
  #                                        "R squared", "RSE"), value=numeric(6))
  #     summary[1, 2] <- range
  #     summary[4, 2] <- as.numeric(cor(date, divergence))
  #     lm.rtt <- lm(df)
  #     summary[2, 2] <- as.numeric(lm.rtt$coefficients[2])
  #     summary[3, 2] <- as.numeric(
  #       abs(lm.rtt$coefficients[1])) / as.numeric(lm.rtt$coefficients[2])
  #     summary[5, 2] <- summary(lm.rtt)$r.squared
  #     summary[6, 2] <- summary(lm.rtt)[["sigma"]]
  #     # summary[7, 2] <- shapiro.test(rstudent(lm(df)))[2]
  #     # summary[8, 2] <- DescTools::RunsTest(rstudent(lm(df)))$p.value
  #     table_dt(summary)
  #     print(summary)
  #   },
  #   digits = 5, width = 8)

    
    print("here do delete")

    output$plot1 <- renderPlot({
      tree <- sub_tree()
      up.table <- up.table()
      down.table <- down.table()
      to_drop <- c(down.table$label,up.table$label)
      tip_reduced <- drop.tip(tree, to_drop)
      tree_download(tip_reduced)
      p <- ggtree(tip_reduced,color=input$color3, size=input$size,layout = input$layout)+mySetTheme2() 
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
    
  })

tree_download <- reactiveVal()
output$tree1 <- downloadHandler(
  filename = function(){
    "tree.nwk"
  },
  content = function(file){
    nwk <- tree_download()
    write.tree(phy=nwk,file=file)
  }
)


observeEvent(input$delete2,{
  output$Summary2 <- renderTable({  
      df1 <- need.keep.table()
      df <- df1[,c("label",input$x_var,input$y_var)]
       lm.rtt <- lm(as.formula(paste(input$y_var, "~", input$x_var)), need.keep.table())
       summary <- data.frame(parameters=c( "Slope(rate)", 
                                         "X-Intercept", "Correlation", 
                                         "R squared", "RSE"), value=numeric(5))
       summary[1, 2] <- as.numeric(lm.rtt$coefficients[2])
        summary[2, 2] <- as.numeric(
       abs(lm.rtt$coefficients[1])) / as.numeric(lm.rtt$coefficients[2])
       summary[3, 2] <- as.numeric(cor(df[,input$x_var], df[,input$y_var]))
       summary[4, 2] <- summary(lm.rtt)$r.squared
       summary[5, 2] <- summary(lm.rtt)[["sigma"]]
       print(summary)
       })

output$plot3 <- renderPlot({

  df1_1 <- merged_data2()
    df1_2 <- df1_1[,c("label",input$x_var,input$y_var)]
  sub_divergence <- sub_divergence()
  # x <- as.character(input$x_var)
  # y  <- as.character()
  p_all2 <- ggplot(df1_1, mapping = aes_string(x=input$x_var ,y=input$y_var)) +
      geom_point(colour="gray") +
      geom_smooth(method = "lm", se = FALSE, formula = y ~ x,colour="gray")
  #  browser()
  if(input$plot_all2){
     need.exclude.table <- need.exclude.table()
      keep.table <- need.keep.table()
      p <- p_all2+
        geom_point(data=keep.table, aes_string(x = input$x_var, y = input$y_var)) +
        geom_smooth(data=keep.table, aes_string(x = input$x_var, y = input$y_var),method = "lm", se = FALSE, formula = y ~ x,colour=input$color2) +
        geom_point(data = need.exclude.table, aes_string(x = input$x_var, y = input$y_var), color = 'gray') +
        # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
        mySetTheme() +
        stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
      plotd3(p)
      print(p)
  }else {
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
  }
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
  if (input$choose_analysis=="Temporal_signal") {
    tree <- sub_tree()
     tree_download(tree)
    if (is.null(tree)) {
      return(NULL)
    }
    df <- data()
    up.table <- up.table()
    down.table <- down.table()
    d <- rbind(up.table,down.table)
    all <- merge(d,df,by='label',all = T)
    p <- ggtree(tree,color=input$color3, size=input$size,layout = input$layout)+theme(legend.position="none")+mySetTheme2() 
    if(input$geom_nodelab){p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
    if (input$tip) {
      
      p <- p+ geom_tiplab(size=input$tipsize)
      p <- p%<+%all+geom_tiplab(aes(color=category))+
        scale_color_manual(values = c("up" = up_color, "down" = down_color))
    }else if(input$tip_point){
      if(input$geom_nodelab){p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
      p <- p%<+%all+ geom_tippoint(aes(color=category),size=input$tipsize)+
        geom_tippoint(aes(color=category),size=input$tipsize)+
        scale_color_manual(values = c("up" = up_color, "down" = down_color))
    }
    plotd1(p)
    p
    } else if (input$choose_analysis=="Phylogenetic_signal") {
      tree <- sub_tree()
      tree_download(tree)
    if (is.null(tree)) {
      return(NULL)
    }

      df1 <- merged_data()
      df <- df1[,c("label",input$x_var,input$y_var)]
      up.table <- need.up.table()
      down.table <- need.down.table() 
    
    d <- rbind(up.table,down.table)
    all <- merge(d,df,by='label',all = T)
    p <- ggtree(tree,color=input$color3, size=input$size,layout = input$layout)+theme(legend.position="none") +mySetTheme2()
    if(input$geom_nodelab){p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
    if (input$tip) {
      
      p <- p+ geom_tiplab(size=input$tipsize)
      p <- p%<+%all+geom_tiplab(aes(color=category))+
        scale_color_manual(values = c("up" = up_color, "down" = down_color))
    }else if(input$tip_point){
      if(input$geom_nodelab){p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
      p <- p%<+%all+ geom_tippoint(aes(color=category),size=input$tipsize)+
        geom_tippoint(aes(color=category),size=input$tipsize)+
        scale_color_manual(values = c("up" = up_color, "down" = down_color))
    }
    plotd1(p)
    p

    }else if(input$choose_analysis=="only_tree"){
       tree <- sub_tree()
       tree_download(tree)
    if (is.null(tree)) {
      return(NULL)
    }

    p <- ggtree(tree,color=input$color3, size=input$size,layout = input$layout)+theme(legend.position="none") +mySetTheme2()
    if(input$geom_nodelab){p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
    if (input$tip) {  
      p <- p+ geom_tiplab(size=input$tipsize)
    }else if(input$tip_point){
      if(input$geom_nodelab){
        p <- p+geom_nodelab(aes(label=node),hjust=-.3)}
      p <- p+geom_tippoint(size=input$tipsize)
    }
    plotd1(p)
    p
    }

    
  },height = height
  )
  plotd1 <- reactiveVal()
  output$downloadplot1 <- downloadHandler(
    filename = function() {
      "tree_plot_reset.pdf"  # 指定要保存的文件名
    },
    content = function(file) {
      p <- plotd1()
      ggsave(file, p, device = "pdf")  # 使用ggsave保存图表为pdf文件
    }
  )

 
tree_download <- reactiveVal()
output$tree1 <- downloadHandler(
  filename = function(){
    "tree.nwk"
  },
  content = function(file){
    nwk <- tree_download()
    write.tree(phy=nwk,file=file)
  }
)

output$plot2 <- renderPlot({
  df1 <- data2_1()
  sub_divergence <- sub_divergence()
  p_all <- ggplot(df1, aes(x = date, y = divergence)) +
      geom_point(colour="gray") +
      geom_smooth(method = "lm", se = FALSE, formula = y ~ x,colour="gray")
  if(input$plot_all){
    df <- data()
    up.table <- up.table()
    down.table <- down.table()
    p <-p_all+
      geom_point(data=df, aes(x = date, y = divergence+sub_divergence)) +
      geom_smooth(data=df, aes(x = date, y = divergence+sub_divergence),method = "lm", se = FALSE, formula = y ~ x,colour=input$color2) +
      stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)+
      geom_point(data = down.table, aes(x = date, y = divergence+sub_divergence), color = down_color) +
      geom_point(data = up.table,aes(x = date, y = divergence+sub_divergence), color =up_color)+
      # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
      mySetTheme()
      plotd2(p)
      p
  }else {
    df <- data()
    up.table <- up.table()
    down.table <- down.table()
     p <-ggplot(data=df, aes(x = date, y = divergence+sub_divergence))+
      geom_point() +
      geom_smooth(data=df, aes(x = date, y = divergence+sub_divergence),method = "lm", se = FALSE, formula = y ~ x,colour=input$color2) +
      stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)+
      geom_point(data = down.table, aes(x = date, y = divergence+sub_divergence), color = down_color) +
      geom_point(data = up.table,aes(x = date, y = divergence+sub_divergence), color =up_color)+
      # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
      mySetTheme() 
      plotd2(p)
      p
  }

  
  #  tree <- sub_tree()
  #   if (is.null(tree)) {
  #     return()
  #   }
  #   df <- data()
  #   up.table <- up.table()
  #   down.table <- down.table()
  #   p <- ggplot(df, aes(x = date, y = divergence)) +
  #     geom_point() +
  #     geom_smooth(method = "lm", se = FALSE, formula = y ~ x,colour=input$color2) +
  #     geom_point(data = down.table, aes(x = date, y = divergence), color = down_color) +
  #     geom_point(data = up.table,aes(x = date, y = divergence), color =up_color)+
  #     # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
  #     mySetTheme() +
  #     stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
  #   plotd2(p)
  #   print(p)
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
  


  # data2_1 <- reactive({
  #    tree <- tree()
  #   if (!is.null(tree)){
  #     tree <- tree %>% as.phylo()
  #     date <-dateType3(tree = tree,pattern = input$regression)
  #     date <- dateNumeric(date = date,format = input$format)
  #     divergence <- getdivergence(tree = tree)
  #     df <- cbind(label=tree$tip.label,date=date,divergence=divergence)|>as.data.frame()
  #     return(df)
  #   }else{
  #     return(NULL)
  #   }
    
  # })
  existing_data <-reactive({
    divergence <- divergence()
    label <- label()
    date <- date()
    df <- data.frame(label=label,divergence=divergence,date=date)
    return(df)
  })
  data2 <- reactive({
    req(input$outdata)
    read.csv(input$outdata$datapath)
  })

  merged_data <- reactive({
    req(data2())
    # 将上传的表格与现有的表格合并
    if (!is.null(data2())) {
      existing_data <- existing_data()
      merged <- merge(existing_data, data2(),all.x=TRUE) %>%unique() %>% na.omit()
      merged[,"divergence"] <- merged[,"divergence"]+sub_divergence()
      merged
    } else {
      
    }
  })
 observeEvent(merged_data(), {
    updateSelectInput(session, "x_var", choices = colnames(merged_data()))
    updateSelectInput(session, "y_var", choices = colnames(merged_data()))
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
  merged_data2 <- reactive({
  req(data2)
  divergence_all <- divergence_all()
  label_all <- label_all()
  data_all <- data.frame(label=label_all,divergence=divergence_all)
  if (!is.null(data2())) {
      data_out <- data2()
      # data_all <- data2_1()
      merged <- merge(data_out, data_all,by="label") %>%unique() %>% na.omit()
      merged
    } else {
      
    }
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

   regression <- reactive({
    req(input$regression_btn)
    lm(as.formula(paste(input$y_var, "~", input$x_var)), merged_data())
  })
  observeEvent(input$regression_btn,{
      
  output$data_table <- renderDataTable({
    req(merged_data())
    merged_data()
  })
 
 observeEvent(input$regression_btn,{
df1 <- merged_data()
    df <- df1[,c("label",input$x_var,input$y_var)]
    need.up.table <- need.up.table()
    need.down.table <- need.down.table()
    p <- ggplot(df, aes_string(x = input$x_var, y =input$y_var)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, formula = y ~ x,colour=input$color2) +
      geom_point(data = need.down.table, aes_string(x = input$x_var, y =input$y_var), color = down_color) +
      geom_point(data = need.up.table,aes_string(x = input$x_var, y =input$y_var), color =up_color)+
      # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
      mySetTheme() +
      stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
    plotd3(p)

    output$Summary2 <- renderTable({  
      df1 <- merged_data()
      df <- df1[,c("label",input$x_var,input$y_var)]
       lm.rtt <- lm(as.formula(paste(input$y_var, "~", input$x_var)), merged_data())
       summary <- data.frame(parameters=c( "Slope(rate)", 
                                         "X-Intercept", "Correlation", 
                                         "R squared", "RSE"), value=numeric(5))
       summary[1, 2] <- as.numeric(lm.rtt$coefficients[2])
        summary[2, 2] <- as.numeric(
       abs(lm.rtt$coefficients[1])) / as.numeric(lm.rtt$coefficients[2])
       summary[3, 2] <- as.numeric(cor(df[,input$x_var], df[,input$y_var]))
       summary[4, 2] <- summary(lm.rtt)$r.squared
       summary[5, 2] <- summary(lm.rtt)[["sigma"]]
       print(summary)
    })

    # output$Summary2 <- renderTable({  
  #     date <- date()|> unlist()|>as.numeric()
  #     divergence <- divergence()|>unlist()|>as.numeric()
  #     df <- cbind(divergence, date)|>as.data.frame()
  #     if (input$format == "yy" | input$format == "yyyy") {
  #       range <- max(date) - min(date)
  #     }
  #     else{
  #       range <- max(date) - min(date)
  #       range <- range * 365
  #     }
  #     ##make a summary and output
  #     summary <- data.frame(Dated.tips=c("Date range", "Slope(rate)", 
  #                                        "X-Intercept", "Correlation", 
  #                                        "R squared", "RSE"), value=numeric(6))
  #     summary[1, 2] <- range
  #     summary[4, 2] <- as.numeric(cor(date, divergence))
  #     lm.rtt <- lm(df)
  #     summary[2, 2] <- as.numeric(lm.rtt$coefficients[2])
  #     summary[3, 2] <- as.numeric(
  #       abs(lm.rtt$coefficients[1])) / as.numeric(lm.rtt$coefficients[2])
  #     summary[5, 2] <- summary(lm.rtt)$r.squared
  #     summary[6, 2] <- summary(lm.rtt)[["sigma"]]
  #     # summary[7, 2] <- shapiro.test(rstudent(lm(df)))[2]
  #     # summary[8, 2] <- DescTools::RunsTest(rstudent(lm(df)))$p.value
  #     table_dt(summary)
  #     print(summary)
  #   },
  #   digits = 5, width = 8)
 })

# data2_1

    output$plot3 <- renderPlot({
      # browser()
    df1_1 <- merged_data2()
    df1_2 <- df1_1[,c("label",input$x_var,input$y_var)]
  sub_divergence <- sub_divergence()
  # x <- as.character(input$x_var)
  # y  <- as.character()
  p_all2 <- ggplot(df1_1, mapping = aes_string(x=input$x_var ,y=input$y_var)) +
      geom_point(colour="gray") +
      geom_smooth(method = "lm", se = FALSE, formula = y ~ x,colour="gray")
  #  browser()
  if(input$plot_all2){
    df1 <- merged_data()
    df <- df1[,c("label",input$x_var,input$y_var)]
    # df[,"divergence"] <- df[,"divergence"]+sub_divergence()
    need.up.table <- need.up.table()
   
    need.down.table <- need.down.table()

    p <- p_all2+
      geom_point(data=df, aes_string(x = input$x_var, y =input$y_var)) +
      geom_smooth(data=df, aes_string(x = input$x_var, y =input$y_var),method = "lm", se = FALSE, formula = y ~ x,colour=input$color2) +
      geom_point(data = need.down.table, aes_string(x = input$x_var, y =input$y_var), color = down_color) +
      geom_point(data = need.up.table,aes_string(x = input$x_var, y =input$y_var), color =up_color)+
      # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
      mySetTheme() +
      stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
    plotd3(p)
    print(p)
  }else {
      df1 <- merged_data()
    df <- df1[,c("label",input$x_var,input$y_var)]
    need.up.table <- need.up.table()
    need.down.table <- need.down.table()
    p <- ggplot(df, aes_string(x = input$x_var, y =input$y_var)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, formula = y ~ x,colour=input$color2) +
      geom_point(data = need.down.table, aes_string(x = input$x_var, y =input$y_var), color = down_color) +
      geom_point(data = need.up.table,aes_string(x = input$x_var, y =input$y_var), color =up_color)+
      # geom_text(data = d, aes(x = date, y = divergence, label = label)) +
      mySetTheme() +
      stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
    plotd3(p)
    print(p)
  }
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
    tree <- tree()
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
    
    output$Summary <- renderTable({  
      # browser()
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

  })
  
  
    
}

