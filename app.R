library(shiny)
library(shinydashboard)
library(shinyjs)

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Temporal Signal"),
                    
                    dashboardSidebar(
                        useShinyjs(),
                        fileInput("file1","choose a tree",accept = "nwk"), 
                        sidebarMenu(menuItem("Sample Dates",tabName = "Dates"),
                                    menuItem("Root-to-tip",tabName = "root")),
                        title="Choose a way to parse dates:",
                        checkboxInput("type1","Defined just by its order"),
                        checkboxInput("type2","Defined by a prefix and its order"),
                        checkboxInput("type3","Defined by regular expression (REGEX)"),
                        selectInput("format","Dates format:",c("yy","yyyy","yyyy-MM-dd","MM-dd-yyyy","yyyy/MM/dd","yyyy.MM.dd","MM/dd/yyyy","MM.dd.yyyy")),
                        selectInput("order1","numerical_order:",c("1","2","3","4","5","6","7","8","last")),
                        textInput("prefix","Date_prefix:"),
                        selectInput("order2","prefix_order:",c("first","last")),
                        textInput("REGEX","regular expression:")
                        
                        
                    ),
                    
                    dashboardBody(
                        tabItems(
                            tabItem(tabName = "Dates",
                                    tableOutput("Sample")
                            ),
                            
                            tabItem(tabName = "root",
                                    box(plotOutput("distplot1"),width=8),
                                    box(width = 4,
                                        sliderInput("height","height:",0,5000,560),
                                        textInput("color3","color:",value = "black"),
                                        sliderInput("size","size:",0,10,1,step=0.1),
                                        checkboxInput("tip","tiplab:",TRUE),
                                        sliderInput("tipsize","tiplab_size:",0,10,3,step = 0.1),
                                        textInput("color4","tiplab_color:","blue")),
                                    box(plotOutput("distPlot2"),width=8),
                                    box(width=4,
                                        selectInput("method","method:",
                                                    c("rms","rsquared","correlation")),
                                        textInput("color1","point.color:",value = "red"),
                                        textInput("color2","line.color:",value = "blue")),
                                    tableOutput("Summary"),
                                    checkboxGroupInput('dele_label',label='Select Your delete label:',choices = c(1))
                            ))
                        
                        
                    )
                    
)



library(ggtree)
library(ggplot2)
library(ggpubr)
library(ape)
library(stringr)

date.type1<-function(tree,order) 
{
    date = numeric(length(tree$tip.label))
    if(order=="last"){
        for (i in 1:length(tree$tip.label)){
            result<-regexpr("\\d+(\\-?|\\d+)+(\\-?|\\d+)$",tree$tip.label[i])
            pos<-result[1]
            length<-attr(result,"match.length")-1
            time<-substr(tree$tip.label[i],pos,pos+length)
            date[i]<-time 
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

date.numeric<-function(date,format) 
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
    tree2<-rtt(tree,date,objective = method)
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
            len<-dataset[which(dataset$from==a & dataset$to==b),]$length  
            divergence[i,1]=divergence[i,1]+len
        }
    }
    return(divergence)
}



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
    
    observeEvent(input$file1,{
        inFile<-input$file1
        tree<-read.tree(inFile$datapath)
        updateCheckboxGroupInput(inputId  = "dele_label",
                                 choices  = tree$tip.label, 
        )
        
    })
    
    treeda<-reactive({
        inFile<-input$file1
        if(is.null(inFile))
            return(NULL)
        Tree<-read.tree(inFile$datapath)
        if(!is.null(input$dele_label)){
            Tree<-drop.tip(Tree,input$dele_label)}
        return(Tree)
    })
    
    output$distplot1 <- renderPlot({   
        tree<-treeda()
        p<-ggtree(tree,color=input$color3,size=input$size)
        if(input$tip){
            p<-p+geom_tiplab(size=input$tipsize,color=input$color4)
        }
        print(p)
    })
    
    output$distPlot2 <- renderPlot({
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
        tree.data<-cbind(divergence,date)
        ggplot(data=tree.data, aes(x=date, y=divergence))+geom_point(color=input$color1)+geom_smooth(method="lm",se=FALSE,colour=input$color2)
    })
    
    
    output$Summary<-renderTable({  
        tree<-treeda()
        date = numeric(length(tree$tip.label))
        if(input$type1){
            date<-date.type1(tree,input$order1)
        }
        if(input$type2){
            date<-date.type2(tree,input$order2,input$prefix)
        }
        if(input$type3){
            date<-date.type3(input$REGEX)
        }
        date<-date.numeric(date,input$format)
        divergence<-getdivergence(tree,date,input$method)
        tree.data<-cbind(divergence,date)
        
        if(input$format=="yy"|input$format=="yyyy"){
            range<-max(date)-min(date)
        }
        else{
            range<-max(date)-min(date)
            range<-range*365
        }
        
        summary<-data.frame(Dated.tips=c("Date range","Slope(rate)","X-Intercept","Correlation","R squared","RSE"),value=numeric(6))
        summary[1,2]<-range
        summary[4,2]<-as.numeric(cor(date,divergence))
        lm.rtt<-lm(tree.data)
        summary[2,2]<-as.numeric(lm.rtt$coefficients[2])
        summary[3,2]<-as.numeric(abs(lm.rtt$coefficients[1]))/as.numeric(lm.rtt$coefficients[2])
        summary[5,2]<-summary(lm.rtt)$r.squared
        summary[6,2]<-summary(lm.rtt)[["sigma"]]
        print(summary)
    },
    digits = 5,width = 8)
    
    
    output$Sample<-renderTable({ 
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