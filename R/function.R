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