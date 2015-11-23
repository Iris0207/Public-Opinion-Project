library("arules")
data<-read.csv("data_new.csv")
class(data)
assodata<-data['topic']
ma<-matrix(rep(0,106200),nrow=10620,ncol=10)
asso<-apply(assodata,1,as.character)

for(i in 1:length(asso)){
    list<-strsplit(asso[i],",")
    li<-unlist(list)
    for(n in li){ma[i,as.numeric(n)+1]=1}
}
colnames(ma)=c("房价","开发商资金链","降息","土地市场","公积金新政","不动产登记","旧城改造","房产税改革","保障性住房","房地产成交量")
d<-as(ma,"transactions")
summary(d)
rules <- apriori(d, parameter = list(support =0.05, confidence = 0.5,target = "rules"))  
summary(rules)
inspect(rules[1:3])
ordered_rules <- sort(rules, by="lift",)  
inspect(ordered_rules[1:5])  
write(rules, file="rules.csv", sep=",", quote=TRUE, row.names=FALSE)  

###plot
library("grid")
library("arulesViz")
library("RColorBrewer")

Size<-size(rules) 
itemFrequencyPlot(d)

Tonerule<-subset(rules, items %in% c("房价"))     ##the rules including topic one
inspect(Tonerule,decreasing=T,topN=5)

plot(rules,control=list(jitter=2, col= rev(brewer.pal(9, "Greens")[4:9])),shading = "lift")   

plot(rules, method="grouped",     
     control=list(col = rev(brewer.pal(9, "Greens"))))

top.rules <- head(sort(rules, decreasing = TRUE, by = "lift"), 10)
inspect(top.rules)
plot(rules, measure="support", method="graph",   
     control=list(type="items",arrowSize=0.3),   
     shading = "lift")


plot(rules, method="graph", control=list(type="items"),measure="support",interactive=TRUE)
plot(rules, method="paracoord", control=list(reorder=TRUE))
plot(rules,method="matrix",type="grid",reorder=TRUE)
