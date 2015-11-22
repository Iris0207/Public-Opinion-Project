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
colnames(ma)=c("一","二","三","四","五","六","七","八","九","十")
d<-as(ma,"transactions")
summary(d)
rules <- apriori(d, parameter = list(support =0.01, confidence = 0.6, minlen = 2))  
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
itemFrequencyPlot(d,topN=5)

Tonerule<-subset(rules, items %in% c("一"))     ##the rules including topic one
inspect(Tonerule,decreasing=T,topN=5)

plot(rules,control=list(jitter=2, col= rev(brewer.pal(9, "Greens")[4:9])),shading = "lift")   

plot(rules, method="grouped",     
     control=list(col = rev(brewer.pal(9, "Greens")))

top.rules <- head(sort(rules, decreasing = TRUE, by = "lift"), 10)
inspect(top.rules)
plot(sort(rules, decreasing = TRUE, by = "lift")[1:20], measure="confidence", method="graph",   
     control=list(type="items"),   
     shading = "lift")


windows();plot(rules, method="graph", control=list(type="items"))
plot(rules, method="paracoord", control=list(reorder=TRUE))

