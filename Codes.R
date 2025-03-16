# 1 - read the data file
marketbasket = read.csv("Assosiation_assign.csv", header = TRUE, colClasses = "factor")
# 2 - inspect the dataset
names(marketbasket)
head(marketbasket)
tail(marketbasket)
summary(marketbasket)
str(marketbasket)
# 3 - check the dimension of the dataset
dim(marketbasket)
# 4 - plot and explore the dataset
yes = colSums(marketbasket == "Yes")
yes
no = colSums(marketbasket == "No")
no
purchased = rbind(yes,no)
purchased
barplot(purchased, legend = rownames(purchased))
barplot(purchased, beside = T, legend =  rownames(purchased))
# 5 - install "arules" package
install.packages("arules")
library(arules)
# 6 - create association rules
rules = apriori(marketbasket)
## it shows that 29339832 rules were generated using the line of code.
# 7 - getting the summary of these rules 
summary(rules)
# 8 - inspect the rules 
inspect(rules)
# 9 - the rules are reduced to a smaller number of rules 
rules = apriori(marketbasket, parameter = list(minlen = 2, maxlen = 3, conf = 0.85))
## this has reduced the rules to 62
# 10 - getting the summary of these rules 
summary(rules)
# 11 - inspect the rules 
inspect(rules)
# 12 - get the summary of the dataset 
summary(marketbasket)
# 13 - plotting to find the most purchased item
barplot(purchased, beside = TRUE, legend = rownames(purchased))
rules <- apriori(marketbasket, 
                 parameter = list(minlen=2, maxlen=3,conf = 0.85), 
                 appearance= list(rhs=c("vegetables=Yes"),default="lhs"))
summary(rules)
# 14 - a lower confidence threshold is added 
rules = apriori(marketbasket, 
                parameter = list(minlen=2, maxlen=3,conf = 0.88), 
                appearance= list(rhs=c("vegetables=Yes"),default="lhs")) 
summary(rules)
# 15 - inspect the rules 
inspect(rules)
# 16 - to visualize these rules we are using the "arulesViz" package
install.packages("arulesViz")
library(arulesViz)
# 17 - plot the rules
plot(rules)
# 18 - plotting the rules in groups 
plot(rules,method = "grouped")
# 19 - displays the support, confidence, and lift with a scatterplot matrix
plot(rules@quality)
# 20 - this is used to create interactive scatter plot for association rules
rules3 = apriori(marketbasket, 
                 parameter = list(minlen=2,maxlen=3, conf = 0.8),
                 appearance =list(rhs=c("lunch.meat=Yes","all.purpose=Yes","juice=Yes")
                                  ,default="lhs") )
summary(rules3) 
plot(rules3)
plot(rules3, measure = c("support", "lift"), shading = "confidence")
# 21 - This code gets the rules only purchase items "YES" on left hand side and right hand side.
rules2 <- apriori(marketbasket, 
                  parameter = list(minlen=2, maxlen=3,conf = 0.88),
                  appearance =list(rhs=c("vegetables=Yes"),
                                   lhs=c("all.purpose=Yes",
                                         "bagels=Yes",
                                         "juice=Yes",
                                         "lunch.meat=Yes",
                                         "poultry=Yes",
                                         "toilet.paper=Yes",
                                         "yogurt=Yes"),
                                   default="none"))
summary(rules2)
inspect(rules2) 





