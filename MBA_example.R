#following a tutorial that produces a working results. iterate from this. learn the parts I misunderstood
#https://datascienceplus.com/a-gentle-introduction-on-market-basket-analysis%E2%80%8A-%E2%80%8Aassociation-rules/

library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)

retail <- read_excel('Online Retail (1).xlsx')
retail <- retail[complete.cases(retail), ] #what does complete cases do? remove null rows?
retail %>% mutate(Description = as.factor(Description)) #why do I need to transform this into a factor? what is the effect?
retail %>% mutate(Country = as.factor(Country)) #above
retail$Date <- as.Date(retail$InvoiceDate) #date conversion for plotting or also for other purpose?
retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")
retail$InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
glimpse(retail)

retail$Time <- as.factor(retail$Time) # time into factor - what does this do?
retail %>%
  ggplot(aes(x=Time)) + 
  geom_histogram(stat="count",fill="indianred")

retail_sorted <- retail[order(retail$CustomerID),]#why order them, is is because the following transformation?
library(plyr)
#I dont understand hod ddply works, below is the tutorial description:
#The function ddply() accepts a data frame, splits it into pieces based on one or more factors.
#It computes on the pieces, and then returns the results as a data frame. We use “,” to separate different items.
itemList <- ddply(retail,c("CustomerID","Date"), 
                  function(df1)paste(df1$Description, 
                                     collapse = ","))

itemList$CustomerID <- NULL #this is ok, removing unwanted columns
itemList$Date <- NULL
colnames(itemList) <- c("items") #renaming columns

write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE) 

tr <- read.transactions('market_basket.csv', format = 'basket', sep=',')
tr
summary(tr)

itemFrequencyPlot(tr, topN=20, type='absolute') #works ok, gives expected results, (separator works unlike my attempt)

rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE) # added sorting, good idea I guess
summary(rules)

inspect(rules[1:10])

topRules <- rules[1:10]
plot(topRules)

plot(topRules, method="graph")
