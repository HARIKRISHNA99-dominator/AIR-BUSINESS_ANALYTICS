library(tidyr)
library("dplyr") 
library(stringr) 
library(ggplot2)
library(arules)
library(arulesViz)
library(datasets)
library(RColorBrewer)

dim(BreadBasket_DMS)
Clean_Data <- drop_na(BreadBasket_DMS)
head(Clean_Data)
str(Clean_Data)
summary(Clean_Data)



#Clean_Data$Date <- as.character(Clean_Data$Date)
 #Clean_Data$Time <- as.character(Clean_Data$Time)
 #Clean_Data$Transaction <- as.character(Clean_Data$Transaction)
 #Clean_Data$Item <- as.character(Clean_Data$Item)
 
 #Clean_Data %>% 
  # mutate_if(is.character, str_trim) -> Clean_Data
 
# Clean_Data$Date <- as.factor(Clean_Data$Date)
# Clean_Data$Time <- as.factor(Clean_Data$Time)
# Clean_Data$Transaction <- as.factor(Clean_Data$Transaction)
# Clean_Data$Item <- as.factor(Clean_Data$Item)
 

 sum(is.na(Clean_Data))
 
 mean(Clean_Data$Transaction)
 mean(Clean_Data$Item)
 mean(Clean_Data$Time)
 mean(Clean_Data$Date)
 
median(Clean_Data$Transaction)
median(Clean_Data$Item)
median(Clean_Data$Time)

sd(Clean_Data$Transaction)

mode(Clean_Data$Date)
mode(Clean_Data$Time)
mode(Clean_Data$Transaction)
mode(Clean_Data$Item)


# DATA Visualization

ggplot(data = BreadBasket_DMS,mapping = aes(x=Item,y=..prop..,fill=cut))+ 
    geom_bar(width = 1)+coord_polar(theta = "x")


 
#Finding top 2
max(Clean_Data$Item)
max(Clean_Data$Item[Clean_Data$Item != max(Clean_Data$Item)]) 

#Comparing
  newdata <- Clean_Data[Clean_Data$Item =="Victorian Sponge",, drop=FALSE]
 View(newdata)
 newdata1 <- Clean_Data[Clean_Data$Item =="Vegan mincepie",, drop=FALSE]
 View(newdata1)
 
 barplot(height = newdata$Transaction,xlab = "Victorian Sponge",main = "Sales Of Victorian Sponge",col = 'red')
 barplot(height = newdata1$Transaction,xlab = "Vegan mincepie",main = "Sales Of Vegan mincepie",col = 'green')


# Q2;
 
 top50 = as.data.frame(head(Clean_Data[order(Clean_Data$Item, decreasing = TRUE), 1:4], n = 50))
 top50
 
 #histogram of top 10 transaction
 hist(top50$Transaction,col = rainbow(top50$Transaction))
 
 #
 
 barplot(top50$Transaction,
                  main = "Each Item",
                  xlab = "Class",
                  col = c("red","green")
          )
 legend("topleft",
                  c("Victorian Sponge","Vegan mincepie"),
                  fill = c("red","green")
           )
 
 
 #TOP 10 Products Top 100 sales According to transaction
 
 top100 = as.data.frame(head(Clean_Data[order(Clean_Data$Item, decreasing = TRUE), 1:4], n = 100))
 view(top100)
 
 ggplot(top100, aes(x = Item ,y = Transaction,fill = "red")) + 
   geom_col() + coord_flip() + xlab("Items")

 
 
 
