#
################################################################################################################################# 
#                                                                                                                               #
# Project: CYO - Hepatitis C Prediction                                                                                         #
# Name   : Sunil Kumar P                                                                                                        #
# Date   : 25/Dec/21                                                                                                            #
#                                                                                                                               #
################################################################################################################################# 

################################################################################################################################# 
#                                                                                                                               #
#                                         Installing all required packages                                                      #
#                                                                                                                               #
################################################################################################################################# 

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(dslabs)
library(data.table)
library(knitr)
library(RColorBrewer)

set.seed(1, sample.kind="Rounding")

################################################################################################################################# 
#                                                                                                                               #
#           Dataset hcvdat0.csv downloaded from https://archive.ics.uci.edu/ml/machine-learning-databases/00571/hcvdat0.csv     #
#                                                                                                                               #
################################################################################################################################# 


Hepc<-read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00571/hcvdat0.csv")

################################################################################################################################# 
#                                                                                                                               #
#                                  Data analysis of the Hepc dataset                                                            #
#                                                                                                                               #
################################################################################################################################# 

str(Hepc)
nrow(Hepc)
length(Hepc)
head(Hepc)

#----------------#
#   1: Category  #
#----------------#

################################################################################################################################# 
#                                                                                                                               #
#             #1a - Data Analysis: Category, by summarizing the frequency of the category values                                 #
#                                                                                                                               #
################################################################################################################################# 

Hepc$Category %>% class
Hepc %>% group_by(Category) %>% summarize(Count=n()) %>% kable()

################################################################################################################################# 
#                                                                                                                               #
#             #1b - Data Analysis: Category (histogram with count) 
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(Category)) + geom_histogram(aes(fill=..count..),stat = "count") + geom_text(stat='count', aes(label=..count..,col=I("white")), position = position_stack(vjust = .5),size=4) +
labs(
  title = "Data Analysis",
  subtitle = "Category",
  x = "Category",
  y = "Count") +
   theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  ) 


#-----------#
#   2: Age  #
#-----------#

################################################################################################################################# 
#                                                                                                                               #
#             #2a - Data Analysis: Age (publishing the Mean, Median and SD)
#                                                                                                                               #
################################################################################################################################# 

Hepc$Age %>% class
tibble(Median=median(Hepc$Age), Mean=mean(Hepc$Age), SD=sd(Hepc$Age)) %>% kable()

################################################################################################################################# 
#                                                                                                                               #
#             #2b - Data Analysis: Age (histogram with count) 
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(Age)) + geom_histogram(aes(fill=..count..),stat = "count") +  geom_text(stat='count', aes(label=..count..,col=I("white")), position = position_stack(vjust = 0.5),size=4) + labs(
  title = "Data Analysis",
  subtitle = "Age",
  x = "Age",
  y = "Count") +
  
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  ) 

################################################################################################################################# 
#                                                                                                                               #
#             #2c - Data Analysis: Age (Exploring the quartile ranges and values)   
#                                                                                                                               #
################################################################################################################################# 

summary(Hepc$Age,na.rm=TRUE)
IQR(Hepc$Age,na.rm=TRUE)

################################################################################################################################# 
#                                                                                                                               #
#             #2d - Data Analysis: Age (boxplot to visualize the IQR)
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(y=Age)) + geom_boxplot(fill=I("light blue")) + scale_y_continuous(breaks=seq(0,100,5)) +
  labs(
    title = "Data Analysis",
    subtitle = "Age",
    x = "Age",
    y = "Count") +
  
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  ) 

################################################################################################################################# 
#                                                                                                                               #
#             #2e - Data Analysis: Age (co-relation with category demonstrated visually via boxplot)
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(Category,Age)) + geom_boxplot() +geom_point(position = "jitter",aes(color=Category)) + scale_y_continuous(breaks=seq(0,100,5)) +
  labs(
    title = "Data Analysis - Correlation",
    subtitle = "Age and Category",
    x = "Category",
    y = "Age") +
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  )


#-----------#
#   3: Sex  #
#-----------#

################################################################################################################################# 
#                                                                                                                               #
#             #3a - Data Analysis: Sex (histogram with count) 
#                                                                                                                               #
################################################################################################################################# 

Hepc$Sex %>% class

Hepc %>% ggplot(aes(Sex)) + geom_histogram(aes(fill=..count..),stat = "count") +  geom_text(stat='count', aes(label=..count..,col=I("white")), position = position_stack(vjust = 0.5),size=4)+
  labs(
    title = "Data Analysis",
    subtitle = "Sex",
    x = "Sex",
    y = "Count") +
    theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
         )

################################################################################################################################# 
#                                                                                                                               #
#             #3b - Data Analysis: Sex (co-relation with category demonstrated visually via boxplot)
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% group_by(Category,Sex) %>% summarize(count=n()) %>% ggplot(aes(Category,count,color=Sex)) + geom_point() +
  labs(
    title = "Data Analysis",
    subtitle = "Sex and Category",
    x = "Category",
    y = "Count") +
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  )


#-----------#
#   4: ALB  #
#-----------#

################################################################################################################################# 
#                                                                                                                               #
#             #4a - Data Analysis: ALB (publishing the Mean, Median and SD)  
#                                                                                                                               #
################################################################################################################################# 


Hepc$ALB %>% class
tibble(Median=median(Hepc$ALB,na.rm=TRUE),Mean=mean(Hepc$ALB,na.rm=TRUE),SD=sd(Hepc$ALB,na.rm=TRUE)) %>% kable()

################################################################################################################################# 
#                                                                                                                               #
#             #4b - Data Analysis: ALB (histogram with count) 
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(y=ALB)) + geom_boxplot(fill=I("light blue")) + scale_y_continuous(breaks=seq(0,90,3)) +
  labs(
    title = "Data Analysis",
    subtitle = "ALB",
    y = "ALB")+
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  )
 
################################################################################################################################# 
#                                                                                                                               #
#             #4c - Data Analysis: ALB (Exploring the quartile ranges and values)   
#                                                                                                                               #
################################################################################################################################# 

summary(Hepc$ALB,na.rm=TRUE)
IQR(Hepc$ALB,na.rm=TRUE)

################################################################################################################################# 
#                                                                                                                               #
#             #4d - Data Analysis: ALB (boxplot to visualize the IQR)
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(ALB)) + geom_histogram(binwidth = 5,aes(fill=..count..)) +
  labs(
    title = "Data Analysis",
    subtitle = "ALB") +  theme(
      plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(face = "bold", hjust = 0.5)
    )


################################################################################################################################# 
#                                                                                                                               #
#             #4e - Data Analysis: ALB (co-relation with category demonstrated visually via boxplot)
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(Category,ALB)) + geom_boxplot() + scale_y_continuous(breaks=seq(0,90,10)) + geom_point(position = "jitter",aes(color=Category)) +
  labs(
    title = "Data Analysis: Correlation",
    subtitle = "ALB and Category ",
    x = "Category",
    y = "ALB") +
  
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  )


##------#
#5: ALP
##-------#

################################################################################################################################# 
#                                                                                                                               #
#             #5a - Data Analysis: ALP (publishing the Mean, Median and SD)
#                                                                                                                               #
################################################################################################################################# 


Hepc$ALP %>% class
tibble(Median=median(Hepc$ALP,na.rm=TRUE),Mean=mean(Hepc$ALP,na.rm=TRUE),SD=sd(Hepc$ALP,na.rm=TRUE)) %>% kable()


################################################################################################################################# 
#                                                                                                                               #
#             #5b - Data Analysis: ALP (histogram with count) 
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(y=ALP)) + geom_boxplot(fill=I("light blue")) + scale_y_continuous(breaks=seq(0,450,10)) +
  labs(
    title = "Data Analysis",
    subtitle = "ALP",
    y = "ALP")+
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  )

################################################################################################################################# 
#                                                                                                                               #
#             #5c - Data Analysis: ALP (Exploring the quartile ranges and values)   
#                                                                                                                               #
################################################################################################################################# 

summary(Hepc$ALP,na.rm=TRUE)
IQR(Hepc$ALP,na.rm=TRUE)

################################################################################################################################# 
#                                                                                                                               #
#             #5d - Data Analysis: ALP (boxplot to visualize the IQR)
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(ALP)) + geom_histogram(binwidth = 25,aes(fill=..count..)) +
  labs(
    title = "Data Analysis",
    subtitle = "ALP") +  theme(
      plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(face = "bold", hjust = 0.5)
    )
# Histogram is left skewed 

################################################################################################################################# 
#                                                                                                                               #
#             #5e - Data Analysis: ALP (co-relation with category demonstrated visually via boxplot)
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(Category,ALP)) + geom_boxplot(aes(fill=Category)) + scale_y_continuous(breaks=seq(0,450,10)) +
  labs(
    title = "Data Analysis - Correlation",
    subtitle = "ALP and Category",
    x = "Category",
    y = "ALP") +
  
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  )

##------#
#6: ALT
##-------#


################################################################################################################################# 
#                                                                                                                               #
#             #6a - Data Analysis: ALT (publishing the Mean, Median and SD)
#                                                                                                                               #
################################################################################################################################# 

Hepc$ALT %>% class
tibble(Median=median(Hepc$ALT,na.rm=TRUE),Mean=mean(Hepc$ALT,na.rm=TRUE),SD=sd(Hepc$ALT,na.rm=TRUE)) %>% kable()

################################################################################################################################# 
#                                                                                                                               #
#             #6b - Data Analysis: ALT (histogram with count) 
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(y=ALT)) + geom_boxplot(fill=I("light blue")) + scale_y_continuous(breaks=seq(0,350,10)) +
  labs(
    title = "Data Analysis",
    subtitle = "ALT",
    y = "ALT") +
  
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
    
  )

################################################################################################################################# 
#                                                                                                                               #
#             #6c - Data Analysis: ALT (Exploring the quartile ranges and values)   
#                                                                                                                               #
################################################################################################################################# 

summary(Hepc$ALT,na.rm=TRUE)
IQR(Hepc$ALT,na.rm=TRUE)

################################################################################################################################# 
#                                                                                                                               #
#             #6d - Data Analysis: ALT (boxplot to visualize the IQR)
#                                                                                                                               #
################################################################################################################################# 


Hepc %>% ggplot(aes(ALT)) + geom_histogram(binwidth = 25,aes(fill=..count..))  +
  labs(
    title = "Data Analysis",
    subtitle = "ALT") +  theme(
      plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(face = "bold", hjust = 0.5)
    )
 
################################################################################################################################# 
#                                                                                                                               #
#             #6e - Data Analysis: ALT (co-relation with category demonstrated visually via boxplot)
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(Category,ALT)) + geom_boxplot() + scale_y_continuous(breaks=seq(0,350,10)) +
  labs(
    title = "Data Analysis - Correlation",
    subtitle = "ALT and Category",
    x = "Category",
    y = "ALT") +
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  ) + geom_point(position = "jitter",aes(color=Category))


##------#
#7: AST
##-------#


################################################################################################################################# 
#                                                                                                                               #
#             #7a - Data Analysis: AST (publishing the Mean, Median and SD)
#                                                                                                                               #
################################################################################################################################# 

Hepc$AST %>% class
tibble(Median=median(Hepc$AST,na.rm=TRUE),Mean=mean(Hepc$AST,na.rm=TRUE),SD=sd(Hepc$AST,na.rm=TRUE)) %>% kable()


################################################################################################################################# 
#                                                                                                                               #
#             #7b - Data Analysis: AST (histogram with count) 
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(y=AST)) + geom_boxplot(fill=I("light blue")) + scale_y_continuous(breaks=seq(0,350,10)) +
  labs(
    title = "Data Analysis",
    subtitle = "AST",
    y = "AST") +
  
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  )

################################################################################################################################# 
#                                                                                                                               #
#             #7c - Data Analysis: AST (Exploring the quartile ranges and values)   
#                                                                                                                               #
################################################################################################################################# 

summary(Hepc$AST,na.rm=TRUE)
IQR(Hepc$AST,na.rm=TRUE)

################################################################################################################################# 
#                                                                                                                               #
#             #7d - Data Analysis: AST (boxplot to visualize the IQR)
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(AST)) + geom_histogram(binwidth = 25,aes(fill=..count..)) +
labs(
  title = "Data Analysis",
  subtitle = "AST") +  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  )

################################################################################################################################# 
#                                                                                                                               #
#             #7e - Data Analysis: AST (co-relation with category demonstrated visually via boxplot)
#                                                                                                                               #
################################################################################################################################# 


Hepc %>% ggplot(aes(Category,AST)) + geom_boxplot(fill=I("light blue")) + scale_y_continuous(breaks=seq(0,350,10)) +
  labs(
    title = "Data Analysis - Correlation",
    subtitle = "AST and Category",
    x = "Category",
    y = "AST") +
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
    )


##------#
#8: BIL
##-------#

################################################################################################################################# 
#                                                                                                                               #
#             #8a - Data Analysis: BIL (publishing the Mean, Median and SD)
#                                                                                                                               #
################################################################################################################################# 

Hepc$BIL %>% class
tibble(Median=median(Hepc$BIL,na.rm=TRUE),Mean=mean(Hepc$BIL,na.rm=TRUE),SD=sd(Hepc$BIL,na.rm=TRUE)) %>% kable()

################################################################################################################################# 
#                                                                                                                               #
#             #8b - Data Analysis: BIL (histogram with count) 
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(y=BIL)) + geom_boxplot(fill=I("light blue")) + scale_y_continuous(breaks=seq(0,350,10)) +
  labs(
    title = "Data Analysis",
    subtitle = "BIL",
    y = "BIL")+
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  )

################################################################################################################################# 
#                                                                                                                               #
#             #8c - Data Analysis: BIL (Exploring the quartile ranges and values)   
#                                                                                                                               #
################################################################################################################################# 

summary(Hepc$BIL,na.rm=TRUE)
IQR(Hepc$BIL,na.rm=TRUE)

################################################################################################################################# 
#                                                                                                                               #
#             #8d - Data Analysis: BIL (boxplot to visualize the IQR)
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(BIL)) + geom_histogram(binwidth = 25,aes(fill=..count..)) +
  labs(
    title = "Data Analysis",
    subtitle = "BIL") +  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  )
 
################################################################################################################################# 
#                                                                                                                               #
#             #8e - Data Analysis: BIL (co-relation with category demonstrated visually via boxplot)
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(Category,BIL)) + geom_boxplot(fill=I("light blue")) + scale_y_continuous(breaks=seq(0,350,10)) +
  labs(
    title = "Data Analysis - Correlation",
    subtitle = "BIL and Category",
    x = "Category",
    y = "BIL") +
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  )


##------#
#9: CHE
##-------#

################################################################################################################################# 
#                                                                                                                               #
#             #9a - Data Analysis: CHE (publishing the Mean, Median and SD)
#                                                                                                                               #
################################################################################################################################# 

Hepc$CHE %>% class
tibble(Median=median(Hepc$CHE,na.rm=TRUE),Mean=mean(Hepc$CHE,na.rm=TRUE),SD=sd(Hepc$CHE,na.rm=TRUE)) %>% kable()

################################################################################################################################# 
#                                                                                                                               #
#             #9b - Data Analysis: CHE (histogram with count) 
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(y=CHE)) + geom_boxplot(fill=I("light blue")) + scale_y_continuous(breaks=seq(0,20,1)) +
  labs(
    title = "Data Analysis",
    subtitle = "CHE",
    y = "CHE")+
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  )

################################################################################################################################# 
#                                                                                                                               #
#             #9c - Data Analysis: CHE (Exploring the quartile ranges and values)   
#                                                                                                                               #
################################################################################################################################# 

summary(Hepc$CHE,na.rm=TRUE)
IQR(Hepc$CHE,na.rm=TRUE)

################################################################################################################################# 
#                                                                                                                               #
#             #9d - Data Analysis: CHE (boxplot to visualize the IQR)
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(CHE)) + geom_histogram(binwidth = 1,aes(fill=..count..))  +
  labs(
    title = "Data Analysis",
    subtitle = "CHE") +  theme(
      plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(face = "bold", hjust = 0.5)
    )

################################################################################################################################# 
#                                                                                                                               #
#             #9e - Data Analysis: CHE (co-relation with category demonstrated visually via boxplot)
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(Category,CHE)) + geom_boxplot(aes(fill=Category)) + scale_y_continuous(breaks=seq(0,20,1)) +
  labs(
    title = "Data Analysis - Correlation",
    subtitle = "CHE and Category",
    x = "Category",
    y = "CHE") +
    theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic", hjust = 0.5)
    )


##------#
#10: CHOL
##-------#

################################################################################################################################# 
#                                                                                                                               #
#             #10a - Data Analysis: CHOL (publishing the Mean, Median and SD)
#                                                                                                                               #
################################################################################################################################# 

Hepc$CHOL %>% class
tibble(Median=median(Hepc$CHOL,na.rm=TRUE),Mean=mean(Hepc$CHOL,na.rm=TRUE),SD=sd(Hepc$CHOL,na.rm=TRUE)) %>% kable()

################################################################################################################################# 
#                                                                                                                               #
#             #10b - Data Analysis: CHOL (histogram with count) 
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(y=CHOL)) + geom_boxplot(fill=I("light blue")) + scale_y_continuous(breaks=seq(0,20,1)) +
  labs(
    title = "Data Analysis",
    subtitle = "CHOL",
    y = "CHOL")+
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  )
################################################################################################################################# 
#                                                                                                                               #
#             #10c - Data Analysis: CHOL (Exploring the quartile ranges and values)   
#                                                                                                                               #
################################################################################################################################# 

summary(Hepc$CHOL,na.rm=TRUE)
IQR(Hepc$CHOL,na.rm=TRUE)

################################################################################################################################# 
#                                                                                                                               #
#             #10d - Data Analysis: CHOL (boxplot to visualize the IQR)
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(CHOL)) + geom_histogram(binwidth = 1,aes(fill=..count..))  +
  labs(
    title = "Data Analysis",
    subtitle = "CHOL") +  theme(
      plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(face = "bold", hjust = 0.5)
    )  

################################################################################################################################# 
#                                                                                                                               #
#             #10e - Data Analysis: CHOL (co-relation with category demonstrated visually via boxplot)
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(Category,CHOL)) + geom_boxplot(aes(col=Category)) + scale_y_continuous(breaks=seq(0,20,1)) +
  labs(
    title = "Data Analysis - Correlation",
    subtitle = "CHOL and Category",
    x = "Category",
    y = "CHOL") +
    theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  )


##------#
#11: CREA 
##-------#

################################################################################################################################# 
#                                                                                                                               #
#             #11a - Data Analysis: CREA (publishing the Mean, Median and SD)
#                                                                                                                               #
################################################################################################################################# 

Hepc$CREA %>% class
tibble(Median=median(Hepc$CREA,na.rm=TRUE),Mean=mean(Hepc$CREA,na.rm=TRUE),SD=sd(Hepc$CREA,na.rm=TRUE)) %>% kable()

################################################################################################################################# 
#                                                                                                                               #
#             #11b - Data Analysis: CREA (histogram with count) 
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(y=CREA)) + geom_boxplot(fill=I("light blue")) + scale_y_continuous(breaks=seq(0,300,50)) +
  labs(
    title = "Data Analysis",
    subtitle = "CREA",
    y = "CREA")+
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  )

################################################################################################################################# 
#                                                                                                                               #
#             #11c - Data Analysis: CREA (Exploring the quartile ranges and values)   
#                                                                                                                               #
################################################################################################################################# 

summary(Hepc$CREA,na.rm=TRUE)
IQR(Hepc$CREA,na.rm=TRUE)

################################################################################################################################# 
#                                                                                                                               #
#             #11d - Data Analysis: CREA (boxplot to visualize the IQR)
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(CREA)) + geom_histogram(binwidth = 50,aes(fill=..count..))  +
  labs(
    title = "Data Analysis",
    subtitle = "CREA") +  theme(
      plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(face = "bold", hjust = 0.5)
    )   

################################################################################################################################# 
#                                                                                                                               #
#             #11e - Data Analysis: CREA (co-relation with category demonstrated visually via boxplot)
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(Category,CREA)) + geom_boxplot(aes(col=Category)) + scale_y_continuous(breaks=seq(0,100,50)) +
  labs(
    title = "Data Analysis - Correlation",
    subtitle = "CREA and Category",
    x = "Category",
    y = "CREA") +
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  ) +geom_point(position = "jitter",aes(color=Category))


##------#
#12: GGT 
##-------#

################################################################################################################################# 
#                                                                                                                               #
#             #12a - Data Analysis: GGT (publishing the Mean, Median and SD)
#                                                                                                                               #
################################################################################################################################# 

Hepc$GGT %>% class
tibble(Median=median(Hepc$GGT,na.rm=TRUE),Mean=mean(Hepc$GGT,na.rm=TRUE),SD=sd(Hepc$GGT,na.rm=TRUE)) %>% kable()

################################################################################################################################# 
#                                                                                                                               #
#             #12b - Data Analysis: GGT (histogram with count) 
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(y=GGT)) + geom_boxplot(fill=I("light blue")) + scale_y_continuous(breaks=seq(0,100,50)) +
  labs(
    title = "Data Analysis",
    subtitle = "GGT",
    y = "GGT")+
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  )

################################################################################################################################# 
#                                                                                                                               #
#             #12c - Data Analysis: GGT (Exploring the quartile ranges and values)   
#                                                                                                                               #
################################################################################################################################# 

summary(Hepc$GGT,na.rm=TRUE)
IQR(Hepc$GGT,na.rm=TRUE)

################################################################################################################################# 
#                                                                                                                               #
#             #12d - Data Analysis: GGT (boxplot to visualize the IQR)
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(GGT)) + geom_histogram(binwidth = 50,aes(fill=..count..))  +
  labs(
    title = "Data Analysis",
    subtitle = "GGT") +  theme(
      plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(face = "bold", hjust = 0.5)
    )   

################################################################################################################################# 
#                                                                                                                               #
#             #12e - Data Analysis: GGT (co-relation with category demonstrated visually via boxplot)
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(Category,GGT)) + geom_boxplot() + scale_y_continuous(breaks=seq(0,100,50)) +
  labs(
    title = "Data Analysis: Correlation",
    subtitle = "GGT and Category",
    x = "Category",
    y = "GGT") +
    theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  )+geom_point(position = "jitter",aes(color=Category))


##------#
#13: PROT 
##-------#

################################################################################################################################# 
#                                                                                                                               #
#             #13a - Data Analysis: PROT (publishing the Mean, Median and SD)
#                                                                                                                               #
################################################################################################################################# 


Hepc$PROT %>% class
tibble(Median=median(Hepc$PROT,na.rm=TRUE),Mean=mean(Hepc$PROT,na.rm=TRUE),SD=sd(Hepc$PROT,na.rm=TRUE)) %>% kable()

################################################################################################################################# 
#                                                                                                                               #
#             #13b - Data Analysis: PROT (histogram with count) 
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(y=PROT)) + geom_boxplot(fill=I("light blue")) + scale_y_continuous(breaks=seq(0,100,10)) +
  labs(
    title = "Data Analysis",
    subtitle = "PROT",
    y = "PROT")+
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)  )

################################################################################################################################# 
#                                                                                                                               #
#             #13c - Data Analysis: PROT (Exploring the quartile ranges and values)   
#                                                                                                                               #
################################################################################################################################# 

summary(Hepc$PROT,na.rm=TRUE)
IQR(Hepc$PROT,na.rm=TRUE)

################################################################################################################################# 
#                                                                                                                               #
#             #13d - Data Analysis: PROT (boxplot to visualize the IQR)
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(PROT)) + geom_histogram(binwidth = 3,aes(fill=..count..))  +
  labs(
    title = "Data Analysis",
    subtitle = "PROT") +  theme(
      plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(face = "bold", hjust = 0.5)
    )   

################################################################################################################################# 
#                                                                                                                               #
#             #13e - Data Analysis: PROT (co-relation with category demonstrated visually via boxplot)
#                                                                                                                               #
################################################################################################################################# 

Hepc %>% ggplot(aes(Category,PROT)) + geom_boxplot(aes(fill=Category)) + scale_y_continuous(breaks=seq(0,100,50)) +
  labs(
    title = "Data Analysis - Correlation",
    subtitle = "PROT and Category",
    x = "Category",
    y = "PROT") +
    theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5),
  )



################################################################################################################################# 
#                                                                                                                               #
#                               Data Cleansing
#                                                                                                                               #
################################################################################################################################# 


################################################################################################################################# 
#                                                                                                                               #
#                               Handling NAs by omitting them before partitioning Hepc into train and test sets
#                                                                                                                               #
################################################################################################################################# 

Hepc_cleaned <- na.omit(Hepc,na.action = "omit", fill = NULL)
nrow(Hepc_cleaned) #589
nrow(Hepc) #615

removed <- 1-(nrow(Hepc_cleaned)/nrow(Hepc))
removed

sum(is.na(Hepc$X)) #0
sum(is.na(Hepc$Category)) #0
sum(is.na(Hepc$Age)) #0
sum(is.na(Hepc$Sex)) #0
sum(is.na(Hepc$ALB)) #1
sum(is.na(Hepc$ALP)) #18
sum(is.na(Hepc$ALT)) #1
sum(is.na(Hepc$AST)) #0
sum(is.na(Hepc$BIL)) #0
sum(is.na(Hepc$CHE)) #0
sum(is.na(Hepc$CHOL)) #10
sum(is.na(Hepc$CREA)) #0
sum(is.na(Hepc$GGT)) #0
sum(is.na(Hepc$PROT)) #1

################################################################################################################################# 
#                                                                                                                               #
#                               Confirming observations with atleast 1 NA are dropped 
#                                                                                                                               #
################################################################################################################################# 

sum(is.na(Hepc_cleaned$X)) 
sum(is.na(Hepc_cleaned$Category)) 
sum(is.na(Hepc_cleaned$Age))
sum(is.na(Hepc_cleaned$Sex))
sum(is.na(Hepc_cleaned$ALB)) 
sum(is.na(Hepc_cleaned$ALP))  
sum(is.na(Hepc_cleaned$ALT))  
sum(is.na(Hepc_cleaned$AST))  
sum(is.na(Hepc_cleaned$BIL))  
sum(is.na(Hepc_cleaned$CHE))  
sum(is.na(Hepc_cleaned$CHOL))  
sum(is.na(Hepc_cleaned$CREA))  
sum(is.na(Hepc_cleaned$GGT))  
sum(is.na(Hepc_cleaned$PROT))  

################################################################################################################################# 
#                                                                                                                               #
#              Correlation among the 10 predictors
#                                                                                                                               #
################################################################################################################################# 

cor(Hepc_cleaned[-(1:4)])
cor(Hepc_cleaned[-(1:4)]) %>% kable()


corind<- which((cor(Hepc_cleaned[-(1:4)]) >= 0.5) &  (cor(Hepc_cleaned[-(1:4)]) < 1.0))
corind
cor(Hepc_cleaned[-(1:4)])[corind]

corind<- which((cor(Hepc_cleaned[-(1:4)]) >=0.3) &  (cor(Hepc_cleaned[-(1:4)]) < 0.5))
corind
cor(Hepc_cleaned[-(1:4)])[corind]
cor(Hepc_cleaned[-(1:4)])[corind] %>% length()

corind<- which((cor(Hepc_cleaned[-(1:4)]) <0.3))
corind
cor(Hepc_cleaned[-(1:4)])[corind]  

################################################################################################################################# 
#                                                                                                                               #
#              Scatter Plot of all 10 predictors to visualize this correlation
#                                                                                                                               #
################################################################################################################################# 

pairs(~.,data = Hepc_cleaned[-(1:4)],main="Scatter Plot of all 10 predictors",col="dark blue")


################################################################################################################################# 
#                                                                                                                               #
# Principal component analysis  to describe variance 
#
################################################################################################################################# 

################################################################################################################################# 
#                                                                                                                               #
# PCA: Data Standardization 
#
################################################################################################################################# 

pca <- prcomp(Hepc_cleaned[-(1:4)],center = TRUE, scale = TRUE)
summary(pca)

################################################################################################################################# 
#                                                                                                                               #
#     PCA: Using a Screeplot to visualize selection of factors  
#
################################################################################################################################# 

screeplot(pca, type = "l", npcs = 10)
abline(h = 1, col="blue", lty=20)

################################################################################################################################# 
#                                                                                                                               #
#     PCA: Plot to illustrate cumulative variance 
#
################################################################################################################################# 

cum_var <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(cum_var)
abline(v = 8, col="blue", lty=20)
abline(h = 0.9, col="blue", lty=20)

summary(pca)$importance[3,] 

################################################################################################################################# 
#                                                                                                                               #
#     PCA: Plotting the first 2 principal components to explain >42% variance
#
################################################################################################################################# 


plot(pca$x[,1],pca$x[,2],xlab ="PC1",ylab="PC2") 


################################################################################################################################# 
#                                                                                                                               #
#     PCA: Plotting the PCs with Category 
#
################################################################################################################################# 

data.frame(type = Hepc_cleaned$Category, pca$x[,1:10]) %>%
  gather(key = "key", value = "value", -type) %>%  
  ggplot(aes(key, value, fill = type)) +
  geom_boxplot()  +
  labs(
    title = "PCA plot",
    subtitle = "10 Features")+
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5),
  )


################################################################################################################################# 
#                                                                                                                               #
#                               Split the Hepc dataset into train and test sets 
#
#                 Owing to the smaller dataset size (600+ observations with about 10 predictors), 
#                  it's a 70:30 split used here for training and test datasets respectively
#
#                                                                                                                               #
################################################################################################################################# 

test_index <- createDataPartition(y = Hepc_cleaned$Category, times = 1, p = 0.3, list = FALSE)
train_set <- Hepc_cleaned[-test_index,]
test_set <- Hepc_cleaned[test_index,]



################################################################################################################################# 
#                                                                                                                               #
#             Let's Explore the train and test sets
#                                                                                                                               #
################################################################################################################################# 



################################################################################################################################# 
#                                                                                                                               #
#              Data check: train set, Category                                         #
#                                                                                                                               #
################################################################################################################################# 

nrow(train_set)
train_set %>% group_by(Category) %>% summarize(Count=n()) %>% kable()

train_set %>% ggplot(aes(Category)) + geom_histogram(aes(fill=..count..),stat = "count") + geom_text(stat='count', aes(label=..count..,col=I("white")), position = position_stack(vjust = .5),size=4) +
  labs(
    title = "Data Analysis",
    subtitle = "train set",
    x = "Category",
    y = "Count") +
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  ) 


################################################################################################################################# 
#                                                                                                                               #
#              Data check: test set, Category      #
#                                                                                                                               #
################################################################################################################################# 

#
#
nrow(test_set)
test_set %>% group_by(Category) %>% summarize(Count=n()) %>% kable()

test_set %>% ggplot(aes(Category)) + geom_histogram(aes(fill=..count..),stat = "count") + geom_text(stat='count', aes(label=..count..,col=I("white")), position = position_stack(vjust = .5),size=4) +
  labs(
    title = "Data Analysis",
    subtitle = "test set",
    x = "Category",
    y = "Count") +
  theme(
    plot.title = element_text(color = I("dark blue"), size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5)
  ) 

################################################################################################################################# 
#                                                                                                                               #
# Applying the various models learned in the course as well as some additional learning on a 
# few interesting methods
# we will be applying 7 models
#
################################################################################################################################# 


models <- c("lda", "naive_bayes", "knn", "multinom", "rf","cforest","nnet")

################################################################################################################################# 
#                                                                                                                               #
# selecting only the predictors and dropping the patient attributes
#
################################################################################################################################# 

train_x <-train_set[-(1:4)]
train_y<-train_set$Category


################################################################################################################################# 
#                                                                                                                               #
# training the various models via lapply 
#
################################################################################################################################# 

fits <- lapply(models, function(model){ 
  print(model)
  train(train_x,train_y,method = model)
}) 

names(fits) <- models

################################################################################################################################# 
#                                                                                                                               #
# predicting the test_set using the trained train_set 
#
################################################################################################################################# 

pred <- sapply(fits, function(object){
  predict(object, newdata = test_set)
})


################################################################################################################################# 
#                                                                                                                               #
# check to ascertain no NAs in the predicted values 
#
################################################################################################################################# 

sum(is.na(pred))

################################################################################################################################# 
#                                                                                                                               #
# Storing the Accuracy of the models as a list 
#
################################################################################################################################# 

n<-seq(1,ncol(pred),1)
Accuracy <- sapply(n,function(i)
  mean(pred[,i]==test_set$Category))
  
names(Accuracy) <- models  

################################################################################################################################# 
#                                                                                                                               #
# Accuracy of the various models
#
################################################################################################################################# 

Accuracy
Accuracy %>% kable(caption="Accuracy of the various models",col.names = c('Accuracy'))

################################################################################################################################# 
#                                                                                                                               #
# Mean of the Accuracy of the various models trained above
#
################################################################################################################################# 

mean(Accuracy)  


################################################################################################################################# 
#                                                                                                                               #
# Using Ensemble to leverage the models trained above to improve predictions
# method is voting, based on 
#
################################################################################################################################# 


detect <- rowMeans((pred == "3=Cirrhosis") | (pred == "2=Fibrosis") | (pred == "1=Hepatitis"))
detect

################################################################################################################################# 
#                                                                                                                               #
# predicting Hepatitis C (1,2,3) using voting 
#
################################################################################################################################# 

pred_classify <- ifelse(detect > .5, "P", "N") #based on a detect by majority of the methods
pred_classify

################################################################################################################################# 
#                                                                                                                               #
# classifying Hepatitis C (1,2,3) based on the data 
#
################################################################################################################################# 

test_classify <- ifelse(test_set$Category %in% c("3=Cirrhosis","2=Fibrosis","1=Hepatitis"),"P","N")
test_classify

################################################################################################################################# 
#                                                                                                                               #
# Reporting Accuracy of the Ensemble(Improvement in accuracy) 
#
################################################################################################################################# 

Accuracy_Ens <-mean(pred_classify==test_classify) 
Accuracy_Ens #.966 for >.50, 


Accuracy_Ens %>% kable(caption="Accuracy of the Ensemble",col.names = "Accuracy")


################################################################################################################################# 
#                                                                                                                               #
# End-of-Code
#
################################################################################################################################# 
