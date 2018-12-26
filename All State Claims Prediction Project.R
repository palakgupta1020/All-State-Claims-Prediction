
setwd("C:/Users/aparn/OneDrive/Documents/MSBA- FALL18/IDS 575 STAT MODELS N METHODS/PROJECT")
library(dplyr)
library('corrplot')
library('car')
library('leaps')
library('randomForest')
library('gbm')
library('stats')
#install.packages("parallel")
library(parallel)


## Import Data
dataset <- read.csv("train.csv", header = TRUE, stringsAsFactors = TRUE)
### Exclude column ID for dataset
dataset_v1 <- dataset[ , -c(1)]

#checking the distribution of continuous variables and plotting them
hist(dataset_v1$cont1, col = rainbow(50))
hist(dataset_v1$cont2, col = rainbow(50))
hist(dataset_v1$cont3, col = rainbow(50))
hist(dataset_v1$cont4, col = rainbow(50))
hist(dataset_v1$cont5, col = rainbow(50))
hist(dataset_v1$cont6, col = rainbow(50))#skewed
hist(dataset_v1$cont7, col = rainbow(50)) #skewed
hist(dataset_v1$cont8, col = rainbow(50)) #skewed
hist(dataset_v1$cont9, col = rainbow(50)) #skewed
hist(dataset_v1$cont10, col = rainbow(50))
hist(dataset_v1$cont11, col = rainbow(50))
hist(dataset_v1$cont12, col = rainbow(50))
hist(dataset_v1$cont13, col = rainbow(50))#skewed
hist(dataset_v1$cont14, col = rainbow(50))


### Finding the correlation between continuous variables
attach(dataset_v1)
dataset_v1_cont = dataset_v1[ , 117:130]
corr <- cor(dataset_v1_cont)
corrplot(corr,method = 'square',title = 'Correlation Matrix' )

#### Finding levels within each categorical variable
lev = lapply(dataset_v1[ , 1:116], function (i) nlevels(i)) #checking the levels within categorical variables
lev
high_lev = lev[lev[]>32] #checking the variables which have more than 32 levels and plotting them

#plotting the categorical variables with extremely high number of levels 
ggplot(data.frame(dataset_v1$cat109), aes(x=dataset_v1$cat109)) + geom_bar(col = "red")
ggplot(data.frame(dataset_v1$cat110), aes(x=dataset_v1$cat110)) + geom_bar(col = "red")
ggplot(data.frame(dataset_v1$cat111), aes(x=dataset_v1$cat111)) + geom_bar(col = "red")
ggplot(data.frame(dataset_v1$cat112), aes(x=dataset_v1$cat112)) + geom_bar(col = "red")
ggplot(data.frame(dataset_v1$cat113), aes(x=dataset_v1$cat113)) + geom_bar(col = "red")
ggplot(data.frame(dataset_v1$cat116), aes(x=dataset_v1$cat116)) + geom_bar(col = "red")

################################################################################################################
############################## Data Treatment for categorical variables#########################################
################################################################################################################

col_bc <- c(74:77)
bc_comb <- function(x){
  x <- as.factor(ifelse(x %in% c("B","C"), "BC","A"))
}

dataset_v1[col_bc] = lapply(dataset_v1[col_bc], bc_comb)

dataset_v1[,86] <- as.factor(ifelse(dataset_v1[,86] %in% c("A","C","D"),"ACD","B"))
dataset_v1[,89] <- as.factor(ifelse(dataset_v1[,89] %in% c("B","C","D"),"BCD","A"))
dataset_v1[,91] <- as.factor(ifelse(dataset_v1[,91] %in% c("B","C","D","E","F","G"),"BCDEFG","A"))
dataset_v1[,93] <- as.factor(ifelse(dataset_v1[,93] %in% c("B","C","D","F","I"),"BCDFI",ifelse(dataset_v1[,93] %in% c("A"),"A","E")))
dataset_v1[,98] <- as.factor(ifelse(dataset_v1[,98] %in% c("B","D","F"),"BDF",
                                    ifelse(dataset_v1[,98] %in% c("A"),"A",
                                           ifelse(dataset_v1[,98] %in% c("C"),"C",
                                                  ifelse(dataset_v1[,98] %in% c("E"),"E","G")))))

### cat 89
lev_89 <- as.data.frame(table(dataset_v1$cat89)/length(dataset_v1$cat89))
lev_89$flag[lev_89$Freq < 0.01] <- 1
dataset_v1[,89] <- as.factor(ifelse(dataset_v1[,89] %in% c("B","C","D","F","I"),"BCDFI",ifelse(dataset_v1[,93] %in% c("A"),"A","B")))

### cat 91
lev_91 <- as.data.frame(table(dataset_v1$cat91)/length(dataset_v1$cat91))
lev_91$flag[lev_91$Freq < 0.01] <- 1
dataset_v1[,91] <- as.factor(ifelse(dataset_v1[,91] %in% c("D","E","F","H"),"DEFH",
                                    ifelse(dataset_v1[,93] %in% c("A"),"A",
                                           ifelse(dataset_v1[,93] %in% c("B"),"B",
                                                  ifelse(dataset_v1[,93] %in% c("C"),"C","G")))))


### cat 96
lev_96 <- as.data.frame(table(dataset_v1$cat96)/length(dataset_v1$cat96))
lev_96$flag[lev_96$Freq < 0.01] <- 1
dataset_v1[,96] <- as.factor(ifelse(dataset_v1[,96] %in% c("A","C","F","I"),"ACFI",
                                    ifelse(dataset_v1[,96] %in% c("B"),"B",
                                           ifelse(dataset_v1[,96] %in% c("D"),"D",
                                                  ifelse(dataset_v1[,96] %in% c("E"),"E","I")))))
table(dataset_v1$cat96)

## cat 102
lev_102 <- as.data.frame(table(dataset_v1$cat102)/length(dataset_v1$cat102))
lev_102$flag[lev_102$Freq < 0.01] <- 1
var <- lev_102$Var1[which(lev_102$flag == 1)]
dataset_v1[,102] <- as.factor(ifelse(dataset_v1[,102] %in% var,"Merge",ifelse(dataset_v1[,102] %in% c("A"),"A",
                                                                              ifelse(dataset_v1[,102] %in% c("B"),"B","C"))))
table(dataset_v1[,102])

## cat 108
lev_108 <- as.data.frame(table(dataset_v1$cat108)/length(dataset_v1$cat102))
lev_108$flag[lev_108$Freq < 0.01] <- 1
var <- lev_108$Var1[which(lev_108$flag == 1)]
dataset_v1[,108] <- as.factor(ifelse(dataset_v1[,108] %in% var,"Merge",ifelse(dataset_v1[,108] %in% c("D"),"D",
                                                                              ifelse(dataset_v1[,108] %in% c("E"),"E",
                                                                                     ifelse(dataset_v1[,108] %in% c("F"),"F",
                                                                                            ifelse(dataset_v1[,108] %in% c("G"),"G",
                                                                                                   ifelse(dataset_v1[,108] %in% c("H"),"H",
                                                                                                          ifelse(dataset_v1[,108] %in% c("I"),"I",
                                                                                                                 ifelse(dataset_v1[,108] %in% c("J"),"J",
                                                                                                                        ifelse(dataset_v1[,108] %in% c("K"),"K","L"))))))))))
table(dataset_v1[,108])

### Cat 103
lev_103 <- as.data.frame(table(dataset_v1$cat103)/length(dataset_v1$cat102))
lev_103$flag[lev_103$Freq < 0.01] <- 1
var <- lev_103$Var1[which(lev_103$flag == 1)]
dataset_v1[,103] <- as.factor(ifelse(dataset_v1[,103] %in% var,"Merge",ifelse(dataset_v1[,103] %in% c("A"),"A",
                                                                              ifelse(dataset_v1[,103] %in% c("B"),"B",
                                                                                     ifelse(dataset_v1[,103] %in% c("C"),"C",
                                                                                            ifelse(dataset_v1[,103] %in% c("D"),"D","E"))))))
table(dataset_v1[,103])

### Cat 100
lev_100 <- as.data.frame(table(dataset_v1$cat100)/length(dataset_v1$cat100))
lev_100$flag[lev_100$Freq < 0.01] <- 1
var <- lev_100$Var1[which(lev_100$flag == 1)]
dataset_v1[,100] <- as.factor(ifelse(dataset_v1[,100] %in% var,"Merge",ifelse(dataset_v1[,100] %in% c("A"),"A",
                                                                              ifelse(dataset_v1[,100] %in% c("B"),"B",
                                                                                     ifelse(dataset_v1[,100] %in% c("F"),"F",
                                                                                            ifelse(dataset_v1[,100] %in% c("G"),"G",
                                                                                                   ifelse(dataset_v1[,100] %in% c("H"),"H",
                                                                                                          ifelse(dataset_v1[,100] %in% c("I"),"I",
                                                                                                                 ifelse(dataset_v1[,100] %in% c("J"),"J",
                                                                                                                        ifelse(dataset_v1[,100] %in% c("K"),"K",
                                                                                                                               ifelse(dataset_v1[,100] %in% c("L"),"L",
                                                                                                                                      ifelse(dataset_v1[,100] %in% c("M"),"M",
                                                                                                                                             ifelse(dataset_v1[,100] %in% c("N"),"N","O")))))))))))))

### Cat 99
lev_99 <- as.data.frame(table(dataset_v1$cat99)/length(dataset_v1$cat99))
lev_99$flag[lev_99$Freq < 0.01] <- 1
var <- lev_99$Var1[which(lev_99$flag == 1)]
dataset_v1[,99] <- as.factor(ifelse(dataset_v1[,99] %in% var,"Merge",ifelse(dataset_v1[,99] %in% c("D"),"D",
                                                                            ifelse(dataset_v1[,99] %in% c("F"),"F",
                                                                                   ifelse(dataset_v1[,99] %in% c("K"),"K",
                                                                                          ifelse(dataset_v1[,99] %in% c("N"),"N",
                                                                                                 ifelse(dataset_v1[,99] %in% c("P"),"P",
                                                                                                        ifelse(dataset_v1[,99] %in% c("R"),"R",
                                                                                                               ifelse(dataset_v1[,99] %in% c("S"),"S","T")))))))))

### Cat111
lev_111 <- as.data.frame(table(dataset_v1$cat111)/length(dataset_v1$cat111))
lev_111$flag[lev_111$Freq < 0.01] <- 1
var <- lev_111$Var1[which(lev_111$flag == 1)]
dataset_v1[,111] <- as.factor(ifelse(dataset_v1[,111] %in% var,"Merge",ifelse(dataset_v1[,111] %in% c("A"),"A",
                                                                              ifelse(dataset_v1[,111] %in% c("C"),"C",
                                                                                     ifelse(dataset_v1[,111] %in% c("E"),"E",
                                                                                            ifelse(dataset_v1[,111] %in% c("G"),"G","I"))))))
### Cat104
lev_104 <- as.data.frame(table(dataset_v1$cat104)/length(dataset_v1$cat104))
lev_104$flag[lev_104$Freq < 0.01] <- 1
var <- lev_104$Var1[which(lev_104$flag == 1)]
dataset_v1[,104] <- as.factor(ifelse(dataset_v1[,104] %in% var,"Merge",ifelse(dataset_v1[,104] %in% c("C"),"C",
                                                                              ifelse(dataset_v1[,104] %in% c("D"),"D",
                                                                                     ifelse(dataset_v1[,104] %in% c("E"),"E",
                                                                                            ifelse(dataset_v1[,104] %in% c("F"),"F",
                                                                                                   ifelse(dataset_v1[,104] %in% c("G"),"G",
                                                                                                          ifelse(dataset_v1[,104] %in% c("H"),"H",
                                                                                                                 ifelse(dataset_v1[,104] %in% c("I"),"I",
                                                                                                                        ifelse(dataset_v1[,104] %in% c("J"),"J",
                                                                                                                               ifelse(dataset_v1[,104] %in% c("K"),"K","L")))))))))))

### Cat106
lev_106 <- as.data.frame(table(dataset_v1$cat106)/length(dataset_v1$cat106))
lev_106$flag[lev_106$Freq < 0.01] <- 1
var <- lev_106$Var1[which(lev_106$flag == 1)]
dataset_v1[,106] <- as.factor(ifelse(dataset_v1[,106] %in% var,"Merge",ifelse(dataset_v1[,106] %in% c("E"),"E",
                                                                              ifelse(dataset_v1[,106] %in% c("F"),"F",
                                                                                     ifelse(dataset_v1[,106] %in% c("G"),"G",
                                                                                            ifelse(dataset_v1[,106] %in% c("H"),"H",
                                                                                                   ifelse(dataset_v1[,106] %in% c("I"),"I",
                                                                                                          ifelse(dataset_v1[,106] %in% c("J"),"J",
                                                                                                                 ifelse(dataset_v1[,106] %in% c("K"),"K","L")))))))))
table(dataset_v1[,106])

### Cat101
lev_101 <- as.data.frame(table(dataset_v1$cat101)/length(dataset_v1$cat101))
lev_101$flag[lev_101$Freq < 0.01] <- 1
var <- lev_101$Var1[which(lev_101$flag == 1)]
dataset_v1[,101] <- as.factor(ifelse(dataset_v1[,101] %in% var,"Merge",ifelse(dataset_v1[,101] %in% c("A"),"A",
                                                                              ifelse(dataset_v1[,101] %in% c("C"),"C",
                                                                                     ifelse(dataset_v1[,101] %in% c("D"),"D",
                                                                                            ifelse(dataset_v1[,101] %in% c("F"),"F",
                                                                                                   ifelse(dataset_v1[,101] %in% c("G"),"G",
                                                                                                          ifelse(dataset_v1[,101] %in% c("I"),"I",
                                                                                                                 ifelse(dataset_v1[,101] %in% c("J"),"J",
                                                                                                                        ifelse(dataset_v1[,101] %in% c("L"),"L",
                                                                                                                               ifelse(dataset_v1[,101] %in% c("M"),"M",
                                                                                                                                      ifelse(dataset_v1[,101] %in% c("O"),"O","Q"))))))))))))


### Cat114
lev_114 <- as.data.frame(table(dataset_v1$cat114)/length(dataset_v1$cat114))
lev_114$flag[lev_114$Freq < 0.01] <- 1
var <- lev_114$Var1[which(lev_114$flag == 1)]
dataset_v1[,114] <- as.factor(ifelse(dataset_v1[,114] %in% var,"Merge",ifelse(dataset_v1[,114] %in% c("A"),"A",
                                                                              ifelse(dataset_v1[,114] %in% c("C"),"C",
                                                                                     ifelse(dataset_v1[,114] %in% c("E"),"E",
                                                                                            ifelse(dataset_v1[,114] %in% c("F"),"F",
                                                                                                   ifelse(dataset_v1[,114] %in% c("I"),"I",
                                                                                                          ifelse(dataset_v1[,114] %in% c("J"),"J","N"))))))))
### Cat105
lev_105 <- as.data.frame(table(dataset_v1$cat105)/length(dataset_v1$cat105))
lev_105$flag[lev_105$Freq < 0.01] <- 1
var <- lev_105$Var1[which(lev_105$flag == 1)]
dataset_v1[,105] <- as.factor(ifelse(dataset_v1[,105] %in% var,"Merge",ifelse(dataset_v1[,105] %in% c("D"),"D",
                                                                              ifelse(dataset_v1[,105] %in% c("E"),"E",
                                                                                     ifelse(dataset_v1[,105] %in% c("F"),"F",
                                                                                            ifelse(dataset_v1[,105] %in% c("G"),"G",
                                                                                                   ifelse(dataset_v1[,105] %in% c("H"),"H","I")))))))

### Cat107
lev_107 <- as.data.frame(table(dataset_v1$cat107)/length(dataset_v1$cat107))
lev_107$flag[lev_107$Freq < 0.01] <- 1
var <- lev_107$Var1[which(lev_107$flag == 1)]
dataset_v1[,107] <- as.factor(ifelse(dataset_v1[,107] %in% var,"Merge",ifelse(dataset_v1[,107] %in% c("D"),"D",
                                                                              ifelse(dataset_v1[,107] %in% c("E"),"E",
                                                                                     ifelse(dataset_v1[,107] %in% c("F"),"F",
                                                                                            ifelse(dataset_v1[,107] %in% c("G"),"G",
                                                                                                   ifelse(dataset_v1[,107] %in% c("H"),"H",
                                                                                                          ifelse(dataset_v1[,107] %in% c("I"),"I",
                                                                                                                 ifelse(dataset_v1[,107] %in% c("J"),"J",
                                                                                                                        ifelse(dataset_v1[,107] %in% c("K"),"K",
                                                                                                                               ifelse(dataset_v1[,107] %in% c("L"),"L","M")))))))))))
table(dataset_v1[,107])

### Cat115
lev_115 <- as.data.frame(table(dataset_v1$cat115)/length(dataset_v1$cat115))
lev_115$flag[lev_115$Freq < 0.01] <- 1
var <- lev_115$Var1[which(lev_115$flag == 1)]
dataset_v1[,115] <- as.factor(ifelse(dataset_v1[,115] %in% var,"Merge",ifelse(dataset_v1[,115] %in% c("H"),"H",
                                                                              ifelse(dataset_v1[,115] %in% c("I"),"I",
                                                                                     ifelse(dataset_v1[,115] %in% c("J"),"J",
                                                                                            ifelse(dataset_v1[,115] %in% c("K"),"K",
                                                                                                   ifelse(dataset_v1[,115] %in% c("L"),"L",
                                                                                                          ifelse(dataset_v1[,115] %in% c("M"),"M",
                                                                                                                 ifelse(dataset_v1[,115] %in% c("N"),"N",
                                                                                                                        ifelse(dataset_v1[,115] %in% c("O"),"O",
                                                                                                                               ifelse(dataset_v1[,115] %in% c("P"),"P",
                                                                                                                                      ifelse(dataset_v1[,115] %in% c("Q"),"Q","R"))))))))))))

### Cat112
lev_112 <- as.data.frame(table(dataset_v1$cat112)/length(dataset_v1$cat112))
lev_112$flag[lev_112$Freq < 0.01] <- 1
var <- lev_112$Var1[which(lev_112$flag == 1)]

dataset_v1[,112] <- as.factor(ifelse(dataset_v1[,112] %in% var,"Merge",ifelse(dataset_v1[,112] %in% c("A"),"A",
                                                                              ifelse(dataset_v1[,112] %in% c("AF"),"AF",
                                                                                     ifelse(dataset_v1[,112] %in% c("AH"),"AH",
                                                                                            ifelse(dataset_v1[,112] %in% c("AI"),"AI",
                                                                                                   ifelse(dataset_v1[,112] %in% c("AK"),"AK",
                                                                                                          ifelse(dataset_v1[,112] %in% c("AN"),"AN",
                                                                                                                 ifelse(dataset_v1[,112] %in% c("AP"),"AP",
                                                                                                                        ifelse(dataset_v1[,112] %in% c("AR"),"AR",
                                                                                                                               ifelse(dataset_v1[,112] %in% c("AS"),"AS",
                                                                                                                                      ifelse(dataset_v1[,112] %in% c("AV"),"AV",
                                                                                                                                             ifelse(dataset_v1[,112] %in% c("AW"),"AW",
                                                                                                                                                    ifelse(dataset_v1[,112] %in% c("C"),"C",
                                                                                                                                                           ifelse(dataset_v1[,112] %in% c("E"),"E",
                                                                                                                                                                  ifelse(dataset_v1[,112] %in% c("F"),"F",
                                                                                                                                                                         ifelse(dataset_v1[,112] %in% c("G"),"G",
                                                                                                                                                                                ifelse(dataset_v1[,112] %in% c("J"),"J",
                                                                                                                                                                                       ifelse(dataset_v1[,112] %in% c("K"),"K",
                                                                                                                                                                                              ifelse(dataset_v1[,112] %in% c("N"),"N",
                                                                                                                                                                                                     ifelse(dataset_v1[,112] %in% c("O"),"O",
                                                                                                                                                                                                            ifelse(dataset_v1[,112] %in% c("S"),"S", "U"))))))))))))))))))))))

table(dataset_v1[,112])

### cat 113
lev_113 <- as.data.frame(table(dataset_v1$cat113)/length(dataset_v1$cat113))
lev_113$flag[lev_113$Freq < 0.01] <- 1
var <- lev_113$Var1[which(lev_113$flag == 1)]


dataset_v1[,113] <- as.factor(ifelse(dataset_v1[,113] %in% var,"Merge",ifelse(dataset_v1[,113] %in% c("A"),"A",
                                                                              ifelse(dataset_v1[,113] %in% c("AD"),"AD",
                                                                                     ifelse(dataset_v1[,113] %in% c("AE"),"AE",
                                                                                            ifelse(dataset_v1[,113] %in% c("AF"),"AF",
                                                                                                   ifelse(dataset_v1[,113] %in% c("AG"),"AG",
                                                                                                          ifelse(dataset_v1[,113] %in% c("AJ"),"AJ",
                                                                                                                 ifelse(dataset_v1[,113] %in% c("AK"),"AK",
                                                                                                                        ifelse(dataset_v1[,113] %in% c("AN"),"AN",
                                                                                                                               ifelse(dataset_v1[,113] %in% c("AS"),"AS",
                                                                                                                                      ifelse(dataset_v1[,113] %in% c("AT"),"AT",
                                                                                                                                             ifelse(dataset_v1[,113] %in% c("AX"),"AX",
                                                                                                                                                    ifelse(dataset_v1[,113] %in% c("BC"),"BC",
                                                                                                                                                           ifelse(dataset_v1[,113] %in% c("BJ"),"BJ",
                                                                                                                                                                  ifelse(dataset_v1[,113] %in% c("BM"),"BM",
                                                                                                                                                                         ifelse(dataset_v1[,113] %in% c("BN"),"BN",
                                                                                                                                                                                ifelse(dataset_v1[,113] %in% c("H"),"H",
                                                                                                                                                                                       ifelse(dataset_v1[,113] %in% c("I"),"I",
                                                                                                                                                                                              ifelse(dataset_v1[,113] %in% c("J"),"J",
                                                                                                                                                                                                     ifelse(dataset_v1[,113] %in% c("K"),"K",
                                                                                                                                                                                                            ifelse(dataset_v1[,113] %in% c("L"),"L",
                                                                                                                                                                                                                   ifelse(dataset_v1[,113] %in% c("M"),"M",
                                                                                                                                                                                                                          ifelse(dataset_v1[,113] %in% c("N"),"N",
                                                                                                                                                                                                                                 ifelse(dataset_v1[,113] %in% c("Q"),"Q",
                                                                                                                                                                                                                                        ifelse(dataset_v1[,113] %in% c("S"),"S",
                                                                                                                                                                                                                                               ifelse(dataset_v1[,113] %in% c("X"),"X","Y")))))))))))))))))))))))))))


### Cat 109
lev_109 <- as.data.frame(table(dataset_v1$cat109)/length(dataset_v1$cat109))
lev_109$flag[lev_109$Freq < 0.01] <- 1
var <- lev_109$Var1[which(lev_109$flag == 1)]

dataset_v1[,109] <- as.factor(ifelse(dataset_v1[,109] %in% var,"Merge",ifelse(dataset_v1[,109] %in% c("AB"),"AB",
                                                                              ifelse(dataset_v1[,109] %in% c("BI"),"BI",
                                                                                     ifelse(dataset_v1[,109] %in% c("BU"),"BU","K")))))

### cat 110
lev_110 <- as.data.frame(table(dataset_v1$cat110)/length(dataset_v1$cat110))
lev_110$flag[lev_110$Freq < 0.01] <- 1
var <- lev_110$Var1[which(lev_110$flag == 1)]



dataset_v1[,110] <- as.factor(ifelse(dataset_v1[,110] %in% var,"Merge",ifelse(dataset_v1[,110] %in% c("AC"),"AC",
                                                                              ifelse(dataset_v1[,110] %in% c("AI"),"AI",
                                                                                     ifelse(dataset_v1[,110] %in% c("BC"),"BC",
                                                                                            ifelse(dataset_v1[,110] %in% c("BP"),"BP",
                                                                                                   ifelse(dataset_v1[,110] %in% c("BS"),"BS",
                                                                                                          ifelse(dataset_v1[,110] %in% c("BT"),"BT",
                                                                                                                 ifelse(dataset_v1[,110] %in% c("C"),"C",
                                                                                                                        ifelse(dataset_v1[,110] %in% c("CL"),"CL",
                                                                                                                               ifelse(dataset_v1[,110] %in% c("CM"),"CM",
                                                                                                                                      ifelse(dataset_v1[,110] %in% c("CO"),"CO",
                                                                                                                                             ifelse(dataset_v1[,110] %in% c("CQ"),"CQ",
                                                                                                                                                    ifelse(dataset_v1[,110] %in% c("CS"),"CS",
                                                                                                                                                           ifelse(dataset_v1[,110] %in% c("DW"),"DW",
                                                                                                                                                                  ifelse(dataset_v1[,110] %in% c("DX"),"DX",
                                                                                                                                                                         ifelse(dataset_v1[,110] %in% c("EB"),"EB",
                                                                                                                                                                                ifelse(dataset_v1[,110] %in% c("EG"),"EG",
                                                                                                                                                                                       ifelse(dataset_v1[,110] %in% c("EL"),"EL","W")))))))))))))))))))

table(dataset_v1[,110])

### Cat 116
lev_116 <- as.data.frame(table(dataset_v1$cat116)/length(dataset_v1$cat116))
lev_116$flag[lev_116$Freq < 0.01] <- 1
var <- lev_116$Var1[which(lev_116$flag == 1)]

dataset_v1[,116] <- as.factor(ifelse(dataset_v1[,116] %in% var,"Merge",ifelse(dataset_v1[,116] %in% c("CB"),"CB",
                                                                              ifelse(dataset_v1[,116] %in% c("CK"),"CK",
                                                                                     ifelse(dataset_v1[,116] %in% c("CR"),"CR",
                                                                                            ifelse(dataset_v1[,116] %in% c("DC"),"DC",
                                                                                                   ifelse(dataset_v1[,116] %in% c("DF"),"DF",
                                                                                                          ifelse(dataset_v1[,116] %in% c("DJ"),"DJ",
                                                                                                                 ifelse(dataset_v1[,116] %in% c("DP"),"DP",
                                                                                                                        ifelse(dataset_v1[,116] %in% c("GC"),"GC",
                                                                                                                               ifelse(dataset_v1[,116] %in% c("GK"),"GK",
                                                                                                                                      ifelse(dataset_v1[,116] %in% c("GS"),"GS",
                                                                                                                                             ifelse(dataset_v1[,116] %in% c("HB"),"HB",
                                                                                                                                                    ifelse(dataset_v1[,116] %in% c("HG"),"HG",
                                                                                                                                                           ifelse(dataset_v1[,116] %in% c("HJ"),"HJ",
                                                                                                                                                                  ifelse(dataset_v1[,116] %in% c("HK"),"HK",
                                                                                                                                                                         ifelse(dataset_v1[,116] %in% c("HQ"),"HQ",
                                                                                                                                                                                ifelse(dataset_v1[,116] %in% c("HV"),"HV",
                                                                                                                                                                                       ifelse(dataset_v1[,116] %in% c("HX"),"HX",
                                                                                                                                                                                              ifelse(dataset_v1[,116] %in% c("IE"),"IE",
                                                                                                                                                                                                     ifelse(dataset_v1[,116] %in% c("IG"),"IG",
                                                                                                                                                                                                            ifelse(dataset_v1[,116] %in% c("KW"),"KW",
                                                                                                                                                                                                                   ifelse(dataset_v1[,116] %in% c("LB"),"LB",
                                                                                                                                                                                                                          ifelse(dataset_v1[,116] %in% c("LM"),"LM",
                                                                                                                                                                                                                                 ifelse(dataset_v1[,116] %in% c("LN"),"LN",
                                                                                                                                                                                                                                        ifelse(dataset_v1[,116] %in% c("LO"),"LO","MD"))))))))))))))))))))))))))

################################################################################################################################################
############################################linear regression ##################################################################
#running a linear regression on the treated set of 131 variables to check for multicollinearity within the categorical variables
#################################################################################################################################

set.seed(575); lr_1 <- lm(loss~., data=dataset_v1)
summary(lr_1)
temp <- as.data.frame(lr_1$coefficients)

############################ based on the results of lm, following treatments are done##################################
##########################on few more categorical variables whose beta coefficients are NA ##############################

table(dataset_v1$cat81)
dataset_v1[,81] <- as.factor(ifelse(dataset_v1[,81] %in% c("A","D"),"Merge",ifelse(dataset_v1[,81] %in% c("B"),"B","C")))

table(dataset_v1$cat85)
dataset_v1[,85] <- as.factor(ifelse(dataset_v1[,85] %in% c("A","D"),"Merge",ifelse(dataset_v1[,85] %in% c("B"),"B","C")))

table(dataset_v1$cat87)
dataset_v1[,87] <- as.factor(ifelse(dataset_v1[,87] %in% c("A","D"),"Merge",ifelse(dataset_v1[,87] %in% c("B"),"B","C")))

table(dataset_v1$cat90)
dataset_v1[,90] <- as.factor(ifelse(dataset_v1[,90] %in% c("A","F","G"),"Merge",ifelse(dataset_v1[,90] %in% c("B"),"B",
                                                                                       ifelse(dataset_v1[,90] %in% c("C"),"C",
                                                                                              ifelse(dataset_v1[,90] %in% c("D"),"D","E")))))


table(dataset_v1$cat92)
dataset_v1[,92] <- as.factor(ifelse(dataset_v1[,92] %in% c("A","I"),"Merge",ifelse(dataset_v1[,92] %in% c("B"),"B",
                                                                                   ifelse(dataset_v1[,92] %in% c("C"),"C",
                                                                                          ifelse(dataset_v1[,92] %in% c("D"),"D",
                                                                                                 ifelse(dataset_v1[,92] %in% c("F"),"F","H"))))))


table(dataset_v1$cat93)
dataset_v1[,93] <- as.factor(ifelse(dataset_v1[,93] %in% c("A","E"),"Merge","BSCDFI"))


table(dataset_v1$cat101)
dataset_v1[,101] <- as.factor(ifelse(dataset_v1[,101] %in% c("A","Q"),"Merge_1",ifelse(dataset_v1[,101] %in% c("C"),"C",
                                                                                       ifelse(dataset_v1[,101] %in% c("D"),"D",
                                                                                              ifelse(dataset_v1[,101] %in% c("F"),"F",
                                                                                                     ifelse(dataset_v1[,101] %in% c("G"),"G",
                                                                                                            ifelse(dataset_v1[,101] %in% c("I"),"I",
                                                                                                                   ifelse(dataset_v1[,101] %in% c("J"),"J",
                                                                                                                          ifelse(dataset_v1[,101] %in% c("L"),"L",
                                                                                                                                 ifelse(dataset_v1[,101] %in% c("M"),"M",
                                                                                                                                        ifelse(dataset_v1[,101] %in% c("Merge"),"Merge","O")))))))))))


table(dataset_v1$cat102)
dataset_v1[,102] <- as.factor(ifelse(dataset_v1[,102] %in% c("A","Merge"),"Merge",ifelse(dataset_v1[,102] %in% c("B"),"B","C")))


table(dataset_v1$cat103)
dataset_v1[,103] <- as.factor(ifelse(dataset_v1[,103] %in% c("A","Merge"),"Merge",ifelse(dataset_v1[,103] %in% c("B"),"B",
                                                                                         ifelse(dataset_v1[,103] %in% c("C"),"C",
                                                                                                ifelse(dataset_v1[,103] %in% c("D"),"D","E")))))

table(dataset_v1$cat108)
dataset_v1[,108] <- as.factor(ifelse(dataset_v1[,108] %in% c("D","F"),"Merge_1",ifelse(dataset_v1[,108] %in% c("E"),"E",
                                                                                       ifelse(dataset_v1[,108] %in% c("G"),"G",
                                                                                              ifelse(dataset_v1[,108] %in% c("H"),"H",
                                                                                                     ifelse(dataset_v1[,108] %in% c("I"),"I",
                                                                                                            ifelse(dataset_v1[,108] %in% c("K"),"K",
                                                                                                                   ifelse(dataset_v1[,108] %in% c("L"),"L","Merge"))))))))

table(dataset_v1$cat111)
dataset_v1[,111] <- as.factor(ifelse(dataset_v1[,111] %in% c("A","Merge"),"Merge",ifelse(dataset_v1[,111] %in% c("C"),"C",
                                                                                         ifelse(dataset_v1[,111] %in% c("E"),"E",
                                                                                                ifelse(dataset_v1[,111] %in% c("G"),"G","I")))))


table(dataset_v1$cat114)
dataset_v1[,114] <- as.factor(ifelse(dataset_v1[,114] %in% c("A","N"),"Merge_1",ifelse(dataset_v1[,114] %in% c("C"),"C",
                                                                                       ifelse(dataset_v1[,114] %in% c("E"),"E",
                                                                                              ifelse(dataset_v1[,114] %in% c("F"),"F",
                                                                                                     ifelse(dataset_v1[,114] %in% c("I"),"I",
                                                                                                            ifelse(dataset_v1[,114] %in% c("J"),"J","Merge")))))))


################################################################################################################################################################
############################ TREATMENT FOR THE RESPONSE VARIABLE WHICH IS HIGHLY RIGHT SKEWED ###################################

quantile(dataset_v1$loss, prob = seq(0, 1, length = 21), type = 5)
hist(dataset_v1$loss, col = rainbow(50), xlim = c(1,30000))
dataset_v1$loss <- ifelse(dataset_v1$loss<=617.028, 617.028, ifelse(dataset_v1$loss>=8508.676, 8508.676, dataset_v1$loss))
hist(dataset_v1$loss, col = rainbow(50), xlim = c(1,10000))
write.csv(dataset_v1, "FINAL_dATA_131.csv") 

############################################### one hot encoding (JUST TRIAL) ###########################################

install.packages("onehot")
library('onehot')
encoded_data = onehot(dataset_v1[ ,1:116], stringsAsFactors = TRUE, addNA = FALSE, max_levels = 27)
temp_1 = predict(encoded_data,dataset_v1[ ,1:116])
temp_2 = as.data.frame(temp_1)
temp_2[] = lapply(temp_2[], factor)
final = cbind(temp_2[ , ] , dataset_v1[ , 117:131])
write.csv(final, "FINAL_ONE_HOT.csv")   ##saving the encoded dataset, with 448 categorical and 14 continuous variables + loss

#########################################################################################################################
############################################## GRADIENT BOOSTING MACHINE MODELS ###################################################################
##########################################################################################################################

detectCores()
#final = read.csv("FINAL_dATA_131.csv")
final = dataset_v1

nr=nrow(final)
set.seed(12345); trnIndex = sample(1:nr, size = round(.6*nr), replace=FALSE)
data_final_trn = final[trnIndex,]   #training data with the randomly selected row-indices
data_final_tst = final[-trnIndex,]  #test data with the other row-indices

train_RMSE_ntree = c()
test_RMSE_ntree= c()
train_RMSE_shrink = c()
test_RMSE_shrink = c()

train_MSE_ntree = c()
test_MSE_ntree = c()
train_MSE_shrink = c()
test_MSE_shrink = c()

#fixing the shrinkage to 0.1 and increasing the number of trees from 500 0 5000
# Gradient boosting with 500 trees and measuring the performance
set.seed(575)
gbm_d_1=gbm(data_final_trn$loss ~ ., data = data_final_trn,
            distribution = "gaussian",
            n.trees = 500,
            shrinkage = .1,
            interaction.depth = 1,
            cv.folds = 10,
            n.cores = 32)

gbm.perf(gbm_d_1)
min(gbm_d_1$cv.error)
pred_gbm_d_trn_1 <- predict(gbm_d_1,data_final_trn,n.trees = 500)
train_RMSE_ntree[1] = sqrt((mean((data_final_trn$loss - pred_gbm_d_trn_1)^2)))
train_MSE_ntree[1] = mean( abs(data_final_trn$loss - pred_gbm_d_trn_1) )

gbm.perf(gbm_d_1)
min(gbm_d_1$cv.error)
pred_gbm_d_tst_1 <- predict(gbm_d_1,data_final_tst,n.trees = 500)
test_RMSE_ntree[1] = sqrt((mean((data_final_tst$loss - pred_gbm_d_tst_1)^2)))
test_MSE_ntree[1] = mean( abs(data_final_tst$loss - pred_gbm_d_tst_1) )

# Gradient boosting with 1000 trees and measuring the performance
set.seed(575)
gbm_d_2=gbm(data_final_trn$loss ~ ., data = data_final_trn,
            distribution = "gaussian",
            n.trees = 1000,
            shrinkage = .1,
            interaction.depth = 1,
            cv.folds = 10,
            n.cores = 30)
gbm.perf(gbm_d_2)
min(gbm_d_2$cv.error)
pred_gbm_d_trn_2 <- predict(gbm_d_2,data_final_trn,n.trees = 1000)
train_RMSE_ntree[2]  <- sqrt((mean((data_final_trn$loss - pred_gbm_d_trn_2)^2)))
train_MSE_ntree[2] = mean( abs(data_final_trn$loss - pred_gbm_d_trn_2) )

gbm.perf(gbm_d_2)
min(gbm_d_2$cv.error)
pred_gbm_d_tst_2 <- predict(gbm_d_2,data_final_tst,n.trees = 1000)
test_RMSE_ntree[2]  <- sqrt((mean((data_final_tst$loss - pred_gbm_d_tst_2)^2)))
test_MSE_ntree[2] = mean( abs(data_final_tst$loss - pred_gbm_d_tst_2) )


# Gradient boosting with 2000 trees and measuring the performance
set.seed(575)
gbm_d_3=gbm(data_final_trn$loss ~ ., data = data_final_trn,
            distribution = "gaussian",
            n.trees = 2000,
            shrinkage = .1,
            interaction.depth = 1,
            cv.folds = 10,
            n.cores = 30)
gbm.perf(gbm_d_3)
min(gbm_d_3$cv.error)
pred_gbm_d_trn_3 <- predict(gbm_d_3,data_final_trn,n.trees = 2000)
train_RMSE_ntree[3]  <- sqrt((mean((data_final_trn$loss - pred_gbm_d_trn_3)^2)))
train_MSE_ntree[3] = mean( abs(data_final_trn$loss - pred_gbm_d_trn_3) )

gbm.perf(gbm_d_3)
min(gbm_d_3$cv.error)
pred_gbm_d_tst_3 <- predict(gbm_d_3,data_final_tst,n.trees = 2000)
test_RMSE_ntree[3]  <- sqrt((mean((data_final_tst$loss - pred_gbm_d_tst_3)^2)))
test_MSE_ntree[3] = mean( abs(data_final_tst$loss - pred_gbm_d_tst_3) )


# Gradient boosting with 5000 trees and measuring the performance (shrinkage =0.1)
set.seed(575)
gbm_d_4=gbm(data_final_trn$loss ~ ., data = data_final_trn,
            distribution = "gaussian",
            n.trees = 5000,
            shrinkage = .1,
            interaction.depth = 1,
            cv.folds = 10,
            n.cores = 28)
gbm.perf(gbm_d_4)
min(gbm_d_4$cv.error)
pred_gbm_d_trn_4 <- predict(gbm_d_4,data_final_trn,n.trees = 5000)
train_RMSE_ntree[4]  <- sqrt((mean((data_final_trn$loss - pred_gbm_d_trn_4)^2)))
train_MSE_ntree[4] = mean( abs(data_final_trn$loss - pred_gbm_d_trn_4) )

train_RMSE_shrink[1]  <- sqrt((mean((data_final_trn$loss - pred_gbm_d_trn_4)^2)))
train_MSE_shrink[1] = mean( abs(data_final_trn$loss - pred_gbm_d_trn_4) )

gbm.perf(gbm_d_4)
min(gbm_d_4$cv.error)
pred_gbm_d_tst_4 <- predict(gbm_d_4,data_final_tst,n.trees = 5000)
test_RMSE_ntree[4]  <- sqrt((mean((data_final_tst$loss - pred_gbm_d_tst_4)^2)))
test_MSE_ntree[4] = mean( abs(data_final_tst$loss - pred_gbm_d_tst_4) )

test_RMSE_shrink[1]  <- sqrt((mean((data_final_tst$loss - pred_gbm_d_tst_4)^2)))
test_MSE_shrink[1] = mean( abs(data_final_tst$loss - pred_gbm_d_tst_4) )

# Gradient boosting with shrinkage=0.01 and measuring the performance
set.seed(575)
gbm_d_5=gbm(data_final_trn$loss ~ ., data = data_final_trn,
            distribution = "gaussian",
            n.trees = 5000,
            shrinkage = .01,
            interaction.depth = 1,
            cv.folds = 10,
            n.cores = 28)
gbm.perf(gbm_d_5)
min(gbm_d_5$cv.error)
pred_gbm_d_trn_5 <- predict(gbm_d_5,data_final_trn,n.trees = 5000)
train_RMSE_shrink[2]  <- sqrt((mean((data_final_trn$loss - pred_gbm_d_trn_5)^2)))
train_MSE_shrink[2] = mean( abs(data_final_trn$loss - pred_gbm_d_trn_5) )

gbm.perf(gbm_d_5)
min(gbm_d_4$cv.error)
pred_gbm_d_tst_5 <- predict(gbm_d_5,data_final_tst,n.trees = 5000)
test_RMSE_shrink[2]  <- sqrt((mean((data_final_tst$loss - pred_gbm_d_tst_5)^2)))
test_MSE_shrink[2] = mean( abs(data_final_tst$loss - pred_gbm_d_tst_5) )


# Gradient boosting with shrinkage=0.001 and measuring the performance
set.seed(575)
gbm_d_6=gbm(data_final_trn$loss ~ ., data = data_final_trn,
            distribution = "gaussian",
            n.trees = 5000,
            shrinkage = .001,
            interaction.depth = 1,
            cv.folds = 10,
            n.cores = 28)
gbm.perf(gbm_d_6)
min(gbm_d_6$cv.error)
pred_gbm_d_trn_6 <- predict(gbm_d_6,data_final_trn,n.trees = 5000)
train_RMSE_shrink[3]  <- sqrt((mean((data_final_trn$loss - pred_gbm_d_trn_6)^2)))
train_MSE_shrink[3] =  mean( abs(data_final_trn$loss - pred_gbm_d_trn_6) )

gbm.perf(gbm_d_6)
min(gbm_d_6$cv.error)
pred_gbm_d_tst_6 <- predict(gbm_d_6,data_final_tst,n.trees = 5000)
test_RMSE_shrink[3]  <- sqrt((mean((data_final_tst$loss - pred_gbm_d_tst_6)^2)))
test_MSE_shrink[3] = mean( abs(data_final_tst$loss - pred_gbm_d_tst_6) )

library('ggplot2')
bcd = c("500","1000","2000","5000")
train_RMSE_ntree <- c(100, 200, 300, 400)
test_RMSE_ntree <- c(150, 250, 350, 450)
abc = data.frame(bcd, train_RMSE_ntree, test_RMSE_ntree)
ggplot(abc, aes(x = bcd , y = train_RMSE_ntree)) + geom_bar(aes(colour = "red")) 

# +geom_line(aes(y=test_RMSE_ntree, colour ="green"))


################## SETTING UP THE DATA FOR XGBOOST, GLM ###########################


set.seed(12345)
nr=nrow(dataset_v1)
colnames(dataset_v1)

trnIndex = sample(1:nr, size = round(0.6*nr), replace=FALSE)
Train_all = dataset_v1[trnIndex,]
Validation_all = dataset_v1[-trnIndex,]



attach(Train_all)

xfactors_trn <- model.matrix(Train_all$loss ~ cat1+cat2+cat3+cat4+cat5+cat6+cat7+cat8+cat9+cat10+cat11+cat12+cat13+cat14+cat15+cat16+cat17+cat18+cat19+cat20+cat21+cat22+cat23+cat24+cat25+cat26+cat27+cat28+cat29+cat30+cat31+cat32+cat33+cat34+cat35+cat36+cat37+cat38+cat39+cat40+cat41+cat42+cat43+cat44+cat45+cat46+cat47+cat48+cat49+cat50+cat51+cat52+cat53+cat54+cat55+cat56+cat57+cat58+cat59+cat60+cat61+cat62+cat63+cat64+cat65+cat66+cat67+cat68+cat69+cat70+cat71+cat72+cat73+cat74+cat75+cat76+cat77+cat78+cat79+cat80+cat81+cat82+cat83+cat84+cat85+cat86+cat87+cat88+cat89+cat90+cat91+cat92+cat93+cat94+cat95+cat96+cat97+cat98+cat99+cat100+cat101+cat102+cat103+cat104+cat105+cat106+cat107+cat108+cat109+cat110+cat111+cat112+cat113+cat114+cat115+cat116
                             , data = Train_all)[ ,-1]#only categorical variables

x_trn <- as.matrix(cbind(Train_all[,117:130], xfactors_trn))
y_trn <- Train_all$loss

xfactors_tst <- model.matrix(Validation_all$loss ~ cat1+cat2+cat3+cat4+cat5+cat6+cat7+cat8+cat9+cat10+cat11+cat12+cat13+cat14+cat15+cat16+cat17+cat18+cat19+cat20+cat21+cat22+cat23+cat24+cat25+cat26+cat27+cat28+cat29+cat30+cat31+cat32+cat33+cat34+cat35+cat36+cat37+cat38+cat39+cat40+cat41+cat42+cat43+cat44+cat45+cat46+cat47+cat48+cat49+cat50+cat51+cat52+cat53+cat54+cat55+cat56+cat57+cat58+cat59+cat60+cat61+cat62+cat63+cat64+cat65+cat66+cat67+cat68+cat69+cat70+cat71+cat72+cat73+cat74+cat75+cat76+cat77+cat78+cat79+cat80+cat81+cat82+cat83+cat84+cat85+cat86+cat87+cat88+cat89+cat90+cat91+cat92+cat93+cat94+cat95+cat96+cat97+cat98+cat99+cat100+cat101+cat102+cat103+cat104+cat105+cat106+cat107+cat108+cat109+cat110+cat111+cat112+cat113+cat114+cat115+cat116
                             , data = Validation_all)[ ,-1]#only categorical variables
x_tst <- as.matrix(cbind(Validation_all[,117:130], xfactors_tst))
y_tst <- Validation_all$loss

########################################################################
##################### XGBOOST##############################
##################################################################

install.packages("xgboost")
library(xgboost)

xgb_1 = xgboost(data = x_trn , label = y_trn, nrounds = 500) #eta=0.3 by default
pred_xg_trn = predict(xgb_1, x_trn) 
mean(abs((pred_xg_trn-as.matrix(y_trn))))
sqrt(mean((pred_xg_trn-as.matrix(y_trn))^2))

pred_xg_tst = predict(xgb_1, x_tst) 
mean(abs((pred_xg_tst-as.matrix(y_tst))))
sqrt(mean((pred_xg_tst-as.matrix(y_tst))^2))

xgb_2 = xgboost(data = x_trn , label = y_trn, nrounds = 500, eta = 0.1 )
pred_xg_trn = predict(xgb_2, x_trn) 
mean(abs((pred_xg_trn-as.matrix(y_trn))))
sqrt(mean((pred_xg_trn-as.matrix(y_trn))^2))

pred_xg_tst = predict(xgb_2, x_tst) 
mean(abs((pred_xg_tst-as.matrix(y_tst))))
sqrt(mean((pred_xg_tst-as.matrix(y_tst))^2))

xgb_2 = xgboost(data = x_trn , label = y_trn, nrounds = 500, eta = 0.05 )
pred_xg_trn = predict(xgb_2, x_trn) 
mean(abs((pred_xg_trn-as.matrix(y_trn))))
sqrt(mean((pred_xg_trn-as.matrix(y_trn))^2))

pred_xg_tst = predict(xgb_2, x_tst) 
mean(abs((pred_xg_tst-as.matrix(y_tst))))
sqrt(mean((pred_xg_tst-as.matrix(y_tst))^2))

xgb_3 = xgboost(data = x_trn , label = y_trn, nrounds = 750, eta = 0.05 )
pred_xg_trn = predict(xgb_3, x_trn) 
mean(abs((pred_xg_trn-as.matrix(y_trn))))
sqrt(mean((pred_xg_trn-as.matrix(y_trn))^2))

pred_xg_tst = predict(xgb_3, x_tst) 
mean(abs((pred_xg_tst-as.matrix(y_tst))))
sqrt(mean((pred_xg_tst-as.matrix(y_tst))^2))

#####################################################################################################
################################ RIDGE , LASSO , ELASTIC NET  ###########################################
##################################################################################################

############################################ lasso regression -1 
library(glmnet)

lr_3 <- glmnet(x_trn, y_trn, alpha=1, family="gaussian")
plot(lr_3,  xvar="lambda" )

lambda_lasso = min(lr_3$lambda)

pred_tst_3 <- predict(lr_3, s = lambda_lasso, newx = x_tst, type = "response")

mean(abs((pred_tst_3-as.matrix(y_tst))))
sqrt(mean((pred_tst_3-as.matrix(y_tst))^2))

pred_trn_3 <- predict(lr_3, s = lambda_lasso, newx = x_trn, type = "response")

mean(abs((pred_trn_3-as.matrix(y_trn))))
sqrt(mean((pred_trn_3-as.matrix(y_trn))^2))



############################### ridge regression -1


lr_4 <- glmnet(x_trn, y_trn, alpha=0, family="gaussian")
plot(lr_4, xvar="lambda", main= "ridge without cross validation")

lambda_ridge <- min(lr_4$lambda)

pred_tst_4 <- predict(lr_4, s = lambda_ridge, newx = x_tst, type = "response")

mean(abs((pred_tst_4-as.matrix(y_tst))))
sqrt(mean((pred_tst_4-as.matrix(y_tst))^2))


pred_trn_4 <- predict(lr_4, s = lambda_ridge, newx = x_trn, type = "response")

mean(abs((pred_trn_4-as.matrix(y_trn))))
sqrt(mean((pred_trn_4-as.matrix(y_trn))^2))



############################################ lasso regression -4


lr_9 <- cv.glmnet(x_trn, y_trn, alpha = 1, family="gaussian", nfolds=20)
lambda_lasso_9 <- lr_9$lambda.min
plot(lr_9, xvar="lambda")

pred_tst_9 <- predict(lr_9, s = lambda_lasso_9, newx = x_tst, type = "response")


mean(abs((pred_tst_9-as.matrix(y_tst))))
sqrt(mean((pred_tst_9-as.matrix(y_tst))^2))


pred_trn_9 <- predict(lr_9, s = lambda_lasso_9, newx = x_trn, type = "response")


mean(abs((pred_trn_9-as.matrix(y_trn))))
sqrt(mean((pred_trn_9-as.matrix(y_trn))^2))


############################### ridge regression -4


lr_10 <- cv.glmnet(x_trn, y_trn, alpha = 0, family="gaussian", nfolds = 20)
lambda_ridge_10 <- lr_10$lambda.min
plot(lr_10, main = "Ridge with 20 fold cross validation")

pred_tst_10 <- predict(lr_10, s = lambda_ridge_10, newx = x_tst, type = "response")

mean(abs((pred_tst_10-as.matrix(y_tst))))
sqrt(mean((pred_tst_10-as.matrix(y_tst))^2))


pred_trn_10 <- predict(lr_10, s = lambda_ridge_10, newx = x_trn, type = "response")

mean(abs((pred_trn_10-as.matrix(y_trn))))
sqrt(mean((pred_trn_10-as.matrix(y_trn))^2))

########## elastic net

lr_11 <- cv.glmnet(x_trn, y_trn, alpha = 0.5, family="gaussian", nfolds = 20)
lambda_elastic_11 <- lr_11$lambda.min
plot(lr_11, xvar="lambda", main= "Elastic net with 20 fold cross validation")

pred_tst_11 <- predict(lr_11, s = lambda_elastic_11, newx = x_tst, type = "response")

mean(abs((pred_tst_11-as.matrix(y_tst))))
sqrt(mean((pred_tst_11-as.matrix(y_tst))^2))

pred_trn_11 <- predict(lr_11, s = lambda_elastic_11, newx = x_trn, type = "response")

mean(abs((pred_trn_11-as.matrix(y_trn))))
sqrt(mean((pred_trn_11-as.matrix(y_trn))^2))

######## elastic net without cv folds

lr_12 <- glmnet(x_trn, y_trn, alpha=0.5, family="gaussian")
plot(lr_12, xvar="lambda")

lambda_elastic_12 = min(lr_12$lambda)

pred_tst_12 <- predict(lr_12, s = lambda_elastic_12, newx = x_tst, type = "response")

mean(abs((pred_tst_12-as.matrix(y_tst))))
sqrt(mean((pred_tst_12-as.matrix(y_tst))^2))


pred_trn_12 <- predict(lr_12, s = lambda_elastic_12, newx = x_trn, type = "response")

mean(abs((pred_trn_12-as.matrix(y_trn))))
sqrt(mean((pred_trn_12-as.matrix(y_trn))^2))












