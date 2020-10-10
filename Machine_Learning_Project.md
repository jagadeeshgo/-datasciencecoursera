---
title: "Machine learning project"
author: "Jagadeesh"
date: "10/10/2020"
output: 
  html_document: 
    keep_md: yes
---

##Introduction
    Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har.The data for this project come from this source: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.
    
In this project We have taken the data from the electronic gadgets from the accelerometres ofc the devices.it was decided that the SVM model with the lowest validation error is with cost = 10 and gamma = 1.
The main result is that when the trained model is used with the test data set from the prediction evaluation, the accuracy is equal to 100%.
##Packages used

```r
library(ggplot2)
library(gridExtra)
#library(caret)
library(e1071)
```
it is used for graphical representation
##load and preprocess the data
This allows to select the features according to accelerometer on belt, forearm ,dumbell

```r
#Read data sets
bdtraining <- read.csv("pml-training.csv")
bdtest <- read.csv("pml-testing.csv")

#Select features according to accelerometers on the belt, 
#forearm, arm and dumbell and outcome feature
selectVector <- grepl("accel|classe", names(bdtraining))

#New Training data
bdtraining <- bdtraining[,selectVector]

#Investagate features with NAs
NAvector <- sapply(bdtraining, function(x){
        sum(is.na(x))
})

#Selection vector
SeleVec <- NAvector==0

#Elimanate features with NAs
bdtraining <- bdtraining[,SeleVec]


#Select adecuate variables in test data set

#Select features according to accelerometers on the belt, 
#forearm, arm and dumbell and outcome feature
selectVector <- grepl("accel", names(bdtest))

#New Training data
bdtest <- bdtest[,selectVector]

#Investagate features with NAs
NAvector <- sapply(bdtest, function(x){
        sum(is.na(x))
})

#Selection vector
SeleVec <- NAvector==0

#Elimanate features with NAs
bdtest <- bdtest[,SeleVec]
```
##Pictographial representation to get a clear picture b/w two gadgets some exploratory data analysis

```r
g1 <- ggplot(bdtraining, aes(x=total_accel_belt, y=total_accel_dumbbell,
                       col=classe)) +
        geom_point(alpha = 0.2, lwd = 5)+
        theme(legend.position="none") + 
        ggtitle("Total_accel_dumbell vs total_accel_belt by classe")

g2 <- ggplot(bdtraining, aes(x=total_accel_belt, y=total_accel_forearm,
                             col=classe)) +
        geom_point(alpha = 0.2, lwd = 5) + 
        ggtitle("Total_accel_forearm vs total_accel_belt by classe")

grid.arrange(g1, g2)
```

![](Machine_Learning_Project_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
##Model Fit
The e1071 library was used to estimate a SVM Algorithm. 5-folds cross validation were applied to determine the cost and gamma parameters of the svm function producing the lowest k-fold Cross Validation error rate

```r
#Fit SVM algorithm whit Cost = 10
#and gamma 1

bdtraining$classe<-as.factor(bdtraining$classe)
set.seed(4621)
svmfit <- svm(classe ~ ., data = bdtraining,
               kernel="radial", cost = 10, gamma=1,
               scale = TRUE,
               cross = 5)

#K fold CV error = 4.122923
    100 - svmfit$tot.accuracy
```

```
## [1] 4.122923
```
##Error test
When evaluating the SVM by training in the test database, bdtest, the following predictions are produced:

```r
bdtest
```

```
##    total_accel_belt accel_belt_x accel_belt_y accel_belt_z total_accel_arm
## 1                20          -38           69         -179              10
## 2                 4          -13           11           39              38
## 3                 5            1           -1           49              44
## 4                17           46           45         -156              25
## 5                 3           -8            4           27              29
## 6                 4          -11          -16           38              14
## 7                 4          -14            2           35              15
## 8                 4          -10           -2           42              22
## 9                 4          -15            1           32              34
## 10               18          -25           63         -158              32
## 11                3          -18            4           27              33
## 12                5          -22            8           40              30
## 13                3           -8            5           24              23
## 14                5          -14            2           49              37
## 15                4            8           19           28              23
## 16                2          -12            5           20              33
## 17               21          -47           69         -187               3
## 18                3          -13            3           24              42
## 19               19          -48           72         -169              11
## 20                3           -9            4           23              30
##    accel_arm_x accel_arm_y accel_arm_z total_accel_dumbbell accel_dumbbell_x
## 1           16          38          93                    9               21
## 2         -290         215         -90                   31             -153
## 3         -341         245         -87                   29             -141
## 4         -238         -57           6                   18              -51
## 5         -197         200         -30                    4              -18
## 6          -26         130         -19                   29             -138
## 7           99          79         -67                   29             -145
## 8          -98         175         -78                   29             -140
## 9         -287         111        -122                    3                0
## 10        -301         -42         -80                    2               -7
## 11        -277         113        -112                    1               -4
## 12        -192         204         -75                   30             -149
## 13         106          97        -168                    8               27
## 14        -277         157        -183                   30             -139
## 15          41          85        -204                   31             -159
## 16        -236         163        -148                   13               43
## 17           2          -9          27                   11               14
## 18          35         -65        -404                    4              -20
## 19          -7          57          89                   10               22
## 20        -223         166        -105                   23              185
##    accel_dumbbell_y accel_dumbbell_z total_accel_forearm accel_forearm_x
## 1               -15               81                  33            -110
## 2               155             -205                  39             212
## 3               155             -196                  34             154
## 4                72             -148                  43             -92
## 5               -30               -5                  24             131
## 6               166             -186                  43             230
## 7               150             -190                  32            -192
## 8               159             -191                  47            -151
## 9                25                9                  36             195
## 10              -20                7                  24            -212
## 11               12                1                  46              -3
## 12              128             -215                  36             182
## 13               10               75                  23             149
## 14              155             -202                  33             232
## 15              136             -221                  24             -18
## 16               71              100                  25             161
## 17              -29               98                  30             144
## 18               37               -1                  25            -129
## 19               -7               91                  23            -148
## 20               81               98                  21              41
##    accel_forearm_y accel_forearm_z
## 1              267            -149
## 2              297            -118
## 3              271            -129
## 4              406             -39
## 5              -93             172
## 6              322            -144
## 7              170            -175
## 8             -331            -282
## 9              204            -217
## 10              98              -7
## 11             405            -203
## 12             263            -148
## 13              46             167
## 14             106            -198
## 15              43            -226
## 16             -61             171
## 17             201            -154
## 18             -29            -202
## 19              21            -172
## 20            -100             179
```

```r
predict(svmfit,newdata = bdtest)
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```
These are the predictions
