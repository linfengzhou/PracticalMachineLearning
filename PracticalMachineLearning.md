Practical Machine Learning Project Work - Weigh Lifting Dataset
========================================================

# Backgroud

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 

## Environment

We first intialize the environment by included the libraries used and by setting the seed for random number generator.

```{r}
setwd("D://")
require(dplyr)
require(caret)
library(e1071)
library(adabag)
library(randomForest)
set.seed(1314)
```

## Data processing


- train_data is the training data set
- test_data is the test set

```{r}
train_data <- read.csv("pml-training.csv")
test_data <- read.csv("pml-testing.csv")
```

We should clean the data first.

```{r}
near_zeros <- nearZeroVar(train_data[,-160])
train_data_cleaned <- train_data[, -near_zeros]
test_data_cleaned <- test_data[, -near_zeros]
```

Deleting the missing data

```{r}
has_nas <- colSums(is.na(train_data_cleaned)) > 0
train_data_cleaned <- train_data_cleaned[, !has_nas]
test_data_cleaned <- test_data_cleaned[, !has_nas]
```

```{r}
train_data_cleaned <- train_data_cleaned[, 7:59]
test_data_cleaned <- test_data_cleaned[, 7:59]
```


## Creating data sets for cross-validation



```{r}

in_train <- createDataPartition(train_data_cleaned$classe, p = 0.7, list=FALSE)
training <- train_data_cleaned[in_train, ]
testing <- train_data_cleaned[-in_train, ]

```

## Prediction with SVM


```{r, cache=TRUE}
a=svm(classe~.,data=training,kernal="sigmoid")
z0=table(training[,53],predict(a,training))
z0
```
```{r}

test_preds <- predict(a,newdata=testing)
confusionMatrix(test_preds,testing$classe)
```

The accuracy of the model (out ot sample) is  0.9392.  

## Prediction with randomForest
.

```{r,cache=TRUE}
b=randomForest(classe~.,data=training, method="class")
z1=table(training[,53],predict(b,training))
z1
```

 
```{r}

test_preds <- predict(b,newdata=testing)
confusionMatrix(test_preds,testing$classe)
```
 The accuracy of the model (out ot sample) is 0.9935.  
```{r}
real_preds <- predict(b, newdata=test_data_cleaned)

```
Therefore, we chose randomForest to predict test data.

And finally write them to a file with a function provided in the project.

```{r}

# function for writing the output files
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

# write the predictons generated in the previsou step into files.
pml_write_files(real_preds)
```




