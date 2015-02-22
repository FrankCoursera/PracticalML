activity <- function() {

        library(caret)
        set.seed(12345)
        
        ## Read  data
        rawTraining <- read.csv("pml-training.csv" , na.strings=c("#DIV/0!","NA","", strip.white=T) )
        evaluation <- read.csv("pml-testing.csv" , na.strings=c("#DIV/0!","NA","", strip.white=T))
  
        # remove columns with names and times etc.
        rawTraining <- rawTraining [ -c(1:7)]
        evaluation <- evaluation [ -c(1:7)]
        
        # remove empty columns with NAs only
        training <- rawTraining[ , colSums(is.na(rawTraining)) == 0   ]
        evaluation <-evaluation[ , colSums(is.na(evaluation)) == 0   ]
        

        # partition clean data now, 70% of data
        part <- createDataPartition(y=training$classe, p=0.70, list=FALSE )
        training <- training[part,]
        test <-  training[-part,]

        
        # parallel random forest of training data        
        ctrl<-trainControl(method="cv", number=4, allowParallel=T, verbose=T)
        model <-train(classe ~ .,data=training, method="rf", trControl=ctrl, verbose=F)
 
        
        # prediction outcome of test data
        
        predTest <- predict(model, newdata=test)
        confusionMatrix(predTest,test$classe)
        
        
        # cross validations
        
        # output for our evaluation data
        pred20<-predict(model, newdata=evaluation)
        
}

