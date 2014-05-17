library(FSelector);
library(cvTools);
library(caret);
library(nnet);

filename <- "/home/radek/Pulpit/LTNN/data.csv";
maxNumOfAtribs <- 12;
numOfValidationRepetition <- 10;

ATmGM <- read.csv(filename);

FirstDataSet <- data.frame(ATmGM);

cols <- c(1:22);

FirstDataSet_1E <- FirstDataSet[cols];

#ranking <- rank.correlation (ClassificationGroup + PatientNo ~ ., data = FirstDataSet_1E);
cat("Preparing attributes ranking...                                                            \r");
ranking <- relief(ClassificationGroup + PatientNo ~ ., data = FirstDataSet_1E, neighbours.count = 10, sample.size = 20);

for(numOfAtribs in 8:maxNumOfAtribs)
{
  cat("Selecting best atributes from ranklist...                                                   \r");
  rankcut <- cutoff.k(ranking, numOfAtribs);
  
  FirstDataSet_1E_SelectedRanks <- FirstDataSet_1E[c(rankcut, "ClassificationGroup")];
  
  hits <- 0;
  tries <- 0;
  
  folds1 <- cvFolds(NROW(FirstDataSet_1E_SelectedRanks), K = numOfValidationRepetition);
  for(repetiton in 1:numOfValidationRepetition)
  {
    train1 <- FirstDataSet_1E_SelectedRanks[folds1$subsets[folds1$which != repetiton], ]; # zbior testowy
    validation1 <- FirstDataSet_1E_SelectedRanks[folds1$subsets[folds1$which == repetiton], ]; # zbor testujacy
    
    #ann1 <- nnet(as.factor(ClassificationGroup) ~ ., data = train1, size = 21, decay = 0); # atrybut size podobnie jak poprzednio
    output <- sprintf("Training neural network [%3d%%]                                  \r", repetiton*100/numOfValidationRepetition)
    cat(output);
    ann1 <- train(train1, as.factor(train1$ClassificationGroup), method = "nnet", 
        tuneGrid=data.frame(.size = 20, .decay = 0 ),  # atrybut size wyliczasz za pomocą wzoru: (ilość cech wejściowych + ilość wyjściowych klas)/2
        # ewentualnie przy testowaniu sprawdzacz czy size+1, size+2, itd. ale bez przesady, są lepsze
        trControl = trainControl(method = "repeatedcv", number = 2, repeats = 5, verbose = FALSE), 
        tuneLength = 10,
        verbose = FALSE,
        trace = FALSE);
  
    #pred1 <- predict(ann1, newdata = validation1, type = "class");
    cat("Predicting result...                                                                    \r");
    pred1 <- predict(ann1, newdata = validation1, type = "raw");
    
    tabela <- table(validation1$ClassificationGroup,pred1);
    
    for(row in 1:maxNumOfAtribs)
    {
      currentColRow <- as.character(row)
      if( currentColRow %in% rownames(tabela))
      {
        if( currentColRow %in% colnames(tabela))
        {
          hits <- hits + as.integer(tabela[currentColRow, currentColRow]);
        }
        else
        {
          #output <- sprintf("No column named %d found.",row); print(output);
        }
      }
      else
      {
        #output <- sprintf("No row named %d found.",row); print(output);
      }
    }
    tries <- tries + sum(tabela);
    
  }
  cat("----------------------------\n");
  output<-sprintf("Number of atribites: %d\n" , numOfAtribs); cat(output);
  output<-sprintf("Validation repetition %d\n", numOfValidationRepetition);
  cat("Selected attributes: "); cat(rankcut);
  output<-sprintf("\nWe've hit %d of %d times. Accuracy: %.4f %%\n",hits, tries, (hits/tries)*100.0); cat(output);
  cat("----------------------------\n");
}

