library(FSelector);
library(cvTools);
library(caret);
library(nnet);

filename <- "/home/radek/Pulpit/LTNN/data.csv";
maxNumOfAtribs <- 20;
numOfValidationRepetition <- 10;

ATmGM <- read.csv(filename);

FirstDataSet <- data.frame(ATmGM);

cols <- c(1:22);

FirstDataSet_1E <- FirstDataSet[cols];

#ranking <- rank.correlation (ClassificationGroup + PatientNo ~ ., data = FirstDataSet_1E);
ranking <- relief(ClassificationGroup + PatientNo ~ ., data = FirstDataSet_1E, neighbours.count = 5, sample.size = 10);

for(numOfAtribs in 20:maxNumOfAtribs)
{
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

    ann1 <- train(train1, as.factor(train1$ClassificationGroup), method = "nnet", 
        tuneGrid=data.frame(.size = 20, .decay = 0 ),  # atrybut size wyliczasz za pomocą wzoru: (ilość cech wejściowych + ilość wyjściowych klas)/2
        # ewentualnie przy testowaniu sprawdzacz czy size+1, size+2, itd. ale bez przesady, są lepsze
        trControl = trainControl(method = "repeatedcv", number = 2, repeats = 5), 
        tuneLength = 5);
  
    #pred1 <- predict(ann1, newdata = validation1, type = "class");
    
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
          output <- sprintf("No column named %d found.",row); print(output);
        }
      }
      else
      {
        output <- sprintf("No row named %d found.",row); print(output);
      }
    }
    tries <- tries + sum(tabela);
    
  }
  
  output<-sprintf("We've hit %d of %d times",hits, tries); print(output);
  output<-sprintf("%.3f %%",(hits/tries)*100.0); print(output);
}

