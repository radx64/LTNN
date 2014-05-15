#-------------------------------------------------------------------------------------------------------------------------------------
# KLASYFIKACJA Z SELEKCJA CECH
# RECZNA WALIDACJA KRZYZOWA
# Algorytm CFS
#-------------------------------------------------------------------------------------------------------------------------------------


# wczytanie odpowiednich, uprzednio zainstalowanych bibliotek

library(caret);
library(cvTools);
library(e1071);
library(klaR);
library(nnet);
library(FSelector);
library(rpart);

# wczytanie danych głównych z wszystkimi atrybutami

ATmGM <- read.csv("/home/radek/Pulpit/med/data.csv");

# przygotowanie odpowiednich frame-ów

FirstDataSet <- data.frame(ATmGM);

# wybór kolumn z datasetu
# niekonieczny jest wybór wszystkich
cols <- c(1:22);

# wlasciwy data frame
FirstDataSet_1E <- FirstDataSet[cols];

# selekcja cech z FSelector

# CFS
# podzbiór cech, które mają wejść do procesu nauki
# 1 param cfs - cechy, które chcesz wykluczyć - w moim przypadku atrybut klasy i niepotrzebna kolumna porządkowa
# 2 param cfs - zbiór danych, na których chcesz operować
# wynik cfs - zbiór z obliczonymi, najbardziej istotnymi cechami
subset_cfs <- cfs(ClassificationGroup + PatientNo ~ .,data = FirstDataSet_1E);

#-------------------------------------------------------------------------------------------------------------------------------------
# finalny data frame
FirstDataSet_1E_CFS <- FirstDataSet_1E[c(subset_cfs, "ClassificationGroup")];
#-------------------------------------------------------------------------------------------------------------------------------------

# ilość powtórzeń walidacji
k_folds <- 5;

#-------------------------------------------------------------------------------------------------------------------------------------
# CFS
#-------------------------------------------------------------------------------------------------------------------------------------
folds1 <- cvFolds(NROW(FirstDataSet_1E_CFS), K = k_folds);



for(j in 1:k_folds)
{
  # Walidacja krzyżowa - podział na zbiory testowy i testujący
  train1 <- FirstDataSet_1E_CFS[folds1$subsets[folds1$which != j], ]; # zbior testowy
  validation1 <- FirstDataSet_1E_CFS[folds1$subsets[folds1$which == j], ]; # zbor testujacy
  
  # ANN package "nnet"
  #ann1 <- nnet(as.factor(ClassificationGroup) ~ ., data = train1, size = 13); # atrybut size podobnie jak poprzednio
  #pred_ann1 <- predict(ann1, newdata = validation1, type = "class");
  
  
  ANN <- train(FirstDataSet_1E[,-18], as.factor(FirstDataSet_1E_CFS[,2]), method = "nnet", 
               tuneGrid=data.frame(.size = 13, .decay = 0 ),  # atrybut size wyliczasz za pomocą wzoru: (ilość cech wejściowych + ilość wyjściowych klas)/2
               # ewentualnie przy testowaniu sprawdzacz czy size+1, size+2, itd. ale bez przesady, są lepsze
               trControl = trainControl(method = "cv", number = 2), 
               tuneLength = 5);

  
  #NN <- neuralnet(as.factor(ClassificationGroup) , train1, hidden = 1);
  
  
}

subset_cfs;
FirstDataSet_1E_CFS;
ANN;
# wyniki w postaci macierz konfuzji

#confusionMatrix(pred_ann1, as.factor(validation1$ClassificationGroup));

