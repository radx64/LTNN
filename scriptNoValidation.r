#-------------------------------------------------------------------------------------------------------------------------------------
# KLASYFIKACJA BEZ SELEKCJI CECH
# NAJBARDZIEJ ZAUTOMATYZOWANA
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


# drzewo decyzyjne

# w funkcji train pierwszy parametr to zbiór atrybutów, które wejdą w skład modelu uczącego, w moim przypadku wszystkie oprócz 18-tego
# drugi parametr to kolumna zawierająca klasę, w moim przypadku 18

# dla każdego klasyfikatora wykorzystana jest 10-krotna walidacja krzyżowa

ANN <- train(FirstDataSet_1E[,-18], as.factor(FirstDataSet_1E[,1]), method = "nnet", 
             tuneGrid=data.frame(.size = 13, .decay = 0 ),  # atrybut size wyliczasz za pomocą wzoru: (ilość cech wejściowych + ilość wyjściowych klas)/2
                                                            # ewentualnie przy testowaniu sprawdzacz czy size+1, size+2, itd. ale bez przesady, są lepsze
             trControl = trainControl(method = "cv", number = 10), 
             tuneLength = 10);
ANN;

