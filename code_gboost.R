setwd("/Users/leonie/Desktop/master/glm")

library(caret)
library(xgboost)
library(Matrix)

data_train <- readRDS(file = "data_train.rds")
data_test <- readRDS(file = "data_test.rds")

# Nous séparons ici `data_train` en 2 jeux de données `data_train1` et `data_train2`.
set.seed(1)
data_train1 = data_train[sample(1:nrow(data_train), round(nrow(data_train) * .75)),]
data_train2 = data_train[setdiff(rownames(data_train), rownames(data_train1)),]

# L’ojectif maintenant est : 
# - construire et entrainer un modèle simple sur `data_train1`
# - évaluer ce modèle sur `data_train2`
X_train <- as.matrix(data_train1[,5:ncol(data_train1)])
Y_train <- data_train1[,1]
Y_train <- as.numeric(Y_train)
Y_train <- as.matrix(Y_train)
X_test <- as.matrix(data_train2[,5:ncol(data_train2)])
Y_test <- data_train2[,1]
Y_test <- as.numeric(Y_test)
Y_test <- as.matrix(Y_test)

# Conversion des données au format adapté à xgboost
data_train_gb <- xgb.DMatrix( data = X_train, label = Y_train)
data_test_gb <- xgb.DMatrix(data = X_test, label = Y_test)

xgb_trcontrol <- trainControl(method = "cv", number = 5, allowParallel = TRUE, 
                             verboseIter = FALSE, returnData = FALSE)

xgbGrid2 <- expand.grid(nrounds = seq(10, 2000, length.out = 25),  
                       max_depth = seq(1, 50, length.out = 1),
                       colsample_bytree = seq(0.01, 1, length.out = 0.01),
                       eta = seq(0.01, 1, length.out = 0.01),
                       gamma=seq(0.01, 10, length.out = 0.01),
                       min_child_weight = seq(0.01, 10, length.out = 0.01),
                       subsample = seq(0.01, 1, length.out = 0.01)
)


# Créer la barre de progression
total_iters <- length(xgbGrid2)  # Nombre total d'itérations (selon le nombre d'hyperparamètres dans tuneGrid)
progress_bar <- txtProgressBar(min = 0, max = total_iters, style = 3)

# Fonction personnalisée pour l'entraînement avec mise à jour de la barre de progression
train_with_progress <- function(data_train_gb, trControl, tuneGrid) {
  model <- NULL
  for (i in 1:total_iters) {
    # Mettre à jour la barre de progression après chaque itération
    setTxtProgressBar(progress_bar, i)
    
    #Extraire la ligne de la grille et conversion en liste
    params_list <- as.list(tuneGrid[i, , drop = FALSE])
    
    # Entraînement du modèle pour la combinaison d'hyperparamètres en cours
    model <- xgb.train(data = data_train_gb, 
                   trControl = trControl, 
                   params = params_list, 
                   nrounds = params_list$nrounds)
  }
  close(progress_bar)  # Fermer la barre de progression à la fin
  return(model)
}

# Entraîner le modèle avec la barre de progression
xgb_model2 <- train_with_progress(data_train_gb, xgb_trcontrol, xgbGrid2)

test_2 <- xgb_model2$bestTune
test_2


predicted2 <- predict(xgb_model2, data_test_gb)

table2 <- table(predicted2, Y_test)
tx_erreur <- (length(Y_test) - sum(diag(table2))) / length(Y_test) *100
tx_erreur


# Premier tour
# nrounds 100; 200
# max depth 3, 5, 10, 15, 20
# colsample_bytree = seq(0.5, 0.9, length.out = 5)

# learningRate : peut varier de 0.01 à 1
# nEstimators : peut varier de 10 à 2000, par
# pas de 25
# maxDepth : peut varier de 1 à 15
# minChildWeight : peut varier de 0.01 à 10.0
# gammaValue : peut varier de 0.01 à 10
# subSample : peut varier de 0.01 à 1.0
# colSampleByTree :  0.01 à 1.0

# xgb_model2 <- train(X_train, Y_train, trControl = xgb_trcontrol, tuneGrid = xgbGrid2, 
#                    method = "xgbTree")
