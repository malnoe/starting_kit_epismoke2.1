setwd("/Users/leonie/Desktop/master/glm")
library(e1071)

data_train <- readRDS(file = "data_train.rds")
data_test <- readRDS(file = "data_test.rds")

# Nous séparons ici `data_train` en 2 jeux de données `data_train1` et `data_train2`.
# set.seed(1) # Pour avoir des résultats aléatoire reproductibles

data_train1 <- data_train[sample(round(nrow(data_train) * 0.75)), ]
data_train2 <- data_train[setdiff(rownames(data_train), rownames(data_train1)), ]

data_train1 <- data_train1[, -c(2:4)]
data_train2 <- data_train2[, -c(2:4)]

# L’ojectif maintenant est : 
# - construire et entrainer un modèle simple sur `data_train1`
# - évaluer ce modèle sur `data_train2`
X_train <- as.matrix(data_train1[,5:ncol(data_train1)])
Y_train <- data_train1[,1]
Y_train <- as.factor(Y_train)
X_test <- as.matrix(data_train2[,5:ncol(data_train2)])
Y_test <- data_train2[,1]
Y_test <- as.factor(Y_test)

svm_modele <- svm(smoking_status ~ ., 
                  data = data_train1, kernel = "linear" )

predict_svm <- predict(svm_modele, data_train2[, -1])

as.vector(predict_svm)

confusionMatrix(predict_svm, data_train2$smoking_status)

tx_prec <- sum(predict_svm == Y_test)/length(Y_test)
tx_prec
tx_erreur <- 1 - tx_prec
tx_erreur

# avec ACP
X_train_acp <- PCA(X = X_train, graph = FALSE, scale.unit = TRUE, ncp = 100)$ind$coord
X_test_acp <- PCA(X = X_test, graph = FALSE, scale.unit = TRUE, ncp = 100)$ind$coord
X_train_acp <- as.data.frame(X_train_acp)
X_test_acp <- as.data.frame(X_test_acp)

data_train_acp <- cbind(Y_train, X_train_acp)
data_test_acp <- cbind(Y_test, X_test_acp)

svm_modele_acp <- svm(Y_train ~ ., 
                  data = data_train_acp, kernel = "linear" )

predict_svm_acp <- predict(svm_modele_acp, data_test_acp[, -1])

confusionMatrix(predict_svm_acp, data_test_acp[,1])

tx_prec_acp <- sum(predict_svm_acp == Y_test)/length(Y_test)
tx_prec_acp
tx_erreur_acp <- 1 - tx_prec_acp
tx_erreur_acp
