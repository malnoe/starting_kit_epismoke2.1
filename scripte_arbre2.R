# Code pour la création du modèle arbre de décision
# Idée : On va faire un permier modèle qui permettra de prédire si oui ou non une personne appartient à une des trois catégories.
# Puis après créer un autre modèle entrainé sur les individus des 2 autres status restant pour prédire si oui ou non un individu appartient à une des deux catégories.
# Au final, on combinerai les deux modèles en seul comme une espèce d'arbre de décision. Par exemple :
# On a un premier modèle qui prédit si la personne fume actuellement ou non et on l'applique à nous données.
# Les personnes labélisées comme ne fumant pas actuellement sont ensuite passées dans le second modèle pour déterminer si elles sont d'anciens fumeurs.
# Les personnes restantes sont alors labélisées comme n'ayant jamais fumées.

# Importation des données
data_train = readRDS(file = "data_train.rds") # Données de train 
data_test = readRDS(file = "data_test.rds") # Données de test
dim(data_train)
dim(data_test)
head(data_train[,1:6])

# Récupération les noms des colonnes
probes = colnames(data_train)[5:10004] #Nom des sondes
head(data_train[,probes[1:10]])

# Description statistique : la densité 
plot(density(as.matrix(data_train[,probes])))
# Commentaire : On voit apparaitre deux piques, ça va nous permettre de faire plus facilement 
#la distinction entre les cas où il y a méthylation (0.7-1.0) et les cas où il n'y a pas de 
# méthylation (0-0.2). On va égalament pouvoir considérer que la distribution de 
# la valeur de chaque sonde est gaussienne.
table(data_train$smoking_status)
# Commentaire : Les 3 statuts sont présents de manière presque équidistribuées. On peut pas juste prédire avec le modèle nulle qui renvoie comme résultat le statut le plus commun. On aurait lors environ 60% d'erreur.
# Remarque : On obtenait 63% d'erreur sur codabench avec le modèle nul

# Séparation des données d'entrainement pour pouvoir faire de l'optimisation des features.
set.seed(1) # On prend une seed 1 pour avoir des résultats aléatoire reproductibles.
data_train1 = data_train[sample(1:nrow(data_train), round(nrow(data_train) * .75)),] # On prend 75% pour l'entrainement et 25% pour l'optimisation des features
data_train2 = data_train[setdiff(rownames(data_train), rownames(data_train1)),]

# Comparaison de la séparation de data_train
prop.table(table(data_train1$smoking_status)) 
prop.table(table(data_train2$smoking_status))
# Commentaire : On remarque que les proportions de chaque groupe (chaque statut tabagique) ne sont pas exactement les mêmes entre data_train1 et data_train2.

# L’ojectif maintenant est : 
#  - construire et entrainer un premier modèle simple sur `data_train1`
#  - évaluer ce modèle sur `data_train2`

# Tester la significativité des sondes
# Commentaire : On ne peut pas constuire de lm(smoking_status~sonde) donc on va plutôt regarder lm(sonde~smoking_status) parce que l'on peut supposer que si le smoking_status décrit bien la sonde alors la sonde va assez bien décrire le smoking_status
siscreening = function(data_train) {
  probes = colnames(data_train)[5:length(data_train)]  # Noms des sondes
  pval_fisher = c()
  r2 = c()
  for (p in probes) {
    m = lm(data_train[,p]~data_train[,"smoking_status"])  #On crée un modèle linéaire sonde~smoking_status
    pval_fisher = c(pval_fisher, anova(m)[1,5])  # On récupère la p-valeur du modèle
    r2 = c(r2, summary(m)$r.squared) # On récupère le r2 associé à ce modèle
  }
  names(pval_fisher)  = probes
  names(r2)           = probes
  return(data.frame(pval_fisher=pval_fisher, r2=r2)) # On renvoie le dataframe  contenant les p-valeur et le r2 pour chaque modèle
}
if (!exists("msiscreening")) {msiscreening = memoise::memoise(siscreening)} # On mémoise la fonciton pour mettre le résultat en cache.
sis_res = msiscreening(data_train1) # Dataframe résutlat

# Graphe R2 et p-value
plot(sis_res$r2, -log10(sis_res$pval),main="-log10(p-value) des modèles linéaire à une sonde en fonction du R2")
# Commentaire : on a une relation logarithme : quand la p-valye devient très petite (-log10(p_val) grand),
# le R² augmente.

# Tri des sondes en fonction de la p-value
sis_probes = rownames(sis_res)[order(sis_res$pval_fisher)]
head(sis_res[sis_probes,])
# Commentaire : ils ont tous un R2 proche de 0.1, c'est mieux que rien mais c'est pas top.


# Maintenant que l'on a obtenu les meilleures sondes, nous allons essayer de construire un modèle d'arbre de décision à la main.
# L'idée va être de faire une première prédiction pour choisir entre statut_1 vs non statut_1 (par exemple never vs current ou former)
# Puis d'appliquer une seconde prédivtion pour ensuite choisir entre statut_2 et statut_3 (toujours dans le même exemple current vs former)
# On va d'abord constuire un modèle pour prédire pour les 3 statuts (never, former et current) puis construirons les modèles de combinaison et nous gererons l'hyperparametrage


# CURRENT
# Fonciton pour le modèle sis avec i sondes.
model_current_sis_i = function(data_train, i, screening_func=msiscreening){
  print(paste0("model sis ", i))
  sis_res = screening_func(data_train)  
  sis_probes = rownames(sis_res)[order(sis_res$pval_fisher)]
  formula = as.formula(paste0(c("current01~1", sis_probes[0:i]), collapse="+")) ; 
  m = glm(formula, data_train, family=binomial(link="logit"))
  return(m)
}
# On regarde le sur apprentissage sur les 50 sondes
iap1 = c()
iap2 = c()
for (i in 0:50) {
  m = model_current_sis_i(data_train1, i)
  pred_train1 = predict.glm(m, data_train1, type="response")
  pred_train2 = predict.glm(m, data_train2, type="response")
  iap1 = c(iap1, sum(ifelse(pred_train1>0.5, 1, 0) != data_train1$current01) / nrow(data_train1))
  iap2 = c(iap2, sum(ifelse(pred_train2>0.5, 1, 0) != data_train2$current01) / nrow(data_train2))
}
plot(iap1, col=4, pch=16, cex=1)
points(iap2, col=2, pch=16, cex=1)
legend("bottomright", c("iap1", "iap2"), col=c(4,2), pch=16, cex=0.8)
i=8
abline(v=i, col=2)
# On a des sondes qui sont sûrement corrélées et qui apporte la même information : 
# on va faire un step pour choisir les meilleurs sondes à parir du critère d'Aikaike (AIC).

# Fonction de création de modèles step pour CURRENT
stepforward_current = function(data_train, sis_probes, nb_sis_probes=200, trace=0, k=2) {
  m_lo = glm(current01 ~ 1, data=data_train[,c("current01", sis_probes[1:nb_sis_probes])])
  m_sup = glm(current01 ~ ., data=data_train[,c("current01", sis_probes[1:nb_sis_probes])])
  m_fwd = step(m_lo, method="forward", scope=list(upper=m_sup,lower=m_lo), trace=trace, k=k)
  return(m_fwd)
}

# Modèle step obtenu à partir des 90 meilleurs sondes.
step_model_current_90 = stepforward_current(data_train1, sis_probes, nb_sis_probes = 90,trace=0)

# Test de l'accuracy sur le step_model_current_90
predicted_probs <- predict(step_model_current_90, newdata = data_train2, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0) # On pose le seuil à 0.5
confusion_matrix <- table(Predicted = predicted_classes, Actual = data_train2$current01)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))
# Accuracy : 0.80

# Est-ce qu'on peut obtenier un meilleur modèle ? 
# On va jouer sur les hyper-paramètres seuil et nombre de sondes 
# pour la construction du modèle

# Fonction générale de prédiction à partir d'un modèle step + seuil de décision
predict_current <- function(data,step_model, seuil){
  predicted_probs <- predict(step_model, newdata = data, type = "response")
  predicted_classes <- ifelse(predicted_probs > seuil, 1, 0)
  return(predicted_classes)
}

# On crée des modèles pour l'hyperparamétrage.
create_modeles_current <- function(from = 50, to = 200, by = 10) {
  res <- list()
  for (i in seq(from = from, to = to, by = by)) {
    print(paste0(i, "/", to))
    res[[length(res) + 1]] <- stepforward_current(data_train1, sis_probes, nb_sis_probes = i, trace = 0)
  }
  return(res)
}
modeles_current <- create_modeles_current()

# Fonction pour l'hyper-paramétrage de CURRENT
hyperparametrage_current<- function(data, list_modeles_current, list_seuil_current){
  best_accuracy <- 0
  best_seuil_current <- 0
  best_num_model_current <- 1
  
  
  nb <- length(list_modeles_current)*length(list_seuil_current)
  i <- 0
  
  num_model_current <- 0
  for(step_model_current in list_modeles_current){
    num_model_current <- num_model_current +1
    for(seuil_current in list_seuil_current){
      i <- i+1
      if(i%%10 == 0){
        print(paste0(i,"/",nb))
      }
      confusion_matrix <- table(Predicted = predict_current(data,step_model_current,seuil_current), Actual = data$current01)
      accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      if(accuracy > best_accuracy){
            best_accuracy <- accuracy
            best_num_model_current <- num_model_current
            best_seuil_current <- seuil_current
      }
    }
  }
  return(c(best_accuracy,best_num_model_current,best_seuil_current))
}

hyperparametrage_current(data_train2,modeles_current,list(0.4,0.425,0.45,0.475,0.5,0.525,0.55,0.575,0.6))
# Res :
# Accuracy : 0.82857
# Modèle n*15 -> 190 sondes
# Seuil : 0.575


# NEVER
model_never_sis_i = function(data_train, i, screening_func=msiscreening){
  print(paste0("model sis ", i))
  sis_res = screening_func(data_train)  
  sis_probes = rownames(sis_res)[order(sis_res$pval_fisher)]
  formula = as.formula(paste0(c("never01~1", sis_probes[0:i]), collapse="+")) ; 
  m = glm(formula, data_train, family=binomial(link="logit"))
  return(m)
}
# On regarde le sur apprentissage sur les 50 sondes
iap1 = c()
iap2 = c()
for (i in 0:50) {
  m = model_never_sis_i(data_train1, i)
  pred_train1 = predict.glm(m, data_train1, type="response")
  pred_train2 = predict.glm(m, data_train2, type="response")
  iap1 = c(iap1, sum(ifelse(pred_train1>0.5, 1, 0) != data_train1$never01) / nrow(data_train1))
  iap2 = c(iap2, sum(ifelse(pred_train2>0.5, 1, 0) != data_train2$never01) / nrow(data_train2))
}
plot(iap1, col=4, pch=16, cex=1)
points(iap2, col=2, pch=16, cex=1)
legend("bottomright", c("iap1", "iap2"), col=c(4,2), pch=16, cex=0.8)
i=8
abline(v=i, col=2)

# On effectue un step 
stepforward_never = function(data_train, sis_probes, nb_sis_probes=200, trace=0, k=2) {
  m_lo = glm(never01 ~ 1, data=data_train[,c("never01", sis_probes[1:nb_sis_probes])])
  m_sup = glm(never01 ~ ., data=data_train[,c("never01", sis_probes[1:nb_sis_probes])])
  m_fwd = step(m_lo, method="forward", scope=list(upper=m_sup,lower=m_lo), trace=trace, k=k)
  # print(m_fwd$call)
  return(m_fwd)
}

# Exemple avec 90 sondes
step_model_never_90 = stepforward_never(data_train1, sis_probes, nb_sis_probes = 90,trace=0)

# On regarde les résultats sur data_train2
predicted_probs <- predict(step_model_never_90, newdata = data_train2, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.55, 1, 0)
confusion_matrix <- table(Predicted = predicted_classes, Actual = data_train2$never01)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))
# Accuracy : 0.69

# Fonction générale
predict_never <- function(data,step_model, seuil){
  predicted_probs <- predict(step_model, newdata = data, type = "response")
  predicted_classes <- ifelse(predicted_probs > seuil, 1, 0)
  return(predicted_classes)
}

# On crée des modèles pour l'hyperparamétrage.
create_modeles_never <- function(from = 50, to = 200, by = 10) {
  res <- list()
  for (i in seq(from = from, to = to, by = by)) {
    print(paste0(i, "/", to))
    res[[length(res) + 1]] <- stepforward_never(data_train1, sis_probes, nb_sis_probes = i, trace = 0)
  }
  return(res)
}
modeles_never <- create_modeles_never()

# Fonction pour l'hyper-paramétrage de never
hyperparametrage_never<- function(data, list_modeles_never, list_seuil_never){
  best_accuracy <- 0
  best_seuil_never <- 0
  best_num_model_never <- 1
  
  
  nb <- length(list_modeles_never)*length(list_seuil_never)
  i <- 0
  
  num_model_never <- 0
  for(step_model_never in list_modeles_never){
    num_model_never <- num_model_never +1
    for(seuil_never in list_seuil_never){
      i <- i+1
      if(i%%10 == 0){
        print(paste0(i,"/",nb))
      }
      confusion_matrix <- table(Predicted = predict_never(data,step_model_never,seuil_never), Actual = data$never01)
      accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      if(accuracy > best_accuracy){
        best_accuracy <- accuracy
        best_num_model_never <- num_model_never
        best_seuil_never <- seuil_never
      }
    }
  }
  return(c(best_accuracy,best_num_model_never,best_seuil_never))
}

hyperparametrage_never(data_train2,modeles_never,list(0.4,0.425,0.45,0.475,0.5,0.525,0.55,0.575,0.6))

# Res
# Accuracy : 0.724
# Modèles n*3 -> 70 sondes
# Seuil = 0.425


# FORMER
model_former_sis_i = function(data_train, i, screening_func=msiscreening){
  print(paste0("model sis ", i))
  sis_res = screening_func(data_train)  
  sis_probes = rownames(sis_res)[order(sis_res$pval_fisher)]
  formula = as.formula(paste0(c("former01~1", sis_probes[0:i]), collapse="+")) ; 
  m = glm(formula, data_train, family=binomial(link="logit"))
  return(m)
}
# On regarde le sur apprentissage sur les 50 sondes
iap1 = c()
iap2 = c()
for (i in 0:50) {
  m = model_former_sis_i(data_train1, i)
  pred_train1 = predict.glm(m, data_train1, type="response")
  pred_train2 = predict.glm(m, data_train2, type="response")
  iap1 = c(iap1, sum(ifelse(pred_train1>0.5, 1, 0) != data_train1$former01) / nrow(data_train1))
  iap2 = c(iap2, sum(ifelse(pred_train2>0.5, 1, 0) != data_train2$former01) / nrow(data_train2))
}
plot(iap1, col=4, pch=16, cex=1)
points(iap2, col=2, pch=16, cex=1)
legend("bottomright", c("iap1", "iap2"), col=c(4,2), pch=16, cex=0.8)
i=8
abline(v=i, col=2)

# On effectue un step 
stepforward_former = function(data_train, sis_probes, nb_sis_probes=200, trace=0, k=2) {
  m_lo = glm(former01 ~ 1, data=data_train[,c("former01", sis_probes[1:nb_sis_probes])])
  m_sup = glm(former01 ~ ., data=data_train[,c("former01", sis_probes[1:nb_sis_probes])])
  m_fwd = step(m_lo, method="forward", scope=list(upper=m_sup,lower=m_lo), trace=trace, k=k)
  # print(m_fwd$call)
  return(m_fwd)
}

step_model_former_90 = stepforward_former(data_train1, sis_probes, nb_sis_probes = 90,trace=0)
# On regarde les résultats sur data_train2
predicted_probs <- predict(step_model_former_90, newdata = data_train2, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.55, 1, 0)
confusion_matrix <- table(Predicted = predicted_classes, Actual = data_train2$former01)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

# Fonction générale
predict_former <- function(data,step_model, seuil){
  predicted_probs <- predict(step_model, newdata = data, type = "response")
  predicted_classes <- ifelse(predicted_probs > seuil, 1, 0)
  return(predicted_classes)
}


# On crée des modèles pour l'hyperparamétrage.
create_modeles_former <- function(from = 50, to = 200, by = 10) {
  res <- list()
  for (i in seq(from = from, to = to, by = by)) {
    print(paste0(i, "/", to))
    res[[length(res) + 1]] <- stepforward_former(data_train1, sis_probes, nb_sis_probes = i, trace = 0)
  }
  return(res)
}
modeles_former <- create_modeles_former()

# Fonction pour l'hyper-paramétrage de former
hyperparametrage_former<- function(data, list_modeles_former, list_seuil_former){
  best_accuracy <- 0
  best_seuil_former <- 0
  best_num_model_former <- 1
  
  
  nb <- length(list_modeles_former)*length(list_seuil_former)
  i <- 0
  
  num_model_former <- 0
  for(step_model_former in list_modeles_former){
    num_model_former <- num_model_former +1
    for(seuil_former in list_seuil_former){
      i <- i+1
      if(i%%10 == 0){
        print(paste0(i,"/",nb))
      }
      confusion_matrix <- table(Predicted = predict_former(data,step_model_former,seuil_former), Actual = data$former01)
      accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      if(accuracy > best_accuracy){
        best_accuracy <- accuracy
        best_num_model_former <- num_model_former
        best_seuil_former <- seuil_former
      }
    }
  }
  return(c(best_accuracy,best_num_model_former,best_seuil_former))
}

hyperparametrage_former(data_train2,modeles_former,list(0.4,0.425,0.45,0.475,0.5,0.525,0.55,0.575,0.6))

# Res : 
# Accuracy : 0.7142
# Modèle n*10 -> 140 sondes
# Seuil : 0.575

# Comme current est le modèle avec les meilleurs résultats on va partir sur lui.
# On va maintenant essayer de créer 2 sous-modèles un pour former et un pour never entrainer SEULEMENT SUR LES GENS PAS CURRENT
# (c'est là différence avec les deux modèles du dessus).

# On va repartir de data_train1 et data_train2 mais en enlevant les gens qui sont current.
data_train1_notcurrent <- data_train1[data_train1$current01!=1,]
data_train2_notcurrent <- data_train2[data_train2$current01!=1,]
dim(data_train1)
dim(data_train1_notcurrent)
dim(data_train2)
dim(data_train2_notcurrent)

# Et on refait la sélection des sondes, les modèles step etc.

# NOT CURRENT - NEVER
# NEVER
model_never_sis_i = function(data_train, i, screening_func=msiscreening){
  print(paste0("model sis ", i))
  sis_res = screening_func(data_train)
  sis_probes = rownames(sis_res)[order(sis_res$pval_fisher)]
  formula = as.formula(paste0(c("never01~1", sis_probes[0:i]), collapse="+")) ; 
  m = glm(formula, data_train, family=binomial(link="logit"))
  return(m)
}

# On effectue un step 
stepforward_never = function(data_train, sis_probes, nb_sis_probes=200, trace=0, k=2) {
  m_lo = glm(never01 ~ 1, data=data_train[,c("never01", sis_probes[1:nb_sis_probes])])
  m_sup = glm(never01 ~ ., data=data_train[,c("never01", sis_probes[1:nb_sis_probes])])
  m_fwd = step(m_lo, method="forward", scope=list(upper=m_sup,lower=m_lo), trace=trace, k=k)
  # print(m_fwd$call)
  return(m_fwd)
}

# Exemple avec 90 sondes
step_model_notcurrent_never_90 = stepforward_never(data_train1_notcurrent, sis_probes, nb_sis_probes = 90,trace=0)

# On regarde les résultats sur data_train2
predicted_probs <- predict(step_model_notcurrent_never_90, newdata = data_train2_notcurrent, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)
confusion_matrix <- table(Predicted = predicted_classes, Actual = data_train2_notcurrent$never01)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))
# Accuracy : 0.5217

# Fonction générale
predict_notcurrent_never <- function(data,step_model, seuil){
  predicted_probs <- predict(step_model, newdata = data, type = "response")
  predicted_classes <- ifelse(predicted_probs > seuil, 1, 0)
  return(predicted_classes)
}

# On crée des modèles pour l'hyperparamétrage.
create_modeles_notcurrent_never <- function(from = 50, to = 200, by = 10) {
  res <- list()
  for (i in seq(from = from, to = to, by = by)) {
    print(paste0(i, "/", to))
    res[[length(res) + 1]] <- stepforward_never(data_train1_notcurrent, sis_probes, nb_sis_probes = i, trace = 0)
  }
  return(res)
}
modeles_notcurrent_never <- create_modeles_notcurrent_never()

# Fonction pour l'hyper-paramétrage de never
hyperparametrage_notcurrent_never<- function(data, list_modeles_never, list_seuil_never){
  best_accuracy <- 0
  best_seuil_never <- 0
  best_num_model_never <- 1
  
  
  nb <- length(list_modeles_never)*length(list_seuil_never)
  i <- 0
  
  num_model_never <- 0
  for(step_model_never in list_modeles_never){
    num_model_never <- num_model_never +1
    for(seuil_never in list_seuil_never){
      i <- i+1
      if(i%%10 == 0){
        print(paste0(i,"/",nb))
      }
      confusion_matrix <- table(Predicted = predict_never(data,step_model_never,seuil_never), Actual = data$never01)
      accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      if(accuracy > best_accuracy){
        best_accuracy <- accuracy
        best_num_model_never <- num_model_never
        best_seuil_never <- seuil_never
      }
    }
  }
  return(c(best_accuracy,best_num_model_never,best_seuil_never))
}

hyperparametrage_notcurrent_never(data_train2_notcurrent,modeles_notcurrent_never,list(0.4,0.425,0.45,0.475,0.5,0.525,0.55,0.575,0.6))
# Res
# Accuracy : 0.5652
# Modèle n*3 -> 70 sondes
# Seuil : 0.425

# NOT CURRENT - former
# former
model_former_sis_i = function(data_train, i, screening_func=msiscreening){
  print(paste0("model sis ", i))
  sis_res = screening_func(data_train)
  sis_probes = rownames(sis_res)[order(sis_res$pval_fisher)]
  formula = as.formula(paste0(c("former01~1", sis_probes[0:i]), collapse="+")) ; 
  m = glm(formula, data_train, family=binomial(link="logit"))
  return(m)
}

# On effectue un step 
stepforward_former = function(data_train, sis_probes, nb_sis_probes=200, trace=0, k=2) {
  m_lo = glm(former01 ~ 1, data=data_train[,c("former01", sis_probes[1:nb_sis_probes])])
  m_sup = glm(former01 ~ ., data=data_train[,c("former01", sis_probes[1:nb_sis_probes])])
  m_fwd = step(m_lo, method="forward", scope=list(upper=m_sup,lower=m_lo), trace=trace, k=k)
  # print(m_fwd$call)
  return(m_fwd)
}

# Exemple avec 90 sondes
step_model_notcurrent_former_90 = stepforward_former(data_train1_notcurrent, sis_probes, nb_sis_probes = 90,trace=0)

# On regarde les résultats sur data_train2
predicted_probs <- predict(step_model_notcurrent_former_90, newdata = data_train2_notcurrent, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)
confusion_matrix <- table(Predicted = predicted_classes, Actual = data_train2_notcurrent$former01)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))
# Accuracy : 0.5217

# Fonction générale
predict_notcurrent_former <- function(data,step_model, seuil){
  predicted_probs <- predict(step_model, newdata = data, type = "response")
  predicted_classes <- ifelse(predicted_probs > seuil, 1, 0)
  return(predicted_classes)
}

# On crée des modèles pour l'hyperparamétrage.
create_modeles_notcurrent_former <- function(from = 50, to = 200, by = 10) {
  res <- list()
  for (i in seq(from = from, to = to, by = by)) {
    print(paste0(i, "/", to))
    res[[length(res) + 1]] <- stepforward_former(data_train1_notcurrent, sis_probes, nb_sis_probes = i, trace = 0)
  }
  return(res)
}
modeles_notcurrent_former <- create_modeles_notcurrent_former()

# Fonction pour l'hyper-paramétrage de former
hyperparametrage_notcurrent_former<- function(data, list_modeles_former, list_seuil_former){
  best_accuracy <- 0
  best_seuil_former <- 0
  best_num_model_former <- 1
  
  
  nb <- length(list_modeles_former)*length(list_seuil_former)
  i <- 0
  
  num_model_former <- 0
  for(step_model_former in list_modeles_former){
    num_model_former <- num_model_former +1
    for(seuil_former in list_seuil_former){
      i <- i+1
      if(i%%10 == 0){
        print(paste0(i,"/",nb))
      }
      confusion_matrix <- table(Predicted = predict_former(data,step_model_former,seuil_former), Actual = data$former01)
      accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      if(accuracy > best_accuracy){
        best_accuracy <- accuracy
        best_num_model_former <- num_model_former
        best_seuil_former <- seuil_former
      }
    }
  }
  return(c(best_accuracy,best_num_model_former,best_seuil_former))
}

hyperparametrage_notcurrent_former(data_train2_notcurrent,modeles_notcurrent_former,list(0.4,0.425,0.45,0.475,0.5,0.525,0.55,0.575,0.6))
# Res
# Accuracy : 0.5652
# Modèle n*3 -> 70 sondes
# Seuil : 0.525
# C'est pas fou du tout ....

# Les deux ont la même accuracy pour le meilleur modèle après hyper-paramétrage
# Donc de manière arbitraire je choisi never

predict_current_never<- function(data,step_model_current,seuil_current,step_model_never,seuil_never){
  
  # Étape 1 : Initialisation du vecteur de prédictions
  n <- nrow(data)
  predictions <- rep(NA, times=n)
  
  # Étape 2 : Prédictions pour "current"
  is_current <- predict_current(data,step_model_current,seuil_current) == 1
  predictions[is_current] <- "current"
  
  # Étape 3 : Prédictions pour "never" parmi les non-classés
  remaining <- is.na(predictions)
  is_never <- predict_notcurrent_never(data[remaining, ],step_model_never,seuil_never) == 1
  predictions[which(remaining)[is_never]] <- "never"
  
  # Étape 4 : Tout le reste est "former"
  predictions[is.na(predictions)] <- "former"
  
  return(predictions)
}

model_current_final <- stepforward_current(data_train1, sis_probes, nb_sis_probes = 190,trace=0)

model_never_final <- stepforward_never(data_train1_notcurrent, sis_probes, nb_sis_probes = 70,trace=0)

predicted <- predict_current_never(data_train2, model_current_final,0.575,model_never_final,0.425)
confusion_matrix <- table(Predicted = predicted, Actual = data_train2$smoking_status)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))
# Accuracy : 0.59047619047619



