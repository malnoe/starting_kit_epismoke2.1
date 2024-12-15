program <-
function(data_train, data_test) {
    ##
    ## YOUR CODE BEGINS HERE
    ## 
    print("C'est parti.")
    
    probes = colnames(data_train)[5:10004]
    
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
    sis_res = msiscreening(data_train) # Dataframe résutlat
    sis_probes = rownames(sis_res)[order(sis_res$pval_fisher)]
    
    # never
    print("never")
    model_never_sis_i = function(data_train, i, screening_func=msiscreening){
      print(paste0("model sis ", i))
      sis_res = screening_func(data_train)  
      sis_probes = rownames(sis_res)[order(sis_res$pval_fisher)]
      formula = as.formula(paste0(c("never01~1", sis_probes[0:i]), collapse="+")) ; 
      m = glm(formula, data_train, family=binomial(link="logit"))
      return(m)
    }
    
    stepforward_never = function(data_train, sis_probes, nb_sis_probes=200, trace=0, k=2) {
      m_lo = glm(never01 ~ 1, data=data_train[,c("never01", sis_probes[1:nb_sis_probes])])
      m_sup = glm(never01 ~ ., data=data_train[,c("never01", sis_probes[1:nb_sis_probes])])
      m_fwd = step(m_lo, method="forward", scope=list(upper=m_sup,lower=m_lo), trace=trace, k=k)
      return(m_fwd)
    }
    
    predict_never <- function(data,step_model, seuil){
      predicted_probs <- predict(step_model, newdata = data, type = "response")
      predicted_classes <- ifelse(predicted_probs > seuil, 1, 0)
      return(predicted_classes)
    }
    
    model_never_final <- stepforward_never(data_train, sis_probes, nb_sis_probes = 190,trace=0)
    
    # current
    print("current")
    data_train_notnever <- data_train[data_train$never01!=1,]
    
    model_current_sis_i = function(data_train, i, screening_func=msiscreening){
      print(paste0("model sis ", i))
      sis_res = screening_func(data_train)
      sis_probes = rownames(sis_res)[order(sis_res$pval_fisher)]
      formula = as.formula(paste0(c("current01~1", sis_probes[0:i]), collapse="+")) ; 
      m = glm(formula, data_train, family=binomial(link="logit"))
      return(m)
    }
    
    stepforward_current = function(data_train, sis_probes, nb_sis_probes=200, trace=0, k=2) {
      m_lo = glm(current01 ~ 1, data=data_train[,c("current01", sis_probes[1:nb_sis_probes])])
      m_sup = glm(current01 ~ ., data=data_train[,c("current01", sis_probes[1:nb_sis_probes])])
      m_fwd = step(m_lo, method="forward", scope=list(upper=m_sup,lower=m_lo), trace=trace, k=k)
      # print(m_fwd$call)
      return(m_fwd)
    }
    
    predict_notnever_current <- function(data,step_model, seuil){
      predicted_probs <- predict(step_model, newdata = data, type = "response")
      predicted_classes <- ifelse(predicted_probs > seuil, 1, 0)
      return(predicted_classes)
    }
    
    model_current_final <- stepforward_current(data_train_notnever, sis_probes, nb_sis_probes = 70,trace=0)
    
    # Mix
    print("Mix")
    predict_never_current<- function(data,step_model_never,seuil_never,step_model_current,seuil_current){
      
      # Étape 1 : Initialisation du vecteur de prédictions
      n <- nrow(data)
      predictions <- rep(NA, times=n)
      
      # Étape 2 : Prédictions pour "never"
      is_never <- predict_never(data,step_model_never,seuil_never) == 1
      predictions[is_never] <- "never"
      
      # Étape 3 : Prédictions pour "current" parmi les non-classés
      remaining <- is.na(predictions)
      is_current <- predict_notnever_current(data[remaining, ],step_model_current,seuil_current) == 1
      predictions[which(remaining)[is_current]] <- "current"
      
      # Étape 4 : Tout le reste est "current"
      predictions[is.na(predictions)] <- "current"
      
      return(predictions)
    }
    
    print("Maintenant on prédit")
    data_pred <- predict_never_current(data_test, model_never_final,0.575,model_current_final,0.425)
    
    ##
    ## YOUR CODE ENDS HERE
    ##

    return(data_pred)
    
}
