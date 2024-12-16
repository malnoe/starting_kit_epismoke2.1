program <-
function(data_train, data_test) {
    ##
    ## YOUR CODE BEGINS HERE
    ## 
    
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
    
    model_current_sis_i = function(data_train, i, screening_func=msiscreening){
      print(paste0("model sis ", i))
      sis_res = screening_func(data_train)  
      sis_probes = rownames(sis_res)[order(sis_res$pval_fisher)]
      formula = as.formula(paste0(c("current01~1", sis_probes[0:i]), collapse="+")) ; 
      m = glm(formula, data_train, family=binomial(link="logit"))
      return(m)
    }
    
    # CURRENT
    stepforward_current = function(data_train, sis_probes, nb_sis_probes=200, trace=0, k=2) {
      m_lo = glm(current01 ~ 1, data=data_train[,c("current01", sis_probes[1:nb_sis_probes])])
      m_sup = glm(current01 ~ ., data=data_train[,c("current01", sis_probes[1:nb_sis_probes])])
      m_fwd = step(m_lo, method="forward", scope=list(upper=m_sup,lower=m_lo), trace=trace, k=k)
      return(m_fwd)
    }
    
    predict_current <- function(data,step_model, seuil){
      predicted_probs <- predict(step_model, newdata = data, type = "response")
      predicted_classes <- ifelse(predicted_probs > seuil, 1, 0)
      return(predicted_classes)
    }
    
    model_current_final <- stepforward_current(data_train, sis_probes, nb_sis_probes = 190,trace=0)
    
    # former
    data_train_notcurrent <- data_train[data_train$current01!=1,]
    
    model_former_sis_i = function(data_train, i, screening_func=msiscreening){
      print(paste0("model sis ", i))
      sis_res = screening_func(data_train)
      sis_probes = rownames(sis_res)[order(sis_res$pval_fisher)]
      formula = as.formula(paste0(c("former01~1", sis_probes[0:i]), collapse="+")) ; 
      m = glm(formula, data_train, family=binomial(link="logit"))
      return(m)
    }
    
    stepforward_former = function(data_train, sis_probes, nb_sis_probes=200, trace=0, k=2) {
      m_lo = glm(former01 ~ 1, data=data_train[,c("former01", sis_probes[1:nb_sis_probes])])
      m_sup = glm(former01 ~ ., data=data_train[,c("former01", sis_probes[1:nb_sis_probes])])
      m_fwd = step(m_lo, method="forward", scope=list(upper=m_sup,lower=m_lo), trace=trace, k=k)
      # print(m_fwd$call)
      return(m_fwd)
    }
    
    predict_notcurrent_former <- function(data,step_model, seuil){
      predicted_probs <- predict(step_model, newdata = data, type = "response")
      predicted_classes <- ifelse(predicted_probs > seuil, 1, 0)
      return(predicted_classes)
    }
    
    model_former_final <- stepforward_former(data_train_notcurrent, sis_probes, nb_sis_probes = 100,trace=0)
    
    # Mix
    predict_current_former<- function(data,step_model_current,seuil_current,step_model_former,seuil_former){
      
      # Étape 1 : Initialisation du vecteur de prédictions
      n <- nrow(data)
      predictions <- rep(NA, times=n)
      
      # Étape 2 : Prédictions pour "current"
      is_current <- predict_current(data,step_model_current,seuil_current) == 1
      predictions[is_current] <- "current"
      
      # Étape 3 : Prédictions pour "former" parmi les non-classés
      remaining <- is.na(predictions)
      is_former <- predict_notcurrent_former(data[remaining, ],step_model_former,seuil_former) == 1
      predictions[which(remaining)[is_former]] <- "former"
      
      # Étape 4 : Tout le reste est "never"
      predictions[is.na(predictions)] <- "never"
      
      return(predictions)
    }
    
    data_pred <- predict_current_former(data_test, model_current_final,0.575,model_former_final,0.275)
    
    ##
    ## YOUR CODE ENDS HERE
    ##

    return(data_pred)
    
}
