# # Write a function to predict test set train set
#
# In the provided example, we use a naive method to generate the baseline prediction.


#' The prediction function
#'
#' @param data_train a matrix
#' @param data_test a matrix
#' @return the estimated parameters
#' @examples
#' program(data_train = data_train, data_test = data_test)
program <- function(data_train, data_test) {
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
    
    # NEVER
    data_train_notcurrent <- data_train[data_train$current01!=1,]
    
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
      # print(m_fwd$call)
      return(m_fwd)
    }
    
    predict_notcurrent_never <- function(data,step_model, seuil){
      predicted_probs <- predict(step_model, newdata = data, type = "response")
      predicted_classes <- ifelse(predicted_probs > seuil, 1, 0)
      return(predicted_classes)
    }
    
    model_never_final <- stepforward_never(data_train_notcurrent, sis_probes, nb_sis_probes = 70,trace=0)
    
    # Mix
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
    
    data_pred <- predict_current_never(data_test, model_current_final,0.575,model_never_final,0.425)
    
    ##
    ## YOUR CODE ENDS HERE
    ##

    return(data_pred)
    
}


###########
# Authors
# florent.chuffard@univ-grenoble-alpes.fr
# slim.karkar@univ-grenoble-alpes.fr
# magali.richard@univ-grenoble-alpes.fr
# ---

########################################################
### Package dependencies /!\ DO NOT CHANGE THIS PART ###
########################################################

##  to generate the zip files that contain the programs or the results to submit to the Codalab platform.

if ( !exists(x = "params") ) {
    params                      <- NULL
}
if ( is.null(x = params$package_repository) ) {
    params$package_repository   <- "https://cloud.r-project.org"
}
if ( is.null(x = params$install_dependencies) ) {
    params$install_dependencies <- TRUE
}
print(params)
if ( params$install_dependencies ) {
    installed_packages <- installed.packages( )
    for (package in c("zip") ) {
        if ( !{ package %in% installed_packages } ) {
            print(x = paste("Installation of ", package, sep = "") )
            install.packages(
                pkgs = package
              , repos = params$package_repository
            )
        } else {
            print(x = paste(package, " is installed.", sep = "") )
        }
    }
    remove(list = c("installed_packages", "package") )
}
# -

####################################################
### Submission modes /!\ DO NOT CHANGE THIS PART ###
####################################################

# Participants can submit either
# - a code program, that will be executed on the challenge platform to generate the result (prediction) that will be scored against the ground truth
# - a result (prediction )file, that will be scored against the ground truth

###############################
### Code submission mode
# Participants need make a zip file (no constrain on the namefile) that contains :
#   - the file `metadata` that is generated by this script
#   - your code inside a *R* file named `program.R`. This file will be sourced and have to contain :
#   - a function `program` with `data_train` and `data_test` as argument : a matrix associated to your prediction.
#   - any other files that you want to access from your function `program` : during the ingestion phase (when your code is evaluated), the working directory will be inside the directory obtained by unzipping your submission.

###############################
### Result submission mode  
# Participants have to make a zip file (no constrain on the namefile), with your results as a matrix inside a rds file named `results.txt`.

##################################################################################################
### Submission modes /!\ EDIT THE FOLLOWING CODE BY COMMENTING/UNCOMMENTING THE REQUIRED PARTS ###
##################################################################################################


## define the public data set that you will use for your prediction :
data_train <- readRDS(file = "data_train.rds")
data_test <- readRDS(file = "data_test.rds")



##############################################################
### Generate a submission file /!\ DO NOT CHANGE THIS PART ###
##############################################################

# we use the previously defined function 'program' to estimate A :
data_pred <- program(
  data_train = data_train,
  data_test = data_test
)

###############################
### Code submission mode

# we generate a zip file with the 'program' source code

if ( !dir.exists(paths = "submissions") ) {
    dir.create(path = "submissions")
}

# we save the source code as a R file named 'program.R' :
dump(
    list = c("program")
  , file = paste0("submissions", .Platform$file.sep, "program.R")
)

# we also generate the 'metadata' file
cat(
    "command: Rscript $program/program.R $input $output"
  , file = paste0("submissions", .Platform$file.sep, "metadata")
)

date_suffix = format(x = Sys.time( ), format = "%Y_%m_%d_%H_%M_%S")

# we create the associated zip file :
zip_program <- paste0("submissions", .Platform$file.sep, "program_", date_suffix, ".zip")
zip::zipr(
         zipfile = zip_program
       , files   = paste0("submissions", .Platform$file.sep, c("program.R", "metadata") )
     )
print(x = zip_program)

###############################
### Result submission mode  

#  Generate a zip file with the prediction

# + label="Submission - results zip file" echo=true results="verbatim"
if ( !dir.exists(paths = "submissions") ) {
    dir.create(path = "submissions")
}

## we save the estimated A matrix as a rds file named 'results.txt' :
# saveRDS(
#     object = data_pred
#   , file   = paste0("submissions", .Platform$file.sep, "results.rds")
# )

write.table(
  x=data_pred, 
  file= paste0("submissions", .Platform$file.sep, "results.txt"),
  quote=FALSE,
  row.names=FALSE,
  col.names=FALSE
)


## we create the associated zip file :
zip_results <- paste0("submissions", .Platform$file.sep, "results_", date_suffix, ".zip")
zip::zipr(
         zipfile = zip_results
       , files   = paste0("submissions", .Platform$file.sep, c("results.txt") )
     )
print(x = zip_results)

sessionInfo( )

###############################################################
### How to submit the zip file? /!\ DO NOT CHANGE THIS PART ###
###############################################################
#
# The code above generates the files *`r zip_program`*  and *`r zip_results`*  (the 1st one for code submission, the 2nd one for result submission).
#
# Submit the zip submission file on the challenge in the tab `Participate`, menu `Submit / View Results` menu, sub-menu `CHALLENGE #1`, by clicking the `Submit` button after filling out some metadatas.
#
# On the codalab challenge web page, The *STATUS* become : 
#   - Submitting
#   - Submitted	
#   - Running 
#   - Finished
#
# When it’s finished :
#   - You refresh the page and see your score
#   - If enable, details for report could be downloaded by clicking *Download output from scoring step*.
#   - Some logs are available to download.
#   - Leader board is updated in the `Results` tab.
#


