import("loo")

export("model_validation")
export("iterate_model_validation")



#model_validation <- function(category, levels, prefix) {
model_validation <- function(model_opt, category, levels) {
  #include type of validation ?
  prefix <- paste("./data/processed_data/", model_opt, "/", sep="")
  print(prefix)
  phoneme_group_str <- paste(c(category, levels), collapse = "_")
  model_id = paste0(prefix, "model_", phoneme_group_str)
  print(model_id)
  tmp_env_data <- new.env()
  loaded_model_objc <- load(paste0(model_id,".RData"), 
                                envir = tmp_env_data)
  model <- tmp_env_data[[loaded_model_objc[1]]] 
  ls()
  
  loo <- loo(model)
  waic <-waic(model,moment_match = TRUE)
  dict_validation <- list( model_id = model_id,
                           model_opt = model_opt,
                           phoneme_group_str = phoneme_group_str,
                           waic = waic,
                           #kfold10 = kfold(model,k=10),
                           loo = loo
                          )
  
  return(dict_validation)
}

#category <- "Vowels"
#levels <- c("Level1", "Level2" )#,"Level5")
#dict_validation <- model_validation(category, levels, prefix)
#print(dict_validation)


#create a function that iterate over a list of models, compute the scores

iterate_model_validation <- function(list_to_validate){
  results <- list()
  for (item in list_to_validate){
    #prefix <- paste("./data/processed_data/", item["model_opt"], "/", sep="")
    #print(prefix)
    dict_validation <- model_validation(item["model_opt"], item[["category"]], item[["levels"]])
    results[[dict_validation[["model_id"]]]] = dict_validation
  }
  # Save the corresponding dictionary in an external file .rds
  saveRDS(results, file = "./bayesian_code/model_validation/model_validation_results3.rds")
  return(results)
}