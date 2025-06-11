import("tidyr")
import("purrr")

model_names_list <- list(
  "model3",
  "model4"
)

data1<-list(
  category = "Vowels",
  levels = c("Level1", "Level2")
)

data2<-list(
  category = "Vowels",
  levels = c("Level4", "Level5")
)

data3<-list(
  category = "Vowels",
  levels = c("Level3")
)

data4<-list(
  category = "Consonants",
  levels = c("Level3")
)

data5<-list(
  category = "Consonants",
  levels = c("Level4")
)

data6<-list(
  category = "Consonants",
  levels = c("Level5")
)

data7<-list(
  category = "Consonants",
  levels = c("Level6")
)
########################################################################
data_list_to_validate <- list(
  data1, data2, data3, data4, data5, data6, data7
)

# Create the Cartesian product
combined_model_data_list <- expand.grid(
  model_opt = model_names_list,
  data_index = seq_along(data_list_to_validate),
  stringsAsFactors = FALSE
)

# grid
model_data_grid <- expand_grid(
  model_opt = model_names_list,
  data_index = seq_along(data_list_to_validate)
)

# list
list_to_validate <- pmap(model_data_grid, function(model_opt, data_index) {
  data_entry <- data_list_to_validate[[data_index]]
  list(
    model_opt = model_opt,
    category = data_entry$category,
    levels = data_entry$levels
  )
})

print(list_to_validate)
export("return_dict_exp")
return_dict_exp = function(){
  return(list_to_validate)
}