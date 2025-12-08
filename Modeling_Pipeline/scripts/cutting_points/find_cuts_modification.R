



################################################################
## Especificaciones de A (instancias AAPS)
raw_data_path <- "./Modeling_Pipeline/data/raw/AAPS Score Data (Long Version).csv"
phoneme_grouping_data_path <- "./Modeling_Pipeline/phoneme_grouping/phoneme_grouping2.csv"
subset_data_grouping_path <- "./Modeling_Pipeline/instance_specification/subset_data_grouping2.csv"
#instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit3.csv" #--->pilas usar con grouping1
## MODIFICAR ACORDE:
instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit_cp1V2.csv"

raw_data_type <- "aaps" # coherente con raw_data_path
phoneme_grouping_type <- "grouping2" # coherente con phoneme_grouping_data_path y con subset_data_grouping_path
model_type  <- "binomial" # debemos hablar mejor de response variable, porque esto detemrina el tipo de preprocesamiento 
# y el tipo de modelos que podemos usar. model_type debe ser el mismo que aparece en la columna model_type para todas las filas
# de instance_to_fit.csv!!

list_of_instancesA <- read_instances_specifications_lib$read_instances_specifications(instance_to_fit_path, subset_data_grouping_path,phoneme_grouping_data_path)

# Read preprocessed of raw_data used for instances A
preprocessed_result_listA <-read_preprocessed_files(raw_data_type,model_type,phoneme_grouping_type)
df_finalA <- preprocessed_result_listA$df_final
phoneme_numscore_modeA <- preprocessed_result_listA$phoneme_numscore_mode




## Especificaciones de B (instancias pllr)
raw_data_path <- "./Modeling_Pipeline/data/raw/probabilities-max-frame_W.csv.gz"
phoneme_grouping_data_path <- "./Modeling_Pipeline/phoneme_grouping/phoneme_grouping2.csv"
subset_data_grouping_path <- "./Modeling_Pipeline/instance_specification/subset_data_grouping2.csv"
#instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit2.csv" #---> pilas usar con grouping1
#dependiendo de si beta o binomial para pllr
instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit_cp2.csv"
#instance_to_fit_path <- "./Modeling_Pipeline/instance_specification/instance_to_fit_cp2V2.csv"

raw_data_type <- "pllr" # coherente con raw_data_path
phoneme_grouping_type <- "grouping2" # coherente con phoneme_grouping_data_path y con subset_data_grouping_path
model_type  <- "beta" 
#model_type  <- "binomial"
# debemos hablar mejor de response variable, porque esto detemrina el tipo de preprocesamiento 
# y el tipo de modelos que podemos usar. model_type debe ser el mismo que aparece en la columna model_type para todas las filas
# de instance_to_fit.csv!!


list_of_instancesB <- read_instances_specifications_lib$read_instances_specifications(instance_to_fit_path, subset_data_grouping_path,phoneme_grouping_data_path)

# Read preprocessed of raw_data used for instances B
preprocessed_result_listB <-read_preprocessed_files(raw_data_type,model_type,phoneme_grouping_type)
df_finalB <- preprocessed_result_listB$df_final
phoneme_numscore_modeB <- preprocessed_result_listB$phoneme_numscore_mode

agerangeA <- range(df_finalA$age_months)
agerangeB <- range(df_finalB$age_months)
agerange <- c(agerangeA[1],agerangeB[2])

# Probemos con
# no 8, no 4
k<- 10
instancia_pruebaA <- list_of_instancesA[[k]]

message("agerangeA (aaps): ")
print(agerangeA)
model_typeA<- "binomial"
fitted_modelA <- readRDS(paste0(instancia_pruebaA$fitted_model_file_path,".rds"))
success_frac <- 1.0            # f in [0,1]
target_rule <- "floor"  # how to turn f*n into k


extract_q_result <- extract_q_aaps_binomial(model_typeA,
                                            phoneme_numscore_modeA, #only used for binomial                                        
                                            #agerangeA,
                                            #agerangeB,
                                            agerange,
                                            instancia_pruebaA,
                                            fitted_modelA,
                                            success_frac,
                                            target_rule
)
q_atleast1_ppc<- extract_q_result$q_atleast1_ppc
q_exact1_ppc<- extract_q_result$q_exact1_ppc
q_atleast_k_ppc<- extract_q_result$q_atleast_k_ppc
q_exact_k_ppc<-extract_q_result$q_exact_k_ppc
plot_post_mean<- extract_q_result$plot_post_mean

instancia_pruebaB <- list_of_instancesB[[k]]
agerangeB <- range(df_finalB$age_months)
message("agerangeB (pllr): ")
print(agerangeB)
model_typeB<- "beta"
#model_typeB<- "binomial"
fitted_modelB <- readRDS(paste0(instancia_pruebaB$fitted_model_file_path,".rds"))

crow_joined <- read_crow_mcleod()

list_extracted_x <- extract_x_q_pllr_beta(model_typeB,
                                          phoneme_numscore_modeB, #only used for binomial                                        
                                          #agerangeB,
                                          #agerangeA,
                                          agerange,
                                          instancia_pruebaB,
                                          fitted_modelB,
                                          q_exact_k_ppc,
                                          #q_atleast1_ppc
                                          #q_exact1_ppc
                                          crow_joined # sept22
)

xq_predictive<- list_extracted_x$xq_predictive
xq_mu <- list_extracted_x$xq_mu

age_plot <- list_extracted_x$age_plot
age_plot_crow <-list_extracted_x$age_plot_crow
age_plot_crow2 <-list_extracted_x$age_plot_crow2
age_plot_crow3<-list_extracted_x$age_plot_crow3
age_plot_crow4<-list_extracted_x$age_plot_crow4
age_plot_crow5<-list_extracted_x$age_plot_crow5
xq_predictive_crow<- list_extracted_x$xq_predictive_crow

message("xq_predictive:")
print(xq_predictive)
#message("xq_mu:")
#print(xq_mu)
age_plot
plot_post_mean
age_plot_crow
age_plot_crow2
age_plot_crow3
age_plot_crow4
age_plot_crow5