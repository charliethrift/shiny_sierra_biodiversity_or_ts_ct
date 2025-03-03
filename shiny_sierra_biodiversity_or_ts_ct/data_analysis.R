# Data Analysis Script

# This script runs the analysis for the R shiny app. We will be conducting a lasso 
# to predict lifeform type (tree, shrub, forb) of plant observations from Calflora, 
# Global Information Facilities Database (GBIF), and iNaturalist using climate variables
# impact by fire, topography, historic land cover, and land occupation type.

# Psuedocode:

# 1. Wrangle spatial data 
#  - climate 
#  - landcover
#  - fire perimeters 
#  - land occupation 

# 2. Ensure that all columns are numeric 

# 3. Run lasso model 

# 4. Utilize best predicted model to calculate AUC score, p-value, significance 
#    of each independent variable.

# 5. Visualize results

# Installing packages:
librarian::shelf(prism, here, geodata, sf)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Importing Data

# Prism
prism_set_dl_dir(here("data/climate"))
get_prism_normals(type = "tmax", resolution = "800m", annual = TRUE, keepZip = FALSE)
get_prism_normals(type = "ppt", resolution = "800m", annual = TRUE, keepZip = FALSE)
get_prism_normals(type = "vpdmax", resolution = "800m", annual = TRUE, keepZip = FALSE)

# Reading in PRISM



# Reading in fire perimeters 
fire <- read_st(here("data/fire perimeters", "mtbs_perims_DD.shp"))


# Topography
# use geodata for this 


# Plant observations (GBIF)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Binding Data with GBIF occurrence data 

# Extract raster data for each point 







#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Running Lasso



# Check balance of  plant life form column
plant_df |>
  group_by(life_form) |>
  summarize(n = n()) |>
  ungroup() |>
  mutate(prop = n / sum(n))

# Split data into test and train 

set.seed(345)

plant_split <- initial_split(plant_df, prop = 0.80, strata = life_form)

plant_train_df <- training(plant_split)
plant_test_df <- testing(plant_split)

# Checking the proportion of each split

plant_train_df |>
  group_by(life_form) |>
  summarize(n = n()) |>
  ungroup() |>
  mutate(prop = n / sum(n))

plant_test_df |>
  group_by(life_form) |>
  summarize(n = n()) |>
  ungroup() |>
  mutate(prop = n / sum(n))

# building our lasso model 
lasso_md<- logistic_reg(penalty = 0.037, mixture=1) |>
  set_engine("glmnet")

# adding the recipe and our variables

glm_rec<-recipe(life_form ~ fire + tmax + ppt +vpdmax + gap_code + county, data = plant_train_df)

lasso_rec<-recipe(life_form~.,data=plant_train_df) |>
  update_role(fire, new_role = "fire") |>
  step_rm(gap_code, county) |> # looking into this section 
  step_unknown(all_nominal(),-all_outcomes()) |> 
  step_dummy(all_nominal(),-all_outcomes()) |>
  step_zv(all_numeric(),-all_outcomes()) |> 
  step_normalize(all_numeric(),-all_outcomes())

# Fitting the lasso model 

# adding the model and recipe to the workflow
lasso_wf<-workflow() |>
  add_model(lasso_md) |>
  add_recipe(lasso_rec)

# fitting the lasso to the training data set 
lasso_fit<-lasso_wf |>
  fit(surv_train_df)

# predicting the testing data set 
lasso_test<-surv_test_df |>
  mutate(predict(lasso_fit, new_data = plant_test_df)) |> 
  mutate(predict(lasso_fit,new_data = plant_test_df, type='prob'))

# Seeing how the model did 
table(lasso_test$survived, lasso_test$.pred_class)

#______________________________________________________________________________#
# running the model over multiple folds
folds<-vfold_cv(plant_train_df, v=10, strata = life_form)

lasso_res<-lasso_wf |>
  fit_resamples(folds)

collect_metrics(lasso_res)

# Looking at the final lasso model 
lasso_fit |>
  extract_fit_parsnip() |> 
  tidy()

# fitting lasso to whole data set
final_lasso <-lasso_wf |>
  last_fit(plant_split)

# extracting variables to visualize how well our model did in predicting 
# the whole data set
final_lasso|>
  extract_fit_parsnip() |>
  tidy() |> 
  mutate(odds=exp(estimate),
         prob=odds/(1+estimate))

# plotting our final model 
