setwd("C:\\Users\\drmiller1220\\Documents\\GitHub\\PS5") #setting working directory

library(foreign) # need library foreign to read in .dta file

anes_data <- read.dta("anes_timeseries_2012_stata12.dta") # load in data

anes_data_subset <- subset(anes_data, select=c(ft_dpc,pid_x,tea_supp_x,aidblack_self,
                                               aidblack_dpc,presapp_job_x,
                                               interest_attention,dem_raceeth_x,
                                               gender_respondent_x))
# subsetting some variables to use for models

anes_data_subset$ft_dpc <- ifelse(anes_data_subset$ft_dpc<0, NA, anes_data_subset$ft_dpc)
# all values less than 0 for the thermometer are NAs, so we are recoding them as NAs

# NOTE: below, as we rename the levels of factors, all factors not renamed in the list
# become NAs, so we are implicitly recoding the negative values to NA as we go along

levels(anes_data_subset$pid_x) <- list("strong_dem"="1. Strong Democrat",
                                       "weak_dem"="2. Not very strong Democract",
                                       "ind_dem"="3. Independent-Democrat",
                                       "ind"="4. Independent",
                                       "ind_gop"="5. Independent-Republican",
                                       "weak_gop"="6. Not very strong Republican",
                                       "strong_gop"="7. Strong Republican")
# changing the factor values for PID to be more intuitive

levels(anes_data_subset$tea_supp_x) <- rev(list("strong_supp"="1. Strong support" ,
                                            "weak_supp"="2. Not very strong support",
                                            "lean_supp"="3. Lean toward supporting",
                                            "indiff"="4. Do not lean either way",
                                            "lean_opp"="5. Lean toward opposing",
                                            "weak_opp"="6. Not very strong opposition",
                                            "strong_opp"="7. Strong opposition"))
# changing the tea party support values to be more intuitive; using rev() to make the
# ordering more consistent with PID

levels(anes_data_subset$presapp_job_x) <- rev(list("strong_app"="1. Approve strongly",
                                               "weak_app"="2. Approve not strongly",
                                               "weak_disapp"="4. Disapprove not strongly",
                                               "strong_disapp"="5. Disapprove strongly"))
# changing the presidential approval levels to be more intuitive; using rev() to make level
# ordering more intuitive

levels(anes_data_subset$interest_attention) <- rev(list("always"="1. Always",
                                                    "most"="2. Most of the time",
                                                    "half"="3. About half the time",
                                                    "some"="4. Some of the time",
                                                    "never"="5. Never"))
# changing attention to politics levels to be more intuitive; using rev() to make level
# ordering more intuitive

levels(anes_data_subset$dem_raceeth_x) <- list("white"="1. White non-Hispanic",
                                               "black"="2. Black non-Hispanic",
                                               "hispanic"="3. Hispanic",
                                               "other"="4. Other non-Hispanic")
# changing the race and ethnicity levels to be more intuitive

levels(anes_data_subset$gender_respondent_x) <- list("male"="1. Male",
                                                     "female"="2. Female")
# changing gender levels to be more intuitive

anes_data_subset$aidblack_self <- ifelse(anes_data_subset$aidblack_self<0, NA, anes_data_subset$aidblack_self)
anes_data_subset$aidblack_dpc <- ifelse(anes_data_subset$aidblack_dpc<0, NA, anes_data_subset$aidblack_dpc)
# recoding the attitudes towards aid for AAs such that all negative values are NAs

anes_data_subset <- anes_data_subset[complete.cases(anes_data_subset),]
# removing all rows with an NA observation; committing evil casewise deletion

sample_rows <- sample(1:nrow(anes_data_subset), size = 0.5*nrow(anes_data_subset))
# get vector of numbers from 1 to the number of rows in the data set, and then sample
# a vector of numbers half that size with which to determine our training and test sets

training_set <- anes_data_subset[sample_rows,] # training set includes all rows sampled
test_set <- anes_data_subset[-sample_rows,] # test set includes all rows not sampled

# the models below have some basic political attitudes and demographic characteristics, and
# each model has a `fuller' model which includes the respondents' own attitude towards
# aid for African-Americans, as well as their impression of Obama's attitude towards aid
# for African-Americans.  We are interested in seeing if the inclusion of racial attitudes
# improves model performance

# fitting an lm model including all subsetted variables except the aidblack for the
# respondent and Obama
lm_basic <- lm(ft_dpc ~ pid_x + tea_supp_x + presapp_job_x + interest_attention +
                 dem_raceeth_x + gender_respondent_x, data=training_set)
summary(lm_basic)

# fitting an lm model including all subsetted variables including the aidblack for the
# respondent and Obama
lm_race <- lm(ft_dpc ~ pid_x + tea_supp_x + presapp_job_x + interest_attention +
                dem_raceeth_x + gender_respondent_x + aidblack_self +
                aidblack_dpc, data=training_set)
summary(lm_race)

library(VGAM) # loading package needed for tobit regression

# we might want to use a tobit regression because our DV is censored at 0 and 100; OLS
# models could yield predictions outside of that range, while tobit models will
# yield censored predictions.  This might improve model performance, as OLS predictions
# outside the range [0,100] will create prediction error

# fitting a tobit model including all subsetted variables except the aidblack for the
# respondent and Obama
tobit_basic <- vglm(ft_dpc ~ pid_x + tea_supp_x + presapp_job_x + interest_attention +
                      dem_raceeth_x + gender_respondent_x, 
                    tobit(Lower = 0, Upper = 100, type.fitted="censored"),
                    data=training_set)
summary(tobit_basic)

# fitting a tobit model including all subsetted variables including the aidblack for the
# respondent and Obama
tobit_race <- vglm(ft_dpc ~ pid_x + tea_supp_x + presapp_job_x + interest_attention +
                     dem_raceeth_x + gender_respondent_x + aidblack_self +
                     aidblack_dpc, tobit(Lower = 0, Upper = 100, type.fitted="censored"),
                   data=training_set)
summary(tobit_race)

# storing predicted values based on test set observations; output of predict for tobit
# model is a data.frame, but is a vector for lm; for consistency, and for later use,
# we vectorize the tobit predicted values
lm_basic_predict <- predict(lm_basic, test_set, type="response")
lm_race_predict <- predict(lm_race, test_set, type="response")
tobit_basic_predict <- as.vector(predict(tobit_basic, test_set, type="response"))
tobit_race_predict <- as.vector(predict(tobit_race, test_set, type="response"))

# storing the true values in their own vectpr
true_values <- test_set$ft_dpc

predictions <- matrix(data=c(lm_basic_predict, lm_race_predict, tobit_basic_predict,
                             tobit_race_predict), ncol=4, 
                      dimnames=list(NULL, c("lm_basic_predict", "lm_race_predict", 
                                            "tobit_basic_predict",
                                            "tobit_race_predict"))) 
# creating matrix of predictions by placing each prediction vector in the matrix, and
# naming the columns appropriately

naive_values <- rep(median(test_set$ft_dpc), length(test_set$ft_dpc))
# creating a vector of naive values, which is a vector of the length of the feeling
# thermometers in the test data set with the value as the median thermometer rating.
# by comparing the naive values to the predicted values, we will be able to see how
# much better our model can predict the true value compared to just using the median

###############

# before continuing, compile and install package using documentation file

library(fitR) # loading in package

# calculating only non-mrae fit statistics
stat_results <- fitRstats(true=true_values, predicted=predictions,
                          statistics = c("rmse","mad","meape","rmsle","mape"))

# calculating all fit statistics
stat_results <- fitRstats(true=true_values, predicted=predictions, naive= naive_values,
                          statistics = c("rmse","mad","meape","rmsle","mape","mrae"))

# throwing error if mrae is requested without supplying naive forecasts
stat_results <- fitRstats(true=true_values, predicted=predictions,
                          statistics = c("rmse","mad","meape","rmsle","mape","mrae"))

# throwing error if input is of front class
stat_results <- fitRstats(true=c("c",2,3,"d"), predicted=predictions, naive= naive_values,
                          statistics = c("rmse","mad","meape","rmsle","mape","mrae"))

# throwing warning if some values are NA
stat_results <- fitRstats(true=c(true_values[1:(length(true_values)-1)],NA), 
                          predicted=predictions, naive= naive_values,
                          statistics = c("rmse","mad","meape","rmsle","mape","mrae"))

# using the function from the package to obtain the fit statistics

# having fit the models, we look at the six fit statistics calculated across models.
# we see that, surprisingly, the OLS models tend to perform better than the tobit models.
# the OLS models have lower values for rmse, mape, meape, and mrae than do the tobit models,
# while the tobit models have lower values for mad and rmsle.  Additionally, the ``fuller"
# models almost universally perform better than their reduced form counterparts, which makes
# sense as the additional variables should explain some additional variance and lead to
# better predictions