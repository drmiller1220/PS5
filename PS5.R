setwd("C:\\Users\\drmiller1220\\Documents\\GitHub\\PS5") #setting working directory

library(foreign) # need library foreign to read in .dta file

anes_data <- read.dta("anes_timeseries_2012_stata12.dta") # load in data

anes_data_subset <- subset(anes_data, select=c(ft_dpc,pid_x,tea_supp_x,aidblack_self,
                                             aidblack_dpc,presapp_job_x,
                                             interest_attention,dem_raceeth_x,
                                             gender_respondent_x))

levels(anes_data_subset$pid_x) <- list("strong_dem"="1. Strong Democrat",
                                       "weak_dem"="2. Not very strong Democract",
                                       "ind_dem"="3. Independent-Democrat",
                                       "ind"="4. Independent",
                                       "ind_gop"="5. Independent-Republican",
                                       "weak_gop"="6. Not very strong Republican",
                                       "strong_gop"="7. Strong Republican")
anes_data_subset$pid_x <- as.ordered(anes_data_subset$pid_x)

levels(anes_data_subset$tea_supp_x) <- list("strong_supp"="1. Strong support" ,
                                       "weak_supp"="2. Not very strong support",
                                       "lean_supp"="3. Lean toward supporting",
                                       "indiff"="4. Do not lean either way",
                                       "lean_opp"="5. Lean toward opposing",
                                       "weak_opp"="6. Not very strong opposition",
                                       "strong_opp"="7. Strong opposition")
anes_data_subset$tea_supp_x <- as.ordered(anes_data_subset$tea_supp_x)

levels(anes_data_subset$presapp_job_x) <- list("strong_app"="1. Approve strongly",
                                            "weak_app"="2. Approve not strongly",
                                            "weak_disapp"="4. Disapprove not strongly",
                                            "strong_disapp"="5. Disapprove strongly")
anes_data_subset$presapp_job_x <- as.ordered(anes_data_subset$presapp_job_x)

levels(anes_data_subset$interest_attention) <- list("always"="1. Always",
                                               "most"="2. Most of the time",
                                               "half"="3. About half the time",
                                               "some"="4. Some of the time",
                                               "never"="5. Never")
anes_data_subset$interest_attention <- as.ordered(anes_data_subset$interest_attention)

levels(anes_data_subset$dem_raceeth_x) <- list("white"="1. White non-Hispanic",
                                          "black"="2. Black non-Hispanic",
                                          "hispanic"="3. Hispanic",
                                          "other"="4. Other non-Hispanic")

levels(anes_data_subset$gender_respondent_x) <- list("male"="1. Male",
                                               "female"="2. Female")

anes_data_subset$aidblack_self <- ifelse(anes_data_subset$aidblack_self<0, NA, anes_data_subset$aidblack_self)
anes_data_subset$aidblack_dpc <- ifelse(anes_data_subset$aidblack_dpc<0, NA, anes_data_subset$aidblack_self)

library(mice)

data_sets <- mice(anes_data_subset, m=5)

sample_rows <- sample(1:nrow(anes_data), size = 0.5*nrow(anes_data))
# get vector of numbers from 1 to the number of rows in the data set, and then sample
# a vector of numbers half that size with which to determine our training and test sets

training_set <- anes_data[sample_rows,] # training set includes all rows sampled
test_set <- anes_data[-sample_rows,] # test set includes all rows not sampled

library(VGAM) # loading package needed for tobit regression

tobit_model <- vglm(ft_dpc ~ pid_x, tobit(Lower = 0, Upper = 100, type.fitted="censored"), data=training_set)
summary(tobit_model)
predict(tobit_model, training_set, type="response")
