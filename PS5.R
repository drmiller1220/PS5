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

tobit_model <- with(data_sets, vglm(ft_dpc ~ pid_x, tobit(Lower = 0, Upper = 100, type.fitted="censored")))
summary(pool(tobit_model))


sample_rows <- sample(1:nrow(anes_data), size = 0.5*nrow(anes_data))
# get vector of numbers from 1 to the number of rows in the data set, and then sample
# a vector of numbers half that size with which to determine our training and test sets

training_set <- anes_data[sample_rows,] # training set includes all rows sampled
test_set <- anes_data[-sample_rows,] # test set includes all rows not sampled

library(VGAM) # loading package needed for tobit regression

tobit_model <- vglm(ft_dpc ~ pid_x, tobit(Lower = 0, Upper = 100, type.fitted="censored"), data=training_set)
summary(tobit_model)
predict(tobit_model, training_set, type="response")

###############

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
#anes_data_subset$pid_x <- as.ordered(anes_data_subset$pid_x)

levels(anes_data_subset$tea_supp_x) <- list("strong_supp"="1. Strong support" ,
                                            "weak_supp"="2. Not very strong support",
                                            "lean_supp"="3. Lean toward supporting",
                                            "indiff"="4. Do not lean either way",
                                            "lean_opp"="5. Lean toward opposing",
                                            "weak_opp"="6. Not very strong opposition",
                                            "strong_opp"="7. Strong opposition")
#anes_data_subset$tea_supp_x <- as.ordered(anes_data_subset$tea_supp_x)

levels(anes_data_subset$presapp_job_x) <- list("strong_app"="1. Approve strongly",
                                               "weak_app"="2. Approve not strongly",
                                               "weak_disapp"="4. Disapprove not strongly",
                                               "strong_disapp"="5. Disapprove strongly")
#anes_data_subset$presapp_job_x <- as.ordered(anes_data_subset$presapp_job_x)

levels(anes_data_subset$interest_attention) <- list("always"="1. Always",
                                                    "most"="2. Most of the time",
                                                    "half"="3. About half the time",
                                                    "some"="4. Some of the time",
                                                    "never"="5. Never")
#anes_data_subset$interest_attention <- as.ordered(anes_data_subset$interest_attention)

levels(anes_data_subset$dem_raceeth_x) <- list("white"="1. White non-Hispanic",
                                               "black"="2. Black non-Hispanic",
                                               "hispanic"="3. Hispanic",
                                               "other"="4. Other non-Hispanic")

levels(anes_data_subset$gender_respondent_x) <- list("male"="1. Male",
                                                     "female"="2. Female")

anes_data_subset$aidblack_self <- ifelse(anes_data_subset$aidblack_self<0, NA, anes_data_subset$aidblack_self)
anes_data_subset$aidblack_dpc <- ifelse(anes_data_subset$aidblack_dpc<0, NA, anes_data_subset$aidblack_dpc)

anes_data_subset <- anes_data_subset[complete.cases(anes_data_subset),]
# removing all rows with an NA observation; committing evil casewise deletion

sample_rows <- sample(1:nrow(anes_data_subset), size = 0.5*nrow(anes_data_subset))
# get vector of numbers from 1 to the number of rows in the data set, and then sample
# a vector of numbers half that size with which to determine our training and test sets



training_set <- anes_data_subset[sample_rows,] # training set includes all rows sampled
test_set <- anes_data_subset[-sample_rows,] # test set includes all rows not sampled

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

true_values <- test_set$ft_dpc

predictions <- matrix(data=c(lm_basic_predict, lm_race_predict, tobit_basic_predict,
                      tobit_race_predict), ncol=4, dimnames=list(NULL, 
                         c("lm_basic_predict", "lm_race_predict", "tobit_basic_predict",
                         "tobit_race_predict"))) # creating matrix of predictions
naive_values <- rep(median(test_set$ft_dpc), length(test_set$ft_dpc))

###############

fitRstats <- function(true, predicted, naive=NULL, statistics="rmse"){
  statistics_output <- NULL
  abs_error <- apply(predicted, MAR=2, FUN=function(x) abs(x - true))
  abs_pct_error <- apply(predicted, MAR=2, FUN=function(x) abs(x - true)/abs(true) * 100)
  baseline <- abs(naive-true)
  if("rmse" %in% statistics){
    rmse <- apply(abs_error, MAR=2, FUN=function(x) sqrt(sum(x^2)/length(x)))
    statistics_output <- cbind(statistics_output, rmse)
  }
  if("mad" %in% statistics){
    mad <- apply(abs_error, MAR=2, FUN=function(x) median(x))
    statistics_output <- cbind(statistics_output, mad)
  }
  if("rmsle" %in% statistics){
    rmsle <- apply(predicted, MAR=2, FUN=function(x) sqrt(sum((log(x+1)-log(true+1))^2)/length(true))) 
    statistics_output <- cbind(statistics_output, rmsle)
  }
  if("mape" %in% statistics){
    mape <- apply(abs_pct_error, MAR=2, FUN=function(x) sum(x)/length(x))
    statistics_output <- cbind(statistics_output, mape)
  }
  if("meape" %in% statistics){
    meape <- apply(abs_pct_error, MAR=2, FUN=function(x) median(x))
    statistics_output <- cbind(statistics_output, meape)
  }
  if("mrae" %in% statistics){
    mrae <- apply(abs_pct_error, MAR=2, FUN=function(x) median(x/baseline))
    statistics_output <- cbind(statistics_output, mrae)
  }
  rownames(statistics_output) <- ifelse(is.na(colnames(predicted)), paste0("Model ",1:dim(statistics_output)[1]), colnames(predicted))
  return(statistics_output)
}

fitRstats(true_values, predictions, naive_values, statistics = c("rmse","mad","meape","mrae"))
