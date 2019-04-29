library(data.table)
library(dplyr)
library(caret)
pml_data <- fread("data/pml-training.csv")
validate_testing <- fread("data/pml-testing.csv")
pml_data[,.N, by = "classe"]
validate_testing[,.N, by = "problem_id"]

### data transformation -----

# window-user id's
window_id <- c("classe","user_name","raw_timestamp_part_1")

# euler_angle_vars
euler_angle_vars <- grep("^(pitch|yaw|roll)",names(pml_data),value = T)

# movement vars
  movement_vars <- grep("(x|y|z)$",names(pml_data),value = T)

# original features
orig_features <- c(euler_angle_vars,movement_vars)

transf_data <- function(data){
  if(!"classe" %in% names(data)) window_id <- window_id[-1]
  
  # selecting variables
  all_vars <- c(window_id,orig_features)
  data <- select(data,one_of(all_vars))
  
  # ordering data
  setorderv(data,cols = window_id)

  # getting averages
  data_window_ag <-
    data[,lapply(.SD,mean,na.rm = T),by = c(window_id)] %>%
    setnames(orig_features,paste0("avg_",orig_features))
  
  # # getting standard deviations
  # data_window_ag <-
  #   data_window_ag[
  #     data[,lapply(.SD,sd,na.rm = T),by = c(window_id)],
  #     on = window_id] %>%
  #   setnames(orig_features,paste0("sd_",orig_features))
  
  # getting min
  data_window_ag <-
    data_window_ag[
      data[,lapply(.SD,min,na.rm = T),by = c(window_id)],
      on = window_id] %>%
    setnames(orig_features,paste0("min_",orig_features))
    
    # getting max
    data_window_ag <-
      data_window_ag[
        data[,lapply(.SD,max,na.rm = T),by = c(window_id)],
        on = window_id] %>%
      setnames(orig_features,paste0("max_",orig_features))
    
    # getting amplitude
    data_window_ag <-
      data_window_ag[
        data[,lapply(.SD,function(x){max(x,na.rm = T) - min(x,na.rm = T)}),
             by = c(window_id)],
        on = window_id] %>%
      setnames(orig_features,paste0("amplitude_",orig_features))
    
    # getting first observation
    data_window_ag <-
      data_window_ag[
        data[,lapply(.SD,head,n = 1),by = c(window_id)],
        on = window_id] %>%
      setnames(orig_features,paste0("first_",orig_features))
    
    # getting last observation
    data_window_ag <-
      data_window_ag[
        data[,lapply(.SD,tail,n = 1),by = c(window_id)],
        on = window_id] %>%
      setnames(orig_features,paste0("last_",orig_features))
    
    
    data_window_ag
  
}

# applying data transformation
pml_data_transf <- transf_data(pml_data)
validate_transf <-  transf_data(validate_testing)

# checking NAs
check_pml_data_na <- unlist(lapply(pml_data_transf,function(x)mean(is.na(x))))
check_testing_na <- unlist(lapply(validate_transf,function(x)mean(is.na(x))))
any(check_pml_data_na > 0)
any(check_testing_na > 0)

#### test X training ----
set.seed(342)
inTrain <- createDataPartition(pml_data_transf$classe,p = 0.7, list = F)
training <- pml_data_transf[inTrain,]
testing <- pml_data_transf[-inTrain,]

#### some plotings ----

b_plot <- ggplot(training,aes(col = classe))

# plotar isso aqui
b_plot + geom_boxplot(aes(y = last_accel_arm_y))
b_plot + geom_boxplot(aes(y = last_accel_arm_x))
b_plot + geom_boxplot(aes(y = last_accel_arm_z))

b_plot + geom_boxplot(aes(y = avg_gyros_arm_y))
b_plot + geom_boxplot(aes(y = avg_gyros_arm_x))
b_plot + geom_boxplot(aes(y = avg_gyros_arm_z))

b_plot + geom_boxplot(aes(y = avg_accel_belt_y))
b_plot + geom_boxplot(aes(y = avg_accel_belt_x))
b_plot + geom_boxplot(aes(y = avg_accel_belt_z))

## colocar esse aqui!
b_plot + geom_boxplot(aes(y = amplitude_accel_belt_y))
b_plot + geom_boxplot(aes(y = amplitude_accel_belt_x))
b_plot + geom_boxplot(aes(y = amplitude_accel_belt_z))

b_plot + geom_boxplot(aes(y = avg_pitch_belt))
b_plot + geom_boxplot(aes(y = avg_pitch_arm))
b_plot + geom_boxplot(aes(y = avg_pitch_forearm)) ## esse!
b_plot + geom_boxplot(aes(y = avg_pitch_dumbbell)) ## esse!

b_plot + geom_boxplot(aes(y = amplitude_pitch_belt)) # esse!
b_plot + geom_boxplot(aes(y = amplitude_pitch_arm))
b_plot + geom_boxplot(aes(y = amplitude_pitch_forearm)) 
b_plot + geom_boxplot(aes(y = amplitude_pitch_dumbbell)) ## esse!

b_plot + geom_boxplot(aes(y = avg_roll_belt))
b_plot + geom_boxplot(aes(y = avg_roll_arm)) 
b_plot + geom_boxplot(aes(y = avg_roll_forearm)) ## esse!
b_plot + geom_boxplot(aes(y = avg_roll_dumbbell))

b_plot + geom_boxplot(aes(y = amplitude_roll_belt)) ## esse!!!
b_plot + geom_boxplot(aes(y = amplitude_roll_arm)) 
b_plot + geom_boxplot(aes(y = amplitude_roll_forearm)) ## esse!
b_plot + geom_boxplot(aes(y = amplitude_roll_dumbbell)) # talvez esse

b_plot + geom_boxplot(aes(y = min_pitch_forearm)) # talvez esse


### some trials -----


ti <- Sys.time()
set.seed(3676)
mod_boost <- train(classe~.,method = "gbm",data = training)
tf1 <- difftime(Sys.time(),ti)

ti <- Sys.time()
set.seed(3676)
mod_rf <- train(classe~.,method = "rf",data = training)
tf2 <- difftime(Sys.time(),ti)

# saving the models
save(mod_boost,mod_rf,file = "fitted_models.rda")




summary(training)
na_pattern <- 
  training[,lapply(.SD,function(x)mean(is.na(x))),
           by = new_window] %>%
  t() 

training[,length(unique(user_name))]
training[,length(unique(num_window))]
training[,list(windows = length(unique(num_window)),
               classes = length(unique(classe))),
         by = user_name] %>% summary
training[,list(users = length(unique(user_name)),
               windows = length(unique(num_window))),
         by = classe] %>% summary
training[,list(users = length(unique(user_name)),
               classes = length(unique(classe))),
         by = num_window] %>% summary

ggplot(training,aes(color = classe)) +
  geom_density(aes(x = accel_forearm_z))

length_window <- 
  training[,.N, by = c("user_name","classe","num_window")] %>%
  .[,list(avg_length = mean(N)),
    by = c("num_window","classe")]
qplot(num_window,avg_length,
      # colour = classe,
      data = length_window,
      geom = "line")

setorder(training,user_name,classe,raw_timestamp_part_1)
training[,id_window := 1:.N,by = c("user_name","classe","raw_timestamp_part_1")]
avg_accel <- 
  training[,list(accel_forearm_z = mean(accel_arm_z),
                 accel_forearm_x = mean(accel_arm_x),
                 accel_forearm_y = mean(accel_arm_y)),
    by = c("user_name","id_window","classe")] #%>%
  # select(-user_name) %>%
  # .[,lapply(.SD,mean), by = c("id_window","classe")]
ggplot(data = avg_accel,
       aes(x = id_window,y = accel_forearm_z)) + 
  geom_line(aes(colour = classe),show.legend = T) + 
  facet_grid(user_name~.)

View(na_pattern[sort(rownames(na_pattern)),])
mod_dt <- train(classe~.,method = "rpart",data = training)

training[user_name == "carlitos" &
           raw_timestamp_part_1==1323084232] -> carlitos_stmp1

training[,list(avg_roll_arm = mean(avg_roll_arm,na.rm = T),
               avg2_roll_arm = mean(roll_arm),
               stddev_roll_arm = mean(stddev_roll_arm,na.rm = T),
               stddev2_roll_arm = sd(roll_arm),
               min_roll_arm = mean(min_roll_arm,na.rm = T),
               min2_roll_arm = min(roll_arm),
               max_roll_arm = mean(max_roll_arm,na.rm = T),
               max2_roll_arm = max(roll_arm)),
         by = c("user_name","classe","num_window","id_window")] %>%
  setorder(user_name,classe,num_window) -> see_stats



see_stats[,list(d_avg = 100*mean(avg_roll_arm/avg2_roll_arm - 1,na.rm = T),
                d_stdev = 100*mean(stddev_roll_arm/stddev2_roll_arm - 1,na.rm = T),
                d_min = 100*mean(min_roll_arm/min2_roll_arm - 1,na.rm = T),
                d_max = 100*mean(max_roll_arm/max2_roll_arm - 1,na.rm = T))]

b_plot <- ggplot(see_stats,aes(col = classe))
b_plot + geom_density(aes(x = avg_roll_arm-avg2_roll_arm))
b_plot + geom_density(aes(x = avg_roll_arm-avg2_roll_arm))
