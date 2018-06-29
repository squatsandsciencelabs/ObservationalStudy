library(tidyverse)
library(reshape2)

rep_data <- read.csv("rep_data.csv", stringsAsFactors = FALSE)
set_data <- read.csv("set_data.csv", stringsAsFactors = FALSE)
colnames(rep_data)
colnames(set_data)

# removing row counts
rep_data$X <- NULL
set_data$X <- NULL

# X_id = _id = setID
set_data$X_id <- NULL

# renaming columns for clearer source identification
colnames(rep_data) <- paste("Rep_", colnames(rep_data), sep = '')
colnames(set_data) <- paste("Set_", colnames(set_data), sep = '')

# renaming back setID to use as a merge key
names(rep_data)[names(rep_data) == "Rep_setID"] = "setID"
names(set_data)[names(set_data) == "Set_setID"] = "setID"

# left join of two datasets
full_data <- merge(rep_data, set_data, by = 'setID', all.x = TRUE)
dim(full_data)
# 2490   44
head(full_data)
colnames(full_data)

#table(full_data$Set_tags)

# creating idenifier for each study subject
full_data$StudySubject <- ""
full_data[grepl("mimi", full_data$Set_tags),]$StudySubject <- "mimi"
full_data[grepl("elisa", full_data$Set_tags),]$StudySubject <- "elisa"
full_data[grepl("matt", full_data$Set_tags),]$StudySubject <- "matt"
full_data[grepl("harr", full_data$Set_tags),]$StudySubject <- "harr"
full_data[grepl("chase", full_data$Set_tags),]$StudySubject <- "chase"
full_data[grepl("adam", full_data$Set_tags),]$StudySubject <- "adam"

table(full_data$StudySubject)
#         adam chase elisa  harr  matt  mimi 
#     52   203   356   486   486   416   491 

full_data$StudySubject_anon <- ""
full_data[grepl("chase", full_data$Set_tags),]$StudySubject_anon <- "Subject 1"
full_data[grepl("elisa", full_data$Set_tags),]$StudySubject_anon <- "Subject 2"
full_data[grepl("harr", full_data$Set_tags),]$StudySubject_anon <- "Subject 3"
full_data[grepl("matt", full_data$Set_tags),]$StudySubject_anon <- "Subject 4"
full_data[grepl("mimi", full_data$Set_tags),]$StudySubject_anon <- "Subject 5"
table(full_data$StudySubject_anon)
#     Subject 1 Subject 2 Subject 3 Subject 4 Subject 5 
# 255       356       486       486       416       491 

# creating a flag to exclude reps/sets from the analysis
full_data$exclude = 0
full_data[grepl("adam", full_data$Set_tags),]$exclude <- 1
full_data[full_data$Set_removed == 1,]$exclude <- 1
full_data[full_data$Rep_isValid == 'False',]$exclude <- 1
full_data[full_data$Rep_removed == 'True',]$exclude <- 1
full_data[full_data$Set_deleted == 1,]$exclude <- 1
table(full_data$exclude)
#  0    1 
#1853  637 

# creating a flag for failures
full_data$fail = ifelse(grepl("fail", full_data$Set_tags), 1, 0)
table(full_data$fail)
# 0    1 
# 2450   40  

# deleting invalid reps
full_data_clean <- subset(full_data, exclude == 0)
dim(full_data_clean)
# [1] 1853   46

table(full_data_clean$fail)
#    0    1 
# 1831   22 

table(full_data_clean$Rep_hardware)
#android     ios 
#     97    1765 

table(full_data_clean$StudySubject, full_data_clean$Rep_deviceIdentifier)
table(full_data_clean$Rep_deviceIdentifier)

# creating a flag for reps without relevant information
full_data_clean$unknowns <- 0
full_data_clean[full_data_clean$StudySubject == '',]$unknowns <- 1
table(full_data_clean$unknowns)

full_data_clean %>% 
  group_by(StudySubject) %>% 
  summarize(n_distinct(Set_workoutID))
#0 ""                                     4
#1 chase                                  6
#2 elisa                                  7
#3 harr                                   6
#4 matt                                   7
#5 mimi                                   7

# get the date part only
# caveat: time zone is UTC, so some workouts are spread across multiple days
full_data_clean$Set_initialStartTime_date <- substr(full_data_clean$Set_initialStartTime, 1, 10)

# check why some have 7
full_data_clean %>% 
  group_by(StudySubject, Set_workoutID, Set_initialStartTime_date) %>% 
  summarize(n_sets = n_distinct(setID))

# Extra workouts
# mimi: 500da55e-ed20-4dd5-bad2-5a12c01aa1a9, rpe < 5.5
# matt: ec16e6cb-29c1-4e2b-8235-cbd36efac63b, 2 sets (incorrect reps; 5 rpe)

# flag them out
full_data_clean[full_data_clean$Set_workoutID == "500da55e-ed20-4dd5-bad2-5a12c01aa1a9",]$unknowns <- 1
full_data_clean[full_data_clean$Set_workoutID == "ec16e6cb-29c1-4e2b-8235-cbd36efac63b",]$unknowns <- 1
table(full_data_clean$unknowns)
# 1801   52 

# elisa had 7 workouts (1st one was on two devices)
# combined them by renaming workoutID
full_data_clean[full_data_clean$Set_workoutID == "dc7cace1-274d-487c-ba35-7bc957b7097b",]$Set_workoutID = "01aff772-84bf-4a7b-8bb4-17863086714a"

table(full_data_clean$StudySubject, full_data_clean$unknowns)

table(full_data_clean$Set_rpe)
#   < 5.5    10     6   6.5     7   7.5     8   8.5     9   9.5 
#19   563   156   249   131   162   136   129   108    99   101 
# why 10 obs missing rpe?

# flag missing RPE
full_data_clean[full_data_clean$Set_rpe == "",]$unknowns <- 1
table(full_data_clean$unknowns)
# 1791   62 

table(full_data_clean$Set_exercise)
#bench        bench          squat        squat2 sumo deadlift 
#9           875            20           940             5             4 

full_data_clean[full_data_clean$Set_exercise == "squat2",]$Set_exercise <- "squat"
full_data_clean[full_data_clean$Set_exercise == "bench ",]$Set_exercise <- "bench"

table(full_data_clean$Set_exercise, full_data_clean$unknowns)
#                 0   1
#                 0   9
# bench         891   4
# squat         900  45
# sumo deadlift   0   4

# mimi bench at 85kgx8 @ RPE8.5 is recoded into squat
# only 7 reps, so it doesn't really count
full_data_clean[full_data_clean$setID == "7370d826-be93-4fa0-81dd-927f2a67c2fd",]$Set_exercise <- "squat"

# matt's squat at 120kgx5 @ RPE 10 is recoded into bench
full_data_clean[full_data_clean$setID == "276a14fb-f982-45fe-9774-fff07b962472",]$Set_exercise <- "bench"

# matt's squat at 115kgx5 @ RPE 8 is recoded into bench
full_data_clean[full_data_clean$setID == "e52c3e38-c2a4-4d16-b35c-2d2e3152733b",]$Set_exercise <- "bench"

# elisa's bench at 73.5kgx5 @ RPE 6 is recoded into squat:
full_data_clean[full_data_clean$setID == "59115544-bb13-4e9f-9fac-2f6e90a8a4f5",]$Set_exercise <- "squat"

# harr's squat at 80kgx2 RPE9 is recoded into bench:
full_data_clean[full_data_clean$setID == "0c33a481-e1cb-44b6-ba64-adbcddc58a13",]$Set_exercise <- "bench"

table(full_data_clean$Set_metric)
#  kgs  lbs 
# 1838   15

summary(full_data_clean$Set_weight)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   30.0    57.5    77.5    83.8   105.0   295.0       9 
# The NA's are covered by unknowns variable

# in some instances weight in kg was entered incorrectly
# temp fix: round all weight to multiples of 2.5
# full_data_clean$Set_weight_adj <- mapply(plyr::round_any, full_data_clean$Set_weight, 2.5)

# converting all weight numbers into kg
# this set was definitely in pounds
full_data_clean[full_data_clean$setID == '675e3adc-a530-4420-bfcd-e19183b40101',]$Set_metric <- 'lbs'
full_data_clean$Set_weight_kg <- full_data_clean$Set_weight
full_data_clean[full_data_clean$Set_metric == 'lbs',]$Set_weight_kg <- 
  full_data_clean[full_data_clean$Set_metric == 'lbs',]$Set_weight/2.20462

# full_data_clean$Set_weight_adj_kg <- full_data_clean$Set_weight_adj
# full_data_clean[full_data_clean$Set_metric == 'lbs',]$Set_weight_adj_kg <- 
#   full_data_clean[full_data_clean$Set_metric == 'lbs',]$Set_weight/2.20462

summary(full_data_clean$Set_weight_kg)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  13.61   57.50   77.50   82.91  105.00  265.00       9 

# summary(full_data_clean$Set_weight_adj_kg)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  13.61   57.50   77.50   82.92  105.00  265.00       9 

summary(full_data_clean$Rep_AvgVel)
summary(full_data_clean$Rep_PeakVel)
summary(full_data_clean$Set_weight)

dim(full_data_clean)
# [1] 1853   50
table(full_data_clean$unknowns)
#  0    1 
#1791   62 

full_data_clean$Set_rpe_num <- as.numeric(full_data_clean$Set_rpe)
table(full_data_clean$Set_rpe_num, full_data_clean$Set_rpe, useNA = "ifany")

# 1RM Estimation
# RTS (http://articles.reactivetrainingsystems.com/2015/11/29/beginning-rts/)

full_data_clean %>% 
  group_by(setID) %>% 
  summarize(n_reps = n()) -> n_reps

full_data_clean <- merge(x = full_data_clean, y = n_reps, by = "setID", all.x = TRUE)
table(full_data_clean$n_reps)
#1   2   3   4   5   6   7   8   9 
#7 250  21  24 540  66  63 864  18 

full_data_clean %>% 
  filter(n_reps == 2 & Set_rpe %in% c("9", "9.5", "10") & Set_exercise %in% c("squat", "bench")) %>% 
  group_by(StudySubject, Set_exercise, Set_rpe) %>% 
  summarize(TwoReps_max_weight = max(Set_weight_kg)) -> tworeps

tworeps <- dcast(tworeps, StudySubject ~ Set_rpe + Set_exercise, value.var = "TwoReps_max_weight")

RPE10_2reps <- 0.955
RPE95_2reps <- 0.939
RPE90_2reps <- 0.922

tworeps$Squat_1RM_RPE10 <- round(tworeps$`10_squat`/RPE10_2reps)
tworeps$Bench_1RM_RPE10 <- round(tworeps$`10_bench`/RPE10_2reps)
tworeps$Squat_1RM_RPE95 <- round(tworeps$`9.5_squat`/RPE95_2reps)
tworeps$Bench_1RM_RPE95 <- round(tworeps$`9.5_bench`/RPE95_2reps)
tworeps$Squat_1RM_RPE90 <- round(tworeps$`9_squat`/RPE90_2reps)
tworeps$Bench_1RM_RPE90 <- round(tworeps$`9_bench`/RPE90_2reps)
tworeps$Squat_1RM_RTS <- ifelse(!is.na(tworeps$Squat_1RM_RPE10), tworeps$Squat_1RM_RPE10,
                            ifelse(!is.na(tworeps$Squat_1RM_RPE95), tworeps$Squat_1RM_RPE95, tworeps$Squat_1RM_RPE90))
tworeps$Bench_1RM_RTS <- ifelse(!is.na(tworeps$Bench_1RM_RPE10), tworeps$Bench_1RM_RPE10,
                            ifelse(!is.na(tworeps$Bench_1RM_RPE95), tworeps$Bench_1RM_RPE95, tworeps$Bench_1RM_RPE90))

full_data_clean <- 
  merge(x = full_data_clean, y = tworeps[,c("StudySubject","Squat_1RM_RTS","Bench_1RM_RTS")], 
        by = c("StudySubject"), all.x = TRUE)

table(full_data_clean$StudySubject, full_data_clean$Squat_1RM_RTS)
table(full_data_clean$StudySubject, full_data_clean$Bench_1RM_RTS)

write.csv(full_data_clean[full_data_clean$unknowns == 1, ], file = "Questionable.csv")
full_data_clean <- full_data_clean[full_data_clean$unknowns == 0,]
save(full_data_clean, file = "StudyDataClean.RData")

rm(list=ls())
