library(dplyr)
library(ggplot2)

# ---- 
# DATA
#Bring the dataset
demen<- read.csv('OPTIMAL_combined_3studies_6feb2020.csv')
str(demen)
### DATA CLEANING ###
demen$age<- as.integer(demen$age)
#removing unneeded columns
demen<-demen[, -c(4,7,8,9,14,15,17,18)]
#convert age to integers 
demen$age<- as.integer(demen$age) 
#remove any NA values in smoking
demen<- demen[!is.na(demen$smoking), ]
str(demen)

#DEA 
#Creating visualizations on dementia 
y_demen <- filter(demen, dementia_all == 1)
hist(y_demen$age, 
     main = "Histogram of Age Distribution", 
     xlab = "Age",
     col = 'lightblue')


str(demen)


#Gender bar plot based on wether they have been diagnosed or not
ggplot(demen, aes(x = gender, fill = factor(dementia_all))) +
  geom_bar(position = "dodge", color = 'black') +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, position=position_dodge(width=0.9)) +
  labs(x = "Gender", y = "Count", fill = "Dementia") +
  ggtitle("Count of Dementia by Gender") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "pink"))


#Bar plot based on the categories of smokers
ggplot(demen, aes(x = smoking, fill = factor(dementia_all))) +
  geom_bar(position = "dodge", color = 'black') +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, position=position_dodge(width=0.9)) +
  labs(x = "smoking", y = "Count", fill = "Dementia") +
  ggtitle("Count of Dementia by smoking history") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "pink"))

#Bar plot on the Lacune counts.
ggplot(demen, aes(x = lac_count, fill = factor(dementia_all))) +
  geom_bar(position = "dodge", color = 'black') +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, position=position_dodge(width=0.9)) +
  labs(x = "Lacune count", y = "Count", fill = "Dementia") +
  ggtitle("Count of Dementia by Lacune") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "pink"))

# Bar plot for CMB (Cerebral Microbleeds)
ggplot(demen, aes(x = CMB_count, fill = factor(dementia_all))) +
  geom_bar(position = "dodge", color = 'black') +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, position=position_dodge(width=0.9)) +
  labs(x = "CMB count", y = "Count", fill = "Dementia") +
  ggtitle("Count of Dementia by Cerebral Microbleeds") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "pink"))

#Bar plot for the scans
ggplot(demen, aes(x = study, fill = factor(dementia_all))) +
  geom_bar(position = "dodge", color = 'black') +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, position=position_dodge(width=0.9)) +
  labs(x = "Types of studies", y = "Count", fill = "Dementia") +
  ggtitle("Count of Dementia by the Studies") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "pink"))

# Bar plot for Fezeka scores
ggplot(demen, aes(x = Fazekas, fill = factor(dementia_all))) +
  geom_bar(position = "dodge", color = 'black') +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, position=position_dodge(width=0.9)) +
  labs(x = "Fazeka Score", y = "Count", fill = "Dementia") +
  ggtitle("Count of Dementia by Fazeka scores") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "pink"))


# We want to lump the education years into categories
# 1-12 = 1 = high school education
# 13-17 = 2 = undergrad
# 18-22 = 3 = graduate
# 22+ = 4 = Post Doc
demen$educationyears<-  ifelse(demen$educationyears < 13, 1, 
                               ifelse(demen$educationyears < 17, 2,
                                      ifelse(demen$educationyears < 22, 3,4)))
table(demen$educationyears)

# there are only 2 instances of post doc so we will drop these observations
#dropping this does not affect the count of dementia = 1
demen <- demen[demen$educationyears != 4, ]
table(demen$educationyears)

# removing missing values (found only in smoking column)
#dropping these does not affect the count of dementia = 1
colSums(is.na(demen))
demen<- demen[!is.na(demen$smoking), ]
str(demen)


# converting some variables to factors
gen<- as.factor(demen$gender)
ed <- as.factor(demen$educationyears)
smoke<- as.factor(demen$smoking)
stud <- as.factor(demen$study)
lac<- as.factor(demen$lac_count)
cmb<- as.factor(demen$CMB_count)

demen$hypertension<- as.numeric(as.factor(demen$hypertension))-1
demen$hypercholesterolemia<- as.numeric(as.factor(demen$hypercholesterolemia))-1
str(demen)

# coding indicator variables
temp_gen<- data.frame(model.matrix(~gen -1))
ncol(temp_gen)
temp_edu <- data.frame(model.matrix(~ed - 1))
ncol(temp_edu)
temp_smoke <- data.frame(model.matrix(~smoke - 1))
ncol(temp_smoke)
temp_study<- data.frame(model.matrix(~stud-1))
ncol(temp_study)
temp_lac<- data.frame(model.matrix(~lac-1))
ncol(temp_lac)
temp_cmb<- data.frame(model.matrix(~cmb-1))
ncol(temp_cmb)
temp_stud<- data.frame(model.matrix(~stud-1))
ncol(temp_stud)


# combine it all
demen1<- cbind(demen[,c(4,1:2,6,8:9,11:12)],
               temp_gen[,1:2], 
               temp_edu[,1:3],
               temp_smoke[,1:3],
               temp_study[,1:4],
               temp_lac[,1:4],
               temp_cmb[,1:2])

table(demen1$dementia_all)
str(demen1)

#Export the file for the clean data version
write.csv(demen1, 'dementia_final_ver.csv', row.names = FALSE)
# FITTING ALL IN LOGISTIC FOR PRELIMINARY EXPLORATION

log.reg.1 <- glm(dementia_all ~ . - ID, data = demen1)
summary(log.reg.1)

# we can see there are some 'NA' values in the summary of the output
#this is do to correlations between the predictor variables
# we will drop the variables that have a row of NA's
# we will also drop the ID variable since it is not needed 

drops <- c(9, 12, 15, 19, 23, 25)

updated.df <- demen1[, -drops]

dim(updated.df)

# ----
# 
# # CREATING TRAINING/TESTING 
# 
 set.seed(0)
# 
# # all cases of dementia 
# yes <- updated.df[updated.df$dementia_all == 1, ]
# table(yes$dementia_all)
# 
# # all cases of non dementia
# no <- updated.df[updated.df$dementia_all == 0, ]
# table(no$dementia_all)
# 
# # training 
# tr.rows <- sample(nrow(yes), 58)
# tr.rows
# 
# yes.train <- yes[tr.rows, ]
# table(yes.train$dementia_all)
# 
# ts.rows <- sample(nrow(no), 174)
# ts.rows
# 
# no.train <- no[ts.rows, ]
# table(no.train$dementia_all)
# 
# # final training set 
# dat.train <- rbind(yes.train, no.train)
# table(dat.train$dementia_all)
# 
# # testing 
# 
# # remove the no instances we already used for testing 
# no <- no[-ts.rows, ]
# 
# # select another 240 
# ts.rows2 <- sample(nrow(no), 174)
# 
# no.test <- no[ts.rows2, ]
# 
# # final testing set 
# dat.test <- rbind(yes[-tr.rows, ], no.test)
# table(dat.test$dementia_all)
# 
# 
# # ---- 
# 
# # running all in logistic model with training set 
# 
# log.train1 <- glm(dementia_all ~ ., dat.train, family = "binomial")
# summary(log.train1)
# 
# # checking training error 
# tr1.pred <- predict(log.train1, dat.train, 
#                       type = "response")
# tr1.class <- ifelse(tr1.pred > 0.5, 1, 0)
# 
# err1 <- mean(tr1.class != dat.train$dementia_all)
# err1
# 
# #this is lower since the model was trained on this data
# 
# # predicting using test set
# ts1.pred <- predict(log.train1, dat.test, 
#                     type = "response")
# ts1.class <- ifelse(ts1.pred > 0.5, 1, 0)
# 
# # checking testing error
# err.test1 <- mean(ts1.class != dat.test$dementia_all)
# err.test1
# 
# # 0.22

# ----

# EQUALLY SPLIT DATASET


# all cases of dementia 
yes <- updated.df[updated.df$dementia_all == 1, ]
table(yes$dementia_all)

# all cases of no dementia
no <- updated.df[updated.df$dementia_all == 0, ]
table(no$dementia_all)

# select 116 cases of non dementia 
nd.rows <- sample(nrow(no), 116)
nd.rows

nd.test2 <- no[nd.rows, ]
table(nd.test2$dementia_all)

# combining into the new set
df.v2 <- rbind(yes, nd.test2)
table(df.v2$dementia_all)


# RUNNING CV USING THE BALANCED DATA SET 
log.v2 <- glm(dementia_all ~ ., df.v2, family = "binomial")
cv.results2 <- cv.glm(df.v2,log.v2 , K = 10)
cv.results2$delta

# RUNNING THE ALL IN USING THE BALANCED DATASET 

# making new training/testing sets

library(dplyr)
set.seed(0) # make this example reproducible
df.v2$id <- 1:nrow(df.v2) # create ID column
train.v2 <- df.v2 %>% dplyr::sample_frac(0.70) # use 70% of dataset as training set
test.v2 <- dplyr::anti_join(df.v2, train.v2, by = 'id') 
  
table(train.v2$dementia_all)
table(test.v2$dementia_all)

#dropping ID column
train.v2 <- train.v2[, -c(2,20)]
test.v2 <- test.v2[ , -c(2,20)]

# the model
log.train2 <- glm(dementia_all ~ ., train.v2, family = "binomial")
summary(log.train2)

# predicting using test set
ts2.pred <- predict(log.train2, test.v2, 
                    type = "response")
ts2.class <- ifelse(ts2.pred > 0.5, 1, 0)

# checking testing error

err.test2 <- mean(ts2.class != test.v2$dementia_all, na.rm = TRUE)
err.test2

tab.2.test <- table(test.v2$dementia_all, ts2.class, 
                    dnn = c("Actual","Predicted"))
tab.2.test


# ----
# DECISION TREE 

# # will need to convert response variable to factor
# dat.train$dementia_all <- as.factor(dat.train$dementia_all)
# dat.test$dementia_all <- as.factor(dat.test$dementia_all)
# 
# library(tree)
# 
# # Create a decision tree model
# tree1 <- tree(dementia_all ~., data = dat.train)
# plot(tree1)
# text(tree1, pretty = 0)
# 
# 
# # checking performance on the test set
# tree1.pred <- predict(tree1, dat.test)
# tree1.pred.class <- ifelse(tree1.pred > 0.5, 1, 0)
# 
# # checking error rate 
# tree1.err <- mean(tree1.pred.class != dat.test$dementia_all)
# tree1.err
# 
# #prune the tree!
# prune1 <- prune.misclass(tree1)
# 
# plot(prune1)
# plot(prune1$size, prune1$dev, xlab = "Size of Tree",
#      ylab = "Deviation")
# 
# # according to this plot, 9 seems to be ideal
# prune.tree1 <- prune.misclass(tree1, best = 9)
# summary(prune.tree1)
# 
# prune.tree1
# plot(prune.tree1)
# text(prune.tree1, pretty = 0)
# 
# # make predictions using the freshly pruned tree
# pt1.pred <- predict(prune.tree1, dat.test, type = "class")
# 
# pt1.err <- mean(dat.test$dementia_all != pt1.pred)
# pt1.err


# tree with the training set made from the balanced set
train.v2$dementia_all <- as.factor(train.v2$dementia_all)
test.v2$dementia_all <- as.factor(test.v2$dementia_all)

library(tree)

# Create a decision tree model
treev2 <- tree(dementia_all ~., data = train.v2)
plot(treev2)
text(treev2, pretty = 0)


# checking performance on the test set
treev2.pred <- predict(treev2, test.v2)
treev2.pred.class <- ifelse(treev2.pred > 0.5, 1, 0)

# checking error rate 
treev2.err <- mean(treev2.pred.class != test.v2$dementia_all)
treev2.err

#prune the tree!
prunev2 <- prune.misclass(treev2)

plot(prunev2)
plot(prunev2$size, prunev2$dev, xlab = "Size of Tree",
     ylab = "Deviation")

# according to this plot, 6 seems to be ideal
prune.treev2 <- prune.misclass(treev2, best = 6)
summary(prune.treev2)

prune.treev2
plot(prune.treev2)
text(prune.treev2, pretty = 0)

# make predictions using the freshly pruned tree
pt1.pred <- predict(prune.treev2, test.v2, type = "class")

pt1.err <- mean(test.v2$dementia_all != pt1.pred)
pt1.err

tab.3.test <- table(test.v2$dementia_all, pt1.pred, 
                    dnn = c("Actual","Predicted"))
tab.3.test

# LOGISTIC REGRESSION AGAIN WITH CERTAIN VARIABLES ONLY 

# model 
log.feature <- glm(dementia_all ~ age + hypertension + diabetes + 
                     genfemale + smokeex.smoker + smokecurrent.smoker + SVD.Amended.Score, 
                   train.v2, family = "binomial")
summary(log.feature)

# predicting using test set
ts4.pred <- predict(log.feature, test.v2, 
                    type = "response")
ts4.class <- ifelse(ts4.pred > 0.5, 1, 0)

# checking testing error

err.test4 <- mean(ts4.class != test.v2$dementia_all, na.rm = TRUE)
err.test4

tab.4.test <- table(test.v2$dementia_all, ts4.class, 
                    dnn = c("Actual","Predicted"))
tab.4.test

# fit a tree using these features 
treev3 <- tree(dementia_all ~age + hypertension + diabetes + 
                 genfemale + smokeex.smoker + smokecurrent.smoker + SVD.Amended.Score, 
               data = train.v2)
plot(treev3)
text(treev3, pretty = 0)

treev3.pred <- predict(treev3, test.v2, type = "class")

treev3.err <- mean(test.v2$dementia_all != treev3.pred)
treev3.err

tab.5.test <- table(test.v2$dementia_all, treev3.pred, 
                    dnn = c("Actual","Predicted"))
tab.5.test

# let's try prunning 
prunev3 <- prune.misclass(treev3)

plot(prunev3)
plot(prunev3$size, prunev3$dev, xlab = "Size of Tree",
     ylab = "Deviation")

# according to this plot, 6 seems to be ideal
prune.treev3 <- prune.misclass(treev3, best = 6)
summary(prune.treev3)

#prune.treev2
plot(prune.treev3)
text(prune.treev3, pretty = 0)

# make predictions using the freshly pruned tree
pt3.pred <- predict(prune.treev3, test.v2, type = "class")

pt3.err <- mean(test.v2$dementia_all != pt3.pred)
pt3.err

tab.5.test <- table(test.v2$dementia_all, pt3.pred, 
                    dnn = c("Actual","Predicted"))
tab.5.test




