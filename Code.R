###### Load the packages
source("DataAnalyticsFunctions.R")
library(tidyverse)
library(corrgram)
library(GGally)
library(glmnet)
library(MASS)
library(quantreg)
library(scales)
library(rpart)
library(ggbiplot)
library(dendextend)
library(purrr)
library(dplyr)
library(cluster)
library(tree)
library(randomForest)
set.seed(27)

###### Import the data
abs <- read.csv("Absenteeism_at_work.csv",sep = ";")
# summary(abs)



###### Clean the data 
### Clean the data by formatting the data type
str(abs)
col2factor <- c("ID", "Reason.for.absence", "Month.of.absence", "Day.of.the.week", 
                "Seasons", "Hit.target", "Disciplinary.failure","Education", 
                "Social.drinker", "Social.smoker")
abs[col2factor] <- lapply(abs[col2factor], as.factor)
# Convert some columns to factor

### Clean the data by correlation analysis
ggpairs(abs[!names(abs) %in% c('ID','Reason.for.absence')])
cor(abs$Body.mass.index,abs$Weight)
# Remove Age and Weight as age and service time as well as weight and BMI are correlated
abs1 <- abs[!names(abs) %in% c('Body.mass.index')]



###### Data exploratory
### Histgram of Absenteeism_time_in_hours
hist(abs1$Absenteeism.time.in.hours, breaks = 40, #prob = TRUE,
     xlab = 'Absenteeism time in hours', main = " Absenteeism time Distribution", col = "grey")

### Reasons and time of absenteeism
Reasons <- aggregate(abs1$Absenteeism.time.in.hours, by=list(Category=abs1$Reason.for.absence), FUN=sum)

ggplot( data = Reasons, aes(Category,x))+
  geom_bar(stat= "identity")+
  xlab("Reasons for absence")+
  ylab("Total hours of absence")

abs1%>%group_by(Reason.for.absence)%>%
  count()%>%
  ggplot(aes(Reason.for.absence,n))+
  geom_bar(stat= "identity")+
  xlab("Reasons for absence")+
  ylab("# of times as reason for absence")

### Education Pie Chart
educ <- abs1 %>% group_by(ID,Education)%>%
  count()%>%
  filter(!(ID == 29 & Education == 1))

educ <- educ%>% group_by(Education)%>%
  count()
educ <- educ %>% 
  mutate( percent = n/36)

pie(educ$percent, labels = c("high school", "graduate", "postgraduate"," master and doctor"
), main="Education Level of Employees")




###### PCA
xdata <- model.matrix(Absenteeism.time.in.hours~
                        Transportation.expense + Distance.from.Residence.to.Work + Service.time+Age+Work.load.Average.day+Weight+Height+Pet, 
                      data=abs1)[,-1]
xdata <- scale(xdata)
pca.data <- prcomp(xdata, scale=TRUE)
#plot to compare the variences explained by factors
plot(pca.data,main="PCA: Variance Explained by Factors")
mtext(side=1, "Factors",  line=1, font=2)
#make a graph grouped by absenteeism time
ggbiplot(pca.data, labels=rownames(abs1))
ggbiplot(pca.data,ellipse=TRUE,circle=TRUE, group= abs1$Absenteeism.time.in.hours)+
  labs(color="Absenteeism.time.in.hours")
#datapc <- predict(pca.data)
#datapc

####Loading 1
loadings <- pca.data$rotation[,1:3]
v<-loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:ncol(xdata)],1]
loadingfit <- lapply(1:ncol(xdata), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]

#### Loading 2
v<-loadings[order(abs(loadings[,2]), decreasing=TRUE)[1:ncol(xdata)],2]
loadingfit <- lapply(1:ncol(xdata), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]

#### Loading 3
v<-loadings[order(abs(loadings[,3]), decreasing=TRUE)[1:ncol(xdata)],3]
loadingfit <- lapply(1:ncol(xdata), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]




###### Clustering
#Dropping the variables for Hierarchical Clustering 
drops <- c("ID","Reason.for.absence","Month.of.absence", "Day.of.the.week", "Hit.target")
absclust<- abs1[,!(names(abs1) %in% drops)]
head(absclust)
#converting the categorical data into integers for hierarchical clustering
absclust$Seasons <- (as.integer(absclust$Seasons))
absclust$Disciplinary.failure <- (as.integer(absclust$Disciplinary.failure))
absclust$Education <- (as.integer(absclust$Education))
absclust$Social.drinker <- (as.integer(absclust$Social.drinker))
absclust$Social.smoker <- (as.integer(absclust$Social.smoker))
hierarfinal<- absclust
#Exploratory Data analysis

ggplot(abs1, aes(x= abs1$Age, y= sum(abs1$Absenteeism.time.in.hours)))+
  geom_col()

ggplot(abs1, aes(x=abs$Seasons, y=sum(abs1$Absenteeism.time.in.hours)))+
  geom_col()

#Hierarchical clustering Analysis begins
#scale all the variables
absclust$Transportation.expense <-scale(absclust$Transportation.expense)
absclust$Distance.from.Residence.to.Work<-scale(absclust$Distance.from.Residence.to.Work)
absclust$Service.time<-scale(absclust$Service.time)
absclust$Age <- scale(absclust$Age)
absclust$Work.load.Average.day<-scale(absclust$Work.load.Average.day)
absclust$Son <- scale(absclust$Son)
absclust$Pet <- scale(absclust$Pet)
absclust$Weight<-scale(absclust$Weight)
absclust$Height<-scale(absclust$Height)
absclust$Absenteeism.time.in.hours <- scale(absclust$Absenteeism.time.in.hours)
absclust$Seasons <- scale(as.integer(absclust$Seasons))
absclust$Disciplinary.failure <- scale(as.integer(absclust$Disciplinary.failure))
absclust$Education <- scale(as.integer(absclust$Education))
absclust$Social.drinker <- scale(as.integer(absclust$Social.drinker))
absclust$Social.smoker <- scale(as.integer(absclust$Social.smoker))

head(absclust)

#calculating the euclidean distance
distanceforclust <- dist(absclust)

#Finding the euclidian distance between the scaled variable values
hcabs<-hclust(distanceforclust, method = 'complete')

# We use k=3 after exploring the data
clusters <- cutree(hcabs, k=3)
#Appending the value to the data frame
abs2 <- mutate(absclust, cluster = clusters)

#Representing hierarchical cluster using a dendrogram
dend_clust <- as.dendrogram(hcabs)
dend_20 <- color_branches(dend_clust, k =3)
plot(dend_20, xlab = "feature")

#Understanding the hierarchical clusters
aggregate(hierarfinal, by=list(abs2$cluster), FUN =mean)
count(abs2, cluster)

#kmeans clustering
#Dropping all the categorical variables as they give a wrong measure when using the kmeans clustering analysis
drops <- c("ID","Reason.for.absence","Month.of.absence", "Day.of.the.week","Seasons", "Disciplinary.failure","Education","Social.drinker","Social.smoker", "Hit.target")
abskmeans<- abs1[,!(names(abs1) %in% drops)]
abscat<- abskmeans

#scale all the variables
abskmeans$Transportation.expense <-scale(abskmeans$Transportation.expense)
abskmeans$Distance.from.Residence.to.Work<-scale(abskmeans$Distance.from.Residence.to.Work)
abskmeans$Service.time<-scale(abskmeans$Service.time)
abskmeans$Age <- scale(abskmeans$Age)
abskmeans$Work.load.Average.day<-scale(abskmeans$Work.load.Average.day)
abskmeans$Son <- scale(abskmeans$Son)
abskmeans$Pet <- scale(abskmeans$Pet)
abskmeans$Weight<-scale(abskmeans$Weight)
abskmeans$Height<-scale(abskmeans$Height)
abskmeans$Absenteeism.time.in.hours <- scale(abskmeans$Absenteeism.time.in.hours)

kfit <- lapply(1:dim(unique(abskmeans)), function(k) kmeans(abskmeans,k))
kaic <- sapply(kfit,kIC)
kbic  <- sapply(kfit,kIC,"B")
kHDic  <- sapply(kfit,kIC,"C")
par(mar=c(1,1,1,1))
par(mai=c(1,1,1,1))
plot(kaic, xlab="k (# of clusters)", ylab="IC (Deviance + Penalty)", 
     ylim=range(c(kaic,kbic,kHDic)), # get them on same page
     type="l", lwd=2)
# Vertical line where AIC is minimized
abline(v=which.min(kaic))
# Next we plot BIC
lines(kbic, col=4, lwd=2)
# Vertical line where BIC is minimized
abline(v=which.min(kbic),col=4)
# Next we plot HDIC
lines(kHDic, col=3, lwd=2)
# Vertical line where HDIC is minimized
abline(v=which.min(kHDic),col=3)

# Insert labels
text(c(which.min(kaic),which.min(kbic),which.min(kHDic)),c(mean(kaic),mean(kbic),mean(kHDic)),c("AIC","BIC","HDIC"))

# Use map_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:dim(unique(abskmeans)),  function(k){
  model <- kmeans(x = abskmeans, centers = k)
  model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:dim(unique(abskmeans)),
  tot_withinss = tot_withinss
)

# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(limit = c(1,100))

#Using HDIC we get 3 clusters as the elbow curve analysis informs us of using more clusters
kmeanclust <- kmeans(abskmeans, centers=3)
abs3 <- mutate(abskmeans, clusterkmeans = kmeanclust$cluster)

clusplot(abs3[,-1], kmeanclust$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)
#Cluster Categorisation
pie(colSums(abscat[kmeanclust$cluster==1,]),cex=0.5)
pie(colSums(abscat[kmeanclust$cluster==2,]),cex=0.5)
pie(colSums(abscat[kmeanclust$cluster==3,]),cex=0.5)

#understanding the Clusters
aggregate(abscat, by=list(abs3$clusterkmeans), FUN =mean)
count(abs3,clusterkmeans)




###### Predictive Modeling
### Simple Linear regression
abs_lm <- abs1[!names(abs1) %in% c('ID')]#abs1 %>% select(-c('ID'))
lm <- lm(Absenteeism.time.in.hours ~ ., data = abs_lm)
My_lm <- abs_lm$Absenteeism.time.in.hours
summary(lm)


### Simple Linear regression after stepwise selection
#backwards selection 
FitAll = lm(Absenteeism.time.in.hours ~  . , data = abs_lm) 
lm_backwards <- step(FitAll,direction = 'backward') 
# lm(formula = Absenteeism.time.in.hours ~ Reason.for.absence + Age + Work.load.Average.day + Education + Son + Weight + Height, data = abs_lm)
## AIC=3688.74 

#forwards selection 
lm.nothing = lm(Absenteeism.time.in.hours ~  1 , data = abs_lm) 
lm_forwards <- step(lm.nothing,direction = 'forward', scope=formula('FitAll')) 
# lm(formula = Absenteeism.time.in.hours ~ Reason.for.absence + Height + Son + Age + Day.of.the.week, data = abs1) 
## AIC=3688.74 

#Stepwise Selection 
lm_both <- step(lm.nothing,direction = 'both', scope=formula('FitAll')) 
# lm(formula = Absenteeism.time.in.hours ~ Reason.for.absence + Height + Son + Age + Day.of.the.week, data = abs1) 
## AIC=3688.74 


## Lasso for Simple Linear regression 
Mx_lm <- model.matrix(Absenteeism.time.in.hours ~ ., data=abs_lm)[,-1]
My_lm <- abs_lm$Absenteeism.time.in.hours
lasso_lm <- glmnet(Mx_lm,My_lm)
#summary(lasso_lm)
lassoCV_lm <- cv.glmnet(Mx_lm,My_lm)
# min cvm & ise cvm
features.min.lm <- support(lasso_lm$beta[,which.min(lassoCV_lm$cvm)])
length(features.min.lm)
features.1se.lm <- support(lasso_lm$beta[,which.min((lassoCV_lm$lambda-lassoCV_lm$lambda.1se)^2)])
length(features.1se.lm) 
# new dataset
data.min.lm <- data.frame(Mx_lm[,features.min.lm],My_lm)
data.1se.lm <- data.frame(Mx_lm[,features.1se.lm],My_lm)


### Simple Linear regression w interaction
abs_lm_int <- abs1[!names(abs1) %in% c('ID')]
lm_int <- lm(Absenteeism.time.in.hours ~ .^2, data = abs_lm_int)
# most of them are not significant. run AIC to select only some of them
### How many coefficients in the model?
nrow(summary(lm_int)$coef)
### we have a lot of dummy variables; we actually had 
length(lm_int$coef)
### but 2207 - 678 =1529 were compeltely redundant. 
### do you think all of them matter? Lets look at p-values
pvals<-summary(lm_int)$coef[,4]
hist(pvals, breaks = seq(from=0,to=1,by=0.05), xlab="p-values", main="P-values of Linear regression model with interactions", col="lightblue")


### Simple Linear regression w interaction after stepwise
#backwards selection 
#lm_int_back <- step(lm_int,direction = 'backward') 
# 
## AIC= 3474.99
#forwards selection 
lm_int.nothing = lm(Absenteeism.time.in.hours ~  1 , data = abs_lm_int) 
lm_int_forward <- step(lm_int.nothing,direction = 'forward', scope=formula('lm_int')) 
# lm(formula = Absenteeism.time.in.hours ~ Reason.for.absence + Height + Age + Son + Pet + Day.of.the.week + Seasons + Work.load.Average.day + Social.drinker + Reason.for.absence:Height + Reason.for.absence:Age + Height:Son + Reason.for.absence:Son + Reason.for.absence:Day.of.the.week + Son:Day.of.the.week + Height:Day.of.the.week + Height:Pet + Height:Age + Age:Pet + Height:Work.load.Average.day + Age:Day.of.the.week + Day.of.the.week:Social.drinker + Age:Son + Son:Work.load.Average.day, data = abs_lm_int)
## AIC=3443 
#Stepwise Selection 
lm_int_both <- step(lm_int.nothing,direction = 'both', scope=formula('lm_int')) 
# lm(formula = Absenteeism.time.in.hours ~ Reason.for.absence + Height + Age + Son + Pet + Day.of.the.week + Seasons + Work.load.Average.day + Weight + Reason.for.absence:Height + Reason.for.absence:Age + Reason.for.absence:Son + Reason.for.absence:Day.of.the.week + Son:Day.of.the.week + Height:Age + Age:Pet + Height:Work.load.Average.day + Day.of.the.week:Weight + Age:Day.of.the.week + Age:Son + Son:Work.load.Average.day, data = abs_lm_int)
## AIC=3429.83 


### Lasso for Simple Linear regression w interaction
Mx_lm_int <- model.matrix(Absenteeism.time.in.hours ~ .^2, data=abs_lm_int)[,-1]
My_lm_int <- abs_lm_int$Absenteeism.time.in.hours
lasso_lm_int <- glmnet(Mx_lm_int,My_lm_int)
#summary(lasso_lm_int)
lassoCV_lm_int <- cv.glmnet(Mx_lm_int,My_lm_int)
# min cvm & ise cvm
features.min.lm.int <- support(lasso_lm_int$beta[,which.min(lassoCV_lm_int$cvm)])
length(features.min.lm.int)
features.1se.lm.int <- support(lasso_lm_int$beta[,which.min((lassoCV_lm_int$lambda-lassoCV_lm_int$lambda.1se)^2)])
length(features.1se.lm.int) 
# new dataset
data.min.lm.int <- data.frame(Mx_lm_int[,features.min.lm.int],My_lm_int)
data.1se.lm.int <- data.frame(Mx_lm_int[,features.1se.lm.int],My_lm_int)
# Plot the cross validation of Lasso
par(mar=c(1.5,1.5,2,1.5))
par(mai=c(1.5,1.5,2,1.5))
plot(lassoCV_lm_int, main="Fitting Graph for CV Lasso \n \n # of non-zero coefficients  ", xlab = expression(paste("log(",lambda,")")))


### Tree
abs_tree <- abs1[!names(abs1) %in% c('ID')]
tree <- tree(Absenteeism.time.in.hours ~ ., data=abs_tree, control=tree.control(nobs = dim(abs_tree)[1], mincut = 2, minsize = 5, mindev = 0.002)) 
summary(tree)


### Lasso for Simple Linear regression w interaction
Mx_tree <- model.matrix(Absenteeism.time.in.hours ~ ., data=abs_tree, control=tree.control(nobs = dim(abs_tree)[1], mincut = 2, minsize = 5, mindev = 0.002))[,-1]
My_tree <- abs_tree$Absenteeism.time.in.hours
lasso_tree <- glmnet(Mx_tree,My_tree)
#summary(lasso_lm_int)
lassoCV_tree <- cv.glmnet(Mx_tree,My_tree)
# min cvm & ise cvm
features.min.tree <- support(lasso_tree$beta[,which.min(lassoCV_tree$cvm)])
length(features.min.tree)
features.1se.tree <- support(lasso_tree$beta[,which.min((lassoCV_tree$lambda-lassoCV_tree$lambda.1se)^2)])
length(features.1se.tree) 
# new dataset
data.min.tree <- data.frame(Mx_tree[,features.min.tree],My_tree)
data.1se.tree <- data.frame(Mx_tree[,features.1se.tree],My_tree)


### Random Forest
abs_rf <- abs1[!names(abs1) %in% c('ID')]
rf <- randomForest(Absenteeism.time.in.hours ~., data=abs_rf, nodesize=5, ntree = 500, mtry = 4)
plot(rf, main = 'Error for Random Forest with 500 trees')
# It is overfitting with 500 trees as error went up after the 100th tree. 
# We rerun the model with 100 trees
rf <- randomForest(Absenteeism.time.in.hours ~., data=abs_rf, nodesize=5, ntree = 100, mtry = 4)
plot(rf, main = 'Error for Random Forest with 100 trees')


### OOS - original
nfold <- 10
n <- nrow(abs1)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
OOS.lm <- data.frame(lm=rep(NA,nfold), lm.aic=rep(NA,nfold)) 
L.OOS.lm <- data.frame(L.min.lm=rep(NA,nfold), L.1se.lm=rep(NA,nfold)) 
PL.OOS.lm <- data.frame(PL.min.lm=rep(NA,nfold), PL.1se.lm=rep(NA,nfold)) 
OOS.lm.int <- data.frame(lm.int=rep(NA,nfold), lm.int.aic=rep(NA,nfold)) 
L.OOS.lm.int <- data.frame(L.min.lm.int=rep(NA,nfold), L.1se.lm.int=rep(NA,nfold)) 
PL.OOS.lm.int <- data.frame(PL.min.lm.int=rep(NA,nfold), PL.1se.lm.int=rep(NA,nfold)) 
OOS.tree <- data.frame(rf=rep(NA,nfold), tree=rep(NA,nfold)) 
L.OOS.tree <- data.frame(L.min.tree=rep(NA,nfold), L.1se.tree=rep(NA,nfold)) 
PL.OOS.tree <- data.frame(PL.min.tree=rep(NA,nfold), PL.1se.tree=rep(NA,nfold)) 

for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'

  # Simple Linear regression
  rlm <- glm(Absenteeism.time.in.hours ~ ., data = abs_lm, subset = train)
  test_lm <- abs_lm[-train,]
  rlm$xlevels[["Reason.for.absence"]] <- union(rlm$xlevels[["Reason.for.absence"]], levels(test_lm$Reason.for.absence))
  pred_lm <- predict(rlm, newdata=test_lm, type="response")
  OOS.lm$lm[k] <- R2(y=abs_lm$Absenteeism.time.in.hours[-train], pred=pred_lm)
  
  # Simple Linear regression after stepwise selection
  rlm_aic <- lm(Absenteeism.time.in.hours ~ Reason.for.absence + Height + Son + Age + Day.of.the.week, data = abs_lm, subset = train)
  rlm_aic$xlevels[["Reason.for.absence"]] <- union(rlm_aic$xlevels[["Reason.for.absence"]], levels(test_lm$Reason.for.absence))
  pred_lm_aic <- predict(rlm_aic, newdata=test_lm, type="response")
  OOS.lm$lm.aic[k] <- R2(y=My_lm[-train], pred=pred_lm_aic)
  
  # Lasso for Simple Linear regression
  lassomin_lm  <- glmnet(Mx_lm[train,],My_lm[train],lambda = lassoCV_lm$lambda.min)
  predlassomin_lm <- predict(lassomin_lm, newx=Mx_lm[-train,], type="response")
  L.OOS.lm$L.min.lm[k] <- R2(y=My_lm[-train], pred=predlassomin_lm)
  
  lasso1se_lm  <- glmnet(Mx_lm[train,],My_lm[train],lambda = lassoCV_lm$lambda.1se)
  predlasso1se_lm  <- predict(lasso1se_lm, newx=Mx_lm[-train,], type="response")
  L.OOS.lm$L.1se.lm[k] <- R2(y=My_lm[-train], pred=predlasso1se_lm)
  
  # Post Lasso for Simple Linear regression -- min
  if (length(features.min.lm) == 0){
    rmin_lm <- glm(Absenteeism.time.in.hours~1, data=abs_lm, subset=train)
  } else {
    rmin_lm <- glm(My_lm~., data=data.min.lm, subset=train)
  }
  if (length(features.min.lm) == 0){
    predmin_lm <- predict(rmin_lm, newdata=abs_lm[-train,], type="response")
  } else {
    predmin_lm <- predict(rmin_lm, newdata=data.min.lm[-train,], type="response")
  }
  PL.OOS.lm$PL.min.lm[k] <- R2(y=My_lm[-train], pred=predmin_lm)
  
  # Post Lasso for Simple Linear regression -- 1se
  if (length(features.1se.lm) == 0){  
    r1se_lm <- glm(Absenteeism.time.in.hours~1, data=abs_lm, subset=train) 
  } else {
    r1se_lm <- glm(My_lm~., data=data.1se.lm, subset=train)
  }
  
  if ( length(features.1se.lm) == 0){  
    pred1se_lm <- predict(r1se_lm, newdata=abs_lm[-train,], type="response")
  } else {
    pred1se_lm  <- predict(r1se_lm, newdata=data.1se.lm[-train,], type="response")
  }
  PL.OOS.lm$PL.1se.lm[k] <- R2(y=My_lm[-train], pred=pred1se_lm)

  # Simple Linear regression w interaction
  rlm_int <- lm(Absenteeism.time.in.hours ~ Reason.for.absence + Height + Son + Age + Day.of.the.week + (Reason.for.absence + Height + Son + Age + Day.of.the.week)*Reason.for.absence + (Reason.for.absence + Height + Son + Age + Day.of.the.week)*Education + (Reason.for.absence + Height + Son + Age + Day.of.the.week)*Son, data = abs_lm_int, subset = train)
  test_lm_int <- abs_lm_int[-train,]
  rlm_int$xlevels[["Reason.for.absence"]] <- union(rlm_int$xlevels[["Reason.for.absence"]], levels(test_lm_int$Reason.for.absence))
  pred_lm_int <- predict(rlm_int, newdata=test_lm_int, type="response")
  OOS.lm.int$lm.int[k] <- R2(y=My_lm_int[-train], pred=pred_lm_int)
  
  # Simple Linear regression w interaction after stepwise
  rlm_int_aic <- lm(Absenteeism.time.in.hours ~ Reason.for.absence + Height + Age + Son + Reason.for.absence*Height + Reason.for.absence*Age + Height*Son + Reason.for.absence*Son, data = abs_lm_int, subset = train)
  rlm_int_aic$xlevels[["Reason.for.absence"]] <- union(rlm_int_aic$xlevels[["Reason.for.absence"]], levels(test_lm_int$Reason.for.absence))
  pred_lm_int_aic <- predict(rlm_int_aic, newdata=abs_lm_int[-train,], type="response")
  OOS.lm.int$lm.int.aic[k] <- R2(y=My_lm_int[-train], pred=pred_lm_int_aic)
  
  # Lasso for Simple Linear regression w interaction
  lassomin_lm_int  <- glmnet(Mx_lm_int[train,],My_lm_int[train],lambda = lassoCV_lm_int$lambda.min)
  predlassomin_lm_int <- predict(lassomin_lm_int, newx=Mx_lm_int[-train,], type="response")
  L.OOS.lm.int$L.min.lm.int[k] <- R2(y=My_lm_int[-train], pred=predlassomin_lm_int)
  
  lasso1se_lm_int  <- glmnet(Mx_lm_int[train,],My_lm_int[train],lambda = lassoCV_lm_int$lambda.1se)
  predlasso1se_lm_int  <- predict(lasso1se_lm_int, newx=Mx_lm_int[-train,], type="response")
  L.OOS.lm.int$L.1se.lm.int[k] <- R2(y=My_lm_int[-train], pred=predlasso1se_lm_int)

  # Post Lasso for Simple Linear regression w interaction -- min
  if (length(features.min.lm.int) == 0){
    rmin_lm_int <- glm(Absenteeism.time.in.hours~1, data=abs_lm_int, subset=train)
  } else {
    rmin_lm_int <- glm(My_lm_int~., data=data.min.lm.int, subset=train)
  }
  if (length(features.min.lm.int) == 0){
    predmin_lm_int <- predict(rmin_lm_int, newdata=abs_lm_int[-train,], type="response")
  } else {
    predmin_lm_int <- predict(rmin_lm_int, newdata=data.min.lm.int[-train,], type="response")
  }
  PL.OOS.lm.int$PL.min.lm.int[k] <- R2(y=My_lm_int[-train], pred=predmin_lm_int)
  
  # Post Lasso for Simple Linear regression w interaction -- 1se
  if (length(features.1se.lm.int) == 0){  
    r1se_lm_int <- glm(Absenteeism.time.in.hours~1, data=abs_lm_int, subset=train) 
  } else {
    r1se_lm_int <- glm(My_lm_int~., data=data.1se.lm.int, subset=train)
  }
  
  if ( length(features.1se.lm.int) == 0){  
    pred1se_lm_int <- predict(r1se_lm_int, newdata=abs_lm_int[-train,], type="response")
  } else {
    pred1se_lm_int  <- predict(r1se_lm_int, newdata=data.1se.lm.int[-train,], type="response")
  }
  PL.OOS.lm.int$PL.1se.lm.int[k] <- R2(y=My_lm_int[-train], pred=pred1se_lm_int)
  
  # Tree
  rtree <- tree(Absenteeism.time.in.hours ~ ., data = abs_tree, subset = train, control=tree.control(nobs = dim(abs_tree)[1], mincut = 2, minsize = 5, mindev = 0.002)) 
  pred_tree <- predict(rtree, newdata=abs_tree[-train,])
  OOS.tree$tree[k] <- R2(y=abs_tree$Absenteeism.time.in.hours[-train], pred=pred_tree)
  
  # Lasso for Tree
  lassomin_tree  <- glmnet(Mx_tree[train,],My_tree[train],lambda = lassoCV_tree$lambda.min)
  predlassomin_tree <- predict(lassomin_tree, newx=Mx_tree[-train,], type="response")
  L.OOS.tree$L.min.tree[k] <- R2(y=My_tree[-train], pred=predlassomin_tree)
  
  lasso1se_tree  <- glmnet(Mx_tree[train,],My_tree[train],lambda = lassoCV_tree$lambda.1se)
  predlasso1se_tree  <- predict(lasso1se_tree, newx=Mx_tree[-train,], type="response")
  L.OOS.tree$L.1se.tree[k] <- R2(y=My_tree[-train], pred=predlasso1se_tree)
  
  # Post Lasso for Simple Linear regression w interaction -- min
  if (length(features.min.tree) == 0){
    rmin_tree <- glm(Absenteeism.time.in.hours~1, data=abs_tree, subset=train)
  } else {
    rmin_tree <- glm(My_tree~., data=data.min.tree, subset=train)
  }
  if (length(features.min.tree) == 0){
    predmin_tree <- predict(rmin_tree, newdata=abs_tree[-train,], type="response")
  } else {
    predmin_tree <- predict(rmin_tree, newdata=data.min.tree[-train,], type="response")
  }
  PL.OOS.tree$PL.min.tree[k] <- R2(y=My_tree[-train], pred=predmin_tree)
  
  # Post Lasso for Simple Linear regression w interaction -- 1se
  if (length(features.1se.tree) == 0){  
    r1se_tree <- glm(Absenteeism.time.in.hours~1, data=abs_tree, subset=train) 
  } else {
    r1se_tree <- glm(My_tree~., data=data.1se.tree, subset=train)
  }
  
  if ( length(features.1se.tree) == 0){  
    pred1se_tree <- predict(r1se_tree, newdata=abs_tree[-train,], type="response")
  } else {
    pred1se_tree  <- predict(r1se_tree, newdata=data.1se.tree[-train,], type="response")
  }
  PL.OOS.tree$PL.1se.tree[k] <- R2(y=My_tree[-train], pred=pred1se_tree)
  
  # Random Forest
  rrf <- randomForest(Absenteeism.time.in.hours ~., data=abs_rf, subset = train, nodesize=5, ntree = 500, mtry = 4)
  pred_rf <- predict(rrf, newdata=abs_rf[-train,])
  OOS.tree$rf[k] <- R2(y=abs_rf$Absenteeism.time.in.hours[-train], pred=pred_rf)
  
  print(paste("Iteration",k,"of",nfold,"completed"))
}

R2performance <- cbind(OOS.lm, L.OOS.lm, PL.OOS.lm, OOS.lm.int, L.OOS.lm.int, PL.OOS.lm.int, OOS.tree,L.OOS.tree, PL.OOS.tree)
par(mar=c(6.0,6.0,6.0,6.0))
barplot(t(as.matrix(R2performance)), 
        beside=TRUE, legend=TRUE, 
        args.legend = list(x='right', bty = "n", inset=c(-0.3, - 0.2)),
        ylab= bquote( "Out of Sample" ~ R^2), xlab="Fold", names.arg = c(1:10),
        main="Performance: Out of Sample" ~ R^2)

### OOS - final
R2performance <- cbind(L.OOS.lm[names(L.OOS.lm) %in% c('L.min.lm')], 
                       PL.OOS.lm[names(PL.OOS.lm) %in% c('PL.min.lm')], 
                       L.OOS.lm.int[names(L.OOS.lm.int) %in% c('L.min.lm.int')], 
                       PL.OOS.lm.int[names(PL.OOS.lm.int) %in% c('PL.min.lm.int')], 
                       OOS.tree[names(OOS.tree) %in% c('rf')],
                       L.OOS.tree[names(L.OOS.tree) %in% c('L.min.tree')], 
                       PL.OOS.tree[names(PL.OOS.tree) %in% c('PL.min.tree')])
par(mar=c(6.0,6.0,6.0,3.0))
barplot(t(as.matrix(R2performance)), 
        beside=TRUE, legend=TRUE, 
        args.legend = list(x='right', bty = "n", inset=c(-0.26, - 0.2)),
        ylab= bquote( "Out of Sample" ~ R^2), xlab="Fold", names.arg = c(1:10),
        main="Final Performance: Out of Sample" ~ R^2)
boxplot(R2performance, col="plum", las = 2, ylab=expression(paste("OOS ",R^2)), xlab="", main="10-fold Cross Validation")
colMeans(R2performance)


### Refit the whole dataset using our selected model
pred_abs <- predict(rf, newdata=abs_rf)
R2(y=abs_rf$Absenteeism.time.in.hours, pred=pred_abs)
rf$importance
varImpPlot(rf, main = 'Importance of each variable')
library(devtools)
devtools::install_github('araastat/reprtree')
library(reprtree)



###### Causal Modeling: Double selection 
## step 1
lmyx<- lm(Absenteeism.time.in.hours ~ Reason.for.absence +
            Height + Son + Day.of.the.week , data = abs1)

controly <- predict(lmyx)
controly<- as.data.frame(controly)
rownum <- rownames(controly)
e <- cbind(rownum=rownum, controly)
e
rownum <- rownames(abs1)
abs2 <- cbind(rownum=rownum, abs1)

## STEP 2
lmdx <- lm (Age ~ Service.time+ Education+ Son+ Weight+ Height , data = abs1)
summary(lmdx)
controld <- predict(lmdx)
controld<- as.data.frame(controld)
rownum3 <- rownames(controld)
g <- cbind(rownum=rownum3, controld)
g
abs3 <- left_join(abs2, e, by = "rownum", copy = FALSE)

abs3 <- left_join(abs3, g, by = "rownum", copy = FALSE)

## Step 3

absenseCausal<- lm(Absenteeism.time.in.hours~ Age + controly + controld, data = abs3)
summary(absenseCausal)### Regression Tree



######### For Deployment 
ggplot(aes(Day.of.the.week), data = abs1)+
  geom_histogram(stat= "count")

  
