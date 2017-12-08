```{r echo=FALSE, message=FALSE}
sqlite    <- dbDriver("SQLite")
exampledb <- dbConnect(sqlite,"../input/database.sqlite")

#Take the variables we are interested in
dataset <- dbGetQuery(exampledb,"SELECT reviews.reviewid, reviews.score, reviews.best_new_music, 
reviews.author, reviews.pub_year, genres.genre FROM reviews JOIN genres 
ON reviews.reviewid = genres.reviewid")

dbDisconnect(exampledb)

#Review statistics
str(dataset)

summary(dataset)

# A look at whats missing
#missmap(dataset)

#Code missing to none
dataset$genre[is.na(dataset$genre)] <- "none"

#Lets see how many genres we are working with
#unique(dataset$genre)

#Lets look at how many reviews there are a year
#table(dataset$pub_year)

count_table <- table(dataset$genre, dataset$pub_year)
count_table <- as.data.frame(count_table)
count_table <- rename(count_table, c("Var1"="genre", "Var2"="year"))
count_table$year <- as.numeric(as.character(count_table$year))
ggplot(count_table, aes(count_table, x=year, y=Freq, fill=genre)) + geom_area()

my_fun=function(vec){ as.numeric(vec[3]) / sum(count_table$Freq[count_table$year==vec[2]]) *100 }
count_table$prop=apply(count_table , 1 , my_fun)
ggplot(count_table, aes(x=year, y=prop, fill=genre)) + geom_area(alpha=0.6 , size=0.5, colour="black")

normalize <- function(x) {
return ((x - min(x)) / (max(x) - min(x))) }

dataset_n <- as.data.frame(lapply(dataset[c(2,3,5)], normalize))
summary(dataset_n)

# create a list of 80% of the rows in the original dataset we can use for training
#validation_index <- createDataPartition(dataset$score, p=0.8, list=FALSE)
# select 20% of the data for validation
#validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
#dataset <- dataset[validation_index,]

# dimensions of dataset
#dim(dataset)

# summarize the class distribution
percentage <- prop.table(table(dataset$genre)) * 100
cbind(freq=table(dataset$genre), percentage=percentage)

# split input and output
x <- dataset[,c(3,5)]
y <- dataset[,2]

dataset$pub_year <- as.character(dataset$pub_year)

#Lets see if scores change over the years
p <- ggplot(dataset, aes(pub_year, score))
p + geom_boxplot(aes(color=pub_year))

#Lets compare scores across genres
p <- ggplot(dataset, aes(genre, score))
p + geom_boxplot(aes(color=genre))

# barplot for class breakdown
#plot(y)

# scatterplot matrix
featurePlot(x=x, y=y, plot="pairs")

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "RMSE"

# a) linear algorithms
#set.seed(7)
#fit.lda <- train(score~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
#set.seed(7)
#fit.cart <- train(score~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
#set.seed(7)
#fit.knn <- train(score~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
#set.seed(7)
#fit.svm <- train(score~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
#set.seed(7)
#fit.rf <- train(score~., data=dataset_n, method="rf", metric=metric, trControl=control)
#summary(fit.rf)
#plot(fit.rf)

aggdata <-aggregate(dataset$score, by=list(dataset$genre,dataset$pub_year), FUN=mean, na.rm=TRUE)
aggdata<-rename(aggdata, c("Group.1"="genre", "Group.2"="year","x"="mean_score"))
ggplot(data=aggdata, aes(x=year, y=mean_score, colour=genre, group = genre)) + geom_smooth(method = "auto", se = FALSE)
#dataset %>% ggvis(score, year) %>% layer_points()
```