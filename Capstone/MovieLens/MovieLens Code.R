---
title: "MovieLens"
author: "Hiroki Hirayama"
date: "15/04/2020"
output: 
  html_document: 
    theme: flatly
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


Initial attachment of "edX" data to create validation and edx dataset. 
```{r Attaching Data, include=FALSE}
#################################
# Create edx set, validation set#
#################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org") ; library(tidyverse)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org") ; library(caret)
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org") ; library(data.table)
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org") ; library(lubridate)

###########################################
#### Downloading and Attaching Dataset ####
###########################################

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


##################################################
#### End of Downloading and Attaching Dataset ####
##################################################

################################################################################
```


**Exploration and Analysis**

Checking for NAs, number of unique movies and numbers. 
```{r, echo=TRUE}

head(edx,10)
str(edx)

lapply(edx,function(x){
  sum(is.na(x))})

### Checking for number of Unique Movies and Users
edx %>% summarise(no_of_users=n_distinct(userId),n_distinct(movieId),n_distinct(title))
```

```{r echo=FALSE}
```

Plotting the distribution of ratings
```{r echo=TRUE}
edx %>% ggplot(aes(rating)) +
  geom_histogram(breaks=seq(0,5,by=0.5),binwidth=0.5,color="green",fill="blue",alpha=.5) +
  labs(title="Ratings Distribution",x="Rating",y="Number of Ratings") +
  xlim(c(0,5)) + scale_y_continuous(breaks=c(seq(0,3000000,500000)))
mean(edx$rating)
```
The distribution of ratings does not look normal on prima facie; however, this is due to the .5 ratings being given less frequently than round ratings.

Note that the distribution of both round and .5 ratings follow the same pattern:

- There is a rising trend in numbers of round ratings from 0 and this peaks off at ‘4’ and decreases at ’5’.

- There is also a rise of number of .5 ratings from 0, peaking off at 3.5 and decreasing after that.

Through this, it can be inferred that each movie has a different mean rating – and hence, movie ID is deemed a significant variable to be included in the model.

Number of ratings per movie. 
```{r echo=TRUE}
edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 25, fill = "blue", colour = "green", alpha=0.5) +
  scale_x_log10() +
  xlab("Number of Ratings") +
  ylab("Number of Movies") +
  ggtitle("Number of Ratings Per Movie")
### Some movies are rated more than others.
edx %>% group_by(movieId,genres) %>% summarise(mean=mean(rating),n=n()) %>%
  ggplot(aes(n,mean)) + geom_point(colour="orange",alpha=0.5) + geom_smooth(method="loess") + xlab("Number of Ratings") +
  ylab("Mean Rating") +
  ggtitle("Mean Rating x Number of Ratings")
```
There seem to be two groups of movies: ones that are rated more often have scores that are closer to the mean - ones that are rated less often have scores further away from the mean. 

*However, there does not seem to be a strong effect observed here. Hence, this was not modelled in.*

Number of ratings and mean ratings given by users. 
```{r echo=TRUE}
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "green",fill="blue",alpha=0.5) +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of users") +
  ggtitle("Number of ratings given by users")

```
```{r echo=TRUE}
edx %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(mu = mean(rating)) %>%
  ggplot(aes(mu)) +
  geom_histogram(bins = 30, color = "green",fill="blue",alpha=0.5) +
  xlab("Mean Rating Given") +
  ylab("Number of users") +
  ggtitle("Mean Movie Ratings Given By Users")
```

The mean movie ratings distribution graph indicates that users give different mean ratings – this is to be expected as most users have different baseline for ratings (i.e., rating bias).

A further graph plotted indicated that the majority of raters give below 100 movie.
This is to be expected as not all raters is as interested or passionate in giving movie ratings.Both these indicate that user effects are deemed significant and hence, will be included in the model.

*There is a user effect in ratings and hence this will be modelled in.* 

Checking time effect on ratings. 

Converting timestamp to numeric, examining and correcting for wrongly coded dates. 
```{r echo=TRUE}
knitr::opts_chunk$set(echo = TRUE)
### Coding for Age of Movies
edx_with_dates <- edx %>% mutate(date_of_release=as.numeric(str_extract(edx$title,"(\\d{4})")))

### Checking for Wrongly Coded Dates
edx_with_dates %>% filter(date_of_release>2018) %>% group_by(movieId,title,date_of_release) %>% summarize(n=n())
edx_with_dates %>% filter(date_of_release<1900) %>% group_by(movieId,title,date_of_release) %>% summarize(n=n())

### Fixing code for Wrongly Coded Dates
edx_with_dates[edx_with_dates$movieId == "6290", "date_of_release"] <- 2003
edx_with_dates[edx_with_dates$movieId == "6645", "date_of_release"] <- 1971
edx_with_dates[edx_with_dates$movieId == "8198", "date_of_release"] <- 1960
edx_with_dates[edx_with_dates$movieId == "8905", "date_of_release"] <- 1992
edx_with_dates[edx_with_dates$movieId == "53953", "date_of_release"] <- 2007
edx_with_dates[edx_with_dates$movieId == "27266", "date_of_release"] <- 2004
edx_with_dates[edx_with_dates$movieId == "671", "date_of_release"] <- 1996
edx_with_dates[edx_with_dates$movieId == "2308", "date_of_release"] <- 1973
edx_with_dates[edx_with_dates$movieId == "4159", "date_of_release"] <- 2001
edx_with_dates[edx_with_dates$movieId == "5310", "date_of_release"] <- 1985
edx_with_dates[edx_with_dates$movieId == "8864", "date_of_release"] <- 2004
edx_with_dates[edx_with_dates$movieId == "1422", "date_of_release"] <- 1997
edx_with_dates[edx_with_dates$movieId == "4311", "date_of_release"] <- 1998
edx_with_dates[edx_with_dates$movieId == "5472", "date_of_release"] <- 1972


```

Calculate Age of Movie and Plotting Relationship of Age of Movie and Average rating. 
```{r echo=TRUE}
edx_with_dates <- edx_with_dates %>% mutate(age=2018-date_of_release)
head(edx_with_dates)

edx_with_dates %>%
  group_by(age) %>%
  summarise(average_rating=mean(rating)) %>%
  ggplot(aes(age,average_rating)) + geom_point() + geom_smooth(method="loess")

```
It seems that older movies tend to have higher average ratings, although this starts decreasing after a certain point. 

Note that there is a relationship between age and average ratings – hence, the age was deemed a significant predictor and decided to be included in the model

*There is an age effect found in ratings and hence this will be modelled in.*

**Summary from data exploration and preliminary analysis**

Variables to include in model are : 
- Movie ID
- User ID
- Age

```{r echo=TRUE}
summary(lm(rating~userId+movieId+age,data=edx_with_dates))
```

Preliminary Model and Fit Results
Based on the prior analysis, the predictors decided to be significantly relevant to be included in the model are age, User ID and movie ID.
A linear model was fitted and indicated an adjusted R squared of 0.014. Whilst the R squared is low, it is important to note that RMSE is the evaluating benchmark here.
The purpose of this fit is to determine whether the predictors chosen are statistically significant relevant – the fit indicated that all predictors are significant at p<.05 and hence will be included in fitting the final model.


**Start of modelling on Edx dataset**

Note that all these factors will be regularised as well to decrease occurence of overfitting. 

The model is then fitted with regularisation. The parameters were decided based on the lowest RMSE achieved on the edX dataset.

Before that, as RMSE has been chosen as the metric for evaluation, a function has been created to calculate RMSE. 
```{r echo=TRUE}
#RMSE Calculator Function Calculator
RMSE <- function(true, predicted){
  sqrt(mean((true - predicted)^2))}
```

Tuning parameter : Choosing optimal lambda value for regularisation factor. 
```{r echo=TRUE}
#Choosing the Tuning Value
lambdas <- seq(0,2,0.10)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx_with_dates$rating)

  bmovie <- edx_with_dates %>%
    group_by(movieId) %>%
    summarize(bmovie = sum(rating - mu)/(n() + l))

  buser <- edx_with_dates %>%
    left_join(bmovie, by="movieId") %>%
    group_by(userId) %>%
    summarize(buser = sum(rating - bmovie - mu)/(n() +l))

  bage <- edx_with_dates %>%
    left_join(bmovie, by="movieId") %>%
    left_join(buser, by = "userId") %>%
    group_by(age) %>%
    summarize(bage = sum(rating - bmovie - buser - mu)/(n() +l))


  predicted_ratings <- edx_with_dates %>%
    left_join(bmovie, by = "movieId") %>%
    left_join(buser, by = "userId") %>%
    left_join(bage, by = "age") %>%
    mutate(pred = mu + bmovie +  buser + bage) %>% .$pred

  return(RMSE(predicted_ratings, edx_with_dates$rating))
})

#########Finding Optimal Lambda
qplot(lambdas, rmses)
lambdas[which.min(rmses)]
```
The optimal lambda value for regularisation found is 0.3. 

***

**Testing on Validation Set**

Recoding the validation test set to extract age out of timestamp. 
```{r include=FALSE}
validation <- validation %>% mutate(date_of_release=as.numeric(str_extract(validation$title,"(\\d{4})")))


### Checking for Wrongly Coded Dates
validation %>% filter(date_of_release>2018) %>% group_by(movieId,title,date_of_release) %>% summarize(n=n())
validation %>% filter(date_of_release<1900) %>% group_by(movieId,title,date_of_release) %>% summarize(n=n())

validation[validation$movieId == "6290", "date_of_release"] <- 2003
validation[validation$movieId == "6645", "date_of_release"] <- 1971
validation[validation$movieId == "8198", "date_of_release"] <- 1960
validation[validation$movieId == "8905", "date_of_release"] <- 1992
validation[validation$movieId == "53953", "date_of_release"] <- 2007
validation[validation$movieId == "27266", "date_of_release"] <- 2004
validation[validation$movieId == "671", "date_of_release"] <- 1996
validation[validation$movieId == "2308", "date_of_release"] <- 1973
validation[validation$movieId == "4159", "date_of_release"] <- 2001
validation[validation$movieId == "5310", "date_of_release"] <- 1985
validation[validation$movieId == "8864", "date_of_release"] <- 2004
validation[validation$movieId == "1422", "date_of_release"] <- 1997
validation[validation$movieId == "4311", "date_of_release"] <- 1998
validation[validation$movieId == "5472", "date_of_release"] <- 1972

validation <- validation %>% mutate(age=2018-date_of_release)
```

Testing 

```{r echo=TRUE}
mu <- mean(validation$rating)
l <- 0.3
bmovie <- validation %>%
  group_by(movieId) %>%
  summarize(bmovie = sum(rating - mu)/(n() + l))

buser <- validation %>%
  left_join(bmovie, by="movieId") %>%
  group_by(userId) %>%
  summarize(buser = sum(rating - bmovie - mu)/(n() +l))

bage <- validation %>%
  left_join(bmovie, by="movieId") %>%
  left_join(buser, by = "userId") %>%
  group_by(age) %>%
  summarize(bage = sum(rating - bmovie - buser - mu)/(n() +l))


predicted_ratings <- validation %>%
  left_join(bmovie, by = "movieId") %>%
  left_join(buser, by = "userId") %>%
  left_join(bage, by = "age") %>%
  mutate(pred = mu + bmovie +  buser + bage) %>% .$pred

RMSE(predicted_ratings, validation$rating)
```

Reported RMSE : 0.8250934


