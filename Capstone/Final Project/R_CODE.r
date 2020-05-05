#####################
####Initial Setup####
#####################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org") ; library(tidyverse)
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org") ; library(dplyr)
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org") ; library(ggplot2)
if(!require(stats)) install.packages("stats", repos = "http://cran.us.r-project.org") ; library(stats)
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org") ; library(ggthemes)
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org") ; library(stringr)
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org") ; library(gridExtra)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org") ; library(caret)
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org") ; library(lubridate)
if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org") ; library(nnet)
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org") ; library(psych)
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org") ; library(randomForest)
link <- ("https://raw.githubusercontent.com/hirokihirayama/harvard_capstone/master/Documents/harvardcapstone/Final%20Project/investments_VC.csv")
datasource <- read.csv(link)

############################
####End of Initial Setup####
############################

#####################################################
####Data Pre - Processing and Feature Engineering####
#####################################################

head(datasource)
str(datasource)
#Checking for complete cases in data.
sum(complete.cases(datasource))/nrow(datasource)
sum(complete.cases(datasource))
#Describing data.
describe(datasource)
datasource1 <- datasource[,-c(1,2,3,4,6,8,9,10,14,15,16,32:39)]
####Removing factors that are redundant.
datasource1[datasource1==""] <- NA
###Further detected that a few features have "" rather than NA coded into them.
###Forcing NA codes for data with "" entries.

###Checking for complete cases again.
sum(complete.cases(datasource1))
mean(complete.cases(datasource1))
vcdata <- na.omit(datasource1)
sum(complete.cases(vcdata))

###Feature Engineering***
vcdata$market <- as.factor(vcdata$market) #Converting to factors.
#Status, Country Code, State Code, City are all already in factors.
#Further investigating "city" feature as it has 4189 levels.
vcdata %>% group_by(city) %>% summarise(n=n()) %>% arrange(desc(n))
#Found features that contain \x in the data.
vcdata$city <- factor(str_replace_all(vcdata$city, "[^[A-Za-z]]", " "))
#Removed all \x in data. Reduced to 4127 levels after removing \x in data.
#Converting to date format.
vcdata$founded_at <- as.Date(vcdata$founded_at, "%Y-%m-%d")
vcdata$first_funding_at <- as.Date(vcdata$first_funding_at, "%Y-%m-%d")
vcdata$last_funding_at <- as.Date(vcdata$last_funding_at, "%Y-%m-%d")

vcdata %>% select(seed,venture,equity_crowdfunding,undisclosed,convertible_note,
  debt_financing,angel,grant,private_equity,post_ipo_equity,post_ipo_debt,
  secondary_market,product_crowdfunding) %>% range()
# Range is 0 to 30,079,503,000. This will be expensive to compute - hence, the
# data will be transformmed with log transformations.

vcdata_funding <- vcdata[,8:20]
#Coding 0s with NAs first as log 0 is infinity.
vcdata_funding[vcdata_funding == 0] <- NA
vcdata_funding <- log(vcdata_funding)

#Recoding NAs with 0.
vcdata_funding[is.na(vcdata_funding)] <- 0

#Checking if transformation was done correctly.
range(vcdata_funding)

#Repackaging dataset.
vcdata[,8:20] <- vcdata_funding
#Removing extra data sets to clean up environment.
rm(vcdata_funding)

vcdata <- vcdata%>%mutate(funding_days_gap=last_funding_at-first_funding_at)
#Checking if feature has been implemented correctly.

range(as.numeric(vcdata$funding_days_gap))
#Finding for rows where date gaps are above 5000 days.

sum(vcdata$funding_days_gap>5000)
vcdata_outliers <- vcdata[vcdata$funding_days_gap>5000,]

#Extracting Outlier rows to be further investigated.
vcdata <- vcdata[!(vcdata$funding_days_gap>5000),]

#Removing Outlier rows for east of rebuilding dataset after.
vcdata_outliers$first_funding_at[c(2,5,15,17)] <- as.Date(c("2016-06-01","2012-08-01","2013-07-05","2012-07-24"),"%Y-%m-%d")
vcdata_outliers$last_funding_at[c(2,5,15,17)] <- as.Date(c("2016-07-08","2014-11-19","2019-07-26","2014-11-11"),"%Y-%m-%d")
#Corrected outliers for the actual dates found on CrunchBase.
vcdata_outliers <- vcdata_outliers%>%mutate(funding_days_gap=last_funding_at-first_funding_at)

#Reconstructing Dataset
vcdata <- rbind(vcdata,vcdata_outliers)

#Checking if implemented correctly.
range(vcdata$funding_days_gap)

#Removing redundant status factor level.
vcdata$status <- factor(as.character(vcdata$status))

#############################################################
####End of Data Pre - Processing and Feature Engineering####
############################################################

###########################
####Analysis and Plots#####
###########################

#Pie Chart of Intended Predicted Outcomes

vcdata %>% group_by(status) %>%
  summarise(n=n()) %>%
  mutate(prop=scales::percent(round(n/sum(n),2))) %>%
  ggplot(aes(x="",y=n,fill=status)) +
  geom_bar(width=1,stat="identity") +
  coord_polar("y") + theme_fivethirtyeight() +
  xlab(" ") + ylab(" ") + guides(fill=guide_legend(title="Status"))

#Proportion of startups that are operating are too high - this will definitely cause a problem with the prediction algorithm built.

#Start-up Formation Trends Over Time
vcdata %>% mutate(founded=substring(founded_at,1,4)) %>%
  count(founded)%>% arrange(-n) %>% head(12) %>%
  ggplot(aes(founded, n)) + geom_col(aes(fill=n)) + coord_flip() +
  theme(legend.position="none") +
  ggtitle("Year Founded") + xlab("") + ylab(" ") + theme_fivethirtyeight() + theme(legend.position="none")

#Start-up Patterns by Industry
vcdata %>% mutate(founded=substring(founded_at,1,4)) %>% filter(market!="") %>%
  group_by(market) %>% summarize(n=n())%>% arrange(-n) %>% head(10) %>%
  ggplot(aes(reorder(market, n), n)) + geom_col(aes(fill=n)) + coord_flip() +
  theme(legend.position="none") +
  ggtitle("Top 10 Industries") + xlab("Market") + ylab("Count") +
  theme_fivethirtyeight()

#Start-Up Patterns by Industry, by Year
vcdata %>% mutate(founded=substring(founded_at,1,4)) %>%
  filter(market %in% c(' Software ',' Biotechnology ',' Mobile ', ' Curated Web ',
  ' Enterprise Software ', ' Health Care ', ' E-Commerce ',' Hardware + Software ',' Health and Wellness ', ' Advertising ')) %>%
  filter(founded>=2004) %>% group_by(market,founded) %>% summarize(n=n())%>% arrange(-n) %>%
  ggplot(aes(x=founded,y=n, fill=market)) + geom_col(position="fill") +
  theme(legend.position="none") +
  ggtitle("Industry Trends from 2004 to 2014") +
  theme_fivethirtyeight() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + guides(fill=guide_legend(title="Market"))

#Location
vcdata %>% count(city)%>% arrange(-n) %>% head(10) %>%
 ggplot(aes(reorder(city, n), n)) + geom_col(aes(fill=city),show.legend=FALSE) + coord_flip() +
 theme(legend.position="none") +
 ggtitle("Top 10 Cities") + xlab(" ") + ylab(" ") + theme_fivethirtyeight()

#Distribution of Funding
seed <- vcdata %>% filter(seed>0) %>% ggplot(aes(x=seed,fill=status)) + geom_histogram(aes(fill=status),bins=70) + xlab(" ") + ylab(" ") + theme_fivethirtyeight() + ggtitle("Seed Funding") + theme(legend.text=element_text(size=8), legend.key.size = unit(0.25,"line"), plot.title = element_text(size = 10, face = "bold")) +theme(legend.title = element_text(size = 8)) + theme(legend.position="none") + theme(axis.text.x = element_text(size=6),axis.text.y=element_text(size=6))

equity_crowdfunding <- vcdata %>% filter(equity_crowdfunding>0) %>% ggplot(aes(x=equity_crowdfunding,fill=status)) + geom_histogram(aes(fill=status),bins=70) + xlab(" ") + ylab(" ") + theme_fivethirtyeight() + ggtitle("Equity Crowd Funding") + theme(plot.title = element_text(size = 10, face = "bold")) +
theme(legend.position="none") + theme(axis.text.x = element_text(size=6),axis.text.y=element_text(size=6)) +  scale_fill_manual(values=c("#f8766d","#639cfb","black"))

convertiblenote <- vcdata %>% filter(convertible_note>0) %>% ggplot(aes(x=convertible_note,fill=status)) + geom_histogram(aes(fill=status),bins=70) + xlab(" ") + ylab(" ") + theme_fivethirtyeight() + ggtitle("Convertible Notes Funding ") + theme(legend.text=element_text(size=8), legend.key.size = unit(0.25,"line"), plot.title = element_text(size = 10, face = "bold")) +theme(legend.title = element_text(size = 8)) + theme(legend.position="none") + theme(axis.text.x = element_text(size=6),axis.text.y=element_text(size=6))

debtfinancing <- vcdata %>% filter(debt_financing>0) %>% ggplot(aes(x=debt_financing,fill=status)) + geom_histogram(aes(fill=status),bins=70) + xlab(" ") + ylab(" ") + theme_fivethirtyeight() + ggtitle("Debt Financing") +
theme(plot.title = element_text(size = 10, face = "bold")) +
theme(legend.position="none")+ theme(axis.text.x = element_text(size=6),axis.text.y=element_text(size=6))

angel <- vcdata %>% filter(angel>0) %>% ggplot(aes(x=angel,fill=status)) + geom_histogram(aes(fill=status),bins=70) + xlab(" ") + ylab(" ") + theme_fivethirtyeight() + ggtitle("Angel Funding") +
theme(plot.title = element_text(size = 10, face = "bold")) +
theme(legend.position="none")+ theme(axis.text.x = element_text(size=6),axis.text.y=element_text(size=6))

grant <- vcdata %>% filter(grant>0) %>% ggplot(aes(x=grant,fill=status)) + geom_histogram(aes(fill=status),bins=70) + xlab(" ") + ylab(" ") + theme_fivethirtyeight() + ggtitle("Grant Funding ") +
theme(plot.title = element_text(size = 10, face = "bold")) +
theme(legend.position="none")+ theme(axis.text.x = element_text(size=6),axis.text.y=element_text(size=6))

private_equity <- vcdata %>% filter(private_equity>0) %>% ggplot(aes(x=private_equity,fill=status)) + geom_histogram(aes(fill=status),bins=70) + xlab(" ") + ylab(" ") + theme_fivethirtyeight() + ggtitle("Private Equity") +
theme(plot.title = element_text(size = 10, face = "bold")) +
theme(legend.position="none")+ theme(axis.text.x = element_text(size=6),axis.text.y=element_text(size=6))

post_ipo_equity<- vcdata %>% filter(post_ipo_equity>0) %>% ggplot(aes(x=post_ipo_equity,fill=status)) + geom_histogram(aes(fill=status),bins=70) + xlab(" ") + ylab(" ") + theme_fivethirtyeight() + ggtitle("Post IPO Equity") + theme(plot.title = element_text(size = 10, face = "bold")) +
theme(legend.position="none")+ theme(axis.text.x = element_text(size=6),axis.text.y=element_text(size=6))

undisclosed <- vcdata %>% filter(undisclosed>0) %>% ggplot(aes(x=undisclosed,fill=status)) + geom_histogram(aes(fill=status),bins=70)  + xlab(" ") + ylab(" ") + theme_fivethirtyeight() + ggtitle("Undisclosed Funding") + theme(plot.title = element_text(size = 10, face = "bold"))  +
theme(legend.position="none") + theme(axis.text.x = element_text(size=6),axis.text.y=element_text(size=6))

post_ipo_debt<- vcdata %>% filter(post_ipo_debt>0) %>% ggplot(aes(x=post_ipo_debt,fill=status)) + geom_histogram(aes(fill=status),bins=70) + xlab(" ") + ylab(" ") + theme_fivethirtyeight() + ggtitle("Post IPO Debt") + theme(plot.title = element_text(size = 10, face = "bold")) +
theme(legend.position="none")+ theme(axis.text.x = element_text(size=6),axis.text.y=element_text(size=6)) +  scale_fill_manual(values=c("#f8766d","#639cfb","black"))

product_crowdfunding <- vcdata %>% filter(product_crowdfunding>0) %>% ggplot(aes(x=product_crowdfunding,fill=status)) + geom_histogram(aes(fill=status),bins=70) + xlab(" ") + ylab(" ") + theme_fivethirtyeight() + ggtitle("Product Crowdfunding") +
theme(plot.title = element_text(size = 10, face = "bold")) +
theme(legend.position="none") + scale_fill_manual(values=c("#f8766d","#639cfb","black"))+ theme(axis.text.x = element_text(size=6),axis.text.y=element_text(size=6))

secondary_market <- vcdata %>% filter(secondary_market>0) %>% ggplot(aes(x=secondary_market,fill=status)) + geom_histogram(aes(fill=status),bins=70) + xlab(" ") + ylab(" ") + theme_fivethirtyeight() + ggtitle("Secondary Market") +
theme(plot.title = element_text(size = 10, face = "bold")) +
theme(legend.position="none")+ theme(axis.text.x = element_text(size=6),axis.text.y=element_text(size=6))

grid.arrange(secondary_market,post_ipo_debt,post_ipo_equity,product_crowdfunding,equity_crowdfunding,convertiblenote,grant,undisclosed,private_equity,angel,debtfinancing, seed,ncol=3,nrow=4)
#Undisclosed Funding

vcdata %>% filter(venture>0) %>% ggplot(aes(x=venture,fill=status)) + geom_histogram(aes(fill=status),bins=30) + xlab(" ") + ylab(" ") + theme_fivethirtyeight() + ggtitle("Venture Capital") + theme(plot.title = element_text(size = 10, face = "bold"))
#Funding Patterns
gap <-vcdata %>% filter(as.numeric(funding_days_gap)>0) %>% ggplot(aes(as.numeric(funding_days_gap))) +
  geom_histogram(bins=20)  +
  ggtitle("Gap in Between Last and First Funding Date (Days)") + xlab(" ") + ylab(" ") + theme_fivethirtyeight() + theme(plot.title = element_text(size = 6, face = "bold"))

#Plotting Funding Rounds Distribution
rounds <- vcdata %>% filter(post_ipo_debt>0) %>%ggplot(aes(x=funding_rounds)) + geom_histogram(bins=30) + xlab(" ") + ylab(" ") + theme_fivethirtyeight() + ggtitle("Funding Rounds Obtained") + theme(plot.title = element_text(size = 6, face = "bold"))
grid.arrange(gap,rounds,ncol=2,nrow=1)

##################################
####End of Analysis and Plots#####
##################################

#################################
####Algorithm Implementation#####
#################################

#####PCA
#Removing non.numeric vectors
vcdataforpc <- vcdata[,-c(1,2,3,5,6,7)]
vcdataforpc$funding_days_gap <- as.numeric(vcdataforpc$funding_days_gap)
vcdata.pr <- prcomp(vcdataforpc,center=TRUE,scale=TRUE)
summary(vcdata.pr)
#Cut off point decided for Eigenvalue = 1
screeplot(vcdata.pr, type = "l", npcs = 20, main = "Screeplot of Principal Component Eigenvalues")
abline(h = 1, col="blue", lty=5)
legend("topright", legend=c("Eigenvalue of 1"),
       col=c("blue"), lty=5, cex=1.00)
#Further checking if data is suitable for pca.
cumpro <- cumsum(vcdata.pr$sdev^2 / sum(vcdata.pr$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
#Not suitable for running PCA.

#####randomForest
#Sample Partitioning to Test and Train set
set.seed(1,sample.kind="Rounding")
test_index <- createDataPartition(vcdata$status,times=1,p=0.8,list=FALSE)
train <- vcdata[test_index,]
test <- vcdata[-test_index,]
#Implementation
set.seed(1,sample.kind="Rounding")
#Removing categorical predictors > 53 categories as Rtree cannot handle more than 53 categories.
train_forest <- train[,-c(1,3)]
train_rf <- randomForest(status ~ ., data=train_forest)
train_rf
#Tuning Parameters
res <- tuneRF(x = subset(train_forest, select=-status),
              y = train_forest$status,
              ntreeTry = 500)
#Lowest mtry = 2
#Finding best node size tuning.
nodesize <- seq(1, 10)
oob_err <- c()
for (i in 1:length(nodesize)) {
    model <- randomForest(formula = status ~ .,
                          data = train_forest,
                          nodesize = nodesize[i],mtry=2)
    oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}
which.min(oob_err)

#Final optimal model with mtry=2, nodesize=4
finalrtree <- randomForest(status ~ ., data=train_forest,mtry=2,nodesize=4)

########################################
####End of Algorithm Implementation#####
#######################################

###################
####Evaluation#####
###################

#Random Forest Tree
####Evaluation of Algorithm
test_forest<- test[,-c(1,3)]
#Prediction by assuming that all outcomes are "operating"
guess <- mean(test_forest$status=="operating")
#Accuracy of just predicing that all startups are operating is 86.4%
random <- confusionMatrix(predict(finalrtree, test_forest), test_forest$status)$overall["Accuracy"]
#Rtree's final accuracy is 86.5%.
print(random-guess)
#Minimal Improvement (no difference).

##########################
####End of Evaluation#####
#########################

###########
####END####
###########
