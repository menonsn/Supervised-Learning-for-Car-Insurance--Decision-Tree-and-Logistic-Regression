#Import all the libs
library('PerformanceAnalytics')
library('ggcorrplot')
library('ggplot2')
library('ROCR')
library('tidyverse')
library('car')
library("glmnet")
library("dplyr")
library('rpart')
library('tidyverse')
library('corrgram')
library('glmnet')
library('boot')

# Load the data
bankruptcy.data<-read.csv("IBM_cust.csv")
head(bankruptcy.data)

# Data Exploration
#1 Check the datatype of the columns
sapply(bankruptcy.data, class)
summary(bankruptcy.data)
dim(bankruptcy.data)
str(bankruptcy.data)
glimpse(bankruptcy.data)



# Use sapply() function to count the number of observations with each feature that contains â???~NAâ???T.
sapply(bankruptcy.data, function(x) sum(is.na(x)))



#Similarly, the number of unique observations per column is revealed below.
sapply(bankruptcy.data, function(x) length(unique(x)))



#Using the missmap() function under the Amelia package, the visualization of the amount of missing and observed values per features is observed below.
#Most information in the Cabin and Age features are missing in both datasets.
library(Amelia)
missmap(bankruptcy.data, main = "Missing Values vs. Observed")



#Your data contains 9134 customers with information about their income, education, gender,residence and so on. 
#Each customer owns a car and you as entrepreneur offers 4 different car insurances to them.
#The target of this dataset is the Response. 
#The response can be "Yes" - the customer accept the offer and "No" - the customer didn´t accept the offer.



# Relation between numerical variables
nums <- unlist(lapply(bankruptcy.data, is.numeric)) 
bankruptcy_numeric<-bankruptcy.data[,nums]
corr<-cor(bankruptcy_numeric)



library("PerformanceAnalytics")
#my_data <- bankruptcy_numeric
chart.Correlation(corr, histogram=TRUE, pch=19)



library(ggcorrplot)
ggcorrplot(corr, hc.order = TRUE, type = "lower",lab = TRUE)
# Exploratory Data Analysis
# Relation between categorial variables and response variable



# Gender - > Response
library(ggcorrplot)
tbl_gen <- with(bankruptcy.data, table(Gender, Response))
ggplot(as.data.frame(tbl_gen), aes(factor(Response),Freq, fill=Gender) )+ geom_col(position = 'dodge')



# State - > Response
library(ggcorrplot)
tbl_State <- with(bankruptcy.data, table(State, Response))
ggplot(as.data.frame(tbl_State), aes(factor(State),Freq, fill=Response) )+ geom_col(position = 'dodge')



# Coverage -> Response
library(ggcorrplot)
tbl_Coverage <- with(bankruptcy.data, table(Coverage, Response))
ggplot(as.data.frame(tbl_Coverage), aes(factor(Coverage),Freq, fill=Response) )+ geom_col(position = 'dodge')




# Education -> Response
library(ggcorrplot)
tbl_Education <- with(bankruptcy.data, table(Education, Response))
ggplot(as.data.frame(tbl_Coverage), aes(factor(Coverage),Freq, fill=Response) )+ geom_col(position = 'dodge')




# EmploymentStatus   -> Response
library(ggcorrplot)
tbl_EmploymentStatus <- with(bankruptcy.data, table(EmploymentStatus, Response))
ggplot(as.data.frame(tbl_EmploymentStatus), aes(factor(EmploymentStatus),Freq, fill=Response) )+ geom_col(position = 'dodge')




# Income   -> Response
library(ggcorrplot)
tbl_Income <- with(bankruptcy.data, table(Income, Response))
ggplot(as.data.frame(tbl_Income), aes(factor(Response),Freq, fill=Income) )+ geom_col(position = 'dodge')



#Location Code - > Response
library(ggcorrplot)
tbl_LocationCode <- with(bankruptcy.data, table(Location.Code, Response))
ggplot(as.data.frame(tbl_LocationCode), aes(factor(Location.Code),Freq, fill=Response) )+ geom_col(position = 'dodge')




#Marital.Status -> Response
library(ggcorrplot)
tbl_MaritalStatus <- with(bankruptcy.data, table(Marital.Status, Response))
ggplot(as.data.frame(tbl_MaritalStatus), aes(factor(Marital.Status),Freq, fill=Response) )+ geom_col(position = 'dodge')



#Monthly.Premium.Auto -> Response
library(ggcorrplot)
ggplot(bankruptcy.data, aes(x = Monthly.Premium.Auto,fill=Response)) + geom_histogram(position = 'dodge') 



#Months.Since.Last.Claim  -> Response
library(ggcorrplot)
ggplot(bankruptcy.data, aes(x = Months.Since.Last.Claim ,fill=Response)) + geom_histogram(position = 'dodge') 



#Months.Since.Policy.Inception  -> Response
library(ggcorrplot)
ggplot(bankruptcy.data, aes(x = Months.Since.Policy.Inception ,fill=Response)) + geom_histogram(position = 'dodge') 



#Number.of.Open.Complaints  -> Response
library(ggcorrplot)
ggplot(bankruptcy.data, aes(x = Number.of.Open.Complaints  ,fill=Response)) + geom_histogram(position = 'dodge') 



#Number.of.Policies  -> Response
library(ggcorrplot)
ggplot(bankruptcy.data, aes(x = Number.of.Policies  ,fill=Response)) + geom_histogram(position = 'dodge') 



#Policy.Type -> Response
library(ggcorrplot)
tbl_PolicyType <- with(bankruptcy.data, table(Policy.Type, Response))
ggplot(as.data.frame(tbl_PolicyType), aes(factor(Policy.Type),Freq, fill=Response) )+ geom_col(position = 'dodge')



#Renew.Offer.Type -> Response
library(ggcorrplot)
tbl_RenewOfferType <- with(bankruptcy.data, table(Renew.Offer.Type, Response))
ggplot(as.data.frame(tbl_RenewOfferType), aes(factor(Renew.Offer.Type),Freq, fill=Response) )+ geom_col(position = 'dodge')



#Sales.Channel -> Response
library(ggcorrplot)
tbl_SalesChannel <- with(bankruptcy.data, table(Sales.Channel, Response))
ggplot(as.data.frame(tbl_SalesChannel), aes(factor(Sales.Channel),Freq, fill=Response) )+ geom_col(position = 'dodge')




#Total.Claim.Amount  -> Response
library(ggcorrplot)
ggplot(bankruptcy.data, aes(x = Total.Claim.Amount  ,fill=Response)) + geom_histogram(position = 'dodge') 



#Vehicle.Class -> Response
library(ggcorrplot)
tbl_VehicleClass <- with(bankruptcy.data, table(Vehicle.Class, Response))
ggplot(as.data.frame(tbl_VehicleClass), aes(factor(Vehicle.Class),Freq, fill=Response) )+ geom_col(position = 'dodge')



#Vehicle.Size -> Response
library(ggcorrplot)
tbl_VehicleSize <- with(bankruptcy.data, table(Vehicle.Size, Response))
ggplot(as.data.frame(tbl_VehicleSize), aes(factor(Vehicle.Size),Freq, fill=Response) )+ geom_col(position = 'dodge')




# Data Wrangling - cleaning



#All categorial features are well distributet, so I will keep them and encode them to numerical data.
#Some columns don´t make sense or are not so important, e.g. Customer (because it´s just a unique number), 
#Policy is the same as Policy Type, Effective To Date is also not important, so I will drop them.
#The data is inbalanced regarding the outcome "Response"



bankruptcy.data = subset(bankruptcy.data , select = -c(Customer,Policy,Effective.To.Date) )
str(bankruptcy.data)



#Encode the categorial Data to numerical 



#Step 1
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}



table(bankruptcy.data[["State"]], encode_ordinal(bankruptcy.data[["State"]]), useNA = "ifany")



table(bankruptcy.data[["Response"]], encode_ordinal(bankruptcy.data[["Response"]]), useNA = "ifany")



table(bankruptcy.data[["Coverage"]], encode_ordinal(bankruptcy.data[["Coverage"]]), useNA = "ifany")



table(bankruptcy.data[["Education"]], encode_ordinal(bankruptcy.data[["Education"]]), useNA = "ifany")



table(bankruptcy.data[["EmploymentStatus "]], encode_ordinal(bankruptcy.data[["EmploymentStatus "]]), useNA = "ifany")



table(bankruptcy.data[["Gender"]], encode_ordinal(bankruptcy.data[["Gender"]]), useNA = "ifany")



table(bankruptcy.data[["Location.Code"]], encode_ordinal(bankruptcy.data[["Location.Code"]]), useNA = "ifany")



table(bankruptcy.data[["Marital.Status"]], encode_ordinal(bankruptcy.data[["Marital.Status"]]), useNA = "ifany")



table(bankruptcy.data[["Policy.Type"]], encode_ordinal(bankruptcy.data[["Policy.Type"]]), useNA = "ifany")



table(bankruptcy.data[["Renew.Offer.Type"]], encode_ordinal(bankruptcy.data[["Renew.Offer.Type"]]), useNA = "ifany")



table(bankruptcy.data[["Sales.Channel"]], encode_ordinal(bankruptcy.data[["Sales.Channel"]]), useNA = "ifany")



table(bankruptcy.data[["Vehicle.Class"]], encode_ordinal(bankruptcy.data[["Vehicle.Class"]]), useNA = "ifany")



table(bankruptcy.data[["Vehicle.Size"]], encode_ordinal(bankruptcy.data[["Vehicle.Size"]]), useNA = "ifany")




#Step 2



bankruptcy.data.new <- bankruptcy.data
bankruptcy.data.new[["State"]] <- encode_ordinal(bankruptcy.data[["State"]])
bankruptcy.data.new[["Response"]] <- encode_ordinal(bankruptcy.data[["Response"]])
bankruptcy.data.new[["Coverage"]] <- encode_ordinal(bankruptcy.data[["Coverage"]])
bankruptcy.data.new[["Education"]] <- encode_ordinal(bankruptcy.data[["Education"]])
bankruptcy.data.new[["EmploymentStatus"]] <- encode_ordinal(bankruptcy.data[["EmploymentStatus"]])
bankruptcy.data.new[["Gender"]] <- encode_ordinal(bankruptcy.data[["Gender"]])
bankruptcy.data.new[["Location.Code"]] <- encode_ordinal(bankruptcy.data[["Location.Code"]])
bankruptcy.data.new[["Marital.Status"]] <- encode_ordinal(bankruptcy.data[["Marital.Status"]])
bankruptcy.data.new[["Policy.Type"]] <- encode_ordinal(bankruptcy.data[["Policy.Type"]])
bankruptcy.data.new[["Renew.Offer.Type"]] <- encode_ordinal(bankruptcy.data[["Renew.Offer.Type"]])
bankruptcy.data.new[["Sales.Channel"]] <- encode_ordinal(bankruptcy.data[["Sales.Channel"]])
bankruptcy.data.new[["Vehicle.Class"]] <- encode_ordinal(bankruptcy.data[["Vehicle.Class"]])
bankruptcy.data.new[["Vehicle.Size"]] <- encode_ordinal(bankruptcy.data[["Vehicle.Size"]])
head(bankruptcy.data.new)




str(bankruptcy.data.new)
# Correlation Graph



#Analyzing the relationship between feature variables and the target variable
nums_new <- unlist(lapply(bankruptcy.data.new, is.numeric)) 
bankruptcy_numeric_new<-bankruptcy.data.new[,nums_new]
corrnew<-cor(bankruptcy_numeric_new)



library("PerformanceAnalytics")
#my_data_new <- bankruptcy_numeric_new
chart.Correlation(corrnew, histogram=TRUE, pch=19)



library(ggcorrplot)
ggcorrplot(corrnew, hc.order = TRUE, type = "lower",lab = TRUE)



library("ggplot2")



# Correlation heatmap plot
library(reshape2)
melted_cormat <- melt(corrnew)
head(melted_cormat)



library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+theme(axis.text.x=element_text(angle = 90)) 



head(bankruptcy.data.new)



Y <- bankruptcy.data.new[c(Response)]
X <- bankruptcy.data.new[-c(Response)]


set.seed(13255870)
index <- sample(nrow(bankruptcy.data.new),nrow(bankruptcy.data.new)*0.70)
bankruptcy.train = bankruptcy.data.new[index,]
bankruptcy.test = bankruptcy.data.new[-index,]

names(bankruptcy.train)

bankruptcy.rpart <- rpart(formula = Response ~ ., data = bankruptcy.train)
bankruptcy.rpart

prp(bankruptcy.rpart,digits = 4, extra = 1)

bankruptcy.rpart0 <- rpart(formula = Response ~ ., data = bankruptcy.train, method = "class")
bankruptcy.rpart0
prp(bankruptcy.rpart0, extra = 1)

pred0<- predict(bankruptcy.rpart0, type="class")
table(bankruptcy.train$Response, pred0, dnn = c("True", "Pred"))

credit.train.pred.tree1<- predict(bankruptcy.rpart0, bankruptcy.train, type="class")
table(bankruptcy.rpart0$default, credit.train.pred.tree1, dnn=c("Truth","Predicted"))

str(bankruptcy.train)
