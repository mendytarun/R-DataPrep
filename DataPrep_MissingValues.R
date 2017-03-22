


# replacing blanks with NA while importing data

fin.df <- read.csv("Future-500.csv",na.strings = "")
fin.df
head(fin.df)
str(fin.df)
summary(fin.df)
tail(fin.df,3)

# factors recap..we got some columns as factor though they 
# should not be and other way around

levels(fin.df$Industry)

# changing from non factor to factor

fin.df$ID <- factor(fin.df$ID)
fin.df$Inception <- factor(fin.df$Inception)
str(fin.df)

# Factor Variable Trap

# if we need to convert a factor in to numeric we first need to convert it to  character 
# other wise we'll se factorize value instead of real value 
# here in b we get 1 2 1 instead of 11 12 11

a <- factor(c("11","12","11")) # factor
b <- as.numeric(a)
b

# right way

c <- as.numeric(as.character(a))
c


head(fin.df)
str(fin.df)

# removing special characters from revenue expenses and growth

fin.df$Growth <- gsub("%","",fin.df$Growth)  # gsub function find and replaces and also converts factor into character if performed on factor
fin.df$Revenue <- gsub("\\$","",fin.df$Revenue)
fin.df$Revenue <- gsub(",","",fin.df$Revenue)
fin.df$Expenses <- gsub(",","",fin.df$Expenses)
fin.df$Expenses <- gsub(" Dollars","",fin.df$Expenses)

str(fin.df)

# converting to numeric

fin.df$Growth <- as.numeric(fin.df$Growth)
fin.df$Revenue <- as.numeric(fin.df$Revenue)
fin.df$Expenses <- as.numeric(fin.df$Expenses)
fin.df$Profit <- as.numeric(fin.df$Profit)
str(fin.df)


# how to locate missing data

head(fin.df,25)

# function complete.case picks the row which don't  have NA in any of the columns and give reult in true or false
# factors will have NA's like <NA> in order to distinguish with orignal data

missing.fin.df <- fin.df[!complete.cases(fin.df),]
missing.fin.df
nrow(missing.fin.df)

# filtering non missing values - effect of NA

fin.df[fin.df$Revenue == 9746272,]

# here we get two NA rows as they have revenue as NA anything compared with NA is neither true nor false


# filtering using which - non miissing value

# which browses vector and picks only true value..ignore NA too

fin.df[which(fin.df$Revenue==9746272),]


# filtering using is.na for missing data..gives rows with NA

fin.df[is.na(fin.df$Expenses),]

fin.df[is.na(fin.df$State),]

# removing records with missing data

nrow(fin.df)

fin.df_backup <- fin.df

nrow(fin.df[!complete.cases(fin.df),]) # getting rows which have atleast ine column as null

fin.df[!is.na(fin.df$Industry),] # df without Industry NA rows

fin.df <- fin.df[!is.na(fin.df$Industry),]

nrow(fin.df) # 2 rowsremived which have industry NA

# resetting the row index in dataframe...hwn you delete rows... row id doesnot reset..they remain same unlike excel

rownames(fin.df) <- 1:nrow(fin.df) 
tail(fin.df)
rownames(fin.df) <- NULL # faster way of resetting the row names

# replacing missing values - Factual Analysis Method

fin.df[!complete.cases(fin.df),]
fin.df[is.na(fin.df$State) & fin.df$City == "New York","State"] <- "NY"




fin.df[!complete.cases(fin.df),]
fin.df[is.na(fin.df$State) & fin.df$City == "San Francisco","State"] <- "CA"  

fin.df[c(11,377),]


# replacing missing value with median..median imputation

fin.df[!complete.cases(fin.df),]
nrow(fin.df[!complete.cases(fin.df),])

v.industry <- (fin.df[is.na(fin.df$Employees),])$Industry  # getting industries which have EMployess as NA
v.industry



mean(fin.df[,"Employees"],na.rm = T)                              # mean without Industry
mean(fin.df[fin.df$Industry==v.industry[1],"Employees"],na.rm=T)  # mean with Industry


median(fin.df[,"Employees"],na.rm = T)                              # median without industry
med.emp.retail <- median(fin.df[fin.df$Industry==v.industry[1],"Employees"],na.rm=T)  # median with Induustry

fin.df[is.na(fin.df$Employees) & fin.df$Industry == v.industry[1],"Employees"] <- med.emp.retail
fin.df[3,]


mean(fin.df[fin.df$Industry==v.industry[2],"Employees"],na.rm=T)  # mean with Industry
med.emp.service <- median(fin.df[fin.df$Industry==v.industry[2],"Employees"],na.rm=T)  # median with Induustry
med.emp.service
fin.df[is.na(fin.df$Employees) & fin.df$Industry == v.industry[2],"Employees"] <- med.emp.service
fin.df[330,]


# median imputation in growth

fin.df[!complete.cases(fin.df),]

growth.industry <- (fin.df[is.na(fin.df$Growth),"Industry"])
growth.industry
med.growth.service <- median(fin.df[fin.df$Industry==growth.industry[1],"Growth"],na.rm=T)

fin.df[is.na(fin.df$Growth) & fin.df$Industry == growth.industry[1],"Growth"] <- med.growth.service

fin.df[!complete.cases(fin.df),]


# median imputation in revenue and expenses

fin.df[is.na(fin.df$Revenue),]

func.median.set <- function(na_column,median_column){
  
            median.out <- median(fin.df[fin.df$Industry == na_column,median_column],na.rm=T)
            return(median.out)         
  }

median.revenue <- func.median.set( "Construction","Revenue")
median.expenses <- func.median.set( "Construction","Expenses")
fin.df[is.na(fin.df$Revenue) & fin.df$Industry == "Construction","Revenue"] <- median.revenue
fin.df[is.na(fin.df$Expenses) & fin.df$Industry == "Construction","Expenses"] <- median.expenses
fin.df[!complete.cases(fin.df),]

# calculating missing data - Revenue - Expenses = Profit or Expenses = Revenue - profit

fin.df[is.na(fin.df$Profit),"Profit"] <- fin.df[is.na(fin.df$Profit),"Revenue"] - fin.df[is.na(fin.df$Profit),"Expenses"]

fin.df[!complete.cases(fin.df),]


str(fin.df)

# scatter plot classified by industry showing revenue profit expenses
library(ggplot2)
p <- ggplot(data=fin.df)
p + geom_point(aes(x=Revenue,y=Expenses, color = Industry,size=Profit))

# scatter plot that includes industry trends for the expenses~revenue
# relationship

d <- ggplot(data=fin.df,aes(x=Revenue,y=Expenses,color=Industry))
  d + geom_point() + geom_smooth(fill=NA,size=1.3)

# box plots showing gwoth by industry
  
e <- ggplot(data=fin.df,aes(x=Industry,y=Growth,color=Industry))  
e + geom_boxplot()  
  
e + geom_jitter()+geom_boxplot(size=1, alpha=0.5,outlier.color = NA)
