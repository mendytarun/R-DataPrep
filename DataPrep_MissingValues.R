

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





