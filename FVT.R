getwd()
fin.df <- read.csv("Future-500.csv")
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

