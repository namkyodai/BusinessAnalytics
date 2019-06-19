df <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

str(df)

sum(is.na(df))
summary(df)
xtabs(~ admit +rank ,data=df)
df$rank <- as.factor(df$rank)
logit <- glm(admit ~ gre+gpa+rank,data=df,family="binomial")
summary(logit)
