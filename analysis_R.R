setwd("C:\\Users\\Methma Bandara\\Documents\\MSC\\Analysis")
df=read.csv("final.csv")
df
#some descriptive statistics of dependent variables
#1st dependent variable
summary(df$OverallSatisfaction)

#mode
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate the mode using the user function.
result <- getmode(df$Assesment)
print(result)
result <- getmode(df$ContentWorkload)
print(result)
result <- getmode(df$ContentWorkload)
print(result)
result <- getmode(df$CommunicationFlexibility)
print(result)
result <- getmode(df$EaseOfAccess)
print(result)


#Summary statistics of independent variables
summary(df$EaseOfAccess)
summary(df$CommunicationFlexibility)
summary(df$ContentWorkload)
summary(df$Assesment)




#This is use for convert integer to factors (independent variables)
df$EaseOfAccess=as.factor(df$EaseOfAccess)
df$CommunicationFlexibility=as.factor(df$CommunicationFlexibility)
df$ContentWorkload=as.factor(df$ContentWorkload)
df$Assesment=as.factor(df$Assesment)

#This is use for convert integer to factors (independent variables)
df$OverallSatisfaction=as.factor(df$OverallSatisfaction)


#chi-sq test (independent variables vs dependent 1st variable)
t1=table(df$EaseOfAccess,df$OverallSatisfaction)
t1
chisq.test(t1)

t2=table(df$CommunicationFlexibility,df$OverallSatisfaction)
t2
chisq.test(t2)

t3=table(df$ContentWorkload,df$OverallSatisfaction)
t3
chisq.test(t3)

t4=table(df$Assesment,df$OverallSatisfaction)
t4
chisq.test(t4)

#test for proportion
prop.test(x=165,n=175, p =0.6, alternative = "greater")
res$p.value
prop.test(x=147,n=175, p =0.6, alternative = "greater")
res$p.value
prop.test(x=155,n=175, p =0.6, alternative = "greater")
res$p.value
prop.test(x=128,n=175, p =0.6, alternative = "greater")
res$p.value
prop.test(x=161,n=175, p =0.6, alternative = "greater")
res$p.value






