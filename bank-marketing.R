library(readr)
library(mlr)
library(ggplot2)
library(magrittr)
library(cowplot)
library(dplyr)
library(gridExtra)
library(GGally)
library(dlookr)
require(modeest)
library(moments)
library(e1071)
library(qcc)
library(car)
library(gplots)
library(kableExtra)
library(formattable)

# calcolo del coefficiente di variazione
sigma2<-function(x){var(x)*(length(x)-1)/length(x)}
cv<-function(x, na.rm = TRUE) {sqrt(sigma2(x))/abs(mean(x))}

# Statistiche descrittive per variabili quantitative
StatDescQnt<-function(x)
{
  a<-round(mean(x, na.rm=TRUE),2)
  b<-round(sd(x, na.rm=TRUE),2)
  c<-round(median(x, na.rm=TRUE),2)
  d<-quantile(x, probs = 0.25, na.rm=TRUE)
  e<-quantile(x, probs = 0.75, na.rm=TRUE)
  f<-min(x, na.rm=TRUE)
  g<-max(x, na.rm=TRUE)
  h<-range(x, na.rm = TRUE)
  i<-round(var(x, na.rm = TRUE),2)
  l<-round(IQR(x, na.rm = TRUE),2)
  m<-round(mfv(x, na.rm = TRUE),2)
  n<-round(cv(x),2)
  o<-round(skewness(x),2)
  p<-round(kurtosis(x),2)
  out<-c("media"=a,
         "mediana"=c,
         "moda"=m,
         "sd"=b,
         "variance"=i,
         "coeff.var"=n,
         "Q1"=d,
         "Q3"=e,
         "min"=f,
         "max"=g,
         "range"=h,
         "interquartile range"=l,
         "skewness"=o,
         "kurtosis"=p)
  out
}

contingency_table <- function(x, y){
  cont_table <- table(x, y)
  balloonplot(t(cont_table), main ="", xlab ="", ylab="", label = TRUE, show.margins = TRUE)
  print(chisq.test(cont_table))
}

bank <- read.csv("bank.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)

#Struttura del dataset
str(bank)
#Conteggio degli NAs per ciascun attributo
sapply(bank, function(x) sum(is.na(x)))

bank$default <-ifelse(bank$default =="yes", "defaulter","no defaulter")
bank$housing <-ifelse(bank$housing =="yes", "housing loan","no housing loan")
bank$loan <-ifelse(bank$loan =="yes", "personal loan","no personal loan")

bank$age<-as.numeric(bank$age)
bank$balance<-as.numeric(bank$balance)
bank$day<-as.numeric(bank$day)
bank$duration<-as.numeric(bank$duration)
bank$campaign<-as.numeric(bank$campaign)
bank$pdays<-as.numeric(bank$pdays)
bank$previous<-as.numeric(bank$previous)

bank$job<-as.factor(bank$job)
bank$marital<-as.factor(bank$marital)
bank$education<-as.factor(bank$education)
bank$default<-as.factor(bank$default)
bank$housing<-as.factor(bank$housing)
bank$loan<-as.factor(bank$loan)
bank$contact<-as.factor(bank$contact)
bank$month<-as.factor(bank$month)
bank$poutcome<-as.factor(bank$poutcome)
bank$y<-as.factor(bank$y)

#Separo variabili quantitative e qualitative
qnt_var = bank[,c('age','balance','campaign','day','duration','pdays','previous')]
qlt_var = bank[,c('job','marital','education','default','housing','loan','contact','month','poutcome','y')]


table(bank$job)
round((prop.table(table(bank$job))*100),2)
job_sum <- bank%>% group_by(job) %>% summarise(count = n()) %>% mutate(freq = count/sum(count))
job_sum$job <- job_sum$job %>% factor(levels = job_sum$job[order(-job_sum$count)]) 
ggplot(job_sum,aes(x = job, y = count)) + geom_bar(stat="identity") +theme(axis.text.x=element_text(angle=45,hjust=1)) + labs(title = "Figura 1")

table(bank$marital)
round((prop.table(table(bank$marital))*100),2)
marital_sum <- bank%>% group_by(marital) %>% summarise(count = n()) %>% mutate(freq = count/sum(count)) 
marital_sum$marital <- marital_sum$marital %>% factor(levels = marital_sum$marital[order(-marital_sum$count)]) 
ggplot(marital_sum,aes(x = marital, y = count)) + geom_bar(stat="identity") + labs(title = "Figura 2")

table(bank$education)
round((prop.table(table(bank$education))*100),2)
education_sum <- bank%>% group_by(education) %>% summarise(count = n()) %>% mutate(freq = count/sum(count)) 
education_sum$education <- education_sum$education %>% factor(levels = education_sum$education[order(-education_sum$count)]) 
ggplot(education_sum,aes(x = education, y = count)) + geom_bar(stat="identity") + labs(title = "Figura 3")

table(bank$default)
round((prop.table(table(bank$default))*100),2)
default_sum <- bank%>% group_by(default) %>% summarise(count = n()) %>% mutate(freq = count/sum(count)) 
default_sum$default <- default_sum$default %>% factor(levels = default_sum$default[order(-default_sum$count)]) 
ggplot(default_sum,aes(x = default, y = count)) + geom_bar(stat="identity") + labs(title = "Figura 4")

table(bank$housing)
round((prop.table(table(bank$housing))*100),2)
housing_sum <- bank%>% group_by(housing) %>% summarise(count = n()) %>% mutate(freq = count/sum(count)) 
housing_sum$housing <- housing_sum$housing %>% factor(levels = housing_sum$housing[order(-housing_sum$count)]) 
ggplot(housing_sum,aes(x = housing, y = count)) + geom_bar(stat="identity") + labs(title = "Figura 5")

table(bank$loan)
round((prop.table(table(bank$loan))*100),2)
loan_sum <- bank%>% group_by(loan) %>% summarise(count = n()) %>% mutate(freq = count/sum(count))
loan_sum$loan <- loan_sum$loan %>% factor(levels = loan_sum$loan[order(-loan_sum$count)])
ggplot(loan_sum,aes(x = loan, y = count)) + geom_bar(stat="identity") + labs(title = "Figura 6")

table(bank$contact)
round((prop.table(table(bank$contact))*100),2)
contact_sum <- bank%>% group_by(contact) %>% summarise(count = n()) %>% mutate(freq = count/sum(count)) 
contact_sum$contact <- contact_sum$contact %>% factor(levels = contact_sum$contact[order(-contact_sum$count)])
ggplot(contact_sum,aes(x = contact, y = count)) + geom_bar(stat="identity") + labs(title = "Figura 7")

table(bank$month)
round((prop.table(table(bank$month))*100),2)
month_sum <- bank%>% group_by(month) %>% summarise(count = n()) %>% mutate(freq = count/sum(count))
month_sum$month <- month_sum$month %>% factor(levels = month_sum$month[order(-month_sum$count)])
ggplot(month_sum,aes(x = month, y = count)) + geom_bar(stat="identity") + labs(title = "Figura 8")

table(bank$poutcome)
round((prop.table(table(bank$poutcome))*100),2)
poutcome_sum <- bank%>% group_by(poutcome) %>% summarise(count = n()) %>% mutate(freq = count/sum(count)) 
poutcome_sum$poutcome <- poutcome_sum$poutcome %>% factor(levels = poutcome_sum$poutcome[order(-poutcome_sum$count)]) 
ggplot(poutcome_sum,aes(x = poutcome, y = count)) + geom_bar(stat="identity") + labs(title = "Figura 9")

table(bank$y)
round((prop.table(table(bank$y))*100),2)
y_sum <- bank%>% group_by(y) %>% summarise(count = n()) %>% mutate(freq = count/sum(count))
y_sum$y <- y_sum$y %>% factor(levels = y_sum$y[order(-y_sum$count)]) 
ggplot(y_sum,aes(x = y, y = count)) + geom_bar(stat="identity") + labs(title = "Figura 10")

StatDesc<-sapply(qnt_var,StatDescQnt)
grid.table(StatDesc)

p <- ggplot(bank, aes(x = factor(1), y = age)) +   geom_boxplot(width = .50)
p1 <- ggplot(bank, aes(x = age)) +
  geom_density(fill = "dodgerblue", alpha = .2) +
  geom_histogram(colour="white",aes(y=..density..),alpha = 1/2) +
  geom_vline(xintercept= median(bank$age)) +
  geom_vline(xintercept= mean(bank$age),linetype=2) +
  labs(title = "Figura 11")
plot_grid(p1, p + coord_flip() + theme(axis.title.y=element_blank(), 
                                       axis.text.y=element_blank(),
                                       axis.ticks.y = element_blank()), ncol=1, align="v", 
          rel_heights = c(2,1))


p_balance <- ggplot(bank, aes(x = factor(1), y = balance)) +geom_boxplot(width = .50)

p1_balance <- ggplot(bank, aes(x = balance)) + labs(title = "Figura 12") + geom_histogram(colour="white",aes(y=..count..), alpha=1/2) +
  geom_density(fill = "dodgerblue", alpha = .2) +
  geom_vline(xintercept= median(bank$balance)) +
  geom_vline(xintercept= mean(bank$balance),linetype=2)

plot_grid(p1_balance, p_balance + coord_flip() + theme(axis.title.y=element_blank(), 
                                                       axis.text.y=element_blank(),
                                                       axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1))


p_campaign <- ggplot(bank, aes(x = factor(1), y = campaign )) + geom_boxplot(width = .50)
p1_campaign <-  ggplot(bank, aes(x = campaign)) +
  labs(title = "Figura 13") + geom_histogram(colour ="white", aes(y=..count..),alpha = 1/2) +
  geom_density(fill = "dodgerblue", alpha = .2) +
  geom_vline(xintercept= median(bank$campaign)) +
  geom_vline(xintercept= mean(bank$campaign),linetype=2)
plot_grid(p1_campaign, p_campaign + coord_flip() + theme(axis.title.y=element_blank(),
														 axis.text.y=element_blank(),
                                                         axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1)) 


p_day <- ggplot(bank, aes(x = factor(1), y = day )) + geom_boxplot(width = .50)
p1_day <-  ggplot(bank, aes(x = day)) +
  labs(title = "Figura 14") + geom_histogram(colour ="white", aes(y=..count..),alpha = 1/2) +
  geom_density(fill = "dodgerblue", alpha = .2) +
  geom_vline(xintercept= median(bank$day)) +
  geom_vline(xintercept= mean(bank$day),linetype=2)
plot_grid(p1_day, p_day + coord_flip() + theme(axis.title.y=element_blank(),
											   axis.text.y=element_blank(),
                                               axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1))


p_duration <- ggplot(bank, aes(x = factor(1), y = duration)) +   geom_boxplot(width = .50)
p1_duration <- ggplot(bank, aes(x = duration)) +
  labs(title = "Figura 15") + geom_histogram(colour ="white", aes(y=..count..),alpha = 1/2) +
  geom_density(fill = "dodgerblue", alpha = .2) +
  geom_vline(xintercept= median(bank$duration)) +
  geom_vline(xintercept= mean(bank$duration),linetype=2)
plot_grid(p1_duration, p_duration + coord_flip() + theme(axis.title.y=element_blank(),
														 axis.text.y=element_blank(),
                                                         axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1))


p_pdays <- ggplot(bank, aes(x = factor(1), y = pdays)) + geom_boxplot(width = .50)
p1_pdays <- ggplot(bank, aes(x = pdays)) +
  labs(title = "Figura 16") + geom_histogram(colour ="white", aes(y=..count..),alpha = 1/2) +
  geom_density(fill = "dodgerblue", alpha = .2) +
  geom_vline(xintercept= median(bank$pdays)) +
  geom_vline(xintercept= mean(bank$pdays),linetype=2)
plot_grid(p1_pdays, p_pdays + coord_flip() + theme(axis.title.y=element_blank(),
												   axis.text.y=element_blank(),
                                                   axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1))


p_previous <- ggplot(bank, aes(x = factor(1), y = previous)) + geom_boxplot(width = .50)
p1_previous <- ggplot(bank, aes(x = previous)) +
  labs(title = "Figura 17") + geom_histogram(colour ="white", aes(y=..count..),alpha = 1/2) +
  geom_density(fill = "dodgerblue", alpha = .2) +
  geom_vline(xintercept= median(bank$previous)) +
  geom_vline(xintercept= mean(bank$previous),linetype=2)
plot_grid(p1_previous, p_previous + coord_flip() + theme(axis.title.y=element_blank(), 
                                                         axis.text.y=element_blank(),
                                                         axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1))


contingency_table(bank$job, bank$education)

contingency_table(bank$job, bank$marital)

contingency_table(bank$job, bank$housing)

contingency_table(bank$job, bank$loan)

contingency_table(bank$job, bank$contact)

contingency_table(bank$marital, bank$education)

contingency_table(bank$marital, bank$housing)

contingency_table(bank$marital, bank$loan)

contingency_table(bank$marital, bank$contact)

contingency_table(bank$marital, bank$contact)

contingency_table(bank$education, bank$loan)

contingency_table(bank$education, bank$contact)

contingency_table(bank$housing, bank$loan)

contingency_table(bank$housing, bank$contact)

contingency_table(bank$loan, bank$contact)

ggpairs(qnt_var, title="correlogram with ggpairs()") 

ggcorr(qnt_var, method = c("everything", "pearson")) 

#Divido la variabile target in due sottoinsiemi separati con Yes e No
bank_yes <- bank %>% filter(bank$y =="yes")
bank_no <- bank %>% filter(bank$y =="no")

bank_yes_job <-bank_yes%>% group_by(job) %>% summarise(count = n())
bank_yes_job$y <- c(strrep("yes",1))
bank_no_job <-bank_no%>% group_by(job) %>% summarise(count = n())
bank_no_job$y <- c(strrep("no",1))
bank_job <- rbind(bank_yes_job, bank_no_job)
bank_job$y <- factor(c(bank_job$y), levels = c("yes","no"), ordered = TRUE)
ggplot(bank_job,aes(x = job, y = count, fill = y)) + geom_bar(stat="identity", position ="dodge") + theme(axis.text.x=element_text(angle=45,hjust=1))
contingency_table(bank$job, bank$y)

bank_yes_marital <- bank_yes%>% group_by(marital) %>% summarise(count = n())
bank_no_marital <- bank_no  %>% group_by(marital) %>% summarise(count = n())
bank_yes_marital$y <-c(strrep("yes",1))
bank_no_marital$y <-c(strrep("no",1))
bank_marital <- rbind(bank_yes_marital,bank_no_marital)
bank_marital$y <- factor(c(bank_marital$y), levels = c("yes","no"), ordered = TRUE)
ggplot(bank_marital,aes(x = marital, y = count, fill = y)) + geom_bar(stat="identity", position ="dodge") + facet_grid(~y) + theme(axis.text.x=element_text(angle=45,hjust=1))
contingency_table(bank$marital, bank$y)

bank_yes_education <-bank_yes%>% group_by(education) %>% summarise(count = n())
bank_no_education <-bank_no%>% group_by(education) %>% summarise(count = n())
bank_yes_education$y <- c(strrep("yes",1))
bank_no_education$y <- c(strrep("no",1))
bank_education <- rbind(bank_yes_education, bank_no_education)
bank_education$y <- factor(c(bank_education$y), levels = c("yes","no"), ordered = TRUE)
ggplot(bank_education,aes(x = education, y = count, fill = y)) + geom_bar(stat="identity", position ="dodge") + facet_grid(~y) + theme(axis.text.x=element_text(angle=45,hjust=1))
contingency_table(bank$education, bank$y)

bank_yes_default <-bank_yes%>% group_by(default) %>% summarise(count = n())
bank_no_default <-bank_no%>% group_by(default) %>% summarise(count = n())
bank_yes_default$y <- c(strrep("yes",1))
bank_no_default$y <- c(strrep("no",1))
bank_default <- rbind(bank_yes_default, bank_no_default)
bank_default$y <- factor(c(bank_default$y), levels = c("yes","no"), ordered = TRUE)
ggplot(bank_default,aes(x = default, y = count, fill = y)) + geom_bar(stat="identity", position ="dodge") + facet_grid(~y) + theme(axis.text.x=element_text(angle=45,hjust=1))
contingency_table(bank$default, bank$y)

bank_yes_housing <-bank_yes%>% group_by(housing) %>% summarise(count = n())
bank_no_housing <-bank_no%>% group_by(housing) %>% summarise(count = n())
bank_yes_housing$y <- c(strrep("yes",1))
bank_no_housing$y <- c(strrep("no",1))
bank_housing <- rbind(bank_yes_housing, bank_no_housing)
bank_housing$y <- factor(c(bank_housing$y), levels = c("yes","no"), ordered = TRUE)
ggplot(bank_housing,aes(x = housing, y = count, fill = y)) + geom_bar(stat="identity", position ="dodge") + facet_grid(~y) + theme(axis.text.x=element_text(angle=45,hjust=1))
contingency_table(bank$housing, bank$y)

bank_yes_loan <-bank_yes%>% group_by(loan) %>% summarise(count = n())
bank_no_loan <-bank_no%>% group_by(loan) %>% summarise(count = n())
bank_yes_loan$y <- c(strrep("yes",1))
bank_no_loan$y <- c(strrep("no",1))
bank_loan <- rbind(bank_yes_loan, bank_no_loan)
bank_loan$y <- factor(c(bank_loan$y), levels = c("yes","no"), ordered = TRUE)
ggplot(bank_loan,aes(x = loan, y = count, fill = y)) + geom_bar(stat="identity", position ="dodge") + facet_grid(~y) + theme(axis.text.x=element_text(angle=45,hjust=1))
contingency_table(bank$loan, bank$y)

bank_yes_contact <-bank_yes%>% group_by(contact) %>% summarise(count = n())
bank_no_contact <-bank_no%>% group_by(contact) %>% summarise(count = n())
bank_yes_contact$y <- c(strrep("yes",1))
bank_no_contact$y <- c(strrep("no",1))
bank_contact <- rbind(bank_yes_contact, bank_no_contact)
bank_contact$y <- factor(c(bank_contact$y), levels = c("yes","no"), ordered = TRUE)
ggplot(bank_contact,aes(x = contact, y = count, fill = y)) + geom_bar(stat="identity", position ="dodge") + facet_grid(~y) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
contingency_table(bank$contact, bank$y)

bank_yes_month <-bank_yes%>% group_by(month) %>% summarise(count = n())
bank_no_month <-bank_no%>% group_by(month) %>% summarise(count = n())
bank_yes_month$y <- c(strrep("yes",1))
bank_no_month$y <- c(strrep("no",1))
bank_month <- rbind(bank_yes_month, bank_no_month)
bank_month$y <- factor(c(bank_month$y), levels = c("yes","no"), ordered = TRUE)
ggplot(bank_month,aes(x = month, y = count, fill = y)) + geom_bar(stat="identity", position ="dodge") + facet_grid(~y) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
contingency_table(bank$month, bank$y)

bank_yes_poutcome <-bank_yes%>% group_by(poutcome) %>% summarise(count = n())
bank_no_poutcome <-bank_no%>% group_by(poutcome) %>% summarise(count = n())
bank_yes_poutcome$y <- c(strrep("yes",1))
bank_no_poutcome$y <- c(strrep("no",1))
bank_poutcome <- rbind(bank_yes_poutcome, bank_no_poutcome)
bank_poutcome$y <- factor(c(bank_poutcome$y), levels = c("yes","no"), ordered = TRUE)
ggplot(bank_poutcome,aes(x = poutcome, y = count, fill = y)) + geom_bar(stat="identity", position ="dodge") + facet_grid(~y)
contingency_table(bank$poutcome, bank$y)

ggplot(data=bank, aes(age, fill =y)) + geom_histogram(aes(y=..count..)) + facet_grid(~y)

balance_yes <- ggplot(bank_yes,aes(balance)) + geom_histogram(binwidth = 10) + labs(title = "Term Deposits Yes by Balance", x="Balance", y="Count") + xlim(c(0,2800)) + ylim(c(0,1000))
balance_no <- ggplot(bank_no,aes(balance)) + geom_histogram(binwidth = 10) + labs(title = "Term Deposits No by Balance", x="Balance", y="Count") + xlim(c(0,2800)) +ylim(c(0,1000))
grid.arrange(balance_yes,balance_no)

duration_yes <- ggplot(bank_yes,aes(duration)) + geom_histogram(binwidth = 10) + labs(title = "Term Deposits Yes by Duration", x="Duration", y="Count") + xlim(c(0,2800))
duration_no <- ggplot(bank_no,aes(duration)) + geom_histogram(binwidth = 10) + labs(title = "Term Deposits No by Duration", x="Duration", y="Count") + xlim(c(0,2800))
grid.arrange(duration_yes,duration_no)

ggplot(data=bank, aes(campaign, fill =y)) + geom_histogram(aes(y=..count..)) + facet_grid(~y)

ggplot(data=bank, aes(day, fill =y)) + geom_histogram(aes(y=..count..)) + facet_grid(~y)

ggplot(data=bank, aes(pdays, fill =y)) + geom_histogram(aes(y=..count..)) + facet_grid(~y)

ggplot(data=bank, aes(previous, fill =y)) + geom_histogram(aes(y=..count..)) + facet_grid(~y)

ggplot(data = bank, aes(x=default, y = balance, fill = y)) + geom_boxplot(width = .50) +scale_fill_manual(values = c("red","blue"))

ggplot(data = bank, aes(x=housing, y = duration, fill = y)) + geom_boxplot(width= .50) +scale_fill_manual(values = c("red","blue"))

ggplot(data = bank, aes(x=loan, y = duration, fill = y)) + geom_boxplot(width= .50) +scale_fill_manual(values = c("red","blue"))

#Analizzo gli outliers per le variabili numeriche
bank %>% plot_outlier(age)
bank %>% plot_outlier(balance)
bank %>% plot_outlier(duration)
bank %>% plot_outlier(campaign)
bank %>% plot_outlier(pdays)
bank %>% plot_outlier(previous)

bank$age_imp <- imputate_outlier(bank, age, method = "capping")
bank$balance_imp <- imputate_outlier(bank, balance, method = "capping")
bank$duration_imp <- imputate_outlier(bank, duration, method = "capping")
bank$campaign_imp <-imputate_outlier(bank, campaign, method = "capping")
bank$pdays_imp <-imputate_outlier(bank, pdays, method = "capping")
bank$previous_imp <-imputate_outlier(bank, previous, method = "capping")

plot(bank$age_imp)
plot(bank$balance_imp)
plot(bank$duration_imp)
plot(bank$campaign_imp)
plot(bank$pdays_imp)
plot(bank$previous_imp)

#Age dopo la rimozione degli outliers
p_age_imp <- ggplot(bank, aes(x = factor(1), y = age_imp)) +geom_boxplot(width = .50)

p1_age_imp <- ggplot(bank, aes(x = age_imp)) + labs(title = "Age without outliers") + geom_histogram(colour="white",aes(y=..count..), alpha=1/2) +
  geom_density(fill = "dodgerblue", alpha = .2) +
  geom_vline(xintercept= median(bank$age_imp)) +
  geom_vline(xintercept= mean(bank$age_imp),linetype=2)

plot_grid(p1_age_imp, p_age_imp + coord_flip() + theme(axis.title.y=element_blank(), 
                                                               axis.text.y=element_blank(),
                                                               axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1))

#Balance dopo la rimozione degli outliers
p_balance_imp <- ggplot(bank, aes(x = factor(1), y = balance_imp)) +geom_boxplot(width = .50)

p1_balance_imp <- ggplot(bank, aes(x = balance_imp)) + labs(title = "Balance without outliers") + geom_histogram(colour="white",aes(y=..count..), alpha=1/2) +
  geom_density(fill = "dodgerblue", alpha = .2) +
  geom_vline(xintercept= median(bank$balance_imp)) +
  geom_vline(xintercept= mean(bank$balance_imp),linetype=2)

plot_grid(p1_balance_imp, p_balance_imp + coord_flip() + theme(axis.title.y=element_blank(), 
                                                               axis.text.y=element_blank(),
                                                               axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1))

#Duration dopo la rimozione degli outliers
p_duration_imp <- ggplot(bank, aes(x = factor(1), y = duration_imp)) +geom_boxplot(width = .50)

p1_duration_imp <- ggplot(bank, aes(x = duration_imp)) + labs(title = "Duration without outliers") + geom_histogram(colour="white",aes(y=..count..), alpha=1/2) +
  geom_density(fill = "dodgerblue", alpha = .2) +
  geom_vline(xintercept= median(bank$duration_imp)) +
  geom_vline(xintercept= mean(bank$duration_imp),linetype=2)

plot_grid(p1_duration_imp, p_duration_imp + coord_flip() + theme(axis.title.y=element_blank(), 
                                                                 axis.text.y=element_blank(),
                                                                 axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1))

#Campaign dopo la rimozione degli outliers
p_campaign_imp <- ggplot(bank, aes(x = factor(1), y = campaign_imp)) +geom_boxplot(width = .50)

p1_campaign_imp <- ggplot(bank, aes(x = campaign_imp)) + labs(title = "Campaign without outliers") + geom_histogram(colour="white",aes(y=..count..), alpha=1/2) +
geom_density(fill = "dodgerblue", alpha = .2) +
geom_vline(xintercept= median(bank$campaign_imp)) +
geom_vline(xintercept= mean(bank$campaign_imp),linetype=2)

plot_grid(p1_campaign_imp, p_campaign_imp + coord_flip() + theme(axis.title.y=element_blank(), 
                                                                 axis.text.y=element_blank(),
                                                                 axis.ticks.y = element_blank()), ncol=1, align="v",
                                                                 rel_heights = c(2,1))

#Previous dopo la rimozione degli outliers
p_previous_imp <- ggplot(bank, aes(x = factor(1), y = previous_imp)) +geom_boxplot(width = .50)

p1_previous_imp <- ggplot(bank, aes(x = previous_imp)) + labs(title = "Previous without outliers") + geom_histogram(colour="white",aes(y=..count..), alpha=1/2) +
  geom_density(fill = "dodgerblue", alpha = .2) +
  geom_vline(xintercept= median(bank$previous_imp)) +
  geom_vline(xintercept= mean(bank$previous_imp),linetype=2)

plot_grid(p1_previous_imp, p_previous_imp + coord_flip() + theme(axis.title.y=element_blank(), 
                                                                 axis.text.y=element_blank(),
                                                                 axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1))

#Pdays dopo la rimozione degli outliers
p_pdays_imp <- ggplot(bank, aes(x = factor(1), y = pdays_imp)) +geom_boxplot(width = .50)

p1_pdays_imp <- ggplot(bank, aes(x = pdays_imp)) + labs(title = "Pdays without outliers") + geom_histogram(colour="white",aes(y=..count..), alpha=1/2) +
  geom_density(fill = "dodgerblue", alpha = .2) +
  geom_vline(xintercept= median(bank$pdays_imp)) +
  geom_vline(xintercept= mean(bank$pdays_imp),linetype=2)

plot_grid(p1_pdays_imp, p_pdays_imp + coord_flip() + theme(axis.title.y=element_blank(), 
                                                                 axis.text.y=element_blank(),
                                                                 axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1))


#Correlazione tra le variabili numeriche dopo la rimozione degli outliers
ggcorr(bank[,c(18:23)], method = c("everything", "pearson"))

library(rockchalk)
bank$job_level <- combineLevels(bank$job, levs = c("unemployed", "student", "retired"), newLabel = "no_active_income")
bank$job_level <- combineLevels(bank$job_level, levs = c("admin.", "management"), newLabel = "administration_management")
bank$job_level <- combineLevels(bank$job_level, levs = c("blue-collar", "technician"), newLabel = "blue-collar")
bank$job_level <- combineLevels(bank$job_level, levs = c("services", "housemaid"), newLabel = "services")
bank$job_level <- combineLevels(bank$job_level, levs = c("self-employed", "entrepreneur"), newLabel = "self-employed")

bank$marital_level <- combineLevels(bank$marital, levs = c("single", "divorced"), newLabel = "no_married")

bank$month_level <- combineLevels(bank$month, levs = c("sep","oct","nov","dec","jan","feb"), newLabel = "autumn_winter")
bank$month_level <- combineLevels(bank$month_level, levs = c("mar","apr","may","jun","jul","aug"), newLabel = "spring_summer")

bank$poutcome_level <- combineLevels(bank$poutcome, levs = c("unknown", "other","failure"), newLabel = "rest")


library(caret)
set.seed(1234)
# Creazione dei dataset di train e test
train <- createDataPartition(bank$y, p = 0.6, list=FALSE)

bank_train <- bank[train, ]
bank_test <- bank[-train, ] 
#Conversione di Yes No in 1 e 0 respettivamente
bank_train$ObsY <- ifelse (bank_train$y == "yes", 1,0)
bank_test$ObsY <- ifelse (bank_test$y == "yes", 1,0)
bank_test$ObsY <- as.integer(bank_test$ObsY)
bank_train$ObsY <- as.integer(bank_train$ObsY)


#Regressione logistica sui dati originali
bank_glm <- glm(y ~ age + job + marital + education + default
                + balance + housing + loan + contact + day
                + month + duration + campaign + pdays + previous
                + poutcome, data = bank_train, family = "binomial")
summary(bank_glm)
bank_train$ObsY <- as.integer(bank_train$ObsY)


#Regressione logistica sui dati standardizzati
bank_glm1 <- glm(y ~ age_imp + job_level + marital_level + education + default
                + balance_imp + housing + loan + contact + day
                + month_level + duration_imp + campaign_imp + previous_imp
                + poutcome_level, data = bank_train, family = "binomial")
summary(bank_glm1)


#Predizione sui dati originali
bank_test$predSub <- predict.glm(bank_glm, newdata = bank_test, type = "response")
library(SDMTools)
#Confusion Matrix - dati originali e soglia a 0,5
confusion.matrix(bank_test$ObsY, bank_test$predSub, threshold = 0.5)
accuracy(bank_test$ObsY, bank_test$predSub, threshold = 0.5)


#Predizione sui dati standardizzati
bank_test$predSub1 <- predict.glm(bank_glm1, newdata = bank_test, type = "response")
#Confusion Matrix - dati standardizzati e soglia a 0,5
confusion.matrix(bank_test$ObsY, bank_test$predSub1, threshold = 0.5)
accuracy(bank_test$ObsY, bank_test$predSub1, threshold = 0.5)


#Curva di ROC - Dati originali
library(pROC)
myROC <- roc(bank_test$y, bank_test$predSub)
myROC
plot(myROC)


myROC1 <- roc(bank_test$y, bank_test$predSub1)
myROC1
plot(myROC1)


# Distribuzione di TPR (Sensitivity) and TNR (Specificity) sul valore della soglia per i dati originali
matplot(data.frame(myROC$sensitivities, myROC$specificities), x = myROC$thresholds, type='l', xlab = 'threshold', ylab='TPR, TNR')
legend('bottomright', legend=c('TPR', 'TNR'), lty=1:2, col=1:2)

#Confusion Matrix - dati originali e soglia ottima a 0,1
cm <- confusion.matrix(bank_test$ObsY, bank_test$predSub, threshold = 0.1)
accuracy(bank_test$ObsY, bank_test$predSub, threshold = 0.1)
fourfoldplot(cm, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")


# Distribuzione di TPR (Sensitivity) and TNR (Specificity) sul valore della soglia per i dati standardizzati
matplot(data.frame(myROC1$sensitivities, myROC1$specificities), x = myROC1$thresholds, type='l', xlab = 'threshold', ylab='TPR, TNR')
legend('bottomright', legend=c('TPR', 'TNR'), lty=1:2, col=1:2)

#Confusion Matrix - dati standardizzati e soglia ottima a 0,1
cm1 <- confusion.matrix(bank_test$ObsY, bank_test$predSub1, threshold = 0.1)
accuracy(bank_test$ObsY, bank_test$predSub1, threshold = 0.1)
fourfoldplot(cm1, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")


library(ROSE)
#ROSE
data_rose <- ROSE(y ~ age_imp + job_level + marital_level + education + default
                + balance_imp + housing + loan + contact + day
                + month_level + duration_imp + campaign_imp + pdays_imp + previous_imp
                + poutcome_level, data = bank_train, seed = 1)$data
table(data_rose$y)


glm_rose <- glm(y ~ marital_level + education + default
                + balance_imp + housing + loan + contact
                + month_level + duration_imp + campaign_imp + previous_imp 
                + poutcome_level,data = data_rose, family = "binomial")
summary(glm_rose)


#Predizione con dati bilanciati
pred_glm_rose <- predict.glm(glm_rose, newdata = bank_test, type = "response")
# Confusion Matrix
cm_rose <- confusion.matrix(bank_test$ObsY, pred_glm_rose)
accuracy(bank_test$ObsY, pred_glm_rose)
fourfoldplot(cm_rose, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")


#AUC ROSE
myROCS <-roc(bank_test$ObsY, pred_glm_rose)
myROCS
plot(myROCS)

# Distribuzione di TPR (Sensitivity) and TNR (Specificity) sul valore della soglia per i dati bilanciati
matplot(data.frame(myROCS$sensitivities, myROCS$specificities), x = myROCS$thresholds, type='l', xlab = 'threshold', ylab='TPR, TNR')
legend('bottomright', legend=c('TPR', 'TNR'), lty=1:2, col=1:2)

