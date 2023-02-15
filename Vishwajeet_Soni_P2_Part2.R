
####### ----------Project2--------------------

# required libraries

library(tidyverse)
library(tidymodels)
library(car)
library(parsnip)
library(dplyr)
library(pROC)
library(ROCit)

options(scipen=999)

# reading the datas

store_train<- read.csv(r"{D:\Data\Edvancer\Data_Project2\store_train.csv}",sep = ",")

head(store_train)
glimpse(store_train)

store_test<- read.csv(r"{D:\Data\Edvancer\Data_Project2\store_test.csv}",sep = ",")
dim(store_test)

# checking the target bw train and test

setdiff(names(store_train),names(store_test))

## Event Rate

prop.table(table(store_train$store))

table(store_train$store_Type)
# Part 1__________

# que1
store_train %>% 
#   # rowwise() %>% 
#   group_by(store_Type,Areaname) %>% 
#   summarise(sum(sales0,sales1,sales2,sales3,sales4)) %>% 
#   view()
# # 
# ans:- 38680

# q2
glimpse(store_train)
table(store_train$storecode)
# maybe no
#3
table(store_train$country)
# 4
# stringr::str_count(unique(store_train$Areaname))
nrow(data.frame(unique(store_train$Areaname)))
# 5
# prop.table(table(store_train$store_Type))

store_train %>% 
  filter(store_Type=="Grocery Store") %>% 
  summarise("totalnum"=n(),"input"=sum(store))
table(store_train$store_Type)
182/432
182+250
# 6
library(nortest)
ad.test(store_train$sales0)
ad.test(store_train$sales1)
ad.test(store_train$sales2)
ad.test(store_train$sales3)

hist((store_train$sales2))

# 7) Number of outliers for total sales based on following limits (q1-1.5*IQR, q3+1.5*IQR)?

Tsales<-store_train %>% 
  rowwise() %>% 
  select(sales0,sales1,sales2,sales3,sales4) %>% 
  mutate(totalsales=sum(c(sales0,sales1,sales2,sales3,sales4)))
head(Tsales)

IQR(Tsales$totalsales) ->iqr

quantile(Tsales$totalsales)

# 0%      25%      50%      75%     100% 
# 2173.00  3422.00  3888.00  4968.75 11140.00


summary(Tsales$totalsales)
IQR(Tsales$totalsales)
3422-(1546.75*1.5)->Tmin
4969+(1.5*1546.75)->Tmax

sum(Tsales$totalsales>Tmax)
sum(Tsales$totalsales<Tmin)

# other ways
# mean(Tsales$totalsales)-> mean
# sd(Tsales$totalsales)-> std
# get threshold values for outliers
# Tmin = mean-(3*std)
# Tmax = mean+(3*std)
Tsales$totalsales->x
# find outlier
x[which(x < Tmin | x > Tmax)]
x[which(x > Tmin & x < Tmax)]
sum(x)
hist(x)
boxplot(x)

# 8
# which store type has maximum variance in total sales?
glimpse(store_train)
table(store_train$store_Type)
var(store_train$store_Type)

store_train %>%
  rowwise() %>%
  group_by(store_Type) %>%
  summarise("ts" = sum(sales0, sales1, sales2, sales3, sales4)) 

store_train %>%
  rowwise() %>%
  group_by(store_Type) %>% 
  select(sales0, sales1, sales2, sales3, sales4) %>%
  summarise("ts" = var())




Tsales %>% 
  group_by(totalsales) %>% 
  summarise(var(store_Type))

# 9
# unique(store_train$state_alpha)-1

# q10

# Factor

# -------------Part 2---------------------------###
# 
# for(i in 1:ncol(store_train)){
#   if(class(store_train[,i])=="character"){
#     print(paste("Summary (Table) for ",names(store_train)[i]));
#     print(table(store_train[,i]))
#   }
# }


glimpse(store_train)
# visdat::vis_dat(store_train)

# Target :- "store"

names(store_train)

# table(store_train$store)
# tail(store_train$CouSub)
table(store_train$storecode)

# changing into factor
store_train$country<- as.factor(store_train$country)
store_train$State<- as.factor(store_train$State)

# storecode fn
storecode_func=function(x){
  x=substr(x,1,5)
  
  return(x)
}

# storecode_func(store_train$storecode)

## transfomraton fit
dp_pipe = recipe(store ~ ., data = store_train) %>%
  update_role(Id,countytownname,Areaname, new_role = "drop_vars") %>%
  update_role(
    store_Type,
    country,
    State,
    state_alpha,
    countyname,
    storecode,
    new_role = "to_dummies"
  ) %>%
  step_rm(has_role("drop_vars")) %>%
  step_mutate_at(storecode,fn=storecode_func) %>% 
  step_unknown(has_role("to_dummies"), new_level = "__missing__") %>%
  step_other(has_role("to_dummies"),
             threshold = 0.005,
             other = "__other__") %>%
  step_dummy(has_role("to_dummies")) %>%
  step_impute_median(all_numeric(),-all_outcomes())

dp_pipe=prep(dp_pipe)

## final transformation -> from fit 
train=bake(dp_pipe,new_data=NULL)
test=bake(dp_pipe,new_data=store_test)

head(train)

head(test)

head(train$storecode_METRO)
head(test$storecode_X__other__)

# visdat::vis_dat(train)

set.seed(2)
dim(train)
s=sample(1:nrow(train),0.8*nrow(train))
t1=train[s,] ## create as model
t2=train[-s,] ## validating this dataset


# computing vif
head(t1$store)

for_vif=lm(store~. -storecode_X__other__ -state_alpha_AL -state_alpha_AR -state_alpha_CA -state_alpha_CO -state_alpha_CT -state_alpha_FL -state_alpha_GA -state_alpha_IA -state_alpha_ID -state_alpha_IL -state_alpha_IN -state_alpha_KS -state_alpha_KY -state_alpha_LA -state_alpha_MA -state_alpha_ME -state_alpha_MI -state_alpha_MN -state_alpha_MO -state_alpha_MS -state_alpha_NE -state_alpha_NH -state_alpha_OK -state_alpha_PA -state_alpha_PR -state_alpha_NM -state_alpha_NY -state_alpha_OH -state_alpha_RI -state_alpha_RI -state_alpha_SC -state_alpha_SD -state_alpha_TN -state_alpha_TX -state_alpha_UT -state_alpha_VA -state_alpha_VT -state_alpha_WA -state_alpha_MT -state_alpha_NC -state_alpha_ND  -state_alpha_OR -state_alpha_WI   -state_alpha_WV -state_alpha_WY -state_alpha_X__other__ -store_Type_X__other__ -sales0 -sales2 -countyname_X__other__ -sales3 -State_X23 -sales1 -country_X__other__    -State_X25  -country_X27    -State_X33,data=t1)

# Using this for vif values only

# aliasing

alias(for_vif)

# sorting 

sort(vif(for_vif),decreasing = T)[1:3]

summary(for_vif)

log_fit=glm(store~. -storecode_X__other__ -state_alpha_AL -state_alpha_AR -state_alpha_CA -state_alpha_CO -state_alpha_CT -state_alpha_FL -state_alpha_GA -state_alpha_IA -state_alpha_ID -state_alpha_IL -state_alpha_IN -state_alpha_KS -state_alpha_KY -state_alpha_LA -state_alpha_MA -state_alpha_ME -state_alpha_MI -state_alpha_MN -state_alpha_MO -state_alpha_MS -state_alpha_NE -state_alpha_NH -state_alpha_OK -state_alpha_PA -state_alpha_PR -state_alpha_NM -state_alpha_NY -state_alpha_OH -state_alpha_RI -state_alpha_RI -state_alpha_SC -state_alpha_SD -state_alpha_TN -state_alpha_TX -state_alpha_UT -state_alpha_VA -state_alpha_VT -state_alpha_WA -state_alpha_MT -state_alpha_NC -state_alpha_ND  -state_alpha_OR -state_alpha_WI   -state_alpha_WV -state_alpha_WY -state_alpha_X__other__ -store_Type_X__other__ -sales0 -sales2 -countyname_X__other__ -sales3 -State_X23 -sales1 -country_X__other__    -State_X25  -country_X27    -State_X33 ,data=t1,
            family = "binomial")
summary(log_fit)

log_fit=stats::step(log_fit)
 
####
# 3517-2251
# aic 2437.65,AIC=2252.34


summary(log_fit)

formula(log_fit)

log_fit=glm(store ~ country_X7 + country_X13 + country_X51 + country_X89 + 
              country_X91 + country_X103 + country_X123 + State_X37 + State_X46 + 
              countyname_Coos.County + countyname_Jackson.County + 
              countyname_Litchfield.County + countyname_New.Haven.County + 
              countyname_Penobscot.County + 
              countyname_Windham.County + countyname_Worcester.County + 
              countyname_York.County + storecode_METRO, 
            family = "binomial", data = t1)

summary(log_fit)

# saveRDS(log_fit,file='D:/mylogitproject2R.RDS')
#### performance on t2 with auc score
# readRDS(file='D:/mylogit.RDS')

options(scipen=999)
val.score=predict(log_fit,newdata = t2,type='response')
tr.score=predict(log_fit,newdata = t1,type='response')
# tr.score

# install.packages('caTools')

pROC::auc(pROC::roc(t2$store,val.score)) ## 80%

pROC::auc(pROC::roc(t1$store,tr.score)) ## 20%

#Insatll required packages
# install.packages('caret')

#Import required library
library(caret)


# confusionMatrix()

### now fitting model on the entire data

for_vif=lm(store ~ country_X7 + country_X13 + country_X51 + country_X89 + 
             country_X91 + country_X103 + country_X123 + State_X37 + State_X46 + 
             countyname_Coos.County + countyname_Jackson.County + 
             countyname_Litchfield.County + countyname_New.Haven.County + 
             countyname_Penobscot.County + 
             countyname_Windham.County + countyname_Worcester.County + 
             countyname_York.County + storecode_METRO,data=train)

sort(vif(for_vif),decreasing=T)[1:3]

## 


summary(for_vif)

log_fit.final=glm(store ~ country_X7 + country_X13 + country_X51 + 
                    country_X89 + country_X91 + country_X103 + country_X123 + 
                    State_X37 + State_X46 + countyname_Coos.County + countyname_Jackson.County + 
                    countyname_Litchfield.County + countyname_New.Haven.County + 
                    countyname_Penobscot.County + countyname_Windham.County + 
                    countyname_Worcester.County + countyname_York.County + storecode_METRO,data=train,family = "binomial")

summary(log_fit.final)


log_fit.final=stats::step(log_fit.final)

summary(log_fit.final)

formula(log_fit.final)

log_fit.final=glm(store ~ country_X7 + country_X13 + country_X51 + country_X89 + 
                    country_X103 + State_X37 + countyname_Coos.County + countyname_Jackson.County + 
                    countyname_Litchfield.County + countyname_New.Haven.County + 
                    countyname_Worcester.County + countyname_York.County + storecode_METRO , data=train,
                  family="binomial")
summary(log_fit.final)


### finding cutoff for hard classes


train.score=predict(log_fit.final,newdata = train,type='response')

real=train$store

m = measureit(score = round(train.score,3), class = real,
              measure = c("ACC", "SENS", "SPEC","PREC","FSCR"))

cutoff_data =data.frame(Cutoff = m$Cutoff,
                        TP=m$TP,
                        TN=m$TN,
                        FP=m$FP,
                        FN=m$FN, 
                        Depth = m$Depth,
                        Accuracy = m$ACC,
                        Sensitivity = m$SENS,
                        Specificity = m$SPEC, 
                        F1 = m$FSCR) %>% 
  mutate(P=TP+FN,
         N=TN+FP,
         KS=(TP/P)-(FP/N)) %>% 
  select(-P,-N) %>% 
  na.omit() %>% 
  arrange(Cutoff)


# Depth	:What portion of the observations fall on or above the cutoff.

#### visualize how these measures move across cutoffs

ggplot(cutoff_data,aes(x=Cutoff,y=KS))+geom_line()


cutoff_long=cutoff_data %>% 
  select(Cutoff,Accuracy:KS) %>% 
  gather(Measure,Value,Accuracy:KS)

ggplot(cutoff_long,aes(x=Cutoff,y=Value,color=Measure))+geom_line()

# KS plot

rocit = rocit(score = train.score, 
              class = real) 

kplot=ksplot(rocit)

# cutoff on the basis of KS

my_cutoff=kplot$`KS Cutoff`
my_cutoff

# Lift Chart

gtable10 = gainstable(score = train.score, 
                      class = real, 
                      ngroup = 10)

print(gtable10)

plot(gtable10, type = 1)
##

### submission
## unseen data : Real life challenge
test.prob.score= predict(log_fit.final,newdata = test,type='response')
class(test.prob.score)
tp<- data.frame("store"=test.prob.score)
view(tp)
class((tp))
write.csv(tp,"Vishwajeet_Soni_P2_Part2again.csv",row.names = F)

test.predicted=as.numeric(test.prob.score>my_cutoff)

write.csv(test.predicted,"proper_submission_file_name.csv",row.names = F)






