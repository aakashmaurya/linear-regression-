####################################################################
################### linear regression case study################


sessionInfo()  
getwd()
setwd("F:\\r\\linear\\linear")

### packages required 

install.packages('readxl',dependencies = T)
install.packages('dplyr',dependencies = T)
install.packages("psych", dependencies = TRUE)
install.packages("MASS", dependencies = TRUE)
install.packages("car", dependencies = TRUE)
install.packages("sqldf", dependencies = TRUE)


# loading required packages

library(readxl)
library(dplyr)
require(psych)
library(MASS)
library(car)
require(sqldf)

# importing UK Linear Regsession Case xls file

givendata<-read_excel("Linear Regression Case.xlsx",1)


# ___ decriptive statistics_______

  mystats=function(x){
      if(class(x)=="numeric"){
          Var_Type=class(x)
          n<-length(x)
          nmiss<-sum(is.na(x))
          mean<-mean(x,na.rm=T)
          std<-sd(x,na.rm=T)
          var<-var(x,na.rm=T)
          min<-min(x,na.rm=T)
          p1<-quantile(x,0.01,na.rm=T)
          p5<-quantile(x,0.05,na.rm=T)
          p10<-quantile(x,0.1,na.rm=T)
          q1<-quantile(x,0.25,na.rm=T)
          q2<-quantile(x,0.5,na.rm=T)
        q3<-quantile(x,0.75,na.rm=T)
        p90<-quantile(x,0.9,na.rm=T)
          p95<-quantile(x,0.95,na.rm=T)
          p99<-quantile(x,0.99,na.rm=T)
          max<-max(x,na.rm=T)
          UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
          LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
          UC2=quantile(x,0.99,na.rm=T)
          LC2=quantile(x,0.01,na.rm=T)
  
          return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,UC1=UC1,LC1=LC1,UC2=UC2,LC2=LC2))
        }
      else{
          Var_Type=class(x)
          n<-length(x)
          nmiss<-sum(is.na(x))
          fre<-table(x)
          prop<-prop.table(table(x))
            
            return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
        }
    }

  num_var= sapply(givendata,is.numeric)
  Other_var= !sapply(givendata,is.numeric)
  
  num_diag_stats<-t(data.frame(apply(givendata[num_var], 2, FUN=mystats)))
 othr_diag_stats<-data.frame(t(apply(givendata[Other_var], 2, FUN=mystats)))
 
 #missing value imputation ##

 givendata$lntollmon[is.na(givendata$lntollmon)] <- 0 
 givendata$lntollten[is.na(givendata$lntollten)] <- 0
 givendata$lnequipmon[is.na(givendata$lnequipmon)] <- 0
 givendata$lnequipten[is.na(givendata$lnequipten)] <- 0
 givendata$lncardmon[is.na(givendata$lncardmon)] <- 0
 givendata$lncardten[is.na(givendata$lncardten)] <- 0
 givendata$lnwiremon[is.na(givendata$lnwiremon)] <- 0
 givendata$lnwireten[is.na(givendata$lnwireten)] <- 0
 givendata$longten[is.na(givendata$longten)] <- 2667.453
 givendata$lnlongten[is.na(givendata$lnlongten)] <- 7.889
 givendata$lncreddebt[is.na(givendata$lncreddebt)] <- 0
 givendata$lnothdebt[is.na(givendata$lnothdebt)] <- 0
 givendata$commutetime[is.na(givendata$commutetime)] <- 0
 
 
 ####outlier treatment #####
 
 givendata$ reside  [givendata$ reside >6.383]<-6
 givendata$ employ  [givendata$ employ >38.8031861]<-38.8
 givendata$ ed  [givendata$ ed >24.3862484]<-24.3862484
 givendata$ income  [givendata$	 income >220.8921336]<-220.8921336
   givendata$ lninc  [givendata$	lninc >5.9411251]<-5.9411251
 givendata$ debtinc  [givendata$	debtinc >29.1535099]<-29.1535099
 givendata$ creddebt  [givendata$ creddebt >12.1045216]<-12.1045216	
 givendata$ lncreddebt  [givendata$ lncreddebt >3.6887217]<-3.6887217
 givendata$ othdebt  [givendata$ othdebt >19.839975]<-19.839975	
 givendata$ lnothdebt  [givendata$ lnothdebt >4.0826499]<-4.0826499
 givendata$ carvalue  [givendata$ carvalue >86.9274901]<-86.9274901	
 givendata$ lncreddebt  [givendata$ lncreddebt >-3.9496287]<-3.9496287
 givendata$ lnothdebt  [givendata$ lnothdebt >-2.6888193]<-2.6888193
 givendata$ carvalue  [givendata$ carvalue >-40.4623301]<-40.4623301
 givendata$ cardspent[givendata$ cardspent>1072.6375  ]<-1072.6375
 givendata$ card2spent[givendata$ card2spent<599.752  ]<- 599.752
 givendata$ longmon[givendata$ longmon>51.79  ]<-51.79
 givendata$ lnlongmon[givendata$ lnlongmon>4.5543129   ]<-4.5543129
 givendata$ tollmon[givendata$ tollmon>51.58345  ]<-51.58345
 givendata$ lntollmon[givendata$ lntollmon>6.4735885]<-6.4735885
 givendata$ tollten [givendata$ tollten>3425.287269]<-3425.287269
 givendata$ lntollten[givendata$ lntollten>13.316839]<-13.316839
 givendata$ equipmon [givendata$ equipmon>70.6301378]<-70.6301378
 givendata$ lnequipmon [givendata$ lnequipmon>6.3707655]<-6.3707655
 givendata$ equipten [givendata$ equipten>3206.838272]<-3206.838272
 givendata$ lnequipten [givendata$ lnequipten>12.1217387]<-12.1217387
 givendata$ lncardmon [givendata$ lncardmon>6.2729328]<-6.2729328
 givendata$ cardten [givendata$ cardten>3487.154971]<-3487.154971
 givendata$ lncardten [givendata$ lncardten>13.7912755]<-13.7912755
 givendata$ wiremon [givendata$ wiremon>70.1006995]<-70.1006995
 givendata$ lnwiremon [givendata$ lnwiremon>5.8023953]<-5.8023953
 givendata$ wireten [givendata$ wireten>3424.98461]<-3424.98461
 givendata$ lnwireten [givendata$ lnwireten>11.1032683]<-11.1032683
 givendata$ wireten [givendata$ wireten>3424.98461]<-3424.98461
 givendata$ wireten [givendata$ wireten>3424.98461]<-3424.98461
 givendata$ wireten [givendata$ wireten>3424.98461]<-3424.98461
 givendata$ wireten [givendata$ wireten>3424.98461]<-3424.98461
 givendata$ wireten [givendata$ wireten>3424.98461]<-3424.98461
 
 
 #  deriverd variable :
 
 givendata<- mutate(givendata,total_spent= cardspent + card2spent)
 givendata<- mutate(givendata,Total_fee = cardfee + card2fee)
 givendata<- mutate(givendata,Total_items=carditems + card2items)
 givendata<- mutate(givendata,ln_spent = log(total_spent))

 ###univaraite analysis of dependent varaible
 hist(givendata$total_spent)
 hist(givendata$ln_spent)
 givendata$ ln_spent [givendata$ ln_spent>7.5]<-7.5
 givendata$ ln_spent [givendata$ ln_spent<6.2]<-6.2
 
 
 ########################## factor analysis ###########################3
 reqdata<-select_if(givendata, is.numeric)
 reqdata<-reqdata[ , -which(names(reqdata) %in% c("cardten","carvalue","townsize"))]
 ## FACTOR ANALYSIS 
 corrm<- cor(reqdata)                                ### CORRELATION MATRIX

 corrm<-cor.smooth(corrm)
 
 ### DECIDING NUMBER OF FACTORS USING SCREE PLOT & KAISER TEST(NUMBER OF EIGEN VALUES OVER 1)
 
 scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) ### SCREE PLOT
 
 eigen(corrm)$values                                                     ### EIGEN VALUES
 
 
 eigen_values <- mutate(data.frame(eigen(corrm)$values)
                        ,cum_sum_eigen=cumsum(eigen.corrm..values)
                        , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                        , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  # CALCULATING VARIANCE, CUMULATIVE VARIANCE etc... 
 
 write.csv(eigen_values, "F:\\r\\linear\\linear\\creditcardEIGENVALUES.CSV")
 
 FA<-fa(r=corrm, 38, rotate="varimax",fm="ml")               ### CONDUCTING FACTOR ANALYSIS
 print(FA)                                                    ### PRINT THE RESULTS
 FA_SORT<-fa.sort(FA)                                         ### SORTING THE LOADINGS
 ls(FA_SORT)                                                  ### LISTING OUT THE OBJECTS
 FA_SORT$loadings
                                  
 Loadings<-data.frame(FA_SORT$loadings[1:ncol(reqdata),]) ### CAPTURING ONLY LOADINGS INTO DATA FRAME
 
 write.csv(Loadings, "F:\\r\\linear\\linear\\creditcardloading.csv") ### SAVING THE FILE
 
 
 #Splitting data into Training, Validaton and Testing Dataset
 
 train_ind <- sample(1:nrow(givendata), size = floor(0.70 * nrow(givendata)))
 
 training<-givendata[train_ind,]
 testing<-givendata[-train_ind,]
 
 # building model
 
 fit1<-lm(ln_spent~ +	agecat
          +	edcat
          +	jobcat
          +	empcat
          +	retire
          +	income
          +	lninc
          +	inccat
          +	debtinc
          +	creddebt
          +	lncreddebt
          +	othdebt
          +	lnothdebt
          +	default
          +	jobsat
          +	reside
          +	pets
          +	homeown
          +	cars
          +	commute
          +	card
          +	cardtype
          +	card2
          +	card2type
          +	tenure
          +	churn
          +	longmon
          +	lnlongmon
          +	longten
          +	lnlongten
          +	tollfree
          +	tollmon
          +	lntollmon
          +	tollten
          +	lntollten
          +	equip
          +	equipmon
          +	lnequipmon
          +	equipten
          +	lnequipten
          +	callcard
          +	cardmon
          +	lncardmon
          +	cardten
          +	lncardten
          +	wireless
          +	wiremon
          +	lnwiremon
          +	wireten
          +	lnwireten
          +	internet
          +	callid
          +	callwait
          +	forward
          +	confer
          +	ebill
          +	response_01
          +	response_02
          +	response_03
          +	Total_fee
          +	Total_items
          ,data=training)
 
 step1<-stepAIC(fit1,direction = "both")
 
 
 fit <- lm ( ln_spent~Total_items+lninc+ card+ card2 +response_03 +  gender + lncardmon +lnwireten,data=training)
summary(fit)  

#multicollinearity check using vif
vif(fit)

###########SCORING USING PREDICT FUNCTION
t1<-cbind(training, pred_spent = exp(predict(fit)))
names(t1)
t1<- transform(t1, APE = abs(pred_spent - total_spent)/total_spent)
mean(t1$APE)
View(t1)

t2<-cbind(testing, pred_spent=exp(predict(fit,testing)))
t2<- transform(t2, APE = abs(pred_spent - total_spent)/total_spent)
mean(t2$APE)
View(t2)

#############Decile Analysis Reports - t1(training)

# find the decile locations 
decLocations <- quantile(t1$pred_spent, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
t1$decile <- findInterval(t1$pred_spent,c(-Inf,decLocations, Inf))

t1_DA <- sqldf("select decile, count(decile) as count, avg(pred_spent) as avg_pre_spent,   
               avg(total_spent) as avg_Actual_spent,sum(pred_spent) as total_pre_spent ,
              sum(total_spent) as total_act_spent
               from t1
               group by decile
               order by decile desc")

View(t1_DA)
write.csv(t1_DA,"decilefortaining.csv")


###################Decile Analysis Reports - t2(testing)

# find the decile locations 
decLocations <- quantile(t2$pred_spent, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
t2$decile <- findInterval(t2$pred_spent,c(-Inf,decLocations, Inf))

require(sqldf)
t2_DA <- sqldf("select decile, count(decile) as count, avg(pred_spent) as avg_pre_spent,   
               avg(total_spent) as avg_Actual_spent ,sum(pred_spent) as total_pre_spent ,
               sum(total_spent) as total_act_spent
               from t2
               group by decile
               order by decile desc")

View(t2_DA)
write.csv(t2_DA,"decilefortesting.csv")


###################### ENF OF CASE STUDY ######################
