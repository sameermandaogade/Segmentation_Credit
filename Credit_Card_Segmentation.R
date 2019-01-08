# Import the file

mydata<-read.csv("E:\\Career\\R Analytics\\Data science Case studies\\Credit Card Segmentation\\CC GENERAL.csv")



#################################################################
# deriving "intelligent" KPIs

#---------------- MOnthly average purchase------------
mydata$Monthly_Avg_Purchase<-mydata$PURCHASES/12

# ---------------Cash advance amount-------------------
mydata$Monthly_Avg_Cash_Advance_Amt<-mydata$CASH_ADVANCE/12

#---------------Purchases by type (one-off, installments)-----------------
mydata$Purchase_type<-ifelse(mydata$ONEOFF_PURCHASES==0 & mydata$INSTALLMENTS_PURCHASES>0,"Installation Purchase",ifelse(mydata$ONEOFF_PURCHASES>0 & mydata$INSTALLMENTS_PURCHASES==0,"One-off purchase",
                                              ifelse(mydata$ONEOFF_PURCHASES>0 & mydata$INSTALLMENTS_PURCHASES>0,"Both","No purchase"
                                                     )))

View(mydata$Purchase_type)
# convert it to factor
mydata$Purchase_type<-factor(mydata$Purchase_type)

#str(mydata$Purchase_type)
#levels(mydata$Purchase_type)


# ------Average amount per purchase and cash advance transaction------


#PURCHASES_TRX--Number of transaction for purchase

mydata$Avg_Amt_per_Purchase<-mydata$PURCHASES/mydata$PURCHASES_TRX

mydata$Avg_Amt_per_Purchase[mydata$PURCHASES_TRX==0]=0


# The above statement is done because is some observation Purchase_TRX is 0 so ultimately 
# Avg_Amt_per_Purchase is comes as NaN so to replace this by zero as it have no significance
# and to avoid further error.

View(mydata$Avg_Amt_per_Purchase)


mydata$Avg_cash_Adv_transaction<-mydata$CASH_ADVANCE/mydata$CASH_ADVANCE_TRX

mydata$Avg_cash_Adv_transaction[mydata$CASH_ADVANCE_TRX==0]=0

# The above statement is done because is some observation cash_advance_TRX is 0 so ultimately 
# Avg_cash_Adv_transaction is comes as NaN so to replace this by zero as it have no significance
# and to avoid further error.


#-------------------Limit usage (balance to credit limit ratio)------------------

mydata$Limit_Usage<-mydata$BALANCE/mydata$CREDIT_LIMIT



#--------------------Payments to minimum payments ratio etc.-----------------
mydata$Pay_to_MinPay_ratio<-mydata$PAYMENTS/mydata$MINIMUM_PAYMENTS


View(mydata)
#------------------------------------------------------------------
#-------- Analysis on the given KPIS is done in excel and tableau-----------------------
#------------------------------------------------------------------
names(mydata)

mydata1<-mydata

mydata_KPI<-mydata[,c("CUST_ID","Monthly_Avg_Purchase","Monthly_Avg_Cash_Advance_Amt"
                      ,"Purchase_type", "Avg_Amt_per_Purchase" ,           
                 "Avg_cash_Adv_transaction" , "Limit_Usage" , "Pay_to_MinPay_ratio")]

#class(mydata_KPI$Purchase_type)
write.csv(mydata_KPI,"E:\\Career\\R Analytics\\Data science Case studies\\Credit Card Segmentation\\KPI.csv")

quantile(mydata$Avg_Amt_per_Purchase,probs = seq(0.1,0.9,by=0.1))
# below 30.642-low 68.42-med and above high

quantile(mydata$Monthly_Avg_Purchase,probs = seq(0.1,0.9,by=0.1))
# below 46.46 -low 118.-med and above high

quantile(mydata$Monthly_Avg_Cash_Advance_Amt,probs = seq(0.1,0.9,by=0.1))
# L 30  M 130 H

quantile(mydata$Avg_cash_Adv_transaction,probs = seq(0.1,0.9,by=0.1))
# L 178 M 351 H

quantile(mydata$Limit_Usage,probs = seq(0.1,0.9,by=0.1),na.rm = TRUE)
# L 0.15 M 0.62 H

quantile(mydata$Pay_to_MinPay_ratio,probs = seq(0.1,0.9,by=0.1),na.rm = TRUE)
# L 1.48 M 5.12 H
#------------------------------------------------------------------------------- 
#----------------------------FACTOR ANALYSIS-----------------------------------------


mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a,is.na=TRUE)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  iqr=IQR(x,na.rm=T)
  UC=quantile(x,0.951,na.rm=T)
  LC=quantile(x,0.051,na.rm=T)
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}


num_var<-sapply(mydata, is.numeric)  # Getting all the numeric variables
str(mydata)
#sum(num_var)
options(scipen=999)
#Applying above defined function on numerical variables
my_num_data <- t(data.frame(apply(mydata[num_var], 2, mystats)))
View(my_num_data)

write.csv(my_num_data,"E:\\Career\\R Analytics\\Data science Case studies\\Credit Card Segmentation\\variables_statistics_summary.csv")

# Outlier treatment


M1_fun <- function(x){
  quantiles <- quantile(x, c(.05, .95 ),na.rm=TRUE )
  # Above line will calc the P5 and P95
  
  x[ x < quantiles[1] ] <- quantiles[1]  # if value < P1, then P5
  x[ x > quantiles[2] ] <- quantiles[2]  # if value > P99, then P95
  x
}

mydata[,num_var] <- apply(data.frame(mydata[,num_var]), 2, M1_fun) 


my_num_data2 <- t(data.frame(apply(mydata[num_var], 2, mystats)))
View(my_num_data2)


# Missing value treatment

mydata[,num_var] <- apply(data.frame(mydata[,num_var]), 
                          2, 
                          function(x){x <- replace(x, is.na(x), mean(x, na.rm=TRUE))})



options(scipen = 999)


#---------------------------------Factor analysis-------------------------------------

clus1<-c("BALANCE",  "BALANCE_FREQUENCY", "Monthly_Avg_Purchase",  "ONEOFF_PURCHASES" ,"INSTALLMENTS_PURCHASES", 
         "Monthly_Avg_Cash_Advance_Amt","PURCHASES_FREQUENCY", "ONEOFF_PURCHASES_FREQUENCY","PURCHASES_INSTALLMENTS_FREQUENCY",
        "CASH_ADVANCE_FREQUENCY" , "CASH_ADVANCE_TRX"  ,"PURCHASES_TRX","CREDIT_LIMIT","PAYMENTS",                        
        "MINIMUM_PAYMENTS" ,"PRC_FULL_PAYMENT", "TENURE" , "Avg_cash_Adv_transaction","Limit_Usage","Pay_to_MinPay_ratio"                                 
)
mydata2<-mydata[clus1]
sum(is.na(mydata2))
corrm<-cor(mydata2)


View(corrm)

require(psych)
require(GPArotation)


### DECIDING NUMBER OF FACTORS USING SCREE PLOT & KAISER TEST(NUMBER OF EIGEN VALUES OVER 1)

plot.new()
scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=T) ### SCREE PLOT
VSS.scree(corrm, main = "scree plot")


require(dplyr)
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values)) 



plot(eigen_values$pct_var,type='b')

FA<-fa(r=corrm, 5, rotate="varimax", fm="ml")             

print(FA)                                               

FA_SORT<-fa.sort(FA)                                

ls(FA_SORT)                                       
FA_SORT$loadings
#FA_SORT$e.values                                         
Loadings<-data.frame(FA_SORT$loadings[1:ncol(mydata2),]) 
View(Loadings)
write.csv(Loadings, "E://Career//R Analytics//Data science Case studies//Credit Card Segmentation//loadings_F.csv") ### SAVING THE FILE



####################  Iteration 1 ##################################


# AFTER DOING FACTOR ANALYSIS -IMP VARIABLES ARE LISTED BELOW
#1.Purchases installments frequnecy
#2.ONEOFF_PURCHASES
#3.Payment to minimum payment ratio
#4.PAYMENTS
#5.CASH_ADVANCE_TRX
#6.BALANCE





#----------------------------------------------#
#--------------CLUSTERING-Iteration 1----------------------#
#-----------------------------------------------#

inputdata_final<-mydata[,c("PURCHASES_INSTALLMENTS_FREQUENCY","ONEOFF_PURCHASES","Pay_to_MinPay_ratio",
                           "BALANCE","CASH_ADVANCE_TRX","PAYMENTS")]


mydata_clus <- mydata2

wss <- (nrow(mydata_clus)-1)*sum(apply(mydata_clus,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata_clus,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#One of the metrics that is commonly used to compare results across different values of K 
#is the mean distance between data points and their cluster centroid. Since increasing 
#the number of clusters will always reduce the distance to data points,
#increasing K will always decrease this metric, to the extreme of reaching zero 
#when K is the same as the number of data points. Thus, this metric cannot be used 
#as the sole target. Instead, mean distance to the centroid as a function of K is 
#plotted and the "elbow point," where the rate of decrease sharply shifts, 
#can be used to roughly determine K.

# 5 or 6 cluster is optimal

# standardise the variable for clustering solution
inputdata_Final<-data.frame(scale(inputdata_final))

inputdata_clus<-inputdata_Final



cluster_three <- kmeans(inputdata_clus,3)
cluster_four <- kmeans(inputdata_clus,4)
cluster_five <- kmeans(inputdata_clus,5)
cluster_six <- kmeans(inputdata_clus,6)
cluster_seven<-kmeans(inputdata_clus,7)
cluster_eight<-kmeans(inputdata_clus,8)



summary(cluster_three)



mydata_new<-cbind(mydata,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster ,
                  km_clust_6=cluster_six$cluster,km_clust_7=cluster_seven$cluster,km_clust_8=cluster_eight$cluster )
View(mydata_new) 

write.csv(mydata_new,"E:\\Career\\R Analytics\\Data science Case studies\\Credit Card Segmentation\\profile1Final.csv",row.names = F)

#Graph based on k-means - Optional
require(cluster)

clusplot(inputdata_clus, #dataframe
         cluster_three$cluster, #clusterdata
         color = TRUE, #color
         #shade = TRUE, # Lines in clusters
         lines =6, # lines connecting centroids
         labels = 2 # Labels clusters and cases
)

###Profiling

#Converting into factors
mydata_new$km_clust_3=factor(mydata_new$km_clust_3)
mydata_new$km_clust_4=factor(mydata_new$km_clust_4)
mydata_new$km_clust_5=factor(mydata_new$km_clust_5)
mydata_new$km_clust_6=factor(mydata_new$km_clust_6)
mydata_new$km_clust_7=factor(mydata_new$km_clust_7)
mydata_new$km_clust_8=factor(mydata_new$km_clust_8)


#install.packages("tables")
require(tables)


profile<-tabular(1+PURCHASES_INSTALLMENTS_FREQUENCY+ ONEOFF_PURCHASES+ Pay_to_MinPay_ratio+BALANCE+CASH_ADVANCE_TRX
                 +PAYMENTS+
                  PURCHASES_FREQUENCY	+INSTALLMENTS_PURCHASES	+PURCHASES_TRX+ ONEOFF_PURCHASES_FREQUENCY+ Monthly_Avg_Purchase	+
                  Limit_Usage	+MINIMUM_PAYMENTS	+PRC_FULL_PAYMENT	+BALANCE_FREQUENCY	+CASH_ADVANCE_FREQUENCY	+
                  Monthly_Avg_Cash_Advance_Amt+TENURE+CREDIT_LIMIT	+ Avg_cash_Adv_transaction	
                 ~ mean +(mean*km_clust_3)+(mean*km_clust_4)+(mean*km_clust_5)+(mean*km_clust_6)+(mean*km_clust_7)+(mean*km_clust_8),
                 data=mydata_new)

profile1<-as.matrix(profile)


profile1<-data.frame(profile1)
View(profile1)

profile<-tabular(1~length+(length*km_clust_3)+(length*km_clust_4)+(length*km_clust_5)+(length*km_clust_6)+
                   (length*km_clust_7)+(length*km_clust_8),
                 data=mydata_new)

profile2<-as.matrix(profile)
profile2<-data.frame(profile2)
View(profile2)


write.csv(profile1,"E:\\Career\\R Analytics\\Data science Case studies\\Credit Card Segmentation\\profile1Final_1.csv",row.names = F)
write.csv(profile2,"E:\\Career\\R Analytics\\Data science Case studies\\Credit Card Segmentation\\profile2Final_1.csv",row.names = F)
#############################END OF k-Means Segmentation############################


####################  Iteration 2 ##################################


# AFTER DOING FACTOR ANALYSIS -IMP VARIABLES ARE LISTED BELOW
#1.Purchases installments frequnecy
#2.ONEOFF_PURCHASES
#3.Limit usage
#4.PAYMENTS
#5.CASH_ADVANCE_TRX
#6.BALANCE





#----------------------------------------------#
#--------------CLUSTERING-Iteration 2----------------------#
#-----------------------------------------------#

inputdata_final_2<-mydata[,c("PURCHASES_INSTALLMENTS_FREQUENCY","ONEOFF_PURCHASES","Limit_Usage",
                           "BALANCE","CASH_ADVANCE_TRX","PAYMENTS")]


# Standardize the selected variables for clustering
inputdata_Final_2<-data.frame(scale(inputdata_final_2))

inputdata_clus_2<-inputdata_Final_2



cluster_three_2 <- kmeans(inputdata_clus_2,3)
cluster_four_2 <- kmeans(inputdata_clus_2,4)
cluster_five_2 <- kmeans(inputdata_clus_2,5)
cluster_six_2 <- kmeans(inputdata_clus_2,6)
cluster_seven_2<-kmeans(inputdata_clus_2,7)
cluster_eight_2<-kmeans(inputdata_clus_2,8)



summary(cluster_three_2)



mydata_new_2<-cbind(mydata,km_clust_3_2=cluster_three_2$cluster,km_clust_4_2=cluster_four_2$cluster,km_clust_5_2=cluster_five_2$cluster ,
                  km_clust_6_2=cluster_six_2$cluster,km_clust_7_2=cluster_seven_2$cluster,km_clust_8_2=cluster_eight_2$cluster )
View(mydata_new_2) 

write.csv(mydata_new_2,"E:\\Career\\R Analytics\\Data science Case studies\\Credit Card Segmentation\\profile1Final_2.csv",row.names = F)

#Graph based on k-means - Optional
require(cluster)

clusplot(inputdata_clus_2, #dataframe
         cluster_three$cluster, #clusterdata
         color = TRUE, #color
         #shade = TRUE, # Lines in clusters
         lines =6, # lines connecting centroids
         labels = 2 # Labels clusters and cases
)

###Profiling

#Converting into factors
mydata_new_2$km_clust_3_2=factor(mydata_new_2$km_clust_3_2)
mydata_new_2$km_clust_4_2=factor(mydata_new_2$km_clust_4_2)
mydata_new_2$km_clust_5_2=factor(mydata_new_2$km_clust_5_2)
mydata_new_2$km_clust_6_2=factor(mydata_new_2$km_clust_6_2)
mydata_new_2$km_clust_7_2=factor(mydata_new_2$km_clust_7_2)
mydata_new_2$km_clust_8_2=factor(mydata_new_2$km_clust_8_2)


#install.packages("tables")
require(tables)


profile_2<-tabular(1+PURCHASES_INSTALLMENTS_FREQUENCY+ ONEOFF_PURCHASES+Limit_Usage +BALANCE+CASH_ADVANCE_TRX
                 +PAYMENTS+
                   PURCHASES_FREQUENCY	+INSTALLMENTS_PURCHASES	+PURCHASES_TRX+ ONEOFF_PURCHASES_FREQUENCY+ Monthly_Avg_Purchase	+
                   Pay_to_MinPay_ratio	+MINIMUM_PAYMENTS	+PRC_FULL_PAYMENT	+BALANCE_FREQUENCY	+CASH_ADVANCE_FREQUENCY	+
                   Monthly_Avg_Cash_Advance_Amt+TENURE+CREDIT_LIMIT	+ Avg_cash_Adv_transaction	
                 ~ mean +(mean*km_clust_3_2)+(mean*km_clust_4_2)+(mean*km_clust_5_2)+(mean*km_clust_6_2)+(mean*km_clust_7_2)+(mean*km_clust_8_2),
                 data=mydata_new_2)

profile1_2<-as.matrix(profile_2)


profile1_2<-data.frame(profile1_2)
View(profile1)

profile_2<-tabular(1~length+(length*km_clust_3_2)+(length*km_clust_4_2)+(length*km_clust_5_2)+(length*km_clust_6_2)+
                   (length*km_clust_7_2)+(length*km_clust_8_2),
                 data=mydata_new_2)

profile2_2<-as.matrix(profile_2)
profile2_2<-data.frame(profile2_2)
View(profile2_2)


write.csv(profile1_2,"E:\\Career\\R Analytics\\Data science Case studies\\Credit Card Segmentation\\profile1Final_2_2.csv",row.names = F)
write.csv(profile2_2,"E:\\Career\\R Analytics\\Data science Case studies\\Credit Card Segmentation\\profile2Final_2_2.csv",row.names = F)
#############################END OF k-Means Segmentation############################



