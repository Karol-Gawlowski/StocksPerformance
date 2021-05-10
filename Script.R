#########################################################################################################
############################################# Initialization ############################################
#########################################################################################################

library(RSNNS)
library(plyr)
library(tibble)
library(tidyverse)
library(Matrix)
library(moments)
library(kernlab)
library(dplyr)
library(caret)
library(naniar)
library(pROC)

confusionMatrix=caret::confusionMatrix
train=caret::train

set.seed(1)
options(scipen=999)

data=read.csv("US-stock.csv")

# handy function
'%ni%'=Negate('%in%')

# number of variables, to track data cleaning
Variables_Count=ncol(data)

# Treshold above which (absolute val.) to drop correlated variables
Treshold_to_drop_cor=0.7 
# percentage of NA above which we drop a variable
Treshold_to_drop=0.3

# percentage of NA above which we drop an instance
Treshold_to_drop_tick=0.3

#########################################################################################################
######################### Feature Engineering (Domain/Accounting knowledge) #############################
#########################################################################################################

# We create new variables using accounting formulas (more in the report)
data=data %>% mutate(IncomeCover = EBIT/Interest.Expense %>% 
                                   replace(is.infinite(EBIT/Interest.Expense),NA),
                     MarketPrice = PE.ratio*EPS.Diluted %>% 
                                   replace(is.infinite(PE.ratio*EPS.Diluted),NA),
                     AfterTax = Earnings.before.Tax - Income.Tax.Expense %>% 
                                replace(is.infinite(Earnings.before.Tax - Income.Tax.Expense),NA),
                     OrdinaryShares = AfterTax/EPS %>% 
                                      replace(is.infinite(AfterTax/EPS),NA)) %>% 
              relocate(Sector, .after=OrdinaryShares) %>% 
              relocate(Class, .before=Sector)

#########################################################################################################
################################### Initial Exploratory Data ANalysis ###################################
#########################################################################################################

# Distribution of sectors by Class 
data %>%  mutate(Class=factor(Class)) %>% 
                  count(Sector,Class) %>% 
                  ggplot(aes(x=reorder(Sector,n),y=n,fill=Class)) + 
                  geom_col()+
                  ggtitle("Count by sector")+
                  xlab("")+
                  ylab("")+
                  theme(axis.text.x = element_text(angle = 45, vjust =0.5, hjust=0.5,size=15))

# ----------------------------------------------------------------------------------------------------------
# Density of revenue by Class
data %>% mutate(Class=factor(Class)) %>% 
                  mutate(Revenue=Revenue/1000000) %>% 
                  ggplot(aes(x=Revenue,fill=Class)) + 
                  geom_density(alpha=.5)+
                  xlim(0,5000)+
                  ggtitle("Revenue in M by Class")+
                  xlab("")+
                  ylab("")+
                  theme(axis.text.x = element_text(vjust =0.5, hjust=0.5,size=15))

# ----------------------------------------------------------------------------------------------------------
# Density of profit margin  by Class
data %>% mutate(Class=factor(Class)) %>% 
                  ggplot(aes(x=netProfitMargin,fill=Class)) + 
                  geom_density(alpha=.5)+
                  xlim(0,1)+
                  ggtitle("Net Profit Margin by Class")+
                  xlab("")+
                  ylab("")+
                  theme(axis.text.x = element_text(vjust =0.5, hjust=0.5,size=15))

# ----------------------------------------------------------------------------------------------------------
# Density of Gross Profit by Class - not informative
data %>% mutate(Class=factor(Class)) %>% 
                # mutate(Gross.Profit=Gross.Profit) %>% 
                ggplot(aes(x=Revenue,fill=Class)) + 
                geom_density(alpha=500)+
                xlim(0,5000000)+
                ggtitle("Gross Profit in Thousands, by Class")+
                xlab("")+
                ylab("")+
                theme(axis.text.x = element_text(vjust =0.5, hjust=0.5,size=15))

# ----------------------------------------------------------------------------------------------------------
# % of NA
sapply(data, function(x){ return(sum(is.na(x))/length(x))}) %>% 
                as.data.frame() %>% 
                mutate(Variable=colnames(data),NA_perc=.*1) %>% 
                remove_rownames() %>% 
                select(Variable,NA_perc) %>% 
                filter(NA_perc>0.35) %>%
                arrange(NA_perc) %>% 
                ggplot(aes(x=reorder(Variable,NA_perc),y=NA_perc))+
                geom_col()+
                ggtitle("NA % Distribution")+
                xlab("")+
                ylab("")+
                theme(axis.text.x = element_text(angle = 45, vjust =0.5, hjust=0.5,size=10))

# total percentage of NA
sum(is.na(data))/(nrow(data)*ncol(data))
vis_miss(data,warn_large_data = FALSE,show_perc_col = FALSE)

# ----------------------------------------------------------------------------------------------------------
# automatic density plot generation
# This is a relatively lengthy procedure, so by default the control variable generate_plots is set to "no".
generate_plots="no"

# # NOT RUN 
if (generate_plots!="no") {
  for (i in 1:(ncol(data)-1)){
    if (is.numeric(data[,i])) {
      variable_name=colnames(data)[i]
      
      print(ggplot(data,aes(x=eval(as.symbol(variable_name)),colour=Class))+geom_density()+ggtitle(paste(variable_name,i)))
      
      print(paste("done:",i,"/",(ncol(data)-1)))
    }
  }
  rm(generate_plots,variable_name)
}  


#########################################################################################################
############################################# Data Cleaning #############################################
#########################################################################################################

# Drop variables with lots of NA
Variables_to_Drop=sapply(data, function(x){ return(sum(is.na(x))/length(x))}) %>% 
                  as.data.frame() %>% 
                  mutate(Variable=colnames(data),NA_perc=.*1) %>% 
                  remove_rownames() %>% 
                  select(Variable,NA_perc) %>% 
                  arrange(NA_perc) %>% 
                  filter(NA_perc>Treshold_to_drop) %>% 
                  select(Variable) %>% 
                  as.matrix() %>% 
                  apply(1, toString)

data=data %>% select(-all_of(Variables_to_Drop))
print(paste("Dropped: ",length(Variables_to_Drop)," variables, which contained more than", Treshold_to_drop*100,"% NA"))

# ----------------------------------------------------------------------------------------------------------
# Drop instances with lots of NA
Tickers_to_Drop=apply(data,1,function(x){return(sum(is.na(x))/length(x))}) %>% 
                 as.data.frame() %>% 
                 mutate(Ticker=data$X,NA_perc=.*1) %>% 
                 select(Ticker,NA_perc) %>% 
                 arrange(NA_perc) %>% 
                 filter(NA_perc>Treshold_to_drop_tick) %>% 
                 select(Ticker) %>% 
                 as.matrix() %>% 
                 apply(1, toString)
  
data=subset(data, subset = !(X %in% Tickers_to_Drop))
print(paste("Dropped: ",length(Tickers_to_Drop)," instances, which had more than ", Treshold_to_drop_tick*100,"% NA"))
rm(Tickers_to_Drop)

# ----------------------------------------------------------------------------------------------------------
# Drop variables with variance<1
Abs_Variation_coef=apply(data %>% select(-X,-Sector,-Class),2,function(x){return(var(x,na.rm = TRUE)/mean(x,na.rm = TRUE))}) %>% 
                    as.data.frame() %>% 
                    mutate(Variance=.*1) %>% 
                    rownames_to_column(var = "Variable") %>% 
                    select(Variable,Variance) %>% 
                    mutate(Variance=abs(Variance)) %>% 
                    arrange(Variance) %>% 
                    filter(Variance<=1) %>% 
                    select(Variable) %>% 
                    as.matrix() %>% 
                    apply(1, toString)

# operatingProfitMargin is the only element with zero variation, so we drop it
data=data %>% select(-all_of(Abs_Variation_coef))
print(paste("Dropped: ",length(Abs_Variation_coef)," variables, which had variance lower than 1 "))


# ----------------------------------------------------------------------------------------------------------
# Drop variables based on correlation
# First we prepare the correlation table
Flat_correlations=cor(data %>% select(-X,-Sector,-Class),use = "complete.obs") %>% 
                   tril() %>% 
                   as.matrix() %>% 
                   as.data.frame() %>%
                   gather(row,value) %>% 
                   mutate(column=rep(colnames(data %>% select(-X,-Sector,-Class)),length(colnames(data))-3))

# Thanks to this table, we found that there are duplicate variables and extremely high correlations - we drop those
Duplicates=Flat_correlations %>% 
                            filter(value>=0.999 | value<=-0.999) %>% 
                            mutate(index=(row!=column)*1) %>% 
                            filter(index==1) %>% 
                            select(row) %>%
                            apply(1, toString) %>% unique()

data=data %>% select(-all_of(Duplicates))
print(paste("Found: ",length(Duplicates)," duplicate variables to drop"))

# ----------------------------------------------------------------------------------------------------------
# Dropping highly correlated data - variables that have mod correlations higher than set Treshold_to_drop_cor
# we recalculate the correlation coefficients on remaining variables
Flat_correlations=cor(data %>% select(-X,-Sector,-Class),use = "complete.obs") %>% 
                      tril() %>% 
                      as.matrix() %>% 
                      as.data.frame() %>%
                      gather(row,value) %>% 
                      mutate(column=rep(colnames(data %>% select(-X,-Sector,-Class)),length(colnames(data))-3))

# retrieve the highly correlated variable names
Highly_correlated=Flat_correlations %>% 
                  filter(((value>Treshold_to_drop_cor | value<(-Treshold_to_drop_cor)) & value!=1)) %>% 
                  relocate(column, .after=row) %>% 
                  arrange(-value) %>% 
                  select(column) %>% 
                  unique() %>%
                  apply(1, toString) 

data=data %>% select(-all_of(Highly_correlated))

total_dropped=length(c(Highly_correlated,Duplicates,Abs_Variation_coef,Variables_to_Drop))

print(paste("Found: ",length(Highly_correlated)," highly correlated (above",Treshold_to_drop_cor*100,"%) variables to drop"))
print(paste("Dropped a total of:",total_dropped,"Variables,",
            "leaving:",ncol(data),"in the dataset"))

rm(Highly_correlated,Duplicates,Abs_Variation_coef,Variables_to_Drop,Flat_correlations)

# ----------------------------------------------------------------------------------------------------------
# Imputation - median based 
# first find variables with missing values
To_impute=apply(data,2,is.na) %>% 
                colSums %>% 
                as.data.frame() %>%
                rownames_to_column() %>% 
                filter(.!=0) %>% 
                select(rowname) %>% 
                as.matrix() %>% 
                apply(1, toString)

Imputed = predict(preProcess(data %>% select(To_impute), method='medianImpute'),
                                data %>% select(To_impute))

# check
sum(is.na(Imputed))==0

# ----------------------------------------------------------------------------------------------------------
# Imputation consistency check
Initial_means=apply(data[,To_impute],2,function(x){return(mean(x,na.rm=TRUE))}) %>% 
              as.data.frame() %>% 
              rownames_to_column() %>% 
              rename(Initial_mean=2) 

Imputation_means=apply(Imputed,2,mean) %>% 
              as.data.frame() %>% 
              rownames_to_column() %>% 
              rename(Imputation_mean=2) 

# check if rows order match, so we can easily compare the differences in means before and after imputation
sum((Imputation_means==Initial_means)[,1])==length(To_impute)

# the largest relative difference is 4.3%, for dividendpaidAndCapexCoverageRatios Variable
# we conclude that imputation worked correctly and did not introduce random errors in our data
# we also checked the tree approach (bagimpute) but it seems like this method introduced 
# significant differences
Consistency_check=data.frame(Initial_means,Imputation_means$Imputation_mean) %>% 
                             rename(Imputation_mean=Imputation_means.Imputation_mean) %>% 
                             mutate(abs_diff=Imputation_mean-Initial_mean,rel_diff=round(abs_diff/Initial_mean,3)) %>% 
                             arrange(-rel_diff)

# so finally we can impute these variables
data[,To_impute]=Imputed
rm(To_impute,Imputed,Imputation_means, Initial_means,Consistency_check)

print(paste("There are ",ncol(data)," variables left in the dataset (including Class)."))

#########################################################################################################
########################################## Feature Engineering ##########################################
#########################################################################################################

# automated feature engineering with dynamic quantile-based splitting
# we split a variable into two or three intervals, in such a way that the
# sum of the three new features is the same as the initial value 
# (more details in the report)
tmp=data.frame(tmp=matrix(nrow=nrow(data),ncol=1))

# this variable informs how many new variables have been created
counter=0
for (i in 2:(ncol(data)-2)) {
  
  # retrieve name of currently edited variable
  current_variable=data %>% select(i) %>% colnames()
  
  k=1
  l=-0.9
  
  # find the quantiles of this variable
  Q=quantile(data[,i],probs=seq(0,1,by=0.01)) %>% 
              as.data.frame() %>% 
              rownames_to_column() %>% 
              rename(Quantile=2) %>% 
              mutate(Abs_Change=Quantile-lag(Quantile),
                     Rel_Change=Abs_Change/lag(Quantile),
                     Right=(Rel_Change>k)*1,
                     Left=(Rel_Change<l)*1,
                     Probs=seq(0,1,by=0.01)) %>% 
              filter(Probs>0.95 | Probs<0.05)

  # ----------------------------------------------------------------------------------------------------------
  # partition the variable if there are both left and right tails
  if (sum(Q$Right!=0,na.rm = TRUE)!=0 & sum(Q$Left!=0,na.rm = TRUE)!=0) {
    counter=counter+2
    # find the treshold for both tails
    Lower=Q$Quantile[which(Q$Left==1)[1]]
    Upper=Q$Quantile[which(Q$Right==1)[1]-1]

    # split the feature into three variables
    output=data %>% 
           select(i) %>% 
           rename(Variable=1) %>% 
           rowwise() %>% 
           mutate(a=min(Variable,Lower),
                  b=min(Upper,max(0,Variable-Lower)),
                  c=max(0,Variable-Upper-Lower)) %>% 
           select(-Variable)
    
    # rename the columns
    colnames(output)=c(paste(current_variable,"Lower",sep="_"),
                       paste(current_variable,"Center",sep="_"),
                       paste(current_variable,"Upper",sep="_"))
    
    # consistency check
    ctrl=cbind(data %>% select(i),output) %>% 
      rename(Variable=1) %>% 
      rowwise %>%
      mutate(check=sum(across(2:4)),
             diff=check-Variable) %>% 
      select(diff) %>% sum() %>% round()

  print(paste("(center) Loop: ",i,
              "Control sum:",ctrl,current_variable))
    
  # ----------------------------------------------------------------------------------------------------------
  # partition the variable only for the case of right tail 
  }else if (sum(Q$Right!=0,na.rm = TRUE)!=0 & sum(Q$Left!=0,na.rm = TRUE)==0) {
    counter=counter+1
    Upper=Q$Quantile[which(Q$Right==1)[1]-1]
    
    # Prepare the modified output
    output=data %>% 
           select(i) %>% 
           rename(Variable=1) %>% 
           rowwise() %>% 
           mutate(a=min(Variable,Upper),
                  b=max(Variable-Upper,0)) %>% 
           select(-Variable)
    
    colnames(output)=c(paste(current_variable,"Center",sep="_"),
                       paste(current_variable,"Upper",sep="_"))
    
  # Consistency check based on the sum of the new variables and the initial value
    ctrl=cbind(data %>% select(i),output) %>% 
                rename(Variable=1) %>% 
                rowwise %>%
                mutate(check=sum(across(2:3)),
                       diff=check-Variable) %>% 
                select(diff) %>% sum() %>% round()
    
    print(paste("(right) Loop: ",i,
                "Control sum:",ctrl,current_variable))
    
  # ----------------------------------------------------------------------------------------------------------
  # partition the variable  only for the left tail
  }else if (sum(Q$Right!=0,na.rm = TRUE)==0 & sum(Q$Left!=0,na.rm = TRUE)!=0) {
    counter=counter+1
    Lower=Q$Quantile[which(Q$Left==1)[1]]
    
    # Prepare the modified output
    output=data %>% 
            select(i) %>% 
            rename(Variable=1) %>% 
            rowwise() %>% 
            mutate(a=min(Variable,Lower),
                   b=max(Variable-Lower,0)) %>% 
            select(-Variable)
    
    colnames(output)=c(paste(current_variable,"Lower",sep="_"),
                       paste(current_variable,"Center",sep="_"))
    
    # Consistency check based on the sum of the new variables and the initial value
    ctrl=cbind(data %>% select(i),output) %>% 
                rename(Variable=1) %>% 
                rowwise %>%
                mutate(check=sum(across(2:3)),
                       diff=check-Variable) %>% 
                select(diff) %>% sum %>% round()
    
    print(paste("(left) Loop: ",i,
                "Control sum:",ctrl,current_variable))
    
  }
  
  tmp=cbind(tmp,output)
}

# ----------------------------------------------------------------------------------------------------------
# Second approach to splitting - transform the _Upper and _Lower variables from data_FE to binary.
# We use the object tmp generated in the first Feature Engineering process and find
# _Upper and _Lower partial names to apply appropriate transformations.
# We leave the _Center variables unchanged
tmp2=data.frame(tmp=matrix(nrow=nrow(data),ncol=1))
names=colnames(tmp)
for (i in 2:ncol(tmp)){
  
  # transform _Upper to binary 
  if (grepl("_Upper", names[i], fixed = TRUE)){
    tmp2[,i]=(tmp[,i]!=0)*1
    colnames(tmp2)[i]=names[i]
  # transform _Lower to binary
  }else if(grepl( "_Lower", names[i], fixed = TRUE)){
    tmp2[,i]=(tmp[,i]!=max(tmp[,i]))*1
    colnames(tmp2)[i]=names[i]
  }else if(grepl( "_Center", names[i], fixed = TRUE)){
    tmp2[,i]=tmp[,i]
    colnames(tmp2)[i]=names[i]
  } 

}

#########################################################################################################
########################################### Dataset Processing ##########################################
#########################################################################################################
# create new dataset with engineered features FE and FE2
data_FE=cbind(data %>% select(Class,X,Sector),tmp[,-1])
data_FE2=cbind(data %>% select(Class,X,Sector),tmp2[,-1])

# one hot encoding of Sector variable
dummy = dummyVars(" ~ Sector", data=data)
newdata = data.frame(predict(dummy, newdata = data)) 

# include the one hot encoded variables in both datasets (basic and with engineered features FE and FE2)
data=cbind(data,newdata) %>% select(-Sector)
data_FE=cbind(data_FE,newdata) %>% select(-Sector)
data_FE2=cbind(data_FE2,newdata) %>% select(-Sector)

# Clean new variables that have zero variance in the set FE
Zero_variance=apply(data_FE %>% select(-Class,-X) ,2,sd) %>% 
                    as.data.frame() %>% 
                    rownames_to_column() %>% 
                    rename(SD=2) %>% 
                    filter(SD<1 & SD>=0 | is.nan(SD)) %>%
                    arrange(SD) %>% 
                    select(rowname) %>% apply(1,toString)

data_FE=data_FE %>% select(-all_of(Zero_variance))

# Clean new variables that have small coef of variance in the set FE2
Zero_variance=apply(data_FE2 %>% select(-Class,-X) ,2,function(x){return(var(x,na.rm = TRUE)/mean(x,na.rm = TRUE))}) %>% 
                    as.data.frame() %>% 
                    rownames_to_column() %>% 
                    rename(SD=2) %>% 
                    arrange(SD) %>% 
                    filter(SD<1 & SD>=0 | is.nan(SD)) %>%
                    select(rowname) %>% apply(1,toString)

data_FE2=data_FE2 %>% select(-all_of(Zero_variance))

# ----------------------------------------------------------------------------------------------------------
# Delete highly correlated of the new features
Highly_correlated=cor(data_FE %>% select(-Class,-X)) %>% 
                        tril() %>% 
                        as.matrix() %>% 
                        as.data.frame() %>%
                        gather(row,value) %>% 
                        mutate(column=rep(colnames(data_FE %>% select(-X,-Class)),length(colnames(data_FE))-2)) %>% 
                        mutate(index=(row!=column)*1) %>% 
                        filter(index==1) %>% 
                        arrange(-value) %>% 
                        filter(value>Treshold_to_drop_cor) %>% 
                        select(column) %>% 
                        unique() %>%
                        apply(1, toString) 

data_FE=data_FE %>% select(-all_of(Highly_correlated))

Highly_correlated=cor(data_FE2 %>% select(-Class,-X)) %>% 
                      tril() %>% 
                      as.matrix() %>% 
                      as.data.frame() %>%
                      gather(row,value) %>% 
                      mutate(column=rep(colnames(data_FE2 %>% select(-X,-Class)),length(colnames(data_FE2))-2)) %>% 
                      mutate(index=(row!=column)*1) %>% 
                      filter(index==1) %>% 
                      arrange(-value) %>% 
                      filter(value>Treshold_to_drop_cor) %>% 
                      select(column) %>% 
                      unique() %>%
                      apply(1, toString) 

data_FE2=data_FE2 %>% select(-all_of(Highly_correlated))


# ----------------------------------------------------------------------------------------------------------
# Normalize the data 
Normalized = data.frame(predict(preProcess(data %>% select(-X,-Class),method = c("range")), 
                                newdata = data)) 

Normalized_FE = data.frame(predict(preProcess(data_FE %>% select(-X,-Class),method = c("range")), 
                                newdata = data_FE)) 

Normalized_FE2 = data.frame(predict(preProcess(data_FE2 %>% select(-X,-Class),method = c("range")), 
                                   newdata = data_FE2)) 

print(paste("There are ",ncol(data)," variables, with",counter,"introduced through feature engineering."))

rm(Zero_variance,newdata,dummy,Highly_correlated,Q,output,tmp)

# change of data type of Class variable for modeling purposes
data$Class=factor(data$Class)
Normalized$Class=factor(Normalized$Class)
Normalized_FE$Class=factor(Normalized_FE$Class)
Normalized_FE2$Class=factor(Normalized_FE2$Class)

#########################################################################################################
############################################## Train-Test Split  ########################################
#########################################################################################################

# we select the same instances in all three cases of the prepared 
# datasets - cleaned,  normalized and normalized with feature engineering in versions 1 and 2

# row selection for train-test split
SelectRows=createDataPartition(Normalized$Class, p = .75, list = FALSE)

# basic cleaned dataset
train=data[SelectRows,]
test=data[-SelectRows,]

# Normalized clean dataset
train_Normalized=Normalized[SelectRows,]
test_Normalized=Normalized[-SelectRows,]

# Normalized clean dataset with Feature Engineering
train_Normalized_FE=Normalized_FE[SelectRows,]
test_Normalized_FE=Normalized_FE[-SelectRows,]

# Normalized clean dataset with Feature Engineering v2
train_Normalized_FE2=Normalized_FE2[SelectRows,]
test_Normalized_FE2=Normalized_FE2[-SelectRows,]

rm(SelectRows)

#########################################################################################################
###################################### Dimensionality Reduction - PCA  ##################################
#########################################################################################################
# ----------------------------------------------------------------------------------------------------------
# apply the PCA to each dataset and keep 40 first components (42 including X and Class)
Dimensions_to_leave=1:42

# basic cleaned dataset
Rotation=prcomp(train %>% select(-X,-Class) %>% as.matrix())$rotation

train_DimRed=cbind(train %>% select(X,Class),
                   (train %>% select(-X,-Class) %>% as.matrix()) %*% Rotation)[,Dimensions_to_leave]

test_DimRed=cbind(test %>% select(X,Class),
           (test %>% select(-X,-Class) %>% as.matrix()) %*% prcomp(train %>% select(-X,-Class) %>% as.matrix())$rotation)[,Dimensions_to_leave]

# Normalized clean dataset
Rotation=prcomp(train_Normalized %>% select(-X,-Class) %>% as.matrix())$rotation

train_Normalized_DimRed=cbind(train_Normalized %>% 
                                select(X,Class),
                              (train_Normalized %>% 
                                 select(-X,-Class) %>% 
                                 as.matrix()) %*% Rotation)[,Dimensions_to_leave]

test_Normalized_DimRed=cbind(test_Normalized %>% 
                               select(X,Class),
                             (test_Normalized %>% 
                                select(-X,-Class) %>% 
                                as.matrix()) %*% Rotation)[,Dimensions_to_leave]

# Normalized clean dataset with Feature Engineering
Rotation=prcomp(train_Normalized_FE %>% select(-X,-Class) %>% as.matrix())$rotation

train_Normalized_FE_DimRed=cbind(train_Normalized_FE %>% 
                                   select(X,Class),
                                (train_Normalized_FE %>% 
                                   select(-X,-Class) %>% 
                                   as.matrix()) %*% Rotation)[,Dimensions_to_leave]

test_Normalized_FE_DimRed=cbind(test_Normalized_FE %>% 
                                  select(X,Class),
                               (test_Normalized_FE %>% 
                                  select(-X,-Class) %>% 
                                  as.matrix()) %*% Rotation)[,Dimensions_to_leave]

# Normalized clean dataset with Feature Engineering 2
Rotation=prcomp(train_Normalized_FE2 %>% select(-X,-Class) %>% as.matrix())$rotation

train_Normalized_FE2_DimRed=cbind(train_Normalized_FE2 %>% 
                                   select(X,Class),
                                 (train_Normalized_FE2 %>% 
                                    select(-X,-Class) %>% 
                                    as.matrix()) %*% Rotation)[,Dimensions_to_leave]

test_Normalized_FE2_DimRed=cbind(test_Normalized_FE2 %>% 
                                  select(X,Class),
                                (test_Normalized_FE2 %>% 
                                   select(-X,-Class) %>% 
                                   as.matrix()) %*% Rotation)[,Dimensions_to_leave]

rm(Rotation)

#########################################################################################################
############################################### Resampling ##############################################
#########################################################################################################
# we noticed that the dataset is imbalanced, so we up-sample the Class=0
percentage=0.4
Resample_selection=sample(which(train$Class==0),size = round(percentage*length(which(train$Class==0))))

# apply the resampled instances to the training sets both pre and after PCA
train=rbind(train,train[Resample_selection,])
train_Normalized=rbind(train_Normalized,train_Normalized[Resample_selection,])
train_Normalized_FE=rbind(train_Normalized_FE,train_Normalized_FE[Resample_selection,])
train_Normalized_FE2=rbind(train_Normalized_FE2,train_Normalized_FE2[Resample_selection,])

train_DimRed=rbind(train_DimRed,train_DimRed[Resample_selection,])
train_Normalized_DimRed=rbind(train_Normalized_DimRed,train_Normalized_DimRed[Resample_selection,])
train_Normalized_FE_DimRed=rbind(train_Normalized_FE_DimRed,train_Normalized_FE_DimRed[Resample_selection,])
train_Normalized_FE2_DimRed=rbind(train_Normalized_FE2_DimRed,train_Normalized_FE2_DimRed[Resample_selection,])


#########################################################################################################
########################################## Modeling w/o dim red #########################################
#########################################################################################################

# Prepare an object to holdresults
Results=data.frame(Basic=rep(NA,5),
                   Normalized=rep(NA,5),
                   Normalized_FE=rep(NA,5),
                   Normalized_FE2=rep(NA,5),
                   Basic_PCA=rep(NA,5),
                   Normalized_PCA=rep(NA,5),
                   Normalized_FE_PCA=rep(NA,5),
                   Normalized_FE2_PCA=rep(NA,5))

row.names(Results)=c("Logistic_Regression",
                     "SVM_Radial",
                     "Decision_Tree",
                     "Random_Forest",
                     "Neural_Network")

# Create a matrix to hold specificty values
Sensitivity=Results

# cross validation object for caret train function
CV=trainControl(method="repeatedcv",repeats = 2, number = 5)

# ----------------------------------------------------------------------------------------------------------
# Logistic Regression
LogReg_basic=caret::train(form=Class~.,
             data=train %>% select(-X),
             method = "glm",
             family = "binomial",
             trControl=CV)

LogReg_Normalized=caret::train(form=Class~.,
             data=train_Normalized %>% select(-X),
             method = "glm",
             family = "binomial",
             trControl=CV)

LogReg_Normalized_FE=caret::train(form=Class~.,
                        data=train_Normalized_FE %>% select(-X),
                        method = "glm",
                        family = "binomial",
                        trControl=CV)

LogReg_Normalized_FE2=caret::train(form=Class~.,
                                  data=train_Normalized_FE2 %>% select(-X),
                                  method = "glm",
                                  family = "binomial",
                                  trControl=CV)

Results[1,1:4]=c(confusionMatrix(predict(object=LogReg_basic,newdata = test),test$Class)$overall[1],
                 confusionMatrix(predict(object=LogReg_Normalized,newdata = test_Normalized),test_Normalized$Class)$overall[1],
                 confusionMatrix(predict(object=LogReg_Normalized_FE,newdata = test_Normalized_FE),test_Normalized_FE$Class)$overall[1],
                 confusionMatrix(predict(object=LogReg_Normalized_FE2,newdata = test_Normalized_FE2),test_Normalized_FE2$Class)$overall[1])

Sensitivity[1,1:4]=c(confusionMatrix(predict(object=LogReg_basic,newdata = test),test$Class)$byClass[1],
                 confusionMatrix(predict(object=LogReg_Normalized,newdata = test_Normalized),test_Normalized$Class)$byClass[1],
                 confusionMatrix(predict(object=LogReg_Normalized_FE,newdata = test_Normalized_FE),test_Normalized_FE$Class)$byClass[1],
                 confusionMatrix(predict(object=LogReg_Normalized_FE2,newdata = test_Normalized_FE2),test_Normalized_FE2$Class)$byClass[1])
Results
# ----------------------------------------------------------------------------------------------------------
# Radial kernel SVM 
SVM_basic=caret::train(Class~.,
          data=train %>% select(-X),
          method="svmRadial",
          trControl=CV)

SVM_Normalized=caret::train(Class~.,
          data=train_Normalized %>% select(-X),
          method="svmRadial",
          trControl=CV)

SVM_Normalized_FE=caret::train(Class~.,
          data=train_Normalized_FE %>% select(-X),
          method="svmRadial",
          trControl=CV)

SVM_Normalized_FE2=caret::train(Class~.,
                               data=train_Normalized_FE2 %>% select(-X),
                               method="svmRadial",
                               trControl=CV)

Results[2,1:4]=c(confusionMatrix(predict(object=SVM_basic,newdata = test),test$Class)$overall[1],
                 confusionMatrix(predict(object=SVM_Normalized,newdata = test_Normalized),test_Normalized$Class)$overall[1],
                 confusionMatrix(predict(object=SVM_Normalized_FE,newdata = test_Normalized_FE),test_Normalized_FE$Class)$overall[1],
                 confusionMatrix(predict(object=SVM_Normalized_FE2,newdata = test_Normalized_FE2),test_Normalized_FE2$Class)$overall[1])

Sensitivity[2,1:4]=c(confusionMatrix(predict(object=SVM_basic,newdata = test),test$Class)$byClass[1],
                 confusionMatrix(predict(object=SVM_Normalized,newdata = test_Normalized),test_Normalized$Class)$byClass[1],
                 confusionMatrix(predict(object=SVM_Normalized_FE,newdata = test_Normalized_FE),test_Normalized_FE$Class)$byClass[1],
                 confusionMatrix(predict(object=SVM_Normalized_FE2,newdata = test_Normalized_FE2),test_Normalized_FE2$Class)$byClass[1])

Results

# ----------------------------------------------------------------------------------------------------------
# Decision Tree 
DecTree_basic=caret::train(Class~.,
              data=train %>% select(-X),
              method="rpart",
              trControl=CV)

DecTree_Normalized=caret::train(Class~.,
                         data=train_Normalized %>% select(-X),
                         method="rpart",
                         trControl=CV)

DecTree_Normalized_FE=caret::train(Class~.,
                            data=train_Normalized_FE %>% select(-X),
                            method="rpart",
                            trControl=CV)

DecTree_Normalized_FE2=caret::train(Class~.,
                                   data=train_Normalized_FE2 %>% select(-X),
                                   method="rpart",
                                   trControl=CV)

Results[3,1:4]=c(confusionMatrix(predict(object=DecTree_basic,newdata = test),test$Class)$overall[1],
                 confusionMatrix(predict(object=DecTree_Normalized,newdata = test_Normalized),test_Normalized$Class)$overall[1],
                 confusionMatrix(predict(object=DecTree_Normalized_FE,newdata = test_Normalized_FE),test_Normalized_FE$Class)$overall[1],
                 confusionMatrix(predict(object=DecTree_Normalized_FE2,newdata = test_Normalized_FE2),test_Normalized_FE2$Class)$overall[1])

Sensitivity[3,1:4]=c(confusionMatrix(predict(object=DecTree_basic,newdata = test),test$Class)$byClass[1],
                 confusionMatrix(predict(object=DecTree_Normalized,newdata = test_Normalized),test_Normalized$Class)$byClass[1],
                 confusionMatrix(predict(object=DecTree_Normalized_FE,newdata = test_Normalized_FE),test_Normalized_FE$Class)$byClass[1],
                 confusionMatrix(predict(object=DecTree_Normalized_FE2,newdata = test_Normalized_FE2),test_Normalized_FE2$Class)$byClass[1])

Results
# ----------------------------------------------------------------------------------------------------------
# Random Forest
RanFor_basic=caret::train(Class~.,
             data=train %>% select(-X),
             method="rf",
             trControl=CV)

RanFor_Normalized=caret::train(Class~.,
                        data=train_Normalized %>% select(-X),
                        method="rf",
                        trControl=CV)

RanFor_Normalized_FE=caret::train(Class~.,
                           data=train_Normalized_FE %>% select(-X),
                           method="rf",
                           trControl=CV)

RanFor_Normalized_FE2=caret::train(Class~.,
                                  data=train_Normalized_FE2 %>% select(-X),
                                  method="rf",
                                  trControl=CV)

Results[4,1:4]=c(confusionMatrix(predict(object=RanFor_basic,newdata = test),test$Class)$overall[1],
                 confusionMatrix(predict(object=RanFor_Normalized,newdata = test_Normalized),test_Normalized$Class)$overall[1],
                 confusionMatrix(predict(object=RanFor_Normalized_FE,newdata = test_Normalized_FE),test_Normalized_FE$Class)$overall[1],
                 confusionMatrix(predict(object=RanFor_Normalized_FE2,newdata = test_Normalized_FE2),test_Normalized_FE2$Class)$overall[1])

Sensitivity[4,1:4]=c(confusionMatrix(predict(object=RanFor_basic,newdata = test),test$Class)$byClass[1],
                 confusionMatrix(predict(object=RanFor_Normalized,newdata = test_Normalized),test_Normalized$Class)$byClass[1],
                 confusionMatrix(predict(object=RanFor_Normalized_FE,newdata = test_Normalized_FE),test_Normalized_FE$Class)$byClass[1],
                 confusionMatrix(predict(object=RanFor_Normalized_FE2,newdata = test_Normalized_FE2),test_Normalized_FE2$Class)$byClass[1])

Results
# ----------------------------------------------------------------------------------------------------------
# Vanilla Feed Forward Neural Network
NN_basic=caret::train(Class~.,
               data=train %>% select(-X),
               method="nnet",
               trControl=CV)

NN_Normalized=caret::train(Class~.,
                    data=train_Normalized %>% select(-X),
                    method="nnet",
                    trControl=CV)

NN_Normalized_FE=caret::train(Class~.,
                    data=train_Normalized_FE %>% select(-X),
                    method="nnet",
                    trControl=CV)

NN_Normalized_FE2=caret::train(Class~.,
                              data=train_Normalized_FE2 %>% select(-X),
                              method="nnet",
                              trControl=CV)

Results[5,1:4]=c(confusionMatrix(predict(object=NN_basic,newdata = test),test$Class)$overall[1],
                 confusionMatrix(predict(object=NN_Normalized,newdata = test_Normalized),test_Normalized$Class)$overall[1],
                 confusionMatrix(predict(object=NN_Normalized_FE,newdata = test_Normalized_FE),test_Normalized_FE$Class)$overall[1],
                 confusionMatrix(predict(object=NN_Normalized_FE2,newdata = test_Normalized_FE2),test_Normalized_FE2$Class)$overall[1])

Sensitivity[5,1:4]=c(confusionMatrix(predict(object=NN_basic,newdata = test),test$Class)$byClass[1],
                 confusionMatrix(predict(object=NN_Normalized,newdata = test_Normalized),test_Normalized$Class)$byClass[1],
                 confusionMatrix(predict(object=NN_Normalized_FE,newdata = test_Normalized_FE),test_Normalized_FE$Class)$byClass[1],
                 confusionMatrix(predict(object=NN_Normalized_FE2,newdata = test_Normalized_FE2),test_Normalized_FE2$Class)$byClass[1])


Results
#########################################################################################################
########################################### Modeling w/ dim red #########################################
#########################################################################################################


# Logistic Regression 
LogReg_basic_PCA=caret::train(form=Class~.,
                   data=train_DimRed %>% select(-X),
                   method = "glm",
                   family = "binomial",
                   trControl=CV)

LogReg_Normalized_PCA=caret::train(form=Class~.,
                        data=train_Normalized_DimRed %>% select(-X),
                        method = "glm",
                        family = "binomial",
                        trControl=CV)

LogReg_Normalized_FE_PCA=caret::train(form=Class~.,
                                       data=train_Normalized_FE_DimRed %>% select(-X),
                                       method = "glm",
                                       family = "binomial",
                                       trControl=CV)

LogReg_Normalized_FE2_PCA=caret::train(form=Class~.,
                           data=train_Normalized_FE2_DimRed %>% select(-X),
                           method = "glm",
                           family = "binomial",
                           trControl=CV)

Results[1,5:8]=c(confusionMatrix(predict(object=LogReg_basic_PCA,newdata = test_DimRed),test_DimRed$Class)$overall[1],
                 confusionMatrix(predict(object=LogReg_Normalized_PCA,newdata = test_Normalized_DimRed),test_Normalized_DimRed$Class)$overall[1],
                 confusionMatrix(predict(object=LogReg_Normalized_FE_PCA,newdata = test_Normalized_FE_DimRed),test_Normalized_FE_DimRed$Class)$overall[1],
                 confusionMatrix(predict(object=LogReg_Normalized_FE2_PCA,newdata = test_Normalized_FE2_DimRed),test_Normalized_FE2_DimRed$Class)$overall[1])

Sensitivity[1,5:8]=c(confusionMatrix(predict(object=LogReg_basic_PCA,newdata = test_DimRed),test_DimRed$Class)$byClass[1],
                 confusionMatrix(predict(object=LogReg_Normalized_PCA,newdata = test_Normalized_DimRed),test_Normalized_DimRed$Class)$byClass[1],
                 confusionMatrix(predict(object=LogReg_Normalized_FE_PCA,newdata = test_Normalized_FE_DimRed),test_Normalized_FE_DimRed$Class)$byClass[1],
                 confusionMatrix(predict(object=LogReg_Normalized_FE2_PCA,newdata = test_Normalized_FE2_DimRed),test_Normalized_FE2_DimRed$Class)$byClass[1])


Results
# ----------------------------------------------------------------------------------------------------------
# Radial kernel SVM 
SVM_basic_PCA=caret::train(Class~.,
                data=train_DimRed %>% select(-X),
                method="svmRadial",
                trControl=CV)

SVM_Normalized_PCA=caret::train(Class~.,
                     data=train_Normalized_DimRed %>% select(-X),
                     method="svmRadial",
                     trControl=CV)

SVM_Normalized_FE_PCA=caret::train(Class~.,
                        data=train_Normalized_FE_DimRed %>% select(-X),
                        method="svmRadial",
                        trControl=CV)

SVM_Normalized_FE2_PCA=caret::train(Class~.,
                                   data=train_Normalized_FE2_DimRed %>% select(-X),
                                   method="svmRadial",
                                   trControl=CV)

Results[2,5:8]=c(confusionMatrix(predict(object=SVM_basic_PCA,newdata = test_DimRed),test_DimRed$Class)$overall[1],
                 confusionMatrix(predict(object=SVM_Normalized_PCA,newdata = test_Normalized_DimRed),test_Normalized_DimRed$Class)$overall[1],
                 confusionMatrix(predict(object=SVM_Normalized_FE_PCA,newdata = test_Normalized_FE_DimRed),test_Normalized_FE_DimRed$Class)$overall[1],
                 confusionMatrix(predict(object=SVM_Normalized_FE2_PCA,newdata = test_Normalized_FE2_DimRed),test_Normalized_FE2_DimRed$Class)$overall[1])

Sensitivity[2,5:8]=c(confusionMatrix(predict(object=SVM_basic_PCA,newdata = test_DimRed),test_DimRed$Class)$byClass[1],
                 confusionMatrix(predict(object=SVM_Normalized_PCA,newdata = test_Normalized_DimRed),test_Normalized_DimRed$Class)$byClass[1],
                 confusionMatrix(predict(object=SVM_Normalized_FE_PCA,newdata = test_Normalized_FE_DimRed),test_Normalized_FE_DimRed$Class)$byClass[1],
                 confusionMatrix(predict(object=SVM_Normalized_FE2_PCA,newdata = test_Normalized_FE2_DimRed),test_Normalized_FE2_DimRed$Class)$byClass[1])

Results

# ----------------------------------------------------------------------------------------------------------
# Decision Tree 
DecTree_basic_PCA=caret::train(Class~.,
                    data=train_DimRed %>% select(-X),
                    method="rpart",
                    trControl=CV)

DecTree_Normalized_PCA=caret::train(Class~.,
                         data=train_Normalized_DimRed %>% select(-X),
                         method="rpart",
                         trControl=CV)

DecTree_Normalized_FE_PCA=caret::train(Class~.,
                            data=train_Normalized_FE_DimRed %>% select(-X),
                            method="rpart",
                            trControl=CV)

DecTree_Normalized_FE2_PCA=caret::train(Class~.,
                                       data=train_Normalized_FE2_DimRed %>% select(-X),
                                       method="rpart",
                                       trControl=CV)

Results[3,5:8]=c(confusionMatrix(predict(object=DecTree_basic_PCA,newdata = test_DimRed),test_DimRed$Class)$overall[1],
                 confusionMatrix(predict(object=DecTree_Normalized_PCA,newdata = test_Normalized_DimRed),test_Normalized_DimRed$Class)$overall[1],
                 confusionMatrix(predict(object=DecTree_Normalized_FE_PCA,newdata = test_Normalized_FE_DimRed),test_Normalized_FE_DimRed$Class)$overall[1],
                 confusionMatrix(predict(object=DecTree_Normalized_FE2_PCA,newdata = test_Normalized_FE2_DimRed),test_Normalized_FE2_DimRed$Class)$overall[1])

Sensitivity[3,5:8]=c(confusionMatrix(predict(object=DecTree_basic_PCA,newdata = test_DimRed),test_DimRed$Class)$byClass[1],
                 confusionMatrix(predict(object=DecTree_Normalized_PCA,newdata = test_Normalized_DimRed),test_Normalized_DimRed$Class)$byClass[1],
                 confusionMatrix(predict(object=DecTree_Normalized_FE_PCA,newdata = test_Normalized_FE_DimRed),test_Normalized_FE_DimRed$Class)$byClass[1],
                 confusionMatrix(predict(object=DecTree_Normalized_FE2_PCA,newdata = test_Normalized_FE2_DimRed),test_Normalized_FE2_DimRed$Class)$byClass[1])

Results
# ----------------------------------------------------------------------------------------------------------
# Random Forest
RanFor_basic_PCA=caret::train(Class~.,
             data=train_DimRed %>% select(-X),
             method="rf",
             trControl=CV)

RanFor_Normalized_PCA=caret::train(Class~.,
                        data=train_Normalized_DimRed %>% select(-X),
                        method="rf",
                        trControl=CV)

RanFor_Normalized_FE_PCA=caret::train(Class~.,
                           data=train_Normalized_FE_DimRed %>% select(-X),
                           method="rf",
                           trControl=CV)

RanFor_Normalized_FE2_PCA=caret::train(Class~.,
                                      data=train_Normalized_FE2_DimRed %>% select(-X),
                                      method="rf",
                                      trControl=CV)

Results[4,5:8]=c(confusionMatrix(predict(object=RanFor_basic_PCA,newdata = test_DimRed),test_DimRed$Class)$overall[1],
                 confusionMatrix(predict(object=RanFor_Normalized_PCA,newdata = test_Normalized_DimRed),test_Normalized_DimRed$Class)$overall[1],
                 confusionMatrix(predict(object=RanFor_Normalized_FE_PCA,newdata = test_Normalized_FE_DimRed),test_Normalized_FE_DimRed$Class)$overall[1],
                 confusionMatrix(predict(object=RanFor_Normalized_FE2_PCA,newdata = test_Normalized_FE2_DimRed),test_Normalized_FE2_DimRed$Class)$overall[1])

Sensitivity[4,5:8]=c(confusionMatrix(predict(object=RanFor_basic_PCA,newdata = test_DimRed),test_DimRed$Class)$byClass[1],
                 confusionMatrix(predict(object=RanFor_Normalized_PCA,newdata = test_Normalized_DimRed),test_Normalized_DimRed$Class)$byClass[1],
                 confusionMatrix(predict(object=RanFor_Normalized_FE_PCA,newdata = test_Normalized_FE_DimRed),test_Normalized_FE_DimRed$Class)$byClass[1],
                 confusionMatrix(predict(object=RanFor_Normalized_FE2_PCA,newdata = test_Normalized_FE2_DimRed),test_Normalized_FE2_DimRed$Class)$byClass[1])

Results
# ----------------------------------------------------------------------------------------------------------
# Vanilla Feed Forward Neural Network
NN_basic_PCA=caret::train(Class~.,
               data=train_DimRed %>% select(-X),
               method="nnet",
               trControl=CV)

NN_Normalized_PCA=caret::train(Class~.,
                    data=train_Normalized_DimRed %>% select(-X),
                    method="nnet",
                    trControl=CV)

NN_Normalized_FE_PCA=caret::train(Class~.,
                       data=train_Normalized_FE_DimRed %>% select(-X),
                       method="nnet",
                       trControl=CV)

NN_Normalized_FE2_PCA=caret::train(Class~.,
                                  data=train_Normalized_FE2_DimRed %>% select(-X),
                                  method="nnet",
                                  trControl=CV)

Results[5,5:8]=c(confusionMatrix(predict(object=NN_basic_PCA,newdata = test_DimRed),test_DimRed$Class)$overall[1],
                 confusionMatrix(predict(object=NN_Normalized_PCA,newdata = test_Normalized_DimRed),test_Normalized_DimRed$Class)$overall[1],
                 confusionMatrix(predict(object=NN_Normalized_FE_PCA,newdata = test_Normalized_FE_DimRed),test_Normalized_FE_DimRed$Class)$overall[1],
                 confusionMatrix(predict(object=NN_Normalized_FE2_PCA,newdata = test_Normalized_FE2_DimRed),test_Normalized_FE2_DimRed$Class)$overall[1])


Sensitivity[5,5:8]=c(confusionMatrix(predict(object=NN_basic_PCA,newdata = test_DimRed),test_DimRed$Class)$byClass[1],
                 confusionMatrix(predict(object=NN_Normalized_PCA,newdata = test_Normalized_DimRed),test_Normalized_DimRed$Class)$byClass[1],
                 confusionMatrix(predict(object=NN_Normalized_FE_PCA,newdata = test_Normalized_FE_DimRed),test_Normalized_FE_DimRed$Class)$byClass[1],
                 confusionMatrix(predict(object=NN_Normalized_FE2_PCA,newdata = test_Normalized_FE2_DimRed),test_Normalized_FE2_DimRed$Class)$byClass[1])

Results
#########################################################################################################
#################################### Model Assessment and comparison ####################################
#########################################################################################################
# variable importance plot for the classifier with the highest accuracy
varImp(RanFor_Normalized_FE)$importance %>% 
  as.data.frame() %>%  
  rownames_to_column() %>% 
  arrange(Overall) %>% 
  filter(Overall>35) %>% 
  ggplot(aes(y=reorder(rowname,Overall),x=Overall))+
  geom_col()+
  xlab("")+
  ylab("")+
  ggtitle("Variable Importance plot - Random Forest",
          subtitle = "Normalized data with Feature Engineering")

# variable importance plot for the classifier with the lowest false positive rate
varImp(NN_Normalized)$importance %>% 
  as.data.frame() %>%  
  rownames_to_column() %>% 
  arrange(Overall) %>% 
  filter(Overall>60) %>% 
  ggplot(aes(y=reorder(rowname,Overall),x=Overall))+
  geom_col()+
  xlab("")+
  ylab("")+
  ggtitle("Variable Importance plot - Neural Network",
          subtitle = "Normalized data")
  


# generate a ROC plot
LogReg_basic_pred <-  predict(LogReg_Normalized_FE2, test_Normalized_FE2 %>% select(-X), type="prob")
LogReg_basic_roc <- roc(test_Normalized_FE2$Class, LogReg_basic_pred[,1])


RanFor_basic_pred <-  predict(RanFor_Normalized_FE2$finalModel, test_Normalized_FE2 %>% select(-X), type="prob")
RanFor_basic_roc <- roc(test_Normalized_FE2$Class, RanFor_basic_pred[,1])


NN_basic_pred <-  predict(NN_Normalized_FE2$finalModel, test_Normalized_FE2 %>% select(-X), type="raw")
NN_basic_roc <- roc(test_Normalized_FE2$Class, NN_basic_pred[,1])


plot(LogReg_basic_roc,main="Normalized_FE2 - ROC Curves for Test Set")
lines(RanFor_basic_roc, col="red")
lines(NN_basic_roc, col="purple")
legend("bottomright", legend=c("Logistic Regression", "Random Forest", "Neural Network"),
       col=c("black", "red", "purple"), lty=1:2, cex=1)

