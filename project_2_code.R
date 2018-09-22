---
title: "Project"
author: "An-Chi Ho (ah3508)"
date: "`r Sys.Date()`"
output:
  rmdformats::html_docco:
    highlight: kate
    toc: true
    toc_depth: 3
---

```{r knitr_init, echo=FALSE, cache=FALSE}
# DO NOT edit this block
knitr::opts_chunk$set(
  cache=TRUE,
  comment=NA,
  message=FALSE,
  warning=FALSE,
  fig.width=15,
  fig.height=10
)
```


Package Installation:
```{r}
if(!require(pacman)) install.packages('pacman')
pacman::p_load(dplyr, ggplot2, mapproj, readr, ggthemes, viridis, reshape2,BBmisc, car, broom)

```


##Q1

Read in the data:
```{r}
#after project_2a_read_in_data.Rmd
full_data_5 <- read_csv('full_data_2a.csv')
full_data_5 <- full_data_5[,-1]

full_data_5 <- full_data_5%>%
  select(index, year, total_amount, yr_mean_AQI, yr_mean_AQI25)

```

Check distribution:
```{r}
full_data_5%>%
  melt(id.vars=c('index', 'year','total_amount'), variable.name='var', value.name='value')%>%
  ggplot(aes(value,..density..))+
  geom_histogram(na.rm = TRUE, binwidth=1)+
  facet_wrap('var')
  ggsave("2a_1.png", plot=last_plot())

hist(full_data_5$total_amount, main="Histogram of investment", xlab = "investment amount")
hist(log(full_data_5$total_amount), main="Histogram of log(investment)", xlab = "log of investment amount")
 
```


#===============================#

(This is the most likely reasonable one for the question we wanna answer.)
Lag correlation: 
money_(t) vs. AQI_(t-1). Heavy pollution last year leads to the decision of more investments this year.
```{r}
full_data_5_lag <- full_data_5%>%
  group_by(index)%>%
  mutate(yr_mean_AQI = lag(yr_mean_AQI,k=1))%>%
    mutate(yr_mean_AQI25 = lag(yr_mean_AQI25,k=1))

```

```{r}
cor_lag_money <- matrix(data = NA, nrow = 2, ncol = 2)
rownames(cor_lag_money) <- c("AQI10", "AQI25")
colnames(cor_lag_money) <- c("Pearson", "Kendall")

for (i in 1:2){
  cor_lag_money[i,1] <- cor(full_data_5_lag$total_amount,full_data_5_lag[,i+3],use = "na.or.complete",
                            method = c("pearson"))
  cor_lag_money[i,2] <- cor(full_data_5_lag$total_amount,full_data_5_lag[,i+3],use = "na.or.complete",
                            method = c("kendall"))
}
#pearson,"kendall", "spearman"

#transform investment into log
cor_lag_money_log <- matrix(data = NA, nrow = 2, ncol = 2)
rownames(cor_lag_money_log) <- c("AQI10", "AQI25")
colnames(cor_lag_money_log) <- c("Pearson", "Kendall")

for (i in 1:2){
  cor_lag_money_log[i,1] <- cor(log(full_data_5_lag$total_amount),full_data_5_lag[,i+3],use = "na.or.complete",method = c("pearson"))
  cor_lag_money_log[i,2] <- cor(log(full_data_5_lag$total_amount),full_data_5_lag[,i+3],use = "na.or.complete",  method = c("kendall"))
}
#pearson,"kendall", "spearman"

write.csv(cor_lag_money_log, file = "table_2a_2.csv")

```
Same bad correlation.


#=====================#

scatter plot:
```{r}

ggplot(data = full_data_5_lag, aes(x = yr_mean_AQI, y = log(total_amount)))+
  geom_point(size = 3,alpha = 0.2, col = "black", na.rm = TRUE)+
  geom_smooth()+
  ggtitle("log(investment) vs. AQI10")+
  ylab("log of investment")+
  xlab("AQI10")+
  xlim(0,70)

ggplot(data = full_data_5_lag, aes(x = yr_mean_AQI25, y = log(total_amount)))+
  geom_point(size = 3, alpha = 0.2, col = "black", na.rm = TRUE)+
  geom_smooth()+
  ggtitle("log(investment) vs. AQI2.5")+
  ylab("log of investment")+
  xlab("AQI2.5")+
  xlim(0,70)

```
There is a trend!

#=========================#

Multiple Linear Regression:
money~AQI10+AQI25
```{r}

lm1 <- lm(log(total_amount) ~ yr_mean_AQI + yr_mean_AQI25, data = full_data_5_lag, na.action=na.omit)
summary(lm1)
tidy(summary(lm1))
write.csv(tidy(summary(lm1)), file = "table_2a_1.csv")

par(mfrow = c(2, 2)) # set up 2 by 2 plot
plot(lm1) # built-in diagnostic plots
par(mfrow = c(1, 1)) # go back to 1 by 1 plots (default)  

#Is residual normal distribution?
plot(density(lm1$residuals))
boxplot(lm1$residuals, main = "Residual boxplot")

vif(lm1)   #pm10 and pm25 are independent

```

R^2 is extremely low (which means large bias) but pm25 passes significant test. Residual plots seem to have no problem.

#========================#

##Q2

Read in the data:
```{r}
#after project_2a_read_in_data.Rmd
full_data_5 <- read_csv('full_data_2a.csv')
full_data_5 <- full_data_5[,-1]

full_data_5 <- full_data_5%>%
  select(index, year, total_amount, yr_mean_AQI, yr_mean_AQI25)
```

CA, index of county with complete pm10 and pm25 data, and at least one year investment:
190: 2011 max
193: 2009 start
195: 2010 max
196: 2011 start
198: 2009 start
199: 2011 start
202: 2012 start (small amount)
206: 2009 start
207: 2009 start
210: 2009 start (not continueous)
216: 2008 start
217: 2009 start (2004 has one value)
219: 2011 start (2010 has 90 dollars)
220: 2008 start
222: 2009 start (2013 has one value)
223: 2011 start (2009 has one value)
225: 2009 start but small; 2015 max
226: 2012 max 
228: 2009 start, 2010 max
231: 2010 start, 2011 max
232: 2015 max
233: 2009 start (2007 has one value)
237: 2009 start
239: 2007 max
240: 2009 start, 2011 max
(total: 25)

#=========================#
pre- and post-investment difference (MK test)

```{r}
case <- full_data_5%>%
  filter(index==190 | index==193 | index==195 | index==196 |index==198 | index==199 |index==202 | index==206 |index==207 | index==210 |index==216 | index==217| index==219| index==220| index==222| index==223| index==225| index==226| index==228| index==231| index==232| index==233| index==237| index==239| index==240)%>%
  mutate(index2 = rep(1:25,times = 1, each = 16))

yr <- 2009

case_pre <- case%>%
  filter(year<yr)

case_post <- case%>%
  filter(year>=yr)

```


```{r}
#case%>%
#  filter(index==226)%>%
#  ggplot(aes(x=year,y=yr_mean_AQI25))+geom_line()
```



```{r}
#create new data frame
mk_tau <- data.frame(matrix(data = NA, nrow = 25, ncol = 9))
colnames(mk_tau) <- c("index", "mk_tau_AQI10_pre","p_AQI10_pre","mk_tau_AQI25_pre","p_AQI25_pre","mk_tau_AQI10_post","p_AQI10_post","mk_tau_AQI25_post","p_AQI25_post")
mk_tau[,1] <- unique(case$index)


for (n in 1:25){     #loop for county
  
  #case_pre
  tmp <- case_pre%>%
    filter(index2==n)
  
  if(any(is.na(tmp$yr_mean_AQI))) {
    mk_tau[n,2]=NaN
    mk_tau[n,3]=NaN
  }  else{
    mk_tau[n,2] <- mk.test(tmp$yr_mean_AQI)$estimates[3]
    mk_tau[n,3] <- mk.test(tmp$yr_mean_AQI)$p.value
  }
  
   if(any(is.na(tmp$yr_mean_AQI25))) {
    mk_tau[n,4]=NaN
    mk_tau[n,5]=NaN
  }  else{
    mk_tau[n,4] <- mk.test(tmp$yr_mean_AQI25)$estimates[3]
    mk_tau[n,5] <- mk.test(tmp$yr_mean_AQI25)$p.value
  }
  
  #case_post
    tmp <- case_post%>%
    filter(index2==n)
  
  if(any(is.na(tmp$yr_mean_AQI))) {
    mk_tau[n,6]=NaN
    mk_tau[n,7]=NaN
  }  else{
    mk_tau[n,6] <- mk.test(tmp$yr_mean_AQI)$estimates[3]
    mk_tau[n,7] <- mk.test(tmp$yr_mean_AQI)$p.value
  }
  
   if(any(is.na(tmp$yr_mean_AQI25))) {
    mk_tau[n,8]=NaN
    mk_tau[n,9]=NaN
  }  else{
    mk_tau[n,8] <- mk.test(tmp$yr_mean_AQI25)$estimates[3]
    mk_tau[n,9] <- mk.test(tmp$yr_mean_AQI25)$p.value
  }
  
}

```

scatter plot:
```{r}
#for plotting, seperate AQI10 and AQI25
#AQI10
tmp <- mk_tau%>%
  select(index, mk_tau_AQI10_pre, mk_tau_AQI10_post)%>%
  rename(pre = "mk_tau_AQI10_pre", post = "mk_tau_AQI10_post")

qwe <- mk_tau%>%
  select(index, p_AQI10_pre, p_AQI10_post)%>%
    rename(pre = "p_AQI10_pre", post = "p_AQI10_post")

tmp2 <- melt(tmp,id.vars = c("index"))%>%
  rename(tau = "value")

qwe2 <- melt(qwe,id.vars = c("index"))%>%
  rename(pval = "value")

mk_tau_10 <- merge(tmp2, qwe2, by=c("index","variable"))


#AQI25     
tmp <- mk_tau%>%
  select(index, mk_tau_AQI25_pre, mk_tau_AQI25_post)%>%
  rename(pre = "mk_tau_AQI25_pre", post = "mk_tau_AQI25_post")

qwe <- mk_tau%>%
  select(index, p_AQI25_pre, p_AQI25_post)%>%
    rename(pre = "p_AQI25_pre", post = "p_AQI25_post")

tmp2 <- melt(tmp,id.vars = c("index"))%>%
  rename(tau = "value")

qwe2 <- melt(qwe,id.vars = c("index"))%>%
  rename(pval = "value")

mk_tau_25 <- merge(tmp2, qwe2, by=c("index","variable"))



#scatter plot

#AQI10
ggplot(data = mk_tau_10, aes(x = index, y = tau, color = variable, alpha = pval))+
  scale_alpha(limits = c(0,1), range = c(0.8, 0.2))+
scale_colour_manual(values = c("red", "blue"))+
  geom_point(size=3)+  
  geom_hline(yintercept=0)+
    labs(title="pre- and post-investment monotonic trend in  county", subtitle="California, PM10")+
  ylim(-1,1)+  
  xlab("county ID")


#AQI25
ggplot(data = mk_tau_25, aes(x = index, y = tau, color = variable, alpha = pval))+
  scale_alpha(limits = c(0,1), range = c(0.8, 0.2))+
scale_colour_manual(values = c("red", "blue"))+
  geom_point(size=3)+  
  geom_hline(yintercept=0)+
  labs(title="pre- and post-investment monotonic trend in  county", subtitle="California, PM2.5")+
  ylim(-1,1)+
  xlab("county ID")
    


```


We expected that post-investment would have more negative tau value (pollutant decreases faster).
But the results don't look like that. Many counties even have positive trend after investment!


#=========================#
MA case

```{r}
case <- full_data_5%>%
  filter(index==1221 | index==1227 | index==1228 | index==1217 | index==1219 | index==1226)%>%
  mutate(index2 = rep(1:6,times = 1, each = 16))

yr <- 2009

case_pre <- case%>%
  filter(year<yr)

case_post <- case%>%
  filter(year>=yr)

```


```{r}

#create new data frame
mk_tau <- data.frame(matrix(data = NA, nrow = 6, ncol = 9))
colnames(mk_tau) <- c("index", "mk_tau_AQI10_pre","p_AQI10_pre","mk_tau_AQI25_pre","p_AQI25_pre","mk_tau_AQI10_post","p_AQI10_post","mk_tau_AQI25_post","p_AQI25_post")
mk_tau[,1] <- unique(case$index)


for (n in 1:6){     #loop for county
    
  #case_pre
  tmp <- case_pre%>%
    filter(index2==n)
  
    if(any(is.na(tmp$yr_mean_AQI))) {
    mk_tau[n,2]=NaN
    mk_tau[n,3]=NaN
  }  else{
    mk_tau[n,2] <- mk.test(tmp$yr_mean_AQI)$estimates[3]
    mk_tau[n,3] <- mk.test(tmp$yr_mean_AQI)$p.value
  }
  
   if(any(is.na(tmp$yr_mean_AQI25))) {
    mk_tau[n,4]=NaN
    mk_tau[n,5]=NaN
  }  else{
    mk_tau[n,4] <- mk.test(tmp$yr_mean_AQI25)$estimates[3]
    mk_tau[n,5] <- mk.test(tmp$yr_mean_AQI25)$p.value
  }
  
  
  
  #case_post
    tmp <- case_post%>%
    filter(index2==n)
  
  if(any(is.na(tmp$yr_mean_AQI))) {
    mk_tau[n,6]=NaN
    mk_tau[n,7]=NaN
  }  else{
    mk_tau[n,6] <- mk.test(tmp$yr_mean_AQI)$estimates[3]
    mk_tau[n,7] <- mk.test(tmp$yr_mean_AQI)$p.value
  }
  
   if(any(is.na(tmp$yr_mean_AQI25))) {
    mk_tau[n,8]=NaN
    mk_tau[n,9]=NaN
  }  else{
    mk_tau[n,8] <- mk.test(tmp$yr_mean_AQI25)$estimates[3]
    mk_tau[n,9] <- mk.test(tmp$yr_mean_AQI25)$p.value
  }
  
}

```

scatter plot:
```{r}
#for plotting, seperate AQI10 and AQI25
#AQI10
tmp <- mk_tau%>%
  select(index, mk_tau_AQI10_pre, mk_tau_AQI10_post)%>%
  rename(pre = "mk_tau_AQI10_pre", post = "mk_tau_AQI10_post")

qwe <- mk_tau%>%
  select(index, p_AQI10_pre, p_AQI10_post)%>%
    rename(pre = "p_AQI10_pre", post = "p_AQI10_post")

tmp2 <- melt(tmp,id.vars = c("index"))%>%
  rename(tau = "value")

qwe2 <- melt(qwe,id.vars = c("index"))%>%
  rename(pval = "value")

mk_tau_10 <- merge(tmp2, qwe2, by=c("index","variable"))


#AQI25     
tmp <- mk_tau%>%
  select(index, mk_tau_AQI25_pre, mk_tau_AQI25_post)%>%
  rename(pre = "mk_tau_AQI25_pre", post = "mk_tau_AQI25_post")

qwe <- mk_tau%>%
  select(index, p_AQI25_pre, p_AQI25_post)%>%
    rename(pre = "p_AQI25_pre", post = "p_AQI25_post")

tmp2 <- melt(tmp,id.vars = c("index"))%>%
  rename(tau = "value")

qwe2 <- melt(qwe,id.vars = c("index"))%>%
  rename(pval = "value")

mk_tau_25 <- merge(tmp2, qwe2, by=c("index","variable"))



#scatter plot

#AQI10
ggplot(data = mk_tau_10, aes(x = index, y = tau, color = variable, alpha = pval))+
  scale_alpha(limits = c(0,1), range = c(0.8, 0.2))+
scale_colour_manual(values = c("red", "blue"))+
  geom_point(size=3)+  
  geom_hline(yintercept=0)+
  labs(title="pre- and post-investment monotonic trend in county", subtitle="Massachusetts, PM10")+
  ylim(-1,1)+
  xlab("county ID")
    
#AQI25
ggplot(data = mk_tau_25, aes(x = index, y = tau, color = variable, alpha = pval))+
  scale_alpha(limits = c(0,1), range = c(0.8, 0.2))+
scale_colour_manual(values = c("red", "blue"))+
  geom_point(size=3)+  
  geom_hline(yintercept=0)+
  labs(title="pre- and post-investment monotonic trend in county", subtitle="Massachusetts, PM2.5")+
  ylim(-1,1)+
  xlab("county ID")
    


```

Much better than CA.


