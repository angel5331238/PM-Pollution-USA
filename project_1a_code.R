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
pacman::p_load(dplyr, ggplot2, mapproj, readr, ggthemes, viridis, reshape2,BBmisc, car, lme4, radiant.data, trend, broom)
```

Read in the data:
Don't have to run this chunk if you just ran `project_1a_read_in_data.Rmd`!
```{r}
#after project_1a_read_in_data.Rmd
full_data_5 <- read_csv('full_data_1a.csv')
  full_data_5 <- full_data_5[,-1]  #R saves the data frame with an additional column, so remove it.

```

#========================#
Normalize pm10 and pm25 (each county divides their own time average value)

```{r}
qwew<-full_data_5%>%
  group_by(index)%>%
  summarise(county_mean_pm10 = mean(yr_mean_pm10, na.rm=TRUE),county_mean_pm25 = mean(yr_mean_pm25, na.rm=TRUE))

sdfd <- merge(full_data_5,qwew, by=c("index"), all.x = TRUE)

full_data_5_nor <- sdfd%>%
  mutate(yr_mean_pm10 = yr_mean_pm10/county_mean_pm10,yr_mean_pm25 = yr_mean_pm25/county_mean_pm25)%>%
  
```

#=============================#

Check distribution:
```{r}
full_data_5_nor%>%
  rename(PM10 = "yr_mean_pm10", PM2.5 = "yr_mean_pm25")%>%
  melt(id.vars=c('index', 'year','code_state','code_county'), variable.name='var', value.name='value')%>%
  ggplot(aes(value,..density..))+
  geom_histogram(na.rm = TRUE, binwidth=0.05)+
  facet_wrap('var')+
  ggtitle("Normalized PM concentration distribution")+
  xlab("concentration (μg/m3)")

```


#=====================#

scatter plot: PM10 & PM25
```{r}
full_data_5_nor%>%
  select(year,yr_mean_pm10,yr_mean_pm25)%>%
  ggplot(aes(x=yr_mean_pm10,y=yr_mean_pm25), na.rm=TRUE)+
  geom_point(size=3, alpha=0.2)+
    geom_abline(col="blue")+
  xlim(0,2.5)+
  ylim(0,2.5)+
  ylab("PM2.5 (μg/m3)")+
  xlab("PM10 (μg/m3)")+
  labs(title="Relationship between PM10 and PM2.5", subtitle="normalized data")

```

positive correlated.

#=====================#

Full pooling model: (pm10)
```{r}
# Fit a model on all the data pooled together
lm_pooled_10 <- lm(yr_mean_pm10 ~ year, data = full_data_5_nor, na.action = na.omit) 
summary(lm_pooled_10)
write.csv(tidy(summary(lm_pooled_10)), file = "table_1a_1.csv")

par(mfrow = c(2, 2)) # set up 2 by 2 plot
plot(lm_pooled_10) # built-in diagnostic plots
par(mfrow = c(1, 1)) # go back to 1 by 1 plots (default)  

#Is residual normal distribution?
plot(density(lm_pooled_10$residuals))
boxplot(lm_pooled_10$residuals, main = "Residual boxplot")

#make data frame
df_pooled_10 <- data_frame(
  index = unique(full_data_5_nor$index),
  Intercept = coef(lm_pooled_10)[1], 
  slope_year = coef(lm_pooled_10)[2],
  Model = "Full pooling")

```


No pooling model:
```{r}

df_no_pooling_10 <- lmList(yr_mean_pm10 ~ year | index, data = full_data_5_nor, na.action=na.omit) %>% 
  coef() %>% 
  # Index IDs are stored as row-names. Make them an explicit column
  rownames_to_column("index") %>%
  mutate(Model = "No pooling") %>% 
  mutate(index = as.numeric(index)) %>%
  rename(Intercept = `(Intercept)`, slope_year = 'year') 

```


```{r}

df_model_10 <- bind_rows(df_no_pooling_10, df_pooled_10) %>%
  left_join(full_data_5_nor, by = "index")


ggplot(df_model_10) + 
  aes(x = year, y = yr_mean_pm10) + 
  geom_abline(aes(intercept = Intercept, slope = slope_year, color = Model, size = Model),  alpha = 0.5)+
  scale_size_discrete(range=c(1, 0.1)) +
  geom_point(alpha = 0) +
  ylim(0.5,1.7)+
  scale_colour_manual(values = c("red", "grey")) +
  ylab("PM10 (μg/m3)")+
  ggtitle("PM10 linear regression")



```


#==================================#

Full pooling model: (pm25)
```{r}
# Fit a model on all the data pooled together
lm_pooled_25 <- lm(yr_mean_pm25 ~ year, data = full_data_5_nor, na.action=na.omit) 
summary(lm_pooled_25)
write.csv(tidy(summary(lm_pooled_25)), file = "table_1a_2.csv")

par(mfrow = c(2, 2)) # set up 2 by 2 plot
plot(lm_pooled_25) # built-in diagnostic plots
par(mfrow = c(1, 1)) # go back to 1 by 1 plots (default)  

#Is residual normal distribution?
plot(density(lm_pooled_25$residuals))
boxplot(lm_pooled_25$residuals, main = "Residual boxplot")

#make data frame
df_pooled_25 <- data_frame(
  index = unique(full_data_5_nor$index),
  Intercept = coef(lm_pooled_25)[1], 
  slope_year = coef(lm_pooled_25)[2],
  Model = "Full pooling")


```


No pooling model:
```{r}

df_no_pooling_25 <- lmList(yr_mean_pm25 ~ year | index, data = full_data_5_nor, na.action=na.omit) %>% 
  coef() %>% 
  # Index IDs are stored as row-names. Make them an explicit column
  rownames_to_column("index") %>%
    mutate(Model = "No pooling") %>% 
  mutate(index = as.numeric(index)) %>%
  rename(Intercept = `(Intercept)`, slope_year = 'year') 

```


```{r}

df_model_25 <- bind_rows(df_no_pooling_25, df_pooled_25) %>%
  left_join(full_data_5_nor, by = "index")

  
#plot only one station
zxc <- df_model_25%>%
  filter(index==222)

ggplot(zxc) + 
  aes(x = year, y = yr_mean_pm10) +
  geom_line(alpha=0.3, size = 0.1)+
  geom_abline(aes(intercept = Intercept, slope = slope_year, color = Model), size = .25, alpha = 0.5)+ 
 geom_point(alpha = 0) +
scale_colour_grey(start = 0.1, end = 0.7)



#plot all regression lines
ggplot(df_model_25) + 
  aes(x = year, y = yr_mean_pm25) + 
  geom_abline(aes(intercept = Intercept, slope = slope_year, color = Model, size = Model), alpha = 0.4)+ 
 scale_size_discrete(range=c(1, 0.1)) +
  geom_point(alpha = 0) +
    ylim(0.5,1.7)+
  #  scale_color_brewer(palette = "Greys")
  #scale_colour_grey(start = 0.1, end = 0.9)
  scale_colour_manual(values = c("red", "grey")) +
  ylab("PM2.5 (μg/m3)")+
  ggtitle("PM2.5 linear regression")




```

#==================#
Mann Kendall non-parametric test



```{r}
#check auto-correlation
#choose random county


par(mfrow=c(2,1))
sdfsd<-full_data_5_nor%>%
  filter(index==232)%>%
  select(year, yr_mean_pm10)
acf(sdfsd$yr_mean_pm10,lag.max = NULL, na.action = na.omit)
pacf(sdfsd$yr_mean_pm10, na.action = na.omit)
par(mfrow = c(1, 1)) # go back to 1 by 1 plots (default)  


par(mfrow=c(2,1))
sdfsd<-full_data_5_nor%>%
  filter(index==232)%>%
  select(year, yr_mean_pm25)
acf(sdfsd$yr_mean_pm25,lag.max = NULL, na.action = na.omit)
pacf(sdfsd$yr_mean_pm25, na.action = na.omit)
par(mfrow = c(1, 1)) # go back to 1 by 1 plots (default)  

```

not highly autocorrelated.


MK trend test:
```{r}
#create new data frame
mk_tau <- data.frame(matrix(data = NA, nrow = 3142, ncol = 5))
colnames(mk_tau) <- c("index", "mk_tau_pm10","p_pm10","mk_tau_pm25","p_pm25")
mk_tau[,1] <- c(1:3142)

for (n in 1:3142){     #loop for county
  
  tmp <- full_data_5_nor%>%
    filter(index==n)
  
  if(any(is.na(tmp$yr_mean_pm10))) {
    mk_tau[n,2]=NaN
    mk_tau[n,3]=NaN
    
  }  else{
    mk_tau[n,2] <- mk.test(tmp$yr_mean_pm10)$estimates[3]
    mk_tau[n,3] <- mk.test(tmp$yr_mean_pm10)$p.value

  }
  
   if(any(is.na(tmp$yr_mean_pm25))) {
    mk_tau[n,4]=NaN
    mk_tau[n,5]=NaN

  }  else{
    mk_tau[n,4] <- mk.test(tmp$yr_mean_pm25)$estimates[3]
    mk_tau[n,5] <- mk.test(tmp$yr_mean_pm25)$p.value
  }
  
}

```

scatter plot:
```{r}
#scatter plot

if (!require("RColorBrewer")) {
install.packages("RColorBrewer")
library(RColorBrewer)
}
blups <- brewer.pal(11,"RdYlBu")


ggplot(data = mk_tau, aes(x = index, y = mk_tau_pm10, color = mk_tau_pm10, size = p_pm10))+
  scale_size_continuous(range=c(2.5, 0.5),limits = c(0,1)) +
  scale_color_gradientn(colors=blups, limits=c(-1, 1))+
  geom_point()+  
  geom_hline(yintercept=0)+
  ylim(-1,1)+
    xlab("county index")+
  ylab("PM10 (μg/m3)")+
    ggtitle("Mann-Kendall tau value of PM10")



ggplot(data = mk_tau, aes(x=index, y=mk_tau_pm25, color=mk_tau_pm25, size = p_pm25))+
  scale_size_continuous(range=c(2.5, 0.5),limits = c(0,1)) +
  scale_color_gradientn(colors=blups, limits=c(-1, 1))+
  geom_point()+
  geom_hline(yintercept=0)+
  ylim(-1,1)+
  xlab("county index")+
  ylab("PM2.5 (μg/m3)")+
  ggtitle("Mann-Kendall tau value of PM2.5")

length(which(mk_tau$mk_tau_pm10>0))
length(which(mk_tau$mk_tau_pm25>0))
```

Most counties have significant negative trend. 
PM10: only 16 counties' tau > 0
PM25: only 4 counties' tau > 0
**CAVEAT**: only if the county has data in all 19 years will it has value. mk.test couldn't operate if there is NaN.

