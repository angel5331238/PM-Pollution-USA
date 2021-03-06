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
pacman::p_load(dplyr, ggplot2, mapproj, readr, ggthemes, viridis, reshape2, sp, gstat, locfit, xts)
```

Read in location code data:
```{r}

codes <- read_csv('codes.csv') %>%  
  rename(code_county = "FIPS County", code_state = "FIPS State")%>%
  select(code_state, code_county)%>%
  mutate(index = seq(1:n()))  #give each county an index to replace "state"+"code" 

```

Create a new complete matrix:
```{r}
zxc <- nrow(codes)
full_data <- matrix(data = NA, nrow = zxc*19, ncol = 2)
colnames(full_data) <- c("year", "index")

full_data[,1] <- rep(1999:2017, times = zxc, each = 1)
full_data[,2] <- rep(1:zxc, times = 1, each = 19)

full_data_2 <- merge(full_data,codes, by=c("index"), all.x = TRUE)

```

#=========================================#

Read in pm10 data and average into yearly data:
```{r}

read_yr_avg <- function(file_name, year, weights=NULL){
  
  good_matrix <- read_csv(file_name) %>% 
    select("Arithmetic Mean", "State Code", "County Code") %>%
    rename(pm10 = "Arithmetic Mean", state = "State Code", county = "County Code") %>%
    filter(state!="02" & state!="15" & state!="72")%>% #exclude Alaska, Hawaii, and Puerto Rico
    group_by(state, county)%>%
    summarise(yr_mean_pm10 = mean(pm10, na.rm=TRUE))%>%
    ungroup(state)%>%
    mutate(code_state = as.numeric(state))%>%
    mutate(code_county = as.numeric(county))%>%
    select(code_state,code_county,yr_mean_pm10)%>%
    mutate(year=rep(year,n()))
  
  return(good_matrix)
  
}

pm10_1999 <- read_yr_avg('pm10_1999.csv', 1999)
pm10_2000 <- read_yr_avg('pm10_2000.csv', 2000)
pm10_2001 <- read_yr_avg('pm10_2001.csv', 2001)
pm10_2002 <- read_yr_avg('pm10_2002.csv', 2002)
pm10_2003 <- read_yr_avg('pm10_2003.csv', 2003)
pm10_2004 <- read_yr_avg('pm10_2004.csv', 2004)
pm10_2005 <- read_yr_avg('pm10_2005.csv', 2005)
pm10_2006 <- read_yr_avg('pm10_2006.csv', 2006)
pm10_2007 <- read_yr_avg('pm10_2007.csv', 2007)
pm10_2008 <- read_yr_avg('pm10_2008.csv', 2008)
pm10_2009 <- read_yr_avg('pm10_2009.csv', 2009)
pm10_2010 <- read_yr_avg('pm10_2010.csv', 2010)
pm10_2011 <- read_yr_avg('pm10_2011.csv', 2011)
pm10_2012 <- read_yr_avg('pm10_2012.csv', 2012)
pm10_2013 <- read_yr_avg('pm10_2013.csv', 2013)
pm10_2014 <- read_yr_avg('pm10_2014.csv', 2014)
pm10_2015 <- read_yr_avg('pm10_2015.csv', 2015)
pm10_2016 <- read_yr_avg('pm10_2016.csv', 2016)
pm10_2017 <- read_yr_avg('pm10_2017.csv', 2017)

```

Merge air data into full_data:
```{r}
#First, create a matrix for data put-in afterward
#give air data index 
pm10_ind <- merge(codes,pm10_1999, by=c("code_state", "code_county"), all.x = TRUE)%>%
  select(index, yr_mean_pm10, year)
#merge with full_data_2
full_data_3 <- merge(full_data_2, pm10_ind, by=c("index", "year"), all.x = TRUE)
##full_data_3 is the data frame we use to fill in all other years
##pm10_ind is only used once; can reuse this name afterward


merge_full_data <- function(full_data_2, full_data_3, file_name, year, weights=NULL){
  #full_data_2: the base matrix for merging (give index)
  #full_data_3: the matrix just created to fill in other years' data
  #file_name: the data read in from previous function
  
  pm10_ind <- merge(codes, file_name, by=c("code_state", "code_county"), all.x = TRUE)%>%
    select(index, yr_mean_pm10, year)
  full_data_4 <- merge(full_data_2, pm10_ind, by=c("index", "year"), all.x = TRUE)
  full_data_3[which(full_data_3$year==year),] <- full_data_4[which(full_data_4$year==year),]
  ##full_data_4 is only used once; can reuse this name afterward
  
  return(full_data_3)
}

full_data_3 <- merge_full_data(full_data_2, full_data_3, pm10_2000, 2000)
full_data_3 <- merge_full_data(full_data_2, full_data_3, pm10_2001, 2001)
full_data_3 <- merge_full_data(full_data_2, full_data_3, pm10_2002, 2002)
full_data_3 <- merge_full_data(full_data_2, full_data_3, pm10_2003, 2003)
full_data_3 <- merge_full_data(full_data_2, full_data_3, pm10_2004, 2004)
full_data_3 <- merge_full_data(full_data_2, full_data_3, pm10_2005, 2005)
full_data_3 <- merge_full_data(full_data_2, full_data_3, pm10_2006, 2006)
full_data_3 <- merge_full_data(full_data_2, full_data_3, pm10_2007, 2007)
full_data_3 <- merge_full_data(full_data_2, full_data_3, pm10_2008, 2008)
full_data_3 <- merge_full_data(full_data_2, full_data_3, pm10_2009, 2009)
full_data_3 <- merge_full_data(full_data_2, full_data_3, pm10_2010, 2010)
full_data_3 <- merge_full_data(full_data_2, full_data_3, pm10_2011, 2011)
full_data_3 <- merge_full_data(full_data_2, full_data_3, pm10_2012, 2012)
full_data_3 <- merge_full_data(full_data_2, full_data_3, pm10_2013, 2013)
full_data_3 <- merge_full_data(full_data_2, full_data_3, pm10_2014, 2014)
full_data_3 <- merge_full_data(full_data_2, full_data_3, pm10_2015, 2015)
full_data_3 <- merge_full_data(full_data_2, full_data_3, pm10_2016, 2016)
full_data_3 <- merge_full_data(full_data_2, full_data_3, pm10_2017, 2017)

```

#===========================#

Read in pm25 data and average into yearly data:
```{r}

read_yr_avg2 <- function(file_name, year, weights=NULL){
  good_matrix <- read_csv(file_name) %>% 
    select("Arithmetic Mean", "State Code", "County Code") %>%
    rename(pm25 = "Arithmetic Mean", state = "State Code", county = "County Code") %>%
    filter(state!="02" & state!="15" & state!="72")%>%
    group_by(state, county)%>%
    summarise(yr_mean_pm25 = mean(pm25, na.rm=TRUE))%>%
    ungroup(state)%>%
    mutate(code_state = as.numeric(state))%>%
    mutate(code_county = as.numeric(county))%>%
    select(code_state,code_county,yr_mean_pm25)%>%
    mutate(year=rep(year,n()))
  
  return(good_matrix)
}


pm25_1999 <- read_yr_avg2('pm25_1999.csv', 1999)
pm25_2000 <- read_yr_avg2('pm25_2000.csv', 2000)
pm25_2001 <- read_yr_avg2('pm25_2001.csv', 2001)
pm25_2002 <- read_yr_avg2('pm25_2002.csv', 2002)
pm25_2003 <- read_yr_avg2('pm25_2003.csv', 2003)
pm25_2004 <- read_yr_avg2('pm25_2004.csv', 2004)
pm25_2005 <- read_yr_avg2('pm25_2005.csv', 2005)
pm25_2006 <- read_yr_avg2('pm25_2006.csv', 2006)
pm25_2007 <- read_yr_avg2('pm25_2007.csv', 2007)
pm25_2008 <- read_yr_avg2('pm25_2008.csv', 2008)
pm25_2009 <- read_yr_avg2('pm25_2009.csv', 2009)
pm25_2010 <- read_yr_avg2('pm25_2010.csv', 2010)
pm25_2011 <- read_yr_avg2('pm25_2011.csv', 2011)
pm25_2012 <- read_yr_avg2('pm25_2012.csv', 2012)
pm25_2013 <- read_yr_avg2('pm25_2013.csv', 2013)
pm25_2014 <- read_yr_avg2('pm25_2014.csv', 2014)
pm25_2015 <- read_yr_avg2('pm25_2015.csv', 2015)
pm25_2016 <- read_yr_avg2('pm25_2016.csv', 2016)
pm25_2017 <- read_yr_avg2('pm25_2017.csv', 2017)

```

Merge air data into full_data:
```{r}
#First, create a matrix for data put-in afterward
#give air data index 
pm10_ind <- merge(codes,pm25_1999, by=c("code_state", "code_county"), all.x = TRUE)%>%
  select(index, yr_mean_pm25, year)
#merge with full_data_2
full_data_5 <- merge(full_data_3, pm10_ind, by=c("index", "year"), all.x = TRUE)
##full_data_5 is the data frame we use to fill in all other years
##pm10_ind is only used once; can reuse this name afterward


merge_full_data2 <- function(full_data_2, full_data_3, file_name, year, weights=NULL){
  #full_data_2: the base matrix for merging (give index)
  #full_data_3: the matrix just created to fill in other years' data
  #file_name: the data read in from previous function
  
  pm10_ind <- merge(codes, file_name, by=c("code_state", "code_county"), all.x = TRUE)%>%
    select(index, yr_mean_pm25, year)
  full_data_4 <- merge(full_data_2, pm10_ind, by=c("index", "year"), all.x = TRUE)
  full_data_3[which(full_data_3$year==year),] <- full_data_4[which(full_data_4$year==year),]
  ##full_data_4 is only used once; can reuse this name afterward
  
  return(full_data_3)
}

full_data_5 <- merge_full_data2(full_data_3, full_data_5, pm25_2000, 2000)
full_data_5 <- merge_full_data2(full_data_3, full_data_5, pm25_2001, 2001)
full_data_5 <- merge_full_data2(full_data_3, full_data_5, pm25_2002, 2002)
full_data_5 <- merge_full_data2(full_data_3, full_data_5, pm25_2003, 2003)
full_data_5 <- merge_full_data2(full_data_3, full_data_5, pm25_2004, 2004)
full_data_5 <- merge_full_data2(full_data_3, full_data_5, pm25_2005, 2005)
full_data_5 <- merge_full_data2(full_data_3, full_data_5, pm25_2006, 2006)
full_data_5 <- merge_full_data2(full_data_3, full_data_5, pm25_2007, 2007)
full_data_5 <- merge_full_data2(full_data_3, full_data_5, pm25_2008, 2008)
full_data_5 <- merge_full_data2(full_data_3, full_data_5, pm25_2009, 2009)
full_data_5 <- merge_full_data2(full_data_3, full_data_5, pm25_2010, 2010)
full_data_5 <- merge_full_data2(full_data_3, full_data_5, pm25_2011, 2011)
full_data_5 <- merge_full_data2(full_data_3, full_data_5, pm25_2012, 2012)
full_data_5 <- merge_full_data2(full_data_3, full_data_5, pm25_2013, 2013)
full_data_5 <- merge_full_data2(full_data_3, full_data_5, pm25_2014, 2014)
full_data_5 <- merge_full_data2(full_data_3, full_data_5, pm25_2015, 2015)
full_data_5 <- merge_full_data2(full_data_3, full_data_5, pm25_2016, 2016)
full_data_5 <- merge_full_data2(full_data_3, full_data_5, pm25_2017, 2017)

```
full_data_5 is the final matrix for use.

Save the matrix so we don't have to rerun every time we want to analyze it.
```{r}
write.csv(full_data_5,"full_data_1a.csv")
```



