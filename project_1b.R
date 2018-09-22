---
title: "Project"
author: "An-Chi Ho (ah3508) and Sruti"
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
pacman::p_load(dplyr, ggplot2, mapproj, readr, ggthemes, viridis, reshape2, sp, gstat, locfit, ggmap, automap)
```

Read in the data:
```{r}
mean_1999 <- read_csv('pm10_1999.csv') %>% 
  select("Date Local", "Arithmetic Mean", "State Code", "County Code", Longitude, Latitude, AQI) %>%
  rename(date = "Date Local",pm10 = "Arithmetic Mean", state = "State Code", county = "County Code", lon = Longitude, lat = Latitude) %>%
  filter(state!="02" & state!="15" & state!="72")%>%
  group_by(lat, lon) %>%
  summarise(mean_pm10_1999 = mean(pm10, na.rm=TRUE))


mean_2000 <- read_csv('pm10_2000.csv') %>% 
  select("Date Local", "Arithmetic Mean", "State Code", "County Code", Longitude, Latitude, AQI) %>%
  rename(date = "Date Local",pm10 = "Arithmetic Mean", state = "State Code", county = "County Code", lon = Longitude, lat = Latitude) %>%
  filter(state!="02" & state!="15" & state!="72")%>%
  group_by(lat, lon) %>%
  summarise(mean_pm10_2000 = mean(pm10, na.rm=TRUE))


mean_2001 <- read_csv('pm10_2001.csv') %>% 
  select("Date Local", "Arithmetic Mean", "State Code", "County Code", Longitude, Latitude, AQI) %>%
  rename(date = "Date Local",pm10 = "Arithmetic Mean", state = "State Code", county = "County Code", lon = Longitude, lat = Latitude) %>%
  filter(state!="02" & state!="15" & state!="72")%>%
  group_by(lat, lon) %>%
  summarise(mean_pm10_2001 = mean(pm10, na.rm=TRUE))

mean_2002 <- read_csv('pm10_2002.csv') %>% 
  select("Date Local", "Arithmetic Mean", "State Code", "County Code", Longitude, Latitude, AQI) %>%
  rename(date = "Date Local",pm10 = "Arithmetic Mean", state = "State Code", county = "County Code", lon = Longitude, lat = Latitude) %>%
  filter(state!="02" & state!="15" & state!="72")%>%
  group_by(lat, lon) %>%
  summarise(mean_pm10_2002 = mean(pm10, na.rm=TRUE))

mean_2003 <- read_csv('pm10_2003.csv') %>% 
  select("Date Local", "Arithmetic Mean", "State Code", "County Code", Longitude, Latitude, AQI) %>%
  rename(date = "Date Local",pm10 = "Arithmetic Mean", state = "State Code", county = "County Code", lon = Longitude, lat = Latitude) %>%
  filter(state!="02" & state!="15" & state!="72")%>%
  group_by(lat, lon) %>%
  summarise(mean_pm10_2003 = mean(pm10, na.rm=TRUE))

mean_2004 <- read_csv('pm10_2004.csv') %>% 
  select("Date Local", "Arithmetic Mean", "State Code", "County Code", Longitude, Latitude, AQI) %>%
  rename(date = "Date Local",pm10 = "Arithmetic Mean", state = "State Code", county = "County Code", lon = Longitude, lat = Latitude) %>%
  filter(state!="02" & state!="15" & state!="72")%>%
  group_by(lat, lon) %>%
  summarise(mean_pm10_2004 = mean(pm10, na.rm=TRUE))

mean_2005 <- read_csv('pm10_2005.csv') %>% 
  select("Date Local", "Arithmetic Mean", "State Code", "County Code", Longitude, Latitude, AQI) %>%
  rename(date = "Date Local",pm10 = "Arithmetic Mean", state = "State Code", county = "County Code", lon = Longitude, lat = Latitude) %>%
  filter(state!="02" & state!="15" & state!="72")%>%
  group_by(lat, lon) %>%
  summarise(mean_pm10_2005 = mean(pm10, na.rm=TRUE))

mean_2006 <- read_csv('pm10_2006.csv') %>% 
  select("Date Local", "Arithmetic Mean", "State Code", "County Code", Longitude, Latitude, AQI) %>%
  rename(date = "Date Local",pm10 = "Arithmetic Mean", state = "State Code", county = "County Code", lon = Longitude, lat = Latitude) %>%
  filter(state!="02" & state!="15" & state!="72")%>%
  group_by(lat, lon) %>%
  summarise(mean_pm10_2006 = mean(pm10, na.rm=TRUE))

mean_2007 <- read_csv('pm10_2007.csv') %>% 
  select("Date Local", "Arithmetic Mean", "State Code", "County Code", Longitude, Latitude, AQI) %>%
  rename(date = "Date Local",pm10 = "Arithmetic Mean", state = "State Code", county = "County Code", lon = Longitude, lat = Latitude) %>%
  filter(state!="02" & state!="15" & state!="72")%>%
  group_by(lat, lon) %>%
  summarise(mean_pm10_2007 = mean(pm10, na.rm=TRUE))

mean_2008 <- read_csv('pm10_2008.csv') %>% 
  select("Date Local", "Arithmetic Mean", "State Code", "County Code", Longitude, Latitude, AQI) %>%
  rename(date = "Date Local",pm10 = "Arithmetic Mean", state = "State Code", county = "County Code", lon = Longitude, lat = Latitude) %>%
  filter(state!="02" & state!="15" & state!="72")%>%
  group_by(lat, lon) %>%
  summarise(mean_pm10_2008 = mean(pm10, na.rm=TRUE))

mean_2009 <- read_csv('pm10_2009.csv') %>% 
  select("Date Local", "Arithmetic Mean", "State Code", "County Code", Longitude, Latitude, AQI) %>%
  rename(date = "Date Local",pm10 = "Arithmetic Mean", state = "State Code", county = "County Code", lon = Longitude, lat = Latitude) %>%
  filter(state!="02" & state!="15" & state!="72")%>%
  group_by(lat, lon) %>%
  summarise(mean_pm10_2009 = mean(pm10, na.rm=TRUE))

mean_2010 <- read_csv('pm10_2010.csv') %>% 
  select("Date Local", "Arithmetic Mean", "State Code", "County Code", Longitude, Latitude, AQI) %>%
  rename(date = "Date Local",pm10 = "Arithmetic Mean", state = "State Code", county = "County Code", lon = Longitude, lat = Latitude) %>%
  filter(state!="02" & state!="15" & state!="72")%>%
  group_by(lat, lon) %>%
  summarise(mean_pm10_2010 = mean(pm10, na.rm=TRUE))

mean_2011 <- read_csv('pm10_2011.csv') %>% 
  select("Date Local", "Arithmetic Mean", "State Code", "County Code", Longitude, Latitude, AQI) %>%
  rename(date = "Date Local",pm10 = "Arithmetic Mean", state = "State Code", county = "County Code", lon = Longitude, lat = Latitude) %>%
  filter(state!="02" & state!="15" & state!="72")%>%
  group_by(lat, lon) %>%
  summarise(mean_pm10_2011 = mean(pm10, na.rm=TRUE))

mean_2012 <- read_csv('pm10_2012.csv') %>% 
  select("Date Local", "Arithmetic Mean", "State Code", "County Code", Longitude, Latitude, AQI) %>%
  rename(date = "Date Local",pm10 = "Arithmetic Mean", state = "State Code", county = "County Code", lon = Longitude, lat = Latitude) %>%
  filter(state!="02" & state!="15" & state!="72")%>%
  group_by(lat, lon) %>%
  summarise(mean_pm10_2012 = mean(pm10, na.rm=TRUE))

mean_2013 <- read_csv('pm10_2013.csv') %>% 
  select("Date Local", "Arithmetic Mean", "State Code", "County Code", Longitude, Latitude, AQI) %>%
  rename(date = "Date Local",pm10 = "Arithmetic Mean", state = "State Code", county = "County Code", lon = Longitude, lat = Latitude) %>%
  filter(state!="02" & state!="15" & state!="72")%>%
  group_by(lat, lon) %>%
  summarise(mean_pm10_2013 = mean(pm10, na.rm=TRUE))

mean_2014 <- read_csv('pm10_2014.csv') %>% 
  select("Date Local", "Arithmetic Mean", "State Code", "County Code", Longitude, Latitude, AQI) %>%
  rename(date = "Date Local",pm10 = "Arithmetic Mean", state = "State Code", county = "County Code", lon = Longitude, lat = Latitude) %>%
  filter(state!="02" & state!="15" & state!="72")%>%
  group_by(lat, lon) %>%
  summarise(mean_pm10_2014 = mean(pm10, na.rm=TRUE))

mean_2015 <- read_csv('pm10_2015.csv') %>% 
  select("Date Local", "Arithmetic Mean", "State Code", "County Code", Longitude, Latitude, AQI) %>%
  rename(date = "Date Local",pm10 = "Arithmetic Mean", state = "State Code", county = "County Code", lon = Longitude, lat = Latitude) %>%
  filter(state!="02" & state!="15" & state!="72")%>%
  group_by(lat, lon) %>%
  summarise(mean_pm10_2015 = mean(pm10, na.rm=TRUE))

mean_2016 <- read_csv('pm10_2016.csv') %>% 
  select("Date Local", "Arithmetic Mean", "State Code", "County Code", Longitude, Latitude, AQI) %>%
  rename(date = "Date Local",pm10 = "Arithmetic Mean", state = "State Code", county = "County Code", lon = Longitude, lat = Latitude) %>%
  filter(state!="02" & state!="15" & state!="72")%>%
  group_by(lat, lon) %>%
  summarise(mean_pm10_2016 = mean(pm10, na.rm=TRUE))

mean_2017 <- read_csv('pm10_2017.csv') %>% 
  select("Date Local", "Arithmetic Mean", "State Code", "County Code", Longitude, Latitude, AQI) %>%
  rename(date = "Date Local",pm10 = "Arithmetic Mean", state = "State Code", county = "County Code", lon = Longitude, lat = Latitude) %>%
  filter(state!="02" & state!="15" & state!="72")%>%
  group_by(lat, lon) %>%
  summarise(mean_pm10_2017 = mean(pm10, na.rm=TRUE))



```

USA map:
```{r}
usa_map <- map_data("state") %>%
  rename(lon = long) %>%
  ggplot(aes( x = lon ,y = lat)) +
  geom_polygon(aes(group = group) ,color = 'black', fill ='white') +
  coord_map(projection="albers", parameters = c(25, 50)) +
  theme_map()
```

```{r}
#To plot the timeseries, we need to get the values for all the years for a particular station. 

pm10 <- merge(mean_1999, mean_2000, by=c("lat", "lon"))
pm10 <- merge(pm10, mean_2001, by=c("lat", "lon"))
pm10 <- merge(pm10, mean_2002, by=c("lat", "lon"))
pm10 <- merge(pm10, mean_2003, by=c("lat", "lon"))
pm10 <- merge(pm10, mean_2004, by=c("lat", "lon"))
pm10 <- merge(pm10, mean_2005, by=c("lat", "lon"))
pm10 <- merge(pm10, mean_2006, by=c("lat", "lon"))
pm10 <- merge(pm10, mean_2007, by=c("lat", "lon"))
pm10 <- merge(pm10, mean_2008, by=c("lat", "lon"))
pm10 <- merge(pm10, mean_2009, by=c("lat", "lon"))
pm10 <- merge(pm10, mean_2010, by=c("lat", "lon"))
pm10 <- merge(pm10, mean_2011, by=c("lat", "lon"))
pm10 <- merge(pm10, mean_2012, by=c("lat", "lon"))
pm10 <- merge(pm10, mean_2013, by=c("lat", "lon"))
pm10 <- merge(pm10, mean_2014, by=c("lat", "lon"))
pm10 <- merge(pm10, mean_2015, by=c("lat", "lon"))
pm10 <- merge(pm10, mean_2016, by=c("lat", "lon"))
pm10 <- merge(pm10, mean_2017, by=c("lat", "lon"))
rm(mean_1999, mean_2000, mean_2001, mean_2002, mean_2003, mean_2004, mean_2005, mean_2006, mean_2007, mean_2008, mean_2009, mean_2010, mean_2011, mean_2012, mean_2013, mean_2014, mean_2015, mean_2016, mean_2017)
```


```{r}
#Calculating the time average of the pm values from 1999-2017, and getting them in one table
pm10$mean_all <- rowMeans(pm10[,3:19])
pm10<- pm10 %>%
  select(lat, lon, mean_all)
```
#To read in the pm25 data
```{r}
readin <- function(filename, year, weights = NULL)
{
  var <- read_csv(filename) %>% 
  select("Date Local", "Arithmetic Mean", "State Code", "County Code", Longitude, Latitude, AQI) %>%
  rename(date = "Date Local",pm25 = "Arithmetic Mean", state = "State Code", county = "County Code", lon = Longitude, lat = Latitude) %>%
  filter(state!="02" & state!="15" & state!="72")%>%
  group_by(lat, lon) %>%
  summarise(mean_year = mean(pm25, na.rm=TRUE))
  return(var)
}
```

```{r}
pm25_1999 <- readin('pm25_1999.csv', 1999 )
pm25_2000 <- readin('pm25_2000.csv', 2000)
pm25_2001 <- readin('pm25_2001.csv', 2001)
pm25_2002 <- readin('pm25_2002.csv', 2002)
pm25_2003 <- readin('pm25_2003.csv', 2003)
pm25_2004 <- readin('pm25_2004.csv', 2004)
pm25_2005 <- readin('pm25_2005.csv', 2005)
pm25_2006 <- readin('pm25_2006.csv', 2006)
pm25_2007 <- readin('pm25_2007.csv', 2007)
pm25_2008 <- readin('pm25_2008.csv', 2008)
pm25_2009 <- readin('pm25_2009.csv', 2009)
pm25_2010 <- readin('pm25_2010.csv', 2010)
pm25_2011 <- readin('pm25_2011.csv', 2011)
pm25_2012 <- readin('pm25_2012.csv', 2012)
pm25_2013 <- readin('pm25_2013.csv', 2013)
pm25_2014 <- readin('pm25_2014.csv', 2014)
pm25_2015 <- readin('pm25_2015.csv', 2015)
pm25_2016 <- readin('pm25_2016.csv', 2016)
pm25_2017 <- readin('pm25_2017.csv', 2017)

```

```{r}
pm25 <- merge(pm25_1999, pm25_2000, by=c("lat", "lon"))
pm25 <- merge(pm25, pm25_2001, by=c("lat", "lon"))
pm25 <- merge(pm25, pm25_2002, by=c("lat", "lon"))
pm25 <- merge(pm25, pm25_2003, by=c("lat", "lon"))
pm25 <- merge(pm25, pm25_2004, by=c("lat", "lon"))
pm25 <- merge(pm25, pm25_2005, by=c("lat", "lon"))
pm25 <- merge(pm25, pm25_2006, by=c("lat", "lon"))
pm25 <- merge(pm25, pm25_2007, by=c("lat", "lon"))
pm25 <- merge(pm25, pm25_2008, by=c("lat", "lon"))
pm25 <- merge(pm25, pm25_2009, by=c("lat", "lon"))
pm25 <- merge(pm25, pm25_2010, by=c("lat", "lon"))
pm25 <- merge(pm25, pm25_2011, by=c("lat", "lon"))
pm25 <- merge(pm25, pm25_2012, by=c("lat", "lon"))
pm25 <- merge(pm25, pm25_2013, by=c("lat", "lon"))
pm25 <- merge(pm25, pm25_2014, by=c("lat", "lon"))
pm25 <- merge(pm25, pm25_2015, by=c("lat", "lon"))
pm25 <- merge(pm25, pm25_2016, by=c("lat", "lon"))
pm25 <- merge(pm25, pm25_2017, by=c("lat", "lon"))
rm(pm25_1999, pm25_2000, pm25_2001, pm25_2002, pm25_2003, pm25_2004, pm25_2005, pm25_2006, pm25_2007, pm25_2008, pm25_2009, pm25_2010, pm25_2011, pm25_2012, pm25_2013, pm25_2014, pm25_2015, pm25_2016, pm25_2017)

```

#Perform kNN regression for pm10

```{r}
knnreg <- function(x_train, y_train, x_test, k, weights=NULL){
  # Implement a K-Nearest-Neighbor Regression
  # The predtion is carried out using a kernel regression approach with random sampling.
  #
  # Inputs:
  #   x_train: training data predictors
  #   y_train: training data observed predictand
  #   x_test: test data predictors
  #   k: the number of nearest-neighbors to use
  #   weights: weights to assign to each observation (default NULL)
  # Returns:
  #   y_test: nearest neighbor estimate of predictand given x_test
  
  # Get the names of y_test
  y_names <- names(y_test)
  
  # Convert all inputs to matrix format
  x_train <- as.matrix(x_train)
  x_test <- as.matrix(x_test)
  y_train <- as.matrix(y_train)
  
  # Make sure the input dimensions are correct
  n_train <- nrow(x_train)
  if (nrow(y_train) != n_train) stop('x_train and y_train have different number of rows')
  n_predictors <- ncol(x_train)
  if (ncol(x_test) != n_predictors) stop('x_train and x_test have different number predictors')
  n_test <- nrow(x_test)
  n_predictand <- ncol(y_train)
  
  # set weights if none are given
  if (is.null(weights)) weights <- rep(1, n_predictors)
  
  # Initialize the y_test matrix
  y_test <- matrix(NA, nrow = n_test, ncol = n_predictand)
  
  # Loop through each test data point:
  for (n in 1:n_test){
    # compute the distance from our test point to each training point
    distance <- matrix(NA, nrow = n_train, ncol = n_predictors)
    for (i in 1:n_predictors){
      distance[, i] <- 100 * weights[i] * (x_train[, i]- x_test[n, i])^2
    }
    distance <- rowSums(distance, na.rm=TRUE)
    distance_rank <- rank(distance)
    
    # get the positions of the neighbors for each prediction vector
    neighbor_idx <- rep(NA, n_test)
    neighbor_idx[rank(distance_rank, ties.method='first')] <- seq(1, n_train)
    neighbor_idx <- neighbor_idx[1:k] # keep only k nearest neighbors!
    
    # We make the prediction by taking the average value of the k nearest
    # neighbors, weighted by the *rank* of their distance from the
    # test point. Here "distance" means Euclidean distance.
    sum_dist_rank <- sum(distance_rank[1:k]^(-1)) # normalizing factor
    dist_prob <- (distance_rank[1:k] ^ (-1)) / sum_dist_rank # sampling probabilities
    sampled_indices <- sample(neighbor_idx, 1000, replace=T, prob=dist_prob) # re-sample the indices
    
    # make the predictions
    y_sampled <- y_train[neighbor_idx]
    if (n_predictand == 1) {
      y_test[n, ] <- mean(y_sampled, na.rm = TRUE)
    } else{
      y_test[n, ] <- colMeans(y_sampled, na.rm = TRUE)
    }
  }
  
  # convert to data frame
  y_test <- data.frame(y_test)
  names(y_test) <- y_names
  return(y_test)
}
#knnreg function ends
```



```{r}
K <- 10   #k-fold
n_nearest_neigh <- 2:20
mse <- rep(NA, length(n_nearest_neigh))

for (i in 1:length(n_nearest_neigh)){
  pm10_test1 <- pm10 %>% 
    mutate(index = sample(1:n())) %>%
    mutate(fold = (index %% K) + 1)
  
  # for each sub-fold create a data frame of true and predicted y
  results_df <- vector('list', length = K) # create an empty list
  for (k_i in 1:K){
    x_test <- filter(pm10_test1, fold == k_i) %>% select(lon, lat)
    x_train <- filter(pm10_test1, fold != k_i) %>% select(lon, lat)
    y_train <- filter(pm10_test1, fold != k_i) %>% select(mean_all)
    y_test <- filter(pm10_test1, fold == k_i) %>% select(mean_all)
    y_hat <- knnreg(x_train=x_train, y_train=y_train, x_test=x_test, k=n_nearest_neigh[i])
    results_df[[k_i]] <- data.frame(y_hat = y_hat$mean_all, y_true = y_test$mean_all) 
  }
  # convert our list of data frames to a single data frame
  results_df <- bind_rows(results_df)
  # get MSE
  mse[i] <- mean((results_df$y_hat - results_df$y_true) ^ 2)
  
}
plot(n_nearest_neigh, mse, main ="MSE vs Nearest Neighbor for pm10")

```

```{r}
#get k=3 as the nearest neighbor in knn for pm10
K <- 10

pm10_test1 <- pm10 %>% 
  mutate(index = sample(1:n())) %>%
  mutate(fold = (index %% K) + 1)

# for each sub-fold create a data frame of true and predicted y
results_df <- vector('list', length = K) # create an empty list

for (k_i in 1:K){
  x_test <- filter(pm10_test1, fold == k_i) %>% select(lon, lat)
  x_train <- filter(pm10_test1, fold != k_i) %>% select(lon, lat)
  y_train <- filter(pm10_test1, fold != k_i) %>% select(mean_all)
  y_test <- filter(pm10_test1, fold == k_i) %>% select(mean_all)
  lat_test <- filter(pm10_test1, fold == k_i) %>% select(lat)
  lon_test <- filter(pm10_test1, fold == k_i) %>% select(lon)
  
  y_hat <- knnreg(x_train=x_train, y_train=y_train, x_test=x_test, k=3)
  results_df[[k_i]] <- data.frame(y_hat = y_hat$mean_all, y_true = y_test$mean_all, lon = lon_test$lon, lat = lat_test, resid = y_hat$mean_all - y_test$mean_all) 
}
# convert our list of data frames to a single data frame
results_df <- bind_rows(results_df)


#plot

if (!require("RColorBrewer")) {
install.packages("RColorBrewer")
library(RColorBrewer)
}
blups <- brewer.pal(9,"RdBu")


mse_pm10_knn <- mean((results_df$y_hat - results_df$y_true)^2)
##kNN, k=4
#kNNPlot <- ggplot(results_df, aes(x=lon, y=lat)) + 
#geom_point(aes(size=resid, color=resid)) +
#scale_size_continuous(range=c(0.1, 1)) +
#coord_map(projection="albers", parameters = c(25, 50)) +
  #scale_color_viridis(colors=blups) +  
  #scale_color_gradientn(colors=blups)+
  #theme_map() +
  #theme_few() +
  #ggtitle("kNN Residuals")

#usa_map+
 # geom_point(data=results_df, aes(color=resid)) +
  #    scale_size_continuous(range=c(0.1, 2)) +
#  scale_color_viridis() +   
  #  scale_color_gradientn(colors=blups)+

  #ggtitle("kNN Residuals for pm10")


```
#Splitting the data to perform locfit
```{r}
set.seed(1234)
test_size <- floor(0.4 * nrow(pm10))
test_idx <- sample(seq_len(nrow(pm10)), size = test_size)
train <- pm10[-test_idx, ] %>% as_tibble()
test  <- pm10[test_idx , ] %>% as_tibble()

train_sp <- train
coordinates(train_sp) <- ~lon + lat

test_sp <- test
coordinates(test_sp) <- ~lon + lat
```
#Performing locfit
```{r}
lf_pm10 <- locfit(mean_all ~ lon + lat, data = train)
predictloc <- predict(lf_pm10, newdata= test)
mse_pm10_locfit <- mean((predictloc - test$mean_all)^2)
```

```{r}

alpha_vals <- exp(seq(2, -2, length.out=50)) # values to try 
gcv_1 <- gcvplot (mean_all ~ lon + lat , data = pm10 , alpha = alpha_vals ) %>%
  summary() %>%
  as_tibble() %>% mutate(alpha = alpha_vals)

ggplot(gcv_1, aes(x= alpha, y = GCV)) +
  geom_point() +
  ggtitle("GSV score as a function of alpha") +
  theme(plot.title = element_text(hjust = 0.5))

#From the graph, the minimum value of the GCV is obtained for an alpha value between 0 and 0.025. To make our selection of the alpha, we use the following code: 
minalpha <- gcv_1$alpha[which.min(gcv_1$GCV)]
```


```{r}
lf_pm10_minalpha <- locfit(mean_all ~ lat + lon , data = train , alpha = minalpha ) 
predictloc2 <- predict(lf_pm10_minalpha, newdata = test)
mse_pm10_locfit <- mean((predictloc2 - test$mean_all)^2)

```

#Performing the kriging, we need spatial coordinates
```{r}
train_sp <- train
coordinates(train_sp) <- ~lon + lat

test_sp <- test
coordinates(test_sp) <- ~lon + lat
```

```{r}
krige_model <- autoKrige(formula = mean_all ~ lat + lon,
            input_data = train_sp,
            new_data  = test_sp,
            model = c("Sph", "Exp", "Gau", "Ste"),
            kappa = c(0.05, seq(0.2, 2, 0.1),5, 10),
            remove_duplicates = TRUE,
            verbose = FALSE
            )
plot(krige_model)
```


```{r}
krige_df <- 
  krige_model$krige_output %>%
  as_data_frame() %>%
  rename(krige_pred = var1.pred) %>%
  select(lon, lat, krige_pred)


test$predictloc <- predictloc2
  results_df <- results_df %>%
    select(y_hat, lat, lon)
join <- full_join(test, krige_df, by = NULL, copy = FALSE, suffix = c(".test_df", ".krige_df"))

join <- merge(join, results_df, by=c("lat", "lon") , all.x =TRUE)
glimpse(join)

```

```{r}
mse_pm10_krige <-  mean((join$krige_pred - join$mean_all)^2)
```

```{r}
join <- as.data.frame(join)
meltData <- melt(join, id.vars = c("lat","lon", "mean_all"))


usa_map + geom_point(data = meltData, (aes(color=value)), size = 1.5) +
  facet_wrap(~variable) +
  ggtitle("PM 10 Predictions from Local Regression, Kriging and kNN") +
  scale_color_viridis() + 
  theme(legend.position = 'bottom') + 
  theme(plot.title = element_text(hjust = 0.5))


join$resid_locfit = join$predictloc - join$mean_all
join$resid_kriging = join$krige_pred - join$mean_all
join$resid_kNN = join$y_hat - join$mean_all


meltData2 <- melt(join, id.vars = c("lat","lon", "mean_all", "predictloc", "krige_pred", "y_hat"))

usa_map + geom_point(data = meltData2, (aes(color=value)), size = 1.5) +
  facet_wrap(~variable) +
  ggtitle("PM 10 Residuals from Local Regression, Kriging and kNN") +
  scale_color_viridis() + 
  theme(legend.position = 'bottom') + 
  theme(plot.title = element_text(hjust = 0.5))
```



#Performing the same analysis for pm25

```{r}
colnames(pm25) <- c("lat", "lon", "mean_1999", "mean_2000", "mean_2001", "mean_2002", "mean_2003", "mean_2004", "mean_2005", "mean_2006", "mean_2007", "mean_2008", "mean_2009", "mean_2010", "mean_2011", "mean_2012", "mean_2013", "mean_2014", "mean_2015", "mean_2016", "mean_2017")


#Calculating the time average of the pm values from 1999-2017, and getting them in one table
pm25$mean_all <- rowMeans(pm25[,3:19])
pm25<- pm25 %>%
  select(lat, lon, mean_all)

```

#Perform knn for pm25
```{r}
K <- 10   #k-fold
n_nearest_neigh <- 2:20
mse <- rep(NA, length(n_nearest_neigh))

for (i in 1:length(n_nearest_neigh)){
  pm25_test1 <- pm25 %>% 
    mutate(index = sample(1:n())) %>%
    mutate(fold = (index %% K) + 1)
  
  # for each sub-fold create a data frame of true and predicted y
  results_df <- vector('list', length = K) # create an empty list
  for (k_i in 1:K){
    x_test <- filter(pm25_test1, fold == k_i) %>% select(lon, lat)
    x_train <- filter(pm25_test1, fold != k_i) %>% select(lon, lat)
    y_train <- filter(pm25_test1, fold != k_i) %>% select(mean_all)
    y_test <- filter(pm25_test1, fold == k_i) %>% select(mean_all)
    y_hat <- knnreg(x_train=x_train, y_train=y_train, x_test=x_test, k=n_nearest_neigh[i])
    results_df[[k_i]] <- data.frame(y_hat = y_hat$mean_all, y_true = y_test$mean_all) 
  }
  # convert our list of data frames to a single data frame
  results_df <- bind_rows(results_df)
  # get MSE
  mse[i] <- mean((results_df$y_hat - results_df$y_true) ^ 2)
  
}
plot(n_nearest_neigh, mse, main="MSE vs. Nearest neigbor for pm25")
```


#For pm25 kNN, the nearest neighbor is 2

```{r}
#get k=2 residual
K <- 10

pm25_test1 <- pm25 %>% 
  mutate(index = sample(1:n())) %>%
  mutate(fold = (index %% K) + 1)

# for each sub-fold create a data frame of true and predicted y
results_df <- vector('list', length = K) # create an empty list

for (k_i in 1:K){
  x_test <- filter(pm25_test1, fold == k_i) %>% select(lon, lat)
  x_train <- filter(pm25_test1, fold != k_i) %>% select(lon, lat)
  y_train <- filter(pm25_test1, fold != k_i) %>% select(mean_all)
  y_test <- filter(pm25_test1, fold == k_i) %>% select(mean_all)
  lat_test <- filter(pm25_test1, fold == k_i) %>% select(lat)
  lon_test <- filter(pm25_test1, fold == k_i) %>% select(lon)
  
  y_hat <- knnreg(x_train=x_train, y_train=y_train, x_test=x_test, k=3)
  results_df[[k_i]] <- data.frame(y_hat = y_hat$mean_all, y_true = y_test$mean_all, lon = lon_test$lon, lat = lat_test, resid = y_hat$mean_all - y_test$mean_all) 
}
# convert our list of data frames to a single data frame
results_df <- bind_rows(results_df)
mse_pm25_knn <- mean((results_df$y_hat - results_df$y_true)^2)

#plot

#if (!require("RColorBrewer")) {
#install.packages("RColorBrewer")
#library(RColorBrewer)

#}
#blups <- brewer.pal(9,"RdBu")



##kNN, k=4
#kNNPlot <- ggplot(results_df, aes(x=lon, y=lat)) + 
 # geom_point(aes(size=resid, color=resid)) +
  #scale_size_continuous(range=c(0.1, 1)) +
  #coord_map(projection="albers", parameters = c(25, 50)) +
  #scale_color_viridis(colors=blups) +  
  #scale_color_gradientn(colors=blups)+
  
#theme_map() +
 # theme_few() +
  #ggtitle("kNN Residuals")

#usa_map+
 # geom_point(data=results_df, aes(color=resid)) +
  #    scale_size_continuous(range=c(0.1, 2)) +
#  scale_color_viridis() +   
  
#  scale_color_gradientn(colors=blups)+

 # ggtitle("kNN Residuals")


```

```{r}
set.seed(1234)
test_size <- floor(0.4 * nrow(pm25))
test_idx <- sample(seq_len(nrow(pm25)), size = test_size)
train <- pm25[-test_idx, ] %>% as_tibble()
test  <- pm25[test_idx , ] %>% as_tibble()

train_sp <- train
coordinates(train_sp) <- ~lon + lat

test_sp <- test
coordinates(test_sp) <- ~lon + lat
```

```{r}
lf_pm25_1 <- locfit(mean_all ~ lon + lat, data = train)
predictloc <- predict(lf_pm25_1, newdata= test)
MSE <- mean((predictloc - test$mean_all)^2)
```

```{r}

alpha_vals <- exp(seq(2, -2, length.out=50)) # values to try 
gcv_1 <- gcvplot (mean_all ~ lon + lat , data = pm25 , alpha = alpha_vals ) %>%
  summary() %>%
  as_tibble() %>% mutate(alpha = alpha_vals)

ggplot(gcv_1, aes(x= alpha, y = GCV)) +
  geom_point() +
  ggtitle("GSV score as a function of alpha") +
  theme(plot.title = element_text(hjust = 0.5))

#From the graph, the minimum value of the GCV is obtained for an alpha value between 0 and 0.025. To make our selection of the alpha, we use the following code: 
minalpha <- gcv_1$alpha[which.min(gcv_1$GCV)]
```


```{r}
lf_pm25_2 <- locfit(mean_all ~ lat + lon , data = train , alpha = minalpha ) 
predictloc2 <- predict(lf_pm25_2, newdata = test)
mse_pm25_locfit <- mean((predictloc2 - test$mean_all)^2)

```


```{r}
train_sp <- train
coordinates(train_sp) <- ~lon + lat

test_sp <- test
coordinates(test_sp) <- ~lon + lat
```


```{r}
krige_model <- autoKrige(formula = mean_all ~ lat + lon,
            input_data = train_sp,
            new_data  = test_sp,
            model = c("Sph", "Exp", "Gau", "Ste"),
            kappa = c(0.05, seq(0.2, 2, 0.1),5, 10),
            remove_duplicates = TRUE,
            verbose = FALSE
            )



plot(krige_model)
```

```{r}
krige_df <- 
  krige_model$krige_output %>%
  as_data_frame() %>%
  rename(krige_pred = var1.pred) %>%
  select(lon, lat, krige_pred)


test$predictloc <- predictloc2
  results_df <- results_df %>%
    select(y_hat, lat, lon)
join <- full_join(test, krige_df, by = NULL, copy = FALSE, suffix = c(".test_df", ".krige_df"))

join <- merge(join, results_df, by=c("lat", "lon") , all.x =TRUE)
glimpse(join)
```

```{r}
mse_pm25_krige <-  mean((join$krige_pred - join$mean_all)^2)
```

```{r}
join <- as.data.frame(join)

meltData <- melt(join, id.vars = c("lat","lon", "mean_all"))


usa_map + geom_point(data = meltData, (aes(color=value)), size = 1.5) +
  facet_wrap(~variable) +
  ggtitle("PM2.5 Predictions from Local Regression, Kriging and kNN") +
  scale_color_viridis() + 
  theme(legend.position = 'bottom') + 
  theme(plot.title = element_text(hjust = 0.5))


join$resid_locfit = join$predictloc - join$mean_all
join$resid_kriging = join$krige_pred - join$mean_all
join$resid_kNN = join$y_hat - join$mean_all


tmp<-join%>%
  rename(locfit = "resid_locfit", kriging = "resid_kriging", kNN = "resid_kNN")



meltData2 <- melt(tmp, id.vars = c("lat","lon", "mean_all", "predictloc", "krige_pred", "y_hat"))

usa_map + geom_point(data = meltData2, (aes(color=value)), size = 1.5) +
  facet_wrap(~variable) +
  ggtitle("PM2.5 Residuals from Local Regression, Kriging and kNN") +
  scale_color_gradient2() +
  theme(legend.position = 'bottom') + 
  theme(plot.title = element_text(hjust = 0.5))


```
#MSE Matrix for pm10 and pm25
```{r}
mse<- data.frame(matrix(ncol=2, nrow=3))
rownames(mse) <- c("KNN", "Kriging", "Localfit")
colnames(mse) <- c("pm10", "pm25")
mse[1,1] <- mse_pm10_knn
mse[2,1] <- mse_pm10_krige
mse[3,1] <- mse_pm10_locfit
mse[1,2] <- mse_pm25_knn
mse[2,2] <- mse_pm25_krige
mse[3,2] <- mse_pm25_locfit



```

