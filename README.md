# Charlotte Lynx Blue Line’s Impact on Air Pollution
Samirah Huda and Landon Thomas

## Research Question
What effect does the Lynx Blue Line light rail have on urban air pollution in Charlotte, South Carolina?

## Background

## Data

**PM2.5 Data from SEDAC**

We obtained daily PM2.5 concentrations for the contiguous United States at 1 km spatial resolution for the years 2000 to 2016 in GeoTIFF format from NASA’s Socioeconomic Data and Applications Center (SEDAC). We then found the coordinates of the city center for the target city and each control city, and we created a buffer with a 10 kilometer radius around each city center. Finally, we cropped and adjusted the projections of the GeoTIFF files and extracted the PM2.5 data, creating a CSV file of the data set containing the monthly average PM2.5 concentrations for each city. 

**Meteorology data from NOAA**

Landon explains work here

## Estimation Methodology & Results

**Machine Learning to Compare Predicted Baseline with Actual Data**

We used data before Charlotte’s light rail opening to predict future PM2.5 levels had the light rail not opened. We then compared the predicted PM2.5 data with actual data after the light rail opening data. This comparison allowed us to gain more insight about whether or not the PM2.5 concentrations would have been higher if the light rail had not opened. 

Machine learning methods we used include the Random Forest model and the XGBoost model. 

Create training data

``` r
set.seed(112)

shuffled<-df3 %>% sample_frac(size=1, replace=FALSE)

train<-shuffled %>%
  dplyr::slice(1:100)

test<-shuffled %>%
  dplyr::slice(101:145)
```
Estimate RMSE with the Random Forest model
``` r
model4 <- ranger(new_monthly_avg ~ ., data=train, importance='impurity')
v1 <- vip(model4)
v1

#rf model evaluation
predict_test<-predict(model4, test)

rmse_test_rf<-sqrt(mean(test$new_monthly_avg-predict_test$predictions)^2)

rmse_test_rf

#create data for the graph
openlr <- updated %>%
  filter(lr_op>0) %>%
  mutate(city=as.factor(city))%>%
  dplyr::select(-temp, -date, -meanpm25) %>%
  dplyr::select(-lr_op, -lr_month)

# rf model with all data
predict_test<-predict(model4, openlr)

rf_pred<-predict_test$predictions

rf_df<-cbind(openlr, rf_pred)
```
Estimate RMSE with the XGBoost model
``` r
# Split data in to test and train group
set.seed(112)

shuffled<-df3 %>% sample_frac(size=1, replace=FALSE)
all_data <- df2 %>%
  dplyr::select(-lr_op)
all_data2 <- data.matrix(all_data)

train<-shuffled %>%
  dplyr::slice(1:100)

test<-shuffled %>%
  dplyr::slice(101:145)

#define predictor and response variables in training set
train_x = data.matrix(train[, -4])
train_y = train[,4]

#define predictor and response variables in testing set
test_x = data.matrix(test[, -4])
test_y = test[, 4]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model
xgb_model = xgboost(data = xgb_train, max.depth = 3, nrounds = 14, verbose = 0)
predict_xgb<-predict(xgb_model, xgb_test)
rmse_test_xgb<-sqrt(mean(test_y-predict_xgb)^2)

rmse_test_xgb

#create data for the graph
openlr <- updated %>%
  filter(lr_op>0) %>%
  mutate(city=as.factor(city))%>%
  dplyr::select(-temp, -date, -meanpm25) %>%
  dplyr::select(-lr_op, -lr_month)

openlr_matrix <- data.matrix(openlr)

all_x = data.matrix(openlr_matrix[, -4])
all_y = openlr_matrix[, 4]

xgb_all = xgb.DMatrix(data = all_x, label = all_y)

# rf model with all data
predict_all_xgb<-predict(xgb_model, xgb_all)

xgb_df<-cbind(rf_df, predict_all_xgb)
```

Plot predicted PM2.5 with actual data
``` r
openlr <- updated %>%
  filter(lr_op>0)

lr_removed <- df2 %>%
  dplyr::select(-lr_op)

# rf model with all data
model5 <- ranger(new_monthly_avg ~ ., data=lr_removed, importance='impurity')
predict_test<-predict(model4, lr_removed)

# xgb model with all data
matrix <- data.matrix(lr_removed)

my <- df2 %>%
  dplyr::select(lr_month, month, year, city)

pred_df<-merge(xgb_df, my, by=c("month","year", "city"), all.x=TRUE)

p <- ggplot() + 
  geom_line(data = df2, aes(x = lr_month, y = new_monthly_avg, color = "Actual Data"), size=1) + facet_wrap(~ city) +
  geom_line(data=pred_df, aes(x = lr_month, y = rf_pred, color = "RF Counterfactual"), size=1) + facet_wrap(~ city) +
  geom_line(data=pred_df, aes(x = lr_month, y = predict_all_xgb, color = "XGB Counterfactual"), size=1) + facet_wrap(~ city) +
  xlab("Months since light rail opening") + ylab("Mean PM2.5 (μg/m3)") + geom_vline(xintercept=0, linetype="dashed")+theme_bw()
```

**Comparing PM2.5 Changes with Counterfactual Cities**

Landon work on this part

## Conclusion

## References

Cite papers in APA format

## 
