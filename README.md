# Charlotte Lynx Blue Line’s Impact on Air Pollution
Samirah Huda and Landon Thomas

## Research Question

## Background

## Data

**PM2.5 Data from SEDAC**

Samirah explain how you get PM2.5 data for each city here

**Meteorology data from NOAA**

We complied 3 daily weather variables, average wind, precipitation, and
average temperature in 4 cities from 2000 to 2016. The weather variables
were recorded by NOAA from monitoring stations at the airport in each
city.

## Estimation Methodology & Results

**Machine Learning to Compare Predicted Baseline with Actual Data**

Samirah to work on this part

We use data before Charlotte’s light rail opening to predict future
PM2.5 levels had the light rail not opened. We then compare the
predicted PM2.5 data with actual data after light rail opening data.

Machine learning methods we used include….

Create training data

``` r
1 + 1
```

    [1] 2

Estimate RMSE from each algorithm

Plot predicted PM2.5 with actual data

**Comparing PM2.5 Changes with Counterfactual Cities**

We gathered a list of cities with similar characteristics to Charlotte
and downloaded EPA PM2.5 data for each of the cities to gather a rough
idea of which city had a similar trend to Charlotte, before receiving
the treatment. This allowed us to build a counterfactual control city to
estimate the effect of the treatment. The Synth package was used in the
creation of the counterfactual.

Importing the daily EPA PM2.5 data.

``` r
for(x in 2000:2016){
  assign(paste0("df",x), read.csv(paste0("/Users/landonthomas/Desktop/Junior Year/Fall Semester/F23 PRM/Data/daily_88101_",x,".csv")))
}
```

Adjusting county and state codes for cities that span multiple counties
or states.

``` r
dfMonth <- dfChar %>%
  select(Arithmetic.Mean, State.Code, County.Code, Latitude, Longitude, City.Name, Date.Local) %>%
  mutate(Year = as.numeric(substr(Date.Local, 0, 4))) %>%
  mutate(Month = as.numeric(substr(Date.Local, 3, 4))*12+as.numeric(substr(Date.Local, 6,7))) %>%
  mutate(County.Code = ifelse(City.Name == "Columbus", 87, County.Code)) %>%
  mutate(County.Code = ifelse(City.Name == "Jacksonville", 31, County.Code)) %>%
  mutate(County.Code = ifelse(City.Name == "Kansas City", 209, County.Code)) %>%
  mutate(State.Code = ifelse(City.Name == "Kansas City", 20, State.Code))

dfMonth2 <- dfMonth %>% 
  group_by(Month, City.Name, County.Code) %>%
  summarize(PM = mean(Arithmetic.Mean))
```

Creating a balance panel to combine with cleaned weather data.

``` r
vector <- as.data.frame(table(dfMonth2$Month))

County.Code <- unique(dfMonth2$County.Code)

Month <- unique(dfMonth2$Month)

bp <- crossing(County.Code, Month)

bp2 <- merge(bp, dfMonth2, by=c("Month","County.Code"), all.x = TRUE)

meandf <- dfMonth2 %>%
  group_by(County.Code, City.Name) %>%
  summarize(meanAllTime = mean(PM)) %>%
  rename(CN=City.Name)

bp3 <- merge(bp2, meandf, by=c("County.Code"), all.x = TRUE)


bp4 <- bp3 %>%
  mutate(PM = ifelse(is.na(PM), meanAllTime, PM)) %>%
  mutate(City.Name=ifelse(is.na(City.Name), CN, City.Name))

bp5 <- bp4 %>%
  mutate(lr_op = ifelse(Month > 7*12+11, 1, 0))
```

Cleaning weather data.

``` r
weather2 <- weather %>%
  mutate(first = word(NAME, 1))

weather3 <- weather2 %>%
  mutate(City.Name = ifelse(first == "JOHN", "COLUMBUS", first)) %>%
  mutate(City.Name = ifelse(first == "KANSAS", "KANSAS CITY", City.Name)) %>%
  mutate(City.Name = ifelse(first == "ST", "TAMPA", City.Name)) %>%
  mutate(City.Name = ifelse(first == "MCCARRAN", "LAS VEGAS", City.Name))

weather4 <- weather3 %>%
  mutate(nameLower = tolower(City.Name)) %>%
  mutate(Month=as.numeric(substr(DATE, 3, 4))*12+as.numeric(substr(DATE, 6,7))) %>%
  dplyr::select(TAVG, AWND, PRCP, nameLower, Month)
```

Combining weather and PM2.5 data for each city.

``` r
prep2 <- bp5 %>%
  mutate(nameLower = tolower(City.Name))

prep3 <- merge(prep2, weather4, by = c("nameLower", "Month") , all.x=TRUE)

prep4 <- prep3 %>%
  mutate(T = ifelse(City.Name == "Charlotte" & lr_op == 1, 1 , 0)) %>%
  filter(County.Code!=163 & County.Code!=7)
```

Creating the synthetic control using the Synth package.

``` r
library("Synth")
```

    ##
    ## Synth Package: Implements Synthetic Control Methods.

    ## See https://web.stanford.edu/~jhain/synthpage.html for additional information.

``` r
prep4 <- read.csv("prep4.csv")
dataprep.out <- dataprep(prep4, 
                         time.predictors.prior = 1:(8*12),
                         predictors.op = "mean",
                         predictors = c("AWND", "PRCP"),
                         dependent = "PM",
                         unit.variable = "County.Code",
                         unit.names.variable = "City.Name",
                         time.variable = "Month",
                         treatment.identifier = 119,
                         controls.identifier = c(3,31,57,61,79,87,95,97,209),
                         time.optimize.ssr = 1:(8*12),
                         time.plot = 1:12*16)

synth.out <- synth(dataprep.out)
```


    X1, X0, Z1, Z0 all come directly from dataprep object.


    **************** 
     searching for synthetic control unit  
     

    **************** 
    **************** 
    **************** 

    MSPE (LOSS V): 12.04959 

    solution.v:
     3.46696e-05 0.9999653 

    solution.w:
     0.1137685 0.2627167 0.04326885 0.3113394 0.06198184 0.05731318 0.04390384 0.04537258 0.06033523 

``` r
print(synth.tables   <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth.out)
)
```

    $tab.pred
         Treated Synthetic Sample Mean
    AWND   2.515     3.175       3.575
    PRCP  85.531    85.530      83.477

    $tab.v
         v.weights
    AWND 0        
    PRCP 1        

    $tab.w
        w.weights   unit.names unit.numbers
    3       0.114    Las Vegas            3
    31      0.263 Jacksonville           31
    57      0.043        Tampa           57
    61      0.311   Cincinnati           61
    79      0.062    Milwaukee           79
    87      0.057     Columbus           87
    95      0.044      Orlando           95
    97      0.045 Indianapolis           97
    209     0.060  Kansas City          209

    $tab.loss
               Loss W   Loss V
    [1,] 3.636059e-05 12.04959

Graphing the synthetic control versus Charlotte with the light rail
opening date.

``` r
path.plot(synth.res    = synth.out,
          dataprep.res = dataprep.out,
          Ylab         = c("PM"),
          Xlab         = c("Month"),
          Legend       = c("Charlotte","Synthetic State Charlotte"),
          Legend.position = c("topleft")
)

abline(v   = 8*12,
       lty = 2)
```

![](README_files/figure-commonmark/unnamed-chunk-9-1.png)

A linear combination of the cities using a weighted average, with
Cincinnati having the most weight, predicts the PM2.5 trends in
Charlotte if the city hadn’t received the treatment. One limitation of
the model is there are some big spikes in the PM2.5 levels in Charlotte
that the synthetic control city doesn’t account for, so some of the
seasonality variation hasn’t been captured by the model.

## Conclusion

## References

Cite papers in APA format

## 
