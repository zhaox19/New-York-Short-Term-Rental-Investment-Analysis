# New-York-Short-Term-Rental-Investment-Analysis
Provided an optimized short-term rental investment plan for a real estate company



---
title: "Data Challange"
author: "Ximin Zhao"
date: "1/18/2020"
output:
  word_document: default
  pdf_document: default
  html_document: default
---
# Abstract

Base on the cost and revenue data set we were given, the goal of this data challenge is about getting the zip code in New York of short term rental properties that are best to invest in. I separated this data challenge task into some sub-steps which include data cleaning, data munging, visualization and what's next.


# Key Assumptions

* The occupancy rate is always 75%.
* The investor will pay for the property in cash (i.e. no mortgage/interest rate will need to be accounted for).
* The time value of money discount rate is 0% (i.e. $1 today is worth the same 100 years from now).
* All properties and all square feet within each locale can be assumed to be homogeneous (i.e. a 1000 square foot property in a locale such as Bronx or Manhattan generates twice the revenue and costs twice as much as any other 500 square foot property within that same locale.
* The cost of properties in 2019 can be represented by the 2017 value. (If assume in 2020 the error will increase)
* The company will invest in those properties in 2019. 
* Objective elements won't affect the revenue, for example, customers' reviews, neighborhood environment, host (rules, easy to contact, attitude, cancellation policy, easy to book), and traffic convenience. (Applied when filtering the Airbnb data)
* Nightly price and cleaning fee will keep the same in the next several years. (Applied when calculating revenue)
* Assume customers will only stay the minimum amount of nights that the host is willing to rent out the property. (Applied when calculating the cleaning fee)
* The only source of revenue is the nightly price, clean fee, and extra people fee. The only cost is the sale price of the property. Ignore all other types of revenue and cost. (Applied when calculating the revenue and break even point)
* The clean fee will be charged and added to the final rental price. (Applied when calculating the revenue and break-even point)
* All customers will bring guests. The number of guests will always reach the maximum guests included. (Applied when calculating the revenue and break-even point)
* 9999 in price equals to NA. (Applied to outlier analysis for Airbnb data)
* The trend of the house price won't change after 2017. (Applied to revenue and house price trend plots)


# Load Packages And Import Data

```{r , message=FALSE, warning=FALSE}
# Import Libraries
library(tidyverse) # Tidying Data
library(ggrepel) # ggplot theme
# Import Data (Zillow Data and Airbnb Data)
Adata<-read_csv("E:\\C1DataChallange\\listings.csv")

Zdata<-read_csv("E:\\C1DataChallange\\Zip_Zhvi_2bedroom.csv")
```

# Data Quality Check, Data Cleaning And Data Munging

## Data Quality Issues

* Some of the data have inconsistency and bad accuracy, for example, some properties that have state and city in New York, but the market variables show they are in California. And some properties have 0 bed, but the bed type is "real bed".

* There are a lot of missing values in variables such as square_feet, weekly_price, monthly_price. They are all over 50% of the amount of data, so I decided to drop these 3 variables. Others like cleaning_fee, review_scores there is about 20% missing value in the data set, so I impute them with the median.

* There are some outliers in the price column. Some properties have a nightly price over 3000 dollars are considered as outliers and some properties have 3 million dollar cost are also considered as outlier. There are two properties have nightly price equals to 9999, I assume they are NA values and drop these two rows.


## Airbnb Data Cleaning and Munging

* Using unique() function to take a look at the different levels of state column. Notice that there are NY, Ny, ny, and New York all represent New York. Using str_replace_all to replace those patterns by "NY". Base on the unique() function result, we know that this data set is updated until 2019.

```{r,echo=T,results="markup"}
unique(Adata$state)

unique(Adata$last_scraped)

NY<-c("Ny|ny|New York")
Adata$state<-str_replace_all(Adata$state,NY,"NY")
```

* Filter the Airbnb data that only contain 2 bedroom properties in New York. There are some properties with the state in NY, but market in other states. That is a data quality issue.

```{r,echo=T, results='hide'}
FilteredAdata<-Adata%>%filter(bedrooms==2)%>%filter(state=="NY")%>%filter(market=="New York")
```

* Base on the assumptions, subjective elements won't affect the revenue such as customers' reviews, neighborhood environment, host (rules, easy to contact, attitude, cancellation policy, easy to book), and traffic convenience. Only select columns from Airbnb data that may affect the final decision making.

```{r,echo=T,tidy=TRUE,results='hide'}
FilteredAdata<-FilteredAdata%>%select(id,
                                      neighbourhood,neighbourhood_group_cleansed,
                                      zipcode,latitude,longitude,is_location_exact,
                                      property_type,room_type,accommodates,
                                      square_feet,price,weekly_price,monthly_price,
                                      cleaning_fee,guests_included,extra_people,
                                      minimum_nights,maximum_nights,availability_30,
                                      availability_60,availability_90,availability_365,
                                      number_of_reviews,first_review,last_review,
                                      review_scores_rating,review_scores_accuracy,
                                      review_scores_checkin,review_scores_communication,
                                      review_scores_value,reviews_per_month)
```

* Check the data quality, missing values, duplicated rows, negative values, outliers, and bad data. Showing the percentage of missing values in every column of the Airbnb data set (drop the whole columns if the number of missing values is over 50%).

```{r,echo=T, results='hide'}
TotalNumOfNA <-sapply(FilteredAdata, function(x) sum(length(which(is.na(x)))))  
NAPercent<-as.data.frame(TotalNumOfNA/nrow(FilteredAdata)*100)

NAPercent<-rownames_to_column(NAPercent,var="variable_name")
names(NAPercent)[2]<-"NA_Percentage"
print(NAPercent)

var<-NAPercent%>%filter(NA_Percentage>50)%>%pull(variable_name)
FilteredAdata <- FilteredAdata%>%select(-var)
```

* Impute NA values with the median for 26 to 31 columns (Review_scores). For better imputing, the NA values in review_score_rating, perform sentiment analysis to some customer reviews data may be a good option, rearrange the result and then score them in the range from 1 to 100 and use that as a reference to impute the NA values would be more accurate. 

```{r,echo=T, results='hide'}
ReviewScore<-c("review_scores_rating","review_scores_accuracy","review_scores_checkin","review_scores_communication","review_scores_value","reviews_per_month")
for (i in 1:length(ReviewScore)){
                FilteredAdata[ReviewScore[i]]<-imputeMissings::impute(FilteredAdata[ReviewScore[i]],method = "median/mode")
}
```

* Convert price, cleaning_fee, and extra_people into numeric for later calculation, but first delete the dollar sign and comma.

```{r,echo=T, results='hide'}
pricev<-c("price","cleaning_fee","extra_people")
for (i in 1:length(pricev)){
        FilteredAdata[pricev[i]]<-sapply(FilteredAdata[pricev[i]],function(x) gsub("\\$","",x))
        FilteredAdata[pricev[i]]<-as.numeric(sapply(FilteredAdata[pricev[i]],function(x) gsub("\\,","",x)))
        }
```

* Impute NA values in cleaning_fee by its median.

```{r,echo=T, results='hide'}
FilteredAdata["cleaning_fee"]<-imputeMissings::impute(FilteredAdata["cleaning_fee"],method = "median/mode")
```

* After checking, there is no duplicated row in this data set.

```{r,echo=T, results='hide'}
Duplicate<-FilteredAdata[which(duplicated(FilteredAdata) ==T),]
```

* After checking there is no negative or zero row in this data set.

```{r,echo=T, results='hide'}
# Get all numeric columns
FilteredAdataNu<-select_if(FilteredAdata, is.numeric)
# Drop latitude and logitude column because they are ok to have negative values. Drop zipcode because there are some NA values, but merge data later will handle it.
FilteredAdataNu<-FilteredAdataNu%>%select(-c(zipcode,longitude,latitude))
# Check other columns and see if there are negative values in them
NegColumn<- apply(FilteredAdataNu, 2, function(col) any(col < 0))
any(NegColumn==TRUE)
```

* Check outliers in nightly price. This plot is just for showing the outliers of price please ignore the messy.

```{r,echo=FALSE,fig.width=12, fig.height=8}
ggplot(FilteredAdata,aes(reorder(as.factor(zipcode),price),price),na.rm=T)+
        geom_point()+
        xlab("Zipcode")+
        ylab("Price")+
        scale_y_continuous(breaks=seq(0,10000,500))+
        theme(panel.background=element_blank(),panel.grid=element_blank())
```


* It is interesting to see there are several properties that have a nightly price of over $2500. The maximum price of them almost reaches 10000 dollars per night?! I doubt the reality of this data. Take a look at the data and I found out the value of those prices is 9999. Assume 9999 represents NA. Therefore I drop these two rows in case it may affect the final result.

```{r,echo=T, results='hide'}
out<-which(FilteredAdata$price>7500)
print(out)
fi<-FilteredAdata[out[1],"price"]
se<-FilteredAdata[out[2],"price"]
```

```{r,echo=T, results='hide'}
FilteredAdata<-FilteredAdata[-c(out[1],out[2]),]
```

## Zillow Data Cleaning and Munging

* Extract data only for New York properties. Using unique() function to see if there is any other format that represents New York and NY in the data set.

* There are lots of NA values in house price from 1996-04 to 2007-05. I decided to drop them and only left 2007-06 to 2017-06 10 years of data.

```{r,echo=T, results='hide'}
FilteredZdata<-Zdata%>%filter(City=='New York')%>%filter(State=="NY")%>%
                       filter(Metro=="New York")%>%select(-c(RegionID,City:Metro,`1996-04`:`2007-05`))
```

* Change the RegionName into factor data rather than character data. Also, change the "RegionName" into "zip code" for the sake of merging tables.

```{r,echo=T, results='hide'}
FilteredZdata$RegionName<-as.numeric(FilteredZdata$RegionName)

names(FilteredZdata)[names(FilteredZdata)=="RegionName"]<-"zipcode"
```
 
* Check the NA value in the filtered Data. There is no NA values in the data set.

```{r,echo=T, results='hide'}
TOTNAZ <-as.data.frame(sapply(FilteredZdata, function(x) sum(length(which(is.na(x))))))
names(TOTNAZ)[1]<-"Total NA"
```

* Check and see if there are negative numbers in every numeric columns. There are no negative values in numeric columns.

```{r,echo=T, results='hide'}
# Get all numeric columns
FilteredZdataNu<-select_if(FilteredZdata, is.numeric)
# Check and see if there are negative values in them
NegColumnZ<- apply(FilteredZdataNu, 2, function(col) any(col < 0))
any(NegColumnZ==TRUE)
```

* It is helpful to have a basic idea about what the trend looks like for the house price in 10 years. Rather than visualize every month's data, I decided to use yearly data by obtaining the median price of each 12 months. let the median be the house price of that year. For obtaining more accurate house price data, we can do time series forecasting. However, there are many issues with that approach, first, for example, if we use the monthly data, we have 25 empty values in the time series (assume the estate company will invest in July 2019, this assumption is just for the statement here, it doesn't apply to this project. ) this will leads to big error due to the uncertainty and large time gap. Second, if we use the median price of 12 months, we have 2 empty values in the times series need to predict, but we only have eleven data points to build up our forecasting model which will also introduce a relatively big error. Third, building up a forecasting model and figure out the solution for all these problems is time-consuming. In addition, according to the plot below, the house prices had not fluctuated dramatically in the last 3 years. Therefore, I decided to directly use the 2017 house price to represent the 2019 house price in New York

```{r,echo=T, results='hide'}
sy<-2007
for (i in sy:2017){
        sy<-as.character(i)
# Get the columns that have 2007-,2008-.... pattern in their names
        tb<-FilteredZdata[, grepl( sy , names( FilteredZdata ))]
# Get the median cost for a unit in specific area and year
        FilteredZdata[sy] <- apply(tb,1, median)
        FilteredZdata<-select(FilteredZdata,-c(colnames(tb)))}

yaz<-as.data.frame(FilteredZdata%>%select(zipcode,`2007`:`2017`))
yazt <- as.data.frame(t(yaz))
colnames(yazt)<-yazt[1,]
yazt<-yazt[-1,]
year <- c(2007:2017)
yazt<-data.frame(year=year,yazt)
colnames(yazt) <- gsub("[A-Z]", "", colnames(yazt))
colu<- names(yazt)[2:26]
yaztmelt<-reshape2::melt(yazt,id="year")
yaztmelt$variable <- as.factor(gsub("[A-Z]", "",yaztmelt$variable))
```

* House Price VS Zipcode For 2007-2017.

```{r,echo=FALSE,fig.width=12, fig.height=8}
theme_bl<-theme(panel.background=element_blank(),
                        panel.grid=element_blank())
ggplot(yaztmelt,aes(x=year,y=value,group=variable,color=variable))+
                # Using line plot
                geom_line()+
                # insert points on the line plot
                geom_point()+
                # Change the title of the legend from "variable" into "Zipcode"
                labs(color = "zip code")+
                # Change the x axis title to Year
                xlab("Year")+
                # Change the y axis title to Price in Dollar
                ylab("Price in Dollar")+
                # Change the plot title to Trend of House Price of Different Area in NewYork
                ggtitle("Trend of House Price of Different Zip Codes in New York")+
                # Center the plot title and make its font in bold
                theme(plot.title = element_text(hjust = 0.5,face="bold",size=20))+
                # Change the interval of x axis and the distance between gap
                scale_x_continuous(breaks=seq(2007,2017,2))+
                # Bold the x and y axis
                theme(axis.text=element_text(face="bold"),
                      axis.line.x = element_line(color="black", size = 0.5),
                      axis.line.y = element_line(color="black", size = 0.5))+
                # Edit the tick marks of y axis
                scale_y_continuous(labels = scales::comma)+
                theme_bl
```

* Change the name of 2017 into Property_Sale_Price and treat it as the price of house in 2019.

```{r,echo=T, results='hide'}
FilteredZdata<-FilteredZdata[,-c(4:13)]
names(FilteredZdata)[4]<-"Property_Sale_Price"
```

## Merge And Finalize Data

* Merge the filtered Airbnb data and filtered Zillow data

```{r,echo=T, results='hide'}
Merged<-merge(FilteredAdata,FilteredZdata, by.y="zipcode")
```

* Because some of the types of rooms are private room which means the houses were rent by the room. If there are two bedrooms in the unit, then the price, clean fee, guests_included and accommodate should be doubled.

```{r,echo=T, results='hide'}
for (i in 1:nrow(Merged)){
        if(Merged$`room_type`[i]=="Private room"){
                Merged$price[i]<-2*Merged$price[i]
                Merged$cleaning_fee[i]<-2*Merged$cleaning_fee[i]
                Merged$guests_included[i]<-2*Merged$guests_included[i]
                Merged$accommodates[i]<-2*Merged$accommodates[i]
                
        }}
```

* Assume customers will always bring the maximum allowed number of guests. This will generate a fee for each extra person and bring different revenue. Therefore, create a new column to show the total extra guests fee. The total extra fee equals to the guests_included times the number of extra people the guest bring in.

```{r,echo=T, results='hide'}
Merged$Total_Extra_Fee <- Merged$guests_included * Merged$extra_people
Merged<-Merged%>%select(-c("guests_included","extra_people"))
```

* Although, I assumed the occupancy rate is 75% at the beginning. However, I still curious that, if the occupancy rate is not always 75% for different areas, what is the final result looks like? Therefore, I assume the occupancy rate equal to 365-availability_365 which is the number of the booked day of the next year. I create a new data frame called MergedAvailability just for this situation I assumed. This is used in break-even analysis below assume occupancy rate is not always 75%. (Note: this assumption is not for the whole project, it is just for the plot which considers availability below)

* There are some 0's in the RentOutDay_NextYear column because many guests did not book the room for the next year yet. However, if those are used to calculate the break-even point, infinity will be in the result, because no rent out means no revenue. Therefore, I drop those 226 rows and only left those properties that have the break-even period less than 100 years.

```{r,echo=T, results='hide'}
Merged$RentOutDay_NextYear<-365-Merged$availability_365
MergedAvailability<-Merged
MergedAvailability$BreakEvenPoint_availability<-
        Merged$Property_Sale_Price/((Merged$price+Merged$Total_Extra_Fee)*Merged$RentOutDay_NextYear+(Merged$cleaning_fee*Merged$RentOutDay_NextYear/Merged$minimum_nights))

MergedAvailability<-MergedAvailability[!(MergedAvailability$BreakEvenPoint_availability>=100),]

```

* Before making visualization, check NA again. Although, there are some NA's in review_time, they won't affect the final result. 

* Convert zipcode, id, neighbourhood, neighbourhood_group_cleansed, property_type, room_type into factors for plotting and future use.

```{r,echo=T, results='hide'}
NAC <-as.list(sapply(Merged, function(x) sum(length(which(is.na(x))))))
cols<-c("zipcode","id","neighbourhood","neighbourhood_group_cleansed","property_type","room_type")
Merged[cols]<-lapply(Merged[cols],as.factor)
```

# Visualization

* The bar plot shows there are 1016 properties located in Manhattan, 26 properties locate in Staten Island, 500 in Brooklyn and 16 in Queens. It is clear to see that zip code: 11215,10036,10003,10025, and 11217 has the most number of properties. They are all located in Brooklyn and Manhattan. Notice that there is one property that is located at zip code 10013, but it is in Brooklyn rather than Manhattan.

* More properties means higher demand and higher occupancy rate, but it also means higher competition and higher the property cost. However, base on the trend line of the house price, the worth of the house in Manhattan and Brooklyn have a high probability to increase in the next several years. This should also be considered as one of the major profit if the estate company considers selling their properties in the future.

```{r,echo=FALSE,fig.width=12, fig.height=8}
ggplot(Merged,aes(zipcode,fill=neighbourhood_group_cleansed))+
        geom_bar()+
        geom_text(stat='count', aes(label=..count..), vjust= -0.3) + 
        labs(x = "zip code", y = "Number of Properties",fill="Property Location") +
        theme_bw() + 
        theme(axis.text.x = element_text(angle = 90),plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank())+
        theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))
```

* Assume occupancy rate equal to 75%, the price per night will not change in the future and the company will not sell these properties. Base on the break-even plot below, the top 5 zip codes are 10308,10314,10306,10309,11234. They will achieve the break-even point shorter than properties locates in other areas. The break-even point is also called the payback period, it is calculated by the house price divided by revenue. The shorter the payback period, the shorter the investment will start making profit.

```{r,echo=FALSE,fig.width=12, fig.height=8}
Merged$BreakEvenPoint<-
        Merged$Property_Sale_Price/((Merged$price+Merged$Total_Extra_Fee)*0.75*365+(Merged$cleaning_fee*0.75*365/Merged$minimum_nights))

ggplot(Merged,aes(reorder(zipcode,BreakEvenPoint,FUN=median),BreakEvenPoint,fill=neighbourhood_group_cleansed))+
        geom_boxplot()+
        labs(x="zip code",y="Break-Even Year", fill="Borough")+
        ggtitle("Break-Even vs Zip Code With 75% Occupancy Rate For All Properties")+
        theme(plot.title = element_text(hjust = 0.5,face="bold",size=20))+
        theme(axis.text.x = element_text(angle = 90),plot.background = element_blank(),panel.grid.major = element_blank())
```

* Assume occupancy rate can be represented by the booking demand in the next year and the estate company will never sell their properties. Assume customer can't cancel their booking. There are more outliers in the plot because many guests did not book the room for the next year yet. Base on this pack back period plot the top 5 zip codes worth to invest are 10314,10304,10309,11434,10303. （Note: This is the only plot that doesn't follow the 75% occupancy rate assumption）

```{r,echo=FALSE,fig.width=12, fig.height=8}
ggplot(MergedAvailability,aes(reorder(zipcode,BreakEvenPoint_availability,FUN=median),BreakEvenPoint_availability,fill=neighbourhood_group_cleansed))+
        geom_boxplot()+
        labs(x="zip code", y="Break-Even Year", fill="Borough")+
        ggtitle("Break-Even vs Zip Code Base On Future Occupancy Rate")+
        theme(plot.title = element_text(hjust = 0.5,face="bold",size=20))+
        theme(axis.text.x = element_text(angle = 90),plot.background = element_blank(),panel.grid.major = element_blank())
```

* We can see from the two plots below, properties that cost more, its nightly price is higher too. For the Manhattan area, 10013 has the highest median of nightly price and 10021 has the lowest sale price. For the Brooklyn area, 11201 has the highest median of nightly price and 11234 has the lowest sale price. For Staten Island, 10308 has the highest median of nightly price and 10309 has the lowest sale price.

```{r,echo=FALSE,fig.width=14, fig.height=8}
ggplot(Merged,aes(reorder(zipcode,price),price,fill=neighbourhood_group_cleansed))+
        geom_boxplot()+
        labs(x="zip code",y="Nightly Price", fill="Borough")+
        ggtitle("Nightly Price Vs Zip Code")+
        theme(plot.title = element_text(hjust = 0.5,face="bold",size=20))+
        theme(plot.background = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 90))+
        scale_y_continuous(breaks=seq(0,4000,500))
```


```{r,echo=FALSE,fig.width=14, fig.height=8}

ggplot(Merged,aes(zipcode,Property_Sale_Price,fill=neighbourhood_group_cleansed))+
        geom_col()+
        labs(x="zip code",y="Sale Price", fill="Borough")+
        ggtitle("Sale Price Vs Zip Code")+
        theme(plot.title = element_text(hjust = 0.5,face="bold",size=20))+
        theme(plot.background = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 90))+
        scale_y_continuous(labels = scales::comma)

```

* The box plot below shows the revenue per year of properties can earn in the different zip code areas. The trend line plot below shows the trend of house prices in different areas base on time. For picking the best zip code, we need to choose the zip code that has higher revenue and a larger slope of trend line which indicates that the house price may increase a lot in the future. It is easy to see that properties located in Manhattan (10013) receive the highest revenue, but its house price is too high. At the same time, it's property cost is flatten out which means if the company considers selling these properties in the future, they won't receive a large profit for selling the houses. It is ok to say under those assumptions, if the estate company has enough cash, they should invest in those properties in Manhattan and Brooklyn. If they don't have enough cash to buy properties in these areas, they can still consider investing in the staten island area so that they can buy a lot more units. Although the worth of those properties won't increase a lot in the near future, they will achieve the break-even point in a relatively short period of time. Also, the company will pay much less to invest in those cheaper properties so that they can buy more units for rent, and this accumulating profit rate may overpass only buy a few units in Manhattan and Brooklyn.

* Base on the comparison and analysis, if estate company have enough money to invest, they can choose the zip code in Manhattan: 10011,10014,10003 or in Brooklyn: 11201,11234 or in Staten Island: 10308

```{r,echo=FALSE,fig.width=14, fig.height=8}
Merged$Revenue<-(Merged$price+Merged$Total_Extra_Fee)*0.75*365+(Merged$cleaning_fee*0.75*365/Merged$minimum_nights)


ggplot(Merged,aes(reorder(zipcode,Revenue,FUN=median),Revenue,fill=neighbourhood_group_cleansed))+
        geom_boxplot()+
        labs(x="zip code",y="Revenue Per Year", fill="Borough")+
        ggtitle("Revenue Vs Zip Code")+
        theme(plot.title = element_text(hjust = 0.5,face="bold",size=20))+
        theme(plot.background = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 90))
```


```{r,echo=FALSE,fig.width=12, fig.height=8}
ggplot(yaztmelt,aes(x=year,y=value,group=variable,color=variable))+
                                 # Only show the trend line to predict the 2019 price
                                 geom_smooth(method = 'loess',se=FALSE,size=0.01)+
                                 labs(color = "zip code")+
                                 # Change the x axis title to Year
                                 xlab("Year")+
                                 # Change the y axis title to Price in Dollar
                                 ylab("Sale Price of Houses")+
                                 #Change the plot title to Trend of House Price of Different Area in New York
                                 ggtitle("Trend of House Price of Different Areas in New York")+
                                 #Center the plot title and make its font in bold
                                 theme(plot.title = element_text(hjust = 0.5,face="bold",size=20))+
                                # Change the interval of x axis and the distance between gap
                                 scale_x_discrete(breaks=seq(2007,2017,2),labels=seq(2007,2017,2))+
                                # Bold the x and y axis
                                 theme(axis.text=element_text(face="bold"),
                                 axis.line.x = element_line(color="black", size = 0.5),
                                 axis.line.y = element_line(color="black", size = 0.5))+
                                # Edit the tick marks of y axis
                                 scale_y_continuous(labels = scales::comma)+
                                 theme_bl+
                                 geom_text_repel(data = subset(yaztmelt, year == "2017"), aes(label = variable, colour = variable, x = 2017, y = value))+
                                 theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))

```

# Key Insights and Conclusions

* The company can invest in Manhattan and Brooklyn area. Those two areas are very popular for tourism and the house price in these areas are still increasing. Looking at the properties in Queen and Staten Island, their house price is cheap and don't have a large rising trend. If invest in those properties, the company can expect the main revenue comes from the rent.

* If the company considers they may sell the properties in the future, and they have enough money to invest, the top zip code they should consider is 10011,10014,10003,11201,11234.

* If the company considers they will sell the properties in the future, but they don't have enough money to invest, zip code 10308 is a great choice.

* If the company would like to recieve a shorter payback , they can invest in zip code:10308,10314,10306,10309,11234

# What's Next?

1. Doing sentiment analysis of those customer review data and consider those review_scores variables, obtain more information about the customer attitude for these properties. Check and see if the customers' reviews affect the occupancy rate and the nightly price (revenue) of properties. Combine the sentiment analysis results with the price and availability data to have a better understanding of which properties the company should invest in.

2. Obtain the updated version data. Using the newest data to perform analysis rather than using this old version cost data. If there is no new data available, doing time-series forecasting to predict the future house price(cost), nightly price, and cleaning fee (Revenue) so that the investment company can make a better decision after considering the rising or dropping of the house price. This option also requires to gather more complete historical data to minimize the error.

3. Making map plot and show the demand, nightly price distribution, profit rate distribution. There are some areas that may have a higher demand and higher nightly price for example in Manhattan or Brooklyn, it is very likely that customers will spend more to live in that area just for convenience.

4. Doing web scraping or just directly download the newest Zillow data from Zillow's website to replace the house price data rather than use the 2017 Zillow data.

5. Obtaining and considering other cost data for a more accurate result, such as business overhead, employee's salary, maintenance fee, taxes, advertisement cost, and equipment cost.

6. Go deeper into the zip code data to find out specific properties that have a short pay-back period and locate in the area such as Manhattan and Brooklyn that have higher house prices.

7. Doing some research about the social environment around the properties or zip code. For example, check the average crime rate of the area. Give them scores and add them to the data set. 

8. Optimized the code so the user can use it on updated data sets and doing analysis easily.

9. Obtain festival, holiday season, seasonal trend, and even the financial crisis cycle data and consider their effect on the cost (house price, labor cost, utility cost, etc) and revenue (nightly price)

10. Build up a machine learning model to find out the relevant parameters that may affect the cost and revenue in different zip codes. Through the model, the investor can have a better understanding of what and how the contribution of the parameters to the cost and revenue. For example, consider amenities variable, tidying data in amenities column and convert them into factor. Take those hardware facilities into account during the analysis and see if they have a positive or negative effect on revenue. For example, some properties may not have a refrigerator but some customers will need to use a refrigerator. This property will lose some customers. Also, free parking is important, especially in the Manhattan area. It will cost you a lot of parking in those areas.

11. Getting the age of the house data, take the age of those house into account. The worth of the house may affect by the age of the house, the older the house, maybe the lower the worth is and the cost to maintain the old house is higher too. Also, customers may prefer to live in a new unit rather than an old one.

12. Rather than compare the time that reaches the break-even point, it is more reasonable to compare the profit rate after reach the break-even point. For example, if "A" person spent 10 dollars bought a house, the daily revenue is 2 dollars. "B" person spent 1000 dollars bought a house and daily revenue is 100 dollars. Assume no value or daily revenue change, A needs 10/2=5 days to reach the break even point. B needs 1000/100=10 days to reach the break-even point. Once A reaches his break-even point, he starts making a profit by 2 dollars a day. When B reaches his break-even point, A has already earn 2*5=10 dollars. However, on day 11, A earns another 2 dollars, B earns 100 dollars. Right now, the total profit of A is 12 dollars, but B has received 100 dollars profit. Therefore, even A reach the break-even point shorter than B, B can still earn more money faster than A. However, it also depends on the economic situation of the estate company. If they don't have much money to buy expensive properties they can still spend its money on those cheaper properties. They can buy a lot of cheaper properties and may still have the same profit rate as buying a few very expensive properties.



# Metadata

* Major generated dataset: FilteredAdata, FilteredZdata, Merged, MergedAvailability

* FilteredAdata: Filtered Airbnb data

* FilteredZdata: Filtered Zillow data

* Merged: The data set formed by inner join FilteredAdata and FilteredZdata and add some new variables.

* MergedAvailability: The data set is based on Merged data set. One column BreakEvenPoint_Availability is added to Merged.




* Metadata For Major New Variables

* Property_Sale_Price: Median house price of 2017 in New York area. This sale price can be treated as the cost the estate company may invest.

* Total_Extra_Fee: Total extra fee for bringing extra guests into the apartment. This equals to the number of extra guests multiply by the extra fee for one additional guest.

* RentOutDay_NextYear: Total amount of rent out day of the properties. For example, if a property has 360 days available in the next year, then the rent out day (not available) is 5 days.

* BreakEvenPoint: The payback period in (year) which is calculated an under-occupancy rate equal to 75% assumption.

* Revenue: Yearly revenue. It equals to price plus total extra fee plus total cleaning fee.

* BreakEvenPoint_availability: The time to reach break-even point when assuming the next year's availability as the occupancy rate.

* zipcode (FilteredZdata. This column was RegionName in the original Zillow data set): zipcode

* variable_name (in NAPercent data set): column names of the FilteredAdata

* NA_Percentage (in NAPercent data set): percentage of missing value in every column of FilteredAdata



