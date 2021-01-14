# Load the Library 

library(dplyr)
library(ggplot2)
library(xlsx)
library(tidyr)
library(DBI)
library(formattable)
library(lubridate)
library(hrbrthemes)
library(viridis)
library(leaflet)
library(htmltools)



# Load the Data

data = read.xlsx('ANZ%20synthesised%20transaction%20dataset.xlsx; filename%2A.xlsx', as.data.frame = T,sheetIndex = 1)

# some basic insights

dim(data)

head(data)


# Preparing data to gain some insights


# check for numbers NAs in each columns

colSums(is.na(data))

# Checking how many percent of Data in a column constitute a NA input

colMeans(is.na(data))*100

# Split the lat and long data into two columns

data <- data %>% filter(long_lat != '0,0') %>%
          separate(long_lat, into = c('long', 'lat'), sep = '-')

data <-  data %>% separate(merchant_long_lat, into = c('merchant_long', 'merchant_lat'), sep = '-')




# What is the spread between credit and debit transaction?

total_pie <- aggregate(amount~movement, data, sum)

total_pie <- total_pie %>% 
              mutate(percent = round((amount/sum(amount)*100),2))
 
data$movement <- as.factor(data$movement)
data_movement_count <- summary(data$movement)

data_movement_count <- data.frame(as.list(data_movement_count))
data_movement_count<- as.data.frame(t(data_movement_count))
colnames(data_movement_count) <- c("count")
data_movement_count <- sqlRownamesToColumn(data_movement_count)
colnames(data_movement_count) <- c('movement', 'count')
data_movement_count$movement <- as.factor(data_movement_count$movement)

data_movement <- merge(total_pie, data_movement_count, by = 'movement')

data_movement <- data_movement %>%
                  mutate(Percent = round((count/sum(count))*100,2))

colnames(data_movement) <- c('Type of Transaction', 'Transaction Amount', 'Percentage Transaction Amount(%)',
                             'Number of Transaction', 'Percentage Number of Transaction (%)')

formattable(data_movement, align = c("l","r", "r","r","r"), list(
                                     'Type of Type of Transaction' = formatter("span", style = ~style(color= 'black', font.weight = 'bold')),
                                      'Transaction Amount' = color_bar("#71CA97"),
                                      'Percentage Transaction Amount(%)' = color_bar("#71CA97"),
                                      'Number of Transaction' = color_bar("#71CA97"),
                                      'Percentage Number of Transaction (%)' = color_bar("#71CA97")))


## A look into demography of customers

ggplot(data, aes(x = age, fill = gender))+
  geom_histogram(bins = 30, color ='black' )+
  facet_grid(gender~.)+
  labs(x = 'Age', y = 'Frequency', title = 'Distribution of Age of both Genders')+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'none')

Age_wise <- aggregate(amount~age+txn_description+gender, data, median)

Age_spending <- aggregate(amount~age+txn_description+gender, data, sum)

Age_spending <- filter(Age_wise, txn_description !="PAY/SALARY")

Age_Salary <- filter(Age_wise, txn_description == 'PAY/SALARY')

Age_spending$txn_description <- as.factor(Age_spending$txn_description)
Age_spending$gender <- as.factor(Age_spending$gender)

Age_Salary$gender <- as.factor(Age_Salary$gender)

ggplot(Age_Salary, aes(x = age, y = amount, color = gender, shape = gender))+
  geom_point()+
  labs(x = 'Age', y = 'Amount', title = 'Median Salary w.r.t Age and Gender' )+
  geom_smooth(method = lm, se = F)

ggplot(Age_spending, aes( x = age, y = txn_description, color = gender ))+
  geom_point(aes(size = amount))+
  labs(x = 'Age', y = 'Type of Transaction', title = 'Total sum of spending w.r.t Age and Transaction Type')


# Look into volume and amount of transaction over a course of a day

# convert date into weekdays in new column

data$days <- wday(data$date, label = T)

av_date_spending <- data %>%
                      filter(data$txn_description != 'PAY/SALARY') %>%  
                      aggregate(amount~days+gender,., sum)

av_date_spending_count <- data %>%
                            filter(data$txn_description != 'PAY/SALARY') %>%
                            aggregate(status~days+gender,.,length)

av_date_spending_count['Count'] <- av_date_spending_count$status

# drop the status column from av_date_spending count

av_date_spending_count <- subset(av_date_spending_count, select = -c(status))


av_date <- merge(av_date_spending, av_date_spending_count)

ggplot(av_date, aes(x = days, y = Count, color = gender, size = amount))+
  geom_point(alpha =0.7) +
  labs(x = 'Days', y = 'Number of transaction', color = 'Gender', 
       title = 'Number of transaction w.r.t weekdays and the size of the transactions')+
  scale_size(range = c(.1,25), name = 'Amount in $AUD')+
  theme_minimal()

# A Location based insights!

lat_long <- data[, c('long', 'lat', 'amount', 'txn_description' )]

for (i in (1:3)){
  lat_long[,i] <-  as.numeric(lat_long[,i])
  
}

for ( i in (2)){
  lat_long[,i] <- (-1)*(lat_long[,i])
  
}

lat_long <- filter(lat_long, long <200)

loc_salary <- filter(lat_long, txn_description == "PAY/SALARY")

loc_spending <- filter(lat_long, txn_description == 'POS' | txn_description == 'SALES-POS')


tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("Location of transaction of salaries to the customer")
)

loc_salary %>% 
  leaflet()%>%
  addTiles()%>%
  addCircles(weight = 1, radius =(loc_salary$amount)*50)%>%
  addControl(title, position = "topleft", className="map-title")



# ****************************************
  
  
cal <- colorFactor(palette = c('yellow', 'red'), domain = loc_spending$txn_description)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))

title1 <- tags$div(
  tag.map.title, HTML("Location of transaction of POS and SALES POS")
)  


loc_spending %>%
  leaflet()%>%
  addTiles()%>%
  addCircleMarkers(color = ~cal(loc_spending$txn_description), radius = (loc_spending$amount)*10^-2, weight = 1)%>%
  addLegend(values = ~loc_spending$txn_description, pal = cal,
            title = 'Type of transaction')%>%
  addControl(title1, position = "topleft", className="map-title")




