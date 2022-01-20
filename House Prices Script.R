require(dplyr)
require(tidyr)
require(ggplot2)
require(stringr)
require(lubridate)
require(scales)
library(DataExplorer)
require(plotly)

house_prices_original <- read.csv("C:/Users/Harun/Documents/land-registry-house-prices-MSOA.csv")

head(house_prices_original)

summary(housingclean)

sum(is.na(house_prices_original$Value)) #find all of the N/A's 

housingclean <- na.omit(house_prices_original)  #create new table without N/A's

sum(is.na(housingclean$Value))


date_column <- as.Date(housingclean$Year, format = "%m/%d/%Y") #format the date, change from character to date class in a new variable

class(date_column) #class of this variable is date

class(housingclean$Year) #class of our column is still character

housingclean$Year <- date_column #put the date variable into our table

class(housingclean$Year) #check class again and now it is date


### data explorer data introduction

housingclean %>% introduce()

str(housingclean)

summary(housingclean[,c(1,2,3,5)], Value == 'Median')

housingclean %>% plot_intro()

## separate mean, median sales

median_housing <- subset(housingclean, Measure == 'Median', select = c("Code", "Area", "Year", "Measure", "Value"))

sales_housing <- subset(housingclean, Measure == 'Sales', select = c("Code", "Area", "Year", "Measure", "Value"))

mean_housing <- subset(housingclean, Measure == 'Mean', select = c("Code", "Area", "Year", "Measure", "Value"))


### london housing by sales

s <- sales_housing %>%
  group_by(Year, Value, Measure, Area) %>%
  summarise(Count = n()) %>%
  mutate(TotalSales = Value) %>%
  arrange(desc(Count)) %>%
  
  ggplot(aes(x = Year, y = TotalSales/100, fill = TotalSales)) +
  geom_bar(stat = 'identity', color = 'indianred3') +
  scale_y_continuous(labels = comma) +
  labs(x = 'Years',
       y = 'Total Sales',
       title = 'London House Sales') +
  coord_flip() +
  theme_bw()

s
### london housing by median

median_housing %>%
  group_by(Year, Value, Measure, Area) %>%
  summarise(Count = n()) %>%
  mutate(TotalValue = Value) %>%
  mutate(Percentage = (TotalValue/(nrow(housingclean) * 100))) %>%
  arrange(desc(Count)) %>%
  
  ggplot(aes(x = Year, y = TotalValue)) +
  geom_bar(stat = 'identity', color = 'indianred3') +
  scale_y_continuous(labels = comma) +
  labs(x = 'Years',
       y = 'Total Price',
       title = 'Median London House Prices') +
  coord_flip() +
  theme_bw()



m


### Overall look at house prices in London
 
p <- housingclean %>%
  group_by(Year, Value, Measure, Area) %>%
  filter(Measure == 'Sales') %>%
  summarise(Count = n()) %>%
  mutate(TotalValue = Value) %>%
  mutate(Percentage = (TotalValue/(nrow(housingclean) * 100))) %>%
  arrange(desc(Count)) %>%
  
  ggplot(aes(x = Year, y = TotalValue)) +
  geom_bar(stat = 'identity', color = 'indianred3') +
  scale_y_continuous(labels = comma) +
  labs(x = 'Years',
       y = 'Total Price',
       title = 'London House Prices') +
  coord_flip() +
  theme_bw()

ggplotly(p)


### housing prices in dagenham and barking 003

housingclean %>%
  group_by(Year, Value, Measure, Code) %>%
  filter(Code == "E02000004") %>% #barking and dagenham 003
  summarise(Count = n()) %>%
  mutate(TotalValue = Value) %>%
  arrange(desc(Count)) %>%
  
  ggplot(aes(x = as.Date(Year), y = TotalValue, fill = factor(Code)) +
  geom_bar(stat = 'identity', ) +
  scale_y_continuous(labels = comma) + 
  labs(x = 'Years',
       y = 'Total Price of Homes',
       title = 'Housing Prices in Barking and Dagenham 003'))


### housing prices in Kensington and Chelsea 012

housingclean %>%
  group_by(Year, Value, Measure, Code) %>%
  filter(Code == "E02000588") %>% #Kensington and Cheslea 012
  summarise(Count = n()) %>%
  mutate(TotalValue = Value) %>%
  arrange(desc(Count)) %>%
  
  ggplot(aes(x = as.Date(Year), y = TotalValue)) +
  geom_bar(stat = 'identity', color = 'white', fill = 'brown') +
  scale_y_continuous(labels = comma) + 
  labs(x = 'Years',
       y = 'Total Price of Homes',
       title = 'Housing Prices in Kensington and Chelsea 012')

ggplot(data=sales_housing, aes(x=log(Value))) + 
  geom_histogram(aes(y=..density..)) + 
  geom_density()

ggplot(data=median_housing, aes(x=Value, fill = "indianred3")) + 
  geom_histogram(aes(y=..density..)) + 
  geom_density()


attach(median_housing)

names(median_housing)

class(Value)

class(Year)

plot(Year, Value, main = "Scatterplot")

cor(Value, Year)
