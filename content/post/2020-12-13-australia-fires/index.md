---
title: Australia Fires
author: ~
date: '2020-12-13'
slug: australia-fires
categories: []
tags: []
---
```{r}
Rainfall<-read.csv(here::here("dataset","2020-01-07","rainfall.csv"))
Temperature<-read.csv(here::here("dataset","2020-01-07","temperature.csv"))
Fires<-read.csv(here::here("dataset","2020-01-07","MODIS_C6_Australia_and_New_Zealand_7d.csv"))
knitr::opts_chunk$set(echo = TRUE, tidy = TRUE)
```
Using the Australia fires data from tinytuesday I will investigate the location and frequency of the fires in Australia. I will use the rainfall and MODIS datasets to explore this.

How many of these fires happened near the cities? I will examine this by counting the number of instances of fires with longitude and latitude equal to the city coordinates rounded to 0 decimal places.
```{r}
CityCoord <- Rainfall %>%
  select(city_name,lat,long)%>%
  rename(longitude=long,latitude=lat)%>%
  mutate(longitude = round(longitude,digits=0),latitude=round(latitude,digits=0))%>%
  group_by(city_name)%>%
  summarize(latitude=mean(latitude),longitude=mean(longitude))%>%
  mutate(longitude = round(longitude,digits=0),latitude=round(latitude,digits=0))
Fires %>%
  mutate(longitude = round(longitude,digits=0),latitude=round(latitude,digits=0))%>%
  left_join(CityCoord,by = c("longitude","latitude"))%>%
  na.omit(city_name)%>%
  count(city_name)%>%
  ggplot(aes(city_name,n))+
  geom_bar(stat="identity")+
  ylab("Number of nearby fires")+
  xlab("City")+
  ggtitle("Number of fires vs Nearby Cities")
```
Where have fires happened the most?
```{r}
Fires %>%
  mutate(longitude = round(longitude,digits=2),latitude=round(latitude,digits=2))%>%
  group_by(longitude,latitude)%>%
  summarize(total = n())%>%
  ungroup()%>%
  slice_max(total, n = 10)%>%
  ggplot(aes(longitude,latitude))+
  borders("world")+
  coord_quickmap(xlim = 110:160,ylim = -50:-10)+
  geom_point()
```

How has the frequency of fires changed over the 7 days?
```{r}
Fires %>%
  count(acq_date)%>%
  ggplot(aes(acq_date,n))+
  geom_line(group = 1)+
  ylab("Number of Fires")+
  xlab("Date")+
  ggtitle("Number of Fires over time")
```