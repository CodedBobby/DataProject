---
title: Volcano
author: ~
date: '2020-12-10'
slug: a-first-post
categories: []
tags: []
---
Using the volcano data from tinytuesday I will investigate volcanic eruptions and the affect they have on the environment. I will use the volcano, eruptions and sulfur datasets in order to explore this.


```{r, message=FALSE}
library(here)
library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)
knitr::opts_chunk$set(echo = TRUE, tidy = TRUE)
```

```{r}
dat1<-read.csv(here::here("dataset","volcano.csv"))
dat2<-read.csv(here::here("dataset","eruptions.csv"))
dat3<-read.csv(here::here("dataset","events.csv"))
dat4<-read.csv(here::here("dataset","sulfur.csv"))
```

Question 1: To what extent does a volcano eruption impact the atmosphere? To explore this I will attempt to determine the relation between major volcanic eruptions (defined here as any eruption with a vei of 4 or higher) and spikes in sulfur levels.
```{r}
filtered <- dat1 %>%
  left_join(dat2, by = c("volcano_number","volcano_name","longitude","latitude"))%>%
  filter(vei >= 4, start_year >= 500,start_year <= 706)
ggplot()+
  geom_line(data=dat4, aes(year, neem, color = "blue"))+
  geom_line(data=dat4, aes(year, wdc, color = "red"))+
  geom_point(data=filtered, aes(start_year,0))+
  scale_color_manual(name="Location of Ice Cores",labels=c("Greenland","Antartica"),values=c("blue"="blue","red"="red"))+
  ylab("Sulfur (ng/g)")+
  xlab("Year")+
  ggtitle("Year vs Sulfur")
```

Question 2: Are there any areas currently at risk from volcano eruptions?
```{r}
dat1 %>%
  left_join(dat2, by = c("volcano_number","volcano_name","longitude","latitude"))%>%
  group_by(volcano_name,volcano_number)%>%
  filter(start_year >= 1800, start_year == max(start_year))%>%
  select(region,longitude,latitude)%>%
  ggplot(aes(longitude,latitude))+
  geom_point()+
  borders("world")+
  coord_quickmap()+
  ggtitle("Location of Recently Erupted Volcanoes")
```
Question 3: What volcano has erupted the most often?
```{r}
dat1 %>%
  left_join(dat2, by = c("volcano_number","volcano_name","longitude","latitude"))%>%
  group_by(volcano_number,volcano_name)%>%
  summarize(total = n())%>%
  ungroup()%>%
  slice_max(total, n = 10)
```