---
title: "Network of game mechanics"
excerpt: "TidyTuesday 2022 Week 4"
date: 2022-01-24
author: "Federica Gazzelloni"
draft: false
images:
series:
tags:
categories:
layout: single
---

## Overview

about 

![](featured.png)


```{r message=FALSE, warning=FALSE, paged.print=FALSE}
library(tidyverse)
```


```{r eval=FALSE, include=FALSE}
# ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
# details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')
```

```{r}
# saveRDS(ratings,"ratings.rds")
# saveRDS(details,"details.rds")
rat <- readRDS("ratings.rds")
det <- readRDS("details.rds")
```

```{r}
names(rat)
```

```{r}
names(det)
```


```{r}
det%>%head()
```


source of inspiration: https://www.thewayir.com/blog/boardgames/
```{r message=FALSE, warning=FALSE, paged.print=FALSE}
require(widyr)
require(igraph)
require(ggraph)
require(ggforce)

board_games <- rat %>%
  select(id,name) %>%
  left_join(select(det,id,boardgamemechanic),by="id") %>%
  rename(mechanic=boardgamemechanic) %>%
  tidyr::separate_rows(mechanic, sep = ",") %>% 
  mutate(mechanic = str_remove_all(mechanic, "[[:punct:]]"),
         mechanic = str_trim(mechanic),
         mechanic = gsub("^and ","",mechanic)) %>% 
  filter(!is.na(mechanic))

head(board_games)
```


```{r message=FALSE, warning=FALSE, paged.print=FALSE}
mechanic <- board_games %>% 
  count(mechanic,sort=T) %>%
  mutate(mechanic_pct=round(n/sum(n)*100,2))%>%
  #slice(1:30) %>%
  left_join(select(board_games,name,mechanic),by="mechanic") %>%
   mutate(name=as.factor(name),mechanic=as.factor(mechanic)) %>% 
   distinct() 
```


```{r fonts, message=FALSE, warning=FALSE, paged.print=FALSE}
library(extrafont)
library(showtext)
showtext::showtext_auto()
showtext::showtext_opts(dpi=320)
library(sysfonts)
#font_families_google()
font_add_google(name="Piedra",family="games")

family = "games"
```



```{r}
board_games50 <-board_games%>%
  select(name,mechanic)%>%
  count(name,sort=T) %>%
  slice(1:50)
```



```{r message=FALSE, warning=FALSE, paged.print=FALSE}
plot <- board_games50%>%
  left_join(mechanic,by="name") %>%
  filter(mechanic_pct >2 ) %>%
   pairwise_cor(mechanic, name, sort = T) %>% 
   filter(correlation > .15) %>% 
  graph_from_data_frame() %>% 
  ggraph() +
  geom_edge_link(linejoin = "round",
                 color="grey5",
                 edge_colour="red",
                 edge_width=0.5,
                 edge_linetype="solid") +
  geom_node_point(color="midnightblue",size=40,alpha=0.4) +
  geom_node_text(aes(label = name), 
                 repel = T,
                 size=5,
                 nudge_y = 0,
                 color="orange",
                 family=family) + 
  theme_void() +
   theme(text = element_text(family=family),
         plot.background = element_rect(color="beige",fill="beige"))

library(cowplot)

final <-ggdraw()+
  draw_plot(plot) +
  draw_label("Network of game \nmechanics",x=0.5,y=0.85,size=55,fontfamily=family)+
  draw_label("Sliced by the first 50 games by frequency, 
             filtered mechanics greater than 2% proportion of total,
             then finally taken just the most highly correlated ones",
             x=0.8,y=0.12,size=11,fontfamily=family) +
  draw_label("DataSource: Kaggle & Board Games Geek | Viz: Federica Gazzelloni",
             x=0.8,y=0.03,angle=0,size=11,alpha=0.5,fontfamily=family) +
   draw_image("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png",x=0.09,y=-0.47,scale=0.05)
  

ggsave("w4_board_games.png",
        plot =final,
        bg="white",
        dpi = 320,
        width = 11,
        height = 6
       )
```


