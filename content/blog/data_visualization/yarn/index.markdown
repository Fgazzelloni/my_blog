---
title: "Yarn: hierarchical edge bundling visualization"
excerpt: "TidyTuesday 2022 Week 41"
date: 2022-10-11
author: "Federica Gazzelloni"
draft: false
images:
series:
tags:
categories:
layout: single
---



## Overview

This post is all about **hierarchical edge bundling visualization**, the dataset comes from [#TidyTuesday](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-10-11) 2022 week 41 **Ravelry data**.

The picture below is the result of the hierarchical edge bundling visualization.

![](featured.png)

First thing load the libraries and set the fonts:


```r
library(tidyverse)
library(igraph)
library(ggraph)
library(RColorBrewer)
library(showtext)
library(sysfonts)
library(extrafont)

showtext::showtext_auto()
showtext::showtext_opts(dpi=320)
font_add_google(name="Pangolin",family="pangolin")
```

Helpful tip is how to set the **dpi** option inside the `showtext::showtext_opts` function. This sets the size of your text, and it can be very useful when used in conjunction with the same option inside the `ggsave` function. If `showtxet` dpi is of a certain value, then you should set the `ggsave` dpi lower than that value to balance the text size outcome in your final .png file.

A perfect result comes from a nice balance trade-off between the **dpi** of the two functions.

Let's have a look at the data, there are 100000 observation and 24 variables referring to the various types of yarns, companies, names, yardage, weights, textures, ratings, ...


```r
yarn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-11/yarn.csv')

yarn%>%dim
```

```
[1] 100000     24
```


```r
yarn%>%names
```

```
 [1] "discontinued"              "gauge_divisor"            
 [3] "grams"                     "id"                       
 [5] "machine_washable"          "max_gauge"                
 [7] "min_gauge"                 "name"                     
 [9] "permalink"                 "rating_average"           
[11] "rating_count"              "rating_total"             
[13] "texture"                   "thread_size"              
[15] "wpi"                       "yardage"                  
[17] "yarn_company_name"         "yarn_weight_crochet_gauge"
[19] "yarn_weight_id"            "yarn_weight_knit_gauge"   
[21] "yarn_weight_name"          "yarn_weight_ply"          
[23] "yarn_weight_wpi"           "texture_clean"            
```

Let's select the names, the textures and the yardage for the length of the yarn, which is different from type to type.


```r
  yarn %>%
  select(yarn_weight_name,
         texture_clean,
         texture_clean,yardage)%>%
  head()
```

```
# A tibble: 6 × 3
  yarn_weight_name texture_clean yardage
  <chr>            <chr>           <dbl>
1 Aran             cable plied       364
2 Aran             plied             315
3 Worsted          plied             220
4 Aran             plied             170
5 Aran             singles           210
6 Worsted          plied             210
```

And, tidy the `texture_clean` a bit more, grouping for most common texture names such as merino, acrylic, cotton, nylon, aran, cashmere, wool, silk, jersey,...and calculate the yardage average.

<details>

Tidy data:


```r
df <- yarn%>%
  mutate(
    texture_clean=case_when(str_detect(texture_clean,
                                       "merino")~"merino",
                            str_detect(texture_clean,
                                            "ply|plied|play|plies")~"ply",
                            str_detect(texture_clean,
                                       "acrylique|acrylic|polyacryl|acrilyc|acryt")~"acrylic",
                            str_detect(texture_clean,"nylon")~"nylon",
                            str_detect(texture_clean,"cotton")~"cotton",
                            str_detect(texture_clean,"wool")~"wool",
                            str_detect(texture_clean,"polyamide|polyamid")~"polyamid",
                            str_detect(texture_clean,"angora")~"angora",
                            str_detect(texture_clean,"cashmere")~"cashmere",
                            str_detect(texture_clean,"aran")~"aran",
                            str_detect(texture_clean,"silk")~"silk",
                            str_detect(texture_clean,"jersey")~"jersey",
                            TRUE~texture_clean))%>%
  filter(str_detect(texture_clean,
                    c("merino|ply|acrylic|nylon|cotton|wool|angora|cashmere|aran|silk|jersey")))%>%
  count(texture_clean,yarn_weight_name,yardage,grams) %>%
  mutate(yarn_weight_name=case_when(yarn_weight_name=="Aran / Worsted"~"Aran",
                                    yarn_weight_name=="DK / Sport"~"DK",
                                    yarn_weight_name=="Light Fingering"~"Fingering",
                                    yarn_weight_name=="Super Bulky"~"Bulky",
                                    TRUE~yarn_weight_name))%>%
  filter(!yarn_weight_name=="No weight specified",!is.na(yarn_weight_name))%>%
  filter(!is.na(yardage),!is.na(grams))%>%
  select(-n)%>%
  group_by(yarn_weight_name,texture_clean)%>%
  summarise_all(.funs=mean)%>%
  select(yarn_weight_name,texture_clean,yardage)
```

</details>


```r
df%>%head
```

```
# A tibble: 6 × 3
# Groups:   yarn_weight_name [1]
  yarn_weight_name texture_clean yardage
  <chr>            <chr>           <dbl>
1 Aran             acrylic           77 
2 Aran             angora            27 
3 Aran             aran             196.
4 Aran             cotton           144.
5 Aran             merino           129.
6 Aran             nylon            158 
```

### Shape the data for making the hierarchical edge bundling visualization.

Now, for setting the data ready for being used inside one of the `{ggraph}` functions, a new vector is created named **YARN**. This is done to have a central point to all the yarns' types.

So, what is needed is a dataframe with two columns **from** and **to**. Actually, what is needed are two dataframe **hierarchy** and **vertices**. As follow:


```r
d1<- df%>%
  select(-texture_clean,-yardage)%>%
  mutate(from = "YARN",.before=everything())%>%
  rename(to = yarn_weight_name)
  
d2 <- df%>%
  select(-yardage) %>%
  rename(from = yarn_weight_name, 
         to = texture_clean)
  

hierarchy <- rbind(d1, d2)
vertices <- data.frame(name = unique(c(as.character(hierarchy$from), 
                                       as.character(hierarchy$to))) ) 
```

Hierarchy:


```r
hierarchy%>%head
```

```
# A tibble: 6 × 2
# Groups:   to [1]
  from  to   
  <chr> <chr>
1 YARN  Aran 
2 YARN  Aran 
3 YARN  Aran 
4 YARN  Aran 
5 YARN  Aran 
6 YARN  Aran 
```

Vertices:


```r
vertices%>%head
```

```
       name
1      YARN
2      Aran
3     Bulky
4    Cobweb
5        DK
6 Fingering
```

Then create the graph and the layout with `graph_from_data_frame()` function.


```r
mygraph <- graph_from_data_frame(hierarchy, vertices=vertices )
```


```r
ggraph(mygraph, layout = 'dendrogram', circular = F) + 
  geom_edge_diagonal()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="672" />


```r
ggraph(mygraph, layout = 'dendrogram', circular = T) + 
  geom_edge_diagonal()+
  geom_node_point(color="navy",size=5)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />

I can even `filter` the leafs out to point just the main nodes:


```r
ggraph(mygraph, layout = 'dendrogram', circular = T) + 
  geom_edge_diagonal()+
  geom_node_point(aes(filter=!leaf),
                  color="navy",
                  size=5)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="672" />

I like the circular type, and we can have a look at the inside calculation of the function with `create_layout()` specifying the type of layout as a **dendrogram**. It is a dataframe graph and it has the x, and y vectors, the leafs, and the names and other specifications.


```r
df1 <- create_layout(mygraph, layout = 'dendrogram')
df1%>%class
```

```
[1] "layout_tbl_graph" "layout_ggraph"    "data.frame"      
```


```r
df1%>%head()
```

```
      x y  leaf      name .ggraph.orig_index circular .ggraph.index
1 33.25 2 FALSE      YARN                  1    FALSE             1
2  4.00 1 FALSE      Aran                  2    FALSE             2
3 13.00 1 FALSE     Bulky                  3    FALSE             3
4 20.00 1 FALSE    Cobweb                  4    FALSE             4
5 26.50 1 FALSE        DK                  5    FALSE             5
6 35.50 1 FALSE Fingering                  6    FALSE             6
```

To add the labels to the leafs they would need to be oriented by a specific angle level, the reason for this is that the subgroups are not all the same.

What is needed is a function for node angle adjustments and another similar function to adjust the horizontal distance of the text around the dendrogram. Likely the {ggraph} package provides a function for calculating the angles of your data:

    - node_angle(x,y)


```r
node_angle(df1$x,df1$y,degrees = T)%>%head()
```

```
[1]  3.442215 14.036243  4.398705  2.862405  2.161079  1.613539
```

These values need to be adjusted:


```r
node_ang_adj <- function(x,y) {
  ifelse(node_angle(x,y) > 90 & node_angle(x,y) < 270 , 
         node_angle(x,y) + 180, node_angle(x,y))
  }

node_hjust_adj <- function(x,y) {
  ifelse(node_angle(x,y) > 90 & node_angle(x,y) < 270 , 1,0)
}
```

Finally, we can make the **hierarchical edge bundling visualization type circular dendrogram**:


```r
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(aes(color=factor(x)),
                     alpha=0.9,
                     show.legend = F) +
  geom_node_point(aes(color=factor(x)),
                  size=10,
                  show.legend = F)+
  geom_node_point(aes(color=factor(x)),
                  size=10,
                  shape=8,
                  show.legend = F)+
  geom_node_label(aes(filter=!leaf,label=name,color=factor(x)),
                  label.padding = unit(0.1, "lines"),
                  label.r = unit(0.1, "lines"),
                  label.size = 0.1,
                  family = "pangolin",
                  fontface="bold",
                  show.legend = F,
                  size=4, 
                  alpha=1)+
  geom_node_text(aes(x = x*1.1, 
                     y=y*1.1, 
                     hjust = node_hjust_adj(x,y),
                     angle=node_ang_adj(x,y),
                     filter = leaf, 
                     label=name,
                     color=factor(x)),
                 family = "pangolin",
                 fontface="bold",
                 show.legend = F,
                 size=4, 
                 alpha=1)+
  scale_color_manual(values = rep(RColorBrewer::brewer.pal(10,"Paired"),10))+
  scale_x_discrete(expand = c(0,0.3))+
  scale_y_discrete(expand = c(0,0.3))+
  coord_fixed()+
  labs(caption="What's inside your YARN?\ntextures for each type\n\nDataSource: #TidyTuesday 2022 week41 Ravelry data\nDataViz: Federica Gazzelloni (FG) Twitter: @fgazzelloni\n",
       alt="Infographics") +
  theme_graph()+
  theme(plot.margin = margin(5,5,5,5,unit = "pt"),
        plot.caption = element_text(face="bold",family="pangolin"))
```

Save it with setting **dpi**:


```r
ggsave("featured.png",
      dpi=280,
      bg="white",
      width = 9,height = 9)
```

### Resources:

-   [Hierarchical edge bundling](https://r-graph-gallery.com/hierarchical-edge-bundling.html)
-   [ggplot extensions](https://exts.ggplot2.tidyverse.org/ggraph.html)
-   [ggraph](https://ggraph.data-imaginist.com/)
-   [tidygraph](https://tidygraph.data-imaginist.com/index.html)
-   [igraph](https://igraph.org/)
