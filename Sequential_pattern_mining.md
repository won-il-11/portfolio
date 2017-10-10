# Sequential_Pattern_Mining
wonilChoi  
2017/ 1/ 16  




```r
# Required Packages
suppressPackageStartupMessages({
    library(arulesSequences)
    library(stringr)
    library(tidyr)
    library(googleVis)
    library(dplyr)
})
```

```
## Creating a generic function for 'toJSON' from package 'jsonlite' in package 'googleVis'
```

```r
sessionInfo()
```

```
## R version 3.4.2 (2017-09-28)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 15063)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=Korean_Korea.949  LC_CTYPE=Korean_Korea.949   
## [3] LC_MONETARY=Korean_Korea.949 LC_NUMERIC=C                
## [5] LC_TIME=Korean_Korea.949    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] dplyr_0.7.4            googleVis_0.6.2        tidyr_0.7.1           
## [4] stringr_1.2.0          arulesSequences_0.2-19 arules_1.5-3          
## [7] Matrix_1.2-11         
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.13     bindr_0.1        knitr_1.17       magrittr_1.5    
##  [5] lattice_0.20-35  R6_2.2.2         rlang_0.1.2      tools_3.4.2     
##  [9] grid_3.4.2       htmltools_0.3.6  assertthat_0.2.0 yaml_2.1.14     
## [13] rprojroot_1.2    digest_0.6.12    tibble_1.3.4     bindrcpp_0.2    
## [17] purrr_0.2.3      codetools_0.2-15 glue_1.1.1       evaluate_0.10.1 
## [21] rmarkdown_1.6    stringi_1.1.5    compiler_3.4.2   backports_1.1.1 
## [25] jsonlite_1.5     pkgconfig_2.0.1
```



```r
############## Data Process #################
raw_data <- read.csv('exmdata2.csv')
head(raw_data)
```

```
##   sequenceID eventID items
## 1          1       1   E20
## 2          1       2   E15
## 3          1       3    E2
## 4          1       4   E15
## 5          1       5    E8
## 6          1       6  <NA>
```

```r
visu <- reshape(raw_data, idvar='sequenceID', timevar='eventID', direction='wide')
rownames(visu) <- 1:NROW(visu)
visu_slt <- visu[,-1]
head(visu)
```

```
##   sequenceID items.1 items.2 items.3 items.4 items.5 items.6 items.7
## 1          1     E20     E15      E2     E15      E8    <NA>    <NA>
## 2          2     E16     E11     E20      E7     E17      E5    <NA>
## 3          3      E3      E6     E16     E12      E1     E20    <NA>
## 4          4     E14      E9      E7     E18     E19      E5     E15
## 5          5     E20      E5      E2     E15      E8     E14     E11
## 6          6      E2     E20      E5      E2     E15     E18     E14
##   items.8 items.9 items.10
## 1    <NA>    <NA>     <NA>
## 2    <NA>    <NA>     <NA>
## 3    <NA>    <NA>     <NA>
## 4    <NA>    <NA>     <NA>
## 5      E1    <NA>     <NA>
## 6      E1     E17     <NA>
```

```r
visu_plot <- data.frame()
for (i in 2:ncol(visu_slt)){
    visu_cache <- visu_slt %>%
        group_by(visu_slt[ , i-1], visu_slt[ , i]) %>% summarise(n=n())
    visu_cache = visu_cache[complete.cases(visu_cache),]
    colnames(visu_cache)[1:2] <- c('from', 'to')
    visu_cache$from <- paste0(strrep(" ", i-1), visu_cache$from)
    visu_cache$to <- paste0(strrep(" ", i), visu_cache$to)
    visu_plot <- rbind(visu_plot, data.frame(visu_cache))
}
head(visu_plot, 5)
```

```
##   from    to n
## 1   E1   E20 1
## 2  E12   E13 1
## 3  E13   E20 1
## 4  E14   E11 1
## 5  E14    E9 1
```

```r
# Plot All Data Sequences
plot(gvisSankey(visu_plot, from='from', to='to', weight='n',
                options=list(height=700, width=1500,
                             sankey="{link:{color:{fill:'lightblue'}}}")))
```

```
## starting httpd help server ... done
```

![](pic/SankeyPlot_1.jpg)


```r
# Data Type 'transactons'
trans_data = as(data.frame(event=raw_data$items), 'transactions')
item_Info = data.frame(sequenceID=raw_data$sequenceID, eventID=raw_data$eventID)
trans_data@itemsetInfo = item_Info

########## Frequent Sequence mining - cspade ##########
spade = cspade(trans_data, parameter = list(support = 0.2))
spade
```

```
## set of 75 sequences
```

```r
seqnce = as(spade, "data.frame")
head(seqnce,3)
```

```
##        sequence support
## 1  <{event=E1}>    0.45
## 2 <{event=E11}>    0.25
## 3 <{event=E12}>    0.20
```

```r
seqnce$support = seqnce$support * sum(!duplicated(as(trans_data, "data.frame")$sequenceID))
seqnce$sequence = gsub("event=|<|>|\\{|}","",seqnce$sequence)
head(seqnce,3)
```

```
##   sequence support
## 1       E1       9
## 2      E11       5
## 3      E12       4
```

```r
spade_tab <- ruleInduction(spade, confidence = 0.4, control = list(verbose = TRUE))
seqnce_tab = as(spade_tab, 'data.frame')
seqnce_tab$rule = gsub("event=|\\{|}","",seqnce_tab$rule)
head(seqnce_tab,3)
```

```
##            rule support confidence      lift
## 1 <E15> => <E8>    0.45  0.6428571 1.0714286
## 2  <E2> => <E8>    0.35  0.5833333 0.9722222
## 3 <E20> => <E8>    0.35  0.4375000 0.7291667
```

```r
############ Plot Selected Sequences ############
maxLength <-  max(str_count(seqnce$sequence, ","))+1
patrn <- separate(seqnce, sequence, as.character(1:maxLength), ",", fill = "right")

limlen = 4
patrn = subset(patrn, !is.na(patrn[,limlen]))

patterns = data.frame()
for (i in 2:maxLength) {
    tmp <- patrn %>% group_by(patrn[ , i-1], patrn[ , i]) %>% summarise(n=max(support))
    colnames(tmp) <- c('from', 'to', 'n')
    tmp = tmp[complete.cases(tmp),]
    tmp$from <- paste0(tmp$from, '(', i-1, ')')
    tmp$to <- paste0(tmp$to, '(', i, ')')
    patterns <- rbind(patterns, data.frame(tmp))
}
head(patterns,4)
```

```
##     from     to n
## 1  E2(1) E15(2) 5
## 2 E20(1) E15(2) 5
## 3 E20(1)  E2(2) 6
## 4 E20(1)  E5(2) 7
```

```r
plot(gvisSankey(patterns, from='from', to='to', weight='n', options=
                list(height=700, width=1500, sankey="{link:{color:{fill:'lightblue'}}}")))
```
![](pic/SankeyPlot_2.jpg)


```r
############ Visualization - networkD3 #######################
library(networkD3)
library(magrittr)
```

```
## 
## Attaching package: 'magrittr'
```

```
## The following object is masked from 'package:tidyr':
## 
##     extract
```

```r
library(gsubfn)
```

```
## Loading required package: proto
```

```r
patterns_d3 <- patterns
head(patterns,4)
```

```
##     from     to n
## 1  E2(1) E15(2) 5
## 2 E20(1) E15(2) 5
## 3 E20(1)  E2(2) 6
## 4 E20(1)  E5(2) 7
```

```r
links <- data.frame(source=patterns_d3[, 1],
                   target=patterns_d3[, 2],
                   value=patterns_d3$n)
nodes <- data.frame(name = c(as.character(patterns_d3[, 1]), as.character(patterns_d3[, 2]))[!duplicated(c(patterns_d3[, 1], patterns_d3[, 2]))])
nodes$index <- 0:(NROW(nodes)-1)

links$source <- as.character(links$source)
links$target <- as.character(links$target)
links$source <- as.integer(gsubfn("\\S+", setNames(as.list(nodes$index), nodes$name), links$source))
links$target <- as.integer(gsubfn("\\S+", setNames(as.list(nodes$index), nodes$name), links$target))

data_d3 <- list(nodes=data.frame(name=nodes$name), links=links)
data_d3$nodes$name <- as.character(data_d3$nodes$name)


sankeyNetwork(Links=data_d3$links, Nodes=data_d3$nodes, Source="source",
              Target="target", Value="value", NodeID="name",
              units="weight", fontSize=12, nodeWidth=20)
```

<!--html_preserve--><div id="htmlwidget-945b03e06489650ff175" style="width:672px;height:480px;" class="sankeyNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-945b03e06489650ff175">{"x":{"links":{"source":[0,1,1,1,2,2,3,4,4,5,5,5,6,6,7,7,7,8,9,9,10,11],"target":[3,3,4,5,3,4,8,6,8,6,7,8,12,10,12,9,10,12,13,11,13,14],"value":[5,5,6,7,6,6,6,6,6,6,7,6,6,5,7,5,5,6,5,4,5,4]},"nodes":{"name":["E2(1)","E20(1)","E5(1)","E15(2)","E2(2)","E5(2)","E15(3)","E2(3)","E8(3)","E15(4)","E8(4)","E8(5)","E14(4)","E14(5)","E14(6)"],"group":["E2(1)","E20(1)","E5(1)","E15(2)","E2(2)","E5(2)","E15(3)","E2(3)","E8(3)","E15(4)","E8(4)","E8(5)","E14(4)","E14(5)","E14(6)"]},"options":{"NodeID":"name","NodeGroup":"name","LinkGroup":null,"colourScale":"d3.scaleOrdinal(d3.schemeCategory20);","fontSize":12,"fontFamily":null,"nodeWidth":20,"nodePadding":10,"units":"weight","margin":{"top":null,"right":null,"bottom":null,"left":null},"iterations":32,"sinksRight":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


```r
################## Find Sequence we want ###################
# Specific Event 'E2' Filtering
event <- 'E2'
num <- 2
patrn_sub <- patrn[patrn[,num] == event & !is.na(patrn[,num]),]
patrn_sub <- patrn_sub[,apply(patrn_sub, 2, function(arg){any(!is.na(arg))})]
head(patrn_sub,4)
```

```
##      1  2   3   4    5 support
## 24  E5 E2 E15  E8 <NA>       5
## 25 E20 E2 E15  E8 <NA>       5
## 50  E5 E2  E8 E14 <NA>       6
## 52 E20 E2  E8 E14 <NA>       5
```

```r
maxLength_sub <- ncol(patrn_sub)-1

patterns_sub <- data.frame()
for (i in 2:maxLength_sub) {
    tmp <- patrn_sub %>% group_by(patrn_sub[ , i-1], patrn_sub[ , i]) %>% summarise(n=max(support))
    colnames(tmp) <- c('from', 'to', 'n')
    tmp <- tmp[complete.cases(tmp),]
    tmp$from <- paste0(tmp$from, '(', i-1, ')')
    tmp$to <- paste0(tmp$to, '(', i, ')')
    patterns_sub <- rbind(patterns_sub, data.frame(tmp))
}
head(patterns_sub,4)
```

```
##     from     to n
## 1 E20(1)  E2(2) 6
## 2  E5(1)  E2(2) 6
## 3  E2(2) E15(3) 6
## 4  E2(2)  E8(3) 6
```

```r
plot(gvisSankey(patterns_sub, from='from', to='to', weight='n',
                options=list(height=700, width=1400,
                             sankey="{link:{color:{fill:'lightblue'}}}")))
```

![](pic/SankeyPlot_3.jpg)


```r
# Find Specific Pattern with Support(count)
n <- 66
reg_pattern <- as.character(patrn[n,-length(patrn)][,!is.na(patrn[n,-length(patrn)])])
reg_tmp <- paste(reg_pattern, collapse = '-')
reg_patrn <- gsub('-', '+,+(.|)+', reg_tmp)
reg_data <- apply(visu_slt, 1, paste, collapse=',')
reg_find <- visu_slt[grep(reg_patrn, reg_data),]

find_obs <- function(n=NULL, reg_pattern=NULL) {
    if (is.null(reg_pattern))
        reg_pattern=as.character(patrn[n,-length(patrn)][,!is.na(patrn[n,-length(patrn)])])
    reg_tmp <- paste(reg_pattern, collapse='-')
    reg_patrn <- gsub('-', '+,+(.|)+', reg_tmp)
    reg_data <- apply(visu_slt, 1, paste, collapse=',')
    reg_find <- visu_slt[grep(reg_patrn, reg_data),]
    reg_find
}

head(find_obs(70))      # print patterns support=70
```

```
##   items.1 items.2 items.3 items.4 items.5 items.6 items.7 items.8 items.9
## 1     E20     E15      E2     E15      E8    <NA>    <NA>    <NA>    <NA>
## 2     E16     E11     E20      E7     E17      E5    <NA>    <NA>    <NA>
## 3      E3      E6     E16     E12      E1     E20    <NA>    <NA>    <NA>
## 4     E14      E9      E7     E18     E19      E5     E15    <NA>    <NA>
## 5     E20      E5      E2     E15      E8     E14     E11      E1    <NA>
## 6      E2     E20      E5      E2     E15     E18     E14      E1     E17
##   items.10
## 1     <NA>
## 2     <NA>
## 3     <NA>
## 4     <NA>
## 5     <NA>
## 6     <NA>
```

```r
head(find_obs(3))       # print patterns support=3
```

```
##    items.1 items.2 items.3 items.4 items.5 items.6 items.7 items.8 items.9
## 5      E20      E5      E2     E15      E8     E14     E11      E1    <NA>
## 10      E9     E20      E5      E2     E15      E8     E14    <NA>    <NA>
## 12      E2     E10     E20      E5      E2     E15      E8     E14    <NA>
## 19     E14     E11      E5      E2     E15      E8     E14      E1    <NA>
## 20     E20      E5      E2     E15      E8     E14      E4    <NA>    <NA>
##    items.10
## 5      <NA>
## 10     <NA>
## 12     <NA>
## 19     <NA>
## 20     <NA>
```

```r
find_obs(reg_pattern = c("E5", "E2", "E14"))    # # print patterns including E5,E2,E14
```

```
##    items.1 items.2 items.3 items.4 items.5 items.6 items.7 items.8 items.9
## 5      E20      E5      E2     E15      E8     E14     E11      E1    <NA>
## 6       E2     E20      E5      E2     E15     E18     E14      E1     E17
## 10      E9     E20      E5      E2     E15      E8     E14    <NA>    <NA>
## 11     E18      E5     E20      E5      E2     E14    <NA>    <NA>    <NA>
## 12      E2     E10     E20      E5      E2     E15      E8     E14    <NA>
## 14      E1     E20     E20      E5      E2      E8      E8     E14     E11
## 19     E14     E11      E5      E2     E15      E8     E14      E1    <NA>
## 20     E20      E5      E2     E15      E8     E14      E4    <NA>    <NA>
##    items.10
## 5      <NA>
## 6      <NA>
## 10     <NA>
## 11     <NA>
## 12     <NA>
## 14     <NA>
## 19     <NA>
## 20     <NA>
```
