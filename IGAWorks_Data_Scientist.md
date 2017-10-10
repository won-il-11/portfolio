# IGAWorks_Data_Science
wonilChoi  
2017/ 10/ 9  



## 요약
  
  순위와 판매량이 모두 있는 데이터 5622건에 대해 판매량 추정 모델을 만들기 위해 데이터를 탐색한 결과 순위로 평균 판매량을 추정하는 것이 가장 성능이 좋을 것이라고 가정하고 선형 회귀 모형, GAM, log normal분포 적합 후 샘플링 방법을 시도했습니다. 테스트 결과, 순위가 높은(1~30) 경우 log normal분포에서 샘플링하는 방법이 가장 test MSE가 낮았고, 그외의 순위는(31~540) GAM이 
가장 낮았습니다. 따라서 upper tail에서는 샘플링 방법을, 그 외 구간은 GAM 모형을 사용한 것이 성능이 좋습니다.


## 데이터 전처리

```r
suppressPackageStartupMessages({
    library(caret)
    library(car)
    library(ggplot2)
    library(extremeStat)
    library(gamlss)
    library(gamlss.dist)
    library(fitdistrplus)
    library(data.table)
    library(mixtools)
    library(mclust)
    library(lubridate)
    library(plotly)
    library(readxl)
    library(stringr)
})
```

  `readxl`패키지로 xlsx 확장자 파일을 읽어오고 변수명을 영어로 바꿨습니다. R에서 한글 코드를 인식하는 과정에서 에러가 발생하므로 불가피한 변경입니다. 그리고 데이터 처리에서 강력한 성능을 보유한 `data.table`패키지 형식으로 데이터를 변환했습니다.


```r
path <- "IGAWorks_Data Scientist Interview Question.xlsx"

rank_data <- read_excel(path,sheet=2,col_types=c("text","text","numeric"))
sold_data <- read_excel(path,sheet=3,col_types=c("text","text","numeric"))

str(rank_data)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	97620 obs. of  3 variables:
##  $ 서적명: chr  "12" "12" "12" "12" ...
##  $ 일자  : chr  "20150101" "20150102" "20150103" "20150104" ...
##  $ 순위  : num  62 68 72 76 78 82 86 86 87 86 ...
```

```r
str(sold_data)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	217567 obs. of  3 variables:
##  $ 서적명: chr  "2" "2" "2" "2" ...
##  $ 일자  : chr  "20150101" "20150102" "20150106" "20150107" ...
##  $ 판매량: num  0 0 0 0 0 0 0 0 0 0 ...
```

```r
names(rank_data) <- c("book_id","date","rank")
names(sold_data) <- c("book_id","date","sale")

sold_data <- as.data.table(sold_data)
rank_data <- as.data.table(rank_data)
sold_data <- sold_data[order(book_id,date,-sale)]
```

  데이터를 보던 중 같은 날짜에 판매 데이터가 나뉘어 있는 경우가 있었습니다. 서적명 2856의 경우를 예시로 제시하면서 중복된 날짜 데이터를 더해주는 방법으로 중복 행을 삭제했습니다. 또한 판매량이 0인 케이스가 90% 정도로 매우 많아서 사실상 `0=NA`로 받아들이는 게 맞다고 판단했습니다.
  

```r
# find duplicated rows example
print.AsIs(sold_data[book_id%in%2856,])    # Many rows are duplicated.
```

```
##     book_id     date    sale
## 1      2856 20150101       0
## 2      2856 20150102       0
## 3      2856 20150103       0
## 4      2856 20150104       0
## 5      2856 20150105       0
## 6      2856 20150106       0
## 7      2856 20150107       0
## 8      2856 20150108       0
## 9      2856 20150109       0
## 10     2856 20150110       0
## 11     2856 20150111       0
## 12     2856 20150112       0
## 13     2856 20150113       0
## 14     2856 20150114       0
## 15     2856 20150114       0
## 16     2856 20150115       0
## 17     2856 20150115       0
## 18     2856 20150116       0
## 19     2856 20150116       0
## 20     2856 20150117       0
## 21     2856 20150117       0
## 22     2856 20150118       0
## 23     2856 20150118       0
## 24     2856 20150119       0
## 25     2856 20150119       0
## 26     2856 20150120       0
## 27     2856 20150120       0
## 28     2856 20150121       0
## 29     2856 20150121       0
## 30     2856 20150122       0
## 31     2856 20150122       0
## 32     2856 20150123       0
## 33     2856 20150123       0
## 34     2856 20150124       0
## 35     2856 20150124       0
## 36     2856 20150125       0
## 37     2856 20150125       0
## 38     2856 20150126       0
## 39     2856 20150126       0
## 40     2856 20150127       0
## 41     2856 20150127       0
## 42     2856 20150128       0
## 43     2856 20150128       0
## 44     2856 20150129    8500
## 45     2856 20150129       0
## 46     2856 20150130       0
## 47     2856 20150130       0
## 48     2856 20150131       0
## 49     2856 20150131       0
## 50     2856 20150201       0
## 51     2856 20150201       0
## 52     2856 20150202       0
## 53     2856 20150202       0
## 54     2856 20150203       0
## 55     2856 20150203       0
## 56     2856 20150204       0
## 57     2856 20150204       0
## 58     2856 20150205       0
## 59     2856 20150205       0
## 60     2856 20150206       0
## 61     2856 20150206       0
## 62     2856 20150207       0
## 63     2856 20150207       0
## 64     2856 20150208       0
## 65     2856 20150208       0
## 66     2856 20150209       0
## 67     2856 20150209       0
## 68     2856 20150210       0
## 69     2856 20150210       0
## 70     2856 20150211       0
## 71     2856 20150211       0
## 72     2856 20150212       0
## 73     2856 20150212       0
## 74     2856 20150213       0
## 75     2856 20150213       0
## 76     2856 20150214       0
## 77     2856 20150214       0
## 78     2856 20150215       0
## 79     2856 20150215       0
## 80     2856 20150216       0
## 81     2856 20150216       0
## 82     2856 20150217       0
## 83     2856 20150217       0
## 84     2856 20150218       0
## 85     2856 20150218       0
## 86     2856 20150219       0
## 87     2856 20150219       0
## 88     2856 20150220       0
## 89     2856 20150220       0
## 90     2856 20150221       0
## 91     2856 20150221       0
## 92     2856 20150222       0
## 93     2856 20150222       0
## 94     2856 20150223       0
## 95     2856 20150223       0
## 96     2856 20150224       0
## 97     2856 20150224       0
## 98     2856 20150225       0
## 99     2856 20150225       0
## 100    2856 20150226       0
## 101    2856 20150226       0
## 102    2856 20150227       0
## 103    2856 20150227       0
## 104    2856 20150228       0
## 105    2856 20150228       0
## 106    2856 20150301       0
## 107    2856 20150301       0
## 108    2856 20150302       0
## 109    2856 20150302       0
## 110    2856 20150303       0
## 111    2856 20150303       0
## 112    2856 20150304       0
## 113    2856 20150304       0
## 114    2856 20150305       0
## 115    2856 20150305       0
## 116    2856 20150306       0
## 117    2856 20150306       0
## 118    2856 20150307       0
## 119    2856 20150307       0
## 120    2856 20150308       0
## 121    2856 20150308       0
## 122    2856 20150309       0
## 123    2856 20150309       0
## 124    2856 20150310       0
## 125    2856 20150310       0
## 126    2856 20150311  219725
## 127    2856 20150311       0
## 128    2856 20150312    9350
## 129    2856 20150312       0
## 130    2856 20150313  135575
## 131    2856 20150313       0
## 132    2856 20150314  640475
## 133    2856 20150314       0
## 134    2856 20150315  640475
## 135    2856 20150315       0
## 136    2856 20150316  804100
## 137    2856 20150316       0
## 138    2856 20150317 1304325
## 139    2856 20150317       0
## 140    2856 20150318 1014475
## 141    2856 20150318       0
## 142    2856 20150319       0
## 143    2856 20150319       0
## 144    2856 20150320  420750
## 145    2856 20150320       0
## 146    2856 20150321       0
## 147    2856 20150321       0
## 148    2856 20150322       0
## 149    2856 20150322       0
## 150    2856 20150323  757350
## 151    2856 20150323       0
## 152    2856 20150324 1481975
## 153    2856 20150324       0
## 154    2856 20150325    9350
## 155    2856 20150325       0
## 156    2856 20150326  210375
## 157    2856 20150326       0
## 158    2856 20150327  430100
## 159    2856 20150327       0
## 160    2856 20150328       0
## 161    2856 20150328       0
## 162    2856 20150329  210375
## 163    2856 20150329       0
## 164    2856 20150330       0
## 165    2856 20150330       0
## 166    2856 20150331    9350
## 167    2856 20150331       0
## 168    2856 20150401       0
## 169    2856 20150401       0
## 170    2856 20150402       0
## 171    2856 20150402       0
## 172    2856 20150403  659175
## 173    2856 20150403    1530
## 174    2856 20150404  126225
## 175    2856 20150404       0
## 176    2856 20150405 2061675
## 177    2856 20150405       0
## 178    2856 20150406       0
## 179    2856 20150406       0
## 180    2856 20150407  210375
## 181    2856 20150407       0
## 182    2856 20150408  766700
## 183    2856 20150408     765
## 184    2856 20150409  336600
## 185    2856 20150409   23715
## 186    2856 20150410       0
## 187    2856 20150410       0
## 188    2856 20150411  462825
## 189    2856 20150411       0
## 190    2856 20150412  210375
## 191    2856 20150412       0
## 192    2856 20150413  126225
## 193    2856 20150413       0
## 194    2856 20150414  589050
## 195    2856 20150414     765
## 196    2856 20150415       0
## 197    2856 20150415       0
## 198    2856 20150416  126225
## 199    2856 20150416       0
## 200    2856 20150417  430100
## 201    2856 20150417   45900
## 202    2856 20150418  546975
## 203    2856 20150418     765
## 204    2856 20150419       0
## 205    2856 20150419       0
## 206    2856 20150420  986425
## 207    2856 20150420       0
## 208    2856 20150421   13770
## 209    2856 20150421       0
## 210    2856 20150422    1530
## 211    2856 20150422       0
## 212    2856 20150423       0
## 213    2856 20150423       0
## 214    2856 20150424    1530
## 215    2856 20150424       0
## 216    2856 20150425       0
## 217    2856 20150425       0
## 218    2856 20150426 1093950
## 219    2856 20150426       0
## 220    2856 20150427       0
## 221    2856 20150427       0
## 222    2856 20150428    1530
## 223    2856 20150428       0
## 224    2856 20150429       0
## 225    2856 20150429       0
## 226    2856 20150430       0
## 227    2856 20150430       0
## 228    2856 20150501  355300
## 229    2856 20150501       0
## 230    2856 20150502  126225
## 231    2856 20150502     765
## 232    2856 20150503  420750
## 233    2856 20150503       0
## 234    2856 20150504    9350
## 235    2856 20150504     765
## 236    2856 20150505       0
## 237    2856 20150505       0
## 238    2856 20150506       0
## 239    2856 20150506       0
## 240    2856 20150507  430100
## 241    2856 20150507       0
## 242    2856 20150508  345950
## 243    2856 20150508       0
## 244    2856 20150509   39780
## 245    2856 20150509       0
## 246    2856 20150510  135575
## 247    2856 20150510    2295
## 248    2856 20150511 2365550
## 249    2856 20150511       0
## 250    2856 20150512  430100
## 251    2856 20150512       0
## 252    2856 20150513       0
## 253    2856 20150513       0
## 254    2856 20150514       0
## 255    2856 20150514       0
## 256    2856 20150515       0
## 257    2856 20150515       0
## 258    2856 20150516   61200
## 259    2856 20150516       0
## 260    2856 20150517 1893375
## 261    2856 20150517       0
## 262    2856 20150518    9350
## 263    2856 20150518       0
## 264    2856 20150519  631125
## 265    2856 20150519   61200
## 266    2856 20150520  219725
## 267    2856 20150520     765
## 268    2856 20150521     765
## 269    2856 20150521       0
## 270    2856 20150522       0
## 271    2856 20150522       0
## 272    2856 20150523       0
## 273    2856 20150523       0
## 274    2856 20150524  210375
## 275    2856 20150524       0
## 276    2856 20150525   12240
## 277    2856 20150525       0
## 278    2856 20150526       0
## 279    2856 20150526       0
## 280    2856 20150527       0
## 281    2856 20150527       0
## 282    2856 20150528  757350
## 283    2856 20150528   77265
## 284    2856 20150529       0
## 285    2856 20150529       0
## 286    2856 20150530  673200
## 287    2856 20150530     765
## 288    2856 20150531  420750
## 289    2856 20150531   97920
```

```r
unique(sold_data[book_id%in%2856,date])    # should be reduced to 289->151
```

```
##   [1] "20150101" "20150102" "20150103" "20150104" "20150105" "20150106"
##   [7] "20150107" "20150108" "20150109" "20150110" "20150111" "20150112"
##  [13] "20150113" "20150114" "20150115" "20150116" "20150117" "20150118"
##  [19] "20150119" "20150120" "20150121" "20150122" "20150123" "20150124"
##  [25] "20150125" "20150126" "20150127" "20150128" "20150129" "20150130"
##  [31] "20150131" "20150201" "20150202" "20150203" "20150204" "20150205"
##  [37] "20150206" "20150207" "20150208" "20150209" "20150210" "20150211"
##  [43] "20150212" "20150213" "20150214" "20150215" "20150216" "20150217"
##  [49] "20150218" "20150219" "20150220" "20150221" "20150222" "20150223"
##  [55] "20150224" "20150225" "20150226" "20150227" "20150228" "20150301"
##  [61] "20150302" "20150303" "20150304" "20150305" "20150306" "20150307"
##  [67] "20150308" "20150309" "20150310" "20150311" "20150312" "20150313"
##  [73] "20150314" "20150315" "20150316" "20150317" "20150318" "20150319"
##  [79] "20150320" "20150321" "20150322" "20150323" "20150324" "20150325"
##  [85] "20150326" "20150327" "20150328" "20150329" "20150330" "20150331"
##  [91] "20150401" "20150402" "20150403" "20150404" "20150405" "20150406"
##  [97] "20150407" "20150408" "20150409" "20150410" "20150411" "20150412"
## [103] "20150413" "20150414" "20150415" "20150416" "20150417" "20150418"
## [109] "20150419" "20150420" "20150421" "20150422" "20150423" "20150424"
## [115] "20150425" "20150426" "20150427" "20150428" "20150429" "20150430"
## [121] "20150501" "20150502" "20150503" "20150504" "20150505" "20150506"
## [127] "20150507" "20150508" "20150509" "20150510" "20150511" "20150512"
## [133] "20150513" "20150514" "20150515" "20150516" "20150517" "20150518"
## [139] "20150519" "20150520" "20150521" "20150522" "20150523" "20150524"
## [145] "20150525" "20150526" "20150527" "20150528" "20150529" "20150530"
## [151] "20150531"
```

```r
sold_data[book_id%in%2856,][!duplicated(sold_data[book_id%in%2856,],by=c("book_id","date")),]
```

```
##      book_id     date   sale
##   1:    2856 20150101      0
##   2:    2856 20150102      0
##   3:    2856 20150103      0
##   4:    2856 20150104      0
##   5:    2856 20150105      0
##  ---                        
## 147:    2856 20150527      0
## 148:    2856 20150528 757350
## 149:    2856 20150529      0
## 150:    2856 20150530 673200
## 151:    2856 20150531 420750
```

```r
                                           # when duplicate erased
nrow(sold_data[sale=="0",])/nrow(sold_data) # 93.8% is 0
```

```
## [1] 0.9381064
```

```r
# date duplicate process should consider sum function.
# rbind sum sale on same date
tmp <- unique(sold_data$book_id)
setkey(sold_data,book_id)

sold_no_dup <- data.table()
for(i in tmp){
    sold_no_dup <- rbindlist(list(sold_data[i,.(book_id,sale=sum(sale)),by=date],sold_no_dup))
}
sold_data <- unique(sold_no_dup,by=c(1,2,3))
sold_data[,.N,by=book_id][order(-N)]    # N does not exceed 151 - okay
```

```
##       book_id   N
##    1:     998 151
##    2:     997 151
##    3:     996 151
##    4:     995 151
##    5:     994 151
##   ---            
## 1811:    1025   3
## 1812:    1946   2
## 1813:    1794   2
## 1814:    1869   1
## 1815:    1495   1
```


## EDA

  데이터를 탐구하며 몇가지 특징을 알아냈는데, 우리가 목표로 하는 판매량~순위 공식을 유도하기 위해 가용할 수 있는 샘플 비율이 매우 적다는 점과, 5월 15일~17일 기간만 501~540위 순위가 랭크되지 않았다는 점입니다. 또한, `sale`가 일정 날짜마다 상승, 하락을 반복하는 패턴을 가지고 있으며, 1위부터 10위까지의 매출 비중이 절반을 넘는다는 사실을 발견했습니다.


```r
summary(sold_data)
```

```
##      date             book_id               sale         
##  Length:208073      Length:208073      Min.   :0.00e+00  
##  Class :character   Class :character   1st Qu.:0.00e+00  
##  Mode  :character   Mode  :character   Median :0.00e+00  
##                                        Mean   :2.42e+06  
##                                        3rd Qu.:0.00e+00  
##                                        Max.   :1.07e+10
```

```r
length(unique(sold_data$book_id))    # sale data pid rows 1815
```

```
## [1] 1815
```

```r
length(unique(rank_data$book_id))    # rank data pid rows 1295
```

```
## [1] 1295
```

```r
length(union(unique(rank_data$book_id), unique(sold_data$book_id)))        # All pid 2864
```

```
## [1] 2864
```

```r
length(intersect(unique(rank_data$book_id), unique(sold_data$book_id)))    # both pid 246
```

```
## [1] 246
```

```r
head(rank_data[,.N,by=date][order(N)],3)   # what happened on 0515~0517?
```

```
##        date   N
## 1: 20150515 500
## 2: 20150516 500
## 3: 20150517 500
```

```r
head(rank_data[,.N,by=rank][order(-rank)],41)  # why 500~540 is not calculated on 0515~0517?
```

```
##     rank   N
##  1:  540 178
##  2:  539 178
##  3:  538 178
##  4:  537 178
##  5:  536 178
##  6:  535 178
##  7:  534 178
##  8:  533 178
##  9:  532 178
## 10:  531 178
## 11:  530 178
## 12:  529 178
## 13:  528 178
## 14:  527 178
## 15:  526 178
## 16:  525 178
## 17:  524 178
## 18:  523 178
## 19:  522 178
## 20:  521 178
## 21:  520 178
## 22:  519 178
## 23:  518 178
## 24:  517 178
## 25:  516 178
## 26:  515 178
## 27:  514 178
## 28:  513 178
## 29:  512 178
## 30:  511 178
## 31:  510 178
## 32:  509 178
## 33:  508 178
## 34:  507 178
## 35:  506 178
## 36:  505 178
## 37:  504 178
## 38:  503 178
## 39:  502 178
## 40:  501 178
## 41:  500 181
##     rank   N
```

```r
rank_data[date%in%20150515:20150517,][order(date,-rank)]
```

```
##       book_id     date rank
##    1:     864 20150515  500
##    2:    1204 20150515  499
##    3:     474 20150515  498
##    4:     390 20150515  497
##    5:     389 20150515  496
##   ---                      
## 1496:     437 20150517    5
## 1497:     431 20150517    4
## 1498:    2165 20150517    3
## 1499:    2395 20150517    2
## 1500:    1597 20150517    1
```

```r
# find clue on sold data
sold_data[date%in%20150515:20150517,sum(sale),by=date]
```

```
##        date         V1
## 1: 20150515 3841109447
## 2: 20150516 3980989578
## 3: 20150517 3521568921
```

```r
# sales time series has pattern.
ggplot(data=sold_data[,.(sale=sum(sale)),by=date],
      aes(x=date,y=sale)) + geom_line(aes(group=1))
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/eda-1.png)<!-- -->

```r
# how many pid in sold data?
sold_data[date%in%20150515:20150517,.N,by=date]     # over 1000 pid, so rank ~500 is clearly error.
```

```
##        date    N
## 1: 20150515 1610
## 2: 20150516 1606
## 3: 20150517 1607
```

```r
sold_data[date%in%20150515,sum(sale),by=book_id][order(-V1)][1:10]    # same sold pattern
```

```
##     book_id         V1
##  1:    2395 2565257075
##  2:     734  323850000
##  3:     744  260051125
##  4:    1626   98058125
##  5:     695   51317475
##  6:     532   49690575
##  7:     692   49050100
##  8:    1677   49028000
##  9:     690   47867325
## 10:    1634   43108175
```

```r
sold_data[date%in%20150516,sum(sale),by=book_id][order(-V1)][1:10]    # same sold pattern
```

```
##     book_id         V1
##  1:    2395 2582479775
##  2:     734  322944750
##  3:     744  305075625
##  4:    1634  112863850
##  5:    1626   84439850
##  6:     690   67806200
##  7:     532   52229100
##  8:     695   50055225
##  9:     692   39835675
## 10:    1677   33355275
```

```r
sold_data[date%in%20150517,sum(sale),by=book_id][order(-V1)][1:10]    # same sold pattern
```

```
##     book_id         V1
##  1:    2395 2362334450
##  2:     734  276250000
##  3:     744  263038025
##  4:    1634   79746150
##  5:    1626   68540175
##  6:     695   49269825
##  7:     692   45155825
##  8:     690   41528025
##  9:     532   35431825
## 10:     742   33451750
```

```r
# sold pattern is stable.
sold_data[date%in%20150518,sum(sale),by=book_id][order(-V1)][1:10]
```

```
##     book_id         V1
##  1:    2395 2933170225
##  2:     744  288396925
##  3:     734  253933250
##  4:    1626   78871925
##  5:     690   63047050
##  6:     695   34674475
##  7:     692   26399725
##  8:    2420   23502500
##  9:     291   22546250
## 10:     290   22036250
```

```r
sold_data[date%in%20150519,sum(sale),by=book_id][order(-V1)][1:10]
```

```
##     book_id         V1
##  1:    2395 2958918425
##  2:     734  346931750
##  3:     744  306089675
##  4:    1626   75258150
##  5:     690   62065300
##  6:     692   40845475
##  7:    1858   40279800
##  8:    1677   34240125
##  9:     695   28059350
## 10:     290   22656750
```

```r
sold_data[date%in%20150520,sum(sale),by=book_id][order(-V1)][1:10]
```

```
##     book_id         V1
##  1:    2395 2875023000
##  2:     734  305328500
##  3:     695  259747675
##  4:     744  234853300
##  5:     690  161465150
##  6:     692  124794450
##  7:    1626   63968025
##  8:    1858   56689050
##  9:     291   36533000
## 10:    2420   26233975
```

```r
sold_data[date%in%20150514,sum(sale),by=book_id][order(-V1)][1:10]
```

```
##     book_id         V1
##  1:    2395 2643430725
##  2:     734  344862000
##  3:     744  249411250
##  4:    2420  143328700
##  5:    1626  107735375
##  6:     692   64024125
##  7:     695   57703525
##  8:    1677   57555200
##  9:     690   54758275
## 10:    1634   43842150
```

```r
sold_data[date%in%20150513,sum(sale),by=book_id][order(-V1)][1:10]
```

```
##     book_id         V1
##  1:    2395 2727884600
##  2:     734  387693500
##  3:     744  272526150
##  4:    1626  124457850
##  5:     690   74168875
##  6:     695   66212025
##  7:    1634   45959925
##  8:     692   38447200
##  9:    1677   28643725
## 10:     742   28160500
```

```r
sold_data[date%in%20150512,sum(sale),by=book_id][order(-V1)][1:10]
```

```
##     book_id         V1
##  1:    2395 2993019150
##  2:     734  368339000
##  3:     744  292928275
##  4:    1626  102583525
##  5:     690  100797675
##  6:     695   66525250
##  7:    1634   62439300
##  8:     291   37026000
##  9:     692   36399550
## 10:    1677   33283875
```

```r
### Join rank and sale
# I have to find rank based sale pattern
setkey(sold_data,book_id,date)
setkey(rank_data,book_id,date)
join_data <- merge(sold_data,rank_data,all.y=TRUE)[order(date,rank)]
join_data[sale%in%0,sale:=NA]
nrow(join_data[is.na(sale),])/nrow(join_data)  # 94.24% is NA or 0. This dataset is not good.
```

```
## [1] 0.9424093
```

```r
join_data[,sum(sale,na.rm=TRUE),by=rank]
```

```
##      rank          V1
##   1:    1 11993073725
##   2:    2 65963107175
##   3:    3 12593081075
##   4:    4 51582313750
##   5:    5 42917730800
##  ---                 
## 536:  536     8821300
## 537:  537     3672425
## 538:  538     2415148
## 539:  539     1246525
## 540:  540     5016275
```

```r
join_data[!is.na(sale),.N,by=rank]
```

```
##      rank  N
##   1:    5 34
##   2:    8 37
##   3:   21 15
##   4:   32 14
##   5:   56 13
##  ---        
## 536:   15  3
## 537:  531  2
## 538:    2 24
## 539:   17  4
## 540:    1  5
```

```r
quantile(join_data[,sum(sale,na.rm=TRUE),by=rank]$V1,
         probs=c(.8,.9,.99,.999))         # There are big difference
```

```
##         80%         90%         99%       99.9% 
##   348713690   710296933 17834741654 58211859519
```

```r
join_data[,sum(sale,na.rm=TRUE)/join_data[,sum(sale,na.rm=TRUE)],by=rank]  # proportion of each rank
```

```
##      rank           V1
##   1:    1 2.952997e-02
##   2:    2 1.624178e-01
##   3:    3 3.100734e-02
##   4:    4 1.270086e-01
##   5:    5 1.056743e-01
##  ---                  
## 536:  536 2.172026e-05
## 537:  537 9.042435e-06
## 538:  538 5.946701e-06
## 539:  539 3.069258e-06
## 540:  540 1.235133e-05
```

```r
### Visualize sale rate of ranking
# Use mean instead of sum because of many NAs.
dat <- join_data[,mean(sale,na.rm=TRUE),by=rank]
dat$V1 <- dat$V1/sum(dat$V1)
dat[,category:=rep(c("Rank 1~10","Rank ~100","Rank ~540"),c(10,90,440))]
dat <- dat[,sum(V1),by=category]
dat$ymax = cumsum(dat$V1)
dat$ymin = c(0, head(dat$ymax, n=-1))
ggplot(dat,aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
    geom_rect() +
    coord_polar(theta="y") +
    xlim(c(0, 4)) +
    theme(panel.grid=element_blank()) +
    theme(axis.text=element_blank()) +
    theme(axis.ticks=element_blank()) +
    annotate("text", x = 0, y = 0, label = "순위별 매출 비중") +
    labs(title="") + ylab("") + xlab("")
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/eda-2.png)<!-- -->

  `sale`의 평균과 합계를 `rank `별로 비교한 결과, Pareto 분포와 같이 long tail 분포라는 것을 파악했습니다. 일반적으로 판매량 데이터는 포아송 분포, 판매 재고 관리와 관련해서는 음이항 분포로 가정하는 경우가 많다고 합니다. 그래서 이러한 분포를 적용할 수 있을지 확인하는 차원에서 시뮬레이션을 진행했습니다. 하지만 pdf 꼴이 데이터와는 상당한 차이가 있다고 판단하고 long tail, extreme value를 반영할 수 있는 방법을 찾기로 결정했습니다.


```r
############ rank - sale relationship -> skewed long tail distribution.
joint <- join_data[!is.na(sale),.(sum=sum(sale),mean=mean(sale)),by=rank]
# plot of sum and mean by rank
qplot(data=joint[rank<100], x=rank, y=sum, geom='col')
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/eda2-1.png)<!-- -->

```r
qplot(data=joint[rank<100], x=rank, y=mean, geom='col')
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/eda2-2.png)<!-- -->

```r
# mean plot seems pareto dist.
qplot(data=joint[rank<100], x=mean, geom='density')
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/eda2-3.png)<!-- -->

```r
# is this poisson or nbinom?

# simulate poisson
tmp <- data.frame(replicate(5000,rpois(1000,1000)))
for(i in 1:5000){
    tmp[,i] <- sort(tmp[,i])
}
tmp1 <- apply(tmp,1,sum)
tmp2 <- apply(tmp,1,mean)
tmp <- data.table(rank=1000:1,sum=tmp1,mean=tmp2)
qplot(data=tmp,x=rank,y=sum)
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/eda2-4.png)<!-- -->

```r
qplot(data=tmp,x=rank,y=mean)
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/eda2-5.png)<!-- -->

```r
# simulate nbinom
tmp <- data.frame(replicate(5000,rnbinom(1000,200,.001)))
for(i in 1:5000){
    tmp[,i] <- sort(tmp[,i])
}
tmp2 <- apply(tmp,1,mean)
tmp1 <- apply(tmp,1,sum)
tmp <- data.table(rank=1000:1,sum=tmp1,mean=tmp2)
qplot(data=tmp,x=rank,y=mean)
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/eda2-6.png)<!-- -->

```r
qplot(data=tmp,x=mean, geom='density')    # CLT
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/eda2-7.png)<!-- -->


## 판매량 추정

  전체 데이터를 extreme distritbution에 적합하기 위해 `extremeStat` 패키지를 사용하여 확인했습니다. 그 결과 Generalized Normal, Log-normal (3 parameter)이 적합한 것으로 나왔는데, 이론적 편의를 위해 log normal 분포 `qqPlot`을 검토해보니 어느 정도 적합도가 있을 것으로 판단했으며, GAM 모형에 피팅을 시도했습니다.


```r
############# fit extreme distribution
sale_dist <- distLquantile(na.omit(join_data$sale),probs=c(0.8,0.9,0.99), list=TRUE, quiet=TRUE)
sale_dist$quant
```

```
##                                80%       90%        99%       RMSE
## gno                    42955535.18 111756007 1071915801 0.03432699
## ln3                    42955535.18 111756007 1071915801 0.03432699
## kap                    47324346.32 122441183  952189311 0.05992747
## gpa                    44166467.69  93185833  716322172 0.07336381
## wak                    44166467.69  93185833  716322172 0.07336381
## glo                    43604817.09  89538053  678355511 0.07822858
## gev                    43088899.38  88480325  676458057 0.07911660
## wei                    41847729.65 130705758 1223901106 0.07974483
## pe3                    33741284.97 165293865 1258956147 0.14965227
## exp                   149560959.55 237502524  529638076 0.19675978
## gam                    53770033.38 196098247 1119009119 0.21996366
## gum                   156687026.02 225365702  440416383 0.22298507
## ray                   165697066.03 226499217  380108205 0.22947533
## nor                   166870158.38 216335108  333810065 0.25148035
## lap                    49809355.73  96842311  253082405 0.26405444
## revgum                168618993.12 201396441  264832870 0.28307424
## rice                  103411511.55 123691432  174926100 0.41773303
## quantileMean           34094597.92 102696924 1244154521         NA
## weighted1              69746241.57 138765628  789773220         NA
## weighted2              68070134.05 137416968  802104029         NA
## weighted3              42896845.39 110582776  925424799         NA
## weightedc                      NaN       NaN        NaN         NA
## GPD_LMO_lmomco         44214945.69  93280372  716913280 0.07329158
## GPD_LMO_extRemes       34961212.95  73024356  603395725 0.08473508
## GPD_PWM_evir           34961625.21  73027894  603313893 0.08484476
## GPD_PWM_fExtremes      34990211.36  73079814  603690870 0.08484476
## GPD_MLE_extRemes       48076439.65 119297933 1904057405 0.09481057
## GPD_MLE_ismev          33211338.54 103727689 3881125060 0.02324297
## GPD_MLE_evd           191857183.93 347850204 1661765124 0.33597239
## GPD_MLE_Renext_Renouv           NA        NA         NA         NA
## GPD_MLE_evir           33140701.82 103812085 3925233229 0.02318014
## GPD_MLE_fExtremes      33180896.48 103931092 3929613831 0.02318014
## GPD_GML_extRemes       39270537.77  87295425  916713181 0.08773205
## GPD_MLE_Renext_2par    33150658.12 103535820 3873626547 0.02327843
## GPD_BAY_extRemes                NA        NA         NA         NA
## n_full                     5622.00        NA         NA         NA
## n                          5622.00        NA         NA         NA
## threshold                     4.25        NA         NA         NA
```

```r
plotLquantile(sale_dist, nbest=3, linargs=list(lwd=1), heights=seq(0.04, 0.01, len=8), breaks=80)
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/fit-1.png)<!-- -->

```r
# most fitted distribution is log normal dist or generalized normal.
logSale_ext <- distLquantile(log(na.omit(join_data$sale)),probs=c(0.8,0.9,0.99), list=TRUE, quiet=TRUE)
plotLquantile(logSale_ext, nbest=3, linargs=list(lwd=1), heights=seq(0.04, 0.01, len=8), breaks=80)
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/fit-2.png)<!-- -->

```r
### dignosis for log-normal
join_data[!is.na(sale),logSale:=log(sale)]
joint[,logMean:=log(mean)]
logSale <- as.vector(na.omit(join_data$logSale))
# normal qqplot
qqPlot(logSale)
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/fit-3.png)<!-- -->

```r
# exponential qqplot
params <- as.list(fitdistr(logSale, "exponential")$estimate)
qplot(sample = logSale, geom = 'blank') +
  stat_qq(distribution = qexp, dparams = params)
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/fit-4.png)<!-- -->

```r
# not clearly log-normal, but close.

##### fit log-normal dist.
fitdist(logSale, "lnorm",  start = c(meanlog=0, sdlog=0.1), method='mle')
```

```
## Fitting of the distribution ' lnorm ' by maximum likelihood 
## Parameters:
##          estimate  Std. Error
## meanlog 2.7404052 0.002301849
## sdlog   0.1725924 0.001629929
```

```r
# mean=2.7404, sd=.17259

############## GAM
# comparing various distributions.
mGG <- gamlss(logMean ~ poly(rank,3), family = GG, data = joint, trace = FALSE)
mLOGNO <- gamlss(logMean ~ poly(rank,3), family = LOGNO, data = joint, trace = FALSE)
msep1 <- gamlss(logMean ~ poly(rank,3), family = SEP1, data = joint, trace = FALSE)
msep2 <- gamlss(logMean ~ poly(rank,3), family = SEP2, data = joint, trace = FALSE)
msep3 <- gamlss(logMean ~ poly(rank,3), family = SEP3, data = joint, trace = FALSE,
                  method = mixed(10, 40))
msep4 <- gamlss(logMean ~ poly(rank,3), family = SEP4, data = joint, trace = FALSE,
                  method = mixed(20, 50))
mst1 <- gamlss(logMean ~ poly(rank,3), family = ST1, data = joint, trace = FALSE)
mst2 <- gamlss(logMean ~ poly(rank,3), family = ST2, data = joint, trace = FALSE,
                 method = mixed(10, 40))
mst3 <- gamlss(logMean ~ poly(rank,3), family = ST3, data = joint, trace = FALSE,
                 method = mixed(10, 40))
mst4 <- gamlss(logMean ~ poly(rank,3), family = ST4, data = joint, trace = FALSE)
mst5 <- gamlss(logMean ~ poly(rank,3), family = ST5, data = joint, trace = FALSE)
mjsu <- gamlss(logMean ~ poly(rank,3), family = JSU, data = joint, trace = FALSE)
# comparing criteria - AIC
GAIC(mGG, mLOGNO, msep1, msep2, msep3, msep4, mst1, mst2, mst3, mst4, mst5, mjsu)
```

```
##        df      AIC
## mst3    7 834.7535
## mst2    7 835.1555
## mjsu    7 835.7001
## mst5    7 838.0036
## mst1    7 838.9213
## msep1   7 840.6989
## msep2   7 841.0890
## msep3   7 845.0533
## mst4    7 846.3773
## msep4   7 849.2603
## mGG     6 945.3439
## mLOGNO  5 949.1612
```

```r
### mst3 - Skew T type 3 distribution is best model.
wp(mst3)
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/fit-5.png)<!-- -->

```r
### plot is almost uniformly distributed - model is okay.
joint <- joint[order(rank)]
model_prediction <- predict(mst3,newdata=data.frame(rank=1:540))
joint[,mst3_pred:=model_prediction]
```

##### 판매량 추정을 위한 변수 추가 탐색

  `date` 속성을 통해 더 나은 추정을 할 수 없을지 탐색한 결과, 큰 의미가 없을 것으로 판단했습니다. 그러나 전체 `sale` 데이터와 비교하여 월별로 구분된 `sale`은 bimodal distribution에 가까운 것으로 보여, `mclust` 패키지를 통해 몇개의 분포로 구분되는지를 확인했습니다. 그 결과 4가지 분포로 나눌 수 있다는 결론을 얻었고, rank에 따라 4가지 분포에서 판매가 발생한다는 가설을 세웠습니다.


```r
################## date variable & time series
join_data[,month:=str_sub(date,start=5,end=6)]
join_data[,sum(sale,na.rm=T),by=month]
```

```
##    month           V1
## 1:    01  75217657492
## 2:    02  74429807405
## 3:    03  67282830805
## 4:    04  56529247960
## 5:    05 132672778560
## 6:    06            0
```

```r
join_data[,mean(sale,na.rm=T),by=month]
```

```
##    month        V1
## 1:    01  68629250
## 2:    02  70818085
## 3:    03  57067711
## 4:    04  52148753
## 5:    05 109465989
## 6:    06       NaN
```

```r
join_data[!is.na(sale),.N,by=month]
```

```
##    month    N
## 1:    01 1096
## 2:    02 1051
## 3:    03 1179
## 4:    04 1084
## 5:    05 1212
```

```r
qplot(data=join_data[date<20150301,],x=logSale,bins=100) + ggtitle("logSale on Jan,Feb")
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/clust-1.png)<!-- -->

```r
qplot(data=join_data[date>=20150301,],x=logSale,bins=100)+ ggtitle("logSale on Mar,Apr,May")
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/clust-2.png)<!-- -->

```r
qplot(data=join_data[month%in%"01",],x=logSale,bins=100) + ggtitle("logSale on Jan")
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/clust-3.png)<!-- -->

```r
qplot(data=join_data[month%in%"02",],x=logSale,bins=100) + ggtitle("logSale on Feb")
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/clust-4.png)<!-- -->

```r
qplot(data=join_data[month%in%"03",],x=logSale,bins=100) + ggtitle("logSale on Mar")
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/clust-5.png)<!-- -->

```r
qplot(data=join_data[month%in%"04",],x=logSale,bins=100) + ggtitle("logSale on Apr")
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/clust-6.png)<!-- -->

```r
qplot(data=join_data[month%in%"05",],x=logSale,bins=100) + ggtitle("logSale on May")
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/clust-7.png)<!-- -->

```r
# seems little bit bimodal normal
one_ln <- Mclust(na.omit(join_data$logSale),G=1)
summary(one_ln)
```

```
## ----------------------------------------------------
## Gaussian finite mixture model fitted by EM algorithm 
## ----------------------------------------------------
## 
## Mclust X (univariate normal) model with 1 component:
## 
##  log.likelihood    n df       BIC       ICL
##       -12517.08 5622  2 -25051.44 -25051.44
## 
## Clustering table:
##    1 
## 5622
```

```r
two_ln <- Mclust(na.omit(join_data$logSale),G=2)
summary(two_ln)
```

```
## ----------------------------------------------------
## Gaussian finite mixture model fitted by EM algorithm 
## ----------------------------------------------------
## 
## Mclust V (univariate, unequal variance) model with 2 components:
## 
##  log.likelihood    n df       BIC       ICL
##       -12321.11 5622  5 -24685.39 -24688.16
## 
## Clustering table:
##    1    2 
##   19 5603
```

```r
three_ln <- Mclust(na.omit(join_data$logSale),G=3)
summary(three_ln)
```

```
## ----------------------------------------------------
## Gaussian finite mixture model fitted by EM algorithm 
## ----------------------------------------------------
## 
## Mclust V (univariate, unequal variance) model with 3 components:
## 
##  log.likelihood    n df       BIC       ICL
##       -12280.78 5622  8 -24630.64 -28156.82
## 
## Clustering table:
##    1    2    3 
##   30 3355 2237
```

```r
auto_ln <- Mclust(na.omit(join_data$logSale))
summary(auto_ln)
```

```
## ----------------------------------------------------
## Gaussian finite mixture model fitted by EM algorithm 
## ----------------------------------------------------
## 
## Mclust V (univariate, unequal variance) model with 4 components:
## 
##  log.likelihood    n df       BIC       ICL
##       -12188.23 5622 11 -24471.44 -26851.09
## 
## Clustering table:
##    1    2    3    4 
##   59 3062 1666  835
```

```r
1-pchisq(92.56397, df=3)
```

```
## [1] 0
```

```r
join_data[!is.na(sale),clust:=auto_ln$classification]
qplot(data=join_data[clust%in%"4"],x=month) + ggtitle("relationship between clust4 and month")
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/clust-8.png)<!-- -->

```r
qplot(data=join_data[clust%in%"3"],x=month) + ggtitle("relationship between clust3 and month")
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/clust-9.png)<!-- -->

```r
qplot(data=join_data[clust%in%"2"],x=month) + ggtitle("relationship between clust2 and month")
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/clust-10.png)<!-- -->

```r
qplot(data=join_data[clust%in%"1"],x=month) + ggtitle("relationship between clust1 and month")
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/clust-11.png)<!-- -->

```r
# no serious signal on plot. month is not important variable

qplot(data=join_data[clust%in%"4"],x=rank) + ggtitle("relationship between clust4 and rank")
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/clust-12.png)<!-- -->

```r
qplot(data=join_data[clust%in%"3"],x=rank) + ggtitle("relationship between clust3 and rank")
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/clust-13.png)<!-- -->

```r
qplot(data=join_data[clust%in%"2"],x=rank) + ggtitle("relationship between clust2 and rank")
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/clust-14.png)<!-- -->

```r
qplot(data=join_data[clust%in%"1"],x=rank) + ggtitle("relationship between clust1 and rank")
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/clust-15.png)<!-- -->

```r
# rank is criteria of clust.

join_data[,weekend:=FALSE]
join_data[wday(ymd(join_data$date)) %in% c(1, 7),weekend:=TRUE]
join_data[,mean(sale,na.rm=T),by=weekend]
```

```
##    weekend       V1
## 1:   FALSE 70884086
## 2:    TRUE 75540158
```

```r
# weekend, weekday is not significant.


join_data[,date:=ymd(date)]
plot_ly(data=join_data[,.(logSale=log(sum(sale,na.rm=T))),by=date],x=~date,y=~logSale) %>% add_lines(mode='lines')
```

<!--html_preserve--><div id="279c373bc0a" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="279c373bc0a">{"x":{"visdat":{"279c53f62e83":["function () ","plotlyVisDat"]},"cur_data":"279c53f62e83","attrs":{"279c53f62e83":{"x":{},"y":{},"alpha":1,"sizes":[10,100],"type":"scatter","mode":"lines"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"title":"date"},"yaxis":{"domain":[0,1],"title":"logSale"},"hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"data":[{"x":["2015-01-01","2015-01-02","2015-01-03","2015-01-04","2015-01-05","2015-01-06","2015-01-07","2015-01-08","2015-01-09","2015-01-10","2015-01-11","2015-01-12","2015-01-13","2015-01-14","2015-01-15","2015-01-16","2015-01-17","2015-01-18","2015-01-19","2015-01-20","2015-01-21","2015-01-22","2015-01-23","2015-01-24","2015-01-25","2015-01-26","2015-01-27","2015-01-28","2015-01-29","2015-01-30","2015-01-31","2015-02-01","2015-02-02","2015-02-03","2015-02-04","2015-02-05","2015-02-06","2015-02-07","2015-02-08","2015-02-09","2015-02-10","2015-02-11","2015-02-12","2015-02-13","2015-02-14","2015-02-15","2015-02-16","2015-02-17","2015-02-18","2015-02-19","2015-02-20","2015-02-21","2015-02-22","2015-02-23","2015-02-24","2015-02-25","2015-02-26","2015-02-27","2015-02-28","2015-03-01","2015-03-02","2015-03-03","2015-03-04","2015-03-05","2015-03-06","2015-03-07","2015-03-08","2015-03-09","2015-03-10","2015-03-11","2015-03-12","2015-03-13","2015-03-14","2015-03-15","2015-03-16","2015-03-17","2015-03-18","2015-03-19","2015-03-20","2015-03-21","2015-03-22","2015-03-23","2015-03-24","2015-03-25","2015-03-26","2015-03-27","2015-03-28","2015-03-29","2015-03-30","2015-03-31","2015-04-01","2015-04-02","2015-04-03","2015-04-04","2015-04-05","2015-04-06","2015-04-07","2015-04-08","2015-04-09","2015-04-10","2015-04-11","2015-04-12","2015-04-13","2015-04-14","2015-04-15","2015-04-16","2015-04-17","2015-04-18","2015-04-19","2015-04-20","2015-04-21","2015-04-22","2015-04-23","2015-04-24","2015-04-25","2015-04-26","2015-04-27","2015-04-28","2015-04-29","2015-04-30","2015-05-01","2015-05-02","2015-05-03","2015-05-04","2015-05-05","2015-05-06","2015-05-07","2015-05-08","2015-05-09","2015-05-10","2015-05-11","2015-05-12","2015-05-13","2015-05-14","2015-05-15","2015-05-16","2015-05-17","2015-05-18","2015-05-19","2015-05-20","2015-05-21","2015-05-22","2015-05-23","2015-05-24","2015-05-25","2015-05-26","2015-05-27","2015-05-28","2015-05-29","2015-05-30","2015-05-31","2015-06-01","2015-06-02","2015-06-03","2015-06-04","2015-06-05","2015-06-06","2015-06-07","2015-06-08","2015-06-09","2015-06-10","2015-06-11","2015-06-12","2015-06-13","2015-06-14","2015-06-15","2015-06-16","2015-06-17","2015-06-18","2015-06-19","2015-06-20","2015-06-21","2015-06-22","2015-06-23","2015-06-24","2015-06-25","2015-06-26","2015-06-27","2015-06-28","2015-06-29","2015-06-30"],"y":[22.6990421343502,21.87125963316,21.632400350058,21.6249246921024,21.4746210417015,21.7306555813664,21.6570148836958,21.8738623410653,21.7389366186389,21.6564418848994,21.5929385724905,21.5445000124611,21.566479247241,21.5368941224315,21.5626652026171,21.4759117457424,21.4894146451992,21.419967334307,21.29111191692,21.3163954276879,21.3186351690983,21.1572052615575,21.3152211124943,21.2789481206447,21.6398396975358,21.2960971787762,21.2855974385762,21.1290119029156,21.8286375464199,21.765598919658,21.5729390842954,22.8784799113354,22.0973605250011,21.8998710153845,21.8037754353563,21.8482429798795,21.7593540824974,21.7503709706965,21.6213759480543,21.5106971290372,21.5154761447082,21.4717464456141,21.5309307612281,21.4320997695301,21.5539190846987,21.3992898912139,21.3361097024402,22.0156122927571,21.9023311652442,21.6390175675855,21.4799556178794,21.4807815620894,21.4518886581121,21.2909411664903,22.0815779853728,18.8460337534985,21.345439280042,21.4742632402344,21.4630976069846,22.7969703196925,21.9427486936373,21.7940409081202,21.6372116737017,21.6523468167545,21.5739793057106,21.5584371307642,21.494833194974,21.4435709241265,21.4663523544571,21.4000920594271,21.4692919588411,21.4292203177885,21.9010295559877,21.6772198409458,21.3493360091346,21.2844525360688,21.309755626686,21.1246447201307,21.3498186563104,21.1388310575942,21.14037878729,21.0298233382287,20.9970396274056,21.0029305977846,21.3298272399364,21.096309523308,21.0611051083924,21.1909914813663,21.1523778126702,21.0960040170314,22.4182336362971,21.6983267682582,21.6814747348053,21.4522226997508,21.4705853278598,21.1897042260823,21.1205533629201,21.0250249894866,21.1116678617786,21.0727478602448,20.9492890261208,20.8868492405655,20.7867119545973,21.3519793908672,21.2495283470145,21.4698098013205,21.288530945947,21.1642989487053,21.2901853462264,21.1717717944946,21.0934067698608,21.1543525260202,21.3121275709768,21.1839155434055,21.2066090655449,20.9806373351969,20.9973669219869,20.9873825956676,21.1258048314902,22.3512090271493,23.2455700808827,22.6507314941434,22.5196058506944,22.4266534321738,22.4410260134483,22.3028601190388,22.319622283077,22.3385016534044,22.2930214291431,22.2163799980818,22.170345990894,22.1604651798484,22.0846190012064,22.05074397219,22.0191749708119,22.0606090692584,21.9384449143127,22.075383852108,22.1250697127111,22.0947540538382,22.0125141222221,21.8886290857062,21.8939881410563,21.8359169183288,21.8794339862629,21.7088071375541,21.7604902381475,21.7312207197505,21.8093148806709,21.8558190237606,21.7361252228684,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"type":"scatter","mode":"lines","line":{"fillcolor":"rgba(31,119,180,1)","color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1}},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":{"render":[{"code":"function(el, x) { var ctConfig = crosstalk.var('plotlyCrosstalkOpts').set({\"on\":\"plotly_click\",\"persistent\":false,\"dynamic\":false,\"selectize\":false,\"opacityDim\":0.2,\"selected\":{\"opacity\":1}}); }","data":null}]}}</script><!--/html_preserve-->

```r
# sale drastically increase every 1th day.
```

  앞서 `mclust` 패키지 함수 `Mclust`를 통해 4개의 normal 분포가 합쳐진 mixture distribution일 가능성이 높음을 확인했습니다. cluster별로 log-normal 분포에 피팅하고 모수를 추정하여 4쌍의 log normal 모수를 얻었고, 이를 가지고 `mixtools`패키지의 `normalmixEM`함수로 mixture distribution을 추정했습니다. `ggplot`을 통해 mixture distribution을 시각화하여 검토해보면 합리적인 추론이었다고 보입니다. mixture distribution에서 난수를 추출하기 위해 `generate_sale` 함수를 만들었습니다.



```r
############ log-normal generating
param_clust <- data.frame(meanlog=1:4,sdlog=1:4,
                            lambda=as.vector(table(auto_ln$classification)/auto_ln$n))
for(i in 1:4){
    param_clust[i,1:2] <- fitdistr(na.omit(join_data[clust%in%i,logSale]),"log-normal")$estimate
}

mix_dist <- normalmixEM(join_data[!is.na(sale),logSale],k=4)
```

```
## number of iterations= 811
```

```r
plot_mix_comps <- function(x, mu, sigma, lam) {
    lam * dnorm(x, mu, sigma)
}
ggplot(data.frame(x = mix_dist$x)) +
    geom_histogram(aes(x, ..density..), binwidth = 1, colour = "black", 
                   fill = "white") +
    stat_function(geom = "line", fun = plot_mix_comps,
                  args = list(mix_dist$mu[1], mix_dist$sigma[1], lam = mix_dist$lambda[1]),
                  colour = "red", lwd=1) +
    stat_function(geom = "line", fun = plot_mix_comps,
                  args = list(mix_dist$mu[2], mix_dist$sigma[2], lam = mix_dist$lambda[2]),
                  colour = "blue", lwd = 1) +
    stat_function(geom = "line", fun = plot_mix_comps,
                  args = list(mix_dist$mu[3], mix_dist$sigma[3], lam = mix_dist$lambda[3]),
                  colour = "green",lwd=1) +
    stat_function(geom = "line", fun = plot_mix_comps,
                  args = list(mix_dist$mu[4], mix_dist$sigma[4], lam = mix_dist$lambda[4]),
                  colour = "yellow", lwd = 1) +
    ylab("Density")
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/mix-1.png)<!-- -->

```r
summary(mix_dist)
```

```
## summary of normalmixEM object:
##           comp 1    comp 2    comp 3     comp 4
## lambda  0.562313  0.126013  0.294767  0.0169074
## mu     14.401880 19.257294 16.889422 10.6491155
## sigma   1.256909  1.250452  0.737451  4.8152827
## loglik at estimate:  -12183.65
```

```r
# parameter estimation
beta <- sqrt(log(mix_dist$sigma^2/mix_dist$mu^2+1))
param_mix <- data.frame(lambda=mix_dist$lambda,        # change log-normal parameter
                        beta=beta,
                        alpha=log(mix_dist$mu)-beta^2/2)

beta <- sqrt(log(exp(param_clust$sdlog)^2/exp(param_clust$meanlog)^2+1))
param_clust <- data.frame(lambda=param_clust$lambda,        # change log-normal parameter
                        beta=beta,
                        alpha=param_clust$meanlog-beta^2/2)

param_clust
```

```
##       lambda       beta    alpha
## 1 0.01049449 0.29859547 1.817107
## 2 0.54464603 0.07595583 2.650907
## 3 0.29633582 0.06102225 2.821524
## 4 0.14852366 0.05511381 2.951841
```

```r
param_mix
```

```
##       lambda       beta    alpha
## 1 0.56231312 0.08710847 2.663565
## 2 0.12601260 0.06486567 2.955786
## 3 0.29476690 0.04364270 2.825735
## 4 0.01690738 0.43131719 2.272460
```

```r
# two options for mixture distribution.


generate_logSaleMean_mix <- function(n=100) {
    idx <- sample(size = 540*n,
                  1:4,
                  prob = param_mix$lambda,
                  replace = TRUE)
    sale_rand <-exp(rlnorm(n = 540*n,
            meanlog = param_mix$alpha[idx],
            sdlog = param_mix$beta[idx]))
    dim(sale_rand) <- c(540,n)
    for (i in 1:n) {
        sale_rand[, i] <- sort(sale_rand[, i],decreasing=TRUE)
    }
    sale_rand <- apply(sale_rand,1,mean)
    return(log(sale_rand))
}

generate_logSaleMean_clust <- function(n=100) {
    idx <- sample(size = 540*n,
                  1:4,
                  prob = param_clust$lambda,
                  replace = TRUE)
    sale_rand <-exp(rlnorm(n = 540*n,
            meanlog = param_clust$alpha[idx],
            sdlog = param_clust$beta[idx]))
    dim(sale_rand) <- c(540,n)
    for (i in 1:n) {
        sale_rand[, i] <- sort(sale_rand[, i],decreasing=TRUE)
    }
    sale_rand <- apply(sale_rand,1,mean)
    return(log(sale_rand))
}

generated_sale <- generate_logSaleMean_mix(2000)
generated_sale <- sort(generated_sale,decreasing=T)[1:540]
joint <- joint[order(rank)]
joint[,rand1:=log(generated_sale)]

generated_sale <- generate_logSaleMean_clust(2000)
generated_sale <- sort(generated_sale,decreasing=T)[1:540]
joint <- joint[order(rank)]
joint[,rand2:=log(generated_sale)]
```

### 결론

  전체 데이터셋을 학습시킨 트레이닝 MSE로 추정값을 평가한다면, 전반적으로 cluster 기반 randomNumber 2의 MSE가 가장 높습니다. 하지만, 랭킹 10위 안쪽 데이터가 매출 비중 절반이 넘으므로 전체보단 랭킹 10위권의 MSE를 중점으로 봐야한다고 생각합니다. 그런 경우 randomNumber 2방식이 오히려 가장 좋다고 평가합니다.



```r
lm_pred <- predict(lm(logMean~rank,data=joint))
joint[,lm:=lm_pred]
ggplot(data=joint) +
    geom_point(aes(x=rank, y=logMean)) + 
    geom_line(aes(x=rank, y=lm,colour="lm")) +
    geom_line(aes(x=rank,y=mst3_pred,color="GAM")) +
    geom_line(aes(x=rank,y=rand1,color="randomNumber1")) +
    geom_line(aes(x=rank,y=rand2,color="randomNumber2")) +
    scale_color_manual(name = "imputations", 
       values=c(lm="red", GAM="blue",randomNumber1="green",randomNumber2="purple"))
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/final-1.png)<!-- -->

```r
mse <- function(v1,v2){
    return(mean((v1-v2)^2))
}

joint[,.(MSE_rand1=mse(logMean,rand1),
         MSE_rand2=mse(logMean,rand2),
         MSE_mst3=mse(logMean,mst3_pred),
         MSE_lm=mse(logMean,lm)
         )]
```

```
##    MSE_rand1 MSE_rand2 MSE_mst3    MSE_lm
## 1:  166.5311  166.6543 2.110157 0.4904076
```

```r
joint[rank<=10,.(MSE_rand1=mse(logMean,rand1),
         MSE_rand2=mse(logMean,rand2),
         MSE_mst3=mse(logMean,mst3_pred),
         MSE_lm=mse(logMean,lm)
         )]
```

```
##    MSE_rand1 MSE_rand2 MSE_mst3   MSE_lm
## 1:  307.8897  311.8798 25.59152 6.633327
```

  test set으로 검증한 결과도 유사하게 나왔습니다. rank가 1~30인 경우 cluster 기반의 randomNumber2 방식이, rank가 30~540인 경우 mst3 모델이 가장 MSE가 낮았습니다. 결과적으로 두가지 방식을 혼용하는 것이 최적의 해법이라고 결론 내리겠습니다.



```r
# Data split
train <- join_data[!is.na(sale),]
set.seed(1)
idx <- createDataPartition(train$sale,list=FALSE,p=.7)
test <- train[-idx,]
train <- train[idx,]
train_by_rank <- train[,.(logMean=log(mean(sale))),by=rank][order(rank)]
test_by_rank <- test[,.(logMean=log(mean(sale))),by=rank][order(rank)]
# log-normal 
mst3 <- gamlss(logMean ~ rank, family = SEP3, 
                data = train_by_rank,
                trace = FALSE, method = mixed(10, 40))
model_prediction <- predict(mst3,newdata=data.frame(rank=1:540))
test_by_rank[,mst3_pred:=model_prediction[test_by_rank$rank]]

# random number
mix_dist <- normalmixEM(train[,logSale],k=4)
```

```
## number of iterations= 546
```

```r
beta <- sqrt(log(mix_dist$sigma^2/mix_dist$mu^2+1))
param_mix <- data.frame(lambda=mix_dist$lambda,        # change log-normal parameter
                        beta=beta,
                        alpha=log(mix_dist$mu)-beta^2/2)
test_by_rank[,rand1:=generate_logSaleMean_mix()[test_by_rank$rank]]
test_by_rank[,rand2:=generate_logSaleMean_clust()[test_by_rank$rank]]


# linear regression
lm_pred <- predict(lm(logMean~rank,data=train_by_rank),newdata=data.frame(rank=1:540))
test_by_rank[,lm:=lm_pred[test_by_rank$rank]]

# Visualization
ggplot(data=test_by_rank) +
    geom_point(aes(x=rank, y=logMean)) + 
    geom_line(aes(x=rank, y=lm,colour="lm")) +
    geom_line(aes(x=rank,y=mst3_pred,color="GAM")) +
    geom_line(aes(x=rank,y=rand1,color="randomNumber1")) +
    geom_line(aes(x=rank,y=rand2,color="randomNumber2")) +
    scale_color_manual(name = "imputations", 
       values=c(lm="red", GAM="blue",randomNumber1="green",randomNumber2="purple"))
```

![](IGAWorks_Data_Scientist_Interview_Question_files/figure-html/final2-1.png)<!-- -->

```r
mse <- function(v1,v2){
    return(mean((v1-v2)^2))
}

test_by_rank[,.(MSE_rand1=mse(logMean,rand1),
         MSE_rand2=mse(logMean,rand2),
         MSE_mst3=mse(logMean,mst3_pred),
         MSE_lm=mse(logMean,lm)
         )]
```

```
##    MSE_rand1 MSE_rand2  MSE_mst3    MSE_lm
## 1: 0.9032553  1.051405 0.7965372 0.7388589
```

```r
test_by_rank[rank<=30,.(MSE_rand1=mse(logMean,rand1),
         MSE_rand2=mse(logMean,rand2),
         MSE_mst3=mse(logMean,mst3_pred),
         MSE_lm=mse(logMean,lm)
         )]
```

```
##    MSE_rand1 MSE_rand2 MSE_mst3   MSE_lm
## 1:  2.496304  1.000471 3.872084 2.727345
```

```r
test_by_rank[rank%in%31:540,.(MSE_rand1=mse(logMean,rand1),
         MSE_rand2=mse(logMean,rand2),
         MSE_mst3=mse(logMean,mst3_pred),
         MSE_lm=mse(logMean,lm)
         )]
```

```
##    MSE_rand1 MSE_rand2  MSE_mst3    MSE_lm
## 1: 0.8045408  1.054561 0.6059584 0.6156408
```
