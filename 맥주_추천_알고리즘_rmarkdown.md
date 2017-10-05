맥주 추천 엔진 설명과 코드
--------------------------

알고리즘은 몇 단계 과정을 걸쳐 발전했습니다.

1.  유클리드 거리를 사용한 단순 알고리즘
2.  코사인 유사도, 상관계수를 추가하여 추천기준 다양화
3.  Keras 패키지를 활용한 사용자 점수 딥러닝 모형 예측
4.  3가지 추천 기준을 모두 활용한 앙상블 모델
5.  추천엔진 성능 테스트

``` r
suppressPackageStartupMessages({
    library(distances)
    library(data.table)
    library(keras)
    library(RANN)}
)
sessionInfo()
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
    ## [1] RANN_2.5.1        keras_2.0.8       data.table_1.10.4 distances_0.1.2  
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_0.12.13    digest_0.6.12   rprojroot_1.2   R6_2.2.2       
    ##  [5] jsonlite_1.5    backports_1.1.1 magrittr_1.5    evaluate_0.10.1
    ##  [9] tfruns_0.9.1    stringi_1.1.5   reticulate_1.2  rmarkdown_1.6  
    ## [13] tools_3.4.2     stringr_1.2.0   yaml_2.1.14     compiler_3.4.2 
    ## [17] tensorflow_1.4  htmltools_0.3.6 knitr_1.17

### 데이터 전처리

``` r
####### Data Read ########
beers <- fread("beer_reviews.csv", stringsAsFactors = FALSE)

score_col <- c("review_overall", "review_aroma", "review_appearance", "review_palate", "review_taste")
scale_beer <- data.table(beers[,scale(.SD), .SDcols=score_col])  # scale function
scale_beer[,beer_name:=beers$beer_name]            # add beer_name column
str(scale_beer)
```

    ## 
    Read 20.2% of 1586614 rows
    Read 35.3% of 1586614 rows
    Read 50.4% of 1586614 rows
    Read 66.2% of 1586614 rows
    Read 82.6% of 1586614 rows
    Read 98.3% of 1586614 rows
    Read 1586614 rows and 13 (of 13) columns from 0.168 GB file in 00:00:08
    ## Classes 'data.table' and 'data.frame':   1586614 obs. of  6 variables:
    ##  $ review_overall   : num  -3.213 -1.132 -1.132 -1.132 0.256 ...
    ##  $ review_aroma     : num  -2.49 -1.77 -1.77 -1.05 1.1 ...
    ##  $ review_appearance: num  -2.178 -1.366 -1.366 -0.555 0.257 ...
    ##  $ review_palate    : num  -3.289 -1.09 -1.09 -1.823 0.376 ...
    ##  $ review_taste     : num  -3.132 -1.083 -1.083 -1.083 0.966 ...
    ##  $ beer_name        : chr  "Sausa Weizen" "Red Moon" "Black Horse Black Beer" "Sausa Pils" ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

먼저 데이터를 읽어오고 분석에 필요한 점수와 맥주명만 남깁니다. 또한 각 평가 항목마다 편차가 있을 수 있으므로 이를 보정하기 위해 `scale`함수로 표준화합니다.

추천 엔진은 2가지 방법으로 구성할 수 있습니다. 하나는 동일한 사람이 평가한 여러 맥주를 비교하는 방법, 다른 방법은 동일한 맥주에 매겨진 평가 데이터를 하나의 수치로 정리하여 전체 맥주 간 차이를 비교하는 방법이 있습니다. 여기서는 후자의 방법 택했으며 각각 맥주 평가 항목의 평균값이 맥주 간 차이를 비교하는 특성값이 됩니다.

------------------------------------------------------------------------

### 추천 엔진 1 - 유클리드 거리

``` r
####################### Logic 1 Euclid ############################
euclid <- function(user_input, n=5, analytic_data=scale_beer,filter=TRUE){
   
     mean_beer <-         # calculate mean first to compare each beers.
        analytic_data[,lapply(.SD,mean),.SDcols=score_col, by=beer_name]
    
    # If data size very large, filter=TRUE
    if(filter){
    # ANN clustering algorithm is very fast for high dimensional data
        if(nrow(mean_beer)<=1000) nk=nrow(mean_beer) else nk=1000
        target <- nn2(data=mean_beer[,-1],                          # filtering by cluster
            query=mean_beer[beer_name %chin% user_input, ][,-1],    # ANN algorithm calculate
            k=nk)                                                   # nearest 1000 beers
    
    target_idx <- target$nn.idx[1,]                      # shrink data dimension
    mean_beer <- mean_beer[target_idx,]                  # 56857 -> 1000
    }
    # calculate euclidean distance. 
    euc_dist=distance_columns(distances(mean_beer,id_variable="beer_name"),1)
    result <- data.table(beer_name=mean_beer$beer_name,
                         euc_dist=as.vector(euc_dist))
    result <- result[-1,]
    return(result[order(euc_dist)][1:n])
}
```

첫번째는 유클리드 거리입니다. 각 맥주마다 여러 건의 평가가 있는데, 맥주상품 간 비교를 위해서는 단일값이 필요하므로 평균을 구합니다. 코드의 특징은 ANN 군집 알고리즘으로 유사한 군집끼리 묶어주고 동일 군집 내에서 유클리드 거리를 계산합니다. 전체 데이터에 대해 유클리드 거리를 구하면 distance matrix 계산에 엄청난 컴퓨팅 파워가 필요하기 때문에 `(차원의 저주)` 불가피하게 비교 데이터를 한정할 필요가 있습니다.

------------------------------------------------------------------------

### 추천 엔진 2,3 - 코사인 거리, 상관계수

``` r
######################### Logic 2 cosine ##############################
cosine <- function(user_input, n = 5, analytic_data=scale_beer, filter=TRUE) {
    cos_similarity <- function(ma, mb) {     # cosine similiarity formula
        ma <- as.matrix(ma)
        mb <- as.matrix(mb)
        mat = tcrossprod(ma, mb)
        t1 = sqrt(apply(ma, 1, crossprod))
        t2 = sqrt(apply(mb, 1, crossprod))
        mat / outer(t1, t2)
    }
    
    mean_beer <-              # calculate mean first to compare each beers.
        analytic_data[, lapply(.SD, mean), .SDcols = score_col, by = beer_name]
    
    # If data size very large, filter=TRUE
    if(filter){
        if(nrow(mean_beer)<=1000) nk=nrow(mean_beer) else nk=1000
       target <- nn2(data=mean_beer[,-1],                         # filtering by cluster
            query=mean_beer[beer_name %chin% user_input, ][,-1],  # ANN algorithm calculate
            k=nk)                                                 # nearest 1000 beers
    
    target_idx <- target$nn.idx[1,]                      # shrink data dimension
    mean_beer <- mean_beer[target_idx,]                  # 56857 -> 1000
    }
    
    n1 <- nrow(mean_beer)
    result <- data.table(beer_name = mean_beer$beer_name,
                         cos_sim = rep(0, time = n1))
    
    # cosine similarity between user_input and beers within ANN cluster.
    for (i in 2:n1) {              # fast loop computation 'set'
        set(result, i, 2L, cos_similarity(mean_beer[1, 2:6], mean_beer[i, 2:6]))
    }
    
    return(result[order(-cos_sim)][1:n])
    # bigger number, better similarity
}


######################## Logic 3 corr ######################################
correlation <- function(user_input, n = 5, analytic_data=scale_beer, filter=TRUE) {
    
    mean_beer <-               # calculate mean first to compare each beers.
        analytic_data[, lapply(.SD, mean), .SDcols = score_col, by = beer_name]
    
    # If data size very large, filter=TRUE
    if(filter){
        if(nrow(mean_beer)<=1000) nk=nrow(mean_beer) else nk=1000
        target <- nn2(data=mean_beer[,-1],                          # filtering by cluster
            query=mean_beer[beer_name %chin% user_input, ][,-1],    # ANN algorithm calculate
            k=nk)                                                   # nearest 1000 beers
    
    target_idx <- target$nn.idx[1,]                      # shrink data dimension
    mean_beer <- mean_beer[target_idx,]                  # 56857 -> 1000
    }
    n1 <- nrow(mean_beer)
    cor_result <- data.table(beer_name = mean_beer$beer_name,
                         corr = rep(0, time = n1))
    for (i in 2:n1) { 
        set(cor_result, i, 2L, 
            cor(unlist(mean_beer[1, score_col, with=FALSE]), 
                unlist(mean_beer[i, score_col, with=FALSE])))
    }
    return(cor_result[order(-corr)][1:n])
}
```

앞선 유클리드 거리와 거의 유사한 알고리즘이며 계산 방법이 코사인 유사도, 상관계수로 바뀐 점이 유일한 차이입니다.

------------------------------------------------------------------------

### Deep Learning Prediction

``` r
##################### Deep Learning Logic #########################
idx <- sample.int(n=dim(beers)[1],size=10000)
output <- beers[idx,]$review_overall
input <- as.matrix(beers[idx,c(5:6,9:10)])

model <- keras_model_sequential()
model %>% 
    layer_dense(units = 64, activation = 'relu', input_shape=c(4)) %>%
    layer_dense(units = 1)
summary(model)

model %>% compile(
    loss = 'mse',
    optimizer = 'adam'
)
model %>% fit(
    input, output, 
    epochs = 5, 
    batch_size = 24
)


deep_learning <- function(recommeded_item){
    # predict for 
    pred_input <- beers[beer_name %chin% recommeded_item,lapply(.SD,mean,2),.SDcols=score_col]
    expected_score <- predict_on_batch(model,as.matrix(pred_input[,-1]))[1,1]
    return(expected_score)
}
```

    ## ___________________________________________________________________________
    ## Layer (type)                     Output Shape                  Param #     
    ## ===========================================================================
    ## dense_1 (Dense)                  (None, 64)                    320         
    ## ___________________________________________________________________________
    ## dense_2 (Dense)                  (None, 1)                     65          
    ## ===========================================================================
    ## Total params: 385
    ## Trainable params: 385
    ## Non-trainable params: 0
    ## ___________________________________________________________________________

딥러닝 모형을 트레이닝하고 예측하는 코드입니다. 맥주 평가 데이터는 `"review_overall", "review_aroma", "review_appearance", "review_palate", "review_taste"`로 구성됐는데 이중 review\_overall은 반응 변수, 나머지 값은 독립 변수로 설정했습니다. 컴퓨팅 파워를 감안하여 2개의 레이어로 구성했으며 딥러닝 모형 파라미터 부분은 자세히 알지 못합니다.

------------------------------------------------------------------------

### 추천 모형 4 - 앙상블 모델

``` r
#################### Ensemble #####################
kernel_ensemble <- function(user_input, ensemble_data, iter){

    ens_result <- vector()
    nr <- nrow(ensemble_data)
    resample <- copy(ensemble_data[1==0,])
    
    for (i in 1:iter) {
        while (nrow(resample[beer_name %chin% user_input, ]) == 0){
            idx <- sample.int(nr, floor(nr*.7))
            resample <- ensemble_data[idx, ]
        }
        ens_result[3*i-2] <- euclid(user_input,n=1,resample,filter=TRUE)$beer_name
        ens_result[3*i-1] <- cosine(user_input,n=1,resample,filter=TRUE)$beer_name
        ens_result[3*i] <- correlation(user_input,n=1,resample,filter=TRUE)$beer_name
        resample <- copy(ensemble_data[1==0,])
    }
    
    return(ens_result)
}

ensemble <- function(user_input, ensemble_data=scale_beer, iter=10){
    ensem_result <- kernel_ensemble(user_input, ensemble_data, iter)
    return(data.frame(
        Our_Recommendation=
            names(table(ensem_result)[which.max(table(ensem_result))]),
        Predicted_Score=
            deep_learning(ensem_result),
        stringsAsFactors=FALSE)
    )
}
    

########## Util #############
beer_search <- function(user_input) return(beers[,.N,by=beer_name][beer_name %like% user_input])
```

최종 모형인 앙상블 모형입니다. 전체 데이터에서 원래 데이터의 `70%`크기의 샘플 데이터를 추출하고 각각 유클리드거리, 코사인 유사도, 상관계수를 구하여 가장 유사한 맥주를 `result`에 저장합니다. 이러한 작업을 `iter`번(기본값 10) 반복하여 최다 투표를 받은 맥주를 최종 추천 상품으로 결정합니다. output으로 딥러닝 예측 점수도 함께 출력됩니다.

#### Recommendation

``` r
answer <- ensemble(user_input="Guinness Draught", iter=3)
answer
```

    ##   Our_Recommendation Predicted_Score
    ## 1 Grape Expectations        3.562467

------------------------------------------------------------------------

### Result Test

각 알고리즘별 성능을 측정해보기 위해 여러 방법을 쓸 수 있겠지만, 여기서는 간소화된 방법으로 `Guinness Draught` 평가 점수가 있는 사람(2210명)이 작성한 리뷰를 평가 기준으로 삼겠습니다. 30건 이상의 리뷰가 있는 맥주 4370종에 대해 계산한 평균 맥주 평점을 계산하여 비교했습니다. `Guinness Draught`의 평점과 가장 가까운 경우 추천 성능이 좋다고 할 수 있겠습니다.

``` r
review_filter_reviewer <- beers[beer_name %in% "Guinness Draught",
                         list(review_profilename, review_overall)]
review_filter_beer <- beers[review_profilename %in% review_filter_reviewer$review_profilename,
                                .(mean=mean(review_overall),.N), 
                                by = beer_name][order(-mean)][N>29]
review_filter_data <- beers[beer_name %in% review_filter_beer$beer_name &
                            review_profilename %in% review_filter_reviewer$review_profilename,
                            .SD, .SDcols=c(score_col,"beer_name")]
```

`Sierra Nevada Pale Ale`와 `Stone IPA (India Pale Ale)`가 4.5점 또는 5.0점을 가장 많이 받은 것으로 확인됐습니다. 그렇다면 사용자가 `Sierra Nevada Pale Ale`을 입력했을 때 어떤 맥주를 추천하는지 알고리즘별로 확인하겠습니다.

``` r
set.seed(1001)
test <- function(user_input,analytic_data){
    ens <- ensemble(user_input,analytic_data)$Our_Recommendation
    euc <- euclid(user_input,n=1,analytic_data,filter=TRUE)$beer_name
    cos <- cosine(user_input,n=1,analytic_data,filter=TRUE)$beer_name
    cor <- correlation(user_input,n=1,analytic_data,filter=TRUE)$beer_name
    return(
        rbind(ens,euc,cos,cor)
    )
}
result <- test("Guinness Draught",review_filter_data)
result
review_filter_beer[beer_name %chin% c(result[,1],"Guinness Draught")]
```

    ##     [,1]                        
    ## ens "Tetley's English Ale"      
    ## euc "Tetley's English Ale"      
    ## cos "Belhaven Best"             
    ## cor "Wachusett Country Pale Ale"
    ##                     beer_name     mean    N
    ## 1:       Tetley's English Ale 3.747727  220
    ## 2:           Guinness Draught 3.685068 2210
    ## 3: Wachusett Country Pale Ale 3.559524   42
    ## 4:              Belhaven Best 3.546512   43

결과를 보면 `correlation > cosine similiarity > ensemble > euclid` 순으로
