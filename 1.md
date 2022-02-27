논문조사 및 문헌정리
================
‘수학과 201521139 이재학’
2022-02-23

<br/>

##### **라이브러리 설치**

``` r
#install.packages('pracma')                 #Hausdorff Distance
#install.packages('dtw')                    #DTW  
#install.packages('SimilarityMeasures')     #Frechet Distance
#install.packages('TSdist')                 #Frechet Distance
#install.packages('trajectories')           #Discrete Frechet Distance
#install.packages('tictoc')                 #시간측정
#install.packages('ggplot2')                #데이터시각화
#install.packages('glue')                   #Like Python f-string
#install.packages('rgl')                    #3차원 plot
#install.packages('scatterplot3d')          #3차원 scatterplot
```

##### **라이브러리 불러오기**

``` r
library(pracma)
library(dtw)
library(SimilarityMeasures)
library(TSdist)
library(trajectories)
library(tictoc)
library(ggplot2)
library(glue)
library(rgl)
library(scatterplot3d)
```

##### **Eulcidean Distance 사용자 함수 정의**

``` r
euclidean <- function(a, b) sqrt(sum((a - b)^2))
```

##### **데이터 불러오기 -\> 사용자에 맞게 주소 수정 필요\!**

``` r
climate_seoul<-read.csv('C:/Users/이재학/Desktop/climate.csv')
climate_multi<-read.csv('C:/Users/이재학/Desktop/climate_multi.csv')
```

**데이터 : 2009년 서울의 기상**  
**2009-01-01 \~ 2009-12-31 24시**  
**1시간마다의 기온,강수량,풍속,풍향,습도 등등 측정**

``` r
head(climate_seoul)
```

    ##              date Temp
    ## 1 2009-01-01 0:00 -7.6
    ## 2 2009-01-01 1:00 -7.8
    ## 3 2009-01-01 2:00 -8.1
    ## 4 2009-01-01 3:00 -8.5
    ## 5 2009-01-01 4:00 -8.8
    ## 6 2009-01-01 5:00 -9.0

``` r
head(climate_multi)
```

    ##              date temp  X wind_power wind_direction moisture
    ## 1 2009-01-01 0:00 -7.6 NA        2.1            290       55
    ## 2 2009-01-01 1:00 -7.8 NA        2.3            320       54
    ## 3 2009-01-01 2:00 -8.1 NA        2.0            340       52
    ## 4 2009-01-01 3:00 -8.5 NA        2.4            290       52
    ## 5 2009-01-01 4:00 -8.8 NA        2.1            320       53
    ## 6 2009-01-01 5:00 -9.0 NA        1.7            340       55

**시계열 데이터 분석을 위해 1월 vs 2월의 기온(1차원 데이터)비교**

``` r
jan<-climate_seoul[c(1:747),]
feb<-climate_seoul[748:1419,]
tail(jan)
```

    ##                 date Temp
    ## 742 2009-01-31 18:00  8.7
    ## 743 2009-01-31 19:00  7.3
    ## 744 2009-01-31 20:00  6.3
    ## 745 2009-01-31 21:00  5.3
    ## 746 2009-01-31 22:00  4.7
    ## 747 2009-01-31 23:00  4.0

``` r
tail(feb)
```

    ##                  date Temp
    ## 1414 2009-02-28 18:00  8.3
    ## 1415 2009-02-28 19:00  6.7
    ## 1416 2009-02-28 20:00  5.7
    ## 1417 2009-02-28 21:00  5.6
    ## 1418 2009-02-28 22:00  5.7
    ## 1419 2009-02-28 23:00  5.1

##### **데이터 시각화**

``` r
plot(jan[,2],col='red',main='1월 vs 2월(기온)',xlab='date',ylab='temperature',pch=0)
par(new=TRUE)
plot(feb[,2],col='blue',main='1월 vs 2월(기온)',xlab='date',ylab='temperature',pch=1)
legend(x='topright',legend=c("1월","2월"),col=c("red","blue"),pch=c(0,1))
```

![](문헌조사-및-논문정리-최종-R마크다운-_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

-----

##### **1월의 기온이 상대적으로 낮고, 일수가 1월이 더 길기에 데이터 길이가 다름**

-----

##### **Hausdorff Distance**

``` r
hausdorff_dist(jan[,2],feb[,2])
```

    ## [1] NA

``` r
summary(jan[,])
```

    ##      date                Temp        
    ##  Length:747         Min.   :-11.000  
    ##  Class :character   1st Qu.: -5.400  
    ##  Mode  :character   Median : -2.250  
    ##                     Mean   : -1.961  
    ##                     3rd Qu.:  1.300  
    ##                     Max.   : 11.900  
    ##                     NA's   :3

##### **1월의 경우 결측치가 3개 있어서 분석 불가능 -\> 결측치 : 평균값으로 대체**

``` r
hd_jan<-jan[,2]
hd_jan[is.na(hd_jan)]<-mean(hd_jan,na.rm=TRUE)
start_time<-Sys.time()
hausdorff_dist(hd_jan,feb[,2])
```

    ## [1] 4.1

``` r
end_time<-Sys.time()

glue('Hausdorff Distance 거리 값 : {hausdorff_dist(hd_jan,feb[,2])} , 실행 시간 : {end_time-start_time}')
```

    ## Hausdorff Distance 거리 값 : 4.1 , 실행 시간 : 0.110471963882446

##### **Euclidean Distance**

``` r
euclidean(jan[,2],feb[,2])
```

    ## Warning in a - b: 두 객체의 길이가 서로 배수관계에 있지 않습니다

    ## [1] NA

##### **1월이 2월보다 일자가 더 길기에, 데이터의 갯수를 맞춰야 함\!**

##### **마찬가지로 NA값 평균 대체한 데이터로 측정**

``` r
ed_jan<-hd_jan[1:672]
start_time<-Sys.time()
e_d<-euclidean(ed_jan,feb[,2])
end_time<-Sys.time()
glue('Euclidean Distance 거리 값 : {e_d} , 실행 시간 : {end_time-start_time}')
```

    ## Euclidean Distance 거리 값 : 230.976285947503 , 실행 시간 : 0.013962984085083

##### **DTW**

``` r
jan<-climate_seoul[1:747,2]
feb<-climate_seoul[748:1419,2]
dtw_jan<-jan
dtw_jan[is.na(dtw_jan)]<-mean(dtw_jan,na.rm=TRUE)
start_time<-Sys.time()
dtwdistance<-dtw(dtw_jan,feb)
end_time<-Sys.time()
glue('DTW 거리 값 : {dtwdistance$distance} , 실행 시간 : {end_time-start_time}')
```

    ## DTW 거리 값 : 2008.01733870968 , 실행 시간 : 0.169190883636475

##### **Frechet Distance**

``` r
options(max.print=1)
jan<-climate_seoul[1:747,2]
feb<-climate_seoul[748:1419,2]
fd_jan<-jan
fd_jan[is.na(fd_jan)]<-mean(fd_jan,na.rm=TRUE)
start_time<-Sys.time()
fd_distance<-FrechetDistance(fd_jan,feb)
```

    ##             Q1        Q2        Q3        Q4        Q5        Q6        Q7
    ##             Q8        Q9       Q10       Q11       Q12       Q13       Q14
    ##            Q15       Q16       Q17       Q18       Q19       Q20       Q21
    ##            Q22       Q23       Q24       Q25       Q26       Q27       Q28
    ##            Q29       Q30       Q31       Q32       Q33       Q34       Q35
    ##            Q36       Q37       Q38       Q39       Q40       Q41       Q42
    ##            Q43       Q44       Q45       Q46       Q47       Q48       Q49
    ##            Q50       Q51       Q52       Q53        Q54        Q55        Q56
    ##             Q57        Q58       Q59       Q60       Q61       Q62       Q63
    ##            Q64       Q65       Q66       Q67       Q68       Q69       Q70
    ##             Q71        Q72        Q73       Q74        Q75        Q76
    ##             Q77        Q78        Q79        Q80        Q81        Q82
    ##             Q83        Q84       Q85       Q86       Q87       Q88       Q89
    ##            Q90       Q91        Q92        Q93        Q94        Q95        Q96
    ##             Q97        Q98       Q99      Q100      Q101      Q102      Q103
    ##           Q104      Q105      Q106      Q107      Q108      Q109      Q110
    ##           Q111      Q112      Q113      Q114      Q115      Q116      Q117
    ##           Q118      Q119      Q120      Q121      Q122      Q123      Q124
    ##           Q125       Q126       Q127       Q128       Q129       Q130      Q131
    ##           Q132      Q133      Q134      Q135      Q136      Q137      Q138
    ##           Q139      Q140      Q141      Q142      Q143      Q144      Q145
    ##           Q146      Q147       Q148      Q149      Q150       Q151      Q152
    ##            Q153       Q154       Q155       Q156       Q157       Q158
    ##            Q159      Q160      Q161       Q162       Q163       Q164       Q165
    ##            Q166       Q167       Q168       Q169       Q170       Q171
    ##            Q172       Q173       Q174       Q175       Q176       Q177
    ##            Q178       Q179       Q180      Q181      Q182      Q183      Q184
    ##           Q185      Q186      Q187      Q188      Q189      Q190       Q191
    ##            Q192      Q193      Q194      Q195      Q196      Q197      Q198
    ##           Q199      Q200      Q201      Q202      Q203      Q204      Q205
    ##           Q206      Q207      Q208      Q209      Q210      Q211      Q212
    ##           Q213      Q214      Q215      Q216      Q217      Q218      Q219
    ##           Q220      Q221      Q222      Q223      Q224      Q225      Q226
    ##           Q227      Q228      Q229      Q230      Q231      Q232      Q233
    ##           Q234      Q235      Q236      Q237      Q238      Q239      Q240
    ##           Q241      Q242      Q243      Q244      Q245      Q246      Q247
    ##           Q248      Q249      Q250      Q251      Q252      Q253      Q254
    ##           Q255      Q256      Q257      Q258      Q259      Q260      Q261
    ##           Q262      Q263      Q264      Q265      Q266      Q267      Q268
    ##           Q269      Q270      Q271      Q272      Q273      Q274      Q275
    ##           Q276     Q277     Q278     Q279     Q280     Q281     Q282     Q283
    ##          Q284     Q285     Q286     Q287     Q288     Q289     Q290     Q291
    ##          Q292     Q293     Q294     Q295     Q296     Q297     Q298     Q299
    ##          Q300     Q301     Q302     Q303     Q304     Q305      Q306      Q307
    ##           Q308      Q309      Q310      Q311      Q312      Q313      Q314
    ##           Q315      Q316      Q317      Q318      Q319      Q320      Q321
    ##           Q322      Q323      Q324      Q325      Q326      Q327      Q328
    ##           Q329      Q330      Q331      Q332      Q333      Q334      Q335
    ##           Q336      Q337      Q338      Q339      Q340      Q341      Q342
    ##           Q343      Q344      Q345      Q346      Q347      Q348      Q349
    ##           Q350      Q351       Q352      Q353      Q354       Q355       Q356
    ##            Q357       Q358      Q359      Q360      Q361      Q362      Q363
    ##           Q364      Q365      Q366      Q367      Q368      Q369      Q370
    ##           Q371      Q372       Q373       Q374       Q375       Q376       Q377
    ##            Q378       Q379       Q380       Q381      Q382      Q383      Q384
    ##           Q385      Q386      Q387      Q388      Q389      Q390      Q391
    ##           Q392      Q393      Q394       Q395       Q396      Q397       Q398
    ##            Q399       Q400       Q401       Q402       Q403      Q404      Q405
    ##           Q406      Q407      Q408      Q409      Q410      Q411      Q412
    ##           Q413      Q414      Q415      Q416      Q417      Q418      Q419
    ##           Q420      Q421      Q422       Q423       Q424       Q425       Q426
    ##            Q427      Q428       Q429       Q430       Q431       Q432
    ##            Q433       Q434       Q435       Q436       Q437       Q438
    ##            Q439       Q440       Q441       Q442       Q443       Q444
    ##            Q445       Q446       Q447       Q448       Q449       Q450
    ##            Q451       Q452       Q453       Q454       Q455       Q456
    ##            Q457       Q458       Q459       Q460       Q461       Q462
    ##            Q463       Q464       Q465       Q466       Q467       Q468
    ##            Q469       Q470       Q471       Q472      Q473       Q474      Q475
    ##           Q476      Q477      Q478      Q479      Q480      Q481      Q482
    ##           Q483      Q484      Q485      Q486      Q487      Q488      Q489
    ##           Q490      Q491      Q492      Q493       Q494       Q495       Q496
    ##            Q497      Q498      Q499      Q500      Q501      Q502      Q503
    ##           Q504      Q505       Q506       Q507      Q508       Q509       Q510
    ##            Q511       Q512       Q513       Q514       Q515       Q516
    ##            Q517       Q518       Q519       Q520       Q521       Q522
    ##            Q523       Q524       Q525      Q526      Q527      Q528       Q529
    ##           Q530       Q531       Q532       Q533       Q534       Q535
    ##            Q536      Q537      Q538      Q539      Q540      Q541      Q542
    ##           Q543      Q544      Q545      Q546      Q547      Q548      Q549
    ##           Q550      Q551      Q552      Q553      Q554      Q555      Q556
    ##           Q557      Q558      Q559      Q560      Q561      Q562      Q563
    ##           Q564      Q565      Q566      Q567      Q568      Q569      Q570
    ##           Q571      Q572      Q573      Q574      Q575      Q576      Q577
    ##           Q578      Q579      Q580      Q581      Q582      Q583      Q584
    ##           Q585      Q586      Q587      Q588      Q589      Q590      Q591
    ##           Q592      Q593      Q594      Q595      Q596       Q597       Q598
    ##            Q599       Q600       Q601       Q602       Q603       Q604
    ##            Q605       Q606       Q607       Q608       Q609      Q610      Q611
    ##           Q612      Q613      Q614      Q615      Q616      Q617      Q618
    ##           Q619      Q620      Q621      Q622      Q623      Q624      Q625
    ##           Q626      Q627      Q628      Q629      Q630       Q631       Q632
    ##            Q633       Q634       Q635      Q636      Q637      Q638      Q639
    ##           Q640      Q641      Q642      Q643      Q644      Q645       Q646
    ##            Q647       Q648       Q649       Q650       Q651       Q652
    ##            Q653      Q654      Q655       Q656       Q657       Q658      Q659
    ##            Q660       Q661      Q662      Q663      Q664      Q665      Q666
    ##            Q667       Q668       Q669       Q670       Q671       Q672
    ##  [ getOption("max.print") 에 도달했습니다 -- 747 행들을 생략합니다 ]
    ##            Q1       Q2       Q3       Q4       Q5       Q6       Q7       Q8
    ##            Q9      Q10      Q11      Q12      Q13      Q14      Q15      Q16
    ##           Q17      Q18      Q19      Q20      Q21      Q22      Q23      Q24
    ##           Q25      Q26      Q27      Q28      Q29      Q30      Q31      Q32
    ##           Q33      Q34      Q35      Q36      Q37      Q38      Q39      Q40
    ##           Q41      Q42      Q43      Q44      Q45      Q46      Q47      Q48
    ##           Q49      Q50      Q51      Q52      Q53      Q54      Q55      Q56
    ##           Q57      Q58      Q59      Q60      Q61      Q62      Q63      Q64
    ##           Q65      Q66      Q67      Q68      Q69      Q70      Q71      Q72
    ##           Q73      Q74      Q75      Q76      Q77      Q78      Q79      Q80
    ##           Q81      Q82      Q83      Q84      Q85      Q86      Q87      Q88
    ##           Q89      Q90      Q91      Q92      Q93      Q94      Q95      Q96
    ##           Q97      Q98      Q99     Q100     Q101     Q102     Q103     Q104
    ##          Q105     Q106     Q107     Q108     Q109     Q110     Q111     Q112
    ##          Q113     Q114     Q115     Q116     Q117     Q118     Q119     Q120
    ##          Q121     Q122     Q123     Q124     Q125     Q126     Q127     Q128
    ##          Q129     Q130     Q131     Q132     Q133     Q134     Q135     Q136
    ##          Q137     Q138     Q139     Q140     Q141     Q142     Q143     Q144
    ##          Q145     Q146     Q147     Q148     Q149     Q150     Q151     Q152
    ##          Q153     Q154     Q155     Q156     Q157     Q158     Q159     Q160
    ##          Q161     Q162     Q163     Q164     Q165     Q166     Q167     Q168
    ##          Q169     Q170     Q171     Q172     Q173     Q174     Q175     Q176
    ##          Q177     Q178     Q179     Q180     Q181     Q182     Q183     Q184
    ##          Q185     Q186     Q187     Q188     Q189     Q190     Q191     Q192
    ##          Q193     Q194     Q195     Q196     Q197     Q198     Q199     Q200
    ##          Q201     Q202     Q203     Q204     Q205     Q206     Q207     Q208
    ##          Q209     Q210     Q211     Q212     Q213     Q214     Q215     Q216
    ##          Q217     Q218     Q219     Q220     Q221     Q222     Q223     Q224
    ##          Q225     Q226     Q227     Q228     Q229     Q230     Q231     Q232
    ##          Q233     Q234     Q235     Q236     Q237     Q238     Q239     Q240
    ##          Q241     Q242     Q243     Q244     Q245     Q246     Q247     Q248
    ##          Q249     Q250     Q251     Q252     Q253     Q254     Q255     Q256
    ##          Q257     Q258     Q259     Q260     Q261     Q262     Q263     Q264
    ##          Q265     Q266     Q267     Q268     Q269     Q270     Q271     Q272
    ##          Q273     Q274     Q275     Q276     Q277     Q278     Q279     Q280
    ##          Q281     Q282     Q283     Q284     Q285     Q286     Q287     Q288
    ##          Q289     Q290     Q291     Q292     Q293     Q294     Q295     Q296
    ##          Q297     Q298     Q299     Q300     Q301     Q302     Q303     Q304
    ##          Q305     Q306     Q307     Q308     Q309     Q310     Q311     Q312
    ##          Q313     Q314     Q315     Q316     Q317     Q318     Q319     Q320
    ##          Q321     Q322     Q323     Q324     Q325     Q326     Q327     Q328
    ##          Q329     Q330     Q331     Q332     Q333     Q334     Q335     Q336
    ##          Q337     Q338     Q339     Q340     Q341     Q342     Q343     Q344
    ##          Q345     Q346     Q347     Q348     Q349     Q350     Q351     Q352
    ##          Q353     Q354     Q355     Q356     Q357     Q358     Q359     Q360
    ##          Q361     Q362     Q363     Q364     Q365     Q366     Q367     Q368
    ##          Q369     Q370     Q371     Q372     Q373     Q374     Q375     Q376
    ##          Q377     Q378     Q379     Q380     Q381     Q382     Q383     Q384
    ##          Q385     Q386     Q387     Q388     Q389     Q390     Q391     Q392
    ##          Q393     Q394     Q395     Q396     Q397     Q398     Q399     Q400
    ##          Q401     Q402     Q403     Q404     Q405     Q406     Q407     Q408
    ##          Q409     Q410     Q411     Q412     Q413     Q414     Q415     Q416
    ##          Q417     Q418     Q419     Q420     Q421     Q422     Q423     Q424
    ##          Q425     Q426     Q427     Q428     Q429     Q430     Q431     Q432
    ##          Q433     Q434     Q435     Q436     Q437     Q438     Q439     Q440
    ##          Q441     Q442     Q443     Q444     Q445     Q446     Q447     Q448
    ##          Q449     Q450     Q451     Q452     Q453     Q454     Q455     Q456
    ##          Q457     Q458     Q459     Q460     Q461     Q462     Q463     Q464
    ##          Q465     Q466     Q467     Q468     Q469     Q470     Q471     Q472
    ##          Q473     Q474     Q475     Q476     Q477     Q478     Q479     Q480
    ##          Q481     Q482     Q483     Q484     Q485     Q486     Q487     Q488
    ##          Q489     Q490     Q491     Q492     Q493     Q494     Q495     Q496
    ##          Q497     Q498     Q499     Q500     Q501     Q502     Q503     Q504
    ##          Q505     Q506     Q507     Q508     Q509     Q510     Q511     Q512
    ##          Q513     Q514     Q515     Q516     Q517     Q518     Q519     Q520
    ##          Q521     Q522     Q523     Q524     Q525     Q526     Q527     Q528
    ##          Q529     Q530     Q531     Q532     Q533     Q534     Q535     Q536
    ##          Q537     Q538     Q539     Q540     Q541     Q542     Q543     Q544
    ##          Q545     Q546     Q547     Q548     Q549     Q550     Q551     Q552
    ##          Q553     Q554     Q555     Q556     Q557     Q558     Q559     Q560
    ##          Q561     Q562     Q563     Q564     Q565     Q566     Q567     Q568
    ##          Q569     Q570     Q571     Q572     Q573     Q574     Q575     Q576
    ##          Q577     Q578     Q579     Q580     Q581     Q582     Q583     Q584
    ##          Q585     Q586     Q587     Q588     Q589     Q590     Q591     Q592
    ##          Q593     Q594     Q595     Q596     Q597     Q598     Q599     Q600
    ##          Q601     Q602     Q603     Q604     Q605     Q606     Q607     Q608
    ##          Q609     Q610     Q611     Q612     Q613     Q614     Q615     Q616
    ##          Q617     Q618     Q619     Q620     Q621     Q622     Q623     Q624
    ##          Q625     Q626     Q627     Q628     Q629     Q630     Q631     Q632
    ##          Q633     Q634     Q635     Q636     Q637     Q638     Q639     Q640
    ##          Q641     Q642     Q643     Q644     Q645     Q646     Q647     Q648
    ##          Q649     Q650     Q651     Q652     Q653     Q654     Q655     Q656
    ##          Q657     Q658     Q659     Q660     Q661     Q662     Q663     Q664
    ##          Q665     Q666     Q667     Q668     Q669     Q670     Q671     Q672
    ##  [ getOption("max.print") 에 도달했습니다 -- 747 행들을 생략합니다 ]
    ## 
    ## 
    ##  Result = 16.16447

``` r
end_time<-Sys.time()
glue('Frechet Distance 거리 값 : {fd_distance} , 실행 시간 : {end_time-start_time}')
```

    ## Frechet Distance 거리 값 : 16.1644672043343 , 실행 시간 : 31.8308250904083

##### **Discrete Frechet Distance**

``` r
jan<-climate_seoul[1:747,2]
feb<-climate_seoul[748:1419,2]
dfd_jan<-jan
dfd_jan[is.na(dfd_jan)]<-mean(dfd_jan,na.rm=TRUE)
dfd_jan<-data.frame(dfd_jan,y=c(1:length(dfd_jan)))
feb<-data.frame(feb,y=c(1:length(feb)))
date1 <- seq(as.POSIXct("2009-1-1 0:00"), as.POSIXct("2009-1-31 23:00"), by = "hour")
date2 <- seq(as.POSIXct("2009-2-1 0:00"), as.POSIXct("2009-2-28 23:00"), by = "hour")
dfd_jan<-as.Track(dfd_jan$dfd_jan,dfd_jan$y,date1)
feb<-as.Track(feb$feb,feb$y,date2)
start_time<-Sys.time()
dfd_distance<-frechetDist(dfd_jan,feb)
end_time<-Sys.time()
glue('Discrete Frechet Distance 거리 값 : {dfd_distance} , 실행 시간 : {end_time-start_time}')
```

    ## Discrete Frechet Distance 거리 값 : 743.004205910034 , 실행 시간 : 0.898972034454346

##### **DTW Multi**

``` r
jan_multi<-climate_multi[1:747,c("temp","wind_power","wind_direction","moisture")]
feb_multi<-climate_multi[748:1419,c("temp","wind_power","wind_direction","moisture")]
jan_multi<-na.omit(jan_multi)
feb_multi<-na.omit(feb_multi)
start_time<-Sys.time()
dtw_multi_distance<-dtw(jan_multi,feb_multi)
end_time<-Sys.time()
glue('DTW_multi 거리 값 : {dtw_multi_distance$distance} , 실행 시간 : {end_time-start_time}')
```

    ## DTW_multi 거리 값 : 47375.732863482 , 실행 시간 : 0.277275085449219

-----

# 참고 링크

## <라이브러리>

[Discrete Frechet
Distance](https://www.rdocumentation.org/packages/trajectories/versions/0.2-3/topics/frechetDist)  
[Frechet
Distance](https://www.rdocumentation.org/packages/SimilarityMeasures/versions/1.4/topics/Frechet)  
[DTW](https://cran.r-project.org/web/packages/dtw/index.html)  
[Hausdorff
Distance](https://www.rdocumentation.org/packages/pracma/versions/1.9.9/topics/hausdorff_dist)

## <기타>

[실행시간 측정 in
R](https://didalsgur.tistory.com/entry/R-%EB%8F%99%EC%9E%91-%EC%8B%9C%EA%B0%84-running-time-%EA%B3%84%EC%82%B0-5%EA%B0%80%EC%A7%80-%EB%B0%A9%EB%B2%95)

## <논문>

### 메인

[computing the frechet distance between polygonal curves : 선택한 논문,
Frechet
Distance](https://www.worldscientific.com/doi/abs/10.1142/S0218195995000064)

### Frechet Distance 관련

[다차원 프레셰 거리 기반 종단자료 군집분석 : 교수님 및 선배님
논문](https://dcoll.ajou.ac.kr/dcollection/srch/srchDetail/000000030579)  
[대립생성망의 성능 비교에 관한 연구 : FID(Frechet inception
distance](https://www.dbpia.co.kr/Journal/articleDetail?nodeId=NODE07540262)  
[이산 프레셰 거리 척도를 이용한 궤적 유사도 고속계산 휴리스틱 알고리즘 : Frechet Distance 속도 개선 알고리즘 및
간단
설명](https://www.dbpia.co.kr/Journal/articleDetail?nodeId=NODE06648726)
[The Frechet distance between Multivariate Noraml Distributions :
Multinomial 에서Frechet
distance](https://www.sciencedirect.com/science/article/pii/0047259X8290077X)

### DTW 관련

[Computing and Visualizing Dynamic Time Warping Alignments in R The dtw
Package : R이용한 DTW 설명 및 함수
구현](https://www.jstatsoft.org/article/view/v031i07)

### 유사도 비교

[Trajectory similarity measure : 궤적 유사도
비교](https://dl.acm.org/doi/abs/10.1145/2782759.2782767?casa_token=qb7UgPmNj8gAAAAA:SXaxwuIYGoqILUgBOlaj33FbXw9el2PK0pPANBpD6HmojL9Z4h_7f-LmfK5m1swObFWhSdtqDWYf8g)  
[Review on trajectory similarity measures : 유사도들비교 및 TSM 논문 리뷰
관련](https://ieeexplore.ieee.org/abstract/document/7397286)  
[products of Euclidean metrics and applications to proximity questions
among curves : dfd vs DTW](https://arxiv.org/abs/1712.06471)

### 기타

[Interpoint distances : Applications, properties, and visualization :
interpoint
disctance](https://onlinelibrary.wiley.com/doi/abs/10.1002/asmb.2508)

# <참고한 개념>

#### Hausdorff distance

[Hausdorff distance](https://progworks.tistory.com/72)  
[Hausdorff distance
개념](https://dhpark1212.tistory.com/entry/Hausdorff-Distance)  
[Template
Matching](https://velog.io/@codren/%ED%85%9C%ED%94%8C%EB%A6%BF-%EB%A7%A4%EC%B9%AD)

#### Dynamic Time Warping

[DTW Youtube](https://www.youtube.com/watch?v=03J4xzymOWM)  
[DTW tistory1](https://hamait.tistory.com/862)  
[DTW tistory2](https://leo-bb.tistory.com/58)  
[Computing DTW](https://www.youtube.com/watch?v=_K1OsqCicBY)  
[DTW 계산 설명](https://hwa-a-nui.tistory.com/2)  
[DTW 함수
옵션1](https://dynamictimewarping.github.io/py-api/html/api/dtw.dtw.html)  
[DTW 함수
옵션2\_깃허브](https://github.com/DynamicTimeWarping/dtw-python/blob/master/dtw/dtwPlot.py)

#### Frechet distance

[유튜브 : Frechet Distance Between Two Point
Sets](https://www.youtube.com/watch?v=12vrDDBnEFg)  
[위키피디아](https://en.wikipedia.org/wiki/Fr%C3%A9chet_distance)  
[유튜브 : GAN 성능의 정량적 평가 방법 - Python, Deep
Learning](https://www.youtube.com/watch?v=19An2T4utEM)

##### **참고사항**

  - 데이터의 결측치는 평균값 대체
  - SimilarityMeasures 패키지의 Frechet Distance는 input이 matrix여야하고, 너무 느려서
    TSdist로 대체
  - Frechet Distance 출력 함수가 너무 길어서 print max option 적용
  - Discrete Frechet Distance -\> 적절한 라이브러리 찾기 어려움
      - 1.trajectories 라이브러리 : Track object로 변환해야 함\! (이거로) -\> 파이썬으로
        적용했던 값과 큰 차이..
      - 2.Python in R : R에서 쓰는 파이썬 스크립트
  - DTW multi 에서는 결측치 제거

##### **R VS Python**

|           거리기준            | 값(R)  | 실행시간(R) | 값(Python) | 실행시간(Python) |
| :-----------------------: | :---: | :-----: | :-------: | :----------: |
|    Euclidean Distance     |  230  |  0.01초  |    230    |    0.001초    |
|    Hausdorff Distance     |  4.1  |  0.16초  |    19     |    0.006초    |
|     Frechet Distance      | 16.1  |  27.5초  |   11.4    |    12.8초     |
| Discrete Frechet Distance |  743  |   1초    |   11.4    |     7.8초     |
|            DTW            | 2008  |  0.1초   |   1991    |    0.05초     |
|         DTW Multi         | 47375 |  0.1초   |   47375   |    0.07초     |

  - 파이썬 코드로는 결측치 제거, R에서는 결측치 평균값 대체(DTW\_multi는 결측치 제거)
  - Hausdorff distance의 경우 R과 파이썬의 알고리즘이 다름 -\> 거리,출력 형태 다름
  - 거리 값과 실행시간의 경우 유효숫자 고려한 반올림
  - DTW의 경우 R 기반으로 하여 파이썬으로 확장하였기에 비슷함
  - 나머지 거리기준의 경우, 실행하는 코드의 알고리즘에 따라 다를 수 있음
  - 전반적으로, 파이썬 코드의 실행시간이 더 빠르다.
      - [R vs Python 실행시간
        관점](https://yozm.wishket.com/magazine/detail/1178/)
  - 라이브러리 불러올 때, 경고 해결하기
      - [(R마크다운) knitr::chunk 경고 메시지 없애기 (warning,
        message)](https://bpapa.tistory.com/47)

-----
