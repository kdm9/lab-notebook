Nazia's qPCR on Touch Expt
==========================

Load us some data!

```r
library(plyr)
touch_qpcr = read.csv("Assay64_touch_qpcr.csv")
str(touch_qpcr)
```

```
## 'data.frame':	288 obs. of  5 variables:
##  $ Genotype: Factor w/ 2 levels "ccr1","Col": 2 2 2 2 2 2 2 2 2 1 ...
##  $ Rep     : int  1 1 1 2 2 2 3 3 3 1 ...
##  $ Touch   : Factor w/ 2 levels "NotTouched","Touched": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Primer  : Factor w/ 8 levels "Cyclo","Cyclo Prom",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ Value   : num  21.1 20.9 20.7 20.2 20.6 ...
```

```r
summary(touch_qpcr)
```

```
##  Genotype        Rep           Touch            Primer       Value      
##  ccr1:144   Min.   :1   NotTouched:144   Cyclo     :36   Min.   : 8.56  
##  Col :144   1st Qu.:1   Touched   :144   Cyclo Prom:36   1st Qu.:17.11  
##             Median :2                    IC2       :36   Median :20.64  
##             Mean   :2                    PP2A      :36   Mean   :20.70  
##             3rd Qu.:3                    STPK      :36   3rd Qu.:22.82  
##             Max.   :3                    TCH3      :36   Max.   :40.77  
##                                          (Other)   :72
```

```r
summary(touch_qpcr$Primer)
```

```
##      Cyclo Cyclo Prom        IC2       PP2A       STPK       TCH3 
##         36         36         36         36         36         36 
##        TUK       WRKY 
##         36         36
```


Collapse tech reps down to biological reps

```r

touch_qpcr_old = touch_qpcr
touch_qpcr = ddply(touch_qpcr, .(Genotype, Touch, Rep, Primer), summarise, Value = mean(Value))
```




```r
primers = list()
aovs = list()
for (i in 1:length(levels(touch_qpcr$Primer))) {
    primer = levels(touch_qpcr$Primer)[i]
    primers[[i]] = touch_qpcr[touch_qpcr$Primer == primer, ]
    primers[[i]]$Value = primers[[i]]$Value/primers[[1]]$Value
    print(primer)
    print(summary(primers[[i]]))
    aovs[[i]] = aov(Value ~ Genotype * Touch, data = primers[[i]])
    print(summary(aovs[[i]]))
    print(TukeyHSD(aovs[[i]]))
    boxplot(Value ~ Genotype * Touch, data = primers[[i]])
    title(primer)
}
```

```
## [1] "Cyclo"
##  Genotype        Touch        Rep           Primer       Value  
##  ccr1:6   NotTouched:6   Min.   :1   Cyclo     :12   Min.   :1  
##  Col :6   Touched   :6   1st Qu.:1   Cyclo Prom: 0   1st Qu.:1  
##                          Median :2   IC2       : 0   Median :1  
##                          Mean   :2   PP2A      : 0   Mean   :1  
##                          3rd Qu.:3   STPK      : 0   3rd Qu.:1  
##                          Max.   :3   TCH3      : 0   Max.   :1  
##                                      (Other)   : 0              
##                Df   Sum Sq  Mean Sq F value Pr(>F)
## Genotype        1 1.97e-31 1.97e-31       1   0.35
## Touch           1 1.97e-31 1.97e-31       1   0.35
## Genotype:Touch  1 1.97e-31 1.97e-31       1   0.35
## Residuals       8 1.58e-30 1.97e-31               
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = Value ~ Genotype * Touch, data = primers[[i]])
## 
## $Genotype
##                diff        lwr       upr  p adj
## Col-ccr1 -4.441e-16 -1.035e-15 1.472e-16 0.1215
## 
## $Touch
##                          diff        lwr       upr  p adj
## Touched-NotTouched -4.441e-16 -1.035e-15 1.472e-16 0.1215
## 
## $`Genotype:Touch`
##                                      diff        lwr       upr  p adj
## Col:NotTouched-ccr1:NotTouched -6.661e-16 -1.827e-15 4.950e-16 0.3245
## ccr1:Touched-ccr1:NotTouched   -6.661e-16 -1.827e-15 4.950e-16 0.3245
## Col:Touched-ccr1:NotTouched    -6.661e-16 -1.827e-15 4.950e-16 0.3245
## ccr1:Touched-Col:NotTouched     0.000e+00 -1.161e-15 1.161e-15 1.0000
## Col:Touched-Col:NotTouched      0.000e+00 -1.161e-15 1.161e-15 1.0000
## Col:Touched-ccr1:Touched        0.000e+00 -1.161e-15 1.161e-15 1.0000
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-31.png) 

```
## [1] "Cyclo Prom"
##  Genotype        Touch        Rep           Primer       Value     
##  ccr1:6   NotTouched:6   Min.   :1   Cyclo Prom:12   Min.   :31.6  
##  Col :6   Touched   :6   1st Qu.:1   Cyclo     : 0   1st Qu.:31.8  
##                          Median :2   IC2       : 0   Median :32.9  
##                          Mean   :2   PP2A      : 0   Mean   :33.8  
##                          3rd Qu.:3   STPK      : 0   3rd Qu.:35.5  
##                          Max.   :3   TCH3      : 0   Max.   :38.0  
##                                      (Other)   : 0                 
##                Df Sum Sq Mean Sq F value Pr(>F)  
## Genotype        1  17.84   17.84    6.59  0.033 *
## Touch           1   2.95    2.95    1.09  0.327  
## Genotype:Touch  1  12.42   12.42    4.58  0.065 .
## Residuals       8  21.67    2.71                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = Value ~ Genotype * Touch, data = primers[[i]])
## 
## $Genotype
##           diff    lwr  upr  p adj
## Col-ccr1 2.439 0.2478 4.63 0.0333
## 
## $Touch
##                       diff    lwr upr  p adj
## Touched-NotTouched -0.9911 -3.182 1.2 0.3274
## 
## $`Genotype:Touch`
##                                   diff     lwr    upr  p adj
## Col:NotTouched-ccr1:NotTouched  4.4733  0.1702 8.7765 0.0419
## ccr1:Touched-ccr1:NotTouched    1.0433 -3.2598 5.3465 0.8630
## Col:Touched-ccr1:NotTouched     1.4478 -2.8554 5.7509 0.7119
## ccr1:Touched-Col:NotTouched    -3.4300 -7.7331 0.8731 0.1249
## Col:Touched-Col:NotTouched     -3.0256 -7.3287 1.2776 0.1891
## Col:Touched-ccr1:Touched        0.4044 -3.8987 4.7076 0.9898
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-32.png) 

```
## [1] "IC2"
##  Genotype        Touch        Rep           Primer       Value     
##  ccr1:6   NotTouched:6   Min.   :1   IC2       :12   Min.   :8.68  
##  Col :6   Touched   :6   1st Qu.:1   Cyclo     : 0   1st Qu.:8.93  
##                          Median :2   Cyclo Prom: 0   Median :9.20  
##                          Mean   :2   PP2A      : 0   Mean   :9.15  
##                          3rd Qu.:3   STPK      : 0   3rd Qu.:9.27  
##                          Max.   :3   TCH3      : 0   Max.   :9.72  
##                                      (Other)   : 0                 
##                Df Sum Sq Mean Sq F value Pr(>F)
## Genotype        1  0.002   0.002    0.02   0.88
## Touch           1  0.238   0.238    2.80   0.13
## Genotype:Touch  1  0.066   0.066    0.78   0.40
## Residuals       8  0.680   0.085               
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = Value ~ Genotype * Touch, data = primers[[i]])
## 
## $Genotype
##             diff     lwr    upr  p adj
## Col-ccr1 0.02611 -0.3621 0.4143 0.8806
## 
## $Touch
##                      diff     lwr    upr  p adj
## Touched-NotTouched 0.2817 -0.1065 0.6699 0.1328
## 
## $`Genotype:Touch`
##                                   diff     lwr    upr  p adj
## Col:NotTouched-ccr1:NotTouched  0.1744 -0.5880 0.9368 0.8813
## ccr1:Touched-ccr1:NotTouched    0.4300 -0.3324 1.1924 0.3371
## Col:Touched-ccr1:NotTouched     0.3078 -0.4546 1.0702 0.5918
## ccr1:Touched-Col:NotTouched     0.2556 -0.5068 1.0180 0.7141
## Col:Touched-Col:NotTouched      0.1333 -0.6291 0.8957 0.9412
## Col:Touched-ccr1:Touched       -0.1222 -0.8846 0.6402 0.9536
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-33.png) 

```
## [1] "PP2A"
##  Genotype        Touch        Rep           Primer       Value     
##  ccr1:6   NotTouched:6   Min.   :1   PP2A      :12   Min.   :19.8  
##  Col :6   Touched   :6   1st Qu.:1   Cyclo     : 0   1st Qu.:20.2  
##                          Median :2   Cyclo Prom: 0   Median :20.4  
##                          Mean   :2   IC2       : 0   Mean   :20.5  
##                          3rd Qu.:3   STPK      : 0   3rd Qu.:20.7  
##                          Max.   :3   TCH3      : 0   Max.   :21.1  
##                                      (Other)   : 0                 
##                Df Sum Sq Mean Sq F value Pr(>F)
## Genotype        1  0.152  0.1519    0.94   0.36
## Touch           1  0.040  0.0404    0.25   0.63
## Genotype:Touch  1  0.114  0.1141    0.71   0.42
## Residuals       8  1.287  0.1608               
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = Value ~ Genotype * Touch, data = primers[[i]])
## 
## $Genotype
##            diff     lwr    upr  p adj
## Col-ccr1 -0.225 -0.7589 0.3089 0.3596
## 
## $Touch
##                      diff     lwr    upr  p adj
## Touched-NotTouched 0.1161 -0.4178 0.6501 0.6296
## 
## $`Genotype:Touch`
##                                    diff     lwr    upr  p adj
## Col:NotTouched-ccr1:NotTouched -0.03000 -1.0786 1.0186 0.9997
## ccr1:Touched-ccr1:NotTouched    0.31111 -0.7375 1.3597 0.7800
## Col:Touched-ccr1:NotTouched    -0.10889 -1.1575 0.9397 0.9864
## ccr1:Touched-Col:NotTouched     0.34111 -0.7075 1.3897 0.7314
## Col:Touched-Col:NotTouched     -0.07889 -1.1275 0.9697 0.9947
## Col:Touched-ccr1:Touched       -0.42000 -1.4686 0.6286 0.5974
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-34.png) 

```
## [1] "STPK"
##  Genotype        Touch        Rep           Primer       Value     
##  ccr1:6   NotTouched:6   Min.   :1   STPK      :12   Min.   :17.0  
##  Col :6   Touched   :6   1st Qu.:1   Cyclo     : 0   1st Qu.:18.0  
##                          Median :2   Cyclo Prom: 0   Median :19.0  
##                          Mean   :2   IC2       : 0   Mean   :19.0  
##                          3rd Qu.:3   PP2A      : 0   3rd Qu.:19.6  
##                          Max.   :3   TCH3      : 0   Max.   :22.8  
##                                      (Other)   : 0                 
##                Df Sum Sq Mean Sq F value Pr(>F)   
## Genotype        1   7.78    7.78   10.08 0.0131 * 
## Touch           1  16.32   16.32   21.15 0.0018 **
## Genotype:Touch  1   0.03    0.03    0.05 0.8370   
## Residuals       8   6.17    0.77                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = Value ~ Genotype * Touch, data = primers[[i]])
## 
## $Genotype
##           diff    lwr     upr  p adj
## Col-ccr1 -1.61 -2.779 -0.4407 0.0131
## 
## $Touch
##                      diff    lwr    upr  p adj
## Touched-NotTouched -2.332 -3.502 -1.163 0.0018
## 
## $`Genotype:Touch`
##                                   diff    lwr      upr  p adj
## Col:NotTouched-ccr1:NotTouched -1.5022 -3.799  0.79427 0.2333
## ccr1:Touched-ccr1:NotTouched   -2.2244 -4.521  0.07205 0.0576
## Col:Touched-ccr1:NotTouched    -3.9422 -6.239 -1.64573 0.0026
## ccr1:Touched-Col:NotTouched    -0.7222 -3.019  1.57427 0.7501
## Col:Touched-Col:NotTouched     -2.4400 -4.736 -0.14351 0.0378
## Col:Touched-ccr1:Touched       -1.7178 -4.014  0.57872 0.1554
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-35.png) 

```
## [1] "TCH3"
##  Genotype        Touch        Rep           Primer       Value     
##  ccr1:6   NotTouched:6   Min.   :1   TCH3      :12   Min.   :14.5  
##  Col :6   Touched   :6   1st Qu.:1   Cyclo     : 0   1st Qu.:15.2  
##                          Median :2   Cyclo Prom: 0   Median :16.2  
##                          Mean   :2   IC2       : 0   Mean   :16.1  
##                          3rd Qu.:3   PP2A      : 0   3rd Qu.:16.8  
##                          Max.   :3   STPK      : 0   Max.   :18.2  
##                                      (Other)   : 0                 
##                Df Sum Sq Mean Sq F value Pr(>F)   
## Genotype        1   7.39    7.39   15.83 0.0041 **
## Touch           1   5.64    5.64   12.07 0.0084 **
## Genotype:Touch  1   0.03    0.03    0.07 0.8031   
## Residuals       8   3.74    0.47                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = Value ~ Genotype * Touch, data = primers[[i]])
## 
## $Genotype
##            diff    lwr     upr  p adj
## Col-ccr1 -1.569 -2.479 -0.6597 0.0041
## 
## $Touch
##                      diff   lwr     upr  p adj
## Touched-NotTouched -1.371 -2.28 -0.4608 0.0084
## 
## $`Genotype:Touch`
##                                   diff    lwr     upr  p adj
## Col:NotTouched-ccr1:NotTouched -1.4678 -3.254  0.3189 0.1120
## ccr1:Touched-ccr1:NotTouched   -1.2689 -3.056  0.5178 0.1834
## Col:Touched-ccr1:NotTouched    -2.9400 -4.727 -1.1533 0.0033
## ccr1:Touched-Col:NotTouched     0.1989 -1.588  1.9856 0.9834
## Col:Touched-Col:NotTouched     -1.4722 -3.259  0.3145 0.1107
## Col:Touched-ccr1:Touched       -1.6711 -3.458  0.1156 0.0670
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-36.png) 

```
## [1] "TUK"
##  Genotype        Touch        Rep           Primer       Value     
##  ccr1:6   NotTouched:6   Min.   :1   TUK       :12   Min.   :22.4  
##  Col :6   Touched   :6   1st Qu.:1   Cyclo     : 0   1st Qu.:23.5  
##                          Median :2   Cyclo Prom: 0   Median :24.3  
##                          Mean   :2   IC2       : 0   Mean   :24.0  
##                          3rd Qu.:3   PP2A      : 0   3rd Qu.:24.7  
##                          Max.   :3   STPK      : 0   Max.   :25.1  
##                                      (Other)   : 0                 
##                Df Sum Sq Mean Sq F value Pr(>F)   
## Genotype        1   2.93   2.927    15.3 0.0045 **
## Touch           1   2.69   2.695    14.1 0.0056 **
## Genotype:Touch  1   2.30   2.300    12.0 0.0085 **
## Residuals       8   1.53   0.191                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = Value ~ Genotype * Touch, data = primers[[i]])
## 
## $Genotype
##             diff   lwr     upr  p adj
## Col-ccr1 -0.9878 -1.57 -0.4056 0.0045
## 
## $Touch
##                       diff   lwr     upr  p adj
## Touched-NotTouched -0.9478 -1.53 -0.3656 0.0056
## 
## $`Genotype:Touch`
##                                    diff    lwr     upr  p adj
## Col:NotTouched-ccr1:NotTouched -0.11222 -1.256  1.0311 0.9884
## ccr1:Touched-ccr1:NotTouched   -0.07222 -1.216  1.0711 0.9968
## Col:Touched-ccr1:NotTouched    -1.93556 -3.079 -0.7923 0.0028
## ccr1:Touched-Col:NotTouched     0.04000 -1.103  1.1833 0.9995
## Col:Touched-Col:NotTouched     -1.82333 -2.967 -0.6800 0.0041
## Col:Touched-ccr1:Touched       -1.86333 -3.007 -0.7200 0.0036
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-37.png) 

```
## [1] "WRKY"
##  Genotype        Touch        Rep           Primer       Value     
##  ccr1:6   NotTouched:6   Min.   :1   WRKY      :12   Min.   :20.5  
##  Col :6   Touched   :6   1st Qu.:1   Cyclo     : 0   1st Qu.:21.3  
##                          Median :2   Cyclo Prom: 0   Median :22.2  
##                          Mean   :2   IC2       : 0   Mean   :22.0  
##                          3rd Qu.:3   PP2A      : 0   3rd Qu.:22.7  
##                          Max.   :3   STPK      : 0   Max.   :23.2  
##                                      (Other)   : 0                 
##                Df Sum Sq Mean Sq F value  Pr(>F)    
## Genotype        1   1.23    1.23    7.07 0.02884 *  
## Touch           1   6.07    6.07   34.89 0.00036 ***
## Genotype:Touch  1   1.75    1.75   10.03 0.01326 *  
## Residuals       8   1.39    0.17                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = Value ~ Genotype * Touch, data = primers[[i]])
## 
## $Genotype
##             diff    lwr      upr  p adj
## Col-ccr1 -0.6406 -1.196 -0.08508 0.0288
## 
## $Touch
##                      diff    lwr     upr p adj
## Touched-NotTouched -1.423 -1.978 -0.8673 4e-04
## 
## $`Genotype:Touch`
##                                   diff     lwr     upr  p adj
## Col:NotTouched-ccr1:NotTouched  0.1222 -0.9687  1.2131 0.9831
## ccr1:Touched-ccr1:NotTouched   -0.6600 -1.7509  0.4309 0.2861
## Col:Touched-ccr1:NotTouched    -2.0633 -3.1543 -0.9724 0.0014
## ccr1:Touched-Col:NotTouched    -0.7822 -1.8731  0.3087 0.1780
## Col:Touched-Col:NotTouched     -2.1856 -3.2765 -1.0946 0.0009
## Col:Touched-ccr1:Touched       -1.4033 -2.4943 -0.3124 0.0142
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-38.png) 


