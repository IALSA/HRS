# Basic EDA of HRS data"



<!--  Set the working directory to the repository's base directory; this assumes the report is nested inside of two directories.-->


<!-- Set the report-wide options, and point to the external code file. -->


<!-- Load the sources.  Suppress the output when loading sources. --> 


<!-- Load 'sourced' R files.  Suppress the output when loading packages. --> 


<!-- Load any Global functions and variables declared in the R file.  Suppress the output. --> 


<!-- Declare any global functions specific to a Rmd output.  Suppress the output. --> 


The names of the variables in the original dataset is as follows:
<!-- Load the datasets.   -->

```
 [1] "wave"     "mstat"    "ragender" "rahispan" "raracem"  "raedyrs"  "raedegrm" "ravetrn"  "agem_m"   "alzhe_10"
[11] "demntia"  "retliv_1" "mstot"    "cogtot"   "edu"      "cesd"     "bmi"      "height"   "smokev"   "smoken"  
[21] "dob"      "alz"      "dem"      "retmon"   "retyr"    "retsat"   "retemp"   "retmer"   "lbrf"     "inlbrf"  
[31] "demen"    "pid"      "yoi"      "agey"     "bagey"    "blbrf"    "eduy"     "bage"     "ageg"     "zcog"    
[41] "cagey"    "wavey"    "eduh"     "male"     "retit"    "year"     "tret"     "retst"    "flag"     "retem"   
[51] "cdob"     "raeth"    "retire"   "wret"    
```

<!-- Tweak the datasets.   -->


The dimensions (rows-columns) are. Definitions are given [here](./data/derived/datafromsean.md/), [edit]()
<!-- Basic View   -->

```
[1] 54
```

The data view for the first few observations: 
<!-- Basic View   -->

```
   wave      mstat ragender        rahispan           raracem    raedyrs     raedegrm ravetrn agem_m alzhe_10
1     1 7. WIDOWED 2.FEMALE 0. NOT HISPANIC 1.WHITE/CAUCASIAN       <NA> 0. NO DEGREE    0.NO    785     <NA>
2     2 1. MARRIED   1.MALE 0. NOT HISPANIC 1.WHITE/CAUCASIAN       <NA>        2. HS    0.NO    795   1. YES
3     3 1. MARRIED   1.MALE 0. NOT HISPANIC 1.WHITE/CAUCASIAN       <NA>        2. HS    0.NO    822   1. YES
4     4 1. MARRIED   1.MALE 0. NOT HISPANIC 1.WHITE/CAUCASIAN       <NA>        2. HS    0.NO    842   1. YES
5     5 1. MARRIED   1.MALE 0. NOT HISPANIC 1.WHITE/CAUCASIAN       <NA>        2. HS    0.NO    865   1. YES
6     6 1. MARRIED   1.MALE 0. NOT HISPANIC 1.WHITE/CAUCASIAN       <NA>        2. HS    0.NO    896   1. YES
7     7       <NA>   1.MALE 0. NOT HISPANIC 1.WHITE/CAUCASIAN       <NA>         <NA>    <NA>    918   1. YES
8     3 1. MARRIED 2.FEMALE 0. NOT HISPANIC 1.WHITE/CAUCASIAN 17.17+ YRS        5. BA    0.NO    790    0. NO
9     4 1. MARRIED 2.FEMALE 0. NOT HISPANIC 1.WHITE/CAUCASIAN 17.17+ YRS        5. BA    0.NO    810    0. NO
10    5 1. MARRIED 2.FEMALE 0. NOT HISPANIC 1.WHITE/CAUCASIAN 17.17+ YRS        5. BA    0.NO    833    0. NO
```

```
   demntia            retliv_1 mstot cogtot edu cesd  bmi height smokev smoken
1     <NA>                <NA>     6     14   0    1 17.4 1.6002 1. YES  1.YES
2     <NA> 3. STAY SAME AS NOW    13     31   2    1 28.3 1.6510  0. NO   0.NO
3     <NA> 3. STAY SAME AS NOW    11     18   2    0 26.6 1.6510  0. NO   0.NO
4     <NA> 3. STAY SAME AS NOW    14     20   2    0 27.1 1.6256  0. NO   0.NO
5     <NA> 3. STAY SAME AS NOW    14     26   2    0 24.7 1.6256  0. NO   0.NO
6     <NA> 3. STAY SAME AS NOW    10     17   2    0 24.0 1.6256  0. NO   0.NO
7     <NA>                <NA>    NA     12   2   NA   NA 1.6510   <NA>   <NA>
8    0. NO 3. STAY SAME AS NOW    15     27   3    0 25.0 1.6510  0. NO   0.NO
9    0. NO 3. STAY SAME AS NOW    15     26   3    0 24.7 1.6256  0. NO   0.NO
10   0. NO 3. STAY SAME AS NOW    14     24   3    0 25.7 1.6256  0. NO   0.NO
```

```
        dob    alz  dem retmon retyr        retsat                      retemp      retmer         lbrf inlbrf
1  1934.792   <NA> <NA>     NA  1978       1. VERY         0.NO RETIRE EMPSTAT MATCHED (3)    5.RETIRED   0:NO
2  1936.042   <NA> <NA>      7  1995          <NA>         0.NO RETIRE EMPSTAT MATCHED (3) 4.PARTLY RET  1:YES
3  1936.042   <NA> <NA>      7  1995          <NA> 2.RETIRE PLUS OTHER EMPSTAT MATCHED (3) 4.PARTLY RET  1:YES
4  1936.042   <NA> <NA>      7  1995 2. MODERATELY       1.ONLY RETIRE EMPSTAT MATCHED (3)    5.RETIRED   0:NO
5  1936.042   <NA> <NA>      7  1995       1. VERY       1.ONLY RETIRE EMPSTAT MATCHED (3)    5.RETIRED   0:NO
6  1936.042 1. YES <NA>      7  1995       1. VERY       1.ONLY RETIRE EMPSTAT MATCHED (3)    5.RETIRED   0:NO
7  1936.042 1. YES <NA>      7  1995       1. VERY       1.ONLY RETIRE EMPSTAT MATCHED (3)    5.RETIRED   0:NO
8  1938.708   <NA> <NA>      6  2002       1. VERY         0.NO RETIRE EMPSTAT MATCHED (3)    5.RETIRED   0:NO
9  1938.708   <NA> <NA>      6  2002       1. VERY       1.ONLY RETIRE EMPSTAT MATCHED (3)    5.RETIRED   0:NO
10 1938.708   <NA> <NA>      6  2002       1. VERY       1.ONLY RETIRE EMPSTAT MATCHED (3)    5.RETIRED   0:NO
```

```
   demen pid      yoi     agey     bagey blbrf eduy bage ageg       zcog
1     NA   2       NA 65.41666 0.4166641     5    9  785   13 -1.5395491
2     NA   3       NA 66.25000 1.2500000     4   13  795   13  1.7827483
3     NA   3       NA 68.50000 1.2500000     4   13  795   13 -0.7578321
4     NA   3       NA 70.16666 1.2500000     4   13  795   13 -0.3669735
5     NA   3       NA 72.08334 1.2500000     4   13  795   13  0.8056021
6     NA   3       NA 74.66666 1.2500000     4   13  795   13 -0.9532613
7     NA   3 2012.542 76.50000 1.2500000     4   NA  795   13 -1.9304076
8     NA   4       NA 65.83334 0.8333359     5   17  790   13  1.0010313
9     NA   4       NA 67.50000 0.8333359     5   17  790   13  0.8056021
10    NA   4       NA 69.41666 0.8333359     5   17  790   13  0.4147435
```

```
       cagey     wavey eduh male    retit     year      tret retst flag retem     cdob raeth retire wret
1  0.4166641  0.000000    0    0       NA 2000.208  0.000000     2    1     0 4.791626     1      0    0
2  1.2500000  0.000000    1    1 1995.542 2002.292  6.750000    NA    1     2 6.041626     1      1    1
3  1.2500000  2.250000    1    1 1995.542 2004.542  9.000000    NA    1     2 6.041626     1      1    1
4  1.2500000  3.916664    1    1 1995.542 2006.208 10.666626     1    1     2 6.041626     1      1    1
5  1.2500000  5.833336    1    1 1995.542 2008.125 12.583374     2    1     2 6.041626     1      1    1
6  1.2500000  8.416664    1    1 1995.542 2010.708 15.166626     2    1     2 6.041626     1      1    1
7  1.2500000 10.250000    1    1 1995.542 2012.542 17.000000     2    1     2 6.041626     1      1    1
8  0.8333359  0.000000    2    0 2002.458 2004.542  2.083374     2    1     1 8.708374     1      1    1
9  0.8333359  1.666664    2    0 2002.458 2006.208  3.750000     2    1     1 8.708374     1      1    1
10 0.8333359  3.583328    2    0 2002.458 2008.125  5.666626     2    1     1 8.708374     1      1    1
```






# Session Information
For the sake of documentation and reproducibility, the current report was rendered on a system using the following software.


```
Report rendered by koval_000 at 2015-08-31, 09:37 -0700
```

```
R version 3.2.0 (2015-04-16)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 8 x64 (build 9200)

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] dplyr_0.4.1        testit_0.4         ggplot2_1.0.1      RColorBrewer_1.1-2 scales_0.2.5       knitr_1.10.5      
[7] memisc_0.97        MASS_7.3-40        lattice_0.20-31   

loaded via a namespace (and not attached):
 [1] Rcpp_0.11.6         formatR_1.2         nloptr_1.0.4        plyr_1.8.2          tools_3.2.0        
 [6] rpart_4.1-9         digest_0.6.8        lme4_1.1-7          evaluate_0.7        gtable_0.1.2       
[11] nlme_3.1-120        mgcv_1.8-6          Matrix_1.2-0        DBI_0.3.1           yaml_2.1.13        
[16] parallel_3.2.0      SparseM_1.6         proto_0.3-10        gridExtra_0.9.1     stringr_1.0.0      
[21] cluster_2.0.1       nnet_7.3-9          R6_2.0.1            survival_2.38-2     rmarkdown_0.7      
[26] foreign_0.8-63      latticeExtra_0.6-26 minqa_1.2.4         Formula_1.2-1       reshape2_1.4.1     
[31] car_2.0-25          magrittr_1.5        htmltools_0.2.6     Hmisc_3.16-0        splines_3.2.0      
[36] rsconnect_0.3.79    assertthat_0.1      pbkrtest_0.4-2      colorspace_1.2-6    quantreg_5.11      
[41] stringi_0.4-1       acepack_1.3-3.3     munsell_0.4.2      
```
