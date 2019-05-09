mocapr
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

`mocapr` uses a series of tidyverse packages to import
([`tidyr`](https://github.com/tidyverse/tidyr),
[`dplyr`](https://github.com/tidyverse/dplyr),
[`stringr`](https://github.com/tidyverse/stringr),
[`forcats`](https://github.com/tidyverse/forcats)), plot
([`ggplot2`](https://github.com/tidyverse/ggplot2)), animate
([`gganimate`](https://github.com/thomasp85/gganimate)), and analyse
motion capture data.

The current state of the package is **work in progress**.

\#\#Why this package? Motion capture data has become readily availble,
and more will come It is becoming easy to collect this type of data Most
systems are using their own export formats (especially true for
markerless motion capture), and

\#\#The Vision

\#\#Can you help? Yes. Clinical background, ended up with the
programming part by nessescity. No programming skills when I started the
Ph.D., and I consider myself a beginner/intermediate R-user. While I
think the code is correct, I am quite sure that it is not optimal.
Input….

Data + video(if possible) Function to import data into the
`mocapr`format

## Installation

`mocapr` can be installed directly from github using devtools:

``` r
# install.packages('devtools')
devtools::install_github('steenharsted/mocapr')
```

\#\#The Data All data currently obtained from the CapturyLive system
Kinetisense to come Vicon plug-in-gait likely to come

``` r
library(mocapr)

#Data
Jump <- mocapr::jump_1
head(Jump)
```

    ##   mocap_system Frame Time_Seconds Marks       CGX      CGY      CGZ
    ## 1      Captury     1         0.02  <NA> -1451.172 1093.062 20.57521
    ## 2      Captury     2         0.04  <NA> -1449.542 1092.984 21.05189
    ## 3      Captury     3         0.06  <NA> -1446.707 1092.955 21.53393
    ## 4      Captury     4         0.08  <NA> -1443.601 1093.066 22.08807
    ## 5      Captury     5         0.10  <NA> -1441.658 1093.448 22.37633
    ## 6      Captury     6         0.12  <NA> -1440.261 1093.129 22.68840
    ##         LWX      LWY       LWZ       LEX      LEY       LEZ       LSX
    ## 1 -1320.917 2093.561 -143.2579 -1400.252 1852.079 -205.0099 -1507.033
    ## 2 -1336.232 2101.508 -141.1362 -1408.546 1857.761 -202.6119 -1504.406
    ## 3 -1353.364 2109.466 -136.9046 -1416.933 1863.679 -199.9054 -1500.317
    ## 4 -1371.364 2117.471 -131.2936 -1425.210 1869.811 -195.9928 -1494.579
    ## 5 -1384.305 2122.384 -128.5984 -1431.495 1873.394 -193.3898 -1490.472
    ## 6 -1396.443 2124.675 -127.3916 -1436.671 1874.357 -191.7618 -1486.832
    ##        LSY       LSZ       RWX      RWY      RWZ       REX      REY
    ## 1 1568.448 -130.0948 -1284.540 2084.786 210.0962 -1401.697 1853.605
    ## 2 1569.547 -130.4716 -1304.433 2093.068 209.2426 -1413.185 1857.974
    ## 3 1570.914 -130.6519 -1325.287 2101.967 207.5316 -1423.107 1862.348
    ## 4 1572.446 -131.0100 -1344.200 2108.544 206.2050 -1433.707 1865.804
    ## 5 1573.183 -131.2953 -1362.941 2113.934 204.2452 -1443.670 1868.302
    ## 6 1572.166 -131.5522 -1376.251 2116.101 203.6260 -1450.673 1868.651
    ##        REZ       RSX      RSY      RSZ       LTX      LTY       LTZ
    ## 1 245.4625 -1519.307 1576.690 162.1188 -1453.629 92.91063 -175.0269
    ## 2 245.6494 -1517.688 1576.011 161.7678 -1453.498 92.87769 -175.3028
    ## 3 245.4128 -1513.913 1575.667 161.5754 -1452.264 92.73561 -175.9558
    ## 4 244.7614 -1511.446 1575.325 160.8607 -1452.397 92.80754 -175.9800
    ## 5 243.8500 -1509.498 1574.787 160.3246 -1452.727 92.79442 -175.7252
    ## 6 244.2570 -1507.853 1573.564 159.9081 -1452.484 92.65646 -176.3908
    ##         LAX      LAY       LAZ     LADF      X53      X54       LKX
    ## 1 -1581.491 149.1225 -176.5166 6.394482 1.803385 10.08701 -1484.164
    ## 2 -1581.290 149.2440 -176.9659 6.636145 1.661780 10.26167 -1482.021
    ## 3 -1580.024 149.1785 -177.4335 6.669647 1.701915 10.42342 -1480.311
    ## 4 -1579.962 149.6932 -177.3698 6.807603 1.510403 10.27798 -1477.891
    ## 5 -1579.828 150.7160 -176.8329 6.792926 1.443368 10.12471 -1474.535
    ## 6 -1579.323 151.1547 -177.0485 6.982129 1.373172 10.17907 -1470.844
    ##        LKY       LKZ      LKF  LKVarus      LKRot       LHX      LHY
    ## 1 536.3976 -108.6289 6.362895 3.809988  -9.809719 -1413.936 971.1885
    ## 2 535.7590 -107.5762 6.671565 4.100821  -9.949487 -1410.958 970.4474
    ## 3 535.3362 -106.7037 6.676920 4.330059 -10.097342 -1407.013 969.6971
    ## 4 535.2006 -106.4562 6.699937 4.313101  -9.934794 -1402.418 969.1443
    ## 5 535.4001 -106.1595 7.014700 4.344284  -9.797573 -1398.950 969.3169
    ## 6 534.9097 -106.1440 7.504097 4.440947  -9.891158 -1396.525 969.0080
    ##         LHZ       LHF       LHA    LHRot       RTX      RTY      RTZ
    ## 1 -65.64785 -16.38885 -2.109021 14.26249 -1454.935 91.57564 217.8142
    ## 2 -64.93083 -16.58659 -2.093663 14.35643 -1454.543 91.28037 218.6626
    ## 3 -64.50708 -16.80845 -2.009136 14.47366 -1454.193 91.20999 219.5112
    ## 4 -63.80358 -16.93389 -1.973338 14.66800 -1453.819 91.19615 220.3760
    ## 5 -63.43177 -17.12397 -1.919258 14.69692 -1453.236 91.08175 220.3906
    ## 6 -63.04065 -17.14420 -1.955840 14.87052 -1453.238 90.95175 220.5658
    ##         RAX      RAY      RAZ     RADF      X77      X78       RKX
    ## 1 -1582.688 146.3043 203.8659 6.845938 4.163546 10.06912 -1479.708
    ## 2 -1582.129 146.2329 204.0715 7.032393 4.527633 10.13035 -1476.971
    ## 3 -1581.678 146.1726 204.1057 7.344432 4.549152 10.12632 -1474.124
    ## 4 -1581.181 145.9628 203.3354 7.520119 4.481398 10.31865 -1472.338
    ## 5 -1580.280 146.3835 202.7179 7.471104 4.427817 10.21369 -1469.903
    ## 6 -1580.147 146.4650 202.5947 7.634772 4.439952 10.30030 -1467.951
    ##        RKY      RKZ      RKF   RKVarus    RKRot       RHX      RHY
    ## 1 534.4233 150.7443 9.309985 -3.928898 9.729461 -1434.256 973.0568
    ## 2 533.7891 151.1066 9.567210 -4.047089 9.795340 -1431.063 972.4067
    ## 3 533.1957 152.0625 9.663925 -3.937803 9.844075 -1426.751 971.6165
    ## 4 532.8079 152.6680 9.538114 -3.853013 9.946123 -1422.945 971.0065
    ## 5 532.9055 152.9095 9.617330 -3.812587 9.855378 -1419.632 971.0138
    ## 6 532.5219 153.2473 9.774779 -3.832791 9.947783 -1416.974 970.5555
    ##        RHZ       RHF       RHA     RHRot       HAX      HAY      HAZ
    ## 1 113.9037 -12.63798 -1.823282 -7.307815 -1424.096 972.1226 24.12794
    ## 2 114.6440 -12.79827 -1.743872 -7.596035 -1421.011 971.4270 24.85661
    ## 3 115.1089 -12.88040 -1.828830 -8.026571 -1416.882 970.6568 25.30090
    ## 4 115.7246 -12.87509 -1.898747 -8.684763 -1412.681 970.0754 25.96050
    ## 5 116.0802 -13.12429 -1.954595 -8.843609 -1409.291 970.1653 26.32424
    ## 6 116.4994 -13.40343 -1.993802 -9.136455 -1406.750 969.7818 26.72937

\#\#Example 1

``` r
#Project to Anatomical Planes 
Jump <- cbind(Jump,
              #Upper extremity
              Project_to_AP(Jump, Y=LSY, X=LSX, Z=LSZ, New_Name = "LS"),
              Project_to_AP(Jump, Y=LEY, X=LEX, Z=LEZ, New_Name = "LE"),
              Project_to_AP(Jump, Y=LWY, X=LWX, Z=LWZ, New_Name = "LW"),
              Project_to_AP(Jump, Y=RSY, X=RSX, Z=RSZ, New_Name = "RS"),
              Project_to_AP(Jump, Y=REY, X=REX, Z=REZ, New_Name = "RE"),
              Project_to_AP(Jump, Y=RWY, X=RWX, Z=RWZ, New_Name = "RW"),
              #Lower extremity
              Project_to_AP(Jump,Y=LKY, X=LKX, Z=LKZ, New_Name = "LK"),
              Project_to_AP(Jump,Y=LHY, X=LHX, Z=LHZ, New_Name = "LH"),
              Project_to_AP(Jump,Y=LAY, X=LAX, Z=LAZ, New_Name = "LA"),
              Project_to_AP(Jump,Y=LTY, X=LTX, Z=LTZ, New_Name = "LT"),
              Project_to_AP(Jump,Y=RKY, X=RKX, Z=RKZ, New_Name = "RK"),
              Project_to_AP(Jump,Y=RHY, X=RHX, Z=RHZ, New_Name = "RH"),
              Project_to_AP(Jump,Y=RAY, X=RAX, Z=RAZ, New_Name = "RA"),
              Project_to_AP(Jump,Y=RTY, X=RTX, Z=RTZ, New_Name = "RT"))

#Animate the anatomical projections
Jump %>% animate_anatomical(nframes = nrow(.)*2, fps = 50, rewind = FALSE)
```

    ## Registered S3 methods overwritten by 'ggplot2':
    ##   method         from 
    ##   [.quosures     rlang
    ##   c.quosures     rlang
    ##   print.quosures rlang

![](README_files/figure-gfm/unnamed-chunk-2-1.gif)<!-- -->

\#\#Example 2

``` r
#Project to movements planes
Jump <- cbind(Jump,
              #Upper extremity
              Project_to_MP(Jump, Y=LSY, X=LSX, Z=LSZ, New_Name = "LS"),
              Project_to_MP(Jump, Y=LEY, X=LEX, Z=LEZ, New_Name = "LE"),
              Project_to_MP(Jump, Y=LWY, X=LWX, Z=LWZ, New_Name = "LW"),
              Project_to_MP(Jump, Y=RSY, X=RSX, Z=RSZ, New_Name = "RS"),
              Project_to_MP(Jump, Y=REY, X=REX, Z=REZ, New_Name = "RE"),
              Project_to_MP(Jump, Y=RWY, X=RWX, Z=RWZ, New_Name = "RW"),
              #Lower extremity
              Project_to_MP(Jump,Y=LKY, X=LKX, Z=LKZ, New_Name = "LK"),
              Project_to_MP(Jump,Y=LHY, X=LHX, Z=LHZ, New_Name = "LH"),
              Project_to_MP(Jump,Y=LAY, X=LAX, Z=LAZ, New_Name = "LA"),
              Project_to_MP(Jump,Y=LTY, X=LTX, Z=LTZ, New_Name = "LT"),
              Project_to_MP(Jump,Y=RKY, X=RKX, Z=RKZ, New_Name = "RK"),
              Project_to_MP(Jump,Y=RHY, X=RHX, Z=RHZ, New_Name = "RH"),
              Project_to_MP(Jump,Y=RAY, X=RAX, Z=RAZ, New_Name = "RA"),
              Project_to_MP(Jump,Y=RTY, X=RTX, Z=RTZ, New_Name = "RT"))

#Animate the movement plane projections
Jump %>% animate_movement(nframes = nrow(.)*2, fps = 50, rewind = FALSE)
```

![](README_files/figure-gfm/unnamed-chunk-3-1.gif)<!-- -->
