
# MitoFREQ: A Novel Approach for Mitogenome Frequency Estimation from Top-level Haplogroups and SNVs

``` r
library(mitofreq)
```

``` r
head(d_helix_TLHG_freq)
```

    ## # A tibble: 6 × 2
    ##   TLHG      N
    ##   <chr> <int>
    ## 1 A      6232
    ## 2 C      3083
    ## 3 D      2275
    ## 4 E       334
    ## 5 F       929
    ## 6 G       299

``` r
head(d_helix_positioninfo)
```

    ## # A tibble: 6 × 2
    ##   Position ExcludeReason          
    ##      <int> <chr>                  
    ## 1        5 Not seen twice (2)     
    ## 2       10 <NA>                   
    ## 3       11 No homoplasmic variants
    ## 4       12 Not seen twice (2)     
    ## 5       16 <NA>                   
    ## 6       18 <NA>

``` r
xtabs(~ ExcludeReason, d_helix_positioninfo)
```

    ## ExcludeReason
    ##                                                                    INDEL 
    ##                                                                       18 
    ##                                                          INDEL neighbour 
    ##                                                                      124 
    ##                                 INDEL neighbour, No homoplasmic variants 
    ##                                                                       49 
    ##                                      INDEL neighbour, Not seen twice (2) 
    ##                                                                       18 
    ##                                           INDEL, No homoplasmic variants 
    ##                                                                       33 
    ##                                                INDEL, Not seen twice (2) 
    ##                                                                       12 
    ##                                                  No homoplasmic variants 
    ##                                                                     1518 
    ##                                                               Non-binary 
    ##                                                                     4520 
    ##                                              Non-binary, INDEL neighbour 
    ##                                                                      106 
    ##                     Non-binary, INDEL neighbour, No homoplasmic variants 
    ##                                                                      225 
    ## Non-binary, INDEL neighbour, No homoplasmic variants, Not seen twice (2) 
    ##                                                                        6 
    ##                                      Non-binary, No homoplasmic variants 
    ##                                                                     2026 
    ##                  Non-binary, No homoplasmic variants, Not seen twice (2) 
    ##                                                                      157 
    ##                                                       Not seen twice (2) 
    ##                                                                      647

``` r
head(d_helix_SNV_freq_long)
```

    ## # A tibble: 6 × 6
    ##   Position Ref   Alt   TLHG  n_Ref n_Alt
    ##      <int> <chr> <chr> <chr> <int> <int>
    ## 1       10 T     C     H     71369     7
    ## 2       16 A     T     C      3082     1
    ## 3       16 A     T     H     71369     7
    ## 4       16 A     T     K     14575   246
    ## 5       16 A     T     L4-6     87     1
    ## 6       16 A     T     M      3312     1
