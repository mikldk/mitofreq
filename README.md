
# MitoFREQ: A Novel Approach for Mitogenome Frequency Estimation from Top-level Haplogroups and SNVs

``` r
library(mitofreq)
```

## TLHG distribution

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
head(d_gnomAD_TLHG_freq)
```

    ## # A tibble: 6 × 2
    ##   TLHG       N
    ##   <chr>  <dbl>
    ## 1 A     2678. 
    ## 2 C      867. 
    ## 3 D      603. 
    ## 4 E       34.0
    ## 5 F      282. 
    ## 6 G       91.0

## Exclusion

``` r
head(d_helix)
```

    ## # A tibble: 6 × 6
    ##   Position Ref   Alt   HGHom             HGHomN ExcludeReason         
    ##      <int> <chr> <chr> <list>             <int> <chr>                 
    ## 1        5 A     C     <tibble [1 × 2]>       1 Variant only seen once
    ## 2       10 T     C     <tibble [1 × 2]>       7 <NA>                  
    ## 3       11 C     T     <tibble [0 × 0]>       0 <NA>                  
    ## 4       12 T     C     <tibble [1 × 2]>       1 Variant only seen once
    ## 5       16 A     T     <tibble [10 × 2]>    273 <NA>                  
    ## 6       18 C     T     <tibble [1 × 2]>       3 <NA>

``` r
xtabs(~ ExcludeReason, d_helix, addNA = TRUE)
```

    ## ExcludeReason
    ## More than one reference  Variant only seen once                    <NA> 
    ##                     638                    3120                   10346

``` r
head(d_gnomAD)
```

    ## # A tibble: 6 × 8
    ##   Position Ref   Alt    HGHomN N_TLHG     n          TLHG       ExcludeReason
    ##      <int> <chr> <chr>   <int> <list>     <list>     <list>     <chr>        
    ## 1        3 T     C          19 <int [29]> <int [29]> <chr [29]> <NA>         
    ## 2        6 C     CCTCAA      0 <int [29]> <int [29]> <chr [29]> No variants  
    ## 3        7 A     G           0 <int [29]> <int [29]> <chr [29]> No variants  
    ## 4        8 G     T           5 <int [29]> <int [29]> <chr [29]> <NA>         
    ## 5        9 G     A          15 <int [29]> <int [29]> <chr [29]> <NA>         
    ## 6       10 T     C          11 <int [29]> <int [29]> <chr [29]> <NA>

``` r
xtabs(~ ExcludeReason, d_gnomAD, addNA = TRUE)
```

    ## ExcludeReason
    ## More than one reference             No variants                    <NA> 
    ##                    2350                    5761                   10053

## SNV frequency information

``` r
head(d_helix_refined_long)
```

    ## # A tibble: 6 × 7
    ##   Position Ref   TLHG      n Base  Type  N_TLHG
    ##      <int> <chr> <chr> <int> <chr> <chr>  <int>
    ## 1       10 T     H         7 C     Alt    71376
    ## 2       10 T     H     71369 T     Ref    71376
    ## 3       16 A     C         1 T     Alt     3083
    ## 4       16 A     C      3082 A     Ref     3083
    ## 5       16 A     H         7 T     Alt    71376
    ## 6       16 A     H     71369 A     Ref    71376

``` r
head(d_gnomAD_refined_long)
```

    ## # A tibble: 6 × 7
    ##   Position Ref   TLHG      n Base  Type  N_TLHG
    ##      <int> <chr> <chr> <int> <chr> <chr>  <int>
    ## 1        3 T     A      2680 T     Ref     2680
    ## 2        3 T     C       868 T     Ref      868
    ## 3        3 T     D       603 T     Ref      603
    ## 4        3 T     E        34 T     Ref       34
    ## 5        3 T     F       282 T     Ref      282
    ## 6        3 T     G        91 T     Ref       91
