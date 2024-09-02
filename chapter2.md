# chapter2_潜在的結果変数の枠組み


## 前準備

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ✔ purrr     1.0.2     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
DATA02 <- "https://raw.githubusercontent.com/mtakahashi123/causality/main/data02.csv"
df <- read_csv(DATA02)
```

    Rows: 20 Columns: 7
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    dbl (7): x1, y3, t1, y0, y1, y0t, y1t

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
df
```

    # A tibble: 20 × 7
          x1    y3    t1    y0    y1   y0t   y1t
       <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
     1    74    76     1    NA    76    68    76
     2    82    75     0    75    NA    75    84
     3    72    75     1    NA    75    65    75
     4    96    84     0    84    NA    84    97
     5    83    75     0    75    NA    75    84
     6    72    74     1    NA    74    65    74
     7    85    76     0    76    NA    76    87
     8    87    77     0    77    NA    77    89
     9    86    77     0    77    NA    77    87
    10    77    80     1    NA    80    70    80
    11    95    87     0    87    NA    87    96
    12    84    75     0    75    NA    75    85
    13    74    77     1    NA    77    67    77
    14    58    61     1    NA    61    52    61
    15    91    81     0    81    NA    81    93
    16    80    72     0    72    NA    72    84
    17    80    72     0    72    NA    72    82
    18    89    80     0    80    NA    80    89
    19    88    80     0    80    NA    80    90
    20    86    78     0    78    NA    78    87

## 処置効果

個体因果効果（ICE: individual causal effect）

$\tau_i = Y_i(1) - Y_i(0)$

平均因果効果（ATE: average treatment effect）

$\tau_{ATE} = \mathbb{E}[Y_i(1) - Y_i(0)] = \mathbb{E}[Y_i(1)] - \mathbb{E}[Y_i(0)]$

処置群の平均処置効果（ATT: average treatment effect on the treated）

$\tau_{ATT} = \mathbb{E}[Y_i(1) - Y_i(0) | T_i = 1] = \mathbb{E}[Y_i(1) | T_i = 1] - \mathbb{E}[Y_i(0) | T_i = 1]$

``` r
df |>
  summarize(
    ATE = mean(y1t - y0t),
    naive_ATE = mean(y1, na.rm = TRUE) - mean(y0, na.rm = TRUE),
    ATT = mean(if_else(t1 == 1, y1t, NA), na.rm = TRUE) - mean(if_else(t1 == 1, y0t, NA), na.rm = TRUE)
  )
```

    # A tibble: 1 × 3
        ATE naive_ATE   ATT
      <dbl>     <dbl> <dbl>
    1  10.0     -3.95  9.33

処置が無作為に割り付けられている場合、ATE とATT
は同じになるが、処置の割り付けが無作為でない場合、個体処置効果 $\tau_i$
が母集団全体で異なっているならば、ATE と ATT
は異なる値となる可能性がある。

## 無作為割付けによる分析の例

``` r
set.seed(1)
r0 <- runif(20, min = 0, max = 1)
r1 <- round(r0, digits = 0)
y2 <- NULL
y2[r1 == 1] <- df$y1t[r1 == 1]
y2[r1 == 0] <- df$y0t[r1 == 0]

df2 <- tibble(
  r1,
  y2
)

df2 |>
  group_by(r1) |>
  summarise(
    mean_y2 = mean(y2),
  ) |>
  pivot_wider(
    names_from = r1,
    values_from = mean_y2,
    names_prefix = "mean_y2_r1_",
  ) |>
  mutate(
    difference = mean_y2_r1_1 - mean_y2_r1_0
  )
```

    # A tibble: 1 × 3
      mean_y2_r1_0 mean_y2_r1_1 difference
             <dbl>        <dbl>      <dbl>
    1         72.7         85.2       12.5

## 2 標本 $t$ 検定

``` r
t.test(
  df2$y2[r1 == 1],
  df2$y2[r1 == 0],
  var.equal = FALSE
)
```


        Welch Two Sample t-test

    data:  df2$y2[r1 == 1] and df2$y2[r1 == 0]
    t = 3.2178, df = 14.878, p-value = 0.005801
    alternative hypothesis: true difference in means is not equal to 0
    95 percent confidence interval:
      4.219264 20.811039
    sample estimates:
    mean of x mean of y 
     85.18182  72.66667 
