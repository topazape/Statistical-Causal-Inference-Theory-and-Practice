# chapter14_操作変数法による非遵守への対応


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
DATA14 <- "https://raw.githubusercontent.com/mtakahashi123/causality/main/data14.csv"
data14 <- read_csv(DATA14)
```

    Rows: 20 Columns: 7
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    dbl (7): y3, t1, d1, y0t, y1t, d0t, d1t

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## 遵守者と非遵守者の４つの種類

- $T_i$：個体 $i$ が処置に割り付けられたかどうかを表す二値変数

- $D_i$：個体 $i$ が実際に処置を受けたかどうかを表す二値変数

遵守者と非遵守者の４つの異なるタイプ

- 遵守者

  - $D_i(T_i = 1) - D_i(T_i = 0) = 1$

- 常に処置を受けない人

  - $D_i(T_i = 1) - D_i(T_i = 0) = 0$

<!-- -->

- 常に処置を受ける人

  - $D_i(T_i = 1) - D_i(T_i = 0) = 0$

- 天邪鬼

  - $D_i(T_i = 1) - D_i(T_i = 0) = -1$

## 単調性の仮定と推定対象

操作変数法を利用することで、実験研究における非遵守に対処できる。そのためには、以下の仮定を置く必要がある。

1.  操作変数の外生性
2.  操作変数の関連性
3.  除外制約
4.  単調性（monotonicity）： $D_i(T_i = 1) \ge D_i(T_i = 0)$

- 遵守者

  - $D_i(T_i = 1) = D_i(T_i = 0) + 1$ より、仮定４を満たす

- 常に処置を受けない人

  - $D_i(T_i = 1) = D_i(T_i = 0)$ より、仮定４を満たす

- 常に処置を受ける人

  - $D_i(T_i = 1) = D_i(T_i = 0)$ より、仮定４を満たす

- 天邪鬼

  - $D_i(T_i = 1) = D_i(T_i = 0) - 1$ より、仮定４を満たしていない

仮定４（単調性）は、天邪鬼がいないという仮定である。残りの３種類の中で、常に処置を受ける人と常に処置を受けない人は、処置を受けるか受けないかを変えないので除外制約により処置効果はゼロ。したがって、残った遵守者について、平均処置効果（ATE）を推定する。

遵守者（complier）を $C$
で表す。遵守者に限定した平均因果効果を遵守者の平均因果効果（CACE:
complier average causal effect）または局所的な平均処置効果（LATE: local
average treatment effect）という。

$$
CACE = \mathbb{E}[Y_i(1) - Y_i(0) | C]
$$

真の CACE の計算

``` r
m1 <- data14 |> filter(d1t == 1, d0t == 0) |> pull(y1t) |> mean()
m0 <- data14 |> filter(d1t == 1, d0t == 0) |> pull(y0t) |> mean()

m1 - m0
```

    [1] 7.875

## 無作為化奨励デザインと４つの推定量

無作為化奨励デザイン下では以下の４種類の推定量が考えられる。

- AT（As Treated）：実際に受けた処置に基づく推定量

- PP（Per Protocol）：処置の割付けを遵守した被験者のみに基づく推定量

- ITT（Intention To Treat）：処置意図に基づく推定量

- IV（Instrumental Variable）：操作変数推定量

#### AT 推定量

実際に受けた処置に基づく推定量。交絡の影響は、過大にも過小にもなるため、交絡の調整を行わないならば
AT は推奨されない。

$$
AT = \mathbb{E}[Y_i | D_i = 1] - \mathbb{E}[Y_i | D_i = 0]
$$

AT の計算

``` r
m1at <- data14 |> filter(d1 == 1) |> pull(y3) |> mean()
m0at <- data14 |> filter(d1 == 0) |> pull(y3) |> mean()

m1at - m0at
```

    [1] 2.916667

#### PP 推定量

処置の割付けを遵守した被験者のみに基づく推定量。交絡の影響は、過大にも過小にもなるため、交絡の調整を行わないならば
PP は推奨されない。

$$
PP = \mathrm{E}[Y_i | T_i = D_i = 1] - \mathbb{E}[Y_i | T_i = D_i = 0]
$$

PP の計算

``` r
m1pp <- data14 |> filter(t1 == 1, d1 == 1) |> pull(y3) |> mean()
m0pp <- data14 |> filter(t1 == 0, d1 == 0) |> pull(y3) |> mean()

m1pp - m0pp
```

    [1] 4.625

#### ITT 推定量

処置意図に基づく推定量。実際に受けた処置 $D_i$
にかかわらず、処置の割付け $T_i$
に基づく推定量。偏りはあり得るが、効果がある場合には、効果の歌唱推定になることが知られている。したがって、無意味な推定量ではなく、CACE
の真値を過小推定しているという条件付きで推奨される。

$$
ITT = \mathrm{E}[Y_i | T_i = 1] - \mathrm{E}[Y_i | T_i = 0]
$$

ITT の計算

``` r
m1itt <- data14 |> filter(t1 == 1) |> pull(y3) |> mean()
m0itt <- data14 |> filter(t1 == 0) |> pull(y3) |> mean()

m1itt - m0itt
```

    [1] 3.6

#### IV 推定量

操作変数推定量。

$$
IV = \frac{\mathrm{cov}[T_i, Y_i]}{\mathrm{cov}[T_i, D_i]}
$$

$T_i$ と $D_i$ は二値のため、

$$
IV
= \frac{\mathbb{E}[Y_i|T_i=1] - \mathbb{E}[Y_i|T_i=0]}{\mathbb{E}[D_i|T_i=1] - \mathbb{E}[D_i|Ti=0]}
= \frac{\mathbb{E}[Y_i|T_i=1] - \mathbb{E}[Y_i|T_i=0]}{\mathrm{Pr}[D_i=1|T_i=1] - \mathrm{Pr}[D_i=1|T_i=0]}
$$

## R による無作為化奨励デザインの分析

R パッケージ `AER` による二段階最小二乗法

``` r
library(AER)
```

    Loading required package: car

    Loading required package: carData


    Attaching package: 'car'

    The following object is masked from 'package:dplyr':

        recode

    The following object is masked from 'package:purrr':

        some

    Loading required package: lmtest

    Loading required package: zoo


    Attaching package: 'zoo'

    The following objects are masked from 'package:base':

        as.Date, as.Date.numeric

    Loading required package: sandwich

    Loading required package: survival

``` r
modelIV <- data14 |> AER::ivreg(formula = y3 ~ d1 | t1, data = _)
summary(modelIV)
```


    Call:
    AER::ivreg(formula = y3 ~ d1 | t1, data = data14)

    Residuals:
       Min     1Q Median     3Q    Max 
    -11.10  -3.10  -1.10   4.15  10.90 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)   70.100      4.176  16.786 1.93e-12 ***
    d1             9.000      6.603   1.363     0.19    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 5.906 on 18 degrees of freedom
    Multiple R-Squared: -0.2786,    Adjusted R-squared: -0.3496 
    Wald test: 1.858 on 1 and 18 DF,  p-value: 0.1897 

``` r
confint(modelIV)
```

                    2.5 %   97.5 %
    (Intercept) 61.915211 78.28479
    d1          -3.941288 21.94129

操作変数推定量は一致推定量であるから、大標本であれば、偏りも小さく、有意な結果が期待できる。

手作業による二段階最小二乗法

``` r
model1 <- data14 |> lm(d1 ~ t1, data = _)
model2 <- data14 |> lm(y3 ~ t1, data = _)

summary(model1)
```


    Call:
    lm(formula = d1 ~ t1, data = data14)

    Residuals:
       Min     1Q Median     3Q    Max 
      -0.8   -0.4    0.2    0.2    0.6 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)  
    (Intercept)   0.4000     0.1491   2.683   0.0152 *
    t1            0.4000     0.2108   1.897   0.0739 .
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.4714 on 18 degrees of freedom
    Multiple R-squared:  0.1667,    Adjusted R-squared:  0.1204 
    F-statistic:   3.6 on 1 and 18 DF,  p-value: 0.07394

``` r
summary(model2)
```


    Call:
    lm(formula = y3 ~ t1, data = data14)

    Residuals:
       Min     1Q Median     3Q    Max 
     -9.30  -3.85   0.00   1.85  10.70 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)   73.700      1.539  47.896   <2e-16 ***
    t1             3.600      2.176   1.654    0.115    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 4.866 on 18 degrees of freedom
    Multiple R-squared:  0.132, Adjusted R-squared:  0.08375 
    F-statistic: 2.737 on 1 and 18 DF,  p-value: 0.1154

``` r
data14 |>
  group_by(t1, d1) |>
  summarize(n = n())
```

    `summarise()` has grouped output by 't1'. You can override using the `.groups`
    argument.

    # A tibble: 4 × 3
    # Groups:   t1 [2]
         t1    d1     n
      <dbl> <dbl> <int>
    1     0     0     6
    2     0     1     4
    3     1     0     2
    4     1     1     8
