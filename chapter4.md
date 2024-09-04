# chapter4_推測統計の基礎


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
DATA04 <- "https://raw.githubusercontent.com/mtakahashi123/causality/main/data04.csv"
```

## 標準誤差

標本平均 $\bar{X}$ の標準誤差 $s.e.(\bar{X})$ は以下である。ここで、 $n$
は標本サイズ、 $\sigma$ は母標準偏差を表す。

$$
s.e.(\bar{X}) = \frac{\sigma}{\sqrt{n}}
$$

### 母集団データ

``` r
x1 <- tribble(
  ~name, ~height,
  "A", 165,
  "B", 166,
  "C", 171,
  "D", 180,
)

x1
```

    # A tibble: 4 × 2
      name  height
      <chr>  <dbl>
    1 A        165
    2 B        166
    3 C        171
    4 D        180

$N$ を母集団サイズとした時、母平均 $\mu$ 、母標準偏差 $\sigma$
は以下である。

$$
\mu = \frac{1}{N} \sum_{i=1}^N{X_i} \\
\sigma = \sqrt{\frac{1}{N} \sum_{i=1}^{N}({X_i - \mu})^{2}}
$$

``` r
N1 <- 4 # 母集団サイズ
n1 <- 2 # 標本サイズ
mu <- mean(x1$height)
hensa <- x1$height - mu
hensa2 <- hensa^2
sigma2 <- sum(hensa2) / N1
sigma <- sqrt(sigma2)

print(mu)
```

    [1] 170.5

``` r
print(sigma)
```

    [1] 5.937171

### 標本抽出と標本平均

``` r
xs <- combn(x1$height, n1) # 標本サイズは 2
xbars <- apply(xs, 2, mean) # 標本平均; 母平均に一致する

print(xbars)
```

    [1] 165.5 168.0 172.5 168.5 173.0 175.5

``` r
print(mean(xbars))
```

    [1] 170.5

``` r
hensab <- xbars - mu
hensa2b <- hensab^2
sigma2b <- sum(hensa2b) / ncol(xs)
sigmab <- sqrt(sigma2b) # 標本平均の標準偏差

print(sigmab)
```

    [1] 3.427827

### 標本平均のばらつき

``` r
se0 <- sigma / sqrt(n1) # 標本平均の標準誤差
correct <- sqrt((N1 - n1) / (N1 - 1)) # 有限修正項
se1 <- se0 * correct
print(se1)
```

    [1] 3.427827

## 信頼区間

### 標本平均の信頼区間

$$
\bar{X} \pm t_\alpha(df) \times \frac{s}{\sqrt{n}}
$$

### 信頼区間による $t$ 検定

``` r
qt(p = 0.025, df = 49, lower.tail = FALSE)
```

    [1] 2.009575

### 信頼区間による対応のある場合の 2 標本 $t$ 検定

``` r
df <- read_csv(DATA04)
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
     3    72    75     1    65    NA    65    75
     4    96    84     0    84    NA    84    97
     5    83    75     0    NA    84    75    84
     6    72    74     1    65    NA    65    74
     7    85    76     0    NA    87    76    87
     8    87    77     0    77    NA    77    89
     9    86    77     0    77    NA    77    87
    10    77    80     1    NA    80    70    80
    11    95    87     0    NA    96    87    96
    12    84    75     0    75    NA    75    85
    13    74    77     1    67    NA    67    77
    14    58    61     1    NA    61    52    61
    15    91    81     0    NA    93    81    93
    16    80    72     0    NA    84    72    84
    17    80    72     0    NA    82    72    82
    18    89    80     0    NA    89    80    89
    19    88    80     0    NA    90    80    90
    20    86    78     0    NA    87    78    87

``` r
n1 <- nrow(df)
diff <- df$y1t - df$y0t
m1 <- mean(diff)
s1 <- sd(diff)
talpha <- qt(p = 0.025, df = n1 - 1, lower.tail = FALSE)
print(m1 - talpha * s1 / sqrt(n1))
```

    [1] 9.433675

``` r
print(m1 + talpha * s1 / sqrt(n1))
```

    [1] 10.66633

``` r
t.test(
  x = df$y1t - df$y0t
)
```


        One Sample t-test

    data:  df$y1t - df$y0t
    t = 34.13, df = 19, p-value < 2.2e-16
    alternative hypothesis: true mean is not equal to 0
    95 percent confidence interval:
      9.433675 10.666325
    sample estimates:
    mean of x 
        10.05 

### 信頼区間による対応のない場合の 2 標本 $t$ 検定

標準誤差

$$
s.e.(\bar{Y}^{obs}_1 - \bar{Y}^{obs}_0) = \sqrt{\frac{s_0^2}{n_0} + \frac{s_1^2}{n_1}} \\
df = \frac{
\left( \frac{s_0^2}{n_0} + \frac{s_1^2}{n_1} \right)^2
}{
\frac{ \left( \frac{s_0^2}{n_0} \right)^2 }{n_0 - 1} +
\frac{ \left( \frac{s_1^2}{n_1} \right)^2 }{n_1 - 1}
}
$$

``` r
y0obs <- df |> filter(!is.na(y0)) |> pull(y0)
y1obs <- df |> filter(!is.na(y1)) |> pull(y1)
n0 <- length(y0obs)
n1 <- length(y1obs)
s0 <- sd(y0obs)
s1 <- sd(y1obs)
numerator <- (s1^2 / n1 + s0^2 / n0)^2
denominator <- ((s1^2 / n1)^2) / (n1 - 1) + ((s0^2 / n0)^2) / (n0 - 1)
df1 <- numerator / denominator
xbar <- mean(y1obs) - mean(y0obs)
se1 <- sqrt((s1^2 / n1) + (s0^2 / n0))
talpha <- qt(p = 0.025, df = df1, lower.tail = FALSE)
print(xbar - talpha * se1)
```

    [1] 3.447297

``` r
print(xbar + talpha * se1)
```

    [1] 18.46937

``` r
t.test(y1obs, y0obs, var.equal = FALSE)
```


        Welch Two Sample t-test

    data:  y1obs and y0obs
    t = 3.0692, df = 17.674, p-value = 0.006714
    alternative hypothesis: true difference in means is not equal to 0
    95 percent confidence interval:
      3.447297 18.469370
    sample estimates:
    mean of x mean of y 
     84.08333  73.12500 
