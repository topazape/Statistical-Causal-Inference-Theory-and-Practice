# chapter13_操作変数法の基礎


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

## 操作変数の定義

操作変数の３つの仮定

1.  操作変数の外生性： $\mathrm{cov}[Z, U] = 0$
2.  操作変数の関連性： $\mathrm{cov}[Z, X_i] \ne 0$
3.  除外制約： $X$ の値が決まれば、 $Z$ に関係なく、 $Y$ への影響は同じ

## R による操作変数推定量

- `x1 <- 1 + 2 * z1 + 2 * x2 + e1`

  - 変数 `x1` は操作変数 `z1` 及び交絡因子 `x2`
    と関連がある：仮定２（操作変数の関連性）

- `y1 <- 3 + 1.5 * x1 + 1.5 * x2 + e2`

  - 結果変数 `y1` は説明変数 `x1` と共変量 `x2`
    の影響は受けているが、操作変数 `z1`
    の影響は受けていない：仮定３（除外制約）

- `u1 <- x2 + e2`

  - 操作変数 `z1` は変数 `1` とは無関係：仮定１（操作変数の外生性）

- 推定対象

  - `x1` の偏回帰係数 `1.5`

データの生成と共分散の値

``` r
set.seed(1)
n1 <- 1000
x2 <- rnorm(n = n1)
z1 <- runif(n = n1)
e1 <- rnorm(n = n1)
x1 <- 1 + 2 * z1 + 2 * x2 + e1

e2 <- rnorm(n = n1)
y1 <- 3 + 1.5 * x1 + 1.5 * x2 + e2
u1 <- x2 + e2

cov(x1, u1)
```

    [1] 2.15697

``` r
cov(z1, u1)
```

    [1] 0.004487261

``` r
cov(z1, x1)
```

    [1] 0.1970798

``` r
cov(z1, y1)
```

    [1] 0.3053871

分析

``` r
sr1 <- lm(y1 ~ x1) # 単回帰モデル
mr1 <- lm(y1 ~ x1 + x2) # 重回帰モデル（正しいが、観測されない共変量 x2 を用いている）
iv1 <- cov(z1, y1) / cov(z1, x1) # 操作変数推定量

summary(sr1)
```


    Call:
    lm(formula = y1 ~ x1)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -4.2233 -0.9238 -0.0132  0.9084  3.7874 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)  1.97124    0.05276   37.36   <2e-16 ***
    x1           2.04833    0.01694  120.94   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 1.303 on 998 degrees of freedom
    Multiple R-squared:  0.9361,    Adjusted R-squared:  0.9361 
    F-statistic: 1.463e+04 on 1 and 998 DF,  p-value: < 2.2e-16

``` r
summary(mr1)
```


    Call:
    lm(formula = y1 ~ x1 + x2)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -2.93575 -0.65215 -0.00119  0.69165  3.00777 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)  3.06901    0.06308   48.66   <2e-16 ***
    x1           1.49319    0.02727   54.76   <2e-16 ***
    x2           1.50629    0.06413   23.49   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 1.046 on 997 degrees of freedom
    Multiple R-squared:  0.9589,    Adjusted R-squared:  0.9588 
    F-statistic: 1.162e+04 on 2 and 997 DF,  p-value: < 2.2e-16

``` r
print(iv1)
```

    [1] 1.54956

操作変数の関連性の確認

``` r
modelDiag1 <- lm(x1 ~ z1)
summary(modelDiag1)
```


    Call:
    lm(formula = x1 ~ z1)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -7.8445 -1.5825  0.0047  1.5870  8.5940 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)   0.8077     0.1448   5.579 3.12e-08 ***
    z1            2.3242     0.2542   9.144  < 2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 2.339 on 998 degrees of freedom
    Multiple R-squared:  0.07731,   Adjusted R-squared:  0.07639 
    F-statistic: 83.62 on 1 and 998 DF,  p-value: < 2.2e-16

## 二段階最小二乗法１：操作変数が１個の場合

操作変数推定量は、二段階最小二乗法（2SLS: two stage least
squares）による回帰モデルによって計算することもできる。

$$
Y_i = \beta_0 + \beta_1 X_{1i} +  U_i \\
\mathrm{cov}[X_1, U] \ne 0
$$

ここで、 $X_{1i}$ に対する操作変数 $Z_i$ があり、 $Z_i$
は以下を満足するとする。

$$
\begin{aligned}
\mathrm{cov}[Z, U] &= 0 \\
\mathrm{cov}[Z, X_1] &\ne 0
\end{aligned}
$$

以下のように、二段階で推定を行う。

$$
\begin{aligned}
\hat{X}_{1i} &= \hat{\delta}_0 + \hat{\delta}_1 Z_{1i} \\
\hat{Y}_i &= \hat{\beta}_0 + \hat{\beta}_{1iv} \hat{X}_{1i}
\end{aligned}
$$

## 二段階最小二乗法２：操作変数が複数個の場合

操作変数となり得る変数の候補が複数個ある場合、推定量の分散が小さくなるように、複数の操作変数を組み合わせて使用する方が良い。

$$
\begin{aligned}
\hat{X}_{1i} &= \hat{\delta}_0 + \hat{\delta}_1 Z_{1i} + \hat{\delta}_2 Z_{2i} \\
\hat{Y}_i &= \hat{\beta}_0 + \hat{\beta}_1 \hat{X}_{1i}
\end{aligned}
$$

## R による二段階最小二乗法

データの入力（操作変数が `z1` と `z2` の２個ある）

``` r
set.seed(1)
n1 <- 1000
u1 <- rnorm(n = n1)
z1 <- runif(n = n1)
z2 <- runif(n = n1)
e1 <- rnorm(n = n1)
x1 <- 1 + 2 * z1 + 2 * z2 + 2 * u1 + e1
e2 <- rnorm(n = n1)
y1 <- 3 + 1.5 * x1 + 1.5 * u1 + e2
```

二段階最小二乗法

``` r
first <- lm(x1 ~ z1 + z2)
x1hat <- predict(first)
second <- lm(y1 ~ x1hat)

summary(first)
```


    Call:
    lm(formula = x1 ~ z1 + z2)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -6.6482 -1.6682  0.0974  1.6358  8.7827 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)   0.8622     0.1917   4.497 7.71e-06 ***
    z1            2.0900     0.2563   8.153 1.06e-15 ***
    z2            2.1720     0.2477   8.769  < 2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 2.359 on 997 degrees of freedom
    Multiple R-squared:  0.1251,    Adjusted R-squared:  0.1233 
    F-statistic: 71.25 on 2 and 997 DF,  p-value: < 2.2e-16

``` r
summary(second)
```


    Call:
    lm(formula = y1 ~ x1hat)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -15.3429  -3.4687   0.1981   3.4085  19.9730 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)   2.7846     0.5626   4.949 8.73e-07 ***
    x1hat         1.5723     0.1815   8.661  < 2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 5.113 on 998 degrees of freedom
    Multiple R-squared:  0.06991,   Adjusted R-squared:  0.06898 
    F-statistic: 75.02 on 1 and 998 DF,  p-value: < 2.2e-16

二段階目の標準誤差

``` r
yhat2 <- coef(second)[1] + coef(second)[2] * x1
resid2 <- y1 - yhat2
residsd2 <- sd(resid2)
se1 <- summary(second)$coefficient[2, 2]
residse <- summary(second)$sigma
se1 * residsd2 / residse
```

    [1] 0.06256237

R パッケージ AER による二段階最小二乗法

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
modelIV <- AER::ivreg(formula = y1 ~ x1 | z1 + z2)
summary(modelIV)
```


    Call:
    AER::ivreg(formula = y1 ~ x1 | z1 + z2)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -6.49116 -1.08120  0.03875  1.15469  6.18235 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)  2.78462    0.19399   14.35   <2e-16 ***
    x1           1.57230    0.06259   25.12   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 1.763 on 998 degrees of freedom
    Multiple R-Squared: 0.8894, Adjusted R-squared: 0.8893 
    Wald test:   631 on 1 and 998 DF,  p-value: < 2.2e-16 

``` r
confint(modelIV)
```

                   2.5 %   97.5 %
    (Intercept) 2.404400 3.164838
    x1          1.449621 1.694984

二段階最小二乗法における頑健な標準誤差

``` r
summary(modelIV, vcov = sandwich)
```


    Call:
    AER::ivreg(formula = y1 ~ x1 | z1 + z2)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -6.49116 -1.08120  0.03875  1.15469  6.18235 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)  2.78462    0.19581   14.22   <2e-16 ***
    x1           1.57230    0.06368   24.69   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 1.763 on 998 degrees of freedom
    Multiple R-Squared: 0.8894, Adjusted R-squared: 0.8893 
    Wald test: 609.7 on 1 and 998 DF,  p-value: < 2.2e-16 

## 二値変数の場合の二段階最小二乗法

処置の割付け変数 $T_i$ は二値変数である。 $U_i$ が未観測の行楽とする。

$$
Y_i = \beta_0 + \beta_1 T_i + U_i
$$

$T_i$ に対する操作変数 $Z_i$
がある。二段階最小二乗法によって推定するとき、一段階目の推定は、ロジスティック回帰モデルやプロビット回帰モデルを使ったほうが良さそうに見える。

$$
\begin{aligned}
\hat{T}_i &= \hat{\delta}_0 + \hat{\delta}_1 Z_i \\
\hat{Y}_i &= \hat{\beta}_0 + \hat{\beta}_1 \hat{T}_i
\end{aligned}
$$

しかしながら、一段階目において通常の最小二乗法を使った時にだけ、一段階目の斬さが予測値及び共変量と無双感になることが保証されると知られている。

## 操作変数の妥当性の検証

二段階最小二乗法と診断

- `Weak instruments`

  - 一段階目のモデルにおける F 検定の結果

  - 有意であれば、操作変数の関連性が満たされている

- `Wu-Hausman`

  - Hausman のない生成の検定の結果

  - 有意であれば操作変数の外生性が満たされている

- `Sargan`

  - Sargan の過剰識別制約の検定（Sargan test of overidentifying
    restriction）

  - 操作変数が必要な数以上にモデルに含まれている場合に使える検定

これらの検定結果はミスリーディングな結果を返す傾向があると指摘されている。

``` r
summary(modelIV, diagnostics = TRUE)
```


    Call:
    AER::ivreg(formula = y1 ~ x1 | z1 + z2)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -6.49116 -1.08120  0.03875  1.15469  6.18235 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)  2.78462    0.19399   14.35   <2e-16 ***
    x1           1.57230    0.06259   25.12   <2e-16 ***

    Diagnostic tests:
                     df1 df2 statistic p-value    
    Weak instruments   2 997    71.255  <2e-16 ***
    Wu-Hausman         1 997   126.976  <2e-16 ***
    Sargan             1  NA     1.007   0.316    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 1.763 on 998 degrees of freedom
    Multiple R-Squared: 0.8894, Adjusted R-squared: 0.8893 
    Wald test:   631 on 1 and 998 DF,  p-value: < 2.2e-16 

- 操作変数の外生性が満たされる時、操作変数の関連性を満たすことが難しくなる

- 操作変数の関連性が満たされる時、操作変数の外生性を満たすことが難しくなる

観察研究において、適切な操作変数を見つけることは本質的に難しい。

「結局は操作変数が解析者による何らかの操作や介入や、突発的な災害や事故の前後といった変数以外では操作変数法の解析結果はあまり信頼されません」

操作変数法の基本事項は、統計的因果推論の立場からも重要。
