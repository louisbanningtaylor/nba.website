---
editor: visual
title: Team Win Logistic Regression
toc-title: Table of contents
---

::: cell
``` {.r .cell-code}
library(tidyverse)
library(naniar)
library(mplot)
library(ggplot2)
```
:::

::: cell
``` {.r .cell-code}
pbpdata = read.csv('2024pbp.csv')
```
:::

::: cell
``` {.r .cell-code}
advdata = read.csv('advanced.csv')
#view(advdata)
view(colnames(data))

advdata = advdata %>% 
  mutate(
    date = as.Date(date, format = '%d/%m/%Y'), 
    team = as.factor(TEAM),
    y = W
    ) %>% 
  select(y,date, team, eFG., OREB., TOV.) %>% 
  rename(efg = eFG., 
         oreb_rate = OREB., 
         tov_rate = TOV.)

view(advdata)
```
:::

:::: cell
``` {.r .cell-code}
data = read.csv('2024.csv')
#colnames(data)
data = data %>% 
  select(date, team, against, fga, fta) %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(date, team, against) %>%
  summarise(
    fga = sum(fga), 
    fta = sum(fta)
  ) %>% 
  mutate(
    fta_rate = fta*100 / fga
  ) %>% 
  select(date, team, against, fta_rate)
```

::: {.cell-output .cell-output-stderr}
    `summarise()` has grouped output by 'date', 'team'. You can override using the
    `.groups` argument.
:::

``` {.r .cell-code}
view(data)
```
::::

::: cell
``` {.r .cell-code}
df = data %>% 
  left_join(
    advdata, 
    by = c('date' = 'date', 'team' = 'team')
  )
#view(df)

opp_df = df %>% 
  select(
    date, 
    opp = team, 
    opp_fta_rate = fta_rate, 
    opp_oreb_rate = oreb_rate, 
    opp_tov_rate = tov_rate, 
    opp_efg = efg)
#view(opp_df)

df = df %>% 
  left_join( 
    opp_df, 
    by = c('date' = 'date', 'against' = 'opp')) 

view(df)
```
:::

:::::::: cell
``` {.r .cell-code}
null = glm(y ~ 1, family = 'binomial', data = df)
full = glm(y ~., family = 'binomial', data = df)
```

::: {.cell-output .cell-output-stderr}
    Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
:::

``` {.r .cell-code}
step.back.aic = step(full, direction = 'backward', trace = FALSE)
```

::: {.cell-output .cell-output-stderr}
    Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
:::

``` {.r .cell-code}
coef_table = round(summary(step.back.aic)$coef, 3)
add_info = step.back.aic %>% 
  broom::glance() %>% 
  round(2) %>% t()

coef_table
```

::: {.cell-output .cell-output-stdout}
                  Estimate Std. Error z value Pr(>|z|)
    (Intercept)      0.000      1.498   0.000        1
    fta_rate         0.156      0.015  10.225        0
    efg              0.942      0.056  16.961        0
    oreb_rate        0.370      0.026  14.121        0
    tov_rate        -0.764      0.054 -14.262        0
    opp_fta_rate    -0.156      0.015 -10.225        0
    opp_oreb_rate   -0.370      0.026 -14.121        0
    opp_tov_rate     0.764      0.054  14.262        0
    opp_efg         -0.942      0.056 -16.961        0
:::

``` {.r .cell-code}
step.back.aic
```

::: {.cell-output .cell-output-stdout}

    Call:  glm(formula = y ~ fta_rate + efg + oreb_rate + tov_rate + opp_fta_rate + 
        opp_oreb_rate + opp_tov_rate + opp_efg, family = "binomial", 
        data = df)

    Coefficients:
      (Intercept)       fta_rate            efg      oreb_rate       tov_rate  
       -6.664e-14      1.561e-01      9.418e-01      3.695e-01     -7.643e-01  
     opp_fta_rate  opp_oreb_rate   opp_tov_rate        opp_efg  
       -1.561e-01     -3.695e-01      7.643e-01     -9.418e-01  

    Degrees of Freedom: 2445 Total (i.e. Null);  2437 Residual
    Null Deviance:      3391 
    Residual Deviance: 557.9    AIC: 575.9
:::

``` {.r .cell-code}
add_info
```

::: {.cell-output .cell-output-stdout}
                     [,1]
    null.deviance 3390.88
    df.null       2445.00
    logLik        -278.97
    AIC            575.95
    BIC            628.17
    deviance       557.95
    df.residual   2437.00
    nobs          2446.00
:::
::::::::

:::: cell
``` {.r .cell-code}
coefs = 2.232
cols = c('efg', 'fta_rate', 'oreb_rate', 'tov_rate')
values = c(0.942, 0.156, 0.370, -0.764)
abs(values) / coefs
```

::: {.cell-output .cell-output-stdout}
    [1] 0.42204301 0.06989247 0.16577061 0.34229391
:::
::::

:::::: cell
``` {.r .cell-code}
df = df %>% 
  select(!c(team, date, against)) %>% 
  ungroup() %>%
  mutate(pred_prob = predict(full, type = 'response'), 
         pred_win = round(pred_prob))
```

::: {.cell-output .cell-output-stderr}
    Adding missing grouping variables: `date`, `team`
:::

``` {.r .cell-code}
1 - mean(df$y != df$pred_win)
```

::: {.cell-output .cell-output-stdout}
    [1] 0.954211
:::

``` {.r .cell-code}
library(caret)
```

::: {.cell-output .cell-output-stderr}
    Loading required package: lattice

    Attaching package: 'caret'

    The following object is masked from 'package:purrr':

        lift
:::

``` {.r .cell-code}
confusion.glm = confusionMatrix(
data = as.factor(df$pred_win),
reference = as.factor(df$y))
```
::::::

to evaluate what makes a team a good shooting team... I would want to
remove the shooting observations that come as a result of live-ball
turnovers and offensive rebounds.
