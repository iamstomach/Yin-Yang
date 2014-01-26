Title
========================================================

This is an R Markdown document. Markdown is a simple formatting syntax for authoring web pages (click the **MD** toolbar button for help on Markdown).

When you click the **Knit HTML** button a web page will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```r
library(pander)
```

```
## Warning: package 'pander' was built under R version 3.0.2
```

```r
d <- data.frame(name = c("张三", "李四"), age = c(20, 21), employed = factor(c("Y", 
    "N")))
pandoc.table(d)
```

```
## 
## -----------------------
##  name   age   employed 
## ------ ----- ----------
##  张三   20       Y     
## 
##  李四   21       N     
## -----------------------
```

```r
pandoc.table(d, style = "simple")
```

```
## 
## 
##  name   age   employed 
## ------ ----- ----------
##  张三   20       Y     
##  李四   21       N
```

```r
pandoc.table(d, style = "grid")
```

```
## 
## 
## +--------+-------+------------+
## |  name  |  age  |  employed  |
## +========+=======+============+
## |  张三  |  20   |     Y      |
## +--------+-------+------------+
## |  李四  |  21   |     N      |
## +--------+-------+------------+
```

```r
pandoc.table(d, style = "rmarkdown")
```

```
## 
## 
## |  name  |  age  |  employed  |
## |:------:|:-----:|:----------:|
## |  张三  |  20   |     Y      |
## |  李四  |  21   |     N      |
```

```r
pandoc.table(d, style = "rmarkdown", caption = "survey")
```

```
## 
## 
## |  name  |  age  |  employed  |
## |:------:|:-----:|:----------:|
## |  张三  |  20   |     Y      |
## |  李四  |  21   |     N      |
## 
## Table: survey
```

```r
set.caption("survey")
pandoc.table(d, style = "rmarkdown")
```

```
## 
## 
## |  name  |  age  |  employed  |
## |:------:|:-----:|:----------:|
## |  张三  |  20   |     Y      |
## |  李四  |  21   |     N      |
## 
## Table: survey
```

```r
pander(prcomp(iris[, 1:4]))
```

```
## 
## ----------------------------------------------------
##       &nbsp;         PC1      PC2     PC3      PC4  
## ------------------ -------- ------- -------- -------
##  **Sepal.Length**   0.3614  -0.6566  0.582   0.3155 
## 
##  **Sepal.Width**   -0.08452 -0.7302 -0.5979  -0.3197
## 
##  **Petal.Length**   0.8567  0.1734  -0.07624 -0.4798
## 
##  **Petal.Width**    0.3583  0.07548 -0.5458  0.7537 
## ----------------------------------------------------
## 
## Table: Principal Components Analysis
## 
## 
## ----------------------------------------------------------
##            &nbsp;             PC1     PC2    PC3     PC4  
## ---------------------------- ------ ------- ------ -------
##    **Standard deviation**    2.056  0.4926  0.2797 0.1544 
## 
##  **Proportion of Variance**  0.9246 0.05307 0.0171 0.00521
## 
##  **Cumulative Proportion**   0.9246 0.9777  0.9948    1   
## ----------------------------------------------------------
```

```r

library(xtable)
xtable(head(mtcars[, 1:5]))
```

```
## % latex table generated in R 3.0.0 by xtable 1.7-1 package
## % Sun Jan 26 20:04:36 2014
## \begin{table}[ht]
## \centering
## \begin{tabular}{rrrrrr}
##   \hline
##  & mpg & cyl & disp & hp & drat \\ 
##   \hline
## Mazda RX4 & 21.00 & 6.00 & 160.00 & 110.00 & 3.90 \\ 
##   Mazda RX4 Wag & 21.00 & 6.00 & 160.00 & 110.00 & 3.90 \\ 
##   Datsun 710 & 22.80 & 4.00 & 108.00 & 93.00 & 3.85 \\ 
##   Hornet 4 Drive & 21.40 & 6.00 & 258.00 & 110.00 & 3.08 \\ 
##   Hornet Sportabout & 18.70 & 8.00 & 360.00 & 175.00 & 3.15 \\ 
##   Valiant & 18.10 & 6.00 & 225.00 & 105.00 & 2.76 \\ 
##    \hline
## \end{tabular}
## \end{table}
```


