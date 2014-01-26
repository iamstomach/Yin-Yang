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

<TABLE >
<TR> <TH>  </TH> <TH> mpg </TH> <TH> cyl </TH> <TH> disp </TH> <TH> hp </TH> <TH> drat </TH>  </TR>
  <TR> <TD align="right"> Mazda RX4 </TD> <TD align="right"> 21.00 </TD> <TD align="right"> 6.00 </TD> <TD align="right"> 160.00 </TD> <TD align="right"> 110.00 </TD> <TD align="right"> 3.90 </TD> </TR>
  <TR> <TD align="right"> Mazda RX4 Wag </TD> <TD align="right"> 21.00 </TD> <TD align="right"> 6.00 </TD> <TD align="right"> 160.00 </TD> <TD align="right"> 110.00 </TD> <TD align="right"> 3.90 </TD> </TR>
  <TR> <TD align="right"> Datsun 710 </TD> <TD align="right"> 22.80 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 108.00 </TD> <TD align="right"> 93.00 </TD> <TD align="right"> 3.85 </TD> </TR>
  <TR> <TD align="right"> Hornet 4 Drive </TD> <TD align="right"> 21.40 </TD> <TD align="right"> 6.00 </TD> <TD align="right"> 258.00 </TD> <TD align="right"> 110.00 </TD> <TD align="right"> 3.08 </TD> </TR>
  <TR> <TD align="right"> Hornet Sportabout </TD> <TD align="right"> 18.70 </TD> <TD align="right"> 8.00 </TD> <TD align="right"> 360.00 </TD> <TD align="right"> 175.00 </TD> <TD align="right"> 3.15 </TD> </TR>
  <TR> <TD align="right"> Valiant </TD> <TD align="right"> 18.10 </TD> <TD align="right"> 6.00 </TD> <TD align="right"> 225.00 </TD> <TD align="right"> 105.00 </TD> <TD align="right"> 2.76 </TD> </TR>
   </TABLE>


