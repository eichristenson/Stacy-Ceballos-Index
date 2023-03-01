Virus Graphs and I<sub>SC</sub> Code
================
Elijah I. Christenson
2022-06-23

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you
execute code within the notebook, the results appear beneath the code.

## Read in your data to the R environment from excel using Import Dataset function

Data should be in the following format in excel:

hpi=Hours post infection, abs=absorbance at 600nm

| time.hpi | ctrl.abs | virus1.abs | virus2.abs | ctrl.sd | virus1.sd | virus2.sd |
|----------|----------|------------|------------|---------|-----------|-----------|
| 0        | 0.2      | 0.2        | 0.2        | 0.05    | 0.05      | 0.05      |
| ——–      | ——–      | ———-       | ———-       | ——-     | ———       | ——–       |
| 2        | 0.4      | 0.3        | 0.35       | 0.1     | 0.03      | 0.02      |

**Example Data Below**

``` r
datafile <- read_excel(Your Data File Here)#put your final data excel sheet here
datafile
```

    ## # A tibble: 16 × 7
    ##    time.hpi ctrl.abs ctrl.sd virus1.abs virus1.sd virus2.abs virus2.sd
    ##       <dbl>    <dbl>   <dbl>      <dbl>     <dbl>      <dbl>     <dbl>
    ##  1        1     0.15    0.02       0.1       0.02       0.13      0.02
    ##  2        2     0.3     0.02       0.18      0.02       0.25      0.02
    ##  3        3     0.45    0.02       0.3       0.02       0.4       0.02
    ##  4        4     0.58    0.02       0.45      0.02       0.55      0.02
    ##  5        5     0.72    0.02       0.49      0.02       0.67      0.02
    ##  6        6     0.75    0.02       0.52      0.02       0.7       0.02
    ##  7        7     0.8     0.02       0.52      0.02       0.72      0.02
    ##  8        8     0.81    0.02       0.54      0.02       0.72      0.02
    ##  9        9     0.82    0.02       0.53      0.02       0.72      0.02
    ## 10       10     0.82    0.02       0.54      0.02       0.72      0.02
    ## 11       11     0.82    0.02       0.56      0.02       0.72      0.02
    ## 12       12     0.82    0.02       0.54      0.02       0.71      0.02
    ## 13       13     0.81    0.02       0.53      0.02       0.71      0.02
    ## 14       14     0.81    0.02       0.53      0.02       0.71      0.02
    ## 15       15     0.81    0.02       0.53      0.02       0.71      0.02
    ## 16       16     0.8     0.02       0.53      0.02       0.71      0.02

## Load necessary libraries

``` r
r <- getOption("repos")
r["CRAN"] <- "http://cran.cnr.berkeley.edu/"
options(repos = r)

#packages that are needed
if(!require("devtools")) install.packages("devtools")
```

    ## Loading required package: devtools

    ## Loading required package: usethis

``` r
devtools::install_github("briandconnelly/growthcurve", build_vignettes = TRUE)
```

    ## Skipping install of 'growthcurve' from a github remote, the SHA1 (b763c288) has not changed since last install.
    ##   Use `force = TRUE` to force installation

``` r
if(!require("growthcurver")) install.packages("growthcurver")
```

    ## Loading required package: growthcurver

``` r
if(!require("dplyr")) install.packages("dplyr")
```

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
if(!require("ggplot2")) install.packages("ggplot2")
```

    ## Loading required package: ggplot2

``` r
if(!require("growthcurve")) install.packages("growthcurve")
```

    ## Loading required package: growthcurve

``` r
if(!require("readxl")) install.packages("readxl")
```

    ## Loading required package: readxl

``` r
library(growthcurver)
library(dplyr)
library(ggplot2)
library(growthcurve)
library(readxl)
```




``` r
if(!require("tidyverse")) install.packages("tidyverse")
```

    ## Loading required package: tidyverse

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ tibble  3.1.7     ✔ purrr   0.3.4
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
if(!require("DescTools")) install.packages("DescTools")
```

    ## Loading required package: DescTools

``` r
if(!require("growthrates")) install.packages("growthrates")
```

    ## Loading required package: growthrates

    ## Loading required package: lattice

    ## Loading required package: deSolve

``` r
if(!require("readxl")) install.packages("readxl")
if(!require("knitr")) install.packages("knitr")
```

    ## Loading required package: knitr

``` r
if(!require("here")) install.packages("here")
```

    ## Loading required package: here

    ## here() starts at C:/Users/Elijah Christenson/Downloads

``` r
library("tidyverse")
library("DescTools") 
library("growthrates")
library("readxl") 
library("knitr")
library("here")
```

## The following finds max values and labels them 

``` r
#the following finds asymptotes
con_abs=datafile[c("ctrl.abs")] #change to specify column
Nasymcon <- max(con_abs) #picks max value from specified column
inf_abs=datafile[c("virus1.abs")] #change to specify column
Nasyminf <- max(inf_abs) #pick max value from specified column
```

## To calculate PIMAX, ISC, and PIAUC
Using the equations below from “Quantifying relative virulence”,
<https://doi.org/10.1099/jgv.0.001515>

![PI\_{max} = (1- N\_{asymptote(infected)}/N\_{asymptote(control)}) \* 100](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;PI_%7Bmax%7D%20%3D%20%281-%20N_%7Basymptote%28infected%29%7D%2FN_%7Basymptote%28control%29%7D%29%20%2A%20100 "PI_{max} = (1- N_{asymptote(infected)}/N_{asymptote(control)}) * 100")

![I\_{SC} = \[PI\_{AUC} \* PI\_{max}\]^{1/2}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;I_%7BSC%7D%20%3D%20%5BPI_%7BAUC%7D%20%2A%20PI_%7Bmax%7D%5D%5E%7B1%2F2%7D "I_{SC} = [PI_{AUC} * PI_{max}]^{1/2}")

![PI\_{AUC} = (1- AUC\_{(infected)}/AUC\_{(control)}) \* 100](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;PI_%7BAUC%7D%20%3D%20%281-%20AUC_%7B%28infected%29%7D%2FAUC_%7B%28control%29%7D%29%20%2A%20100 "PI_{AUC} = (1- AUC_{(infected)}/AUC_{(control)}) * 100")

``` r
PIMAX=(1-(as.numeric(Nasyminf))/as.numeric(Nasymcon))*100 #calculates PImax
AUCcon=AUC(datafile$time.hpi[1:15], datafile$ctrl.abs[1:15])#AUC for control, change ratio as needed, ratio should be [1:total number of time points to include]
AUCinf=AUC(datafile$time.hpi[1:15], datafile$virus1.abs[1:15])#AUC for infected, change ratio as needed 
PIAUC=(1-(AUCinf/AUCcon))*100 #calculates PIauc
IscTest=(PIAUC*PIMAX)^(0.5) #calculates Isc
```

### Code to make graph with asymptotes and shading

    This helps to better visualize the ISC and AUC

``` r
ggplot(datafile, aes(x=time.hpi)) + 
  
  theme_light(base_size = 10)+
  geom_point(aes(y = ctrl.abs), color = "darkred") + #copy to add more datasets
  geom_point(aes(y = virus1.abs), color="steelblue")+
  geom_errorbar(aes(ymin=ctrl.abs-ctrl.sd, ymax=ctrl.abs+ctrl.sd),
                color="darkred", width=.1)+
  geom_errorbar(aes(ymin=virus1.abs-virus1.sd, ymax=virus1.abs+virus1.sd),
                color="steelblue", width=.1)+ #copy geom_errorbar to add data

  geom_vline(xintercept= 11, linetype= "solid", color= "black", size=0.75)+ 
#this is a line that should intersect at the time point with the highest absorbance value on the control
  
  geom_hline(yintercept= Nasymcon, linetype= "dashed", color= "darkred", size=0.70)+ #Horizontal lines, intersect curve at highest absorbance value, adjust manually
  geom_hline(yintercept= Nasyminf, linetype= "dashed", color= "steelblue", size=0.70)+
  annotate("text", x = 2.2, y = 0.56, label = "N[asymptote]", vjust = -0.5)+ #dashed line labels, adjust to where you want it on graph
  
  ggtitle("testgraph")+ #main title
  
  labs(x="Hours post-infection (hpi)", y="Absorbance (OD 600nm)")+ #axis labels
  geom_ribbon(data = datafile %>% filter(time.hpi < 12), ymin=-Inf, 
              aes(ymax=ctrl.abs), fill='darkred', alpha=0.6)+ 
geom_ribbon( data = datafile %>% filter(time.hpi < 12), ymin=-Inf, 
             aes(ymax=virus1.abs), fill='steelblue', alpha=0.4) #Fill colors, you will need to change the (time.hpi < intercept+1) argument to one integer higher than your vertical x-intercept line
```

![](Virus-Graph-and-Isc-Code-Elijah-Christenson-Final_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
