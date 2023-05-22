## R Markdown

renv::init()

## Calculates I<sub>SC</sub>, as well as AUC and other necessary values to achieve I<sub>SC</sub>

This loads necessary packages and finds N<sub>asymptote</sub> for both
control and infected

    install.packages("dplyr",repos = "http://cran.us.r-project.org")

    ## Installing package into 'C:/Users/plymaler/AppData/Local/R/win-library/4.3'
    ## (as 'lib' is unspecified)

    ## package 'dplyr' successfully unpacked and MD5 sums checked

    ## Warning: cannot remove prior installation of package 'dplyr'

    ## Warning in file.copy(savedcopy, lib, recursive = TRUE): problem copying
    ## C:\Users\plymaler\AppData\Local\R\win-library\4.3\00LOCK\dplyr\libs\x64\dplyr.dll
    ## to C:\Users\plymaler\AppData\Local\R\win-library\4.3\dplyr\libs\x64\dplyr.dll:
    ## Permission denied

    ## Warning: restored 'dplyr'

    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\plymaler\AppData\Local\Temp\Rtmpmgjkgh\downloaded_packages

    install.packages("ggplot2",repos = "http://cran.us.r-project.org")

    ## Installing package into 'C:/Users/plymaler/AppData/Local/R/win-library/4.3'
    ## (as 'lib' is unspecified)

    ## package 'ggplot2' successfully unpacked and MD5 sums checked
    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\plymaler\AppData\Local\Temp\Rtmpmgjkgh\downloaded_packages

    install.packages("readxl",repos = "http://cran.us.r-project.org")

    ## Installing package into 'C:/Users/plymaler/AppData/Local/R/win-library/4.3'
    ## (as 'lib' is unspecified)

    ## package 'readxl' successfully unpacked and MD5 sums checked
    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\plymaler\AppData\Local\Temp\Rtmpmgjkgh\downloaded_packages

    install.packages("DescTools",repos = "http://cran.us.r-project.org")

    ## Installing package into 'C:/Users/plymaler/AppData/Local/R/win-library/4.3'
    ## (as 'lib' is unspecified)

    ## package 'DescTools' successfully unpacked and MD5 sums checked
    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\plymaler\AppData\Local\Temp\Rtmpmgjkgh\downloaded_packages

    library("dplyr")

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library("ggplot2")
    library("readxl") 
    library("DescTools")

## Read in your data to the R environment from excel using Import Dataset function

Data should be in the following format in excel:

hpi=Hours post infection, abs=absorbance at 600nm

<table>
<colgroup>
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 16%" />
<col style="width: 16%" />
<col style="width: 12%" />
<col style="width: 14%" />
<col style="width: 14%" />
</colgroup>
<thead>
<tr class="header">
<th>time.hpi</th>
<th>ctrl.abs</th>
<th>virus1.abs</th>
<th>virus2.abs</th>
<th>ctrl.sd</th>
<th>virus1.sd</th>
<th>virus2.sd</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>0</td>
<td>0.2</td>
<td>0.2</td>
<td>0.2</td>
<td>0.05</td>
<td>0.05</td>
<td>0.05</td>
</tr>
<tr class="even">
<td>2</td>
<td>0.4</td>
<td>0.3</td>
<td>0.35</td>
<td>0.1</td>
<td>0.03</td>
<td>0.02</td>
</tr>
</tbody>
</table>

    testing = read_xlsx("testing.xlsx")
    time0 = data.frame(time.hpi=0,   
                       ctrl.abs=0,   
                       ctrl.sd=0,
                       virus1.abs=0,
                       virus1.sd=0,
                       virus2.abs=0,
                       virus2.sd=0)
    testing = bind_rows(time0, testing)

    #the following finds asymptotes
    time=testing[c("time.hpi")] #change to specify column
    con_abs=testing[c("ctrl.abs")] #change to specify column
    Nasymcon <- max(con_abs) #picks max value from specified column
    con_inf=testing[c("virus1.abs")] #change to specify column
    Nasyminf <- max(con_inf) #pick max value from specified column

## To calculate PI<sub>max</sub>, I<sub>SC</sub>, PI<sub>AUC</sub>, and V<sub>R</sub>

Using the equations below from “Quantifying relative virulence”,
<https://doi.org/10.1099/jgv.0.001515>
*P**I*<sub>*m**a**x*</sub> = (1−*N*<sub>*a**s**y**m**p**t**o**t**e*(*i**n**f**e**c**t**e**d*)</sub>/*N*<sub>*a**s**y**m**p**t**o**t**e*(*c**o**n**t**r**o**l*)</sub>) \* 100
*I*<sub>*S**C*</sub> = \[*P**I*<sub>*A**U**C*</sub>\**P**I*<sub>*m**a**x*</sub>\]<sup>1/2</sup>
*P**I*<sub>*A**U**C*</sub> = (1−*A**U**C*<sub>(*i**n**f**e**c**t**e**d*)</sub>/*A**U**C*<sub>(*c**o**n**t**r**o**l*)</sub>) \* 100
*V*<sub>*R*</sub> = *V*<sub>*R*</sub> + (*I*<sub>*S**C*</sub>\[*i*+1\]+*I*<sub>*S**C*</sub>\[*i*\]) \* (*t*\[*i*+1\]−*t*\[*i*\])/(*t*<sub>*n*</sub>−*t*<sub>1</sub>)/2

    PIMAX=(1-(Nasyminf/Nasymcon))*100 #calculates PImax
    AUCcon=AUC(testing$time.hpi[1:16], testing$ctrl.abs[1:16])#AUC for control, change ratio as needed, ratio should be [1:total number of time points to include]
    AUCinf=AUC(testing$time.hpi[1:16], testing$virus1.abs[1:16])#AUC for infected, change ratio as needed 
    PIAUC=(1-(AUCinf/AUCcon))*100 #calculates PIauc
    Isc=(PIAUC*PIMAX)^(0.5) #calculates Isc

    # To calculate VR, we calculate N.asy.inf, N.asy.ctr, AUC.inf, AUC.ctr, PI.AUC, PI.max and I.sc for each upperbound
    time = testing$time.hpi[1:16]
    ctr.abs = testing$ctrl.abs[1:16]
    inf.abs = testing$virus1.abs[1:16]

    Nasyminf.vec = numeric()
    Nasyminf.vec[1] = 0
    Nasymcon.vec = numeric()
    Nasymcon.vec[1] = 0
    AUCinf.vec = numeric()
    AUCinf.vec[1] = 0
    AUCcon.vec = numeric()
    AUCcon.vec[1] = 0
    PIAUC.vec = numeric()
    PIAUC.vec[1] = 0
    PIMAX.vec= numeric()
    PIMAX.vec[1] = 0 
    Isc.vec = numeric()
    Isc.vec[1] = 0

    for(i in 2:length(time)){
      time.temp = time[1:i]
      ctr.abs.temp = ctr.abs[1:i]
      inf.abs.temp = inf.abs[1:i]
      Nasyminf.vec[i] = max(inf.abs.temp)
      Nasymcon.vec[i] = max(ctr.abs.temp)
      AUCinf.vec[i] = AUC(time.temp, inf.abs.temp)
      AUCcon.vec[i] = AUC(time.temp, ctr.abs.temp)
      PIAUC.vec[i] = (1-AUCinf.vec[i]/AUCcon.vec[i])*100
      PIMAX.vec[i] = (1-Nasyminf.vec[i]/Nasymcon.vec[i])*100
      Isc.vec[i] = sqrt(PIAUC.vec[i]*PIMAX.vec[i])
    }

    results = data.frame(time,
                         ctr.abs,
                         inf.abs,
                         Nasymcon.vec,
                         Nasyminf.vec,
                         AUCcon.vec,
                         AUCinf.vec,
                         PIAUC.vec,
                         PIMAX.vec,
                         Isc.vec)

    results

    ##    time ctr.abs inf.abs Nasymcon.vec Nasyminf.vec AUCcon.vec AUCinf.vec
    ## 1     0    0.00    0.00         0.00         0.00      0.000      0.000
    ## 2     1    0.15    0.10         0.15         0.10      0.075      0.050
    ## 3     2    0.30    0.18         0.30         0.18      0.300      0.190
    ## 4     3    0.45    0.30         0.45         0.30      0.675      0.430
    ## 5     4    0.58    0.45         0.58         0.45      1.190      0.805
    ## 6     5    0.72    0.49         0.72         0.49      1.840      1.275
    ## 7     6    0.75    0.52         0.75         0.52      2.575      1.780
    ## 8     7    0.80    0.52         0.80         0.52      3.350      2.300
    ## 9     8    0.81    0.54         0.81         0.54      4.155      2.830
    ## 10    9    0.82    0.53         0.82         0.54      4.970      3.365
    ## 11   10    0.82    0.54         0.82         0.54      5.790      3.900
    ## 12   11    0.82    0.56         0.82         0.56      6.610      4.450
    ## 13   12    0.82    0.54         0.82         0.56      7.430      5.000
    ## 14   13    0.81    0.53         0.82         0.56      8.245      5.535
    ## 15   14    0.81    0.53         0.82         0.56      9.055      6.065
    ## 16   15    0.81    0.53         0.82         0.56      9.865      6.595
    ##    PIAUC.vec PIMAX.vec  Isc.vec
    ## 1    0.00000   0.00000  0.00000
    ## 2   33.33333  33.33333 33.33333
    ## 3   36.66667  40.00000 38.29708
    ## 4   36.29630  33.33333 34.78328
    ## 5   32.35294  22.41379 26.92865
    ## 6   30.70652  31.94444 31.31937
    ## 7   30.87379  30.66667 30.77005
    ## 8   31.34328  35.00000 33.12122
    ## 9   31.88929  33.33333 32.60332
    ## 10  32.29376  34.14634 33.20714
    ## 11  32.64249  34.14634 33.38595
    ## 12  32.67776  31.70732 32.18888
    ## 13  32.70525  31.70732 32.20242
    ## 14  32.86841  31.70732 32.28264
    ## 15  33.02043  31.70732 32.35721
    ## 16  33.14749  31.70732 32.41941

    # Finally we calculate VR 
    VR = 0 # initial value
    # Note that here t0 is not involved, so t1 = time[2]
    for(i in 2:(nrow(results)-1)){
      VR = VR + (Isc.vec[i+1] + Isc.vec[i])*(time[i+1]-time[i])/(time[nrow(results)]-time[2])/2
    }

    VR

    ## [1] 32.59454

### Code to make graph with asymptotes and shading

    This helps to better visualize the I~SC~ and AUC

    ggplot(data=testing, aes(x=time.hpi)) + 
      
      theme_light(base_size = 10)+
      geom_point(aes(y = ctrl.abs), color = "darkred") + #copy to add more datasets
      geom_point(aes(y = virus1.abs), color="steelblue")+
      geom_errorbar(aes(ymin=ctrl.abs-ctrl.sd, ymax=ctrl.abs+ctrl.sd),
                    color="darkred", width=.1)+
      geom_errorbar(aes(ymin=virus1.abs-virus1.sd, ymax=virus1.abs+virus1.sd),
                    color="steelblue", width=.1)+ #copy geom_errorbar to add data

      geom_vline(xintercept=which.max(testing$ctrl.abs), linetype= "solid", color= "black", linewidth=0.75)+ 
    #this is a line that should intersect at the time point with the highest absorbance value on the control
      
      geom_hline(yintercept= Nasymcon, linetype= "dashed", color= "darkred", linewidth=0.70)+ #Horizontal lines, intersect curve at highest absorbance value
      geom_hline(yintercept= Nasyminf, linetype= "dashed", color= "steelblue", linewidth=0.70)+
      annotate("text", x = 2.2, y = 0.56, label = "N[asymptote]", vjust = -0.5)+ #dashed line labels, adjust to where you want it on graph
      
      ggtitle("testgraph")+ #main title
      
      labs(x="Hours post-infection (hpi)", y="Absorbance (OD 600nm)")+ #axis labels
      geom_ribbon(data = testing %>% filter(time.hpi < 12), ymin=-Inf, 
                  aes(ymax=ctrl.abs), fill='darkred', alpha=0.6)+ 
    geom_ribbon( data = testing %>% filter(time.hpi < 12), ymin=-Inf, 
                 aes(ymax=virus1.abs), fill='steelblue', alpha=0.4) #Fill colors, you will need to change the (time.hpi < intercept+1) argument to one integer higher than your vertical x-intercept line

![](ISC-and-Growth-Curve-Graphs-5-19-2023_files/figure-markdown_strict/unnamed-chunk-5-1.png)

renv::snapshot()
