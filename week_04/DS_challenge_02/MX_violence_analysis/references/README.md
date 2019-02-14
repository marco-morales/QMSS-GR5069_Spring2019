
# Data dictionary

The dataset `AllViolenceData_171220.csv` cointans information on **Confrontations** and **Aggressions** between armed forces and criminal organizations between December 2006 and November 2011 in Mexico. The data is a transformation of datasets released by the [Drug Policy Program (PPD)](http://www.politicadedrogas.org/) and is preserved at the  event level. 


## Definition

PDD subscribes to the original definitions of **Confrontations** and **Aggressions** which define

**Confrontations** as sporadic and isolated acts of violence, crimes or other affrays carried out by organized crime involving firearmas and military-grade equipment. In particular, a **Confrontation** must involve 

* at least three (3) organized crime members, or less if using military-grade equipment and explosives  
* violent resistance to armes forces and other government authorities
* a response to an "aggression" where fire is exchanged
* events where criminals cannot be subdued on a single tactical manouver
* extraordinarily, skirmishes within penitentiaries involving organized crime

As defined above, Confrontations can take place:

1. Against (federal, state, municipal) government authorities
  * during an operation aiming to combat organized crime
  * during routine activities where a crime is being committed
  * during operations prompted by civilian denounciations

2. Between or within criminal organizations:
  * skirmishes with rival organizations
  * skirmishes happening in "known" territories of a criminal organizaton
  * disciplinary acts within an organization


**Agressions** are defined as attacks perpetrated by criminal organizations 
against any level of government, where the authorities lack the means to carry 
out an armed response. In particular, **Aggressions** must involve

* no sustained armed response by the authorities
* use of high-caliber weapons, hand granades and/or explosives
* existing evidence that suggests that perpetrators are members of organized crime
* one or more aggressors participating

The attacks are carried out against (federal, state, municipal) government 
facilities, defined as
  * facilities owned by the (federal, state, municipal) government
  * facilities used by authorities 
  * checkpoints 
  * patrols and convoys
  * uniformed public officials and/or labeled vehicles  


## Raw data

The original Confrontations dataset was downloaded from the 
[Drug Policy Program (PPD)](http://www.politicadedrogas.org/) site on `2/9/2017 at 19:23 hrs (EST)` from 
http://www.data-ppd.net/PPD/documentos/CIDE-PPD/Zip/A-E.zip and the original Aggressions data on `2/16/2017 at 7:23 hrs (EST)` from 
http://www.data-ppd.net/PPD/documentos/CIDE-PPD/Zip/A-A.zip 

Additional inputs: 
* a conversion table for State and Municipality names, was obtained from Mexico's [National Institute for Statistics and Geography (INEGI)](http://www.inegi.org.mx/)
* a conversion table for armed forces names that was manually transcribed from PDD's documentation 

The dataset was manipulated using the script `Gather_ViolenceData_171220.R`

## Variables 

The manipulated dataset `AllViolenceData_171220.csv` contains the following variables:

Variable |  description 
--- | --- 
`event_id` | original unique id for every "event" on each database
`unix_timestamp` | UNIX (numeric) timestamp  
`date` | date when the "event" took place 
`state_code` | a unique numeric code per state as defined by INEGI 
`state`      | full state name as defined by INEGI
`state_abbr` | state name abbreviation as defined by INEGI 
`mun_code`  | a unique numeric code per municipality as defined by INEGI
`municipality` | full municipality name as defined by INEGI
`detained` | number of people detained in the event
`total_people_dead` | number of people killed in the event
`military_dead` | number Army personnel killed in the event
`navy_dead` | number Navy personnel killed in the event
`federal_police_dead` | number Federal Police personnel killed in the event
`afi_dead` | number AFI (Federal Investigation Agency) personnel killed in the event
`state_police_dead` | number state police personnel killed in the event
`ministerial_police_dead` | number ministerial police personnel killed in the event
`municipal_police_dead` | number municipal police personnel killed in the event
`public_prosecutor_dead` | number public prosecutors killed in the event
`organized_crime_dead` | number alleged criminals  killed in the event
`civilian_dead` | number civilians killed in the event
`total_people_wounded` | number of people wounded in the event
`military_wounded` | number of Army personnel wounded in the event
`navy_wounded` | number of Navy personnel wounded in the event
`federal_police_wounded` | number of Federal Police personnel wounded in the event
`afi_wounded` | number of AFI personnel wounded in the event
`state_police_wounded` | number of state police personnel wounded in the event
`ministerial_police_wounded` | number of ministerial police personnel wounded in the event
`municipal_police_wounded` | number of municipal police personnel wounded in the event
`public_prosecutor_wounded` | number of public prosecutors wounded in the event
`organized_crime_wounded` | number of alleged criminals wounded in the event
`civilian_wounded` | number of civilians wounded in the event
`long_guns_seized` | number of long guns seized in the event
`small_arms_seized` | number of small arms seized in the event
`cartridge_sezied` | number of cartridged seized in the event
`clips_seized` | number of clips seized in the event
`vehicles_seized` | number vehicles seized in the event
`afi` | binary indicator for participation of AFI
`army` | binary indicator for participation of the Army
`federal_police` | binary indicator for participation of the FederalPolice
`ministerial_police` | binary indicator for participation of Ministerial Police
`municipal_police` | binary indicator for participation of Municipal Police
`navy` | binary indicator for participation of the Navy  
`other` | binary indicator for participation of other armed forces or government entities
`state_police` | binary indicator for participation of State Police
`source` | string indicating whether event was on Confrontations or Aggresions database 
`organized_crime_lethality` | event-level lethality index for organized crime 
`army_lethality` | event-level lethality index for Army    
`navy_lethality` | event-level lethality index for Navy
`federal_police_lethality` | event-level lethality index for Federal Police 
`organized_crime_lethality_diff` | event-level difference between dead and wounded for organized crime 
`army_lethality_diff` | event-level difference between dead and wounded for Army
`navy_lethality_diff` | event-level difference between dead and wounded for Navy 
`federal_police_lethality_diff` | event-level difference between dead and wounded for Federal Police 
`organized_crime_NewIndex` | event-level new dead to wounded index for organized crime 
`army_NewIndex` | event-level new dead to wounded index for Army 
`navy_NewIndex` | event-level new dead to wounded index for Navy    
`federal_police_NewIndex` | event-level new dead to wounded index for Federal Police 
`perfect_lethality` | binay indicator for events of perfect lethality    
`category` | categorical variable indicating type of event {perfect_lethality, no_dead_wounded, dead_wounded, just_wounded}          
`global_id` | a unique ID for every event in the full data set   


## Summary statistics for the dataset

```
    event_id    unix_timestamp           date              state_code      state          
 Min.   :   1   Min.   :1.165e+09   Min.   :2006-12-02   Min.   : 1.0   Length:5396       
 1st Qu.: 675   1st Qu.:1.261e+09   1st Qu.:2009-12-15   1st Qu.:11.0   Class :character  
 Median :1350   Median :1.286e+09   Median :2010-09-30   Median :19.0   Mode  :character  
 Mean   :1589   Mean   :1.278e+09   Mean   :2010-07-02   Mean   :18.1                     
 3rd Qu.:2486   3rd Qu.:1.305e+09   3rd Qu.:2011-05-05   3rd Qu.:27.0                     
 Max.   :3835   Max.   :1.322e+09   Max.   :2011-11-27   Max.   :32.0                     
                                                                                          
  state_abbr           mun_code      municipality          detained      total_people_dead
 Length:5396        Min.   :  0.00   Length:5396        Min.   : 0.000   Min.   : 0.000   
 Class :character   1st Qu.: 12.00   Class :character   1st Qu.: 0.000   1st Qu.: 0.000   
 Mode  :character   Median : 28.00   Mode  :character   Median : 0.000   Median : 0.000   
                    Mean   : 35.62                      Mean   : 1.041   Mean   : 1.178   
                    3rd Qu.: 39.00                      3rd Qu.: 1.000   3rd Qu.: 2.000   
                    Max.   :551.00                      Max.   :40.000   Max.   :52.000   
                                                                                          
 military_dead      navy_dead        federal_police_dead    afi_dead       state_police_dead 
 Min.   :0.0000   Min.   :0.000000   Min.   : 0.00000    Min.   :0.00000   Min.   : 0.00000  
 1st Qu.:0.0000   1st Qu.:0.000000   1st Qu.: 0.00000    1st Qu.:0.00000   1st Qu.: 0.00000  
 Median :0.0000   Median :0.000000   Median : 0.00000    Median :0.00000   Median : 0.00000  
 Mean   :0.0252   Mean   :0.004077   Mean   : 0.02687    Mean   :0.00556   Mean   : 0.03595  
 3rd Qu.:0.0000   3rd Qu.:0.000000   3rd Qu.: 0.00000    3rd Qu.:0.00000   3rd Qu.: 0.00000  
 Max.   :6.0000   Max.   :3.000000   Max.   :12.00000    Max.   :6.00000   Max.   :12.00000  
                                                                                             
 ministerial_police_dead municipal_police_dead public_prosecutor_dead organized_crime_dead
 Min.   :0.00000         Min.   :0.00000       Min.   :0.0000000      Min.   : 0.0000     
 1st Qu.:0.00000         1st Qu.:0.00000       1st Qu.:0.0000000      1st Qu.: 0.0000     
 Median :0.00000         Median :0.00000       Median :0.0000000      Median : 0.0000     
 Mean   :0.03299         Mean   :0.09859       Mean   :0.0009266      Mean   : 0.8523     
 3rd Qu.:0.00000         3rd Qu.:0.00000       3rd Qu.:0.0000000      3rd Qu.: 1.0000     
 Max.   :8.00000         Max.   :7.00000       Max.   :1.0000000      Max.   :29.0000     
                                                                                          
 civilian_dead      total_people_wounded military_wounded  navy_wounded    
 Min.   : 0.00000   Min.   : 0.0000      Min.   :0.0000   Min.   :0.00000  
 1st Qu.: 0.00000   1st Qu.: 0.0000      1st Qu.:0.0000   1st Qu.:0.00000  
 Median : 0.00000   Median : 0.0000      Median :0.0000   Median :0.00000  
 Mean   : 0.09563   Mean   : 0.9392      Mean   :0.1292   Mean   :0.01279  
 3rd Qu.: 0.00000   3rd Qu.: 1.0000      3rd Qu.:0.0000   3rd Qu.:0.00000  
 Max.   :52.00000   Max.   :30.0000      Max.   :9.0000   Max.   :9.00000  
                                                                           
 federal_police_wounded  afi_wounded       state_police_wounded ministerial_police_wounded
 Min.   : 0.00000       Min.   : 0.00000   Min.   :0.00000      Min.   : 0.00000          
 1st Qu.: 0.00000       1st Qu.: 0.00000   1st Qu.:0.00000      1st Qu.: 0.00000          
 Median : 0.00000       Median : 0.00000   Median :0.00000      Median : 0.00000          
 Mean   : 0.08358       Mean   : 0.01019   Mean   :0.05893      Mean   : 0.05411          
 3rd Qu.: 0.00000       3rd Qu.: 0.00000   3rd Qu.:0.00000      3rd Qu.: 0.00000          
 Max.   :16.00000       Max.   :15.00000   Max.   :8.00000      Max.   :17.00000          
                                                                                          
 municipal_police_wounded public_prosecutor_wounded organized_crime_wounded civilian_wounded 
 Min.   :0.0000           Min.   :0.000000          Min.   : 0.000          Min.   : 0.0000  
 1st Qu.:0.0000           1st Qu.:0.000000          1st Qu.: 0.000          1st Qu.: 0.0000  
 Median :0.0000           Median :0.000000          Median : 0.000          Median : 0.0000  
 Mean   :0.1483           Mean   :0.001297          Mean   : 0.283          Mean   : 0.1614  
 3rd Qu.:0.0000           3rd Qu.:0.000000          3rd Qu.: 0.000          3rd Qu.: 0.0000  
 Max.   :9.0000           Max.   :2.000000          Max.   :30.000          Max.   :27.0000  
                                                                                             
 long_guns_seized  small_arms_seized cartridge_sezied   clips_seized     vehicles_seized   
 Min.   :  0.000   Min.   : 0.0000   Min.   :    0.0   Min.   :   0.00   Min.   :  0.0000  
 1st Qu.:  0.000   1st Qu.: 0.0000   1st Qu.:    0.0   1st Qu.:   0.00   1st Qu.:  0.0000  
 Median :  0.000   Median : 0.0000   Median :    0.0   Median :   0.00   Median :  0.0000  
 Mean   :  1.669   Mean   : 0.5019   Mean   :  273.8   Mean   :  11.67   Mean   :  0.9861  
 3rd Qu.:  2.000   3rd Qu.: 0.0000   3rd Qu.:    0.0   3rd Qu.:   1.00   3rd Qu.:  1.0000  
 Max.   :144.000   Max.   :34.0000   Max.   :86365.0   Max.   :4000.00   Max.   :354.0000  
                                                                                           
      afi                army        federal_police   ministerial_police municipal_police
 Min.   :0.000000   Min.   :0.0000   Min.   :0.0000   Min.   :0.00000    Min.   :0.0000  
 1st Qu.:0.000000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000    1st Qu.:0.0000  
 Median :0.000000   Median :0.0000   Median :0.0000   Median :0.00000    Median :0.0000  
 Mean   :0.003151   Mean   :0.3436   Mean   :0.1105   Mean   :0.07895    Mean   :0.2298  
 3rd Qu.:0.000000   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:0.00000    3rd Qu.:0.0000  
 Max.   :1.000000   Max.   :1.0000   Max.   :1.0000   Max.   :1.00000    Max.   :1.0000  
                                                                                         
      navy             other         state_police        source         
 Min.   :0.00000   Min.   :0.0000   Min.   :0.00000   Length:5396       
 1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.00000   Class :character  
 Median :0.00000   Median :0.0000   Median :0.00000   Mode  :character  
 Mean   :0.03039   Mean   :0.0341   Mean   :0.08432                     
 3rd Qu.:0.00000   3rd Qu.:0.0000   3rd Qu.:0.00000                     
 Max.   :1.00000   Max.   :1.0000   Max.   :1.00000                     
                                                                        
 organized_crime_lethality army_lethality  navy_lethality federal_police_lethality
 Min.   :  0               Min.   :0.000   Min.   :  0    Min.   :  0             
 1st Qu.:  1               1st Qu.:0.000   1st Qu.:  0    1st Qu.:  0             
 Median :Inf               Median :0.000   Median :  0    Median :  0             
 Mean   :Inf               Mean   :  Inf   Mean   :Inf    Mean   :Inf             
 3rd Qu.:Inf               3rd Qu.:0.143   3rd Qu.:Inf    3rd Qu.:  1             
 Max.   :Inf               Max.   :  Inf   Max.   :Inf    Max.   :Inf             
 NA's   :3090              NA's   :5011    NA's   :5356   NA's   :5183            
 organized_crime_lethality_diff army_lethality_diff navy_lethality_diff
 Min.   :-18.0000               Min.   :-8.000      Min.   :-7.00000   
 1st Qu.:  0.0000               1st Qu.: 0.000      1st Qu.: 0.00000   
 Median :  0.0000               Median : 0.000      Median : 0.00000   
 Mean   :  0.5693               Mean   :-0.104      Mean   :-0.00871   
 3rd Qu.:  1.0000               3rd Qu.: 0.000      3rd Qu.: 0.00000   
 Max.   : 28.0000               Max.   : 3.000      Max.   : 3.00000   
                                                                       
 federal_police_lethality_diff organized_crime_NewIndex army_NewIndex    navy_NewIndex   
 Min.   :-14.00000             Min.   :-1.0000          Min.   :-1.000   Min.   :-1.000  
 1st Qu.:  0.00000             1st Qu.: 0.0000          1st Qu.:-1.000   1st Qu.:-1.000  
 Median :  0.00000             Median : 1.0000          Median :-1.000   Median :-1.000  
 Mean   : -0.05671             Mean   : 0.4636          Mean   :-0.687   Mean   :-0.365  
 3rd Qu.:  0.00000             3rd Qu.: 1.0000          3rd Qu.:-0.750   3rd Qu.: 1.000  
 Max.   :  5.00000             Max.   : 1.0000          Max.   : 1.000   Max.   : 1.000  
                               NA's   :3090             NA's   :5011     NA's   :5356    
 federal_police_NewIndex perfect_lethality   category           global_id   
 Min.   :-1.000          Min.   :0.0000    Length:5396        Min.   :   1  
 1st Qu.:-1.000          1st Qu.:0.0000    Class :character   1st Qu.:1350  
 Median :-1.000          Median :0.0000    Mode  :character   Median :2698  
 Mean   :-0.503          Mean   :0.2745                       Mean   :2698  
 3rd Qu.: 0.000          3rd Qu.:1.0000                       3rd Qu.:4047  
 Max.   : 1.000          Max.   :1.0000                       Max.   :5396  
 NA's   :5183                                                               
```


## Summary session information

```
R version 3.4.3 (2017-11-30)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS High Sierra 10.13.2

Matrix products: default
BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] bindrcpp_0.2    reshape2_1.4.3  stringr_1.2.0   lubridate_1.7.1 magrittr_1.5    tidyr_0.7.2    
 [7] dplyr_0.7.4     readxl_1.0.0    readr_1.1.1     here_0.1       

loaded via a namespace (and not attached):
 [1] Rcpp_0.12.14     rprojroot_1.3-1  assertthat_0.2.0 plyr_1.8.4       cellranger_1.1.0 R6_2.2.2        
 [7] backports_1.1.2  stringi_1.1.6    rlang_0.1.6      tools_3.4.3      glue_1.2.0       purrr_0.2.4     
[13] hms_0.4.0        yaml_2.1.16      rsconnect_0.8.5  compiler_3.4.3   pkgconfig_2.0.1  tidyselect_0.2.3
[19] bindr_0.1        tibble_1.3.4    
```
