Findings
================
Kojo Manu
2024-02-29

``` r
library(pacman)
p_load(tidyr, dplyr, ggplot2, sf, ggspatial, tigris, scales)
```

``` r
getwd()
```

    ## [1] "/Users/kojomanu/Desktop/UO/Oregon Classes/EC 418-419/Markdown Files"

``` r
Census_IncomeF <- read.csv("/Users/kojomanu/Desktop/UO/Oregon Classes/EC 418-419/Data Sets/Cleaned Data Sets/Census_IncomeF.csv")

# Extract median income data
median_income <- as.numeric(Census_IncomeF$Estimate..Households..Median.income..dollars.)
```

    ## Warning: NAs introduced by coercion

``` r
# Calculate quintiles
quintiles <- quantile(median_income, probs = seq(0, 1, by = 0.2), na.rm = T)

# Create new column for quintiles
Census_IncomeF$income_quintile <- cut(median_income, breaks = quintiles, labels = FALSE, include.lowest = TRUE)

# Create table of census tract, income quitile, and county
tract_quintile_table <- cbind(Census_IncomeF$County, Census_IncomeF$X2020.Census.Tract, Census_IncomeF$income_quintile, median_income)

# Name each column
colnames(tract_quintile_table) <- c("County", "X2020.Census.Tract", "Income Quintile", "Median Income")
```

``` r
WA_EV_data <- read.csv("/Users/kojomanu/Desktop/UO/Oregon Classes/EC 418-419/Data Sets/Cleaned Data Sets/WA_EV_data.csv")

# Merging EV data with quintile data
EV_Q <- merge(WA_EV_data, tract_quintile_table, by = c("X2020.Census.Tract", "County"))

# Aggregate federal tax credit by income quintile
subsidy_by_quintile <- aggregate(EV_Q$Credit.Amount ~ EV_Q$"Income Quintile", FUN = sum)

# Renaming columns
colnames(subsidy_by_quintile) <- c("Income Quintile", "Credit Amount")

subsidy_by_quintile
```

    ##   Income Quintile Credit Amount
    ## 1               1      59041489
    ## 2               2     111175630
    ## 3               3     142831069
    ## 4               4     234788002
    ## 5               5     550161396

``` r
sum(subsidy_by_quintile$"Credit Amount")
```

    ## [1] 1097997586

``` r
sum_quint_1t4 <- subsidy_by_quintile %>%
  filter(`Income Quintile` %in% c("1", "2", "3", "4")) %>%
  summarize(total_credit = sum(`Credit Amount`))

sum_quint_1t4
```

    ##   total_credit
    ## 1    547836190

Making a map of WA to show variables by census tract

``` r
# Using this one so we can match it to the shapefile by FIPS code
WA_FIPS <- read.csv("/Users/kojomanu/Desktop/UO/Oregon Classes/EC 418-419/Data Sets/Cleaned Data Sets/WA_FIPS.csv")

# Aggregate data by census tract
ev_by_tract <- aggregate(. ~ X2020.Census.Tract, data = WA_FIPS, FUN = length)
names(ev_by_tract)[2] <- "EV_Count"

# Remove all but the first two columns
ev_by_tract <- ev_by_tract[, 1:2]

# Washington census tract map
WA_tracts <- tracts(state = "WA", year = 2020)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |==                                                                    |   4%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |=====                                                                 |   8%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |=======                                                               |  11%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |==============                                                        |  21%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  22%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |=====================                                                 |  31%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  39%  |                                                                              |============================                                          |  40%  |                                                                              |============================                                          |  41%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |==============================                                        |  44%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |===================================                                   |  51%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |=====================================                                 |  54%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |========================================                              |  58%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |==========================================                            |  61%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  76%  |                                                                              |======================================================                |  77%  |                                                                              |======================================================                |  78%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  79%  |                                                                              |========================================================              |  80%  |                                                                              |========================================================              |  81%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |==========================================================            |  84%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |===============================================================       |  91%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |=================================================================     |  94%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |====================================================================  |  98%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================|  99%  |                                                                              |======================================================================| 100%

``` r
# Rename the census tract column
names(WA_tracts)[names(WA_tracts) == "GEOID"] <- "X2020.Census.Tract"

# Adding EV data to the WA map
EV_map_data <- merge(WA_tracts, ev_by_tract, by.x = "X2020.Census.Tract", by.y = "X2020.Census.Tract", all.x = T)

# Map showing the # of EVs by tract. Logging the scale because all of the lower tracts have too similar of numbers
EV_map <- ggplot() +
            geom_sf(data = EV_map_data, aes(fill = EV_Count), color = NA) +
            scale_fill_viridis_c(trans = "log", labels = comma) +
            labs(fill = "EV Count") +
            theme_minimal()

EV_map
```

![](Findings_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# Saving the image
ggsave("EV_map.png", plot = EV_map)
```

    ## Saving 7 x 5 in image

``` r
# Loading Income Data
Census_IncomeF <- read.csv("/Users/kojomanu/Desktop/UO/Oregon Classes/EC 418-419/Data Sets/Cleaned Data Sets/Census_IncomeF.csv")

# Formatting census tract column
Census_IncomeF$Geography <- substr(Census_IncomeF$Geography, nchar(Census_IncomeF$Geography) - 10, nchar(Census_IncomeF$Geography))

# Specific columns
Tract_Income <- Census_IncomeF[, c("Estimate..Households..Median.income..dollars.", "Geography")]

# Renaming columns
names(Tract_Income)[names(Tract_Income) == "Estimate..Households..Median.income..dollars."] <- "Median_Income"
names(Tract_Income)[names(Tract_Income) == "Geography"] <- "X2020.Census.Tract"

# Merginf with map data
EV_map_data <- merge(EV_map_data, Tract_Income, by.x = "X2020.Census.Tract", by.y = "X2020.Census.Tract", all.x = T)

EV_map_data$Median_Income <- as.numeric(EV_map_data$Median_Income)
```

    ## Warning: NAs introduced by coercion

``` r
# Map of income by tract
Income_map <- ggplot() +
            geom_sf(data = EV_map_data, aes(fill = Median_Income), color = NA) +
            scale_fill_viridis_c() +
            labs(fill = "Median Income") +
            theme_minimal()

Income_map
```

![](Findings_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
# Saving Image
ggsave("Income_map.png", plot = Income_map)
```

    ## Saving 7 x 5 in image

Using EV Ration instead of EV Count for the map

``` r
county_code <- read.csv("/Users/kojomanu/Desktop/UO/Oregon Classes/EC 418-419/Data Sets/Cleaned Data Sets/geographic_codes.csv") 
county_code <- county_code[, 1:2]

EV_Ratio <- read.csv("/Users/kojomanu/Desktop/UO/Oregon Classes/EC 418-419/Data Sets/Cleaned Data Sets/kojo_data3.csv")

# Removing unnecessary data
EV_Ratio <- EV_Ratio[c("Census_Tract", "EVRatio", "County")]
EV_Ratio <- EV_Ratio[-(1:59), ]

EV_Ratio <- merge(EV_Ratio, county_code, by.x = c("County"), by.y = c("COUNTY_NAME"))
EV_Ratio$Census_Tract <- EV_Ratio$Census_Tract * 100

EV_Ratio$Census_Tract <- as.numeric(as.character(EV_Ratio$Census_Tract))
EV_Ratio$Census_Tract <- sprintf("%06d", EV_Ratio$Census_Tract)

EV_Ratio$COUNTYFP <- as.numeric(as.character(EV_Ratio$COUNTYFP))
EV_Ratio$COUNTYFP <- sprintf("%03d", EV_Ratio$COUNTYFP)

EV_Ratio$X2020.Census.Tract <- paste(EV_Ratio$COUNTYFP, EV_Ratio$Census_Tract, sep = "")

EV_Ratio$X2020.Census.Tract <- paste("53", EV_Ratio$X2020.Census.Tract, sep = "")

EV_Ratio <- EV_Ratio[c("X2020.Census.Tract", "EVRatio")]

EV_Ratio$X2020.Census.Tract <- as.numeric(EV_Ratio$X2020.Census.Tract)
```

    ## Warning: NAs introduced by coercion

``` r
EV_Ratio <- EV_Ratio %>%
  group_by(X2020.Census.Tract) %>%
  summarize(EVRatio = mean(EVRatio, na.rm = TRUE))

EV_Ratio$EVRatio <- EV_Ratio$EVRatio * 100

EV_map_data <- merge(EV_map_data, EV_Ratio, by.x = "X2020.Census.Tract", by.y = "X2020.Census.Tract", all.x = T)

EV_map_data$EVRatio <- as.numeric(EV_map_data$EVRatio)

EVRatio_map <- ggplot() +
            geom_sf(data = EV_map_data, aes(fill = EVRatio), color = NA) +
            scale_fill_viridis_c() +
            labs(fill = "EV Ratio (%)") +
            theme_minimal()

EVRatio_map
```

![](Findings_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ggsave("EVRatio_map.png", plot = EVRatio_map)
```

    ## Saving 7 x 5 in image
