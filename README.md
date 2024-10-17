# Tutorial for Spatial Autocorrelation Analysis in R 
This tutorial uses Global Moran’s I and Local Interpretation of Spatial Autocorrelation (LISA) testing methods to determine the spatial correlation between median total income and percent French speaking population in Kelowna, British Columbia. 

## Introduction
Spatial autocorrelation is a spatial analysis technique that allows us to determine whether an observation is distributed randomly (no correlation), clustered (positive correlation), or dispersed (negative correlation). Global Moran's I is a measures spatial association based on location and value simultaneousy (ESRI, 2024), giving us a generalized inferential statistic. Local Moran's I or LISA, compares values and locations within the context of all neighbouring values, providing a comprehensive statistic describing 'hotspots' and significant outliers (ESRI, 2024). Spatial Autocorrelation is preffered over other measures of spatial association as it accounts for Tobler's Law, which states that nearby observations are not independant, and more often than not will influence observations at neighbouring locations (source).

### Data 
The data used in this analysis was accesses from Statistics Canada Open Government Portal (2016), consisting of a shapefile containing the census boundaries of multiple Canadian cities in a multi-polygon format, and an excel spreadhseet (csv) file containing census data. Census data is most often used for spatial analysis as it provides data to work with on multiple spatial and temporal scales, making it useful for both autocorrelation and time-series analyses (source). By comparing the two census variables in used in this tutorial we look to gain a deeper understanding of the relationships between French culture and socioeconomic status in our study area. These relationships are important from a census context as they manifest in access to resources, public policy, policing, and locations of high density immigrant populations (Statistics Canada, 2014).

### Packages & Libraries

One of the best things about using R is that we can access a variety of open-source code packages to help us execute 
a multitude of tasks For example, the tmap package allows us to access functions to create maps that visualize our spatial data (Tennekes et. Al, 2023), spdep allows us to calculate spatial staistics (Bivand et.al, 2009), and the knitr package allows us to embed R code within a pdf, html, or word document output (Yihuei Xie, 2001-2024). With a better sense for how packages expand our capabilities in R, we can now begin by installing the necessary packages and calling them from our package library using the ‘library’ function. Whenever we install a new package in R it will be saved to our package library, so we must simply use the 'library' function to access them at anytime.

``` {r Libraries, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
install.packages("st")
install.packages("knitr"
install.packages("tmap")
install.packages("spdep")
install.packages("raster")
install.packages("e1071")
install.packages("shinyjs")

#Load in libraries:
library("st")
library("knitr")
library("tmap")
library("spdep")
library("raster")
library("e1071")
library("shinyjs")
```

## Analysis
### Step 1: Creating a working directory

Before starting our spatial analysis, we need to create a working directory to pull our data from. The working directory is the folder containing the census and shapefiles we downloaded onto our desktop. The assignment operator <- is used to assign the file path to our data to the object ‘dir’, which tells R where to access and read the data. To set the working directory we use the function setwd(dir). At any point, we can verify the current working directory using the function get(wd).

```{r working directory, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
dir <- "C:/Users/Eve/OneDrive/Assignment 3"
setwd(dir)
get(wd)
```


### Step 2: Read-in Datasets

This first step in preparing our spatial and census data for analysis is by ‘reading in’ the csv and shapefile necessary for our spatial autocorrelation analysis. These files represent the census data (csv) and study area (shp) used in the analyses. The process of bringing these datasets in R allows us to create two data frames to work within. We can check to see if we’ve successfully loaded these datasets into R by looking at the Global Environment Pane on the top right corner of the R window.  

```{r Read in data, echo=TRUE, eval=TRUE, warning=FALSE}
#From the working dir read in the census data (csv.)
csv <- read.csv("C:/Users/Eve/OneDrive/Assignment 3/ucgsJQnBVLvP_data.csv") 
#Read in shapefile
shp <- st_read("C:/Users/Eve/OneDrive/Assignment 3/lda_000b16a_e.shp") 
```
### Step 3: Data cleanup

Next, we’ll want to clean up the census data to make it easier to work with. This is done by renaming the columns so that we know what attributes they refer to and applying the new column names to the census data frame. This is a necessary step as the census data downloaded for this analysis does not contain specific column names. GEO UID stands for geographic unique identifier, which is a code assigned to the geographic areas in our census data set. The shapefile equivalent of GEO UID is the DA UID (dissemination area unique identifier), which in our dataset consists of 8 characters. To maintain consistency between the two geographic identifiers we must remove any ID’s with less than 8 characters from our census data frame. This process, and the process for renaming the csv columns is shown in the code below: 

```{r change columns, echo=TRUE, eval=TRUE, warning=FALSE}
#New column names
cols <- c("GEO UID", "Province code", "Province name", "CD code",
        "CD name", "DA name", "Population", "Land area", 
        "Median total income", "Income Sample Size", "French Knowledge", 
        "Language Sample Size")

#Apply those names to dataframe
colnames(csv) <- cols

#Add column to count number of GE0 UID charactors
csv$len <- nchar(csv$`GEO UID`)

#Remove GEO UIDs with less than 8 numbers
csv_clean <- subset(csv, csv$len == 8)

```
### Step 4: Merge spatial and aspatial data

The relationship between GEOUID and DAUID is shown in the code below, where we will use the merge function to combine our aspatial (census data) and spatial (shape file) together into one data frame called "census_DAs".

```{r merge data, echo=TRUE, eval=TRUE, warning=FALSE}
#Merge spatial and aspatial data
census_DAs <- merge(shp, csv_clean, 
                    by.x = "DAUID", 
                    by.y = "GEO UID", 
                    all.x = TRUE)
```
### Step 5: Subset variables to the study area and remove NA values

Next we will want to subset our data to include only the census data applicable to the study area, which in this case is Kelowna. This process produces a new data frame containing the subset data called "Municp". Then we will create a new variable called 'PercFrench' in our Municp data frame based on the rate between french knowledge and language sample size, as shown in the code below. Also, to maintain accuracy in our results, any missing data in the form of 0 or NA values must be removed. This will make sure that the polygons we are interested in will contain values for our variables of interest.  

```{r subset data, echo=TRUE, eval=TRUE, warning=FALSE}
#Subset for Vancouver
Municp <- subset(census_DAs, census_DAs$CMANAME == "Kelowna")

#Convert to rate
Municp$PercFrench <- (Municp$`French Knowledge`/Municp$`Language Sample Size`)*100
```
```{r NA Remove, echo=TRUE, eval=TRUE, warning=FALSE}
#Remove Income NA
Income_noNA <- Municp[which(!is.na(Municp$`Median total income`)),]

#Remove French NA
French_noNA <- Municp[which(!is.na(Municp$PercFrench)),]
```

### Step 6: Calculate descriptive statistics

Next, we will take a closer look at the two variables we are interested in: Median total income and Percentage of respondents with French language knowledge. We will look at some descriptive stats and do a final check for NA values in the data.

```{r DescriptiveStats, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate descriptive stats for Income
meanIncome <- mean(Income_noNA$`Median total income`)
stdevIncome <- sd(Income_noNA$`Median total income`)
skewIncome <- skewness(Income_noNA$`Median total income`)

#Calculate descriptive stats for French
meanFrench <- mean(French_noNA$PercFrench)
stdevFrench <- sd(French_noNA$PercFrench)
skewFrench <- skewness(French_noNA$PercFrench)

#Create dataframe for display in table
data <- data.frame(Variable = c("Income", "French Language"),
                   Mean = c(round(meanIncome,2), round(meanFrench,2)),
                   Std.Dev = c(round(stdevIncome,2), round(stdevFrench,2)),
                   Skewness = c(round(skewIncome,2), round(skewFrench,2)))
#Print table 
kable(data, caption = paste0("Descriptive statistics for selected ", 2016, " census variables"))
```
![1](https://github.com/user-attachments/assets/4c95be6d-4efb-4ded-bbd0-4cd3ecf0a33e)


### Step 7: Map variables of interest

The first step in mapping our variables of interest is by creating two ‘map objects’ called map_Income, and map_French. We use the 'tmap' package to access our mapping functions. For example, tm_shape to tell R what variable we are mapping and the function tm_polygons to customize the parameters for our map design elements. Next, we will print the maps side by side for comparison, using the function ‘tmap_arrange’. The code and output maps for median total income and percentage of population with French knowledge are shown below: 

```{r StudyArea, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="City of Vancouver census dissemination areas showing median total income (left) and percentage of respondants with knowledge of french (right)."}

#Map median Income
map_Income <- tm_shape(Income_noNA) + 
  tm_polygons(col = "Median total income", 
              title = "Median total income", 
              style = "jenks", 
              palette = "PuBu", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Map French Knowledge
map_French <- tm_shape(French_noNA) + 
  tm_polygons(col = "PercFrench", 
              title = "Percentage with \n French Knowledge", 
              style = "jenks", 
              palette = "RdPu", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Print maps side by side
tmap_arrange(map_Income, map_French, ncol = 2, nrow = 1)

```
<img width="1000" alt="Rplot02" src="https://github.com/user-attachments/assets/2048fd60-6fb6-4f5d-9dec-06d819d9f199">

### Step 8: Calculate Queen and Rook weights

```{r Neighbours, echo=TRUE, eval=TRUE, warning=FALSE}
#Income Neighbours - Queens weight
Income.nb <- poly2nb(Income_noNA)
Income.net <- nb2lines(Income.nb, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net) <- crs(Income_noNA)

#Income Neighbours - Rooks weight
Income.nb2 <- poly2nb(Income_noNA, queen = FALSE)
Income.net2 <- nb2lines(Income.nb2, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net2) <- crs(Income_noNA)
```

```{r Neighbours, echo=TRUE, eval=TRUE, warning=FALSE}
#French Neighbours - Queens weight
French.nb <- poly2nb(French_noNA)
French.net <- nb2lines(French.nb, coords=st_coordinates(st_centroid(French_noNA)))
crs(French.net) <- crs(French_noNA)

#French Neighbours - Rooks weight
French.nb2 <- poly2nb(French_noNA, queen = FALSE)
French.net2 <- nb2lines(French.nb2, coords=st_coordinates(st_centroid(French_noNA)))
crs(French.net2) <- crs(French_noNA)
```
### Step 9: Mapping weighted neighbourhoods

```{r Neighboursmap, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Kelowna census dissemination areas showing median total income neighbours queens weight (left)  rooks weight (middle) and the combination of the two (right)."}
#Make queens map
IncomeQueen <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(Income.net) + tm_lines(col='blue')

#Make rooks map
IncomeRook <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(Income.net2) + tm_lines(col='red', lwd = 2)

#Make combined map
IncomeBoth <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
               tm_shape(Income.net) + tm_lines(col='blue', lwd = 2) +
               tm_shape(Income.net2) + tm_lines(col='red', lwd = 2)

#Print maps in a three pane figure
tmap_arrange(IncomeQueen, IncomeRook, IncomeBoth, ncol = 3, nrow = 1)
```
<img width="1000" alt="Rplot04" src="https://github.com/user-attachments/assets/39771e49-ba9d-428f-a578-229e5975573d">

We'll do the same thing for french knowlege speakers

``` {r Neighboursmap2, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Kelowna census dissemination areas showing percent french knowledge speakers queens weight (left)  rooks weight (middle) and the combination of the two (right)."}
#Make queens map french
FrenchQueen <- tm_shape(French_noNA) + tm_borders(col='lightgrey') + 
  tm_shape(French.net) + tm_lines(col='blue')

#Make rook map
FrenchRook <- tm_shape(French_noNA) + tm_borders(col='lightgrey') + 
  tm_shape(French.net2) + tm_lines(col='blue')

#Make combined French map
FrenchBoth <- tm_shape(French_noNA) + tm_borders(col='lightgrey') + 
  tm_shape(French.net) + tm_lines(col='blue', lwd = 2) +
  tm_shape(French.net2) + tm_lines(col='red', lwd = 2)

#Print maps in a three pane figure
tmap_arrange(FrenchQueen, FrenchRook, FrenchBoth, ncol = 3, nrow = 1)
```
<img width="1000" alt="Rplot03" src="https://github.com/user-attachments/assets/698148d9-2cb0-4378-b8d7-1be7cfd032c4">

### Step 10: Create a weights matrix
Describe the code for the weighted matrix file.
By creating a weighted neighbourhood matrix we are essentially finding a deviation fron from the mean

Weights are defined by “style” (ie. type), and can include “B”, “W”, and “C”. The B weights matrix is the most basic of the three, as it employs a binary weighting scheme, whereby each neighbour is given a weight of 1, and all other polygons are given a weight of 0 (see figures above). A W weights matrix employs a row standardized weighting scheme, with each neighbour given equal weights that sum to 1 [11]. Comparatively, a C weights matrix is a globally standardized method of weighting, with all neighbours given equal weight across the entire study area [13].

Creating a weights matrix in R uses the “nb2listw” function from the “spdep” library. We can apply this function to the vri.nb variable created above, as it contains all of the neighbour links to which we want to assign weights. Additionally, if there are any polygons in our file with zero neighbour links, we still want the program to run. Therefore, we define “zero.policy” as equal to “TRUE”, which assigns weights vectors of zero length for regions with no neighbours [13]. Subsequently, we can print off our list of weights matrices (“print.listw”) in order to assess the distribution of weights for each observation (i) and its neighbours (j). The example of code below is using a weights matrix of type W. You can read more about the different styles of spatial weighting [here](https://r-spatial.github.io/spdep/reference/nb2listw.html

```{r Final weights, echo=TRUE, eval=TRUE, warning=FALSE}.
#Create Income weights matrix
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "W")

#Create French weights matrix
French.lw <- nb2listw(French.nb, zero.policy = TRUE, style = "W")

#subset
subset_weights<-head(Income.lw[["weights"]])[c(1:3)]
print(subset_weights)

```
# Spatial Autocorrelation Statistics

## Global Moran’s I

Now that we have determined how to choose and weight our neighbours, we can calculate the Global Moran’s I statistic. This method of testing for spatial autocorrelation looks across the entire study area for every location simultaneously [14]. The equation for this statistic is

$$
I = \frac{\sum_{i=1}^n\sum_{j=1}^nW_{i,j}(x_i - \bar{x})(x_j - \bar{x})}{(\sum_{i=1}^n\sum_{j=1}^nW_{i,j})\sum_{i=1}^n(x_i - \bar{x})^2}
$$

Here, if $x$ is the variable being assessed, $x_i$ is the variable value at a point of interest (i) and $x_j$ represents a neighbour to $x_i$ (here determined by the queen weighting scheme). The spatial weighting applied to the weighting matrix $W_{i,j}$ is multiplied by both the differences of $x_i$ and the mean value of variable $x$, and $x_j$ and the mean value of variable $x$.

The denominator in this case is used to standardize our values, and therefore relatively high values of I correspond with positive spatial autocorrelation, and relatively low values of I correspond with negative spatial autocorrelation. Remember that the global Moran’s I statistic provides an indication of how spatially autocorrelated our data is over the entire dataset, thus representing a spatial pattern at the global scale [15].


```{r Global Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate Global Moran's I for Income
miIncome <- moran.test(Income_noNA$`Median total income`, Income.lw, zero.policy = TRUE)

#Extract Global Moran's I results for Income
mIIncome <- miIncome$estimate[[1]]
eIIncome <- miIncome$estimate[[2]]
varIncome <- miIncome$estimate[[3]]

#Calculate Global Moran's I for French
miFrench <- moran.test(French_noNA$PercFrench, French.lw, zero.policy = TRUE)

#Extract Global Moran's I results for French
mIFrench <- miFrench$estimate[[1]]
eIFrench <- miFrench$estimate[[2]]
varFrench <- miFrench$estimate[[3]]
```


Describe the results.


```{r Global Morans Range, echo=TRUE, eval=TRUE, warning=FALSE}
#Function to calculate the range of global Moran's I
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}

#Calculate the range for the Income variable
range <- moran.range(Income.lw)
minRange <- range[1]
maxRange <- range[2]
```

Describe what the results indicate.

However, we can still go a step further and figure out whether these patterns are statistically significant. To do so, we can use a Z-test. Here our null hypothesis is ?, and the alternate hypothesis is ?. Using an $\alpha$ value of 0.05, if our Z-score falls above or below 1.96, we can say ?. A value greater than +1.96 would imply ?, and a value less than -1.96 would imply ?.

We can calculate a Z-test using the following code:

```{r Global Morans ZScore, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate z-test for Income
zIncome <- (mIIncome - eIIncome) / (sqrt(varIncome))

#Calculate z-test for French
zFrench <- (mIFrench - eIFrench) / (sqrt(varFrench))
```

The zscores for both variable confirm that ?

## Local spatial autocorrelation

Explain local spatial autocorrelation

The calculation for Local Moran’s I has many of the same features as our global calculation, although arranged in a different way.

$$
I_i = \frac{x_i - \bar{x}}{S_i^2}\sum{_{j=1}^n}W_{i,j}(x_j - \bar{x})\space \space where \space \space S_i^2 = \frac{\sum_{i=1}^n (x_i - \bar{x})^2}{n-1} 
$$

Again, instead of typing out these calculations, we can use the localmoran() function to deal with all of the messy calculations for us, as long as we input our variable and weighting scheme.


```{r Local Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate LISA test for Income
lisa.testIncome <- localmoran(Income_noNA$`Median total income`, Income.lw)

#Extract LISA test results for Income
Income_noNA$Ii <- lisa.testIncome[,1]
Income_noNA$E.Ii<- lisa.testIncome[,2]
Income_noNA$Var.Ii<- lisa.testIncome[,3]
Income_noNA$Z.Ii<- lisa.testIncome[,4]
Income_noNA$P<- lisa.testIncome[,5]

#Calculate LISA test for Income
lisa.testFrench <- localmoran(French_noNA$PercFrench, French.lw)

#Extract LISA test results for Income
French_noNA$Ii <- lisa.testFrench [,1]
French_noNA$E.Ii<- lisa.testFrench [,2]
French_noNA$Var.Ii<- lisa.testFrench [,3]
French_noNA$Z.Ii<- lisa.testFrench [,4]
French_noNA$P<- lisa.testFrench [,5]
```


Now going back to our basic mapping template we can visualize some of these results to understand what this test is doing.


```{r MappingLocalMoransI, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Kamloops census dissemination areas showing LISA z-scores for median total income (left) and percentage of respondants with knowledge of french (right)."}
#Map LISA z-scores for Income
map_LISA_Income <- tm_shape(Income_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(Income_noNA$Z.Ii),-1.96,1.96,max(Income_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Map LISA z-scores for French
map_LISA_French <- tm_shape(French_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(French_noNA$Z.Ii),-1.96,1.96,max(French_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Plot maps in a 2 pane figure
tmap_arrange(map_LISA_Income, map_LISA_French, ncol = 2, nrow = 1)
```
![Rplot05](https://github.com/user-attachments/assets/1f0840c1-7bca-4874-80da-3704d3cf244c)

Explain the results.


While these maps are great for visualizing where the data is and getting a rough idea of how many polygons are significantly positively or negatively spatially autocorrelated, it can be even more informative to graph these trends.

```{r MoransIScatter, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for median total income."}
#Create Moran's I scatter plot for Income
moran.plot(Income_noNA$`Median total income`, Income.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Median Total Income ($)", 
           ylab="Spatially Lagged Median Total Income ($)", quiet=NULL)
```


```{r MoransIScatter2, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for percentage of respondants with knowledge of french."}
#Create Moran's I scatter plot for French
moran.plot(French_noNA$PercFrench, French.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Respondants with knowledge of French (%)", 
           ylab="Spatially Lagged knowledge of French (%)", quiet=NULL)
```


In these plots, the points with diamonds are considered statistically significant, and the regression line shows the overall trend. For both plots we can see that the trend shows?




## Summary

Provide a brief summary.

## References
