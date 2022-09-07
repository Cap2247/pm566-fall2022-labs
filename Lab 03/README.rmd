---
title: "Lab 03"
author: "CP"
date: "`r Sys.Date()`"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
1. Read in the data
First download and then read in with data.table:fread()

```{r}
if (!file.exists("met_all.gz"))
download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz", "met_all.gz", method="libcurl", timeout = 60)
met <- data.table::fread("met_all.gz")

```
## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```


## 2. Check the dimensions, headers, footers. How many columns, rows are there?

```{r}
dim(met)
head(met)
tail(met)

```
There are 2,377,343 rows and 30 columns in the met dataset.

3. Take a look at the variables.
```{r}
str(met)
```

4. Take a closer look at the key variables.

```{r}
table(met$day)
```
```{r}
table(met$year)
```
```{r}
table(met$hour)
```
```{r}
summary(met$temp)
summary(met$elev)
summary(met$wind.sp)
```

Replace elevations with 9999 as NA.

```{r}
met[met$elev==9999.0] <- NA
summary(met$elev)
```

The weather station with the highest elevation is at `r max(met$elev,na.rm=TRUE)`


Remove temps below -40 Celsius

```{r}
met <- met[temp>-40]
met2 <- met[order(temp)]
head(met2)
```


5. Check the data against an external data source.

```{r}
met <- met[temp>-15][order(temp)]
```

We removed temperatures colder than -15C. The new dataset has minimum temp -3C which is reasonable

6. Calculate summary statistics

```{r}
elev <- met[elev==max(elev)]
summary(elev)
```


```{r}
cor(elev$temp, elev$wind.sp, use="complete")
cor(elev$temp, elev$hour, use="complete")
cor(elev$wind.sp, elev$day, use="complete")
cor(elev$wind.sp, elev$hour, use="complete")
cor(elev$temp, elev$day, use="complete")
```



```

7. Exploratory graphs



```{r}
hist(met$temp)
hist(met$elev, breaks=100)
hist(met$wind.sp)
```




