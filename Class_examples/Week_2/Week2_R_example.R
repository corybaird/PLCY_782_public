# Search for R-Packages

# Resource 1
# https://www.r-pkg.org/

# Resource 2
# https://cran.r-project.org/web/packages/available_packages_by_name.html

# Resource 3
# https://rdrr.io/find/?repos=cran%2Cbioc%2Crforge%2Cgithub&page=0&fuzzy_slug=

# 1. DPLYR
#Step 1
install.packages('dplyr')
#Step 2
library(dplyr)

# 1. Italy: Italian household survey data
install.packages("italy")
library(italy)

## 1.1 Check information on the package
??italy

## 1.2 Inspect data
italy08 %>% 
  head(10)

italy10 %>% 
  head(10)

# 2. Pakistan household survey

## 2.1 Install
install.packages('PakPMICS2018hh')
library(PakPMICS2018hh)

## 2.2 Package details
??PakPMICS2018hh

## 2.3 Load package
PakPMICS2018hh


# 3. Povcalnet
# Examples https://rdrr.io/cran/povcalnetR/f/vignettes/povcalnetR.Rmd
install.packages('povcalnetR')
library(povcalnetR)

china_df = povcalnet(country = c("CHN"))

# 4. Search github!

