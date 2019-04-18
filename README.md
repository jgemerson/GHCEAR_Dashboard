# Global Health CEA Registry Dashboard Shiny App

## Background
The [Global Health Cost-Effectiveness Analysis (GH CEA) Registry](http://healtheconomics.tuftsmedicalcenter.org/orchard) is a database of all English-language cost-per-DALY averted cost-utility analyses published from 1995 to present. The GH CEA Registry is maintained and updated by Tufts Medical Center's [Center for the Evaluation of Value and Risk in Health (CEVR)](http://cevr.tuftsmedicalcenter.org/) and funded by the [Bill and Melinda Gates Foundation](https://www.gatesfoundation.org/).

This R Shiny App serves as an analytic tool and data visualization creator for the database to help researchers and policymakers access evidence for cost-effective health interventions primarily focusing on low- and middle-income (LMIC) settings. It integrates both reactive R Shiny features as well as R Markdown functionality for downloadable reports.


## Links and Access
### [Dashboard](https://cevr.shinyapps.io/DashboardShinyApp/)

The GH CEA Registry data are open-access, available on the [GH CEA Registry website](http://healtheconomics.tuftsmedicalcenter.org/orchard/download-dataset) and the [search feature itself](https://cevr.shinyapps.io/LeagueTables/).

## Code structure
Built with [R Shiny](https://shiny.rstudio.com/) and [R Markdown](https://rmarkdown.rstudio.com/) via [R Studio IDE](https://www.rstudio.com/).

Packages used:
 - [Shiny Dashboard](https://rstudio.github.io/shinydashboard/)
 - [ggplot2](https://ggplot2.tidyverse.org/)
 - [plotly](https://plot.ly/r/)
 - [treemapify](https://www.rdocumentation.org/packages/treemapify/versions/0.2.1)
 - [XL connect](https://www.rdocumentation.org/packages/XLConnect/versions/0.2-15)
 - [DT](https://rstudio.github.io/DT/)
 - [dyplr](https://www.rdocumentation.org/packages/dplyr/versions/0.7.8)

## Version
Current verion: 1.0.11. Last updated April 18, 2019.

## Screenshots
![](https://github.com/jgemerson/GHCEAR_Dashboard/blob/master/Dashboard%20for%20github/Screenshots/full.JPG)
_________________
![](https://github.com/jgemerson/GHCEAR_Dashboard/blob/master/Dashboard%20for%20github/Screenshots/Tabs.JPG)
_________________
![](https://github.com/jgemerson/GHCEAR_Dashboard/blob/master/Dashboard%20for%20github/Screenshots/Map.JPG)
_________________
![](https://github.com/jgemerson/GHCEAR_Dashboard/blob/master/Dashboard%20for%20github/Screenshots/Pie%20and%20treemap.png)
_________________
![](https://github.com/jgemerson/GHCEAR_Dashboard/tree/master/Dashboard%20for%20github/Screenshots/Reportdownload1.JPG)
_________________
![](https://github.com/jgemerson/GHCEAR_Dashboard/blob/master/Dashboard%20for%20github/Screenshots/Report%20download1.JPG)
_________________
![](https://github.com/jgemerson/GHCEAR_Dashboard/blob/master/Dashboard%20for%20github/Screenshots/Report%20download2.JPG)
_________________
![](https://github.com/jgemerson/GHCEAR_Dashboard/blob/master/Dashboard%20for%20github/Screenshots/Dropdown.png)
_________________
![](https://github.com/jgemerson/GHCEAR_Dashboard/blob/master/Dashboard%20for%20github/Screenshots/Stacked%20bar%20and%20bar.JPG)


 
## Authors and Acknowledgement
Funded by the [The Bill and Melinda Gates Foundation](https://www.gatesfoundation.org/) and supported by [CEVR](http://cevr.tuftsmedicalcenter.org/). 

Developed by Joanna Emerson, with the support from Brittany D'Cruz, Peter J. Neumann, Josh T. Cohen, David Kim, Rachel Bacon, and Ari Panzer.  

<img src="https://pbs.twimg.com/profile_images/958789469632516096/hUT1dpXt.jpg" width="150" height="150"> <img src="https://datadent.org/wp-content/uploads/2018/04/arton10.jpg" width="400" height="100">
