# Spatial data visualization

This is a workshop written for UCSB MEDS Winter 2023. The rendered template document is [here](https://an-bui.github.io/interactive-data-vis/code/interactive-data-vis.html).

# Libraries

You will need to have the following libraries installed:

```
# general use
library(here) # file organization
library(tidyverse) # manipulating
library(sf) # reading in spatial data, etc.
library(janitor) # cleaning variable names
library(lterdatasampler) # data source
library(randomcoloR) # random color generator

# Javascript package wrappers
library(leaflet) # interactive map
library(plotly) # interactive plots
library(ggiraph) # more interactive plots
library(echarts4r) # even more interactive plots
```

## Caveats

If you want to use custom icons and have the legends disappear as you click group layers, that only works in a Shiny app per [this Stack Overflow post](https://stackoverflow.com/questions/50373497/r-leaflet-show-hide-addcontrol-element-with-group-layers).
