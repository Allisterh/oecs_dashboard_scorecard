# library(packrat)
# packrat::set_opts(local.repos = "./packrat/")
# library(renv)
# 
# renv::init()

options(scipen=999, shiny.usecairo=T)
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(bslib))
suppressPackageStartupMessages(library(bsicons))
suppressPackageStartupMessages(library(thematic))
suppressPackageStartupMessages(library(waiter))
suppressPackageStartupMessages(library(cicerone))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(spsComps))
suppressPackageStartupMessages(library(bsplus))
suppressPackageStartupMessages(library(prompter))
suppressPackageStartupMessages(library(shinybusy))
suppressPackageStartupMessages(library(shinyalert))
suppressPackageStartupMessages(library(gfonts))
suppressPackageStartupMessages(library(fontawesome))
suppressPackageStartupMessages(library(r2social))
suppressPackageStartupMessages(library(gotop))


suppressPackageStartupMessages(library(Cairo))
suppressPackageStartupMessages(library(countrycode))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggtext))
suppressPackageStartupMessages(library(formattable))
suppressPackageStartupMessages(library(sparkline))
suppressPackageStartupMessages(library(ggrepel))

library(devtools)
library(zoo)

# remotes::install_github('daattali/shinytip') 

suppressPackageStartupMessages(library(shinytip))
# ggflags not installed
# install.packages("ggflags", repos = c(
#  "https://jimjam-slam.r-universe.dev",
#  "https://cloud.r-project.org"))

# devtools::install_github("jimjam-slam/ggflags")
# if (!require("remotes")) install.packages("remotes")
# remotes::install_github("jimjam-slam/ggflags", dependencies = TRUE)
suppressPackageStartupMessages(library(ggflags))


# suppressPackageStartupMessages(library(shinycssloaders))
# suppressPackageStartupMessages(library(shinyglide))
# suppressPackageStartupMessages(library(shinythemes))



# https://stackoverflow.com/questions/75664021/bslib-change-only-navbar-color
#my additions to the dashboard




