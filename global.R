library(fpp3)
library(tidyverse)
library(tsibble)
library(shinydashboard)
library(plotly)
library(shiny)
library(shinyWidgets)
library(shinythemes)

data("aus_production")

aus_production <- na.omit(aus_production)