

## library() calls go here
library(conflicted)
library(dotenv)
library(targets)
library(tarchetypes)
## data cleaning
library(biomformat)
library(janitor)
library(glue)
library(rsample)
## data analysis
library(tidyverse)
library(tidymodels)
library(ranger)
library(magrittr)
library(pROC)
library(Matrix)
library(xgboost)
library(ranger)
library(predomics)
library(gt)
library(gtsummary)
#phylogenetic tree tools
library(ape)
library(phytools)
library(vegan)

conflict_prefer("filter",    "dplyr")
conflict_prefer("select",    "dplyr")
conflict_prefer("slice",     "dplyr")
conflict_prefer('summarise', 'dplyr')
conflict_prefer('summarize', 'dplyr')
conflict_prefer('select',    'dplyr')
conflict_prefer("map", "purrr")
conflict_prefer("mutate", "dplyr")
