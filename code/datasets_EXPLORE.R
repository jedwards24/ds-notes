library(tidyverse)
pn <- ls("package:datasets")
nn <- length(pn)
classes <- character(nn)
len <- integer(nn)
cols <- integer(nn)
rows <- integer(nn)
miss <- logical(nn)
fct <- logical(nn)

for(i in 1 : nn){
  dt <- get(pn[i])
  classes[i] <- class(dt)[1]
  len[i] <- length(dt)
  if(classes[i] == "data.frame"){
    cols[i] <- ncol(dt)
    rows[i] <- nrow(dt)
    if(sum(count_levels(dt) > 0)) fct[i] <- TRUE
  } 
  if(any(is.na(unlist(dt)))) miss[i] <- TRUE
}
info <- tibble(name = pn, class = classes, length = len, cols, rows, missing = miss, contains_factor = fct)
count(info, class)
count(info, contains_factor)
filter(info, contains_factor)
as_tibble(attenu)

info %>% filter(class == "data.frame") %>% select(-class, -length) %>% print(n = Inf)
head(quakes)

get(pn[4])
attenu
?infert
logical(1)
count_nas(airquality)
is.data.frame(dt)
is.list(dt)
is.vector(dt)
is.matrix(dt)
class(dt)
class(get(pn[1]))
dim(1:10)
nrow(1:10)


x <- factor(c("a", "b", "c"))
y <- factor(c("1", "2", "3"))
unlist(list(x, y))
unlist(list(1:3, x))

# data in other packages -----------

data(package = "dplyr")
data(package = "ggplot2")
#https://stackoverflow.com/questions/27709936/get-a-list-of-the-data-sets-in-a-particular-package/27710355

pkgs <- setdiff(.packages(TRUE), c("base", "stats"))
dsets <- data(package = pkgs)$result[, "Item"]

data(package = .packages(all.available = TRUE)) #all available packages
## get the names of all the data sets
names(data())
data()$title 
data()$header 
data()$footer
data()$results %>% as_tibble
ds <- data(package = .packages(all.available = TRUE))$results %>% as_tibble
ds
ds %>% filter(str_detect(Item, "weather"))
count(ds, Package, sort = T)
library(tidyverse)
library(edwards)
dt <- data(package = .packages(all.available = FALSE))$results 
table(dt[, 1])

  tibble::as_tibble() %>% 
  dplyr::count(Package, sort = T) %>% prinf
tidy_depends <- packageDescription("tidyverse")$Imports %>% 
  str_split(",")  %>% 
  unlist() %>%
  str_remove_all("\n") %>% 
  str_remove_all("\\(.+?\\)") %>% 
  str_squish()


data(package = tidy_depends)$results %>%
  as_tibble() %>% view


dataset_info <- function(datasets) {
  nn <- length(datasets)
  classes <- character(nn)
  len <- integer(nn)
  cols <- integer(nn)
  rows <- integer(nn)
  miss <- logical(nn)
  fct <- logical(nn)
  
  for(i in 1 : nn){
    dt <- get(datasets[i])
    classes[i] <- class(dt)[1]
    len[i] <- length(dt)
    if(classes[i] == "data.frame"){
      cols[i] <- ncol(dt)
      rows[i] <- nrow(dt)
      if(sum(count_levels(dt, all = TRUE) > 0)) fct[i] <- TRUE
    } 
    if(any(is.na(unlist(dt)))) miss[i] <- TRUE
  }
  tibble(name = datasets, class = classes, length = len, cols, rows, missing = miss, contains_factor = fct)
}

dataset_info(ls("package:datasets"))
ls("package:datasets") %>% str
ds <- data(package = tidy_depends)$results %>%
  as_tibble()  
left_join(ds, dataset_info(ds$Item)) 
dataset_info(ds$Item)
get(ds$Item[3])
datasets <- ds$Item
library(broom)
library(lubridate)
library(modelr)
broom::argument_glossary


data(package = tidy_depends)$results %>%
  as_tibble() %>% 
  left_join(dataset_info(.$Item), by = c(Item = "name")) %>% 
  select(-LibPath) 
