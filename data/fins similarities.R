library(stringr)
library(sjmisc)
library(tidyverse)
library(data.table)

library(dplyr)

library(tidyr)

library(purrr)

string <- c("Hello", "Helo", "Hole", "Apple", "Ape", "New", "Old", "System", "Systemic")
str_pos(string, "hel")   # partial match
str_pos(string, "stem")  # partial match
str_pos(string, "R")     # no match
str_pos(string, "saste") # similarity to "System"

sjmisc::str_find(string, "hel")

# finds two indices, because partial matching now
# also applies to "Systemic"
str_pos(string,
        "sytsme",
        part.dist.match = 1)

# finds nothing
str_pos("We are Sex Pistols!", "postils")
# finds partial matching of similarity
str_pos("We are Sex Pistols!", "postils", part.dist.match = 1)
# }

#####################

df <-structure(list(label = structure(c(5L, 6L, 7L, 8L, 3L, 1L, 2L, 
                                        9L, 10L, 4L), .Label = c(" holand", " holandindia", " Holandnorway", 
                                                                 " USAargentinabrazil", "Afghanestan ", "Afghanestankabol", "Afghanestankabolindia", 
                                                                 "indiaAfghanestan ", "USA", "USAargentina "), class = "factor"), 
                    value = structure(c(5L, 4L, 1L, 9L, 7L, 10L, 6L, 3L, 2L, 
                                        8L), .Label = c("1941029507", "2367321518", "2849255881", 
                                                        "2913128511", "2927576083", "4550996370", "457707181.9", 
                                                        "637943892.6", "796495286.2", "89291651.19"), class = "factor")), .Names = c("label", 
                                                                                                                                     "value"), class = "data.frame", row.names = c(NA, -10L))



library(magrittr)
df$label %>% 
  tolower %>% 
  trimws %>% 
  stringdist::stringdistmatrix(method = "jw", p = 0.1) %>% 
  as.dist %>% 
  `attr<-`("Labels", df$label) %>% 
  hclust %T>% 
  plot %T>% 
  rect.hclust(h = 0.3) %>% 
  cutree(h = 0.3) %>% 
  print -> df$group

df <- data.frame(label=c("hello world 10","hello world 15", "hello world 25", "hello world 99", "joyreactor", "joyReaCtor - 00"))
df$label %>% 
  tolower %>% 
  trimws %>% 
  stringdist::stringdistmatrix(method = "jw", p = 0.1) %>% 
  as.dist %>% 
  `attr<-`("Labels", df$label) %>% 
  hclust %T>% 
  plot %T>% 
  rect.hclust(h = 0.3) %>% 
  cutree(h = 0.3) %>% 
  print -> df$group



a <- "Happy day"
b <- "Tappy Pay"

a <- "hello world 10"
b <- "hello world 05"

adist(a,b) 
Reduce(setdiff, strsplit(c(a, b), split = " "))

setdiff(strsplit(a,"")[[1]],strsplit(b,"")[[1]])

sapply(setdiff(utf8ToInt(a), utf8ToInt(b)), intToUtf8)
#######################



string = c("G1:E001", "G2:E002", "G3:E003")

substring(string, 4)
substring(string, regexpr(":", string) + 1)


df <- data.frame(string)

df %>% 
  separate(string, into = c("pre", "post")) %>% 
  pull("post")

library("stringr")

str_extract(string = string, pattern = "E[0-9]+")
