library(psych)#for EFA &ESEM
library(lavaan)# CFA & ESEM
library(tidyverse)#pipecommand
library(GPArotation)#rotation
install.packages("data.table")
library(data.table)
#Step1: EFA####
esem_data <- IRT %>% 
             select(item6 , item11 , item15 , item20 , item26 , item31 , item36,
                     item7 ,   item27 , item32 , item37,
                     Ritem8 , Ritem13 , Ritem18 , Ritem23 , Ritem28 , Ritem34,
                    Ritem10 , Ritem14,  Ritem25 , Ritem30,
                    item9 , item19 , item21 , item24 , item29 , item33)
names(esem_data) <- paste0("x", c(1:27)) 
#"Item11x1"  "Item20x2"  "Item26x3"  "Item36x4"  "Item2x5"   "Item7x6"   "Item27x7" 
#"Item32x8"  "Item8x9"   "Item13x10" "Item18x11" "Item23x12" "Item14x13" "Item25x14"
#"Item30x15" "Item9x16"  "Item19x17" "Item21x18" "Item24x19"
esem_data
esem_efa <- fa(esem_data, nfactors = 5, rotate = "geominQ", fm ="ml", delta =.5)#delta ranges from (.1-.9) experiment with it
esem_efa$loadings

#Step 2: ESEM: model making####
esem_loadings <- data.table(matrix(round(esem_efa$loadings,2), nrow =27, ncol =5))
names(esem_loadings) <- c("F1", "F2")
esem_loadings$item <- paste0("x", c(1:27))
esem_loadings <- melt(esem_loadings, "item", variable.name = "latent")
esem_loadings


anchors <- c(F1 ="x7", F2 ="x13") #highest loadings in F1 and F2

make_esem_model <- function (loadings_dt, anchors){
  
  # make is_anchor variable
  loadings_dt[, is_anchor := 0]
  for (l in names(anchors)) loadings_dt[latent != l & item == anchors[l], is_anchor := 1]
  
  # make syntax column per item; syntax is different depending on is_anchor
  loadings_dt[is_anchor == 0, syntax := paste0("start(",value,")*", item)]
  loadings_dt[is_anchor == 1, syntax := paste0(value,"*", item)]
  
  #Make syntax for each latent variable
  each_syntax <- function (l){
    paste(l, "=~", paste0(loadings_dt[latent == l, syntax], collapse = "+"),"\n")
  }
  
  # Put all syntaxes together
  paste(sapply(unique(loadings_dt$latent), each_syntax), collapse = " ")
}

esem_model <- make_esem_model(esem_loadings, anchors)

writeLines((esem_model))


#Step 3: Fit ESEM Model
library(lavaan)
library(lavaanPlot)

esem_fit <- cfa(esem_model, esem_data, std.lv=T,  ordered = names(esem_data),estimator = "WLSMV", mimic = "Mplus")
summary(esem_fit, fit.measures = T, standardized = T)
fitmeasures (esem_fit,c("gfi", "agfi","srmr","pgfi", 
                             "nfi","ifi", "cfi","tli", 
                             "rmsea", 
                             "aic","bic", "nnfi"))

semPaths (esem_fit,
          what= "std",# color of edge
          whatLabels = "std",# edge level,
          intercepts = F,
          style ="lisrel",
          residScale = 8,#This makes the residuals larger
          theme = "colorblind",
          nCharNodes = 0,
          reorder =T,
          rotation =2,
          layout ="tree",
          cardinal = "lat cov",#makes laten variable connet
          curvePivot =T,#changes curve into round straight lines
          sizeMan =8,
          sizeMan2 = 2,
          sizeLat = 10,
          color = list(lat = rgb(245, 253, 118, maxColorValue = 255), 
                       man = rgb(155, 253, 175, maxColorValue = 255)))

title("ESEM Analysis", line = 3)
