## ------------------------------------------------------------------------

#load necessary packages
library(mfaR)
library(MFAg)

# Read data set
wines <- read.csv("wines.csv") 

# Add row names
row.names(wines) <- wines[,1]

# Separate the grand table into individual tables (create the set list)
col_ind = grep("V2", colnames(wines))
sets = list()
for (i in 1:(length(col_ind))){
  # First 9 tables
  if (i < 10){
    sets[[i]]= (col_ind[i]-1):(col_ind[i+1]-2)
  }
  # Last table 
  else{
    sets[[i]]= (col_ind[i]-1):(col_ind[i]+2)
  }
}

# Construct an object of class 'mfa'
mfa_wine <- mfa_const(data = wines, sets  = sets, ncomps = 4)
mfa_wine


## ------------------------------------------------------------------------

print(mfa_wine)


## ----include = FALSE-----------------------------------------------------

library(ggplot2)
library(png)
library(gridGraphics)


## ----eval = FALSE--------------------------------------------------------
#  
#  library(ggplot2)
#  library(png)
#  library(gridGraphics)
#  

## ----fig.width = 8.5, fig.height = 4.25----------------------------------

NZ <- readPNG("nz.png")
FR <- readPNG("fr.png")
CA <- readPNG("ca.png")

cfs = mfa_wine@cfs

plot_compromise(cfs[, 1], cfs[, 2], rownames_vec = row.names(wines), NZ, FR, CA)


## ----include = FALSE-----------------------------------------------------

library(gridExtra)


## ----eval = FALSE--------------------------------------------------------
#  
#  library(gridExtra)
#  

## ----fig.width = 8.5, fig.height = 11------------------------------------

plot = list()

for (i in 1:10){
  
  pfs = mfa_wine@pfs[[i]]
  plot[[i]] = plot_pfs(pfs[, 1], pfs[, 2], rownames(wines), NZ, FR, CA)
   
  }
  print (do.call(grid.arrange,  plot))


## ------------------------------------------------------------------------

col_ind = grep("V15", colnames(wines))
colnames(wines)[col_ind] <- "Peach"
col_ind = grep("V14", colnames(wines))
colnames(wines)[col_ind] <- "Grass"
col_ind = grep("V13", colnames(wines))
colnames(wines)[col_ind] <- "Melon"
col_ind = grep("V12", colnames(wines))
colnames(wines)[col_ind] <- "Hay"
col_ind = grep("V11", colnames(wines))
colnames(wines)[col_ind] <- "Vegetal"
col_ind = grep("V10", colnames(wines))
colnames(wines)[col_ind] <- "Flinty"
col_ind = grep("V9", colnames(wines))
colnames(wines)[col_ind] <- "Grassy"
col_ind = grep("V8", colnames(wines))
colnames(wines)[col_ind] <- "Leafy"
col_ind = grep("V7", colnames(wines))
colnames(wines)[col_ind] <- "Tropical"
col_ind = grep("V6", colnames(wines))
colnames(wines)[col_ind] <- "Citrus"
col_ind = grep("V5", colnames(wines))
colnames(wines)[col_ind] <- "Smoky"
col_ind = grep("V4", colnames(wines))
colnames(wines)[col_ind] <- "Mineral"
col_ind = grep("V3", colnames(wines))
colnames(wines)[col_ind] <- "Green Pepper"
col_ind = grep("V2", colnames(wines))
colnames(wines)[col_ind] <- "Passion Fruit"
col_ind = grep("V1", colnames(wines))
colnames(wines)[col_ind] <- "Cat Pee"


## ----fig.width = 8.5, fig.height = 11------------------------------------

plot = list()

for (i in 1:10){
  
  plot[[i]] = plot_vload(mfa_wine@mol[[i]][,1], mfa_wine@mol[[i]][,2], colnames(wines)[sets[[i]]])
 
  }
  print (do.call(grid.arrange,  plot))
  

## ------------------------------------------------------------------------

eigen_table(mfa_wine)


## ------------------------------------------------------------------------

COD(mfa_wine)


## ------------------------------------------------------------------------

CVD(mfa_wine)


## ------------------------------------------------------------------------

CTD(mfa_wine)


## ------------------------------------------------------------------------

table1 <- matrix(rnorm(100), nrow = 10)
table2 <- matrix(rnorm(50), nrow = 10)
RV(table1, table2)


## ------------------------------------------------------------------------

RV_table(wines, sets)


## ------------------------------------------------------------------------

table1 <- matrix(rnorm(100), nrow = 10)
table2 <- matrix(rnorm(50), nrow = 10)
Lg(table1, table2)


## ------------------------------------------------------------------------

Lg_table(wines, sets)


