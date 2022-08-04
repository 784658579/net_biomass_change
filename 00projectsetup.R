# this is for project setup
# setup folders for the project
# for data
if(!dir.create(file.path(".", "data"))){
  dir.create(file.path(".", "data"))
# copy all the data to the data folder
}
# for Rcodes
if(!dir.create(file.path(".", "Rcode"))){
  dir.create(file.path(".", "Rcode"))
}

# for intermediate data storage
if(!dir.create(file.path(".", "intermediate"))){
  dir.create(file.path(".", "intermediate"))
}

# for final results
if(!dir.create(file.path(".", "results"))){
  dir.create(file.path(".", "results"))
}

# we may need this folder to save our date related documents
if(!dir.create(file.path(".", "documents"))){
  dir.create(file.path(".", "documents"))
}


## I think these are all the required packages
packages_wehave <- data.frame(installed.packages())$Package
packages_weneed <- c("devtools", "raster", "data.table", "picante", "nlme", "usethis", "sp", "ape",
                     "vegan", "permute", "lattice", "sf", "rgdal", "maptools", "remotes",
                     "speciesgeocodeR")
packages_needinstall <- packages_weneed[!(packages_weneed %in% packages_wehave)]

install.packages(packages_needinstall)
if("speciesgeocodeR" %in% packages_needinstall){
remotes::install_github("azizka/speciesgeocodeR")
}


# library it when you need to use it in a specific R file
# library(devtools);library(raster); library(data.table); library(picante); library(nlme)
# library(usethis); library(sp); library(ape); library(vegan); library(permute); library(lattice)
# library(rgeos);library(rgdal);library(speciesgeocodeR);library(maptools);library(sf)





