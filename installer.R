# Command to run this file -> Rscript installer.R requirements.txt
# install pacman package if not required (can be from r console too)
if (!require("pacman")) install.packages("pacman")
library("pacman")

infile <- commandArgs(trailingOnly=TRUE)[1]

# check to make sure specified file exists
if (!file.exists(infile)) {
    stop("Invalid filepath specified...")
}

# get list of packages
packages <- readLines(file(infile))
print(sprintf("Installing: %s", paste0(packages, collapse='\n')))

suppressWarnings(p_install(packages, character.only=TRUE))

print("Done!")