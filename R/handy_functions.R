

library(S7)
library(glue)
library(purrr)
library(tidyverse)
library(mapview)
library(sf)
library(tmap)


#######################################
# Define the IUCN Red List on S7 class
IUCNStatus <- new_class(
  "IUCNStatus",
  properties = list(
    category = class_character,
    color = class_character,
    description = class_character
  ),
  validator = function(self) {
    valid_categories <- c("DD", "LC", "NT", "VU", "EN", "CR", "EX")
    if (!self@category %in% valid_categories) {
      "Category must be one of: DD, LC, NT, VU, EN, CR, EX"
    }
  }
)

#######################################
# Function to get color from category
get_iucn_color <- function(category) {
  colors <- c(
    DD = "#D1D1C6",
    LC = "#60C659",
    NT = "#CCE226",
    VU = "#F9E814",
    EN = "#FC7F3F",
    CR = "#D81E05",
    EX = "#000000"
  )
  
  if (!category %in% names(colors)) {
    stop("Invalid category. Must be one of: DD, LC, NT, VU, EN, CR, EX")
  }
  
  return(colors[[category]])
} # end get_iucn_color

#######################################
# Constructor function with predefined colors
iucn_status <- function(category) {
  descriptions <- c(
    DD = "Data Deficient",
    LC = "Least Concern",
    NT = "Near Threatened",
    VU = "Vulnerable",
    EN = "Endangered",
    CR = "Critically Endangered",
    EX = "Extinct"
  )
  
  color <- get_iucn_color(category)
  
  IUCNStatus(
    category = category,
    color = color,
    description = descriptions[[category]]
  )
}

#######################################
# Print method for IUCNStatus
method(print, IUCNStatus) <- function(x, ...) {
  cat("IUCN Red List Status\n")
  cat("Category:", x@category, "\n")
  cat("Description:", x@description, "\n")
  cat("Color:", x@color, "\n")
}

#######################################
# Get all categories function
get_all_iucn_categories <- function() {
  categories <- c("DD", "LC", "NT", "VU", "EN", "CR", "EX")
  lapply(categories, iucn_status)
}

# Example usage:
# Get color directly:
# color <- get_iucn_color("EN")  # Returns "#FC7F3F"
# 
# Or create full status object:
status <- iucn_status("EN")
# status@color
