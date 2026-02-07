

library(S7)
library(glue)
# library(purrr)
#library(tidyverse)
#library(mapview)
#library(sf)
#library(tmap)


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
  iucn_color_map <- c(
    DD = "#D1D1C6",
    LC = "#60C659",
    NT = "#CCE226",
    VU = "#F9E814",
    EN = "#FC7F3F",
    CR = "#D81E05",
    EX = "#000000"
  )
  
  if (!category %in% names(iucn_color_map)) {
    stop("Invalid category. Must be one of: DD, LC, NT, VU, EN, CR, EX")
  }
  
  return(iucn_color_map[[category]])
} # end get_iucn_color

#######################################
# Constructor function with predefined colors
iucn_status <- function(category) {
  descriptions <- c(
    DD = "Datos Deficientes",
    LC = "Preocupación Menor",
    NT = "Casi Amenazado",
    VU = "Vulnerable",
    EN = "En Peligro",
    CR = "En Peligro Critico",
    EX = "Extinto"
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


#####################################
#####################################


library(readxl)
categ_table <- read_excel("C:/CodigoR/Mammal_Eval/data/Categorias_finales2025_sin_sub_especie_con_biomodelo.xlsx")
get_biomodelo <- function(sp){
  sp_row <- which(categ_table$NOMBRE==sp)
  biomodelo <- categ_table$Enlace_BioModelo[sp_row]
  estado <- categ_table$Estado[sp_row]
  
  if (is.na(biomodelo) == TRUE) {
    #warning(paste("Species", sp, "not found in database"))
    estado <- "No hay Biomodelo"
    biomodelo <- "https://biomodelos.humboldt.org.co/es/species/visor?species_id=0"
  }
  
  return(c(biomodelo, estado))
}


####################################


