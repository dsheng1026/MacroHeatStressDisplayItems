conv_MIL_BIL = 1000.0
# conv_75_90 = 2.129
conv_75_90 = 2.129173
conv_75_15 = 3.507477
CONV_90_15 <- conv_75_15 / conv_75_90

# Load libs ----
library(tidyr)
library(stringr)
library(ggplot2)
library(ggsci)
library(scales)
library(dplyr)
library(gcamdata)
library(purrr)
library(patchwork)
# library(broom)
library(sf)

library(scales)
library(RColorBrewer)
show_col(brewer.pal(12, "Paired"))
brewer.pal(12, "Paired")

# define color palette ----
scenario_colors <- c(
  "Crop" = "#006400",      # dark green
  "Crop: OS" = "#B2DF8A",      # light green
  "Combined" = "#2878B5",  # lake blue
  "Combined: OS" = "#A7C7E7",  # light blue
  "Labor" = "#E63946",         # warm red
  "Ref" = "black"
)


# paired_colors <- c(
#   "EN_capital" = "#A6CEE3", # 
#   "AG_capital" = "#B2DF8A", 
#   "AG_labor" = "#33A02C", 
#   "AG_land" = "#1B5E20",
#   "MA_capital" = "#FDBF6F", 
#   "MA_labor" = "#FF7F00"
# )

paired_colors2 <- c(
  "Ag_capital" = "#B2DF8A", 
  "Ag_labor" = "#33A02C", 
  "Ag_land" = "#1C9099", 
  "NonAg_capital" = "#FDBF6F", 
  "NonAg_labor" = "#FF7F00"
)

# exp_colors <- c(
#   "CG_Ag"     = "#FDBE85",  # Light orange
#   "CG_NonAg"  = "#E6550D",  # Dark orange
#   
#   "INV_Ag"     = "#B2DF8A",  # Light blue
#   "INV_NonAg"  = "#33A02C",  # Dark blue
#   
#   "NX_Ag"      = "#D4B9DA",  # Light purple
#   "NX_NonAg"   = "#756BB1"   # Dark purple
# )

exp_colors <- c(
  "CG_Ag"     = "#1F77B4",  # Medium blue
  "NX_Ag"     = "#C49E60",   # Gold/yellow
  "INV_Ag"    = "#E31A1C",  # Strong red
  
  "CG_NonAg"  = "#6BAED6",  # Light blue
  "NX_NonAg" =  "#FDD835",   # Pale yellow
  "INV_NonAg" = "#F78DB2"   # Soft red
)

# land_colors <- c(
#   "Cropland" = "#E1AD01",
#   "Forest - Managed" = "#228B22",
#   "Forest - Unmanaged" = "#6B8E23",
#   "Other Arable" = "#F4A460",
#   "Other Natural" = "#708090",
#   "Pasture - Managed" = "#90EE90",
#   "Pasture - Unmanaged" = "#F0E68C"
# )

land_colors <- c(
  "Cropland" = "#E1AD01",
  "Forest" = "#228B22",
  "Other Natural" = "#708090",
  "Pasture" = "#90EE90"
)



source("R/LoadPackagesFuncs.R")
source("R/GCAM_module_funcs.R")

DIR_DATA <- "data"
DIR_OUTPUT <- "output"
DIR_MODULE = "HeatStress"

Project <- "HeatStress"
Version <- "VFood"
Scenario <- Load_GCAM(projnm = Project, versionnm = Version, return_availscen = T); Scenario

MODEL_FUTURE_YEARS  <- seq(2020, 2100, 5); MODEL_FUTURE_YEARS

# Check availability
Load_GCAM(projnm = Project, return_availversion = T)
Load_GCAM(projnm = Project, versionnm = Version, return_availscen = T)
Load_GCAM(projnm = Project, versionnm = Version, return_availquery = T)


# Modify/customize read csv function ----
read_csv_bind <- function(.multiCSVpath){
  
  library(doParallel)
  myCluster <-
    makeCluster(4, # number of cores to use
                type = "PSOCK") # type of cluster
  #detectCores()
  registerDoParallel(myCluster)
  
  foreach(csvDir = .multiCSVpath,
          .combine=rbind,
          .packages = "dplyr" ,.errorhandling = "remove"
  ) %dopar% {
    readr::read_csv(csvDir, skip = 1)%>%
      select(-matches("^X|\\...")) %>%
      na.omit() %>%
      filter(scenario != "scenario") %>%
      mutate(scenario = gsub(",date.*$", "", scenario)) %>%
      gcamdata::gather_years() %>%
      mutate(ss = sub(".*/([^/]+)/.*", "\\1", csvDir))
  } -> df
  
  stopCluster(myCluster)
  return(df)
}

rm(ListVFood)
# Load everything into lists ----
Load_GCAM(projnm = Project, versionnm = "VFood", outputlistnm = "ListVFood")

# create a project data output folder and save data
# dir.create(file.path(DIR_OUTPUT, Project, "ProjectRDS"), showWarnings = F) # somehow not working
ListVFood %>% saveRDS(file.path(DIR_OUTPUT, Project, "ProjectRDS", paste0("ListVFood", ".RDS")))

# Load the list [when needed]
ListVFood <- readRDS(file.path(DIR_OUTPUT, Project, "ProjectRDS", paste0("ListVFood", ".RDS")))

## theme1 ----
theme1 <- theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
                strip.background = element_rect(fill="grey99"),
                strip.text = element_text(size = 12),
                axis.text.x.bottom = element_text(size = 12),
                axis.text.y = element_text(size = 12),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_line(linetype = 2, color = "grey80", size = 0.3),
                panel.spacing.y = unit(0.5, "lines"),
                panel.spacing.x = unit(0.5, "lines"))

## theme2 ----
theme2 <- theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
                strip.background = element_rect(fill="grey99"),
                strip.text = element_text(size = 12),
                axis.text.x.bottom = element_text(size = 12),
                axis.text.y = element_text(size = 12),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                panel.spacing.y = unit(0.5, "lines"),
                panel.spacing.x = unit(0.5, "lines"))


themeds <- theme(
  # panel.border = element_rect(colour = "black", size=1),
  text = element_text(family= fontfamily, size = 15),
  axis.text.y = element_text(angle = 0, color = "black", size = 15, margin = margin(r = 10)),
  axis.text.x = element_text(angle = 90, color = "black", size = 15, margin = margin(t = 10), vjust= 0.5),
  axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
  axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0))
)


gather_time <- function(.data){
  .data %>%
    gather(year, value, names(.)[grepl("[0-9]{4}", names(.))]) %>%
    mutate(year = as.integer(gsub("X", "", year))) %>%
    return()
}

basin_to_country_mapping <- read.csv("data/maps/basin_to_country_mapping.csv", skip = 7, header = T)

gcam_macro_TFP_open_core <- read.csv("C:/Model/KLEAM/input/gcamdata/inst/extdata/socioeconomics/gcam_macro_TFP_open_core.csv",
                                     skip = 6, header = T)


reg_KLEAM_HS10 <- c("North America", "Europe", "Reforming Economies", "Pacific OECD", "Middle East", 
                    "China+", "South Asia", "Southeast Asia", "Africa", "Latin America")

reg_order <- reg_KLEAM_HS10

scenario_target <- "CL_LS"

