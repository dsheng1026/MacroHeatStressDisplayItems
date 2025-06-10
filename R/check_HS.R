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
  "Ag_land" = "#007F5C", 
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
  "CG_Ag"     = "#E6550D",  # Dark orange
  "NX_Ag"     = "#FDAE6B",  # Medium orange
  "INV_Ag"    = "#FEE0B6",  # Pale peach (lighter but still warm)
  
  "CG_NonAg"  = "#54278F",  # Dark purple
  "NX_NonAg"  = "#9E9AC8",  # Medium purple
  "INV_NonAg" = "#CBC9E2"   # Cool lavender (more distinguishable from pale orange)
)

land_colors <- c(
  "Cropland" = "#E1AD01",
  "Forest - Managed" = "#228B22",
  "Forest - Unmanaged" = "#6B8E23",
  "Other Arable" = "#F4A460",
  "Other Natural" = "#708090",
  "Pasture - Managed" = "#90EE90",
  "Pasture - Unmanaged" = "#F0E68C"
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

reg_KLEAM_HS10 <- c("North America", "Europe", "Reforming Economies", "Pacific OECD", "Middle East", 
                     "China+", "South Asia", "Southeast Asia", "Africa", "Latin America")

reg_order <- reg_KLEAM_HS10

SCENARIO <- Scenario; SCENARIO

PluckBind <- function(.query ){
  ListVFood %>% purrr::pluck(.query) %>%
    mutate(branch = scenario, scenario = ss) %>%
    filter(scenario %in% SCENARIO) %>%
    mutate(scenario = gsub("food_MRI", "Ref", scenario),
           scenario = gsub("food_HS_", "", scenario),
           scenario = gsub("_MRI", "", scenario))
}

SCE_NAME <- function(.data){
  .data %>% 
    mutate(scenario = gsub("CA_LS", "Crop", scenario),
           scenario = gsub("CA_OS", "Crop: OS", scenario),
           scenario = gsub("CLA_LS", "Combined", scenario),
           scenario = gsub("CLA_OS", "Combined: OS", scenario),
           scenario = gsub("LA", "Labor", scenario)) %>% 
    return()
}

REG <- st_read("data/maps/region_boundaries_moirai_combined_3p1_0p5arcmin.shp") 
WB <- st_read("data/maps/reg_glu_boundaries_moirai_landcells_3p1_0p5arcmin.shp") 


# PLOT ----

# scenario_target <- "CLA"
scenario_target <- "CLA_LS"

# schematic ----
## Figure 1 ----
### the schematic is generated outside this script


# Macroeconomic responses  ----

## Figure 2 ----

### biophysical changes ----

#### labor shocks ----
LaborTech2015 <- read.csv("data/HeatStress/VFood/food_MRI/AgLaborTech2015.csv", skip = 1, header = T) %>% 
  select(region, subsector, technology, sector, labor = X2015) 

# df.pi %>% filter(year == 2015, scenario == "Ref") %>% 
#   select(region, technology = LandLeaf, year, crop, land, WB, IRR, mgmt) ->
#   LandTech2015

splits <- strsplit(LaborTech2015$technology, '_')
splits <- lapply(splits, 'length<-', max(lengths(splits)))
splits <- do.call(rbind, splits)
LaborTech2015[c('crop', 'WB', 'IRR', 'mgmt')] <- splits
head(LaborTech2015)

HS_PC_GCAM <- readRDS("C:/Model/KLEAM/HELPS_GCAM_mapping_Dunne_Food_MRI.rds")

HS_PC_GCAM %>% rename(sector = AgSupplySector, subsector = AgSupplySubsector,
                      technology = AgProductionTechnology) %>% 
  left_join(LaborTech2015 %>% group_by(subsector, technology, sector, WB, IRR, mgmt) %>% 
              summarise(labor = sum(labor, na.rm = T))) %>% 
  filter(!is.na(labor)) %>% 
  group_by(WB, year) %>% 
  summarise(IO_mult = weighted.mean(IO_mult, labor, na.rm = T)) %>% 
  mutate(eta_mult = 1/IO_mult) ->
  df.eta.shock

WB %>% 
  left_join(basin_to_country_mapping %>% select(glu_id = GCAM_basin_ID, WB = GLU_name)) %>% 
  left_join(df.eta.shock %>% filter(year == 2100)) %>% mutate(index = 100*eta_mult - 100) %>% 
  mutate(year = 2100) %>% 
  bind_rows(WB %>%
              left_join(basin_to_country_mapping %>% select(glu_id = GCAM_basin_ID, WB = GLU_name)) %>%
              left_join(df.eta.shock %>% filter(year == 2050)) %>% mutate(index = 100*eta_mult - 100) %>% 
              mutate(year = 2050)) ->
  df.plot.labor 

# df.plot.labor %>% 
#   ggplot() +
#   geom_sf(aes(fill = index)) +
#   scale_fill_gradient2(low = "red", high = "lightblue", midpoint = 0) +
#   coord_sf(datum = NA) +
#   facet_wrap(~ year, ncol = 1) +
#   theme_bw() + theme0 + theme1 +
#   theme(legend.position="right",
#         plot.title = element_text(hjust = 0.5,
#                                   color = "Gray40",
#                                   size = 16,
#                                   face = "bold"),
#         plot.subtitle = element_text(color = "blue"),
#         plot.caption = element_text(color = "Gray60"))  +
#   guides(fill = guide_colorbar(title = "%",
#                                title.position = "top",
#                                title.theme = element_text(size = 10,
#                                                           face = "bold",
#                                                           colour = "black",
#                                                           angle = 0))) ->
#   Fig2.Eta.shock
# 
# Write_png(Fig2.Eta.shock, "Fig2.Eta.shock", DIR_MODULE, w = 5, h = 4, r = 300)

#### crop shocks ----

PluckBind("Land") %>%  # thous km2
  select(scenario, region, LandLeaf, year, value) %>% 
  filter(year == 2015,
         scenario == "Ref") %>% 
  select(-scenario, -year) ->
  LandTech2015

splits <- strsplit(LandTech2015$LandLeaf, '_')
splits <- lapply(splits, 'length<-', max(lengths(splits)))
splits <- do.call(rbind, splits)
LandTech2015[c('crop', 'WB', 'IRR', 'mgmt')] <- splits

# reference AgYld
AgYld_ref <- readRDS("C:/Model/heat_paper/Result/paper_MacroHS/data/HeatStress/AgYld_ref.rds")

AgYld_ref %>% 
  arrange(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year) %>% 
  mutate(inter_mult = (1+AgProdChange)^5) %>% 
  group_by(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology) %>% 
  mutate(yld= cumprod(inter_mult)) %>% 
  mutate(scenario = "Ref") %>% 
  bind_rows(read.csv("C:/Model/gaia/agyield_impact_mri-esm2-0_r1i1p1f1_w5e5_Food-MRI.csv") %>% 
              select(-X) %>% 
              arrange(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year) %>% 
              mutate(inter_mult = (1+AgProdChange)^5) %>% 
              group_by(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology) %>% 
              mutate(yld = cumprod(inter_mult),
                     scenario = "gaia")) %>% 
  select(-AgProdChange, -inter_mult) %>% 
  spread(scenario, yld) %>% 
  mutate(index = gaia / Ref) %>% 
  left_join(LandTech2015 %>% rename(AgProductionTechnology = LandLeaf)) %>% 
  group_by(WB, year) %>% 
  summarise(index = weighted.mean(index, value, na.rm = T)) ->
  df.yld.shock


WB %>% 
  left_join(basin_to_country_mapping %>% select(glu_id = GCAM_basin_ID, WB = GLU_name)) %>% 
  left_join(df.yld.shock %>% filter(year == 2100)) %>% mutate(index = 100*index - 100) %>% 
  mutate(year = 2100) %>% 
  bind_rows(WB %>%
              left_join(basin_to_country_mapping %>% select(glu_id = GCAM_basin_ID, WB = GLU_name)) %>%
              left_join(df.yld.shock %>% filter(year == 2050)) %>% mutate(index = 100*index - 100) %>% 
              mutate(year = 2050)) ->
  df.plot.crop

max(df.plot.crop$index)

df.plot.crop %>% mutate(shock = "Crop") %>% 
  bind_rows(df.plot.labor %>% select(names(df.plot.crop)) %>% 
              mutate(shock = "Labor")) ->
  df.bio 

max5 <- ceiling(max(df.bio$index, na.rm = TRUE) / 5) * 5; max5
min5 <- floor(min(df.bio$index, na.rm = TRUE) / 5) * 5; min5

df.bio %>% 
  mutate(bin = cut(
    index,
    breaks = seq(min5, max5, by = 5),
    include.lowest = TRUE,
    right = FALSE
  )) ->
  df.bio.bin

bin_levels <- levels(df.bio.bin$bin)
colors <- c(
  colorRampPalette(c("darkred", "#FFE4E1"))(length(bin_levels[bin_levels < "[0,5)"])) ,
  colorRampPalette(c("#E6F0FA", "#95B5D8"))(length(bin_levels[bin_levels >= "[0,5)"]))
)


# red_shades <- c("#D89895", "#EBBEBB", "#FFE4E1")
# 
# # Matching blue side (mirrored in tone)
# blue_shades <- c("#95B5D8", "#BBD2EB", "#E1EDF9")

df.bio.bin %>% 
  ggplot() +
  geom_sf(aes(fill = bin)) +
  scale_fill_manual(values = colors, drop = FALSE) +
  coord_sf(datum = NA) +
  facet_grid(shock ~ year) +
  labs(fill = "%") +
  # labs(fill = "%", title = "% biophysical shock in labor and crop") +
  theme_bw() + theme0 + theme1 ->
  Fig2.bio.shock; Fig2.bio.shock

Write_png(Fig2.bio.shock, "Fig2.bio.shock", DIR_MODULE, w = 8, h = 3.5, r = 300)


### GDP ----
PluckBind("SAM_NA") %>% 
  # select(scenario, region, Account, year, value) %>% 
  filter(Account == "GDP") -> 
  df.GDP

### global trend ----

df.GDP %>% 
  group_by(scenario, Account, Units, year) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  group_by(year) %>% 
  filter(year >= 2015) %>% 
  mutate(delta = value - value[scenario == "Ref"],
         delta = delta / 1000, # mil 1990$ to bil 1990$
         delta = delta * CONV_90_15, # 1990$ to 2015$
         index = 100*(value / value[scenario == "Ref"] -1), # %
         region = "World") %>% 
  filter(scenario == scenario_target) %>%
  SCE_NAME() ->
  dg.gdp.layer

scaling_factor <- max(abs(dg.gdp.layer$delta)) / 1  # adjust denominator to match desired rel axis range

dg.gdp.layer %>% 
  ggplot(aes(x = year)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_bar(aes(y = delta, fill = Account),
           stat = "identity", position = "stack") +
  geom_line(aes(y = index * scaling_factor), 
            color = "royalblue", linetype = "dashed", linewidth = 1) +
  scale_y_continuous(
    name = "billion 2015$",
    sec.axis = sec_axis(~ . / scaling_factor, name = "%")
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  facet_wrap(~ region, nrow = 1) +
  labs(x = "", y = "", fill = "Factor") +
  theme_bw() + themeds + theme(legend.position = "none") ->
  Fig2.1.global; Fig2.1.global

Write_png(Fig2.1.global, "Fig2.1.global", DIR_MODULE, w = 6, h = 6, r = 300)

df.GDP %>% 
  group_by(scenario, Account, Units, year) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  group_by(year) %>% 
  filter(year >= 2015) %>%  
  mutate(index = 100*(value / value[scenario == "Ref"] -1)) %>% 
  # filter(scenario == scenario_target) %>%
  SCE_NAME() %>% 
  ggplot(aes(x = year, y = index, color = scenario)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = scenario_colors) +
  labs(x = "", y = "%", title = "Global GDP reponse") +
  theme_bw() + themeds ->
  FigS2.gdp.global; FigS2.gdp.global

Write_png(FigS2.gdp.global, "FigS2.gdp.global", DIR_MODULE, w = 6, h = 6, r = 300)

### 10 regions trend ----

df.GDP %>% 
  group_by(scenario, Account, Units, year) %>% 
  left_join_error_no_match(Regmapping %>% select(region, REG10_main), by = "region") %>% 
  group_by(scenario, region = REG10_main, year) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  group_by(region, year) %>% 
  mutate(delta = value - value[scenario == "Ref"],
         delta = delta / 1000, # mil 1990$ to bil 1990$
         delta = delta * CONV_90_15, # 1990$ to 2015$
         index = 100*(value / value[scenario == "Ref"] -1)) %>% 
  filter(year >= 2015) %>% 
  filter(scenario == scenario_target) %>%
  SCE_NAME() ->
  dg.gdp.layer

scaling_factor <- max(abs(dg.gdp.layer$delta)) / 2.5  # adjust denominator to match desired rel axis range

dg.gdp.layer %>% 
  mutate(region = factor(region, levels = reg_order)) %>% 
  ggplot(aes(x = year)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_bar(aes(y = delta, fill = scenario),
           stat = "identity", position = "stack") +
  geom_line(aes(y = index * scaling_factor), 
            color = "royalblue", linetype = "dashed", linewidth = 1) +
  scale_y_continuous(
    name = "billion 2015$",
    sec.axis = sec_axis(~ . / scaling_factor, name = "%")) +
  scale_fill_brewer(palette = "Pastel1") +
  facet_wrap(~ region, nrow = 2) +
  labs(x = "", y = "", fill = "Factor") +
  theme_bw() + themeds + theme(legend.position = "none") ->
  Fig2.1.10reg; Fig2.1.10reg

Write_png(Fig2.1.10reg, "Fig2.1.10reg", DIR_MODULE, w = 12, h = 6, r = 300)


df.GDP %>% 
  left_join_error_no_match(Regmapping %>% select(region, REG10_main)) %>% 
  group_by(scenario, year, region =REG10_main) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  filter(year >= 2015) %>% 
  group_by(region, year) %>% 
  mutate(index = 100*(value / value[scenario == "Ref"] -1),
         region = factor(region, levels = reg_order)) %>% 
  filter(scenario != "Ref") %>%
  SCE_NAME() %>% 
  ggplot(aes(x = year, y = index, color = scenario)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8) +
  geom_line(linewidth = 1) +
  facet_wrap(~ region, ncol = 5) +
  scale_color_manual(values = scenario_colors) +
  labs(x = "", y = "%", title = "Regional GDP reponse") +
  theme_bw() + themeds -> FigS2.gdp.reg10; FigS2.gdp.reg10

Write_png(FigS2.gdp.reg10, "FigS2.gdp.reg10", DIR_MODULE, w = 12, h = 6, r = 300)


### 32 regions map: 2100 ----

REG %>% rename(region = reg_nm) %>% 
  left_join(df.GDP %>% select(scenario, region, year, value) %>% 
              filter(year == 2100) %>% 
              group_by(region, year) %>% 
              mutate(index = 100*(value / value[scenario == "Ref"] -1)) %>% 
              filter(scenario == scenario_target) %>% SCE_NAME(),
            by = "region") %>% 
  bind_rows(REG %>% rename(region = reg_nm) %>% 
              left_join(df.GDP %>% select(scenario, region, year, value) %>% 
                          filter(year == 2050) %>% 
                          group_by(region, year) %>% 
                          mutate(index = 100*(value / value[scenario == "Ref"] -1)) %>% 
                          filter(scenario == scenario_target) %>% SCE_NAME(),
                        by = "region")) ->
  gdp.32.plot

max1 <- ceiling(max(gdp.32.plot$index)); max1
min1 <- floor(min(gdp.32.plot$index)); min1


gdp.32.plot %>% 
  mutate(bin = cut(
    index,
    breaks = seq(min1, max1, by = 1),
    include.lowest = TRUE,
    right = FALSE
  )) ->
  gdp.32.plot

bin_levels <- levels(gdp.32.plot$bin); bin_levels
colors <- c(
  colorRampPalette(c("darkred", "#FFE4E1"))(length(bin_levels[bin_levels < "[0,1)"])) ,
  colorRampPalette(c("#E6F0FA", "#95B5D8"))(length(bin_levels[bin_levels >= "[0,1)"]))
)

gdp.32.plot %>% 
  ggplot() +
  geom_sf(aes(fill = bin)) +
  facet_grid(scenario ~ year) +
  scale_fill_manual(values = colors, drop = FALSE) +
  coord_sf(datum = NA) + 
  labs(fill = "%") +
  # labs(title = "% change of GDP relative to Ref (Ref = 0)", fill = "%") +
  theme_bw() + theme0 + theme1 +
  theme(legend.position="right") ->
  Fig2.1.32map; Fig2.1.32map

  Write_png(Fig2.1.32map, "Fig2.1.32map", DIR_MODULE, w = 8, h = 2.5, r = 300)
  
  
### 10 regions channels: 2100 ----
  
  df.GDP %>% select(scenario, region, year, value) %>% 
    left_join_error_no_match(Regmapping %>% select(region, REG10_main)) %>% 
    group_by(scenario, year, region =REG10_main) %>% 
    summarise(value = sum(value, na.rm = T)) %>% 
    filter(year %in% c(2050, 2100)) %>% 
    group_by(region, year) %>% 
    mutate(index = 100*(value / value[scenario == "Ref"] -1)) %>% 
    filter(scenario %in% c("CA_LS", "LA", "CLA_LS")) %>% 
    SCE_NAME() -> 
    df.GDP.channel.r10
  
  df.GDP %>% select(scenario, region, year, value) %>% 
    group_by(scenario, year) %>% 
    summarise(value = sum(value, na.rm = T)) %>% 
    filter(year %in% c(2050, 2100)) %>% 
    group_by(year) %>% 
    mutate(index = 100*(value / value[scenario == "Ref"] -1),
           region = "World") %>% 
    filter(scenario %in% c("CA_LS", "LA", "CLA_LS")) %>% 
    SCE_NAME() -> 
    df.GDP.channel.glb
    
  df.GDP.channel.r10 %>% 
    bind_rows(df.GDP.channel.glb) %>% 
    filter(!grepl("Combined", scenario)) %>% 
    mutate(region = factor(region, levels = c(rev(reg_order), "World"))) %>% 
    ggplot() +
    geom_bar(aes(y = region, x = index, fill = scenario), 
             stat = "identity", position = "stack") +
    geom_errorbarh(data = df.GDP.channel.r10 %>% bind_rows(df.GDP.channel.glb) %>% 
                     filter(grepl("Combined", scenario)) %>% 
                     mutate(region = factor(region, levels = c(rev(reg_order), "World"))),
                   aes(y = region, xmin = index, xmax = index, linetype = "Combined")) +
    facet_wrap(~ year, nrow = 2) +
    scale_fill_brewer(palette =  "Accent") +
    labs(x = "%", y = "", linetype = "") +
    # labs(title = "% change of GDP relative to Ref (Ref = 0)") +
    theme_bw() + theme0 + themeds ->
    Fig2.1.channel.10; Fig2.1.channel.10
  
  Write_png(Fig2.1.channel.10, "Fig2.1.channel.10", DIR_MODULE, w = 8, h = 6, r = 300)
  
  # Input market  responses  ----
  
  ## Figure 3 ----
  
  ### GDP decomposition: income method ----
  
  #### Return to labor ----
  
  PluckBind("LaborSupplyAll") %>% filter(year >= 2015) %>% 
    mutate(sector = "Labor_Total",
           sector = ifelse(grepl("Labor_Ag", market), "Labor_Ag", sector),
           sector = ifelse(grepl("Labor_Materials", market), "Labor_Materials", sector),
           region = gsub("Labor_Ag", "", market),
           region = gsub("Labor_Materials", "", region),
           region = gsub("Labor_Total", "", region)) %>%
    select(-market) ->
    df.labor # mil person
  
  df.labor %>% 
    group_by(scenario, year, region) %>% 
    mutate(share = 100 * value / value[sector == "Labor_Total"]) %>% 
    filter(sector == "Labor_Ag") %>% 
    group_by(region, year, sector) %>% 
    mutate(delta = value - value[scenario == "Ref"],
           delta_share = share - share[scenario == "Ref"]) %>% 
    filter(year == 2100) %>% 
    filter(scenario == scenario_target) ->
    delta.labor.2100.r32
  
  REG %>% rename(region = reg_nm) %>% 
    left_join(delta.labor.2100.r32, by = "region") %>% 
    SCE_NAME() %>% 
    ggplot() +
    geom_sf(aes(fill = delta)) +
    scale_fill_gradient2(low = "red", high = "blue", midpoint = 0) +
    coord_sf(datum = NA) + 
    # labs(title = "Absolute changes in agricultural employment (Ref = 0)") +
    theme_bw() + theme0 + theme1 +
    theme(legend.position="right",
          plot.title = element_text(size = 16,
                                    face = "bold"),
          plot.subtitle = element_text(color = "blue"))  +
    guides(fill = guide_colorbar(title = "million\npeople",
                                 title.position = "top",
                                 title.theme = element_text(size = 10,
                                                            face = "bold",
                                                            colour = "black",
                                                            angle = 0))) ->
    Fig3.AgL.32map; Fig3.AgL.32map
  
  Write_png(Fig3.AgL.32map, "Fig3.AgL.32map", DIR_MODULE, w = 6, h = 2, r = 300)
    
  
  REG %>% rename(region = reg_nm) %>% 
    left_join(delta.labor.2100.r32, by = "region") %>% 
    SCE_NAME() %>% 
    ggplot() +
    geom_sf(aes(fill = delta_share)) +
    scale_fill_gradient2(low = "red", high = "blue", midpoint = 0) +
    coord_sf(datum = NA) + 
    # labs(title = "Absolute changes in agricultural labor share of labor (Ref = 0) ") +
    theme_bw() + theme0 + theme1 +
    theme(legend.position="right",
          plot.title = element_text(size = 16,
                                    face = "bold"),
          plot.subtitle = element_text(color = "blue"))  +
    guides(fill = guide_colorbar(title = "% point",
                                 title.position = "top",
                                 title.theme = element_text(size = 10,
                                                            face = "bold",
                                                            colour = "black",
                                                            angle = 0))) ->
    Fig3.AgLShare.32map; Fig3.AgLShare.32map
  
  Write_png(Fig3.AgLShare.32map, "Fig3.AgLShare.32map", DIR_MODULE, w = 6, h = 2, r = 300)
  
  df.labor %>% 
    left_join_error_no_match(Regmapping %>% select(region, REG10_main)) %>% 
    group_by(scenario, year, sector, region =REG10_main) %>% 
    summarise(value = sum(value, na.rm = T)) ->
    df.labor.10
  
  df.labor.10 %>% 
    group_by(scenario, sector, year) %>% 
    summarise(value = sum(value, na.rm = T)) %>% 
    mutate(region = "World") ->
    df.labor.glb
  
  
  df.labor.glb %>% 
    group_by(scenario, year, region) %>% 
    mutate(share = 100 * value / value[sector == "Labor_Total"]) %>% 
    filter(sector == "Labor_Ag") %>% 
    filter(year >= 2020) %>% 
    filter(scenario %in% c(scenario_target, "Ref") ) %>% 
    SCE_NAME() %>% 
    ggplot() +
    geom_line(aes(x = year, y = share, color = scenario), linewidth = 0.8) +
    labs(x = "", y = "%") +
    theme_bw() + themeds ->
  Fig3.glb.AgLShare; Fig3.glb.AgLShare
  
  Write_png(Fig3.glb.AgLShare, "Fig3.glb.AgLShare", DIR_MODULE, w = 6, h = 4, r = 300)
  
  
  df.labor.glb %>% 
    group_by(scenario, year, region) %>% 
    filter(sector == "Labor_Ag") %>% 
    filter(year >= 2020) %>% 
    filter(scenario %in% c(scenario_target, "Ref") ) %>% 
    SCE_NAME() %>% 
    ggplot() +
    geom_line(aes(x = year, y = value, color = scenario), linewidth = 0.8) +
    labs(x = "", y = "million people") +
    # labs(title = "Agricultural labor") +
    theme_bw() + themeds ->
    Fig3.glb.AgL; Fig3.glb.AgL
  
  Write_png(Fig3.glb.AgL, "Fig3.glb.AgL", DIR_MODULE, w = 6, h = 4, r = 300)
  
  df.labor.10 %>% 
    bind_rows(df.labor.glb) %>% 
    group_by(scenario, year, region) %>% 
    mutate(share = 100 * value / value[sector == "Labor_Total"]) %>% 
    filter(sector == "Labor_Ag") %>% 
    filter(year >= 2020) %>% 
    SCE_NAME() %>% 
    ggplot() +
    geom_line(aes(x = year, y = share, color = scenario), linewidth = 1) +
    facet_wrap( ~ region, ncol = 6) +
    scale_color_manual(values = scenario_colors) +
    labs(x = "", y = "%") +
    labs(title = "Agricultural labor share of labor force") +
    theme_bw() + themeds ->
    Fig3.S1.AgLShare; Fig3.S1.AgLShare
  
  Write_png(Fig3.S1.AgLShare, "Fig3.S1.AgLShare", DIR_MODULE, w = 10, h = 6, r = 300)
  
  
  PluckBind("LaborPriceAll") %>% filter(year >= 2015) %>% 
    mutate(sector = "Labor_Total",
           sector = ifelse(grepl("Labor_Ag", market), "Labor_Ag", sector),
           sector = ifelse(grepl("Labor_Materials", market), "Labor_Materials", sector),
           region = gsub("Labor_Ag", "", market),
           region = gsub("Labor_Materials", "", region),
           region = gsub("Labor_Total", "", region)) %>%
    select(-market) ->
    df.wage # 1990 $/ppl
  
  df.wage %>% select(scenario, region, year, sector, value) %>% mutate(variable = "wage") %>% 
    bind_rows(df.labor %>% select(scenario, region, year, sector, value) %>% mutate(variable = "labor")) %>% 
    spread(variable, value) %>% 
    mutate(VAL = labor * wage) %>% 
    gather(variable, value, VAL:wage) %>% 
    filter(variable == "VAL") %>% 
    group_by(region, year, sector, variable) %>% 
    mutate(delta = value - value[scenario == "Ref"],
           index = 100 * value / value[scenario == "Ref"] - 100) %>% 
    filter(year == 2100) %>% 
    filter(scenario == scenario_target) ->
    delta.VAL.2100.r32
  
  REG %>% rename(region = reg_nm) %>%
    left_join(delta.VAL.2100.r32, by = "region") %>% 
    SCE_NAME() %>% 
    filter(sector == "Labor_Total") %>% 
    ggplot() +
    geom_sf(aes(fill = index)) +
    scale_fill_gradient2(low = "red", high = "blue", midpoint = 0) +
    coord_sf(datum = NA) + 
    # labs(title = "Relative change in total return to labor (Ref = 0)") +
    # facet_wrap(~ sector, ncol = 1) +
    theme_bw() + theme0 + theme1 +
    theme(legend.position="right",
          plot.title = element_text(size = 16,
                                    face = "bold"),
          plot.subtitle = element_text(color = "blue"))  +
    guides(fill = guide_colorbar(title = "%",
                                 title.position = "top",
                                 title.theme = element_text(size = 10,
                                                            face = "bold",
                                                            colour = "black",
                                                            angle = 0))) ->
    Fig3.R2L.32map; Fig3.R2L.32map
  
  Write_png(Fig3.R2L.32map, "Fig3.R2L.32map", DIR_MODULE, w = 6, h = 2, r = 300)
  
  df.wage %>% select(scenario, region, year, sector, value) %>% mutate(variable = "wage") %>% 
    bind_rows(df.labor %>% select(scenario, region, year, sector, value) %>% mutate(variable = "labor")) %>% 
    spread(variable, value) %>% 
    mutate(VAL = labor * wage) %>% 
    left_join(Regmapping %>% select(region, REG10_main), by = "region") %>% 
    group_by(scenario, year, region = REG10_main, sector) %>%  
    summarise(VAL = sum(VAL, na.rm = T), # million 1990$
              labor = sum(labor)) %>% 
    mutate(wage = VAL/labor) %>% 
    gather(variable, value, VAL:wage) ->
    plot.wage.10
  
  plot.wage.10 %>% 
    filter(variable == "labor") %>% 
    group_by(scenario, year, region, variable) %>% 
    mutate(share = 100 * value / value[sector == "Labor_Total"]) %>% 
    group_by(region, sector, year, variable) %>% 
    mutate(delta_share = share - share[scenario == "Ref"],
           delta = value - value[scenario == "Ref"]) %>%
    filter(sector == "Labor_Ag") %>% 
    SCE_NAME() %>% 
    mutate(region = factor(region, levels = reg_order)) %>%
    ggplot() +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_line(aes(x = year, y = delta_share, color = scenario),
              linewidth = 0.8) +
    facet_wrap(~ region, ncol = 5) +
    labs(x = "", y = "%", title = "Relative changes in the agricultural labor share (Ref = 0)") +
    # scale_x_continuous(breaks = seq(2000, 2100, by = 25)) +
    scale_x_continuous(breaks = c(2025, 2050, 2075, 2100)) +
    scale_color_manual(values = scenario_colors) +
    theme_bw() + themeds -> Fig3.S1.AgLShare.delta; Fig3.S1.AgLShare.delta
  
  Write_png(Fig3.S1.AgLShare.delta, "Fig3.S1.AgLShare.delta", DIR_MODULE, w = 10, h = 6, r = 300)
  
  # plot.wage.10 %>% 
  #   filter(variable == "wage") %>% 
  #   group_by(scenario, year, region, variable) %>% 
  #   mutate(ratio = value / value[sector == "Labor_Materials"]) %>% 
  #   filter( sector == "Labor_Ag") %>% 
  #   SCE_NAME() %>% 
  #   mutate(region = factor(region, levels = reg_order)) %>%
  #   ggplot() +
  #   # geom_hline(yintercept = 1, linetype = "dotted") +
  #   geom_line(aes(x = year, y = ratio, color = scenario),
  #             linewidth = 0.8) +
  #   facet_wrap(~ region, ncol = 5, scales = "free_y") +
  #   labs(x = "", y = "index", title = "Agricultural wage relative to Material wage (Material wage = 1)") +
  #   # scale_x_continuous(breaks = seq(2000, 2100, by = 25)) +
  #   scale_x_continuous(breaks = c(2025, 2050, 2075, 2100)) +
  #   scale_color_manual(values = scenario_colors) +
  #   theme_bw() + themeds + 
  #   theme(axis.text.x = element_text(angle = 30, hjust = 1))
  
  
  # df.wage %>% select(scenario, region, year, sector, value) %>% mutate(variable = "wage") %>% 
  #   bind_rows(df.labor %>% select(scenario, region, year, sector, value) %>% mutate(variable = "labor")) %>% 
  #   spread(variable, value) %>% 
  #   mutate(VAL = labor * wage) %>% 
  #   group_by(scenario, year, sector) %>%  
  #   summarise(VAL = sum(VAL, na.rm = T),
  #             labor = sum(labor, na.rm = T)) %>% 
  #   mutate(wage = VAL/labor, region = "World") %>% 
  #   gather(variable, value, VAL:wage) %>% 
  #   group_by(year, sector, region, variable) %>% 
  #   mutate(index = 100 * value / value[scenario == "Ref"] - 100) %>% 
  #   filter(scenario != 'Ref') %>%
  #   SCE_NAME() %>% 
  #   # filter(grepl("Canesm5", scenario)) %>% 
  #   ggplot() +
  #   geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.8) +
  #   geom_line(aes(x = year, y = index, color = sector), linewidth = 1) +
  #   facet_grid(scenario ~ variable, scales = "free_y") +
  #   labs(x = "", y = "% change relative to Ref (Ref = 0)", title = "") +
  #   theme_bw() + themeds + 
  #   theme(axis.text.x = element_text(angle = 30, hjust = 1),
  #         legend.position = "bottom")
  
  
  plot.wage.10 %>% 
    group_by(scenario, sector, region, variable) %>% 
    mutate(index = value / value[year == 2015])  %>% 
    filter(grepl(scenario_target, scenario)) %>% 
    ggplot() +
    geom_hline(yintercept = 1, linetype = "dotted", linewidth = 0.8) +
    geom_line(aes(x = year, y = index, color = sector), linewidth = 1) +
    facet_grid(variable ~ region, scales = "free_y") +
    labs(x = "", y = "Relative to 2015 (2015 = 1)", title = scenario_target) +
    theme_bw() + themeds + 
    theme(legend.position = "bottom") -> Fig3.S1.LaborMetric.trend; Fig3.S1.LaborMetric.trend
  
  Write_png(Fig3.S1.LaborMetric.trend, "Fig3.S1.LaborMetric.trend", DIR_MODULE, w = 14, h = 6, r = 300)
  
  plot.wage.10 %>% 
    group_by(year, sector, region, variable) %>% 
    mutate(index = 100 * value / value[scenario == "Ref"] - 100)  %>% 
    filter(grepl(scenario_target, scenario)) %>% 
    mutate(region = factor(region, level = reg_order)) %>% 
    ggplot() +
    geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.8) +
    geom_line(aes(x = year, y = index, color = sector), linewidth = 1) +
    facet_grid(variable ~ region, scales = "free_y") +
    labs(x = "", y = "% change relative to Ref (Ref = 0)", title = scenario_target) +
    theme_bw() + themeds + 
    theme(legend.position = "bottom") -> Fig3.S1.LaborMetrics.delta; Fig3.S1.LaborMetrics.delta
  
  Write_png(Fig3.S1.LaborMetrics.delta, "Fig3.S1.LaborMetrics.delta", DIR_MODULE, w = 14, h = 6, r = 300)
  
  plot.wage.10 %>% 
    group_by(year, sector, region, variable) %>% 
    mutate(index = 100 * value / value[scenario == "Ref"] - 100)  %>% 
    filter(grepl(scenario_target, scenario)) %>% 
    filter(variable == "wage") %>% 
    ggplot() +
    geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.8) +
    geom_line(aes(x = year, y = index, color = sector), linewidth = 1) +
    facet_wrap( ~ region, ncol = 5) +
    labs(x = "", y = "% change relative to Ref (Ref = 0)", title = scenario_target) +
    theme_bw() + themeds 
  
  
  plot.wage.10 %>% 
    group_by(year, sector, region, variable) %>% 
    mutate(delta = value - value[scenario == "Ref"])  %>% 
    filter(variable == "VAL") ->
    R2L.10 

  
  #### Return to land ----
  
  PluckBind("Land") %>%  # thous km2
    select(scenario, region, LandLeaf, year, value) %>% 
    mutate(variable = "land") %>% 
    bind_rows(PluckBind("ProfitRate") %>% # $1975/thous km2
                select(scenario, region, LandLeaf, year, value) %>% 
                mutate(variable = "pi")) %>% 
    filter(year >= 2015) %>% 
    spread(variable, value) %>% 
    mutate(value = land * pi) ->
    df.pi
  
  splits <- strsplit(df.pi$LandLeaf, '_')
  splits <- lapply(splits, 'length<-', max(lengths(splits)))
  splits <- do.call(rbind, splits)
  df.pi[c('crop', 'WB', 'IRR', 'mgmt')] <- splits
  
  unique(df.pi$crop)
  
  df.pi %>% 
    select(scenario, region, year, land, value, crop) %>% 
    left_join(LandMapping %>% select(crop = LandLeaf, group = LandCover3), by = "crop") %>% 
    mutate(group = ifelse(is.na(group), crop, group)) %>% 
    filter(!grepl("Rock|Urban", group)) %>% 
    mutate(group = gsub(" - Unmanaged| - Managed", "", group)) %>%
    mutate(group = gsub("Unmanaged|Managed", "", group)) %>% 
    mutate(group = gsub("Fixed", "Land", group),
           group = ifelse(grepl("wood", group), "Forest", group)) %>%  
    group_by(scenario, region, group, year) %>% 
    summarise(value = sum(value, na.rm = T),
              land = sum(land, na.rm = T)) %>% 
    mutate(value = conv_75_90*value/10^6) -> # million 1990$
    df.landvalue
  
  
  df.landvalue %>% 
    filter(group != "Other Land") %>% 
    group_by(scenario, year, region) %>%  
    summarise(value = sum(value, na.rm = T),
              land = sum(land)) %>% 
    group_by(year, region) %>% 
    mutate(index = 100 * value / value[scenario == "Ref"] - 100,
           delta = value - value[scenario == "Ref"] ) ->
    R2A.32
  
  R2A.32 %>% filter(year == 2100, scenario == scenario_target) ->
    delta.VAA.2100.r32
  
  REG %>% rename(region = reg_nm) %>%
    left_join(delta.VAA.2100.r32, by = "region") %>% 
    SCE_NAME() %>% 
    ggplot() +
    geom_sf(aes(fill = index)) +
    scale_fill_gradient2(low = "red", high = "blue", midpoint = 0) +
    coord_sf(datum = NA) + 
    # labs(title = "Relative changes in total return to agricultural land (Ref = 0)") +
    # facet_wrap(~ sector, ncol = 1) +
    theme_bw() + theme0 + theme1 +
    theme(legend.position="right",
          plot.title = element_text(size = 16,
                                    face = "bold"),
          plot.subtitle = element_text(color = "blue"))  +
    guides(fill = guide_colorbar(title = "%",
                                 title.position = "top",
                                 title.theme = element_text(size = 10,
                                                            face = "bold",
                                                            colour = "black",
                                                            angle = 0))) ->
    Fig3.R2A.32map; Fig3.R2A.32map
  
  Write_png(Fig3.R2A.32map, "Fig3.R2A.32map", DIR_MODULE, w = 6, h = 2, r = 300)
  
  
  df.landvalue %>% 
    filter(group != "Other Land") %>% 
    left_join(Regmapping %>% select(region, REG10_main), by = "region") %>% 
    group_by(scenario, year, group, region = REG10_main) %>%  
    summarise(value = sum(value, na.rm = T),
              land = sum(land)) %>% 
    group_by(year, region, group) %>% 
    mutate(index = 100 * value / value[scenario == "Ref"] - 100,
           delta = value - value[scenario == "Ref"]) ->
    R2A.10
  
  
  R2A.10 %>% 
    group_by(scenario, year, region) %>% 
    summarise(value = sum(value, na.rm = T),
              delta = sum(delta, na.rm = T),
              land = sum(land, na.rm = T)) ->
    R2A.agg.10
  
  
  R2A.10 %>% 
    filter(grepl(scenario_target, scenario)) %>% 
    mutate(region = factor(region, levels = reg_order)) %>% 
    ggplot() +
    geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.8) +
    geom_bar(aes(x = year, y = delta, fill = group),
             stat = "identity", position = "stack") +
    geom_errorbar(data = R2A.agg.10 %>% filter(grepl(scenario_target, scenario)) %>% 
                    mutate(region = factor(region, levels = reg_order)), 
                  aes(x = year, ymin = delta, ymax = delta), linetype = "dashed", linewidth = 1) +
    facet_wrap(~ region, ncol = 5) +
    # facet_wrap(~ region, ncol = 5, scales = "free_y") +
    labs(x = "", y = "million 1900$", title = "Return to land relative to Ref (Ref = 0)") +
    scale_fill_brewer(palette = "Set2") +
    theme_bw() + themeds + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) ->
    Fig3.S2.R2A.10; Fig3.S2.R2A.10
  
  Write_png(Fig3.S2.R2A.10, "Fig3.S2.R2A.10", DIR_MODULE, w = 12, h = 6, r = 300)
  
  #### Return to capital ----
  
  # energy service value = R2K_EN
  # Pk_Ag * Qk_Ag = R2K_AG
  # MA * fac-share-capital = R2K_MA
  
  PluckBind("SAM_NA") %>% 
    filter(Account %in% c("energy-service-value", "fac-share-capital", "materials-gross-output" )) %>% 
    select(-Units) %>% spread(Account, value) %>% 
    mutate(R2K_MA = `materials-gross-output` * `fac-share-capital`) %>%
    select(scenario, region, year, R2K_MA, R2K_EN = `energy-service-value`) -> 
    df.R2K_EM
  
  PluckBind("AgCapitalDemand") %>% select(scenario, region, year, value) %>% 
    mutate(variable = "Q") %>% # billion 1975$
    bind_rows(PluckBind("AgCapitalPrice") %>%  
                mutate(region = gsub("Capital_Ag", "", market)) %>% 
                select(scenario, region, year, value) %>% 
                mutate(variable = "P")) %>% # 1975$/$
    spread(variable, value) %>% 
    mutate(R2K_AG = P*Q,
           R2K_AG = conv_MIL_BIL * conv_75_90 * R2K_AG) %>% # billion 1975$ to million 1990$
    select(-P, -Q) %>% 
    left_join(df.R2K_EM, by = c("scenario", "region", "year")) ->
    R2K
  
  
  R2K %>% mutate(R2K_NonAg = R2K_MA + R2K_EN) %>% 
    select(-R2K_MA, -R2K_EN) %>% 
    gather(sector, value, R2K_AG:R2K_NonAg) %>% 
    filter(year >= 2015) %>% 
    left_join(Regmapping %>% select(region, REG10_main), by = "region") %>% 
    group_by(scenario, year, region = REG10_main, sector) %>%  
    summarise(value = sum(value, na.rm = T)) %>% 
    group_by(year, region, sector) %>% 
    mutate(delta = value - value[scenario == "Ref"],
           index = 100 * value / value[scenario == "Ref"] - 100) ->
    R2K.10
  
  
  # R2K.10 %>% ungroup() %>% 
  #   filter(scenario == scenario_target) %>% 
  #   SCE_NAME() %>% 
  #   mutate(region = factor(region, levels = reg_order)) %>% 
  #   ggplot() +
  #   geom_bar(aes(x = year, y = delta, fill = sector), 
  #            stat = "identity", position = "stack") +
  #   # facet_wrap(~ region, ncol = 5, scales = "free_y") +
  #   geom_errorbar(data = R2K.10 %>% filter(scenario == scenario_target) %>% SCE_NAME() %>% 
  #                   mutate(region = factor(region, levels = reg_order)) %>% 
  #                   group_by(scenario, year, region) %>% 
  #                   summarise(delta = sum(delta, na.rm = T)),
  #                 aes(x = year, ymin = delta, ymax = delta), linewidth = 0.8) +
  #   facet_wrap(~ region, ncol = 5) +
  #   labs(x = "", y = "change relative to Ref (million 1990$)", title = "Return to capital") +
  #   theme_bw() + themeds + 
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1),
  #         legend.position = "bottom")
  
  
  #### decomposition ----
  
  plot.wage.10 %>% 
    group_by(year, sector, region, variable) %>% 
    mutate(delta = value - value[scenario == "Ref"],
           index = 100 * value / value[scenario == "Ref"] - 100)  %>% 
    filter(variable == "VAL") %>% ungroup() %>% 
    filter(sector != "Labor_Total") %>% 
    mutate(sector = gsub("Labor_Ag", "Ag", sector ),
           sector = gsub("Labor_Materials", "NonAg", sector),
           input = "labor") %>% 
    select(-value, -variable) %>% 
    bind_rows(R2A.10 %>% group_by(scenario, year, region) %>% 
                summarise(value = sum(value, na.rm = T)) %>% 
                mutate(sector = "Ag", input = "land") %>% 
                group_by(year, sector, region) %>% 
                mutate(delta = value - value[scenario == "Ref"],
                       index = 100 * value / value[scenario == "Ref"] - 100) %>% 
                select(-value)) %>% 
    bind_rows(R2K.10 %>% filter(year >= 2015) %>% 
                mutate(delta = value - value[scenario == "Ref"]) %>% 
                mutate(sector = gsub("R2K_", "", sector), input = "capital") %>% select(-value)) %>% 
    mutate(sector = gsub("AG", "Ag", sector),
           delta = delta * CONV_90_15) -> # 1990$ to 2015$
    GVA.10
  
  ##### global ----
  
  GVA.10 %>% 
    # filter(scenario %in% scenario_target) %>% 
    filter(scenario != "Ref") %>% 
    mutate(account = paste0(sector, "_", input)) %>% 
    filter(scenario == scenario_target) %>% 
    group_by(scenario, year, sector, account) %>% 
    summarise(delta = sum(delta, na.rm = T)) %>% 
    SCE_NAME() ->
    df.plot
  
  df.plot %>% 
    group_by(scenario, year) %>% 
    summarise(delta = sum(delta, na.rm = T)) %>% 
    mutate(region = "World")->
    df.GVA
  
 
  df.plot %>% 
    ggplot(aes(x = year)) +
    geom_hline(yintercept = 0, color = "grey") +
    geom_bar(aes(y = CONV_90_15*delta/1000, fill = account),
             stat = "identity", position = "stack") +
    geom_errorbar(data = df.GVA, aes(ymin = delta/1000, ymax = delta/1000), linewidth = 1) +
    # scale_alpha_manual(values = c("AG" = 0.5, "MA" = 0.9, "EN" = 0.2)) +
    scale_fill_manual(values = paired_colors2) +
    facet_wrap(~ region, nrow = 1) +
    labs(x = "", y = "billion 2015$", fill = "Factor") +
    theme_bw() + themeds + theme(legend.position = "none") ->
  Fig3.R2KLA.glb; Fig3.R2KLA.glb
  
  Write_png(Fig3.R2KLA.glb, "Fig3.R2KLA.glb", DIR_MODULE, w = 5, h = 6, r = 300)
  
  
  ##### 10 regions ----
  
  GVA.10 %>% 
    # filter(scenario %in% scenario_target) %>% 
    filter(scenario != "Ref") %>% 
    mutate(account = paste0(sector, "_", input)) %>% 
    filter(scenario == scenario_target) %>% 
    SCE_NAME() %>% 
    mutate(region = factor(region, levels = reg_order)) ->
    df.plot
  
  df.plot %>% 
    group_by(scenario, region, year) %>% 
    summarise(delta = sum(delta, na.rm = T)) %>% 
    mutate(region = factor(region, levels = reg_order)) ->
    df.GVA
  
  
  df.plot %>% 
    ggplot(aes(x = year)) +
    geom_hline(yintercept = 0, color = "grey") +
    geom_bar(aes(y = CONV_90_15*delta/1000, fill = account),
             stat = "identity", position = "stack") +
    geom_errorbar(data = df.GVA, aes(ymin = CONV_90_15*delta/1000, ymax = CONV_90_15*delta/1000), linewidth = 1) +
    # scale_alpha_manual(values = c("AG" = 0.5, "MA" = 0.9, "EN" = 0.2)) +
    scale_fill_manual(values = paired_colors2) +
    facet_wrap(~ region, nrow = 2) +
    labs(x = "", y = "billion 2015$", 
         fill = "Factor") +
    theme_bw() + themeds ->
    Fig3.R2KLA.10; Fig3.R2KLA.10
  
  Write_png(Fig3.R2KLA.10, "Fig3.R2KLA.10", DIR_MODULE, w = 12, h = 6, r = 300)
  
  
  df.plot %>% 
    ggplot(aes(x = year)) +
    geom_hline(yintercept = 0, color = "grey") +
    geom_bar(aes(y = CONV_90_15*delta/1000, fill = account),
             stat = "identity", position = "stack") +
    geom_errorbar(data = df.GVA, aes(ymin = CONV_90_15*delta/1000, ymax = CONV_90_15*delta/1000), linewidth = 1) +
    # scale_alpha_manual(values = c("AG" = 0.5, "MA" = 0.9, "EN" = 0.2)) +
    scale_fill_manual(values = paired_colors2) +
    facet_wrap(~ region, nrow = 2, scales = "free_y") +
    labs(x = "", y = "billion 2015$", 
         title = "GDP response decomposition: income method, Combined_LS",
         fill = "Factor") +
    theme_bw() + themeds + theme(legend.position = "none") ->
    Fig3.R2KLA.10; Fig3.R2KLA.10
  
  Write_png(Fig3.R2KLA.10, "Fig3.R2KLA.10.freey", DIR_MODULE, w = 14, h = 6, r = 300)
  
  
  ### Land   ----
  
  PluckBind("Aggland") %>% filter(year >= 2015) %>% 
    left_join_error_no_match(LandMapping %>% select(LandLeaf, land = LandCover6), by = "LandLeaf") %>% 
    group_by(scenario, region, land, year, branch) %>%
    # to Mha
    summarise(value = sum(value)/10, .groups = "drop") %>%
    Agg_reg(land, region) %>% 
    filter(year >= 2015) %>% 
    filter(!grepl("Other Fixed", land)) %>%
    mutate(land = gsub("Fixed", "Land", land)) %>% 
    mutate(land = ifelse(land %in% c("Biomass", "Corn", "Legumes", "Other crops",
                                     "Other grains", "Rice", "Soybean", "Vege&Fruits", "Wheat" ),
                         "Cropland", land)) %>% 
    group_by_at(vars(-value)) %>% summarise(value = sum(value), .groups = "drop") -> Pland
  
  Pland %>% 
    group_by(scenario, land, year) %>% 
    summarise(value = sum(value, na.rm = T)) %>% 
    group_by(land, year) %>% 
    mutate(delta = value - value[scenario == "Ref"],
           index = 100 * value / value[scenario == "Ref"] - 100) %>% 
    filter(scenario == scenario_target) %>% 
    SCE_NAME() ->
    delta.cropland.2100.glb
  
  delta.cropland.2100.glb %>% 
    mutate(region = "World") %>% 
    ggplot() +
    geom_bar(aes(x = year, y = delta, fill = land), 
             stat = "identity", position = "stack") +
    labs(x = "", y = "Mha", fill = "") +
    # labs(title = "Absolute changes in land use (Ref = 0)") +
    facet_wrap(~ region) +
    theme_bw() + themeds  +
    # theme(legend.position = c(0.22, 0.18)) +
    theme(legend.position = "none") +
    scale_fill_manual(values = land_colors) ->
    Fig3.LUC.glb; Fig3.LUC.glb
  
  Write_png(Fig3.LUC.glb, "Fig3.LUC.glb", DIR_MODULE, w = 5, h = 6, r = 300)
  
  
  Pland %>% 
    filter(land == "Cropland") %>% 
    group_by(land, region, year) %>% 
    mutate(delta = value - value[scenario == "Ref"],
           index = 100 * value / value[scenario == "Ref"] - 100) %>% 
    filter(year == 2100,
           scenario == scenario_target) %>% 
    SCE_NAME() ->
    delta.cropland.2100.r32
  
  REG %>% rename(region = reg_nm) %>% 
    left_join(delta.cropland.2100.r32) %>% 
    ggplot() +
    geom_sf(aes(fill = index)) +
    scale_fill_gradient2(low = "red", high = "blue", midpoint = 0) +
    coord_sf(datum = NA) + 
    # labs(title = "Relative changes in cropland (Ref = 0)") +
    theme_bw() + theme0 + theme1 +
    theme(legend.position="right",
          plot.title = element_text(size = 16,
                                    face = "bold"),
          plot.subtitle = element_text(color = "blue"))  +
    guides(fill = guide_colorbar(title = "%",
                                 title.position = "top",
                                 title.theme = element_text(size = 10,
                                                            face = "bold",
                                                            colour = "black",
                                                            angle = 0))) ->
    Fig3.cropland.32map.pct; Fig3.cropland.32map.pct
  
  Write_png(Fig3.cropland.32map.pct, "Fig3.cropland.32map.pct", DIR_MODULE, w = 6, h = 2, r = 300)
  
  
  
  REG %>% rename(region = reg_nm) %>% 
    left_join(delta.cropland.2100.r32) %>% 
    ggplot() +
    geom_sf(aes(fill = delta)) +
    scale_fill_gradient2(low = "red", high = "blue", midpoint = 0) +
    coord_sf(datum = NA) + 
    # labs(title = "Absolute changes in cropland (Ref = 0)") +
    theme_bw() + theme0 + theme1 +
    theme(legend.position="right",
          plot.title = element_text(size = 16,
                                    face = "bold"),
          plot.subtitle = element_text(color = "blue"))  +
    guides(fill = guide_colorbar(title = "Mha",
                                 title.position = "top",
                                 title.theme = element_text(size = 10,
                                                            face = "bold",
                                                            colour = "black",
                                                            angle = 0))) ->
    Fig3.cropland.32map.mha; Fig3.cropland.32map.mha
  
  Write_png(Fig3.cropland.32map.mha, "Fig3.cropland.32map.mha", DIR_MODULE, w = 6, h = 2, r = 300)
  
  
  Pland %>% 
    left_join(Regmapping %>% select(region, REG10_main), by = "region") %>% 
    group_by(scenario, year, region = REG10_main, land) %>% 
    summarise(value = sum(value, na.rm = T)) %>% 
    group_by(land, region, year) %>% 
    mutate(delta = value - value[scenario == "Ref"]) %>% 
    filter(scenario == scenario_target) %>% 
    SCE_NAME() %>% 
    mutate(region = factor(region, levels = reg_order)) %>% 
    ggplot() +
    geom_bar(aes(x = year, y = delta, fill = land), 
             stat = "identity", position = "stack") +
    facet_wrap(~ region, ncol = 5) +
    labs(x = "", y = "Mha", fill = "") +
    # labs(title = "Absolute changes in land use (Ref = 0): Combined") +
    theme_bw() + themeds +
    scale_fill_manual(values = land_colors)  ->
    Fig3.LUC.10; Fig3.LUC.10
    
  Write_png(Fig3.LUC.10, "Fig3.LUC.10", DIR_MODULE, w = 12, h = 6, r = 300)
  
  
  Pland %>% 
    left_join(Regmapping %>% select(region, REG10_main), by = "region") %>% 
    group_by(scenario, year, region = REG10_main, land) %>% 
    summarise(value = sum(value, na.rm = T)) %>% 
    group_by(land, region, year) %>% 
    mutate(delta = value - value[scenario == "Ref"]) %>% 
    filter(scenario != "Ref") %>% 
    SCE_NAME() %>% 
    mutate(region = factor(region, levels = reg_order)) %>% 
    ggplot() +
    geom_bar(aes(x = year, y = delta, fill = land), 
             stat = "identity", position = "stack") +
    facet_grid(region~ scenario, scales = "free_y") +
    labs(x = "", y = "Mha", 
         title = "Absolute changes in land use (Ref = 0): Combined") +
    theme_bw() + themeds  +
    scale_fill_manual(values = land_colors) ->
    Fig3.LUC.10.scenario; Fig3.LUC.10.scenario
  
  Write_png(Fig3.LUC.10.scenario, "Fig3.LUC.10.scenario", DIR_MODULE, w = 14, h = 18, r = 300)
  

### AG K-L ----
  
  PluckBind("AgCapitalDemand") %>% 
    select(scenario, region, year, AgK = value) %>% # billion 1975$
    filter(year >= 2015) %>% 
    left_join_error_no_match(df.labor %>% filter(sector == "Labor_Ag") %>% 
                               select(scenario, region, year, AgL = value), # million people
                             by = c("scenario", "region", "year")) %>% 
    left_join_error_no_match(Regmapping %>% select(region, REG10_main)) %>% 
    group_by(scenario, region = REG10_main, year) %>% 
    summarise(AgK = sum(AgK, na.rm = T),
              AgL = sum(AgL, na.rm = T)) %>% 
    mutate(K_L = AgK / AgL) %>% # bil 1975$ / mil people = thousand 1975$ per labor
    filter(scenario %in% c("Ref", "CL_LS")) %>% 
    SCE_NAME() %>% 
    ggplot() +
    geom_line(aes(x = year, y = K_L, color = scenario), linewidth = 0.8) +
    facet_wrap(~ region, ncol = 5, scales = "free_y") +
    labs(x = "", y = "thousand 1975$ per labor", 
         title = "Agricultural capital-labor ratio") +
    theme_bw() + themeds
  
  
  PluckBind("AgCapitalDemand") %>% 
    select(scenario, region, year, AgK = value) %>% # billion 1975$
    filter(year >= 2015) %>% 
    left_join_error_no_match(df.labor %>% filter(sector == "Labor_Ag") %>% 
                               select(scenario, region, year, AgL = value), # million people
                             by = c("scenario", "region", "year")) %>% 
    left_join_error_no_match(Regmapping %>% select(region, REG10_main)) %>% 
    group_by(scenario, region = REG10_main, year) %>% 
    summarise(AgK = sum(AgK, na.rm = T),
              AgL = sum(AgL, na.rm = T)) %>% 
    mutate(K_L = AgK / AgL) %>% # bil 1975$ / mil people = thousand 1975$ per labor
    group_by(region, year) %>% 
    mutate(delta = K_L - K_L[scenario == "Ref"],
           index = K_L / K_L[scenario == "Ref"]) %>% 
    filter(scenario == scenario_target) %>% 
    SCE_NAME() %>% 
    mutate(region = factor(region, levels = reg_order)) %>%
    ggplot() +
    geom_line(aes(x = year, y = index), linewidth = 0.8) +
    facet_wrap(~ region, ncol = 5) +
    labs(x = "", y = "index", 
         title = "Agricultural capital-labor ratio relative to Ref (Ref = 1)") +
    theme_bw() + themeds
  
  PluckBind("AgCapitalDemand") %>% 
    select(scenario, region, year, AgK = value) %>% # billion 1975$
    filter(year >= 2015) %>% 
    left_join_error_no_match(df.labor %>% filter(sector == "Labor_Ag") %>% 
                               select(scenario, region, year, AgL = value), # million people
                             by = c("scenario", "region", "year")) %>% 
    left_join_error_no_match(Regmapping %>% select(region, REG10_main)) %>% 
    group_by(scenario, region = REG10_main, year) %>% 
    summarise(AgK = sum(AgK, na.rm = T),
              AgL = sum(AgL, na.rm = T)) %>% 
    mutate(K_L = AgK / AgL) %>% # bil 1975$ / mil people = thousand 1975$ per labor
    group_by(scenario, region) %>% 
    mutate(trend = K_L / K_L[year == 2015]) %>% 
    filter(scenario %in% c("Ref", "CLA_LS")) %>%
    SCE_NAME() %>% 
    mutate(region = factor(region, levels = reg_order)) %>%
    ggplot() +
    geom_hline(yintercept = 1, linetype = "dotted") +
    geom_line(aes(x = year, y = trend, color = scenario), linewidth = 0.8) +
    facet_wrap(~ region, ncol = 5) +
    labs(x = "", y = "index", 
         title = "Agricultural capital-labor ratio relative to 2015") +
    theme_bw() + themeds ->
  Fig3.S1.Ag_KL_ratio.10; Fig3.S1.Ag_KL_ratio.10
  
  Write_png(Fig3.S1.Ag_KL_ratio.10, "Fig3.S1.Ag_KL_ratio.10", DIR_MODULE, w = 12, h = 6, r = 300)
  
  
  ### technology adoption rate ----
  # soi <- "LA"
  soi <- "CLA_LS"
  # X-hi technology is more capital-intensive compared to X-lo technology
  df.pi %>% 
    filter(!is.na(mgmt)) %>% 
    left_join_error_no_match(Regmapping %>% select(region, REG10_main), by = "region") %>% 
    group_by(scenario, region = REG10_main, year, crop, IRR, mgmt) %>% 
    summarise(land = sum(land, na.rm =T)) %>% 
    group_by(scenario, region, crop, year, IRR, mgmt) %>% 
    summarise(land = sum(land, na.rm = T)) %>% 
    group_by(scenario, region, crop, IRR, year) %>% 
    mutate(total = sum(land, na.rm = T)) %>% 
    mutate(share = land / total) %>% 
    group_by(region, crop, year, IRR) %>% 
    mutate(delta = share - share[scenario == "Ref"]) %>% 
    filter(scenario == soi) %>% 
    filter(mgmt == "hi") %>% 
    SCE_NAME() %>% 
    ggplot(aes(weight = land)) +
    geom_smooth(aes(x = year, y = 100*delta, color = IRR), se = F) +
    geom_hline(yintercept = 0) +
    facet_wrap(~ region, ncol = 5) +
    labs(x = "", y = "% point", title = paste0("Capital-intensive technology adoption share relative to Ref (Ref = 0): ",  soi)) +
    # scale_color_manual(values = scenario_colors) +
    theme_bw() + themeds
  
  df.pi %>% 
    filter(!is.na(mgmt)) %>% 
    left_join_error_no_match(Regmapping %>% select(region, REG10_main), by = "region") %>% 
    group_by(scenario, region = REG10_main, year, IRR, mgmt) %>% 
    summarise(land = sum(land, na.rm =T)) %>% 
    group_by(scenario, region, year, IRR, mgmt) %>% 
    summarise(land = sum(land, na.rm = T)) %>% 
    group_by(scenario, region, IRR, year) %>% 
    mutate(total = sum(land, na.rm = T)) %>% 
    mutate(share = land / total) %>% 
    group_by(region, year, IRR) %>% 
    mutate(delta = share - share[scenario == "Ref"]) %>% 
    filter(scenario %in% c("Ref", scenario_target)) %>% 
    filter(mgmt == "hi") %>% 
    filter(year == 2100) %>% 
    mutate(region = factor(region, levels = reg_order)) %>% 
    SCE_NAME() %>% 
    ggplot() +
    geom_bar(aes(x = IRR, y = 100*delta, fill = IRR), 
             stat = "identity", position = "dodge") +
    geom_hline(yintercept = 0) +
    facet_wrap(~ region, ncol = 5) +
    # scale_alpha_manual(values = c("Ref" = 0.5, "Labor" = 0.9)) +
    labs(x = "", y = "% percentage", title = "Absolute changes in capital-intensive technology adoption rate (Ref = 0): 2100") +
    # scale_color_manual(values = scenario_colors) +
    theme_bw() + themeds ->
  Fig3.S1.tech_adoption.2100.r10; Fig3.S1.tech_adoption.2100.r10
  
  Write_png(Fig3.S1.tech_adoption.2100.r10, "Fig3.S1.tech_adoption.2100.r10", DIR_MODULE, w = 12, h = 6, r = 300)
  
  # df.pi %>% 
  #   filter(!is.na(mgmt)) %>% 
  #   left_join_error_no_match(Regmapping %>% select(region, REG10_main), by = "region") %>% 
  #   group_by(scenario, region = REG10_main, year, crop, IRR, mgmt) %>% 
  #   summarise(land = sum(land, na.rm =T)) %>% 
  #   group_by(scenario, region, crop, year, mgmt) %>% 
  #   summarise(land = sum(land, na.rm = T)) %>% 
  #   group_by(scenario, region, crop, year) %>% 
  #   mutate(total = sum(land, na.rm = T)) %>% 
  #   mutate(share = land / total) %>% 
  #   group_by(region, crop, year) %>% 
  #   mutate(delta = share - share[scenario == "Ref"]) %>% 
  #   filter(scenario == "L_Dunne") %>% 
  #   filter(mgmt == "hi") %>% 
  #   SCE_NAME() %>% 
  #   ggplot(aes(weight = land)) +
  #   geom_smooth(aes(x = year, y = delta)) +
  #   geom_hline(yintercept = 0) +
  #   facet_wrap(~ region, ncol = 5) +
  #   # scale_color_manual(values = scenario_colors) +
  #   theme_bw() + themeds
  
  
  
  PluckBind("AgCapitalDemand") %>% 
    select(scenario, region, year) %>% 
    mutate(input = "capital") %>% 
    head
  
  Pland %>% 
    filter(land == "Cropland") %>% 
    group_by(scenario, region, year) %>% 
    summarise(value = sum(value, na.rm = T)) %>% 
    mutate(input = "land") %>% head
  
  
  
  # Output market  responses  ----
  
  ## Figure 4 ----
  
  ### GDP decomposition: expenditure method ----
  
  #### Investment ----
  
  INV_s <- PluckBind("SAM_s_invest") %>% 
    mutate(Units = "billion 1975$ per timestep")
  INV_r <- PluckBind("SAM_r_invest") %>% 
    mutate(Units = "billion 1975$ per timestep")
  
  
  INV_s %>% filter(sector == "Capital_Ag") %>% 
    mutate(value = conv_75_90 * conv_MIL_BIL * value / 5,
           Units = "million 1990$",
           Account = "AG-investment") %>% 
    select(scenario, region, Account, Units, year, value) %>% 
    bind_rows(PluckBind("SAM_NA") %>% 
                filter(grepl("investment", Account))  %>% 
                select(scenario, region, Account, Units, year, value)) %>% 
    bind_rows(PluckBind("SAM_NA") %>% 
                filter(Account %in% c("consumer-durable", "capital-net-export", "savings"))  %>% 
                select(scenario, region, Account, Units, year, value)) -> df.INV
  
  
  df.INV %>% 
    mutate(Account = gsub("materials-capital", "MA", Account),
           Account = gsub("gcam", "AE", Account)) %>%
    spread(Account, value) %>% 
    mutate(demand = `AE-investment` + `MA-investment` + `consumer-durable`,
           supply = savings + `capital-net-export`, # rename capital-net-export to FDI
           `EN-investment` = supply - `AG-investment` - `MA-investment` - `consumer-durable`,
           check = demand / supply) %>% 
    select(-Units) -> check0
  
  summary(check0 %>% filter(year >= 2020))
  
  
  df.INV %>% 
    mutate(Account = gsub("materials-capital", "MA", Account),
           Account = gsub("gcam", "AE", Account)) %>%
    spread(Account, value) %>% 
    mutate(demand = `AE-investment` + `MA-investment` + `consumer-durable`,
           supply = savings + `capital-net-export`, # rename capital-net-export to FDI
           EN = supply - `AG-investment` - `MA-investment` - `consumer-durable`,
           CD = `consumer-durable`,
           AG = `AG-investment`,
           MA = `MA-investment`,
           FDI = `capital-net-export`,
           NonAg = EN + MA + CD) %>% 
    select(scenario, region, year, AG, NonAg) %>% 
    gather(sector, value, AG: NonAg) %>%  
    group_by(region, year, sector) %>% 
    mutate(delta = value - value[scenario == "Ref"]) %>% 
    filter(year >= 2020) ->
    plot.INV
  
  plot.INV %>% 
    left_join_error_no_match(Regmapping %>% select(region, REG10_main), by = "region") %>% 
    group_by(scenario, region = REG10_main, year) %>%  
    summarise(delta = sum(delta, na.rm = T)) %>% 
    filter(scenario == scenario_target) %>% 
    SCE_NAME() ->
    INV.demand.10
  
  plot.INV %>% 
    left_join_error_no_match(Regmapping %>% select(region, REG10_main)) %>% 
    group_by(scenario, region = REG10_main, year, sector) %>%  
    summarise(delta = sum(delta, na.rm = T)) %>% 
    filter(scenario == scenario_target) %>% 
    SCE_NAME() %>% 
    mutate(region = factor(region, levels = reg_order),
           sector = ifelse(sector == "AG", "Ag", "Non-Ag")) %>% 
    ggplot() +
    geom_bar(aes(x = year, y = CONV_90_15*delta/10^3, fill = sector),
             stat = "identity", position = "stack") +
    geom_errorbar(data = INV.demand.10 %>% mutate(region = factor(region, levels = reg_order)), 
                  aes(x = year, ymin = CONV_90_15*delta / 10^3, ymax = CONV_90_15*delta / 10^3)) + 
    facet_wrap(~ region, ncol = 5) +
    labs(x = "", y = "billion 2015$") +
    # labs(title = "Absolute changes in investment relative (Ref = 0)") +
    scale_fill_brewer(palette = "Set2") +
    theme_bw() + themeds ->
    Fig4.INV.10; Fig4.INV.10
  
  Write_png(Fig4.INV.10, "Fig4.INV.10", DIR_MODULE, w = 14, h = 6, r = 300)
  

  
  #### Exports ----
  traded_prices <- PluckBind("Trade_Price") %>% 
    mutate(commodity = gsub("USAtraded ", "", market)) %>%
    select(Units, scenario, year, commodity, value)
  
  # by commodity
  ag_en_exports_value <- PluckBind("AE_EX") %>%
    # remove iron and steel since it isn't primary sector
    filter(!input %in% c("iron and steel")) %>%
    filter(!grepl("statistical differences", subsector)) %>%
    rename(exports = value, Units_Q = Units) %>% 
    separate(subsector, into = c("region", "commodity"),
             sep = " traded ") %>% 
    left_join(traded_prices %>% rename(price = value, Units_P = Units),
              by = c("scenario", "commodity", "year")) %>% 
    mutate(value = price*exports*conv_75_90*conv_MIL_BIL,
           Unit = "million $1990") %>%
    select(scenario, region, commodity, year, value, Unit, price, exports)
  
  #### Imports ----
  
  # by commodity
  en_imports_value <- PluckBind("EN_IM") %>% 
    # filter for imports and
    # remove iron and steel since it isn't primary sector
    filter(grepl("traded", input)) %>%
    filter(sector != "regional iron and steel") %>%
    rename(imports = value, Units_Q = Units) %>%
    mutate(commodity = gsub("imported ", "", technology),
           commodity = gsub("crude ", "", commodity)) %>%
    left_join(traded_prices %>% rename(price = value, Units_P = Units),
              by = c("scenario", "commodity", "year")) %>%
    # multiply price by quantity and convert to million 1990$ 
    mutate(value = price*imports*conv_75_90*conv_MIL_BIL,
           Unit = "million $1990") %>%
    select(scenario, region, commodity, year, value, Unit, price, imports)
  
  
  ag_imports_value <- PluckBind("AG_IM") %>% 
    # filter for imports and
    # remove iron and steel since it isn't primary sector
    filter(grepl("traded", input)) %>%
    filter(sector != "regional iron and steel") %>%
    rename(imports = value, Units_Q = Units) %>%
    mutate(commodity = gsub("imported ", "", subsector)) %>% 
    left_join(traded_prices %>% rename(price = value, Units_P = Units),
              by = c("scenario", "commodity", "year")) %>% 
    # multiply price by quantity and convert to billion 2015 USD
    mutate(value = price*imports*conv_75_90*conv_MIL_BIL,
           Unit = "million $1990") %>%
    select(scenario, region, commodity, year, value, Unit, price, imports)
  
  
  ag_en_imports_value <- bind_rows(en_imports_value, ag_imports_value)
  
  
  # test: check that total global trade is balanced
  
  # aggregate across all primary commodities
  ag_en_exports_value_total <- ag_en_exports_value %>%
    group_by(scenario, region, year, Unit) %>%
    summarize(value = sum(value)) %>%
    ungroup() %>%
    mutate(Variable = "GDP|Exports|Primary") %>%
    select(scenario, region, year, Variable, value, Unit)
  
  # aggregate across all primary commodities
  ag_en_imports_value_total <- ag_en_imports_value %>%
    group_by(scenario, region, year, Unit) %>%
    summarize(value = sum(value)) %>%
    ungroup() %>%
    mutate(Variable = "GDP|Imports|Primary") %>%
    select(scenario, region, year, Variable, value, Unit)
  
  
  ag_en_imports_value_total %>%
    rename(imports = value) %>%
    full_join(ag_en_exports_value_total %>% rename(exports = value),
              by = c("scenario", "region", "year")) %>%
    group_by(scenario, year) %>%
    summarize(imports = sum(imports, na.rm = T),
              exports = sum(exports, na.rm = T)) %>%
    ungroup() %>%
    mutate(net_trade = exports / imports) %>% 
    filter(year >= 2020) %>% summary() # net_trade = 1
  
  ##### check AE trade balance ----
  
  ag_en_imports_value %>% select(-imports, -price) %>% mutate(account = "import") %>% 
    bind_rows(ag_en_exports_value %>% select(-exports, -price) %>% mutate(account = "export")) %>% 
    filter(year >= 2020) %>% 
    spread(account, value) %>% 
    group_by(scenario, commodity, year, Unit) %>% 
    summarise(export = sum(export, na.rm = T),
              import= sum(import, na.rm = T)) %>% 
    mutate(net.trade = export / import) %>% summary() # net.trade = 1
  
  
  ag_en_imports_value %>% select(-imports, -price) %>% mutate(account = "import") %>% 
    bind_rows(ag_en_exports_value %>% select(-exports, -price) %>% mutate(account = "export")) %>% 
    filter(year >= 2020) %>% 
    spread(account, value) %>% 
    group_by(scenario, year, Unit) %>% 
    summarise(export = sum(export, na.rm = T),
              import= sum(import, na.rm = T)) %>% 
    mutate(net.trade = export / import) %>% summary() # net.trade = 1
  
  
  ag_en_imports_value %>% select(-imports, -price) %>% mutate(account = "import") %>% 
    bind_rows(ag_en_exports_value %>% select(-exports, -price) %>% mutate(account = "export")) %>% 
    filter(commodity %in% c("beef", "corn", "dairy", "fibercrop", "fruits", "industrial_roundwood",
                            "legumes", "misccrop", "nuts_seeds", "oilcrop", "oilpalm", "othergrain",
                            "pork", "poultry", "rice", "root_tuber", "sawnwood", "sheepgoat",
                            "soybean", "sugarcrop", "vegetables", "wheat", "woodpulp")) %>% 
    mutate(input = "AG") %>% 
    group_by(scenario, region, input, year, account) %>% 
    summarise(value = sum(value, na.rm = T)) %>% 
    spread(account, value) %>% 
    mutate(net.export = export - import) ->
    df.AG.NX
  
  # use gcam-net-export - AG-net-export = EN-net-export
  
  PluckBind("SAM_NA") %>% 
    # select(scenario, region, Account, year, value) %>% 
    filter(Account == "gcam-net-export") %>% 
    select(scenario, region, year, value ) %>% 
    mutate(input = "AE") ->
    df.gcam.NX
  
  
  df.gcam.NX %>% filter(year >= 2015) %>% 
    bind_rows(df.AG.NX %>% select(scenario, region, year, input, value = net.export) %>%  
                filter(year >= 2015)) %>% 
    spread(input, value) %>% 
    mutate(EN = AE - AG) %>% 
    gather(input, value, AE:EN) ->
    df.AE.NX
  
  # 10 regions trade 
  
  df.AE.NX %>% 
    left_join(Regmapping %>% select(region, REG10_main), by = "region") %>% 
    group_by(scenario, region = REG10_main, year, input) %>%
    summarise(value = sum(value, na.rm = T)) ->
    plot.trade.10
  
  
  plot.trade.10 %>% 
    group_by(region, year, input) %>% 
    mutate(delta = value - value[scenario == "Ref"]) %>% 
    filter(scenario != "Ref") ->
    delta.trade 
  
  ggplot() +
    geom_bar(data = delta.trade %>% filter(input != "AE") %>% 
               mutate(region = factor(region, levels = reg_order)), aes(x = year, y = delta, fill = input), stat = "identity", position = "stack") +
    geom_errorbar(data = delta.trade %>% filter(input == "AE") %>% 
                    mutate(region = factor(region, levels = reg_order)), aes(x = year, ymin = delta, ymax = delta), linewidth = 1) +
    facet_grid(region ~ scenario, scales = "free_y") +
    labs(x = "", y = "Net export relative to Ref (Ref = 0 million 1990$)") +
    theme_bw() + themeds +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  
  
  PluckBind("SAM_NA") %>% 
    filter(Account %in% c("GDP", "materials-gross-output", "ag-food-service-value", "gcam-net-export",
                          "ag-nonfood-service-value", "energy-service-value", "fac-share-labor", "fac-share-capital")) %>% 
    select(scenario, region, Account, year, value) %>% # "million 1990$"
    spread(Account, value) %>% 
    rename(NonFood = `ag-nonfood-service-value`, EN = `energy-service-value`, NX_AE = `gcam-net-export`, Food = `ag-food-service-value`) %>% 
    mutate(MA_VAL = `materials-gross-output` * `fac-share-labor`,
           MA_VAK = `materials-gross-output` * `fac-share-capital`) %>% 
    filter(year >= 2015) ->
    GDP.decomp 
  
  GDP.decomp %>% 
    mutate(MA = NonFood + EN + MA_VAL + MA_VAK,
           SUM = NonFood + EN + MA_VAL + MA_VAK + NX_AE + Food,
           check1 = MA / `materials-gross-output`,
           check2 = GDP / SUM) %>% summary() # expect check1 = check2 = 1

  
  #### GDP decompose: EXP ----
  
  PluckBind("SAM_NA") %>% 
    filter(Account %in% c("GDP", "investment", "ag-food-service-value", "gcam-net-export", "materials-net-export")) %>% 
    spread(Account, value) %>% 
    rename(CG_AG = `ag-food-service-value`, NX_AE = `gcam-net-export`, 
           NX_MA = `materials-net-export`, INV = `investment`) %>% 
    mutate(CG_MA = GDP - CG_AG - NX_AE - NX_MA - INV) %>% 
    gather(account, value, CG_AG:CG_MA) %>% 
    select(-ss, -branch, -Units) %>% 
    bind_rows(df.AE.NX %>% filter(input != "AE") %>% 
                mutate(input = gsub("AG", "NX_AG", input),
                       input = gsub("EN", "NX_EN", input)) %>% 
                rename(account = input)) %>% 
    filter(year >= 2015) ->
    GDP.decom.exp.32
  
  GDP.decom.exp.32 %>% spread(account, value) %>% 
    mutate(index = (NX_EN + NX_AG)/NX_AE,
           index2 = GDP / (NX_AG + CG_AG + CG_MA +NX_EN + NX_MA + INV)) %>% summary() # expect index = index2 = 1
  
  GDP.decom.exp.32 %>% 
    left_join(Regmapping %>% select(region, REG10_main), by = "region") %>% 
    group_by(scenario, year, region = REG10_main, account) %>% 
    summarise(value = sum(value, na.rm = T)) ->
    decom.GDP.EXP
  
  decom.GDP.EXP %>% 
    # filter(year %in% c(2015, 2050, 2100)) %>% 
    group_by(year, region, account) %>% 
    mutate(delta = value - value[scenario == "Ref"]) %>% 
    filter(account != "NX_AE") ->
    plot.decom.exp
  
  plot.INV %>% 
    left_join_error_no_match(Regmapping %>% select(region, REG10_main)) %>% 
    group_by(scenario, region = REG10_main, year, sector) %>%  
    summarise(delta = sum(delta, na.rm = T),
              value = sum(value, na.rm = T)) %>% 
    mutate(region = factor(region, levels = reg_order),
           account = ifelse(sector == "AG", "INV_Ag", "INV_NonAg")) ->
    plot.decom.inv
  
  plot.decom.exp %>% 
    mutate(account = gsub("AG", "Ag", account),
           account = gsub("MA", "NonAg", account),
           account = gsub("EN", "NonAg", account)) %>% 
    group_by(scenario, year, region, account) %>% 
    summarise(value = sum(value, na.rm = T),
              delta = sum(delta, na.rm = T)) %>% 
    bind_rows(plot.decom.inv %>% select(-sector)) ->
    plot.decom.exp.AGNONAG
    
  
  check %>% select(-delta) %>% spread(account, value) %>% 
    filter(year >= 2020) %>% 
    mutate(check = INV - INV_Ag - INV_NonAg) %>% summary() 
  
  plot.decom.exp.AGNONAG %>% 
    group_by(scenario, year, account) %>% 
    summarise(value = sum(value, na.rm = T),
              delta = sum(delta, na.rm = T)) %>% 
    mutate(region = "World") ->
    plot.decom.exp.glb
  
  
  ###### Key regions ----
  ggplot() +
    geom_bar(data = plot.decom.exp.AGNONAG %>% filter(!account %in% c("GDP", "INV")) %>% 
               filter(scenario == scenario_target) %>%
               SCE_NAME() %>% 
               mutate(region = factor(region, levels = reg_order)),
             aes(x = year, y = CONV_90_15 * delta/1000, fill = account), stat = "identity", position = "stack") +
    # geom_errorbar(data = plot.decom.exp.AGNONAG %>% filter(!account %in% c("GDP", "INV")) %>% 
    #                 filter(scenario == scenario_target) %>%
    #                 group_by(scenario, year, region) %>% 
    #                 summarise(delta = sum(delta, na.rm = T)) %>% 
    #                 SCE_NAME() %>% 
    #                 mutate(region = factor(region, levels = reg_order)),
    #               aes(x = year, ymin = CONV_90_15 * delta/1000, ymax = CONV_90_15 * delta/1000),
    #               linewidth = 1) +
    scale_fill_manual(values = exp_colors) +
    facet_wrap(~ region, ncol = 5, scales = "free_y") +
    labs(x = "", y = "billion 2015$", 
         # title = "GDP response decomposition: expenditure method, Combined",
         fill = "") +
    theme_bw() + themeds  ->
    Fig4.expenditure.10; Fig4.expenditure.10
  
  Write_png(Fig4.expenditure.10, "Fig4.expenditure.10.freey", DIR_MODULE, w = 14, h = 6, r = 300)
  
  
  ggplot() +
    geom_bar(data = plot.decom.exp.AGNONAG %>% filter(!account %in% c("GDP", "INV")) %>% 
               filter(scenario == scenario_target) %>%
               SCE_NAME() %>% 
               mutate(region = factor(region, levels = reg_order)),
             aes(x = year, y = CONV_90_15 * delta/1000, fill = account), stat = "identity", position = "stack") +
    # geom_errorbar(data = plot.decom.exp.AGNONAG %>% filter(!account %in% c("GDP", "INV")) %>% 
    #                 filter(scenario == scenario_target) %>%
    #                 group_by(scenario, year, region) %>% 
    #                 summarise(delta = sum(delta, na.rm = T)) %>% 
    #                 SCE_NAME() %>% 
    #                 mutate(region = factor(region, levels = reg_order)),
    #               aes(x = year, ymin = CONV_90_15 * delta/1000, ymax = CONV_90_15 * delta/1000),
    #               linewidth = 1) +
    scale_fill_manual(values = exp_colors) +
    facet_wrap(~ region, ncol = 5) +
    labs(x = "", y = "billion 2015$", 
         # title = "GDP response decomposition: expenditure method, Combined",
         fill = "") +
    theme_bw() + themeds  ->
    Fig4.expenditure.10; Fig4.expenditure.10
  
  Write_png(Fig4.expenditure.10, "Fig4.expenditure.10", DIR_MODULE, w = 14, h = 6, r = 300)
  

  
  ggplot() +
    geom_bar(data = plot.decom.exp.AGNONAG %>% filter(!account %in% c("GDP", "INV")) %>% 
               # filter(scenario == scenario_target) %>% 
               filter(scenario != "Ref") %>% 
               SCE_NAME() %>% 
               mutate(region = factor(region, levels = reg_order)),
             aes(x = year, y = CONV_90_15*delta/1000, fill = account), stat = "identity", position = "stack") +
    # facet_grid(region ~ scenario) +
    facet_grid(region~ scenario, scales = "free_y") +
    scale_fill_manual(values = exp_colors) +
    labs(x = "", y = "billion 2015$", title = "Absolute changes in GDP Ref",
         fill = "") +
    theme_bw() + themeds ->
    Fig4.S1.expenditure.10.scenario; Fig4.S1.expenditure.10.scenario
  
  Write_png(Fig4.S1.expenditure.10.scenario, "Fig4.S1.expenditure.10.scenario", DIR_MODULE, w = 8, h = 14, r = 300)
  
  
  ###### Global ----
  
  ggplot() +
    geom_hline(yintercept = 0) +
    geom_bar(data = plot.decom.exp.glb %>% filter(!account %in% c("GDP", "INV")) %>% 
               filter(scenario == scenario_target) %>% SCE_NAME(),
             aes(x = year, y = CONV_90_15*delta/1000, fill = account), stat = "identity", position = "stack") +
    geom_errorbar(data = plot.decom.exp.glb %>% filter(!account %in% c("GDP", "INV")) %>% group_by(scenario, year) %>% 
                    summarise(delta = sum(delta, na.rm = T)) %>% filter(scenario == scenario_target) %>% SCE_NAME(),
                  aes(x = year, ymin = CONV_90_15*delta / 1000, ymax = CONV_90_15*delta / 1000), linewidth = 0.8) +
    scale_fill_manual(values = exp_colors) +
    facet_wrap(~ region) +
    labs(x = "", y = "billion 2015$", fill = "Factor") +
    # labs(title = "GDP response decomposition: expenditure method, Combined") +
    theme_bw() + themeds + theme(legend.position = "none") ->
    Fig4.exp.glb; Fig4.exp.glb
  
  Write_png(Fig4.exp.glb, "Fig4.exp.glb", DIR_MODULE, w = 5, h = 6, r = 300)
  
  
  
  
  ggplot() +
    geom_hline(yintercept = 0) +
    geom_bar(data = plot.decom.exp.glb %>% filter(!account %in% c("GDP", "INV")) %>% 
               filter(scenario != "Ref") %>% SCE_NAME(),
             aes(x = year, y = CONV_90_15*delta/1000, fill = account), stat = "identity", position = "stack") +
    geom_errorbar(data = plot.decom.exp.glb %>% filter(!account %in% c("GDP", "INV")) %>% group_by(scenario, year) %>% 
                    summarise(delta = sum(delta, na.rm = T)) %>% filter(scenario != "Ref") %>% SCE_NAME(),
                  aes(x = year, ymin = CONV_90_15*delta / 1000, ymax = CONV_90_15*delta / 1000), linewidth = 0.8) +
    scale_fill_manual(values = exp_colors) +
    facet_wrap(~ scenario, nrow = 1) +
    # ylim(-100, 400) +
    labs(x = "", y = "billion 2015$", title = "GDP change relative to Ref (billion 2015$)",
         fill = "") +
    theme_bw() + themeds ->
    Fig4.S1.expenditure.global.scenario; Fig4.S1.expenditure.global.scenario
  
  Write_png(Fig4.S1.expenditure.global.scenario, "Fig4.S1.expenditure.global.scenario", DIR_MODULE, w = 8, h = 4, r = 300)
  
  
  ### AG market outputs ----
  
source("R/AgBalElement_Storage.R")

AgElement_SUA %>% filter(element == "Production") %>% 
  select(scenario, region, sector, year, element, value) %>% 
  bind_rows(AgElement_SUA %>% 
              filter(element %in% c("Bioenergy", "Feed", "Food", "Other use")) %>% 
              filter(sector != "OtherMeatFish") %>%
              mutate(sector = tolower(sector)) %>% 
              group_by(scenario, region, sector, year) %>% 
              summarise(value = sum(value, na.rm = T)) %>% 
              mutate(element = "Consumption")) %>% 
  spread(element, value) %>% 
      left_join_error_no_match(
        "Agprices" %>% PluckBind() %>%
          bind_rows("Meatprices" %>% PluckBind()) %>%
          mutate(sector = tolower(sector)) %>%
          rename(Price = value) %>% select(-Units) ) %>% 
      mutate(Revenue = Production * Price) %>% 
      gather(element, value, Revenue, Production, Consumption , Price) ->
  AgElement_AreaYieldPrice

AgElement_AreaYieldPrice %>% 
  filter(sector != "pasture") %>% 
  mutate(group = ifelse(sector %in% c("beef", "dairy","pork","poultry","sheepgoat" ), "animal", sector),
         group = ifelse(group %in% c("corn", "rice", "soybean", "wheat", "animal"), group, "others")) %>% 
  group_by(scenario, region, group, year, element) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  spread(element, value) %>% 
  mutate(Price = Revenue / Production) %>% 
  gather(account, value, Consumption:Revenue) ->
  AYPP_32

AgElement_AreaYieldPrice %>% 
  filter(sector != "pasture") %>% 
  mutate(group = ifelse(sector %in% c("beef", "dairy","pork","poultry","sheepgoat" ), "animal", sector),
         group = ifelse(group %in% c("corn", "rice", "soybean", "wheat", "animal"), group, "others")) %>% 
  group_by(scenario, region, year, element) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  spread(element, value) %>% 
  mutate(Price = Revenue / Production,
         group = "AG") %>% 
  gather(account, value, Consumption:Revenue) ->
  AGG_AYPP_32


AgElement_AreaYieldPrice %>% 
  filter(sector != "pasture") %>% 
  mutate(group = ifelse(sector %in% c("beef", "dairy","pork","poultry","sheepgoat" ), "animal", sector),
         group = ifelse(group %in% c("corn", "rice", "soybean", "wheat", "animal"), group, "others")) %>% 
  left_join_error_no_match(Regmapping %>% select(region, REG10_main), by = "region") %>% 
  group_by(scenario, region = REG10_main, group, year, element) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  spread(element, value) %>% 
  mutate(Price = Revenue / Production) %>%
  gather(account, value, Consumption:Revenue) ->
  AYPP_10
  

AgElement_AreaYieldPrice %>% 
  filter(sector != "pasture") %>% 
  mutate(group = ifelse(sector %in% c("beef", "dairy","pork","poultry","sheepgoat" ), "animal", sector),
         group = ifelse(group %in% c("corn", "rice", "soybean", "wheat", "animal"), group, "others")) %>% 
  left_join_error_no_match(Regmapping %>% select(region, REG10_main), by = "region") %>% 
  group_by(scenario, region = REG10_main, year, element) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  spread(element, value) %>% 
  mutate(Price = Revenue / Production,
         group = "AG") %>% 
  gather(account, value, Consumption:Revenue) ->
  AGG_AYPP_10

AgElement_AreaYieldPrice %>% 
  filter(sector != "pasture") %>% 
  mutate(group = ifelse(sector %in% c("beef", "dairy","pork","poultry","sheepgoat" ), "animal", sector),
         group = ifelse(group %in% c("corn", "rice", "soybean", "wheat"), "Key" ,group),
         group = ifelse(group %in% c("Key", "animal"), group, "others")) %>%  
  left_join_error_no_match(Regmapping %>% select(region, REG10_main), by = "region") %>% 
  group_by(scenario, group, region = REG10_main, year, element) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  spread(element, value) %>% 
  mutate(Price = Revenue / Production) %>% 
  gather(account, value, Consumption:Revenue) ->
  Key_AYPP_10


AgElement_AreaYieldPrice %>% 
  filter(sector != "pasture") %>% 
  mutate(group = ifelse(sector %in% c("beef", "dairy","pork","poultry","sheepgoat" ), "animal", sector),
         group = ifelse(group %in% c("corn", "rice", "soybean", "wheat", "animal"), group, "others")) %>% 
  group_by(scenario, group, year, element) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  spread(element, value) %>% 
  mutate(Price = Revenue / Production) %>%
  gather(account, value, Consumption:Revenue) ->
  AYPP_glb


AgElement_AreaYieldPrice %>% 
  filter(sector != "pasture") %>% 
  mutate(group = ifelse(sector %in% c("beef", "dairy","pork","poultry","sheepgoat" ), "animal", sector),
         group = ifelse(group %in% c("corn", "rice", "soybean", "wheat"), "Key" ,group),
         group = ifelse(group %in% c("Key", "animal"), group, "others")) %>%  
  group_by(scenario, group, year, element) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  spread(element, value) %>% 
  mutate(Price = Revenue / Production) %>% 
  gather(account, value, Consumption:Revenue) ->
  Key_AYPP_glb

AgElement_AreaYieldPrice %>% 
  filter(sector != "pasture") %>% 
  mutate(group = ifelse(sector %in% c("beef", "dairy","pork","poultry","sheepgoat" ), "animal", sector),
         group = ifelse(group %in% c("corn", "rice", "soybean", "wheat", "animal"), group, "others")) %>% 
  group_by(scenario, year, element) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  spread(element, value) %>% 
  mutate(Price = Revenue / Production,
         group = "AG") %>%
  gather(account, value, Consumption:Revenue) ->
  AGG_AYPP_glb


AGG_AYPP_32 %>% 
  group_by(region, group, year, account) %>% 
  mutate(index = 100*(value / value[scenario == "Ref"] - 1),
         index = ifelse(is.infinite(index), NA, index),
         delta = value - value[scenario == "Ref"]) ->
  agg_AYPP_change_32


AGG_AYPP_10 %>% 
  group_by(region, group, year, account) %>% 
  mutate(index = 100*(value / value[scenario == "Ref"] - 1),
         index = ifelse(is.infinite(index), NA, index),
         delta = value - value[scenario == "Ref"]) ->
  agg_AYPP_change_10

AYPP_10 %>% 
  group_by(region, group, year, account) %>% 
  mutate(index = 100*(value / value[scenario == "Ref"] - 1),
         index = ifelse(is.infinite(index), NA, index),
         delta = value - value[scenario == "Ref"]) ->
  AYPP_change_10

Key_AYPP_10 %>% 
  group_by(region, group, year, account) %>% 
  mutate(index = 100*(value / value[scenario == "Ref"] - 1),
         index = ifelse(is.infinite(index), NA, index),
         delta = value - value[scenario == "Ref"]) ->
  Key_AYPP_change_10


AGG_AYPP_glb %>% 
  group_by(group, year, account) %>% 
  mutate(index = 100*(value / value[scenario == "Ref"] - 1),
         index = ifelse(is.infinite(index), NA, index),
         delta = value - value[scenario == "Ref"]) %>% 
  mutate(region = "World") ->
  agg_AYPP_change_glb

Key_AYPP_10 %>% 
  group_by(region, group, year, account) %>% 
  mutate(index = 100*(value / value[scenario == "Ref"] - 1),
         index = ifelse(is.infinite(index), NA, index),
         delta = value - value[scenario == "Ref"]) ->
  Key_AYPP_change_10

Key_AYPP_glb %>% 
  mutate(region = "World") %>% 
  group_by(region, group, year, account) %>% 
  mutate(index = 100*(value / value[scenario == "Ref"] - 1),
         index = ifelse(is.infinite(index), NA, index),
         delta = value - value[scenario == "Ref"]) ->
  Key_AYPP_change_glb


#### plot ----
Key_AYPP_change_10 %>% 
  bind_rows(agg_AYPP_change_10) %>% 
  bind_rows(Key_AYPP_change_glb) %>% 
  bind_rows(agg_AYPP_change_glb) %>%
  filter(year == 2100) %>% 
  filter(scenario == scenario_target) %>% 
  mutate(region = factor(region, levels = c(rev(reg_order), "World")),
         group = gsub("animal", "Livestock", group),
         group = gsub("others", "Other crops", group),
         group = gsub("Key", "Key crops", group),
         group = gsub("AG", "Agriculture", group),
         group = factor(group, levels = c("Agriculture", "Key crops",
                                          "Other crops", "Livestock")),
         account = factor(account, levels = c("Production", "Price",
                                          "Revenue", "Consumption"))) %>% 
  SCE_NAME() %>% 
  ggplot() +
  geom_tile(aes(y = region, x = group, fill = index)) +
  scale_fill_gradient2(low = "#E69F00", mid = "white", high = "#009E73", midpoint = 0,
                       limits = c(-60, 60)) +
  facet_grid(~ account) +
  # facet_grid(group ~ region, scales = "free_y") +
  labs(x = "", y = "", fill = "%") +
  # labs(title = "Relative changes in agricultural market outcomes (Ref = 0):\n2100") +
  theme_bw() + themeds ->
Fig4.AGPQR.10; Fig4.AGPQR.10

Write_png(Fig4.AGPQR.10, "Fig4.AGPQR.10", DIR_MODULE, w = 10, h = 6, r = 300)



# TRADE plots ----

PluckBind("AG_IM") %>% 
  # filter for imports and
  # remove iron and steel since it isn't primary sector
  filter(grepl("traded", input)) %>%
  filter(sector != "regional iron and steel") %>%
  rename(imports = value, Units_Q = Units) %>%
  mutate(commodity = gsub("imported ", "", subsector)) %>% 
  left_join(traded_prices %>% rename(price = value, Units_P = Units),
            by = c("scenario", "commodity", "year")) %>% 
  # multiply price by quantity and convert to billion 2015 USD
  mutate(value = price*imports*conv_75_90*conv_MIL_BIL,
         Unit = "million $1990") %>% 
  group_by(scenario, region, commodity, year, Units_Q , Unit) %>% 
  summarise(imports = sum(imports, na.rm = T),
            IM_value = sum(value, na.rm = T)) ->
  df.import.ag

PluckBind("EN_IM") %>% 
  # filter for imports and
  # remove iron and steel since it isn't primary sector
  filter(grepl("traded", input)) %>%
  filter(sector != "regional iron and steel") %>%
  rename(imports = value, Units_Q = Units) %>%
  mutate(commodity = gsub("imported ", "", technology),
         commodity = gsub("crude ", "", commodity)) %>%
  left_join(traded_prices %>% rename(price = value, Units_P = Units),
            by = c("scenario", "commodity", "year")) %>%
  # multiply price by quantity and convert to million 1990$ 
  mutate(value = price*imports*conv_75_90*conv_MIL_BIL,
         Unit = "million $1990") %>% 
  group_by(scenario, region, commodity, year, Units_Q, Unit) %>% 
  summarise(imports = sum(imports, na.rm = T),
            IM_value = sum(value, na.rm = T)) ->
  df.import.en

df.import.ag %>% bind_rows(df.import.en) ->
  df.import

df.export <- PluckBind("AE_EX") %>%
  # remove iron and steel since it isn't primary sector
  filter(!input %in% c("iron and steel")) %>%
  filter(!grepl("statistical differences", subsector)) %>%
  rename(exports = value, Units_Q = Units) %>% 
  separate(subsector, into = c("region", "commodity"),
           sep = " traded ") %>% 
  left_join(traded_prices %>% rename(price = value, Units_P = Units),
            by = c("scenario", "commodity", "year")) %>% 
  mutate(value = price*exports*conv_75_90*conv_MIL_BIL,
         Unit = "million $1990") %>% 
  select(scenario, region, commodity, year, Units_Q, Unit, exports, EX_value = value)

df.export %>% full_join(df.import, 
                        by = c("scenario", "region", "commodity", "year", "Units_Q", "Unit")) ->
  df.trade

df.trade %>% 
  mutate(NX_Q = case_when(is.na(exports) ~ -imports, is.na(imports) ~ exports, TRUE ~ exports - imports),
         NX_value = case_when(is.na(EX_value) ~ -IM_value, is.na(IM_value) ~ EX_value, TRUE ~ EX_value - IM_value)) %>% 
  filter(!commodity %in% c("Afr_MidE pipeline gas", "EUR pipeline gas", "LA pipeline gas", "LNG","coal", 
                          "N.Amer pipeline gas",   "PAC pipeline gas" , "RUS pipeline gas", "ammonia",
                          "woodpulp" )) %>%
  mutate(group = "Other crops",
         # group = ifelse(commodity %in% c("sugarcrop"), "Sugarcrop", group),
         group = ifelse(commodity %in% c("corn","wheat","rice","soybean"), "Key crops", group),
         group = ifelse(commodity %in% c("beef","dairy","poultry","sheepgoat", "pork"), "Livestock", group),
         group = ifelse(commodity %in% c("industrial_roundwood", "sawnwood" ), "Forestry" , group)) %>%
  # mutate(group = commodity) %>% 
  gather(var, value, exports:NX_value) %>% 
  left_join(Regmapping %>% select(region, REG10_main), by = "region") %>% 
  group_by(scenario, year, region = REG10_main, group, var, Units_Q, Unit) %>%  
  summarise(value = sum(value, na.rm = T)) %>% 
  group_by(region, group, var, year, Units_Q, Unit) %>% 
  mutate(delta = value - value[scenario == "Ref"],
         index = 100 * value / value[scenario == "Ref"] - 100) %>% 
  filter(year >= 2015,
         scenario != "Ref") ->
  plot.trade.10

plot.trade.10 %>% 
  filter(scenario == scenario_target) %>%
  filter(group != "Forestry") %>% 
  SCE_NAME() %>% 
  mutate(region = factor(region, levels = rev(reg_order)),
         var = gsub("NX_Q", "Net export quantity", var),
         var = gsub("exports", "Export quantity", var),
         var = gsub("imports", "Import quantity", var)) %>%
  filter(year == 2100) %>% 
  filter(grepl("quantity", var)) ->
  plot.trade.10.quantity

# plot.trade.10.quantity %>% 
#   select(scenario, region, group, var, delta) %>% 
#   mutate(var = paste0(var, " (Mt)") ) %>% 
#   rename(value = delta) %>% 
#   bind_rows(plot.trade.10.quantity %>% 
#               select(scenario, region, group, var, index) %>% 
#               mutate(var = paste0(var, " (%)") ) %>% 
#               filter(!grepl("Net", var)) %>% 
#               rename(value = index)) ->
#   df.plot

plot.trade.10.quantity %>% 
  select(scenario, region, group, var, delta) %>% 
  rename(value = delta) ->
  df.plot.q
  
plot.trade.10 %>% 
  filter(scenario == scenario_target) %>%
  filter(group != "Forestry") %>% 
  SCE_NAME() %>% 
  mutate(region = factor(region, levels = rev(reg_order)),
         var = gsub("NX_value", "Net export value", var),
         var = gsub("EX_value", "Export value", var),
         var = gsub("IM_value", "Import value", var)) %>%
  filter(year == 2100) %>% 
  filter(grepl("value", var)) ->
  plot.trade.10.value

plot.trade.10.value %>% 
  select(scenario, region, group, var, delta) %>% 
  rename(value = delta) %>% 
  mutate(value =CONV_90_15 * value / 1000) -> # mil 1990$ --> bil 2015$
  df.plot.v

df.plot.q %>% 
  ggplot() +
  geom_tile(aes(x = group, y = region, fill = value)) +
  scale_fill_gradient2(low = "#E69F00", mid = "white", high = "#009E73", midpoint = 0) +
  facet_grid(~ var) +
  labs(x = "", y = "", 
       # title = "Absolute changes in trade volume (Ref = 0):\n2100",
       fill = "Mt") +
  theme_bw() + themeds ->
  Fig4.trade.volume.10; Fig4.trade.volume.10

Write_png(Fig4.trade.volume.10, "Fig4.trade.volume.10", DIR_MODULE, w = 8, h = 6, r = 300)


df.plot.v %>% 
  ggplot() +
  geom_tile(aes(x = group, y = region, fill = value)) +
  scale_fill_gradient2(low = "#E69F00", mid = "white", high = "#009E73", midpoint = 0) +
  facet_grid(~ var) +
  labs(x = "", y = "", 
       # title = "Absolute changes in trade value (Ref = 0):\n2100",
       fill = "billion\n2015$") +
  theme_bw() + themeds ->
  Fig4.trade.value.10; Fig4.trade.value.10

Write_png(Fig4.trade.value.10, "Fig4.trade.value.10", DIR_MODULE, w = 8, h = 6, r = 300)


plot.trade.10 %>% 
  filter(scenario == scenario_target) %>%
  filter(group != "Forestry") %>% 
  SCE_NAME() %>% 
  mutate(region = factor(region, levels = rev(reg_order)),
         var = gsub("EX_value", "Export value", var),
         var = gsub("IM_value", "Import value", var),
         var = gsub("exports", "Export quantity", var),
         var = gsub("imports", "Import quantity", var),
         group = factor(group, levels = c("Key crops", "Other crops", "Livestock"))) %>%
  filter(year == 2100) %>% 
  filter(var %in% c("Export value", "Import value", "Export quantity", "Import quantity")) %>% 
  ggplot() +
  geom_tile(aes(x = group, y = region, fill = index)) +
  scale_fill_gradient2(low = "#E69F00", mid = "white", high = "#009E73", midpoint = 0) +
  facet_grid(~ var) +
  labs(x = "", y = "", 
       # title = "Relative change in trade outcome (Ref = 0):\n2100",
       fill = "%") +
  theme_bw() + themeds ->
Fig4.trade.pct.10; Fig4.trade.pct.10

Write_png(Fig4.trade.pct.10, "Fig4.trade.pct.10", DIR_MODULE, w = 10, h = 6, r = 300)
# 
# plot.trade.10 %>% 
#   filter(scenario == scenario_target) %>%
#   filter(group != "Forestry") %>% 
#   filter(var %in% c("EX_value", "IM_value")) %>% 
#   mutate(delta =CONV_90_15  * delta / 10^3) %>% 
#   bind_rows(plot.trade.10 %>% 
#               filter(scenario == scenario_target) %>%
#               filter(group != "Forestry") %>% 
#               filter(var %in% c("imports", "exports"))) %>% 
#   SCE_NAME() %>% 
#   mutate(region = factor(region, levels = rev(reg_order)),
#          var = gsub("EX_value", "Export (billion 2015$)", var),
#          var = gsub("IM_value", "Import (billion 2015$)", var),
#          var = gsub("exports", "Export (million ton)", var),
#          var = gsub("imports", "Import (million ton)", var),
#          group = factor(group, levels = c("Key crops", "Other crops", "Livestock"))) %>%
#   filter(year == 2100) %>% 
#   ggplot() +
#   geom_tile(aes(x = group, y = region, fill = delta)) +
#   scale_fill_gradient2(low = "#E69F00", mid = "white", high = "#009E73", midpoint = 0) +
#   facet_grid(scenario~ var) +
#   labs(x = "", y = "", fill = "",
#        title = "Trade outcome relative to Ref (Ref = 0): 2100") +
#   theme_bw() + themeds ->
#   Fig4.trade.level.10; Fig4.trade.level.10
# 
# Write_png(Fig4.trade.level.10, "Fig4.trade.level.10", DIR_MODULE, w = 14, h = 6, r = 300)



# !!!! STOP HERE ----

## Water ----

PluckBind("WaterWWSec") %>% 
  filter(sector %in% unique(MapAgCOMM$AgCOMM)) %>% # filter out ag water use: irrigation water
  group_by(scenario, Units, year) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  group_by( year) %>% 
  mutate(index = value / value[scenario == "Ref"],
         delta = value - value[scenario == "Ref"]) ->
  df.water.glb

PluckBind("WaterWWSec") %>% 
  filter(sector %in% unique(MapAgCOMM$AgCOMM)) %>% 
  group_by(scenario, region, Units, year) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  group_by(region, year) %>% 
  mutate(index = value / value[scenario == "Ref"],
         delta = value - value[scenario == "Ref"]) ->
  df.water

df.water %>% 
  SCE_NAME() %>% 
  ggplot() +
  geom_line(aes(x = year, y = index, color = scenario), linewidth = 0.8) +
  facet_wrap(~ region, ncol = 8) +
  scale_color_manual(values = scenario_colors) +
  labs(x = "", y = "Irrigation water changes relative to Ref (Ref = 1)") +
  theme_bw() + themeds 

REG %>% left_join(df.water %>% 
                    filter(year == 2100, scenario == "CL_LS") %>% 
                    select(reg_nm = region, index, delta),
                  by = "reg_nm") %>%  
  ggplot() +
  geom_sf(aes(fill = 100*index-100)) +
  # scale_fill_viridis() +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0) +
  coord_sf(datum = NA) + 
  labs(title = "% change of irrigated water withdrawal relative to Ref (Ref = 0)") +
  theme_bw() + theme0 + theme1 +
  theme(legend.position="right",
        plot.title = element_text(hjust = 0.5,
                                  color = "Gray40",
                                  size = 16,
                                  face = "bold"),
        plot.subtitle = element_text(color = "blue"),
        plot.caption = element_text(color = "Gray60"))  +
  guides(fill = guide_colorbar(title = "%",
                               title.position = "top",
                               title.theme = element_text(size = 10,
                                                          face = "bold",
                                                          colour = "black",
                                                          angle = 0)))


REG %>% left_join(df.water %>% 
                    filter(year == 2100, scenario == "CL_LS") %>% 
                    select(reg_nm = region, index, delta),
                  by = "reg_nm") %>%  
  ggplot() +
  geom_sf(aes(fill = delta)) +
  # scale_fill_viridis() +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0) +
  coord_sf(datum = NA) + 
  labs(title = "Level change of irrigated water withdrawal relative to Ref (Ref = 0)") +
  theme_bw() + theme0 + theme1 +
  theme(legend.position="right",
        plot.title = element_text(hjust = 0.5,
                                  color = "Gray40",
                                  size = 16,
                                  face = "bold"),
        plot.subtitle = element_text(color = "blue"),
        plot.caption = element_text(color = "Gray60"))  +
  guides(fill = guide_colorbar(title = "km^3",
                               title.position = "top",
                               title.theme = element_text(size = 10,
                                                          face = "bold",
                                                          colour = "black",
                                                          angle = 0)))

PluckBind("WaterWWSec") %>% 
  filter(sector %in% unique(MapAgCOMM$AgCOMM)) %>% 
  group_by(scenario, region, sector, Units, year) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  filter(year >= 2015) %>% spread(scenario, value) %>% 
  filter(!is.na(Ref)) %>% 
  group_by(region, sector, year) %>% 
  mutate(index = CL_LS / Ref,
         delta = CL_LS - Ref) ->
  df.water.sec


df.water.sec %>% 
  select(region, sector, Units, year, index, delta) %>% 
  filter(year == 2100) %>% 
  gather(variable, value, index:delta) %>% 
  group_by(sector, variable, year) %>% 
  summarise(y05 = quantile(value, 0.05, na.rm = T),
            y25 = quantile(value, 0.25, na.rm = T),
            y50 = quantile(value, 0.50, na.rm = T),
            y75 = quantile(value, 0.75, na.rm = T),
            y95 = quantile(value, 0.95, na.rm = T),
            mean = mean(value, na.rm = T)) ->
  box.water.sec

box.water.sec %>% 
  filter(variable == "index") %>% 
  ggplot() +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_boxplot(aes(x = sector, lower = y25, upper = y75, middle = y50, 
                   ymin = y05, ymax = y95, alpha = year), stat = "identity") +
  geom_errorbar(aes(x = sector, ymin = mean, ymax = mean), color = "red") +
  labs(x = "", y = "index", 
       title = "Water withdrawal changes relative to Ref (Ref = 1)\nCombined: LS, 2100") +
  theme_bw() + themeds 


box.water.sec %>% 
  filter(variable == "delta") %>% 
  ggplot() +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_boxplot(aes(x = sector, lower = y25, upper = y75, middle = y50, 
                   ymin = y05, ymax = y95, alpha = year), stat = "identity") +
  geom_errorbar(aes(x = sector, ymin = mean, ymax = mean), color = "red") +
  labs(x = "", y = "km^3", 
       title = "Water withdrawal changes relative to Ref (Ref = 0)\nCombined: LS, 2100") +
  theme_bw() + themeds 





# check additional results ----
PSUA %>% 
  na.omit() %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>% 
  Agg_reg(element, AgCOMM5, region) %>% 
  rename(sector = AgCOMM5) %>% 
  mutate(element = gsub("Supply: ", "", element),
         element = gsub("Demand: ", "", element)) %>% 
  filter(element %in% c("Export", "Import")) %>% 
  spread(element, value) %>% 
  mutate(value = Import + Export,
         element = "Net import (Mt)") %>% 
  select(-Export, -Import)->
  check

check %>% 
  group_by(element, sector, region, year) %>% 
  mutate(delta = value - value[scenario == "Ref"]) %>% 
  filter(scenario == "CL_LS") %>%  
  SCE_NAME() %>% 
  ggplot() +
  geom_bar(aes(x = year, y = -delta, fill = sector),
           stat = "identity", position = "stack") +
  facet_wrap(~ region, ncol = 8) +
  labs(x = "", y = "Mt", title = "Changes in net export relative to Ref (Ref = 0): Combined_LS") +
  theme_bw() + themeds 



PSUA %>% 
  na.omit() %>% 
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>% 
  left_join_error_no_match(Regmapping) %>% 
  Agg_reg(element, AgCOMM5, REG10_main) %>% 
  rename(sector = AgCOMM5, region = REG10_main) %>% 
  mutate(element = gsub("Supply: ", "", element),
         element = gsub("Demand: ", "", element)) %>% 
  filter(element %in% c("Export", "Import")) %>% 
  spread(element, value) %>% 
  mutate(value = Import + Export,
         element = "Net import (Mt)") %>% 
  select(-Export, -Import)->
  Key1.A.TRADE.reg10

Key1.A.TRADE.reg10 %>% 
  group_by(element, sector, region, year) %>% 
  mutate(delta = value - value[scenario == "Ref"]) %>% 
  filter(scenario == "CL_LS") %>%  
  SCE_NAME() %>% 
  mutate(region = factor(region, levels = reg_order)) %>% 
  ggplot() +
  geom_bar(aes(x = year, y = -delta, fill = sector),
           stat = "identity", position = "stack") +
  facet_wrap(~ region, ncol = 5) +
  labs(x = "", y = "Mt", title = "Changes in net export relative to Ref (Ref = 0): Combined_LS") +
  theme_bw() + themeds


ag_en_exports_value %>% rename(Mt = exports) %>% mutate(variable = "export") %>% 
  bind_rows(ag_en_imports_value %>% rename(Mt = imports) %>% mutate(variable = "import")) %>% 
  rename(sector = commodity) %>% 
  left_join(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>%
      mutate(sector = tolower(sector)), by = "sector") %>%
  na.omit() %>% 
  select(scenario, sector,  year, variable, value, price, Mt) %>% 
  group_by(scenario, sector,  year, variable) %>% 
  summarise(value = sum(value, na.rm = T),
            Mt = sum(Mt, na.rm = T),
            price = value / Mt) %>% 
  group_by(sector, year, variable) %>% 
  mutate(index_value = value / value[scenario == "Ref"],
         index_Q = Mt / Mt[scenario == "Ref"],
         index_P = price / price[scenario == "Ref"]) %>% 
  filter(year >= 2015) ->
  index_trade_glb_sec

index_trade_glb_sec %>% 
  filter(scenario == "CL_LS") %>%  
  ungroup() %>% 
  SCE_NAME() %>% 
  select(scenario, sector, year, variable, index_value, index_Q, index_P) %>% 
  gather(index, value, index_value:index_P) %>% 
  # filter(index == "index_P") %>% 
  # mutate(region = factor(region, levels = reg_order)) %>% 
  ggplot() +
  geom_hline(yintercept = 1, linewidth = 0.8, color = "grey") +
  geom_line(aes(x = year, y = value, color = index, linetype = variable)) +
  facet_wrap(~ sector, ncol = 10) +
  labs(x = "", y = "index", title = "Changes in trade metric relative to Ref (Ref = 0): Combined_LS, Other Crops") +
  theme_bw() + themeds

ag_en_exports_value %>% rename(Mt = exports) %>% mutate(variable = "export") %>% 
  bind_rows(ag_en_imports_value %>% rename(Mt = imports) %>% mutate(variable = "import")) %>% 
  rename(sector = commodity) %>% 
  left_join(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>%
      mutate(sector = tolower(sector)), by = "sector") %>%
  na.omit() %>% 
  select(scenario, region, sector = AgCOMM5, year, variable, value, price, Mt) %>% 
  group_by(scenario, region, sector, year, variable) %>% 
  summarise(value = sum(value, na.rm = T),
            Mt = sum(Mt, na.rm = T),
            price = value / Mt) %>% 
  group_by(region, sector, year, variable) %>% 
  mutate(index_value = value / value[scenario == "Ref"],
         index_Q = Mt / Mt[scenario == "Ref"],
         index_P = price / price[scenario == "Ref"]) %>% 
  filter(year >= 2015) ->
  index_trade

index_trade %>% 
  filter(scenario == "CL_LS") %>%  
  filter(sector == "Other crops") %>%  
  ungroup() %>% 
  SCE_NAME() %>% 
  select(scenario, region, sector, year, variable, index_value, index_Q, index_P) %>% 
  gather(index, value, index_value:index_P) %>% 
  # filter(index == "index_P") %>% 
  # mutate(region = factor(region, levels = reg_order)) %>% 
  ggplot() +
  geom_line(aes(x = year, y = value, color = index, linetype = variable)) +
  facet_wrap(~ region, ncol = 8) +
  labs(x = "", y = "index", title = "Changes in trade metric relative to Ref (Ref = 0): Combined_LS, Other Crops") +
  theme_bw() + themeds





ag_en_exports_value %>% rename(Mt = exports) %>% mutate(variable = "export") %>% 
  bind_rows(ag_en_imports_value %>% rename(Mt = imports) %>% mutate(variable = "import")) %>% 
  rename(sector = commodity) %>% 
  left_join(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM5) %>% 
      mutate(sector = tolower(sector)), by = "sector") %>% 
  left_join_error_no_match(Regmapping %>% select(region, REG10_main)) %>% 
  na.omit() %>% 
  select(scenario, region = REG10_main, sector = AgCOMM5, year, variable, value, price, Mt) %>% 
  group_by(scenario, region, sector, year, variable) %>% 
  summarise(value = sum(value, na.rm = T),
            Mt = sum(Mt, na.rm = T),
            price = value / Mt) %>% 
  group_by(region, sector, year, variable) %>% 
  mutate(index_value = value / value[scenario == "Ref"],
         index_Q = Mt / Mt[scenario == "Ref"],
         index_P = price / price[scenario == "Ref"]) %>% 
  filter(year >= 2015) ->
  index_trade_10

index_trade_10 %>% 
  select(scenario, region, sector, year, variable, value) %>% 
  spread(variable, value) %>% 
  mutate(NX_V = export - import) %>% 
  group_by(region, sector, year) %>% 
  mutate(index_NX_V = NX_V / NX_V[scenario == "Ref"]) ->
  index_NXV

index_trade_10 %>% 
  filter(scenario == "CL_LS") %>%  
  filter(sector == "Other crops") %>% 
  SCE_NAME() %>% 
  select(scenario, region, sector, year, variable, index_value, index_Q, index_P) %>% 
  gather(index, value, index_value:index_P) %>% 
  mutate(region = factor(region, levels = reg_order)) %>%
  ggplot() +
  geom_line(aes(x = year, y = value, color = index, linetype = variable), linewidth = 0.8) +
  facet_wrap(~ region, ncol = 5) +
  labs(x = "", y = "index", title = "Changes in trade metric relative to Ref (Ref = 0): Combined_LS, Other Crops") +
  theme_bw() + themeds


AgElement_AreaYieldPrice %>% 
  mutate(group = ifelse(sector %in% c("beef", "dairy","pork","poultry","sheepgoat" ), "animal", sector),
         group = ifelse(group %in% c("corn", "rice", "soybean", "wheat", "animal", "sugarcrop"), group, "others")) %>% 
  filter(group != "animal") %>% 
  select(scenario, region, group, year, element, value) %>% 
  filter(element %in% c("Area", "Production", "Revenue")) %>% 
  group_by(scenario, region, year, element) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  spread(element, value) %>% 
  mutate(Yield = Production / Area,
         Price = Revenue / Production) %>%  
  gather(account, value, Area:Price) %>% 
  mutate(group = 'AG') %>% 
  bind_rows(AYPP_32) ->
  AGG_AYPP_32


AgElement_AreaYieldPrice %>% 
  mutate(group = ifelse(sector %in% c("beef", "dairy","pork","poultry","sheepgoat" ), "animal", sector),
         group = ifelse(group %in% c("corn", "rice", "soybean", "wheat", "animal", "sugarcrop"), group, "others")) %>% 
  filter(group != "animal") %>% 
  select(scenario, region, group, year, element, value) %>% 
  left_join_error_no_match(Regmapping %>% select(region, REG10_main)) %>% 
  filter(element %in% c("Area", "Production", "Revenue")) %>% 
  group_by(scenario, region = REG10_main, group, year, element) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  spread(element, value) %>% 
  mutate(Yield = Production / Area,
         Price = Revenue / Production) %>% 
  gather(account, value, Area:Price) ->
  AYPP_10

AgElement_AreaYieldPrice %>% 
  mutate(group = ifelse(sector %in% c("beef", "dairy","pork","poultry","sheepgoat" ), "animal", sector),
         group = ifelse(group %in% c("corn", "rice", "soybean", "wheat", "animal", "sugarcrop"), group, "others")) %>% 
  filter(group != "animal") %>% 
  select(scenario, region, group, year, element, value) %>% 
  left_join_error_no_match(Regmapping %>% select(region, REG10_main)) %>% 
  filter(element %in% c("Area", "Production", "Revenue")) %>% 
  group_by(scenario, region = REG10_main, year, element) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  spread(element, value) %>% 
  mutate(Yield = Production / Area,
         Price = Revenue / Production) %>% 
  gather(account, value, Area:Price) %>% 
  mutate(group = 'AG') %>% 
  bind_rows(AYPP_10) ->
  AGG_AYPP_10


df.KLA.ag %>% spread(input, value) %>% 
  mutate(LK_Ag = Labor_Ag / Capital_Ag) %>% 
  gather(input, value, Capital_Ag:LK_Ag) %>% 
  # filter(scenario == "CL_LS") %>%
  SCE_NAME() %>% 
  mutate(region = factor(region, levels = reg_order)) %>% 
  filter(input == "LK_Ag", year >= 2015) %>% 
  ggplot() +
  geom_line(aes(x = year, y = 1/value, color = scenario),linewidth = 0.8) +
  facet_wrap(~ region, ncol = 5, scales = "free_y") +
  scale_color_manual(values = scenario_colors) +
  labs(x = "", y = "index", title = "Capital-labor ratio in Ag") +
  theme_bw() + themeds 


plot.wage.10 %>% 
  filter(variable == "VAL") %>% ungroup() %>% 
  filter(sector != "Labor_Total") %>% 
  mutate(sector = gsub("Labor_Ag", "AG", sector ),
         sector = gsub("Labor_Materials", "MA", sector),
         input = "labor") %>% 
  select(-variable) %>% 
  bind_rows(R2A.10 %>% group_by(scenario, year, region) %>% 
              summarise(value = sum(value, na.rm = T)) %>% mutate(sector = "AG", input = "land")) %>% 
  bind_rows(R2K %>% gather(sector, value, R2K_AG:R2K_EN) %>% 
              filter(year >= 2015) %>% 
              left_join(Regmapping %>% select(region, REG10_main), by = "region") %>% 
              group_by(scenario, year, region = REG10_main, sector) %>%  
              summarise(value = sum(value, na.rm = T)) %>% 
              mutate(sector = gsub("R2K_", "", sector), input = "capital")) ->
  level.GVA.10

level.GVA.10 %>% 
  spread(input, value) %>% 
  mutate(KL = capital / labor) %>% 
  filter(sector != "EN") %>% 
  filter(scenario == "CL_LS") %>% 
  SCE_NAME() %>% 
  mutate(region = factor(region, levels = reg_order)) %>% 
  ggplot() +
  geom_line(aes(x = year, y = KL, color = sector), linewidth = 0.8) +
  facet_wrap(~ region, ncol = 5) +
  labs(x = "", y = "index", title = "Return to Capital vs. Return to Labor: Combined_LS") +
  theme_bw() + themeds 


PluckBind("SAM_NA") %>% 
  filter(Account %in% c("GDP", "ag-food-service-value")) %>% 
  filter(year >= 2015) %>% 
  left_join_error_no_match(Regmapping %>% select(region, REG10_main)) %>% 
  group_by(scenario, year, region =REG10_main, Account) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  spread(Account, value) %>% 
  mutate(food_exp_share = 100 *`ag-food-service-value` / GDP) %>% 
  group_by(region, year) %>% 
  mutate(delta_share = food_exp_share - food_exp_share[scenario == "Ref"]) %>% 
  SCE_NAME() %>% View()
mutate(region = factor(region, levels = reg_order)) %>% 
  ggplot() +
  geom_line(aes(x = year, y = delta_share, color = scenario), linewidth = 0.8) +
  facet_grid(~ region) +
  scale_x_continuous(breaks = c(2025, 2050, 2075, 2100)) +
  scale_color_manual(values = scenario_colors) +
  labs(x = "", y = "%", title = "Changes in food expenditure share of GDP") +
  theme_bw() + themeds + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))




PluckBind("SAM_NA") %>% 
  filter(Account %in% c("GDP", "ag-food-service-value")) %>% 
  filter(year >= 2015) %>% 
  left_join_error_no_match(Regmapping %>% select(region, REG10_main)) %>% 
  group_by(scenario, year, region =REG10_main, Account) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  spread(Account, value) %>% 
  mutate(food_exp_share = 100 *`ag-food-service-value` / GDP) %>% 
  group_by(region, year) %>% 
  mutate(delta_share = food_exp_share - food_exp_share[scenario == "Ref"]) %>% 
  SCE_NAME() %>% 
  mutate(region = factor(region, levels = reg_order)) %>% 
  ggplot() +
  geom_line(aes(x = year, y = food_exp_share, color = scenario), linewidth = 0.8) +
  facet_wrap(~ region, ncol = 5, scales = "free_y") +
  scale_x_continuous(breaks = c(2025, 2050, 2075, 2100)) +
  scale_color_manual(values = scenario_colors) +
  labs(x = "", y = "%", title = "Food expenditure share of GDP") +
  theme_bw() + themeds + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


R2K %>% gather(sector, value, R2K_AG:R2K_EN) %>% 
  filter(year >= 2015) %>% 
  left_join(Regmapping %>% select(region, REG10_main), by = "region") %>% 
  group_by(scenario, year, region = REG10_main, sector) %>%  
  summarise(value = sum(value, na.rm = T)) %>% 
  group_by(year, region, sector) %>% 
  mutate(delta = value - value[scenario == "Ref"])  ->
  R2K.10


R2L.10 %>% filter(sector != "Labor_Total") %>% 
  SCE_NAME() %>% 
  mutate(region = factor(region, levels = reg_order)) %>% 
  ggplot() +
  geom_bar(aes(x = year, y = delta, fill = sector), 
           stat = "identity", position = "stack") +
  # facet_wrap(~ region, ncol = 5, scales = "free_y") +
  geom_errorbar(data = R2L.10 %>% filter(sector != "Labor_Total") %>% SCE_NAME() %>% 
                  mutate(region = factor(region, levels = reg_order)) %>% 
                  group_by(scenario, year, region) %>% 
                  summarise(delta = sum(delta, na.rm = T)),
                aes(x = year, ymin = delta, ymax = delta), linewidth = 0.8) +
  facet_wrap(~ region, ncol = 5) +
  labs(x = "", y = "change relative to Ref (million 1990$)", title = "Return to labor") +
  theme_bw() + themeds + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# metrics vs. income per capita ----

# obtain income per capita

PluckBind("SAM_NA") %>%  
  filter(Account == "gdp-per-capita") %>% 
  select(scenario, region, year, Units, income = value) ->
  df.income

## ag labor share ----

PluckBind("LaborSupplyAll") %>% 
  filter(scenario == "Ref") %>% 
  mutate(sector = "Labor_Total",
         sector = ifelse(grepl("Labor_Ag", market), "Labor_Ag", sector),
         sector = ifelse(grepl("Labor_Materials", market), "Labor_Materials", sector),
         region = gsub("Labor_Ag", "", market),
         region = gsub("Labor_Materials", "", region),
         region = gsub("Labor_Total", "", region)) %>%
  select(scenario, region, Units, year, value, sector) %>% 
  mutate(sector = gsub("Labor_", "", sector)) %>% 
  spread(sector, value) %>% 
  mutate(share = Ag / Total) %>% 
  select(scenario, region, year, share) ->
  df.AgLaborShare

## capital intensity ----


## capital/labor ratio
PluckBind("LaborSupplyAll") %>% 
  filter(scenario == "Ref") %>% 
  mutate(sector = "Labor_Total",
         sector = ifelse(grepl("Labor_Ag", market), "Labor_Ag", sector),
         sector = ifelse(grepl("Labor_Materials", market), "Labor_Materials", sector),
         region = gsub("Labor_Ag", "", market),
         region = gsub("Labor_Materials", "", region),
         region = gsub("Labor_Total", "", region)) %>%
  select(scenario, region, Units, year, value, sector) %>% 
  filter(sector == "Labor_Ag") ->
  df.AgL.ref

PluckBind("AgCapitalDemand") %>% 
  filter(scenario == "Ref") %>% 
  select(scenario, region, year, AgK = value) %>% # billion 1975$
  left_join_error_no_match(df.AgL.ref %>% select(scenario, region, year, AgL = value), # million people
                           by = c("scenario", "region", "year")) %>% 
  mutate(K_L = AgK / AgL) %>% # bil 1975$ / mil people = thousand 1975$ per labor
  select(scenario, region, year, K_L) -> 
  df.AgCapitalLaborRatio

## biophysical impact ----

## summary ----
df.AgCapitalLaborRatio %>% 
  left_join_error_no_match(df.AgLaborShare, by = c("scenario", "region", "year")) %>% 
  left_join_error_no_match(df.income %>% filter(scenario == "Ref"), by = c("scenario", "region", "year")) %>%
  gather(metric, value, K_L:share) %>% 
  left_join_error_no_match(Regmapping %>% select(region, REG10_main), by = "region") %>% 
  mutate(REG10_main = factor(REG10_main, levels = reg_order)) ->
  df.income.summary


df.income.summary %>% filter(metric == "share") %>% 
  filter(income > 0) %>% mutate(value = 100*value) %>% 
  filter(year <= 2015) -> data.share   

global_share_model <- lm(value ~ income, data = data.share)
slope_share <- coef(global_share_model)[2]
intercept_share <- coef(global_share_model)[1]

global_share_quadratic <- lm(value ~ income + I(income^2), data = data.share)
x_seq <- seq(min(data.share$income), max(data.share$income), length.out = 200)
pred_df <- data.frame(income = x_seq)
pred_df$y <- predict(global_share_quadratic, newdata = pred_df)

global_share_cubic <- lm(value ~ income + I(income^2) + I(income^3), data = data.share)
x_seq <- seq(min(data.share$income), max(data.share$income), length.out = 200)
pred_df2 <- data.frame(income = x_seq)
pred_df2$y <- predict(global_share_cubic, newdata = pred_df2)


loess_fit <- loess(value ~ income, data = data.share)
x_seq <- seq(min(data.share$income), max(data.share$income), length.out = 200)
pred_df3 <- data.frame(income = x_seq)
pred_df3$y <- predict(loess_fit, newdata = pred_df3)

df.income.summary %>% 
  ggplot() +
  geom_point(aes(x = income, y = value, color = REG10_main)) +
  facet_wrap(~ metric, scales = "free_y") +
  scale_color_brewer(palette = "RdYlGn") +
  theme_bw()

df.income.summary %>% 
  filter(metric == "share") %>% 
  ggplot() +
  geom_point(aes(x = income, y = value* 100, color = REG10_main)) +
  geom_abline(slope = slope_share, intercept = intercept_share, color = "black", linewidth = 0.8, linetype = "dashed") +
  # geom_line(data = pred_df, aes(x = income, y = y), color = "red", linetype = "dashed", inherit.aes = FALSE) +
  # geom_line(data = pred_df2, aes(x = income, y = y), color = "blue", linetype = "dashed", inherit.aes = FALSE) +
  geom_line(data = pred_df3, aes(x = income, y = y), color = "purple", linewidth = 0.8, linetype = "dashed", inherit.aes = FALSE) +
  facet_wrap(~ region, ncol = 8) +
  scale_color_brewer(palette = "RdYlGn") +
  labs(x = "GDP per capita (thous 1990$ per capita)", y = "%", 
       title = "Agricultural labor share\n(Black dashed line denote historical global linear trend, purple dashed line denotes historical global loess trend)") +
  theme_bw()


df.income.summary %>% filter(metric == "K_L") %>% 
  filter(income > 0) %>% mutate(value = 100*value) %>% 
  filter(year <= 2015) -> data.ratio  

global_share_model <- lm(value ~ income, data = data.ratio)
slope_share <- coef(global_share_model)[2]
intercept_share <- coef(global_share_model)[1]

global_share_quadratic <- lm(value ~ income + I(income^2), data = data.ratio)
x_seq <- seq(min(data.share$income), max(data.share$income), length.out = 200)
pred_df <- data.frame(income = x_seq)
pred_df$y <- predict(global_share_quadratic, newdata = pred_df)

global_share_cubic <- lm(value ~ income + I(income^2) + I(income^3), data = data.ratio)
x_seq <- seq(min(data.share$income), max(data.share$income), length.out = 200)
pred_df2 <- data.frame(income = x_seq)
pred_df2$y <- predict(global_share_cubic, newdata = pred_df2)


loess_fit <- loess(value ~ income, data = data.ratio)
x_seq <- seq(min(data.share$income), max(data.share$income), length.out = 200)
pred_df3 <- data.frame(income = x_seq)
pred_df3$y <- predict(loess_fit, newdata = pred_df3)


df.income.summary %>% 
  filter(metric == "K_L") %>% 
  ggplot() +
  geom_point(aes(x = income, y = value, color = REG10_main)) +
  facet_wrap(~ region, ncol = 8) +
  scale_color_brewer(palette = "RdYlGn") +
  labs(x = "GDP per capita (thous 1990$ per capita)",
       y = "thousand 1975$ per labor",
       title = "Capital-Labor Ratio in Agriculture") +
  theme_bw()


df.income.summary %>% 
  filter(metric == "K_L") %>% 
  ggplot() +
  geom_point(aes(x = income, y = value* 100, color = REG10_main)) +
  geom_abline(slope = slope_share, intercept = intercept_share, color = "black", linewidth = 0.8, linetype = "dashed") +
  # geom_line(data = pred_df, aes(x = income, y = y), color = "red", linetype = "dashed", inherit.aes = FALSE) +
  # geom_line(data = pred_df2, aes(x = income, y = y), color = "blue", linetype = "dashed", inherit.aes = FALSE) +
  geom_line(data = pred_df3, aes(x = income, y = y), color = "purple", linewidth = 0.8, linetype = "dashed", inherit.aes = FALSE) +
  facet_wrap(~ region, ncol = 8) +
  scale_color_brewer(palette = "RdYlGn") +
  labs(x = "GDP per capita (thous 1990$ per capita)", y = "thousand 1975$ per labor", 
       title = "Agricultural Capital-Labor Ratio\n(Black dashed line denote historical global linear trend, purple dashed line denotes historical global loess trend)") +
  theme_bw()

# plot GCAM regions ----

color_palette_32 <- c(
  "#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d", "#666666",
  "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00",
  "#cab2d6", "#6a3d9a", "#ffff99", "#b15928", "#8dd3c7", "#ffffb3", "#bebada", "#fb8072",
  "#80b1d3", "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd", "#ccebc5", "#ffed6f"
)

REG %>% 
  ggplot() +
  geom_sf(aes(fill = reg_nm)) +
  geom_sf_text(aes(label = reg_nm), size = 3) +
  scale_fill_manual(values = setNames(color_palette_32, unique(REG$reg_nm))) +
  theme(legend.position = "bottom") 


