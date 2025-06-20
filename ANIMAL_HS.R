# generate water basin shock using HELPS package and BASD monthly output
# library(HELPS)
library(data.table)
library(zoo)

# read in HELPS outputs

# translate HELPS outputs to GCAM input
# read in required csv inputs
basin_to_country_mapping <- read.csv("inst/extdata/water/basin_to_country_mapping.csv", skip = 7, header = T)
iso_GCAM_regID <- read.csv("inst/extdata/common/iso_GCAM_regID.csv", skip = 6, header = T)
GCAM_region_names <- read.csv("inst/extdata/common/GCAM_region_names.csv", skip = 6, header = T)
# FAO_ag_items_PRODSTAT <- read.csv("inst/extdata/aglu/FAO/FAO_ag_items_PRODSTAT.csv", skip = 10, header = T)

# input.dir <- "C:/Model/HELPS/GCAM_Food/MRI/"
input.dir <- "C:/Model/HS_package/HELPS/Macro_HS/"
output.dir <- "../../"


# eta_list <- list.files(input.dir, pattern = "ANNUAL_WB", full.names = FALSE); eta_list
eta_list <- "C:/Model/HELPS/GCAM_Food/MRI/ANNUAL_WB_ANIMAL_NONCROP.rds"


## livestock yield ----
  THI <- readRDS(eta_list)
  df.thi <- do.call(rbind, THI)
  basin_to_country_mapping %>% full_join(df.thi %>% rename(GCAM_basin_ID = region_id),
                                         by = "GCAM_basin_ID") -> df.thi.WB
  print(dim(df.thi.WB))


# eta.all <-
df.thi.WB %>%
  filter(!is.na(GCAM_basin_ID)) %>%
  filter(!is.na(crop)) %>%
  group_by(GCAM_basin_ID, year) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  arrange(GCAM_basin_ID, year) ->
  animal_thi

animal_thi %>%
  select(GCAM_basin_ID, year, value ) %>%
  # filter(!is.na(value)) %>%
  as.data.table() ->
  dt
# define moving average time window
K = 20
### Centered Moving Average with Adaptive Edges ----
setkey(dt, GCAM_basin_ID, year)
dt[, y_ma := rollapply(value, width = K, FUN = mean, fill = NA, partial = TRUE, align = "center"),
   by = .(GCAM_basin_ID)]

dt %>% as.data.frame() %>%
  mutate(value = ifelse(is.na(y_ma), 1, y_ma)) %>%
  filter(year %in% c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS)) %>%
  group_by(GCAM_basin_ID) %>%
  left_join_error_no_match(basin_to_country_mapping, by = "GCAM_basin_ID") %>%
  mutate(iso = tolower(ISO)) %>%
  left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
  left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
  mutate(margin = 0,
         # follow Fig. 4b from Liu et al. (2024): https://www.nature.com/articles/s43247-024-01232-x/figures/4
         margin = ifelse(region %in% c("Canada", "Russia"), 0.875, margin),
         margin = ifelse(region %in% c("USA", "Central Asia",
                                       "European Free Trade Association"), 0.375, margin),
         margin = ifelse(region %in% c("Europe_Eastern", "China", "Taiwan", "Japan"), 0.125, margin),
         margin = ifelse(region %in% c("Europe_Non_EU", "Pakistan", "Argentina", "South Korea"), -0.125, margin),
         margin = ifelse(region %in% c( "South America_Southern", "Africa_Northern", "South Africa",
                                        "South Asia", "India", "Australia_NZ"), -0.375, margin),
         margin = ifelse(region %in% c("South America_Northern", "Colombia", "Brazil", "Mexico",
                                       "Central America and Caribbean","Middle East",
                                       "Africa_Eastern", "Africa_Western", "Africa_Southern",
                                       "Southeast Asia" , "Indonesia"), -0.625, margin),
         margin = margin / 100, # original margin is in %
         delta = value - value[year == 2015],
         delta = ifelse(year == 2015, 0, delta),
         animal_mult = (1 + margin)^delta) %>%
  group_by(region, year) %>%
  summarise(animal_mult = mean(animal_mult, na.rm = T)) ->
  HS_PC_GCAM_animal

saveRDS(HS_PC_GCAM_animal, file ="C:/Model/KLEAM/Animal_HS_MRI.rds")

#### XML ----
HS_PC_GCAM_animal <- readRDS("C:/Model/KLEAM/Animal_HS_MRI.rds")

inputs_of("module_aglu_an_input_xml") %>% load_from_cache() -> all_data

MODULE_INPUTS <-
  c("L202.RenewRsrc",
    "L202.RenewRsrcPrice",
    "L202.maxSubResource",
    "L202.RenewRsrcCurves",
    "L202.ResTechShrwt",
    "L202.UnlimitedRenewRsrcCurves",
    "L202.UnlimitedRenewRsrcPrice",
    "L202.Supplysector_in",
    "L202.SubsectorAll_in",
    "L202.SubsectorInterpTo_in",
    "L202.StubTech_in",
    "L202.StubTechInterp_in",
    "L202.GlobalTechCoef_in",
    "L202.GlobalTechShrwt_in",
    "L202.StubTechProd_in",
    "L202.Supplysector_an",
    "L202.SubsectorAll_an",
    "L202.GlobalTechShrwt_an",
    "L202.StubTechInterp_an",
    "L202.StubTechProd_an",
    "L202.StubTechCoef_an",
    "L202.StubTechCost_an",
    "L202.StubTechCost_For_proc",
    "L202.StubTechProd_in_Forest",
    "L202.StubTechProd_in_pulp_energy",
    "L2082.StubTechCoef_laborcapital_an_tfp_MA",
    "L2082.StubTechCost_an_adj")

get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


# only apply the heat stress to dairy and beef for future periods
# L202.StubTechCoef_an %>%
#   filter(year %in% MODEL_FUTURE_YEARS) %>%
#   filter(supplysector %in% c("Beef" , "Dairy")) %>%
#   left_join_error_no_match(HS_PC_GCAM_animal %>% rename(market.name = region),
#                            by = c("market.name", "year")) %>%
#   mutate(coefficient = coefficient / animal_mult) %>%
#   select(names(L202.StubTechCoef_an)) %>%
#   bind_rows(L202.StubTechCoef_an %>%
#               filter(year %in% MODEL_FUTURE_YEARS) %>%
#               filter(supplysector %in% c("Beef" , "Dairy"))) %>%
#   bind_rows(L202.StubTechCoef_an %>% filter(year %in% MODEL_BASE_YEARS)) ->
#   L202.StubTechCoef_an

L202.StubTechCoef_an %>%
  filter(year %in% MODEL_FUTURE_YEARS) %>%
  left_join_error_no_match(HS_PC_GCAM_animal %>% rename(market.name = region),
                           by = c("market.name", "year")) %>%
  filter(supplysector %in% c("Pork", "Poultry", "SheepGoat", "Beef", "Dairy")) %>%
  # poultry and swine are sensitive to heat stress, but they are mostly indoor
  mutate(delta = 1 - animal_mult,
         delta = ifelse(supplysector %in% c("Pork", "Poultry"),
                        delta * 0.5, delta),
         # sheepgoat is more resist to heat stress
         delta = ifelse(supplysector %in% c("SheepGoat"),
                        delta * 0.7, delta),
         delta = ifelse(subsector == "Mixed", 0.5 * delta, delta),
         prod_mult = 1 - delta,
         coefficient = coefficient / prod_mult) %>%
  group_by(year, supplysector, subsector) %>%
  summarise(y05 = quantile(prod_mult , 0.05),
            y25 = quantile(prod_mult , 0.25),
            y50 = quantile(prod_mult , 0.50),
            y75 = quantile(prod_mult , 0.75),
            y95 = quantile(prod_mult , 0.95),
            mean = mean(prod_mult)) ->
  live.prod.shock.box

saveRDS(live.prod.shock.box, file ="C:/Model/KLEAM/live.prod.shock.box.rds")

L202.StubTechCoef_an %>%
  filter(year %in% MODEL_FUTURE_YEARS) %>%
  left_join_error_no_match(HS_PC_GCAM_animal %>% rename(market.name = region),
                           by = c("market.name", "year")) %>%
  filter(supplysector %in% c("Pork", "Poultry", "SheepGoat", "Beef", "Dairy")) %>%
  # poultry and swine are sensitive to heat stress, but they are mostly indoor
  mutate(delta = 1 - animal_mult,
         delta = ifelse(supplysector %in% c("Pork", "Poultry"),
                        delta * 0.5, delta),
         # sheepgoat is more resist to heat stress
         delta = ifelse(supplysector %in% c("SheepGoat"),
                        delta * 0.7, delta),
         delta = ifelse(subsector == "Mixed", 0.5 * delta, delta),
         prod_mult = 1 - delta,
         coefficient = coefficient / prod_mult) %>%
  select(names(L202.StubTechCoef_an)) %>%
  bind_rows(L202.StubTechCoef_an %>% filter(year %in% MODEL_BASE_YEARS)) %>%
  bind_rows(L202.StubTechCoef_an %>% filter(year %in% MODEL_FUTURE_YEARS) %>%
              filter(supplysector %in% c("sawnwood_processing", "woodpulp_processing", "woodpulp_energy"))) ->
  L202.StubTechCoef_an_HS

create_xml("an_input_yld_HS_MRI.xml") %>%
  add_xml_data(L202.RenewRsrc, "RenewRsrc") %>%
  add_xml_data(L202.RenewRsrcPrice, "RenewRsrcPrice") %>%
  add_xml_data(L202.maxSubResource, "maxSubResource") %>%
  add_xml_data(L202.RenewRsrcCurves, "RenewRsrcCurves") %>%
  add_node_equiv_xml("resource") %>%
  add_node_equiv_xml("subresource") %>%
  add_xml_data(L202.ResTechShrwt, "ResTechShrwt") %>%
  add_xml_data(L202.UnlimitedRenewRsrcCurves, "UnlimitRsrc") %>%
  add_xml_data(L202.UnlimitedRenewRsrcPrice, "UnlimitRsrcPrice") %>%
  add_logit_tables_xml(L202.Supplysector_in, "Supplysector") %>%
  add_logit_tables_xml(L202.SubsectorAll_in, "SubsectorAll", "SubsectorLogit") %>%
  add_xml_data(L202.SubsectorInterpTo_in, "SubsectorInterpTo") %>%
  add_xml_data(L202.StubTech_in, "StubTech") %>%
  add_xml_data(L202.StubTechInterp_in, "StubTechInterp") %>%
  add_xml_data(L202.GlobalTechCoef_in, "GlobalTechCoef") %>%
  add_xml_data(L202.GlobalTechShrwt_in, "GlobalTechShrwt") %>%
  add_xml_data(L202.StubTechProd_in, "StubTechProd") %>%
  add_xml_data(L202.StubTechProd_in_Forest, "StubTechProd") %>%
  add_xml_data(L202.StubTechProd_in_pulp_energy, "StubTechProd") %>%
  add_logit_tables_xml(L202.Supplysector_an, "Supplysector") %>%
  add_logit_tables_xml(L202.SubsectorAll_an, "SubsectorAll", "SubsectorLogit") %>%
  add_xml_data(L202.GlobalTechShrwt_an, "GlobalTechShrwt") %>%
  add_xml_data(L202.StubTechInterp_an, "StubTechInterp") %>%
  add_xml_data(L202.StubTechProd_an, "StubTechProd") %>%
  add_xml_data(L202.StubTechCoef_an_HS, "StubTechCoef") %>%
  add_xml_data(L2082.StubTechCoef_laborcapital_an_tfp_MA, "StubTechCoef") %>%
  add_xml_data(L2082.StubTechCoef_laborcapital_an_tfp_MA, "StubPriceConversion") %>%
  add_xml_data(L2082.StubTechCost_an_adj, "StubTechCost") %>%
  add_xml_data(L202.StubTechCost_For_proc, "StubTechCost") %>%
  add_precursors(MODULE_INPUTS) ->
  an_input_eta.xml
an_input_eta.xml %>% gcamdata::run_xml_conversion()


## livestock labor & yield ----

HS_PC_GCAM_noncrop <- readRDS("C:/Model/HS_package/HELPS/Macro_HS/ANNUAL_WB_CanESM5_Food-CanESM5_Dunne_NONCROP.rds")

df.eta <- do.call(rbind, HS_PC_GCAM_noncrop)
basin_to_country_mapping %>% full_join(df.eta %>% rename(GCAM_basin_ID = region_id),
                                       by = "GCAM_basin_ID") -> df.eta.WB
print(dim(df.eta.WB))


# eta.all <-
df.eta.WB %>%
  filter(!is.na(GCAM_basin_ID)) %>%
  filter(!is.na(crop)) %>%
  mutate(iso = tolower(ISO)) %>%
  as_tibble() %>%
  left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
  left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
  group_by(region, year) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  arrange(region, year) %>%
  ungroup() %>%
  as.data.table() ->
  animal_eta

animal_eta %>%
  as.data.table() ->
  dt
# define moving average time window
K = 20
### Centered Moving Average with Adaptive Edges ----
setkey(dt, region, year)
dt[, y_ma := rollapply(value, width = K, FUN = mean, fill = NA, partial = TRUE, align = "center"),
   by = .(region)]


dt %>% as_tibble() %>%
  group_by(region, year) %>%
  summarise(value = mean(value),
            y_ma = mean(y_ma)) %>%
  mutate(value = ifelse(is.na(y_ma), 1, y_ma)) %>%
  filter(year %in% c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS)) %>%
  group_by(region) %>%
  mutate(IO_mult = value[year == MODEL_FINAL_BASE_YEAR]/value) %>%  # IO_t / IO_2015 = PWC_2015 / PWC_t; value = PWC
  select(-y_ma) ->
  HS_PC_GCAM_animal

saveRDS(HS_PC_GCAM_animal, file ="C:/Model/KLEAM/HELPS_GCAM_mapping_Dunne_Food_MRI_animal.rds")

HS_PC_GCAM_animal <- readRDS("C:/Model/KLEAM/HELPS_GCAM_mapping_Dunne_Food_MRI_animal.rds")

L2082.StubTechCoef_laborcapital_an_tfp_MA %>%
  filter(year %in% MODEL_FUTURE_YEARS) %>%
  left_join_error_no_match(HS_PC_GCAM_animal, by = c("region", "year")) %>%
  mutate(coefficient = ifelse(minicam.energy.input == "Labor_Ag", coefficient * IO_mult, coefficient )) %>%
  bind_rows(L2082.StubTechCoef_laborcapital_an_tfp_MA %>%
              filter(year %in% MODEL_BASE_YEARS)) ->
  L2082.StubTechCoef_laborcapital_an_HS


#### XML ----
create_xml("an_input_yld_eta_HS_MRI.xml") %>%
  add_xml_data(L202.RenewRsrc, "RenewRsrc") %>%
  add_xml_data(L202.RenewRsrcPrice, "RenewRsrcPrice") %>%
  add_xml_data(L202.maxSubResource, "maxSubResource") %>%
  add_xml_data(L202.RenewRsrcCurves, "RenewRsrcCurves") %>%
  add_node_equiv_xml("resource") %>%
  add_node_equiv_xml("subresource") %>%
  add_xml_data(L202.ResTechShrwt, "ResTechShrwt") %>%
  add_xml_data(L202.UnlimitedRenewRsrcCurves, "UnlimitRsrc") %>%
  add_xml_data(L202.UnlimitedRenewRsrcPrice, "UnlimitRsrcPrice") %>%
  add_logit_tables_xml(L202.Supplysector_in, "Supplysector") %>%
  add_logit_tables_xml(L202.SubsectorAll_in, "SubsectorAll", "SubsectorLogit") %>%
  add_xml_data(L202.SubsectorInterpTo_in, "SubsectorInterpTo") %>%
  add_xml_data(L202.StubTech_in, "StubTech") %>%
  add_xml_data(L202.StubTechInterp_in, "StubTechInterp") %>%
  add_xml_data(L202.GlobalTechCoef_in, "GlobalTechCoef") %>%
  add_xml_data(L202.GlobalTechShrwt_in, "GlobalTechShrwt") %>%
  add_xml_data(L202.StubTechProd_in, "StubTechProd") %>%
  add_xml_data(L202.StubTechProd_in_Forest, "StubTechProd") %>%
  add_xml_data(L202.StubTechProd_in_pulp_energy, "StubTechProd") %>%
  add_logit_tables_xml(L202.Supplysector_an, "Supplysector") %>%
  add_logit_tables_xml(L202.SubsectorAll_an, "SubsectorAll", "SubsectorLogit") %>%
  add_xml_data(L202.GlobalTechShrwt_an, "GlobalTechShrwt") %>%
  add_xml_data(L202.StubTechInterp_an, "StubTechInterp") %>%
  add_xml_data(L202.StubTechProd_an, "StubTechProd") %>%
  add_xml_data(L202.StubTechCoef_an_HS, "StubTechCoef") %>%
  add_xml_data(L2082.StubTechCoef_laborcapital_an_HS, "StubTechCoef") %>%
  add_xml_data(L2082.StubTechCoef_laborcapital_an_HS, "StubPriceConversion") %>%
  add_xml_data(L2082.StubTechCost_an_adj, "StubTechCost") %>%
  add_xml_data(L202.StubTechCost_For_proc, "StubTechCost") %>%
  add_precursors(MODULE_INPUTS) ->
  an_input_eta.xml
an_input_eta.xml %>% gcamdata::run_xml_conversion()


# livestock labor ----
#### XML ----
create_xml("an_input_eta_HS_MRI.xml") %>%
  add_xml_data(L202.RenewRsrc, "RenewRsrc") %>%
  add_xml_data(L202.RenewRsrcPrice, "RenewRsrcPrice") %>%
  add_xml_data(L202.maxSubResource, "maxSubResource") %>%
  add_xml_data(L202.RenewRsrcCurves, "RenewRsrcCurves") %>%
  add_node_equiv_xml("resource") %>%
  add_node_equiv_xml("subresource") %>%
  add_xml_data(L202.ResTechShrwt, "ResTechShrwt") %>%
  add_xml_data(L202.UnlimitedRenewRsrcCurves, "UnlimitRsrc") %>%
  add_xml_data(L202.UnlimitedRenewRsrcPrice, "UnlimitRsrcPrice") %>%
  add_logit_tables_xml(L202.Supplysector_in, "Supplysector") %>%
  add_logit_tables_xml(L202.SubsectorAll_in, "SubsectorAll", "SubsectorLogit") %>%
  add_xml_data(L202.SubsectorInterpTo_in, "SubsectorInterpTo") %>%
  add_xml_data(L202.StubTech_in, "StubTech") %>%
  add_xml_data(L202.StubTechInterp_in, "StubTechInterp") %>%
  add_xml_data(L202.GlobalTechCoef_in, "GlobalTechCoef") %>%
  add_xml_data(L202.GlobalTechShrwt_in, "GlobalTechShrwt") %>%
  add_xml_data(L202.StubTechProd_in, "StubTechProd") %>%
  add_xml_data(L202.StubTechProd_in_Forest, "StubTechProd") %>%
  add_xml_data(L202.StubTechProd_in_pulp_energy, "StubTechProd") %>%
  add_logit_tables_xml(L202.Supplysector_an, "Supplysector") %>%
  add_logit_tables_xml(L202.SubsectorAll_an, "SubsectorAll", "SubsectorLogit") %>%
  add_xml_data(L202.GlobalTechShrwt_an, "GlobalTechShrwt") %>%
  add_xml_data(L202.StubTechInterp_an, "StubTechInterp") %>%
  add_xml_data(L202.StubTechProd_an, "StubTechProd") %>%
  add_xml_data(L202.StubTechCoef_an, "StubTechCoef") %>%
  add_xml_data(L2082.StubTechCoef_laborcapital_an_HS, "StubTechCoef") %>%
  add_xml_data(L2082.StubTechCoef_laborcapital_an_HS, "StubPriceConversion") %>%
  add_xml_data(L2082.StubTechCost_an_adj, "StubTechCost") %>%
  add_xml_data(L202.StubTechCost_For_proc, "StubTechCost") %>%
  add_precursors(MODULE_INPUTS) ->
  an_input_eta.xml
an_input_eta.xml %>% gcamdata::run_xml_conversion()
