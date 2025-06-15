# generate water basin shock using HELPS package and BASD monthly output
# library(HELPS)

# read in HELPS outputs

# translate HELPS outputs to GCAM input
# read in required csv inputs
basin_to_country_mapping <- read.csv("inst/extdata/water/basin_to_country_mapping.csv", skip = 7, header = T)
# FAO_ag_items_PRODSTAT <- read.csv("inst/extdata/aglu/FAO/FAO_ag_items_PRODSTAT.csv", skip = 10, header = T)

L2082.AgCoef_laborcapital_bio_irr_mgmt <- load_from_cache("L2082.AgCoef_laborcapital_bio_irr_mgmt")[[1]]
L2082.AgCoef_laborcapital_ag_irr_mgmt <- load_from_cache("L2082.AgCoef_laborcapital_ag_irr_mgmt")[[1]]
# update labor IO under heat stress -------------------------------------

# get all subsectors ----
L2082.AgCoef_laborcapital_bio_irr_mgmt %>%
  filter(year == 2015, minicam.energy.input == "Labor_Ag") %>%
  separate(AgSupplySubsector, into = c("GCAM_Subsector", "waterbasin"), sep = "_") %>%
  select(AgSupplySector, GCAM_Subsector) %>% unique() -> biomass_sec

L2082.AgCoef_laborcapital_ag_irr_mgmt %>%
  filter(year == 2015, minicam.energy.input == "Labor_Ag") %>%
  separate(AgSupplySubsector, into = c("GCAM_Subsector", "waterbasin"), sep = "_") %>%
  select(AgSupplySector, GCAM_Subsector) %>% unique() -> crop_sec

biomass_sec %>% bind_rows(crop_sec) -> GCAM.sec; GCAM.sec

crop_mapping <- read.csv("C:/Model/HS_package/GCAM_SPAM_crop_mapping.csv") %>%
  select(SPAM, GCAM_Subsector) %>%
  mutate(SPAM = trimws(SPAM))

input.dir <- "C:/Model/HS_package/HELPS/Macro_HS/"
output.dir <- "../../"


# MRI-ESM2-0 ----
GCM <- "MRI"

eta_list <- list.files(input.dir, pattern = "ANNUAL_WB_MRI-ESM2-0", full.names = FALSE); eta_list

eta_list <- eta_list[!grepl("NONCROP", eta_list)] # no NONCROP mapping for GCAM crops

yld_MRI <- read.csv("C:/Model/gaia/agyield_impact_mri-esm2-0_r1i1p1f1_w5e5_Food-MRI.csv") %>%
  select(-X)


# CanESM5 ----
GCM <- "CanESM5"

eta_list <- list.files(input.dir, pattern = "ANNUAL_WB_CanESM5", full.names = FALSE); eta_list

eta_list <- eta_list[!grepl("NONCROP", eta_list)] # no NONCROP mapping for GCAM crops
# !!! need to update the gaia yield shock ----
# yld_MRI <- read.csv("C:/Model/gaia/agyield_impact_mri-esm2-0_r1i1p1f1_w5e5_Food-MRI.csv") %>%
#   select(-X)


# NorESM2-MM
GCM <- "NorESM2"

eta_list <- list.files(input.dir, pattern = "ANNUAL_WB_NorESM2", full.names = FALSE); eta_list

eta_list <- eta_list[!grepl("NONCROP", eta_list)] # no NONCROP mapping for GCAM crops

# !!! need to update the gaia yield shock ----
# yld_MRI <- read.csv("C:/Model/gaia/agyield_impact_mri-esm2-0_r1i1p1f1_w5e5_Food-MRI.csv") %>%
#   select(-X)


# start processing----

ETA <- list()
for (i in 1:length(eta_list)){
  eta <- readRDS(paste0(input.dir, eta_list[[i]]))
  df.eta <- do.call(rbind, eta) %>% filter(!is.na(region_id))
  basin_to_country_mapping %>% full_join(df.eta %>% rename(GCAM_basin_ID = region_id),
                                         by = "GCAM_basin_ID") -> df.eta.WB
  df.eta.WB$index = i
  ETA[[i]] <- df.eta.WB
  df.eta.WB
  print(dim(df.eta.WB))
}

eta.all <- do.call(rbind, ETA) %>%
  separate(crop, into = c("SPAM", "IRR_RFD"), sep = "_") # if include NONCROP, then NA values for IRR_RFD

eta.all %>% filter(!is.na(SPAM)) ->
  #Guam, Micronesia, Federated States of, French Southern Territories missing values
  eta.all

eta.all %>% filter(!is.na(smw)) %>%
  filter(SPAM != "NONCROP") %>% #
  left_join(crop_mapping, by = "SPAM", relationship = "many-to-many") %>%
  left_join(GCAM.sec, by = c("GCAM_Subsector")) %>%
  mutate(AgSupplySubsector = paste(GCAM_Subsector, GLU_name, sep = aglu.CROP_GLU_DELIMITER)) %>%
  group_by(GCAM_basin_ID, AgSupplySector, AgSupplySubsector, IRR_RFD, year) %>%
  summarise(value = weighted.mean(value, smw, na.rm = T)) %>%
  as_tibble() %>%
  repeat_add_columns(tibble(MGMT = c("hi", "lo"))) %>%
  mutate(IRR_RFD = ifelse(IRR_RFD == "I", "IRR", IRR_RFD),
         IRR_RFD = ifelse(IRR_RFD == "R", "RFD", IRR_RFD),
         AgProductionTechnology = paste(paste(AgSupplySubsector, IRR_RFD, sep = aglu.IRR_DELIMITER),
                                        MGMT, sep = aglu.MGMT_DELIMITER)) ->
  HS_PC

# smooth the heat stress shock ----
## define the window for moving average ---
library(data.table)
library(zoo)
HS_PC %>% select(-MGMT, -IRR_RFD) %>%
  # filter(!is.na(value)) %>%
  as.data.table() ->
  dt
# define moving average time window
K = 20
### Centered Moving Average with Adaptive Edges ----
setkey(dt, GCAM_basin_ID, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year)
dt[, y_ma := rollapply(value, width = K, FUN = mean, fill = NA, partial = TRUE, align = "center"),
   by = .(GCAM_basin_ID, AgSupplySector, AgSupplySubsector, AgProductionTechnology)]

dt %>% as.data.frame() %>%
  mutate(value = ifelse(is.na(y_ma), 1, y_ma)) %>%
  filter(year %in% c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS)) %>%
  group_by(AgSupplySector, AgSupplySubsector, AgProductionTechnology) %>%
  mutate(IO_mult = value[year == MODEL_FINAL_BASE_YEAR]/value) %>%  # IO_t / IO_2015 = PWC_2015 / PWC_t; value = PWC
  select(-y_ma) ->
  HS_PC_GCAM

saveRDS(HS_PC_GCAM, file =paste0("C:/Model/KLEAM/HELPS_GCAM_mapping_Dunne_Food_",GCM,".rds"))

# build XMLS ----


# read in gaia impact for land scaling ----

inputs_of("module_aglu_ag_prodchange_ref_IRR_MGMT_xml") %>% load_from_cache() -> all_data
L2052.AgProdChange_ag_irr_ref <- get_data(all_data, "L2052.AgProdChange_ag_irr_ref")
L2052.AgProdChange_bio_irr_ref <- get_data(all_data, "L2052.AgProdChange_bio_irr_ref")

yld_ref <- L2052.AgProdChange_ag_irr_ref %>%
  bind_rows(L2052.AgProdChange_bio_irr_ref)

yld_ref %>% mutate(scenario = "Ref") %>%
  bind_rows(yld_MRI %>% mutate(scenario = GCM)) %>%
  arrange(scenario, region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year) %>%
  mutate(inter_mult = (1+AgProdChange)^5) %>%
  group_by(scenario, region, AgSupplySector, AgSupplySubsector, AgProductionTechnology) %>%
  mutate(mult = cumprod(inter_mult)) %>%
  group_by(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year) %>%
  mutate(yld_mult = mult / mult[scenario == "Ref"],
         AIO_mult = 1 / yld_mult) %>%
  filter(scenario == GCM) %>%
  select(-inter_mult, -mult, -scenario) ->
  yld_IO_gaia


# read in HELPS labor impact ----
HS_PC_GCAM <- readRDS(paste0("C:/Model/KLEAM/HELPS_GCAM_mapping_Dunne_Food_",GCM,".rds"))

# read in XML gcamdata inputs ----
inputs_of("module_aglu_ag_input_IRR_MGMT_xml") %>% load_from_cache() -> all_data

MODULE_INPUTS <-
  c("L2062.AgCoef_Fert_ag_irr_mgmt",
    "L2062.AgCoef_Fert_bio_irr_mgmt",
    "L2082.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA",
    "L2082.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA",
    "L2082.AgCoef_laborcapital_for_tfp_MA")

get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


outputs_of("module_aglu_L2082.ag_laborcapital_irr_mgmt") %>% load_from_cache() -> all_data

MODULE_OUTPUTS <-
  c("L2082.AgCoef_laborcapital_ag_irr_mgmt",
    "L2082.AgCoef_laborcapital_bio_irr_mgmt",
    "L2082.AgCoef_laborcapital_for")

get_data_list(all_data, MODULE_OUTPUTS, strip_attributes = TRUE)

## ag ----

L2082.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA %>%
  filter(year >= 2025) %>%
  left_join_error_no_match(HS_PC_GCAM, by = c("AgSupplySector","AgSupplySubsector","AgProductionTechnology","year")) %>%
  mutate(coefficient = ifelse(minicam.energy.input == "Labor_Ag", coefficient*IO_mult, coefficient)) %>% # apply HS multiplier to labor IO
  ungroup() %>%
  select(colnames(L2082.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA)) %>%
  bind_rows(L2082.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA %>% filter(year < 2025)) ->
  L2082.AgCoef_laborcapital_ag_irr_mgmt_HS

L2082.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA %>%
  filter(year >= 2025) %>%
  left_join_error_no_match(yld_IO_gaia, by = c("region", "AgSupplySector","AgSupplySubsector","AgProductionTechnology","year")) %>%
  mutate(coefficient = coefficient*AIO_mult) %>% # apply yld multiplier to labor and capital IO
  ungroup() %>%
  select(colnames(L2082.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA)) %>%
  bind_rows(L2082.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA %>% filter(year < 2025)) ->
  L2082.AgCoef_laborcapital_ag_irr_mgmt_gaia

L2082.AgCoef_laborcapital_ag_irr_mgmt_gaia %>%
  filter(year >= 2025) %>%
  left_join_error_no_match(HS_PC_GCAM, by = c("AgSupplySector","AgSupplySubsector","AgProductionTechnology","year")) %>%
  mutate(coefficient = ifelse(minicam.energy.input == "Labor_Ag", coefficient*IO_mult, coefficient)) %>% # apply HS multiplier to labor IO
  ungroup() %>%
  select(colnames(L2082.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA)) %>%
  bind_rows(L2082.AgCoef_laborcapital_ag_irr_mgmt_gaia %>% filter(year < 2025)) ->
  L2082.AgCoef_laborcapital_ag_irr_mgmt_gaia_HS

## bio ----
L2082.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA %>%
  filter(year >= 2025) %>%
  left_join_error_no_match(HS_PC_GCAM, by = c("AgSupplySector","AgSupplySubsector","AgProductionTechnology","year")) %>%
  mutate(coefficient = ifelse(minicam.energy.input == "Labor_Ag", coefficient*IO_mult, coefficient)) %>% # apply HS multiplier to labor IO
  ungroup() %>%
  select(colnames(L2082.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA)) %>%
  bind_rows(L2082.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA %>% filter(year < 2025)) ->
  L2082.AgCoef_laborcapital_bio_irr_mgmt_HS

L2082.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA %>%
  filter(year >= 2025) %>%
  left_join_error_no_match(yld_IO_gaia, by = c("region", "AgSupplySector","AgSupplySubsector","AgProductionTechnology","year")) %>%
  mutate(coefficient = coefficient*AIO_mult) %>% # apply yld multiplier to labor and capital IO
  ungroup() %>%
  select(colnames(L2082.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA)) %>%
  bind_rows(L2082.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA %>% filter(year < 2025)) ->
  L2082.AgCoef_laborcapital_bio_irr_mgmt_gaia

L2082.AgCoef_laborcapital_bio_irr_mgmt_gaia %>%
  filter(year >= 2025) %>%
  left_join_error_no_match(HS_PC_GCAM, by = c("AgSupplySector","AgSupplySubsector","AgProductionTechnology","year")) %>%
  mutate(coefficient = ifelse(minicam.energy.input == "Labor_Ag", coefficient*IO_mult, coefficient)) %>% # apply HS multiplier to labor IO
  ungroup() %>%
  select(colnames(L2082.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA)) %>%
  bind_rows(L2082.AgCoef_laborcapital_bio_irr_mgmt_gaia %>% filter(year < 2025)) ->
  L2082.AgCoef_laborcapital_bio_irr_mgmt_gaia_HS


#####################################---------------------------------------------





# make sure to include the price.unit.conversion into XML
# otherwise, will see negative profit error in calibration years
create_xml(paste0("xml/ag_input_IRR_MGMT_HELPS_Food_",GCM,"_Dunne_LS.xml")) %>%
  add_xml_data(L2062.AgCoef_Fert_ag_irr_mgmt, "AgCoef") %>%
  add_xml_data(L2082.AgCoef_laborcapital_ag_irr_mgmt_gaia_HS, "AgCoef") %>%
  add_xml_data(L2082.AgCoef_laborcapital_ag_irr_mgmt_gaia_HS, "AgPriceConversion") %>%
  add_xml_data(L2062.AgCoef_Fert_bio_irr_mgmt, "AgCoef") %>%
  add_xml_data(L2082.AgCoef_laborcapital_bio_irr_mgmt_gaia_HS, "AgCoef") %>%
  add_xml_data(L2082.AgCoef_laborcapital_bio_irr_mgmt_gaia_HS, "AgPriceConversion") %>%
  add_xml_data(L2082.AgCoef_laborcapital_for_tfp_MA, "AgCoef") %>%
  add_xml_data(L2082.AgCoef_laborcapital_for_tfp_MA, "AgPriceConversion") %>%
  add_precursors("L2082.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA",
                 "L2082.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA",
                 "L2082.AgCoef_laborcapital_for_tfp_MA",
                 "L2062.AgCoef_Fert_ag_irr_mgmt",
                 "L2062.AgCoef_Fert_bio_irr_mgmt") ->
  ag_input_IRR_MGMT_HS.xml
ag_input_IRR_MGMT_HS.xml %>% gcamdata::run_xml_conversion()


create_xml(paste0("xml/ag_input_IRR_MGMT_HELPS_Food_",GCM,"_Dunne_OS.xml")) %>%
  add_xml_data(L2062.AgCoef_Fert_ag_irr_mgmt, "AgCoef") %>%
  add_xml_data(L2082.AgCoef_laborcapital_ag_irr_mgmt_HS, "AgCoef") %>%
  add_xml_data(L2082.AgCoef_laborcapital_ag_irr_mgmt_HS, "AgPriceConversion") %>%
  add_xml_data(L2062.AgCoef_Fert_bio_irr_mgmt, "AgCoef") %>%
  add_xml_data(L2082.AgCoef_laborcapital_bio_irr_mgmt_HS, "AgCoef") %>%
  add_xml_data(L2082.AgCoef_laborcapital_bio_irr_mgmt_HS, "AgPriceConversion") %>%
  add_xml_data(L2082.AgCoef_laborcapital_for_tfp_MA, "AgCoef") %>%
  add_xml_data(L2082.AgCoef_laborcapital_for_tfp_MA, "AgPriceConversion") %>%
  add_precursors("L2082.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA",
                 "L2082.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA",
                 "L2082.AgCoef_laborcapital_for_tfp_MA",
                 "L2062.AgCoef_Fert_ag_irr_mgmt",
                 "L2062.AgCoef_Fert_bio_irr_mgmt") ->
  ag_input_IRR_MGMT_HS.xml
ag_input_IRR_MGMT_HS.xml %>% gcamdata::run_xml_conversion()


create_xml(paste0("xml/ag_input_IRR_MGMT_gaia_Food_",GCM,"_LS.xml")) %>%
  add_xml_data(L2062.AgCoef_Fert_ag_irr_mgmt, "AgCoef") %>%
  add_xml_data(L2082.AgCoef_laborcapital_ag_irr_mgmt_gaia, "AgCoef") %>%
  add_xml_data(L2082.AgCoef_laborcapital_ag_irr_mgmt_gaia, "AgPriceConversion") %>%
  add_xml_data(L2062.AgCoef_Fert_bio_irr_mgmt, "AgCoef") %>%
  add_xml_data(L2082.AgCoef_laborcapital_bio_irr_mgmt_gaia, "AgCoef") %>%
  add_xml_data(L2082.AgCoef_laborcapital_bio_irr_mgmt_gaia, "AgPriceConversion") %>%
  add_xml_data(L2082.AgCoef_laborcapital_for_tfp_MA, "AgCoef") %>%
  add_xml_data(L2082.AgCoef_laborcapital_for_tfp_MA, "AgPriceConversion") %>%
  add_precursors("L2082.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA",
                 "L2082.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA",
                 "L2082.AgCoef_laborcapital_for_tfp_MA",
                 "L2062.AgCoef_Fert_ag_irr_mgmt",
                 "L2062.AgCoef_Fert_bio_irr_mgmt") ->
  ag_input_IRR_MGMT_HS.xml
ag_input_IRR_MGMT_HS.xml %>% gcamdata::run_xml_conversion()
