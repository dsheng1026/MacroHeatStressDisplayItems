# Pasture heat stress ----

# use heat stress yield impact of OtherGrain to approximate pasture heat stress yield impact
# Pasture has C3 and C4, therefore use both OtherGrain and OtherGrain4

AgYld_ref <- readRDS("C:/Model/heat_paper/Result/paper_MacroHS/data/HeatStress/AgYld_ref.rds")
AgYld_gaia <- read.csv("C:/Model/gaia/agyield_impact_mri-esm2-0_r1i1p1f1_w5e5_Food-MRI.csv") %>% select(-X)

AgYld_ref %>%
  filter(AgSupplySector %in% c("OtherGrain")) %>%
  arrange(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year) %>%
  mutate(scenario = "Ref") %>%
  bind_rows(AgYld_gaia %>% filter(AgSupplySector %in% c("OtherGrain")) %>%
              arrange(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year) %>%
              mutate(scenario = "HS")) %>%
  separate(AgSupplySubsector, into = c("Crop", "WB"), "_") %>%
  group_by(scenario, region, AgSupplySector, WB, year) %>%
  summarise(AgProdChange = mean(AgProdChange)) %>%
  mutate(AgSupplySector = "Pasture",
         AgSupplySubsector = paste0(AgSupplySector, "_", WB),
         AgProductionTechnology = AgSupplySubsector) %>%
  ungroup() %>%
  arrange(scenario, region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year) %>%
  spread(scenario, AgProdChange) %>%
  mutate(AgProdChange = (HS - Ref)/(1+Ref)) %>%
  select(names(AgYld_ref)) ->
  Pasture_HS

# Note:
# heat stress yld impact = (1+d')^5/(1+d)^5 for GCAM crops, where d is AgProdChange in Ref, d' is in HS
# Pasture yld does not change in the Ref, set Pasture heat stress yld AgProdChange = x
# (1+x)^5 = (1+d')^5/(1+d)^5
# x = (1+d')/(1+d) - 1 = (d'-d)/(1+d)


# generate XML ----
create_xml("ag_prodchange_Pasture_MRI.xml") %>%
  add_xml_data(Pasture_HS, "AgProdChange") ->
  ag_prodchange_Pasture.xml

ag_prodchange_Pasture.xml %>% gcamdata::run_xml_conversion()
