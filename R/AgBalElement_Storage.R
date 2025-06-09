
## Compile data ----
# storage is commented out!

MapAgCOMM %>% pull(regsector)


# SCENARIO <- c("L_HS1_GFDL", "L_HS1_HadGEM", "L_HS2_GFDL", "L_HS2_HadGEM")
SCENARIO <- Scenario[!Scenario %in% c("KNX_ref")]; SCENARIO




## Start with demand ----
"AgBal" %>% PluckBind() %>% 
  filter(Units == "Mt") %>%
  mutate(input = gsub("regional |_", "", input) ) %>%
  mutate(sector = if_else(grepl("NonFoodDemand", sector), "Other use", sector),
         sector = if_else(grepl("ethanol|biomassOil", sector), "Bioenergy", sector),
         sector = if_else(grepl("FoodDemand", sector), "Food", sector),
         sector = if_else(grepl("FeedCrops|Fodder", sector), "Feed", sector)) %>%
  Agg_reg(branch, region, element = sector, sector = input) %>% 
  ## Bind storage ----
  # bind_rows(
  # "AgStorageClose" %>% PluckBind() %>%
  #   transmute(branch, scenario, year,region, sector, value, element = "ClosingStockBeforeLoss") %>%
  #   bind_rows("AgStorageOpen" %>% PluckBind()  %>% transmute(branch, scenario, year, region, sector, value, element = "Opening stocks")) %>%
  #   bind_rows("AgStorageCloseLoss" %>% PluckBind()  %>% transmute(branch, scenario, year,region, sector, value, element = "Closing stocks")) %>%
  #   mutate(sector = gsub("regional |_", "", sector)) %>%
  #   spread(element, value) %>%
  #   mutate(`Stock loss` = ClosingStockBeforeLoss -`Closing stocks`) %>%
  #   select(-ClosingStockBeforeLoss) %>%
  #   gather(element, value, -branch:-sector) ) %>%
  ## Bind Trade ----
bind_rows(
  "RegAgsource" %>% PluckBind() %>%
    mutate(sector = gsub("total |_", "", sector)) %>%
    filter(grepl("imported", subsector)) %>%
    Agg_reg(branch, region, sector, element = "Import") %>%
    bind_rows(
      "TradedAgsource" %>% PluckBind() %>%
        filter(Units == "Mt") %>%
        mutate(sector = gsub("traded |_", "", sector)) %>%
        group_by(sector) %>%
        mutate(subsector = gsub(unique(sector),"", subsector),
               subsector = gsub("nuts_seeds|root_tuber","", subsector),
               subsector = gsub(" traded ","", subsector)) %>%
        ungroup() %>%
        transmute(branch, scenario, region = subsector, sector, year, value, element = "Export")
    )
) %>%
  # Bind Production ----
bind_rows(
  "Agprod" %>% PluckBind() %>%
    filter(Units == "Mt") %>%
    Agg_reg(branch, region, sector = tolower(sector), element = "Production") %>%
    bind_rows(
      "MeatProd" %>% PluckBind() %>% filter(Units == "Mt") %>%
        Agg_reg(branch, region, sector = tolower(sector), element = "Production")
    ) 
) %>% filter(sector != "iron and steel") -> AgElement_SUA
# ** Done AgElement_SUA ----

AgElement_SUA %>% 
  filter(element == "Production") %>%
  spread(element, value) %>%
  left_join_error_no_match(
    "Agprices" %>% PluckBind() %>% #  1975$/kg
      bind_rows("Meatprices" %>% PluckBind()) %>%
      mutate(sector = tolower(sector)) %>%
      rename(Price = value) %>% select(-Units),
    by = c("scenario", "branch", "region", "sector", "year") ) ->
  AgMeatPrice

# **Done AgMeatPrice----

AgElement_SUA %>% filter(element == "Production") %>%
  spread(element, value) %>%
  inner_join(
    "Aggland" %>% PluckBind() %>%
      mutate(LandLeaf = gsub("C4$|Tree$", "", LandLeaf) ) %>%
      group_by(scenario, region, sector = LandLeaf, year) %>%
      summarise(Area = sum(value)/10, .groups = "drop") %>% # to Mha
      mutate(sector = tolower(sector)),
    by = c("scenario", "region", "sector", "year")
  ) %>%
  mutate(Yield = Production / Area) %>% select(-Production) %>%
  gather(element, value, Area, Yield) %>%
  bind_rows(
    AgElement_SUA %>% filter(element == "Production") %>%
      spread(element, value) %>%
      left_join_error_no_match(
        "Agprices" %>% PluckBind() %>%
          bind_rows("Meatprices" %>% PluckBind()) %>%
          mutate(sector = tolower(sector)) %>%
          rename(Price = value) %>% select(-Units) ) %>%
      mutate(Revenue = Production * Price) %>%
      select(-Production) %>%
      gather(element, value, Revenue, Price)
  ) ->
  AgElement_AreaYieldPrice


# AgElement_SUA %>%
#   bind_rows(AgElement_AreaYieldPrice) -> AgElement



# Good with data ----

c("Corn", "Wheat", "Rice", "OtherGrain", "RootTuber",
  "Soybean", "OilCrop", "OilPalm", 
  "FiberCrop", "SugarCrop", "Legumes", "NutsSeeds", "MiscCrop",
  "Vegetables", "Fruits",
  "Beef", "Dairy", "SheepGoat", "Pork", "Poultry",
  "OtherMeatFish") ->
  GCAMCOMM

c("Production", "Import") -> ElEMSupply
c("Export", "Food", "Feed",  "Bioenergy", "Other use") -> ELEMDemand
c(ElEMSupply, ELEMDemand) -> ELEMAll

c(ElEMSupply, ELEMDemand %>% rev) -> ELEMLevel
c(paste0("Supply: ", ElEMSupply), paste0("Demand: ", ELEMDemand) %>% rev ) -> ELEMLabel

library("RColorBrewer")
c(brewer.pal(n = length(ELEMAll), name = "BrBG")[1:length(ElEMSupply)],
  brewer.pal(n = length(ELEMAll), name = "BrBG")[length(ElEMSupply)+1:length(ELEMDemand)]
) -> ColUpdate


# PSUA ----
AgElement_SUA %>%
  # rename(region0 = region) %>%
  # left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG10_AR6)) %>%
  filter(year >= 2015, element %in% ELEMAll) %>%
  Agg_reg(branch, element, sector, region) %>%
  mutate(sector = factor(sector, levels = tolower(GCAMCOMM))) %>%
  mutate(DS = if_else(element %in% ElEMSupply, "Supply", "Demand")) %>%
  mutate(value = if_else(!element %in% ElEMSupply, -value, value)) %>%
  mutate(element = factor(element, levels = ELEMLevel,
                          labels = ELEMLabel)) -> PSUA

# PAgPrice ----
AgMeatPrice %>%
  # rename(region0 = region) %>%
  # left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG10_AR6), by = "region0") %>%
  rename(sector0 = sector) %>%
  left_join_error_no_match(
    MapAgCOMM %>% transmute(sector0 = tolower(AgCOMM), sector = AgCOMM4),
    by = "sector0") %>%
  filter(sector != "Pasture", year >= 2015) %>% 
  group_by_at(vars(scenario, year, branch, region, sector)) %>%
  summarise(value = weighted.mean(Price, w = Production), prod = sum(Production), .groups = "drop") %>%
  drop_na() ->
  PAgPrice


AgMeatPrice %>%
  rename(sector0 = sector) %>%
  left_join_error_no_match(
    MapAgCOMM %>% transmute(sector0 = tolower(AgCOMM), sector = AgCOMM4),
    by = "sector0") %>%
  filter(sector != "Pasture", year >= 2015) %>% 
  group_by_at(vars(scenario, year, branch, region, sector)) %>%
  summarise(value = weighted.mean(Price, w = Production), prod = sum(Production), .groups = "drop") %>%
  drop_na() ->
  PAgPrice_32
