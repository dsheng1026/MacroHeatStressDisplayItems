# generate results across ESMs

PluckBind_sen <- function(.query ){
  ListVFood %>% purrr::pluck(.query) %>%
    mutate(branch = scenario, scenario = ss) %>%
    filter(scenario %in% Scenario)
}

SCE_SEN <- function(.data){
  .data %>% 
    mutate(ESM = ifelse(grepl("MRI", scenario), "MRI-ES2-0", "ESM"),
           ESM = ifelse(grepl("CanESM5", scenario), "CanESM5", ESM),
           ESM = ifelse(grepl("NorESM2", scenario), "NorESM2-MM", ESM),
           scenario = ifelse(grepl("HS", scenario), scenario, "Ref"),
           scenario = gsub("food_HS_", "", scenario),
           scenario = gsub("_MRI", "", scenario),
           scenario = gsub("_CanESM5", "", scenario),
           scenario = gsub("_NorESM2", "", scenario)) %>% 
    return()
}

# climate forcing ----
PluckBind_sen("TotalClimateForcing") %>% 
  SCE_SEN() -> 
  df.CF

df.CF %>% 
  group_by(ESM, year) %>% 
  mutate(index = 100 * value / value[scenario == "Ref"] - 100) %>% 
  ggplot() +
  geom_line(aes(x = year, y = index, color = scenario)) +
  facet_wrap(~ ESM) +
  labs(x = "", y = "%") +
  theme_bw()


df.CF %>% 
  group_by(ESM, year) %>% 
  mutate(index = 100 * value / value[scenario == "Ref"] - 100) %>% 
  filter(scenario == "CL_LS") %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "darkgrey") +
  geom_line(aes(x = year, y = index, color = ESM), linewidth = 0.8) +
  labs(x = "", y = "%", title = "Relative changes in global total climate forcing:\nCombined_LS") +
  ylim(-1, 1) +
  theme_bw() + theme0 + theme1 ->
  FigSen.climate.forcing

Write_png(FigSen.climate.forcing, "FigSen.climate.forcing", DIR_MODULE, w = 8, h = 6, r = 300)

# GMT ----
PluckBind_sen("GMT") %>% 
  SCE_SEN() -> 
  df.GMT

df.GMT %>% 
  group_by(ESM, year) %>% 
  mutate(delta = value - value[scenario == "Ref"]) %>% 
  ggplot() +
  geom_line(aes(x = year, y = delta, color = scenario)) +
  facet_wrap(~ ESM) +
  labs(x = "", y = "degree-C") +
  theme_bw()


df.GMT %>% 
  group_by(ESM, year) %>% 
  mutate(delta = value - value[scenario == "Ref"]) %>% 
  filter(scenario == "CL_LS") %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "darkgrey") +
  geom_line(aes(x = year, y = delta, color = ESM), linewidth = 0.8) +
  labs(x = "", y = "degree-C", title = "Absolute changes in global mean temperature:\nCombined_LS") +
  ylim(-0.01, 0.01) +
  theme_bw() + theme0 + theme1 ->
  FigSen.GMT; FigSen.GMT

Write_png(FigSen.GMT, "FigSen.GMT", DIR_MODULE, w = 8, h = 6, r = 300)

# GDP ----

PluckBind_sen("SAM_NA") %>% 
  SCE_SEN() %>% 
  # select(scenario, region, Account, year, value) %>% 
  filter(Account == "GDP") -> 
  df.GDP

### global trend ----

df.GDP %>% 
  group_by(ESM, scenario, Account, Units, year) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  group_by(ESM, year) %>% 
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
  facet_wrap(~ ESM, nrow = 1) +
  labs(x = "", y = "", fill = "Factor") +
  theme_bw() + themeds + theme(legend.position = "none") ->
  FigSen.GDP.glb; FigSen.GDP.glb

Write_png(FigSen.GDP.glb, "FigSen.GDP.glb", DIR_MODULE, w = 8, h = 6, r = 300)

## boxplot: 32 regions across ESM ----

df.GDP %>% 
  group_by(ESM, scenario, Account, Units, year) %>% 
  group_by(ESM, region, year) %>% 
  mutate(index = 100*(value / value[scenario == "Ref"] -1)) %>% 
  filter(year >= 2015) %>% 
  filter(scenario == scenario_target) %>% 
  SCE_NAME() ->
  dg.gdp.layer

dg.gdp.layer %>% 
  group_by(ESM, scenario, year) %>% 
  summarise(
    y05 = quantile(index, 0.05, na.rm = TRUE),
    y25 = quantile(index, 0.25, na.rm = TRUE),
    y50 = quantile(index, 0.50, na.rm = TRUE),
    y75 = quantile(index, 0.75, na.rm = TRUE),
    y95 = quantile(index, 0.95, na.rm = TRUE),
    ymean = mean(index, na.rm = TRUE)) %>% 
  filter(year %in% c(2050, 2075, 2100)) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(x = year, color = ESM)) +
  geom_boxplot(
    aes(lower = y25, upper = y75, middle = y50, ymin = y05, ymax = y95),
    stat = "identity") +
  geom_errorbar(
    aes(ymin = ymean, ymax = ymean),
    width = 0.8, linetype = "dashed", 
    position = position_dodge(0.9)) +
  labs(x = "", y = "%", color = "ESM",
       title = "Boxplot of relative changes in regional GDP") +
  theme_bw() + themeds ->
  FigSen.GDP.box; FigSen.GDP.box

Write_png(FigSen.GDP.box, "FigSen.GDP.box", DIR_MODULE, w = 6, h = 6, r = 300)

# Ag market output ----

source("R/AgBalElement_Storage_sen.R")

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
    "Agprices" %>% PluckBind_sen() %>%
      bind_rows("Meatprices" %>% PluckBind_sen()) %>%
      mutate(sector = tolower(sector)) %>%
      rename(Price = value) %>% select(-Units) ) %>% 
  mutate(Revenue = Production * Price) %>% 
  gather(element, value, Revenue, Production, Consumption , Price) ->
  AgElement_AreaYieldPrice

AgElement_AreaYieldPrice %>% 
  filter(sector != "pasture") %>% 
  mutate(group = ifelse(sector %in% c("beef", "dairy","pork","poultry","sheepgoat" ), "animal", sector),
         group = ifelse(group %in% c("corn", "rice", "soybean", "wheat"), "Key" ,group),
         group = ifelse(group %in% c("Key", "animal"), group, "others")) %>%  
  group_by(scenario, region, group, year, element) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  spread(element, value) %>% 
  mutate(Price = Revenue / Production) %>% 
  gather(account, value, Consumption:Revenue) %>% 
  SCE_SEN() ->
  AYPP_32

AYPP_32 %>% 
  group_by(ESM, region, group, year, account) %>% 
  mutate(index = 100 * (value / value[scenario == "Ref"] - 1)) %>% 
  filter(scenario == scenario_target) %>% 
  group_by(ESM, scenario, group, year, account) %>% 
  summarise(
    y05 = quantile(index, 0.05, na.rm = TRUE),
    y25 = quantile(index, 0.25, na.rm = TRUE),
    y50 = quantile(index, 0.50, na.rm = TRUE),
    y75 = quantile(index, 0.75, na.rm = TRUE),
    y95 = quantile(index, 0.95, na.rm = TRUE),
    ymean = mean(index, na.rm = TRUE)) %>% 
  filter(year == 2100) %>% 
  ggplot(aes(x = account, color = ESM)) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.8) +
  geom_boxplot(
    aes(lower = y25, upper = y75, middle = y50, ymin = y05, ymax = y95),
    stat = "identity") +
  geom_errorbar(
    aes(ymin = ymean, ymax = ymean),
    width = 0.8, linetype = "dashed", 
    position = position_dodge(0.9)) +
  facet_wrap(~ group) +
  labs(x = "", y = "%", color = "ESM",
       title = "Boxplot of relative changes in regional agricultural market outcomes:\nCombined LS, 2100") +
  theme_bw() + themeds ->
  FigSen.PQR.box; FigSen.PQR.box

Write_png(FigSen.PQR.box, "FigSen.PQR.box", DIR_MODULE, w = 12, h = 6, r = 300)

