library(HELPS)

SOI_LIST <- SECTOR_ALL

# SOI_LIST <- c("CNUT_I", "OILP_I", "OOIL_I", "COFF_I",
#               "RCOF_I", "COCO_I", "RUBB_I", "TEAS_I", "TOBA_I", "REST_I", "WHEA_R", "RICE_R", "MAIZ_R", "SOYB_R",
#               "BARL_R", "MILL_R", "PMIL_R", "SORG_R", "OCER_R", "POTA_R", "SWPO_R", "YAMS_R", "CASS_R", "BEAN_R",
#               "CHIC_R", "COWP_R", "PIGE_R", "LENT_R", "GROU_R", "SUNF_R", "RAPE_R", "SESA_R", "SUGC_R", "SUGB_R",
#               "COTT_R", "OFIB_R", "BANA_R", "PLNT_R", "CITR_R", "TROF_R", "TEMF_R", "TOMA_R", "ONIO_R", "VEGE_R",
#               "ORTS_R", "OPUL_R", "CNUT_R", "OILP_R", "OOIL_R", "COFF_R", "RCOF_R", "COCO_R", "RUBB_R", "TEAS_R",
#               "TOBA_R", "REST_R", "NONCROP")




LHR_Dunne <- function(WBGT, workload = NULL){ # Dunne et al., 2013
  eta = ifelse(WBGT <= 25, 1, 1 - 0.25*((WBGT - 25)^(2/3)))
}

YEAR <- seq(2015, 2100, 1); YEAR # all future years
# YEAR <- seq(2015, 2100, 5); YEAR # GCAM model years


# MRI ----
start_t = Sys.time()
for (s in 1:length(SOI_LIST)){
  ANNUAL_REG <- list()
  SOI <- SOI_LIST[[s]]
  print(SOI)
  for (i in 1:length(YEAR)){
    YEAR_INPUT <- YEAR[i]
    print(YEAR_INPUT)
    esi.mon <- cal_heat_stress(TempRes = "month", SECTOR = SOI, HS = WBGT_ESI, YEAR_INPUT = YEAR_INPUT,
                               "basd/MRI-ESM2-0_STITCHES_W5E5v2_Food-MRI_hurs_global_monthly_2015_2100.nc",
                               "basd/MRI-ESM2-0_STITCHES_W5E5v2_Food-MRI_tas_global_monthly_2015_2100.nc",
                               "basd/MRI-ESM2-0_STITCHES_W5E5v2_Food-MRI_rsds_global_monthly_2015_2100.nc")

    pwc.mon <- cal_pwc(WBGT = esi.mon,  LHR = LHR_Dunne, workload = "high")
    rm(esi.mon)
    pwc.ann <- monthly_to_annual(input_rack = pwc.mon, SECTOR = SOI)
    rm(pwc.mon)
    # ANNUAL_GRID[[i]] <- pwc.foster.ann
    reg_pwc <- grid_to_region(grid_annual_value = pwc.ann, SECTOR = SOI, rast_boundary = reg_WB_raster)
    rm(pwc.ann)
    ANNUAL_REG[[i]] <- reg_pwc %>% dplyr::mutate(crop = SOI, year = YEAR_INPUT)
  }
  saveRDS(ANNUAL_REG, file = paste0("Macro_HS/ANNUAL_WB_MRI-ESM2-0_Food-MRI_Dunne_", SOI,".rds"))
}
end_t = Sys.time()
end_t - start_t



# CanESM5 ----

start_t = Sys.time()
for (s in 1:length(SOI_LIST)){
  ANNUAL_REG <- list()
  SOI <- SOI_LIST[[s]]
  print(SOI)
  for (i in 1:length(YEAR)){
    YEAR_INPUT <- YEAR[i]
    print(YEAR_INPUT)
    esi.mon <- cal_heat_stress(TempRes = "month", SECTOR = SOI, HS = WBGT_ESI, YEAR_INPUT = YEAR_INPUT,
                               "basd/CanESM5_STITCHES_W5E5v2_Food-CanESM5_hurs_global_monthly_2015_2100.nc",
                               "basd/CanESM5_STITCHES_W5E5v2_Food-CanESM5_tas_global_monthly_2015_2100.nc",
                               "basd/CanESM5_STITCHES_W5E5v2_Food-CanESM5_rsds_global_monthly_2015_2100.nc")

    pwc.mon <- cal_pwc(WBGT = esi.mon,  LHR = LHR_Dunne, workload = "high")
    rm(esi.mon)
    pwc.ann <- monthly_to_annual(input_rack = pwc.mon, SECTOR = SOI)
    rm(pwc.mon)
    # ANNUAL_GRID[[i]] <- pwc.foster.ann
    reg_pwc <- grid_to_region(grid_annual_value = pwc.ann, SECTOR = SOI, rast_boundary = reg_WB_raster)
    rm(pwc.ann)
    ANNUAL_REG[[i]] <- reg_pwc %>% dplyr::mutate(crop = SOI, year = YEAR_INPUT)
  }
  saveRDS(ANNUAL_REG, file = paste0("Macro_HS/ANNUAL_WB_CanESM5_Food-CanESM5_Dunne_", SOI,".rds"))
}
end_t = Sys.time()
end_t - start_t
