path_hourly_data <- "0.0-data/household_data_60min_singleindex.csv"
path_hourly_data_long <- "0.0-data/household_data_60min_singleindex_long.csv"
path_ref_units <- "0.0-data/household_data_ref_units.csv"
regex_col_names <- "DE_KN_([[:alpha:]]+)([[:digit:]]?)_([[:alpha:]|_(?=[:alpha:]+)]+)([[:digit:]])?"
cols_grouping <- c("household_type", "id_household", "energy_type", "id_ener_source")
cols_ener <- c(
  "DE_KN_industrial1_grid_import",
  "DE_KN_industrial1_pv_1",
  "DE_KN_industrial1_pv_2",
  "DE_KN_industrial2_grid_import",
  "DE_KN_industrial2_pv",
  "DE_KN_industrial2_storage_charge",
  "DE_KN_industrial2_storage_decharge",
  "DE_KN_industrial3_area_offices",
  "DE_KN_industrial3_area_room_1",
  "DE_KN_industrial3_area_room_2",
  "DE_KN_industrial3_area_room_3",
  "DE_KN_industrial3_area_room_4",
  "DE_KN_industrial3_compressor",
  "DE_KN_industrial3_cooling_aggregate",
  "DE_KN_industrial3_cooling_pumps",
  "DE_KN_industrial3_dishwasher",
  "DE_KN_industrial3_ev",
  "DE_KN_industrial3_grid_import",
  "DE_KN_industrial3_machine_1",
  "DE_KN_industrial3_machine_2",
  "DE_KN_industrial3_machine_3",
  "DE_KN_industrial3_machine_4",
  "DE_KN_industrial3_machine_5",
  "DE_KN_industrial3_pv_facade",
  "DE_KN_industrial3_pv_roof",
  "DE_KN_industrial3_refrigerator",
  "DE_KN_industrial3_ventilation",
  "DE_KN_public1_grid_import",
  "DE_KN_public2_grid_import",
  "DE_KN_residential1_dishwasher",
  "DE_KN_residential1_freezer",
  "DE_KN_residential1_grid_import",
  "DE_KN_residential1_heat_pump",
  "DE_KN_residential1_pv",
  "DE_KN_residential1_washing_machine",
  "DE_KN_residential2_circulation_pump",
  "DE_KN_residential2_dishwasher",
  "DE_KN_residential2_freezer",
  "DE_KN_residential2_grid_import",
  "DE_KN_residential2_washing_machine",
  "DE_KN_residential3_circulation_pump",
  "DE_KN_residential3_dishwasher",
  "DE_KN_residential3_freezer",
  "DE_KN_residential3_grid_export",
  "DE_KN_residential3_grid_import",
  "DE_KN_residential3_pv",
  "DE_KN_residential3_refrigerator",
  "DE_KN_residential3_washing_machine",
  "DE_KN_residential4_dishwasher",
  "DE_KN_residential4_ev",
  "DE_KN_residential4_freezer",
  "DE_KN_residential4_grid_export",
  "DE_KN_residential4_grid_import",
  "DE_KN_residential4_heat_pump",
  "DE_KN_residential4_pv",
  "DE_KN_residential4_refrigerator",
  "DE_KN_residential4_washing_machine",
  "DE_KN_residential5_dishwasher",
  "DE_KN_residential5_grid_import",
  "DE_KN_residential5_refrigerator",
  "DE_KN_residential5_washing_machine",
  "DE_KN_residential6_circulation_pump",
  "DE_KN_residential6_dishwasher",
  "DE_KN_residential6_freezer",
  "DE_KN_residential6_grid_export",
  "DE_KN_residential6_grid_import",
  "DE_KN_residential6_pv",
  "DE_KN_residential6_washing_machine"
)
