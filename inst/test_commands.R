
#projectes shade classes
test_ssa.csv <- fread("data/solar_shadow_angles/solar_shadow_angles.csv", data.table = FALSE)
test_ssa_criteria <- projected_shade_class(test_ssa.csv)


##import bgt
BGT_DeBilt <- import_bgt("De Bilt", "temp_150cm")
BGT_HvH <-import_bgt("Hoek van Holland", "site")

BGT_DeBilt <- import_single_bgt("De Bilt", "site", delete_raw_gmls = TRUE)
