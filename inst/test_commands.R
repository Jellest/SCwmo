
#projectes shade classes
test_ssa.csv <- fread("data/solar_shadow_angles/solar_shadow_angles.csv", data.table = FALSE)
test_ssa_criteria <- projected_shade_class(test_ssa.csv)

