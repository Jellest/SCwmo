#Temperature
class_c <- c(1:5)
criteria_colNames <-  c("Class", "Criteria")

V_Temp_slope_ct <- c(10, 10, 25, -1, -1) 
V_Temp_slope <- data.frame(class=1:5, criteria=V_Temp_slope_ct)

V_Temp_vege_ct <- c(19, 19, -1, -1, -1)
V_Temp_vege <- data.frame(class=1:5, criteria=V_Temp_vege_ct)

V_Temp_shade_ct <- c(5, 7, 7, 20, -1)
V_Temp_shade <- data.frame(class=1:5, criteria=V_Temp_shade_ct)

V_Temp_water_ct <- c(100, 30, 10, -1, -1)
V_Temp_water <- data.frame(class=1:5, criteria=V_Temp_water_ct)

V_Temp_source_ct <- c(100, 30, 10, 10, -1)
V_Temp_water <- data.frame(class=1:5, criteria=V_Temp_source_ct)