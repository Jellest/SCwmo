#Temperature
class_c <- c(1:5)
criteria_colNames <-  c("Class", "Criteria")

V_Temp_slope_ct <- c(19, 19, -1, -1, -1) 
V_Temp_slope <- data.frame(class=1:5, criteria=V_Temp_slope_ct)

V_Temp_openSpace_ct < c(50, 25, -1, -1, -1)
V_Temp_openSpace <- data.frame(class=1:5, criteria=V_Temp_openSpace_ct)
;
V_Temp_vege_ct <- c(10, 10, 25, -1, -1)
V_Temp_vege <- data.frame(class=1:5, criteria=V_Temp_vege_ct)

V_Temp_shade_ct <- c(5, 7, 7, 20, -1)
V_Temp_shade <- data.frame(class=1:5, criteria=V_Temp_shade_ct)

V_Temp_water_ct <- c(100, 30, 10, -1, -1)
V_Temp_water <- data.frame(class=1:5, criteria=V_Temp_water_ct)

V_Temp_source_ct <- c(100, 30, 10, 10, -1)
V_Temp_water <- data.frame(class=1:5, criteria=V_Temp_source_ct)

#rad 1
V_Temp_SourceRad1_ct <- c(100, 30, 10, -1, -1)
V_Temp_SourceRad1Rel_ct <- c(0.05, 0,1, 0.1, -1, -1)
V_Temp_SourceRad1 <- data.frame(class=1:5, criteria=V_Temp_SourceRad1_ct, relative=V_Temp_SourceRad1Rel_ct)

V_Temp_SourceRad2_ct <- c(10:30, 5:10, -1, -1, -1)

#rad 2
#V_Temp_SourceRad2Rel_ct <- c(0.10, 0,05, -1, -1, -1)

V_Temp_SourceRad2_ctC1 <- length(c(10:30))
V_Temp_SourceRad2_ctC2 <- length(c(5:10))
V_Temp_SourceRad2_ctC3C5 <- c(-1, -1, -1)
V_Temp_SourceRad2Rel_c3c5 <- c(3,4,5)

V_Temp_SourceRad2Rel_ct1 <- rep(0.01,V_Temp_SourceRad2_ctC1)
V_Temp_SourceRad2Rel_ct2 <- rep(0.01,V_Temp_SourceRad2_ctC2)

V_Temp_SourceRad2Rel <- c(V_Temp_SourceRad2Rel_ct1, V_Temp_SourceRad2Rel_ct2, V_Temp_SourceRad2Rel_c3c5)

rep1 <- rep(1,V_Temp_SourceRad2_ctC1)
rep2 <- rep(2,V_Temp_SourceRad2_ctC2)

V_Temp_SourceRad2Rel_class <- c(rep1, rep2, V_Temp_SourceRad2Rel_c3c5)

V_Temp_SourceRad2 <- data.frame(class=V_Temp_SourceRad2Rel_class, criteria=V_Temp_SourceRad2_ct, relative=V_Temp_SourceRad2Rel_ct)

#rad 3
V_Temp_SourceRad3_ct <- c(10, 5, 5, -1, -1)
V_Temp_SourceRad3Rel_ct <- c(0.01, 0.01, 0.05, -1, -1)
V_Temp_SourceRad3 <- data.frame(class=1:5, criteria=V_Temp_SourceRad3_ct, relative=V_Temp_SourceRad3Rel_ct)

  