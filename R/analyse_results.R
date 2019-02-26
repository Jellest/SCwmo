analyse_differences <- function(AHN3 = FALSE){
  if(AHN3 == FALSE){
    AHN <- "AHN2"
    aws_names_list <- AWS_temperature_names
  } else {
    AHN <- "AHN3"
    aws_names_list <- AWS_temperature_ahn3Only_names
  }
  
  if(AHN3 == TRUE){
    summary_table_ahn3 <- fread(paste0("output/", AHN, "_summary_aws_classifications_final.csv"), data.table = FALSE)
    rownames(summary_table_ahn3) <- AWS_temperature_ahn3Only_names
    #summary_table <- results[["summary"]]
    summary_table_ahn3$shading_class1Count <- NA
    summary_table_ahn3$shading_class2Count <- NA
    summary_table_ahn3$shading_class3Count <- NA
    summary_table_ahn3$shading_class4Count <- NA
    summary_table_ahn3$shading_class5Count <- NA
    rownames(summary_table_ahn3) <- aws_names_list
    for (r in 1:nrow(summary_table_ahn3)){
      row <- summary_table_ahn3[r,]
      aws_name <- row[1,"AWS"]
      aws_name_trim <- getAWS_name_trim(aws_name = aws_name)
      
  
      auto_class <- row[1,"final class"]
      manual_class <- row[1,"manual_class_R"]
      overview_shading_table <- select(fread(paste0("output/", aws_name_trim, "/solar_shadow_angles/", aws_name_trim, "_", AHN, "_ah_solar_shadow_angles_classes.csv"), data.table = FALSE)[1,],class1_count, class2_count, class3_count, class4_count, class5_count)#results[["AWS"]][[aws_name]][["overview_shading_table"]]
      #View(overview_shading_table)
      count1 <- overview_shading_table[1,"class1_count"] 
      count2 <- overview_shading_table[1,"class2_count"] 
      count3 <- overview_shading_table[1,"class3_count"]
      count4 <- overview_shading_table[1,"class4_count"]
      count5 <- overview_shading_table[1,"class5_count"]
      summary_table_ahn3[r, "shading_class1Count"] <- count1
      summary_table_ahn3[r, "shading_class2Count"] <- count2
      summary_table_ahn3[r, "shading_class3Count"] <- count3
      summary_table_ahn3[r, "shading_class4Count"] <- count4
      summary_table_ahn3[r, "shading_class5Count"] <- count5 
    }
    fwrite(summary_table_ahn3, "output/ahn3_summary_results.csv")
  } else { #AHN2
    summary_table_ahn2 <- fread(paste0("output/", AHN, "_summary_aws_classifications_final.csv"), data.table = FALSE)
    summary_table_ahn2 <- filter(summary_table_ahn2, AWS %in% AWS_temperature_ahn2Only_names)

    #summary_table <- results[["summary"]]
    summary_table_ahn2$shading_class1Count <- NA
    summary_table_ahn2$shading_class2Count <- NA
    summary_table_ahn2$shading_class3Count <- NA
    summary_table_ahn2$shading_class4Count <- NA
    summary_table_ahn2$shading_class5Count <- NA
    #rownames(summary_table_ahn2) <- AWS_temperature_ahn2Only_names
    
    for (r in 1:length(AWS_temperature_ahn2Only_names)){
      #break
      row <- summary_table_ahn2[r,]
      aws_name <- AWS_temperature_ahn2Only_names[r]
      aws_name_trim <- getAWS_name_trim(aws_name = aws_name)
      
      
      auto_class <- row[1,"final class"]
      manual_class <- row[1,"manual_class_R"]
      overview_shading_table <- select(fread(paste0("output/", aws_name_trim, "/solar_shadow_angles/", aws_name_trim, "_", AHN, "_ah_solar_shadow_angles_classes.csv"), data.table = FALSE)[1,],class1_count, class2_count, class3_count, class4_count, class5_count)#results[["AWS"]][[aws_name]][["overview_shading_table"]]
      #View(overview_shading_table)
      count1 <- overview_shading_table[1,"class1_count"] 
      count2 <- overview_shading_table[1,"class2_count"] 
      count3 <- overview_shading_table[1,"class3_count"]
      count4 <- overview_shading_table[1,"class4_count"]
      count5 <- overview_shading_table[1,"class5_count"]
      summary_table_ahn2[r, "shading_class1Count"] <- count1
      summary_table_ahn2[r, "shading_class2Count"] <- count2
      summary_table_ahn2[r, "shading_class3Count"] <- count3
      summary_table_ahn2[r, "shading_class4Count"] <- count4
      summary_table_ahn2[r, "shading_class5Count"] <- count5 
    }
    rownames(summary_table_ahn2) <- AWS_temperature_ahn2Only_names
    fwrite(summary_table_ahn2, "output/ahn2_summary_results.csv")
    
    summary_table_all <- fread("output/ahn3_summary_results.csv", data.table = FALSE)
    rownames(summary_table_all) <- AWS_temperature_ahn3Only_names
    
    #View(summary_table_all)
    summary_table_all <- rbind(summary_table_all, summary_table_ahn2)
    summary_table_all <- summary_table_all[order(summary_table_all[,'AWS']), ]
    fwrite(summary_table_all, "output/all_summary_results.csv")
    
    View(summary_table_all)
  }
}

View(fread(paste0("output/", "AHN2", "_summary_aws_classifications_final.csv"), data.table = FALSE))
View(fread(paste0("output/", "AHN3", "_summary_aws_classifications_final.csv"), data.table = FALSE))

analyse_differences(AHN3 = FALSE)

analyses_results_ahn2 <- analyse_differences(AHN3 = FALSE)
analyses_results_ahn3 <- analyse_differences(AHN3 = TRUE)

no_shadow_angles_ahn2 <- View(filter(analyses_results_ahn2, shading_class4Count == 0 & shading_class5Count == 0))
no_shadow_angles_ahn3 <- View(filter(analyses_results_ahn3, shading_class4Count == 0 & shading_class5Count == 0))

overview_results <- data.frame(AWS = as.character(), AHN2_partial_match = as.logical(), AHN3_partial_match = as.logical(), AHN2_noShading = as.logical(), AHN3_noShading = as.logical(), Detailed_BGT = as.logical(),stringsAsFactors = FALSE)

for(a in 1:length(AWS_temperature_names)){
  aws_name <- AWS_temperature_names[a]
  overview_results[a,"AWS"] <- aws_name
  
  if(analyses_results_ahn2[aws_name, "has AHN3"] == FALSE){
    overview_results[a,"AHN3_partial_match"] <- NA
    overview_results[a,"AHN3_noShading"] <- NA
    
    #ahn3 ONLY  
  } else {
    overview_results[a,"auto_AHN3_Finalclass"] <- analyses_results_ahn3[aws_name, "final_class"]
    overview_results[a,"auto_AHN3_shadingclass"] <- analyses_results_ahn3[aws_name, "shades_class"]
    overview_results[a,"manual_class"] <- analyses_results_ahn3[aws_name, "Manual_class_R"]
    if(analyses_results_ahn3[aws_name, "AHN selected"] == "AHN3"){
      if(analyses_results_ahn3[aws_name, "Partial match"] == TRUE){
        overview_results[a,"AHN3_partial_match"] <- TRUE
      } else {
        overview_results[a,"AHN3_partial_match"] <- FALSE
        if(analyses_results_ahn3[aws_name, "shading_class2Count"] == 0
           & analyses_results_ahn3[aws_name, "shading_class3Count"] == 0
           & analyses_results_ahn3[aws_name, "shading_class4Count"] == 0
           & analyses_results_ahn3[aws_name, "shading_class5Count"] == 0){
          overview_results[a, "AHN3_noShading"] <- TRUE
        } else {
          overview_results[a, "AHN3_noShading"] <- FALSE
        }
      }
    }
  }
  
  
  
  
  
  overview_results[a,"auto_AHN2_Finalclass"] <- analyses_results_ahn2[aws_name, "final_class"]
  overview_results[a,"auto_AHN2_shadingclass"] <- analyses_results_ahn2[aws_name, "shades_class"]
  overview_results[a,"auto_objectsclass"] <- analyses_results_ahn2[aws_name, "objects_class"]
  overview_results[a,"manual_class"] <- analyses_results_ahn2[aws_name, "Manual_class_R"]
  if(analyses_results_ahn2[aws_name, "AHN selected"] == "AHN2"){
    if(analyses_results_ahn2[aws_name, "Partial match"] == TRUE){
      overview_results[a,"AHN2_partial_match"] <- TRUE
    } else {
      overview_results[a,"AHN2_partial_match"] <- FALSE
      if(analyses_results_ahn2[aws_name, "shading_class2Count"] == 0
         & analyses_results_ahn2[aws_name, "shading_class3Count"] == 0
         & analyses_results_ahn2[aws_name, "shading_class4Count"] == 0
         & analyses_results_ahn2[aws_name, "shading_class5Count"] == 0){
        overview_results[a, "AHN2_noShading"] <- TRUE
      } else {
        overview_results[a, "AHN2_noShading"] <- FALSE
      }
    }
  }
}
View(overview_results)
View(filter(overview_results, AHN3_partial_match == FALSE))


final_results <- function(){
  results <- fread("output/all_summary_results.csv", data.table = FALSE)
  results[,"automated_class_is"] <- 0
  results[,"class_difference"] <- 0
  for(r in 1:nrow(results)){
    if(results[r,"exact_match"] == FALSE){
      final_class <- results[r,"final_class"]
      manual_class_l <- results[r,"manual_class_Rl"]
      manual_class_h <- results[r,"manual_class_Rh"]
      if(final_class < manual_class_l){
        difference <- manual_class_l - final_class  
        string <- ""
        for(n in 1:difference){
          string <- paste0(string, "-")
        }
        results[r,"automated_class_is"] <- string
        results[r,"class_difference"] <- difference
      }
      if(final_class > manual_class_l){
        difference <- final_class - manual_class_h
        string <- ""
        for(n in 1:difference){
          string <- paste0(string, "+")
        }
        results[r,"automated_class_is"] <- string
        results[r,"class_difference"] <- difference
      }
    }
  }
  #class(results[,"class_difference"]) <- as.numeric(levels(results[,"class_difference"]))[results[,"class_difference"]]#results[, "class_difference"])
  View(results)
  fwrite(results, "output/all_final_results.csv")
return (results)}
final_results <- final_results()

table <- xtable(select(final_results, AWS, AHN_selected, final_class, manual_class_R, automated_class_is)) 
print.xtable(table,
             type = getOption("xtable.type", "latex"),
             include.rownames = getOption("xtable.include.rownames", FALSE),
             booktabs = getOption("xtable.booktabs", TRUE)
             )
View(select(final_results, AWS, AHN_selected, shades_class, objects_class, vegetation_height_class, partial_match, manual_class_R, automated_class_is ))
xtable((select(final_results, AWS, AHN_selected, shades_class, objects_class, vegetation_height_class, partial_match, manual_class_R, automated_class_is )))


dr <- function(){
  data <- fread("output/all_final_results.csv", data.table = FALSE)
  shades_counter <- 0
  objects_counter <- 0
  shadesObjects_counter <- 0
  for(r in 1:nrow(data)){
    count_shade <- FALSE
    final_class <- data[r, "final_class"]
    shades_class <- data[r,"shades_class"]
    objects_class <- data[r,"objects_class"]
    if(shades_class == final_class){
      shades_counter <- shades_counter + 1
      count_shade <- TRUE
    }
    if(objects_class == final_class){
      objects_counter <- objects_counter + 1
      if(count_shade == TRUE){
        shadesObjects_counter <- shadesObjects_counter + 1
      }
    }
    
  }
  
  final_class1_count <- nrow(subset(data,final_class == 1))
  final_class2_count <- nrow(subset(data,final_class == 2))
  final_class3_count <- nrow(subset(data,final_class == 3))
  final_class4_count <- nrow(subset(data,final_class == 4))
  final_class5_count <- nrow(subset(data,final_class == 5))
  
    
    
  shadesOnly_counter <- shades_counter - shadesObjects_counter
  objectsOnly_counter <- objects_counter - shadesObjects_counter
  
  relShadesOnly <- shadesOnly_counter / nrow(data)
  relObjectsOnly <- objectsOnly_counter / nrow(data)
  relShadesObjects_counter <- shadesObjects_counter / nrow(data)
  
  class_count<- data.frame(c(final_class1_count, final_class2_count,  final_class3_count, final_class4_count, final_class5_count, shadesOnly_counter, objectsOnly_counter, shadesObjects_counter))
  rownames(class_count) <- c("class 1 count" , 
                    "class 2 count",
                    "class 3 count",
                    "class 4 count",
                    "class 5 count",
                    "shades count",
                    "objects count",
                    "shadesObjects count")
  colnames(class_count) <- "count"
  
  relatives <- c()
  for(r in 1:nrow(class_count)){
    relative <- round((class_count[r,"count"] / nrow(data)), digits = 3) 
    relatives <- append(relatives, relative)
  }
  relatives.df <- data.frame(relatives)
  colnames(relatives.df) <- "relative"
  
  class_count <- cbind(class_count, relatives.df)
  
  View(class_count)
  fwrite(class_count, "output/class_counts.csv")
  shades_distr <- data.frame(AWS = as.character(), total_count = as.numeric(), notClass1_count = as.numeric(), notClass1_rel = as.numeric(),class_1 = as.numeric(), class_1rel = as.numeric(), class_2 = as.numeric(), class_2rel = as.numeric(), class_3 = as.numeric(), class_3rel = as.numeric(), class_4 = as.numeric(), class_4rel = as.numeric(), class_5 = as.numeric(), class_5rel = as.numeric())
  names(shades_distr) <- c("AWS", "total_count", "notClass1_count", "notClass1_rel", "class1_count", "class1_rel", "class2_count", "class2_rel", "class3_count", "class3_rel", "class4_count", "class4_rel", "class5_count", "class5_rel")
  for(n3 in AWS_temperature_ahn3Only_names){
    aws_name_trim <- getAWS_name_trim(aws_name = n3)
    shadesOverview <- fread(paste0("output/", aws_name_trim, "/solar_shadow_angles/", aws_name_trim, "_AHN3_ah_solar_shadow_angles_classes.csv"),data.table = FALSE)[1,]
    class1_count <- shadesOverview[1,"class1_count"]
    class2_count <- shadesOverview[1,"class2_count"]
    class3_count <- shadesOverview[1,"class3_count"]
    class4_count <- shadesOverview[1,"class4_count"]
    class5_count <- shadesOverview[1,"class5_count"]
    total_count <- class1_count + class2_count + class3_count + class4_count + class5_count
    total_not1_count <- total_count - class1_count
    
    not1_relCount <- round((total_not1_count / total_count), digits = 3)
    class1_relCount <- round((shadesOverview[1,"class1_count"] / total_count), digits = 3)
    class2_relCount <- round((shadesOverview[1,"class2_count"] / total_count), digits = 3)
    class3_relCount <- round((shadesOverview[1,"class3_count"] / total_count), digits = 3)
    class4_relCount <- round((shadesOverview[1,"class4_count"] / total_count), digits = 3)
    class5_relCount <- round((shadesOverview[1,"class5_count"] / total_count), digits = 3)
    
    input <- data.frame(n3, total_count, total_not1_count, not1_relCount, class1_count, class1_relCount, class2_count, class2_relCount, class3_count, class3_relCount, class4_count, class4_relCount, class5_count, class5_relCount, stringsAsFactors = FALSE)
    names(input) <- c("AWS", "total_count", "notClass1_count", "notClass1_rel", "class1_count", "class1_rel", "class2_count", "class2_rel", "class3_count", "class3_rel", "class4_count", "class4_rel", "class5_count", "class5_rel")
    rownames(input) <- n3
    shades_distr <- rbind(shades_distr, input)

  }
  for(n2 in AWS_temperature_ahn2Only_names){
    aws_name_trim <- getAWS_name_trim(aws_name = n2)
    shadesOverview <- fread(paste0("output/", aws_name_trim, "/solar_shadow_angles/", aws_name_trim, "_AHN2_ah_solar_shadow_angles_classes.csv"),data.table = FALSE)[1,]
    class1_count <- shadesOverview[1,"class1_count"]
    class2_count <- shadesOverview[1,"class2_count"]
    class3_count <- shadesOverview[1,"class3_count"]
    class4_count <- shadesOverview[1,"class4_count"]
    class5_count <- shadesOverview[1,"class5_count"]
    total_count <- class1_count + class2_count + class3_count + class4_count + class5_count
    total_not1_count <- total_count - class1_count
    
    not1_relCount <- round((total_not1_count / total_count), digits = 3)
    class1_relCount <- round((shadesOverview[1,"class1_count"] / total_count), digits = 3)
    class2_relCount <- round((shadesOverview[1,"class2_count"] / total_count), digits = 3)
    class3_relCount <- round((shadesOverview[1,"class3_count"] / total_count), digits = 3)
    class4_relCount <- round((shadesOverview[1,"class4_count"] / total_count), digits = 3)
    class5_relCount <- round((shadesOverview[1,"class5_count"] / total_count), digits = 3)
    
    input <- data.frame(n2, total_count, total_not1_count, not1_relCount, class1_count, class1_relCount, class2_count, class2_relCount, class3_count, class3_relCount, class4_count, class4_relCount, class5_count, class5_relCount, stringsAsFactors = FALSE)
    names(input) <- c("AWS", "total_count", "notClass1_count", "notClass1_rel", "class1_count", "class1_rel", "class2_count", "class2_rel", "class3_count", "class3_rel", "class4_count", "class4_rel", "class5_count", "class5_rel")
    
    rownames(input) <- n2
    shades_distr <- rbind(shades_distr, input)
    
  }
  shades_distr <- shades_distr[order(rownames(shades_distr)),]
  print(mean(unlist(shades_distr$class1_rel)))
  print(sd(unlist(shades_distr$class1_rel))) 
  View(shades_distr)
  fwrite(shades_distr, "output/all_shades_distribution.csv")
}
dr()

