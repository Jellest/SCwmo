generate_map <- function(aws_name, sensor_name, aws.df = AWS.df, ahn, bgt.sf) {
  
  aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name)
  single_aws <- select_single_aws(aws.df = aws.df, aws_name = aws_name, sensor_name =sensor_name)
  
  buffer_100m <- createBuffer(single_aws[["aws_rd.sf"]], distance = 100)
  buffer_30m <- createBuffer(single_aws[["aws_rd.sf"]], distance = 30)
  buffer_10m <- createBuffer(single_aws[["aws_rd.sf"]], distance = 10)
  buffer_5m <- createBuffer(single_aws[["aws_rd.sf"]], distance = 5)
  buffer_3m <- createBuffer(single_aws[["aws_rd.sf"]], distance = 3)

  
  
  
    
}