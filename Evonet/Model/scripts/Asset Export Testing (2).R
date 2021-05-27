
files <- list.files(paste0(getwd(), "//Exports"))

# a <- fread(paste0(getwd(), "//Exports//", files[1]))
# b <- fread(paste0(getwd(), "//Exports//", files[2]))
# c <- fread(paste0(getwd(), "//Exports//", files[3]))
# d <- fread(paste0(getwd(), "//Exports//", files[4]))

req.names <- c("Result", "Health_Index_Asset_Category", "Asset_Number", "Timestep", "Quantity Resource", "LevelCode",
               "Intervention Costs", "Future PoF", "Financial Consequence Cost","Safety Consequence Cost", 
               "Environmental Consequence Cost", "Network Consequence Cost","Monetised Risk Cost", 
               "Probability of Failure Mode One", "Probability of Failure Mode Two","Probability of Failure Mode Three",
               "Number of Customer Interruptions", "CML","Health Index", "Criticality Index", "Stepwise NPV")

for(i in 1:length(files)){
   dt <- fread(paste0(getwd(), "//Exports//", files[i]))
   
   if(is.null(dt$LevelCode)){
     dt$LevelCode <- dt$`Level Code`
   }  
   
   req <- names(dt) %in% req.names
   dt <- dt[, req, with = F]
   
   if(is.null(dt$Result)){
     dt$Result <- files[i]
   }  
   
      if(i == 1){
     output <- dt
   } else {
     output <- rbind(output, dt)
   }
}

unique(output$Result)

customers <- output[grepl("Customer", output$Result)]
customers$Result <- "Maintain Customer Interruptions Level by Asset Class"
ni <- output[grepl("No Investment", output$Result)]

output <- output[!(grepl("Customer", output$Result))]

output$Result <- ifelse(grepl("Maintain", output$Result), "Maintain Risk", output$Result)
output$Result <- ifelse(grepl("No Investment", output$Result), "No Investment", output$Result)
output$Result <- ifelse(grepl("Increase", output$Result), "Increase", output$Result)
output$Result <- ifelse(grepl("Decrease", output$Result), "Decrease", output$Result)

main <- output[grepl("Maintain", output$Result)]
main$Result <- "Maintain Risk by Asset Class"

no.cao <- rbind(main, ni, customers)

#output[, .(sum(`Monetised Risk Cost`)), by = list(Health_Index_Asset_Category, Result, Timestep)]

fwrite(output, "Exports for CAO.csv")

if.null.blank <- function(x){
  if(is.null(x)){
    x <- ""
  }
}

req.names <- c("Result", "AssetId", "Health_Index_Asset_Category", "Asset_Number", "Coast_Side",
               "Timestep", "Quantity Resource", "LevelCode", "Relation_3",
               "Intervention Costs", "Future PoF", "Financial Consequence Cost","Safety Consequence Cost", 
               "Environmental Consequence Cost", "Network Consequence Cost","Monetised Risk Cost", 
               "Probability of Failure Mode One", "Probability of Failure Mode Two","Probability of Failure Mode Three",
               "Number of Customer Interruptions", "CML","Health Index", "Criticality Index")

for(i in 1:length(files)){
  dt <- fread(paste0(getwd(), "//Exports//", files[i]))
  
  if(is.null(dt$LevelCode)){
    dt$LevelCode <- dt$`Level Code`
    
  }
  
  dt$Coast_Side <- ifelse(dt$Coast_Side == "", dt$Transformer_Coast_Side, dt$Coast_Side)
  if(!is.null(dt$SubType)){
    dt$Health_Index_Asset_Category <- ifelse(dt$SubType == "Pettersen Coil", "EHV Pettersen Coil", 
                                             dt$Health_Index_Asset_Category)}
    
  req <- names(dt) %in% req.names
  dt <- dt[, req, with = F]
  
  if(is.null(dt$Result)){
    dt$Result <- files[i]
  }  
  
  if(i == 1){
    for.dashboard <- dt
  } else {
    for.dashboard <- rbind(for.dashboard, dt)
  }
}

unique(for.dashboard$Result) 

customers <- for.dashboard[grepl("Customer", for.dashboard$Result)]
customers$Result <- "Maintain Customer Interruptions by Asset Class"
ni <- for.dashboard[grepl("No Investment", for.dashboard$Result)]
ni$Result <- "No Investment"
main <- for.dashboard[grepl("Maintain", for.dashboard$Result)]
main <- main[!(grepl("Customer", main$Result))]
main$Result <- "Maintain Risk by Asset Class"

for.dashboard <- rbind(customers, ni, main)
fwrite(for.dashboard, "No Need for CAO Exports.csv")
