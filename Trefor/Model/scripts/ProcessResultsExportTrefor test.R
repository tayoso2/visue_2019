
gc()
setwd("C:/Users/TOsosanya/Desktop/Electricity/Danish DNOs/Trefor/Model")

# load libraries
library(data.table)
library(tidyverse)
library(stringi)
library(pbapply)

# Function to detect attributes
GetAttributeNames <- function(result, timesteps = NULL) {
  # Estimate timesteps unless supplied
  if (is.null(timesteps)) {
    timesteps <- GetTimestepsNumber(result)
  }
  # Get table of column information
  col.info <- ResultColumnSummary(result)
  # Build timesteps sequence
  rev.t.seq <-
    rep(seq(timesteps - 1, 0, -1), floor(nrow(col.info) / timesteps))
  rev.t.seq <-
    c(rev.t.seq, rep(NA, nrow(col.info) - length(rev.t.seq)))
  col.info[, rev.t.seq := rev(rev.t.seq), ]
  # Determine index of likely first measure
  first.measure.index <- min(which(col.info$last.digit.in.name == 0
                                   & col.info$rev.t.seq == 0))
  # Subset to only attributes
  col.info <- col.info[1:first.measure.index - 1, ]
  attribute.names <- col.info$col.name
  
  message(
    paste(
      "Detected",
      length(attribute.names),
      "attributes in result output. Manual check of these is advised."
    )
  )
  
  return(attribute.names)
}


# Function to return info about columns in result
ResultColumnSummary <- function(result) {
  # data table of column names
  col.info <- data.table(col.name = names(result))
  # class of each column
  col.info[, col.class := unlist(lapply(result, class)), ]
  # does a col.name end with a 0?
  col.info[, ends.with.num := grepl("\\d$", col.name), ]
  # could it be a measure based on class and name?
  col.info[, could.be.measure := ends.with.num &
             (col.class == "integer" | col.class == "numeric"),]
  # ending number in col name
  col.info[, last.num.in.name := str_extract(col.name, "\\d+$"), ]
  # ending digit in col name
  col.info[, last.digit.in.name := str_extract(col.name, "\\d$"), ]
  # name without last number
  col.info[, name.without.last.num :=
             stri_replace_last(col.name, replacement = "", fixed = last.num.in.name), ]
  
  return(col.info)
}


# Function to detect measures
GetMeasureNames <- function(result, timesteps = NULL) {
  # Estimate timesteps if not supplied
  if (is.null(timesteps)) {
    timesteps <- GetTimestepsNumber(result)
  }
  # Get column information
  col.info <- ResultColumnSummary(result)
  # Build timesteps sequence
  rev.t.seq <-
    rep(seq(timesteps - 1, 0, -1), floor(nrow(col.info) / timesteps))
  rev.t.seq <-
    c(rev.t.seq, rep(NA, nrow(col.info) - length(rev.t.seq)))
  col.info[, rev.t.seq := rev(rev.t.seq), ]
  # Determine index of likely first measure
  first.measure.index <-
    min(which(col.info$last.digit.in.name == 0 &
                col.info$rev.t.seq == 0))
  # Subset to measures
  col.info <- col.info[first.measure.index:nrow(col.info), ]
  # Remove timestep from measure names and get unique instances
  measures.names <-
    stri_replace_last(
      col.info$col.name,
      replacement = "",
      fixed = as.character(col.info$rev.t.seq)
    )
  measures.names <- unique(measures.names)
  
  message(
    paste(
      "Detected",
      length(measures.names),
      "measures in result output. Manual check of these is advised."
    )
  )
  
  return(measures.names)
}


# Function to detect number of timesteps
GetTimestepsNumber <- function(result) {
  # Get column information
  col.info <- ResultColumnSummary(result)
  # Aggregate count by name without number for everything that could be a measure
  col.summary <-
    col.info[(could.be.measure), .N, name.without.last.num]
  # Function to identify modal
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  # Most common number of measures
  modal.t.num <- Mode(col.summary$N)
  # Check this agrees with last measure name (should be last timestep)
  if (str_detect(col.info$col.name[nrow(col.info)], as.character(modal.t.num - 1))) {
    message(paste(
      "Detected",
      modal.t.num,
      "timesteps (zero indexed) in result output."
    ))
    return(modal.t.num)
  } else {
    stop("Unable to detect timesteps in result output. Please specify manually instead.")
  }
}

# Function to convert a given measure into a long format
MeltSingleMeasure <-
  function(measure, result, attributes, timesteps) {
    result <- as.data.table(result)
    # Subset to specified attributes and measure
    cols.required <-
      c(attributes, paste0(measure, 0:(timesteps - 1)))
    x <- result[, cols.required, with = FALSE]
    # Coerce all measure columns to numeric data type
    num.cols <- names(x)[grepl(measure, names(x))]
    x <- x[, (num.cols) := lapply(.SD, as.numeric), .SDcols = num.cols]
    # Melt to long format
    x <-
      melt(
        x,
        id.vars = attributes,
        variable.name = "Measure",
        value.name = "Value"
      )
    # Get rid of NAs
    x <- na.omit(x, c("Value"))
    # Remove measure names to get timesteps
    x$Timestep <- as.integer(str_replace(x$Measure, measure, ""))
    # Remove measure name
    x$Measure <- NULL
    
    return(x)
  }

# Function to convert a set of attributes and measures into long format
MakeLong <-
  function(result,
           scenarios = NULL,
           measures = NULL,
           attributes = NULL,
           timesteps = NULL,
           sep = "auto") {
    # If result is a file path (character), load it
    if (is.character(result)) {
      result <- fread(result, fill = TRUE, sep = sep)
    }
    # Autodetect measures if not supplied
    if (is.null(measures)) {
      measures <- GetMeasureNames(result)
    }
    # Autodetect attributes if not supplied
    if (is.null(attributes)) {
      attributes <- GetAttributeNames(result)
    }
    # Autodetect timesteps if not supplied
    if (is.null(timesteps)) {
      timesteps <- GetTimestepsNumber(result)
    }
    message(paste("Converting to long format."))
    # Get a list of melted measures
    x <-
      pblapply(
        measures,
        MeltSingleMeasure,
        result = result,
        attributes = attributes,
        timesteps = timesteps
      )
    # Bind list to single data.table using measure as id
    names(x) <- measures
    x <- rbindlist(x, idcol = "Measure")
    # Reorder columns
    cols.required <- c(attributes, "Measure", "Timestep", "Value")
    x <- x[, cols.required, with = FALSE]
    x$Scenario <- scenarios
    return(x)
  }

# Function to convert a set of files all at once
MultipleMakeLong <-
  function(files,
           measures = NULL,
           attributes = NULL,
           timesteps = NULL) {
    # Get list of long format for each result
    all.results <-
      lapply(
        files,
        MakeLong,
        measures = measures,
        attributes = attributes,
        timesteps = timesteps
      )
    # If scenarios, not provided, use filename as id
    if (is.null(scenarios)) {
      names(all.results) <- files
    } else {
      names(all.results) <- scenarios
    }
    # Bind all results to single data.table
    all.results <- rbindlist(all.results, idcol = "Scenario")
    
    return(all.results)
  }

# Function to convert long results to semi-long (semi-wide) format
MakeSemiLong <- function(result,
                         measure = "Measure",
                         value = "Value"){
  dcast(result, paste("...~", measure), value.var = value)
}


### Examples ###

# Convert a single result (manually specify attributes/measures to reduce memory required).
long.result <- MakeLong(
  "Assets.csv",
  scenarios = c("No Proactive Investment"),
  measures = c("Age", "Quantity Resource","Intervention Costs", "Future PoF", "Financial Consequence Cost","Safety Consequence Cost", 
               "Environmental Consequence Cost", "Network Consequence Cost","Monetised Risk Cost", 
               "Probability of Failure Mode One", "Probability of Failure Mode Two","Probability of Failure Mode Three",
               "Number of Customer Interruptions", "CML","Health Index", "Criticality Index", "Stepwise NPV"),
  attributes = c("Asset_Number"),
  sep = ","
)
#long.result$scenarios <-  c("No Proactive Investment") # just added


write.csv(long.result, "../Model/Output Data/long_results.csv", na = "")
asset.attributes <- fread("Assets.csv")
asset.attributes <- asset.attributes[, c(1:20)]
write.csv(asset.attributes, "../Model/Output Data/attributes.csv", na = "")