
readData <- function(terrorism_location, military_budget_location, density_location, emigrants_location, population_location) {
  terrorism <- read.csv(terrorism_location)
  print(paste("Read --Terrorist Attacks-- dataset with size: ", toString(dim(terrorism))))

  military_budget <- read.csv(military_budget_location)
  names(military_budget) <- substring(names(military_budget), 2,5)
  colnames(military_budget)[1] <- "Country"
  print(paste("Read --Military Budget-- dataset with size: ", toString(dim(military_budget))))
  
  density <- read.csv(density_location)
  names(density) <- substring(names(density), 2,5)
  colnames(density)[1] <- "Country"
  print(paste("Read --Population Density-- dataset with size: ", toString(dim(density))))
  
  emigrants <- read.csv(emigrants_location)
  names(emigrants) <- substring(names(emigrants), 2,5)
  colnames(emigrants)[1] <- "Country"
  print(paste("Read --Emigrants-- dataset with size: ", toString(dim(emigrants))))
  
  population <- read_csv(population_location)
  population = subset(population, population$Variant == 'Medium' & population$Location == "World", select = c("Time", "PopTotal"))
  print(paste("Read --Population-- dataset with size: ", toString(dim(population))))
  
  return(list(terrorism, military_budget, density, emigrants, population))
  
}

getDataForRegression <- function(terrorism, military_budget, density, emigrants, population) {
  df = subset(getMergedDataset(terrorism, military_budget, density, emigrants, 1))
  groupColumns = c("year")
  dataColumns = c("attacks", "military_budget","emigrants")
  res = ddply(df, groupColumns, function(x) colSums(x[dataColumns]))
  res = inner_join(res, population, by= c("year" = "Time"))
  return(res)
}

getMergedDataset <- function(terrorism, military_budget, density, emigrants, worldwide) {
  #Only select data from North America and Europe
  if (worldwide == 0) {
    df = subset(terrorism, terrorism$region == 1 | terrorism$region == 8 | terrorism$region == 9, select = c(iyear, country_txt))
  }
  else if (worldwide == 1) {
    df = subset(terrorism, select = c(iyear, country_txt))
  }
  
  colnames(df) <- c("year", "country")
  
  final_data <- df %>%
    group_by(year, country) %>%
    summarise(attacks = n())
  
  
  test = inner_join(final_data, military_budget, by = c("country" = "Country"))
  
  for (row in 1:nrow(test)) {
    year <- test[row, "year"]
    test[row, "1960"] = test[row, toString(test[row, "year"]) ]
  }
  colnames(test)[4] <- "military_budget"
  
  final_data = test[, c(1, 2, 3, 4)]
  final_data = na.omit(final_data)
  
  
  test = inner_join(final_data, density, by = c("country" = "Country"))
  
  for (row in 1:nrow(test)) {
    year <- test[row, "year"]
    test[row, "1961"] = test[row, toString(test[row, "year"]) ]
  }
  colnames(test)[5] <- "population_density"
  
  final_data = test[, c(1, 2, 3, 4, 5)]
  final_data = na.omit(final_data)
  
  
  test = inner_join(final_data, emigrants, by = c("country" = "Country"))
  
  for (row in 1:nrow(test)) {
    year <- test[row, "year"]
    if(toString(test[row, "year"]) %in% colnames(test)){
      test[row, "1990"] = test[row, toString(test[row, "year"]) ]
    }
    else {
      test[row, "1990"] = NA
    }
    
  }
  colnames(test)[6] <- "emigrants"
  final_data = test[, c(1, 2, 3, 4, 5, 6)]
  final_data = na.omit(final_data)
  
  
  final_data$attackslevel <- findInterval(final_data$attacks, c(0, 4), rightmost.closed = TRUE)
  final_data$attackslevel <- as.factor(final_data$attackslevel)
  
  return(final_data)
}

normalize <- function(row) { (row - mean(row)) / sd(row) }

