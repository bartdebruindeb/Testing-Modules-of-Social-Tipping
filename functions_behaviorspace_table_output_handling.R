#Made by Maarten Jensen (Umea University) & Kurt Kreulen (TU Delft) for ASSOCC

# READ DATA ---------------------------------------------------------------
loadData <- function(p_files_path, p_files_names) {
  
  #read in datafiles using filesNames and filesPath variables
  for (i in 1:length(p_files_names)) {
    print(paste("read csv from:", p_files_path, p_files_names[i], sep=""))
    #bind data from dataframe into new dataframe
    if (exists('t_df') && is.data.frame(get('t_df'))) {
      temp_df <- read.csv(paste(p_files_path, p_files_names[i], sep=""), skip = 6, sep = ",",head=TRUE,stringsAsFactors = TRUE)
      temp_df$X.run.number. <- temp_df$X.run.number + max_run_number
      t_df <- rbind(t_df, temp_df)
    }  else {
      t_df <- read.csv(paste(p_files_path, p_files_names[i], sep=""), skip = 6, sep = ",",head=TRUE,stringsAsFactors = TRUE)
    }
    max_run_number <- max(t_df$X.run.number.)
  }
  return(t_df)
}

unpacklists <- function(t_df){
}

# REMOVE IRRELEVANT VARIABLES ---------------------------------------------------------------

#Loop through dataframe and identify variables that do NOT vary (i.e. that are FIXED)
#Unfixed variables are either independent or dependent and therefore relevant to include in the analysis
removeVariables <- function(p_clean_df) {

  relevant_var_list <- list()
  index <- 1
  for (i in colnames(p_clean_df)){
    if (class(p_clean_df[[i]]) == "numeric" | class(p_clean_df[[i]]) == "integer") {
      if (min(p_clean_df[[i]]) != max(p_clean_df[[i]])) {
        relevant_var_list[[index]] <- i
        index <- index + 1
      }
    } else {
      if (nlevels(p_clean_df[[i]]) != 1) {
        relevant_var_list[[index]] <- i
        index <- index + 1
      }
    }
  }
  #Fixed variables are irrelevant and may therefore be dropped from the dataframe (i.e. select to keep only relevant variables)
  p_clean_df <- dplyr::select(p_clean_df, unlist(relevant_var_list))
  #remove redundant variables in order to free up working space
  rm(list = "relevant_var_list")
  
  return(p_clean_df);
}

# RENAME VARIABLES ---------------------------------------------------------------
printColumnNames <- function(p_clean_df) {
  
  variable_names <- names(p_clean_df)
  index <- 1
  for (i in variable_names) {
    # print(i)
    print(paste("Column", index, "is called:", i))
    index <- index + 1
  }
}

changeColumnNames <- function(p_clean_df, p_new_variable_names) {
  
  variable_names <- names(p_clean_df)
  #change variable names
  for (i in 1:length(variable_names)){
    colnames(p_clean_df)[i] = p_new_variable_names[[i]]
  }
  
  #remove redundant variables to free up working memory
  rm(list = "p_new_variable_names")
  
  return(p_clean_df)
}



# Compute modularity --------------------------------------------------------------
ComputeModularity <- function(p_clean_df, column_number, responses) {
 
  for(i in 1:length(p_clean_df$column_number)) {
    aj_matrix <- matrix(scan(text = toString(p_clean_df$adjacency_matrix[i])),ncol = responses,byrow = TRUE)
    
    aj_graph <- graph_from_adjacency_matrix(aj_matrix, mode = "max", TRUE)
    
    md <- modularity(aj_graph,  membership(cluster_leiden(aj_graph, weights = TRUE)))
    if (exists('md_df') && is.data.frame(get('md_df'))) {
      md_df <- rbind(md_df, md)
    }
    else {
      md_df <- data.matrix(md)
    }
  }
    p_clean_df <- cbind(p_clean_df, md_df)
return (p_clean_df)
}
  