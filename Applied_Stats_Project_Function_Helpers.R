library(tidyverse)
library(ggcorrplot)
library(naniar)
library(gridExtra)
library(grid)
library(stringr)
library(ggplot2)
library(ggpmisc)

########################################### PLOTTING HELPERS #######################################################

get_plot_labels <- function(plot_kind, plot_type_info, extra_info=NULL){
  
  if(plot_kind == "residual" && plot_type_info == "externally_studentized"){
    plot_title <- "Externally Studentized Residuals vs Fitted Values"
    plot_xlabel <- "Fitted Values"
    plot_ylabel <- "Externally Studentized Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  } 
  else if(plot_kind == "residual" && plot_type_info == "internally_studentized"){
    plot_title <- "Internally Studentized Residuals vs Fitted Values"
    plot_xlabel <- "Fitted Values"
    plot_ylabel <- "Internally Studentized Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual" && plot_type_info == "regular") {
    plot_title <- "Residuals vs Fitted Values"
    plot_xlabel <- "Fitted Values"
    plot_ylabel <- "Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel) 
  }
  else if(plot_kind == "residual" && plot_type_info == "deleted"){
    plot_title <- "Deleted (Prediction) Residuals vs Fitted Values"
    plot_xlabel <- "Fitted Values"
    plot_ylabel <- "Deleted (aka Prediction) Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_histogram" && plot_type_info == "externally_studentized"){
    plot_title <- "Distribution of Externally Studentized Residuals"
    plot_xlabel <- "Externally Studentized Residual Values"
    plot_ylabel <- "Count of Externally Studentized Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_histogram" && plot_type_info == "internally_studentized"){
    plot_title <- "Distribution of Internally Studentized Residuals"
    plot_xlabel <- "Internally Studentized Residual Values"
    plot_ylabel <- "Count of Internally Studentized Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_histogram" && plot_type_info == "regular"){
    plot_title <- "Distribution of Residuals"
    plot_xlabel <- "Residual Values"
    plot_ylabel <- "Count of Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_histogram" && plot_type_info == "deleted"){
    plot_title <- "Distribution of Deleted Residuals"
    plot_xlabel <- "Deleted Residual Values"
    plot_ylabel <- "Count of Deleted Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_qq" && plot_type_info == "externally_studentized"){
    plot_title <- "QQ Plot of Externally Studentized Residuals"
    plot_xlabel <- "Theoretical Quantiles"
    plot_ylabel <- "Externally Studenzied Residuals Quantiles"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_qq" && plot_type_info == "internally_studentized"){
    plot_title <- "QQ Plot of Internally Studentized Residuals"
    plot_xlabel <- "Theoretical Quantiles"
    plot_ylabel <- "Internally Studenzied Residuals Quantiles"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_qq" && plot_type_info == "deleted"){
    plot_title <- "QQ Plot of Deleted (Prediction) Residuals"
    plot_xlabel <- "Theoretical Quantiles"
    plot_ylabel <- "Deleted (Prediction) Residuals Quantiles"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_qq" && plot_type_info == "regular"){
    plot_title <- "QQ Plot of Residuals"
    plot_xlabel <- "Theoretical Quantiles"
    plot_ylabel <- "Residuals Quantiles"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_vs_leverage" && plot_type_info == "externally_studentized"){
    plot_title <- "Externally Studentized Residuals vs Leverage"
    plot_xlabel <- "Leverage"
    plot_ylabel <- "Externally Studentized Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_vs_leverage" && plot_type_info == "internally_studentized"){
    plot_title <- "Internally Studentized Residuals vs Leverage"
    plot_xlabel <- "Leverage"
    plot_ylabel <- "Internally Studentized Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_vs_leverage" && plot_type_info == "deleted"){
    plot_title <- "Deleted Residuals vs Leverage"
    plot_xlabel <- "Leverage"
    plot_ylabel <- "Deleted Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_vs_leverage" && plot_type_info == "regular"){
    plot_title <- "Residuals vs Leverage"
    plot_xlabel <- "Leverage"
    plot_ylabel <- "Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "case_stat_vs_obs"){
    full_name <- case_stat_name_map(case_stat=plot_type_info, get_full_name=TRUE)
    plot_title <- paste0(full_name_to_title(full_name), " vs Observation Number")
    plot_xlabel <- "Observation Number"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=full_name)
  }
  else if(plot_kind =="partial_residual"){
    variable_name <- full_name_to_title(plot_type_info)
    plot_title <- paste0("Partial Residual Plot for Explanatory Variable ", variable_name)
    plot_xlabel <- variable_name
    plot_ylabel <- paste0("Partial Residual of ", variable_name)
    
    if(extra_info){
      plot_title <- paste0("Augmented ", plot_title)
      plot_ylabel <- paste0("Augmented ", plot_ylabel)
    }
    
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "correlation"){
    return_list <- paste0("Heatmap of ", str_to_title(plot_type_info), " Correlation Coefficients")
  }
  return(return_list)
  
}

##################################################################################################



########################################### GENERAL UTILITY #######################################################


# Function to filter a dataframe by either:
# 1) col_names --> a list of column names to include
# 2) remove_columns --> a list of column names to remove
#
filter_columns <- function(df, col_names=NULL, remove_columns=NULL){
  
  # If we are filtering by a list of column names to include
  if(!is.null(col_names)){
    df <- df[, names(df) %in% col_names]
  } else if(!is.null(remove_columns)){
    df <- df[, !(names(df) %in% remove_columns)]
  }
  return(df)
}


# Removes all columns from a dataframe that DO NOT contain missing values...
# could modify to allow a threshold of missings
get_columns_with_missings <- function(df){
  
  # get the names of the columns with at least one missing value
  columns_with_missings <- colnames(df)[colSums(is.na(df))>0]  
  
  # Filter the dataframe to only contain the columns with at least one missing value
  df <- df[,columns_with_missings]
  
  return(df)
  
}


# Remove rows that do not contain any missings and are duplicates
remove_duplicates_without_missings <- function(df){
  
  # If a row is a complete case (no missings in that row) AND its a duplicate.
  complete_and_duplicate_filter <- duplicated(df) & complete.cases(df)
  
  df <- df[!complete_and_duplicate_filter,]
  
  return(df)
}


##################################################################################################