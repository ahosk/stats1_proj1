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


add_obs_number_column <- function(df){
  
  # If the dataframe already has an "Id" column for observation identifiers,
  # then use that, otherwise create 1 by number the rows of the dataframe.
  if("Id" %in% names(df)){
    df[,"obs_number"] <- df[,"Id"]
  }else{
    df[,"obs_number"] <- seq(1, as.numeric(nrow(df))) 
  }
  return(df)
}


filter_by_observation_numbers <- function(df, observation_numbers){
  
  if(is.null(observation_numbers)){
    return(df)
  }
  
  # Remove the desired observation numbers
  df <- df[!(df[,"obs_number"] %in% observation_numbers),]
  
  return(df)
}


get_plotting_data <- function(df, x_var, y_var, shade_var, shape_var, size_var, keep_values, 
                              remove_less_than, remove_greater_than, filter_column){
  
  df[,"Response"] <- df[,y_var]
  df[,"Explanatory"] <- df[,x_var]
  
  df <- filter_scatter_data(df=df, 
                            filter_column=filter_column, 
                            keep_values=keep_values, 
                            remove_less_than=remove_less_than, 
                            remove_greater_than=remove_greater_than)
  
  # If we want to shade the points based on a variable
  if(!is.null(shade_var)){
    df[,"shading_variable"] <- as.factor(df[,shade_var])
    shading_variable <- df[,"shading_variable"]
  }
  else{
    shading_variable <- NULL
  }
  
  # If we want to change the points shapes based on a variable
  if(!is.null(shape_var)){
    df[,"shape_variable"] <- as.factor(df[,shape_var])
    shape_variable <- df[,"shape_variable"]
  }
  else{
    shape_variable <- NULL
  }
  
  # If we want to change the points sizes based on a variable
  if(!is.null(size_var)){
    df[,"size_variable"] <- df[,size_var]
    size_variable <- df[,"size_variable"]
  }
  else{
    size_variable <- NULL
  }
  
  
  return_list <- list(data_frame=df, 
                      shading_var=shading_variable, 
                      shape_var=shape_variable, 
                      size_var=size_variable)
  
  return(return_list)
  
}

add_obs_numbers <- function(p, df, obs_txt_size, obs_txt_color,
                            obs_txt_vjust, obs_txt_hjust, identify_obs, 
                            x_var=NULL, y_var=NULL, called_from=NULL) {
  
  if(typeof(identify_obs) == "logical" && !identify_obs){
    return(p)
  }
  
  # If we didn't call this function from the plot_partial_residuals function 
  if(!is.null(called_from)){
    
    # This section of code only runs when called from plot_scatter, so these names aren't correct,
    # it is a little messy, but its the result of reusing this function for a secondary purpose (it 
    # was originally designed to add obs_numbers to the partial residual plots, but can be repurposed to
    # add obs_numbers to other plots too, just need the name to match).
    df[,"partial_resid"] <- df[,y_var]
    df[,"analysis_variable"] <- df[,x_var]
  }
  
  # If we don't want to identify all observatins, filter based on the vector passed
  # to identify_obs
  if(typeof(identify_obs) != "logical"){
    df <- df[df[,"obs_number"] %in% identify_obs,]
    
  }
  
  p <- p + geom_text(data=df, mapping=aes(x=analysis_variable, y=partial_resid, label=obs_number), 
                     hjust=obs_txt_hjust, vjust=obs_txt_vjust, color=obs_txt_color, size=obs_txt_size)
  
  return(p)
}


add_legend_data <- function(p, shade_var, shape_var){
  
  if(is.null(shade_var) == FALSE){
    p <- p + labs(color=shade_var)
  }
  else{
    p <- p + labs(color=NULL)
  }
  
  if(is.null(shade_var) == FALSE){
    p <- p + labs(shape=shape_var)
  }
  else{
    p <- p + labs(shape=NULL)
  }
  
  return(p)
  
}

add_regression_and_title <- function(df, show_regression, p, conf_level, pred_band, 
                                     conf_band, reg_linecolor, conf_linecolor, pred_linecolor,
                                     conf_linetype, pred_linetype, x_var, y_var, round_digits, 
                                     reg_table, table_loc){
  
  if(!show_regression){
    p <- add_scatterplot_title(p=p, show_regression=show_regression, x_var=x_var, y_var=y_var)
    return(p)
  }
  
  fit <- lm(Response~Explanatory, data=df)
  
  # Data for prediction band
  pred_data <- as.data.frame(predict(fit, interval="prediction", level=conf_level))
  pred_data <- rename(pred_data, pred_lower=lwr, pred_upper=upr)
  
  # Data for confidence band
  conf_data <- as.data.frame(predict(fit, interval="confidence", level=conf_level))
  conf_data <- rename(conf_data, conf_lower=lwr, conf_upper=upr)
  
  # combine original data, confidence band data, and prediction band data.
  df <- cbind(df, conf_data, pred_data[,c("pred_lower", "pred_upper")])
  
  
  # Add the regression line
  p <- p + geom_smooth(method="lm", color=reg_linecolor, level=conf_level)
  
  # Add the confidence band
  if(conf_band){
    p <- p + geom_line(data=df, mapping=aes(x=Explanatory, y=conf_lower), 
                       color=conf_linecolor, linetype=conf_linetype) +
      geom_line(data=df, mapping=aes(x=Explanatory, y=conf_upper), 
                color=conf_linecolor, linetype=conf_linetype)
  }
  
  # Add the prediction band
  if(pred_band){
    p <- p + geom_line(data=df, mapping=aes(x=Explanatory, y=pred_lower), color=pred_linecolor, 
                       linetype=pred_linetype) + 
      geom_line(data=df, mapping=aes(x=Explanatory, y=pred_upper), color=pred_linecolor, 
                linetype=pred_linetype)
    
  }
  
  p <- add_scatterplot_title(p=p, show_regression=show_regression, x_var=x_var, y_var=y_var, 
                             fit=fit, round_digits=round_digits)
  
  p <- add_regression_table(reg_table=reg_table, table_loc=table_loc, p=p, x_var=x_var, 
                            fit=fit, round_digits=round_digits)
  
  return(p)
  
}

filter_scatter_data <- function(df, filter_column, keep_values, remove_less_than, 
                                remove_greater_than) {
  
  # If there is nothing we want to filter
  if(is.null(filter_column)){
    return(df)
  }
  
  # Categorical filtering based on a vector of categories to keep
  if(!is.null(keep_values)){
    df <- df[df[,filter_column] %in% keep_values,]
    return(df)
  }
  
  # Numeric filtering, remove everything less than
  if(!is.null(remove_less_than)){
    df <- df[df[,filter_column] >= remove_less_than,]
  }
  
  # Numeric filtering, remove everything greater than
  if(!is.null(remove_greater_than)){
    df <- df[df[,filter_column] <= remove_greater_than,]
  }
  
  return(df)
  
}

add_scatterplot_title <- function(p, show_regression, x_var, y_var, fit=NULL, round_digits=4){
  
  if(!show_regression){
    
    plot_title <- paste0("Scatter Plot of ", y_var, " vs ", x_var)
    p <- p + ggtitle(plot_title) + 
      xlab(x_var) + 
      ylab(y_var)
    
    return(p)
  }
  
  # Save summary statistics to add to title
  rmse <- round(summary(fit)$sigma, round_digits)
  r_square <- round(summary(fit)$r.squared, round_digits)
  adj_r_square <- round(summary(fit)$adj.r.squared, round_digits)
  
  # Create plot title.
  plot_title <- paste0("Regression of ", y_var, " on ", x_var, "\n", 
                       "RMSE=", rmse, "  R Square=", r_square, "  Adjusted R Square=", adj_r_square)
  
  p <- p + ggtitle(plot_title) + 
    xlab(x_var) + 
    ylab(y_var)
  
  return(p)
  
}

add_regression_table <- function(reg_table, table_loc, p, x_var, fit=NULL, round_digits=4){
  
  if(!reg_table){
    return(p)
  }
  
  table_coordinates <- get_table_location(table_loc=table_loc)
  
  # Information for creating the table
  annot_data <- data.frame(summary(fit)$coefficients)
  
  # Round values so table isn't way too big
  annot_data <- round(annot_data, digits=round_digits)
  
  # Adjust the tables row names based on the dataset.
  row.names(annot_data)[row.names(annot_data) == "Explanatory"] <- paste0(x_var, "(Slope)")
  
  # Adjust the tables column names
  names(annot_data) <- c("Estimate", "Std_Error", "T_value", "P_value")
  
  p <- p + 
    annotate(geom="table", 
             x=table_coordinates[["x_coordinate"]], 
             y=table_coordinates[["y_coordinate"]], 
             label=list(annot_data), 
             table.rownames=TRUE)
  
  return(p)
  
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