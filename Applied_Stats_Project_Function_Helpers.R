library(tidyverse)
library(ggcorrplot)
library(naniar)
library(gridExtra)
library(grid)
library(stringr)
library(ggplot2)
library(ggpmisc)
library(scales)
library(olsrr)
library(caret)
library(HH)
library(EnvStats)

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
  else if(plot_kind == "means_plot"){
    continuous_variable_name <- full_name_to_title(plot_type_info)
    categorical_variable_name <- full_name_to_title(extra_info)
    plot_title <- paste0("Mean ", continuous_variable_name, " for each level of ", categorical_variable_name)
    plot_xlabel <- categorical_variable_name
    plot_ylabel <- paste0("Mean ", continuous_variable_name)
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "factor_level_counts"){
    variable_name <- full_name_to_title(plot_type_info)
    plot_title <- paste0("Frequency of Levels in Variable ", variable_name)
    plot_xlabel <- paste0("Levels in ", variable_name)
    plot_ylabel <- paste0("Count of level occurences")
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
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


# Convert a full name to a title by removing underscores and adding capitolization
full_name_to_title <- function(case_stat){
  full_name_split <- str_replace_all(case_stat, pattern="_", replacement=" ")
  title <- str_to_title(full_name_split)
  return(title)
}


add_obs_number_column <- function(df){
  
  
  if("obs_number" %in% names(df)){
    return(df)
  }
  
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
                            x_var=NULL, y_var=NULL, called_from=NULL, show_text=TRUE, show_points=FALSE) {
  
  # If we passed in FALSE, nothing to do here, head out of the function.
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
  
  if(show_text){
    p <- p + geom_text(data=df, mapping=aes(x=analysis_variable, y=partial_resid, label=obs_number), 
                       hjust=obs_txt_hjust, vjust=obs_txt_vjust, color=obs_txt_color, size=obs_txt_size)  
  }
   
  if(show_points){
    p <- p + geom_point(data=df, mapping=aes(x=analysis_variable, y=partial_resid), color=obs_txt_color)
  }
    
  
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

get_table_location <- function(table_loc){
  
  if(table_loc == "upper_right"){
    x_coord <- Inf
    y_coord <- Inf
  }
  else if(table_loc == "lower_right"){
    x_coord <- Inf
    y_coord <- -Inf
    
  }
  else if(table_loc == "upper_left"){
    x_coord <- -Inf
    y_coord <- Inf
    
  }
  else if(table_loc == "lower_left"){
    x_coord <- -Inf
    y_coord <- -Inf
  }
  
  coords <- list(x_coordinate=x_coord, 
                 y_coordinate=y_coord)
  
  return(coords)
  
}

calculate_summary_stats <- function(df, column_name){
  
  summary_vector <- c(min(df[,column_name]), 
                      quantile(df[,column_name], probs=c(0.25)), 
                      mean(df[,column_name]), 
                      median(df[,column_name]), 
                      quantile(df[,column_name], probs=c(0.75)), 
                      max=max(df[,column_name]))
  
  
  summary_stat_names <- c("min", "25%", "mean", "median", "75%", "max")
  
  summary_df <- data.frame(summary_stats=summary_vector)
  rownames(summary_df) <- summary_stat_names
  
  return(summary_df)
  
}

add_table_to_plot <- function(p, df, x_var, table_loc){
  
  annot_table <- calculate_summary_stats(df=df, column_name=x_var)
  
  table_coordinates <- get_table_location(table_loc=table_loc)
  
  p <- p + 
    annotate(geom="table", 
             x=table_coordinates[["x_coordinate"]], 
             y=table_coordinates[["y_coordinate"]], 
             label=list(annot_table), 
             table.rownames=TRUE)
  
}


# 1. Calculates all case statistics and stores in a dataframe
# 2. Copies the data for the residual type being plotted to a column named "Resid_Plot_Column"
# 3. Filters the dataframe based on the "remove_less"than" and "remove_greater_than" filters.
get_residual_plot_data <- function(fit, residual_type, remove_less_than, remove_greater_than, dataframe=NULL){
  
  
  # Create the case statistics dataframe
  case_df <- create_case_df(fit, dataframe=dataframe)
  
  # Add the appropriate Resid_Plot_Column for this residual_type
  case_df <- set_resid_plot_column(case_df, residual_type=residual_type)
  
  # Filter according to the remove_less_than and remove_greater_than parameters, which filter out values
  # "less than" or "greater than" the specified value.
  case_df <- filter_by_absolute_value(df=case_df, 
                                      filter_column="Resid_Plot_Column", 
                                      remove_less_than=remove_less_than, 
                                      remove_greater_than=remove_greater_than)
  
  return(case_df)
  
}


# Calculate all the case statistics to assess model fit.
create_case_df <- function(fit, dataframe=NULL){
  
  case_stats <- HH::case(fit)
  
  case_df <- as.data.frame(case_stats)
  
  # Add deleted residuals to the dataframe
  case_df['deleted_resids'] <- case_df[,"e"] / (1 - case_df[, "h"])
  
  # Add fitted values to the dataframe
  case_df[,"fitted_values"] <- fit$fitted.values
  
  # Add a column to track observation number
  case_df[,"obs_number"] <- dataframe[,"obs_number"]
  
  
  return(case_df)
  
}

# Sets the column to be plotted (residual type) based on user input.
set_resid_plot_column <- function(case_df, residual_type){
  
  if(residual_type == "externally_studentized"){
    case_df[,"Resid_Plot_Column"] <- case_df[,"stu.res"]
    
  }
  else if(residual_type == "internally_studentized"){
    case_df[,"Resid_Plot_Column"] <- case_df[,"sta.res"]  
  }
  else if(residual_type == "regular"){
    case_df[,"Resid_Plot_Column"] <- case_df[,"e"]
    
  }
  else if(residual_type == "deleted") {
    case_df[,"Resid_Plot_Column"] <- case_df[,"deleted_resids"]
  }
  else{
    case_df[,"Resid_Plot_Column"] <- case_df[, residual_type]
  }
  
  return(case_df)
}

# Function to filter a dataset based on a given column
# Removes values less than remove_less_than and greater than remove_greater_than
filter_by_absolute_value <- function(df, filter_column, remove_less_than=NULL, remove_greater_than=NULL){
  
  if(is.null(remove_less_than) == FALSE){
    df <- df[abs(df[,filter_column]) >= remove_less_than,]
  }
  
  if(is.null(remove_greater_than) == FALSE){
    df <- df[abs(df[,filter_column]) <= remove_greater_than,]
  }
  
  return(df)
  
}

# The deleted and regular residual plots can not have their binwidth set directly in a very intuitive manner
# Therefore it is easier to set the number of bins instead. To overlay the normal curve however, we need to know
# binwidth. This function calculates the correct bin width based on the x-axis values and the number of bins.
get_binwith <- function(binwidth, num_bins, current_plot){
  
  if(is.null(binwidth) == TRUE){
    
    build <- ggplot_build(current_plot)
    x_min <- build$layout$panel_params[[1]]$x.range[1]
    x_max <- build$layout$panel_params[[1]]$x.range[2]
    x_length <- x_max - x_min
    binwidth <- x_length / num_bins
    
  } 
  
  return(binwidth)
  
}


# Fits a linear model based on string values specifying the response and explanatory variables
# used in the plot_partial_residuals function.
build_lm_from_strings <- function(df, response_var, explanatory_vars, x=FALSE){
  
  # Set up the formula for the linear model fit
  lm_formula <- as.formula(paste0(response_var, "~",str_c(explanatory_vars, collapse=" + ")))
  
  # Fit the model
  fit <- lm(lm_formula, data=as.data.frame(df), x=x)
  
  return(fit)
  
}

compute_partial_residuals <- function(df, fit, analysis_var){
  
  df[,"residuals"] <- fit$residuals
  
  # Setting initial value of the component residuals for the analysis variables to zero.
  df[,"component_resid"] <- 0
  
  # Get the coefficient values for these analysis variable(s)
  analysis_var_coefs <- get_analysis_coefs(fit=fit, analysis_var=analysis_var)
  
  for(index in 1:length(analysis_var_coefs)){
    
    # For each analysis variable, multiple its X values by its coefficient and add it to the component residual column
    df[,"component_resid"] <- df[,"component_resid"] + (df[,analysis_var[index]] * analysis_var_coefs[index])
    
  }
  
  df[,"partial_resid"] <- df[,"residuals"] + df[,"component_resid"]
  
  df <- set_analysis_var_column(df=df, analysis_var=analysis_var)
  
  return(df)
  
}

# used in the plot_partial_residuals function
get_analysis_coefs <- function(fit, analysis_var){
  
  # Holds the coefficients for the analysis variables
  analysis_var_coefs <- c()
  
  # For each analysis variable (one variable for standard partial resids, two if augmented)
  for(analysis_index in 1:length(analysis_var)){
    
    # Grab the name of this analysis variable
    analysis_var_name <- analysis_var[analysis_index]
    
    # Loop through the coefficients
    for(coef_index in 1:length(fit$coefficients)){
      
      # Grab the name of the current coef
      coef_name <- names(fit$coefficients[coef_index])
      
      # Check if the name for this coef matches the analysis variable we are currently finding a coef for.
      if(analysis_var_name == coef_name){
        
        # Add the coef to the list
        analysis_var_coefs <- append(analysis_var_coefs, fit$coefficients[[coef_index]])
      }
    }
  }
  
  return(analysis_var_coefs)
}


add_shading_variable <- function(fit, df, shade_by_case){
  
  if(is.null(shade_by_case)){
    return(df)
  }
  
  # Get the short name for the case statistic we want to shade by
  shading_variable <- case_stat_name_map(case_stat=shade_by_case, get_short_name=TRUE)
  
  case_df <- get_residual_plot_data(fit=fit, 
                                    residual_type=shading_variable, 
                                    remove_less_than=NULL, 
                                    remove_greater_than=NULL)
  
  df[,"shade_by_case"] <- case_df[,shading_variable]
  
  return(df)
  
}


add_least_squares_line <- function(p, df, x_var, y_var, linecolor, linetype, 
                                   show_legend, add_least_squares){
  
  if(!add_least_squares){
    return(p)
  }
  
  df <- assign_x_y(df=df, x=x_var, y=y_var)
  
  fit <- lm(y_variable~x_variable, data=df)
  
  # Get the least squares intercept and slope
  ls_coefs <- fit$coeff
  intercept <- ls_coefs[[1]]
  slope <- ls_coefs[[2]]
  
  p <- p + 
    geom_abline(slope=slope, 
                intercept=intercept, 
                color=linecolor,
                linetype=linetype, 
                show.legend=show_legend)
  
  return(p)
  
}

# Used in plot_partial_residuals. Helps show the change in the effect of a predictor with the removal of a point.
add_point_removal_comparison_line <- function(p, df, x_var, y_var, linecolor, linetype, 
                                              show_legend, add_pt_removal_line, id_removal_compare_pts,
                                              obs_txt_size, obs_txt_hjust, obs_txt_vjust, obs_txt_color) {
  
  if(typeof(add_pt_removal_line) == "logical" && !add_pt_removal_line){
    return(p)
  }
  
  # Data frame that only contains the points we are removing, saving this so we can
  # color these points differently.
  removed_pts_df <- df[df[,"obs_number"] %in% add_pt_removal_line,]
  
  # Color the points we are removing differently
  p <- p + 
    geom_point(data=removed_pts_df, 
               mapping=aes(x=analysis_variable, y=partial_resid), 
               color=linecolor)
  
  # Remove the list of desired observation numbers before fitting 
  # the new least squares line
  df <- df[!(df[,"obs_number"] %in% add_pt_removal_line),]
  
  p <- add_least_squares_line(p=p, 
                              df=df, 
                              x_var=x_var, 
                              y_var=y_var, 
                              linecolor=linecolor, 
                              linetype=linetype,
                              show_legend=show_legend, 
                              add_least_squares=TRUE)
  
  # Identify the removed points
  if(id_removal_compare_pts){
    p <- p + geom_text(data=removed_pts_df, mapping=aes(x=analysis_variable, y=partial_resid, label=obs_number), 
                       hjust=obs_txt_hjust, vjust=obs_txt_vjust, color=obs_txt_color, size=obs_txt_size)    
  }
  
  
  return(p)
  
}

# Used in plot_partial_residuals
check_datatypes <- function(df, analysis_var){
  
  if(!is.numeric(df[,analysis_var])){
    print(paste0(analysis_var, " is  of non-numeric data type."))
    print("Multiple Regression Coefficients Exist For this categorical variable")
    print("Please specify which level you wish to plot the component plus residuals for")
    check_failed = TRUE
  } else{
    check_failed = FALSE
  }
  return(check_failed)
}

# Used in plot_partial_residuals
set_analysis_var_column <- function(df, analysis_var){
  df[,"analysis_variable"] <- df[, analysis_var[1]]
  return(df)
}

# Used in add_least_squares_line, which is used in plot_partial_residuals
assign_x_y <- function(df, x, y){
  
  df[, "x_variable"] <- df[,x]
  df[, "y_variable"] <- df[,y]
  return(df)
}


# Converts between an "full" and "shortened" name for the various case statistics
# used in a few places, such as plot_case_stat_vs_obs
case_stat_name_map <- function(case_stat, get_full_name=NULL, get_short_name=NULL){
  
  full_to_short <- list(cooks_d="cook", leverage="h", DFFITS="dffit", 
                        externally_studentized_residuals="stu.res", deleted_standard_deviation="si")
  
  short_to_full <- list(cook="cooks_d", h="leverage", dffit="DFFITS", stu.res="externally_studentized_residuals", 
                        si="deleted_standard_deviation")
  
  if(!is.null(get_full_name)){
    
    # If its already the full name
    if(case_stat %in% short_to_full){
      return(case_stat)
    }
    else{
      return(short_to_full[[case_stat]])
    }
    
  }
  else if(!is.null(get_short_name)){
    
    if(case_stat %in% full_to_short){
      return(case_stat)
    }
    else{
      return(full_to_short[[case_stat]])
    }
  }
}

# Each case statistics has some type of "rule of thumb" to help decide what is worth further investigation
# All plots that use case statistics allow for modification of this rule of thumb by the user.
#
# This is the function that handles determining what the rule of thumb is based on the input and the statistic
# being plotted. This function then adds the appropriate annotations to the plot to show the rule of thumb and 
# the points that are violating it.
plot_rule_of_thumb <- function(fit, case_df, case_stat, cook_rot, dffit_rotm, 
                               resid_rot, std_rotm, leverage_rotm, p, ref_linecolor, 
                               ref_linetype, annot_reflines, plot_rot_reflines, 
                               flag_extreme, max_flagged, extreme_value_shape, extreme_value_color, 
                               obs_txt_hjust, obs_txt_size, obs_txt_vjust) {
  
  if(!plot_rot_reflines){
    return(p)
  }
  
  
  if(case_stat == "h"){
    p <- plot_leverage_rot(fit=fit, 
                           p=p, 
                           case_df=case_df, 
                           leverage_rotm=leverage_rotm, 
                           ref_linecolor=ref_linecolor, 
                           ref_linetype=ref_linetype, 
                           annot_reflines=annot_reflines, 
                           flag_extreme=flag_extreme,
                           max_flagged=max_flagged, 
                           extreme_value_shape=extreme_value_shape, 
                           extreme_value_color=extreme_value_color, 
                           obs_txt_hjust=obs_txt_hjust, 
                           obs_txt_size=obs_txt_size, 
                           obs_txt_vjust=obs_txt_vjust)
    
  }
  else if(case_stat == "cook"){
    p <- plot_cook_rot(p=p, 
                       ref_linecolor=ref_linecolor, 
                       ref_linetype=ref_linetype, 
                       cook_rot=cook_rot, 
                       case_df=case_df, 
                       flag_extreme=flag_extreme,
                       max_flagged=max_flagged,                            
                       extreme_value_shape=extreme_value_shape, 
                       extreme_value_color=extreme_value_color, 
                       obs_txt_hjust=obs_txt_hjust, 
                       obs_txt_size=obs_txt_size, 
                       obs_txt_vjust=obs_txt_vjust)
    
  }
  else if(case_stat == "dffit"){
    p <- plot_dffit_rot(fit=fit, 
                        p=p, 
                        dffit_rotm=dffit_rotm, 
                        ref_linetype=ref_linetype, 
                        ref_linecolor=ref_linecolor, 
                        annot_reflines=annot_reflines, 
                        case_df=case_df, 
                        flag_extreme=flag_extreme,
                        max_flagged=max_flagged,                            
                        extreme_value_shape=extreme_value_shape, 
                        extreme_value_color=extreme_value_color, 
                        obs_txt_hjust=obs_txt_hjust, 
                        obs_txt_size=obs_txt_size, 
                        obs_txt_vjust=obs_txt_vjust)
    
  }
  else if(case_stat == "stu.res"){ # Externally Studentized Residuals
    p <- plot_ext_std_resid_rot(p=p, 
                                ref_linetype=ref_linetype, 
                                ref_linecolor=ref_linecolor, 
                                resid_rot=resid_rot, 
                                case_df=case_df,
                                flag_extreme=flag_extreme,
                                max_flagged=max_flagged,                            
                                extreme_value_shape=extreme_value_shape, 
                                extreme_value_color=extreme_value_color, 
                                obs_txt_hjust=obs_txt_hjust, 
                                obs_txt_size=obs_txt_size, 
                                obs_txt_vjust=obs_txt_vjust)
    
  }
  else if(case_stat == "si"){  # deleted standard deviation
    p <- plot_deleted_std_rot(fit=fit, 
                              p=p, 
                              ref_linetype=ref_linetype, 
                              ref_linecolor=ref_linecolor, 
                              std_rotm=std_rotm,
                              annot_reflines=annot_reflines, 
                              case_df=case_df,
                              flag_extreme=flag_extreme,
                              max_flagged=max_flagged,                            
                              extreme_value_shape=extreme_value_shape, 
                              extreme_value_color=extreme_value_color, 
                              obs_txt_hjust=obs_txt_hjust, 
                              obs_txt_size=obs_txt_size, 
                              obs_txt_vjust=obs_txt_vjust)
  }
  
  return(p)
  
}

plot_cook_rot <- function(p, cook_rot, ref_linecolor, ref_linetype, case_df,
                          flag_extreme, max_flagged, extreme_value_shape, 
                          extreme_value_color,obs_txt_size, obs_txt_vjust, obs_txt_hjust){
  
  p <- p + 
    geom_hline(yintercept=cook_rot, 
               color=ref_linecolor, 
               linetype=ref_linetype)
  
  if(flag_extreme){
    
    threshold_filter <- (case_df[,"cook"] >= cook_rot)
    case_df <- case_df[threshold_filter,]
    
    # Grab the top  max_flagged values that exceeded the threshold, or all of them if less than max_flagged.
    num_extreme_observations <- as.numeric(nrow(case_df))
    num_obs_to_flag <- as.numeric(min(num_extreme_observations, max_flagged))
    case_df <- case_df[order(abs(case_df[,"dffit"]), decreasing=TRUE),][1:num_obs_to_flag,]
    
    p <- p + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color) + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color, 
                 shape=extreme_value_shape) + 
      geom_text(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column, label=obs_number), hjust=obs_txt_hjust,
                vjust=obs_txt_vjust, color=extreme_value_color, size=obs_txt_size)
    
  }
  
  return(p)
  
}

plot_dffit_rot <- function(fit, p, dffit_rotm, case_df, ref_linetype, 
                           ref_linecolor, annot_reflines,flag_extreme, 
                           max_flagged, extreme_value_shape, extreme_value_color,
                           obs_txt_size, obs_txt_vjust, obs_txt_hjust) {
  
  num_parameters <- length(fit$coefficients)
  num_observations <- as.numeric(nrow(case_df))
  
  upper_threshold <- dffit_rotm * sqrt((num_parameters - 1)/num_observations)
  lower_threshold <- upper_threshold * -1
  
  p <- p + 
    geom_hline(yintercept=upper_threshold, 
               color=ref_linecolor, 
               linetype=ref_linetype) + 
    geom_hline(yintercept=lower_threshold, 
               color=ref_linecolor,
               linetype=ref_linetype)
  
  
  if(flag_extreme){
    
    above_filter <- (case_df[,"dffit"] >= upper_threshold)
    below_filter <- (case_df[,"dffit"] <= lower_threshold)
    
    # Combine the above filters, and subset to only include values that exceeded the threshold.
    exceeded_threshold <- above_filter | below_filter
    case_df <- case_df[exceeded_threshold,]
    
    # Grab the top  max_flagged values that exceeded the threshold, or all of them if less than max_flagged.
    num_extreme_observations <- as.numeric(nrow(case_df))
    num_obs_to_flag <- as.numeric(min(num_extreme_observations, max_flagged))
    case_df <- case_df[order(abs(case_df[,"dffit"]), decreasing=TRUE),][1:num_obs_to_flag,]
    
    p <- p + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color) + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color, 
                 shape=extreme_value_shape) + 
      geom_text(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column, label=obs_number), hjust=obs_txt_hjust,
                vjust=obs_txt_vjust, color=extreme_value_color, size=obs_txt_size)
    
  }
  
  
  if(!annot_reflines){
    return(p)
  }
  
  return(p)
  
}

plot_leverage_rot <- function(fit, p, case_df, leverage_rotm, ref_linecolor, 
                             ref_linetype, annot_reflines, flag_extreme, 
                             max_flagged, extreme_value_shape,extreme_value_color,
                             obs_txt_size, obs_txt_vjust, obs_txt_hjust){
  
  threshold <- get_leverage_threshold(fit=fit, 
                                      leverage_line_multiplier=leverage_rotm, 
                                      case_df=case_df)
  
  average_leverage <- get_average_leverage(fit=fit, case_df=case_df)
  
  p <- p + 
    geom_hline(yintercept=threshold, 
               color=ref_linecolor, 
               linetype=ref_linetype) +
    geom_hline(yintercept=average_leverage, 
               color=ref_linecolor, 
               linetype=ref_linetype)
  
  if(flag_extreme){
    
    case_df <- case_df[case_df[,'h'] >= threshold,]
    
    # Grab the top  max_flagged values that exceeded the threshold, or all of them if less than max_flagged.
    num_extreme_observations <- as.numeric(nrow(case_df))
    num_obs_to_flag <- as.numeric(min(num_extreme_observations, max_flagged))
    case_df <- case_df[order(abs(case_df[,"h"]), decreasing=TRUE),][1:num_obs_to_flag,]
    
    p <- p + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color) + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color, 
                 shape=extreme_value_shape) + 
      geom_text(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column, label=obs_number), hjust=obs_txt_hjust,
                vjust=obs_txt_vjust, color=extreme_value_color, size=obs_txt_size)
    
  }
  
  if(!annot_reflines){
    return(p)
  }
  
  return(p)
}

get_leverage_threshold <- function(fit, leverage_line_multiplier, case_df){
  
  # Calculate average leverage
  avg_leverage <- get_average_leverage(fit=fit, case_df=case_df)
  
  # Calculate the leverage level at which we want to inspect observations closer
  leverage_threshold <- leverage_line_multiplier * avg_leverage
  
  return(leverage_threshold)
  
}

get_average_leverage <- function(fit, case_df){
  
  num_parameters <- length(fit$coefficients)
  num_observations <- as.numeric(nrow(case_df))
  
  # Calculate the average leverage
  avg_leverage = num_parameters / num_observations
  
  return(avg_leverage)
  
}

plot_ext_std_resid_rot <- function(p, ref_linetype, ref_linecolor,resid_rot, case_df, 
                                   flag_extreme,max_flagged, extreme_value_shape, 
                                   extreme_value_color, obs_txt_size, obs_txt_vjust, 
                                   obs_txt_hjust){
  
  upper_threshold <- resid_rot
  lower_threshold <- resid_rot * -1
  
  p <- p + 
    geom_hline(yintercept=upper_threshold, 
               color=ref_linecolor, 
               linetype=ref_linetype) + 
    geom_hline(yintercept=lower_threshold, 
               color=ref_linecolor,
               linetype=ref_linetype)
  
  
  if(flag_extreme){
    
    above_filter <- (case_df[,"stu.res"] >= upper_threshold)
    below_filter <- (case_df[,"stu.res"] <= lower_threshold)
    
    # Combine the above filters, and subset to only include values that exceeded the threshold.
    exceeded_threshold <- above_filter | below_filter
    case_df <- case_df[exceeded_threshold,]
    
    # Grab the top  max_flagged values that exceeded the threshold, or all of them if less than max_flagged.
    num_extreme_observations <- as.numeric(nrow(case_df))
    num_obs_to_flag <- as.numeric(min(num_extreme_observations, max_flagged))
    case_df <- case_df[order(abs(case_df[,"stu.res"]), decreasing=TRUE),][1:num_obs_to_flag,]
    
    p <- p + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color) + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color, 
                 shape=extreme_value_shape) + 
      geom_text(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column, label=obs_number), hjust=obs_txt_hjust,
                vjust=obs_txt_vjust, color=extreme_value_color, size=obs_txt_size)
    
  }
  
  return(p)
  
}

plot_deleted_std_rot <- function(fit, p, ref_linetype, case_df,
                                 ref_linecolor, std_rotm, annot_reflines,
                                 flag_extreme, max_flagged, extreme_value_shape, 
                                 extreme_value_color, obs_txt_size, obs_txt_vjust, obs_txt_hjust) {
  
  rmse <- summary(fit)$sigma
  
  upper_multiplier <- (1 + std_rotm)
  lower_multiplier <- (1 - std_rotm)
  
  upper_threshold <- rmse * upper_multiplier
  lower_threshold <- rmse * lower_multiplier
  
  p <- p + 
    geom_hline(yintercept=rmse, 
               color=ref_linecolor, 
               linetype=ref_linetype) + 
    geom_hline(yintercept=upper_threshold, 
               color=ref_linecolor, 
               linetype=ref_linetype) + 
    geom_hline(yintercept=lower_threshold, 
               color=ref_linecolor, 
               linetype=ref_linetype)
  
  if(flag_extreme){
    
    # Filter to find values above the upper threshold or below
    # the lower threshold
    above_filter <- (case_df[,"si"] >= upper_threshold)
    below_filter <- (case_df[,"si"] <= lower_threshold)
    
    # Combine the above filters, and subset to only include values that exceeded the threshold.
    exceeded_threshold <- above_filter | below_filter
    case_df <- case_df[exceeded_threshold,]
    
    case_df[,"si_difference"] <- rmse - case_df[,"si"]
    
    # Grab the top  max_flagged values that exceeded the threshold, or all of them if less than max_flagged.
    num_extreme_observations <- as.numeric(nrow(case_df))
    num_obs_to_flag <- as.numeric(min(num_extreme_observations, max_flagged))
    case_df <- case_df[order(abs(case_df[,"si_difference"]), decreasing=TRUE),][1:num_obs_to_flag,]
    
    p <- p + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color) + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color, 
                 shape=extreme_value_shape) + 
      geom_text(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column, label=obs_number), hjust=obs_txt_hjust,
                vjust=obs_txt_vjust, color=extreme_value_color, size=obs_txt_size)
    
  }
  
  if(!annot_reflines){
    return(p)
  }
  
  return(p)
  
}

# Used in plot_residuals_vs_leverage
add_reference_lines <- function(fit, residual_type, case_df, add_reference_lines,
                                leverage_line_multiplier, p, resid_line_threshold, 
                                reference_linetype, reference_linecolor, annotate_thresholds) {
  
  if(!add_reference_lines){
    return(p)
  }
  
  leverage_threshold <- get_leverage_threshold(fit, leverage_line_multiplier, case_df)
  
  p <- p + geom_vline(xintercept=leverage_threshold, 
                      color=reference_linecolor, 
                      linetype=reference_linetype)
  
  # It only makes sense to plot the residual reference lines to externally studentized
  # residuals, so if its any other type, end the function here.
  if(residual_type != "externally_studentized"){
    return(p)
  }
  
  positive_resid_thresh=resid_line_threshold
  negative_resid_thresh=resid_line_threshold*-1
  
  p <- p + 
    geom_hline(yintercept=positive_resid_thresh, color=reference_linecolor, 
               linetype=reference_linetype) + 
    geom_hline(yintercept=negative_resid_thresh, color=reference_linecolor,
               linetype=reference_linetype)
  
  # If we don't want to annotate the threshold values, function can end here
  if(!annotate_thresholds){
    return(p)  
  }
  
  # Annotate the leverage threshold
  leverage_annot <- paste0(leverage_line_multiplier, "x avg leverage")
  leverage_df <- data.frame(txt=leverage_annot, x_txt=leverage_threshold, y_txt=-Inf)
  
  p <- p +
    geom_text(data=leverage_df, mapping=aes(x=x_txt, y=y_txt, label=txt, hjust=-0.05, vjust=-0.5), 
              color=reference_linecolor)
  
  return(p)
  
}

flag_extreme_observations <- function(fit, residual_type, case_df, flag_extreme_obs, 
                                      max_points_flagged, show_extreme_obs_numbers, resid_line_threshold, 
                                      extreme_value_color, p, leverage_line_multiplier, obs_txt_size, obs_txt_vjust,
                                      obs_txt_hjust){
  
  # Current implementation, flagging only makes sense for externally studentized residuals
  if(!flag_extreme_obs || residual_type != "externally_studentized"){
    return(p)
  }
  
  leverage_threshold <- get_leverage_threshold(fit, leverage_line_multiplier, case_df)
  
  # Filter dataframe to only contain observation that exceed the acceptable thresholds
  # for both leverage and (externally studentized) residual size
  case_df <- case_df[case_df[,"h"] > leverage_threshold,]
  case_df <- case_df[abs(case_df[,"Resid_Plot_Column"]) > resid_line_threshold,]
  
  num_values_to_flag <- min(as.numeric(nrow(case_df)), max_points_flagged)
  
  # Dataframe containing up to max_point_flagged observations with the largest Cooks D
  case_df <- case_df[order(abs(case_df[,"cook"]), decreasing=TRUE),][1:num_values_to_flag,]
  
  # Plot extreme values in red
  p <- p + geom_point(data=case_df, mapping=aes(x=h, y=Resid_Plot_Column), shape=8, color=extreme_value_color) +
    geom_point(data=case_df, mapping=aes(x=h, y=Resid_Plot_Column), color=extreme_value_color)
  
  # If we don't need to show the extreme observation numbers, we can stop the function here.
  if(!show_extreme_obs_numbers){
    return(p)
  }
  
  # Annotate the observation numbers for the extreme points
  p <- p +
    geom_text(data=case_df, mapping=aes(x=h, y=Resid_Plot_Column, label=obs_number),
              hjust=obs_txt_hjust, vjust=obs_txt_vjust, size=obs_txt_size, 
              color=extreme_value_color)
  
  return(p)
}

# Function to calculate the intercept and slope of the line that goes through the first and third
# quartiles of the (theoretical_quartile, data_quartile) x,y pairs. 
get_quartile_line_params <- function(qq){
  
  # Get the first and third quartiles for the theoretical distribution
  x_quartiles <- quantile(qq$x)
  first_quartile_x <- x_quartiles[[2]]
  third_quartile_x <- x_quartiles[[4]]
  
  # Get the first and third quartiles for the observed distribution (the data).
  y_quartiles <- quantile(qq$y)
  first_quartile_y <- y_quartiles[[2]]
  third_quartile_y <- y_quartiles[[4]]
  
  # Fit a line between the first and third quartiles, to get the intercept and slope for plotting
  line_df <- data.frame(x_quartiles= c(first_quartile_x, third_quartile_x), 
                        y_quartiles=c(first_quartile_y, third_quartile_y))
  
  quartile_line_fit <- lm(y_quartiles~x_quartiles, data=line_df)
  
  
  # Extract line parameters from the fit
  quartile_line_coefs <- quartile_line_fit$coeff
  quartile_line_intercept <- quartile_line_coefs[[1]]
  quartile_line_slope <- quartile_line_coefs[[2]]
  
  line_params <- list(slope=quartile_line_slope, intercept=quartile_line_intercept)
  
  return(line_params)
}

flag_n_largest <- function(qq_df, case_df, p, flag_largest_resid, flag_nlargest, flag_color_resid, 
                           flag_marker_shape, flag_txt_hjust,flag_txt_vjust, flag_txt_size){
  
  
  # Combined the case statistics (residual values) with the qqplot data
  # combined_df <- cbind(qq_df, case_df)
  combined_df <- merge(x=qq_df, y=case_df, by.x="obs_number", by.y="obs_number")
  
  #return(list(c_df=combined_df, q_df=qq_df, case_df=case_df))
  
  # Sort by absolute value of residual, so the largest residuals are at the top of the
  # dataframe, then grab the nlargest of those.
  nlargest_resids <- combined_df[order(abs(combined_df[,"Resid_Plot_Column"]), 
                                       decreasing=TRUE),][1:flag_nlargest,]
  
  p <- p + 
    geom_point(data=nlargest_resids, mapping=aes(x=x_values, y=y_values), color=flag_color_resid) + 
    geom_point(data=nlargest_resids, mapping=aes(x=x_values, y=y_values), color=flag_color_resid, 
               shape=flag_marker_shape) + 
    geom_text(data=nlargest_resids, mapping=aes(x=x_values,y=y_values, label=obs_number), 
              hjust=flag_txt_hjust, vjust=flag_txt_vjust, color=flag_color_resid, size=flag_txt_size)    
  
  return(p)
  
}

########################################### END PLOTTING HELPERS #######################################################





########################################### DATA CLEANING REPORT  #######################################################


add_missing_counts_to_file <- function(df, filepath){
  
  # Calculate the number of missing values per column
  df_missings <- sapply(df, function(x) sum(is.na(x)))
  
  # Grab the column names
  df_col_names <- names(df_missings)
  
  # For each column
  for(col_index in 1:length(df_col_names)){
    
    # Grab the name of this column and the number of missing values
    column_name <- df_col_names[col_index]
    num_missings <- df_missings[df_col_names[col_index]]
    
    # Write that information to the file
    cat(column_name, ":", num_missings, "\n", file=filepath, append=TRUE)
  }
  
}

build_data_cleaning_report <- function(df_orig, df_missing, df_duplicate, df_final, df_filtering, filepath){
  
  # BEFORE ANY CLEANING
  # calculate total number of rows in the data set
  df_total_rows <- nrow(df_orig)
  # Calculate the total number of duplicates
  df_complete_duplicates <- sum(duplicated(df_orig))
  # Calculate the number of rows that are exactly the same, only different prices
  df_dup_only_diff_prices <- sum((duplicated(df_orig[,names(df_orig) != "MSRP"]) & !duplicated(df_orig)))
  
  # AFTER THE MISSING VALUES FUNCTION RUNS
  # calculate total number of rows in the data set
  after_missings_total_rows <- nrow(df_missing)
  # Calculate the total number of duplicates
  after_missings_complete_duplicates <- sum(duplicated(df_missing))
  # Calculate the number of rows that are exactly the same, only different prices
  after_missings_dup_only_diff_prices <- sum((duplicated(df_missing[,names(df_missing) != "MSRP"]) & !duplicated(df_missing)))
  
  # AFTER THE EXTRA FILTERING FUNCTION RUNS
  # calculate total number of rows in the data set
  after_filtering_total_rows <- nrow(df_filtering)
  # Calculate the total number of duplicates
  after_filtering_complete_duplicates <- sum(duplicated(df_filtering))
  # Calculate the number of rows that are exactly the same, only different prices
  after_filtering_dup_only_diff_prices <- sum((duplicated(df_filtering[,names(df_filtering) != "MSRP"]) & !duplicated(df_filtering)))
  
  
  # AFTER THE HANDLE DUPLICATE FUNCTION RUNS
  # calculate total number of rows in the data set
  ad_total_rows <- nrow(df_duplicate)
  # Calculate the total number of duplicates
  ad_complete_duplicates <- sum(duplicated(df_duplicate))
  # Calculate the number of rows that are exactly the same, only different prices
  ad_dup_only_diff_prices <- sum((duplicated(df_duplicate[,names(df_duplicate) != "MSRP"]) & !duplicated(df_duplicate)))
  
  
  # AFTER THE HANDLE DUPLICATE FUNCTION RUNS
  # calculate total number of rows in the data set
  final_total_rows <- nrow(df_final)
  # Calculate the total number of duplicates
  final_complete_duplicates <- sum(duplicated(df_final))
  # Calculate the number of rows that are exactly the same, only different prices
  final_dup_only_diff_prices <- sum((duplicated(df_final[,names(df_final) != "MSRP"]) & !duplicated(df_final)))
  
  
  cat("=========================================\n", file=filepath)
  cat("          DATA CLEANING REPORT           \n", file=filepath, append=TRUE)
  cat("Report Generated: ", Sys.time(), " ", Sys.Date(), file=filepath, append=TRUE)
  cat("\n=========================================\n\n", file=filepath, append=TRUE)
  
  
  cat(">>>>> Prior To Any Cleaning <<<<<\n\n", file=filepath, append=TRUE)
  cat("Total number of rows: ", df_total_rows, "\n", file=filepath, append=TRUE)
  cat("Number of fully duplicate rows: ", df_complete_duplicates, "\n", file=filepath, append=TRUE)
  cat("Number of rows duplicate other than different MSRP's: ",df_dup_only_diff_prices, "\n\n", file=filepath, append=TRUE)
  cat("MISSING VALUES\n", file=filepath, append=TRUE)
  add_missing_counts_to_file(df=df_orig, filepath=filepath)
  cat("\n", "=======================================================", "\n\n\n", file=filepath, append=TRUE)
  
  
  cat(">>>>> After Running the Clean Missing Values Function <<<<<\n\n", file=filepath, append=TRUE)
  cat("Total number of rows: ", after_missings_total_rows, "\n", file=filepath, append=TRUE)
  cat("Number of fully duplicate rows: ", after_missings_complete_duplicates, "\n", file=filepath, append=TRUE)
  cat("Number of rows duplicate other than different MSRP's: ",after_missings_dup_only_diff_prices, "\n\n", file=filepath, append=TRUE)
  cat("MISSING VALUES\n", file=filepath, append=TRUE)
  add_missing_counts_to_file(df=df_missing, filepath=filepath)
  cat("\n", "=======================================================", "\n\n\n", file=filepath, append=TRUE)
  
  
  cat(">>>>> After Applying Additional Filters (e.g. Electric Cars, Expensive Cars) <<<<<\n\n", file=filepath, append=TRUE)
  cat("Total number of rows: ", after_filtering_total_rows, "\n", file=filepath, append=TRUE)
  cat("Number of fully duplicate rows: ", after_filtering_complete_duplicates, "\n", file=filepath, append=TRUE)
  cat("Number of rows duplicate other than different MSRP's: ",after_filtering_dup_only_diff_prices, "\n\n", file=filepath, append=TRUE)
  cat("MISSING VALUES\n", file=filepath, append=TRUE)
  add_missing_counts_to_file(df=df_filtering, filepath=filepath)
  cat("\n", "=======================================================", "\n\n\n", file=filepath, append=TRUE)
  
  
  cat(">>>>> After duplicate handling  <<<<<\n\n", file=filepath, append=TRUE)
  cat("Total number of rows: ", ad_total_rows, "\n", file=filepath, append=TRUE)
  cat("Number of fully duplicate rows: ", ad_complete_duplicates, " <-- should be zero ", "\n", file=filepath, append=TRUE)
  cat("Number of rows duplicate other than different MSRP's: ",ad_dup_only_diff_prices, "<-- should be zero", "\n\n", file=filepath, append=TRUE)
  cat("MISSING VALUES\n", file=filepath, append=TRUE)
  add_missing_counts_to_file(df=df_duplicate, filepath=filepath)
  cat("\n", "=======================================================", "\n\n\n", file=filepath, append=TRUE)
  
  
  cat(">>>>> At conclusion of data cleaning script  <<<<<\n\n", file=filepath, append=TRUE)
  cat("Total number of rows: ", final_total_rows, "\n", file=filepath, append=TRUE)
  cat("Number of fully duplicate rows: ", final_complete_duplicates, " <-- should be zero ", "\n", file=filepath, append=TRUE)
  cat("Number of rows duplicate other than different MSRP's: ",final_dup_only_diff_prices, "<-- should be zero", "\n\n", file=filepath, append=TRUE)
  cat("MISSING VALUES\n", file=filepath, append=TRUE)
  add_missing_counts_to_file(df=df_final, filepath=filepath)
  
  cat(">>>>> FACTOR COLUMN LEVELS <<<<<\n\n", file=filepath, append=TRUE)
  column_names <- colnames(df_final)
  for(column_index in 1:length(column_names)){
    
    column_name <- column_names[column_index]
    
    if(class(df_final[,column_name]) == "factor"){
      factor_levels <- stringr::str_flatten(as.character(levels(df_final[,column_name])), ", ")
      
      cat("~~~~~~~~~~~~~~~~~~~~\n\n", file=filepath, append=TRUE)
      cat("Factor Column Name: ", column_name, "\nNumber of levels: ", length(levels(df_final[,column_name])), "\n\nFactor Levels: ", factor_levels, "\n",  file=filepath, append=TRUE)
      cat("~~~~~~~~~~~~~~~~~~~~\n\n\n", file=filepath, append=TRUE)
    }
    
  }
  
  cat("\n", "=======================================================", "\n\n\n", file=filepath, append=TRUE)
  
}


########################################### END CLEANING REPORT  #######################################################




########################################### DATA CLEANING HELPERS #######################################################

market_categories_to_binary <- function(df){
  
  split_categories <- str_split(string=df[,"Market_Category"],
                                pattern=",")  
  
  
  
  # Get a character vector of the unique categories
  unique_categories <- unique(unlist(split_categories))
  
  # Remove N/A from unique_categories vector
  unique_categories <- unique_categories[unique_categories != "N/A"]
  
  # Remove High-Performance from unique_categories vector
  unique_categories <- unique_categories[unique_categories != "High-Performance"] 
  
  # Add all the new columns, initialized to zero
  for(category_index in 1:length(unique_categories)){
    
    category_name <- unique_categories[category_index]
    
    # Create a column of all zeros with this name
    df[,category_name] <- rep(0, nrow(df))
  }
  
  
  for(row_index in 1:length(split_categories)){
    
    categories <- split_categories[[row_index]]
    
    if("Factory Tuner" %in% categories){
      df[row_index, "Factory Tuner"] <- 1
    }
    
    if(("High-Performance" %in% categories) | ("Performance" %in% categories)){
      df[row_index, "Performance"] <- 1
    }
    
    if("Luxury" %in% categories){
      df[row_index, "Luxury"] <- 1
    }
    
    if("Flex Fuel" %in% categories){
      df[row_index, "Flex Fuel"] <- 1
    }
    
    if("Hatchback" %in% categories){
      df[row_index, "Hatchback"] <- 1
    }
    
    if("Hybrid" %in% categories){
      df[row_index, "Hybrid"] <- 1
    }
    
    if("Diesel" %in% categories){
      df[row_index, "Diesel"] <- 1
    }
    
    if("Crossover" %in% categories){
      df[row_index, "Crossover"] <- 1
    }
    
    if("Exotic" %in% categories){
      df[row_index, "Exotic"] <- 1
    }
  }
  
  return(df)
}

clean_vehicle_style_column <- function(df){
  
  
  df[,"Vehicle_Style"] <- plyr::mapvalues(df[,"Vehicle_Style"],
                                          from=c("2dr Hatchback", "4dr Hatchback", "Convertible", "Coupe", "Sedan", "Wagon", 
                                                 "Crew Cab Pickup", "Extended Cab Pickup", "Regular Cab Pickup",
                                                 "Cargo Minivan", "Cargo Van",
                                                 "Passenger Minivan", "Passenger Van",
                                                 "2dr SUV", "4dr SUV", "Convertible SUV"),
                                          to=c("car", "car", "car", "car", "car", "car",
                                               "truck", "truck", "truck",
                                               "cargo_van", "cargo_van",
                                               "passenger_van", "passenger_van",
                                               "suv", "suv", "suv"))
  
  return(df)
  
}



clean_fuel_type_column <- function(df){
  
  # STEP 1: Remove the natural gas car
  df <- df[df[,"Engine_Fuel_Type"] != "natural gas",]
  
  
  # STEP 2: Map to the new values
  df[,"Engine_Fuel_Type"] <- plyr::mapvalues(df[,"Engine_Fuel_Type"],
                                             from=c("premium unleaded (required)", 
                                                    "regular unleaded", 
                                                    "premium unleaded (recommended)", 
                                                    "flex-fuel (unleaded/E85)", 
                                                    "flex-fuel (premium unleaded recommended/E85)", 
                                                    "flex-fuel (premium unleaded required/E85)",
                                                    "flex-fuel (unleaded/natural gas)",
                                                    "diesel"),
                                             to=c("regular", 
                                                  "regular", 
                                                  "regular", 
                                                  "flex", 
                                                  "flex", 
                                                  "flex",
                                                  "flex",
                                                  "diesel"))
  
  
  
  
  
  return(df)
  
}


cleaning_filters <- function(df, remove_electric_cars, expensive_threshold){
  
  # If we decided not to do any additional filters, just leave this function
  if(!expensive_threshold & !remove_electric_cars){
    return(df)
  }
  
  # If we decided to filter based on car price
  if(expensive_threshold != FALSE){
    
    # Only keep the cars with price less than or equal to expensive_threshold
    df <- df[df[,"MSRP"] <= expensive_threshold,]
    
  }
  
  # If we decided to remove electric cars
  if(remove_electric_cars != FALSE){
    
    # Only keep the rows where the cars engine_fuel_type isn't electric
    df <- df[df[,"Engine_Fuel_Type"] != "electric",]
    
  }
  
  
  # UPDATE THE TWO DIRECT_DRIVE TRANSMISSIONS CHEVYS TO BE AUTOMATIC TRANSMISSIONS
  chevy_direct_drive_filter <- (df[,"Transmission_Type"] == "DIRECT_DRIVE") & (df[,"Make"] == "Chevrolet") & (df[,"Model"] == "Malibu")
  df[chevy_direct_drive_filter, "Transmission_Type"] <- "AUTOMATIC"
  
  # Remove UNKNOWN Transmission_Type values
  df <- df[df[,"Transmission_Type"] != "UNKNOWN",]
  
  
  # Cleaning the Engine_Fuel_Type column (binning values)
  df <- clean_fuel_type_column(df=df)
  
  # Bin vehicle_style column 
  df <- clean_vehicle_style_column(df)
  
  return(df)
  
}


fill_in_missings <- function(df){
  
  df[,"obs_number"] <- seq(1, as.numeric(nrow(df)))
  
  # Read in the file that contains the filled-in missing information
  fillin_file <- read.csv("./car_dataset_missing_fillins.csv", check.names=FALSE, na.strings=c(""))
  
  # Get the rows that are missing at least one item
  missing_rows <- df[!complete.cases(df),]
  
  # Remove the rows missing at least one field
  complete_df <- df[complete.cases(df),]
  
  # Grab the names of the dataframe columns
  column_names <- names(df)
  
  # Go down the rows in the dataset containing the missings
  for(row_num in 1:nrow(missing_rows)){
    
    # Grab the row
    this_row <- missing_rows[row_num, ]
    
    # Grab the row_id which we can use to get the corresponding row from fillin_file
    row_id <- missing_rows[row_num, "obs_number"]
    
    # Loop across the columns of this row to find the missings
    for(column_index in 1:length(column_names)){
      
      # Grab the name of this column
      column_name <- column_names[column_index]
      
      # Can be deleted, only for showing how the function is operating.
      # print(paste("Processing Row: ", row_num, " Column: ", column_name))
      
      # Check if this column has missing info, if it does, grab the replacement from fillin_file
      if(is.na(this_row[,column_name])){
        
        # Grab the replacement value
        replacement_value <- fillin_file[fillin_file[,"obs_number"]==row_id, column_name]
        
        # These lines can be deleted, they are only here in case of troubleshooting, or to display how the function is operating.
        # car_name <- fillin_file[fillin_file[,"obs_number"]==row_id, "Make"]
        # model <- fillin_file[fillin_file[,"obs_number"]==row_id, "Model"]
        # going_to <- missing_rows[row_num, "Make"]
        # print(paste0("Row: ", row_num, " Column: ", column_name, " Replacing with: ", replacement_value, " <-- ", car_name, " ", model, "... (", going_to, ")"))
        
        
        # If we had the right answer in our look-up file, fill it in! 
        if(length(replacement_value) != 0){
          
          # Fill in the missing information
          missing_rows[row_num, column_name] <- replacement_value          
        }
      }
    }
  }
  
  complete_df <- rbind(complete_df, missing_rows)
  
  # Lazily dropping any other rows NA's for now (should just be RX-7 and RX-8 left at this stage, so only these are dropped).
  complete_df <- tidyr::drop_na(complete_df)
  
  # Remove the obs_number column for now. The data will get their final obs_numbers at the end of data cleaning
  complete_df <- complete_df[,!(names(complete_df) %in% "obs_number")]
  
  return(complete_df)  
}


# SETTING DATA TYPES
set_datatypes <- function(df){
  
  
  ##### Columns with Factor Data Type #####
  
  factor_columns <- c("Make", "Model", "Engine_Fuel_Type", "Transmission_Type", "Market_Category",
                      "Vehicle_Size", "Vehicle_Style", "Driven_Wheels", "Year", "Engine_Cylinders")
  
  
  # For each column that needs to be a factor
  for(column_index in 1:length(factor_columns)){
    
    # Grab the name of this column
    column_name <- factor_columns[column_index]
    
    # If this column doesn't have a factor data type.
    if(class(df[,column_name]) != "factor"){
      
      df[,column_name] <- factor(df[,column_name])
      
    }
  }
  
  ##### Columns with Numeric Data Type #####
  
  numeric_columns <- c("MSRP", "Engine_HP", "Number_of_Doors",
                       "highway_MPG", "city_mpg", "Popularity", "Age")
  
  # For each column that needs to be a factor
  for(column_index in 1:length(numeric_columns)){
    
    # Grab the name of this column
    column_name <- numeric_columns[column_index]
    
    # If this column doesn't have a factor data type.
    if(class(df[,column_name]) != "numeric"){
      
      df[,column_name] <- as.numeric(df[,column_name])
    }
  }
  
  return(df)
  
}


#### DUPLICATE VALUE HANDLING 

clean_duplicates <- function(df){
  
  # Remove fully duplicate rows
  df <- df[!duplicated(df),]
  
  clean_duplicates_df <- avg_multiple_priced_duplicates(df=df)
  
  return(clean_duplicates_df)
}



avg_multiple_priced_duplicates <- function(df){
  
  count <- 0
  
  # Create a copy of the dataframe to edit
  new_df <- df
  
  for(index in 1:nrow(df)){
    
    # Grab a dataframe row, which we will use to search for rows that are
    # exactly the same, other than having a different MSRP
    df_row <- df[index,]
    
    # Get a set of rows that are the same as  this one (other than different MSRP)
    filtered_data <- filter_by_example(df=df, row_example=df_row)
    
    # Check how many rows matched df_row
    num_matching_rows <- nrow(filtered_data)
    
    # If more than one (itself) matched... there are duplicates
    if(num_matching_rows > 1){
      
      # Get the rownames for the duplicates
      row_names <- rownames(filtered_data)
      
      # Grab the first row name, we will keep this one (arbitrary) and drop the others
      first_row_name <- row_names[1]
      
      # Calculate the average price for these duplicate rows
      average_price <- mean(filtered_data[,"MSRP"])
      
      # Set the MSRP for the first instance of these duplicates to the average
      new_df[rownames(new_df) == first_row_name, "MSRP"] <- average_price
      
      # Remove the rest of the duplicates
      removal_row_names <- row_names[row_names != first_row_name]
      new_df <- new_df[!(rownames(new_df) %in%removal_row_names),]
      
      # Just for keeping track of how many things we remove, in case of troubleshooting.
      count <- count + 1
      
    }
  }
  
  return(new_df)
}


filter_by_example <- function(df, row_example){
  
  filtered_df <- filter_all_columns(df=df,
                                    make=row_example$Make, 
                                    model=row_example$Model, 
                                    yr=row_example$Year, 
                                    Engine_fuel_type=row_example$Engine_Fuel_Type, 
                                    Engine_HP=row_example$Engine_HP, 
                                    Engine_Cylinders=row_example$Engine_Cylinders, 
                                    Transmission_Type=row_example$Transmission_Type, 
                                    Driven_wheels=row_example$Driven_Wheels, 
                                    Number_of_Doors=row_example$Number_of_Doors,
                                    Market_Category=row_example$Market_Category, 
                                    vehicle_size=row_example$Vehicle_Size, 
                                    vehicle_style=row_example$Vehicle_Style, 
                                    highway_mpg=row_example$highway_MPG,
                                    city_mpg=row_example$city_mpg,
                                    popularity=row_example$Popularity)
  
  return(filtered_df)
  
}

filter_all_columns <- function(df, make, model, yr, Engine_fuel_type, Engine_HP, Engine_Cylinders, 
                               Transmission_Type, Driven_wheels, Number_of_Doors, Market_Category, vehicle_size, 
                               vehicle_style, highway_mpg, city_mpg, popularity){
  
  
  f1 <- df[,"Make"] == make
  f2 <- df[,"Model"] == model
  f3 <- df[,"Year"] == yr
  f4 <- df[,"Engine_Fuel_Type"] == Engine_fuel_type
  f5 <- df[,"Engine_HP"] == Engine_HP
  f6 <- df[,"Engine_Cylinders"] == Engine_Cylinders
  f7 <- df[,"Transmission_Type"] == Transmission_Type
  f8 <- df[,"Driven_Wheels"] == Driven_wheels
  f9 <- df[,"Number_of_Doors"] == Number_of_Doors
  f10 <- df[,"Market_Category"] == Market_Category
  f11 <- df[,"Vehicle_Size"] == vehicle_size
  f12 <- df[,"Vehicle_Style"] == vehicle_style
  f13 <- df[,"highway_MPG"] == highway_mpg
  f14 <- df[,"city_mpg"] == city_mpg
  f15 <- df[,"Popularity"] == popularity
  
  full_filter <- (f1 & f2 & f3 & f4 & f5 & f6 & f7 & f8 & f9 & f10 & f11 & f12 & f13 & f14 & f15)
  
  filtered_df <- df[full_filter,]
  
  return(filtered_df)
}


########################################### END DATA CLEANING HELPERS #######################################################




############################################# MODELING HELPERS #################################################

get_predictor_combos_manual <- function(features){
  
  # CREATES A LIST OF LISTS, WHERE EACH SUBLIST CONTAINS A COMBINATION OF FEATURES
  for(num_features in 1:length(features)){
    
    new_combos <- utils::combn(x=features,
                               m=num_features,
                               simplify=FALSE)
    
    if(num_features == 1){
      combined_combos <- new_combos
    }else{
      #combined_combos <- c(combined_combos, new_combos)
      combined_combos <- append(combined_combos, new_combos)
    }
  }
  
  return(combined_combos)
}


filter_predictor_squared_terms <- function(candidate_combinations){
  
  # Get the indicies of the candidate predictors that are squared terms
  squared_pred_indicies <- grep("_Squared$", candidate_combinations)
  
  # Get the actual names of the squared candidate predictors
  squared_preds <- test_predictors[squared_pred_indicies]
  
  for(squared_index in 1:length(squared_preds)){
    
    # Get the name of the squared predictor
    squared_predictor <- squared_preds[squared_index]
    
    # Get the name of the standard (non-squared version) of the predictor
    standard_predictor <- gsub(pattern="_Squared",
                               x=squared_predictor,
                               replacement="")
    
    # Remove all candidate predictor sets where the squared term is included, but the standard term is not
    candidate_combinations <- Filter(function(x){!((squared_predictor %in% x) & !(standard_predictor %in% x))}, candidate_combinations)
    
  }
  
  return(candidate_combinations)
  
}

generate_model_metrics <- function(model, val_data, test_data, train_data, predictor_set, response_variable){
  
  # Akaike Information Criterion
  aic <- olsrr::ols_aic(model=model)
  
  # Amemiyas Prediction Criterion
  apc <- olsrr::ols_apc(model=model)
  
  # Estimated Mean Squared Error of Prediction
  fpe <- olsrr::ols_fpe(model=model)
  
  # Hockings Sp -- Average prediction mean squared error
  hsp <- olsrr::ols_hsp(model)
  
  # Estimated error of prediction, assuming multivariate normality
  msep <- olsrr::ols_msep(model)
  
  # Prediction R-squared --> used to determine how well the model predicts
  # response s for new observations (larger is better)
  pred_rsq <- olsrr::ols_pred_rsq(model)
  
  # Prediction sum of squares (PRESS)
  press <- olsrr::ols_press(model)
  
  # Bayesian Information Criterion (SAS method uses residual sum of squares to compute SBC)
  sbc_SAS <- olsrr::ols_sbc(model, method="SAS")
  
  # Bayesian Information Criterion (R method uses loglikelihood to compute SBC)
  sbc_R <- olsrr::ols_sbc(model, method="R")
  
  # Root mean squared error
  rmse <- summary(model)$sigma
  
  # R-squared
  r_squared <- summary(model)$r.squared
  
  # Adjusted R-squared
  adjusted_r_squared <- summary(model)$adj.r.squared
  
  # Overall F-statistic
  f_statistic_value <- summary(model)$fstatistic[[1]]
  
  # Numerator Degrees of freedom for F-statistic
  f_stat_num_df <- summary(model)$fstatistic[[2]]
  
  # Denominator Degrees of freedom for F-statistc
  f_stat_denom_df <- summary(model)$fstatistic[[3]]
  
  #### TEST SET #### 
  
  # Predictions on test set
  test_set_predictions <- predict(model, test_data)
  
  # Get the metrics for the models predictions on the test set
  test_pred_metrics <- caret::postResample(pred=test_set_predictions, 
                                           obs=test_data[,response_variable])
  
  
  # RMSE for the predictions on the test set
  test_rmse <- test_pred_metrics[[1]]
  
  # R-squared for the predictions on the test set
  test_rsquared <- test_pred_metrics[[2]]
  
  # Mean absolute error for the predictions on the test set
  test_mae <- test_pred_metrics[[3]]
  
  
  #### VALIDATION SET ####
  
  # Use this model to make predictions on the validation set
  val_set_predictions <- predict(model, val_data)
  
  # Get the metrics for the models predictions on the validation set
  pred_metrics <- caret::postResample(pred=val_set_predictions, 
                                      obs=val_data[,response_variable])
  
  
  # RMSE for the predictions on the validation set
  val_rmse <- pred_metrics[[1]]
  
  # R-squared for the preditions on the validation set
  val_rsquared <- pred_metrics[[2]]
  
  # Mean absolute error for the predictions on the validation set
  val_mae <- pred_metrics[[3]]
  
  #### TRAIN SET ####
  
  # Predictions on test set
  train_set_predictions <- predict(model, train_data)
  
  # Get the metrics for the models predictions on the test set
  train_pred_metrics <- caret::postResample(pred=train_set_predictions, 
                                            obs=train_data[,response_variable])
  
  
  # RMSE for the predictions on the test set
  train_rmse <- train_pred_metrics[[1]]
  
  # R-squared for the predictions on the test set
  train_rsquared <- train_pred_metrics[[2]]
  
  # Mean absolute error for the predictions on the test set
  train_mae <- train_pred_metrics[[3]]
  
  
  # If the target is log transformed, tested by its name starting with the word log
  if(gdata::startsWith(str=response_variable, pattern="log")){
    
    # Calculate predictions metrics using back-transformed predictions and regular (not logged) response
    test_pred_metrics_orig <- caret::postResample(pred=exp(test_set_predictions), 
                                                  obs=exp(test_data[,response_variable]))
    
    
    # BACK TRANSFORMED TEST SET METRICS
    # RMSE for the predictions on the test set
    test_rmse_orig <- test_pred_metrics_orig[[1]]
    
    # R-squared for the predictions on the test set
    test_rsquared_orig <- test_pred_metrics_orig[[2]]
    
    # Mean absolute error for the predictions on the test set
    test_mae_orig <- test_pred_metrics_orig[[3]]
    

    # BACK TRANSFORMED VAL SET METRICS
    # Get the metrics for the models predictions on the validation set
    val_pred_metrics_orig <- caret::postResample(pred=exp(val_set_predictions), 
                                                 obs=exp(val_data[,response_variable]))
    
    
    # RMSE for the predictions on the validation set
    val_rmse_orig <- val_pred_metrics_orig[[1]]
    
    # R-squared for the predictions on the validation set
    val_rsquared_orig <- val_pred_metrics_orig[[2]]
    
    # Mean absolute error for the predictions on the validation set
    val_mae_orig <- val_pred_metrics_orig[[3]]
    
    
    # Get the metrics for the models predictions on the test set (comparing back transformed preds to untransformed target)
    train_pred_metrics_orig <- caret::postResample(pred=exp(train_set_predictions), 
                                                   obs=exp(train_data[,response_variable]))
    
    
    # BACK TRANSFORMED TRAIN SET METRICS
    # RMSE for the predictions on the test set
    train_rmse_orig <- train_pred_metrics_orig[[1]]
    
    # R-squared for the predictions on the test set
    train_rsquared_orig <- train_pred_metrics_orig[[2]]
    
    # Mean absolute error for the predictions on the test set
    train_mae_orig <- train_pred_metrics_orig[[3]]
    
  }else{
    test_rmse_orig <- "Target_Not_Log_Transformed"
    test_rsquared_orig <- "Target_Not_Log_Transformed"
    test_mae_orig <- "Target_Not_Log_Transformed"
    val_rmse_orig <- "Target_Not_Log_Transformed"
    val_rsquared_orig <- "Target_Not_Log_Transformed"
    val_mae_orig <- "Target_Not_Log_Transformed"
    train_rmse_orig <- "Target_Not_Log_Transformed"
    train_rsquared_orig <- "Target_Not_Log_Transformed"
    train_mae_orig <- "Target_Not_Log_Transformed"
  }
  
  model_predictors <- stringr::str_c(predictor_set, collapse="  ")
  
  
  model_eval_data <- data.frame(predictors=model_predictors,
                                val_mae=val_mae,
                                val_rmse=val_rmse,
                                val_rsq=val_rsquared,
                                train_rmse=train_rmse,
                                train_rsquared=train_rsquared,
                                train_mae=train_mae,
                                val_rmse_orig=val_rmse_orig,
                                val_rsq_orig=val_rsquared_orig,
                                val_mae_orig=val_mae_orig,
                                train_rmse_orig=train_rmse_orig,
                                train_rsq_orig=train_rsquared_orig,
                                train_mae_orig=train_mae_orig,
                                aic=aic,
                                apc=apc,
                                fpe=fpe,
                                hsp=hsp,
                                msep=msep,
                                rmse=rmse,
                                press=press,
                                sbc_SAS=sbc_SAS,
                                sbc_R=sbc_R, 
                                pred_rsq=pred_rsq,
                                rsq=r_squared,
                                adj_rsq=adjusted_r_squared,
                                test_rmse=test_rmse,
                                test_rsquared=test_rsquared,
                                test_mae=test_mae,
                                fstat=f_statistic_value,
                                f_num_df=f_stat_num_df,
                                f_denom_df=f_stat_denom_df)
  
  return(model_eval_data)
  
}

############################################# END MODELING HELPERS #################################################





