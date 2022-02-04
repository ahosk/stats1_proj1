source("./Applied_Stats_Project_Function_Helpers.R")



############################################# PLOTTING FUNCTIONS #################################################


# Function to display a correlation matrix
# corr_type --> one of "pearson", "kendall" or "spearman"
plot_correlation <- function(df, col_names=NULL, remove_columns=NULL, round_digits=2,
                             method="square", outline_color="white", type="lower", 
                             theme="theme_minimal", show_diag=FALSE, hc_order=TRUE, 
                             annotate_values=TRUE, corr_type="pearson",
                             legend_title="Correlation Coefficient", title=NULL) {
  
  
  # Filter to only numeric columns (data must be numeric to include in correlation matrix).
  df <- dplyr::select_if(df, is.numeric)
  
  # Remove any user specified columns
  df <- filter_columns(df, col_names=col_names, remove_columns=remove_columns)
  
  # Create the correlation matrix.
  # use="complete.obs" --> Do not consider any records that have NA's in one or more columns when making corr matrix.
  corr_matrix <- cor(df, method=corr_type, use="complete.obs")
  
  if(!is.null(title)){
    plot_title <- title
    
  }else{
    plot_title <- get_plot_labels(plot_kind="correlation", plot_type_info=corr_type)  
    
  }
  
  
  p <- ggcorrplot(corr_matrix, method=method, outline.col=outline_color, 
                  type=type, ggtheme=theme, title=plot_title, show.diag=show_diag, digits=round_digits,
                  hc.order=hc_order, lab=annotate_values, legend.title=legend_title)
  
  return(p)
  
}


plot_missings <- function(df, omit_complete_columns=FALSE, plot_percent_missing=FALSE){
  
  if(omit_complete_columns){
    df <- get_columns_with_missings(df) 
  }
  
  p <- naniar::gg_miss_var(df, show_pct=plot_percent_missing)
  
  return(p)
  
}

plot_transform_comparison_hist <- function(df, continuous_variable, fill_color_orig="#bc5090", outline_color_orig="#003f5c", 
                                           fill_color_trans="#bc5090", outline_color_trans="#003f5c", num_bins_orig=NULL, binwidth_orig=NULL, 
                                           num_bins_trans=NULL, binwidth_trans=NULL, title_orig=NULL, title_trans=NULL, table_loc_orig="upper_right", 
                                           table_loc_trans="upper_right", add_table_orig=TRUE, add_table_trans=TRUE){
  
  
  orig_plot <- plot_histogram(df=df, 
                              continuous_variable=continuous_variable, 
                              fill_color=fill_color_orig, 
                              outline_color=outline_color_orig, 
                              num_bins=num_bins_orig,
                              binwidth=binwidth_orig,
                              table_loc=table_loc_orig,
                              add_table=add_table_orig, 
                              title=title_orig)
  
  
  
  transformed_var_name <- paste("Log_", continuous_variable)
  
  df[,transformed_var_name] <- log(df[,continuous_variable])
  
  trans_plot <- plot_histogram(df=df, 
                               continuous_variable=transformed_var_name, 
                               fill_color=fill_color_trans, 
                               outline_color=outline_color_trans, 
                               num_bins=num_bins_trans,
                               binwidth=binwidth_trans,
                               table_loc=table_loc_trans,
                               add_table=add_table_trans, 
                               title=title_trans)
  
  p <- gridExtra::grid.arrange(orig_plot, trans_plot, nrow=1, ncol=2)
  
  return(p)
  
}


plot_histogram <- function(df, continuous_variable, fill_color="#bc5090",  
                           outline_color="#003f5c", num_bins=NULL, binwidth=NULL,
                           title=NULL, table_loc="upper_right", add_table=TRUE){
  
  
  df[,"continuous_var"] <- df[,continuous_variable]
  
  
  if(is.null(title)){
    plot_title <-  paste0("Distribution of ", stringr::str_to_title(continuous_variable))   
  }
  else{
    plot_title <- title
  }
  
  
  p <- ggplot(data=df, mapping=aes(x=continuous_var)) +
    geom_histogram(binwidth=binwidth, bins=num_bins, fill=fill_color, color=outline_color) + 
    ggtitle(plot_title) +
    ylab(paste0("Distribution of ", stringr::str_to_title(continuous_variable))) +
    xlab(stringr::str_to_title(continuous_variable))
  
  
  if(add_table){
    p <- add_table_to_plot(p=p, df=df, x_var=continuous_variable, table_loc=table_loc)   
  }
 
  
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


plot_scatter <- function(data, x_var, y_var, shade_var=NULL, shape_var=NULL, 
                         size_var=NULL, max_size=8, size_guide=FALSE, alpha=0.8,
                         show_regression=FALSE, conf_level=0.95, pred_band=TRUE, conf_band=TRUE, 
                         reg_linecolor="#ffa600", conf_linecolor="#bc5090", pred_linecolor="#003f5c",
                         conf_linetype="solid", pred_linetype="dashed", round_digits=5, reg_table=TRUE,
                         table_loc="upper_left", filter_column=NULL, keep_values=NULL, 
                         remove_less_than=NULL, remove_greater_than=NULL, identify_obs=FALSE, 
                         obs_txt_color="red", obs_txt_size=3, obs_txt_vjust=-0.4, obs_txt_hjust=0,
                         remove_obs_numbers=NULL, remove_legend=FALSE){
  
  df <- data
  
  df <- add_obs_number_column(df)
  
  df <- filter_by_observation_numbers(df, observation_numbers=remove_obs_numbers)
  
  data_list <- get_plotting_data(df=df, 
                                 x_var=x_var, y_var=y_var, 
                                 shade_var=shade_var, shape_var=shape_var,
                                 size_var=size_var, filter_column=filter_column, 
                                 keep_values=keep_values, remove_less_than=remove_less_than, 
                                 remove_greater_than=remove_greater_than)
  
  df <- data_list[["data_frame"]]
  shading_variable <- data_list[["shading_var"]]
  shape_variable <- data_list[["shape_var"]]
  size_variable <- data_list[["size_var"]]
  
  p <- ggplot(data=df, mapping=aes(x=Explanatory, y=Response)) + 
    geom_point(aes(color=shading_variable, shape=shape_variable, size=size_variable), alpha=alpha) +
    scale_shape_manual(values=1:nlevels(shape_variable)) + 
    scale_size_area(max_size=max_size, guide=size_guide)
  
  # If we want points to point their observation number, for investigative purposes
  p <- add_obs_numbers(p=p, df=df, obs_txt_color=obs_txt_color, obs_txt_size=obs_txt_size, obs_txt_vjust=obs_txt_vjust, 
                       obs_txt_hjust=obs_txt_hjust, identify_obs=identify_obs, x_var=x_var, y_var=y_var, 
                       called_from="scatter")
  
  if(!remove_legend){
    p <- add_legend_data(p, shade_var, shape_var) 
  }else{
    p <- p + theme(legend.position = "none")
  }
  
  p <- add_regression_and_title(df=df, show_regression=show_regression, p=p, conf_level=conf_level, 
                                pred_band=pred_band, conf_band=conf_band, reg_linecolor=reg_linecolor, 
                                conf_linecolor=conf_linecolor, pred_linecolor=pred_linecolor, 
                                conf_linetype=conf_linetype, pred_linetype=pred_linetype, x_var=x_var, 
                                y_var=y_var, round_digits=round_digits, reg_table=reg_table, 
                                table_loc=table_loc)
  
  return(p)
  
}


plot_means_by_category <- function(df, continuous_variable, categorical_variable, errorbar_width=0.1, xtick_rotation=45,
                                   remove_legend=FALSE){
  
  df[,"x_variable"] <- df[,categorical_variable]
  df[,"y_variable"] <- df[,continuous_variable]
  
  plot_labels <- get_plot_labels(plot_kind="means_plot", 
                                 plot_type_info=continuous_variable, 
                                 extra_info=categorical_variable)
  
  
  summary_data <- df %>% group_by(x_variable) %>% summarize_at(vars(y_variable), list(Mean=mean, SD=sd))
  
  summary_data <- summary_data[order(summary_data$Mean),]
  
  p <- ggplot(data=summary_data, aes(x=x_variable,y=Mean, colour=x_variable))+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=errorbar_width) +
    scale_x_discrete(limits=summary_data$x_variable) +
    geom_line(mapping=aes(x=x_variable, y=Mean))+
    ggtitle(plot_labels$title) +
    ylab(plot_labels$ylabel)+ 
    xlab(plot_labels$xlabel) +
    labs(color=categorical_variable) +
    scale_y_continuous(labels=scales::comma_format(big.mark=",", decimal.mark=".")) +
    theme(axis.text.x=element_text(angle=xtick_rotation))
  
  
  if(remove_legend){
    p <- p + theme(legend.position="none")
  }
  
  
  return(p)
  
}

plot_factor_level_counts <- function(df, factor_variable, xtick_rotation=0, outline_color="black", 
                                     add_freq_txt=TRUE, txt_vjust=-0.5, txt_hjust=NULL, text_rotation=0,
                                     remove_legend=FALSE){
  
  freq_df <- data.frame(table(df[,factor_variable]))
  
  
  plot_labels <- get_plot_labels(plot_kind="factor_level_counts", 
                                 plot_type_info=factor_variable, 
                                 extra_info=NULL)
  
  # Reorder and reset the levels so bars plot smallest to largest in frequency
  freq_df <- freq_df[order(freq_df[,"Freq"]),]
  freq_df[,"Var1"] <- factor(freq_df$Var1, levels=freq_df$Var1)
  
  p <- ggplot(data=freq_df, mapping=aes(x=Var1, y=Freq, fill=Var1)) +
    geom_col(colour=outline_color) +
    xlab(plot_labels$xlabel) +
    ylab(plot_labels$ylabel) +
    ggtitle(plot_labels$title) +
    theme(axis.text.x=element_text(angle=xtick_rotation)) +
    labs(fill=factor_variable) 
  
  
  if(add_freq_txt){
    p <- p + geom_text(mapping = aes(x=Var1, y=Freq, label=Freq, vjust=txt_vjust, 
                                     hjust=txt_hjust, angle=text_rotation))
    
  }  
  
  if(remove_legend){
    p <- p + theme(legend.position="none")
  }
  
  return(p)
  
}


############################################# DATA CLEANING REPORT #################################################


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
  
  
  cat(">>>>> At Conclusion of Data Cleaning Script <<<<<\n\n", file=filepath, append=TRUE)
  cat("Total number of rows: ", ad_total_rows, "\n", file=filepath, append=TRUE)
  cat("Number of fully duplicate rows: ", ad_complete_duplicates, " <-- should be zero ", "\n", file=filepath, append=TRUE)
  cat("Number of rows duplicate other than different MSRP's: ",ad_dup_only_diff_prices, "<-- should be zero", "\n\n", file=filepath, append=TRUE)
  cat("MISSING VALUES\n", file=filepath, append=TRUE)
  add_missing_counts_to_file(df=df_duplicate, filepath=filepath)
  cat("\n", "=======================================================", "\n\n\n", file=filepath, append=TRUE)
  
  
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


############################################# DATA CLEANING #################################################


clean_data <- function(df, duplicate_handling=TRUE, build_cleaning_report=TRUE, remove_electric_cars=TRUE,
                       expensive_threshold=500000, report_filepath="Data_Cleaning_Report.txt"){
  
  # replace spaces in column names with underscores
  names(df) <- gsub(" ", "_", names(df))
  
  
  #### ONLY TO GENERATE DATA CLEANING REPORT
  df_orig <- df

  
  # >>>> ACTUAL DATA CLEANING FUNCTION <<<<
  # fill in missing values
  df <- fill_in_missings(df=df)
  
  
  #### ONLY TO GENERATE DATA CLEANING REPORT
  df_missing <- df
  
  ### Additional one off adjustments found during eda
  #
  # Fix the Audi with crazy high MPG
  df[(df[,"Make"]=="Audi") & (df[,"Model"]=="A6") & (df[,"Year"]==2017) & (df[,"highway_MPG"] > 300), "highway_MPG"] <- 35
  
  
  
  
  # Does things such as removing electric vehicles and filtering based on
  # car price, if desired.
  df <- cleaning_filters(df=df, 
                         expensive_threshold=expensive_threshold, 
                         remove_electric_cars=remove_electric_cars)
  
  
  #### ONLY TO GENERATE DATA CLEANING REPORT
  df_filtering <- df
  
  
  # >>>> ACTUAL DATA CLEANING FUNCTION <<<<
  if(duplicate_handling){
    df <- clean_duplicates(df=df)  
  }
  
  #### ONLY TO GENERATE DATA CLEANING REPORT
  df_dup <- df
  
  ### Ensure datatypes match the assignments data dictionary
  df <- set_datatypes(df)
  
  if(build_cleaning_report){
    build_data_cleaning_report(df_orig=df_orig, 
                               df_missing=df_missing, 
                               df_duplicate=df_dup, 
                               df_final=df,
                               df_filtering=df_filtering,
                               filepath=report_filepath)  
  }
  
  
  
  # Leave this at the end! 
  # Add an observation number column, it is helpful for plotting and investigating points.
  df[,"obs_number"] <- seq(1, as.numeric(nrow(df))) 
  
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
                      "Vehicle_Size", "Vehicle_Style", "Driven_Wheels")
  
  
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
  
  numeric_columns <- c("MSRP", "Year", "Engine_HP", "Engine_Cylinders", "Number_of_Doors",
                       "highway_MPG", "city_mpg", "Popularity")
  
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

############################################# END DATA CLEANING #################################################


############################################# MODELING FUNCTIONS #################################################

create_train_val_test_sets <- function(df, train_pct=0.8, val_pct=0.1, random_seed=42){
  
  # Set a random seed for repeat-ability
  set.seed(random_seed)
  
  # Get the total number of observations
  total_observations <- nrow(df)
  
  # Get the number of training samples as train_percentage % of the total samples
  num_train_samples <- floor(total_observations * train_pct)
  
  # Number of samples in the validation set
  num_val_samples <- floor(total_observations * val_pct)
  
  # Anything that isn't train or val becomes test
  num_test_samples <- total_observations - num_train_samples - num_val_samples
  
  # Get the indicies for the test set
  test_indicies <- sample(x=total_observations, 
                          size=num_test_samples, 
                          replace=FALSE)
  
  # Create the test set
  test_data <- df[test_indicies,]
  
  # Create the NOT test set (will be split in train and val next)
  not_test_data <- df[-test_indicies,]
  
  # Size of the combined training and validation sets
  combined_train_val_set_size <- num_train_samples + num_val_samples
  
  # Get the validation set indices
  val_indicies <- sample(x=combined_train_val_set_size, size=num_val_samples, replace=FALSE)
  
  # Create the validation set
  val_data <- not_test_data[val_indicies,]
  
  # Create the train set
  train_data <- not_test_data[-val_indicies,]
  
  all_datasets <- list(train=train_data, 
                       val=val_data, 
                       test=test_data)
  
  return(all_datasets)
  
}

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


run_best_subset_selection <- function(train_data, val_data, candidate_predictors, response_variable="MSRP",
                                      save_every=1000, save_path="./model_checkpoints/", base_save_name="project1_models",
                                      order_column="val_rmse", filter_combinations=TRUE){
  
  # Generate all possible predictor combinations
  predictor_combinations <- get_predictor_combos_manual(features=candidate_predictors)
  
  # Filter the combinations to remove any that include a squared term without including the 
  # non-squared version of that same term... usually that is a good rule of thumb.
  if(filter_combinations){
    predictor_combinations <- filter_predictor_squared_terms(candidate_combinations = predictor_combinations)  
  }
  
  for(combo_index in 1:length(predictor_combinations)){
    
    # Get the set of predictors for this model
    predictor_set <- predictor_combinations[[combo_index]]
    
    # Generate a formula using this set of predictors
    lm_formula <- as.formula(paste0(response_variable, "~", stringr::str_c(predictor_set, 
                                                                           collapse=" + ")))
    
    
    # Fit a regression using these predictors and the training data
    fit <- lm(formula=lm_formula, 
              data=train_data)
    
    
    # Generate the internal and validation metrics for this model
    model_metrics <- generate_model_metrics(model=fit, 
                                            val_data=val_data, 
                                            predictor_set=predictor_set)
    
    
    # If this is the first model processed, then the result of this
    # model is the start of the final result, otherwise, append the result 
    # of this model to the final result
    if(combo_index == 1){
      final_metrics <- model_metrics
    }else{
      final_metrics <- rbind(final_metrics, model_metrics)
    }
    
    # If it is time to save a checkpoint
    if(combo_index %% save_every == 0){
      
      # Sort the lowest dataframe so the model with the best order_by metric is
      # on top. This makes it easy to see if the best model has improved when checking on progress.
      # save_file <- final_metrics[order(final_metrics[,order_by]),]
      
      final_metrics <- final_metrics[order(final_metrics[,order_column]),]
      save_file_name <- paste(base_save_name, "_iteration_", combo_index, ".csv")
      full_save_path <- gsub(x=paste(save_path, save_file_name),
                             pattern=" ",
                             replacement="",
                             fixed = TRUE)
      
      write.csv(x=final_metrics, file=full_save_path, row.names=FALSE)
    }
  }
  
  # Final Save 
  final_metrics <- final_metrics[order(final_metrics[,order_column]),]
  save_file_name <- paste(base_save_name, "_FINAL.csv")
  full_save_path <- gsub(x=paste(save_path, save_file_name),
                         pattern=" ",
                         replacement="",
                         fixed=TRUE)
  
  write.csv(x=final_metrics, file=full_save_path, row.names=FALSE)
  
  return(final_metrics)
}


generate_model_metrics <- function(model, val_data, predictor_set){
  
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
  
  # Use this model to make predictions on the validation set
  val_set_predictions <- predict(model, val_data)
  
  # Get the metrics for the models predictions on the validation set
  pred_metrics <- caret::postResample(pred=val_set_predictions, 
                                      obs=val_data$MSRP)
  
  
  # RMSE for the predictions on the validation set
  val_rmse <- pred_metrics[[1]]
  
  # R-squared for the preditions on the validation set
  val_rsquared <- pred_metrics[[2]]
  
  # Mean absolute error for the predictions on the validation set
  val_mae <- pred_metrics[[3]]
  
  
  model_predictors <- stringr::str_c(predictor_set, collapse="  ")
  
  
  model_eval_data <- data.frame(predictors=model_predictors,
                                val_mae=val_mae,
                                val_rmse=val_rmse,
                                val_rsq=val_rsquared,
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
                                fstat=f_statistic_value,
                                f_num_df=f_stat_num_df,
                                f_denom_df=f_stat_denom_df)
  
  return(model_eval_data)
  
}
