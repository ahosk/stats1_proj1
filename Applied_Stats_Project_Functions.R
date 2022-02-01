source("./Applied_Stats_Project_Function_Helpers.R")


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


build_data_cleaning_report <- function(df_orig, df_missing, df_duplicate, filepath){
  
  # BEFORE ANY CLEANING
  # Calculate the total number of duplicates
  df_complete_duplicates <- sum(duplicated(df_orig))
  # Calculate the number of rows that are exactly the same, only different prices
  df_dup_only_diff_prices <- sum((duplicated(df_orig[,names(df_orig) != "MSRP"]) & !duplicated(df_orig)))
  
  # AFTER THE MISSING VALUES FUNCTION RUNS
  # Calculate the total number of duplicates
  after_missings_complete_duplicates <- sum(duplicated(df_missing))
  # Calculate the number of rows that are exactly the same, only different prices
  after_missings_dup_only_diff_prices <- sum((duplicated(df_missing[,names(df_missing) != "MSRP"]) & !duplicated(df_missing)))
  
  
  # AFTER THE HANDLE DUPLICATE FUNCTION RUNS
  # Calculate the total number of duplicates
  ad_complete_duplicates <- sum(duplicated(df_duplicate))
  # Calculate the number of rows that are exactly the same, only different prices
  ad_dup_only_diff_prices <- sum((duplicated(df_duplicate[,names(df_duplicate) != "MSRP"]) & !duplicated(df_duplicate)))
  
  
  
  cat("=========================================\n", file=filepath)
  cat("          DATA CLEANING REPORT           \n", file=filepath, append=TRUE)
  cat("Report Generated: ", Sys.time(), " ", Sys.Date(), file=filepath, append=TRUE)
  cat("\n=========================================\n\n", file=filepath, append=TRUE)
  
  
  cat(">>>>> Prior To Any Cleaning <<<<<\n\n", file=filepath, append=TRUE)
  cat("Number of fully duplicate rows: ", df_complete_duplicates, "\n", file=filepath, append=TRUE)
  cat("Number of rows duplicate other than different MSRP's: ",df_dup_only_diff_prices, "\n\n", file=filepath, append=TRUE)
  cat("MISSING VALUES\n", file=filepath, append=TRUE)
  add_missing_counts_to_file(df=df_orig, filepath=filepath)
  cat("\n", "=======================================================", "\n\n\n", file=filepath, append=TRUE)
  
  
  cat(">>>>> After Running the Clean Missing Values Function <<<<<\n\n", file=filepath, append=TRUE)
  cat("Number of fully duplicate rows: ", after_missings_complete_duplicates, "\n", file=filepath, append=TRUE)
  cat("Number of rows duplicate other than different MSRP's: ",after_missings_dup_only_diff_prices, "\n\n", file=filepath, append=TRUE)
  cat("MISSING VALUES\n", file=filepath, append=TRUE)
  add_missing_counts_to_file(df=df_missing, filepath=filepath)
  cat("\n", "=======================================================", "\n\n\n", file=filepath, append=TRUE)
  
  
  cat(">>>>> At Conclusion of Data Cleaning Script <<<<<\n\n", file=filepath, append=TRUE)
  cat("Number of fully duplicate rows: ", ad_complete_duplicates, " <-- should be zero ", "\n", file=filepath, append=TRUE)
  cat("Number of rows duplicate other than different MSRP's: ",ad_dup_only_diff_prices, "<-- should be zero", "\n\n", file=filepath, append=TRUE)
  cat("MISSING VALUES\n", file=filepath, append=TRUE)
  add_missing_counts_to_file(df=df_duplicate, filepath=filepath)
  cat("\n", "=======================================================", "\n\n\n", file=filepath, append=TRUE)
  
}


############################################# DATA CLEANING #################################################


clean_data <- function(df, duplicate_handling=TRUE, build_cleaning_report=TRUE, report_filepath="Data_Cleaning_Report.txt"){
  
  # replace spaces in column names with underscores
  names(df) <- gsub(" ", "_", names(df))
  
  
  #### ONLY TO GENERATE DATA CLEANING REPORT
  df_orig <- df

  
  # >>>> ACTUAL DATA CLEANING FUNCTION <<<<
  # fill in missing values
  df <- fill_in_missings(df=df)
  
  
  #### ONLY TO GENERATE DATA CLEANING REPORT
  df_missing <- df
  
  
  # >>>> ACTUAL DATA CLEANING FUNCTION <<<<
  if(duplicate_handling){
    df <- clean_duplicates(df=df)  
  }
  
  #### ONLY TO GENERATE DATA CLEANING REPORT
  df_dup <- df

  if(build_cleaning_report){
    build_data_cleaning_report(df_orig=df_orig, 
                               df_missing=df_missing, 
                               df_duplicate=df_dup, 
                               filepath=report_filepath)  
  }
  
  
  return(df)
}


fill_in_missings <- function(df){
  
  # FILL IN MISSINGS IN Engine_Fuel_Type
  # Suzuki Verona 2004 manual says regular unleaded
  # Three records here but they are actually all the same... two duplicates
  df[is.na(df[,"Engine_Fuel_Type"]),"Engine_Fuel_Type"] <- "regular unleaded"
  
  #FILL IN MISSINGS IN THE Number_of_Doors column
  df[(is.na(df[,"Number_of_Doors"]) & df[,"Make"] == "Ferrari"), "Number_of_Doors"] <- 2
  df[(is.na(df[,"Number_of_Doors"]) & df[,"Model"] == "Model S"), "Number_of_Doors"] <- 4
  
  
  # Engine_HP column
  df[(is.na(df[,"Engine_HP"]) & df[,"Make"] == "FIAT" & df[,"Model"] == "500e"), "Engine_HP"] <- 111
  df[(is.na(df[,"Engine_HP"]) & df[,"Make"] == "Lincoln" & df[,"Model"] == "Continental" & df[,"Year"] == 2017), "Engine_HP"] <- 350
  df[(is.na(df[,"Engine_HP"]) & df[,"Make"] == "Ford" & df[,"Model"] == "Escape" & df[,"Year"] == 2017), "Engine_HP"] <- 200
  df[(is.na(df[,"Engine_HP"]) & df[,"Make"] == "Ford" & df[,"Model"] == "Focus"), "Engine_HP"] <- 236
  df[(is.na(df[,"Engine_HP"]) & df[,"Make"] == "Ford" & df[,"Model"] == "Freestar" & df[,"Year"] == 2005), "Engine_HP"] <- 200
  
  # Lazily dropping NA's for now
  df <- tidyr::drop_na(df)
  
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
