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

# Function for creating scatter plots
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

# Function to plot the average value of some continuous variable, for each level of a categorical variable
# Error bars representing the standard deviation of each average are also added
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

# FUNCTION TO PLOT RESIDUALS VS FITTED VALUES
# valid residual_types: "externally_studentized", "internally_studentized", "regular", "deleted" (PRESS)
#                 
plot_residuals <- function(fit, dataframe=NULL, residual_type="externally_studentized", plot_zero_hline=TRUE,
                           zero_hline_linetype="solid", zero_hline_color="red", remove_less_than=NULL,
                           remove_greater_than=NULL, flag_extreme_values=TRUE, extreme_thresh_std=3.5,
                           extreme_thresh_regular=3, id_extreme_values=FALSE, extreme_value_color="red",
                           obs_txt_size=3, obs_txt_hjust=0, obs_txt_vjust=-0.4){
  
  
  
  case_df <- get_residual_plot_data(fit=fit, 
                                    dataframe=dataframe,
                                    residual_type=residual_type, 
                                    remove_less_than=remove_less_than, 
                                    remove_greater_than=remove_greater_than)
  
  
  plot_labels <- get_plot_labels(plot_kind="residual", plot_type_info=residual_type)
  
  p <- ggplot(data=case_df, mapping=aes(x=fitted_values, y=Resid_Plot_Column)) + 
    geom_point() +
    xlab(plot_labels$xlabel) + 
    ylab(plot_labels$ylabel) + 
    ggtitle(plot_labels$title)
  
  if(plot_zero_hline){
    p <- p + geom_hline(yintercept=0, linetype=zero_hline_linetype, color=zero_hline_color)
    
  }
  
  if(flag_extreme_values){
    
    extreme_rule_of_thumb <- list("externally_studentized"=case_df[abs(case_df[,"stu.res"]) >= extreme_thresh_std,], 
                                  "internally_studentized"=case_df[abs(case_df[,"sta.res"]) >= extreme_thresh_std,], 
                                  "regular"=case_df[order(abs(case_df[,"e"]), decreasing=TRUE),][1:extreme_thresh_regular,],
                                  "deleted"=case_df[order(abs(case_df[,"deleted_resids"]), decreasing=TRUE),][1:extreme_thresh_regular,])
    
    extreme_df <- as.data.frame(extreme_rule_of_thumb[[residual_type]])
    
    p <- p + geom_point(data=extreme_df, mapping=aes(x=fitted_values, y=Resid_Plot_Column), shape=8, color=extreme_value_color) +
      geom_point(data=extreme_df, mapping=aes(x=fitted_values, y=Resid_Plot_Column), color=extreme_value_color)
    
  }
  
  if(id_extreme_values){
    p <- p +       
      geom_text(data=extreme_df, mapping=aes(x=fitted_values, y=Resid_Plot_Column, label=obs_number), 
                hjust=obs_txt_hjust, vjust=obs_txt_vjust, color=extreme_value_color, size=obs_txt_size)
    
  }
  
  
  return(p)
}

plot_residual_histogram <- function(fit, dataframe=NULL, residual_type="externally_studentized", binwidth=NULL, num_bins=NULL,
                                    remove_less_than=NULL, remove_greater_than=NULL, fill_color="Pink", 
                                    outline_color="Navy", overlay_normal=TRUE, normal_linetype="solid", 
                                    normal_linecolor="black", normal_linesize=0.5){
  
  case_df <- get_residual_plot_data(fit=fit, 
                                    dataframe=dataframe,
                                    residual_type=residual_type, 
                                    remove_less_than=remove_less_than, 
                                    remove_greater_than=remove_greater_than)
  
  plot_labels <- get_plot_labels(plot_kind="residual_histogram", plot_type_info=residual_type)
  
  num_obs <- sum(!is.na(case_df$Resid_Plot_Column))
  
  p <- ggplot(data=case_df, mapping=aes(x=Resid_Plot_Column)) +
    geom_histogram(binwidth=binwidth, bins=num_bins, fill=fill_color, color=outline_color) + 
    xlab(plot_labels$xlabel) + 
    ylab(plot_labels$ylabel) + 
    ggtitle(plot_labels$title) 
  
  if(overlay_normal){
    
    binwidth <- get_binwith(binwidth=binwidth, 
                            num_bins=num_bins, 
                            current_plot=p)
    
    p <- p +stat_function(fun=function(x)
      dnorm(x, mean=mean(case_df$Resid_Plot_Column), sd=sd(case_df$Resid_Plot_Column))*binwidth*num_obs, 
      linetype=normal_linetype, 
      color=normal_linecolor, 
      size=normal_linesize)
    
  }
  
  return(p)
  
}

# PARTIAL (COMPONENT PLUS) RESIDUAL PLOT
# Before using this function, make sure the reference levels for any categorical variables are set as desired
# by performing: df <- within(df, relevel(categorical_variable, ref=desired_level)) before passing df into this function.
#
# identify_obs --> TRUE to identify all, vector to specify which observation numbers to identify
# add_pt_removal_line --> either FALSE or a vector of OBSERVATION NUMBERS for the points to remove and then 
#                         replot a regression line for.
#
#
plot_partial_residuals <- function(df, analysis_var, response_var="log_MSRP", explanatory_vars=c("Engine_HP", "Vehicle_Style"), 
                                   augmented=FALSE, identify_obs=FALSE, show_regression=TRUE, remove_less_than=NULL, 
                                   remove_greater_than=NULL, desired_factor_level=NULL, alpha=0.5, add_least_squares=TRUE,
                                   add_smooth_fit=FALSE, smooth_method="loess", ls_linecolor="red", ls_linetype="solid", 
                                   smooth_linecolor="#B026FF", smooth_linetype="dashed", ls_showlegend=TRUE, obs_txt_color="red", 
                                   obs_txt_vjust=-0.4,obs_txt_hjust=0, obs_txt_size=3, add_pt_removal_line=FALSE, 
                                   removed_pt_color="#30D5C8", removed_pt_showlegend=TRUE, removed_pt_linetype="solid", 
                                   id_removal_compare_pts=TRUE, shade_by_case=NULL){
  
  
  if(check_datatypes(df=df, analysis_var=analysis_var) && is.null(desired_factor_level)){
    return("Invalid datatypes error")
  }
  
  # Add observation number column
  df <- add_obs_number_column(df)
  
  # If the variable we want to plot partial resids for is a categorical variable, each level will have its own coef in the
  # regression model. This section is used to update the analysis_vars name to the one that will be given to its coef in the
  # lm function, which will be the simple concatenation of variable_namelevel_name
  if(!is.null(desired_factor_level)){
    analysis_var <- str_c(analysis_var, desired_factor_level)
  }
  
  # Get plot title and axis labels
  plot_labels <- get_plot_labels(plot_kind="partial_residual", plot_type_info=analysis_var, extra_info=augmented)
  
  if(augmented){
    augmented_data <- get_augmented_data(df=df, analysis_var=analysis_var, explanatory_vars=explanatory_vars)
    df <- augmented_data[["data_frame"]]
    analysis_var <- augmented_data[["analysis_variables"]]
    explanatory_vars <- augmented_data[["explanatory_variables"]]
    
  }
  
  
  fit <- build_lm_from_strings(df=df,response_var=response_var, explanatory_vars=explanatory_vars)
  
  df <- compute_partial_residuals(df=df, fit=fit, analysis_var=analysis_var)
  
  df <- add_shading_variable(fit=fit, df=df, shade_by_case=shade_by_case)
  
  p <- ggplot() +
    geom_point(data=df, mapping=aes(x=analysis_variable, y=partial_resid,  color=shade_by_case), alpha=alpha) +
    xlab(plot_labels$xlabel) + 
    ylab(plot_labels$ylabel) + 
    ggtitle(plot_labels$title) 
  
  p <- add_legend_data(p, shade_var=shade_by_case, shape_var=NULL)
  
  # If we want to plot a least squares line through the data
  p <- add_least_squares_line(p=p, df=df, x_var="analysis_variable", y_var="partial_resid", linecolor=ls_linecolor, 
                              linetype=ls_linetype,show_legend=ls_showlegend, add_least_squares=add_least_squares)
  
  # If we want points to point their observation number, for investigative purposes
  p <- add_obs_numbers(p=p, df=df, obs_txt_color=obs_txt_color, obs_txt_size=obs_txt_size, obs_txt_vjust=obs_txt_vjust, 
                       obs_txt_hjust=obs_txt_hjust, identify_obs=identify_obs)
  
  p <- add_point_removal_comparison_line(p=p, df=df, x_var="analysis_variable", y_var="partial_resid", 
                                         linecolor=removed_pt_color, linetype=removed_pt_linetype, 
                                         show_legend=removed_pt_showlegend,add_pt_removal_line=add_pt_removal_line,
                                         id_removal_compare_pts=id_removal_compare_pts,obs_txt_size=obs_txt_size, 
                                         obs_txt_hjust=obs_txt_hjust, obs_txt_vjust=obs_txt_vjust, obs_txt_color=removed_pt_color)
  
  if(add_smooth_fit){
    p <- p + 
      geom_smooth(data=df, mapping=aes(x=analysis_variable, y=partial_resid),
                  method=smooth_method, color=smooth_linecolor)
    
  }
  
  return(p)
  
}

# This function is intended to be used with the following case statistics:
# 1) DFFITS, 2) Cooks D, 3) Leverage, 4) Externally Studentized Residual, 5) Deleted Standard Deviation
#
# NOTE: rot abbreviates "rule of thumb" (indicates the value is the rule of thumb)
#       rotm abbreviates "rule of thumb multiplier" indicates the value is a multiplier in an equation
#       that creates the rule of thumb.
#
plot_case_stat_vs_obs <- function(fit, dataframe=NULL, case_stat="cook", remove_less_than=NULL, remove_greater_than=NULL, 
                                  cook_rot=1, dffit_rotm=2, leverage_rotm=3, resid_rot=3, std_rotm=0.05,
                                  ref_linecolor="red", ref_linetype="dashed", annot_reflines=TRUE, 
                                  plot_rot_reflines=TRUE, alpha=0.3, flag_extreme=TRUE, max_flagged=5, 
                                  extreme_value_shape=8, extreme_value_color="red", obs_txt_size=3, 
                                  obs_txt_vjust=-0.4, obs_txt_hjust=0){
  
  case_stat_short_name <- case_stat_name_map(case_stat, get_short_name=TRUE)
  
  case_df <- get_residual_plot_data(fit=fit, 
                                    dataframe=dataframe,
                                    residual_type=case_stat_short_name, 
                                    remove_less_than=remove_less_than, 
                                    remove_greater_than=remove_greater_than)
  
  
  plot_labels <- get_plot_labels(plot_kind="case_stat_vs_obs", plot_type_info=case_stat)
  
  p <- ggplot(data=case_df) + 
    geom_point(mapping=aes(x=obs_number, y=Resid_Plot_Column), alpha=alpha) +
    xlab(plot_labels$xlabel) + 
    ylab(plot_labels$ylabel) + 
    ggtitle(plot_labels$title) 
  
  
  p <- plot_rule_of_thumb(fit=fit, case_df=case_df, case_stat=case_stat_short_name, cook_rot=cook_rot, 
                          dffit_rotm=dffit_rotm, resid_rot=resid_rot, std_rotm=std_rotm, leverage_rotm=leverage_rotm,
                          p=p, ref_linecolor=ref_linecolor, ref_linetype=ref_linetype, annot_reflines=annot_reflines, 
                          plot_rot_reflines=plot_rot_reflines, flag_extreme=flag_extreme, max_flagged=max_flagged, 
                          extreme_value_shape=extreme_value_shape, extreme_value_color=extreme_value_color, obs_txt_size=obs_txt_size, 
                          obs_txt_vjust=obs_txt_vjust, obs_txt_hjust=obs_txt_hjust)
  
  return(p)
  
}


plot_residual_vs_leverage <- function(fit, dataframe=NULL, residual_type="externally_studentized", remove_less_than=NULL, 
                                      remove_greater_than=NULL, add_reference_lines=TRUE, 
                                      leverage_line_multiplier=3, resid_line_threshold=2, reference_linetype="dashed",
                                      reference_linecolor="red", annotate_thresholds=TRUE, flag_extreme_obs=TRUE,
                                      max_points_flagged=4, show_all_obs_numbers=FALSE, extreme_value_color="red", 
                                      show_extreme_obs_numbers=TRUE, obs_txt_size=3, obs_txt_vjust=-0.4, obs_txt_hjust=0) {
  
  case_df <- get_residual_plot_data(fit=fit, 
                                    dataframe=dataframe,
                                    residual_type=residual_type, 
                                    remove_less_than=remove_less_than, 
                                    remove_greater_than=remove_greater_than)
  
  plot_labels <- get_plot_labels(plot_kind="residual_vs_leverage", plot_type_info=residual_type)
  
  p <- ggplot(data=case_df, mapping=aes(x=h, y=Resid_Plot_Column)) +
    geom_point() + 
    xlab(plot_labels$xlabel) + 
    ylab(plot_labels$ylabel) + 
    ggtitle(plot_labels$title) 
  
  p <- add_reference_lines(fit=fit, residual_type=residual_type, case_df=case_df, add_reference_lines=add_reference_lines,
                           leverage_line_multiplier=leverage_line_multiplier, p=p, resid_line_threshold=resid_line_threshold,
                           reference_linetype=reference_linetype, reference_linecolor=reference_linecolor, 
                           annotate_thresholds=annotate_thresholds)
  
  p <- flag_extreme_observations(residual_type=residual_type, extreme_value_color=extreme_value_color, fit=fit, p=p, 
                                 show_extreme_obs_numbers=show_extreme_obs_numbers, max_points_flagged=max_points_flagged, 
                                 resid_line_threshold=resid_line_threshold, case_df=case_df, obs_txt_size=obs_txt_size,
                                 obs_txt_vjust=obs_txt_vjust, obs_txt_hjust=obs_txt_hjust, flag_extreme_obs=flag_extreme_obs, 
                                 leverage_line_multiplier=leverage_line_multiplier)
  
  return(p)
  
}

# there are three valid qq_linetypes: 1) "robust" (quartiles), 
#                                     2) "0-1" (intercept zero, slope 1), 3) "least squares"
#
#
plot_residual_qq <- function(fit, dataframe=NULL, residual_type="externally_studentized", distribution="norm", param_list=list(mean=0, sd=1), 
                             estimate_params=FALSE, plot_type="Q-Q", add_line=TRUE, qq_linetype="robust", 
                             duplicate_points_method="standard", points_color="#003f5c", line_color="#ffa600", 
                             linetype="solid", round_digits=5, flag_largest_resid=TRUE, flag_nlargest=3, remove_less_than=NULL, 
                             remove_greater_than=NULL, flag_color_resid="red", flag_marker_shape=8, alpha=1, flag_txt_hjust=0, 
                             flag_txt_vjust=-0.3, flag_txt_size=4){
  
  
  case_df <- get_residual_plot_data(fit=fit, 
                                    dataframe=dataframe,
                                    residual_type=residual_type, 
                                    remove_less_than=remove_less_than, 
                                    remove_greater_than=remove_greater_than)
  
  
  
  plot_labels <- get_plot_labels(plot_kind="residual_qq", plot_type_info=residual_type)
  
  
  resid_column <- setNames(case_df[,"Resid_Plot_Column"], case_df[,"obs_number"])
  
  # Get the QQPlot data
  qq <- EnvStats::qqPlot(x=resid_column, plot.type=plot_type, qq.line.type=qq_linetype, 
                         add.line=add_line, param.list=param_list, duplicate.points.method=duplicate_points_method, 
                         estimate.params=estimate_params, distribution=distribution)
  
  
  # Get intercept and slope of line through 1st and third quartiles.
  line_params <- get_quartile_line_params(qq)
  
  # (theoretical, observed) pairs
  plot_df = data.frame(x_values=qq$x, y_values=qq$y, obs_number=as.numeric(names(qq$y)))
  
  p <- ggplot(data=case_df) + 
    geom_point(data=plot_df,mapping=aes(x=x_values, y=y_values), color=points_color, alpha=alpha) + 
    geom_abline(slope=line_params$slope, intercept=line_params$intercept, color=line_color,linetype=linetype) + 
    xlab(plot_labels$xlabel) + 
    ylab(plot_labels$ylabel) + 
    ggtitle(plot_labels$title)
  
  p <- flag_n_largest(qq_df=plot_df, case_df=case_df, p=p, flag_largest_resid=flag_largest_resid, flag_nlargest=flag_nlargest, 
                      flag_color_resid=flag_color_resid, flag_marker_shape=flag_marker_shape, flag_txt_hjust=flag_txt_hjust,
                      flag_txt_vjust=flag_txt_vjust,flag_txt_size=flag_txt_size)
  
  
  return(p)
  
}

############################################# END PLOTTING FUNCTIONS #################################################





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
  
  
  # CREATE BINARY COLUMNS FOR THE UNIQUE MARKET CATEGORIES, THIS WAY THE ACTUAL UNIQUE
  # CATEGORIES GET THERE ONLY COLUMN, RATHER THAN EACH UNIQUE COMBINATION THAT SHOWS UP
  df <- market_categories_to_binary(df=df)
  names(df) <- gsub(" ", "_", names(df))   # Fix the names since there are new columns now
  
  
  ### Ensure datatypes match the assignments data dictionary
  df <- set_datatypes(df)
  
  # ADD A LOG_MSRP COLUMN!
  df[,"log_MSRP"] <- log(df[,"MSRP"])
  
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



run_best_subset_selection <- function(train_data, val_data, test_data, candidate_predictors, response_variable="MSRP",
                                      save_every=1000, save_path="./model_checkpoints/", base_save_name="project1_models",
                                      order_column="val_rmse", filter_combinations=TRUE, reverse_order=FALSE){
  
  # Generate all possible predictor combinations
  predictor_combinations <- get_predictor_combos_manual(features=candidate_predictors)
  
  # Filter the combinations to remove any that include a squared term without including the 
  # non-squared version of that same term... usually that is a good rule of thumb.
  if(filter_combinations){
    predictor_combinations <- filter_predictor_squared_terms(candidate_combinations = predictor_combinations)  
  }

  # Reverse the predictor combinations if desired
  if(reverse_order){
    predictor_combinations <- rev(predictor_combinations)
  }
  
    
  for(combo_index in 1:(length(predictor_combinations) %/% 2)){
    
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
                                            train_data=train_data,
                                            val_data=val_data, 
                                            test_data=test_data,
                                            response_variable=response_variable,
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
      
      #final_metrics <- final_metrics[order(final_metrics[,order_column]),]
      save_file_name <- paste(base_save_name, "_iteration_", combo_index, ".csv")
      full_save_path <- gsub(x=paste(save_path, save_file_name),
                             pattern=" ",
                             replacement="",
                             fixed = TRUE)
      
      write.csv(x=final_metrics, file=full_save_path, row.names=FALSE)
    }
  }
  
  # Final Save 
  #final_metrics <- final_metrics[order(final_metrics[,order_column]),]
  save_file_name <- paste(base_save_name, "_FINAL.csv")
  full_save_path <- gsub(x=paste(save_path, save_file_name),
                         pattern=" ",
                         replacement="",
                         fixed=TRUE)
  
  write.csv(x=final_metrics, file=full_save_path, row.names=FALSE)
  
  return(final_metrics)
}


add_squared_terms <- function(df, terms){
  
  for(term_index in 1:length(terms)){
    
    term_name <- terms[term_index]
    
    squared_term_name <- gsub(x=paste(term_name, "_Squared"),
                              pattern=" ",
                              replacement="")
    
    df[,squared_term_name] <- df[,term_name]^2
    
  }
  
  return(df)
}

############################################# END MODELING FUNCTIONS #################################################




############################################# MODEL PERFORMANCE ANALYSIS FUNCTIONS #################################################

consolidate_best_metric_names <- function(df){
  
  # if there is only a single row, nothing to consolidate, so head out of the function.
  if(nrow(df) == 1){
    return(df)
  }
  
  # Get all the names of the metrics in the best_per_metric column (this is what
  # we are going to consolidate, so for the models that were the best per multiple metrics
  # we show all the metrics they ranked #1 in on a single line)
  metric_names <- df[,"best_per_metric"]
  
  # Update the first row (arbitrary since all rows are the same other than the metric name), to
  # contain all of the metric names
  df[1, "best_per_metric"] <- stringr::str_c(metric_names, collapse="  ")
  
  # Discard all rows except the first
  unique_row <- df[1,]
  
  return(unique_row)
}


filter_best_models_by_example <- function(df, row_example, filter_columns){
  
  
  for(column_index in 1:length(filter_columns)){
    
    column_name <- filter_columns[column_index]
    
    this_filter <- (df[,column_name] == row_example[,column_name])
    
    if(column_index == 1){
      full_filter <- this_filter
    }else{
      full_filter <- full_filter & this_filter
    }
  }
  
  filtered_df <- df[full_filter,]
  
  return(filtered_df)
}

consolidate_best_models_df <- function(best_df){
  
  # For "best models" that are the same across multiple metrics, grab one row of each
  unique_df <- best_df[!duplicated(best_df[,!(names(best_df) %in% "best_per_metric")]),]
  
  # Grab duplicate versions of the "best models" (same best model, just per a different metric)
  duplicated_df <- best_df[duplicated(best_df[,!(names(best_df) %in% "best_per_metric")]),]
  
  # If the same best model was never chosen for any of the metrics, nothing to do here. 
  if(nrow(duplicated_df) == 0){
    return(unique_df)
  }
  
  # Create a vector of column names to filter on. It will contain all the column names, except best_per_metric
  filter_column_names <- names(best_df)
  filter_column_names <- filter_column_names[filter_column_names != "best_per_metric"]
  
  
  for(row_index in 1:nrow(unique_df)){
    
    this_row <- unique_df[row_index,]
    
    matching_rows <- filter_best_models_by_example(df=best_df, 
                                                   row_example=this_row, 
                                                   filter_columns=filter_column_names)
    
    
    consolidated_row <- consolidate_best_metric_names(df=matching_rows)
    
    # Update the final consolidated dataframe, or create it if this is the first row being consolidated
    if(row_index == 1){
      consolidated_df <- consolidated_row
    }
    else{
      consolidated_df <- rbind(consolidated_df, consolidated_row)
    }
  }
  
  return(consolidated_df)
}


subset_dataframe_by_metric <- function(metric_df, metric_name){
  
  # Subset the dataframe to get the rows with the best models, per the metric specified in metric_name
  #
  # If the metric is a "the lower the better" metric
  if(metric_name %in% c("val_mae", "val_rmse", "rmse","aic", "apc", "fpe", "hsp", "msep",
                        "sbc_SAS", "sbc_R", "press", "train_rmse", "train_mae")){
    best_df <- metric_df[metric_df[,metric_name] == min(metric_df[,metric_name]),]
    
    # Else if the metric is a "the larger the better" metric
  }else if(metric_name %in% c("pred_rsq", "val_rsq", "rsq", "adj_rsq")){ 
    
    best_df <- metric_df[metric_df[,metric_name] == max(metric_df[,metric_name]),]
    
  }  
  
  return(best_df)
  
}


#####
get_best_models_df <- function(metric_df, consolidate=TRUE){
  
  all_metric_names <- c("val_mae", "val_rmse", "rmse","aic", "apc", "fpe", "hsp", "msep", "sbc_SAS", 
                        "sbc_R", "press", "pred_rsq", "val_rsq", "rsq", "adj_rsq", "train_rmse", "train_mae")
  
  
  for(metric_index in 1:length(all_metric_names)){
    
    metric_name <- all_metric_names[metric_index]
    
    best_models <- subset_dataframe_by_metric(metric_df=metric_df, metric_name=metric_name)
    
    best_models[,"best_per_metric"] <- metric_name
    
    if(metric_index == 1){
      final_df <- best_models
    }else{
      final_df <- rbind(final_df, best_models)
    }
    
  }
  
  # If we want to consolidate, so models that were deemed "best" on multiple criteria only get 
  # a single row in the dataframe, with all the metrics they were best at listed within that row 
  # under the "best_per_metric" column
  if(consolidate){
    final_df <- consolidate_best_models_df(best_df=final_df)  
  }
  
  
  return(final_df)
}


get_best_predictors_by_metric <- function(metric_df, metric_name){
  
  # Grab the rows that have the best models, per this metric
  predictors_df <- subset_dataframe_by_metric(metric_df=metric_df, metric_name=metric_name)
  
  # Grab the first set of predictors (will be the only unless ties)
  predictors <- predictors_df[,"predictors"][1]
  
  # Split the string at the double spaces to create a vector of predictors, we will use this
  # to build the formula that recreates the model
  predictor_vector <- stringr::str_split(string=predictors, pattern="  ")[[1]]
  
  return(predictor_vector)
}


get_best_model_by_metric <- function(metric_df, training_df, metric_name, response_variable="log_MSRP"){
  
  
  # Get a vector containing the best predictors, per this metric
  predictor_vector <- get_best_predictors_by_metric(metric_df=metric_df, 
                                                    metric_name=metric_name)
  
  # Create the formula to fit a model using these predictors
  lm_formula <- as.formula(paste0(response_variable, "~", stringr::str_c(predictor_vector, 
                                                                         collapse=" + ")))
  
  # Fit the model
  fit <- lm(formula=lm_formula, 
            data=training_df)
  
  
  return(fit)
}

############################################# END MODEL PERFORMANCE ANALYSIS FUNCTIONS #################################################





########################################### GENERAL UTILITY ######################################################

partial_f_test <- function(full_model, reduced_model){
  
  partial_f_test_result <- anova(reduced_model, full_model)
  return(partial_f_test_result)
  
}

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
# could modify to allow a threshold for number of missings
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

########################################### END GENERAL UTILITY #######################################################




