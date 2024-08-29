

## From Dina's functions: ##
#====================================================================================================

#Libraries

 library(pacman)

 p_load(tableone,knitr,pander,tibble,survival,
        forestplot,packHV,DescTools,descr,stargazer,
        MASS,ModelGood,rms,diagram,Matching,gridExtra,
        plotrix,epitools,epade,plot3D,cmprsk,jpeg,pROC,
        kableExtra,formattable,haven,readxl,plyr,dplyr,tinytex,
        Greg,cmprsk, mice, corrplot, verification,lubridate,
        parsedate,caret,ggplot2,reshape2,survminer,sqldf,
        naniar,lmtest,magrittr,epiR,UncertainInterval 
        ,visreg, smoothHR
        ,mstate
        ,dataPreparation
        ,lcmm
        ,flexsurv
        ,labelled
        ,gt 
        ,gtsummary
        ,coin
        # ,exactRankTests #use coin instead
        #19.4.22:
        ,JM
        ,PMCMRplus
        ,iMRMC
        ,VennDiagram
        
        )

#Missing data functions

nmis <- function(x) {sum(is.na(x))}

mean2 <- function(x) {mean(x,na.rm = T)}

#~~~~Filling missing data
fillna0 <- function(X) { X[is.na(X)] <- "NO"; X}
fillnaMean <- function(X) { M <- mean(X,na.rm=TRUE); X[is.na(X)] <- M; X}
fillnax <- function(X,default.value) { X[is.na(X)] <- default.value ; X}
value_YES_missing_NO <-  function(X) { factor(ifelse(!is.na(X), "available", "missing"), levels = c("missing","available"))}
# fill NA for all kinds of classes:
fillna_all <- function(vars_list, data) {
  
  vars_list_fill <- paste0(vars_list, "_fill")
  data[,vars_list_fill] <- data[,vars_list]
  
  for (i in 1:length(vars_list)) {
    if (class(data[,vars_list_fill[i]]) %in% c("numeric","integer")) {
      data[,vars_list_fill[i]] <- fillnaMean(data[,vars_list_fill[i]]) 
    } else {
      # if (levels(data[,vars_list_fill[i]]) %in% c("0","1")) {data[,vars_list_fill[i]] <- fillna0(data[,vars_list_fill[i]])}
      data[,vars_list_fill[i]] <- fillnax(X =  data[,vars_list_fill[i]], default.value = levels(data[,vars_list_fill[i]])[1])
      
    }
  }
  return(data)
}

# Missing vector for table 1 for variables with more than 2 categories
nmisT <- function(x){
  levc <- length(levels(x))
  if(levc>2)  ret <- c(nmis(x),rep("",levc)) else ret <- nmis(x)
}

nmissing <- function(vars,data){
  t <- c()
  for (i in 1:length(vars)){
    t[(length(t)+1):(length(t)+length(nmisT(data[,vars[i]])))] <- nmisT(data[,vars[i]])
  }
  return(c("",t))
}

min_val <- function(vars,data){
  t <- c()
  for (i in 1:length(vars)){
    if (class(data[,vars[i]]) %in% c("numeric","integer"))  {
    t[(length(t)+1):(length(t)+length(nmisT(data[,vars[i]])))] <- round(min2(data[,vars[i]]),2)
    }
    else {t[(length(t)+1):(length(t)+length(nmisT(data[,vars[i]])))] <- ""}
      
  }
  return(c("",t))
}

max_val <- function(vars,data){
  t <- c()
  for (i in 1:length(vars)){
    if (class(data[,vars[i]]) %in% c("numeric","integer"))  {
      t[(length(t)+1):(length(t)+length(nmisT(data[,vars[i]])))] <- round(max2(data[,vars[i]]),2)
    }
    else {t[(length(t)+1):(length(t)+length(nmisT(data[,vars[i]])))] <- ""}
    
  }
  return(c("",t))
}

# Effect size:
effect_size_calc <- function(vars_to_calc, groups, data) {
  dat_1 <- data[data[[groups]]==levels(data[[groups]])[1],]
  dat_2 <- data[data[[groups]]==levels(data[[groups]])[2],]
  mean_1 <- mean2(dat_1[[vars_to_calc]])
  mean_2 <- mean2(dat_2[[vars_to_calc]])
  sd_1 <- sd2(dat_1[[vars_to_calc]])
  sd_2 <- sd2(dat_2[[vars_to_calc]])
  sd_pooled <- sqrt(((sd_1^2)+(sd_2^2))/2)
  effect_size <- (mean_2 - mean_1) / sd_pooled
  
  return(round(effect_size,2))
}

# Effect size for tableone:
effect_size_for_table <- function(vars,data_effect, groups_effect=strata_for_table) {
  t <- c()
  for (i in 1:length(vars)){
    if (class(data_effect[,vars[i]]) %in% c("numeric","integer"))  {
      t[(length(t)+1):(length(t)+length(nmisT(data_effect[,vars[i]])))] <- effect_size_calc(data=data_effect, vars_to_calc = vars[i],groups = groups_effect )
    }
    else {t[(length(t)+1):(length(t)+length(nmisT(data_effect[,vars[i]])))] <- ""}
    
  }
  return(c("",t))
}

# Form a number to fit a p-value notation (if 0 then make it <0.001):
pval_form <- function(x) {
  x <- ifelse(as.numeric(formatC(x, format = "f"))<0.001, "<0.001", round(as.numeric(formatC(x, format = "f")),3)) 
  return(x)}

#~~~~ Find the number between parenthesis (useful to extract the percentage from tableone):
out_of_par <- function(x) {as.numeric(substr(x,
                                             regexpr("(",x, fixed = T)[1]+1,
                                             regexpr(")",x, fixed = T)[1]-1))}

#~~~~ Find the number which is not in parenthesis (useful to extract the numbers from tableone):
number_from_tableone <- function(x) {as.numeric(substr(x,
                                             1,           
                                             regexpr("(",x, fixed = T)[1]-1))}


#===============#
#TABLEONE
#==============#
table_one_no_strata <- function(variables, data_for_table, factor_variables, non_normal_variables, dictionary, 
                                missing = TRUE, MINIMUM_MAXIMUM = TRUE) {
  
  # Table with no strata:
  tab1 <- print(CreateTableOne(vars = variables,
                               factorVars = factor_variables,
                               data = data_for_table
                               
  ), nonnormal = non_normal_variables, printToggle=F)
  
  if (missing == TRUE) {
   
    tab1 <-  print(CreateTableOne(vars = variables,
                                  factorVars = factor_variables,
                                  data = data_for_table
                                  
    ), nonnormal = non_normal_variables, printToggle=F, missing=T)
  }
  
  if (MINIMUM_MAXIMUM == TRUE) {
    
    min_values <- min_val(data = data_for_table, vars = variables)
    tab1 <- cbind(tab1, min_values)
    
    max_values <- max_val(data = data_for_table, vars = variables)
    tab1 <- cbind(tab1, max_values)
  }
  
  rownames(tab1) <- plyr::revalue(rownames(tab1),
                                  replace = dictionary,
                                  warn_missing = F)
  
  #find rows with >20% missing values:
  tab1_new <-  print(CreateTableOne(vars = variables,
                                    factorVars = factor_variables,
                                    data = data_for_table  ), nonnormal = non_normal_variables, printToggle=F)[,-4]
  Missing  <- nmissing(data = data_for_table,
                       vars = variables)
  tab1_new <- cbind(tab1_new, Missing)
  tab1_new <- as.data.frame(tab1_new)
  tab1_new <- tab1_new %>% dplyr::mutate(row_numbers = row_number(),
                                         Missing_many = case_when(as.numeric(as.character(Missing))>dim(data_for_table)[1]/5  ~ 1, TRUE ~ 0)
                                         
  )
  NA_rows_to_color <- tab1_new[["row_numbers"]][tab1_new[["Missing_many"]]==1 & !is.na(tab1_new[["Missing_many"]])]
  
  
  return(list(tab1 = tab1, NA_rows_to_color=NA_rows_to_color))
  
}

#=======================================#
table_one_strata2groups <- function(variables, strata_for_table, data_for_table, factor_variables, non_normal_variables, dictionary, 
                                    missing_all = "NO", missing_stratified=TRUE, MINIMUM_MAXIMUM = TRUE, effect_size = TRUE) {
  
  # Table with strata:
  tab1 <- print(CreateTableOne(vars = variables, strata = strata_for_table,
                               factorVars = factor_variables,
                               data = data_for_table  ), nonnormal = non_normal_variables, printToggle=F)[,-4]
  
  if (missing_all == TRUE) {
    Missing  <- nmissing(data = data_for_table,
                         vars = variables)
    tab1 <- cbind(tab1, Missing)
  }
  
  if (missing_stratified == TRUE) {
    #missing in group1
    Missing_group1  <- nmissing(data = data_for_table[which(data_for_table[[strata_for_table]]==levels(data_for_table[[strata_for_table]])[1]),]  ,
                                vars = variables)
    #missing in group2
    Missing_group2  <- nmissing(data = data_for_table[which(data_for_table[[strata_for_table]]==levels(data_for_table[[strata_for_table]])[2]),],
                                vars = variables)
    
    tab1 <- cbind(tab1,Missing_group1, Missing_group2)
  }
  
  if (MINIMUM_MAXIMUM == TRUE) {
    
    min_values <- min_val(data = data_for_table, vars = variables)
    tab1 <- cbind(tab1, min_values)
    
    max_values <- max_val(data = data_for_table, vars = variables)
    tab1 <- cbind(tab1, max_values)
  }
  
  if (effect_size == TRUE) {

    effect_size <- effect_size_for_table(vars = variables, data = data_for_table, groups = strata_for_table)
    tab1 <- cbind(tab1, effect_size)
  }
  
  
  rownames(tab1) <- plyr::revalue(rownames(tab1),
                                  replace = dictionary,
                                  warn_missing = F)
  
  #find rows with >20% missing values:
  tab1_new <-  print(CreateTableOne(vars = variables, strata = strata_for_table,
                                    factorVars = factor_variables,
                                    data = data_for_table  ), nonnormal = non_normal_variables, printToggle=F)[,-4]
  Missing  <- nmissing(data = data_for_table,
                       vars = variables)
  tab1_new <- cbind(tab1_new, Missing)
  tab1_new <- as.data.frame(tab1_new)
  tab1_new <- tab1_new %>% dplyr::mutate(row_numbers = row_number(),
                                         Missing_many = case_when(as.numeric(as.character(Missing))>dim(data_for_table)[1]/5  ~ 1, TRUE ~ 0)
                                         
  )
  NA_rows_to_color <- tab1_new[["row_numbers"]][tab1_new[["Missing_many"]]==1 & !is.na(tab1_new[["Missing_many"]])]
  
  
  #find significant rows:
  sig_rows <- tab1_new[["row_numbers"]][(as.numeric(tab1_new[["p"]])<0.05 | tab1_new[["p"]]%in%"<0.001") & !is.na(tab1_new[["p"]])] #warning "NAs introduced by coercion" is fine
  sig_rows <- sig_rows[!is.na(sig_rows)]
  sig_rows <- as.numeric(sig_rows) #warning "NAs introduced by coercion" is fine
  
  # tab1 <- as.data.frame(tab1)
  # tab1 <- tab1 %>%
  #   mutate(
  #     p = cell_spec(p, "html", bold = ifelse((as.numeric(p)<0.05 | p%in%"<0.001") & !is.na(p), T, F))
  #   )  %>%
  #  kable(format = "html") %>%
  #   kable_styling("striped", full_width = F) %>%
  #   row_spec(NA_rows_to_color, color = "red") 
  
  return(list(tab1 = tab1, NA_rows_to_color=NA_rows_to_color, sig_rows=sig_rows ))
  
}

#=======================================#
table_one_strata3groups <- function(variables, strata_for_table, data_for_table, factor_variables, non_normal_variables, dictionary, 
                                    missing_all = "NO", missing_stratified=TRUE, MINIMUM_MAXIMUM = TRUE,p_forTrend=TRUE) {
  
  # Table with strata:
  tab1 <- print(CreateTableOne(vars = variables, strata = strata_for_table,
                               factorVars = factor_variables,
                               data = data_for_table  ), nonnormal = non_normal_variables, printToggle=F)[,-5]
  
  if (missing_all == TRUE) {
    Missing  <- nmissing(data = data_for_table,
                         vars = variables)
    tab1 <- cbind(tab1, Missing)
    colnames(tab1)[5] <- "Missing" 
  }
  
  if (missing_stratified == TRUE) {
    #missing in group1
    Missing_group1  <- nmissing(data = data_for_table[which(data_for_table[[strata_for_table]]==levels(data_for_table[[strata_for_table]])[1]),]  ,
                                vars = variables)
    #missing in group2
    Missing_group2  <- nmissing(data = data_for_table[which(data_for_table[[strata_for_table]]==levels(data_for_table[[strata_for_table]])[2]),],
                                vars = variables)
    
    #missing in group3
    Missing_group3  <- nmissing(data = data_for_table[which(data_for_table[[strata_for_table]]==levels(data_for_table[[strata_for_table]])[3]),],
                                vars = variables)
    
    tab1 <- cbind(tab1,Missing_group1, Missing_group2, Missing_group3)
  }
  
  if (MINIMUM_MAXIMUM == TRUE) {
    
    min_values <- min_val(data = data_for_table, vars = variables)
    tab1 <- cbind(tab1, min_values)
    
    max_values <- max_val(data = data_for_table, vars = variables)
    tab1 <- cbind(tab1, max_values)
  }
  
  # if (effect_size == TRUE) {
  #   
  #   effect_size <- effect_size_for_table(vars = variables, data = data_for_table, groups = strata_for_table)
  #   tab1 <- cbind(tab1, effect_size)
  # }
  # 
  
  if (p_forTrend == TRUE) {
    
    p_Trend <- p_for_trend(vars = variables, data = data_for_table[1:5000,], strata = strata_for_table)
    tab1 <- cbind(tab1[,-4], p_Trend)
    
  }
  
  rownames(tab1) <- plyr::revalue(rownames(tab1),
                                  replace = dictionary,
                                  warn_missing = F)
  
  #find rows with >20% missing values:
  tab1_new <-  print(CreateTableOne(vars = variables, strata = strata_for_table,
                                    factorVars = factor_variables,
                                    data = data_for_table  ), nonnormal = non_normal_variables, printToggle=F)[,-4]
  Missing  <- nmissing(data = data_for_table,
                       vars = variables)
  tab1_new <- cbind(tab1_new, Missing)
  tab1_new <- as.data.frame(tab1_new)
  tab1_new <- tab1_new %>% dplyr::mutate(row_numbers = row_number(),
                                         Missing_many = case_when(as.numeric(as.character(Missing))>dim(data_for_table)[1]/5  ~ 1, TRUE ~ 0)
                                         
  )
  NA_rows_to_color <- tab1_new[["row_numbers"]][tab1_new[["Missing_many"]]==1 & !is.na(tab1_new[["Missing_many"]])]
  
  
  #find significant rows:
  sig_rows <- tab1_new[["row_numbers"]][(as.numeric(tab1_new[["p"]])<0.05 | tab1_new[["p"]]%in%"<0.001") & !is.na(tab1_new[["p"]])] #warning "NAs introduced by coercion" is fine
  sig_rows <- sig_rows[!is.na(sig_rows)]
  sig_rows <- as.numeric(sig_rows) #warning "NAs introduced by coercion" is fine
  
  # tab1 <- as.data.frame(tab1)
  # tab1 <- tab1 %>%
  #   mutate(
  #     p = cell_spec(p, "html", bold = ifelse((as.numeric(p)<0.05 | p%in%"<0.001") & !is.na(p), T, F))
  #   )  %>%
  #  kable(format = "html") %>%
  #   kable_styling("striped", full_width = F) %>%
  #   row_spec(NA_rows_to_color, color = "red") 
  
  return(list(tab1 = tab1, NA_rows_to_color=NA_rows_to_color, sig_rows=sig_rows ))
  
}


#=======================================#
table_one_strata4groups <- function(variables, strata_for_table, data_for_table, factor_variables, non_normal_variables, dictionary, 
                                    missing_all = "NO", missing_stratified=TRUE, MINIMUM_MAXIMUM = TRUE, p_forTrend=TRUE) {
  
  # Table with strata:
  tab1 <- print(CreateTableOne(vars = variables, strata = strata_for_table,
                               factorVars = factor_variables,
                               data = data_for_table  ), nonnormal = non_normal_variables, printToggle=F)[,-6]
  
  if (missing_all == TRUE) {
    Missing  <- nmissing(data = data_for_table,
                         vars = variables)
    tab1 <- cbind(tab1, Missing)
  }
  
  if (missing_stratified == TRUE) {
    #missing in group1
    Missing_group1  <- nmissing(data = data_for_table[which(data_for_table[[strata_for_table]]==levels(data_for_table[[strata_for_table]])[1]),]  ,
                                vars = variables)
    #missing in group2
    Missing_group2  <- nmissing(data = data_for_table[which(data_for_table[[strata_for_table]]==levels(data_for_table[[strata_for_table]])[2]),],
                                vars = variables)
    
    #missing in group3
    Missing_group3  <- nmissing(data = data_for_table[which(data_for_table[[strata_for_table]]==levels(data_for_table[[strata_for_table]])[3]),],
                                vars = variables)
    
    #missing in group4
    Missing_group4  <- nmissing(data = data_for_table[which(data_for_table[[strata_for_table]]==levels(data_for_table[[strata_for_table]])[4]),],
                                vars = variables)
    
    tab1 <- cbind(tab1,Missing_group1, Missing_group2, Missing_group3,Missing_group4)
  }
  
  if (MINIMUM_MAXIMUM == TRUE) {
    
    min_values <- min_val(data = data_for_table, vars = variables)
    tab1 <- cbind(tab1, min_values)
    
    max_values <- max_val(data = data_for_table, vars = variables)
    tab1 <- cbind(tab1, max_values)
  }
  
  if (p_forTrend == TRUE) {

    p_Trend <- p_for_trend(vars = variables, data = data_for_table[1:5000,], strata = strata_for_table)
    tab1 <- cbind(tab1[,1:4], p_Trend, tab1[,6:length(colnames(tab1))])
  }

  
  rownames(tab1) <- plyr::revalue(rownames(tab1),
                                  replace = dictionary,
                                  warn_missing = F)
  
  #find rows with >20% missing values:
  tab1_new <-  print(CreateTableOne(vars = variables, strata = strata_for_table,
                                    factorVars = factor_variables,
                                    data = data_for_table  ), nonnormal = non_normal_variables, printToggle=F)[,-4]
  Missing  <- nmissing(data = data_for_table,
                       vars = variables)
  tab1_new <- cbind(tab1_new, Missing)
  tab1_new <- as.data.frame(tab1_new)
  tab1_new <- tab1_new %>% dplyr::mutate(row_numbers = row_number(),
                                         Missing_many = case_when(as.numeric(as.character(Missing))>dim(data_for_table)[1]/5  ~ 1, TRUE ~ 0)
                                         
  )
  NA_rows_to_color <- tab1_new[["row_numbers"]][tab1_new[["Missing_many"]]==1 & !is.na(tab1_new[["Missing_many"]])]
  
  
  #find significant rows:
  sig_rows <- tab1_new[["row_numbers"]][(as.numeric(tab1_new[["p"]])<0.05 | tab1_new[["p"]]%in%"<0.001") & !is.na(tab1_new[["p"]])] #warning "NAs introduced by coercion" is fine
  sig_rows <- sig_rows[!is.na(sig_rows)]
  sig_rows <- as.numeric(sig_rows) #warning "NAs introduced by coercion" is fine
  
  # tab1 <- as.data.frame(tab1)
  # tab1 <- tab1 %>%
  #   mutate(
  #     p = cell_spec(p, "html", bold = ifelse((as.numeric(p)<0.05 | p%in%"<0.001") & !is.na(p), T, F))
  #   )  %>%
  #  kable(format = "html") %>%
  #   kable_styling("striped", full_width = F) %>%
  #   row_spec(NA_rows_to_color, color = "red") 
  
  return(list(tab1 = tab1, NA_rows_to_color=NA_rows_to_color, sig_rows=sig_rows ))
  
}

#=========#
table_one_kable <- function(tab,
                              rows_to_red = F,
                              rows_to_italic = F
                              ) {
  if (rows_to_red==F & rows_to_italic==F ) {
    kable(tab, format = "html") %>%
      kable_styling(bootstrap_options = c("striped", "hover",  "condensed"), 
                    full_width = T, fixed_thead = T) %>% 
      column_spec(1, width = "25em")
  } else if (rows_to_red!=F & rows_to_italic!=F) {
    kable(tab, format = "html") %>%
      kable_styling(bootstrap_options = c("striped", "hover",  "condensed"), 
                    full_width = T, fixed_thead = T) %>% 
      row_spec(row =  rows_to_red, color = "red") %>%
      row_spec(row =  rows_to_italic, italic = T) %>% 
      column_spec(1, width = "25em") }
  
 else if (rows_to_red!=F & rows_to_italic==F) {
    kable(tab, format = "html") %>%
      kable_styling(bootstrap_options = c("striped", "hover",  "condensed"), 
                    full_width = T, fixed_thead = T) %>% 
      row_spec(row =  rows_to_red, color = "red") %>%
      column_spec(1, width = "25em")
  } else if (rows_to_red==F & rows_to_italic!=F) {
    kable(tab, format = "html") %>%
      kable_styling(bootstrap_options = c("striped", "hover",  "condensed"), 
                    full_width = T, fixed_thead = T) %>% 
      # row_spec(row =  rows_to_red, color = "red") %>%
      row_spec(row =  rows_to_italic, italic = T) %>%
      column_spec(1, width = "25em") }
 
   
}
#==========#

# p for trend: (BY DINA)
# P for trend calculated as follows: 
# chi square for factors, 
# linear regression for normally distributed continuous
# Mann-Kendall for non-normally distributed continuous
library(Kendall)
ptrend <- function(x,y=dat$SOURCE,nrow=1) {
  tabx <- addmargins(table(x, y),1)
  pval <-round(prop.trend.test(tabx[nrow,], tabx["Sum",])$p.value, 3)
  pval <- ifelse(pval<0.001,"<0.001",pval)
  return(pval)
}
trend_fac <- function(x,strata){
  levc <- length(levels(x))
  if(levc>2){  
    ret <- c()
    for(i in 1:levc){
        ret[i] <- tryCatch(ptrend(x,strata,nrow = i) , error=function(e) {NaN})
    }
    return(c("",ret))
  } else 
  ret <- tryCatch(ptrend(x,strata) , error=function(e) {NaN})
  return(ret)
}
p_for_trend <- function(vars, strata, data){
  p_for_trend <- c()
  for(i in 1:length(vars)){
    p_for_trend[(length(p_for_trend)+1):(length(p_for_trend)+length(trend_fac(data[,vars[i]], data[,strata])))] <- 
      if(is.factor(data[,vars[i]])) {
        trend_fac(x = data[,vars[i]], strata = data[,strata])
      }else if ((is.numeric(data[,vars[i]]) | is.integer(data[,vars[i]]))
                & shapiro.test(data[1:5000,vars[i]])$p.value<0.05) {
        p_value(Kendall(data[,vars[i]], data[,strata])$sl[1])
      } else {
        p_value(summary(lm(data[,vars[i]]~as.numeric(data[,strata])))$coefficients[2,"Pr(>|t|)"])
        # anova(modlm) #this is the actual test but this is the same  p value as in the lm model. when reporting, need to report AOV with 1 degree of freedom
      }
  }
  return(c("",p_for_trend))
}



# NONNORMALITY (from the internet)
shapiro_test_df <- function(df, bonf= FALSE, alpha= 0.05) {
  l <- lapply(df, shapiro.test)
  s <- do.call("c", lapply(l, "[[", 1))
  p <- do.call("c", lapply(l, "[[", 2))
  if (bonf == TRUE) {
    sig <- ifelse(p > alpha / length(l), "H0", "Ha")
  } else {
    sig <- ifelse(p > alpha, "H0", "Ha")
  }
  return(list(statistic= s,
              p.value= p,
              significance= sig,
              method= ifelse(bonf == TRUE, "Shapiro-Wilks test with Bonferroni Correction",
                             "Shapiro-Wilks test without Bonferroni Correction")))
} #https://stackoverflow.com/questions/33489330/how-to-test-the-normality-of-many-variables-in-r-at-the-same-time



trim.time.events <- function(is.event, d.to.event, t.limit = 500) {
  is.event[d.to.event > t.limit]   <- 0
  d.to.event[d.to.event > t.limit] <- t.limit
  list(events = is.event, time = d.to.event)
}

# OR (95% CI) (x=glm model) - For forest plot
OR_CI <- function(x){
  OR <- round(exp(coef(x)),2)[-1]
  CI <- round(exp(confint(x)),2)
  s1 <- summary(x)
  p.value <- ifelse(round(s1$coefficients[,"Pr(>|z|)"],3)<0.001,"<0.001",round(s1$coefficients[,"Pr(>|z|)"],3))
  list(total=paste0(OR," (",CI[-1,1],",",CI[-1,2],")"),OR=OR,CI=CI[-1,], p.value=p.value[-1])
}


# HR (95% CI) (x=Cox model)
HR_CI <- function(x){
  HR <- round(exp(coef(x)),2)
  CI <- round(exp(confint(x)),2)
  s1 <- summary(x)
  p.value <- ifelse(round(s1$coefficients[,"Pr(>|z|)"],3)<0.001,"<0.001",round(s1$coefficients[,"Pr(>|z|)"],3))
  list(total=paste0(HR," (",CI[,1],",",CI[,2],")"),HR=HR,CI=CI,p.value=p.value)
}


# Table of multiple univariable logistic regression models:
#glm_formula can be written as (dat_to_fit$MACE=="YES")
univariable_glm <- function(list_of_covariates, glm_formula, data_for_model, table_rownames) {
  table_OR <- data.frame(OR.CI = "",p= "")
  for (i in 1:length(list_of_covariates)) {
    uni_model <- glm(glm_formula ~ data_for_model[,list_of_covariates[i]] , data = data_for_model,  family = "binomial")
    list_OR <- OR_CI(uni_model)
    table_OR1 <- data.frame(OR.CI = list_OR$total,p= as.character(list_OR$p.value))
    table_OR <- rbind(table_OR, table_OR1)
  }
  
  rownames(table_OR) <- c("",table_rownames)
  return(table_OR)
  
}


# Table of multiple univariable cox regression models:
univariable_cox <- function(list_of_covariates, cox_formula, data_for_model, table_rownames) {
  table_HR <- data.frame(HR.CI = "",p= "")
  for (i in 1:length(list_of_covariates)) {
    uni_model <- coxph((cox_formula) ~ data_for_model[,list_of_covariates[i]] , data = data_for_model)
    list_HR <- HR_CI(uni_model)
    table_HR1 <- data.frame(HR.CI = list_HR$total,p= as.character(list_HR$p.value))
    table_HR <- rbind(table_HR, table_HR1)
  }
  
  rownames(table_HR) <- c("",table_rownames)
  return(table_HR)
  
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~



ptrend.mod <- function(x,y=dat.mod$SOURCE,nrow=1) {
  tabx <- addmargins(table(x, y),1)
  pval <-round(prop.trend.test(tabx[nrow,], tabx["Sum",])$p.value, 3)
  pval <- ifelse(pval<0.001,"<0.001",pval)
  return(pval)
}


#~~~~~Turning p-value<0.001 to "<0.001"
p_value <- function(any_p_value){
  p_val <- ifelse(any_p_value<0.001,"<0.001", round(any_p_value,3))
  return(p_val)
}


## Variable dictionary
# This function takes dataframe that consists of 2 columns with variable names and lables and 
# a vector of names to be converted
# Variables:
# var.dataframe: dataframe with 2 columns: 1 with variable's CRF name and another with variable's full descriptive name 
# var.names: vector of 'CRF' names to be converted
# return: vector (character) with dicriptive variables names
var.dict = function(var.dataframe, var.names){
  vec.names        = var.dataframe[,2]
  names(vec.names) = var.dataframe[,1]
  return( as.character(vec.names[var.names]) )
}



## From Michal's functions: ##
#====================================================================================================

#---Directory
wd.path2017 <- function(x) {setwd(paste0("W://Users//Shared_Files//STAT//",x))}

colx <- c("red","blue","pink","green","orange","brown","turquoise","purple","black")
colxBW=c("white","grey80","grey50","grey30","black")


psum2 <- function(...,na.rm=FALSE) {      #returns na for na
  dat <- do.call(cbind,list(...))
  res <- rowSums(dat, na.rm=na.rm) 
  idx_na <- !rowSums(!is.na(dat))
  res[idx_na] <- NA
  res 
}

ns <- function(x) {sum(!is.na(x))}

marg <- function(x){addmargins(table(x))}
marg0 <- function(x){addmargins(table(x, exclude = NULL))}
max2  <- function(x) {max(x,na.rm=TRUE)}
range2 <-function(x) {range(x,na.rm=TRUE)}


sum1  <- function(x) {(sum(x,na.rm=TRUE)>0)*1}                # if sum is >0

tert2 <- function(x,y=c(.33, .66, 1.),...) {quantile(x, y ,na.rm=T)}

p25 <- function(x){ quantile(x, probs = 0.25, na.rm = TRUE)}
p50 <- function(x){ quantile(x, probs = 0.5, na.rm = TRUE)}
p75 <- function(x){ quantile(x, probs = 0.75, na.rm = TRUE)}

minrow <- function(x) {apply(as.data.frame(cbind(x),1,min2))} #	min of rows enter as x is c(v1,v2,..vk)

sumrow <- function(x) {apply(as.data.frame(cbind(x),1,FUN=sum2))} #	sum of rows enter as x is c(v1,v2,..vk)

attr(sumrow, "comment") <- "sum of rows enter as x; where is c(v1,v2,..vk)."
attr(sumrow, "help") <- "sum of rows enter as x; where is c(v1,v2,..vk)."

summary2 <- function(x) {c(summary(x),SD=sd(x),sum=sum2(x),N=ns(x))}

varNames <-function(x) {vx <- dim(x)[2] 
ret <- cbind(no=1:vx,varName=colnames(x))
return(ret)}

#Calculates Percentage in the xstrin, for cathegorical data
prcentCalc <- function(x,xstring=c("YES",1),...){round(100*sum2(x %in% xstring)/length(!is.na(x)),1)}

prcentCalc.tab <- function(x,y,...){round(100*prop.table(table(x,y),2),0)}
prcentCalc.tab1d <- function(x,y,...){round(100*prop.table(table(x,y),2),1)}
prcentCalc.tab2D <- function(x,y,...){round(100*prop.table(table(x,y),2),2)}


# ~~~~ tabar generate table and barplot for cathegorical variables ~~~~~
tabar<- function(c1,c2,ti,colX=c("white","grey80","grey50","grey30","black"))   {
  DT <-dat[,c1]
  colnames(DT )<- c2
  T101 <- CreateCatTable(vars=c2,dat=DT)
  print(T101)
  MX <- apply(DT,2,prcentCalc)
  barplot(MX,ylab="Percent",axis.lty=1,ylim=c(0,110),main = ti,col=colX,cex.axis=0.60)
  box("plot",lty="solid") }


tabar2<- function(c1,c2,ti,colX=c("white","grey80","grey50","grey30","black"))   {
  DT <-dat[,c1]
  colnames(DT )<- c2
  T101 <- CreateCatTable(vars=c2,dat=DT)
  print(T101)
  MX <- apply(DT,2,prcentCalc)
  nx<-length(MX)
  barplot(MX,ylab="Percent",axis.lty=1,ylim=c(0,110),main = ti,col=colX)
  box("plot",lty="solid") 
  XX<-1:nx; XX[1]<-0.75
  for( i in 2:nx ) { XX[i]<-XX[i-1]+1.2 }
  text(XX,MX+5,MX)
}


#~~~~~~~ lables for pei chart
lbls <- function(x){paste(names(x), "\n", x, sep="")}
#~~~ table.np function returning the x*y table n and percent
table.np <- function(x,y){
  xx<-table(x,y)
  xxp<-100*xx/(xx[,1]+xx[,2])  
  ret<-list(Tab.n=xx,Tab.Percent=xxp)
  return(ret) }

#~~~~~~~~~~~ obtain t-score  ~~~~
tcal<- function(x){round((x-mean2(x))/sd2(x),3)}

#~~~ my Date functions

ToDate  <- function(x) {as.Date(as.character(x),format='%Y-%m-%d')}   # String 2011-07-21 to date
ToDate2 <- function(x) {as.Date(x,"%d%b%Y")}                          # Factor 21JUL2011  to date
ToDate3  <- function(x) {as.Date(as.character(x),format='%d/%m/%Y')}   # Fctor to String 21/04/2011to date

DayOfWeek <- function(x) {weekdays(x) }                               # find date of week from date 



# calculating number of days between two dates
timeFun <- function(d1,d2) {
  D1 <- strptime(d1,'%Y-%m-%d')
  D2 <- strptime(d2,'%Y-%m-%d')
  TIME <-   as.numeric(difftime(D1,D2,units='days'))
  return(TIME)
}

#Table with totals Total and Dead
tabx<-function(A,B){
  tx<-table(A,B)
  ret <- cbind(tx,Total=tx[,1]+tx[,2],Dead=tx[,2])
  return(ret)
}

tabTot<-function(A,B){
  tx<-table(A,B)
  ret <- cbind(tx,Total=tx[,1]+tx[,2])
  return(ret)
}

# Descriptives - Vars is c("AGE","Weit"...) of numeric variables, data   

desc <- function(Vars,data=dat){ 
  tmp<-as.data.frame(dat[,Vars])
  tab1<-cbind(
    Mean <- apply(tmp,MARGIN=2,FUN=mean),
    SD <- apply(tmp,MARGIN=2,FUN=sd),
    Q1 <- apply(tmp,MARGIN=2,FUN=p25),
    Median <- apply(tmp,MARGIN=2,FUN=sd),
    Q3 <- apply(tmp,MARGIN=2,FUN=p75),
    MIN <- apply(tmp,MARGIN=2,FUN=min),
    MAX <- apply(tmp,MARGIN=2,FUN=max),
    NMIS <- apply(tmp,MARGIN=2,FUN=nmis)
  )
}

compute_CHADS_VASC <- function(indata) {
  # Computes the CHADS_VASC score
  ret <- indata[,c()]
  ret$CHADS_VASC_A <- 0+(indata$age>=65)
  ret$CHADS_VASC_A2 <- 0+(indata$age>=75)
  ret$CHADS_VASC_SC <- 0+(indata$gender=='M')
  ret$CHADS_VASC_H <- indata$pre_HTN
  ret$CHADS_VASC_C <- indata$pre_CHF
  ret$CHADS_VASC_S <- 2*pmax(indata$pre_CVA,indata$pre_TIA,indata$pre_DVT,indata$pre_PE)
  ret$CHADS_VASC_V <- pmax(indata$pre_MI,indata$pre_PCI,indata$pre_CABG)
  ret$CHADS_VASC_D <- indata$pre_DM
  ret$CHADS_VASC <- rowSums(ret)
  return(ret)
}

#nrisk - function to obtain time at risk for TT=time, GRP-groups, and 
#                     BX=vector of bids for nrisk table - e.g. BX=c(0,3,6,12,18,max(TT))
nrisk<-function(TT,GRP,BX=c(0,3,6,9,12)) {
  bx<-c(BX,max(TT,na.rm=T))
  TTcat<-cut(TT,bx)
  tx <- table(TTcat,GRP)
  nr<-length(tx[,1])
  X<-tx
  for(i in 1:nr-1){
    X[i,]<-apply(tx[i:nr,],2,sum)
  }
  return(X)
}

#format yes/no to 1/0
y1n0 <- function(x) { as.numeric(as.logical(x=="YES")) }
#for data frame:
y1n0_df <- function(df,value_yes) {for (i in 1:length(names(df))) {
  df[,names(df)[i]] <-as.numeric(as.logical(df[,names(df)[i]]==value_yes))
}
  return(df)
  }

#summary for logistic regression
summ.or <- function(mod) {tab=cbind(OR=round(exp(coef(mod)),2),CI=round(exp(confint(mod)),2),P.value=round(summary(mod)$coefficients[,4],3))
return(tab)}

# cox for different independent covariates
cox.multi <- function(covX,time.event,dat) 
{
  f <- as.formula(
    paste("Surv(" ,time.event, ") ~ ",
          paste(covX, collapse= "+")))
  
  mod <- coxph(f , data=dat) 
  
  return(mod)
}

#trend test  
ptrend.michal <- function(y,x,nrow=1) {
  pval <-round(prop.trend.test(table(y,x)[nrow,],
                               apply(table(y,x),2, sum))$p.value, 3)
  pval <- ifelse(pval<0.001,"<0.001",pval)
  return(pval)
}


# nonnormal vars for table 1 (based on shapiro test)
nonnormal_vars <- function(data, variables) {
  
  numeric.vars = variables[sapply(data[,variables], class)== "numeric"]
  shap.test = sapply(dat[,numeric.vars], shapiro.test)
  nonnormal.vars = colnames(shap.test)[shap.test["p.value",] < 0.005]
}



## From Daniel's functions: ##
#================================================================================================
factorial.cov.matrix <- function(df.to.cor, th = 0.2){
  
  df.temp <- df.to.cor[, sapply(df.to.cor, class) == "factor"]
  factorial.df <- sapply(df.temp , function(var.i){as.numeric(var.i) - 1})
  
  cov.matrix <- cor(factorial.df, 
                    use = "pairwise.complete.obs", 
                    method = "spearman")
  cov.matrix <- round(cov.matrix, 4)
  cov.matrix <- ifelse(cov.matrix < th | cov.matrix == 1, NA, cov.matrix)
  cov.matrix <- na.omit(reshape::melt(cov.matrix)) # melt! 
  cov.matrix <- cov.matrix[order(-abs(cov.matrix$value)),] # sort
  return(cov.matrix)
}



median2 <- function(x) return( median(x, na.rm = T ))
table2  <- function(x) return( table(x, useNA = 'always'))
sum2    <- function(x) return( sum(x, na.rm = T))
min2    <- function(x) return( min(x, na.rm = T))
min3    <- function(x) return(ifelse( !all(is.na(x)), min(x, na.rm=T), NA))
max3    <- function(x) return(ifelse( !all(is.na(x)), max(x, na.rm=T), NA))
sd2     <- function(x) return( sd(x, na.rm = T))
psum <- function(...,na.rm=FALSE) { 
  rowSums(do.call(cbind,list(...)),na.rm=na.rm) }  #returns 0 for na
prcentCalc.tab1d <- function(x,y,...){round(100*prop.table(table(x,y),2),1)}

remNAfromCol <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# Missing vector for table 1 - percentages
nmissing.percent <- function(vars,data){
  t <- c()
  for (i in 1:length(vars)){
    t[(length(t)+1):(length(t)+length(nmisT(data[,vars[i]])))] <- nmisT(data[,vars[i]])
  }
  t <- as.numeric(t)/dim(dat)[1]
  t <- round(t*100)
  t <- paste0(t, "%")
  return(c("",t))
}



# p-val notation for CreateTableOne
CTO.pval.notation <- function( tbl.to.process , p.val.col.name = "p"){
  r.len <- dim(tbl.to.process)[1]
  pval.col     <- tbl.to.process[2:r.len, p.val.col.name]
  pval.col.tmp <- as.numeric(pval.col)
  pval.col     <- ifelse(pval.col.tmp < 0.001, "<0.001", 
                         ifelse( pval.col.tmp < 0.05, "<0.05", pval.col))
  tbl.to.process[ 2:r.len , p.val.col.name] <- pval.col
  
  return( tbl.to.process )
}

## ACSIS dictionary

# Variable dictionary
# Another optional path:
# W:/Users/Shared_Files/datamng/acs00_02/csvfiles
ACSIS.dictionary <- function(
  vars.names.path = 'W:/Users/Shared_Files/STAT/2017/17035 ACSIS Generic Report/Variables dictionary.csv')
{
  
  na.definitions <- c("", " ", "N", ".", "UNKNOWN", "A", "NA")
  vars.names <- read.csv(file = vars.names.path,
                         na.strings = na.definitions ,
                         header = T,
                         stringsAsFactors = F
  )
  
  labels.yes  <- paste0(vars.names$Variable, " = YES (%)")
  labels.mn   <- paste0(vars.names$Variable, " (mean (SD))")
  
  vars.yes <- paste0(vars.names$Label, " (%)")
  vars.mn  <- paste0(vars.names$Label, " (mean (SD))")
  
  vars.dictionary        <- c(vars.yes, 
                              vars.mn,
                              "Gender (male) (%)",
                              "Killip score of 1 (%)",
                              "STEMI (%)",
                              "Late period 2008-2016 (%)"
  )
  
  names(vars.dictionary) <- c(labels.yes, 
                              labels.mn,
                              "SEX = MALE (%)",
                              "killip.1 = 1 (%)",
                              "stemi = STEMI (%)",
                              "periods = 2008-2016 (%)")
  
  return(vars.dictionary)
}

vars.dictionary <- function(
  vars.names.path = 'W:/Users/Shared_Files/STAT/2017/17035 ACSIS Generic Report/Variables dictionary.csv')
{
  
  na.definitions <- c("", " ", "N", ".", "UNKNOWN", "A", "NA")
  vars.names <- read.csv(file = vars.names.path,
                         na.strings = na.definitions ,
                         header = T,
                         stringsAsFactors = F
  )
  
  labels.yes  <- paste0(vars.names$Variable, " = YES (%)")
  labels.mn   <- paste0(vars.names$Variable, " (mean (SD))")
  
  vars.yes <- paste0(vars.names$Label, " (%)")
  vars.mn  <- paste0(vars.names$Label, " (mean (SD))")
  
  vars.dictionary        <- c(vars.yes, 
                              vars.mn,
                              "Gender (male) (%)",
                              "Killip score of 1 (%)",
                              "STEMI (%)",
                              "Late period 2008-2016 (%)"
  )
  
  names(vars.dictionary) <- c(labels.yes, 
                              labels.mn,
                              "SEX = MALE (%)",
                              "killip.1 = 1 (%)",
                              "stemi = STEMI (%)",
                              "periods = 2008-2016 (%)")
  
  return(vars.dictionary)
}


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


glm.forestplot <- function(glm.model, 
                           replace.names = NA,
                           title = "OR with 95% CI", 
                           xticks = c(0, 0.5, 1, 1.5, 2, 2.5, 3)
){
  
  var_names <- names(glm.model$coefficients)[2:length(names(glm.model$coefficients))]
  
  if(!is.na(replace.names)){
    var_names <- as.vector(replace.names[var_names])
  }
  
  OR <- exp(coef(glm.model))
  CI <- exp(suppressMessages(confint(glm.model)))
  
  tabtext2 <- cbind(c("",var_names), 
                    c("OR (95% CI) for logistic regression model",
                      paste0(round(OR[-1],2), 
                             " (", 
                             round(CI[,1][-1],2),
                             ",",
                             round(CI[,2][-1],2),")"))
  )
  
  forestplot(new_page = TRUE,
             labeltext = tabtext2,
             mean  = c(NA, OR[-1]),
             lower = c(NA, CI[, 1][-1]),
             upper = c(NA, CI[, 2][-1]),
             clip  = c(0 , 6.5),
             zero  = 1,
             boxsize     = 0.1,
             graph.pos   = 2,
             graphwidth  = unit(3.5, "inches"),
             ci.vertices = TRUE,
             xticks = xticks,
             txt_gp = fpTxtGp(cex = 0.8,
                              title = gpar(cex = 1.5),
                              ticks = gpar(cex = 0.8)),
             is.summary = c(T, rep(F, 13)),
             col = fpColors(box  = "royalblue",
                            line = "darkblue",
                            zero = "darkgrey"),
             title = "OR for UA tertile with 95% CI"
  )
}

# create competing risk variables (event, time): (20.8.2020)
create_vars_competing_risk <- function(main_outcome, competing_outcome, main_time, competing_time, answer_main, answer_competing, name_of_new_event, name_of_new_time, data) {
  data[[name_of_new_event]] <- ifelse(is.na(data[[main_outcome]]) & is.na(data[[competing_outcome]]),NA,
                                      ifelse(!is.na(data[[main_outcome]]) & data[[main_outcome]]==answer_main,1,
                                             ifelse(data[[competing_outcome]]==answer_competing,2,0))) # 1=main, 2=competing, 0=none
  
  data[[name_of_new_time]] <- ifelse(is.na(data[[main_time]]) & is.na(data[[competing_time]]),NA, 
                                     ifelse(is.na(data[[main_time]]),data[[competing_time]], 
                                            ifelse(is.na(data[[competing_time]]),data[[main_time]],
                                                   ifelse(data[[competing_time]] < data[[main_time]], data[[competing_time]], 
                                                          data[[main_time]]))))
  return(data)
}

#================#
# Trend graphs
#================#
# Trend graph for percentages:
plot_pct_trend_graph <- function(x_var, y_var, data) {
  
  # for variable with 2 levels
  if (length(levels(as.factor(data[[y_var]])))==2) {
  event_yes <- levels(as.factor(data[[y_var]]))[2]
  
  pct = round(prop.table(table(data[[y_var]], data[[x_var]]),2)*100)
  n = addmargins(table(data[[y_var]], data[[x_var]]))[3,1:length(levels(as.factor(data[[x_var]])))]
  pct = data.frame(group = pct, value = as.numeric(pct), n = rep(n, each=2))
  
 
  # Chi-squared Trend Test
  t1 <- addmargins(table(data[[y_var]], data[[x_var]]),1 )
  test <- prop.trend.test(t1[event_yes, ], t1["Sum", ])
  pval <- round(as.numeric(formatC(test$p.value, format = "f")),2)
  pval <- ifelse(pval<0.001, "<0.001", pval)
  
  
  ggplot(data=pct, aes(x= group.Var2 , y=value, fill= group.Var1 )) +
    geom_bar(stat="identity", position = position_dodge()) +
    labs(title = paste0(as.character(y_var),"\n number of patients with available data: ",dim(data[complete.cases(data[,c(x_var, y_var)]),])[1] , "\n p value for Chi-square trend test: ",pval), x = paste0(as.character(x_var), " \n (n)"), y = "Percent") +
    theme(legend.position="top")+
    geom_text(aes(label=value), position=position_dodge(width = 1), vjust=1) +
    scale_x_discrete(labels = c(paste(pct[pct$group.Var1==event_yes,]$group.Var2, "\n (",pct[pct$group.Var1==event_yes,]$n,")"))) +
    # scale_fill_grey(start = 0.2, end = 0.6) +
    theme(text = element_text(family = "serif")) +
    theme(axis.text.x = element_text(colour = "black"))
  }
  else {
    event_yes <- levels(as.factor(data[[y_var]]))[1]
    
    pct = round(prop.table(table(data[[y_var]], data[[x_var]]),2)*100)
  n = addmargins(table(data[[y_var]], data[[x_var]]))[length(levels(as.factor(data[[y_var]])))+1,1:length(levels(as.factor(data[[x_var]])))]
  pct = data.frame(group = pct, value = as.numeric(pct), n = rep(n, each=length(levels(as.factor(data[[y_var]])))))
 
  # Chi-squared Trend Test
  t1 <- addmargins(table(data[[y_var]], data[[x_var]]),1 )
  test <- prop.trend.test(t1[event_yes, ], t1["Sum", ])
  pval <- round(as.numeric(formatC(test$p.value, format = "f")),2)
  pval <- ifelse(pval<0.001, "<0.001", pval)
  
  ggplot(data=pct, aes(x= group.Var2 , y=value, fill= group.Var1 )) +
    geom_bar(stat="identity", position = position_dodge()) +
    labs(title = paste0(as.character(y_var),"\n number of patients with available data: ",dim(data[complete.cases(data[,c(x_var, y_var)]),])[1] ,"\n p value for Chi-square trend test: ",pval), x =paste0(as.character(x_var), " \n (n)"), y = "Percent") +
    theme(legend.position="top")+
     geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25) +
    scale_x_discrete(labels = c(paste(pct[pct$group.Var1==event_yes,]$group.Var2, "\n (",pct[pct$group.Var1==event_yes,]$n,")"))) +
    # scale_fill_grey(start = 0.2, end = 0.6) +
    theme(text = element_text(family = "serif")) +
    theme(axis.text.x = element_text(colour = "black"))
  }
}

# Two groups:  Trend graph for percentages:
plot_pct_trend_graph_2groups <- function(x_var, y_var, groups, data) {
  data[[y_var]] <- as.factor(as.character(data[[y_var]]))
  # for variable with 2 levels:
  if (length(levels(as.factor(data[[y_var]])))==2) {
    event_yes <- levels(as.factor(data[[y_var]]))[2]
    
    d2 <- data[complete.cases(data[,c(x_var, y_var, groups)]),] %>% 
      group_by(get(groups), get(x_var), get(y_var)) %>% 
      dplyr::summarise(count=n()) %>%
      dplyr::mutate(perc=count/sum(count))
    
    d2 <- as.data.frame(d2) %>%
    filter(get("get(y_var)") %in% event_yes)

    names(d2) <- c("groups","x_var","y_var","count","perc")

    # p value for trend in each group:
    data_group1 <- data[data[[groups]]==levels(factor(data[[groups]]))[1],]
    data_group2 <- data[data[[groups]]==levels(factor(data[[groups]]))[2],]
    p_val_group1 <- p_for_trend(vars =y_var ,strata = x_var,data = data_group1[1:5000,])
    p_val_group2 <- p_for_trend(vars =y_var ,strata = x_var,data = data_group2[1:5000,])

    # p value for interaction:
    dat_for_model <- data
    model_glm <- glm(factor(get(y_var))==levels(factor(data[[y_var]]))[2] ~ factor(get(groups)) + as.numeric(as.character(get(x_var))) + factor(get(groups))*as.numeric(as.character(get(x_var))), family = "binomial", data=dat_for_model)
    summary_mod <- summary(model_glm)
    p_val_for_interaction <- summary_mod$coefficients[4, "Pr(>|z|)"]
    p_val_for_interaction <- ifelse(as.numeric(formatC(p_val_for_interaction, format = "f"))<0.001,"<0.001",round(as.numeric(formatC(p_val_for_interaction, format = "f")),3))

    # p value for interaction adjusted for age:
    model_glm_adj <- glm(factor(get(y_var))==levels(factor(data[[y_var]]))[2] ~ factor(get(groups)) + as.numeric(as.character(get(x_var)))+ AGE + factor(get(groups))*as.numeric(as.character(get(x_var))), family = "binomial", data=dat_for_model)
    summary_mod_adj <- summary(model_glm_adj)

    p_val_for_interaction_age_adjusted <- summary_mod_adj$coefficients[5, "Pr(>|z|)"]
    p_val_for_interaction_age_adjusted <- ifelse(as.numeric(formatC(p_val_for_interaction_age_adjusted, format = "f"))<0.001,"<0.001",round(as.numeric(formatC(p_val_for_interaction_age_adjusted, format = "f")),3))


    ggplot(d2, aes(x = x_var, y = perc*100, fill = factor(groups))) +
      geom_bar(stat="identity", width = 0.7, position = position_dodge()) +
      labs(title = paste0(as.character(y_var),"\n number of patients with available data: ", dim(data[complete.cases(data[,c(x_var, y_var, groups)]),])[1],
                          "\n p for trend within ", levels(data[[groups]])[1],": ",p_val_group1[2],
                          "\n p for trend within ", levels(data[[groups]])[2],": ",p_val_group2[2],
                          "\n p for interaction: ", p_val_for_interaction,
                          "\n p for interaction - age adjusted: ", p_val_for_interaction_age_adjusted), x = as.character(x_var), y = "percent", fill = as.character(groups)) 
      # theme_minimal(base_size = 14) +
      # geom_line(aes(linetype = groups, color = groups)) #only when the x_var is numberic

      
  }
  else {
    d2 <- data[complete.cases(data[,c(x_var, y_var, groups)]),] %>% 
      group_by(get(groups), get(x_var), get(y_var)) %>% 
      dplyr::summarise(count=n()) %>%
      dplyr::mutate(perc=count/sum(count))
    
   
    names(d2) <- c("groups","x_var","y_var","count","perc")
    # return(as.data.frame(d2))
    
    ggplot(d2, aes(x = x_var, y = perc*100, fill = factor(y_var))) +
      geom_bar(stat="identity", position = position_dodge(), width = 0.7) +
      labs(title = paste0(as.character(y_var),"\n number of patients with available data: ", dim(data[complete.cases(data[,c(x_var, y_var, groups)]),])[1]), x = as.character(x_var), y = "percent", fill = as.character(y_var)) + 
    facet_wrap(~ groups)  +
      geom_text(aes(label=round(perc*100)), position=position_dodge(width=0.7), vjust=-0.25)
    # theme_minimal(base_size = 14) +
      # geom_line(aes(linetype = groups, color = groups))
  
  }
}

# Trend graph for numeric data:
plot_boxplot_trend_graph <- function(x_var, y_var, data) {
  
  data_c <- data[!is.na(data[[x_var]]) & !is.na(data[[y_var]]),]
  data_c[[x_var]] <- as.factor(data_c[[x_var]])
  data_c[[y_var]] <- as.numeric(data_c[[y_var]])
 
  #calc p-value for trend:
  pvalue_for_Trend <- p_for_trend(vars = y_var,strata = x_var,data = data_c[1:5000,])

    ggplot(data=data_c, aes(x= .data[[x_var]] , y=.data[[y_var]] )) +
      geom_boxplot() +
      labs(title = paste0(as.character(y_var),"\n p value for trend: ",pvalue_for_Trend[2], "\n number of patients with available data: ", dim(data_c)[1]), x = as.character(x_var), y =paste0(as.character(y_var), " (up to 80% percentile)") ) +
      # theme(legend.position="top")+
      # geom_text(aes(label=value), vjust=-0.3, size=4) +
      # scale_x_discrete(labels = c(paste(pct[pct$group.Var1==event_yes,]$group.Var2, "\n (",pct[pct$group.Var1==event_yes,]$n,")"))) +
      # scale_fill_grey(start = 0.2, end = 0.6) +
      scale_y_continuous(limits = c(min(data_c[[y_var]]), quantile(data_c[[y_var]], probs = .8) )) +
      theme(text = element_text(family = "serif")) +
      theme(axis.text.x = element_text(colour = "black"))  

  
}

# 2 groups: Trend graph for numeric data:
plot_boxplot_trend_graph_2groups <- function(x_var, y_var, groups, data) {
  
  data_c <- data[!is.na(data[[x_var]]) & !is.na(data[[y_var]]) & !is.na(data[[groups]]),]
  data_c[[x_var]] <- as.factor(data_c[[x_var]])
  data_c[[y_var]] <- as.numeric(data_c[[y_var]])
  data_c[[groups]] <- as.factor(data_c[[groups]])

    #calc p-value for trend:
    # pvalue_for_Trend <- p_for_trend(vars = y_var,strata = x_var,data = data_c[1:5000,])
  
  ggplot(data=data_c, aes(x= .data[[x_var]] , y=.data[[y_var]], color=.data[[groups]]))+  #, fill=.data[[groups]]) ) +
    geom_boxplot() +
    labs(title = paste0(as.character(y_var), "\n number of patients with available data: ", dim(data_c)[1]), x = as.character(x_var), y =paste0(as.character(y_var), " (up to 80% percentile)") ) +
    theme(legend.position="top")+
    # geom_text(aes(label=value), vjust=-0.3, size=4) +
    # scale_x_discrete(labels = c(paste(pct[pct$group.Var1==event_yes,]$group.Var2, "\n (",pct[pct$group.Var1==event_yes,]$n,")"))) +
    # scale_fill_grey(start = 0.2, end = 0.6) +
    scale_y_continuous(limits = c(min(data_c[[y_var]]), quantile(data_c[[y_var]], probs = .8) )) +
    theme(text = element_text(family = "serif")) +
    theme(axis.text.x = element_text(colour = "black")) 
  
  
}


# Trend graph for a list of variables: 
trend_graphs <- function(vars_list, x_var, data) {
 plots <- list()
  for (i in 1:length(vars_list)) {

    if (class(data[,vars_list[i]]) %in%c("factor","character")) {
      plots[[i]]<-  plot_pct_trend_graph(x_var = x_var, y_var = vars_list[i], data = data)
    
    } else if (class(data[,vars_list[i]]) %in%c("numeric","integer")) {
      plots[[i]]<-   plot_boxplot_trend_graph(x_var = x_var, y_var = vars_list[i], data = data)
   
    }
  }
 return(marrangeGrob(plots, ncol = 3, nrow = round(length(vars_list)/3)))
}


# 2 groups: Trend graph for a list of variables: 
trend_graphs_2groups <- function(vars_list, x_var, groups, data) {

  plots <- list()
  for (i in 1:length(vars_list)) {
    
    if (class(data[,vars_list[i]]) %in%c("factor","character") | (class(data[,vars_list[i]]) %in%c("numeric","integer") & levels(as.factor(data[,vars_list[i]])) %in% c("0","1") & max2(as.numeric(data[,vars_list[i]]))==1)  ) {
      plots[[i]]<-  plot_pct_trend_graph_2groups(x_var = x_var, y_var = vars_list[i], groups=groups, data = data)
      
    } else if (class(data[,vars_list[i]]) %in%c("numeric","integer")) {
      plots[[i]]<-   plot_boxplot_trend_graph_2groups(x_var = x_var, y_var = vars_list[i], groups=groups, data = data)
      
    }
  }
  all_plots <-(marrangeGrob(plots, ncol = 1, nrow = length(vars_list)))
  return(all_plots)
 
}



# SELECT SIGNIFICANT VARIABLES FROM TABLE
#=========================================#
sig_vars_from_table <- function(data, is_table_from_excel=F ,strata_for_table, variables_for_table, factor_vars=NULL, nonnormal_vars=NULL, p_trend=F, pct_missing_alowed=10, pval_cutoff = 0.05) { 
  
  
  if (is_table_from_excel==T & p_trend==T) {
    dat_to_fit <- data
    
    tab1 <- print(CreateTableOne(vars = variables_for_table, 
                                 factorVars = factor_vars,
                                 strata = strata_for_table,
                                 data = dat_to_fit
    )
    ,nonnormal = nonnormal_vars, printToggle=F, missing=T
    )
    
   
    
    p_for_trend_1 <- p_for_trend(vars = variables_for_table[!variables_for_table %in% "NULL"], 
                                 strata = strata_for_table, data = dat_to_fit)
    
    tab1 <- cbind(tab1,p_for_trend_1)
    print(tab1)
    
    # Take tab1 as dataframe and take the significant vars (sig<0.05)
    tab1_df <- as.data.frame(tab1)
    tab1_df <- cbind(rownames(tab1_df),tab1_df)
    tab1_df$vars <- tab1_df$`rownames(tab1_df)`
    # tab1_df$p_num <- ifelse(tab1_df$p_for_trend_1=="<0.001","0",as.character(tab1_df$p_for_trend_1))
    tab1_df$p_num <- ifelse(tab1_df$p_for_trend_1=="<0.001","0", ifelse(as.character(tab1_df$p)==" NaN", "2",as.character(tab1_df$p_for_trend_1))) #if it's NaN then the p value doesnt matter but we still need this line to appear
    
    tab1_df$p_num <- as.numeric(tab1_df$p_num)
    tab1_df$sig_vars <- ifelse(tab1_df$p_num<pval_cutoff & as.numeric(tab1_df$Missing)< pct_missing_alowed ,as.character(tab1_df$vars),NA)
    
    sig_vars <- variables_for_table[which(!is.na(tab1_df$sig_vars))-1]
    sig_vars_1 <- paste(sig_vars, collapse = '+') #to put in the model
    
    return(sig_vars_1)
  }
  
  else if (is_table_from_excel==T & p_trend==F) {
    dat_to_fit <- data
    
    tab1 <- print(CreateTableOne(vars = variables_for_table, 
                                 factorVars = factor_vars,
                                 strata = strata_for_table,
                                 data = dat_to_fit
    )
    ,nonnormal = nonnormal_vars, printToggle=F, missing=T
    )
    
   
    print(tab1)
    
    # Take tab1 as dataframe and take the significant vars (sig<0.05)
    tab1_df <- as.data.frame(tab1)
    tab1_df <- cbind(rownames(tab1_df),tab1_df)
    tab1_df$vars <- tab1_df$`rownames(tab1_df)`
    tab1_df$p_num <- ifelse(tab1_df$p=="<0.001","0", ifelse(as.character(tab1_df$p)==" NaN", "2",as.character(tab1_df$p))) #if it's NaN then the p value doesnt matter but we still need this line to appear
    tab1_df$p_num <- as.numeric(tab1_df$p_num)
    tab1_df$sig_vars <- ifelse(tab1_df$p_num<pval_cutoff & as.numeric(tab1_df$Missing)< pct_missing_alowed ,as.character(tab1_df$vars),NA)
    
    sig_vars <- variables_for_table[which(!is.na(tab1_df$sig_vars))-1]
    sig_vars_1 <- paste(sig_vars, collapse = '+') #to put in the model
    
    return(sig_vars_1)
  }
  
  else if (is_table_from_excel==F & p_trend==T) {
    dat_to_fit <- data
    
    tab1 <- print(CreateTableOne(vars = variables_for_table, 
                                 factorVars = factor_vars,
                                 strata = strata_for_table,
                                 data = dat_to_fit
    )
    ,nonnormal = nonnormal_vars, printToggle=F, missing=T
    )
    
  
    p_for_trend_1 <- p_for_trend(vars = variables_for_table, 
                                 strata = strata_for_table, data = dat_to_fit)
    
    tab1 <- cbind(tab1,p_for_trend_1)
    print(tab1)
    
    # Take tab1 as dataframe and take the significant vars (sig<0.05)
    tab1_df <- as.data.frame(tab1)
    tab1_df <- cbind(rownames(tab1_df),tab1_df)
    tab1_df$vars <- tab1_df$`rownames(tab1_df)`
    # tab1_df$p_num <- ifelse(tab1_df$p_for_trend_1=="<0.001","0",as.character(tab1_df$p_for_trend_1))
    tab1_df$p_num <- ifelse(tab1_df$p_for_trend_1=="<0.001","0", ifelse(as.character(tab1_df$p)==" NaN", "2",as.character(tab1_df$p_for_trend_1))) #if it's NaN then the p value doesnt matter but we still need this line to appear
    
    tab1_df$p_num <- as.numeric(tab1_df$p_num)
    tab1_df$sig_vars <- ifelse(tab1_df$p_num<pval_cutoff & as.numeric(tab1_df$Missing)< pct_missing_alowed ,as.character(tab1_df$vars),NA)
    
    tab1_df <- tab1_df[!is.na(tab1_df$p_num),]
    sig_vars <- variables_for_table[which(!is.na(tab1_df$sig_vars))]
    sig_vars_1 <- paste(sig_vars, collapse = '+') #to put in the model
    
    return(sig_vars_1)
  }
  
  else if (is_table_from_excel==F & p_trend==F) {
    dat_to_fit <- data
    
    tab1 <- print(CreateTableOne(vars = variables_for_table, 
                                 factorVars = factor_vars,
                                 strata = strata_for_table,
                                 data = dat_to_fit
    )
    ,nonnormal = nonnormal_vars, missing=T
    )
    
    
    
    # Take tab1 as dataframe and take the significant vars (sig<0.05)
    tab1_df <- as.data.frame(tab1)
    tab1_df <- cbind(rownames(tab1_df),tab1_df)
    tab1_df$vars <- tab1_df$`rownames(tab1_df)`
   
    tab1_df$p_num <- ifelse(tab1_df$p=="<0.001","0", ifelse(as.character(tab1_df$p)==" NaN", "2",as.character(tab1_df$p))) #if it's NaN then the p value doesnt matter but we still need this line to appear
    tab1_df$p_num <- as.numeric(tab1_df$p_num)
    tab1_df$sig_vars <- ifelse(tab1_df$p_num<pval_cutoff & as.numeric(tab1_df$Missing)< pct_missing_alowed ,as.character(tab1_df$vars),NA)
    
    tab1_df <- tab1_df[!is.na(tab1_df$p_num),]
    sig_vars <- variables_for_table[which(!is.na(tab1_df$sig_vars))]
    sig_vars_1 <- paste(sig_vars, collapse = '+') #to put in the model
    
    return(sig_vars_1)
  }
}


#==================#
# Compare two data frames: (need to read the files with NA-strings and with stringsAsFactors=F)
compare_df <- function(df1, df2, ID_coloumn_name_1,ID_coloumn_name_2,path_to_write_the_comparisons_file) {
  #row numbers:
  df1$row_number_original <- 1:nrow(df1)
  df2$row_number_original <- 1:nrow(df2)
  
  
  print(paste0("Number of rows in df1: ", nrow(df1)))
  print(paste0("Number of rows in df2: ", nrow(df2)))
  print(paste0("ID's which appear only in df1 (according to ID_coloumn_name_1): ", 
               ifelse(length(df1[[ID_coloumn_name_1]][!df1[[ID_coloumn_name_1]] %in% df2[[ID_coloumn_name_1]]])>0,
                      df1[[ID_coloumn_name_1]][!df1[[ID_coloumn_name_1]] %in% df2[[ID_coloumn_name_1]]], "None"
                      )
                      ))
  print(paste0("ID's which appear only in df2 (according to ID_coloumn_name_1): ", 
               ifelse(length(df2[[ID_coloumn_name_1]][!df2[[ID_coloumn_name_1]] %in% df1[[ID_coloumn_name_1]]])>0,
                      df2[[ID_coloumn_name_1]][!df2[[ID_coloumn_name_1]] %in% df1[[ID_coloumn_name_1]]], "None"
               )
               ))
  
  #remove rows with missing data in the ID column names:
  df1 <- df1[!is.na(df1[[ID_coloumn_name_1]]) & !is.na(df1[[ID_coloumn_name_2]]), ]
  df2 <- df2[!is.na(df2[[ID_coloumn_name_1]]) & !is.na(df2[[ID_coloumn_name_2]]), ]
  
  print("After removing rows with missing data in one of the ID columns:")
  print(paste0("Number of rows in df1: ", nrow(df1)))
  print(paste0("Number of rows in df2: ", nrow(df2)))
  
  print(paste0("Number of columns in df1: ", ncol(df1)-1)) #minus 1 is for removing "row_number_original"
  print(paste0("Number of columns in df2: ", ncol(df2)-1)) #minus 1 is for removing "row_number_original"
  
  print(paste0("Columns which appear only in df1: ", 
               ifelse(length(names(df1)[!names(df1) %in% names(df2)])>0,
                      names(df1)[!names(df1) %in% names(df2)], "None"  
                      )
                      ))
  print(paste0("Columns which appear only in df2: ", 
               ifelse(length(names(df2)[!names(df2) %in% names(df1)])>0,
                      names(df2)[!names(df2) %in% names(df1)], "None"  
               )
  ))  
  
  #remove columns which appear only in 1 data frame:
  df1 <- df1[,names(df1)[names(df1) %in% names(df2)]]
  df2 <- df2[,names(df2)[names(df2) %in% names(df1)]]
  
  
  
  #find duplicated rows (based on the ID columns):
  df1 <- df1 %>% distinct(get(ID_coloumn_name_1),get(ID_coloumn_name_2), .keep_all = TRUE)
  df2 <- df2 %>% distinct(get(ID_coloumn_name_1),get(ID_coloumn_name_2), .keep_all = TRUE)

  
  #take only rows with the same ID_coloumn_name from both data-frames:
  df1 <- merge(df1, df2[,c(ID_coloumn_name_1,ID_coloumn_name_2)], by=c(ID_coloumn_name_1,ID_coloumn_name_2), all = F)
  df2 <- merge(df2, df1[,c(ID_coloumn_name_1,ID_coloumn_name_2)], by=c(ID_coloumn_name_1,ID_coloumn_name_2), all = F)

  
  print(paste0("Number of rows in each data frame after removing duplicated rows and take only rows which appear in both data frames, according to the ID columns: ", nrow(df1)))
  
  #make NA into strings:
  df1[is.na(df1)] <- "na"
  df2[is.na(df2)] <- "na"
  
  
  if (ncol(df1)==ncol(df2)) {
  count_rows <- 0
  for (i in 1:nrow(df1)) {
    row1 <- df1[i,names(df1)[!names(df1)%in%"row_number_original"]]
    row2 <- df2[i,names(df2)[!names(df2)%in%"row_number_original"]]

    if (as.logical(table(row1==row2)["TRUE"]==length(row1))) {count_rows <- count_rows+1
   }
  }
  print(paste0("Number of identical rows (= all values are identical across the row) in both updated data frames: ", count_rows))
  } 

  
  print("For each column, the differences appear in the following places:")

  differences_between_dfs <- data.frame(ID_coloumn_name_1=NA,
                                        ID_coloumn_name_2=NA,
                                        Column_name= NA,
                                        Value_in_df1 = NA,
                                        Value_in_df2 = NA,
                                        Original_row_in_df1 = NA,
                                        Original_row_in_df2 = NA
  )
  
  for (i in 1:ncol(df1)) {

    for (j in 1:nrow(df1)) {
     if  (df1[j,i] != df2[j,i]) { 
      #row numbers in original data frames:
       df1_orig_row <- df1[j,"row_number_original"]
       df2_orig_row <- df2[j,"row_number_original"]
       #the column with the difference:
       col_diff_name <- names(df1)[i]
       #the values which are different:
       df1_orig_val <- df1[j,i]
       df2_orig_val <- df2[j,i]
       
       differences_between_dfs <- rbind(differences_between_dfs, 
                                data.frame(ID_coloumn_name_1=df1[j,ID_coloumn_name_1],
                                           ID_coloumn_name_2=df1[j,ID_coloumn_name_2],
                                           Column_name= col_diff_name,
                                           Value_in_df1 = df1_orig_val,
                                           Value_in_df2 = df2_orig_val,
                                           Original_row_in_df1 = df1_orig_row,
                                           Original_row_in_df2 = df2_orig_row
                                           )
                                        )
     } 
    
    }
    
   
  }
  differences_between_dfs <- subset(differences_between_dfs, !is.na(ID_coloumn_name_1))
  View(differences_between_dfs)
  write.csv(differences_between_dfs, file = paste0(path_to_write_the_comparisons_file, "/differences_between_files.csv"))
  
  
}

# df1 <- data.frame(a=c(1:10, NA), b=c(2:11, NA), c=c(rep("yes",10),1), ID1=letters[1:11], ID2=paste0(letters[1:11], 11:1))
# df2 <- data.frame(a=c(1:10, NA), b=c(2:11, NA), c=c(rep("yes",8),"NO","NO",1), ID1=letters[1:11], ID2=paste0(letters[1:11], 11:1))
# compare_df(df1=df1, df2=df2, ID_coloumn_name_1 ="ID1",ID_coloumn_name_2 ="ID2" )

# #read the files:
# data.file.path  <- 'D:/Users/308354075/Desktop/2022 03 01.csv'
# na.definitions  <- c("", " ", "N", ".", "UNKNOWN", "A", "NA", "nan", "Unknown")
# df1             <- read.csv(file = data.file.path, header = T, stringsAsFactors = F, na.strings = na.definitions)[,1:12]
# data.file.path  <- 'D:/Users/308354075/Desktop/2021 12 12.csv'
# # na.definitions  <- c("", " ", "N", ".", "UNKNOWN", "A", "NA", "nan", "Unknown")
# df2             <- read.csv(file = data.file.path, header = T, stringsAsFactors = F,na.strings = na.definitions)[,1:12]
# compare_df(df1=df1, df2=df2, ID_coloumn_name_1 =names(df1)[1],ID_coloumn_name_2 =names(df1)[2], path_to_write_the_comparisons_file='D:/Users/308354075/Desktop/' )

# data.file.path  <- 'W:/Users/Shared_Files/datamng/s2021/workfiles/ACSIS21_1.csv'
# na.definitions  <- c("", " ", "N", ".", "UNKNOWN", "A", "NA", "nan", "Unknown")
# df1             <- read.csv(file = data.file.path, header = T, stringsAsFactors = F, na.strings = na.definitions)
# data.file.path  <- 'W:/Users/Shared_Files/datamng/s2021/workfiles/ACSIS21_2.csv'
# df2             <- read.csv(file = data.file.path, header = T, stringsAsFactors = F, na.strings = na.definitions)
# compare_df(df1=df1, df2=df2, ID_coloumn_name_1 ="HAKZAA",ID_coloumn_name_2 ="RECORD_ID" , path_to_write_the_comparisons_file = "W:/Users/Shared_Files/datamng/s2021/workfiles/")

#=================#
#Table one - make the significant p values in bold:
sig_rows_numbers <- function(tab1, p_val_column_name) {
tab1_new <- as.data.frame(tab1)
tab1_new <- tab1_new %>% dplyr::mutate(row_numbers = row_number())
                                       
#find significant rows:
sig_rows <- suppressWarnings(tab1_new[["row_numbers"]][(as.numeric(tab1_new[[p_val_column_name]])<0.05 | tab1_new[[p_val_column_name]]%in%"<0.001") & !is.na(tab1_new[[p_val_column_name]])]) #warning "NAs introduced by coercion" is fine
sig_rows <- sig_rows[!is.na(sig_rows)]
sig_rows <- suppressWarnings(as.numeric(sig_rows)) #warning "NAs introduced by coercion" is fine
return(sig_rows)
}

# from: https://stackoverflow.com/questions/28166168/how-to-change-fontface-bold-italics-for-a-cell-in-a-kable-table-in-rmarkdown/49656650#49656650
format_cells <- function(df, rows ,cols, value = c("italics", "bold", "strikethrough")){
  
  # select the correct markup
  # one * for italics, two ** for bold
  map <- setNames(c("*", "**", "~~"), c("italics", "bold", "strikethrough"))
  markup <- map[value]  
  
  for (r in rows){
    for(c in cols){
      
      # Make sure values are not factors
      # df[[c]] <- as.character( df[[c]])
      # df[[c]] <- gsub(" ","",df[[c]])
      
      # Update formatting
      df[r, c] <- paste0(markup, gsub(" ","",df[[r, c]]), markup)
    }
  }
  
  return(df)
}


#==============#
#Add confidence interval (CI) for tableone:
#CI for proportions for 2 groups:
CI_prop_2g <- function(var, strata, data) {
  n1 = table(data[!is.na(data[[strata]]) & !is.na(data[[var]]),strata])[1]
  n2 = table(data[!is.na(data[[strata]]) & !is.na(data[[var]]),strata])[2]
  factor_levels <- length(levels(as.factor(data[[var]])))
  if (factor_levels>2) {
    tab_levels <- table(data[[var]], data[[strata]])
    prop_df <- data.frame(Group1_CI="", Group2_CI="")
    for (i in 1:factor_levels) {
     prop_df <- rbind(prop_df,
       data.frame(Group1_CI = paste0(round(binom.test(tab_levels[i,1], n1, alternative="two.sided")$conf.int*100, 2)[1], 
                           ", ", round(binom.test(tab_levels[i,1], n1, alternative="two.sided")$conf.int*100, 2)[2]),
                 Group2_CI= paste0(round(binom.test(tab_levels[i,2], n2, alternative="two.sided")$conf.int*100, 2)[1], 
                                ", ", round(binom.test(tab_levels[i,2], n2, alternative="two.sided")$conf.int*100, 2)[2]))
     )
      
    }
  } else {
    prop1 = table(data[[var]], data[[strata]])[2,1]
    prop2 = table(data[[var]], data[[strata]])[2,2]
    CI = round(binom.test(prop1, n1, alternative="two.sided")$conf.int*100, 2)
    CI_1 = paste0(CI[1], ", ", CI[2])
    CI = round(binom.test(prop2, n2, alternative="two.sided")$conf.int*100, 2)
    CI_2 = paste0(CI[1], ", ", CI[2])
    prop_df <- data.frame(Group1_CI = CI_1, Group2_CI= CI_2)
  }
 
  return(prop_df)
}
#CHECK:  CI_prop_2g("SEX","ANGIO",dat)


#CI for means for 2 groups:
CI_mean_2g <- function(var, strata, data) {
  dat1 = subset(data, data[[strata]]%in% levels(as.factor(data[[strata]]))[1])
  dat2 = subset(data, data[[strata]]%in% levels(as.factor(data[[strata]]))[2])
  CI = round(t.test(dat1[[var]], conf.level=0.95)$conf.int,2)
  CI_1 = paste0(CI[1], ", ", CI[2])
  CI = round(t.test(dat2[[var]], conf.level=0.95)$conf.int,2)
  CI_2 = paste0(CI[1], ", ", CI[2])
  return(data.frame(Group1_CI = CI_1, Group2_CI= CI_2))
}
# CHECK:  CI_mean_2g("AGE","ANGIO",dat)

#CI for medians for 2 groups: 
# library(DescTools)

CI_median_2g <- function(var, strata, data) {
  dat1 = subset(data, data[[strata]]%in% levels(as.factor(data[[strata]]))[1])
  dat2 = subset(data, data[[strata]]%in% levels(as.factor(data[[strata]]))[2])
  CI =  MedianCI(dat1[[var]], conf.level = 0.95, sides = "two.sided", na.rm = T)
  CI_1 = paste0(round(CI["lwr.ci"],2), ", ", round(CI["upr.ci"],2))
  CI =  MedianCI(dat2[[var]], conf.level = 0.95, sides = "two.sided", na.rm = T)
  CI_2 = paste0(round(CI["lwr.ci"],2), ", ", round(CI["upr.ci"],2))
  return(data.frame(Group1_CI = CI_1, Group2_CI= CI_2))
}
# CHECK:  CI_median_2g("AGE","ANGIO",dat)


#make 2 columns to add to tableone (1 column for each group):
CI_for_tableone <- function(vars, strata, data){
  CI_group <- data.frame(Group1_CI="", Group2_CI="")
  for(i in 1:length(vars)){
    # CI_group[(nrow(CI_group)+1):(nrow(CI_group)+1+nrow(CI_prop_2g(vars[i], strata, data)))] <- 
      if(is.factor(data[,vars[i]])) {
        CI_group<-  rbind(CI_group,  CI_prop_2g(vars[i], strata, data))
      } else if ((is.numeric(data[,vars[i]]) | is.integer(data[,vars[i]]))
                & shapiro.test(data[1:5000,vars[i]])$p.value<0.05) {
        CI_group<- rbind(CI_group, CI_median_2g(vars[i], strata, data))
      } else {
        CI_group<-  rbind(CI_group, CI_mean_2g(vars[i], strata, data))
      }
  }
  return(CI_group)
}
# CI_for_tableone(c("AGE", "SEX", "TMODE"),"ANGIO",data = dat)
