### Functions and libraries Script
#SET PATH HERE:
data_dir <- 'C:/Users/Jackson_Mejia/Documents/ERM_Practice/'

#suppress library messages
shhh <- suppressPackageStartupMessages # It's a library, so shhh!


shhh(library(dplyr))
shhh(library(data.table))
shhh(library(DescTools))
shhh(library(KFAS))
shhh(library(zoo))
shhh(library(matrixcalc))
shhh(library(ggplot2))
shhh(library(optimx))
shhh(library(reshape2))
shhh(library(tseries))
shhh(library(vars))
shhh(library(MetricsWeighted))
shhh(library(bea.R))
shhh(library(tibble))
shhh(library(plm))



# function to search BEA database to match series code with metric name, table name, etc
bea_string_search <- function(term){
  nipa_ref %>% filter(grepl(term, SeriesCode) | grepl(term, TableName) | grepl(term, LineDescription) | grepl(term, LineNumber) |
                        grepl(term, METRIC_NAME) | grepl(term, CL_UNIT) | grepl(term, UNIT_MULT))
}
#Function that returns the series code of select lines in a table at a given frequency as a character vector. Provide lines as vector
bea_table <- function(TableName, Frequency, Lines){
  if(missing(Lines)){
    paste(bea_string_search(TableName)$SeriesCode, Frequency, sep = "_")
  } else{
    paste(bea_string_search(TableName)$SeriesCode, Frequency, sep = "_")[Lines]
  }
}

#Hodrick-Prescott Filter that deals with missing values
HPFilter = function(x,lambda, na.omit = TRUE) {
  if(na.omit) {
    na_values = is.na(x)
    if(any(na_values)) x = x[-which(na_values)]
  }
  eye <- diag(length(x))
  result <- solve(eye+lambda*crossprod(diff(eye,lag=1,d=2)),x)
  for(idx in which(na_values)) result = append(result, NA, idx - 1) # reinsert NA values
  return(result)
}


##Supply a df with date as the first column, quarterly variables as columns 2:n-1 and column n as the annual column
## to estimate. Ensure that the series begins and ends with a non-NA annual value. Annual value should appear in
## q4 of the relevant year. For example, the annual capital stock for 2019 should be in 2019.75 (2019Q4). If detrend = TRUE, then
# only estimate the detrended series and interpolate the trend series. If flow == TRUE, then estimate a flow variable rather than a stock
kalman_out <- function(df, detrend, flow){# Function follows Ellen R. McGrattan's method from "Intangible Capital"
  if(detrend == TRUE){
    date <- df$date #Save date as a vector to place next to filtered/smoothed values at the end
    Y <- unname(as.matrix(df %>% dplyr::select(-c(1)))) #remove names and date column
    n <- ncol(Y)
    
    #initialize trend matrix
    Ytrend <- matrix(nrow = nrow(Y), ncol = ncol(Y))
    
    #Get trend component of each series after log-transforming
    for(i in 1:(n-1)){
      Ytrend[, i] <- HPFilter(Y[,i], lambda = 1600, na.omit = TRUE)
    }
    Ytrend[,n] <- HPFilter(Y[,n], lambda = 100, na.omit = TRUE)
    
    Y <- Y - Ytrend
    qY_trend <- na.approx(Ytrend) #linearly interpolate to create rough estimate for quarterly values
    
  } else{
    date <- df$date #Save date as a vector to place next to filtered/smoothed values at the end
    Y <- unname(as.matrix(df %>% dplyr::select(-c(1)))) #remove names and date column
  }
  qY <- na.approx(Y)
  
  #create matrix that places lagged values of df next to each other
  big_Y <- unname(cbind(Y, dplyr::lead(Y, n= 1L), dplyr::lead(Y, n= 2L), dplyr::lead(Y, n= 3L)))
  
  ## initialize matrices to create initial estimates following method of McGrattan
  
  n <- ncol(qY) #number of variables
  sumyx <- matrix(0L, nrow = n, ncol = 4*n+1)
  sumxx <- matrix(0L, nrow = 4*n+1, ncol = 4*n+1)
  
  for(i in 5:nrow(qY)){
    Ylag <- rbind(as.matrix(qY[i-1,]),
                  as.matrix(qY[i-2,]),
                  as.matrix(qY[i-3,]),
                  as.matrix(qY[i-4,]),
                  1
    )
    sumyx <- sumyx + as.matrix(qY[i,])%*%t(Ylag)
    sumxx <- sumxx + Ylag%*%t(Ylag)
  }
  
  A <- sumyx %*% solve(sumxx)
  Sig <- matrix(0L, nrow = n, ncol = n)
  
  for(i in 5:nrow(qY)){
    Ylag <- rbind(as.matrix(qY[i-1,]),
                  as.matrix(qY[i-2,]),
                  as.matrix(qY[i-3,]),
                  as.matrix(qY[i-4,]),
                  1
    )
    eps <- qY[i,] - A %*%Ylag
    Sig = Sig + eps %*% (t((eps)/(nrow(qY)-4)))
  }
  A <- A[,1:(4*n)]
  Sig <- t(chol(Sig))
  Theta0 <- rbind(vec(A), vec(Sig))
  
  
  Ct <- diag(4*n) #observation matrix
  A_t <- rbind(matrix(NA, nrow = n, ncol = 4*n), diag(nrow = 3*n, ncol = 4*n)) #state/transition matrix
  B <- rbind(matrix(NA, nrow = n, ncol = n))# variance/covariance matrix for observation state
  
  ss_model <- SSModel(big_Y ~ -1 + SSMcustom(Z = Ct, T = A_t, 
                                             Q = B)) #create state space model object
  
  #write function to be used as input by optim. Each value written as NA is estimated
  #given initial estimate found using method of McGrattan
  objf <- function(pars, model, estimate = TRUE) {
    vect_na <- which(is.na(model$T)) #find NA values
    
    #set initial estimates for state matrix parameters
    for(i in 1:(length(Theta0)- n^2)){
      model$T[vect_na[i]] <- pars[i]
    }
    
    #set initial estimates for state variance/covariance parameters
    for(j in (4*n*n + 1):length(Theta0)){
      model$Q[j - (4*n*n)] <- pars[j]
    }
    
    if (estimate) {
      -logLik(model)
    } else {
      model
    }
  }
  
  #find log-likelihood of parameters
  opt <- suppressWarnings(optimx(par = as.vector(Theta0), fn = objf, method = "nlminb", model = ss_model))
  
  #put optimized parameters back into SS model
  ss_model_opt <- objf(unlist(unname(c(opt[,1:length(Theta0)]))), ss_model, estimate = FALSE)
  #kalman filter/smooth SS model
  kalm <- KFS(ss_model_opt)
  
  #extract smoothed/filtered values for quarterly series from annual series
  kalm_values <- unclass(kalm$alphahat[,n])
  if(detrend == TRUE){
    kalm_values <- kalm_values + qY_trend[,n]
  }
  
  
  #write df with dates and filtered/smoothed values
  bigg <- cbind(date, kalm_values)
  bigg <- as.data.frame(cbind(date, kalm_values)) 
  
  if(flow == TRUE){
    new_colnam <- paste(colnames(df)[df[,ncol(df)]], "KQ", sep = "_")
    colnames(df)[n+1] <- "kalsum"
    bigg <- bigg %>% mutate(year = substr(date, 1, 4)) 
    df <- df %>% mutate(year = substr(date, 1, 4)) %>% group_by(year) %>% summarise(
      sumden = mean(kalsum, na.rm = TRUE)
    ) %>% ungroup
    bigg1 <- bigg %>% group_by(year) %>% summarise(kalmsum = sum(kalm_values, na.rm = TRUE)) %>% ungroup() %>%
      left_join(df, by = c("year"))
    bigg2 <- bigg %>% left_join(bigg1, by = c("year")) %>%
      mutate(kalm_values = (kalm_values/kalmsum)*sumden) %>%
      rename(!!new_colnam := kalm_values) 
  } else{
    new_colnam <- paste(colnames(df)[df[,ncol(df)]], "KQ", sep = "_")
    bigg <- bigg %>% rename(!!new_colnam := kalm_values) %>%
      left_join(df, by = c("date"))
  }
}

#function to extract BEA fixed asset tables
bea_extract <- function(tablename, freq, datasetname){
  #list to input into BEA API function
  FA_spec_list <- list('UserID' = '356252FE-692A-4DA7-B43F-5A36CF6CDAA6',
                       'Method' = 'GetData',
                       'datasetname' = datasetname,
                       'Frequency' = freq,
                       'TableName' = tablename,
                       'Year' = 'ALL')	
  #call BEA table API
  BDT <- as.data.frame(beaGet(FA_spec_list, asTable = TRUE, asWide = TRUE, isMeta = FALSE))
  if(is.data.frame(BDT) && nrow(BDT)==0){ #if df is empty, return df of dates
    if({{freq}} == 'A'){
      year <- (seq(1945, 2020, by = 1))
      BDT <- data.frame(year) %>% rename(date = year)
    } else if({{freq}} == 'Q'){
      year <- (seq(1945, 2020, by = .25))
      BDT <- data.frame(year) %>% rename(date = year)
    }
  } else {
    #transform date colvars from string to numeric and remove character elements
    if({{freq}} == 'A'){
      date <- as.numeric(gsub("DataValue_", "", names(dplyr::select(BDT, contains("Data")))))
      
      BDT <- BDT %>% mutate(SeriesCode = paste(TableName, LineNumber, "A", sep = "_"))
    } else if({{freq}} == 'Q'){ #if quarterly, convert Qs from 19XXQ1 to 19XX.00, etc
      date <- (gsub("DataValue_", "", names(dplyr::select(BDT, contains("Data")))))
      year <- as.numeric(substr(date, start = 1, stop = 4))
      quarter <- quarterly(date)
      date <- as.numeric(paste(year,quarter, sep = ""))
      
      BDT <- BDT %>% mutate(SeriesCode = paste(TableName, LineNumber, "Q", sep = "_"))
    }
    
    # alter SeriesCode so that each BEA table line has a unique name
    
    #initialize vector of colnames to be changed later
    deadcols <- rep(NA, length(unique(BDT$SeriesCode)))
    #create vector of colnames to be changed later (transforming to df in next line causes colnames to be called X1, X2, etc)
    for(i in 1:length(unique(BDT$SeriesCode))){
      deadcols[i] <- paste("X", i, sep = "")
    }
    #transpose BEA table after selecting only numeric variables and removing repeated rows, then rename so that BEA vars are columns
    BDT <- data.frame(t(BDT %>% dplyr::select(SeriesCode, where(is.numeric)) %>%
                          distinct())) %>% rename_at(vars(deadcols), ~ unique(BDT$SeriesCode)) %>% slice(-c(1L)) %>%
      cbind(date) %>% remove_rownames() #add in date vars
  }
}
#function for getting BEA FA metadata
bea_meta <- function(tablename, freq, datasetname){
  
  FA_spec_list <- list('UserID' = '356252FE-692A-4DA7-B43F-5A36CF6CDAA6',
                       'Method' = 'GetData',
                       'datasetname' = datasetname,
                       'Frequency' = freq,
                       'TableName' = tablename,
                       'Year' = 'ALL')	
  
  BDT <- as.data.frame(beaGet(FA_spec_list, asTable = TRUE, asWide = TRUE, isMeta = FALSE))
  #get metadata from BEA table extract
  if(is.data.frame(BDT) && nrow(BDT) != 0){
    BDT <- BDT %>% mutate(SeriesCode = paste(TableName, LineNumber, sep = "_")) %>% # alter SeriesCode so that each BEA table line has a unique name
      dplyr::select((where(is.character)))
  } else { # unless table is empty
    BDT <- data.frame(matrix(ncol=7,nrow=0, dimnames=list(NULL,c("TableName", "SeriesCode", "LineNumber", "LineDescription",
                                                                 "METRIC_NAME", "CL_UNIT", "UNIT_MULT"))))
  }
}


#convert from format 1950Q1 to 1950.00
quarterly <- function(var){
  ifelse(substr(var, start = 5, stop = 6) == "Q1", as.numeric(paste(substr(var, start = 1, stop = 4), ".00", sep = "")), 
         ifelse(substr(var, start = 5, stop = 6) == "Q2", as.numeric(paste(substr(var, start = 1, stop = 4), ".25", sep = "")), 
                ifelse(substr(var, start = 5, stop = 6) == "Q3", as.numeric(paste(substr(var, start = 1, stop = 4), ".50", sep = "")), 
                       as.numeric(paste(substr(var, start = 1, stop = 4), ".75", sep = ""))
         )
         ))
}

#function for setting xrd_sale to NA 
xrd_select <- function(xrdq_adj, saleq_adj, xrd_sale){
  ifelse(!is.na({{xrdq_adj}}/{{saleq_adj}}), {{xrdq_adj}}/{{saleq_adj}}, ifelse(!is.na({{xrd_sale}}), {{xrd_sale}}, NA))
}


#Filtering function
upper_lower_cut <- function(df, var, upper, lower, na.rm){
  df %>%
    filter({{var}} >= quantile({{var}}, lower, na.rm = TRUE) &
             {{var}} <= quantile({{var}}, upper, na.rm = TRUE)
           )
}


clean_naics <- function(naicsh){ #Function to Assign all subsets of NAICs codes to the parent NAICS code
  nu_naics <- ifelse(naicsh == 33 | naicsh == 32, 31,
                     ifelse(naicsh == 45, 44,
                            ifelse(naicsh == 49, 48, naicsh)))
}

#Function for when we crosswalk NAICS codes for prior to 1985. If a native code exists, use that,
# otherwise usethe code we generate
naic_all <- function(naics1, naics2){
  naics <- ifelse(!is.na(naics1), naics1, ifelse(!is.na(naics2), naics2, NA))
}

#Sets priority for which date to use for firm birth year. If Ritter date is available, use that
# otherwise first year in Compustat with final stock price, otherwise Compustat ipodate,
#otherwise first date in Compustat
clean_ipo_date <- function(ipo_1,ipodate, ipo_3){
  ipoyr <- ifelse(!is.na(ipo_1), ipo_1, 
                  ifelse(!is.na(ipodate), ipodate, 
                         ifelse(!is.na(ipo_3), ipo_3, NA)))
}

#calculate effective rate
eff_rate <- function(pi, txt){
  ifelse(pi != 0 & !is.na(pi) & !is.na(txt), txt/pi, NA)
}


#function for interpolating
interp_df <- function(df, var){
  df %>% group_by(gvkey) %>%
    arrange(fyear, .by_group = TRUE) %>%
    mutate(across(c(all_of(var)), .fns = list(interp = ~ na.approx(., maxgap = max_gap, na.rm = FALSE)))) %>%
    ungroup()
}

## Function for eliminating NAs for particular variables and specifying the minimum number of consecutive years
na_filter_a <- function(df, navars){ 
  df %>% 
    filter_at(vars(all_of(navars)),all_vars(!is.na(.))) %>%
    group_by(gvkey) %>%
    arrange(fyear, .by_group = TRUE) %>%
    #filter(n() >= n_observ) %>% 
    ungroup() %>%
    group_by(gvkey, grp = cumsum(c(1, diff(fyear) != 1))) %>% 
    filter(n() >= yrs_invest) %>% ungroup()
}

#function for adding logs and growth rates
log_growth_df <- function(df, datevar, logvars, growthvars){
  #df %>% #filter_at(logvars, all_vars(. > 0) ) %>%
    df2 <- df %>% filter_at(logvars, all_vars(. > 0 & !is.na(.)) ) %>%
      dplyr::select(c(gvkey, datevar, logvars)) %>%
    group_by(gvkey) %>%
    mutate(across(c(all_of(logvars)), list(log = ~case_when(. >= 0  & !is.na(.) ~log(.))))) %>%
    mutate(across(c(all_of(growthvars)), list(growth = ~case_when(!is.na(.) & !is.na(dplyr::lag(., n = 1L)) ~. - dplyr::lag(., n = 1L))))) %>%
    ungroup() %>%
      dplyr::select(-c(logvars))
    df <- df %>% left_join(df2, by = c("gvkey", datevar))
  # mutate_at(logvars, funs(log = log(.))) %>%
  #mutate_at(growthvars, funs(growth = . - dplyr::lag(., n = 1L))) %>% ungroup()
}

## Function for eliminating NAs for particular variables and specifying the minimum number of consecutive years (for quarterly series)
 na_filter_q <- function(df, navars){ 
   df %>% 
     filter_at(vars(all_of(navars)),all_vars(!is.na(.))) %>%
     group_by(gvkey) %>%
     arrange(date, .by_group = TRUE) %>%
     #filter(n() >= n_observ) %>% 
     ungroup() %>%
     group_by(gvkey, grp = cumsum(c(1, diff(date) != .25))) %>% 
     filter(n() >= yrs_invest) %>% ungroup()
 }


#function convert to Q1, Q2, etc
convert_quarterly <- function(var){
  ifelse(substr(var, start = 5, stop = 7) == ".25", "Q2", 
         ifelse(substr(var, start = 5, stop = 7) == ".5", "Q3", ifelse(
           substr(var, start = 5, stop = 7) == ".75", "Q4", "Q1"
         )
         ))
}

#function for interpreting ytd variables as quarterly
ytd <- function(df, var_l){
  df %>% group_by(tic, fyearq) %>%
    mutate(across(c(all_of(var_l)), .fns = list(q = ~. - dplyr::lag(., n = 1L, default = 0)))) %>% ungroup()
  # mutate_at(var_l, funs(q = . - dplyr::lag(., n = 1L, default = 0)))
}

# plotting function for comparing compustat and bea investment rates
invt_plot <- function(df, comp_rate, bea_rate, rate_type){
  ggplot(df, aes(x = Date)) +
    geom_line(aes(y = {{comp_rate}}, colour = "Compustat Investment Rate"), size = 1.25) +
    geom_line(aes(y = {{bea_rate}}*1, colour = "Aggregate Investment Rate"), size = 1.25) +
    #scale_y_continuous(sec.axis = sec_axis(~./1, name = "Aggregate Investment Rate")) +
    #scale_colour_manual(values = c("blue", "red")) +
    labs(y = ifelse(rate_type == "Net", "Net Investment Rate", "Gross Investment Rate"),
         x = "Year",
         colour = "Parameter") + 
    theme_minimal() +
    theme(legend.position = "bottom") +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
    theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10))) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(panel.border = element_blank(), axis.line = element_line()) + 
    theme(legend.title = element_blank())
}



#Function for running VAR and extracting IRF. Takes a TS with variables and a string for the response variable in IRF
run_var_irf <- function(v, invt, nahead){
  Model1 <- VAR(v, p = 12, type = "const", season = NULL, exog = NULL)   #VAR of 12 lags
  # IRF with a tax impulse
  irf <- irf(Model1, impulse = "TAX", response = invt, ortho = FALSE, cumulative = FALSE,  n.ahead = nahead, boot = TRUE, runs = 200, ci = 0.68)
}

# Function for converting IRF from list to DF so that it plays nice with ggplot2. takes irf, string of impulse being used, and string of frequency ("A" or "Q")
extract_varirf <- function(irf, tax, freq){
  period <- seq(0, nrow(irf$irf[[1]]) - 1)
  col2 <- c(irf$irf[[1]])
  col3 <- c(irf$Lower[[1]])
  col4 <- c(irf$Upper[[1]])
  shock_var <- rep(tax, nrow(irf$irf[[1]]))
  frequency <- rep(freq, nrow(irf$irf[[1]]))
  deadcols <- c("col2", "col3", "col4")
  new_names <- c(paste("irf", irf$response, sep = "_"), paste("irf", irf$response, "lower", sep = "_"),
                 paste("irf", irf$response, "upper", sep = "_"))
  df <- data.frame(period, col2, col3, col4, shock_var, frequency) %>% rename_at(vars(deadcols), ~ (new_names))
}

#function to plot IRF as ggplot in for loop below. Takes impulse response function object which contains periods, central measure, and error bounds as well as the impulse response variable as string
plot_irf <- function(irf, invt, tax){
  cname <- names(irf) 
  cname[2] <- "irf_central" 
  cname[3] <- "irf_lower"
  cname[4] <- "irf_upper"
  colnames(irf) <- cname #rename to have general column names for aesthetics below
  irf %>% 
    ggplot(aes(x=period, y=irf_central, ymin=irf_lower, ymax=irf_upper)) +
    geom_hline(yintercept = 0, color="red") +
    geom_ribbon(fill="grey", alpha=0.2) +
    geom_line(size = 1.1) +
    theme_light() +
    ggtitle(paste(tax, invt, sep = " shock to "))+ #make impulse response variable as title
    ylab("Percent")+
    xlab("Periods Ahead") +
    theme(plot.title = element_text(size = 11, hjust=0.5),
          axis.title.y = element_text(size=11)) + theme_minimal()
}


#function that takes impulse response function df and outputs the peak (minimum) value and period in which it occurs
peak_finder <- function(irf){
  cname <- names(irf)
  cname[2] <- "irf_central" 
  colnames(irf) <- cname # rename central tendency in IRF to have a general name to make coding easier in a for loop
  irf <- irf %>% dplyr::select(-matches("lower"), -matches("upper")) %>% # remove extraneous columns
    group_by(irf_central) %>% slice_min(irf_central) %>% ungroup() %>% slice_head(n = 1) #get min value by central IRF tendency and keep only the row with min value and period
}


#Function to create two- or three-variable timeseries df with tax ordered first, then log GDP (if GDP == TRUE), then the log of investment var (unless investment is a rate)
var_ts <- function(invt, tax, method, freq, GDP, trans_negs){ # Method == 1 indicates investment should be logged, freq == 1 indicates annual series
  if(freq == 1){
    if(method == 1){
      invt_var <- romerdata_a %>% dplyr::select(one_of(invt))
      invt_var[invt_var == 0] <- NA
      invt_var <- 100*log(invt_var) #for level variables--take log to get response in percent
      ts_invt <- ts(invt_var, start = c(romerdata_a$Date[1]), frequency = freq) #create TS to start at first non-NA value
    } else if (method == 0) {
      invt_var <- romerdata_a %>%  dplyr::select(one_of(invt))
      ts_invt <- ts(invt_var, start = c(romerdata_a$Date[1]), frequency = freq) #create TS to start at first non-NA value
    }
    tax1 <-  romerdata_a %>% dplyr::select(c(tax))
    TAX <- ts(tax1, start = c(romerdata_a$Date[1]), frequency = 1) #create TS with start date as first non-NA value
    if(invt != "RA_T10103_1" & GDP == TRUE){
      gdpt <- 100*log(romerdata_a %>% dplyr::select(c(RA_T10103_1)))
      GDP <- ts(gdpt, start = c(romerdata_a$Date[1]), frequency = 1 )
      v1 <- na.omit(cbind(TAX,ts_invt, GDP))
    } else{
      v1 <- na.omit(cbind(TAX, ts_invt))
    }
  }else{
    if(method == 1){
      invt_var <- romerdata_q %>% dplyr::select(one_of(invt))
      if(trans_negs == TRUE & any(invt_var < 0)){
        min_val <- invt_var %>% summarise(min_val = min(., na.rm = TRUE))
        invt_var <- invt_var + abs(min_val[1,]) + 1
      } else{
        invt_var <- invt_var
      }
      invt_var[invt_var == 0] <- NA
      invt_var <- 100*log(invt_var) #for level variables--take log to get response in percent
      ts_invt <- ts(invt_var, start = c(romerdata_q$Date[1]), frequency = 4) #create TS to start at first non-NA value
    } else if (method == 0) {
      invt_var <- romerdata_q %>%  dplyr::select(one_of(invt))
      ts_invt <- ts(invt_var, start = c(romerdata_q$Date[1]), frequency = 4) #create TS to start at first non-NA value
    }
    tax1 <-  romerdata_q %>% dplyr::select(c(tax))
    TAX <- ts(tax1, start = c(romerdata_q$Date[1]), frequency = 4) #create TS with start date as first non-NA value
    if(invt != "GDP" & GDP == TRUE){
      gdpt <- 100*log(romerdata_q %>% dplyr::select(c(GDP)))
      GDP <- ts(gdpt, start = c(romerdata_q$Date[1]), frequency = 4 )
      v1 <- na.omit(cbind(TAX, GDP, ts_invt))
    } else{
      v1 <- na.omit(cbind(TAX, ts_invt))
    }
  }
  #colnames(v1)[colnames(romerda) == "ts_invt"] <- invt
}



# create time series for impulse response variables (levels). takes df that contains variables and the variable in question as string. Also takes method as numeric (1 or 0) corresponding to log or level transformation and frequency as numeric (4 = Q, 1 = A)
invt_ts <- function(df, invt, tax, method, freq){
  df <- df %>% filter(!is.na(!!sym(invt)) & !is.na(!!sym(tax))) #remove NA values so that start date is correct
  if(method == 1){ #take log if method is 1 otherwise leave alone
    invt_var <- 100*log(df %>% dplyr::select(one_of(invt))) #for level variables--take log to get response in percent
    invt_var[invt_var == -Inf] <- 0
    
  } else if (method == 0) {
    invt_var <- df %>%  
      dplyr::select(one_of(invt))
  }
  ts_invt <- ts(invt_var, start = c(df$Date[1]), frequency = freq) #create TS to start at first non-NA value
}

# create time series for tax vars. Takes df and IRF var and tax var as strings. Also takes method as numeric (1 or 0) corresponding to log or level transformation and frequency as numeric (4 = Q, 1 = A)
tax_ts <- function(df, invt, tax, freq){
  df <- df %>% filter(!is.na(!!sym(invt)) & !is.na(!!sym(tax))) # have to convert string to symbol to evaluate. Remove NA values so that VAR works
  tax1 <-  df %>% dplyr::select(c(tax))
  TAXts <- ts(tax1, start = c(df$Date[1]), frequency = freq) #create TS with start date as first non-NA value
}

## Computes KZ and HP indices for annual frequency (both) and quarterly frequency (HP)
constrained_index <- function(df, frequency){
  if(frequency == "A"){
    df <- df %>% mutate(kz_index = -1.001909*(ib+dp)/dplyr::lag(ppent, n = 1L) + 0.2826389*(at + prcc_f*csho -ceq -txdb)/at + 3.139193*((dltt + dlc)/(dltt + dlc + seq)) - 39.3678*((dvc + dvp)/dplyr::lag(ppent, n = 1L)) - 1.314759*(che/dplyr::lag(ppent, n = 1L)),
                      hp_index = -0.737*log(ifelse(at < 4500, at, 4500)) + 0.043*(log(ifelse(at < 4500, at, 4500)))^2 - 0.040*ifelse(age < 37, age, 37),
                      kz_index = ifelse(is.infinite(kz_index), NA, kz_index))
    df2 <- df %>% group_by(fyear) %>% summarise(kz_threshold = quantile(kz_index, .66, na.rm = TRUE),
                                              hp_threshold = quantile(hp_index, .66, na.rm = TRUE))
    df <- df %>% left_join(df2, by = c("fyear")) %>%
    mutate(kz_con = ifelse(dplyr::lag(kz_index, n = 1L) > dplyr::lag(kz_threshold, n = 1L), 1, 0),
           hp_con = ifelse(dplyr::lag(hp_index, n = 1L) > dplyr::lag(hp_threshold, n = 1L), 1, 0))
  } else{
    df <- df %>% mutate(hp_index = -0.737*log(ifelse(atq_adj_interp < 4500, atq_adj_interp, 4500)) + 0.043*(log(ifelse(atq_adj_interp < 4500, atq_adj_interp, 4500)))^2 - 0.040*ifelse(age < 37, age, 37))
    
    df2 <- df %>% dplyr::select(c(kz_index, fyearq)) %>% group_by(fyearq) %>% summarise(kz_threshold = quantile(kz_index, .66, na.rm = TRUE)) %>% ungroup()
    df3 <- df %>% dplyr::select(c(hp_index, date)) %>% group_by(date) %>% summarise(hp_threshold = quantile(hp_index, .66, na.rm = TRUE)) %>% ungroup()
    df <- df %>% dplyr::select(-c(kz_threshold)) %>% left_join(df2, by = c("fyearq")) %>% left_join(df3, by = c("date")) %>%
      mutate(hp_con = ifelse(dplyr::lag(hp_index, n = 1L) > dplyr::lag(hp_threshold, n = 1L), 1, 0)) %>% group_by(gvkey, fyearq) %>%
      mutate(kz_con = ifelse(dplyr::lag(kz_index, n = 1L) > dplyr::lag(kz_threshold, n = 1L), 1, 0)) %>% ungroup()
    }
  
  
}

# Function to categorize firm as high roa or not (1 or 0) based on prior period threshold
high_roa <- function(df, roa, datevar, threshold){
  df2 <- df %>% group_by({{datevar}}) %>% summarise(roa_thresh = quantile(roa, threshold, na.rm = TRUE))
  df <- df %>% left_join(df2) %>% mutate(high_roa = ifelse(roa > dplyr::lag(roa_thresh, n = 1L), 1, 0))
}

## Function to plot multiple IRFs on top of each other. If Conf_Int == TRUE, then confidence bands are also plotted
multi_irf_plots <- function(plot_vars, Conf_Int, Title){ # takes a vector of strings where each string corresponds to a dependent variable appended with the index of the desired tax variable. Eg, if we want fixed investment with T_CI and T_CI is the second tax variable, then it would be FI_2
  irfdata <- data.frame(matrix(ncol = 5, nrow = 0, dimnames = list(NULL, c("time", "y", "ymin", "ymax", "group"))))
  for(i in 1:length(plot_vars)){ 
    # test if data is annual. if it is, linearly interpolate so that it is on the same scale
    if( length(ggplot_build(irf_plots[[match(plot_vars[i], invt_vars_exp)]])$data[[3]]$y) < 20 ){
      time2 <- data.frame(seq(0,20,1))
      colnames(time2) <- "time"
      time <- seq(0,20,4)
      y <- ggplot_build(irf_plots[[match(plot_vars[i], invt_vars_exp)]])$data[[3]]$y
      ymin <- ggplot_build(irf_plots[[match(plot_vars[i], invt_vars_exp)]])$data[[3]]$ymin
      ymax <- ggplot_build(irf_plots[[match(plot_vars[i], invt_vars_exp)]])$data[[3]]$ymax
      time2 <- time2 %>% left_join(data.frame(time, y, ymin, ymax)) %>%
        mutate(y = na.approx(y),
               ymin = na.approx(ymin),
               ymax = na.approx(ymax),
               variable = paste(plot_vars[i]))
      irfdata <- irfdata %>% rbind(time2)
    } else {
      irftime <- seq(0,20,1)
      y <- ggplot_build(irf_plots[[match(plot_vars[i], invt_vars_exp)]])$data[[3]]$y
      ymin <- ggplot_build(irf_plots[[match(plot_vars[i], invt_vars_exp)]])$data[[3]]$ymin
      ymax <- ggplot_build(irf_plots[[match(plot_vars[i], invt_vars_exp)]])$data[[3]]$ymax
      irft <- data.frame(irftime, y, ymin, ymax)
      irft <- irft %>% rename(time = irftime) %>% mutate(variable = paste(plot_vars[i]))
      irfdata <- irfdata %>% rbind(irft)
    }
  }
  if(Conf_Int == TRUE){
  irfdata %>% ggplot(aes(time, y, fill = variable, colour = variable)) + 
    geom_ribbon(aes(ymin = ymin, ymax = ymax, group = variable),
                alpha = 0.2, linetype = 0) + geom_line(size = 1.1) + 
    geom_hline(yintercept = 0, color="black", size = 1.25) + ylab("Percent") + theme_minimal() +
    theme(legend.position="bottom") + theme(panel.grid.major = element_blank()) + ggtitle(Title)
  } else{
    irfdata %>% ggplot(aes(time, y, fill = variable, colour = variable)) + 
      geom_line(size = 1.1) + 
      geom_hline(yintercept = 0, color="black", size = 1.25) + ylab("Percent") + theme_minimal() +
      theme(legend.position="bottom") + theme(panel.grid.major = element_blank() ) + ggtitle(Title)
  }
}

## FUnction for plotting two jorda IRFs on top of each other. Takes an lpirfs object with firm controls and one without
jordadf <- function(jorda1, jorda2, period_ahead, Title, flip){
  quarters <- seq(from = 0,to = period_ahead, by = 1)
  if(flip == TRUE){
    y <- -1*c(0, as.vector(jorda1[[1]]))
    ymin <- -1*c(0, as.vector(jorda1[[2]]))
    ymax <- -1*c(0, as.vector(jorda1[[3]]))
    variable = rep("No Controls", period_ahead + 1)
    df1 <- data.frame(quarters, y, ymin, ymax, variable)
    
    quarters <- seq(from = 0,to = period_ahead, by = 1)
    y <- -1*c(0, as.vector(jorda2[[1]]))
    ymin <- -1*c(0, as.vector(jorda2[[2]]))
    ymax <- -1*c(0, as.vector(jorda2[[3]]))
    variable = rep("Firm Controls", period_ahead + 1)
    df2 <- data.frame(quarters, y, ymin, ymax, variable)
    df <- rbind(df1, df2)
  } else{
  y <- c(0, as.vector(jorda1[[1]]))
  ymin <- c(0, as.vector(jorda1[[2]]))
  ymax <- c(0, as.vector(jorda1[[3]]))
  variable = rep("No Controls", period_ahead + 1)
  df1 <- data.frame(quarters, y, ymin, ymax, variable)
  
  quarters <- seq(from = 0,to = period_ahead, by = 1)
  y <- c(0, as.vector(jorda2[[1]]))
  ymin <- c(0, as.vector(jorda2[[2]]))
  ymax <- c(0, as.vector(jorda2[[3]]))
  variable = rep("Firm Controls", period_ahead + 1)
  df2 <- data.frame(quarters, y, ymin, ymax, variable)
  df <- rbind(df1, df2)
  }
  df %>% ggplot(aes(x = quarters, y, fill = variable, colour = variable)) + 
    geom_ribbon(aes(ymin = ymin, ymax = ymax, group = variable),
                alpha = 0.2, linetype = 0) + geom_line(size = 1.1) + 
    geom_hline(yintercept = 0, color="black", size = 1.25) + ylab("Percent") + theme_minimal() +
    theme(legend.position="bottom") + 
    theme(legend.title = element_blank()) + ggtitle(Title) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead")
}

jorda_split <- function(jorda1, jorda2, period_ahead, Title){
  quarters <- seq(from = 0,to = period_ahead, by = 1)
    y <- c(0, as.vector(jorda1[[1]]))
    ymin <- c(0, as.vector(jorda1[[2]]))
    ymax <- c(0, as.vector(jorda1[[3]]))
    variable = rep("Positive Shock", period_ahead + 1)
    df1 <- data.frame(quarters, y, ymin, ymax, variable)
    
    quarters <- seq(from = 0,to = period_ahead, by = 1)
    y <- -1*c(0, as.vector(jorda2[[1]]))
    ymin <- -1*c(0, as.vector(jorda2[[2]]))
    ymax <- -1*c(0, as.vector(jorda2[[3]]))
    variable = rep("Negative Shock", period_ahead + 1)
    df2 <- data.frame(quarters, y, ymin, ymax, variable)
    df <- rbind(df1, df2)

  df %>% ggplot(aes(x = quarters, y, fill = variable, colour = variable)) + 
    geom_ribbon(aes(ymin = ymin, ymax = ymax, group = variable),
                alpha = 0.2, linetype = 0) + geom_line(size = 1.1) + 
    geom_hline(yintercept = 0, color="black", size = 1.25) + ylab("Percent") + theme_minimal() +
    theme(legend.position="bottom") + 
    theme(legend.title = element_blank()) + ggtitle(Title) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead")
}
