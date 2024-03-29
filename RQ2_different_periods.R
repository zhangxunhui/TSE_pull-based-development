library(RMySQL)
library(yaml)
library(plyr)
library(pROC)
library("rjson")
library("stringr")
require(lme4)
library(dplyr)
library(readr)

args<-commandArgs(TRUE)
which_set = args[1] # should be high, mid, low
if ((which_set != "high" & which_set != "mid" & which_set != "low")) {
    print("error with the parameter, should be high/mid/low")
    print(which_set)
    quit()
}



# read common project ids
common_project_ids = read.csv("different_periods_common_proj_ids.csv")
common_project_ids = common_project_ids$x


# read all the project ids from new_pullreq
dbConfig <- yaml.load_file('R/config.yaml')
db_user <- dbConfig$mysql$user
db_password <- dbConfig$mysql$passwd
db_name <- dbConfig$mysql$db
db_host <- dbConfig$mysql$host # for local access
db_port <- as.numeric(dbConfig$mysql$port)
conn <- dbConnect(MySQL(), user = db_user, password = db_password,
                dbname = db_name, host = db_host, port = db_port)



# build up models for each set
# read factor type
factor_type = fromJSON(file = "factor_type.json")
categorical_factors = factor_type$categorical_factors
binary_factors = factor_type$binary_factors

# define some processing functions
convertData_preprocess <- function(df, exclude_cols = c()) {
  # standardize all the independent factors
  for (coln in colnames(df)) {
    if ((!coln %in% categorical_factors) & (!coln %in% exclude_cols)) {
      print(paste(coln, "in categorical factor list"))
      # firstly log transfor those continuous variables
      df[[coln]] <- log(df[[coln]] + 1) # log the value
    }
    if (!coln %in% exclude_cols) {
      # firstly convert to numeric values
      df[[coln]] = as.numeric(df[[coln]])
      df[[coln]] = scale(df[[coln]])
    }
  }
  df
}

convertData_string <- function(df_project) {
    df_project[df_project == 'success'] = 1
    df_project[df_project == 'failure'] = 0
    df_project[df_project == 'male'] = 1
    df_project[df_project == 'female'] = 0

    df_project
}

# remove those outliers
rmout = function(df) {
  df = df[(df[['project_id']] %in% common_project_ids),]
  return(df)
}

# read all the data
q = paste("select * from new_pullreq", sep="")
rs <- dbSendQuery(conn, q)
df <- fetch(rs, n = -1)

df = rmout(df)

research_factors = fromJSON(file = "keep_factors.json", sep=""))
keep_factors = research_factors$basic_model

df = df[c(keep_factors, "merged_or_not", "project_id", "creator_id", "last_closer_id", "last_close_time")]

df = na.omit(df)
# preprocess & type convert
df = convertData_string(df)
df = convertData_preprocess(df, exclude_cols = c("merged_or_not", "project_id", "creator_id", "last_closer_id", "last_close_time"))
df[['merged_or_not']] = as.factor(df[['merged_or_not']]) # do not convert to logical factor & only convert merged_or_not
df$last_close_time = as.Date(df$last_close_time)
# how many columns left
colns = colnames(df)
print(paste("df rows: ", nrow(df), sep=""))



formula_project_part = "sloc+team_size+test_lines_per_kloc+stars+project_age+open_issue_num+open_pr_num+pr_succ_rate+pushed_delta+integrator_availability"
formula_contrib_part = "followers+first_pr+account_creation_days+core_member+contrib_gender+contrib_open+contrib_cons+contrib_extra+contrib_agree+contrib_neur+prev_pullreqs"
formula_inte_part = "prior_review_num+inte_open+inte_cons+inte_extra+inte_agree+inte_neur"
formula_relation_part = "same_user"
formula_pull_request = "lifetime_minutes+num_commits+src_churn+files_added+files_deleted+files_changed+friday_effect+reopen_or_not+commits_on_files_touched+hash_tag+test_inclusion+description_length+ci_exists+has_comments+comment_conflict+num_comments+other_comment+test_churn"

if (which_set == "high") {
    # 1. get pull requests for the last period
    # 2018-06-01 ~ 2019-06-01
    selected_df = df[((df$last_close_time>as.Date('2018-06-01')) & (df$last_close_time<=as.Date('2019-06-01'))),]
    print(dim(selected_df))
    formula = paste("merged_or_not~", paste(c(formula_pull_request, formula_contrib_part, formula_inte_part, formula_relation_part, formula_project_part), collapse="+"), sep="")
    formula = paste(formula, "+(1|project_id)", sep="")
    print(formula)
    model_path = "after2018_model.RData"
    print(model_path)
    nlopt <- function(par, fn, lower, upper, control) {
        .nloptr <<- res <- nloptr(par, fn, lb = lower, ub = upper, 
            opts = list(algorithm = "NLOPT_LN_BOBYQA", print_level = 1,
            maxeval = 1000, xtol_abs = 1e-6, ftol_abs = 1e-6))
        list(par = res$solution,
            fval = res$objective,
            conv = if (res$status > 0) 0 else res$status,
            message = res$message
        )
    }
    model_high <- glmer(formula=formula
                , family = binomial,
                verbose=TRUE,
                data=selected_df,
                control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE))
    save(model_high, file=model_path)
    print("finish build up model for the last period")
} else if (which_set == "mid") {
    # 2. 2016-06-01~2018-06-01
    selected_df = df[((df$last_close_time>as.Date('2016-06-01')) & (df$last_close_time<=as.Date('2018-06-01'))),]
    print(dim(selected_df))
    formula = paste("merged_or_not~", paste(c(formula_pull_request, formula_contrib_part, formula_inte_part, formula_relation_part, formula_project_part), collapse="+"), sep="")
    formula = paste(formula, "+(1|project_id)", sep="")
    print(formula)
    model_path = "2016-2018_model.RData"
    print(model_path)
    nlopt <- function(par, fn, lower, upper, control) {
        .nloptr <<- res <- nloptr(par, fn, lb = lower, ub = upper, 
            opts = list(algorithm = "NLOPT_LN_BOBYQA", print_level = 1,
            maxeval = 1000, xtol_abs = 1e-6, ftol_abs = 1e-6))
        list(par = res$solution,
            fval = res$objective,
            conv = if (res$status > 0) 0 else res$status,
            message = res$message
        )
    }
    model_mid <- glmer(formula=formula
                , family = binomial,
                verbose=TRUE,
                data=selected_df,
                control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE))
    save(model_mid, file=model_path)
    print("finish build up model_mid")
} else {
    # 3. ~2016-06-01
    selected_df = df[(df$last_close_time<=as.Date('2016-06-01')),]
    print(dim(selected_df))
    formula = paste("merged_or_not~", paste(c(formula_pull_request, formula_contrib_part, formula_inte_part, formula_relation_part, formula_project_part), collapse="+"), sep="")
    formula = paste(formula, "+(1|project_id)", sep="")
    print(formula)
    model_path = "before2016_model.RData"
    print(model_path)
    nlopt <- function(par, fn, lower, upper, control) {
        .nloptr <<- res <- nloptr(par, fn, lb = lower, ub = upper, 
            opts = list(algorithm = "NLOPT_LN_BOBYQA", print_level = 1,
            maxeval = 1000, xtol_abs = 1e-6, ftol_abs = 1e-6))
        list(par = res$solution,
            fval = res$objective,
            conv = if (res$status > 0) 0 else res$status,
            message = res$message
        )
    }
    model_low <- glmer(formula=formula
                , family = binomial,
                verbose=TRUE,
                data=selected_df,
                control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE))
    save(model_low, file=model_path)
    print("finish build up model_low")
}