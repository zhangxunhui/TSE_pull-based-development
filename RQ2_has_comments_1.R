library(RMySQL)
library(yaml)
library(plyr)
library(pROC)
library("rjson")
library("stringr")
require(lme4)
library(dplyr)


# read factor type
factor_type = fromJSON(file = "factor_type.json")
categorical_factors = factor_type$categorical_factors
binary_factors = factor_type$binary_factors

convertData_string <- function(df_project) {
  df_project[df_project == 'success'] = 1
  df_project[df_project == 'failure'] = 0
  df_project[df_project == 'male'] = 1
  df_project[df_project == 'female'] = 0

  df_project
}

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

# read data from mysql database
dbConfig <- yaml.load_file('R/config.yaml')
db_user <- dbConfig$mysql$user
db_password <- dbConfig$mysql$passwd
db_name <- dbConfig$mysql$db
db_host <- dbConfig$mysql$host # for local access
db_port <- as.numeric(dbConfig$mysql$port)
conn <- dbConnect(MySQL(), user = db_user, password = db_password,
                dbname = db_name, host = db_host, port = db_port)
q = paste("select * from new_pullreq where last_close_time > '", "2013-08-01 00:00:00", "' order by last_close_time desc", sep="")
rs <- dbSendQuery(conn, q)
df <- fetch(rs, n = -1)

# remove those outliers
rmout = function(df) {
df = df[!(df[["has_comments"]]==0),]
return(df)
}

# read research factors
research_factors = fromJSON(file = "keep_factors.json")
keep_factors = research_factors$basic_model

df = rmout(df)
selected_df = df[c(keep_factors, "merged_or_not", "project_id", "creator_id", "last_closer_id")]
selected_df = na.omit(selected_df)
# preprocess & type convert
selected_df = convertData_string(selected_df)
selected_df = convertData_preprocess(selected_df, exclude_cols = c("merged_or_not", "project_id", "creator_id", "last_closer_id"))
selected_df[['merged_or_not']] = as.factor(selected_df[['merged_or_not']])
selected_df = selected_df[c(keep_factors, "merged_or_not", "project_id", "creator_id", "last_closer_id")]
colns = colnames(selected_df)
print(paste("selected_df rows: ", nrow(selected_df), sep=""))

formula_project_part = "sloc+team_size+test_lines_per_kloc+stars+project_age+open_issue_num+open_pr_num+pr_succ_rate+pushed_delta+integrator_availability"
formula_contrib_part = "prev_pullreqs+followers+first_pr+account_creation_days+core_member+contrib_gender+contrib_open+contrib_cons+contrib_extra+contrib_agree+contrib_neur"
formula_inte_part = "prior_review_num+inte_open+inte_cons+inte_extra+inte_agree+inte_neur"
formula_relation_part = "same_user"
formula_pull_request = "lifetime_minutes+num_commits+src_churn+files_added+files_deleted+files_changed+friday_effect+reopen_or_not+commits_on_files_touched+hash_tag+test_inclusion+description_length+ci_exists+test_churn"
formula = paste("merged_or_not~", paste(c(formula_pull_request, formula_contrib_part, formula_inte_part, formula_relation_part, formula_project_part), collapse="+"), sep="")
formula = paste(formula, "+(1|project_id)", sep="")

model_path = "has_comments_1.RData"
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
model5_has_comments_1 <- glmer(formula=formula
            , family = binomial,
            verbose=TRUE,
            data=selected_df,
            control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE))
save(model5_has_comments_1, file=model_path)
print("finish build up model5_has_comments_1")
