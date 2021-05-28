# consider only those has comment pull request
# contrib_comment
# inte_comment
# ci_exists
# same_user

library(RMySQL)
library(yaml)
library(plyr)
library(pROC)
library("rjson")
library("stringr")
require(lme4)
library(dplyr)


args<-commandArgs(TRUE)
which_model = args[1]
if ((which_model != "has_comments" & which_model != "contrib_comment" & which_model != "inte_comment" & which_model != "diff_user" & which_model != "use_ci")) {
    print("error with the parameter, should be...")
    print(which_model)
    quit()
}

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
  for (coln in colnames(df)) {
    if ((!coln %in% categorical_factors) & (!coln %in% exclude_cols)) {
      print(paste(coln, "in categorical factor list"))
      df[[coln]] <- log(df[[coln]] + 1)
    }

    if (!coln %in% exclude_cols) {
      df[[coln]] = as.numeric(df[[coln]])
      df[[coln]] = scale(df[[coln]])
    }
  }
  df
}

# read data from mysql database
dbConfig <- yaml.load_file('config.yaml')
db_user <- dbConfig$mysql$user
db_password <- dbConfig$mysql$passwd
db_name <- dbConfig$mysql$db
db_host <- dbConfig$mysql$host # for local access
db_port <- as.numeric(dbConfig$mysql$port)
conn <- dbConnect(MySQL(), user = db_user, password = db_password,
                dbname = db_name, host = db_host, port = db_port)
q = paste("select * from new_pullreq where last_close_time > '", "2013-08-01 00:00:00", "'", sep="")
rs <- dbSendQuery(conn, q)
df <- fetch(rs, n = -1)


if(which_model == 'has_comments') {
  print(which_model)
  rmout = function(df) {
    df = df[!(df[["has_comments"]]==0),]
    return(df)
  }
  # self define keep factors
  keep_factors = c("sloc", "team_size", "test_lines_per_kloc", "stars", "project_age", "open_issue_num", "open_pr_num", "pr_succ_rate", "pushed_delta", "integrator_availability", "followers", "first_pr", "account_creation_days", "core_member", "contrib_gender", "contrib_open", "contrib_cons", "contrib_extra", "contrib_agree", "contrib_neur", "prior_review_num", "inte_open", "inte_cons", "inte_extra", "inte_agree", "inte_neur", "lifetime_minutes", "num_commits", "src_churn", "files_added", "files_deleted", "files_changed", "commits_on_files_touched", "friday_effect", "reopen_or_not", "hash_tag", "test_inclusion", "description_length", "ci_exists", "same_user", "first_response_time", "perc_pos_emotion", "perc_neg_emotion", "num_comments", "other_comment", "prev_pullreqs", "comment_conflict", "test_churn")

  print(paste(keep_factors, collapse=', '))
  df = rmout(df)
  selected_df = df[c(keep_factors, "merged_or_not", "project_id", "creator_id", "last_closer_id")]
  selected_df = na.omit(selected_df)
  selected_df = convertData_string(selected_df)
  selected_df = convertData_preprocess(selected_df, exclude_cols = c("merged_or_not", "project_id", "creator_id", "last_closer_id"))
  selected_df[['merged_or_not']] = as.factor(selected_df[['merged_or_not']])
  selected_df = selected_df[c(keep_factors, "merged_or_not", "project_id", "creator_id", "last_closer_id")]
  # how many columns left
  colns = colnames(selected_df)
  print(paste("selected_df rows: ", nrow(selected_df), sep=""))

  formula_project_part = "sloc+team_size+test_lines_per_kloc+stars+project_age+open_issue_num+open_pr_num+pr_succ_rate+pushed_delta+integrator_availability"
  formula_contrib_part = "prev_pullreqs+followers+first_pr+account_creation_days+core_member+contrib_gender+contrib_open+contrib_cons+contrib_extra+contrib_agree+contrib_neur"
  formula_inte_part = "prior_review_num+inte_open+inte_cons+inte_extra+inte_agree+inte_neur"
  formula_relation_part = "same_user"
  formula_pull_request = "lifetime_minutes+num_commits+src_churn+files_added+files_deleted+files_changed+friday_effect+reopen_or_not+commits_on_files_touched+hash_tag+test_inclusion+description_length+ci_exists+num_comments+other_comment+comment_conflict+test_churn+first_response_time+perc_pos_emotion+perc_neg_emotion"
  formula = paste("merged_or_not~", paste(c(formula_pull_request, formula_contrib_part, formula_inte_part, formula_relation_part, formula_project_part), collapse="+"), sep="")
  formula = paste(formula, "+(1|project_id)", sep="")

  model_path = "has_comments_model.RData"

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
  model5_has_comments <- glmer(formula=formula
              , family = binomial,
              verbose=TRUE,
              data=selected_df,
              control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE))
  # because this model build process will take a lot of time, so I need to store the model
  save(model5_has_comments, file=model_path)
  print("finish build up model5_has_comments")
  prob = predict(model5_has_comments, type='response')
  selected_df$prob = prob
  auc_value = roc(merged_or_not~prob, data=selected_df)
} else if (which_model == 'contrib_comment') {
  print(which_model)
  rmout = function(df) {
    df = df[!(df[["contrib_comment"]]==0),]
    return(df)
  }
  # self define keep factors
  keep_factors = c("sloc", "team_size", "test_lines_per_kloc", "stars", "project_age", "open_issue_num", "open_pr_num", "pr_succ_rate", "pushed_delta", "integrator_availability", "followers", "first_pr", "account_creation_days", "core_member", "contrib_gender", "contrib_open", "contrib_cons", "contrib_extra", "contrib_agree", "contrib_neur", "prior_review_num", "inte_open", "inte_cons", "inte_extra", "inte_agree", "inte_neur", "lifetime_minutes", "num_commits", "src_churn", "files_added", "files_deleted", "files_changed", "commits_on_files_touched", "friday_effect", "reopen_or_not", "hash_tag", "test_inclusion", "description_length", "ci_exists", "same_user", "perc_contrib_pos_emo", "perc_contrib_neg_emo", "num_comments", "other_comment", "prev_pullreqs", "comment_conflict", "test_churn")
  df = rmout(df)
  selected_df = df[c(keep_factors, "merged_or_not", "project_id", "creator_id", "last_closer_id")]
  selected_df = na.omit(selected_df)
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
  formula_pull_request = "lifetime_minutes+num_commits+src_churn+files_added+files_deleted+files_changed+friday_effect+reopen_or_not+commits_on_files_touched+hash_tag+test_inclusion+description_length+ci_exists+num_comments+other_comment+comment_conflict+test_churn+perc_contrib_pos_emo+perc_contrib_neg_emo"
  formula = paste("merged_or_not~", paste(c(formula_pull_request, formula_contrib_part, formula_inte_part, formula_relation_part, formula_project_part), collapse="+"), sep="")
  formula = paste(formula, "+(1|project_id)", sep="")

  model_path = "contrib_comment_model.RData"

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
  model5_contrib_comment <- glmer(formula=formula
              , family = binomial,
              verbose=TRUE,
              data=selected_df,
              control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE))
  save(model5_contrib_comment, file=model_path)
  print("finish build up model5_contrib_comment")

} else if(which_model == 'diff_user') {
  print(which_model)
  
  rmout = function(df) {
    df = df[!(df[["same_user"]]==1),]
    return(df)
  }
  keep_factors = c("sloc", "team_size", "test_lines_per_kloc", "stars", "project_age", "open_issue_num", "open_pr_num", "pr_succ_rate", "pushed_delta", "integrator_availability", "followers", "first_pr", "account_creation_days", "core_member", "contrib_gender", "contrib_open", "contrib_cons", "contrib_extra", "contrib_agree", "contrib_neur", "prior_review_num", "inte_open", "inte_cons", "inte_extra", "inte_agree", "inte_neur", "lifetime_minutes", "num_commits", "src_churn", "files_added", "files_deleted", "files_changed", "commits_on_files_touched", "has_comments", "friday_effect", "reopen_or_not", "hash_tag", "test_inclusion", "description_length", "ci_exists", "same_country", "same_affiliation", "contrib_follow_integrator", "open_diff", "cons_diff", "extra_diff", "agree_diff", "neur_diff", "num_comments", "other_comment", "prev_pullreqs", "comment_conflict", "test_churn")
  df = rmout(df)
  selected_df = df[c(keep_factors, "merged_or_not", "project_id", "creator_id", "last_closer_id")]
  selected_df = na.omit(selected_df)
  selected_df = convertData_string(selected_df)
  selected_df = convertData_preprocess(selected_df, exclude_cols = c("merged_or_not", "project_id", "creator_id", "last_closer_id"))
  selected_df[['merged_or_not']] = as.factor(selected_df[['merged_or_not']])
  selected_df = selected_df[c(keep_factors, "merged_or_not", "project_id", "creator_id", "last_closer_id")]
  colns = colnames(selected_df)
  print(paste("selected_df rows: ", nrow(selected_df), sep=""))

  formula_project_part = "sloc+team_size+test_lines_per_kloc+stars+project_age+open_issue_num+open_pr_num+pr_succ_rate+pushed_delta+integrator_availability"
  formula_contrib_part = "prev_pullreqs+followers+first_pr+account_creation_days+core_member+contrib_gender+contrib_open+contrib_cons+contrib_extra+contrib_agree+contrib_neur"
  formula_inte_part = "prior_review_num+inte_open+inte_cons+inte_extra+inte_agree+inte_neur"
  formula_relation_part = "same_country+same_affiliation+contrib_follow_integrator+open_diff+cons_diff+extra_diff+agree_diff+neur_diff"
  formula_pull_request = "lifetime_minutes+num_commits+src_churn+files_added+files_deleted+files_changed+friday_effect+reopen_or_not+commits_on_files_touched+hash_tag+test_inclusion+description_length+ci_exists+has_comments+num_comments+other_comment+comment_conflict+test_churn"
  formula = paste("merged_or_not~", paste(c(formula_pull_request, formula_contrib_part, formula_inte_part, formula_relation_part, formula_project_part), collapse="+"), sep="")
  formula = paste(formula, "+(1|project_id)", sep="")

  model_path = "diff_user_model.RData"

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
  model5_diff_user <- glmer(formula=formula
              , family = binomial,
              verbose=TRUE,
              data=selected_df,
              control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE))
  save(model5_diff_user, file=model_path)
  print("finish build up model5_diff_user")
  prob = predict(model5_diff_user, type='response')
  selected_df$prob = prob
  auc_value = roc(merged_or_not~prob, data=selected_df)
} else if (which_model == 'inte_comment') {
  print(which_model)
  rmout = function(df) {
    df = df[!(df[["inte_comment"]]==0),]
    return(df)
  }

  keep_factors = c("sloc", "team_size", "test_lines_per_kloc", "stars", "project_age", "open_issue_num", "open_pr_num", "pr_succ_rate", "pushed_delta", "integrator_availability", "followers", "first_pr", "account_creation_days", "core_member", "contrib_gender", "contrib_open", "contrib_cons", "contrib_extra", "contrib_agree", "contrib_neur", "prior_review_num", "inte_open", "inte_cons", "inte_extra", "inte_agree", "inte_neur", "lifetime_minutes", "num_commits", "src_churn", "files_added", "files_deleted", "files_changed", "commits_on_files_touched", "friday_effect", "reopen_or_not", "hash_tag", "test_inclusion", "description_length", "ci_exists", "same_user", "perc_inte_pos_emo", "perc_inte_neg_emo", "num_comments", "other_comment", "prev_pullreqs", "comment_conflict", "test_churn")
  df = rmout(df)
  selected_df = df[c(keep_factors, "merged_or_not", "project_id", "creator_id", "last_closer_id")]
  selected_df = na.omit(selected_df)
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
  formula_pull_request = "lifetime_minutes+num_commits+src_churn+files_added+files_deleted+files_changed+friday_effect+reopen_or_not+commits_on_files_touched+hash_tag+test_inclusion+description_length+ci_exists+num_comments+other_comment+comment_conflict+test_churn+perc_inte_pos_emo+perc_inte_neg_emo"
  formula = paste("merged_or_not~", paste(c(formula_pull_request, formula_contrib_part, formula_inte_part, formula_relation_part, formula_project_part), collapse="+"), sep="")
  formula = paste(formula, "+(1|project_id)", sep="")

  model_path = "inte_comment_model.RData"

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
  model5_inte_comment <- glmer(formula=formula
              , family = binomial,
              verbose=TRUE,
              data=selected_df,
              control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE))
  save(model5_inte_comment, file=model_path)
  print("finish build up model5_inte_comment")

} else if (which_model == 'use_ci') {
  print(which_model)

  rmout = function(df) {
    df = df[!(df[["ci_exists"]]==0),]
    return(df)
  }

  keep_factors = c("sloc", "team_size", "test_lines_per_kloc", "stars", "project_age", "open_issue_num", "open_pr_num", "pr_succ_rate", "pushed_delta", "integrator_availability", "followers", "first_pr", "account_creation_days", "core_member", "contrib_gender", "contrib_open", "contrib_cons", "contrib_extra", "contrib_agree", "contrib_neur", "prior_review_num", "inte_open", "inte_cons", "inte_extra", "inte_agree", "inte_neur", "lifetime_minutes", "num_commits", "src_churn", "files_added", "files_deleted", "files_changed", "commits_on_files_touched", "friday_effect", "reopen_or_not", "hash_tag", "test_inclusion", "description_length", "same_user", "has_comments", "ci_latency", "ci_failed_perc", "num_comments", "other_comment", "prev_pullreqs", "comment_conflict", "test_churn")

  df = rmout(df)
  selected_df = df[c(keep_factors, "merged_or_not", "project_id", "creator_id", "last_closer_id")]
  selected_df = na.omit(selected_df)
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
  formula_pull_request = "lifetime_minutes+num_commits+src_churn+files_added+files_deleted+files_changed+friday_effect+reopen_or_not+commits_on_files_touched+hash_tag+description_length+has_comments+test_inclusion+num_comments+other_comment+comment_conflict+test_churn+ci_latency+ci_failed_perc"
  formula = paste("merged_or_not~", paste(c(formula_pull_request, formula_contrib_part, formula_inte_part, formula_relation_part, formula_project_part), collapse="+"), sep="")
  formula = paste(formula, "+(1|project_id)", sep="")

  model_path = "use_ci_model.RData"

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
  model5_use_ci <- glmer(formula=formula
              , family = binomial,
              verbose=TRUE,
              data=selected_df,
              control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE))
  save(model5_use_ci, file=model_path)
  print("finish build up model5_use_ci")
  prob = predict(model5_use_ci, type='response')
  selected_df$prob = prob
  auc_value = roc(merged_or_not~prob, data=selected_df)
}
