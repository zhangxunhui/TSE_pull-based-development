# This repository includes supplyments and programs of TSE paper
1. country_info.csv includes columns including `id`, `contrib_country` and `inte_country`. In order to protect the privacy of developer on Github, here the `id` is just the id of previous dataset (10.5281/zenodo.3922907). Also, we exclude the pull request whose country information cannot be detected for both contributor and integrator.

2. technical_report.pdf include the experiment results that are not presented in the paper.

3. RQ1_basic_model.R is the R script for building mixed-effect logistic regression model for basic model in RQ1.

4. RQ1_special_cases.R is the R script for building mixed-effect logistic regression models for special cases in RQ1, including has_comments=1, contrib_comment=1, inte_comment=1, same_user=0 and ci_exists=1

5. RQ2_same_user_1.R is the R script for building mixed-effect logistic regression model for context: contributor and integrator are the same user.

6. RQ2_same_user_0.R is the R script for building mixed-effect logistic regression model for context: contributor and integrator are different users.

7. RQ2_has_comments_1.R is the R script for building mixed-effect logistic regression model for context: pull request has comments.

8. RQ2_has_comments_0.R is the R script for building mixed-effect logistic regression model for context: pull request does not have comments.

9. RQ2_ci_exists_1.R is the R script for building mixed-effect logistic regression model for context: pull request use CI tools.

10. RQ2_ci_exists_0.R is the R script for building mixed-effect logistic regression model for context: pull request does not use CI tools.

11. RQ2_different_team_sizes.R is the R script for building mixed-effect logistic regression model for contexts: large team, mid team, small team.

12. RQ2_different_periods.R is the R script for building mixed-effect logistic regression model for contexts: before 2016.6, 2016.6-2018.6, after 2018.6.

13. different_periods_common_proj_ids.csv is the list of project id for the analysis of RQ2_different_periods.

14. config.yaml.example is a template, and you need to create file config.yaml similar to the template.

15. mcr_papers.xlsx is the file containing all extra papers related to "modern code review". Column `related` means whether the papers is related to patch decision, `note` shows the note that we take while filtering papers, `snowballing` presents the papers that are found during the backward snowballing process, `factors & results` means the factors and results extracted from the papers if related.