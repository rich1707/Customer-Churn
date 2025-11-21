# load libraries ----

library(tidyverse)
library(readxl)
library(corrr)
library(janitor)

library(patchwork)
library(scales)

library(tidymodels)
library(finetune)
library(vip)

library(xgboost)

# read in and clean data ----

churn <- read_xlsx("Telco_customer_churn.xlsx") |> 
   clean_names()

churn |> 
   map_int(~sum(is.na(.x))) |> 
   sort(decreasing = TRUE)

churn <- churn |> 
   select(-churn_reason)

churn <- churn |> 
   mutate(total_charges = replace_na(total_charges, 0))

churn <- churn |> 
   select(-customer_id, -count)

churn <- churn |> 
   select(-country, -state)

churn <- churn |> 
   select(-lat_long)

churn <- churn |> 
   select(-churn_value)

churn <- churn |> 
   select(-churn_score)

# Data Exploration ---- 

churn |>
   mutate(across(where(is.character), as.factor)) |> 
   mutate(across(everything(), as.numeric)) |> 
   correlate(quiet = TRUE) |> 
   focus(churn_label) |> 
   mutate(churn_label = abs(churn_label)) |>
   ggplot(aes(x = reorder(term, churn_label), y = churn_label)) + 
   geom_point(size = 2) +
   geom_segment(aes(x = term, xend = term, y = 0, yend = churn_label), size = 1.25) +
   labs(x = NULL, y = NULL, title = "Correlations with Churn (absolute value)") +
   coord_flip() +
   theme(
      axis.text = element_text(face = "bold", size = 9),
      plot.title = element_text(face = "bold", size = 14)
   )

## location ----

churn |> 
   count(city, name = "totals") |>  
   summarise(
      total_cities = length(city),
      total_customers = sum(totals),
      min_customers = min(totals),
      mean_customers = mean(totals),
      max_customers = max(totals)
   )

churn |> 
   group_by(city) |> 
   summarise(totals = n(),
             churned = sum(churn_label == "Yes")) |> 
   arrange(desc(churned)) |> 
   ggplot(aes(x = totals, y = churned)) + 
   geom_point(size = 1.5, colour = "#CB9E23") +
   labs(x = "Total Customers", y = "Total Churn",
        title = "Customer churn by city",
        subtitle = "Showing total customers vs. total who churn") +
   theme(
      axis.text.x = element_text(face = "bold", size = 10),
      axis.text.y = element_text(face = "bold", size = 10),
      axis.title.x = element_text(face = "bold", size = 12),
      axis.title.y = element_text(face = "bold", size = 12),
      plot.title = element_text(face = "bold", size = 14)
   )

by_city <- churn |> 
   group_by(city) |> 
   summarise(
      totals = n(),
      churned = sum(churn_label == "Yes"),
      .groups = "drop"
   ) 

lm(churned ~ totals, data = by_city) |> 
   glance() |> 
   select(r.squared, p.value)

churn |> 
   mutate(churn_label = recode(churn_label, Yes = "Churned", No = "Retained")) |> 
   ggplot(aes(x = longitude, y = latitude)) +
   geom_point(size = 3, alpha = 0.5, colour = "#CB9E23", shape = 1) + 
   labs(x = "Longitude", y = "Latitude") + 
   facet_wrap(~churn_label) + 
   theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_text(face = "bold", size = 12),
      strip.text = element_text(face = "bold", size = 12)
   )

churn <- churn |> 
   select(-city, -zip_code, -latitude, -longitude)


## Demographics ----

plot_gender <- churn |> 
   ggplot(aes(x = gender, fill = churn_label)) + 
   geom_bar(position = "dodge2", colour = "black", alpha = 0.5, width = 0.5) +
   labs(x = NULL, y = NULL, title = "Gender") + 
   scale_fill_manual(
      values = c("#CB9E23", "#456355"),
      labels = c("Retained", "Churned")
   ) + 
   theme(
      axis.text.x = element_text(face = "bold", size = 11),
      axis.text.y = element_text(face = "bold", size = 11),
      plot.title = element_text(face = "bold", size = 14),
      legend.title = element_blank(),
      legend.text = element_text(face = "bold")
   )

plot_senior <- churn |> 
   mutate(senior_citizen = if_else(
      senior_citizen == "Yes", "Senior Citizen", "Not Senior"
   )) |>
   ggplot(aes(x = senior_citizen, fill = churn_label)) + 
   geom_bar(position = "dodge2", colour = "black", alpha = 0.5, width = 0.5) +
   labs(x = NULL, y = NULL, title = "Seniors") + 
   scale_fill_manual(
      values = c("#CB9E23", "#456355"),
      labels = c("Retained", "Churned")
   ) +  
   theme(
      axis.text.x = element_text(face = "bold", size = 11),
      axis.text.y = element_text(face = "bold", size = 11),
      plot.title = element_text(face = "bold", size = 14),
      legend.title = element_blank(),
      legend.text = element_text(face = "bold")
   )

plot_partner <- churn |> 
   mutate(partner = if_else(partner == "No", "No Partner", "Partner")) |>
   ggplot(aes(x = partner, fill = churn_label)) + 
   geom_bar(position = "dodge2", colour = "black", alpha = 0.5, width = 0.5) +
   labs(x = NULL, y = NULL, title = "Partners") + 
   scale_fill_manual(
      values = c("#CB9E23", "#456355"),
      labels = c("Retained", "Churned")
   ) +  
   theme(
      axis.text.x = element_text(face = "bold", size = 11),
      axis.text.y = element_text(face = "bold", size = 11),
      plot.title = element_text(face = "bold", size = 14),
      legend.title = element_blank(),
      legend.text = element_text(face = "bold")
   )

plot_dependents <- churn |> 
   mutate(
      dependents = if_else(dependents == "No", "No Dependents", "Dependents"),
      dependents = factor(dependents, levels = c("No Dependents", "Dependents"))
   ) |>
   ggplot(aes(x = dependents, fill = churn_label)) + 
   geom_bar(position = "dodge2", colour = "black", alpha = 0.5, width = 0.5) +
   labs(x = NULL, y = NULL, title = "Dependents") + 
   scale_fill_manual(
      values = c("#CB9E23", "#456355"),
      labels = c("Retained", "Churned")
   ) + 
   theme(
      axis.text.x = element_text(face = "bold", size = 11),
      axis.text.y = element_text(face = "bold", size = 11),
      plot.title = element_text(face = "bold", size = 14),
      legend.title = element_blank(),
      legend.text = element_text(face = "bold")
   )

(plot_gender + plot_senior) / (plot_partner + plot_dependents) + 
   plot_layout(guides = "collect")


## Services ----

churn <- churn |> 
   select(-phone_service)

plot_phone <- churn |> 
   mutate(multiple_lines = case_when(
      multiple_lines == "No" ~ "Single line",
      multiple_lines == "Yes" ~ "Multiple lines",
      .default = "No phone"
   )) |> 
   mutate(multiple_lines = fct_relevel(
      multiple_lines, c("Single line", "Multiple lines", "No Phone")
   )) |> 
   ggplot(aes(x = multiple_lines, fill = churn_label)) + 
   geom_bar(position = "dodge2", colour = "black", alpha = 0.5, width = 0.5) +
   labs(x = NULL, y = NULL, title = "Phone Lines") + 
   scale_fill_manual(
      values = c("#CB9E23", "#456355"),
      labels = c("Retained", "Churned")
   ) + 
   theme(
      axis.text.x = element_text(face = "bold", size = 9),
      axis.text.y = element_text(face = "bold", size = 9),
      plot.title = element_text(face = "bold", size = 14),
      legend.title = element_blank(),
      legend.text = element_text(face = "bold")
   )

plot_internet <- churn |> 
   mutate(internet_service = if_else(
      internet_service == "No", "No internet", internet_service
   )) |> 
   ggplot(aes(x = internet_service, fill = churn_label)) + 
   geom_bar(position = "dodge2",colour = "black", alpha = 0.5, width = 0.5) + 
   labs(x = NULL, y = NULL, title = "Internet Service") +
   scale_fill_manual(
      values = c("#CB9E23", "#456355"),
      labels = c("Retained", "Churned")
   ) + 
   theme(
      axis.text.x = element_text(face = "bold", size = 9),
      axis.text.y = element_text(face = "bold", size = 9),
      plot.title = element_text(face = "bold", size = 14),
      legend.title = element_blank(),
      legend.text = element_text(face = "bold")
   )

plot_movies <- churn |> 
   mutate(streaming_movies = case_when(
      streaming_movies == "No" ~ "No movies",
      streaming_movies == "Yes" ~ "Movies",
      .default = "No internet"
   )) |> 
   mutate(streaming_movies = fct_relevel(
      streaming_movies, c("No movies", "Movies", "No internet")
   )) |> 
   ggplot(aes(x = streaming_movies, fill = churn_label)) + 
   geom_bar(position = "dodge2",colour = "black", alpha = 0.5, width = 0.5) + 
   labs(x = NULL, y = NULL, title = "Stream Movies") +
   scale_fill_manual(
      values = c("#CB9E23", "#456355"),
      labels = c("Retained", "Churned")
   ) + 
   theme(
      axis.text.x = element_text(face = "bold", size = 9),
      axis.text.y = element_text(face = "bold", size = 9),
      plot.title = element_text(face = "bold", size = 14),
      legend.title = element_blank(),
      legend.text = element_text(face = "bold")
   )

plot_tv <- churn |> 
   mutate(streaming_tv = case_when(
      streaming_tv == "No" ~ "No TV",
      streaming_tv == "Yes" ~ "TV",
      .default = "No internet"
   )) |> 
   mutate(streaming_tv = fct_relevel(
      streaming_tv, c("No TV", "TV", "No internet")
   )) |> 
   ggplot(aes(x = streaming_tv, fill = churn_label)) + 
   geom_bar(position = "dodge2",colour = "black", alpha = 0.5, width = 0.5) + 
   labs(x = NULL, y = NULL, title = "Stream TV") +
   scale_fill_manual(
      values = c("#CB9E23", "#456355"),
      labels = c("Retained", "Churned")
   ) + 
   theme(
      axis.text.x = element_text(face = "bold", size = 9),
      axis.text.y = element_text(face = "bold", size = 9),
      plot.title = element_text(face = "bold", size = 14),
      legend.title = element_blank(),
      legend.text = element_text(face = "bold")
   )

(plot_phone + plot_internet) / (plot_movies + plot_tv) + 
   plot_layout(guides = "collect")


plot_security <- churn |> 
   mutate(online_security = case_when(
      online_security == "Yes" ~ "Security",
      online_security == "No" ~ "No security",
      .default = "No internet"
   )) |> 
   mutate(online_security = fct_relevel(
      online_security, c("No security", "Security", "No internet"))
   ) |> 
   ggplot(aes(x = online_security, fill = churn_label)) + 
   geom_bar(position = "dodge2", colour = "black", alpha = 0.5, width = 0.5) + 
   labs(x = NULL, y = NULL, title = "Online Security") + 
   scale_fill_manual(
      values = c("#CB9E23", "#456355"),
      labels = c("Retained", "Churned")
   ) + 
   theme(
      axis.text.x = element_text(face = "bold", size = 9),
      axis.text.y = element_text(face = "bold", size = 9),
      plot.title = element_text(face = "bold", size = 14),
      legend.title = element_blank(),
      legend.text = element_text(face = "bold")
   )

plot_backup <- churn |> 
   mutate(online_backup = case_when(
      online_backup == "Yes" ~ "Backup",
      online_backup == "No" ~ "No backup",
      .default = "No internet"
   )) |> 
   mutate(online_backup = fct_relevel(
      online_backup, c("No backup", "Backup", "No internet")
   )) |> 
   ggplot(aes(x = online_backup, fill = churn_label)) + 
   geom_bar(position = "dodge2", colour = "black", alpha = 0.5, width = 0.5) + 
   labs(x = NULL, y = NULL, title = "Online Backup") + 
   scale_fill_manual(
      values = c("#CB9E23", "#456355"),
      labels = c("Retained", "Churned")
   ) +  
   theme(
      axis.text.x = element_text(face = "bold", size = 9),
      axis.text.y = element_text(face = "bold", size = 9),
      plot.title = element_text(face = "bold", size = 14),
      legend.title = element_blank(),
      legend.text = element_text(face = "bold")
   )

plot_protection <- churn |> 
   mutate(device_protection = case_when(
      device_protection == "Yes" ~ "Protection",
      device_protection == "No" ~ "No protection",
      .default = "No internet"
   )) |> 
   mutate(device_protection = fct_relevel(
      device_protection, c("No protection", "Protection", "No internet")
   )) |> 
   ggplot(aes(x = device_protection, fill = churn_label)) + 
   geom_bar(position = "dodge2", colour = "black", alpha = 0.5, width = 0.5) + 
   labs(x = NULL, y = NULL, title = "Device Protection") + 
   scale_fill_manual(
      values = c("#CB9E23", "#456355"),
      labels = c("Retained", "Churned")
   ) +  
   theme(
      axis.text.x = element_text(face = "bold", size = 9),
      axis.text.y = element_text(face = "bold", size = 9),
      plot.title = element_text(face = "bold", size = 14),
      legend.title = element_blank(),
      legend.text = element_text(face = "bold")
   )

plot_support <- churn |> 
   mutate(tech_support = case_when(
      tech_support == "Yes" ~ "Supported",
      tech_support == "No" ~ "No support",
      .default = "No internet"
   )) |> 
   mutate(tech_support = fct_relevel(
      tech_support, c("No support", "Supported", "No internet")
   )) |> 
   ggplot(aes(x = tech_support, fill = churn_label)) + 
   geom_bar(position = "dodge2", colour = "black", alpha = 0.5, width = 0.5) + 
   labs(x = NULL, y = NULL, title = "Tech Support") + 
   scale_fill_manual(
      values = c("#CB9E23", "#456355"),
      labels = c("Retained", "Churned")
   ) +  
   theme(
      axis.text.x = element_text(face = "bold", size = 9),
      axis.text.y = element_text(face = "bold", size = 9),
      plot.title = element_text(face = "bold", size = 14),
      legend.title = element_blank(),
      legend.text = element_text(face = "bold")
   )

(plot_security + plot_backup) / (plot_protection + plot_support) + 
   plot_layout(guides = "collect")

## Account ----

plot_billing <- churn |> 
   mutate(paperless_billing = if_else(
      paperless_billing == "Yes", "Paperless", "Traditional"
   )) |> 
   ggplot(aes(x = paperless_billing, fill = churn_label)) + 
   geom_bar(position = "dodge2", colour = "black", width = 0.3, alpha = 0.5) +
   labs(x = NULL, y = NULL, title = "Billing") +
   scale_fill_manual(
      values = c("#CB9E23", "#456355"),
      labels = c("Retained", "Churned")
   ) + 
   theme(
      axis.text = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 16),
      legend.text = element_text(face= "bold"),
      legend.title = element_blank()
   )

plot_contract <- churn |> 
   mutate(contract = if_else(contract == "Month-to-month", "Monthly", contract)) |> 
   ggplot(aes(x = contract, fill = churn_label)) + 
   geom_bar(position = "dodge2", colour = "black", alpha = 0.5, width = 0.5) +
   labs(x = NULL, y = NULL, title = "Contract length") +
   scale_fill_manual(
      values = c("#CB9E23", "#456355"),
      labels = c("Retained", "Churned")
   ) + 
   theme(
      axis.text = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 16),
      legend.text = element_text(face = "bold"),
      legend.title = element_blank()
   )

plot_payment <- churn |> 
   mutate(payment_method = case_when(
      payment_method == "Bank transfer (automatic)" ~ "Bank transfer",
      payment_method == "Credit card (automatic)" ~ "Credit card",
      .default = payment_method
   )) |> 
   mutate(payment_method = fct_relevel(
      payment_method, c("Electronic check", "Mailed check", "Bank transfer", "Credit card")
   )) |> 
   ggplot(aes(x = payment_method, fill = churn_label)) + 
   geom_bar(position = "dodge2", colour = "black", width = 0.3, alpha = 0.5) + 
   labs(x = NULL, y = NULL, title = "Payment methods") + 
   scale_fill_manual(
      values = c("#CB9E23", "#456355"),
      labels = c("Retained", "Churned")
   ) + 
   theme(
      axis.text = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 16),
      legend.text = element_text(face = "bold"),
      legend.title = element_blank()
   )

plot_payment / (plot_billing + plot_contract) +
   plot_layout(guides = "collect")


plot_contract_tech <- churn |> 
   filter(internet_service == "Fiber optic") |> 
   select(contract, churn_label) |>
   ggplot(aes(x = contract, fill = churn_label)) + 
   geom_bar(position = "dodge2", colour = "black", width = 0.3, alpha = 0.5) +
   labs(x = NULL, y = NULL, title = "Technically aware customers") +
   scale_fill_manual(
      values = c("#CB9E23", "#456355"),
      labels = c("Retained", "Churned")
   ) + 
   theme(
      axis.text = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 14),
      legend.text = element_text(face = "bold"),
      legend.title = element_blank()
   )

plot_contract_non_tech <- churn |> 
   filter(internet_service != "Fiber optic") |> 
   select(contract, churn_label) |>
   ggplot(aes(x = contract, fill = churn_label)) + 
   geom_bar(position = "dodge2", colour = "black", width = 0.3, alpha = 0.5) + 
   labs(x = NULL, y = NULL, title = "Non-technically aware customers") +
   scale_fill_manual(
      values = c("#CB9E23", "#456355"),
      labels = c("Retained", "Churned")
   ) + 
   theme(
      axis.text = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 14),
      legend.text = element_text(face = "bold"),
      legend.title = element_blank()
   )

plot_contract_tech / plot_contract_non_tech +
   plot_layout(guides = "collect")

### Financial aspects ---- 


plot_monthly_charges <- churn |> 
   select(monthly_charges, churn_label) |> 
   mutate(monthly_charges = cut_number(monthly_charges, n = 6)) |> 
   group_by(monthly_charges) |> 
   mutate(percent_churn = sum(churn_label == "Yes") / length(churn_label)) |> 
   ungroup() |> 
   mutate(monthly_charges = factor(monthly_charges, labels = as.character(1:6))) |> 
   ggplot(aes(x = monthly_charges, y = percent_churn, group = 1)) + 
   geom_point(size = 3.5) + 
   geom_line(size = 0.75) +
   expand_limits(y = 0) + 
   scale_y_continuous(labels = label_percent(accuracy = 1L)) +
   labs(x = NULL, y = NULL, title = "% churn by monthly charges") + 
   theme(
      axis.text = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 14)
   )

plot_total_charges <- churn |> 
   select(total_charges, churn_label) |> 
   mutate(total_charges = cut_number(total_charges, n = 6)) |> 
   group_by(total_charges) |> 
   mutate(percent_churn = sum(churn_label == "Yes") / length(churn_label)) |> 
   ungroup() |> 
   mutate(total_charges = factor(total_charges, labels = as.character(1:6))) |> 
   ggplot(aes(x = total_charges, y = percent_churn, group = 1)) + 
   geom_point(size = 3.5) + 
   geom_line(size = 0.75) +
   expand_limits(y = 0) +
   scale_y_continuous(labels = label_percent(accuracy = 1L)) +
   labs(x = NULL, y = NULL, title = "% churn by total charges") + 
   theme(
      axis.text = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 14)
   )

plot_tenure <- churn |> 
   mutate(tenure = case_when(
      tenure_months <= 12 ~ "1",
      tenure_months <= 24 ~ "2",
      tenure_months <= 36 ~ "3",
      tenure_months <= 48 ~ "4",
      tenure_months <= 60 ~ "5",
      .default = "5+"
   )) |> 
   group_by(tenure) |> 
   mutate(
      totals = n(),
      churned = sum(churn_label == "Yes"),
      percent_churned = churned / totals
   ) |> 
   ungroup() |> 
   mutate(tenure = fct_relevel(tenure, c("1", "2", "3", "4", "5", "5+"))) |> 
   ggplot(aes(x = tenure, y = percent_churned, group = 1)) + 
   geom_point(size = 3.5) +
   geom_line(size = 0.75) + 
   expand_limits(y = 0) + 
   scale_y_continuous(labels = label_percent(accuracy = 1L)) + 
   labs(x = NULL, y = NULL, title = "% churn by tenure") + 
   theme(
      axis.text = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 14)
   )

plot_cltv <- churn |> 
   mutate(group_cltv = cut_number(cltv, n = 6)) |> 
   group_by(group_cltv) |> 
   mutate(percent_churn = sum(churn_label == "Yes") / length(churn_label)) |> 
   ungroup() |> 
   mutate(group_cltv = factor(group_cltv, labels = as.character(1:6))) |> 
   ggplot(aes(x = group_cltv, y = percent_churn, group = 1)) + 
   geom_point(size = 3.5) + 
   geom_line(size = 0.75) + 
   expand_limits(y = 0) + 
   scale_y_continuous(labels = label_percent(accuracy = 1L)) + 
   labs(x = NULL, y= NULL,  title = "% churn by cltv") +
   theme(
      axis.text = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 14)
   )

(plot_monthly_charges + plot_total_charges) / (plot_tenure  + plot_cltv)

# Building a Model ----

churn <- churn |> 
   mutate(avg_cost = total_charges / tenure_months) |> 
   mutate(diff_charge = case_when(
      avg_cost > monthly_charges ~ "Less",
      avg_cost < monthly_charges ~ "More",
      .default = "Same"
   )) |> 
   select(-avg_cost)

churn <- churn |> 
   mutate(able_to_churn = case_when(
      contract == "Month-to-month" ~ "Yes",
      contract == "One year" & tenure_months %% 12 == 0 ~ "Yes",
      contract == "Two year" & tenure_months %% 24 == 0 ~ "Yes",
      tenure_months == 0 ~ "No",
      .default = "No"
   ))

set.seed(2021)

churn_split <- initial_split(churn, prop = 3/4, strata = churn_label)

churn_train <- training(churn_split)
churn_test <- testing(churn_split)

churn_x_folds <- vfold_cv(churn_train, v = 10, strata = churn_label)

churn_recipe <- recipe(churn_label ~ ., data = churn_train) |> 
   step_rm(cltv) |> 
   step_string2factor(churn_label, skip = TRUE) |> 
   step_string2factor(all_nominal_predictors()) |> 
   step_normalize(all_numeric_predictors()) |> 
   step_dummy(all_nominal_predictors(), one_hot = TRUE)

churn_spec <- boost_tree(
   mtry = tune(), trees = tune(), min_n = tune(), 
   tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), 
   sample_size = tune(), stop_iter = tune()
) |> 
   set_engine("xgboost") |> 
   set_mode("classification")

churn_wrkflw <- workflow() |> 
   add_model(churn_spec) |> 
   add_recipe(churn_recipe)

doParallel::registerDoParallel()

set.seed(2022)

churn_tune <- tune_race_anova(
   churn_wrkflw,
   churn_x_folds,
   grid = 30,
   metrics = metric_set(accuracy),
   control = control_race(verbose_elim = TRUE)
)

churn_hypers <- select_best(churn_tune)

churn_wrkflw_final <- finalize_workflow(churn_wrkflw, churn_hypers)

churn_model <- 
   churn_wrkflw_final |> fit(churn_train)

# Evaluation and analysis ----

churn_model |> 
   extract_fit_engine() |> vi() |> 
   arrange(desc(Importance)) |> head(15) |> 
   ggplot(aes(x = fct_reorder(Variable, Importance), y = Importance)) + 
   geom_point(size = 2) + 
   geom_segment(aes(x = Variable, xend = Variable, y = 0, yend = Importance), size = 1.25) +
   labs(x = NULL, y = NULL, title = "Variable importance (absolute value)") +
   coord_flip() +
   theme(
      axis.text = element_text(face = "bold", size = 9),
      plot.title = element_text(face = "bold", size = 14)
   )

churn_preds <- churn_model |> 
   predict(new_data = churn_test)

churn_preds <- churn_test |> 
   select(churn_label) |> 
   bind_cols(churn_preds) |> 
   mutate(churn_label = factor(churn_label))

churn_preds |> 
   summarise(
      percent_retained = sum(churn_label == "No") / n(),
      percent_churned = sum(churn_label == "Yes") / n()
   )

churn_preds |> 
   accuracy(churn_label, .pred_class)

churn_preds |> 
   conf_mat(truth = churn_label, estimate = .pred_class)

churn_preds |> 
   precision(churn_label, .pred_class, event_level = "second")

churn_preds |> 
   recall(churn_label, .pred_class, event_level = "second")

churn_preds |> 
   f_meas(churn_label, .pred_class, event_level = "second")







