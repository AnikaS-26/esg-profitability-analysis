#Phase 2#
install.packages(c("tidyverse","fastDummies","ggplot2","readr","scales"))
library(tidyverse)
library(fastDummies)

DATA_PATH   <- "company_esg_financial_dataset.csv"   
OUT_CLEAN   <- "esg_financial_phase2_clean.csv"
OUT_MODEL   <- "esg_financial_phase2_model_ready_scaled.csv"
FIG_DIR     <- "phase2_figures"
TARGET      <- "ProfitMargin"

if (!dir.exists(FIG_DIR)) dir.create(FIG_DIR)

df <- readr::read_csv(DATA_PATH, show_col_types = FALSE)
cat("Shape:", nrow(df), "rows x", ncol(df), "cols\n")
print(glimpse(df))
cat("\nMissing (top 10):\n")
print(sort(colSums(is.na(df)), decreasing = TRUE)[1:min(10, ncol(df))])
cat("\nDuplicate rows:", sum(duplicated(df)), "\n")

winsorize <- function(x, ql = 0.01, qh = 0.99) {
  if (!is.numeric(x)) return(x)
  lo <- quantile(x, ql, na.rm = TRUE)
  hi <- quantile(x, qh, na.rm = TRUE)
  pmin(pmax(x, lo), hi)
}

df_clean <- df %>%
  distinct() %>%                            
  mutate(
    GrowthRate = if ("GrowthRate" %in% names(.))
      ifelse(is.na(GrowthRate), median(GrowthRate, na.rm = TRUE), GrowthRate) else GrowthRate
  )

for (col in c("ProfitMargin", "CarbonEmissions")) {
  if (col %in% names(df_clean)) {
    df_clean[[col]] <- winsorize(df_clean[[col]])
  }
}

categoricals <- c("Industry", "Region")
categoricals <- intersect(categoricals, names(df_clean))

df_model <- df_clean

if (length(categoricals) > 0) {
  df_model <- fastDummies::dummy_cols(
    df_model, select_columns = categoricals,
    remove_selected_columns = TRUE, remove_first_dummy = TRUE
  )
}

num_cols <- names(df_model)[sapply(df_model, is.numeric)]
num_features <- setdiff(num_cols, TARGET)

df_model[num_features] <- scale(df_model[num_features])

readr::write_csv(df_clean, OUT_CLEAN)
readr::write_csv(df_model, OUT_MODEL)
cat("Saved:", OUT_CLEAN, "|", OUT_MODEL, "\n")
-

p_hist <- ggplot(df_clean, aes(x = .data[[TARGET]])) +
  geom_histogram(bins = 30, color = "black") +
  labs(title = "Distribution of Profit Margin", x = "ProfitMargin", y = "Frequency")
ggsave(file.path(FIG_DIR, "hist_profit_margin.png"), p_hist, width = 7, height = 5, dpi = 300)

if ("ESG_Overall" %in% names(df_clean)) {
  p_sc_overall <- ggplot(df_clean, aes(x = ESG_Overall, y = .data[[TARGET]])) +
    geom_point(size = 1.2, alpha = 0.6) +
    labs(title = "Profit Margin vs ESG Overall", x = "ESG_Overall", y = "ProfitMargin")
  ggsave(file.path(FIG_DIR, "scatter_profit_vs_esg_overall.png"), p_sc_overall, width = 7, height = 5, dpi = 300)
}

for (esg in c("ESG_Environmental","ESG_Social","ESG_Governance")) {
  if (esg %in% names(df_clean)) {
    p_sc <- ggplot(df_clean, aes_string(x = esg, y = TARGET)) +
      geom_point(size = 1.2, alpha = 0.6) +
      labs(title = paste("Profit Margin vs", esg), x = esg, y = "ProfitMargin")
    ggsave(file.path(FIG_DIR, paste0("scatter_profit_vs_", tolower(esg), ".png")),
           p_sc, width = 7, height = 5, dpi = 300)
  }
}

num_df <- df_clean %>% select(where(is.numeric))
if (ncol(num_df) > 1) {
  corr_mat <- cor(num_df, use = "pairwise.complete.obs")
  corr_tbl <- as.data.frame(as.table(corr_mat))
  names(corr_tbl) <- c("Var1","Var2","Corr")
  
  p_heat <- ggplot(corr_tbl, aes(Var1, Var2, fill = Corr)) +
    geom_tile() +
    scale_fill_gradient2(limits = c(-1, 1)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(title = "Correlation Heatmap (Numeric Variables)", x = NULL, y = NULL)
  ggsave(file.path(FIG_DIR, "correlation_heatmap.png"), p_heat, width = 8, height = 6, dpi = 300)
}

if ("Industry" %in% names(df_clean)) {
  pm_by_ind <- df_clean %>%
    group_by(Industry) %>%
    summarise(mean_pm = mean(.data[[TARGET]], na.rm = TRUE)) %>%
    arrange(desc(mean_pm))
  p_pm_ind <- ggplot(pm_by_ind, aes(x = reorder(Industry, mean_pm), y = mean_pm)) +
    geom_col() + coord_flip() +
    labs(title = "Mean Profit Margin by Industry", x = "Industry", y = "Mean ProfitMargin")
  ggsave(file.path(FIG_DIR, "bar_profit_by_industry.png"), p_pm_ind, width = 8, height = 6, dpi = 300)
  
  if ("ESG_Overall" %in% names(df_clean)) {
    esg_by_ind <- df_clean %>%
      group_by(Industry) %>%
      summarise(mean_esg = mean(ESG_Overall, na.rm = TRUE)) %>%
      arrange(desc(mean_esg))
    p_esg_ind <- ggplot(esg_by_ind, aes(x = reorder(Industry, mean_esg), y = mean_esg)) +
      geom_col() + coord_flip() +
      labs(title = "Mean ESG Overall by Industry", x = "Industry", y = "Mean ESG_Overall")
    ggsave(file.path(FIG_DIR, "bar_esg_overall_by_industry.png"), p_esg_ind, width = 8, height = 6, dpi = 300)
  }
}

if (all(c("Year","ESG_Overall") %in% names(df_clean))) {
  yearly <- df_clean %>%
    group_by(Year) %>%
    summarise(
      avg_profit = mean(.data[[TARGET]], na.rm = TRUE),
      avg_esg    = mean(ESG_Overall, na.rm = TRUE)
    ) %>%
    arrange(Year)
  
  p_trend <- ggplot(yearly, aes(x = Year)) +
    geom_line(aes(y = avg_profit)) +
    geom_line(aes(y = avg_esg)) +
    labs(title = "Yearly Trend: Profit Margin & ESG Overall (Averages)",
         x = "Year", y = "Average Value")
  ggsave(file.path(FIG_DIR, "trend_year_profit_esg.png"), p_trend, width = 7, height = 5, dpi = 300)
}

cat("\nFigures saved to:", normalizePath(FIG_DIR), "\n")
cat("Cleaned CSV:", normalizePath(OUT_CLEAN), "\n")
cat("Model-Ready CSV:", normalizePath(OUT_MODEL), "\n")

library(tidyverse)

DATA_PATH <- "company_esg_financial_dataset.csv"
df <- readr::read_csv(DATA_PATH, show_col_types = FALSE)

yearly <- df %>%
  group_by(Year) %>%
  summarise(
    avg_profit = mean(ProfitMargin, na.rm = TRUE),
    avg_esg    = mean(ESG_Overall, na.rm = TRUE)
  ) %>%
  arrange(Year) %>%
  pivot_longer(cols = c(avg_profit, avg_esg),
               names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric,
                         "avg_profit" = "Profit Margin (avg)",
                         "avg_esg"    = "ESG Overall (avg)"))

ggplot(yearly, aes(x = Year, y = value, linetype = metric)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(title = "Yearly Trends: Profit Margin vs ESG Overall (Averages)",
       x = "Year", y = "Average Value", linetype = NULL) +
  theme_minimal()

by_industry <- df %>%
  group_by(Industry, Year) %>%
  summarise(
    avg_profit = mean(ProfitMargin, na.rm = TRUE),
    avg_esg    = mean(ESG_Overall, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(c(avg_profit, avg_esg), names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric,
                         "avg_profit" = "Profit Margin (avg)",
                         "avg_esg"    = "ESG Overall (avg)"))

ggplot(by_industry, aes(Year, value, linetype = metric)) +
  geom_line() +
  geom_point(size = 0.8) +
  facet_wrap(~ Industry, scales = "free_y") +
  labs(title = "Yearly Trends by Industry",
       x = "Year", y = "Average Value", linetype = NULL) +
  theme_minimal(base_size = 10)

lagged <- df %>%
  arrange(CompanyID, Year) %>%
  group_by(CompanyID) %>%
  mutate(ESG_Overall_lag1 = dplyr::lag(ESG_Overall, 1)) %>%
  ungroup() %>%
  filter(!is.na(ESG_Overall_lag1))

ggplot(lagged, aes(ESG_Overall_lag1, ProfitMargin)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Lagged Relationship: ESG (t-1) vs Profit Margin (t)",
       x = "ESG Overall (lagged 1 year)", y = "Profit Margin (current year)") +
  theme_minimal()

yearly_ccf <- df %>%
  group_by(Year) %>%
  summarise(
    pm  = mean(ProfitMargin, na.rm = TRUE),
    esg = mean(ESG_Overall,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Year)

pm  <- scale(yearly_ccf$pm,  center = TRUE, scale = FALSE)
esg <- scale(yearly_ccf$esg, center = TRUE, scale = FALSE)
ccf(esg, pm, main = "CCF: ESG vs Profit Margin (yearly averages)")


#Phase3
install.packages(c("tidyverse", "glmnet", "rpart", "rpart.plot", "Metrics"))

library(tidyverse)
library(glmnet)
library(rpart)
library(rpart.plot)
library(Metrics)

df_model <- read_csv("esg_financial_phase2_model_ready_scaled.csv",
                     show_col_types = FALSE)

glimpse(df_model)
summary(df_model$ProfitMargin)

drop_cols <- intersect(c("CompanyID", "CompanyName"), names(df_model))
if (length(drop_cols) > 0) {
  df_model <- df_model %>% select(-all_of(drop_cols))
}

df_model$ProfitMargin <- as.numeric(df_model$ProfitMargin)

set.seed(123)  
n <- nrow(df_model)
train_idx <- sample(seq_len(n), size = floor(0.8 * n))

train <- df_model[train_idx, ]
test  <- df_model[-train_idx, ]

x_train <- train %>% select(-ProfitMargin) %>% as.matrix()
y_train <- train$ProfitMargin

x_test  <- test %>% select(-ProfitMargin) %>% as.matrix()
y_test  <- test$ProfitMargin

# MODEL 1: Multiple Linear Regression

lm_model <- lm(ProfitMargin ~ ., data = train)
summary(lm_model)

pred_lm <- predict(lm_model, newdata = test)

rmse_lm <- rmse(y_test, pred_lm)
mae_lm  <- mae(y_test, pred_lm)
r2_lm   <- cor(y_test, pred_lm)^2

rmse_lm; mae_lm; r2_lm

# MODEL 2: Regularized Regression (Ridge, Lasso, Elastic Net)
#Ridge
set.seed(123)
ridge_cv <- cv.glmnet(x_train, y_train,
                      alpha = 0,          
                      nfolds = 10,
                      type.measure = "mse",
                      standardize = FALSE)  

best_lambda_ridge <- ridge_cv$lambda.min
ridge_model <- glmnet(x_train, y_train,
                      alpha = 0,
                      lambda = best_lambda_ridge,
                      standardize = FALSE)

pred_ridge <- predict(ridge_model, newx = x_test) %>% as.numeric()

rmse_ridge <- rmse(y_test, pred_ridge)
mae_ridge  <- mae(y_test, pred_ridge)
r2_ridge   <- cor(y_test, pred_ridge)^2

rmse_ridge; mae_ridge; r2_ridge

#Lasso
set.seed(123)
lasso_cv <- cv.glmnet(x_train, y_train,
                      alpha = 1,        
                      nfolds = 10,
                      type.measure = "mse",
                      standardize = FALSE)

best_lambda_lasso <- lasso_cv$lambda.min
lasso_model <- glmnet(x_train, y_train,
                      alpha = 1,
                      lambda = best_lambda_lasso,
                      standardize = FALSE)

pred_lasso <- predict(lasso_model, newx = x_test) %>% as.numeric()

rmse_lasso <- rmse(y_test, pred_lasso)
mae_lasso  <- mae(y_test, pred_lasso)
r2_lasso   <- cor(y_test, pred_lasso)^2

rmse_lasso; mae_lasso; r2_lasso

#Elastic Net
set.seed(123)
elastic_cv <- cv.glmnet(x_train, y_train,
                        alpha = 0.5,      
                        nfolds = 10,
                        type.measure = "mse",
                        standardize = FALSE)

best_lambda_elastic <- elastic_cv$lambda.min
elastic_model <- glmnet(x_train, y_train,
                        alpha = 0.5,
                        lambda = best_lambda_elastic,
                        standardize = FALSE)

pred_elastic <- predict(elastic_model, newx = x_test) %>% as.numeric()

rmse_elastic <- rmse(y_test, pred_elastic)
mae_elastic  <- mae(y_test, pred_elastic)
r2_elastic   <- cor(y_test, pred_elastic)^2

rmse_elastic; mae_elastic; r2_elastic

# MODEL 3: Decision Tree Regression
library(rpart)
library(rpart.plot)

set.seed(123)

tree_small <- rpart(
  ProfitMargin ~ .,
  data   = train,
  method = "anova",
  control = rpart.control(
    maxdepth = 3,   
    minsplit = 200, 
    cp       = 0.01 
  )
)

tree_small
rpart.plot(
  tree_small,
  type = 2,
  fallen.leaves = TRUE,
  cex = 0.8,
  main = "Decision Tree Regression (Depth ≤ 3): ProfitMargin"
)



importance <- tree_small$variable.importance

barplot(
  importance,
  horiz = TRUE,
  las   = 1,
  main  = "Decision Tree Variable Importance",
  xlab  = "Importance Score"
)

pred_tree <- predict(tree_small, newdata = test)

rmse_tree <- rmse(y_test, pred_tree)
mae_tree  <- mae(y_test, pred_tree)
r2_tree   <- cor(y_test, pred_tree)^2

rmse_tree; mae_tree; r2_tree

#MODEL COMPARISON TABLE
results <- tibble(
  Model = c("Linear Regression",
            "Ridge Regression",
            "Lasso Regression",
            "Elastic Net",
            "Decision Tree"),
  RMSE = c(rmse_lm, rmse_ridge, rmse_lasso, rmse_elastic, rmse_tree),
  MAE  = c(mae_lm, mae_ridge, mae_lasso, mae_elastic, mae_tree),
  R2   = c(r2_lm, r2_ridge, r2_lasso, r2_elastic, r2_tree)
)

print(results)