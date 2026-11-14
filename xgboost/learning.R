#===========================
#ファイルの読み込み
#===========================
zip_file <- "adult24csv.zip"
print(unzip(zip_file,list = TRUE))
tmp_dir <- tempdir()
print(tmp_dir)
unzip(zip_file,exdir = tmp_dir)
csv_files <- list.files(
  tmp_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)
df <- read.csv(csv_files[1])

#===========================
#変数の定義
#===========================
#目的変数として使用する変数
purpose_cols = c("K6SPD_A")
#説明変数として使用する変数
features_cols = c("LONELY_A","FDSBALANCE_A","SLPREST_A","FGEFRQTRD_A","SOCERRNDS_A","LSATIS4_A","DEPMED_A","ANXFREQ_A","SOCSCLPAR_A","DEPFREQ_A","SEX_A","AGEP_A")
#使用するすべての変数
use_cols <- c(purpose_cols, features_cols)
df_selected <- df[,use_cols]
print(head(df_selected))

#===========================
#前処理
#===========================
library(dplyr)
# K6SPD_Aの前処理
df_selected <- df_selected %>%
  mutate(
    K6SPD_A = case_when(
      K6SPD_A == 1 ~ 1,
      K6SPD_A == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )


#LONELY_Aの前処理
df_selected <- df_selected %>%
  mutate(
    LONELY_A = case_when(
      LONELY_A == 1 ~ 5,
      LONELY_A == 2 ~ 4,
      LONELY_A == 3 ~ 3,
      LONELY_A == 4 ~ 2,
      LONELY_A == 5 ~ 1,
      TRUE ~ NA_real_
    )
  )


#SLPREST_Aの前処理
df_selected <- df_selected %>%
  mutate(
    SLPREST_A = case_when(
      SLPREST_A == 1 ~ 1,
      SLPREST_A == 2 ~ 2,
      SLPREST_A == 3 ~ 3,
      SLPREST_A == 4 ~ 4,
      TRUE ~ NA_real_
    )
  )




df_selected <- df_selected %>%
  mutate(
    FGEFRQTRD_A = case_when(
      FGEFRQTRD_A == 1 ~ 1,
      FGEFRQTRD_A == 2 ~ 2,
      FGEFRQTRD_A == 3 ~ 3,
      FGEFRQTRD_A == 4 ~ 4,
      TRUE ~ NA_real_
    )
  )


#SOCERRNDS_A
df_selected <- df_selected %>%
  mutate(
    SOCERRNDS_A = case_when(
      SOCERRNDS_A == 1 ~ 1,
      SOCERRNDS_A == 2 ~ 2,
      SOCERRNDS_A == 3 ~ 3,
      SOCERRNDS_A == 4 ~ 4,
      TRUE ~ NA_real_
    )
  )


#LSATIS4_A
df_selected <- df_selected %>%
  mutate(
    LSATIS4_A = case_when(
      LSATIS4_A == 1 ~ 4,
      LSATIS4_A == 2 ~ 3,
      LSATIS4_A == 3 ~ 2,
      LSATIS4_A == 4 ~ 1,
      TRUE ~ NA_real_
    )
  )


#DEPMED_A
df_selected <- df_selected %>%
  mutate(
    DEPMED_A = case_when(
      DEPMED_A== 1 ~ "yes",
      DEPMED_A == 2 ~ "no",
      TRUE ~ NA_character_
    ),
    
    # カテゴリ変数に変換
    DEPMED_A = factor(
      DEPMED_A,
      levels = c("no", "yes")
    )
  )

#ANXFREQ_A
df_selected <- df_selected %>%
  mutate(
    ANXFREQ_A = case_when(
      ANXFREQ_A == 1 ~ 5,
      ANXFREQ_A == 2 ~ 4,
      ANXFREQ_A == 3 ~ 3,
      ANXFREQ_A == 4 ~ 2,
      ANXFREQ_A == 5 ~ 1,
      TRUE ~ NA_real_
    )
  )

#SOCSCLPAR_A
df_selected <- df_selected %>%
  mutate(
    SOCSCLPAR_A = case_when(
      SOCSCLPAR_A == 1 ~ 1,
      SOCSCLPAR_A == 2 ~ 2,
      SOCSCLPAR_A == 3 ~ 3,
      SOCSCLPAR_A == 4 ~ 4,
      TRUE ~ NA_real_
    )
  )

#DEPFREQ_A
df_selected <- df_selected %>%
  mutate(
    DEPFREQ_A = case_when(
      DEPFREQ_A == 1 ~ 5,
      DEPFREQ_A == 2 ~ 4,
      DEPFREQ_A == 3 ~ 3,
      DEPFREQ_A == 4 ~ 2,
      DEPFREQ_A == 5 ~ 1,
      TRUE ~ NA_real_
    )
  )

#SEX_A
df_selected <- df_selected %>%
  mutate(
    SEX_A = case_when(
      SEX_A== 1 ~ "Male",
      SEX_A == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    
    # カテゴリ変数に変換
    SEX_A = factor(
      SEX_A,
      levels = c("Male", "Female")
    )
  )


df_selected <- df_selected %>%
  mutate(
    AGEP_A = case_when(
      as.numeric(AGEP_A) >= 18 &
        as.numeric(AGEP_A) <= 85 ~ as.numeric(AGEP_A),
      TRUE ~ NA_real_
    )
  )

df_selected <- df_selected %>%
  mutate(
    FDSBALANCE_A = case_when(
      FDSBALANCE_A %in% 1:3 ~ as.numeric(FDSBALANCE_A),
      TRUE ~ NA_real_
    )
  )
#==========================
# 欠損値を含む行の削除
#==========================

# 削除前の行数
cat("削除前の行数:", nrow(df_selected), "\n")

# 欠損値が1つでも含まれる行を削除
df_selected <- na.omit(df_selected)

# 削除後の行数
cat("削除後の行数:", nrow(df_selected), "\n")

# 目的変数の人数を確認
print(table(df_selected$K6SPD_A))

#==========================
# 欠損値削除後の目的変数の行数確認
#==========================

# 0と1の行数を表示
print(table(df_selected$K6SPD_A))

# 全体の行数を表示
cat("合計行数:", nrow(df_selected), "\n")

#==========================
# 訓練データとテストデータの分割
# 層化ランダム分割（8:2）
#==========================
#install.packages("caret")
library(caret)

# 乱数シードを固定（再現性の確保）
set.seed(123)

# 目的変数を基準に層化ランダム分割
train_index <- createDataPartition(
  y = as.factor(df_selected$K6SPD_A),
  p = 0.8,
  list = FALSE
)

# 訓練データ（80%）
train_data <- df_selected[train_index, ]

# テストデータ（20%）
test_data <- df_selected[-train_index, ]

# データ数の確認
cat("訓練データ:", nrow(train_data), "行\n")
cat("テストデータ:", nrow(test_data), "行\n")


#==========================
# XGBoostの交差検証とパラメータ最適化
#==========================

# 必要なパッケージ
# install.packages("xgboost")
# install.packages("caret")
# install.packages("pROC")

library(xgboost)
library(caret)
library(pROC)

# 再現性の確保
set.seed(123)

# ==========================
# 1. 目的変数をfactor型に変換
# ==========================

# train_dataを使用する
# 0と1が数値の状態から実行する

train_data$K6SPD_A <- factor(
  train_data$K6SPD_A,
  levels = c(1, 0),
  labels = c("Yes", "No")
)

# 件数を確認
print(table(train_data$K6SPD_A))



# ==========================
# 2. テストデータの目的変数をfactor型に変換
# ==========================

test_data$K6SPD_A <- factor(
  test_data$K6SPD_A,
  levels = c(1, 0),
  labels = c("Yes", "No")
)

# ==========================
# 3. scale_pos_weightを計算
# ==========================

n_yes <- sum(train_data$K6SPD_A == "Yes")
n_no  <- sum(train_data$K6SPD_A == "No")

scale_pos_weight_value <- n_no / n_yes

cat("Yesの件数:", n_yes, "\n")
cat("Noの件数:", n_no, "\n")
cat("scale_pos_weight:",
    scale_pos_weight_value, "\n")

# ==========================
# 4. ハイパーパラメータを指定
# ==========================

tune_grid <- expand.grid(
  nrounds = 100,
  max_depth = 2,
  eta = 0.11,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

# ==========================
# 5. 指定パラメータでXGBoostを学習
# ==========================

set.seed(123)

xgb_model <- train(
  K6SPD_A ~ .,
  data = train_data,
  method = "xgbTree",
  
  # 交差検証を行わない
  trControl = trainControl(
    method = "none"
  ),
  
  # 指定したパラメータを使用
  tuneGrid = tune_grid,
  
  # クラス不均衡への対応
  scale_pos_weight = scale_pos_weight_value,
  
  # GPU設定
  tree_method = "hist",
  device = "cuda",
  
  verbose = FALSE,
  nthread = 1
)

# 学習モデルの確認
print(xgb_model)

# ==========================
# 6. 分類の閾値を指定
# ==========================

threshold <- 0.5

# ==========================
# 7. 訓練データの予測
# ==========================

# Yesの予測確率
train_pred_prob <- predict(
  xgb_model,
  newdata = train_data,
  type = "prob"
)$Yes

# 確率をYes・Noに変換
train_pred_class <- factor(
  ifelse(train_pred_prob >= threshold,
         "Yes", "No"),
  levels = c("Yes", "No")
)

# 混同行列
cm_train <- confusionMatrix(
  data = train_pred_class,
  reference = train_data$K6SPD_A,
  positive = "Yes"
)

# ==========================
# 8. テストデータの予測
# ==========================

# Yesの予測確率
test_pred_prob <- predict(
  xgb_model,
  newdata = test_data,
  type = "prob"
)$Yes

# 確率をYes・Noに変換
test_pred_class <- factor(
  ifelse(test_pred_prob >= threshold,
         "Yes", "No"),
  levels = c("Yes", "No")
)

# 混同行列
cm_test <- confusionMatrix(
  data = test_pred_class,
  reference = test_data$K6SPD_A,
  positive = "Yes"
)

# ==========================
# 9. 訓練データの評価結果
# ==========================

cat("\n===== 訓練データ =====\n")

print(cm_train$table)

cat("Sensitivity:",
    round(cm_train$byClass["Sensitivity"] * 100, 2),
    "%\n")

cat("Specificity:",
    round(cm_train$byClass["Specificity"] * 100, 2),
    "%\n")

cat("Balanced Accuracy:",
    round(cm_train$byClass["Balanced Accuracy"] * 100, 2),
    "%\n")

# ==========================
# 10. テストデータの評価結果
# ==========================

cat("\n===== テストデータ =====\n")

print(cm_test$table)

cat("Sensitivity:",
    round(cm_test$byClass["Sensitivity"] * 100, 2),
    "%\n")

cat("Specificity:",
    round(cm_test$byClass["Specificity"] * 100, 2),
    "%\n")

cat("Balanced Accuracy:",
    round(cm_test$byClass["Balanced Accuracy"] * 100, 2),
    "%\n")



# ==========================================
# 11. テストデータにおけるSHAP値の計算
# ==========================================

library(xgboost)
library(dplyr)
library(ggplot2)

X_test_df <- test_data[, features_cols, drop = FALSE]
X_test_mm <- model.matrix(~ ., data = X_test_df)
term_names <- attr(
  terms(~ ., data = X_test_df),
  "term.labels"
)
assign_index <- attr(X_test_mm, "assign")[-1]
feature_map <- data.frame(
  encoded_feature = colnames(X_test_mm)[-1],
  original_feature = term_names[assign_index]
)

X_test <- X_test_mm[, -1, drop = FALSE]
xgb_final <- xgb_model$finalModel

# 4. 学習時の説明変数名と順序を一致させる
model_features <- xgb_final$feature_names

if (!is.null(model_features)) {
  if (!setequal(colnames(X_test), model_features)) {
    stop("学習時とテストデータの説明変数名が一致しません")
  }
  
  X_test <- X_test[, model_features, drop = FALSE]
}

# 5. テストデータのSHAP値を計算
shap_values <- predict(
  xgb_final,
  newdata = X_test,
  predcontrib = TRUE
)

# 6. Bias（基準値）を除外
shap_values <- shap_values[
  ,
  seq_len(ncol(X_test)),
  drop = FALSE
]

colnames(shap_values) <- colnames(X_test)

# ==========================================
# 12. 説明変数ごとのSHAP重要度
# ==========================================

# ダミー変数を元の説明変数単位に集計
shap_original <- sapply(
  features_cols,
  function(v) {
    
    cols <- feature_map$encoded_feature[
      feature_map$original_feature == v
    ]
    
    rowSums(
      shap_values[, cols, drop = FALSE]
    )
  }
)

# 各説明変数のSHAP絶対値の平均
shap_importance <- data.frame(
  Feature = colnames(shap_original),
  MeanAbsSHAP = colMeans(abs(shap_original))
)

# 重要度の大きい順に並べ替える
shap_importance <- shap_importance %>%
  arrange(desc(MeanAbsSHAP))

# 結果を表示
cat("\n===== テストデータのSHAP重要度 =====\n")
print(shap_importance)




# ==========================================
# 13. 4変数のSHAP依存性プロットを作成・保存
# ==========================================

library(ggplot2)
library(dplyr)

# グラフを作成する4変数
plot_features <- c(
  "DEPFREQ_A",
  "ANXFREQ_A",
  "LONELY_A",
  "FGEFRQTRD_A"
)

# 保存先フォルダの作成
output_dir <- file.path(
  getwd(), "SHAP_dependence_plots"
)
dir.create(output_dir, showWarnings = FALSE,
           recursive = TRUE)

# テストデータの変数値とSHAP値を結合
plot_data_list <- lapply(plot_features, function(v) {
  data.frame(
    Feature = v,
    Value = as.numeric(test_data[[v]]),
    SHAP = as.numeric(shap_original[, v])
  )
})

plot_data <- bind_rows(plot_data_list)

# 各変数値における平均SHAP値
mean_data <- plot_data %>%
  group_by(Feature, Value) %>%
  summarise(
    MeanSHAP = mean(SHAP),
    N = n(),
    .groups = "drop"
  )

# ==========================================
# 14. 変数ごとのグラフを保存
# ==========================================

for (v in plot_features) {
  
  dat_v <- plot_data %>%
    filter(Feature == v)
  
  mean_v <- mean_data %>%
    filter(Feature == v)
  
  p <- ggplot(
    dat_v,
    aes(x = Value, y = SHAP)
  ) +
    
    # SHAP = 0 の基準線
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "gray50"
    ) +
    
    # 各対象者のSHAP値
    geom_jitter(
      width = 0.12,
      height = 0,
      alpha = 0.25,
      color = "steelblue",
      size = 1
    ) +
    
    # 各変数値の平均SHAP値
    geom_line(
      data = mean_v,
      aes(x = Value, y = MeanSHAP),
      inherit.aes = FALSE,
      color = "red",
      linewidth = 0.8
    ) +
    
    geom_point(
      data = mean_v,
      aes(x = Value, y = MeanSHAP),
      inherit.aes = FALSE,
      color = "red",
      size = 2.5
    ) +
    
    scale_x_continuous(
      breaks = sort(unique(dat_v$Value))
    ) +
    
    labs(
      title = paste("SHAP dependence:", v),
      x = paste(v, "(前処理後の値)"),
      y = "SHAP value"
    ) +
    
    theme_bw(base_size = 13)
  
  # PNG形式で保存
  ggsave(
    filename = file.path(
      output_dir,
      paste0("SHAP_", v, ".png")
    ),
    plot = p,
    width = 7,
    height = 5,
    dpi = 300
  )
}

# ==========================================
# 15. 4変数を1枚のグラフにまとめる
# ==========================================

p_all <- ggplot(
  plot_data,
  aes(x = Value, y = SHAP)
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray50"
  ) +
  geom_jitter(
    width = 0.12,
    height = 0,
    alpha = 0.20,
    color = "steelblue",
    size = 0.8
  ) +
  geom_line(
    data = mean_data,
    aes(x = Value, y = MeanSHAP,
        group = Feature),
    inherit.aes = FALSE,
    color = "red",
    linewidth = 0.8
  ) +
  geom_point(
    data = mean_data,
    aes(x = Value, y = MeanSHAP),
    inherit.aes = FALSE,
    color = "red",
    size = 2
  ) +
  facet_wrap(
    ~ Feature,
    ncol = 2,
    scales = "free_y"
  ) +
  scale_x_continuous(breaks = 1:5) +
  labs(
    title = "SHAP dependence plots",
    x = "Feature value",
    y = "SHAP value"
  ) +
  theme_bw(base_size = 12)

ggsave(
  filename = file.path(
    output_dir, "SHAP_4variables.png"
  ),
  plot = p_all,
  width = 12,
  height = 8,
  dpi = 300
)

# 各変数値の平均SHAP値をCSV保存
write.csv(
  mean_data,
  file = file.path(
    output_dir, "SHAP_mean_by_value.csv"
  ),
  row.names = FALSE
)

# グラフ表示
print(p_all)

cat("保存先:", output_dir, "\n")



# ==========================================
# 16. SHAP相互作用値の計算とCSV保存
# ==========================================

# 保存フォルダ
interaction_dir <- file.path(
  getwd(), "SHAP_interactions"
)
dir.create(interaction_dir, showWarnings = FALSE,
           recursive = TRUE)

# SHAP相互作用値を計算
shap_interactions <- predict(
  xgb_final,
  newdata = X_test,
  predinteraction = TRUE
)

# 配列の形状を確認
n_encoded <- ncol(X_test)
stopifnot(length(dim(shap_interactions)) == 3L)
stopifnot(all(dim(shap_interactions) ==
                c(nrow(X_test),
                  n_encoded + 1L,
                  n_encoded + 1L)))

# ダミー変数と元の変数の対応を揃える
feature_map <- feature_map[
  match(colnames(X_test),
        feature_map$encoded_feature),
  , drop = FALSE
]

if (anyNA(feature_map$encoded_feature)) {
  stop("説明変数の対応を確認してください")
}

# 元の説明変数単位で相互作用を集計
pair_results <- list()
k <- 0L

for (i in seq_len(length(features_cols) - 1L)) {
  
  v1 <- features_cols[i]
  idx1 <- which(feature_map$original_feature == v1)
  
  for (j in (i + 1L):length(features_cols)) {
    
    v2 <- features_cols[j]
    idx2 <- which(feature_map$original_feature == v2)
    
    # 各対象者の相互作用SHAP値
    each_person <- numeric(nrow(X_test))
    
    for (a in idx1) {
      for (b in idx2) {
        each_person <- each_person +
          shap_interactions[, a, b]
      }
    }
    
    k <- k + 1L
    
    pair_results[[k]] <- data.frame(
      Feature1 = v1,
      Feature2 = v2,
      MeanAbsSHAPInteraction = mean(abs(each_person)),
      MeanSHAPInteraction = mean(each_person)
    )
  }
}

# 相互作用が強い順に整理
interaction_pairs <- do.call(rbind, pair_results)

interaction_pairs <- interaction_pairs[
  order(-interaction_pairs$MeanAbsSHAPInteraction),
  , drop = FALSE
]

rownames(interaction_pairs) <- NULL

# ==========================================
# 17. 組み合わせ別CSV保存
# ==========================================

write.csv(
  interaction_pairs,
  file = file.path(
    interaction_dir,
    "SHAP_interaction_pairs.csv"
  ),
  row.names = FALSE
)

# ==========================================
# 18. 上三角行列をCSV保存
# ==========================================

interaction_matrix <- matrix(
  NA_real_,
  nrow = length(features_cols),
  ncol = length(features_cols),
  dimnames = list(features_cols, features_cols)
)

for (r in seq_len(nrow(interaction_pairs))) {
  
  interaction_matrix[
    interaction_pairs$Feature1[r],
    interaction_pairs$Feature2[r]
  ] <- interaction_pairs$MeanAbsSHAPInteraction[r]
}

write.csv(
  interaction_matrix,
  file = file.path(
    interaction_dir,
    "SHAP_interaction_upper_triangle.csv"
  ),
  na = ""
)

# 結果表示
cat("\n===== SHAP相互作用 上位10組 =====\n")
print(head(interaction_pairs, 10))

cat("保存先:", interaction_dir, "\n")


