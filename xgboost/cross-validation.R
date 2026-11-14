#===========================
#ファイルの読み込み
#===========================
zip_file <- "adult24csv.zip"
print(unzip(zip_file,list = TRUE))#中身を展開せずにzipファイルの中身の一覧を表示
tmp_dir <- tempdir()#Rの一時保存先フォルダを取得
print(tmp_dir)
unzip(zip_file,exdir = tmp_dir)#tmp_dirにzipファイルを解凍
csv_files <- list.files(#フォルダ内のファイル一覧を取得
  tmp_dir,#探す対象のフォルダ
  pattern = "\\.csv$",#名前が.csvで終わるファイルのみ対象
  full.names = TRUE#ファイル名だけでなく、フォルダを含む完全なパスを返す
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
# 2. scale_pos_weightを計算
# ==========================

n_yes <- sum(train_data$K6SPD_A == "Yes")
n_no  <- sum(train_data$K6SPD_A == "No")

scale_pos_weight_value <- n_no / n_yes

cat("Yesの件数:", n_yes, "\n")
cat("Noの件数:", n_no, "\n")
cat("scale_pos_weight:",
    scale_pos_weight_value, "\n")

# ==========================
# 3. 層化5分割交差検証
# ==========================

set.seed(123)

folds <- createFolds(
  y = train_data$K6SPD_A,
  k = 5,
  returnTrain = FALSE
)

cv_control <- trainControl(
  method = "cv",
  number = 5,
  indexOut = folds,
  index = lapply(
    folds,
    function(i) setdiff(
      seq_len(nrow(train_data)), i
    )
  ),
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  verboseIter = TRUE
)

# ==========================
# 4. ハイパーパラメータ
# ==========================

tune_grid <- expand.grid(
  nrounds = c(100,200,300),
  max_depth = c(2,3,4,5,6),
  eta = seq(0.1, 0.2, by = 0.01),
  gamma = c(0),
  colsample_bytree = c(1),
  min_child_weight = c(1),
  subsample = c(1)
)

# ==========================
# 5. XGBoostの学習
# ==========================

set.seed(123)

xgb_model <- train(
  K6SPD_A ~ .,
  data = train_data,
  method = "xgbTree",
  trControl = cv_control,
  tuneGrid = tune_grid,
  metric = "ROC",
  maximize = TRUE,
  
  # クラス不均衡への対応
  scale_pos_weight = scale_pos_weight_value,
  
  # GPU設定
  tree_method = "hist",
  device = "cuda",
  
  verbose = FALSE,
  nthread = 1
)

# ==========================
# 6. 最適パラメータの表示
# ==========================

print(xgb_model$bestTune)
print(xgb_model)

#==========================
# 6. 交差検証の結果を確認
#==========================

print(xgb_model)

# 各パラメータの結果一覧
print(xgb_model$results)

# 最適モデルの交差検証AUC
best_result <- xgb_model$results[
  which.max(xgb_model$results$ROC),
]

cat("交差検証AUC:", best_result$ROC, "\n")
# ==========================
# 最適パラメータでの交差検証
# Balanced Accuracyの計算
# ==========================

# 最適パラメータの検証データ予測を取得
cv_pred <- xgb_model$pred

# 各foldのBalanced Accuracyを計算
cv_balanced_accuracy <- sapply(
  split(cv_pred, cv_pred$Resample),
  function(df) {
    
    # 確率0.48を閾値としてクラスを判定
    pred_class <- factor(
      ifelse(df$Yes >= 0.48, "Yes", "No"),
      levels = c("Yes", "No")
    )
    
    # 混同行列
    cm <- confusionMatrix(
      data = pred_class,
      reference = factor(df$obs,
                         levels = c("Yes", "No")),
      positive = "Yes"
    )
    
    # Balanced Accuracy
    as.numeric(cm$byClass["Balanced Accuracy"])
  }
)

# 各foldの結果
print(cv_balanced_accuracy)

# 5foldの平均Balanced Accuracy
cat("交差検証 Balanced Accuracy（5fold平均）:",
    round(mean(cv_balanced_accuracy) * 100, 2),
    "%\n")



