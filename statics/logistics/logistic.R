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
group_cols <- c("REGION")
# 調査設計変数を追加
survey_cols <- c(
  "WTFA_A",
  "PPSU",
  "PSTRAT"
)

# 使用するすべての変数
use_cols <- c(
  purpose_cols,
  features_cols,
  group_cols,
  survey_cols
)

df_selected <- df[,use_cols]

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
# 欠損値の確認
#==========================

# 目的変数と説明変数
model_cols <- c(
  purpose_cols,
  features_cols
)

# 変数ごとの欠損数と欠損率
missing_table <- data.frame(
  Variable = model_cols,
  
  Missing_N = sapply(
    df_selected[, model_cols, drop = FALSE],
    function(x) sum(is.na(x))
  ),
  
  Missing_Percent = sapply(
    df_selected[, model_cols, drop = FALSE],
    function(x) round(mean(is.na(x)) * 100, 2)
  )
)

# 欠損率の大きい順に並べる
missing_table <- missing_table[
  order(-missing_table$Missing_Percent),
]

print(missing_table)
#install.packages("survey")
library(survey)

# 調査ウェイトを数値型にする
df_selected$WTFA_A <- as.numeric(
  as.character(df_selected$WTFA_A)
)

# 調査設計変数が有効なデータ
design_ok <- with(
  df_selected,
  !is.na(WTFA_A) &
    is.finite(WTFA_A) &
    WTFA_A > 0 &
    !is.na(PPSU) &
    !is.na(PSTRAT)
)

# 調査設計を定義できるデータを確保
df_survey <- df_selected[design_ok, , drop = FALSE]

# 回帰分析に必要な変数
model_cols <- c(
  purpose_cols,
  features_cols
)

# 分析対象者のフラグ
df_survey$analysis_ok <- complete.cases(
  df_survey[, model_cols, drop = FALSE]
)

#==========================
# 完全ケース人数と除外率の確認
#==========================

n_total <- nrow(df_survey)

n_complete <- sum(df_survey$analysis_ok)

n_excluded <- n_total - n_complete

cat("調査設計が有効な人数:", n_total, "\n")

cat("完全ケース人数:", n_complete, "\n")

cat("除外人数:", n_excluded, "\n")

cat(
  "除外率:",
  round(n_excluded / n_total * 100, 2),
  "%\n"
)

# 調査設計を定義
design_all <- svydesign(
  ids = ~PPSU,
  strata = ~PSTRAT,
  weights = ~WTFA_A,
  data = df_survey,
  nest = TRUE
)

# 回帰分析の対象者だけを指定
design_analysis <- subset(
  design_all,
  analysis_ok
)

#==========================
# 全分析対象者でモデルを学習
#==========================

logistic_model <- svyglm(
  reformulate(
    features_cols,
    response = "K6SPD_A"
  ),
  design = design_analysis,
  family = quasibinomial(link = "logit")
)

# 回帰結果
model_summary <- summary(logistic_model)

print(model_summary)

#==========================
# VIF（多重共線性）の確認
#==========================

# 初回のみインストール
# install.packages("car")

library(car)

# VIFを計算
vif_values <- car::vif(logistic_model)

# 結果を表示
print(vif_values)

#==========================
# Wald検定とオッズ比
#==========================

wald_results <- model_summary$coefficients

# 95%信頼区間
ci <- confint(logistic_model)

wald_table <- data.frame(
  Variable = rownames(wald_results),
  Estimate = unname(wald_results[, 1]),
  Survey_SE = unname(wald_results[, 2]),
  Wald_t = unname(wald_results[, 3]),
  p_value = unname(wald_results[, 4]),
  OR = unname(exp(coef(logistic_model))),
  OR_lower_95 = unname(exp(ci[, 1])),
  OR_upper_95 = unname(exp(ci[, 2])),
  row.names = NULL
)

print(wald_table)









