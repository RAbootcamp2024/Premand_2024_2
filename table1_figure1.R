library(tidyverse)
install.packages("broom")
# install.packages("texreg")
# install.packages("fixest")
install.packages("gridExtra")

library(texreg)
library(plm)
library(fixest)
library(broom)
library(gridExtra)

int_dir <- "C:/Users/Owner/Desktop/Premand_2024_2_new/replication/datasets/final/"
file_name <- paste(int_dir, "village_250.dta", sep = "")

df <- haven::read_dta(file_name)

df <- pdata.frame(df, index = c("codelocalite", "year")) 
  df <- df |>
  mutate(dropped = !is.na(lag(villagehadCT2y)) & is.na(villagehadCT2y),
         year = as.numeric(year))

tab_2015 <- df%>%
  filter(dropped == TRUE & year == 2015)%>%
  count(commune)

tab_2016 <- df%>%
  filter(dropped == TRUE & year == 2016)%>%
  count(commune)

tab_2017 <- df%>%
  filter(dropped == TRUE & year == 2017)%>%
  count(commune)

# TABLE 1
table_name <- "Table1_mainres"
est_list <- list()
outcomes <- c("nbfeatures18_250",  "hadconflict6_250")
df |> select(year, communeid) |>
  as.tibble()

for (y in outcomes){
  # TWFE
  model1 <- feols(as.formula(paste(y , "~villagehadCT2y | year + communeid")),
                                   data = df, cluster = ~communeid)
  
  est_list[[paste0("R_", y ,"_1")]] <- model1
  
  # time FE * 
  model2 <- feols(as.formula(paste(y , "~villagehadCT2y | year * communeid")),
                                   data = df, cluster = ~communeid)
                             
  est_list[[paste0("R_", y ,"_2")]] <- model2
}

model2 <- feols(as.formula(paste("nbfeatures18_250" , "~villagehadCT2y | year * communeid")),
                data = df, cluster = ~communeid)

summary(est_list)
colnum <- length(est_list)
#write table
texreg(est_list,
       file = paste0(table_name,".tex"),
       custom.model.names = c("TWFE \\ Nearest neighbor", "RCT \\ Nearest neighbor",
                              "TWFE \\ 10km radius", "RCT \\ 10km radius"),
       omit.stat = c("Num. groups: year", "Num. groups: communeid", "R$^2$ (full model)", "R$^2$ (proj model)", "Adj. R$^2$ (proj model)", "Num. groups: year:communeid"),
       custom.coef.map = list("villagehadCT2y" = "Treated(Last 2 years)"),
       #custom.gof.names = c("Observations","$R^2$"),
       custom.note = "\\begin{tabular}{l*{" ~ colum ~ "}{c}} \\ toprule \\bottomrule \\end{tabular}",
       stars = c(0.01, 0.05, 0.1),
       single.row = TRUE,
       include.nobs = TRUE, 
       include.rsquared = TRUE,
       use.packages = FALSE,
       digits = 5 
       )

#figure1

figure1_name <- "figure1_mainres"
fig1_list <- list()

titles <- c("Nearest neighbor", "10km radius")
y_positions <- c(0.018, 0.042)

for (i in seq_along(outcomes)) {
  var <- outcomes[i]
  title <- titles[i]
  ypos <- y_positions[i]
  
  # 回帰分析
  model <- lm(as.formula(paste(var, "~ CT_1y + CT_2y + CT_3y + CT_4y + factor(year) * factor(communeid)")),
              data = df)
  
  # 結果を整形
  tidy_model <- tidy(model, conf.int = TRUE)
  
  # 結果をプロット用に整形
  plot_data <- tidy_model %>%
    filter(term %in% paste0("CT_", 1:4, "y")) %>%
    mutate(term = as.factor(term),
           estimate = estimate,
           ci_upper = estimate + qnorm(0.975) * std.error,
           ci_lower = estimate - qnorm(0.975) * std.error)
  
  # プロットを作成
  p <- ggplot(plot_data, aes(x = term, y = estimate, ymin = ci_lower, ymax = ci_upper)) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.3, size = 1, color="green") +
    geom_point(color="red", size=5) +
    geom_hline(yintercept = 0, color = "black") +
    geom_vline(xintercept = 2.5, linetype = "dashed", color = "black") +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "black") +
    annotate("text", x = 0.6, y = ypos, label = "beneficiaries start \n receiving CT", size = 8, hjust = 0) +
    annotate("text", x = 2.6, y = ypos, label = "beneficiaries stop \n receiving CT", size = 8, hjust = 0) +
    labs(title = title, x = "Years from initial Cash Transfer receipt", y = "Severe conflict", size = 8) +
    scale_x_discrete(labels = c("CT_1y" = "1st year", "CT_2y" = "2nd year", "CT_3y" = "3rd year", "CT_4y" = "4th year")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 30, hjust = 0.5),  # タイトルの文字サイズ
      axis.title.x = element_text(size = 25),  # x 軸ラベルの文字サイズ
      axis.title.y = element_text(size = 25),  # y 軸ラベルの文字サイズ
      axis.text.x = element_text(size = 16, angle = 45, hjust = 1),  # x 軸目盛りの文字サイズ
      axis.text.y = element_text(size = 16),  # y 軸目盛りの文字サイズ
      legend.text = element_text(size = 16)  # 凡例の文字サイズ（必要に応じて）
    )
  
  # プロットをリストに追加
  fig1_list[[var]] <- p
}


# グラフを保存
ggsave("event_study_plots.png", arrangeGrob(grobs = fig1_list, ncol = 2), width = 24, height = 12)
