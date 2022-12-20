#Author: Linweiyang
#Article: how EFL learners percieve LMOOCs
library(quanteda)
library(ggplot2)
#library(readtext)
#library(readr)
library(stopwords)
library(gridExtra)
library(tibble)
#library(hrbrthemes)
library(dplyr)
library(quanteda.textplots)
library(quanteda.textstats)
library(stm)
library(stminsights)
library(ggpubr)

rm(list = ls())

setwd("E:\\2022-2025学术\\2022-mooc_research\\慕课满意度")
mooc_corpus_csv <- read.csv2("reviews_seg_for_stm.csv", sep=",", header=T, encoding = "utf-8")
View(mooc_corpus_csv)
mooc_corpus_csv <- subset(mooc_corpus_csv, platform==" UMOOC" )
# Adding column based on other column:
new.mooc <- mooc_corpus_csv %>%
  mutate(course_satisfaction = case_when(
    rating<=2 ~ "negative",
    rating>4 ~ "positive")
    )           %>%
  mutate(course_label = case_when(
    startsWith(course_type, "专门") ~ "专门用途",
    startsWith(course_type, "文学") ~ "文学与文化",
    startsWith(course_type, "语言技能") ~ "语言技能",
    startsWith(course_type, "综合语种") ~ "语言技能",
    startsWith(course_type, "语法") ~ "语言技能",
    startsWith(course_type, "听力") ~ "语言技能",
    startsWith(course_type, "大学英语") ~ "语言技能",
    startsWith(course_type, "翻译") ~ "翻译",
    startsWith(course_type, "写作") ~ "翻译",
    startsWith(course_type, "语言学") ~ "专门用途",
    startsWith(course_type, "跨文化") ~ "专门用途",
  )) %>%
  mutate(course_type_label = case_when(
    startsWith(course_label, "语言") ~ 1,
    startsWith(course_label, "翻译") ~ 2,
    startsWith(course_label, "文学") ~ 3,
    startsWith(course_label, "专门") ~ 4
  ))



library(tidyverse)
df <- drop_na(new.mooc, course_satisfaction)
str_count(df$progress, "\\w+")
df <- drop_na(df, course_label)
df <- subset(df, str_count(seg_review, "\\w+")<=30)
df <- subset(df, str_count(seg_review, "\\w+")>=5)
df <- subset(df, str_count(progress, "\\w+") >1)

table(df$course_title)


negative <- df[which(df$course_satisfaction=="negative"),]
nrow(negative)
View(negative)
#positive <- df[which(df$course_satisfaction=="positive"),]
positive <- sample_n(df[which(df$course_satisfaction=="positive"),], nrow(negative)*20)

df_x <- rbind(positive,negative)

df_x_unique <- df_x %>%
  distinct(seg_review, .keep_all = TRUE) 
nrow(df_x_unique)
table(df_x_unique$course_satisfaction)/nrow(df_x_unique)*100

mooc_corpus <- corpus(df_x_unique, text_field = "seg_review")
en_stopwords <- readLines("stoplist_en.txt", encoding = "UTF-8")
zh_stopwords <- readLines("stoplist_zh.txt", encoding = "UTF-8")
toks <- tokens(mooc_corpus, remove_punct = TRUE, remove_number= TRUE, what="fastestword")
toks <- tokens_remove(toks, en_stopwords)
toks <- tokens_remove(toks, zh_stopwords, padding = TRUE)
toks <- tokens_remove(toks, pattern = "", valuetype = 'fixed')
toks <- tokens_remove(toks, pattern = "。", valuetype = 'fixed')
toks <- tokens_remove(toks, pattern = c("~","一个","江西","english","students","！！！", "...", "......","……", "第一","lot", "第三", "写作"), valuetype = 'fixed')
mooc_dfm <- dfm(toks)

# 对比主题词图
key_stat <- textstat_keyness(mooc_dfm, measure = "lr", target = df_x_unique$course_satisfaction=="positive")
textplot_keyness(key_stat, min_count = 15, 
                 show_legend = FALSE, 
                 margin = 0.4, 
                 n = 15,
                 labelsize = 3,
                 color = sample(colors(),10),
                 )

# structural topic modeling
mydfm.un.trim <-
  dfm_trim(
    mooc_dfm,
    min_termfreq = 5,
    min_docfreq = 0.1,
  ) 

dfm2stm <- convert(mooc_dfm, to = "stm")

out <- list(documents = dfm2stm$documents,
            vocab = dfm2stm$vocab,
            meta = dfm2stm$meta)


model1.stm <- stm(
  dfm2stm$documents,
  dfm2stm$vocab,
  K = 5,
  data = dfm2stm$meta,
  prevalence = ~ course_satisfaction + course_type_label + team_no,
  max.em.its = 75,
  emtol = 1e-05,
  init.type = "Spectral",
)
est1 <- estimateEffect(1:5 ~ course_satisfaction * course_type_label*team_no, model1.stm, meta = dfm2stm$meta)

model2.stm <- stm(
  dfm2stm$documents,
  dfm2stm$vocab,
  K = 6,
  data = dfm2stm$meta,
  prevalence = ~ course_satisfaction + course_type_label + team_no,
  max.em.its = 75,
  emtol = 1e-05,
  init.type = "Spectral"
)
est2 <- estimateEffect(1:6 ~ course_satisfaction * course_type_label*team_no, model2.stm, meta = dfm2stm$meta)

model3.stm <- stm(
  dfm2stm$documents,
  dfm2stm$vocab,
  K =7,
  data = dfm2stm$meta,
  prevalence = ~ course_satisfaction + course_type_label + team_no,
  max.em.its = 75,
  init.type = "Spectral"
)
est3 <- estimateEffect(1:7 ~ course_satisfaction * course_type_label*team_no, model3.stm, meta = dfm2stm$meta)

model4.stm <- stm(
  dfm2stm$documents,
  dfm2stm$vocab,
  K = 8,
  data = dfm2stm$meta,
  prevalence = ~ course_satisfaction + course_type_label + team_no,
  max.em.its = 75,
  init.type = "Spectral"
)
est4 <- estimateEffect(1:8 ~ course_satisfaction * course_type_label*team_no, model4.stm, meta = dfm2stm$meta)

model5.stm <- stm(
  dfm2stm$documents,
  dfm2stm$vocab,
  K = 9,
  data = dfm2stm$meta,
  prevalence = ~ course_satisfaction + course_type_label + team_no,
  max.em.its = 75,
  init.type = "Spectral"
)
est5 <- estimateEffect(1:9 ~ course_satisfaction * course_type_label*team_no, model5.stm, meta = dfm2stm$meta) 

model6.stm <- stm(
  dfm2stm$documents,
  dfm2stm$vocab,
  K = 10,
  data = dfm2stm$meta,
  prevalence = ~ course_satisfaction + course_type_label + team_no,
  max.em.its = 75,
  init.type = "Spectral"
)
est6 <- estimateEffect(1:10 ~ course_satisfaction * course_type_label*team_no, model6.stm, meta = dfm2stm$meta) 

model7.stm <- stm(
  dfm2stm$documents,
  dfm2stm$vocab,
  K = 11,
  data = dfm2stm$meta,
  prevalence = ~ course_satisfaction + course_type_label + team_no,
  max.em.its = 75,
  init.type = "Spectral"
)
est7 <- estimateEffect(1:11 ~ course_satisfaction * course_type_label*team_no, model7.stm,  meta = dfm2stm$meta)

model8.stm <- stm(
  dfm2stm$documents,
  dfm2stm$vocab,
  K = 12,
  data = dfm2stm$meta,
  prevalence = ~ course_satisfaction + course_type_label + team_no,
  max.em.its = 75,
  init.type = "Spectral"
)
est8 <- estimateEffect(1:12 ~ course_satisfaction * course_type_label*team_no, model8.stm, meta = dfm2stm$meta) 

model9.stm <- stm(
  dfm2stm$documents,
  dfm2stm$vocab,
  K = 13,
  data = dfm2stm$meta,
  prevalence = ~ course_satisfaction + course_type_label + team_no,
  max.em.its = 75,
  init.type = "Spectral"
)
est9 <- estimateEffect(1:13 ~ course_satisfaction * course_type_label*team_no, model9.stm, meta = dfm2stm$meta)

model10.stm <- stm(
  dfm2stm$documents,
  dfm2stm$vocab,
  K = 14,
  data = dfm2stm$meta,
  prevalence = ~ course_satisfaction + course_type_label + team_no,
  max.em.its = 75,
  init.type = "Spectral"
)
est10 <- estimateEffect(1:14 ~ course_satisfaction * course_type_label*team_no, model10.stm, meta = dfm2stm$meta)

model11.stm <- stm(
  dfm2stm$documents,
  dfm2stm$vocab,
  K = 15,
  data = dfm2stm$meta,
  prevalence = ~ course_satisfaction + course_type_label + team_no,
  max.em.its = 75,
  init.type = "Spectral"
)
est11 <- estimateEffect(1:15 ~ course_satisfaction * course_type_label*team_no, model11.stm, meta = dfm2stm$meta)

model12.stm <- stm(
  dfm2stm$documents,
  dfm2stm$vocab,
  K = 16,
  data = dfm2stm$meta,
  prevalence = ~ course_satisfaction + course_type_label + team_no,
  max.em.its = 75,
  init.type = "Spectral"
)
est12 <- estimateEffect(1:16 ~ course_satisfaction * course_type_label*team_no, model12.stm, meta = dfm2stm$meta)

model13.stm <- stm(
  dfm2stm$documents,
  dfm2stm$vocab,
  K = 17,
  data = dfm2stm$meta,
  prevalence = ~ course_satisfaction + course_type_label + team_no,
  max.em.its = 75,
  init.type = "Spectral"
)
est13 <- estimateEffect(1:16 ~ course_satisfaction * course_type_label*team_no, model13.stm, meta = dfm2stm$meta)

model14.stm <- stm(
  dfm2stm$documents,
  dfm2stm$vocab,
  K = 18,
  data = dfm2stm$meta,
  prevalence = ~ course_satisfaction + course_type_label + team_no,
  max.em.its = 75,
  init.type = "Spectral"
)
est14 <- estimateEffect(1:18 ~ course_satisfaction * course_type_label*team_no, model14.stm, meta = dfm2stm$meta)

model15.stm <- stm(
  dfm2stm$documents,
  dfm2stm$vocab,
  K = 19,
  data = dfm2stm$meta,
  prevalence = ~ course_satisfaction + course_type_label + team_no,
  max.em.its = 75,
  init.type = "Spectral"
)
est15 <- estimateEffect(1:19 ~ course_satisfaction * course_type_label*team_no, model15.stm, meta = dfm2stm$meta)

model16.stm <- stm(
  dfm2stm$documents,
  dfm2stm$vocab,
  K = 20,
  data = dfm2stm$meta,
  prevalence = ~ course_satisfaction + course_type_label + team_no,
  max.em.its = 75,
  init.type = "Spectral"
)
est16 <- estimateEffect(1:20 ~ course_satisfaction * course_type_label*team_no, model16.stm, meta = dfm2stm$meta)

save.image('model-progress.RData')


stminsights::run_stminsights()
load("model2.RData")


plot(est7, model = model7.stm, method = "difference", covariate = "course_type_label", 
     topics = c(1,2,3,4,5,6,7,8,9,10,11),
     cov.value1 = "Positive", cov.value2 = "Negative", 
     xlab = "More Negative                                    More Positive", 
     main = "Effect of Positive vs. Negative Topics", 
     xlim = c(-0.05, 0.05), labeltype = "custom", 
     custom.labels = c(
       "学习期望",
       "教学风格",
       "语言技能",
       "课堂收获",
       "学习方法",
       "知识性",
       "课程内容",
       "教师素质", 
       "学术型",
       "问题与挑战","高阶课程 "
       )
)

