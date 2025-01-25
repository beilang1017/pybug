# 读取更新日志
log_file_path <- "/Users/celestine/Desktop/CHANGES.txt"
log_data <- readLines(log_file_path)

# 查看文件的前几行，确保加载成功
head(log_data, 10)
# 使用正则表达式提取版本号和日期
version_dates <- str_extract_all(log_data, "\\d+\\.\\d+\\.\\d+\\s*\\(\\d{4}-\\d{2}-\\d{2}\\)")
version_dates <- unlist(version_dates)
# 提取版本号
versions <- str_extract(version_dates, "^\\d+\\.\\d+\\.\\d+")

# 提取日期
dates <- str_extract(version_dates, "(?<=\\().*(?=\\))")

# 输出结果
versions
dates

# 版本发布频率分析
library(ggplot2)

# 将提取到的版本和日期合并成一个数据框
data <- data.frame(version = versions, date = as.Date(dates))

# 按月份统计每个月发布的版本数
data$month <- format(data$date, "%Y-%m")
version_count <- table(data$month)

# 转换为数据框以便绘图
version_count_df <- data.frame(month = names(version_count), count = as.integer(version_count))

# 可视化版本发布频率
ggplot(version_count_df, aes(x = month, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "PyInstaller release frequency", x = "month", y = "version number")

# 版本间的更新内容分析
library(tm)
library(SnowballC)
library(wordcloud)

# 假设已经有每个版本的更新内容 (你可以用正则表达式提取每个版本的更新部分)
# 假设 log_data 中包含版本更新日志
updates <- log_data # 将更新日志内容从文件中提取到此

# 创建文本数据集
corpus <- Corpus(VectorSource(updates))
corpus <- tm_map(corpus, content_transformer(tolower)) # 转为小写
corpus <- tm_map(corpus, removePunctuation) # 去除标点符号
corpus <- tm_map(corpus, removeNumbers) # 去除数字
corpus <- tm_map(corpus, removeWords, stopwords("en")) # 去除常见停用词
corpus <- tm_map(corpus, stripWhitespace) # 去除空格

# 创建词频矩阵
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
word_freqs_df <- data.frame(word = names(word_freqs), freq = word_freqs)

# 绘制词云
wordcloud(words = word_freqs_df$word, freq = word_freqs_df$freq, min.freq = 5, scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"))

#按功能分类的版本分析
# 假设 log_data 已经包含了每个版本的详细更新内容
# 匹配 Features、Bugfix、Hooks、Bootloader 和 Incompatible Changes
feature_updates <- grep("Features", log_data, value = TRUE)
bugfix_updates <- grep("Bugfix", log_data, value = TRUE)
hook_updates <- grep("Hooks", log_data, value = TRUE)
bootloader_updates <- grep("Bootloader", log_data, value = TRUE)
incompatible_updates <- grep("Incompatible Changes", log_data, value = TRUE)

# 统计每个类别的更新数量
category_count <- c(
  Features = length(feature_updates),
  Bugfix = length(bugfix_updates),
  Hooks = length(hook_updates),
  Bootloader = length(bootloader_updates),
  "Incompatible Changes" = length(incompatible_updates)
)

# 创建数据框并绘制条形图
category_count_df <- data.frame(category = names(category_count), count = category_count)
ggplot(category_count_df, aes(x = category, y = count)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  theme_minimal() +
  labs(title = "PyInstaller Update Categories Analysis", x = "Update Categories", y = "Update Count")

# 版本更新趋势分析
# 计算版本号之间的差异
version_dates_num <- as.Date(dates)  # 将版本日期转为日期格式
version_diff <- diff(version_dates_num)  # 计算相邻版本之间的日期差异

# 绘制版本发布时间间隔
ggplot(data.frame(interval = version_diff), aes(x = interval)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Time Between PyInstaller Releases", x = "Days Between Releases", y = "Frequency")

# 版本更新的长度分析
# 计算每次版本更新的字数
log_data_clean <- log_data[grep("^-{2,}$", log_data, invert = TRUE)] # 去除分隔符行
log_data_clean <- paste(log_data_clean, collapse = " ") # 合并所有行成一个文本
# 每个版本的更新日志分隔符行
version_log_start <- grep("^\\d+\\.\\d+\\.\\d+ \\(\\d{4}-\\d{2}-\\d{2}\\)", log_data)

# 获取每个版本更新内容的字数
word_count_per_version <- sapply(1:length(version_log_start), function(i) {
  start_index <- version_log_start[i]
  end_index <- ifelse(i < length(version_log_start), version_log_start[i + 1] - 1, length(log_data))
  length(strsplit(paste(log_data[start_index:end_index], collapse = " "), "\\s+")[[1]]) # 计算字数
})

# 获取每个版本的发布日期
version_dates <- str_extract_all(log_data[version_log_start], "\\d{4}-\\d{2}-\\d{2}")
version_dates <- as.Date(unlist(version_dates))

# 创建数据框
version_word_data <- data.frame(version = versions, date = version_dates, word_count = word_count_per_version)

# 绘制字数变化的折线图
library(ggplot2)
ggplot(version_word_data, aes(x = date, y = word_count)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "red", size = 2) +
  theme_minimal() +
  labs(title = "Word Count per Version", x = "Date", y = "Word Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 按季度划分的 Bugfix 次数折线图
# 提取Bugfix类别的更新内容
bugfix_updates <- grep("Bugfix", log_data, value = TRUE)

# 按季度统计 Bugfix 次数
data$quarter <- paste(format(data$date, "%Y"), "Q", ceiling(as.numeric(format(data$date, "%m")) / 3), sep = "")
bugfix_per_quarter <- table(data$quarter[which(data$version %in% versions[grep("Bugfix", bugfix_updates)])])

# 转换为数据框
bugfix_per_quarter_df <- data.frame(quarter = names(bugfix_per_quarter), count = as.integer(bugfix_per_quarter))

# 绘制 Bugfix 数量的折线图
ggplot(bugfix_per_quarter_df, aes(x = quarter, y = count)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "darkred", size = 2) +
  theme_minimal() +
  labs(title = "Bugfix Updates per Quarter", x = "Quarter", y = "Bugfix Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 提取各类更新信息
feature_updates <- grep("Features", log_data, value = TRUE)
hook_updates <- grep("Hooks", log_data, value = TRUE)
bootloader_updates <- grep("Bootloader", log_data, value = TRUE)
incompatible_updates <- grep("Incompatible Changes", log_data, value = TRUE)

# 按季度统计每个类别的更新次数
features_per_quarter <- table(data$quarter[which(data$version %in% versions[grep("Features", feature_updates)])])
bugfix_per_quarter <- table(data$quarter[which(data$version %in% versions[grep("Bugfix", bugfix_updates)])])
hooks_per_quarter <- table(data$quarter[which(data$version %in% versions[grep("Hooks", hook_updates)])])
bootloader_per_quarter <- table(data$quarter[which(data$version %in% versions[grep("Bootloader", bootloader_updates)])])
incompatible_per_quarter <- table(data$quarter[which(data$version %in% versions[grep("Incompatible Changes", incompatible_updates)])])

# 将所有季度的统计合并为一个统一的季度向量
all_quarters <- sort(unique(c(names(features_per_quarter), 
                              names(bugfix_per_quarter), 
                              names(hooks_per_quarter), 
                              names(bootloader_per_quarter), 
                              names(incompatible_per_quarter))))

# 使用merge确保每个类别的季度都有对应的值
features_per_quarter <- data.frame(quarter = all_quarters, count = as.integer(features_per_quarter[all_quarters]), category = "Features")
bugfix_per_quarter <- data.frame(quarter = all_quarters, count = as.integer(bugfix_per_quarter[all_quarters]), category = "Bugfix")
hooks_per_quarter <- data.frame(quarter = all_quarters, count = as.integer(hooks_per_quarter[all_quarters]), category = "Hooks")
bootloader_per_quarter <- data.frame(quarter = all_quarters, count = as.integer(bootloader_per_quarter[all_quarters]), category = "Bootloader")
incompatible_per_quarter <- data.frame(quarter = all_quarters, count = as.integer(incompatible_per_quarter[all_quarters]), category = "Incompatible Changes")

# 合并所有类别的更新数据
category_per_quarter_df <- rbind(features_per_quarter, bugfix_per_quarter, hooks_per_quarter, bootloader_per_quarter, incompatible_per_quarter)

# 将季度列转换为因子，并按季度顺序排序
category_per_quarter_df$quarter <- factor(category_per_quarter_df$quarter, levels = all_quarters)

# 绘制不同类别更新数量的折线图
ggplot(category_per_quarter_df, aes(x = quarter, y = count, color = category, group = category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Category Updates per Quarter", x = "Quarter", y = "Update Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))