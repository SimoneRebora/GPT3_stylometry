library(rvest)
library(stylo)
library(tidyverse)
library(reshape2)

######
### Section 1. Define baseline with Eltec
######

# read files from ELTEC level 1 format
all_files <- list.files("level1", full.names = T)

# prepare metadata collection
all_titles <- character()
all_authors <- character()

# extract metadata from xml files
for(i in 1:length(all_files)){
  
  doc <- read_html(all_files[i])
  
  all_authors[i] <- doc %>% html_nodes(xpath = "//author") %>% html_text()
  all_titles[i] <- doc %>% html_nodes(xpath = "//title") %>% html_text()
  
  
}

# clean author names
all_authors <- strsplit(all_authors, "(", fixed = T)
all_authors <- sapply(all_authors, function(x) x[1])

all_authors <- strsplit(all_authors, "\n", fixed = F)
all_authors <- sapply(all_authors, function(x) x[1])

# create metadata dataframe
metadata <- data.frame(author = all_authors, title = all_titles, file = all_files)

# find authors with more than one text
my_table <- table(metadata$author)
my_table <- as.data.frame(my_table)
my_table <- my_table[which(my_table$Freq > 1),]

# prepare IDs from filenames
all_files_tmp <- list.files("level1/")
all_files_new <- strsplit(all_files_tmp, "_")
all_files_new <- sapply(all_files_new, function(x) x[1])

metadata$IDs <- all_files_new

# filter author with more than 1 text
metadata <- metadata[which(metadata$author %in% my_table$Var1),]

# read selected files from corpus
all_texts_xml <- lapply(metadata$file, readLines)
# strip xml encoding
all_texts <- lapply(all_texts_xml, function(x) delete.markup(x, markup.type = "xml"))
# convert into stylo format (tokenized texts - default English language)
all_texts <- lapply(all_texts, txt.to.words.ext)

# add names and reorder based on them
names(all_texts) <- paste(gsub("\\W", "", metadata$author),
                          "_",
                          metadata$IDs, 
                          sep = "")

all_texts <- all_texts[order(names(all_texts))]

# split into train and test
selection <- c(((1:10)*3)-1)

train_texts <- all_texts[-selection]
test_texts <- all_texts[selection]

# split the test set into 5,000-word chunks 
test_texts <- lapply(test_texts, function(x) split(x, ceiling(seq_along(x)/5000)))
test_texts <- lapply(test_texts, function(x) x[-length(x)])

# verify stylometry efficiency on train set
stylo(gui = F,
      mfw.min = 50,
      mfw.max = 200,
      mfw.incr = 50,
      distance.measure = "wurzburg",
      parsed.corpus = train_texts,
      write.png.file = T,
      plot.custom.height = 14,
      plot.custom.width = 9,
      plot.font.size = 20)
# good clustering obtained for 150 MFW!!

# prepare loop for attribution quality
attribution_quality <- list()

# verify if loop was already done
if(file.exists("Stylometry_baseline.RData")){
  load("Stylometry_baseline.RData")
}else{
  for(i in 1:length(test_texts)){
    
    attribution_quality[[i]] <- rep(0, length(test_texts[[i]]))
    
    for(n in 1:length(test_texts[[i]])){
      
      new_corpus <- c(train_texts, test = test_texts[[i]][n])
      
      stylo_result <- stylo(gui = F,
                            mfw.min = 150,
                            mfw.max = 150,
                            mfw.incr = 0,
                            distance.measure = "wurzburg",
                            parsed.corpus = new_corpus)
      
      # calculate quality
      distances_table <- stylo_result$distance.table
      
      name <- colnames(distances_table)
      name <- strsplit(name, "_")
      name <- sapply(name, function(x) x[1])
      name <- name[-length(name)]
      
      distances_table_tmp <- data.frame(name, distance = as.vector(distances_table[dim(distances_table)[1],1:(dim(distances_table)[1]-1)]))
      
      distances_mean <- distances_table_tmp %>%
        group_by(name) %>%
        summarize(distance = mean(distance))
      
      prediction <- distances_mean$name[which.min(distances_mean$distance)]
      actual <- distances_mean$name[i]
      
      if(prediction == actual)
        attribution_quality[[i]][n] <- 1
      
    }
    
    
  }
  
  attribution_quality
  
  save(attribution_quality, file = "Stylometry_baseline.RData")
  
}

# check attribution quality overall
mean(unlist(attribution_quality))
length(unlist(attribution_quality))

#check per author
result_tmp <- sapply(attribution_quality, mean)
names(result_tmp) <- names(test_texts)
result_tmp

# save to .csv
names_tmp <- strsplit(names(result_tmp), "_")
names_tmp <- lapply(names_tmp, function(x) x[1])
names(result_tmp) <- names_tmp
write.csv(result_tmp, file = "Stylometry_baseline.csv")

######
### Section 2. GPT-3
######

# find all directories with GPT3 results
all_dirs <- list.files(path = "GPT3", pattern = "try", include.dirs = T, full.names = T)

# prepare new test set
test_texts <- vector(mode = "list", length = 10)

# loop on all dirs to concatenate tokenized texts into the test set
for(i in 1:length(all_dirs)){
  
  gpt_folder <- all_dirs[i]
  
  gpt_files <- list.files(gpt_folder, full.names = T)
  gpt_groups <- list.files(gpt_folder, full.names = F)
  gpt_groups <- strsplit(gpt_groups, "_")
  gpt_groups <- sapply(gpt_groups, function(x) x[1])
  gpt_unique <- unique(gpt_groups)
  
  texts_df <- data.frame(gpt_groups, text = "")
  
  for(n in 1:length(gpt_files)){
    
    tmp_text <- readLines(gpt_files[n])
    tmp_text <- paste(tmp_text, collapse = "\n")
    texts_df$text[n] <- tmp_text
    
  }
  
  texts_df <- texts_df %>%
    group_by(gpt_groups) %>%
    summarize(text = paste(text, collapse = "\n"))
  
  if(i == 1)
    names(test_texts) <- paste("GPT3", texts_df$gpt_groups, sep = "_")
  
  test_texts_tmp <- lapply(texts_df$text, txt.to.words.ext)
  
  for(n in 1:length(test_texts)){
    test_texts[[n]][[i]] <- test_texts_tmp[[n]]
    names(test_texts[[n]])[i] <- paste("GPT3", texts_df$gpt_groups[n], sep = "_")
  }
  
}

attribution_quality <- list()
all_predictions <- character()

attribution_matrix <- matrix(rep(0,100), nrow = 10)

all_names <- names(train_texts)
all_names <- strsplit(all_names, "_")
all_names <- sapply(all_names, function(x) x[1])
all_names <- unique(all_names)

colnames(attribution_matrix) <- all_names

if(file.exists("GPT3_results.RData")){
  load("GPT3_results.RData")
}

for(i in 1:length(test_texts)){
  
  if(!file.exists("GPT3_results.RData")){
    attribution_quality[[i]] <- numeric()
    start_point <- 1
  }else{
    start_point <- length(attribution_quality[[i]])+1
  }
  
  if(start_point > length(test_texts[[i]]))
    break
  
  for(n in start_point:length(test_texts[[i]])){
    
    new_corpus <- c(train_texts, test = test_texts[[i]][n])
    
    stylo_result <- stylo(gui = F,
                          mfw.min = 150,
                          mfw.max = 150,
                          mfw.incr = 0,
                          distance.measure = "wurzburg",
                          parsed.corpus = new_corpus)
    
    # calculate quality
    distances_table <- stylo_result$distance.table
    
    name <- colnames(distances_table)
    name <- strsplit(name, "_")
    name <- sapply(name, function(x) x[1])
    name <- name[-length(name)]
    
    distances_table_tmp <- data.frame(name, distance = as.vector(distances_table[dim(distances_table)[1],1:(dim(distances_table)[1]-1)]))
    
    distances_mean <- distances_table_tmp %>%
      group_by(name) %>%
      summarize(distance = mean(distance))
    
    prediction <- distances_mean$name[which.min(distances_mean$distance)]
    all_predictions <- c(all_predictions, prediction)
    
    attribution_matrix[i,which(colnames(attribution_matrix) == prediction)] <- attribution_matrix[i,which(colnames(attribution_matrix) == prediction)] + 1
    
    actual <- distances_mean$name[i]
    
    if(prediction == actual){
      attribution_quality[[i]][n] <- 1
    }else{
      attribution_quality[[i]][n] <- 0
    }
    
  }
  
}

colnames(attribution_matrix) <- c("Rhoda Broughton", "Charles Dickens", "Benjamin Disraeli", "George Eliot", "Edith Nesbit", "Ouida", "Anthony Trollope", "Mrs. Humphry Ward", "Herbert George Wells", "Charlotte Mary Yonge")
rownames(attribution_matrix) <- c("Rhoda Broughton", "Charles Dickens", "Benjamin Disraeli", "George Eliot", "Edith Nesbit", "Ouida", "Anthony Trollope", "Mrs. Humphry Ward", "Herbert George Wells", "Charlotte Mary Yonge")

save(attribution_matrix, attribution_quality, all_predictions, file = "GPT3_results.RData")

# create heatmap
data <- as.data.frame.table(attribution_matrix)
data_2 <- data

for(i in 1:length(data_2$Var1)){
  
  if(data_2$Var1[i] != data_2$Var2[i])
    data_2$Freq[i] <- NA
  if(data_2$Var1[i] == data_2$Var2[i])
    data_2$Freq[i] <- data_2$Freq[i]/32
  
}

p1 <- ggplot(data, aes(Var1, Var2, fill= Freq)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_gradient(low="white", high="blue") +
  xlab("Imitated author") +
  ylab("Attribution") +
  geom_text(data = data_2, mapping = aes(Var1, Var2, label = round(Freq, 2)))

p1

ggsave(p1, filename = "Heatmap.png", width = 16, height = 9, scale = 0.55)


# check all predictions
table(all_predictions)
colSums(attribution_matrix)

# check attribution quality overall
mean(unlist(attribution_quality))
length(unlist(attribution_quality))

#check per author
result_tmp <- sapply(attribution_quality, mean)
names(result_tmp) <- names(test_texts)
result_tmp

# convert attribution results to dataframe
attribution_df <- as.data.frame(attribution_quality)
colnames(attribution_df) <- names(test_texts)
  
gpt_features <- read.csv("gpt3_features.csv")
gpt_features <- gpt_features[1:length(all_dirs),]

attribution_df <- cbind(attribution_df, gpt_features)
attribution_df$efficiency <- rowMeans(attribution_df[,1:10])

# calculate efficiency based on features
attribution_df %>% group_by(prompt) %>% summarize(efficiency = mean(efficiency))
attribution_df %>% group_by(model) %>% summarize(efficiency = mean(efficiency))
attribution_df %>% group_by(temperature) %>% summarize(efficiency = mean(efficiency))

# test visualization on a single configuration
test_texts_tmp <- lapply(test_texts, function(x) x[[2]])
new_corpus <- c(train_texts, test_texts_tmp)

stylo_result <- stylo(gui = F,
                      mfw.min = 150,
                      mfw.max = 150,
                      mfw.incr = 0,
                      distance.measure = "wurzburg",
                      parsed.corpus = new_corpus)

# test visualization on all configurations
test_texts_tmp <- lapply(test_texts, unlist)
new_corpus <- c(train_texts, test_texts_tmp)

stylo_result <- stylo(gui = F,
                      mfw.min = 50,
                      mfw.max = 200,
                      mfw.incr = 50,
                      distance.measure = "wurzburg",
                      parsed.corpus = new_corpus,
                      write.png.file = T,
                      plot.custom.height = 9,
                      plot.custom.width = 14,
                      plot.font.size = 15)

# visualize z-scores for GPT3 texts
zeta_scores <- stylo_result$table.with.all.zscores
zeta_scores <- zeta_scores[21:30,1:150]

# calulate variance for each set of zeta scores (per word)
my_variance <- numeric()

for(i in 1:dim(zeta_scores)[2]){
  
  my_variance[i] <- mean(abs(zeta_scores[,i] - mean(zeta_scores[,i])))
  
  
}

# reorder the table based on the variance
zeta_scores <- as.data.frame(t(zeta_scores))
zeta_scores$variance <- my_variance
zeta_scores <- zeta_scores[order(zeta_scores$variance),]

# join variance info to the words
zeta_scores$word <- paste(rownames(zeta_scores), " (variance ", round(zeta_scores$variance, 2), ")", sep = "")
zeta_scores$variance <- NULL

# prepare first visualization (10 words with least variance)
zeta_scores_m <- melt(zeta_scores[1:8,1:11], id.vars = "word", value.name = "zeta_value", variable.name = "Style")
zeta_scores_m$word <- factor(zeta_scores_m$word, levels = zeta_scores$word[1:8])

p1 <- ggplot(zeta_scores_m, aes(x = Style, y = zeta_value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~word, nrow = 2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab(NULL)

ggsave(p1, filename = "Zeta_pos.png", width = 14, height = 10, scale = 0.45)

# prepare second visualization (20 words with least vs. most variance)
zeta_scores_m <- melt(zeta_scores[c(1:10, 141:150),1:11], id.vars = "word", value.name = "zeta_value", variable.name = "Style")
zeta_scores_m$word <- factor(zeta_scores_m$word, levels = zeta_scores$word[c(1:10, 141:150)])

p1 <- ggplot(zeta_scores_m, aes(x = Style, y = zeta_value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~word, nrow = 2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(p1, filename = "Zeta_pos_neg.png", width = 16, height = 9, scale = 1)

######
### Section 3. Visualizations to distinguish different GPT3 "voices"
######

### 1. all together vs. Eltec

test_texts_tmp <- test_texts
names(test_texts_tmp) <- NULL

for(i in 1:length(test_texts_tmp)){
  
  names(test_texts_tmp[[i]]) <- paste("GPT3-", gpt_features$model, gsub("GPT3", "", names(test_texts_tmp[[i]])), sep = "")
  
}

test_texts_tmp <- unlist(test_texts_tmp, recursive = F)

test_texts_tmp_2 <- vector(mode = "list", length = length(unique(names(test_texts_tmp))))
names(test_texts_tmp_2) <- unique(names(test_texts_tmp))

for(i in 1:length(test_texts_tmp_2)){
  
  test_texts_tmp_2[[i]] <- unlist(test_texts_tmp[which(names(test_texts_tmp) == names(test_texts_tmp_2)[i])])
  
}

new_corpus <- c(train_texts, test_texts_tmp_2)
new_corpus <- new_corpus[order(names(new_corpus))]

stylo_result <- stylo(gui = F,
                      mfw.min = 150,
                      mfw.max = 150,
                      mfw.incr = 0,
                      distance.measure = "wurzburg",
                      parsed.corpus = new_corpus,
                      write.png.file = T,
                      plot.custom.height = 9,
                      plot.custom.width = 10,
                      plot.font.size = 15)

### 2. only GPT3

new_corpus <- test_texts_tmp_2[order(names(test_texts_tmp_2))]
stylo_result <- stylo(gui = F,
                      mfw.min = 200,
                      mfw.max = 200,
                      mfw.incr = 0,
                      distance.measure = "wurzburg",
                      parsed.corpus = new_corpus,
                      write.png.file = T,
                      plot.custom.height = 9,
                      plot.custom.width = 14,
                      plot.font.size = 15)




### 3. voice by voice vs. Eltec

single_voices <- unique(gpt_features$model)

for(my_voice in single_voices){
  
  test_texts_tmp_3 <- test_texts_tmp_2[which(grepl(my_voice, names(test_texts_tmp_2)))]
  
  new_corpus <- c(train_texts, test_texts_tmp_3)
   
  stylo_result <- stylo(gui = F,
                        mfw.min = 150,
                        mfw.max = 150,
                        mfw.incr = 0,
                        distance.measure = "wurzburg",
                        parsed.corpus = new_corpus)
  
  # visualize z-scores for GPT3 texts
  zeta_scores <- stylo_result$table.with.all.zscores
  zeta_scores <- zeta_scores[21:30,1:150]
  
  # calulate variance for each set of zeta scores (per word)
  my_variance <- numeric()
  
  for(i in 1:dim(zeta_scores)[2]){
    
    my_variance[i] <- mean(abs(zeta_scores[,i] - mean(zeta_scores[,i])))
    
    
  }
  
  # reorder the table based on the variance
  zeta_scores <- as.data.frame(t(zeta_scores))
  zeta_scores$variance <- my_variance
  zeta_scores <- zeta_scores[order(zeta_scores$variance),]
  
  # join variance info to the words
  zeta_scores$word <- paste(rownames(zeta_scores), " (variance ", round(zeta_scores$variance, 2), ")", sep = "")
  zeta_scores$variance <- NULL
  
  # prepare first visualization (10 words with least variance)
  zeta_scores_m <- melt(zeta_scores[1:8,1:11], id.vars = "word", value.name = "zeta_value", variable.name = "Style")
  zeta_scores_m$word <- factor(zeta_scores_m$word, levels = zeta_scores$word[1:8])
  
  p1 <- ggplot(zeta_scores_m, aes(x = Style, y = zeta_value)) +
    geom_bar(stat = "identity") +
    facet_wrap(~word, nrow = 2) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    xlab(NULL)
  
  ggsave(p1, filename = paste("Zeta_pos_", my_voice, ".png", sep = ""), width = 14, height = 10, scale = 0.45)
  
  # prepare second visualization (20 words with least vs. most variance)
  zeta_scores_m <- melt(zeta_scores[c(1:10, 141:150),1:11], id.vars = "word", value.name = "zeta_value", variable.name = "Style")
  zeta_scores_m$word <- factor(zeta_scores_m$word, levels = zeta_scores$word[c(1:10, 141:150)])
  
  p1 <- ggplot(zeta_scores_m, aes(x = Style, y = zeta_value)) +
    geom_bar(stat = "identity") +
    facet_wrap(~word, nrow = 2) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  ggsave(p1, filename = paste("Zeta_posneg_", my_voice, ".png", sep = ""), width = 16, height = 9, scale = 1)
  
}

######
### Section 4. Verify visualisations with real author
######

### 1. Dickens full (just train set)

stylo_result <- stylo(gui = F,
                      mfw.min = 150,
                      mfw.max = 150,
                      mfw.incr = 0,
                      distance.measure = "wurzburg",
                      parsed.corpus = train_texts)

# visualize z-scores for GPT3 texts
zeta_scores <- stylo_result$table.with.all.zscores
zeta_scores <- zeta_scores[3:4,1:150]

# calulate variance for each set of zeta scores (per word)
my_variance <- numeric()

for(i in 1:dim(zeta_scores)[2]){
  
  my_variance[i] <- mean(abs(zeta_scores[,i] - mean(zeta_scores[,i])))
  
  
}

# reorder the table based on the variance
zeta_scores <- as.data.frame(t(zeta_scores))
zeta_scores$variance <- my_variance
zeta_scores <- zeta_scores[order(zeta_scores$variance),]

# join variance info to the words
zeta_scores$word <- paste(rownames(zeta_scores), " (variance ", round(zeta_scores$variance, 2), ")", sep = "")
zeta_scores$variance <- NULL

# prepare first visualization (20 words with least variance)
zeta_scores_m <- melt(zeta_scores[1:20,], id.vars = "word", value.name = "zeta_value", variable.name = "Style")
zeta_scores_m$word <- factor(zeta_scores_m$word, levels = zeta_scores$word[1:20])

ggplot(zeta_scores_m, aes(x = Style, y = zeta_value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~word, nrow = 2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# prepare second visualization (20 words with least vs. most variance)
zeta_scores_m <- melt(zeta_scores[c(1:10, 141:150),], id.vars = "word", value.name = "zeta_value", variable.name = "Style")
zeta_scores_m$word <- factor(zeta_scores_m$word, levels = zeta_scores$word[c(1:10, 141:150)])

ggplot(zeta_scores_m, aes(x = Style, y = zeta_value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~word, nrow = 2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

### 2. Dickens 20,000 words (from test set)

new_texts <- train_texts

# split one novel into 20,000-word chunks 
tmp_list <- lapply(new_texts[3], function(x) split(x, ceiling(seq_along(x)/20000)))
tmp_list <- unlist(tmp_list, recursive = F)

new_texts[3:4] <- NULL

new_texts <- c(new_texts, tmp_list)

stylo_result <- stylo(gui = F,
                      mfw.min = 150,
                      mfw.max = 150,
                      mfw.incr = 0,
                      distance.measure = "wurzburg",
                      parsed.corpus = new_texts)

# visualize z-scores for GPT3 texts
zeta_scores <- stylo_result$table.with.all.zscores
zeta_scores <- zeta_scores[19:28,1:150]

# calulate variance for each set of zeta scores (per word)
my_variance <- numeric()

for(i in 1:dim(zeta_scores)[2]){
  
  my_variance[i] <- mean(abs(zeta_scores[,i] - mean(zeta_scores[,i])))
  
  
}

# reorder the table based on the variance
zeta_scores <- as.data.frame(t(zeta_scores))
zeta_scores$variance <- my_variance
zeta_scores <- zeta_scores[order(zeta_scores$variance),]

# join variance info to the words
zeta_scores$word <- paste(rownames(zeta_scores), " (variance ", round(zeta_scores$variance, 2), ")", sep = "")
zeta_scores$variance <- NULL

# prepare first visualization (20 words with least variance)
zeta_scores_m <- melt(zeta_scores[1:20,], id.vars = "word", value.name = "zeta_value", variable.name = "Style")
zeta_scores_m$word <- factor(zeta_scores_m$word, levels = zeta_scores$word[1:20])

ggplot(zeta_scores_m, aes(x = Style, y = zeta_value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~word, nrow = 2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# prepare second visualization (20 words with least vs. most variance)
zeta_scores_m <- melt(zeta_scores[c(1:10, 141:150),], id.vars = "word", value.name = "zeta_value", variable.name = "Style")
zeta_scores_m$word <- factor(zeta_scores_m$word, levels = zeta_scores$word[c(1:10, 141:150)])

ggplot(zeta_scores_m, aes(x = Style, y = zeta_value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~word, nrow = 2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
