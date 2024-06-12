library(tidyverse)

# Set your directory and import your file. The data should include columns for p value, log(fc), and -log(p value)
# named "padj", "log_fc", and "minuslogpadj", respectively. The file should also include a column named "Label" in
# which each feature the user wants labelled has an "L" in the corresponding row of the "Label" column.
setwd("your directory")
samples <- read.csv("your data", sep = ",", dec = ".", header = TRUE, na = "NA", stringsAsFactors = FALSE)

# A new column, "Expression", is added based on whether a feature is upregulated, downregulated, or not significant according
# to set parameters. Those features are then assigned a color accordingly.
samples <-   mutate(samples, Expression = case_when(log_fc >= 1 & padj <= 0.05 ~ "up",
                                          log_fc <= -1 & padj <= 0.05 ~ "down",
                                          TRUE ~ "ns"))
cols <- c("up" = "red", "down" = "blue", "ns" = "grey") 

# Used to automatically set the axes based on the farthest located feature.
leftmost_x <- floor(min(samples$log_fc))
rightmost_x <- ceiling(max(samples$log_fc))
x_lim <- max(c(abs(leftmost_x), rightmost_x))
y_lim <- ceiling(max(samples$minuslogp))

# Creates a volcano plot and writes it to the set folder. 
tiff("your file name", res = 300, height = 5, width = 5, units = "in")
ggplot(data = samples, aes(x = log_fc, y = minuslogp, color = Expression)) + 
  geom_point() +
  geom_vline(xintercept = c(log2(0.5), log2(2)), linetype = "dashed") + #Adds dashed lines.
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
  scale_y_continuous(name = "-Log10(q-value)", limits = c(0,y_lim+1), #Names axes and adds scales according to limits assigned above.
                     breaks = seq(0,y_lim+1,2)) +
  scale_x_continuous(name = "Log2(fold change)", limits = c(-x_lim,x_lim),
                     breaks = c(-x_lim:x_lim)) +
  scale_color_manual(values = cols) + #Colors added according to expression assigned above.
  theme_classic() + #Can be deleted if classic theme is not desired
  theme(axis.title = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 14, color = "black"),
        legend.position = "none") +
  if ("Label" %in% names(samples)){
  geom_text_repel(data = filter(samples, Label=="L"), aes(label = Features), color = "black", force = 1, force_pull = 5, nudge_y = 0.6)} #Labels features with "L" in label column., The last 3 parameters can be changed if the labels are not to the user's desires.
  
dev.off()
