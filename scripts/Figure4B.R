library(tidyr)
library(reshape2)
library(readr)
library(dplyr)
library(ggplot2)
library(ggtext)
library(ggcorrplot)
library(RColorBrewer)
library(reshape2)
library(tibble)
library(cowplot)
library(scales)
#install.packages("viridis")
library(viridis)
library(here)

#install packages needed for scale_fill_viridis

#Regions
#1A load in all data
#2A create datasets with log scaled values
#3A create triangular matrix of log scaled values
#4A convert triangular log matrix into long format ready for plotting
#5A create ggplot heatmap of triangular log matrix

#3B create triangular matrix of original values
#4B convert triangular matrix original values into long format ready for plotting
#5B create ggplot heatmap of SQUARE ROOTs of original values

#5C create ggplot heatmap of SQUARE ROOTs of log scaled values
#6 extra code for copy and pasting

#set starting directory 
#setwd("C:/Users/tesso")

#region -1. LOAD IN DATA
industry_long <- read.csv(here("raw_data", "Industry_overlap", "Industry_overlap_long.csv"))

print(industry_long)
#rename the first column to Industry1
names(industry_long)[1] <- "Industry1"

#extract unique values from Industry2 and use them to reorder the factor levels
desired_order <- unique(industry_long$Industry2)

industry_long$Industry1 <- factor(industry_long$Industry1, levels = (desired_order))
industry_long$Industry2 <- factor(industry_long$Industry2, levels = (desired_order))
#endregion




#region - 3B. CREATE TRIANGULAR MATRIX of ORIGINAL VALUES
# 3.B CREATE TRIANGULAR MATRIX of VALUES
#identify and remove duplicate rows so that we only have the unique pairs of industry, no duplicate combinations
unique_pairs_orig_df <- industry_long[!duplicated(industry_long[, c("Industry1", "Industry2")]), ]

#create a triangular matrix from unique pairs in unique_pairs_orig_df
triangular_matrix_orig <- spread(unique_pairs_orig_df, key = Industry2, value = Value)

#fill in the upper triangle with NA
triangular_matrix_orig[upper.tri(triangular_matrix_orig)] <- NA
print(triangular_matrix_orig)
#endregion

#region - 4.B CONVERT TRIANGULAR MATRIX INTO LONG FORMAT READY FOR PLOTTING
triangular_matrix_orig_long <- melt(triangular_matrix_orig, id.vars = "Industry1")

names(triangular_matrix_orig_long) <- c("Industry1", "Industry2", "Value")

#change the order of the factor levels in Industry1
triangular_matrix_orig_long$Industry1 <- factor(triangular_matrix_orig_long$Industry1, levels = rev(desired_order))

print(triangular_matrix_orig_long)
#endregion




#region - 2B Create INDUSTRY_QUANT dataframe that has quantiles from industry long values
industry_long$quantiles <- cut(industry_long$Value, breaks = quantile(industry_long$Value, na.rm = TRUE, probs = seq(0, 1, by = 0.25)), include.lowest = TRUE, labels = c("Q1", "Q2", "Q3", "Q4"))

print(industry_long)
#create new dataframe called "industry_quant" that only has the industry1, industry2 and quantiles columns
industry_quant <- industry_long[, c("Industry1", "Industry2", "quantiles")]
print(industry_quant)
#endregion


#region - 3C Create triangular matrix of industry_quant
#identify and remove duplicate rows so that we only have the unique pairs of industry, no dupilcate combinations
unique_pairs_quant_df <- industry_quant[!duplicated(industry_quant[, c("Industry1", "Industry2")]), ]

#create triangular matrix of unique_pairs_quant_df
triangular_matrix_quant <- spread(unique_pairs_quant_df, key = Industry2, value = quantiles)

#fill in the upper triangle with NA
triangular_matrix_quant[upper.tri(triangular_matrix_quant)] <- NA

print(triangular_matrix_quant)
#endregion

#region - 4C Convert triangular matrix into long format ready for plotting
triangular_matrix_quant_long <- melt(triangular_matrix_quant, id.vars = "Industry1")

names(triangular_matrix_quant_long) <- c("Industry1", "Industry2", "quantiles")

#change the order of the factor levels in Industry1
triangular_matrix_quant_long$Industry1 <- factor(triangular_matrix_quant_long$Industry1, levels = rev(desired_order))
print(triangular_matrix_quant_long)
#endregion

#region - 5C Create HEATMAP of QUANTILES triangular matrix
ggplot(data = triangular_matrix_quant_long, aes(x = Industry1, y = Industry2, fill = as.character(quantiles))) +
  geom_tile(color = NA) + 
  theme_minimal() + 
  labs(x = "Industry", y = "Industry", fill = "quantiles") +
  coord_flip() +
  scale_fill_manual(
    values = c("Q1" = "#FDE725FF", "Q2" = "#21908CFF", "Q3" = "#003f5cFF" , "Q4" = "#440154FF"),
    na.value = "white",
    guide = guide_legend()
  ) +
  theme(
    axis.text.x = element_markdown(size = 20, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_markdown(size = 20),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_markdown(size = 20),
    legend.text = element_markdown(size = 15),
    legend.key.size = unit(1, "cm"),
    plot.title = element_text(hjust = 0.5), 
    #make the background white including the plot area
    plot.background = element_rect(fill = "white")
  )

ggsave(here("figures", "Figure4B_quantiles.png"), width = 15, height = 10)


# #print all the rows that are quantile 1
# triangular_matrix_quant_long$quantiles <- as.character(triangular_matrix_quant_long$quantiles)
# triangular_matrix_quant_long[triangular_matrix_quant_long$quantiles == "Q1", ]
# 
# #print the number of each industry_quantiles
# table(triangular_matrix_quant_long$quantiles)
# 



