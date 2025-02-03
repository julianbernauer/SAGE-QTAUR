library(readtext)
library(quanteda)
library(tidyverse)
library(lmtest)
library(ggplot2)

datadir <- getwd()
load(paste0(datadir,"/Data/chapter7/pop_de.RData"))

green <- tempbed$wemb_score_pop[tempbed$party=="Grüne"&tempbed$cntry=="DE"]
svp <- tempbed$wemb_score_pop[tempbed$party=="SVP"]
fpoe <- tempbed$wemb_score_pop[tempbed$party=="FPÖ"]


a <- plotdata_de$year[plotdata_de$party=="AfD"]
b <- plotdata_de$wembed_pop[plotdata_de$party=="AfD"]

ggplot() +
geom_point(mapping = aes(x = a, y = b), shape = 13, size = 7) +
scale_x_continuous(limits = c(2009, 2021),breaks =
c(2009,2013,2017,2021)) +
scale_y_continuous(limits = c(0,1)) +
labs(y = "Semantic Similarity with ’volk elite souverän’",
x = "AfD Manifesto / Election Year"
)

ggplot(data = plotdata_de) +
geom_point(mapping = aes(x = wembed_pop, y = pop_bruno_manifesto,
shape = cntry), size = 3) +
geom_smooth(mapping = aes(x = wembed_pop, y = pop_bruno_manifesto),
method = "lm", se = FALSE, linetype = "dashed", color = "black") +
geom_smooth(mapping = aes(x = wembed_pop, y = pop_bruno_manifesto),
method = "loess", se = TRUE, color = "black") +
ggrepel::geom_label_repel(aes(x = wembed_pop, y = pop_bruno_manifesto,
label = party)) +
labs(
x = "Semantic Similarity with ’volk elite souverän’ (Word Embeddings)",
y = "Holistic Manual Coding (Hawkins and Casthano Silva 2018)",
shape = "Country"
)

# fig. 7.3 - replacement 

dotchart(tempbed$wemb_score_pop,labels=label,cex=.4, xlab="Word
Embeddings Scaled Populism")

# Sample 12 random indices
set.seed(321) # Set seed for reproducibility
sample_indices <- sample(seq_len(nrow(tempbed)), 12)

# Subset the data
sampled_scores <- tempbed$wemb_score_pop[sample_indices]
sampled_labels <- label[sample_indices]

# Sort the sampled data by value
sorted_indices <- order(sampled_scores)
sampled_scores <- sampled_scores[sorted_indices]
sampled_labels <- sampled_labels[sorted_indices]

# Plot the random sample of 12 scores
dotchart(sampled_scores, labels = sampled_labels, cex = .8, xlab = "Word Embeddings Scaled Populism")


png(filename = paste0(datadir, "/Figures/fig7_3_sampled_scores.png"), width = 10, height = 8, units = "in", res = 300)
dotchart(sampled_scores, labels = sampled_labels, cex = .8, xlab = "Word Embeddings Scaled Populism")
dev.off()


par(mfrow=c(3,3))
plot(tempbed$year[tempbed$party=="AfD"],tempbed$wemb_score_
pop[tempbed$party=="AfD"],
main="AfD (DE)", xlab="", ylab="",pch=13, cex=3,
ylim=c(0,1), xlim=c(2006,2018))
plot(tempbed$year[tempbed$party=="CDU"],tempbed$wemb_score_
pop[tempbed$party=="CDU"],
main="CDU (DE)", xlab="", ylab="",pch=13, cex=3,
ylim=c(0,1), xlim=c(2006,2018))
plot(tempbed$year[tempbed$party=="Grüne"&tempbed$cntry=="DE"],
tempbed$wemb_score_pop[tempbed$party=="Grüne"&tempbed$cntry=="DE"],
main="Green (DE)", xlab="", ylab="",pch=13, cex=3,
ylim=c(0,1), xlim=c(2006,2018))
plot(tempbed$year[tempbed$party=="SVP"],tempbed$wemb_score_
pop[tempbed$party=="SVP"],
main="SVP (CH)", xlab="", ylab="Populism Score",pch=13,
cex=3, ylim=c(0,1), xlim=c(2006,2018))
plot(tempbed$year[tempbed$party=="CVP"],tempbed$wemb_score_
pop[tempbed$party=="CVP"],
main="CVP (CH)", xlab="", ylab="",pch=13, cex=3,
ylim=c(0,1), xlim=c(2006,2018))

plot(tempbed$year[tempbed$party=="SPS"],tempbed$wemb_score_
pop[tempbed$party=="SPS"],
main="SPS (CH)", xlab="", ylab="",pch=13, cex=3,
ylim=c(0,1), xlim=c(2006,2018))
plot(tempbed$year[tempbed$party=="FPÖ"],tempbed$wemb_score_
pop[tempbed$party=="FPÖ"],
main="FPÖ (AT)", xlab="", ylab="",pch=13, cex=3,
ylim=c(0,1), xlim=c(2006,2018))
plot(tempbed$year[tempbed$party=="ÖVP"],tempbed$wemb_score_
pop[tempbed$party=="ÖVP"],
main="ÖVP (AT)", xlab="Election Year", ylab="",pch=13,
cex=3, ylim=c(0,1), xlim=c(2006,2018))
plot(tempbed$year[tempbed$party=="SPÖ"],tempbed$wemb_score_
pop[tempbed$party=="SPÖ"],
main="SPÖ (AT)", xlab="", ylab="",pch=13, cex=3,
ylim=c(0,1), xlim=c(2006,2018))

par(mfrow=c(1,2))
acf(svp, lag.max=1, main = "")
acf(fpoe, lag.max=1, main = "")

pred_mod <- lm(tempbed$wemb_score_pop ~ tempbed$party_polidoc_
long)
summary(pred_mod)$r.squared
summary(pred_mod)$adj.r.squared



