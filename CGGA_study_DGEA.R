## ---- setup--------------------------------------------------------
knitr::opts_knit$set(echo = TRUE, root.dir = '/home/mehdimerbah/R Projects/RNASeqDA/data')
library(DESeq2)


## ------------------------------------------------------------------
disease_raw_data <- read.table("CGGA_mRNAseq_325_RSEM_genes.txt",head=T,row.names = 1)
disease_raw_data[1:5, 1:5]


## ------------------------------------------------------------------
metadata <- read.table("CGGA_mRNAseq_325_clinical.txt", sep ="\t", head=T,row.names = 1)
meta <- metadata[order(row.names(metadata)),]
raw <- disease_raw_data[,order(colnames(disease_raw_data))]
NAs <- rownames(meta[is.na(meta$Grade),])
meta <- meta[!rownames(meta)%in%NAs, ]
raw <- raw[ ,!colnames(raw)%in%NAs]
samples <- cbind(rownames(meta), colnames(raw))
samples


## ------------------------------------------------------------------
meta$Grade <- as.factor(meta$Grade)
rounded <- round(raw)
rounded[1:10,1:5]


## ------------------------------------------------------------------
dds <- DESeqDataSetFromMatrix(countData = rounded, colData = meta, design = ~ Grade)
res <- DESeq(dds)
expRes_4v2 <- results(res)
head(expRes_4v2[order(expRes_4v2$padj),], 10)


## ------------------------------------------------------------------
DESeq2::plotDispEsts(res)
plotMA(expRes_4v2, ylim=c(-4,4))


## ------------------------------------------------------------------
log2FoldChange<- expRes_4v2$log2FoldChange
padj<- expRes_4v2$padj
pvalue<- expRes_4v2$pvalue
alpha <- 0.05 # Threshold on the adjusted p-value
cols <- densCols(log2FoldChange, -log10(padj))
plot(log2FoldChange, -log10(padj), col=cols, panel.first=grid(),
     main="Volcano plot", xlab="Effect size: log2(fold-change)", ylab="-log10(adjusted p-value)",
     pch=20, cex=0.6)
abline(v=0)
abline(v=c(-2,2), col="brown")
abline(h=-log10(alpha), col="brown")

filtered <- abs(log2FoldChange) > 2 & padj < alpha 
text(log2FoldChange[filtered],
     -log10(padj)[filtered],
     lab=rownames(res)[filtered], cex=0.4)


## ------------------------------------------------------------------
filtered_4v2 <- subset(expRes_4v2, padj<0.05 & abs(log2FoldChange)>2)
gene_list_4v2 <- rownames(filtered_4v2)
gene_list_4v2
network_matrix <- rounded[gene_list_4v2,]
network_matrix


## ------------------------------------------------------------------
library(GENIE3)
weight_matrix <- GENIE3(as.matrix(network_matrix))
weight_matrix[1:10,1:10]
gene_link_list <- getLinkList(weight_matrix, threshold = 0.05)
gene_link_list
reg_genes <- gene_link_list$regulatoryGene


## ------------------------------------------------------------------
library(igraph)
library(Rgraphviz)
g <- graph.ring(10, dir=TRUE)
graph_data <- graph.data.frame(gene_link_list, directed = F)
adj_list <- get.adjacency(graph_data, sparse = T, attr = "weight", type = "both")

adj_graph <- graph.adjacency(adj_list, mode = "undirected", weighted = T)

#graph.isomorphic(g, g_arasi)
plot(adj_graph, edge.arrow.size=.5, vertex.color="grey", vertex.size=15,

     vertex.frame.color="gray", vertex.label.color="black", 

     vertex.label.cex=0.01, vertex.label.dist=1.3, edge.curved=0.2)


## ------------------------------------------------------------------
library(enrichR)
dbs <- enrichR::listEnrichrDbs()
dbs
dbs <- c("GO_Molecular_Function_2015", "GO_Cellular_Component_2015", "GO_Biological_Process_2015")
enriched_4v2 <- enrichr(gene_list_4v2, dbs)


## ------------------------------------------------------------------
head(enriched_4v2)
plotEnrich(enriched_4v2[[3]],  title= "Enrichment 4 vs 2")



## ------------------------------------------------------------------
expRes_4v3 <- results(res, contrast = c("Grade","4", "3"))
expRes_4v3[order(expRes_4v3$padj),]
expRes_3v2 <- results(res, contrast = c("Grade","3", "2"))
expRes_3v2[order(expRes_3v2$padj),]


## ------------------------------------------------------------------
filtered_4v3 <- subset(expRes_4v3, pvalue<0.05 & abs(log2FoldChange)>1)
gene_list_4v3 <- rownames(filtered_4v3)
filtered_3v2 <- subset(expRes_3v2, pvalue<0.05 & abs(log2FoldChange)>1)
gene_list_3v2 <- rownames(filtered_3v2)
enriched_4v3 <- enrichr(gene_list_4v3, dbs)
enriched_3v2 <- enrichr(gene_list_3v2, dbs)
plotEnrich(enriched_4v3[[3]], title= "Enrichment 4 vs 3")
plotEnrich(enriched_3v2[[3]], title= "Enrichment 3 vs 2")



## ------------------------------------------------------------------
length(gene_list_3v2)
length(gene_list_4v2)
length(gene_list_4v3)
intersect(intersect(gene_list_3v2, gene_list_4v2), gene_list_4v3)


## ------------------------------------------------------------------
library(ggplot2)
pca <- prcomp(t(rounded))
plot(pca$x[,1], pca$x[,2], xlab = "PC1", ylab = "PC2")
percentVar <- round(100*pca$sdev^2/sum(pca$sdev^2),1)

sd_ratio <- sqrt(percentVar[2] / percentVar[1])

pca_data <- data.frame(Sample = rownames(pca$x), PC1 = pca$x[,1], PC2 = pca$x[,2])

ggplot(pca_data, aes(x = PC1, y = PC2, label = Sample))+ geom_point() +
  ggtitle("PCA of Gene-level Count Estimates", ) +
  xlab(paste0("PC1, VarExp: ", percentVar[1], "%")) +
  ylab(paste0("PC2, VarExp: ", percentVar[2], "%")) +
  theme_bw()

#barplot(percentVar, main ="Scree Plot", xlab = "Principal Component", ylab = "Percent Variation")


