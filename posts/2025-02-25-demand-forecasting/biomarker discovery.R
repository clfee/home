rm(list=ls())

# --------- Clinical Metadata (with survival) -----
clinical <- colData(cptac_gbm)
clinical$response <- ifelse(clinical$OS_MONTHS > 12, "Responder", "Non-responder")

# --------- Harmonize Sample IDs  ------------
common_ids <- intersect(colnames(rna_counts), rownames(proteomics))
rna <- rna_counts[, common_ids]
prot <- proteomics[ , common_ids]
clinical <- clinical[common_ids, ]

# -------- Normalize & Filter Low-Variance Genes -------
library(DESeq2)

# RNA seq
dds <- DESeqDataSetFromMatrix(countData = rna,
                              colData = clinical,
                              design = ~ response)

dds <- dds[rowSums(counts(dds)) > 10, ]
vsd <- vst(dds, blind = FALSE)
rna_norm <- assay(vsd)

# Proteomics
prot_log <- log2(prot + 1)
prot_filtered <- prot_log[apply(prot_log, 1, var) > 0.2, ]

# ---------- Simple Differential Expression (Responder vs Non-responder) -----


clinical <- getClinicalData(cbio, "gbm_cptac_2020")

# You might want to create a response variable:
# Example: classify patients into responders/non-responders based on OS or progression
clinical$response <- ifelse(clinical$OS_MONTHS > 12, "Responder", "Non-responder")

# ----------- Visualization ----------
library(pheatmap)

# Heatmap for top 20 RNA & protein features
top_rna <- rownames(res_rna)[1:20]
top_prot <- rownames(res_prot)[1:20]

pheatmap(rna_norm[top_rna, ], annotation_col = as.data.frame(clinical$response))
pheatmap(prot_filtered[top_prot, ], annotation_col = as.data.frame(clinical$response))
