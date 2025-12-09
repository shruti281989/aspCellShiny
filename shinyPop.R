library(Seurat)
library(ShinyCell2)

setwd("~/umea/R/")
seu <- readRDS("shortTrmNitScRNA/RData/integAllLayers.rds")

scConf <- createConfig(seu)

# scConf <- delMeta(scConf, c("seq_folder", "nCount_SCT",
#                             "nFeature_SCT", "tree1.pvalue.P", "log10GenesPerUMI",
#                             "exprTungAll.pvalue.P"))

scConf <- delMeta(scConf, c("nCount_RNA","nFeature_RNA"))

palette <- c("green","grey","blue","pink", 
             "darkorange","purple","turquoise","magenta2",
             "olivedrab1","yellow","red")

scConf = modColours(scConf, meta.to.mod = "tree1.ID.P", 
                    new.colours= palette)

# scConf = modMetaName(scConf, 
#                      meta.to.mod = "integrated_snn_res.0.6", 
#                      new.name = "Clusters")

scConf = modMetaName(scConf, 
                     meta.to.mod = c("exprTungAll.ID.P", "exprTungAll.cor.P"), 
                     new.name = c("Cell types from LCM in Tung et al., 2023", 
                                  "Highest Pearson Correl Value with cell types from LCM in Tung et al., 2023"))

scConf = modMetaName(scConf, 
                     meta.to.mod = c("tree1.ID.P", "tree1.cor.P"), 
                     new.name = c("Tissue section from Aspwood", 
                                  "Highest Pearson Correl Value with tissue section from Aspwood"))

scConf = modColours(scConf, meta.to.mod = "exprTungAll.ID.P", 
                    new.colours= c("seagreen","blue","pink"))

showLegend(scConf)


checkConfig(scConf, seu)

makeShinyFiles(seu, scConf = scConf, dimred.to.use = "umap",
               shiny.prefix = "scPop", shiny.dir = "shinyApp/")

makeShinyCodes(shiny.prefix = "scPop", 
               shiny.dir = "shinyApp/", 
               shiny.title="Populus wood single cell RNA-seq")

shiny::runApp("app/")

# Modify the ui.R and server.R files to customize the app further like,
# 1. Change the name of first tab to zoom-enable UMAP
# 2. Change the helper content for each selection
# 3. Change the title
# 4. Add the gene search tab for potra-potri also
# showing it as extra in every plot
# 5. setting the genes of interest as standard in bubble plot
# 6. Add contact tab with specific info
# 7. specific info to helper tab etc.

# 8. Create hyperlinks to the geneIDs in the gene search tab 
makePlantGenieLink <- function(gene) {
  sprintf(
    '<a href="https://plantgenie.org/gene?id=%s" target="_blank">%s</a>',
    gene, gene
  )
}

df$Potra <- sapply(df$Potra, makePlantGenieLink)
df$PotrI <- sapply(df$Potri, makePlantGenieLink)

library(DT)

DT::datatable(df, escape = FALSE)

observeEvent(input$search_btn, {
  
  # ---- 1. Parse user input ----
  ids <- trimws(unlist(strsplit(input$ids, ",")))
  
  # ---- 2. Filter your diamond table ----
  # Replace `diamond_table` with your actual data frame name
  filtered <- diamond_table[diamond_table[, input$id_type] %in% ids, ]
  
  # ---- 3. Function to build PlantGenIE link ----
  makePlantGenieLink <- function(gene) {
    sprintf(
      '<a href="https://plantgenie.org/gene?id=%s" target="_blank">%s</a>',
      gene, gene
    )
  }
  
  # ---- 4. Convert both Potra and Potri columns to links ----
  # Only convert columns that actually exist
  if ("Potra" %in% names(filtered)) {
    filtered$Potra <- sapply(filtered$Potra, makePlantGenieLink)
  }
  if ("Potri" %in% names(filtered)) {
    filtered$Potri <- sapply(filtered$Potri, makePlantGenieLink)
  }
  
  # ---- 5. Render clickable table ----
  output$filtered_diamond_table <- DT::renderDataTable({
    DT::datatable(
      filtered,
      escape = FALSE,      # <---- IMPORTANT, allows clickable HTML
      rownames = FALSE,
      options = list(
        pageLength = 20,
        autoWidth = TRUE
      )
    )
  })
})

df <- read.table("../docs/labData/reference/potraToPotriPlntgnie.tsv")
colnames(df) <- c("Potra","Potri")

# Follow the steps for the app to be hosted to the scilife servers
# https://serve.scilifelab.se/docs/application-hosting/shiny/#wiki-toc-step-5-create-a-project

# Make a folder sciLife to do the following and navigate to it

# 1. Save information about required R packages
# create a specific directory structure
# app.R is the most important. Create this file by simply combining
# server.R and ui.R

install.packages("renv")
library(renv)
renv::snapshot()

# 2. Create a Dockerfile for your app with name 'Dockerfile' without 
# any extensions

# 3. Build and publish a Docker image
# Docker image needs to be built and published in a so-called image registry. 
# We do it with the manual option on DockerHub
# Option A: Manually building and publishing an image
# In terminal:
# docker build --platform linux/amd64 -t shruti281989/aspcell_nitrate:1.0 .

# 4. Create a user account on SciLifeLab Serve
# 5. Create a project
# 6. Cretae an app