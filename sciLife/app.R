library(shiny) 
library(shinyhelper) 
library(data.table) 
library(Matrix) 
library(DT) 
library(magrittr) 
library(ggplot2) 
library(ggrepel) 
library(hdf5r) 
library(ggdendro) 
library(grid) 
library(gridExtra)
library(readr)
library(dplyr)
scPopconf = readRDS("./scPopconf.rds")
scPopmeta = readRDS("./scPopmeta.rds")
scPopgene = readRDS("./scPopgene.rds")
scPopdimr = readRDS("./scPopdimr.rds")
scPopdef  = readRDS("./scPopdef.rds")

### Start server code 
ui = shinyUI(fluidPage( 
  ### HTML formatting of error messages 
  
  tags$head(tags$style(HTML(".shiny-output-error-validation {color: red; font-weight: bold;}"))), 
  list(tags$style(HTML(".navbar-default .navbar-nav { font-weight: bold; font-size: 16px; }"))), 
  
  ### Page title 
  titlePanel("Nitrate Response Cell Explorer in Poplar Wood"), 
  navbarPage( 
    NULL, 
    ### Tab1.a1: Zoom-enable Dimred 
    tabPanel( 
      HTML("Zoom-enable UMAP"), 
      h4("Zoom-enable reduced dimensions overlaid with cell info or assay expression"), 
      "In this tab, users can visualise either cell information and gene expression ",  
      "in a zoom-enabled low-dimensional representions plots.", 
      br(),br(), 
      fluidRow( 
        column( 
          3, fluidRow( 
            column( 
              12, selectInput("scPopa1dr", "Reduction:", choices = scPopdef$dimrd, 
                              selected = scPopdef$dimrd[1])) 
          ) 
        ), # End of column (6 space) 
        column( 
          3, actionButton("scPopa1togL", "Toggle to subset cells"), 
          conditionalPanel( 
            condition = "input.scPopa1togL % 2 == 1", 
            selectInput("scPopa1sub1", "Cell information to subset:", 
                        choices = scPopconf[grp == TRUE]$UI, 
                        selected = scPopdef$grp1), 
            uiOutput("scPopa1sub1.ui"), 
            actionButton("scPopa1sub1all", "Select all groups", class = "btn btn-primary"), 
            actionButton("scPopa1sub1non", "Deselect all groups", class = "btn btn-primary") 
          ) 
        ), # End of column (6 space) 
        column( 
          6, actionButton("scPopa1tog0", "Toggle graphics controls"), 
          conditionalPanel( 
            condition = "input.scPopa1tog0 % 2 == 1", 
            fluidRow( 
              column( 
                6, sliderInput("scPopa1siz", "Point size:", 
                               min = 0, max = 4, value = 1.25, step = 0.25), 
                radioButtons("scPopa1psz", "Plot size:", 
                             choices = c("Small", "Medium", "Large"), 
                             selected = "Medium", inline = TRUE), 
                radioButtons("scPopa1fsz", "Font size:", 
                             choices = c("Small", "Medium", "Large"), 
                             selected = "Medium", inline = TRUE) 
              ), 
              column( 
                6, radioButtons("scPopa1asp", "Aspect ratio:", 
                                choices = c("Square", "Fixed", "Free"), 
                                selected = "Square", inline = TRUE), 
                checkboxInput("scPopa1txt", "Show axis text", value = FALSE) 
              ) 
            ) 
          ) 
        )  # End of column (6 space) 
      ),   # End of fluidRow (4 space) 
      fluidRow( 
        column( 
          3, style="border-right: 2px solid black", h4("Information to plot"),
          selectInput("scPopa1ass1", "Data type to colour plot:",
                      choices = c("Cell Information", paste0("Assay: ", scPopdef$assay)), 
                      selected = "Cell Information"), 
          selectInput("scPopa1inp1", "Cell Info / Feature Name:", choices = NULL) %>% 
            helper(type = "inline", size = "m", fade = TRUE,
                   title = "Cell Info / Gene to colour cells by",
                   content = c("Select cell info / feature to colour cells",
                               "- Categorical covariates have a fixed colour palette",
                               paste0("- Continuous covariates / gene expression are coloured ",
                                      "in a Blue-Yellow-Red colour scheme, which can be ",
                                      "changed in the plot controls"))),
          strong("Draw box on plot below to zoom: "), 
          plotOutput("scPopa1oup1.br", height = "400px",
                     brush = brushOpts(id = "scPopa1inp1.br", resetOnNew = TRUE)), 
          actionButton("scPopa1tog1", "Toggle plot controls"), 
          conditionalPanel(
            condition = "input.scPopa1tog1 % 2 == 1",
            radioButtons("scPopa1col1", "Colour (Continuous data):",
                         choices = c("White-Red","Blue-Yellow-Red","Yellow-Green-Purple"),
                         selected = "Blue-Yellow-Red"),
            numericInput("scPopa1min1", "Min cutoff (q##):",
                         min = 0, max = 50, value = 0, step = 1),
            numericInput("scPopa1max1", "Max cutoff (q##):",
                         min = 50, max = 100, value = 100, step = 1),
            radioButtons("scPopa1ord1", "Plot order:",
                         choices = c("Max-1st", "Min-1st", "Original", "Random"),
                         selected = "Original", inline = TRUE),
            checkboxInput("scPopa1lab1", "Show cell info labels", value = TRUE)
          )
        ), # End of column (6 space) 
        column( 
          6, style="border-right: 2px solid black", 
          fluidRow(column(12, uiOutput("scPopa1oup1.ui"))),
          fluidRow(column(12, uiOutput("scPopa1oup3.ui"))),
          fluidRow(column(3, downloadButton("scPopa1oup1.dl", "Download Plot")),
                   column(3, radioButtons("scPopa1oup1.f", "Plot format:", 
                                          choices = c("png", "pdf"), selected = "png", inline = TRUE)),
                   column(3, numericInput("scPopa1oup1.h", "Plot height:",
                                          min = 4, max = 20, value = 8, step = 0.5)),
                   column(3, numericInput("scPopa1oup1.w", "Plot width:",
                                          min = 4, max = 20, value = 8, step = 0.5))),
          fluidRow(column(3, downloadButton("scPopa1oup3.dl", "Download Legend")),
                   column(3, radioButtons("scPopa1oup3.f", "Legend format:", 
                                          choices = c("png", "pdf"), selected = "png", inline = TRUE)),
                   column(3, numericInput("scPopa1oup3.h", "Legend height:",
                                          min = 0.2, max = 10, value = 2, step = 0.1)),
                   column(3, numericInput("scPopa1oup3.w", "Legend width:",
                                          min = 4, max = 20, value = 8, step = 0.5))),
          br() 
        ), # End of column (6 space) 
        column(3, h4("Accessory tables"),
               br(), h4("Cell numbers"),
               numericInput("scPopa1splt", "Split continuous cell info into nBins:",
                            min = 2, max = 10, value = 4, step = 1),
               dataTableOutput("scPopa1.dt"),
               conditionalPanel(
                 condition = "input.scPopa1ass1.indexOf('Assay') === 0",  # JS check for starts with 'Assay',
                 h4("Potra/Potri"),
                 DT::dataTableOutput("gene_diamond_table1")
               )
               
        )  # End of column (6 space) 
      )    # End of fluidRow (4 space) 
    )      # End of tab (2 space) 
    , 
    ### Tab1.a2: CellInfo vs AssayExpr on dimRed 
    tabPanel( 
      HTML("Side-by-side UMAP"), 
      h4("Cell information vs assay expression on reduced dimensions"), 
      "In this tab, users can visualise both cell information and gene ",  
      "expression side-by-side on low-dimensional representions.", 
      br(),br(), 
      fluidRow( 
        column( 
          3, fluidRow(
            column(
              12, selectInput("scPopa2dr", "Reduction:", choices = scPopdef$dimrd, 
                              selected = scPopdef$dimrd[1]))
          ) 
        ), # End of column (6 space) 
        column( 
          3, actionButton("scPopa2togL", "Toggle to subset cells"), 
          conditionalPanel( 
            condition = "input.scPopa2togL % 2 == 1", 
            selectInput("scPopa2sub1", "Cell information to subset:", 
                        choices = scPopconf[grp == TRUE]$UI, 
                        selected = scPopdef$grp1), 
            uiOutput("scPopa2sub1.ui"), 
            actionButton("scPopa2sub1all", "Select all groups", class = "btn btn-primary"), 
            actionButton("scPopa2sub1non", "Deselect all groups", class = "btn btn-primary") 
          ) 
        ), # End of column (6 space) 
        column( 
          6, actionButton("scPopa2tog0", "Toggle graphics controls"), 
          conditionalPanel( 
            condition = "input.scPopa2tog0 % 2 == 1", 
            fluidRow( 
              column( 
                6, sliderInput("scPopa2siz", "Point size:", 
                               min = 0, max = 4, value = 1.25, step = 0.25), 
                radioButtons("scPopa2psz", "Plot size:", 
                             choices = c("Small", "Medium", "Large"), 
                             selected = "Medium", inline = TRUE), 
                radioButtons("scPopa2fsz", "Font size:", 
                             choices = c("Small", "Medium", "Large"), 
                             selected = "Medium", inline = TRUE) 
              ), 
              column( 
                6, radioButtons("scPopa2asp", "Aspect ratio:", 
                                choices = c("Square", "Fixed", "Free"), 
                                selected = "Square", inline = TRUE), 
                checkboxInput("scPopa2txt", "Show axis text", value = FALSE) 
              ) 
            ) 
          ) 
        )  # End of column (6 space) 
      ),   # End of fluidRow (4 space) 
      fluidRow( 
        column(
          6, style="border-right: 2px solid black",
          fluidRow(
            column(
              6, selectInput("scPopa2ass1", "Data type to colour plot:",
                             choices = c("Cell Information", paste0("Assay: ", scPopdef$assay)), 
                             selected = "Cell Information"), 
              selectInput("scPopa2inp1", "Cell Info / Feature Name:", choices = NULL) %>% 
                helper(type = "inline", size = "m", fade = TRUE,
                       title = "Cell Info / Gene to colour cells by",
                       content = c("Select cell info / feature to colour cells",
                                   "- Categorical covariates have a fixed colour palette",
                                   paste0("- Continuous covariates / gene expression are coloured ",
                                          "in a Blue-Yellow-Red colour scheme, which can be ",
                                          "changed in the plot controls")))
            ),
            column(
              6, actionButton("scPopa2tog1", "Toggle plot controls"),
              conditionalPanel(
                condition = "input.scPopa2tog1 % 2 == 1",
                radioButtons("scPopa2col1", "Colour (Continuous data):",
                             choices = c("White-Red","Blue-Yellow-Red","Yellow-Green-Purple"),
                             selected = "Blue-Yellow-Red"),
                numericInput("scPopa2min1", "Min cutoff (q##):",
                             min = 0, max = 50, value = 0, step = 1),
                numericInput("scPopa2max1", "Max cutoff (q##):",
                             min = 50, max = 100, value = 100, step = 1),
                radioButtons("scPopa2ord1", "Plot order:",
                             choices = c("Max-1st", "Min-1st", "Original", "Random"),
                             selected = "Original", inline = TRUE),
                checkboxInput("scPopa2lab1", "Show cell info labels", value = TRUE)
              )
            )
          ),
          fluidRow(column(12, uiOutput("scPopa2oup1.ui"))),
          fluidRow(column(12, uiOutput("scPopa2oup3.ui"))),
          fluidRow(column(3, downloadButton("scPopa2oup1.dl", "Download Plot")),
                   column(3, radioButtons("scPopa2oup1.f", "Plot format:", 
                                          choices = c("png", "pdf"), selected = "png", inline = TRUE)),
                   column(3, numericInput("scPopa2oup1.h", "Plot height:",
                                          min = 4, max = 20, value = 8, step = 0.5)),
                   column(3, numericInput("scPopa2oup1.w", "Plot width:",
                                          min = 4, max = 20, value = 8, step = 0.5))),
          fluidRow(column(3, downloadButton("scPopa2oup3.dl", "Download Legend")),
                   column(3, radioButtons("scPopa2oup3.f", "Legend format:", 
                                          choices = c("png", "pdf"), selected = "png", inline = TRUE)),
                   column(3, numericInput("scPopa2oup3.h", "Legend height:",
                                          min = 0.2, max = 10, value = 2, step = 0.1)),
                   column(3, numericInput("scPopa2oup3.w", "Legend width:",
                                          min = 4, max = 20, value = 8, step = 0.5))),
          br() 
        ), # End of column (6 space)
        column( 
          6,
          fluidRow( 
            column(
              6, selectInput("scPopa2ass2", "Data type to colour plot:",
                             choices = c("Cell Information", paste0("Assay: ", scPopdef$assay)), 
                             selected = paste0("Assay: ", scPopdef$assay[1])), 
              selectInput("scPopa2inp2", "Cell Info / Feature Name:", choices = NULL) %>% 
                helper(type = "inline", size = "m", fade = TRUE,
                       title = "Cell Info / Gene to colour cells by",
                       content = c("Select cell info / feature to colour cells",
                                   "- Categorical covariates have a fixed colour palette",
                                   paste0("- Continuous covariates / gene expression are coloured ",
                                          "in a Blue-Yellow-Red colour scheme, which can be ",
                                          "changed in the plot controls")))
            ),
            column(
              6, actionButton("scPopa2tog2", "Toggle plot controls"),
              conditionalPanel(
                condition = "input.scPopa2tog2 % 2 == 1",
                radioButtons("scPopa2col2", "Colour (Continuous data):",
                             choices = c("White-Red","Blue-Yellow-Red","Yellow-Green-Purple"),
                             selected = "White-Red"),
                numericInput("scPopa2min2", "Min cutoff (q##):",
                             min = 0, max = 50, value = 0, step = 1),
                numericInput("scPopa2max2", "Max cutoff (q##):",
                             min = 50, max = 100, value = 100, step = 1),
                radioButtons("scPopa2ord2", "Plot order:",
                             choices = c("Max-1st", "Min-1st", "Original", "Random"),
                             selected = "Max-1st", inline = TRUE),
                checkboxInput("scPopa2lab2", "Show cell info labels", value = TRUE)
              )
            )
          ) , 
          fluidRow(column(12, uiOutput("scPopa2oup2.ui"))),
          fluidRow(column(12, uiOutput("scPopa2oup4.ui"))),
          fluidRow(column(3, downloadButton("scPopa2oup2.dl", "Download Plot")),
                   column(3, radioButtons("scPopa2oup2.f", "Plot format:", 
                                          choices = c("png", "pdf"), selected = "png", inline = TRUE)),
                   column(3, numericInput("scPopa2oup2.h", "Plot height:",
                                          min = 4, max = 20, value = 8, step = 0.5)),
                   column(3, numericInput("scPopa2oup2.w", "Plot width:",
                                          min = 4, max = 20, value = 8, step = 0.5))),
          fluidRow(column(3, downloadButton("scPopa2oup4.dl", "Download Legend")),
                   column(3, radioButtons("scPopa2oup4.f", "Legend format:", 
                                          choices = c("png", "pdf"), selected = "png", inline = TRUE)),
                   column(3, numericInput("scPopa2oup4.h", "Legend height:",
                                          min = 0.2, max = 10, value = 2, step = 0.1)),
                   column(3, numericInput("scPopa2oup4.w", "Legend width:",
                                          min = 4, max = 20, value = 8, step = 0.5))),
          br()
        )  # End of column (6 space) 
      ),   # End of fluidRow (4 space) 
      h4("Relationship between left-side and right-side cell info / feature"),
      fluidRow(
        column(
          6, h4("Comparative plot"), 
          style="border-right: 2px solid black", 
          uiOutput("scPopa2oup5.ui"), 
          fluidRow(column(3, downloadButton("scPopa2oup5.dl", "Download Plot")),
                   column(3, radioButtons("scPopa2oup5.f", "Plot format:", 
                                          choices = c("png", "pdf"), selected = "png", inline = TRUE)),
                   column(3, numericInput("scPopa2oup5.h", "Plot height:",
                                          min = 4, max = 20, value = 8, step = 0.5)),
                   column(3, numericInput("scPopa2oup5.w", "Plot width:",
                                          min = 4, max = 20, value = 8, step = 0.5)))
        ),
        column(
          6, h4("Accessory tables"),
          br(),h4("Cell numbers / statistics"),
          numericInput("scPopa2cut", "Cutoff for Expression:", value = 0), 
          dataTableOutput("scPopa2.dt"),
          br(),
          conditionalPanel(
            condition = "input.scPopa2ass1.indexOf('Assay') === 0 || input.scPopa2ass2.indexOf('Assay') === 0",
            h4("Potra/Potri"),
            DT::dataTableOutput("gene_diamond_table2")
          )
        )  # End of column (6 space) 
      )    # End of fluidRow (4 space) 
    )      # End of tab (2 space) 
    , 
    ### Tab1.a3: Gene coexpression plot 
    tabPanel( 
      HTML("Gene coexpression"), 
      h4("Coexpression of two genes on reduced dimensions"), 
      "In this tab, users can visualise the coexpression of two genes ", 
      "on low-dimensional representions.", 
      br(),br(), 
      fluidRow( 
        column( 
          3, fluidRow( 
            column( 
              12, selectInput("scPopa3dr", "Reduction:", choices = scPopdef$dimrd, 
                              selected = scPopdef$dimrd[1])) 
          ) 
        ), # End of column (6 space) 
        column( 
          3, actionButton("scPopa3togL", "Toggle to subset cells"), 
          conditionalPanel( 
            condition = "input.scPopa3togL % 2 == 1", 
            selectInput("scPopa3sub1", "Cell information to subset:", 
                        choices = scPopconf[grp == TRUE]$UI, 
                        selected = scPopdef$grp1), 
            uiOutput("scPopa3sub1.ui"), 
            actionButton("scPopa3sub1all", "Select all groups", class = "btn btn-primary"), 
            actionButton("scPopa3sub1non", "Deselect all groups", class = "btn btn-primary") 
          ) 
        ), # End of column (6 space) 
        column( 
          6, actionButton("scPopa3tog0", "Toggle graphics controls"), 
          conditionalPanel( 
            condition = "input.scPopa3tog0 % 2 == 1", 
            fluidRow( 
              column( 
                6, sliderInput("scPopa3siz", "Point size:", 
                               min = 0, max = 4, value = 1.25, step = 0.25), 
                radioButtons("scPopa3psz", "Plot size:", 
                             choices = c("Small", "Medium", "Large"), 
                             selected = "Medium", inline = TRUE), 
                radioButtons("scPopa3fsz", "Font size:", 
                             choices = c("Small", "Medium", "Large"), 
                             selected = "Medium", inline = TRUE) 
              ), 
              column( 
                6, radioButtons("scPopa3asp", "Aspect ratio:", 
                                choices = c("Square", "Fixed", "Free"), 
                                selected = "Square", inline = TRUE), 
                checkboxInput("scPopa3txt", "Show axis text", value = FALSE) 
              ) 
            ) 
          ) 
        )  # End of column (6 space) 
      ),   # End of fluidRow (4 space) 
      fluidRow( 
        column( 
          3, style="border-right: 2px solid black", h4("Assay Expression"), 
          selectInput("scPopa3inp1", "Feature 1:", choices=NULL) %>%  
            helper(type = "inline", size = "m", fade = TRUE, 
                   title = "Feature expression to colour cells by", 
                   content = c("Select gene to colour cells by gene expression", 
                               paste0("- Feature expression are coloured in a ", 
                                      "White-Red colour scheme which can be ", 
                                      "changed in the plot controls"))), 
          selectInput("scPopa3inp2", "Feature 2:", choices=NULL) %>% 
            helper(type = "inline", size = "m", fade = TRUE, 
                   title = "Feature expression to colour cells by", 
                   content = c("Select gene to colour cells by gene expression", 
                               paste0("- Feature expression are coloured in a ", 
                                      "White-Blue colour scheme which can be ", 
                                      "changed in the plot controls"))), 
          selectInput("scPopa3ass1", "Assay:", 
                      choices = scPopdef$assay, 
                      selected = scPopdef$assay[1]), 
          actionButton("scPopa3tog1", "Toggle plot controls"), 
          conditionalPanel( 
            condition = "input.scPopa3tog1 % 2 == 1", 
            radioButtons("scPopa3col1", "Colour:", 
                         choices = c("Red (Gene1); Blue (Gene2)", 
                                     "Orange (Gene1); Blue (Gene2)", 
                                     "Red (Gene1); Green (Gene2)", 
                                     "Green (Gene1); Blue (Gene2)"), 
                         selected = "Red (Gene1); Blue (Gene2)"), 
            numericInput("scPopa3min1", "Min cutoff gene 1 (q##):", 
                         min = 0, max = 50, value = 0, step = 1), 
            numericInput("scPopa3max1", "Max cutoff gene 1 (q##):", 
                         min = 50, max = 100, value = 100, step = 1), 
            numericInput("scPopa3min2", "Min cutoff gene 2 (q##):", 
                         min = 0, max = 50, value = 0, step = 1), 
            numericInput("scPopa3max2", "Max cutoff gene 2 (q##):", 
                         min = 50, max = 100, value = 100, step = 1), 
            radioButtons("scPopa3ord1", "Plot order:", 
                         choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                         selected = "Max-1st", inline = TRUE) 
          ) 
        ), # End of column (6 space) 
        column( 
          6, style="border-right: 2px solid black", 
          uiOutput("scPopa3oup1.ui"), 
          fluidRow(column(3, downloadButton("scPopa3oup1.dl", "Download Plot")), 
                   column(3, radioButtons("scPopa3oup1.f", "Plot format:", 
                                          choices = c("png", "pdf"), selected = "png", inline = TRUE)), 
                   column(3, numericInput("scPopa3oup1.h", "Plot height:", 
                                          min = 4, max = 20, value = 8, step = 0.5)), 
                   column(3, numericInput("scPopa3oup1.w", "Plot width:", 
                                          min = 4, max = 20, value = 8, step = 0.5))), 
          br() 
        ), # End of column (6 space) 
        column( 
          3, uiOutput("scPopa3oup2.ui"), 
          fluidRow(column(6, downloadButton("scPopa3oup2.dl", "Download Legend")), 
                   column(6, radioButtons("scPopa3oup2.f", "Legend format:", 
                                          choices = c("png", "pdf"), selected = "png", inline = TRUE))), 
          br(), h4("Cell numbers"), 
          dataTableOutput("scPopa3.dt"),
          br(), h4("Potra/Potri"),
          DT::dataTableOutput("gene_diamond_table3")
        ),
        
        # End of column (6 space) 
      )    # End of fluidRow (4 space) 
    )      # End of tab (2 space) 
    , 
    ### Tab1.b1: violinplot / boxplot 
    tabPanel( 
      HTML("Violinplot / Boxplot"),  
      h4("Cell information / assay expression violin plot / box plot"), 
      "In this tab, users can visualise the assay expression or continuous cell information ",  
      "(e.g. Number of UMIs / module score) across groups of cells (e.g. libary / clusters).", 
      br(),br(), 
      fluidRow( 
        column( 
          3, style="border-right: 2px solid black", 
          selectInput("scPopb1inp1", "Cell information (X-axis):", 
                      choices = scPopconf[grp == TRUE]$UI, 
                      selected = scPopdef$grp1) %>%  
            helper(type = "inline", size = "m", fade = TRUE, 
                   title = "Cell information to group cells by",  
                   content = c("Select categorical cell information to group cells by",  
                               "- Single cells are grouped by this categorical covariate",  
                               "- Plotted as the X-axis of the violin plot / box plot")),  
          selectInput("scPopb1inp2", "Feature name (Y-axis):", choices=NULL) %>%  
            helper(type = "inline", size = "m", fade = TRUE, 
                   title = "Cell Info / Gene to plot", 
                   content = c("Select cell info / feature to plot on Y-axis", 
                               "- Can be continuous cell information (e.g. nUMIs / scores)", 
                               "- Can also be feature expression")), 
          selectInput("scPopb1ass1", "Data type for Y-axis:", 
                      choices = c(paste0("Assay: ", scPopdef$assay)), 
                      selected = "Cell Information"), 
          radioButtons("scPopb1typ", "Plot type:", 
                       choices = c("violin", "boxplot"), 
                       selected = "violin", inline = TRUE), 
          checkboxInput("scPopb1pts", "Show data points", value = FALSE), 
          actionButton("scPopb1togT", "Toggle to perform stats test(s)"), 
          conditionalPanel( 
            condition = "input.scPopb1togT % 2 == 1",  
            radioButtons("scPopb1stg", "Perform global test:",  
                         choices = c("none","kruskal.test","anova"),  
                         selected = "none"), 
            radioButtons("scPopb1stp1", "Perform pairwise test:",  
                         choices = c("none","wilcox.test","t.test"),  
                         selected = "none"), 
            textAreaInput("scPopb1stp2", HTML("Pairwise comparisons to make <br />  
                                             (Separate each pair by new line and <br />  
                                             within each pair using , or ;):"),  
                          height = "150px", value = "none") 
          ), br(), br(),  
          actionButton("scPopb1togL", "Toggle to subset cells"), 
          conditionalPanel( 
            condition = "input.scPopb1togL % 2 == 1", 
            selectInput("scPopb1sub1", "Cell information to subset:", 
                        choices = scPopconf[grp == TRUE]$UI, 
                        selected = scPopdef$grp1), 
            uiOutput("scPopb1sub1.ui"), 
            actionButton("scPopb1sub1all", "Select all groups", class = "btn btn-primary"), 
            actionButton("scPopb1sub1non", "Deselect all groups", class = "btn btn-primary") 
          ), br(), br(), 
          actionButton("scPopb1tog", "Toggle graphics controls"), 
          conditionalPanel( 
            condition = "input.scPopb1tog % 2 == 1", 
            sliderInput("scPopb1siz", "Data point size:",  
                        min = 0, max = 4, value = 1.25, step = 0.25),  
            radioButtons("scPopb1psz", "Plot size:", 
                         choices = c("Small", "Medium", "Large"), 
                         selected = "Medium", inline = TRUE), 
            radioButtons("scPopb1fsz", "Font size:", 
                         choices = c("Small", "Medium", "Large"), 
                         selected = "Medium", inline = TRUE), 
            checkboxInput("scPopb1noi", "Add noise to assay expr", value = TRUE)) 
        ), # End of column (6 space) 
        column(9, uiOutput("scPopb1oup.ui"), 
               fluidRow(column(2, downloadButton("scPopb1oup.dl", "Download Plot")), 
                        column(2, radioButtons("scPopb1oup.f", "Plot format:", 
                                               choices = c("png", "pdf"), selected = "png", inline = TRUE)), 
                        column(2, numericInput("scPopb1oup.h", "Plot height:", 
                                               min = 4, max = 20, value = 8, step = 0.5)), 
                        column(2, numericInput("scPopb1oup.w", "Plot width:", 
                                               min = 4, max = 20, value = 10, step = 0.5))), 
               br(),
               h4("Potra/Potri"),
               DT::dataTableOutput("gene_diamond_table4"),
               br()
        )  # End of column (6 space) 
      )    # End of fluidRow (4 space) 
    )      # End of tab (2 space) 
    , 
    ### Tab1.b2: Proportion plot 
    tabPanel( 
      HTML("Proportion plot"), 
      h4("Proportion / cell numbers across different cell information"), 
      "In this tab, users can visualise the composition of single cells based on one discrete ", 
      "cell information across another discrete cell information. ",  
      "Usage examples include the library or cellcycle composition across clusters.", 
      br(),br(), 
      fluidRow( 
        column( 
          3, style="border-right: 2px solid black", 
          selectInput("scPopb2inp1", "Cell information to plot (X-axis):", 
                      choices = scPopconf[grp == TRUE]$UI, 
                      selected = scPopdef$grp2) %>%  
            helper(type = "inline", size = "m", fade = TRUE, 
                   title = "Cell information to plot cells by",  
                   content = c("Select categorical cell information to plot cells by", 
                               "- Plotted as the X-axis of the proportion plot")), 
          selectInput("scPopb2inp2", "Cell information to group / colour by:", 
                      choices = scPopconf[grp == TRUE]$UI, 
                      selected = scPopdef$grp1) %>%  
            helper(type = "inline", size = "m", fade = TRUE, 
                   title = "Cell information to group / colour cells by", 
                   content = c("Select categorical cell information to group / colour cells by", 
                               "- Proportion / cell numbers are shown in different colours")), 
          radioButtons("scPopb2typ", "Plot value:", 
                       choices = c("Proportion", "CellNumbers"), 
                       selected = "Proportion", inline = TRUE), 
          checkboxInput("scPopb2flp", "Flip X/Y", value = FALSE), 
          selectInput("scPopb2ord1", "Reorder X-axis by which group:", choices=NULL), 
          radioButtons("scPopb2ord2", "Reorder in which order:", 
                       choices = c("Decreasing", "Increasing"), 
                       selected = "Decreasing", inline = TRUE), 
          actionButton("scPopb2togL", "Toggle to subset cells"), 
          conditionalPanel( 
            condition = "input.scPopb2togL % 2 == 1", 
            selectInput("scPopb2sub1", "Cell information to subset:", 
                        choices = scPopconf[grp == TRUE]$UI, 
                        selected = scPopdef$grp1), 
            uiOutput("scPopb2sub1.ui"), 
            actionButton("scPopb2sub1all", "Select all groups", class = "btn btn-primary"), 
            actionButton("scPopb2sub1non", "Deselect all groups", class = "btn btn-primary") 
          ), br(), br(), 
          actionButton("scPopb2tog", "Toggle graphics controls"), 
          conditionalPanel( 
            condition = "input.scPopb2tog % 2 == 1", 
            radioButtons("scPopb2psz", "Plot size:", 
                         choices = c("Small", "Medium", "Large"), 
                         selected = "Medium", inline = TRUE), 
            radioButtons("scPopb2fsz", "Font size:", 
                         choices = c("Small", "Medium", "Large"), 
                         selected = "Medium", inline = TRUE)) 
        ), # End of column (6 space) 
        column(9, uiOutput("scPopb2oup.ui"), 
               fluidRow(column(2, downloadButton("scPopb2oup.dl", "Download Plot")), 
                        column(2, radioButtons("scPopb2oup.f", "Plot format:", 
                                               choices = c("png", "pdf"), selected = "png", inline = TRUE)), 
                        column(2, numericInput("scPopb2oup.h", "Plot height:", 
                                               min = 4, max = 20, value = 8, step = 0.5)), 
                        column(2, numericInput("scPopb2oup.w", "Plot width:", 
                                               min = 4, max = 20, value = 10, step = 0.5))), 
               br() 
        )  # End of column (6 space) 
      )    # End of fluidRow (4 space) 
    )      # End of tab (2 space) 
    , 
    ### Tab1.b3: Bubbleplot / Heatmap 
    tabPanel( 
      HTML("Bubbleplot / Heatmap"), 
      h4("Gene expression bubbleplot / heatmap"), 
      "In this tab, users can visualise the gene expression patterns of ", 
      "multiple genes grouped by categorical cell information (e.g. library / cluster).", br(), 
      "The normalised expression are averaged, log-transformed and then plotted.", 
      br(),br(), 
      fluidRow( 
        column( 
          3, style="border-right: 2px solid black", 
          textAreaInput("scPopb3inp", HTML("List of gene names <br /> 
                                        (Max 50 genes, separated <br /> 
                                         by , or ; or newline):"), 
                        height = "250px", 
                        value = paste0(scPopdef$genes[[1]], collapse = ", ")) %>% 
            helper(type = "inline", size = "m", fade = TRUE, 
                   title = "List of genes to plot on bubbleplot / heatmap", 
                   content = c("Input genes to plot", 
                               "- Maximum 50 genes (due to ploting space limitations)", 
                               "- Genes should be separated by comma, semicolon or newline")), 
          selectInput("scPopb3ass1", "Assay:", 
                      choices = scPopdef$assay, 
                      selected = scPopdef$assay[1]), 
          selectInput("scPopb3grp", "Group by:", 
                      choices = scPopconf[grp == TRUE]$UI, 
                      selected = scPopconf[grp == TRUE]$UI[1]) %>% 
            helper(type = "inline", size = "m", fade = TRUE, 
                   title = "Cell information to group cells by", 
                   content = c("Select categorical cell information to group cells by", 
                               "- Single cells are grouped by this categorical covariate", 
                               "- Plotted as the X-axis of the bubbleplot / heatmap")), 
          radioButtons("scPopb3plt", "Plot type:", 
                       choices = c("Bubbleplot", "Heatmap"), 
                       selected = "Bubbleplot", inline = TRUE), 
          checkboxInput("scPopb3scl", "Scale gene expression", value = TRUE), 
          checkboxInput("scPopb3row", "Cluster rows (genes)", value = TRUE), 
          checkboxInput("scPopb3col", "Cluster columns (samples)", value = FALSE), 
          sliderInput("scPopb3max", "Scale.max:", min=1, max=11, value=3, step=0.1), 
          checkboxInput("scPopb3exp", "expm1 before averaging (check for log-transformed data)",  
                        value = TRUE), 
          br(), 
          actionButton("scPopb3togL", "Toggle to subset cells"), 
          conditionalPanel( 
            condition = "input.scPopb3togL % 2 == 1", 
            selectInput("scPopb3sub1", "Cell information to subset:", 
                        choices = scPopconf[grp == TRUE]$UI, 
                        selected = scPopdef$grp1), 
            uiOutput("scPopb3sub1.ui"), 
            actionButton("scPopb3sub1all", "Select all groups", class = "btn btn-primary"), 
            actionButton("scPopb3sub1non", "Deselect all groups", class = "btn btn-primary") 
          ), br(), br(), 
          actionButton("scPopb3tog", "Toggle graphics controls"), 
          conditionalPanel( 
            condition = "input.scPopb3tog % 2 == 1", 
            radioButtons("scPopb3cols", "Colour scheme:", 
                         choices = c("White-Red", "Blue-Yellow-Red", 
                                     "Yellow-Green-Purple"), 
                         selected = "Blue-Yellow-Red"), 
            radioButtons("scPopb3psz", "Plot size:", 
                         choices = c("Small", "Medium", "Large"), 
                         selected = "Medium", inline = TRUE), 
            radioButtons("scPopb3fsz", "Font size:", 
                         choices = c("Small", "Medium", "Large"), 
                         selected = "Medium", inline = TRUE)) 
        ), # End of column (6 space) 
        column(9, h4(htmlOutput("scPopb3oupTxt")), 
               uiOutput("scPopb3oup.ui"), 
               fluidRow(column(2, downloadButton("scPopb3oup.dl", "Download Plot")), 
                        column(2, radioButtons("scPopb3oup.f", "Plot format:", 
                                               choices = c("png", "pdf"), selected = "png", inline = TRUE)), 
                        column(2, numericInput("scPopb3oup.h", "Plot height:", 
                                               min = 4, max = 20, value = 10, step = 0.5)), 
                        column(2, numericInput("scPopb3oup.w", "Plot width:", 
                                               min = 4, max = 20, value = 10, step = 0.5))), 
               br() ,
               h4("Potra/Potri"),
               DT::dataTableOutput("gene_diamond_table5"),
               br()
        )  # End of column (6 space) 
      )    # End of fluidRow (4 space) 
    ),      # End of tab (2 space) 
    
    ### Tab2: Bubbleplot / Heatmap 
    
    
    
    tabPanel(
      HTML("Potra/Potri search"),
      "In this tab, users can visualise best diamond matches between proteins of 
    P. tremula and P. trichocarpa.",
      
      # Choice: Potri or Potra
      
      radioButtons("id_type", "Select ID type:", choices = c("Potri", "Potra")),
      textInput("ids", "Enter IDs (comma-separated):", ""),
      actionButton("search_btn", "Search"),
      br(), br(),
      # Table output
      DT::dataTableOutput("filtered_diamond_table")
    ),
    
    ### Tab3
    
    tabPanel(
      HTML("Contact"),
      "Prof. Hannele Tuominen, Umeå Plant Science Centre,
  Department of Forest Genetics and Plant Physiology,
  Swedish University of Agricultural Sciences, Umeå, Sweden.",
      br(),
      "hannele.tuominen@slu.se"
    )
    
    
    
    
    
    , br(), 
    p("", style = "font-size: 125%;"), 
    p(em("This webpage was made using "), a("ShinyCell2", 
                                            href = "https://github.com/the-ouyang-lab/ShinyCell2",target="_blank")), 
    br(),br(),br(),br(),br()  
  )))  


source("./shinyFunc.R")
pdf(file = NULL)


### Useful stuff 
# Colour palette 
cList = list(c("grey85","#FFF7EC","#FEE8C8","#FDD49E","#FDBB84", 
               "#FC8D59","#EF6548","#D7301F","#B30000","#7F0000"), 
             c("#4575B4","#74ADD1","#ABD9E9","#E0F3F8","#FFFFBF", 
               "#FEE090","#FDAE61","#F46D43","#D73027")[c(1,1:9,9)], 
             c("#FDE725","#AADC32","#5DC863","#27AD81","#21908C", 
               "#2C728E","#3B528B","#472D7B","#440154")) 
names(cList) = c("White-Red", "Blue-Yellow-Red", "Yellow-Green-Purple") 

# Panel sizes 
pList = c("400px", "600px", "800px") 
names(pList) = c("Small", "Medium", "Large") 
pList2 = c("500px", "700px", "900px") 
names(pList2) = c("Small", "Medium", "Large") 
pList3 = c("600px", "800px", "1000px") 
names(pList3) = c("Small", "Medium", "Large") 
sList = c(18,24,30)
names(sList) = c("Small", "Medium", "Large") 



### Start server code 
server = shinyServer(function(input, output, session) { 
  ### For all tags and Server-side selectize 
  observe_helpers() 
  optCrt="{ option_create: function(data,escape) {return('<div class=\"create\"><strong>' + '</strong></div>');} }" 
  observe({ 
    invalidateLater(30000) # ping every 30 seconds to keep connection alive 
    cat(".") 
  }) 
  
  ### Functions for tab A1 
  getGscPopa1inp1 <- reactive({ 
    req(gsub("^Assay: ", "", input$scPopa1ass1)) 
    if(gsub("^Assay: ", "", input$scPopa1ass1) == "Cell Information"){ 
      res <- scPopconf$UI 
      resDef <- scPopdef$meta1 
      resLen <- length(res) 
    } else { 
      res <- names(scPopgene[[gsub("^Assay: ", "", input$scPopa1ass1)]]) 
      resDef <- scPopdef$gene1[[gsub("^Assay: ", "", input$scPopa1ass1)]] 
      resLen <- 7 
    } 
    return(list(res,resDef,resLen))
  })
  observeEvent(gsub("^Assay: ", "", input$scPopa1ass1), {
    updateSelectizeInput(session, "scPopa1inp1", choices = getGscPopa1inp1()[[1]], 
                         server = TRUE, selected = getGscPopa1inp1()[[2]], options = list( 
                           maxOptions = getGscPopa1inp1()[[3]], create = TRUE, 
                           persist = TRUE, render = I(optCrt))) 
  })
  output$scPopa1sub1.ui <- renderUI({ 
    sub = strsplit(scPopconf[UI == input$scPopa1sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("scPopa1sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  observeEvent(input$scPopa1sub1non, { 
    sub = strsplit(scPopconf[UI == input$scPopa1sub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "scPopa1sub2", label = "Select which cells to show", 
                             choices = sub, selected = NULL, inline = TRUE) 
  }) 
  observeEvent(input$scPopa1sub1all, { 
    sub = strsplit(scPopconf[UI == input$scPopa1sub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "scPopa1sub2", label = "Select which cells to show", 
                             choices = sub, selected = sub, inline = TRUE) 
  }) 
  
  scPopa1oup1xy <- reactiveValues(x = NULL, y = NULL)
  observe({
    brush <- input$scPopa1inp1.br
    if (!is.null(brush)) {
      scPopa1oup1xy$x <- c(brush$xmin, brush$xmax)
      scPopa1oup1xy$y <- c(brush$ymin, brush$ymax)
    } else {
      scPopa1oup1xy$x <- NULL; scPopa1oup1xy$y <- NULL
    }
  })
  scPopa1oup1br <- reactive({
    sc2Ddimr(scPopconf, scPopmeta, scPopdimr, input$scPopa1dr, input$scPopa1inp1,
             "scPopassay_", scPopgene, input$scPopa1ass1, input$scPopa1sub1, input$scPopa1sub2,  
             input$scPopa1min1, input$scPopa1max1, input$scPopa1siz/2, input$scPopa1ord1,
             cList[[input$scPopa1col1]], sList[input$scPopa1fsz]/2, 
             input$scPopa1asp, FALSE, FALSE) 
  })
  output$scPopa1oup1.br <- renderPlot({scPopa1oup1br() + theme(legend.position = "none")}) 
  
  scPopa1oup1 <- reactive({
    sc2Ddimr(scPopconf, scPopmeta, scPopdimr, input$scPopa1dr, input$scPopa1inp1,
             "scPopassay_", scPopgene, input$scPopa1ass1, input$scPopa1sub1, input$scPopa1sub2,  
             input$scPopa1min1, input$scPopa1max1, input$scPopa1siz, input$scPopa1ord1,
             cList[[input$scPopa1col1]], sList[input$scPopa1fsz], 
             input$scPopa1asp, input$scPopa1txt, input$scPopa1lab1)
  })
  output$scPopa1oup1 <- renderPlot({
    if(is.null(scPopa1oup1xy$x[1])){
      scPopa1oup1() + theme(legend.position = "none")
    } else {
      scPopa1oup1() + theme(legend.position = "none") + 
        scale_x_continuous(limits = scPopa1oup1xy$x, expand = c(0, 0)) + 
        scale_y_continuous(limits = scPopa1oup1xy$y, expand = c(0, 0)) 
    }
  })
  output$scPopa1oup1.ui <- renderUI({plotOutput("scPopa1oup1", height = pList[input$scPopa1psz])})
  output$scPopa1oup1.dl <- downloadHandler(
    filename = function() { paste0("scPop",input$scPopa1dr,"_",input$scPopa1inp1,".",input$scPopa1oup1.f) },
    content = function(file) { 
      if(is.null(scPopa1oup1xy$x[1])){
        ggsav(file, height = input$scPopa1oup1.h, width = input$scPopa1oup1.w, 
              plot = scPopa1oup1() + theme(legend.position = "none"))
      } else {
        ggsav(file, height = input$scPopa1oup1.h, width = input$scPopa1oup1.w, 
              plot = scPopa1oup1() + theme(legend.position = "none") + 
                scale_x_continuous(limits = scPopa1oup1xy$x, expand = c(0, 0)) + 
                scale_y_continuous(limits = scPopa1oup1xy$y, expand = c(0, 0)))
      }
    })
  output$scPopa1oup3 <- renderPlot({grid.newpage(); grid.draw(g_legend(scPopa1oup1()))})
  output$scPopa1oup3.ui <- renderUI({plotOutput("scPopa1oup3", height = 72*convertHeight(
    grobHeight(g_legend(scPopa1oup1())), unitTo="in", valueOnly=TRUE) + 50)})
  output$scPopa1oup3.dl <- downloadHandler(
    filename = function() { paste0("scPop",input$scPopa1dr,"_",input$scPopa1inp1,"_leg.",input$scPopa1oup3.f) },
    content = function(file) { 
      grid.newpage(); grid.draw(g_legend(scPopa1oup1()))
      ggsav(file, height = input$scPopa1oup3.h, width = input$scPopa1oup3.w, plot = grid.grab())
    }) 
  
  output$scPopa1.dt <- renderDataTable({
    ggData = sc2Dnum(scPopconf, scPopmeta, scPopdimr, input$scPopa1dr, scPopa1oup1xy$x, scPopa1oup1xy$y, input$scPopa1inp1,
                     "scPopassay_", scPopgene, input$scPopa1ass1, input$scPopa1sub1, input$scPopa1sub2, input$scPopa1splt)
    datatable(ggData, rownames = FALSE, extensions = "Buttons",
              options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>%
      formatRound(columns = c("pctZoom"), digits = 2)
  })
  
  
  ### Functions for tab A2 
  getGscPopa2inp1 <- reactive({ 
    req(gsub("^Assay: ", "", input$scPopa2ass1)) 
    if(gsub("^Assay: ", "", input$scPopa2ass1) == "Cell Information"){ 
      res <- scPopconf$UI 
      resDef <- scPopdef$meta1 
      resLen <- length(res) 
    } else { 
      res <- names(scPopgene[[gsub("^Assay: ", "", input$scPopa2ass1)]]) 
      resDef <- scPopdef$gene1[[gsub("^Assay: ", "", input$scPopa2ass1)]] 
      resLen <- 7 
    } 
    return(list(res,resDef,resLen))
  })
  observeEvent(gsub("^Assay: ", "", input$scPopa2ass1), {
    updateSelectizeInput(session, "scPopa2inp1", choices = getGscPopa2inp1()[[1]], 
                         server = TRUE, selected = getGscPopa2inp1()[[2]], options = list( 
                           maxOptions = getGscPopa2inp1()[[3]], create = TRUE, 
                           persist = TRUE, render = I(optCrt))) 
  })
  getGscPopa2inp2 <- reactive({ 
    req(gsub("^Assay: ", "", input$scPopa2ass2)) 
    if(gsub("^Assay: ", "", input$scPopa2ass2) == "Cell Information"){ 
      res <- scPopconf$UI 
      resDef <- scPopdef$meta1 
      resLen <- length(res) 
    } else { 
      res <- names(scPopgene[[gsub("^Assay: ", "", input$scPopa2ass2)]]) 
      resDef <- scPopdef$gene1[[gsub("^Assay: ", "", input$scPopa2ass2)]] 
      resLen <- 7 
    } 
    return(list(res,resDef,resLen))
  })
  observeEvent(gsub("^Assay: ", "", input$scPopa2ass2), {
    updateSelectizeInput(session, "scPopa2inp2", choices = getGscPopa2inp2()[[1]], 
                         server = TRUE, selected = getGscPopa2inp2()[[2]], options = list( 
                           maxOptions = getGscPopa2inp2()[[3]], create = TRUE, 
                           persist = TRUE, render = I(optCrt))) 
  })
  output$scPopa2sub1.ui <- renderUI({ 
    sub = strsplit(scPopconf[UI == input$scPopa2sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("scPopa2sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  observeEvent(input$scPopa2sub1non, {
    sub = strsplit(scPopconf[UI == input$scPopa2sub1]$fID, "\\|")[[1]]
    updateCheckboxGroupInput(session, inputId = "scPopa2sub2", label = "Select which cells to show",
                             choices = sub, selected = NULL, inline = TRUE)
  })
  observeEvent(input$scPopa2sub1all, {
    sub = strsplit(scPopconf[UI == input$scPopa2sub1]$fID, "\\|")[[1]]
    updateCheckboxGroupInput(session, inputId = "scPopa2sub2", label = "Select which cells to show",
                             choices = sub, selected = sub, inline = TRUE)
  })
  
  scPopa2oup1 <- reactive({
    sc2Ddimr(scPopconf, scPopmeta, scPopdimr, input$scPopa2dr, input$scPopa2inp1,
             "scPopassay_", scPopgene, input$scPopa2ass1, input$scPopa2sub1, input$scPopa2sub2,  
             input$scPopa2min1, input$scPopa2max1, input$scPopa2siz, input$scPopa2ord1,
             cList[[input$scPopa2col1]], sList[input$scPopa2fsz], 
             input$scPopa2asp, input$scPopa2txt, input$scPopa2lab1) 
  })
  output$scPopa2oup1 <- renderPlot({scPopa2oup1() + theme(legend.position = "none")})
  output$scPopa2oup1.ui <- renderUI({plotOutput("scPopa2oup1", height = pList[input$scPopa2psz])})
  output$scPopa2oup1.dl <- downloadHandler(
    filename = function() { paste0("scPop",input$scPopa2dr,"_",input$scPopa2inp1,".",input$scPopa2oup1.f) },
    content = function(file) { ggsav(file, height = input$scPopa2oup1.h, width = input$scPopa2oup1.w, 
                                     plot = scPopa2oup1() + theme(legend.position = "none"))
    })
  output$scPopa2oup3 <- renderPlot({grid.newpage(); grid.draw(g_legend(scPopa2oup1()))})
  output$scPopa2oup3.ui <- renderUI({plotOutput("scPopa2oup3", height = 72*convertHeight(
    grobHeight(g_legend(scPopa2oup1())), unitTo="in", valueOnly=TRUE) + 50)})
  output$scPopa2oup3.dl <- downloadHandler(
    filename = function() { paste0("scPop",input$scPopa2dr,"_",input$scPopa2inp1,"_leg.",input$scPopa2oup3.f) },
    content = function(file) { 
      grid.newpage(); grid.draw(g_legend(scPopa2oup1()))
      ggsav(file, height = input$scPopa2oup3.h, width = input$scPopa2oup3.w, plot = grid.grab())
    })
  
  scPopa2oup2 <- reactive({
    sc2Ddimr(scPopconf, scPopmeta, scPopdimr, input$scPopa2dr, input$scPopa2inp2,
             "scPopassay_", scPopgene, input$scPopa2ass2, input$scPopa2sub1, input$scPopa2sub2,  
             input$scPopa2min2, input$scPopa2max2, input$scPopa2siz, input$scPopa2ord2,
             cList[[input$scPopa2col2]], sList[input$scPopa2fsz], 
             input$scPopa2asp, input$scPopa2txt, input$scPopa2lab2) 
  })
  output$scPopa2oup2 <- renderPlot({scPopa2oup2() + theme(legend.position = "none")})
  output$scPopa2oup2.ui <- renderUI({plotOutput("scPopa2oup2", height = pList[input$scPopa2psz])})
  output$scPopa2oup2.dl <- downloadHandler(
    filename = function() { paste0("scPop",input$scPopa2dr,"_",input$scPopa2inp2,".",input$scPopa2oup2.f) },
    content = function(file) { ggsav(file, height = input$scPopa2oup2.h, width = input$scPopa2oup2.w, 
                                     plot = scPopa2oup2() + theme(legend.position = "none"))
    })
  
  output$scPopa2oup4 <- renderPlot({grid.newpage(); grid.draw(g_legend(scPopa2oup2()))})
  output$scPopa2oup4.ui <- renderUI({plotOutput("scPopa2oup4", height = 72*convertHeight(
    grobHeight(g_legend(scPopa2oup2())), unitTo="in", valueOnly=TRUE) + 50)})
  output$scPopa2oup4.dl <- downloadHandler(
    filename = function() { paste0("scPop",input$scPopa2dr,"_",input$scPopa2inp2,"_leg.",input$scPopa2oup4.f) },
    content = function(file) { 
      grid.newpage(); grid.draw(g_legend(scPopa2oup2()))
      ggsav(file, height = input$scPopa2oup4.h, width = input$scPopa2oup4.w, plot = grid.grab())
    })
  
  scPopa2oup5 <- reactive({
    sc2Dcomp(scPopconf, scPopmeta, input$scPopa2inp1, input$scPopa2inp2,
             "scPopassay_", scPopgene, input$scPopa2ass1, input$scPopa2ass2, input$scPopa2sub1, input$scPopa2sub2,
             input$scPopa2min1, input$scPopa2max1, input$scPopa2min2, input$scPopa2max2, 
             input$scPopa2siz, cList[[input$scPopa2col2]], sList[input$scPopa2fsz]) 
  })
  output$scPopa2oup5 <- renderPlot({scPopa2oup5() + theme(legend.position = "none")})
  output$scPopa2oup5.ui <- renderUI({plotOutput("scPopa2oup5", height = pList[input$scPopa2psz])})
  output$scPopa2oup5.dl <- downloadHandler( 
    filename = function() { paste0("scPopcompare_",input$scPopa2inp1,"_",input$scPopa2inp2,".",input$scPopa2oup5.f) }, 
    content = function(file) { ggsav(file, height = input$scPopa2oup5.h, width = input$scPopa2oup5.w, 
                                     plot = scPopa2oup5()) 
    }) 
  output$scPopa2.dt <- renderDataTable({
    ggData = sc2Dcnum(scPopconf, scPopmeta, input$scPopa2inp1, input$scPopa2inp2,
                      "scPopassay_", scPopgene, input$scPopa2ass1, input$scPopa2ass2, input$scPopa2sub1, input$scPopa2sub2,
                      input$scPopa2min1, input$scPopa2max1, input$scPopa2min2, input$scPopa2max2, input$scPopa2cut)
    datatable(ggData, rownames = FALSE, extensions = "Buttons",
              options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel")))
  })
  
  
  ### Functions for tab A3 
  getGscPopa3inp1 <- reactive({ 
    req(input$scPopa3ass1) 
    res <- names(scPopgene[[input$scPopa3ass1]]) 
    resDef1 <- scPopdef$gene1[[input$scPopa3ass1]] 
    resDef2 <- scPopdef$gene2[[input$scPopa3ass1]] 
    return(list(res,resDef1,resDef2))
  })
  observeEvent(input$scPopa3ass1, {
    updateSelectizeInput(session, "scPopa3inp1", choices = getGscPopa3inp1()[[1]], 
                         server = TRUE, selected = getGscPopa3inp1()[[2]], options = list( 
                           maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt))) 
  })
  observeEvent(input$scPopa3ass1, {
    updateSelectizeInput(session, "scPopa3inp2", choices = getGscPopa3inp1()[[1]], 
                         server = TRUE, selected = getGscPopa3inp1()[[3]], options = list( 
                           maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt))) 
  })
  output$scPopa3sub1.ui <- renderUI({ 
    sub = strsplit(scPopconf[UI == input$scPopa3sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("scPopa3sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  observeEvent(input$scPopa3sub1non, { 
    sub = strsplit(scPopconf[UI == input$scPopa3sub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "scPopa3sub2", label = "Select which cells to show", 
                             choices = sub, selected = NULL, inline = TRUE) 
  }) 
  observeEvent(input$scPopa3sub1all, { 
    sub = strsplit(scPopconf[UI == input$scPopa3sub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "scPopa3sub2", label = "Select which cells to show", 
                             choices = sub, selected = sub, inline = TRUE) 
  }) 
  scPopa3oup1 <- reactive({
    scDRcoex(scPopconf, scPopmeta, scPopdimr, input$scPopa3dr, input$scPopa3inp1, input$scPopa3inp2, 
             paste0("scPopassay_", input$scPopa3ass1, ".h5"), scPopgene[[input$scPopa3ass1]], 
             input$scPopa3sub1, input$scPopa3sub2, input$scPopa3min1, input$scPopa3max1, input$scPopa3min2, input$scPopa3max2, 
             input$scPopa3siz, input$scPopa3col1, input$scPopa3ord1, 
             sList[input$scPopa3fsz], input$scPopa3asp, input$scPopa3txt) 
  }) 
  output$scPopa3oup1 <- renderPlot({scPopa3oup1()}) 
  output$scPopa3oup1.ui <- renderUI({plotOutput("scPopa3oup1", height = pList2[input$scPopa3psz])}) 
  output$scPopa3oup1.dl <- downloadHandler( 
    filename = function() { paste0("scPop",input$scPopa3dr,"_",input$scPopa3inp1,"_",input$scPopa3inp2,".",input$scPopa3oup1.f) }, 
    content = function(file) { ggsav(file, height = input$scPopa3oup1.h, width = input$scPopa3oup1.w, plot = scPopa3oup1())
    }) 
  
  output$scPopa3oup2 <- renderPlot({scDRcoexLeg(input$scPopa3inp1, input$scPopa3inp2, input$scPopa3col1, sList[input$scPopa3fsz])}) 
  output$scPopa3oup2.ui <- renderUI({plotOutput("scPopa3oup2", height = "300px")})
  output$scPopa3oup2.dl <- downloadHandler( 
    filename = function() { paste0("scPop",input$scPopa3dr,"_",input$scPopa3inp1,"_",input$scPopa3inp2,"_leg.",input$scPopa3oup2.f) }, 
    content = function(file) { ggsav(file, height = 3, width = 4, 
                                     plot = scDRcoexLeg(input$scPopa3inp1, input$scPopa3inp2, input$scPopa3col1, sList[input$scPopa3fsz]))
    }) 
  output$scPopa3.dt <- renderDataTable({ 
    ggData = scDRcoexNum(scPopconf, scPopmeta, input$scPopa3inp1, input$scPopa3inp2, 
                         paste0("scPopassay_", input$scPopa3ass1, ".h5"), scPopgene[[input$scPopa3ass1]], 
                         input$scPopa3sub1, input$scPopa3sub2) 
    datatable(ggData, rownames = FALSE, extensions = "Buttons", 
              options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>% 
      formatRound(columns = c("percent"), digits = 2) 
  }) 
  
  
  ### Functions for tab B1 
  getGscPopb1inp1 <- reactive({ 
    req(gsub("^Assay: ", "", input$scPopb1ass1)) 
    if(gsub("^Assay: ", "", input$scPopb1ass1) == "Cell Information"){ 
      res <- scPopconf[is.na(fID)]$UI 
      resDef <- scPopconf[is.na(fID)]$UI[1] 
      resLen <- length(res) 
    } else { 
      res <- names(scPopgene[[gsub("^Assay: ", "", input$scPopb1ass1)]]) 
      resDef <- scPopdef$gene1[[gsub("^Assay: ", "", input$scPopb1ass1)]] 
      resLen <- 7 
    } 
    return(list(res,resDef,resLen))
  })
  observeEvent(gsub("^Assay: ", "", input$scPopb1ass1), {
    updateSelectizeInput(session, "scPopb1inp2", choices = getGscPopb1inp1()[[1]], 
                         server = TRUE, selected = getGscPopb1inp1()[[2]], options = list( 
                           maxOptions = getGscPopb1inp1()[[3]], create = TRUE, 
                           persist = TRUE, render = I(optCrt))) 
  })
  output$scPopb1sub1.ui <- renderUI({ 
    sub = strsplit(scPopconf[UI == input$scPopb1sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("scPopb1sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  observeEvent(input$scPopb1sub1non, { 
    sub = strsplit(scPopconf[UI == input$scPopb1sub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "scPopb1sub2", label = "Select which cells to show", 
                             choices = sub, selected = NULL, inline = TRUE) 
  }) 
  observeEvent(input$scPopb1sub1all, { 
    sub = strsplit(scPopconf[UI == input$scPopb1sub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "scPopb1sub2", label = "Select which cells to show", 
                             choices = sub, selected = sub, inline = TRUE) 
  }) 
  
  scPopb1oup <- reactive({
    scVioBox(scPopconf, scPopmeta, input$scPopb1inp1, input$scPopb1inp2, 
             "scPopassay_", scPopgene, input$scPopb1ass1, 
             input$scPopb1sub1, input$scPopb1sub2, input$scPopb1typ, input$scPopb1pts, 
             input$scPopb1stg, input$scPopb1stp1, input$scPopb1stp2, 
             input$scPopb1siz, sList[input$scPopb1fsz], input$scPopb1noi) 
  }) 
  output$scPopb1oup <- renderPlot({scPopb1oup()}) 
  output$scPopb1oup.ui <- renderUI({plotOutput("scPopb1oup", height = pList2[input$scPopb1psz])}) 
  output$scPopb1oup.dl <- downloadHandler( 
    filename = function() { paste0("scPop",input$scPopb1typ,"_",input$scPopb1inp1,"_",input$scPopb1inp2,".",input$scPopb1oup.f) }, 
    content = function(file) { ggsave(file, height = input$scPopb1oup.h, width = input$scPopb1oup.w, plot = scPopb1oup())
    }) 
  
  
  ### Functions for tab B2 
  observeEvent(input$scPopb2inp1, {
    sub = strsplit(scPopconf[UI == input$scPopb2inp2]$fID, "\\|")[[1]] 
    updateSelectizeInput(session, "scPopb2ord1", choices = c("Original order", sub), 
                         server = TRUE, selected = "Original order", options = list( 
                           create = TRUE, persist = TRUE, render = I(optCrt))) 
  }) 
  output$scPopb2sub1.ui <- renderUI({ 
    sub = strsplit(scPopconf[UI == input$scPopb2sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("scPopb2sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  observeEvent(input$scPopb2sub1non, { 
    sub = strsplit(scPopconf[UI == input$scPopb2sub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "scPopb2sub2", label = "Select which cells to show", 
                             choices = sub, selected = NULL, inline = TRUE) 
  }) 
  observeEvent(input$scPopb2sub1all, { 
    sub = strsplit(scPopconf[UI == input$scPopb2sub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "scPopb2sub2", label = "Select which cells to show", 
                             choices = sub, selected = sub, inline = TRUE) 
  }) 
  
  scPopb2oup  <- reactive({
    scProp(scPopconf, scPopmeta, input$scPopb2inp1, input$scPopb2inp2,  
           input$scPopb2sub1, input$scPopb2sub2, input$scPopb2ord1, input$scPopb2ord2, 
           input$scPopb2typ, input$scPopb2flp, sList[input$scPopb2fsz]) 
  })
  output$scPopb2oup <- renderPlot({scPopb2oup()}) 
  output$scPopb2oup.ui <- renderUI({plotOutput("scPopb2oup", height = pList2[input$scPopb2psz])}) 
  output$scPopb2oup.dl <- downloadHandler( 
    filename = function() { paste0("scPop",input$scPopb2typ,"_",input$scPopb2inp1,"_",input$scPopb2inp2,".",input$scPopb2oup.f) }, 
    content = function(file) { ggsave(file, height = input$scPopb2oup.h, width = input$scPopb2oup.w, plot = scPopb2oup())
    }) 
  
  
  ### Functions for tab B3 
  
  ## Modified to have genes of interest as default
  getGscPopb3inp1 <- reactive({ 
    req(input$scPopb3ass1)
    default_genes_interest <- readLines("./genes_interest.txt")
    resDef <- paste(default_genes_interest, collapse = ",")
    return(resDef) 
  }) 
  observeEvent(input$scPopb3ass1, { 
    updateTextAreaInput(session, "scPopb3inp", value = getGscPopb3inp1()) 
  }) 
  output$scPopb3sub1.ui <- renderUI({ 
    sub = strsplit(scPopconf[UI == input$scPopb3sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("scPopb3sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  observeEvent(input$scPopb3sub1non, { 
    sub = strsplit(scPopconf[UI == input$scPopb3sub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "scPopb3sub2", label = "Select which cells to show", 
                             choices = sub, selected = NULL, inline = TRUE) 
  }) 
  observeEvent(input$scPopb3sub1all, { 
    sub = strsplit(scPopconf[UI == input$scPopb3sub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "scPopb3sub2", label = "Select which cells to show", 
                             choices = sub, selected = sub, inline = TRUE) 
  }) 
  output$scPopb3oupTxt <- renderUI({ 
    geneList = scGeneList(input$scPopb3inp, scPopgene[[input$scPopb3ass1]]) 
    if(nrow(geneList) > 50){ 
      HTML("More than 50 input genes! Please reduce the gene list!") 
    } else { 
      oup = paste0(nrow(geneList[present == TRUE]), " genes OK and will be plotted") 
      if(nrow(geneList[present == FALSE]) > 0){ 
        oup = paste0(oup, "<br/>", 
                     nrow(geneList[present == FALSE]), " genes not found (", 
                     paste0(geneList[present == FALSE]$gene, collapse = ", "), ")") 
      } 
      HTML(oup) 
    } 
  }) 
  
  scPopb3oup <- reactive({
    scBubbHeat(scPopconf, scPopmeta, input$scPopb3inp, input$scPopb3grp, input$scPopb3plt, 
               paste0("scPopassay_", input$scPopb3ass1, ".h5"), scPopgene[[input$scPopb3ass1]], 
               input$scPopb3sub1, input$scPopb3sub2, input$scPopb3scl, 
               input$scPopb3row, input$scPopb3col, cList[[input$scPopb3cols]], sList[input$scPopb3fsz], 
               input$scPopb3exp, input$scPopb3max) 
  }) 
  output$scPopb3oup <- renderPlot({scPopb3oup()}) 
  output$scPopb3oup.ui <- renderUI({plotOutput("scPopb3oup", height = pList3[input$scPopb3psz])})
  output$scPopb3oup.dl <- downloadHandler( 
    filename = function() { paste0("scPop",input$scPopb3plt,"_",input$scPopb3grp,".",input$scPopb3oup.f) }, 
    content = function(file) { ggsave(file, height = input$scPopb3oup.h, width = input$scPopb3oup.w, plot = scPopb3oup())
    }) 
  
  ### Functions for Tab2
  
  diamond_data <- readRDS("./BestDiamond.rds")
  
  
  # Reactive filtering
  filtered_data <- eventReactive(input$search_btn, {
    ids <- strsplit(input$ids, ",\\s*")[[1]]
    
    if (length(ids) == 0 || all(ids == "")) {
      return(diamond_data)
    }
    
    
    if (input$id_type == "Potri") {
      return(diamond_data %>%
               filter(Potri %in% ids))
    } else {
      return(filtered_data <- diamond_data %>%
               filter(Potra %in% ids))
    }
  })
  
  
  output$filtered_diamond_table <- DT::renderDataTable({
    filtered_data()
  })
  
  ## functions to get Potri Potra table in most pages
  # Reactive filtering
  filtered_gene_data <- eventReactive(input$search_btn, {
    ids <- strsplit(input$ids, ",\\s*")[[1]]
    
    if (length(ids) == 0 || all(ids == "")) {
      return(diamond_data)
    }
    
    
    if (input$id_type == "Potri") {
      return(diamond_data %>%
               filter(Potri %in% ids))
    } else {
      return(filtered_data <- diamond_data %>%
               filter(Potra %in% ids))
    }
  })
  
  
  output$gene_diamond_table1 <- renderDataTable({
    req(input$scPopa1ass1, input$scPopa1inp1)
    if (startsWith(input$scPopa1ass1, "Assay")) {
      # Replace with your actual data logic
      gene = input$scPopa1inp1
      diamond_data %>%
        filter(Potra == gene)
    }
  })
  
  output$gene_diamond_table2 <- renderDataTable({
    req(input$scPopa2ass1, input$scPopa2ass2, input$scPopa2inp1, input$scPopa2inp2)
    if (startsWith(input$scPopa2ass1, "Assay") &
        !startsWith(input$scPopa2ass2, "Assay")) {
      # Replace with your actual data logic
      gene = input$scPopa2inp1
      diamond_data %>%
        filter(Potra == gene)
    }
    else if (!startsWith(input$scPopa2ass1, "Assay") &
             startsWith(input$scPopa2ass2, "Assay")) {
      # Replace with your actual data logic
      gene = input$scPopa2inp2
      diamond_data %>%
        filter(Potra == gene)
    }
    else if (startsWith(input$scPopa2ass1, "Assay") &
             startsWith(input$scPopa2ass2, "Assay")) {
      # Replace with your actual data logic
      genes = c(input$scPopa2inp1, input$scPopa2inp2)
      diamond_data %>%
        filter(Potra %in% genes)
    }
  })
  
  
  output$gene_diamond_table3 <- renderDataTable({
    req(input$scPopa3inp1, input$scPopa3inp2)
    # Replace with your actual data logic
    genes = c(input$scPopa3inp1, input$scPopa3inp2)
    diamond_data %>%
      filter(Potra %in% genes)
  })
  
  output$gene_diamond_table4 <- renderDataTable({
    req(input$scPopb1inp2)
    # Replace with your actual data logic
    gene = input$scPopb1inp2
    diamond_data %>%
      filter(Potra == gene)
  })
  
  
  output$gene_diamond_table5 <- renderDataTable({
    req(input$scPopb3inp)
    # Replace with your actual data logic
    genes = strsplit(input$scPopb3inp, ",\\s*")[[1]]
    diamond_data %>%
      filter(Potra %in% genes) %>% 
      slice(match(genes, Potra))
  })
  
  
}) 

shinyApp(ui=ui, server=server)
