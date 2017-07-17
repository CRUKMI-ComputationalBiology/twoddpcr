#' Inline \code{textInputs}.
#'
#' By default, \code{\link[shiny]{textInput}} objects are added to the page 
#' below each other. This function allows for inline \code{textInput} fields.
#'
#' @param inputId The identifier for the textInput object.
#' @param label The text label to display alongside.
#' @param value The default value of the textInput.
#' @param size The size (width) and maxlength.
#'
#' @return A \code{div} with the appropriate label and input field.
#'
#' @references Adapted from: http://stackoverflow.com/a/21132918/1262569

textInputRow <- function (inputId, label, value="", size=NULL)
{
  div(style="display:inline-block",
        tags$label(label, `for` = inputId), 
        tags$input(id = inputId, type="text", value=value,
                          class="input-small", size=size, maxlength=size)
  )
}

#' Inline \code{numericInputs}.
#'
#' By default, \code{\link[shiny]{numericInput}} objects are added to the page 
#' below each other. This function allows for inline \code{numericInput} 
#' fields.
#'
#' @param inputId The identifier for the numericInput object.
#' @param label The text label to display alongside.
#' @param value The default value of the numericInput.
#' @param size The size (width) and maxlength.
#'
#' @return A \code{div} with the appropriate label and input field.
#'
#' @references Adapted from: http://stackoverflow.com/a/21132918/1262569

numericInputRow <- function (inputId, label, value="", size=NULL)
{
  div(style="display:inline-block",
        tags$label(label, `for` = inputId), 
        tags$input(id = inputId, type="number", value=value,
                          class="input-small", size=size, maxlength=size)
  )
}

#' Shiny visualisation UI.
#'
#' A Shiny UI for interactive ddPCR droplet classification.
#'
#' @return A Shiny UI.
#'
#' @import shiny
#' @include shinyVisGlobal.R
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @export

shinyVisUI <- function()
{
  tagList(
    tags$head(
      tags$style(
        HTML("
          .help-block ul, .help-block ol {
            padding-left: 1.2em;
          }
          .multicol {
            height: auto;
            -webkit-column-count: 4;
            -moz-column-count: 4;
            column-count: 4;
          }
          div.checkbox {
            margin-top: 0px;
          }
          .shiny-output-error-validation {
            color: #D8006B;
          }
          "
        )
      ) 
    ),
    navbarPage(title="twoddpcr", id="tdNavbarPage",
      tabPanel("Input",
        sidebarLayout(
          sidebarPanel(width=3,
            radioButtons("datasetType", "Dataset to Use", inline=TRUE,
              c("From File", "Sample KRAS")
            ),
            conditionalPanel("input.datasetType == 'From File'",
              fileInput("inFile", "Choose Amplitude CSV Files",
                accept=c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv"),
                multiple=TRUE
              )
            ),
            uiOutput("useThisDatasetPH"),
            div(style="margin-top: 2ex",
              strong("Help"),
              helpText(
                tags$ol(
                  tags$li(
                    "Upload your own dataset or use the supplied KRAS sample ",
                    "dataset."
                  ),
                  tags$li(
                    "Click the 'Use This Dataset' button to load it."
                  )
                ),
                "Note that empty wells will be ignored."
              )
            ),
            radioButtons("sortWellsBy", "Sort Wells By:",
              c("Numbers First", "Letters First")
            )
          ), # sidebarPanel
          mainPanel(width=8,
            div(
              h4("About"),
              p(
                "A Shiny web application for ddPCR analysis. It is part of
                the ",
                a(
                  href="https://bioconductor.org/packages/twoddpcr/", 
                  "twoddpcr", target="_blank"
                ),
                "Bioconductor package. If you use this package, please ",
                a(
                  href="https://bioconductor.org/packages/release/bioc/vignettes/twoddpcr/inst/doc/twoddpcr.html#citing-twoddpcr", 
                  "cite it.", target="_blank"
                )
              ),
              h4("General Usage"),
              helpText(
                "Work your way along the tabs. For more detailed help, see ",
                "the 'Help' section in each tab's side panel."
              )
            ),
            div(style="padding-bottom: 2ex",
              h4("Labels"),
              "Set the labels for the channels. These will be printed on ",
              "the plots and the summary tables generated."
            ),
            div(
              textInputRow("ch1Label", "Ch1 Label", "Mt Amplitude"),
              textInputRow("ch1Abbrev", "Abbreviation", "Mt", size=4)
            ),
            div(style="padding-bottom: 2ex",
              textInputRow("ch2Label", "Ch2 Label", "Wt Amplitude"),
              textInputRow("ch2Abbrev", "Abbreviation", "Wt", size=4)
            ),
            uiOutput("setChLabelsPH")
          )
        ) # sidebarLayout
      ), # tabPanel "input"
      tabPanel("Select Wells",
        sidebarLayout(
          sidebarPanel(width=3,
            conditionalPanel("output.datasetSelectionSuccess",
              textInput("plateName", "Plate name", ""),
              radioButtons("viewMode", "Samples to Use",
                c("All", "Manual Selection"), inline=TRUE
              ),
              conditionalPanel("input.viewMode == 'Manual Selection'",
                strong("Wells"),
                uiOutput("wellSelectionPH")
              ),
              actionButton("useTheseWells", "Use These Wells"),
              div(style="margin-top: 2ex",
                strong("Help"),
                helpText(
                  tags$ol(
                    tags$li(
                      "Choose the wells that you wish to classify."
                    ),
                    tags$li(
                      "Click the 'Use These Wells' button to confirm the ",
                      "choice."
                    )
                  ),
                  "Use the 'Plate View' options to show/hide empty wells."
                )
              ),
              radioButtons("plateViewMode", "Plate View",
                c("Show All Wells", "Hide Empty Wells")
              )
            )
          ), # sidebarPanel
          mainPanel(width=8,
            h4("Wells in Plate"),
            p("This shows an overview of the plate. Each of the smaller boxes 
              represents a well in the plate. Droplet amplitudes are plotted 
              for wells that were used."),
            plotOutput("plotFacetGrid", height="650")
          ) # mainPanel
        ) #sidebarLayout
      ), #tabPanel "Select Wells"
      tabPanel("Classify",
        sidebarLayout(
          sidebarPanel(width=3,
            conditionalPanel("output.wellSelectionSuccess",
              selectInput("classifyMode", "Classification Mode",
                shinyVis$classifyModes
              ),
              checkboxInput("normalise", "Normalise data", FALSE),
              uiOutput("normaliseCheck"),
              conditionalPanel("input.classifyMode == 'Thresholds'",
                div(class="row-fluid",
                  div(class="span3",
                    numericInput("ch1Threshold", "Ch1 Threshold",
                      shinyVis$threshDefault$ch1,
                      min=shinyVis$threshLimits$ch1$min,
                      max=shinyVis$threshLimits$ch1$max, step=1
                    )
                  ),
                  div(class="span3",
                    numericInput("ch2Threshold", "Ch2 Threshold",
                      shinyVis$threshDefault$ch2,
                      min=shinyVis$threshLimits$ch2$min,
                      max=shinyVis$threshLimits$ch2$max, step=1
                    )
                  )
                )
              ),
              conditionalPanel(
                "input.classifyMode == 'K-Nearest Neighbour'",
                numericInput("knnK", "k (# of nearest neighbours)", 1),
                checkboxInput("knnProb", "Use k-NN Confidence", FALSE),
                conditionalPanel("input.knnProb",
                  sliderInput("knnConf", "Confidence Level",
                    0.0, 1.0, 0.7
                  )
                )
              ),
              conditionalPanel(
                "input.classifyMode == 'K-means Clustering' ||
                 input.classifyMode == 'Grid'",
               uiOutput("selectedClassPH")
              ),
              conditionalPanel(
                "input.classifyMode == 'K-means Clustering'",
                checkboxInput("removeThisClass", "Remove This Class",
                  FALSE
                ),
                tags$strong("Initial Centres"),
                verbatimTextOutput("centresOutput")
              ),
              conditionalPanel(
                "input.classifyMode == 'Grid'",
                strong("Grid Thresholds"),
                verbatimTextOutput("gridThesholdsOutput")
              ),
              actionButton("classify", "Run Classification"),
              div(style="margin-top: 2ex",
                strong("Help"),
                helpText(
                  "Choose a 'Classification Mode'. ",
                  tags$ul(
                    tags$li("'K-means Clustering' works well in most cases."),
                    tags$li("'Thresholds' works well if the clusters ",
                                   "are well-separated.")
                  ),
                  "See the",
                  a(href="https://bioconductor.org/packages/release/bioc/vignettes/twoddpcr/inst/doc/twoddpcr.html#analysis-of-the-data", 
                    "Bioconductor vignette", target="_blank"
                  ),
                  "for a discussion comparing these two approaches.",
                  p(
                    "Use 'Normalise data' to minimise variations between 
                    clusters in different wells."
                  )
                )
              ),
              conditionalPanel(
                "input.classifyMode == 'K-means Clustering'",
                div(
                  strong("K-means Clustering Help"),
                  helpText(
                    "K-means clustering requires well-selected cluster ",
                    "centres.",
                    br(),
                    tags$ol(
                      tags$li("Choose a class from 'Selected Class'."),
                      tags$li("Click on the plot to set a new centre.")
                    ),
                    p("Use the 'Remove This Class' checkbox if there is no
                    cluster."),
                    p(
                      "Note that badly chosen centres could lead to error 
                      messages. In this case, try setting centres close to the 
                      centres of clusters and remove classes if they are not 
                      present."
                    )
                  )
                )
              ),
              conditionalPanel("input.classifyMode == 'Thresholds'",
                div(
                  strong("Thresholds Help"),
                  helpText(
                    tags$ul(
                      tags$li("Divide the plot into four quadrants."),
                      tags$li("Click on the plot to set new thresholds.")
                    )
                  )
                )
              ),
              conditionalPanel("input.classifyMode == 'Grid'",
                div(
                  strong("Grid Help"),
                  helpText(
                    "Divide the plot into four regions ",
                    "(droplets not in the regions are treated as rain).",
                    tags$ol(
                      tags$li("Select a class."),
                      tags$li(
                        "Click on the plot to set new thresholds for this ",
                        "class."
                      )
                    )
                  )
                )
              ),
              conditionalPanel(
                "input.classifyMode == 'K-Nearest Neighbour'",
                div(
                  strong("K-Nearest Neighbour Help"),
                  helpText(
                    tags$ul(
                      tags$li("Use training data."),
                      tags$li(
                        "The k-nearest neighbours in the training data are ",
                        "used to decide on the class of each droplet."
                      ),
                      tags$li(
                        "'Use k-NN Confidence' for setting the fraction of ",
                        "votes required for classification."
                      )
                    )
                  )
                )
              ),
              sliderInput("heatWidth", "Bin Width", 50, 500, 100)
            )
          ), # sidebarPanel
          mainPanel(width=8,
            h4("Droplets in Selected Wells"),
            p("A density plot of the selected wells."),
            plotOutput("plotSelectedWells", height="650",
                              click="plot_click")
          ) # mainPanel
        ) #sidebarLayout
      ), #tabPanel "Classify"
      tabPanel("Results",
        sidebarLayout(
          sidebarPanel(width=3,
            conditionalPanel("output.datasetSelectionSuccess",
              selectInput("clToShow", "Classification To Show",
                c("None")
              ),
              radioButtons("resultsViewMode", "Sample to View",
                c("All", "Manual Selection"), inline=TRUE
              ),
              conditionalPanel(
                "input.resultsViewMode == 'Manual Selection'",
                div(style="padding-bottom: 2ex",
                  strong("Wells"),
                  uiOutput("resultsWellsPH"),
                  actionButton("showResultsWells", "Show Wells")
                )
              ),
              conditionalPanel("output.thresholdsSet",
                checkboxInput("showThresholds", "Show Thresholds", FALSE)
              ),
              checkboxInput("finalCentres", "Show Final Centres", FALSE),
              radioButtons("rainType", "Rain Type",
                c("No Rain", "Mahalanobis", "Standard Deviation")
              ),
              conditionalPanel("input.rainType == 'Mahalanobis'",
                uiOutput("mvnRainNNPH"), uiOutput("mvnRainNPPH"),
                uiOutput("mvnRainPNPH"), uiOutput("mvnRainPPPH")
              ),
              conditionalPanel("input.rainType == 'Standard Deviation'",
                uiOutput("sdRainNNPH"), uiOutput("sdRainNPPH"),
                uiOutput("sdRainPNPH"), uiOutput("sdRainPPPH")
              ),
              conditionalPanel("input.rainType != 'No Rain'",
                div(style="padding-bottom: 2ex",
                  actionButton("makeItRain", "Make it Rain")
                )
              ),
              div(
                strong("Help"),
                helpText(
                  tags$ul(
                    tags$li(
                      "The 'Export Amplitudes to File' button creates a zip ",
                      "file containing CSV droplet amplitude files."
                    ),
                    tags$li(
                      "Note: the exported amplitudes are classifed as 'NN', 
                      'NP', 'PN' and 'PP' (N=Negative, P=Positive). Using the 
                      default labels, these are abbreviations for 'Mt-Wt-', 
                      'Mt-Wt'+, 'Mt+Wt-' and 'Mt+Wt+', respectively."
                    ),
                    tags$li(
                      "The 'Set as Training Data' button creates training ",
                      "data to be used with the K-NN algorithm."
                    )
                  )
                )
              ),
              div(
                strong("Rain Help"),
                helpText(
                  tags$ul(
                    tags$li(
                      "Droplets can fall between clusters and therefore have 
                      classifications that can be subjective. Defining these 
                      droplets as 'rain' helps to remove ambiguity."
                    ),
                    tags$li(
                      "Recommended: 'Mahalanobis' takes cluster rotations into
                      account."
                    ),
                    tags$li(
                      "'Standard Deviation' gives linear cut-offs."
                    )
                  )
                )
              )
            )
          ), # sidebarPanel
          mainPanel(width=8,
            h4("Classification"),
            p("The outcome of the chosen classification method."),
            plotOutput("plotClassification", height="650"),
            uiOutput("exportAmplitudesPH")
          ) # mainPanel
        ) #sidebarLayout
      ), #tabPanel "Results"
      tabPanel("Summary",
        sidebarLayout(
          sidebarPanel(width=3,
            conditionalPanel("output.uploadSuccess ||
                input.datasetType == 'Sample KRAS'",
              div(
                conditionalPanel("output.summarySet",
                  strong("File Options"),
                  div(style="padding-bottom: 2ex",
                    downloadButton("exportSummaryButton",
                      "Export Summary to File"
                    )
                  ),
                  div(style="padding-bottom: 2ex",
                    downloadButton("generateHtmlReport",
                      "Generate HTML Report"
                    )
                  )
                ),
                strong("Help"),
                helpText(
                  tags$ul(
                    tags$li(
                      "Positive/negative droplets counts are summarised here."
                    ),
                    tags$li(
                      "The summary can be exported to a CSV file or a report 
                      generated using the buttons above."
                    ),
                    tags$li(
                      "'AcceptedDroplets' is the total number of droplets in 
                      each well (if necessary, after subtracting rain)."
                    ),
                    tags$li(
                      "The 'Concentration' figures are estimates for the number 
                      of starting molecules per uL."
                    ),
                    tags$li(
                      "The 'CopiesPer20uLWell' figures are estimates for the 
                      number of starting molecules in a 20uL well."
                    ),
                    tags$li(
                      "'Ratio' is the ratio of channel 1 molecules to channel 2 
                      molecules."
                    ),
                    tags$li(
                      "'FracAbun' is short for 'fractional abundance'. It 
                      estimates how many starting channel 1 molecules there 
                      were out of all molecules. The reported figure is a 
                      percentage."
                    )
                  )
                )
              )
            )
          ), # sidebarPanel
          mainPanel(width=8,
            h4("Plate summary"),
            p("A summary of the number of droplets in each of the chosen wells. 
              Poisson estimates of the starting numbers of molecules is also 
              provided."),
            dataTableOutput("wellsSummary")
          ) # mainPanel
        ) #sidebarLayout
      ), #tabPanel "Summary"
      navbarMenu("Advanced",
        tabPanel("Options",
          sidebarLayout(
            sidebarPanel(width=3,
              strong("Help"),
              helpText(
                p(
                  "The options here are global variables used for calculations
                  and plotting."
                ),
                h5("Droplet Volume"),
                p("The volume of each droplet. Defaults to 0.85nl."),
                h5("Minimum Cluster Separation"),
                p(
                  "This figure is used for normalisation. Each well has
                  k-means clustering run on the NN/NP and NN/PN clusters
                  separately. If two cluster centres are less than the
                  'Minimum Separation' figure apart, then the clustering
                  result is rejected."
                ),
                h5("Plot limits"),
                p(
                  "The minimum and maximum values for Ch1 and Ch2 to plot.
                  This allows us to increase the size of the plot only, not
                  decrease it. If droplets lie outside the defined min/max
                  values, the plot area will be increased automatically."
                )
              )
            ), # sidebarPanel
            mainPanel(width=8,
              h4("Options"),
              div(
                numericInput("dropletVolume", "Droplet Volume (nl)", 0.85),
                uiOutput("setDropletVolumePH")
              ),
              div(
                numericInput(
                  "minSeparation", "Minimum Cluster Separation", 2000
                ),
                uiOutput("setMinSeparationPH")
              ),
              div(
                strong("Plot limits")
              ),
              div(
                numericInputRow("ch1min", "Ch1 min", 3000),
                numericInputRow("ch1max", "Ch1 max", 13500)
              ),
              div(style="padding-bottom: 2ex",
                numericInputRow("ch2min", "Ch2 min", 1000),
                numericInputRow("ch2max", "Ch2 max", 9000)
              ),
              uiOutput("setPlotLimitsPH")
            ) # mainPanel
          ) # sidebarLayout
        ), # tabPanel "Options"
        tabPanel("View Training",
          sidebarLayout(
            sidebarPanel(width=3,
              div(
                strong("Help"),
                helpText(
                  tags$ul(
                    tags$li(
                      "The current training data will be shown in the plot on
                      the right."
                    ),
                    tags$li(
                      "To create new training data, classify some data and in 
                      the 'Results' tab click the 'Set as Training Data' 
                      button."
                    )
                  )
                )
              )
            ), # sidebarPanel
            mainPanel(width=8,
              h4("Current training data"),
              p("If set, the current training data will be shown below. This 
                will be used as training data for the k-nearest neighbour 
                alogrithm."),
              plotOutput("plotTraining", height="650")
            ) # mainPanel
          ) # sidebarLayout
        ) # tabPanel "View Training"
      ), # navbarMenu "More"
      tabPanel("Help",
        sidebarLayout(
          sidebarPanel(width=3,
            div(
              "Options appear here."
            ),
            div(
              strong("Help"),
              helpText("Help information for each panel appears here.")
            )
          ), # sidebarPanel
          mainPanel(width=8,
            h4("About"),
            p(
              "A Shiny web application for ddPCR analysis. It is part of
              the ",
              a(
                href="https://bioconductor.org/packages/twoddpcr/", 
                "twoddpcr", target="_blank"
              ),
              "Bioconductor package. If you use this package, please ",
              a(
                href="https://bioconductor.org/packages/release/bioc/vignettes/twoddpcr/inst/doc/twoddpcr.html#citing-twoddpcr", 
                "cite it.", target="_blank"
              )
            ),
            p(
              "The package and this Shiny app are maintained by Anthony
              Chiu. It was developed using ddPCR experiments performed by
              Mahmood Ayub. The project was managed by Caroline Dive,
              Ged Brady (",
              a(href="http://www.cruk.manchester.ac.uk/Research/CRUK-MI-Groups/CEP/Home",
                "Clinical and Experimental Pharmacology Group, Cancer
                Research UK Manchester Institute", target="_blank"),
              ") and Crispin J. Miller (",
              a(href="http://www.cruk.manchester.ac.uk/Research/CRUK-MI-Groups/RNA-Biology-Computational-Biology/Home",
                "RNA Biology Group, Cancer Research UK Manchester
                Institute", target="_blank"),
              ")."
            ),
            h4("General Usage"),
            p(
              "Work your way along the tabs from left-to-right. Help 
              information is available in the side-panel on each tab.
              Each tab can be summarised as:"
            ),
            tags$ol(
              tags$li("Input: Select the dataset."),
              tags$li("Select Wells: Choose the wells to use"),
              tags$li("Classify: Gate the wells."),
              tags$li("Results: View the results."),
              tags$li("Summary: View/extract the computed figures.")
            ),
            h4("Input"),
            p("Select the dataset to use. This can be:"),
            tags$ul(
              tags$li(
                "Loaded from droplet amplitude CSV files. These can be
                exported from Bio-Rad's QuantaSoft software but must be two
                channel data; further instructions can be found in the ",
                a(
                  href="https://bioconductor.org/packages/release/bioc/vignettes/twoddpcr/inst/doc/twoddpcr.html#exporting-droplet-amplitudes-from-quantasoft-to-csv-files",
                  "twoddpcr package vignette.", target="_blank"
                ),
                "Note that empty wells will be ignored."
              ),
              tags$li(
                "The sample KRAS dataset can be used as a toy example."
              )
            ),
            p(
              "The labels for the two targets (in the two channels) can be
              customised."
            ),
            tags$ul(
              tags$li(
                "'Ch1/Ch2 Label' are used in the plots of the droplet
                amplitudes."
              ),
              tags$li(
                "The corresponding 'Abbreviation' field is used in the
                summary of computed figures. They are prefixed to various
                column names."
              )
            ),
            h4("Select Wells"),
            p(
              "This tab shows an overview of the wells that were loaded.
              From here, we select the samples/wells to be used:"
            ),
            tags$ul(
              tags$li("'All' uses all of the wells."),
              tags$li(
                "'Manual Selection' allows the selection of individual wells."
              )
            ),
            p(
              "We can choose to view the nonempty wells by changing the
              'Plate View' to 'Hide Empty Wells'."
            ),
            h4("Classify"),
            p(
              "This tab is used for the gating of the droplets. The 'K-means 
              Clustering' and 'Thresholds' approaches are discussed in the ",
              a(href="https://bioconductor.org/packages/release/bioc/vignettes/twoddpcr/inst/doc/twoddpcr.html#analysis-of-the-data", 
                "Bioconductor vignette.", target="_blank"
              ),
              "In general, 'K-means Clustering' should work without any 
              modifications if the data forms four clear clusters; just click 
              'Run Classification'."
            ),
            p(
              "If the clusters are well separated, 'Thresholds' should work
              too."
            ),
            h5("K-means Clustering"),
            p(
              "K-means clustering generally works well but it requires
              well-selected cluster centres. To do this:"
            ),
            tags$ol(
              tags$li("Choose a class from 'Selected Class'."),
              tags$li(
                "Click on the density plot to set a new centre for the 
                selected class."
              )
            ),
            p(
              "Use the 'Remove This Class' checkbox if there is no cluster."
            ),
            p(
              "Note that badly chosen centres could lead to error messages. In 
              this case, try setting centres close to the centres of clusters 
              and remove classes if they are not present."
            ),
            h5("Thresholds"),
            p(
              "This works well if the clusters are well separated. This mode
              sets thresholds to divide the plot into four quadrants. To do
              this, either"
            ),
            tags$ul(
              tags$li("Click on the plot to set the new thresholds."),
              tags$li(
                "Manually change the 'Ch1 Threshold' and 'Ch2 Threshold'
                figures."
              )
            ),
            h5("Grid"),
            p(
              "This mode divides the plot into four regions, each
              corresponding to one of the four classes. Any droplets
              not included in these regions are treated as 'rain'. To
              change the regions:"
            ),
            tags$ol(
              tags$li("Choose a class from 'Selected Class'."),
              tags$li(
                "Click on the density plot to set a new region for the 
                selected class."
              )
            ),
            h5("K-Nearest Neighbour"),
            p(
              "This method requires the use of training data. This method
              should only be used by experts. The full work-flow is as
              follows:"
            ),
            tags$ol(
              tags$li(
                "To set training data: classify some wells (e.g. one or two
                wells with little noise) and then 'Set as Training Data' in
                the 'Results' tab. The resulting training data can be viewed
                in 'Advanced > View Training'."
              ),
              tags$li(
                "The parameter 'k' can be adjusted, i.e. for each droplet,
                the k-NN algorithm finds the k-nearest training droplets and
                decides on the classification by majority vote."
              ),
              tags$li(
                "'Use k-NN Confidence' refines the majority required, i.e.
                if the proportion of the winning class does not exceed the
                'Confidence Level', then no classification is assigned."
              )
            ),
            h4("Results"),
            p(
              "This tab shows the resulting classification. Which wells to
              show can be changed under 'Sample to View'."
            ),
            p(
              "The ambiguous regions between clusters ('Rain') can be removed
              using two methods. The 'Mahalanobis' method is recommended with
              some manual tweaking of the parameters. It fits clusters to
              ellipses, while the 'Standard Deviation' method is restricted to
              linear cut-offs."
            ),
            tags$ul(
              tags$li(
                "Mahalanobis: This is based on Mahalanobis distance. It fits
                each cluster to a distribution and removes droplets too far
                from the cluster centre. This can be adjusted for each cluster
                with the sliders."
              ),
              tags$li(
                "Standard Deviation: This works calculated the mean and
                standard deviation of each cluster in both channels. These
                two figures are used to find linear cut-offs."
              )
            ),
            "The classification can be exported as a CSV file for later use.
            It can also be set as training data for the k-nearest neighbour
            classification method.",
            h4("Summary"),
            p(
              "This tab shows a summarised count of the numbers of droplets in
              each class and the resulting estimates of the concentration of
              mutants in each sample. It also reports the fractional abundance
              ('FracAbun') of the channel 1 target. In some use cases, this
              corresponds to the variant allele frequency."
            ),
            p(
              "This summary table can be exported as a CSV file, which can
              then be imported in a spreadsheet. The results and summary table 
              can also be used to create an HTML report."
            )
          )
        ) # sidebarLayout
      ) # tabPanel "input"
    ) # navbarPage
  ) # tagList
}

