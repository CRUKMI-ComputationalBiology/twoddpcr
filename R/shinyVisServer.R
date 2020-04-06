#' Shiny visualisation server.
#'
#' A Shiny server for interactive ddPCR droplet classification.
#'
#' @param input Shiny input list.
#' @param output Shiny output list.
#' @param session Shiny session.
#'
#' @return A Shiny server.
#'
#'
#' @author Anthony Chiu, \email{anthony.chiu@cruk.manchester.ac.uk}
#'
#' @include shinyVisGlobal.R
#' @import ggplot2
#' @import shiny
#' @export

shinyVisServer <- function(input, output, session) {
  # Error messages.
  messages <- reactiveValues(
    error="No ddPCR droplet amplitude data loaded.",
    uploadError=""
  )

  # Variables to store the base set of samples, the current selection, and
  # various parameters.
  wells <- reactiveValues(
    all=ddpcrPlate(),  # base set of samples
    selected=ddpcrPlate(),  # current selection
    manaulSelection=c(),  # wells chosen by manual selection
    plateName=""
  )

  # The classification result and its summary table.
  results <- reactiveValues(
    drops=ddpcrPlate(),
    selected=ddpcrPlate(),
    manualSelection=c(),  # wells chosen by manual selection
    baseMode="None",
    mode="None",
    summary=NULL  # summarised data frame of the wells
  )

  # Labels to use for plots etc.
  chLabels <- reactiveValues(
    ch1="Mt Amplitude", ch1Abbrev="Mt",
    ch2="Wt Amplitude", ch2Abbrev="Wt",
    errorMsg=""
  )

  # The internally used symbol for the selected class.
  selectedClass <- reactiveValues(
    value="NN"
  )

  # Labels using the user-specified prefixes.
  abbrevLabels <- function(decreasing=FALSE) {
    abbrevs <- c(chLabels$ch1Abbrev, chLabels$ch2Abbrev)
    labels <- c(paste0(abbrevs, "-", collapse=""),
                paste0(abbrevs, c("-", "+"), collapse=""),
                paste0(abbrevs, c("+", "-"), collapse=""),
                paste0(abbrevs, "+", collapse=""))
    if(decreasing) {
      labels <- rev(labels)
    }
    return(labels)
  }

  # Other global variables.
  globals <- reactiveValues(
    minSeparation=2000,
    plotLimits=list(x=c(1000, 9000), y=c(3000, 13500))
  )

  # Action button for setting the channel labels (as long as they are
  # reasonable).
  output$setChLabelsPH <- renderUI({
    validate(
      need(input$ch1Label != input$ch2Label &&
           input$ch1Abbrev != input$ch2Abbrev,
           "The labels for the channels should be different."),
      need(input$ch1Label != "" && input$ch2Label != "" &&
           input$ch1Abbrev != "" && input$ch2Abbrev != "",
           "The labels should not be empty.")
    )

    actionButton("setChLabels", "Set Labels")
  })


  observeEvent(input$setChLabels, {
    chLabels$ch1 <- input$ch1Label
    chLabels$ch1Abbrev <- input$ch1Abbrev
    chLabels$ch2 <- input$ch2Label
    chLabels$ch2Abbrev <- input$ch2Abbrev
    setSummary(results$mode, sortByLetter=sortByLetter())
  })

  # A map between the selectInput name and the internal name.
  classifyModeMap <- list("Thresholds"="thresholds",
                          "Grid"="grid",
                          "K-Nearest Neighbour"="knn",
                          "K-means Clustering"="kmeans")

  # Get an internal mode name from the long name.
  internalMode <- function(m) {
    if(is.null(m)) {
      return("None")
    } else if(m %in% names(classifyModeMap)) {
      return(classifyModeMap[[m]])
    } else {
      return(m)
    }
  }

  # Get the long name from the internal mode name.
  longMode <- function(m) {
    modeMatch <- (m == classifyModeMap)
    if(any(modeMatch)) {
      return(names(classifyModeMap)[which(modeMatch)])
    } else {
      return(m)
    }
  }

  # Set a mode variable.
  clMode <- reactive({
    switch(input$classifyMode,
           "Thresholds"="thresholds",
           "Grid"="grid",
           "K-Nearest Neighbour"="knn",
           "K-means Clustering"="kmeans")
  })

  # Training set used for k-NN algorithm.
  trainingData <- reactiveValues(
    drops=data.frame(
      "Ch1.Amplitude"=double(),
      "Ch2.Amplitude"=double(),
      class=factor(c(), levels=ddpcr$classesRain)
    )
  )

  # Set the wells$selected variable according to the wells selected in the app.
  setSelectedWells <- function() {
    if(is.null(wells$all)) {
      wells$selected <- ddpcrPlate()
    } else if(input$viewMode == "All") {
      wells$selected <- wells$all
    } else if(input$viewMode == "Manual Selection") {
      if(length(input$wellSelection) == 0) {
        sel <- list()
      } else {
        sel <- input$wellSelection
      }
      wells$selected <- ddpcrPlate(wells=wells$all[sel])
      wells$manualSelection <- input$wellSelection
    }

    # Normalise the wells if desired.
    if(input$normalise) {
      tryCatch({
        wells$selected <- renormalisePlate(wells$selected,
                                           initialCentres=startingCentres(),
                                           minSeparation=globals$minSeparation)
      },
      warning=function(e) {
        messages$error <- as.character(e) ######## protect
      },
      error=function(e) {
        messages$error <- as.character(e) ######## protect
      })
    } else {
      messages$error <- ""
    }
  }

  # Set the results$all variable according to the wells selected in the
  # app.
  setResultsWells <- function() {
    if(is.null(results$all)) {
      results$selected <- ddpcrPlate()
    } else if(input$resultsViewMode == "All") {
      results$selected <- results$all
    } else if(input$resultsViewMode == "Manual Selection") {
      if(length(input$resultsWells) == 0) {
        sel <- list()
      } else {
        sel <- input$resultsWells
      }
      results$selected <- ddpcrPlate(wells=results$all[sel])
      results$manualSelection <- input$resultsWells
    }
  }

  # Check that the given ddpcrPlate object has a rainy 'm' class with
  # a specified prefix.
  rainyModeExists <- function(drops, m, prefix) {
    if(!is.null(drops) && (length(drops) > 0)) {
      mWithRain <- paste0(m, prefix, "Rain")
      if(mWithRain %in% plateClassificationMethod(drops)) {
        return(TRUE)
      }
    }
    return(FALSE)
  }

  # Check that the given ddpcrPlate object has a multivariate rainy 'm' class.
  mvnRainyModeExists <- function(drops, m) {
    rainyModeExists(drops, m, "Mvn")
  }

  # Check that the given ddpcrPlate object has an SD rainy 'm' class.
  sdRainyModeExists <- function(drops, m) {
    rainyModeExists(drops, m, "Sd")
  }

  # Use the current grid thresholds to set training data.
  setTrainingDataFromGrid <- function() {
    td <- gridClassify(wells$selected,
      gridThresholds$NN$ch1, gridThresholds$NN$ch2,
      gridThresholds$NP$ch1, gridThresholds$NP$ch2,
      gridThresholds$PN$ch1, gridThresholds$PN$ch2,
      gridThresholds$PP$ch1, gridThresholds$PP$ch2,
      "class"
    )
    trainingData$drops <- do.call(rbind, removeDropletClasses(td, "class"))
  }

  # Show any existing classification methods.
  updateClassifications <- function(selected=NULL) {
    # Select the correct methods to show.
    if(!is.null(results$all) && !isEmpty(results$all)) {
      clMethods <- commonClassificationMethod(results$all)
    } else {
      clMethods <- c("None")
    }

    # Remove rainy classification methods from the list.
    clMethods <- clMethods[!grepl("Rain$", clMethods)]

    # Get the long form names for the classification methods.
    longClMethods <- vapply(clMethods, longMode, character(1))
    names(longClMethods) <- NULL

    # Do we change the selection?
    if(is.null(selected)) {
      updateSelectInput(session, "clToShow", choices=longClMethods)
    } else {
      updateSelectInput(session, "clToShow", choices=longClMethods,
                        selected=selected)
    }
  }

  # Check whether upload was successful.
  output$uploadSuccess <- reactive({
    return(messages$error == "" &&
           length(input$inFile$name) > 0 && length(wells$all) > 0)
  })
  outputOptions(output, "uploadSuccess", suspendWhenHidden=FALSE)

  # Check whether a dataset has been selected.
  output$datasetSelectionSuccess <- reactive({
    return(!is.null(wells$all) && !isEmpty(wells$all))
  })
  outputOptions(output, "datasetSelectionSuccess", suspendWhenHidden=FALSE)

  # Check whether some wells have been selected.
  output$wellSelectionSuccess <- reactive({
    return(!is.null(wells$selected) && !isEmpty(wells$selected))
  })
  outputOptions(output, "wellSelectionSuccess", suspendWhenHidden=FALSE)

  # Show the "Use This Dataset" action button only if droplet amplitude CSVs
  # have been uploaded.
  output$useThisDatasetPH <- renderUI({
    validate(
      need(input$datasetType == "Sample KRAS" ||
           (messages$uploadError == "" && length(input$inFile$name) > 0 &&
            length(wells$loadedAll) > 0), {
             if(messages$uploadError != "") {
               msg <- paste0("CSV file upload failed: ",
                             sub("\\s+$", "", messages$uploadError), " ")
             } else {
               msg <- ""
             }

             msg <- paste0(msg, "Please upload two channel droplet amplitude ",
                           "CSV files")

             if(messages$uploadError != "") {
               msg <- paste0(msg, ". ")
             } else {
               msg <- paste0(msg, " or use the 'Sample KRAS' dataset. ")
             }

             msg <- paste0(msg, "For details, see the 'Help' tab.")
           })
    )
    actionButton("useThisDataset", "Use This Dataset")
  })

  # Import CSV files and update all of the variables.
  loadFromFile <- function() {
    tryCatch({
      # User uploads CSV files.
      if(is.null(input$inFile)) {
        d <- list()
      } else {
        d <- lapply(input$inFile$datapath, utils::read.csv)
        nonemptyWells <- (vapply(d, nrow, numeric(1)) != 0)  # Remove empty wells.
        d <- d[nonemptyWells]
        names(d) <- extractWellNames(input$inFile$name)[nonemptyWells]

        # Load the plate name.
        wells$loadedPlateName <- extractPlateName(input$inFile$name[1])
        names(wells$loadedPlateName) <- NULL
      }

      wells$loadedAll <- ddpcrPlate(wells=d)
      messages$uploadError <- ""
    },
    warning=function(e) {
      wells$loadedAll <- ddpcrPlate()
      messages$uploadError <- "invalid CSV file format."
    },
    error=function(e) {
      wells$loadedAll <- ddpcrPlate()
      messages$uploadError <- "invalid CSV file format."
    })

  }

  # Get the droplet data for each well in the selected dataset.
  observeEvent(input$inFile, {
    loadFromFile()
  })

  # Load from file or use the sample dataset.
  observeEvent(input$useThisDataset, {
    if(input$datasetType == "From File" && !is.null(wells$loadedAll)) {
      # Set the well data from those loaded.
      wells$plateName <- wells$loadedPlateName
      wells$all <- wells$loadedAll

      # Allow the user to view previous "Results" if they already exist without
      # having to classify anything.
      cmethods <- commonClassificationMethod(wells$all)
      if(length(cmethods[cmethods != "None"]) >= 1) {
        results$all <- wells$all
        results$selected <- wells$all
        messages$error <- ""
      } else {
        results$all <- ddpcrPlate()
      }

      # Update
      updateNavbarPage(session, "tdNavbarPage", selected="Select Wells")
      updateTextInput(session, "plateName", value=wells$plateName)
      updateRadioButtons(session, "viewMode", selected="Overview")
      updateClassifications()
    } else if(input$datasetType == "Sample KRAS") {
      wells$plateName <- "Sample KRAS"
      environment(KRASdata) <- asNamespace("twoddpcr")
      wells$all <- ddpcrPlate(wells=twoddpcr::KRASdata)
      messages$error <- ""

      # Since we changed datasets, the selected wells should be reset.
      updateNavbarPage(session, "tdNavbarPage", selected="Select Wells")
      updateTextInput(session, "plateName", value=wells$plateName)
      updateRadioButtons(session, "viewMode", selected="Overview")

      # Allow the previous classifications in the "Results" tab to be viewed.
      results$all <- wells$all
      results$selected <- wells$all

      # Update the classification modes available.
      updateClassifications()
    }
  })

  observeEvent(input$plateName, {
    wells$plateName <- input$plateName
  })

  # Combine all of the droplet data, or use the data in the selected wells.
  observeEvent(input$useTheseWells, {
    setSelectedWells()
    updateNavbarPage(session, "tdNavbarPage", selected="Classify")
  })

  observeEvent(input$showResultsWells, {
    setResultsWells()
  })

  observeEvent(input$resultsViewMode, {
    setResultsWells()
  })

  # Manual thresholds.
  thresholds <- reactiveValues(
    ch1=shinyVis$threshDefault$ch1,
    ch2=shinyVis$threshDefault$ch2
  )

  # Thresholds to use for gridClassify.
  gridThresholds <- reactiveValues(
    NN=list(ch1=6500, ch2=1900),
    NP=list(ch1=6500, ch2=5000),
    PN=list(ch1=10000, ch2=2900),
    PP=list(ch1=7500, ch2=5000)
  )

  # K-means centres.
  initialCentres <- reactiveValues(
    NN=list(val=c(5000, 1500), removed=FALSE),
    PN=list(val=c(10000, 2000), removed=FALSE),
    NP=list(val=c(5500, 7000), removed=FALSE),
    PP=list(val=c(9000, 6000), removed=FALSE)
  )

  # How are we sorting the wells?
  sortByLetter <- reactive({
    return(input$sortWellsBy == "Letters First")
  })

  # Set the summary of wells for classification method 'm'.
  setSummary <- function(m, sortByLetter) {
    if(!is.null(results$all) && m != "None") {
      s <- plateSummary(results$all, m,
                        ch1Label=chLabels$ch1Abbrev,
                        ch2Label=chLabels$ch2Abbrev,
                        sortByLetter=sortByLetter)
      colnames(s)[1:4] <- abbrevLabels(decreasing=TRUE)
      results$summary <- data.frame("Well"=rownames(s), s, check.names=FALSE)
    } else {
      results$summary <- NULL
    }
  }

  # Checkboxes for the wells. Useful when the view mode is "Manual Selection".
  output$wellSelectionPH <- renderUI({
    if(!is.null(wells$all)) {
      wellNames <- sortWells(names(wells$all), sortByLetter())
      div(align="left", class="multicol",
        checkboxGroupInput("wellSelection", NULL,
                           choices=wellNames,
                           selected=wells$manualSelection)
      )
    }
  })

  # Change the sort order of wells
  observeEvent(input$sortWellsBy, {
    if(!is.null(results$summary)) {
      results$summary <-
        sortDataFrame(results$summary, sortByLetter=sortByLetter())
    }
  })

  # Checkboxes for the wells. Useful when the view mode is "Manual Selection".
  output$resultsWellsPH <- renderUI({
    if(!is.null(results$all) && !is.null(results$selected)) {
      wellNames <- sortWells(names(results$all), sortByLetter())
      div(align="left", class="multicol",
        checkboxGroupInput("resultsWells", NULL,
                           choices=wellNames,
                           selected=results$manualSelection)
      )
    }
  })

  # Normalise the data.
  observeEvent(input$normalise, {
    setSelectedWells()
  })

  # Show an error message if the normalisation failed.
  output$normaliseCheck <- renderUI({
    validate(
      need(messages$error == "", messages$error)
    )
  })

  # Radio buttons for the currently selected class.
  output$selectedClassPH <- renderUI({
    radioButtons("selectedClass", "Selected Class", abbrevLabels(),
                 inline=TRUE)
  })

  # Get the currently selected centre.
  observeEvent(input$selectedClass, {
    abbLabels <- abbrevLabels()
    if(input$selectedClass == abbLabels[1]) {
      selectedClass$value <- "NN"
    } else if(input$selectedClass == abbLabels[2]) {
      selectedClass$value <- "NP"
    } else if(input$selectedClass == abbLabels[3]) {
      selectedClass$value <- "PN"
    } else if(input$selectedClass == abbLabels[4]) {
      selectedClass$value <- "PP"
    }
  })

  # Are we viewing thresholds classification and have thresholds been saved?
  output$thresholdsSet <- reactive({
    return(!is.null(results$baseMode) && !is.na(results$baseMode) &&
       results$baseMode == "thresholds" &&
       !is.null(results$ch1Threshold) &&
       !is.null(results$ch2Threshold))
  })
  outputOptions(output, "thresholdsSet", suspendWhenHidden=FALSE)

  # Toggle the status of the selected class.
  observeEvent(input$removeThisClass, {
    initialCentres[[selectedClass$value]]$removed <- input$removeThisClass
  })

  # Update the "Remove This Class" checkbox depending on the selected class.
  observeEvent(selectedClass$value, {
    updateCheckboxInput(session, "removeThisClass",
                        value=initialCentres[[selectedClass$value]]$removed)
  })

  # Update the mode based on the selected classification to show.
  observeEvent(input$clToShow, {
    m <- internalMode(input$clToShow)
    results$baseMode <- m

    if(input$rainType == "Mahalanobis" &&
       mvnRainyModeExists(results$all, m)) {
      results$mode <- paste0(m, "MahRain")
    } else if(input$rainType == "Standard Deviation" &&
            sdRainyModeExists(results$all, m)) {
      results$mode <- paste0(m, "sdRain")
    } else {
      results$mode <- m
    }

    setSummary(results$mode, sortByLetter=sortByLetter())
  })

  # The numerical input for the thresholds will be rounded to intergers.
  observeEvent(input$ch1Threshold, {
    thresholds$ch1 <- round(input$ch1Threshold)
  })
  observeEvent(input$ch2Threshold, {
    thresholds$ch2 <- round(input$ch2Threshold)
  })

  # Mouse clicks.
  observeEvent(input$plot_click, {
    # Round to integers... surely these don't need to be floats.
    ch1 <- round(input$plot_click$y)
    ch2 <- round(input$plot_click$x)

    # Setting thresholds with mouse clicks.
    if(clMode() == "thresholds") {
      # Update the UI.
      updateNumericInput(session, "ch1Threshold", value=ch1)
      updateNumericInput(session, "ch2Threshold", value=ch2)

      # Set the internal variables.
      thresholds$ch1 <- ch1
      thresholds$ch2 <- ch2
    } else if(clMode() == "grid") {
      gridThresholds[[selectedClass$value]]$ch1 <- ch1
      gridThresholds[[selectedClass$value]]$ch2 <- ch2
    } else if(clMode() == "kmeans" && !input$removeThisClass) {
      # Setting cluster centres.
      initialCentres[[selectedClass$value]] <-
        list(val=c(ch1, ch2), removed=FALSE)
    }
  })

  # Retrieve the initial centres set in the app.
  startingCentres <- function() {
    # Remove any classes that are marked as being 'removed'.
    centres <- lapply(Filter(Negate(function(x) { x$removed }),
                             reactiveValuesToList(initialCentres)),
                      function(x) { x$val })

    # Turn the centres list into a data frame.
    centresDf <- as.data.frame(t(as.data.frame(centres)))
    colnames(centresDf) <- c("Ch1.Amplitude", "Ch2.Amplitude")
    centresDf
  }

  # Retrieve a data frame of grid thresholds.
  getGridThresholds <- function() {
    data.frame(
      "Ch1.Amplitude"=c(gridThresholds$NN$ch1,
                        gridThresholds$NP$ch1,
                        gridThresholds$PN$ch1,
                        gridThresholds$PP$ch1),
      "Ch2.Amplitude"=c(gridThresholds$NN$ch2,
                        gridThresholds$NP$ch2,
                        gridThresholds$PN$ch2,
                        gridThresholds$PP$ch2),
      "class"=c(ddpcr$nn, ddpcr$np, ddpcr$pn, ddpcr$pp)
    )
  }

  # "Run classification" listener.
  observeEvent(input$classify, {
    tryCatch({
      if(isEmpty(wells$selected)) {
        messages$error <- "No wells selected for classification."
        updateNavbarPage(session, "tdNavbarPage", selected="Results")
      } else {
        if(clMode() == "thresholds") {
          results$all <- thresholdClassify(wells$selected,
                                                 ch1Threshold=thresholds$ch1,
                                                 ch2Threshold=thresholds$ch2)
          results$ch1Threshold <- thresholds$ch1
          results$ch2Threshold <- thresholds$ch2
          messages$error <- ""
        } else if(clMode() == "kmeans") {
          withProgress(message="Running k-means algorithm", value=0, {
              results$all <-
                kmeansClassify(wells$selected, centres=startingCentres())
              messages$error <- ""
            incProgress(1)
          })
        } else if(clMode() == "grid") {
          results$all <-
            gridClassify(wells$selected,
                         gridThresholds$NN$ch1, gridThresholds$NN$ch2,
                         gridThresholds$NP$ch1, gridThresholds$NP$ch2,
                         gridThresholds$PN$ch1, gridThresholds$PN$ch2,
                         gridThresholds$PP$ch1, gridThresholds$PP$ch2,
                         classMethodLabel="grid",
                         naLabel=ddpcr$rain)
          messages$error <- ""
        } else if(clMode() == "knn") {
          withProgress(message="Running k-NN algorithm",
                       detail="This can take a while...", value=0, {
            if(nrow(trainingData$drops) == 0) {
              setTrainingDataFromGrid()
            }

            results$all <-
              knnClassify(wells$selected,
                trainingData$drops[, c("Ch1.Amplitude", "Ch2.Amplitude")],
                trainingData$drops$class, k=input$knnK, prob=input$knnProb)

            messages$error <- ""
            incProgress(1)
          })
        }

        # Set the results modes.
        setResultsWells()
        results$mode <- clMode()
        results$baseMode <- clMode()

        # Switch to the results tab.
        setSummary(clMode(), sortByLetter=sortByLetter())
        updateRadioButtons(session, "resultsViewMode", selected="All")
        updateRadioButtons(session, "rainType", selected="No Rain")

        updateClassifications(selected=input$classifyMode)
      }
    },
    warning=function(e) {
      messages$error <- as.character(e) ######## protect
    },
    error=function(e) {
      messages$error <- as.character(e) ######## protect
    })
    updateNavbarPage(session, "tdNavbarPage", selected="Results")
  })

  # Show the Mahalanobis rain sliders with correct labels.
  output$mvnRainNNPH <- renderUI({
    label <- abbrevLabels()
    sliderInput("mvnRainNN", label[1], 1, 100, 30)
  })
  output$mvnRainNPPH <- renderUI({
    label <- abbrevLabels()
    sliderInput("mvnRainNP", label[2], 1, 100, 30)
  })
  output$mvnRainPNPH <- renderUI({
    label <- abbrevLabels()
    sliderInput("mvnRainPN", label[3], 1, 100, 30)
  })
  output$mvnRainPPPH <- renderUI({
    label <- abbrevLabels()
    sliderInput("mvnRainPP", label[4], 1, 100, 30)
  })

  # Show the SD rain sliders with correct labels.
  output$sdRainNNPH <- renderUI({
    label <- abbrevLabels()
    sliderInput("sdRainNN", label[1], 1, 100, 30)
  })
  output$sdRainNPPH <- renderUI({
    label <- abbrevLabels()
    sliderInput("sdRainNP", label[2], 1, 100, 30)
  })
  output$sdRainPNPH <- renderUI({
    label <- abbrevLabels()
    sliderInput("sdRainPN", label[3], 1, 100, 30)
  })
  output$sdRainPPPH <- renderUI({
    label <- abbrevLabels()
    sliderInput("sdRainPP", label[4], 1, 100, 30)
  })

  # Toggle rain: this will change the mode; update the summary.
  observeEvent(input$rainType, {
    # Base mode with and without rain.
    bm <- results$baseMode
    bmMahRain <- paste0(bm, "MahRain")
    bmSdRain <- paste0(bm, "SdRain")

    if(input$rainType == "No Rain") {
      results$mode <- bm
      setSummary(results$mode, sortByLetter=sortByLetter())
    } else if(input$rainType == "Mahalanobis" &&
            bmMahRain %in% commonClassificationMethod(results$all)) {
      results$mode <- bmMahRain
      setSummary(results$mode, sortByLetter=sortByLetter())
    } else if(input$rainType == "Standard Deviation" &&
            bmSdRain %in% commonClassificationMethod(results$all)) {
      results$mode <- bmSdRain
      setSummary(results$mode, sortByLetter=sortByLetter())
    }
  })

  # Add rain.
  observeEvent(input$makeItRain, {
    withProgress(message="Computing rain",
                 detail="This can take a moment...", value=0, {
      if(input$rainType == "Mahalanobis") {
        results$all <-
          mahalanobisRain(
            results$all,
            cMethod=results$baseMode,
            maxDistances=list("NN"=input$mvnRainNN, "NP"=input$mvnRainNP,
                              "PN"=input$mvnRainPN, "PP"=input$mvnRainPP))

        # Set the mode.
        results$mode <- paste0(results$baseMode, "MahRain")
      } else if(input$rainType == "Standard Deviation") {
        results$all <-
          sdRain(
            results$all,
            cMethod=results$baseMode,
            errorLevel=list("NN"=input$sdRainNN, "NP"=input$sdRainNP,
                            "PN"=input$sdRainPN, "PP"=input$sdRainPP))

        # Set the mode.
        results$mode <- paste0(results$baseMode, "SdRain")
      }

      setResultsWells()
      setSummary(results$mode, sortByLetter=sortByLetter())
    })
  })

  # Render a facet_grid of all the wells.
  output$plotFacetGrid <- renderPlot({
    validate(
      need(!is.null(wells$all) && !isEmpty(wells$all),
           "Please load a dataset on the 'Input' tab.")
    )

    withProgress(message="Rendering the plot",
        detail="This can take a moment...", value=0, {
      if(input$plateViewMode == "Show All Wells") {
        showAllWells <- TRUE
      } else {
        showAllWells <- FALSE
      }

      return(facetPlot(wells$all,
                       ch1Label=chLabels$ch1,
                       ch2Label=chLabels$ch2,
                       binwidth=input$heatWidth,
                       plotLimits=globals$plotLimits,
                       showEmptyWells=showAllWells))
      incProgress(1)
    })
  })


  # Render density plot of selected wells along with UI elements for helping
  # with the selected classification mode.
  output$plotSelectedWells <- renderPlot({
    validate(
      need(!is.null(wells$selected) && !isEmpty(wells$selected),
           "Please choose some wells in the 'Select Wells' tab.")
    )

    withProgress(message="Rendering the plot",
        detail="This can take a moment...", value=0, {
      p <- heatPlot(wells$selected,
                    ch1Label=chLabels$ch1, ch2Label=chLabels$ch2,
                    binwidth=input$heatWidth,
                    plotLimits=globals$plotLimits)

      # Draw thresholds.
      if(clMode() == "thresholds") {
        p <- p + geom_hline(yintercept=thresholds$ch1) +
          geom_vline(xintercept=thresholds$ch2)
      } else if(clMode() == "grid") {
        # Draw the regions that are cut out for the grid.
        # Get a data frame of the grid thresholds.
        df <- getGridThresholds()
        ch1 <- substr(df$class, 1, 1)
        ch2 <- substr(df$class, 2, 2)

        # Highlight the regions to keep.
        p <-
          p + geom_rect(data=df[df$class == "NN", ],
                        aes_string(xmin=-Inf, xmax="Ch2.Amplitude",
                                   ymin=-Inf, ymax="Ch1.Amplitude"),
                        fill="green", colour="green4", alpha=0.2) +
          geom_rect(data=df[df$class == "NP", ],
                    aes_string(xmin="Ch2.Amplitude", xmax=Inf,
                               ymin=-Inf, ymax="Ch1.Amplitude"),
                    fill="green", colour="green4", alpha=0.2) +
          geom_rect(data=df[df$class == "PN", ],
                    aes_string(xmin=-Inf, xmax="Ch2.Amplitude",
                               ymin="Ch1.Amplitude", ymax=Inf),
                    fill="green", colour="green4", alpha=0.2) +
          geom_rect(data=df[df$class == "PP", ],
                    aes_string(xmin="Ch2.Amplitude", xmax=Inf,
                               ymin="Ch1.Amplitude", ymax=Inf),
                    fill="green", colour="green4", alpha=0.2)
      }

      # Draw initial centres.
      if(clMode() == "kmeans") {
        centres <- startingCentres()
        selectedCentre <- selectedClass$value
        p <- p +
          geom_point(data=centres[rownames(centres) == selectedCentre, ],
                     aes_string(x="Ch2.Amplitude", y="Ch1.Amplitude"),
                     fill="#FFFFFF", colour="#000000", shape=23, size=6) +
          geom_point(data=centres[rownames(centres) != selectedCentre, ],
                     aes_string(x="Ch2.Amplitude", y="Ch1.Amplitude"),
                     fill="#000000", colour="#FFFFFF", shape=23, size=6)
      }
      return(p)
      incProgress(1)
    })
  })


  # Plot the classification.
  output$plotClassification <- renderPlot({
    validate(
      need(!is.null(results$all) && !isEmpty(results$all),
           "Please classify droplets in the 'Classify' tab."),
      need(messages$error == "", messages$error)
    )

    withProgress(message="Rendering the plot",
        detail="This can take a moment...", value=0, {
      # Re-label the legend.
      labels <- c(abbrevLabels(), "Rain", "N/A")

      p <- dropletPlot(results$selected,
                       ch1Label=chLabels$ch1,
                       ch2Label=chLabels$ch2,
                       cMethod=results$mode,
                       plotLimits=globals$plotLimits,
                       legendLabels=labels)

      # Retrieve thresholds and plot them.
      if(!is.null(results$baseMode) &&
         results$baseMode == "thresholds" &&
         !is.null(results$ch1Threshold) &&
         !is.null(results$ch2Threshold) &&
         input$showThresholds) {
        p <- p + geom_hline(yintercept=results$ch1Threshold) +
          geom_vline(xintercept=results$ch2Threshold)
      }

      # Plot the final centres.
      if(input$finalCentres) {
        finalCentres <- combinedCentres(results$all, results$mode)
        p <- p + geom_point(data=finalCentres, fill="#000000",
                            colour="#FFFFFF", shape=22, size=6)
      }
      return(p)
      incProgress(1)
    })
  })

  # Plot the training data.
  output$plotTraining <- renderPlot({
    validate(
      need(nrow(trainingData$drops) > 0, "No training data has been set.")
    )
    withProgress(message="Rendering the plot",
        detail="This can take a moment...", value=0, {
      labels <- c(abbrevLabels(), "Rain", "N/A")
      p <- dropletPlot(trainingData$drops,
                       ch1Label=chLabels$ch1,
                       ch2Label=chLabels$ch2,
                       cMethod="class",
                       plotLimits=globals$plotLimits,
                       legendLabels=labels)
      return(p)
      incProgress(1)
    })
  })

  # Use the current results as training data.
  observeEvent(input$setClassificationAsTrainingData, {
    if(!is.null(results$all) &&
       sum(numDroplets(results$all)) > 0) {
      trainingData$drops <-
        do.call(rbind, removeDropletClasses(results$all, cMethod=results$mode))
      names(trainingData$drops)[3] <- "class"
    }
  })

  # Show Bio-Rad-like table.
  output$wellsSummary <- renderDataTable({
    validate(
      need(!is.null(results$all) && !isEmpty(results$all) &&
           messages$error == "",
           "Please classify droplets in the 'Classify' tab.")
    )
    if(!is.null(results$summary)) {
      results$summary
    } else {
      data.frame()
    }
  })

  # Show the export amplitudes button when there are some results.
  output$exportAmplitudesPH <- renderUI({
    if(!is.null(results$all) && !is.null(results$summary)) {
      div(
        style="padding-top: 2ex",
        strong("Droplet export"),
        div(
          style="padding-top: 1ex",
          downloadButton("exportAmplitudesButton", "Export Amplitudes to File")
        ),
        div(
          style="padding-top: 2ex",
          actionButton("setClassificationAsTrainingData",
            "Set as Training Data"
          )
        )
      )
    }
  })

  # Export the amplitudes to file.
  output$exportAmplitudesButton <- downloadHandler(
    filename=function() { paste0(wells$plateName, "_", results$mode,
                                 "-amplitudes.zip") },
    content=function(file) {
      exportZip(results$all, file, prefix=paste0(wells$plateName, "_"))
    },
    contentType="application/zip"
  )

  # A summary table has been created.
  output$summarySet <- reactive({
    return(!is.null(results$all) && !is.null(results$summary))
  })
  outputOptions(output, "summarySet", suspendWhenHidden=FALSE)

  # Export the data summary to file.
  output$exportSummaryButton <- downloadHandler(
    filename=function() {
      paste0(wells$plateName, "-", results$mode, ".csv")
    },
    content=function(file) {
      exportTable(results$summary[, -1], file, delim=",",
                  leadingColName="Well")
    }
  )

  # Export the data summary to file.
  output$generateHtmlReport <- downloadHandler(
    filename=function() {
      paste0(wells$plateName, "-", results$mode, "-report.html")
    },
    content=function(file) {
      # Copy the report file to a temporary directory before processing it.
      tempReport <- file.path(normalizePath(tempdir()), "report-html.Rmd")
      rmdLoc <- system.file("rmd", "report-html.Rmd", package = "twoddpcr")
      file.copy(rmdLoc, tempReport, overwrite=TRUE)

      # Check the rain type of the current mode.
      if(length(grep("MahRain", results$mode)) > 0) {
        rainType <- "Mahalanobis"
      } else if(length(grep("SdRain", results$mode)) > 0) {
        rainType <- "Standard deviation"
      } else {
        rainType <- "None"
      }

      # Set up parameters to pass to the Rmd document.
      params <- list(plateName=wells$plateName,
                     plate=results$all,
                     cMethod=results$mode,
                     longMode=longMode(results$baseMode),
                     rainType=rainType,
                     summaryTable=results$summary[, -1])


      rmarkdown::render(tempReport, output_file=file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )

  # Show the initial centres used in k-means clustering.
  output$centresOutput <- renderPrint({
    sc <- startingCentres()

    # Add a class column
    labels <- abbrevLabels()
    lab <- vapply(rownames(sc),
           function(x) {
             if(x == "NN") {
               "NN"=labels[1]
             } else if(x == "NP") {
               "NP"=labels[2]
             } else if(x == "PN") {
               "PN"=labels[3]
             } else if(x == "PP") {
               "PP"=labels[4]
             }
           }, character(1))
    cbind("class"=lab, sc)
  })

  output$gridThesholdsOutput <- renderPrint({
    getGridThresholds()
  })

  # Render the droplet volume action button.
  output$setDropletVolumePH <- renderUI({
    validate(
      need(is.numeric(input$dropletVolume) && input$dropletVolume > 0,
           "The droplet volume should be a positive number.")
    )
    div(
      style="padding-bottom: 2ex",
      actionButton("setDropletVolume", "Set Droplet Volume")
    )
  })

  # Set the droplet volume.
  observeEvent(input$setDropletVolume, {
    setDropletVolume(volume=(input$dropletVolume * 0.001))
    setSummary(results$mode, sortByLetter=sortByLetter())
  })

  # Render the minimum cluster separation action button.
  output$setMinSeparationPH <- renderUI({
    validate(
      need(is.numeric(input$minSeparation) && input$minSeparation > 0,
           "The minimum cluster separation should be a positive number.")
    )
    div(
      style="padding-bottom: 2ex",
      actionButton("setMinSeparation", "Set Minimum Cluster Separation")
    )
  })

  # Set the minimum cluster separation.
  observeEvent(input$setMinSeparation, {
    globals$minSeparation <- input$minSeparation
    setSelectedWells()
  })

  # Render the plot limit action button.
  output$setPlotLimitsPH <- renderUI({
    validate(
      need(is.numeric(input$ch1min) && is.numeric(input$ch1max) &&
           is.numeric(input$ch2min) && is.numeric(input$ch2max),
           "The plot limit figures should be numbers."),
      need(input$ch1min < input$ch1max && input$ch2min < input$ch2max,
           "The min figures should be less than its corresponding max figure.")
    )
    div(
      style="padding-bottom: 2ex",
      actionButton("setPlotLimits", "Set Plot Limits")
    )
  })

  # Set the plot limits.
  observeEvent(input$setPlotLimits, {
    globals$plotLimits <-
      list(x=c(input$ch2min, input$ch2max), y=c(input$ch1min, input$ch1max))
  })
}
