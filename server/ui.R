library(shiny)

shinyUI(navbarPage(
  id = 'navbar',
  title = 'EEG RTool',
  tabPanel('Load Data',
           titlePanel('Load Data'),
           loadDataUI('loadData')
  ),
  tabPanel('Select Data',
          titlePanel('Select Data'),
          selectDataUI('selectData')
  ),
  tabPanel('Display Signals',
          titlePanel('Display Signals'),
          fluidPage(
              displaySignalsUI('displaySignals'),
              fluidRow(
                uiOutput('plotSignals')
              )
          )
  ),
  tabPanel('Signal Filtering',
          titlePanel('Signal Filtering'),
          fluidPage(
              signalFilteringUI('signalFiltering'),
              fluidRow(
                uiOutput('FFTPlotSignals')
              )
          )
  ),
  tabPanel('Ocular Correction',
           titlePanel('Ocular Correction'),
           ocularCorrectionUI('ocularCorrection'),
           fluidPage(
             fluidRow(
               uiOutput('correctedEEGPlots')
             )
           )
  ),
  navbarMenu('Bands Segmentation',
           tabPanel('Calculate Bands Segmentation',
              titlePanel('Calculate Bands Segmentation'),
              calculateBandsSegmentationUI('calculateBandsSegmentation')
           ),
           tabPanel('Display GPS Signal',
              titlePanel('Display GPS Signal'),
              fluidPage(
                displayGPSSignalUI('displayGPSSignal'),
                fluidRow(
                  uiOutput('gpsPlot')
                )
              )
           ),
           tabPanel('Display Bands Filtering',
              titlePanel('Display Bands Filtering'),
              displayBandsFilteringUI('displayBandsFiltering'),
              fluidPage(
                fluidRow(
                  uiOutput('bandsPlots')
                )
              )
           )
  ),
  navbarMenu('Indices Calculation',
          tabPanel('Calculate Indices',
              titlePanel('Calculate Indices'),
              calculateIndicesUI('calculateIndices')
          ),
          tabPanel('Display Values of Reference',
              titlePanel('Display Values of Reference'),
              displayValuesOfReferencesUI('displayValuesOfReferences'),
              fluidPage(
                fluidRow(
                  uiOutput('valuesPlots')
                )
              )
          ),
          tabPanel('Display Indices',
              titlePanel('Display Indices'),
              displayIndicesUI('displayIndices'),
              fluidPage(
                fluidRow(
                  uiOutput('indicesPlots')
                )
              )
          )
  ),
  tabPanel('Execute All Process',
           titlePanel('Execute All Process'),
           executeAllProcessUI('executeAllProcess')
  ),
  navbarMenu('Export Data',
            tabPanel('Export Indices',
               titlePanel('Export Indices'),
               exportIndicesUI('downloadIndices')
            ), 
            tabPanel('Export Signals',
               titlePanel('Export Signals'),
               exportSignalsUI('downloadSignals')
            )
  )
  
))