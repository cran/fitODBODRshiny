#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import ggplot2
#' @importFrom shinydashboard box
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(theme = bslib::bs_theme(bootswatch = "lumen"),
      bslib::page_navbar(
        selected = strong("Alternate Binomial Distributions"),
        window_title = "fitODBOD : Rshiny",
        fillable = TRUE,
        bslib::nav_panel(
          title = strong("Alternate Binomial Distributions"),
          gridlayout::grid_container(
            layout = c(
              "Dataset AddBinDist  BetaCorrBinDist COMPBinDist    ",
              "BinDist CorrBinDist MultiBinDist    LovMultiBinDist"
            ),
            row_sizes = c("475px","475px"),
            col_sizes = c("1fr","1fr","1fr","1fr"),
            gap_size = "1px",
            gridlayout::grid_card(
              area = "Dataset",
              card_header(strong("Fitting Binomial Outcome Data:")),
              card_body(
                max_height = "450px",
                gap = "1px",
                strong("1) Select the Binomial Outcome Data."),
                strong("2) Fit the Binomial distribution."),
                strong("3) Fit the Alternate Binomial Distributions."),
                strong("4) Fit the Binomial Mixture Distributions."),
                strong("5) Summarize the results with tables.")
                ),
              card_footer(
                radioButtons(
                  inputId = "Datasets",
                  label = "Data sets: ",
                  choices = list(
                    "Alcohol Data Week 1" = "ADW1",
                    "Alcohol Data Week 2" = "ADW2",
                    "Course Data" = "CD",
                    "Exam Data" = "ED",
                    "Plant Disease Incidence Data" = "PDID",
                    "Terror Data Argentina" = "TDArg",
                    "Terror Data USA" = "TDUSA"
                  ),
                  width = "100%"
                )
              )
            ),
            gridlayout::grid_card(
              area = "COMPBinDist",
              card_header(strong("COM Poisson Binomial Distribution")),
              card_body(
                gap = "1px",
                tabsetPanel(type="pills",
                  bslib::nav_panel(
                    title = "Frequencies",
                    plotOutput(outputId = "COMPBinFreqPlot", height = "345px")
                  ),
                  bslib::nav_panel(
                    title = "Parameters",
                    plotOutput(outputId = "COMPBinParPlot", height = "345px")
                  )
                )
              )
            ),
            gridlayout::grid_card(
              area = "CorrBinDist",
              card_header(strong("Correlated Binomial Distribution")),
              card_body(
                gap = "1px",
                tabsetPanel(type="pills",
                  bslib::nav_panel(
                    title = "Frequencies",
                    plotOutput(outputId = "CorrBinDistFreqPlot",height = "345px")
                  ),
                  bslib::nav_panel(
                    title = "Parameters",
                    plotOutput(outputId = "CorrBinDistParPlot",height = "345px")
                  )
                )
              )
            ),
            gridlayout::grid_card(
              area = "MultiBinDist",
              card_header(strong("Multiplicative Binomial Distribution")),
              card_body(
                gap = "1px",
                tabsetPanel(type="pills",
                  bslib::nav_panel(
                    title = "Frequencies",
                    plotOutput(outputId = "MultiBinDistFreqPlot",height = "345px")
                  ),
                  bslib::nav_panel(
                    title = "Parameters",
                    plotOutput(outputId = "MultiBinDistParPlot",height = "345px")
                  )
                )
              )
            ),
            gridlayout::grid_card(
              area = "LovMultiBinDist",
              card_header(strong("Lovinson Multiplicative Binomial Distribution")),
              card_body(
                gap = "1px",
                tabsetPanel(type="pills",
                  bslib::nav_panel(
                    title = "Frequencies",
                    plotOutput(outputId = "LovMultiBinDistFreqPlot",height = "345px")
                  ),
                  bslib::nav_panel(
                    title = "Parameters",
                    plotOutput(outputId = "LovMultiBinDistParPlot",height = "345px")
                  )
                )
              )
            ),
            gridlayout::grid_card(
              area = "AddBinDist",
              card_header(strong("Additive Binomial Distribution")),
              card_body(
                gap = "1px",
                plotOutput(outputId = "AddBinDistPlot", height = "345px")
              )
            ),
            gridlayout::grid_card(
              area = "BetaCorrBinDist",
              card_header(strong("Beta-Correlated Binomial Distribution")),
              card_body(
                gap = "1px",
                tabsetPanel(type="pills",
                  bslib::nav_panel(
                    title = "Frequencies",
                    plotOutput(outputId = "BetaCorrBinFreqPlot",height = "345px")
                  ),
                  bslib::nav_panel(
                    title = "Parameters",
                    plotOutput(outputId = "BetaCorrBinParPlot",height = "345px")
                  )
                )
              )
            ),
            gridlayout::grid_card(
              area = "BinDist",
              card_header(strong("Binomial Distribution")),
              card_body(
                gap = "1px",
                plotOutput(outputId = "BinPlot", height = "345px")
              )
            )
          )
        ),
        nav_panel(
          title = strong("Binomial Mixture Distributions"),
          gridlayout::grid_container(
            layout = c(
              "BinDistPlot  TriBinPlot     BetaBinPlot KumBinPlot",
              "GammaBinPlot GrassiaBinPlot GHGBBPlot   McGBBPlot "
            ),
            row_sizes = c("475px","475px"),
            col_sizes = c("1fr","1fr","1fr","1fr"),
            gap_size = "1px",
            gridlayout::grid_card(
              area = "TriBinPlot",
              card_header(strong("Triangular Binomial Distribution")),
              card_body(
                gap = "1px",
                plotOutput(outputId = "TriBinPlot")
              )
            ),
            gridlayout::grid_card(
              area = "BetaBinPlot",
              card_header(strong("Beta-Binomial Distribution")),
              card_body(
                gap = "1px",
                tabsetPanel(type="pills",
                  bslib::nav_panel(
                    title = "Frequencies",
                    plotOutput(outputId = "BetaBinFreqPlot", height = "345px")
                  ),
                  bslib::nav_panel(
                    title = "Parameters",
                    plotOutput(outputId = "BetaBinParaPlot", height = "345px")
                  )
                )
              )
            ),
            gridlayout::grid_card(
              area = "KumBinPlot",
              card_header(strong("Kumaraswamy Binomial Distribution")),
              card_body(
                gap = "1px",
                tabsetPanel(type="pills",
                  bslib::nav_panel(
                    title = "Frequencies",
                    plotOutput(outputId = "KumBinFreqPlot", height = "345px")
                  ),
                  bslib::nav_panel(
                    title = "Parameters",
                    plotOutput(outputId = "KumBinParaPlot", height = "345px")
                  )
                )
              )
            ),
            gridlayout::grid_card(
              area = "GammaBinPlot",
              card_header(strong("Gamma Binomial Distribution")),
              card_body(
                gap = "1px",
                tabsetPanel(type="pills",
                  bslib::nav_panel(
                    title = "Frequencies",
                    plotOutput(outputId = "GamBinFreqPlot", height = "345px")
                  ),
                  bslib::nav_panel(
                    title = "Parameters",
                    plotOutput(outputId = "GamBinParaPlot", height = "345px")
                  )
                )
              )
            ),
            gridlayout::grid_card(
              area = "GrassiaBinPlot",
              card_header(strong("Grassia II Binomial Distribution")),
              card_body(
                gap = "1px",
                tabsetPanel(type="pills",
                  bslib::nav_panel(
                    title = "Frequencies",
                    plotOutput(outputId = "GraBinFreqPlot", height = "345px")
                  ),
                  bslib::nav_panel(
                    title = "Parameters",
                    plotOutput(outputId = "GraBinParaPlot", height = "345px")
                  )
                )
              )
            ),
            gridlayout::grid_card(
              area = "GHGBBPlot",
              card_header(strong("GHGBeta-Binomial Distribution")),
              card_body(
                gap = "1px",
                tabsetPanel(type="pills",
                  bslib::nav_panel(
                    title = "Frequencies",
                    plotOutput(outputId = "GHGBBFreqPlot", height = "345px")
                  ),
                  bslib::nav_panel(
                    title = "Parameters",
                    plotOutput(outputId = "GHGBBParaPlot", height = "345px")
                  )
                )
              )
            ),
            gridlayout::grid_card(
              area = "McGBBPlot",
              card_header(strong("McDonald Generalized Beta Binomial Distribution")),
              card_body(
                gap = "1px",
                tabsetPanel(type="pills",
                  bslib::nav_panel(
                    title = "Frequencies",
                    plotOutput(outputId = "McGBBFreqPlot", height = "345px")
                  ),
                  bslib::nav_panel(
                    title = "Parameters",
                    plotOutput(outputId = "McGBBParaPlot", height = "345px")
                  )
                )
              )
            ),
            gridlayout::grid_card(
              area = "BinDistPlot",
              card_header(strong("Binomial Distribution")),
              card_body(gap = "1px", plotOutput(outputId = "BinPlot1"))
            )
          )
        ),
        bslib::nav_panel(
          title = strong("Table for Alternate Binomial Distributions"),
          uiOutput("ABD_Table_Plot"),
          actionButton("download_ABD", "Download ABD Summary"),
          shinydashboard::box(title = "Conclusion of Results",uiOutput("Text_ABD"),
                              width = 12,status = "primary",solidHeader = TRUE)
        ),
        bslib::nav_panel(
          title = strong("Table for Binomial Mixture Distributions"),
          uiOutput("BMD_Table_Plot"),
          actionButton("download_BMD", "Download BMD Summary"),
          shinydashboard::box(title = "Conclusion of Results",uiOutput("Text_BMD"),
                              width = 12,status = "primary",solidHeader = TRUE)
        ),
        bslib::nav_panel(title=HTML("</a></li><li><a href='http://www.amalan-mahendran.com/' target='_blank'>About Me"))
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",app_sys("app/www")#,package = "fitODBODRshiny")
  )

  tags$head(
    favicon(ext="png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "fitODBODRshiny"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
