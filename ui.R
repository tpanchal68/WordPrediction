# This is the user-interface definition of a Shiny web application.
#

shinyUI(pageWithSidebar(
        headerPanel("Word Prediction"),
        sidebarPanel(
                h4("User Input Area", align="center"),
                radioButtons(
                        "smoothing",
                        label = h3("Smoothing Selection"),
                        choices = list(
                                "Maximum Likelyhood Estimates" = "MLE",
                                "Laplace" = "Laplace",
                                "Good Turing" = "GoodTuring",
                                "Simple Interpolation" = "Interpolation"
                        ),
                        selected = "GoodTuring"
                ),
                br(),
                p("Please enter text in input box below."),
                p("After typing a word, press spacebar to get next word prediction."),
                textInput("inText", label = h3("Text Input:"), value = "Type here"),
                br(),
                actionButton("goButton", "Clear!"),
                p("Click the \"Clear!\" button to clear the text input line.")
        ),
        mainPanel(
                h4("Next Word Suggestion", align="center"),
                br(),
                tabsetPanel(
                        tabPanel(
                                p(icon("table"), "Next Word"),
                                dataTableOutput("wordHint")
                        ),
                        tabPanel(
                                p(icon("list-alt"), "Help"),
                                includeMarkdown("help.md")
                        )
                )
        )
))

