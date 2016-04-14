# Author: Tejash Panchal
# Word Prediction
# Coursera Data Science Capstone Project
# Licence: Freeware (Please see LICENSE.md file for terms and condition)
#
# This is the user-interface definition of a Shiny web application.
# It allows user to select algorithm and type sentence.  Based on both of these inputs,
# application will predict next word.

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
                        ),
                        tabPanel(
                                p(icon("list-alt"), "License"),
                                includeMarkdown("LICENSE.md")
                        ),
                        tabPanel(
                                p(icon("list-alt"), "README"),
                                includeMarkdown("README.md")
                        )
                )
        )
))

