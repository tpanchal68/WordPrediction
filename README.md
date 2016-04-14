---
title: "WordPrediction application"
subtitle: "Coursera Data Science Capstone Project"
author: "Tejash Panchal"
date: "April 19, 2016"
output: html_document
---
Licence: Freeware (Please see LICENSE.md file for terms and conditions)

- ui.R
This is the user-interface definition of a Shiny web application.
It allows user to select algorithm and type sentence.  Based on both of these inputs,
application will predict next word.

- server.R
This is the server side of a Shiny web application.
First it cleans user input texts and converts to lower case.
As user types, it examins each characters and provides hints for the word being typed.
If it detects an empty space, the application sends last two typed words to
appropriate function based on chosen algorithm to get next word prediction.
Prediction function performs probability calculations and returns top 5 predictions.
Server side passes those predictions to UI side and displays in table format.

- global.R
Loads all ngram files for application use.

- prediction.R
Prediction functions for each supported algorithms.

- create_ngrams.R
Reads first 40k lines from each, blog, news, and twitter, files cleans the texts and generates unigrams, bigrams, and trigrams.

# Known bugs
- Need to add exception handling
- No function to handle error if no prediction is found.
- Need to add handlers for when user completes the sentence with ".", ";", or "?"
- Noticed some issues with Apostrophe words handling.


