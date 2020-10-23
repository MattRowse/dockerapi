# make the model
source("make_model.r")
library(dplyr)
library(tidytext)
library(stringr)
library(plumber)
library(httr)

#* Log some information about the incoming request
#* @filter logger
function(req){
  cat(as.character(Sys.time()), "-",req$QUERY_STRING,
      req$REQUEST_METHOD, req$PATH_INFO, "-",
      req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

#' return challenge response
#' @serializer contentType list(type="text/plain")
#' @param challenge
#' @post /challenge
#' @response 200
function(challenge=" ") {
  challenged  <<- challenge
  return(challenged)
}

#' return chatbot inventory response
#' @serializer contentType list(type="text/plain")
#' @param text the message used for analysis
#' @post /inventory
#' @response 200
function(text=" ") {

  # turn parsed message into tidy dataframe
  msg_df <- tibble(line = 1:1, text = text)
  
  # unnest words in dataframe and create response
  msg_df <- msg_df %>% unnest_tokens(word, text)
  body <- inventorypred(msg_df)
  my_message <- paste0(body)
  return(my_message)
}

#' return chatbot ebay response
#' @serializer contentType list(type="text/plain")
#' @param text the message used for analysis
#' @post /ebay
#' @response 200
function(text=" ") {
  
  # turn parsed message into tidy dataframe
  msg_df <- tibble(line = 1:1, text = text)
  
  # unnest words in dataframe and create response
  msg_df <- msg_df %>% unnest_tokens(word, text)
  body <- ebaypred(msg_df)
  my_message <- paste0(body)
  return(my_message)
}

#' return chatbot design response
#' @serializer contentType list(type="text/plain")
#' @param text the message used for analysis
#' @post /design
#' @response 200
function(text=" ") {
  
  # turn parsed message into tidy dataframe
  msg_df <- tibble(line = 1:1, text = text)
  
  # unnest words in dataframe and create response
  msg_df <- msg_df %>% unnest_tokens(word, text)
  body <- designpred(msg_df)
  my_message <- paste0(body)
  return(my_message)
}

#' return chatbot payments response
#' @serializer contentType list(type="text/plain")
#' @param text the message used for analysis
#' @post /payments
#' @response 200
function(text=" ") {
  
  # turn parsed message into tidy dataframe
  msg_df <- tibble(line = 1:1, text = text)
  
  # unnest words in dataframe and create response
  msg_df <- msg_df %>% unnest_tokens(word, text)
  body <- paymentspred(msg_df)
  my_message <- paste0(body)
  return(my_message) 
}