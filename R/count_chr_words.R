#' Counts words and characters in a string
#'
#' @description This function counts the number of words
#' and characters in a given string. The user can decide
#' weather to count spaces as characters or not.
#'
#' @param text The string whose words and characters will be counted.
#' Parameter is called text since it makes intuitive sense to the user (they are inputting written text).
#' @param count_spaces Weather spaces in the string should be counted as characters **(TRUE or FALSE)**.
#' count_spaces is pretty self explanatory. We are asking the user if they
#' should count spaces as character or not.
#'
#' @return A numeric vector whose first element
#' is the number of words in the string and second element is number
#' of characters in the string. `c(words, characters)`
#'
#' @examples
#' count_chr_words("hello world", count_spaces = TRUE)
#' count_chr_words("AATTCCTTTCGGTG", count_spaces = FALSE)
#'
#' @export

count_chr_words <- function(text, count_spaces){

  if(!is.character(text) || length(text) > 1){
    stop('You have provided a non-character input or a vector.\n',
         'Your input is of class: ', class(text)[1])
  }
  if(!is.logical(count_spaces)){
    stop('You have provided a non-logical input.\n',
         'Your input is of class: ', class(count_spaces)[1])
  }

  if(count_spaces == TRUE){
    char <- strsplit(text, split = "")[[1]] %>% length()
    words <- strsplit(text, split = " ")[[1]] %>% length()
  }

  if(count_spaces == FALSE){
    char <- strsplit(text, split = "")[[1]]
    char <- char[char != " "] %>% length()
    words <- strsplit(text, split = " ")[[1]] %>% length()
  }
  print(paste("Number of words in string:", words))
  print(paste("Number of characters in string:", char))

  return(c(words, char))
}
