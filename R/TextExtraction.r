#' @title extractTextInfo
#' @description Given a vector of text, detect sentence-wise entity pairings.
#' @details Given a vector of text, detect sentence-wise entity pairings.  Works even if senetences are broken up over multiple elements of the vector of text.
#' @aliases extractTextInfo
#' @export extractTextInfo
#' @author Jared P. Lander
#' @importFrom magrittr "%>%"
#' @importFrom  dplyr n data_frame group_by do mutate
#' @importFrom useful "reclass<-"
#' @param text A vector of text
#' @param nerModel A ner model supplied by MITIE
#' @return A tbl of entity pairs demarcated by the sentence they are paired in.  Could result in multiple combinations.
#' @examples 
#' \dontrun{
#' ner_model_path <- "mitie/MITIE-models/english/ner_model.dat"
#' ner <- NamedEntityExtractor$new(ner_model_path)
#' textFiles <- file.path('data', 'NYTimes', dir('data/NYTimes/'))
#' thisText <- readLines(textFiles[1])
#' extractTextInfo(thisText, nerModel=ner)
#' }
#' 
extractTextInfo <- function(text, nerModel)
{
    # for one file split into sentences
    sentences <- data_frame(Sentence=sent_detect(text)) %>% 
        # one row per sentence
        mutate(Number=1:n()) %>% 
        as.tbl
    
    # get paired entities
    sentences %>% 
        # for each sentence
        group_by(Number) %>% 
        # extract entities mentioned
        do(extractSentenceInfo(.$Sentence, nerModel=nerModel)) %>% 
        # create edgelist of mentioned entities
        do(pairEntities(.)) %>%
        # make it a pairedEntity class
        reclass(value='pairedEntity')
}
