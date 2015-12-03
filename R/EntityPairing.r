#' @title pairEntities
#' @description Given a list of entities find all combinations.
#' @details Given a list of entities find all combinations.  This expects the data to be of the form |Entity|Type|Start|Stop| as created by extractSenetenceInfo.
#' @aliases pairEntities
#' @export pairEntities
#' @author Jared P. Lander
#' @importFrom dplyr select_ as.tbl mutate_each funs rename
#' @importFrom magrittr "%>%"
#' @importFrom useful "reclass<-"
#' @seealso extractSentenceInfo
#' @param data A tbl created by extractSentenceInfo
#' @param entity The name of the column holding entity
#' @return A two-column tbl like a pair list between the entities found together
#' @examples 
#' \dontrun{
#' ner_model_path <- "mitie/MITIE-models/english/ner_model.dat"
#' ner <- NamedEntityExtractor$new(ner_model_path)
#' textFiles <- file.path('data', 'NYTimes', dir('data/NYTimes/'))
#' thisText <- readLines(textFiles[1])
#' sentences <- sent_detect(thisText)
#' # run on just one sentence
#' oneSent <- extractSentenceInfo(sentences[1], nerModel=ner)
#' pairEntities(oneSent)
#' }
#' 
pairEntities <- function(data, entity='Entity')
{
    # if there is only one row of data
    if(nrow(data) < 2)
    {
        result <- data_frame(One=NA, Two=NA)
        reclass(result) <- 'pairedEntity'
        return(result)
    }
    
    # run combn to get every combination of two for the one column
    result <- combn((data %>% select_('Entity'))[[1]], m=2) %>% 
        # transpose so it's easy to work with
        t %>% 
        # convert to a data.frame then tbl
        as.data.frame %>% as.tbl %>% 
        # they came across as factors so convert to character
        mutate_each(funs(as.character(.))) %>% 
        # give good names
        rename(One=V1, Two=V2)
    reclass(result) <- 'pairedEntity'
    return(result)
}
