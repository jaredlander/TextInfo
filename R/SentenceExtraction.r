#' @title extractSentenceInfo
#' @description Given a sentence and a ner model get info on the all the entities found in the sentence
#' @details Given a sentence and a ner model get info on the all the entities found in the sentence
#' @aliases extractSentenceInfo
#' @export extractSentenceInfo
#' @seealso extractEntity
#' @author Jared P. Lander
#' @import MITIE
#' @param sentence A sentence of text
#' @param A ner model supplied by MITIE
#' @return A tbl with one row per entity found in the sentence
#' @examples 
#' \dontrun{
#' ner_model_path <- "tests/data/ner_model.dat"
#' ner <- NamedEntityExtractor$new(ner_model_path)
#' theText <- "A network of new super PACs said Wednesday that it had raised $31 million to support Senator Ted Cruz's presidential campaign, a sum that could upend expectations in the race for the Republican nomination and rewrite the political rule book for outside spending"
#' extractSentenceInfo(theText, nerModel=ner)
#' }
#' 
extractSentenceInfo <- function(sentence, nerModel)
{
    # get tokens for sentence
    tokens <- mitie_tokenize(text=sentence)
    
    # extract entities from the tokens based on the model
    entities <- nerModel$extract_entities(tokens)
    
    # get entity info
    result <- extractEntity(entities=entities, tokens=tokens, tagNames=nerModel$get_possible_ner_tags())
    
    if(is.null(result))
    {
        result <- data_frame(Entity=NA, Type=NA, Start=NA, Stop=NA)
    }
    
    reclass(result) <- 'Entity'
    
    return(result)
}