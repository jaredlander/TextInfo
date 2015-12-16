#' @title extractSentenceInfo
#' @description Given a sentence and a ner model get info on the all the entities found in the sentence
#' @details Given a sentence and a ner model get info on the all the entities found in the sentence
#' @aliases extractSentenceInfo
#' @export extractSentenceInfo
#' @seealso extractEntity
#' @author Jared P. Lander
#' @import MITIE
#' @importFrom stringr str_extract ignore.case regex
#' @importFrom dplyr bind_rows data_frame
#' @param sentence A sentence of text
#' @param nerModel A ner model supplied by MITIE
#' @param requiredTerms A vector of terms that must be extracted if they exist
#' @param ignore.case Logical indicating if requiredTerms is not case sensitive
#' @return A tbl with one row per entity found in the sentence
#' @examples 
#' \dontrun{
#' ner_model_path <- "tests/data/ner_model.dat"
#' ner <- NamedEntityExtractor$new(ner_model_path)
#' theText <- "A network of new super PACs said Wednesday that it had raised $31 million to support Senator Ted Cruz's presidential campaign, a sum that could upend expectations in the race for the Republican nomination and rewrite the political rule book for outside spending"
#' extractSentenceInfo(theText, nerModel=ner)
#' extractSentenceInfo(theText, nerModel=ner, requiredTerms=c('network'))
#' }
#' 
extractSentenceInfo <- function(sentence, nerModel, requiredTerms=NULL, ignore.case=TRUE)
{
    # get tokens for sentence
    tokens <- mitie_tokenize(text=sentence)
    
    # extract entities from the tokens based on the model
    entities <- nerModel$extract_entities(tokens)
    
    # get entity info
    result <- extractEntity(entities=entities, tokens=tokens, tagNames=nerModel$get_possible_ner_tags())
    
    # check for forced entries
    if(!is.null(requiredTerms))
    {
        # extract the terms
        foundTerms <- str_extract(string=sentence, pattern=regex(requiredTerms, ignore_case=ignore.case))
        
        # make sure they're not in the list already
        foundTerms <- foundTerms[!foundTerms %in% result$Entity]
        
        # add them to the list
        result <- bind_rows(result, data_frame(Entity=foundTerms, Type=NA, Start=NA, Stop=NA))
    }
    
    if(is.null(result))
    {
        result <- data_frame(Entity=NA, Type=NA, Start=NA, Stop=NA)
    }
    
    reclass(result) <- 'Entity'
    
    return(result)
}

# theSentence <- "How many days in Levant are spent underneath a boiling desert sun in Israel's desert of the Levant within the ancient Levant?"
# str_extract_all(string=theSentence, pattern=c('desert', 'Israel'))
# 
# ner_model_path <- "../ISSO/2015_NLP/mitie/MITIE-models/english/ner_model.dat"
# ner <- NamedEntityExtractor$new(ner_model_path)
# theTokens <- mitie_tokenize(theSentence)
# extractEntity(entities=ner$extract_entities(theTokens), tokens=theTokens, tagNames=ner$get_possible_ner_tags())
# texted <- extractSentenceInfo(theSentence, ner)
# texted2 <- extractSentenceInfo(theSentence, ner, requiredTerms=c('levant', 'boiling desert', 'days'))
# extractTextInfo(theSentence, ner)
# extractTextInfo(theSentence, ner, requiredTerms=c('levant', 'boiling desert', 'days'))
# pairEntities(texted)
# pairEntities(texted2)
# result <- extractEntity(entities=ner$extract_entities(theTokens), tokens=theTokens, tagNames=ner$get_possible_ner_tags())
# texted %>% group_by(Entity) %>% summarize(Count=n())
