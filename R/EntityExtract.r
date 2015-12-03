#' @title extractEntityInfo
#' @description For a single entity this grabs the information
#' @details For a single entity this grabs the information
#' @author Jared P. Lander
#' @aliases extractEntityInfo
#' @export extractEntityInfo
#' @seealso extractEntity
#' @importFrom dplyr data_frame
#' @param entity A single entity as found by MITIE
#' @param tokens The tokens from text as found by MITIE
#' @param tagNames The possible tags from this MITIE model
#' @return A one-row tbl holding the Entity, Type, Start and Stop point at the tokens
#' @examples 
#' \dontrun{
#' ner_model_path <- "tests/data/ner_model.dat"
#' ner <- NamedEntityExtractor$new(ner_model_path)
#' tag_names <- ner$get_possible_ner_tags()
#' theText <- "A network of new super PACs said Wednesday that it had raised $31 million to support Senator Ted Cruz's presidential campaign, a sum that could upend expectations in the race for the Republican nomination and rewrite the political rule book for outside spending"
#' tokens <- mitie_tokenize(theText)
#' entities <- ner$extract_entities(tokens)
#' extractEntityInfo(entities[[1]], tokens, tag_names)
#' extractEntityInfo(entities[[2]], tokens, tag_names)
#' }
#' 
extractEntityInfo <- function(entity, tokens, tagNames)
{
    data_frame(Entity=paste(tokens[entity$start:entity$end], collapse=" "),
               Type=tagNames[entity$tag],
               Start=entity$start,
               Stop=entity$end)
}

#' @title extractEntity
#' @description For a collection of entities this grabs the information
#' @details For a collection of entities this grabs the information
#' @author Jared P. Lander
#' @aliases extractEntity
#' @export extractEntity
#' @seealso extractEntityInfo
#' @importFrom dplyr bind_rows
#' @param entities Entities as found by MITIE
#' @param tokens The tokens from text as found by MITIE
#' @param tagNames The possible tags from this MITIE model
#' @return A tbl holding entity info, one row per each
#' @examples 
#' \dontrun{
#' ner_model_path <- "mitie/MITIE-models/english/ner_model.dat"
#' ner <- NamedEntityExtractor$new(ner_model_path)
#' tag_names <- ner$get_possible_ner_tags()
#' theFile <- file.path('data', 'NYTimes', dir(file.path('data', 'NYTimes'))[1])
#' tokens <- mitie_tokenize(samp[1])
#' extractEntity(entities, tokens, tag_names)
#' }
#' 
extractEntity <- function(entities, tokens, tagNames)
{
    Reduce(bind_rows, lapply(entities, extractEntityInfo, tokens=tokens, tagNames=tagNames))
}