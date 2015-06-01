#' @title convertToNodeLink
#' @description Given an edge list split it into nodes and links
#' @details Given an edge list split it into nodes and links
#' @author Jared P. Lander
#' @aliases convertToNodeLink
#' @export convertToNodeLink
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom dplyr select distinct arrange mutate ungroup left_join rename
#' @param data Edgelist
#' @param Character vector specifying the columns used to denote vertices
#' @return A list with the nodes data.frame and links data.frame
#' @examples 
#' \dontrun{
#' convertToNodeLink(textExtract, c('One', 'Two'))
#' }
#' 
convertToNodeLink <- function(data, vertices)
{
    # build a df of nodes
    nodeDF <- stack(data, select=vertices) %>% select(values) %>% distinct %>% arrange(values) %>% 
        mutate(Index=seq(from=0, to=n()-1, by=1))
    
    # create the join associations
    firstJoin <- 'values'
    names(firstJoin) <- vertices[1]
    secondJoin <- 'values'
    names(secondJoin) <- vertices[2]
    
    # build a links df
    linkDF <- data %>% ungroup %>% left_join(nodeDF, by=firstJoin) %>% rename(Source=Index) %>%
        left_join(nodeDF, by=secondJoin) %>% rename(Target=Index) %>% filter(!is.na(Source), !is.na(Target))
    
    nodeDF %<>% mutate(Group=1)
    
    list(Nodes=nodeDF, Links=linkDF)
}