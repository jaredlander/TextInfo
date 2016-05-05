#' @title extractPairs
#' @description Given text, extract entities that cooccur in a sentence
#' @details Given text, extract entities that cooccur in a sentence.  Text can be stored in files or in a column in a tbl or database.
#' @aliases extractPairs
#' @export extractPairs
#' @rdname extractPairs
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom dplyr data_frame group_by do rename
#' @importFrom useful reclass
#' @param x Either a character vector with names of files or a tbl or database source holding the text.  In the latter cases the text is assumed to be in a column named Text and the grouping variable is assumed to be named File.
#' @param nerModel A ner model supplied by MITIE
#' @param \dots Further arguments
#' @return A tbl listing entity cooccurences along with the file name and the sentence number.
#' @examples 
#' \dontrun{
#' ner_model_path <- "mitie/MITIE-models/english/ner_model.dat"
#' ner <- NamedEntityExtractor$new(ner_model_path)
#' textFiles <- file.path('data', 'NYTimes', dir('data/NYTimes/'))
#' extractPairs(textFiles, ner)
#' 
#' textDB <- src_sqlite('data/TimesDB/Articles.sqlite3')
#' textTable <- tbl(textDB, 'Articles')
#' textTable
#' extractPairs(textTable, ner)
#' }
#' 
extractPairs <- function(x, nerModel, ...)
{
    UseMethod('extractPairs')
}

#' @title extractPairs.character
#' @export extractPairs.character
#' @export
#' @rdname extractPairs
extractPairs.character <- function(x, nerModel, ...)
{
    # create a data.frame listing the files
    result <- data_frame(File=x)
    # read into a tbl
    result %<>% group_by(File) %>% 
        do(data_frame(Text=paste(readLines(.$File), collapse=''))) 
    
    # call extractPairs to run the code in extractPairs.tbl
    extractPairs(result, nerModel, ...)
}

#for both data.base stored and tbl stored ' @title extractPairs.tbl ' @export extractPairs.tbl ' @rdname extract.Pairs
#' @title extractPairs.tbl
#' @export extractPairs.tbl
#' @export
#' @rdname extractPairs
#' @param toSub Named vector where the elements are the pattern and the names are the replacement values
#' @param requiredTerms A vector of terms that must be extracted if they exist
#' @param ignore.case Logical indicating if requiredTerms is not case sensitive
#' @importFrom useful moveToFront subVector
extractPairs.tbl <- function(x, nerModel, toSub=NULL, requiredTerms=NULL, ignore.case=TRUE, ...)
{
    dots <- 
    x %>% 
        # group by the file
        group_by(File) %>% 
        # replace special characters with their alternate meanings
        mutate(Text=subVector(Text, toSub=toSub)) %>%
        # do the pair extraction for each sentence in each file
        do(extractTextInfo(text=., nerModel=nerModel, requiredTerms=requiredTerms, ignore.case=ignore.case)) %>%
        # rename Number to SentenceNumber
        rename(SentenceNumber=Number) %>%
        # reorder the columns
        moveToFront(c('One', 'Two')) %>%
        # give it a good class
        reclass(value='pairedEntity')
}

#' @title extractPairs.OrientDB
#' @export extractPairs.OrientDB
#' @export
#' @importFrom httr content
#' @importFrom purrr flatten map reduce
#' @importFrom dplyr bind_rows
#' @param class The document DB class from which to query
#' @param id List of IDs to read, if \code{NULL} it pulls every record
#' @rdname extractPairs
extractPairs.OrientDB <- function(x, nerModel, id=NULL, class, ...)
{
    # copy x to db for easier coding
    db <- x
    
    idQuery <- sprintf("SELECT @rid from %s", class)
    
    # get a list of IDs
    if(is.null(id))
    {
        id <- OrientExpress::query(db, idQuery) %>% content %>% flatten %>% map(function(x) x$rid) %>% 
            flatten %>% sub(pattern="#", replacement="", x=.)
    }
    
    # for now read each ID one at a time
    # eventually write code to read a few at a time
    resultList <- lapply(id, queryAndExtract.OrientDB, db=db, nerModel=nerModel, ...)
    
    # combine into one data_frame
    resultList %>% reduce(bind_rows)
}

#' @title queryAndExtract.OrientDB
#' @description Extracts entities from an OrientDB entry
#' @details Queries the database and runs the result through \code{extractPairs.tbl}
#' @author Jared P. Lander
#' @rdname queryAndExtract.OrientDB
#' @param ID Record ID to query
#' @param db An OrientDB connection
#' @param nerModel A ner model supplied by MITIE
#' @param \dots Further arguments
#' @importFrom httr content
#' @importFrom purrr flatten keep
#' @return A tbl listing entity cooccurences along with the ID and the sentence number.
queryAndExtract.OrientDB <- function(ID, db, nerModel, ...)
{
    # extract text
    theText <- OrientExpress::query(db, query=sprintf('SELECT FROM %s', ID)) %>% content %>% 
        flatten %>% (function(x){ x$result$text}) %>% keep(function(x) x != "") %>% 
        paste(collapse=" ")
    
    # build a data.frame
    extractPairs(dplyr::data_frame(File=ID, Text=theText), nerModel, ...)
}

#' @title extractPairs.es_conn
#' @export extractPairs.es_conn
#' @export
#' @param index Document index
#' @param type Type of document
## @param id ID to search
#' @param q Search query
#' @param search Type of search: search, scroll or page
#' @param scrollHold Time to hold open scroll state
#' @param size Size of entry per shard
#' @rdname extractPairs
extractPairs.es_conn <- function(x, nerModel, index=NULL, type=NULL, id=NULL, q=NULL, 
                                 search=c('search', 'scroll', 'page'), scrollHold="5m", size=10, ...)
{
    # are we scrolling or searching
    search <- match.arg(search)
    
    size <- as.character(size)
    
    if(search == 'search')
    {
        ## call search function
        answer <- dbSearch(x, index=index, type=type, id=id, q=q, size=size)
        
    } else if(search == 'scroll')
    {
        ## call scroll function
        answer <- dbScroll.es_conn(x, nerModel=nerModel, index=index, scrollHold=scrollHold, size=size)
    } else if(search == 'page')
    {
        ## call page function
        stop('paging not available yet')
    }
    
   return(answer)
}

#' @title dbSearch
#' @description Elastic search function
#' @details Searches elasticsearch and forms edgelsist of paired entities
#' @rdname dbSearch
#' @export dbSearch
#' @author Jared P. Lander
#' @param db database connection
#' @param nerModel A ner model supplied by MITIE
#' @param \dots Further arguments
#' @return Edgelist of paired entities
dbSearch <- function(db, nerModel, ...)
{
    UseMethod('dbSearch')
}

#' @title dbSearch.es_conn
#' @author Jared P. Lander
#' @rdname dbSearch
#' @export dbSearch.es_conn
#' @export
#' @param index Document index
#' @param type Type of document
#' @param id ID to search
#' @param q Search query
#' @param size Size of results returned from each shard
#' 
dbSearch.es_conn <- function(db, nerModel, index=NULL, type=NULL, id=NULL, q=NULL, size=10, ...)
{
    size <- as.character(size)
    
    # query data
    theSearch <- elastic::Search(index=index, type=type, id=id, q=q, size=size)
    # form into a data_frame
    theData <- elasticToTbl(theSearch)
    extractPairs.tbl(x=theData, nerModel=nerModel, ...)
}

#' @title elasticToTbl
#' @description Converts a list of elastic hits into a tbl
#' @details Iterates over a list of results and returns the grouping variable and text (content) variable
#' @author Jared P. Lander
#' @export elasticToTbl
#' @rdname elasticToTbl
#' @param data Result from calling an ES search
#' @param group Name of grouping variable
#' @param text Name of text/content variable
#' 
elasticToTbl <- function(data, group='filename', text='content')
{
    data$hits$hits %>% 
        purrr::map_df(function(x) dplyr::data_frame(File=x$`_source`[[group]], Text=x$`_source`[[text]]))
}


#' @title dbScroll
#' @description Elastic search function
#' @details Searches elasticsearch and forms edgelsist of paired entities
#' @rdname dbScroll
#' @export dbScroll
#' @author Jared P. Lander
#' @param db database connection
#' @param nerModel A ner model supplied by MITIE
#' @param \dots Further arguments
#' @return Edgelist of paired entities
dbScroll <- function(db, nerModel, ...)
{
    UseMethod('dbScroll')
}

#' @title dbScroll.es_conn
#' @author Jared P. Lander
#' @rdname dbSroll
#' @export dbScroll.es_conn
#' @export
#' @param index Document index
#' @param scrollHold Time to hold open scroll state
#' @param size Size of entry per shard
#' 
dbScroll.es_conn <- function(db, nerModel, index=NULL, scrollHold="5m", size=10, ...)
{
    # make size a character
    size <- as.character(size)
    
    # do first search to initiate a scroll
    firstSearch <- elastic::Search(index=index, scroll=scrollHold, search_type = "scan", size=size)
    
    ## figure number of iterations
    # get number of results returned
    numResults <- as.numeric(size)*firstSearch$`_shards`$total
    # num iterations is total/numResults, rounded up
    numIter <- ceiling(firstSearch$hits$total/numResults)
    
    ## build empty list to hold results
    results <- vector(mode='list', length=numIter)
    
    ## iterate through the results, writing to the list
    hits <- 1
    iter <- 1
    while(hits != 0){
        res <- elastic::scroll(scroll_id=firstSearch$`_scroll_id`)
        hits <- length(res$hits$hits)
        if(hits > 0)
        {
            theData <- elasticToTbl(res)
            results[[iter]] <- extractPairs.tbl(x=theData, nerModel=nerModel, ...)
        }
        
        iter <- iter + 1
    }
    
    # make into a tbl and return
    dplyr::bind_rows(results)
}

# dbPage <- function(db, ...)
# {
#     UseMethod('dbPage')
# }
# 
# dbPage.es_conn <- function(db, index=NULL, size="10", ...)
# {
#     # convert size to character
#     size <- as.character(size)
#     
#     # do first search to get number of items
#     firstSearch <- elastic::Search(index=index, size="1")
#     # get number of items
#     hits <- firstSearch$hits$total
#     
#     # get set of pages
# }