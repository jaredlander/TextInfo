#' @title textFileToDB
#' @description Writes a text file to an OrientDB database
#' @details Writes a text file to an OrientDB database.  Inserts the class, text, file name, time and user into the database object.
#' @export textFileToDB
#' @rdname textFileToDB
#' @author Jared P. Lander
#' @param file Path to a text file
#' @param db A database connection
#' @param class The database class to insert into
#' @param type The type of database being written to
#' @return An httr response object
#' @examples 
#' \dontrun{
#' library(OrientExpress)
#' theDB <- dbInfo('localhost', database='DocStore', username='TheUser', password='ThePass', port=2480)
#' textFileToDB('A-17.txt', db=theDB, class='Document')
#' }
#' 
textFileToDB <- function(file, db, class, type=c("document", "graph"))
{
    type <- match.arg(type)
    
    # read the file
    theText <- readLines(file)
    
    # call function to write to DB
    textToDB(text=theText, db=db, class=class, type=type, filename=file)
}

#' @title dirToDB
#' @description Writes text files from a directory to an OrientDB database
#' @details Writes text files from a directory to an OrientDB database.  Inserts the class, text, file name, time and user into the database object for each file.
#' @author Jared P. Lander
#' @export dirToDB
#' @rdname dirToDB
#' @param dir The path to a directory
#' @param db A database connection
#' @param class The database class to insert into
#' @param type The type of database being written to
#' @return A list of httr response objects
#' @examples 
#' \dontrun{
#' library(OrientExpress)
#' theDB <- dbInfo('localhost', database='DocStore', username='TheUser', password='ThePass', port=2480)
#' dirToDB('path/to/files/', db=theDB, class='Document')
#' }
#' 
dirToDB <- function(dir, db, class, type=c("document", "graph"))
{
    type <- match.arg(type)
    
    # get file names
    theFiles <- list.files(dir, full.names=TRUE)
    
    # iterate over the list of files and call textFileToDB to read them and write to the DB
    filesToDB(theFiles, db=db, class=class, type=type)
}


#' @title filesToDB
#' @description Writes text files to an OrientDB database
#' @details Writes text files to an OrientDB database.  Inserts the class, text, file name, time and user into the database object for each file.
#' @author Jared P. Lander
#' @export filesToDB
#' @rdname filesToDB
#' @param files A vector of file names
#' @param db A database connection
#' @param class The database class to insert into
#' @param type The type of database being written to
#' @return A list of httr response objects
#' @examples 
#' \dontrun{
#' library(OrientExpress)
#' theDB <- dbInfo('localhost', database='DocStore', username='TheUser', password='ThePass', port=2480)
#' filesToDB(c('file1.txt', 'file2.txt'), db=theDB, class='Document')
#' }
#' 
filesToDB <- function(files, db, class, type=c("document", "graph"))
{
    type <- match.arg(type)
    
    files %>% purrr::map(textFileToDB, db=db, class=class, type=type)
}

#' @title textToDB
#' @description Writes text to a database
#' @details Writes text to an OrientDB database.  Inserts the class, text, file name, time and user into the database object.
#' @author Jared P. Lander
#' @export textToDB
#' @rdname textToDB
#' @param text Text to be inserted
#' @param db A database connection
#' @param class The database class to insert into
#' @param type The type of database being written to
#' @param filename Name of file that is being inserted
#' @return An httr response object
#' @examples 
#' \dontrun{
#' library(OrientExpress)
#' theDB <- dbInfo('localhost', database='DocStore', username='TheUser', password='ThePass', port=2480)
#' theText <- readLines('A-17.txt')
#' textFileToDB(theText, db=theDB, class='Document')
#' }
textToDB <- function(text, db, class, type=c("document", "graph"), filename=NULL)
{
    type <- match.arg(type)
    
    # build list to insert
    obj <- list("@class"=class, text=text, filename=filename, time=Sys.time(), user=db$username)
    
    # insert into orientdb
    OrientExpress::insert(db=db, object=obj, type=type)
}

