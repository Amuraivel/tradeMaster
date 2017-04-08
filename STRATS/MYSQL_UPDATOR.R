mySQLupdate <- function (channel, dat, tablename = NULL, index = NULL, verbose = FALSE, 
    test = FALSE, nastring = NULL, fast = TRUE) 
{
    if (!odbcValidChannel(channel)) 
        stop("first argument is not an open RODBC channel")
    if (missing(dat)) 
        stop("missing parameter")
    if (!is.data.frame(dat)) 
        stop("should be a data frame or matrix")
    if (is.null(tablename)) 
        tablename <- if (length(substitute(dat)) == 1L) 
            as.character(substitute(dat))
        else as.character(substitute(dat)[[2L]])
    if (length(tablename) != 1L) 
        stop(sQuote(tablename), " should be a name")
    dbname <- odbcTableExists(channel, tablename)
    cnames <- colnames(dat)
    cnames <- mangleColNames(cnames)
    cnames <- switch(attr(channel, "case"), nochange = cnames, 
        toupper = toupper(cnames), tolower = tolower(cnames))
    cdata <- sqlColumns(channel, tablename)
    coldata <- cdata[c(4L, 5L, 7L, 9L)]
    if (is.character(index)) {
        intable <- index %in% coldata[, 1L]
        if (any(!intable)) 
            stop("index column(s) ", paste(index[!intable], collapse = " "), 
                " not in database table")
        intable <- index %in% cnames
        if (any(!intable)) 
            stop("index column(s) ", paste(index[!intable], collapse = " "), 
                " not in data frame")
        indexcols <- index
    }
    else {
        haveKey <- FALSE
        indexcols <- sqlPrimaryKeys(channel, tablename)
        if (!(is.numeric(indexcols) || nrow(indexcols) == 0L)) {
            index <- as.character(indexcols[, 4L])
            intable <- index %in% cnames
            if (any(intable)) {
                indexcols <- index[intable][1L]
                haveKey <- TRUE
            }
        }
        if (!haveKey) {
            indexcols <- sqlColumns(channel, tablename, special = TRUE)
            if (!(is.numeric(indexcols) || nrow(indexcols) == 
                0L)) {
                indexcols <- indexcols[c(2L, 3L, 5L, 7L)]
                indexflags <- indexcols[, 1L] %in% cnames
                if (all(indexflags)) {
                  incoldata <- indexcols[, 1L] %in% coldata[, 
                    1L]
                  if (any(!incoldata)) 
                    coldata <- rbind(coldata, indexcols[!incoldata])
                  indexcols <- as.character(indexcols[, 1L])
                  haveKey <- TRUE
                }
            }
        }
        if (!haveKey) {
            m <- match("rownames", tolower(coldata[, 1L]))
            if (is.na(m)) 
                stop("cannot update ", sQuote(tablename), " without unique column")
            indexcols <- coldata[m, 1L]
            dat <- cbind(row.names(dat), dat)
            names(dat)[1L] <- indexcols
            cnames <- c(indexcols, cnames)
        }
    }
    intable <- cnames %in% coldata[, 1L]
    if (any(!intable)) 
        stop("data frame column(s) ", paste(cnames[!intable], 
            collapse = " "), " not in database table")
    cn1 <- cnames[!cnames %in% indexcols]
    cn2 <- quoteColNames(channel, cn1)
    if (fast) {
        query <- paste("UPDATE", dbname, "SET")
        query <- paste(query, paste(paste(cn2, "=?", sep = ""), 
            collapse = ", "))
        paramnames <- c(cn1, indexcols)
        if (length(indexcols)) {
            ind <- quoteColNames(channel, indexcols)
            query <- paste(query, "WHERE", paste(paste(ind, "=?", 
                sep = ""), collapse = " AND "))
        }
        row.names(coldata) <- coldata[, 1L]
        paramdata <- coldata[paramnames, ]
        if (test | verbose) 
            cat("Query: ", query, "\n", sep = "")
        stat <- odbcUpdate(channel, query, dat, paramdata, test = test, 
            verbose = verbose, nastring = nastring)
    }
    else {
        data <- as.matrix(dat)
        if (nchar(enc <- attr(channel, "encoding")) && is.character(data)) 
            data[] <- iconv(data, to = enc)
        colnames(data) <- cnames
        cdata <- sub("\\([[:digit:]]*\\)", "", sqlColumns(channel, 
            tablename)[, "TYPE_NAME"])
        tdata <- sqlTypeInfo(channel)
        tdata <- as.matrix(tdata[match(cdata, tdata[, 1]), c(4, 
            5)])
        for (cn in seq_along(cdata)) {
            td <- as.vector(tdata[cn, ])
            if (is.na(td[1L])) 
                next
            if (identical(td, rep("'", 2L))) 
                data[, cn] <- gsub("'", "''", data[, cn])
            data[, cn] <- paste(td[1L], data[, cn], td[2L], sep = "")
        }
        data[is.na(dat)] <- if (is.null(nastring)) 
            "NULL"
        else nastring
        for (i in 1L:nrow(data)) {
            query <- paste("INSERT INTO", dbname, paste("(",paste(dat[i,],collapse=","),")",sep="") "SET")
            query <- paste(query, paste(paste(cn2, "=", data[i, 
                cn1], sep = ""), collapse = ", "))
            if (length(indexcols)) {
                ind <- quoteColNames(channel, indexcols)
                query <- paste(query, "WHERE", paste(paste(ind, 
                  "=", data[i, indexcols], sep = ""), collapse = " AND "))
            }
            if (verbose) 
                cat("Query: ", query, "\n", sep = "")
            if ((stat <- odbcQuery(channel, query)) < 0L) 
                break
        }
    }
    if (stat < 0L) 
        stop(paste(odbcGetErrMsg(channel), sep = "\n"))
    invisible(stat)
}