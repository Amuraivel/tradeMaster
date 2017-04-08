function (channel, dat, tablename = NULL, append = FALSE, rownames = TRUE, 
    colnames = FALSE, verbose = FALSE, safer = TRUE, addPK = FALSE, 
    typeInfo, varTypes, fast = TRUE, test = FALSE, nastring = NULL) 
{
    if (!odbcValidChannel(channel)) 
        stop("first argument is not an open RODBC channel")
    if (missing(dat)) 
        stop("missing parameter")
    if (!is.data.frame(dat)) 
        stop("should be a data frame")
    if (is.null(tablename)) 
        tablename <- if (length(substitute(dat)) == 1) 
            as.character(substitute(dat))
        else as.character(substitute(dat)[[2L]])
    if (length(tablename) != 1L) 
        stop(sQuote(tablename), " should be a name")
    switch(attr(channel, "case"), nochange = {
    }, toupper = {
        tablename <- toupper(tablename)
        colnames(dat) <- toupper(colnames(dat))
    }, tolower = {
        tablename <- tolower(tablename)
        colnames(dat) <- tolower(colnames(dat))
    })
    keys <- -1
    if (is.logical(rownames) && rownames) 
        rownames <- "rownames"
    if (is.character(rownames)) {
        dat <- cbind(row.names(dat), dat)
        names(dat)[1L] <- rownames
        if (addPK) {
            keys <- vector("list", 4L)
            keys[[4L]] <- rownames
        }
    }
    if (is.logical(colnames) && colnames) {
        dat <- as.data.frame(rbind(colnames(dat), as.matrix(dat)))
    }
    dbname <- odbcTableExists(channel, tablename, abort = FALSE)
    if (length(dbname)) {
        if (!append) {
            if (safer) 
                stop("table ", sQuote(tablename), " already exists")
            query <- paste("DELETE FROM", dbname)
            if (verbose) 
                cat("Query: ", query, "\n", sep = "")
            res <- sqlQuery(channel, query, errors = FALSE)
            if (is.numeric(res) && res == -1L) 
                stop(paste(odbcGetErrMsg(channel), collapse = "\n"))
        }
        if (sqlwrite(channel, tablename, dat, verbose = verbose, 
            fast = fast, test = test, nastring = nastring) == 
            -1) {
            query <- paste("DROP TABLE", dbname)
            if (verbose) {
                cat("sqlwrite returned ", odbcGetErrMsg(channel), 
                  "\n", sep = "\n")
                cat("Query: ", query, "\n", sep = "")
            }
            if (safer) 
                stop("unable to append to table ", sQuote(tablename))
            res <- sqlQuery(channel, query, errors = FALSE)
            if (is.numeric(res) && res == -1L) 
                stop(paste(odbcGetErrMsg(channel), collapse = "\n"))
        }
        else {
            return(invisible(1L))
        }
    }
    types <- sapply(dat, typeof)
    facs <- sapply(dat, is.factor)
    isreal <- (types == "double")
    isint <- (types == "integer") & !facs
    islogi <- (types == "logical")
    colspecs <- rep("varchar(255)", length(dat))
    if (!missing(typeInfo) || !is.null(typeInfo <- typesR2DBMS[[odbcGetInfo(channel)[1L]]])) {
        colspecs <- rep(typeInfo$character[1L], length(dat))
        colspecs[isreal] <- typeInfo$double[1L]
        colspecs[isint] <- typeInfo$integer[1L]
        colspecs[islogi] <- typeInfo$logical[1L]
    }
    else {
        typeinfo <- sqlTypeInfo(channel, "all", errors = FALSE)
        if (is.data.frame(typeinfo)) {
            if (any(isreal)) {
                realinfo <- sqlTypeInfo(channel, "double")[, 
                  1L]
                if (length(realinfo) > 0L) {
                  if (length(realinfo) > 1L) {
                    nm <- match("double", tolower(realinfo))
                    if (!is.na(nm)) 
                      realinfo <- realinfo[nm]
                  }
                  colspecs[isreal] <- realinfo[1L]
                }
                else {
                  realinfo <- sqlTypeInfo(channel, "float")[, 
                    1L]
                  if (length(realinfo) > 0L) {
                    if (length(realinfo) > 1L) {
                      nm <- match("float", tolower(realinfo))
                      if (!is.na(nm)) 
                        realinfo <- realinfo[nm]
                    }
                    colspecs[isreal] <- realinfo[1L]
                  }
                }
            }
            if (any(isint)) {
                intinfo <- sqlTypeInfo(channel, "integer")[, 
                  1L]
                if (length(intinfo) > 0L) {
                  if (length(intinfo) > 1) {
                    nm <- match("integer", tolower(intinfo))
                    if (!is.na(nm)) 
                      intinfo <- intinfo[nm]
                  }
                  colspecs[isint] <- intinfo[1L]
                }
            }
        }
    }
    names(colspecs) <- names(dat)
    if (!missing(varTypes)) {
        if (!length(nm <- names(varTypes))) 
            warning("argument 'varTypes' has no names and will be ignored")
        OK <- names(colspecs) %in% nm
        colspecs[OK] <- varTypes[names(colspecs)[OK]]
        notOK <- !(nm %in% names(colspecs))
        if (any(notOK)) 
            warning("column(s) ", paste(nm[notOK], collapse = ", "), 
                " 'dat' are not in the names of 'varTypes'")
    }
    query <- sqltablecreate(channel, tablename, colspecs = colspecs, 
        keys = keys)
    if (verbose) 
        cat("Query: ", query, "\n", sep = "")
    res <- sqlQuery(channel, query, errors = FALSE)
    if (is.numeric(res) && res == -1) 
        stop(paste(odbcGetErrMsg(channel), collapse = "\n"))
    if (sqlwrite(channel, tablename, dat, verbose = verbose, 
        fast = fast, test = test, nastring = nastring) < 0) {
        err <- odbcGetErrMsg(channel)
        msg <- paste(err, collapse = "\n")
        if ("missing column name" %in% err) 
            msg <- paste(msg, "Check case conversion parameter in odbcConnect", 
                sep = "\n")
        stop(msg)
    }
    invisible(1L)
}