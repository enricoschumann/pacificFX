fetchFX <- function(baseCurrency = "USD",
                    targetCurrencies = c("EUR", "AUD"),
                    startDate = "2010-01-01",
                    endDate,
                    dataFrequency = "daily",
                    notation = "volume") {

    startDate <- as.Date(startDate)
    if (missing(endDate))
        endDate <- Sys.Date() else endDate <- as.Date(endDate)
    sD <- startDate
    eD <- endDate
    done <- FALSE
    res <- NULL
    counter <- 0

    dataFrequency <- tolower(dataFrequency)
    if (!(dataFrequency %in% c("daily", "weekly", "monthly")))
        stop("unknown 'dataFrequency' (must be 'daily', 'weekly' or 'monthly')")

    notation <- tolower(notation)
    if (!(notation %in% c("volume", "price")))
        stop("unknown 'notation' (must be 'price' or 'volume')")

    while (!done) {
        counter <- counter + 1L
        if (counter > 1L)
            Sys.sleep(1)

        eD <- min(sD + 1440L, endDate)

        site <- paste("http://fx.sauder.ubc.ca/cgi/fxdata",
                      "?b=", baseCurrency,
                      paste("&c=", targetCurrencies, sep = "", collapse=""),
                      "&rd=",
                      "&fd=", strftime(sD,"%d"),
                      "&fm=", strftime(sD,"%m"),
                      "&fy=", strftime(sD,"%Y"),
                      "&ld=", strftime(eD,"%d"),
                      "&lm=", strftime(eD,"%m"),
                      "&ly=", strftime(eD,"%Y"),
                      "&y=", dataFrequency,
                      "&q=", notation,
                      "&f=csv&o=",
                      sep = "")

        con <- url(site)
        cat("Downloading data from ", strftime(sD,"%Y-%m-%d"),
            " to ", strftime(eD,"%Y-%m-%d"), " ... ", sep = "")
        flush.console()
        dats <- try(scan(con, what = "character", sep="\n",
                         skip = 1L, quote="\"", quiet = TRUE))
        em <- geterrmessage()
        close(con)
        if (inherits(dats, "try-error")) {
            cat("FAILED.\n", sep = "")
            cat(em)
            return(NULL)
        }
        else {
            cat("OK.\n", sep = "")
        }

        descr <- dats[1L]
        descr <- unlist(strsplit(descr, ","))

        temp <- dats[-c(1L,length(dats))]
        temp <- strsplit(temp, ",")

        Jul.Dates <- sapply(temp, `[[`, 1L)
        Jul.Dates <- as.numeric(Jul.Dates)
        Dates  <- sapply(temp, `[[`, 2L)
        Dates <- as.Date(Dates, "%Y/%m/%d")

        np <- length(descr) - 3L
        if (np < 1L) {
            stop("error while preparing results")
        }
        plist <- vector("list", length = np)
        for (i in seq_len(np)) {
            plist[[i]] <- as.numeric(sapply(temp, `[[`, 3L + i))
        }
        names(plist) <- make.names(descr[4:length(descr)])

        res <- rbind(res, data.frame(Jul.Dates, Dates, plist,
                                     stringsAsFactors = FALSE))

        sD <- eD + 1L

        if (eD >= endDate)
            done <- TRUE
    }
    res
}

