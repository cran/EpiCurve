# ===========================================================================
# Convert a date from one format to another
# ===========================================================================
getFormat <- function(D) {

  if (length(grep("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", D)) > 0) {
    return ("hour")
  }

  if (length(grep("[0-9]{4}-[0-9]{2}-[0-9]{2}", D)) > 0) {
    return ("day")
  }

  if (length(grep("[0-9]{4}-W[0-9]{2}", D, perl=TRUE)) > 0) {
    return ("week")
  }

  if (length(grep("[0-9]{4}-[0-9]{2}", D)) > 0) {
    return ("month")
  }
  return("unknown")
}

date.convert <- function(x, from, to) {
  D <- x
  R = FALSE
  if(from == "day") {
    if (to == "week") {
      D <- ISOweek(D)
      R <- TRUE
    } else if (to == "month") {
      D <- as.character(as.Date(D), format="%Y-%m")
      R <- TRUE
    }
  } else if (from == "week") {
    if (to == "month") {
      D = as.character(ISOweek2date(paste(D, "-1", sep="")), format="%Y-%m")
      R = TRUE
    }
  }

  list(R, D)
}

# ===========================================================================
# Create an hourly * split sequence
# ===========================================================================
createSequence <- function(minTime, maxTime, split) {
  str_split = sprintf("%d hour", split)
  offset = sprintf("%02d%s", split-1, ":59:59")

  strMinTimeDateStart <-paste(substr(minTime, 1,10), "00:00:00", sep=" ")
  strMinTimeDateEnd <- as.character(as.timeDate(strMinTimeDateStart) + 3600*(split-0.999999))
  strMinTimeDateEnd <- paste(substr(strMinTimeDateEnd, 1,13), "59:59", sep=":")
  strMaxTimeDateEnd <-paste(substr(maxTime,1,10), "23:59:59", sep=" ")
  TLow <- seq(as.timeDate(strMinTimeDateStart), as.timeDate(strMaxTimeDateEnd), by=str_split)
  THight <- seq(as.timeDate(strMinTimeDateEnd), as.timeDate(strMaxTimeDateEnd), by=str_split)

  F1 <- format(TLow, "%d %H:%M")
  F2 <- format(THight, "%H:%M")
  Dico <- paste(F1, F2, sep="-")
  df <- data.frame(TLow, THight, Dico)
  colnames(df) <- c("L", "H", "D")
  df
}

setFactors <- function(df1, df2) {

  in.date <- function(TD, L, H) {
    if (TD >= L) {
      if (TD <= H) {
        return(TRUE)
      }
    }
    FALSE
  }

  getFactor <- function(x) {
    L <- as.character(df2$L)
    H <- as.character(df2$H)
    D <- df2$D
    R <- c()

    for (i in 1:length(x)) {
      found <- FALSE
      for (j in 1:nrow(df2)) {
        if (in.date(x[i], L[j], H[j]) == TRUE) {
          R <- c(R, levels(D)[j])
          found <- TRUE
          break
        }
      }
      if (found == FALSE) {
        MSG <- sprintf("Date unclassified %s", as.character(x[i]))
        stop(MSG)
      }
    }
    R
  }

  Date <- NULL
  df1 <- df1 %>% mutate(DateFactor = getFactor(as.POSIXct(Date)))
  df1
}

EpiCurve <- function(x,
                        date = NULL,
                        freq=NULL,
                        cutvar=NULL,
                        period = NULL,
                        to.period = NULL,
                        split = 1,
                        cutorder = NULL,
                        colors = NULL,
                        title = NULL,
                        xlabel = NULL,
                        ylabel=NULL,
                        note = NULL) {


  DF <- x
  .cutorder <- cutorder
  .color = colors

  # ---------------------------------------------------------------------------
  # Rename vars of DF
  # ---------------------------------------------------------------------------

  if (!is.null(date)) {
    names(DF)[names(DF)==date] <- "Date"
  } else {
    stop("Parameter 'date' is NULL!")
  }
  if (!is.null(cutvar)) {
    names(DF)[names(DF)==cutvar] <- "Cut"
  }
  if (!is.null(freq)) {
    names(DF)[names(DF)==freq] <- "Freq"
  }

  # ---------------------------------------------------------------------------


  if (is.null(.color)) {
    .color <- c("#ff0000", "#0000ff", "#00ff00")
  }

  if (period != "hour") {
    # ===========================================================================
    # If freq is NULL, data are not aggregated, so we aggregate all cases by
    # Date. The Date format MUST be %Y-%m-%d
    # If cutvar is NOT NULL, cases are aggregated by Date and Cut
    # After aggregation has been processed, dates are rewritten according to
    # the value of period ("week", "month")
    # ===========================================================================
    if (is.null(freq)) {
      if (is.null(cutvar)) {
        DF <- DF %>%
          group_by(Date) %>%
          summarise(Freq=n()) %>%
          as.data.frame()
      } else {
        DF <- DF %>%
          group_by(Date, Cut) %>%
          summarise(Freq=n()) %>%
          as.data.frame()
      }
      # -------------------------------------------------------------------------
      # rewrites Date and aggregate them again acording to 'period' value
      # -------------------------------------------------------------------------
      if (period == "week") {
        DF$Date <- ISOweek(DF$Date)
      }
      else if (period == "month") {
        DF$Date <-  strtrim(DF$Date, 7)
      }
      if (is.null(cutvar)) {
        DF <- DF %>%
          group_by(Date) %>%
          summarise(Freq = sum(Freq)) %>%
          as.data.frame()
      }
      else {
        DF <- DF %>%
          group_by(Date, Cut) %>%
          summarise(Freq = sum(Freq)) %>%
          as.data.frame()
      }
    } else {
      if (!is.null(to.period)) {
        ret <- date.convert(DF$Date, period, to.period)
        if (ret[[1]] == TRUE) {
          period <- to.period
          DF$Date <- ret[[2]]
          if (is.null(cutvar)) {
            DF <- DF %>%
              group_by(Date) %>%
              summarise(Freq=sum(Freq)) %>%
              as.data.frame()
          } else {
            DF <- DF %>%
              group_by(Date, Cut) %>%
              summarise(Freq=sum(Freq)) %>%
              as.data.frame()
          }
        }
      }
    }

    if (!is.null(date)) {
      names(DF)[names(DF)==date] <- "Date"
      if (period=="week") {
        DF$Date <- ISOweek2date(paste(DF$Date, "-1", sep=""))
      }
      else if(period == "month") {
        DF$Date <- as.Date(paste(DF$Date,"-01",sep=""))
      }
      else if (period == "day") {
        DF$Date <- as.Date(DF$Date)
      }
    }
  } # end if(period != "hour")



  if (!is.null(cutvar)) {
    names(DF)[names(DF)==cutvar] <- "Cut"
    if (!is.null(.cutorder)) {
      DF$Cut <- factor(DF$Cut, levels=cutorder, ordered=TRUE)
    }
    else {
      DF$Cut <- as.factor(DF$Cut)
      .cutorder <- levels(DF$Cut)
    }
  }
  else {
    DF$Cut <- rep("1 cas", length.out = nrow(DF))
  }

  # ===========================================================================
  # Compute max value of "Freq" after data aggregation
  # ===========================================================================
  if (period != "hour") {
    if (!is.null(cutvar)) {
      TMP <- DF %>%
        dplyr::group_by(Date) %>%
        dplyr::summarize(total=sum(Freq)) %>%
        as.data.frame()
      MaxValue = max(TMP$total)
    } else {
      MaxValue = max(DF$Freq)
    }
  }

  if (period == "day") {
    if (max(DF$Date) - min(DF$Date) > 365) {
      print("Error: Too much days, must be < 366")
      return(FALSE)
    }
    DW = data_frame(Date = seq(min(DF$Date), max(DF$Date), by="day"))
    DF <- dplyr::left_join(x = DW, y = DF, by = "Date") %>%
      as.data.frame()
    DF$Freq[is.na(DF$Freq)] <- 0
    DF <- mutate(DF, Day = format(Date, "%Y-%m-%d")) %>%
      mutate(Date = NULL) %>%
      mutate(Date = Day)
  }

    if (period == "week") {
      DW = data_frame(Date = seq(min(DF$Date), max(DF$Date), by="week"))
      DW$Date <- ISOweek(DW$Date)
      DF$Date <- ISOweek(DF$Date)
      DF <- dplyr::left_join(x = DW, y = DF, by = "Date") %>%
        as.data.frame()
      DF$Freq[is.na(DF$Freq)] <- 0

    }

    if (period == "month") {
      DM = data_frame(Date = seq(min(DF$Date), max(DF$Date), by="month"))
      DF <- dplyr::left_join(x = DM, y = DF, by = "Date") %>%
        as.data.frame()
      DF$Freq[is.na(DF$Freq)] <- 0
      DF <- mutate(DF, Mois = format(Date, "%Y-%m")) %>%
        mutate(Date = NULL) %>%
        mutate(Date = Mois)
    }
  # ===========================================================================
  # Hourly with or without splitting and with or without factor
  # ===========================================================================
  if (period == "hour") {
    DateFactor <- Date <- D <- NULL

    if (!(split %in% c(1,2,3,4,6,8,12))) {
      stop("split value MUST be in {1,2,3,4,6,8,12}")
    }

    # cat("DF origin:", nrow(DF), "\n")

    DF$Date <- as.character(as.timeDate(DF$Date))
    minDate <- as.character(min(as.timeDate(DF$Date)))
    maxDate <- as.character(max(as.timeDate(DF$Date)))

    L <- createSequence(minDate, maxDate, split)
    # cat("DF :", nrow(DF), "\n")
    DF <- setFactors(DF, L)
    # return(DF)
    L <- dplyr::rename(L, Date = D) %>%
      select(Date) %>%
      mutate(Date = levels(Date)) %>%
      as.data.frame()
    DF <- DF %>%
      group_by(DateFactor, Cut) %>%
      summarise(Freq=n()) %>%
      rename(Date = DateFactor)

    DF <- dplyr::left_join(x = L, y = DF, by = "Date") %>%
      mutate(Freq = replace(Freq, is.na(Freq), 0)) %>%
      as.data.frame()

    if (!is.null(cutvar)) {
      TMP <- DF %>%
        dplyr::group_by(Date) %>%
        dplyr::summarize(total=sum(Freq)) %>%
        as.data.frame()
      MaxValue = max(TMP$total)
    } else {
      MaxValue = max(DF$Freq, na.rm = TRUE)
    }
  }

  # Init pseudo variables (in AES) for packaging
  Date <- Freq <- Day <- Mois <- Cut <- NULL

  # ===========================================================================
  # Plot the epidemic curve
  # ===========================================================================
  P_ <- ggplot(arrange(DF, Cut), aes(x=Date, y=Freq, fill=factor(Cut)))
  P_ <- P_ +  geom_bar(stat='identity', width=1);

  P_ <- P_ + scale_fill_manual(values = .color, labels=.cutorder,
                               breaks=levels(DF$Cut), limits=levels(DF$Cut),
                               guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(breaks= pretty_breaks(ceiling(2*log2(MaxValue))), expand = c(0,0))

  P_ <- P_ + geom_hline(yintercept=seq(1, MaxValue, by=1), colour="white", size=0.3)

  if (nrow(DF) > 1) {
    P_ <- P_ + geom_vline(xintercept = seq(1.5, nrow(DF), by=1), colour="white", size=0.3)
  }
  note <- gsub('(.{1,90})(\\s|$)', '\\1\n', note)
  P_ <- P_ + xlab(paste(xlabel, note, sep="\n\n")) +
    ylab(ylabel) +
    labs(title = title, fill = "") +
    coord_fixed(ratio=1) +
    theme_bw() +
    labs(fill = cutvar) +
    theme(panel.border = element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.text.x = element_text(angle=90))+
    theme(axis.line.x = element_line(colour="black", linetype="solid", size = 0.5),
          axis.line.y = element_line(colour="black", linetype="solid", size = 0.5))
    P_
}

