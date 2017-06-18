
createSequence <- function(minTime, maxTime, split) {
  str_split = sprintf("%d hour", split)
  offset = sprintf("%02d%s", split-1, ":59:59")
  strMinDate = as.character(minTime)
  strMinTimeDateStart <-paste(strMinDate, "00:00", sep=" ")
  strMinTimeDateEnd <- paste(strMinDate, offset, sep=" ")
  strMaxDate = as.character(maxTime)
  strMaxTimeDateEnd <-paste(strMaxDate, "23:59:59", sep=" ")

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
    L <- df2$L
    H <- df2$H
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
  df1 <- df1 %>% mutate(DateFactor = getFactor(Date))
  df1
}

EpiCurve <- function(x,
                        date = NULL,
                        freq=NULL,
                        cutvar=NULL,
                        period = NULL,
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


  if (is.null(.color)) {
    .color <- rev(brewer.pal(9, "Spectral"))
  }


  if (!is.null(date)) {
    names(DF)[names(DF)==date] <- "Date"
    if (period=="week") {
      DF$Date <- ISOweek2date(paste(DF$Date, "-1", sep=""))
    }
    else if(period == "month") {
      DF$Date <- as.Date(paste(DF$Date,"-01",sep=""))
    }
    else if (period == "hour") {
      DF$Date <- as.POSIXct(DF$Date)
    }
    else {
      DF$Date <- as.Date(DF$Date)
    }
  }

  if (!is.null(freq)) {
    names(DF)[names(DF)==freq] <- "Freq"
  }

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


  # Calcul du max après agrégation
  if (!is.null(cutvar)) {
    TMP <- DF %>%
      dplyr::group_by(Date) %>%
      dplyr::summarize(total=sum(Freq)) %>%
      as.data.frame()
    MaxValue = max(TMP$total)
  } else {
    MaxValue = max(DF$Freq)
  }


  # Calcul du nombre de ticks 'lisibles'
  # TMP <- dplyr::distinct(DF, Date)
  # N = nrow(DF)
  # n_ticks = max(as.integer(log10(N)*10)-10, 1)

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

  # -------------------------------------------------------------------
  # Hourly with or without splitting
  # -------------------------------------------------------------------
  if (period == "hour") {
    DateFactor <- Date <- D <- NULL

    if (!(split %in% c(1,2,3,4,6,8,12))) {
      stop("split value MUST be in {1,2,3,4,6,8,12}")
    }
    minDate = min(as.Date(DF$Date))
    maxDate = max(as.Date(DF$Date))
    L <- createSequence(minDate, maxDate, split)
    DF <- setFactors(DF, L)
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

  P_ <- ggplot(arrange(DF, Cut), aes(x=Date, y=Freq, fill=factor(Cut)))
  P_ <- P_ +  geom_bar(stat='identity', width=1);

  P_ <- P_ + scale_fill_manual(values = .color, labels=.cutorder,
                               breaks=levels(DF$Cut), limits=levels(DF$Cut),
                               guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(breaks= pretty_breaks(), expand = c(0,0)) +

    geom_hline(yintercept=seq(1, MaxValue, by=1), colour="white", size=0.3)
  if (nrow(DF) > 1) {
    P_ <- P_ + geom_vline(xintercept = seq(1.5, nrow(DF), by=1), colour="white", size=0.3)
  }
  note <- gsub('(.{1,90})(\\s|$)', '\\1\n', note)
  P_ <- P_ + xlab(paste(xlabel, note, sep="\n\n")) +
    ylab(ylabel) +
    labs(title = title, fill = "") +
    coord_fixed(ratio=1) +
    theme_bw() +
    theme(panel.border = element_blank()) +
    theme(axis.text.x = element_text(angle=90))+
    theme(axis.line.x = element_line(colour="black", linetype="solid", size = 0.5),
          axis.line.y = element_line(colour="black", linetype="solid", size = 0.5))
    P_
}

