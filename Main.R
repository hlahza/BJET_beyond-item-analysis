#================================================================================================================================
#Load packages
#================================================================================================================================ 
library("tidyverse") ##
library("tidytext")
library("rlang")
library(purrr)
library("stringr")
library("lubridate") ##
#1--my data time global options
my_datetime.glopal.ptions <- options(digits.secs = 3) 

process.data <- function(exam.log, grades, to.eliminate.actions, to.eliminate.students){
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # this section uploads the data of the exam included in this study
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #================================================
  # upload data files                             |  
  #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  # ExamSoftLog.upload: exam event log                  
  # Grades.upload: students grades
  # ItemAnalysis: item analysis including 
  # difficulty and discrimination of each item
  #================================================
  ExamSoftLog.upload <- (read.csv(exam.log, stringsAsFactors = F))  
  Grades.upload <- (read.csv(grades, stringsAsFactors = F)) 
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # preliminary cleaning of the data
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #================================================
  # filter the data                               |  
  #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  # to.eliminate.actions: actions that should be                  
  # eliminated due to their negative effects on the 
  # accuracy of the calculation
  # to.eliminate.students: students that should be
  # considered outliers and deleted
  # seq: sequance
  # StdID: student de-identified id
  # next.snap: following event
  #================================================
  #1--update the names of the attributes and filter the data
  ExamSoftLog <- ExamSoftLog.upload %>% 
    filter(!(tolower(trigger) %in% to.eliminate.actions), !(X %in% to.eliminate.students)) %>% 
    mutate(seq = seq(X %>% length())) %>% 
    select(seq, StdID = X, everything())
  #1--convert the time to AEST time zone
  ExamSoftLog <- ExamSoftLog %>% 
    mutate(timestamp = as_datetime(timestamp, format = "%m/%d/%Y %H:%M:%OS", tz = "Australia/Brisbane"))
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # this function adds more variables to the even log                            |
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #================================================
  # adding duration and new sequence variable     |  
  #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  # time.spent: list that holds the time spent for                  
  # each event in the log.
  # visit.based.seq: updated sequence based on the 
  # visits. So, any events happened in the same 
  # visits will get the same sequence number
  # seq.counter: sequence counter
  # current.snap: current event
  # next.snap: following event
  #================================================
  duration.add <- function(examinee.log){
    time.spent <- list()
    visit.based.seq <- list()
    time.spent[[1]] <- 0
    seq.counter <- 1
    arrange(examinee.log, timestamp)
    #1--calculating the time for each event
    for (snapshot.seq in examinee.log[-1,"seq"]) {
      time.spent[[snapshot.seq]] <- as.numeric(ExamSoftLog[snapshot.seq, "timestamp"] - ExamSoftLog[snapshot.seq -1, "timestamp"], units="secs")
    }
    #1--visit.based.seq detection and addition 
    for (snapshot.seq in examinee.log$seq) {
      if(snapshot.seq == last(examinee.log$seq)){
        visit.based.seq[[snapshot.seq]] <- seq.counter
        break()
      }
      current.snap <- ExamSoftLog[snapshot.seq,]
      next.snap <- ExamSoftLog[snapshot.seq + 1,]
      if(current.snap$student_seq != next.snap$student_seq ){
        visit.based.seq[[snapshot.seq]] <- seq.counter
        seq.counter <- seq.counter + 1
      }else{
        visit.based.seq[[snapshot.seq]] <- seq.counter
      }
    }
    examinee.log$duration <- time.spent %>% unlist()
    examinee.log$visit.based.seq <- visit.based.seq %>% unlist()
    return(examinee.log)
  }
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # this function generates all the exam-taker/item level features               |
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  extract_feature <- function(examinee.log){
    #================================================
    #extracting last event of each visit            |  
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # idxs: indices of last event of each visit                  
    # items.selected: actual records of the events 
    # the algorithm simply looks at the sequence of 
    # the events. If two sequences are precedent to 
    # each other that means they are within one visit
    #================================================
    add_visit.num <- function(item){
      idxs <- which(c((item$seq[-1] - item$seq[-length(item$seq)]), 2) > 1)
      items.selected <- item[idxs,]
      items.selected[,"visit"] <- c(1:length(idxs))
      return(items.selected)
    }
    #================================================
    #detecting initial and final selections         |  
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # fr: final selection                   
    # calc: calculation of the feature           
    # idxs: indices of the events that contain the 
    # final selection 
    #
    # idx.fr: index of the first event that contain 
    # the final response
    # idx.ir: index of the first selection
    #
    # detect.final.response: this function return the
    # first event that contain the final selection
    #
    # detect.initial.response: this function return 
    # the first selection
    #================================================
    detect.final.response <- function(item){
      item$seq.local <- c(1:nrow(item))
      fr <- item %>%
        filter(response != "") %>%
        pull(response) %>% 
        last()
      idxs <- item %>%
        filter(response == fr) %>%
        pull(seq.local)
      idx.fr <- 0
      if(length(idxs) != 0 && !is.na(idxs)){
        idx.fr <- which(c((idxs[-1] - idxs[-length(idxs)])) > 1)
        if(length(idx.fr) != 0){
          idx.fr <- idxs[idxs > last(idxs[idx.fr])] %>% first()
        }else{
          idx.fr <- first(idxs)
        }
      }
      return(idx.fr)
    }
    detect.initial.response <- function(item){
      item$seq.local <- c(1:nrow(item))
      idx.ir <- item %>% 
        filter(response != "") %>% 
        pull(seq.local) %>% 
        first()
      return(idx.ir)
    }
    #================================================
    #frequency based features
    #================================================
    #````````````
    #visit-independent features
    #````````````
    #================================================
    # item action features                          |  
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # simply, this function just sum up the number of                  
    # events received by an item
    #================================================
    calc.IA.beaviour <- function(item){
      result <- data.frame(StdID = first(item$StdID), 
                           item_num = first(item$item_num), 
                           value = nrow(item), 
                           feature = "IA")
      return(result)
    }
    #================================================
    # answer changing                         |  
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # response.prev: all responses except the first                
    # response.next: all responses except the last
    # calc: the calculation of the feature
    #================================================
    calc.AC.beaviour <- function(item){
      item.number <- first(item$item_num)
      responses <- (item$response)[item$response != ""]
      calc <- 0
      if(length(responses) > 1){
        if(prod(responses == first(responses)) != 1){
          #1--this vector holds all the responses except 
          #2--the first response
          response.prev <- (responses[])[-length(responses)]
          response.next <- (responses[])[-1]
          comparisons.responses <- (response.prev == response.next) == F
          calc <- sum(comparisons.responses) 
        }
      }
      result <- data.frame(StdID = first(item$StdID), 
                           item_num = item.number, 
                           value = calc, 
                           feature = "AC")
      return(result)
    }
    #````````````
    #visits based features
    #````````````
    #================================================
    # item visits                                   |  
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # this function simply calculate the number of 
    # visits
    #================================================
    calc.IV.beaviour <- function(item){
      result <- data.frame(StdID = first(item$StdID), 
                           item_num = first(item$item_num), 
                           value = last(item$visit), 
                           feature = "IV")
      return(result)
    }
    #================================================
    # item visits before initial selection          |  
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # idx.ir: initial selection  
    # calc: calculation of the feature
    #================================================
    calc.IVBIS.beaviour <- function(item){
      item.number <- first(item$item_num)
      calc <- 0
      idx.ir <- detect.initial.response(item)
      if(length(idx.ir) != 0 && !is.na(idx.ir)){
        calc <- idx.ir - 1
      }
      result <- data.frame(StdID = first(item$StdID), 
                           item_num = item.number, 
                           value = calc, 
                           feature = "IVBIS")
      return(result)
    }
    #================================================
    # item visit following initial selection         |  
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # idx.ir: initial selection  
    # calc: calculation of the feature
    #================================================
    calc.IVFIS.beaviour <- function(item){
      item.number <- first(item$item_num)
      calc <- 0
      idx.ir <- detect.initial.response(item)
      if(length(idx.ir) != 0 && !is.na(idx.ir)){
        calc <- nrow(item) - idx.ir
      }
      result <- data.frame(StdID = first(item$StdID), 
                           item_num = item.number, 
                           value = calc, 
                           feature = "IVFIS")
      
      return(result)
    }
    #================================================
    # item visits before final selection             |  
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # idx.fr: final selection that was submitted
    # calc: calculation of the feature
    #================================================
    calc.IVBFS.beaviour <- function(item){
      item.number <- first(item$item_num)
      calc <- 0
      idx.fr <- detect.final.response(item)
      if(idx.fr != 0){
        calc <-  idx.fr - 1
      }
      result <- data.frame(StdID = first(item$StdID), 
                           item_num = item.number, 
                           value = calc, 
                           feature = "IVBFS")
      return(result)
    }
    #================================================
    # item visit following final selection           |  
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # idx.fr: final selection that was submitted
    # calc: calculation of the feature
    #================================================
    calc.IVFFS.beaviour <- function(item){
      item.number <- first(item$item_num)
      calc <- 0
      idx.fr <- detect.final.response(item)
      if(idx.fr != 0){
        calc <- nrow(item) - idx.fr
      }
      result <- data.frame(StdID = first(item$StdID), 
                           item_num = item.number, 
                           value = calc, 
                           feature = "IVFFS")
      return(result)
    }
    #================================================
    # time based features
    #================================================
    #================================================
    # time for answering                            |  
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # calc: calculation of the time 
    # idx.fr: final response index 
    #================================================
    calc.AT.beaviour <- function(item){
      item.number <- first(item$item_num)
      calc <- 0
      idx.fr <- detect.final.response(item)
      if(idx.fr != 0){
        calc <- item$duration[1:idx.fr] %>% sum()
      }
      result <- data.frame(StdID = first(item$StdID), 
                           item_num = item.number, 
                           value = calc, 
                           feature = "AT")
      return(result)
    }
    #================================================
    # review time following initial selection       |  
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # calc: calculation of the time 
    # idx.ir: initial response index 
    #================================================
    calc.RTFIS.beaviour <- function(item){
      item.number <- first(item$item_num)
      calc <- 0
      idx.ir <- detect.initial.response(item)
      if(length(idx.ir) != 0 && !is.na(idx.ir)){
        if(idx.ir + 1 <= nrow(item)){
          calc <- item$duration[(idx.ir + 1):nrow(item)] %>% sum()
        }
      }
      result <- data.frame(StdID = first(item$StdID), 
                           item_num = item.number, 
                           value = calc, 
                           feature = "RTFIS")
      
      return(result)
    }
    #================================================
    # review time before final selection            |  
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # calc: calculation of the time
    # idx.fr: final response index
    #================================================
    calc.RTBFS.beaviour <- function(item){
      item.number <- first(item$item_num)
      calc <- 0
      if(nrow(item) > 1){
        idx.ir <- detect.initial.response(item)
        if(length(idx.ir) != 0 && !is.na(idx.ir)){
          idx.fr <- detect.final.response(item)
          if(idx.fr != 0){
            if(idx.ir != idx.fr){
              calc <- item$duration[(idx.ir + 1):idx.fr] %>% sum()
            }
          }
        }
      }
      result <- data.frame(StdID = first(item$StdID), 
                           item_num = item.number, 
                           value = calc, 
                           feature = "RTBFS")
      return(result)
    }
    #================================================
    # review time following final selection         |  
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # calc: calculation of the time 
    # idx.fr: final response index 
    #================================================
    calc.RTFFS.beaviour <- function(item){
      item.number <- first(item$item_num)
      calc <- 0
      if(nrow(item) > 1){
        idx.fr <- detect.final.response(item)
        if(idx.fr != 0 && !is.na(item[idx.fr+1,])){
          calc <- item$duration[(idx.fr+1):nrow(item)] %>% sum()
        }
      }
      result <- data.frame(StdID = first(item$StdID), 
                           item_num = item.number, 
                           value = calc, 
                           feature = "RTFFS")
      return(result)
    }
    #================================================
    # function calls
    #================================================
    #================================================
    # two lists that hold feature functions and the
    # resultant feature data
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # visit.dependent: features that depend on the 
    # on the visit to be calculated
    # visit.independent: features that do not depend
    # on the visits to be calculated
    #================================================
    feature.fun.list <- list(visit.dependent = list(calc.IV.beaviour = calc.IV.beaviour,
                                                    calc.IVBIS.beaviour = calc.IVBIS.beaviour,
                                                    calc.IVFIS.beaviour = calc.IVFIS.beaviour,
                                                    calc.IVBFS.beaviour = calc.IVBFS.beaviour,
                                                    calc.IVFFS.beaviour = calc.IVFFS.beaviour),
                             visit.independent = list(calc.IA.beaviour = calc.IA.beaviour,
                                                      calc.AC.beaviour = calc.AC.beaviour,
                                                      calc.AT.beaviour = calc.AT.beaviour,
                                                      calc.RTFIS.beaviour = calc.RTFIS.beaviour,
                                                      calc.RTBFS.beaviour = calc.RTBFS.beaviour,
                                                      calc.RTFFS.beaviour = calc.RTFFS.beaviour))
    feature.data <- list(calc.IV.beaviour = data.frame(),
                         calc.IVBIS.beaviour = data.frame(),
                         calc.IVFIS.beaviour = data.frame(),
                         calc.IVBFS.beaviour = data.frame(),
                         calc.IVFFS.beaviour = data.frame(),
                         calc.IA.beaviour = data.frame(),
                         calc.AC.beaviour = data.frame(),
                         calc.AT.beaviour = data.frame(),
                         calc.RTFIS.beaviour = data.frame(),
                         calc.RTBFS.beaviour = data.frame(),
                         calc.RTFFS.beaviour = data.frame())
    #================================================
    # calling all visit.dependent features
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # for each exam taker, each item/exam-taker 
    # feature is calculated
    #================================================
    for(feature in names(feature.fun.list$visit.independent)){
      feature.data[[feature]] <- examinee.log %>%
        split(.$item_num) %>%
        purrr::map(~feature.fun.list$visit.independent[[feature]](item = .)) %>%
        bind_rows()
    }
    #================================================
    # calling all visit.independent features
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # for each exam taker, each item/exam-taker 
    # feature is calculated
    #================================================
    for(feature in names(feature.fun.list$visit.dependent)){
      feature.data[[feature]] <- examinee.log %>%
        split(.$item_num) %>%
        purrr::map(~feature.fun.list$visit.dependent[[feature]](item = add_visit.num(.))) %>%
        bind_rows()
    }
    return(bind_rows(feature.data))
  }
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # this function generates diff-f and disc-f                                    |
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  feature.based.item.analysis <- function(examinee.item.features){
    #================================================
    # difficulty-based features (diff-f)
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # diff.f: difficulty-based features. 
    # I calculate the features by aggregating the
    # exam-taker/item pair features and averaging 
    # them (devide by number of students)
    #================================================
    diff.f <- list()
    number.examinee <- n_distinct(examinee.item.features$StdID)
    for(f in unique(examinee.item.features$feature)){
      diff.f[[f]] <- as.data.frame(examinee.item.features %>% 
                                     filter(feature == f) %>% 
                                     group_by(item_num) %>% 
                                     summarise(diff.f = sum(value)/number.examinee))
    }
    #================================================
    # discrimination-based features (disc-f)
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # disc-f: discrimination-based features. 
    # I calculate the features by subtracting 
    # exam-taker/item pair features of top 27% from
    # bottom 27%
    #================================================
    disc.f <- list()
    top.bottom <- list(
      top.features = examinee.item.features %>% 
        filter(StdID %in% (Grades.upload %>%
                             arrange(desc(Grade)) %>% 
                             dplyr::slice_head(prop = .27) %>% 
                             pull(Identifier))),
      bottom.features = examinee.item.features %>% 
        filter(StdID %in% (Grades.upload %>% 
                             arrange(Grade) %>% 
                             dplyr::slice_head(prop = .27) %>% 
                             pull(Identifier)))
    )
    for(f in names(diff.f)){
      diff.f.top <- as.data.frame(top.bottom$top.features %>% 
                                    filter(feature == f) %>% 
                                    group_by(item_num) %>% 
                                    summarise(diff.f = sum(value)/number.examinee))
      diff.f.bottom <- as.data.frame(top.bottom$bottom.features %>% 
                                       filter(feature == f) %>% 
                                       group_by(item_num) %>% 
                                       summarise(diff.f = sum(value)/number.examinee))
      disc.f[[f]] <- diff.f.top %>% 
        select(everything(), diff.f.t = diff.f) %>% 
        left_join(diff.f.bottom %>% select(everything(), diff.f.b = diff.f), by = "item_num") %>% 
        mutate(disc.f = diff.f.t - diff.f.b)
    }
    return(list(diff.f = diff.f, disc.f = disc.f))
  }
  #================================================
  # function calls
  #================================================
  #================================================
  # three function calls
  #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  # log.with.duration: adding the duration to each
  # event and sequence based on visits
  # on the visit to be calculated
  # examinee.item.features: return exam-taker/item 
  # pair features
  # diff.disc.f: return feature-based item analysis
  #================================================
  log.with.duration <- ExamSoftLog %>% 
    split(.$StdID) %>% 
    purrr::map(~duration.add(examinee.log = .)) %>% 
    bind_rows()
  examinee.item.features <- log.with.duration %>% 
    split(.$StdID) %>% 
    purrr::map(~extract_feature(examinee.log = .)) %>% 
    bind_rows()
  diff.disc.f <- feature.based.item.analysis(examinee.item.features)
  output <- list(examinee.item.features = examinee.item.features, 
                 item.level.features = diff.disc.f)
  
  return(output)
}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# program: running the code                                                   |
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#================================================
# three function calls
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# exam.log: the path of the log file. it should 
# be csv file format
# grades: the path of the file that contain the  
# grades. it should be csv file format
# item.analysis: the path of the file that 
# contain the item analysis (diff.I and disc.I)
# to.eliminate.actions: actions that the user
# want to eliminate
# to.eliminate.students: examinees that the user
# want to eliminate
#================================================
exam.features <- process.data(exam.log = "ExamSoftLog.csv", 
                              grades = "Grades.csv",
                              to.eliminate.actions = c("final", "final2"),
                              to.eliminate.students = c(""))