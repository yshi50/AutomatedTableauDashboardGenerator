#' @name Template
#' @param abk_0 ABK Dataframe, or Path (SAS or CSV)
#' @param adsl_0 ADSL Dataframe, or Path (SAS or CSV), Some Situations Allow for Optional
#' @param save_path Path to Output Dashboard, Default getwd()
#' @param UID Unique ID to Identify Subjects Shared Between ABK and ADSL, Optional, Default "SUBJID"
#' @param study_name Study Name, Optional
#' @param special_name Special Name to Rename Dashboard, Optional
#' @param about Study Information, Optional
#' @param URL Website URL, 'http' Not Supported, Optional
#' @param log_fold_change Log-n Fold Change
#' @param reference_line Default Reference Line Value, Optional
#' @param note Creator Message
#' @return
#' @examples
#' @export

Template = function(abk_0, adsl_0 = NA, definition_0 = NA, save_path = getwd(),
                    UID = "SUBJID", study_name = NA, special_name = NA, about = NA, URL = NA,
                    distinct_threshold = 4, log_fold_change = NA,
                    variable_numerical = NA, variable_categorical = NA, variable_keep = c("COUNTRY"),
                    generate_correlation = FALSE,
                    debug_mode = TRUE, save_RDS = FALSE, trash_can = FALSE, connection = "Live",
                    reference_line = NA, note = "No Message", shiny = FALSE){
  
  #####
  cat("\n")
  save_path = file.path(save_path, "GTableauPipeline")
  total_time <- Sys.time()
  console = c()
  
  {
    # Read ABK if Given String Path
    if (typeof(abk_0) == "character")
    {
      abk_path = abk_0
      
      console = console_collect(console, paste0("ABK Path Provided: ", abk_path))
      
      extension <- tools::file_ext(abk_path)
      
      if (extension == "csv") {
        abk_0 = read_csv(abk_path)[, -1]
      } else if (extension == "sas") {
        abk_0 = read_sas(abk_path)
      }
      
    }
    
    colnames(abk_0) = toupper(colnames(abk_0))
    
    # Check if ADSL Not Given Then Create ADSL
    if ((length(adsl_0) == 1 && is.na(adsl_0))){
      console = console_collect(console, "ADSL Not Provided")
      if (!(UID %in% colnames(abk_0)))
        stop("UID Not Found in ABK")
      adsl_0 = abk_0 %>% select(UID) %>% distinct()
    }
    
    # Read ADSL if Given String Path
    if (typeof(adsl_0) == "character")
    {
      adsl_path = adsl_0
      console = console_collect(console, paste0("ADSL(bmPRO) Path Provided: ", adsl_path))
      
      extension <- tools::file_ext(adsl_path)
      
      if (extension == "csv") {
        adsl_0 = read_csv(adsl_path)[, -1]
      } else if (extension == "sas") {
        adsl_0 = read_sas(adsl_path)
      }
    }
    
    if (nrow(adsl_0) == 0){
      console = console_collect(console, "ADSL Is Empty")
      if (!(UID %in% colnames(abk_0)))
        stop("UID Not Found in ABK")
      adsl_0 = abk_0 %>% select(UID) %>% distinct()
    }

    colnames(adsl_0) = toupper(colnames(adsl_0))
  }
  
  # Delete Duplicated Columns
  {
    abk_0 = abk_0[!duplicated(colnames(abk_0))]
    adsl_0 = adsl_0[!duplicated(colnames(adsl_0))]
  }
  
  # Check UID Existence in Both ABK and ADSL
  {
    if (!(UID %in% colnames(adsl_0))){
      stop("UID Not Found in ADSL")
    }
    
  }
  if (!(UID %in% colnames(abk_0))){
    stop("UID Not Found in ABK")
  }
  
  # UID Characterized
  {
    abk_0 = abk_0 %>% mutate(across(all_of(UID), as.character))
    adsl_0 = adsl_0 %>% mutate(across(all_of(UID), as.character))
  }
  
  # No Study Name Input
  {
    if (!is.na(special_name))
      study_name = special_name
    
    if (is.na(study_name)){
      if ("STUDYID" %in% colnames(abk_0))
        study_name = unique(abk_0$STUDYID)
        if (length(study_name) != 1)
          study_name = NA
    }
    
    if (is.na(study_name)){
      if (any(c("SUBJID", "USUBJID") %in% colnames(abk_0))){
        study_name <- abk_0 %>% mutate(Temp = sub("-$", "", str_replace(USUBJID, SUBJID, ""))) %>% select(Temp) %>% 
          distinct() %>% unlist()
      }
      
      if (length(study_name) == 1){
        if (nchar(study_name) > 1){ # Example: GS-US-123-4567
          study_name = unique(substr(study_name, 1, 14))
        }
      } else {
        study_name = NA
      }
    }
    
    if (is.na(study_name))
      readline("Please Input Study Name: ")
    
    if (is.na(study_name))
      stop("Study Name Not Found")
    else{
      console = console_collect(console, paste0("Study Name: ", study_name))
    }

  }
  
  # Create Folder and Update Path
  {
    # Delete Existed Folder for Zip
    if (file.exists(save_path))
      unlink(save_path, recursive = TRUE)
    dir.create(save_path)
    save_path = paste0(file.path(paste0(save_path, "/", study_name)), "/")
    dir.create(save_path)
    
    file.copy(from = system.file("Images", package = "GTableauPipeline"), to = save_path,
              overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)

    log_path = paste0(save_path, "Log.txt")
    log_clear(log_path = log_path)
    
    console = console_collect(console, paste0("Save to Path: ", save_path))
  }
  
  # Check ADSL
  if (nrow(adsl_0) != 0) {
    if (nrow(adsl_0) != nrow(adsl_0 %>% select(all_of(UID)) %>% distinct()))
      log_stop("ADSL Duplicated in Subject Level", log_path = log_path)
  }
  
  {
    # Check TST, TSTLBL, TSTCD
    assay_exist_0 = intersect(c("TST", "TSTLBL", "TSTCD"), colnames(abk_0))
    
    if (length(assay_exist_0) == 0){ # Without Both TST, TSTLBL, TSTCD Report Error
      
      log_stop("Assay Naming Style Not Found, Need TST, TSTLBL, TSTCD", log_path = log_path)
      
    } else if (length(assay_exist_0) != 3){ # Missing Any One of Them Still Work
      
      # Find What Missing
      assay_exist_1 = setdiff(c("TST", "TSTLBL", "TSTCD"), assay_exist_0)
      assay_exist_2 = assay_exist_0[1]
      
      log_warning(paste0("Assay Naming Style Only Found ", paste(assay_exist_0, collapse = ", "),
                         ", Need ", paste(assay_exist_1, collapse = ", "),
                         ", Pipeline Will Generate (May Not Be Accurate)"), log_path = log_path)
      
      for (i in assay_exist_1)
        abk_0 = abk_0 %>% mutate(!!sym(i) := abk_0[, assay_exist_2])
    }
    
    assay_exist_3 = assay_exist_0
    assay_name_distinct = abk_0 %>% select(TST, TSTLBL, TSTCD) %>% distinct()
    assay_name_duplicated = assay_name_distinct %>% mutate(across(everything(), ~duplicated(.)))
    assay_name_overuse = assay_name_duplicated %>% colSums()
    
    if (sum(assay_name_overuse) != 0){
      log_warning("Assay Name Overused", log_path = log_path)
      
      assay_name_column = names(assay_name_overuse[assay_name_overuse != 0])
      assay_name_all = c()
      
      for (i in assay_name_column){
        
        assay_name_warning = assay_name_distinct[assay_name_duplicated %>% select(all_of(i)) %>% unlist(), ]
        assay_name_all = rbind(assay_name_all, head(assay_name_warning, 5))
        
      }
      colnames(assay_name_all) = paste(colnames(assay_name_all), ifelse(assay_name_overuse != 0, "✖", "✔"))
      log_table(assay_name_all, log_path = log_path)
    }
  }
  
  # Get Definition
  if (nrow(adsl_0) != 0){
    if (all(is.na(definition_0))){
      definition_0 = define(adsl_0)
    } else if (typeof(definition_0) == "character") {
      definition_path = definition_0
      
      console = console_collect(console, paste0("Definition Path Provided: ", definition_path))
      definition_0 = define(read_sas(definition_path)) %>% filter(Variable %in% colnames(adsl_0))
    } else {
      console = console_collect(console, paste0("Definition Provided as Dataframe"))
      # definition_0 = define(definition_0) %>% filter(Variable %in% colnames(adsl_0))
    }
    
    if (nrow(definition_0) == 0){
      log_warning("Definition Not Found", log_path = log_path)
    }
    
    if (any(duplicated(definition_0$Description))){
      
      log_warning("Definition Duplicated", log_path = log_path)
      
      definition_warning = definition_0 %>%
        filter(Description %in% definition_0$Description[duplicated(definition_0$Description)])
      colnames(definition_warning) = paste(colnames(definition_warning), c("✔", "✖"))
      log_table(definition_warning, log_path = log_path)
      
      # Add Index to the Same Definition to Differentiate
      definition_1 = definition_0 %>%
        mutate(Description = ifelse(duplicated(Description) | duplicated(Description, fromLast = TRUE),
                                    paste0(Description, " (", Variable, ")"), Description)
        )
    } else
      definition_1 = definition_0
    
    # Delete Error Characters
    definition_2 = definition_1 %>%
      mutate(Description = gsub("'", "", Description)) %>%
      mutate(Description = gsub("< ?|> ?", "", Description)) %>%
      mutate(Description = gsub("\\s*\\(\\s*([^)]+?)\\s*\\)", " (\\1)", Description))
  }
  
  # Check Definition Format
  {
    definition_3 = definition_2 %>% mutate(Description = gsub("(^|\\s)BL(\\s|$)", "\\1Baseline\\2", Description)) %>%
      mutate(Description = gsub("(^|\\s)Grp(\\s|$)", "\\1Group\\2", Description)) %>%
      mutate(Description = gsub("(^|\\s)wk(\\s|$)", "\\1week\\2", Description))
  }
  
  # Check ABK Format
  {
    # Check VISN
    if (!"VISN" %in% colnames(abk_0))
      log_stop("VISN Is Missing", log_path = log_path)
    
    # Check TP
    if (!"TP" %in% colnames(abk_0)){
      log_warning("TP Is Missing, Pipeline Will Generate (May Not Be Accurate)", log_path = log_path)
      abk_0 = abk_0 %>% mutate(TP = "")
    }
    
    # Check TPN
    if (!"TPN" %in% colnames(abk_0)){
      log_warning("TPN Is Missing, Pipeline Will Generate (May Not Be Accurate)", log_path = log_path)
    }
    
    if (length(unique(abk_0$TP)) == 1){
      abk_0$TP = "Not Available"
      abk_0$TPN = 9999
    }
    
    # Update This
    if (9999 %in% abk_0$TPN)
      abk_0$TP[abk_0$TPN == 9999] = "Not Available"
    
    if (NA %in% abk_0$TPN)
      log_warning("TPN Contains NA", log_path = log_path)
    
    {
      check_TP <- abk_0 %>% select(TP, TPN) %>%
        distinct() %>% select(TP) %>% duplicated() %>% any()
      
      check_TPN <- abk_0 %>% select(TP, TPN) %>%
        distinct() %>% select(TPN) %>% duplicated() %>% any()
      
      if (check_TP | check_TPN)
        log_stop("TP and TPN Are Not One-to-One", log_path = log_path)
      }
    
    # Check TPN
    if (all(is.na(as.numeric(unique(abk_0$TPN)))))
      log_stop("TPN Is Not Numeric", log_path = log_path)
    
    # Check TRTN
    if (!"TRTN" %in% colnames(abk_0)){
      log_warning("ABK Is Missing TRTN, Pipeline Will Generate (May Not Be Accurate)", log_path = log_path)
      
      abk_0 = abk_0 %>% left_join(data.frame(TRT = unique(abk_0$TRT), TRTN = 1:length(unique(abk_0$TRT))), by = "TRT")
    }
    
    abk_0$TRT = gsub("_", " ", abk_0$TRT)
    abk_0$VIS = gsub("_", " ", abk_0$VIS)
    abk_0$TP = gsub("_", " ", abk_0$TP)
    
    abk_0$VISN = as.numeric(abk_0$VISN)
    abk_0$TPN = as.numeric(abk_0$TPN)
  }
  
  if (nrow(adsl_0) != 0){
    # Check if Subjects in ABK Are Subset of Subjects in ADSL
    abk_unique = abk_0 %>% select(all_of(UID)) %>% distinct() %>% unlist()
    adsl_unique = adsl_0 %>% select(all_of(UID)) %>% distinct() %>% unlist()
    
    if (!all(abk_unique %in% adsl_unique)){
      log_warning("UID in ABK Does Not Match UID in ADSL Due to Missing Subjects or Different Formating",
                  log_path = log_path)
      
      adsl_0 = bind_rows(adsl_0, setNames(data.frame(unname(abk_unique[!abk_unique %in% adsl_unique])), UID))
    }
  }
  
  # Check ARESULT
  {
    if (!"ARESULT" %in% colnames(abk_0))
      log_stop(paste0("Missing ARESULT"), log_path = log_path)
    else
      abk_0 = abk_0 %>% mutate(ARESULT = as.numeric(ARESULT))
  }
  
  # Compute BASEVAL, CHGVAL, PCHGVAL, FCHGVAL if Not Found
  {
    if (!"BASEVAL" %in% colnames(abk_0)){
      
      console = console_collect(console, "Baseline Value Not Found, Pipeline Will Generate (May Not Be Accurate)")
      
      if (!"VISN" %in% colnames(abk_0)){
        
        VIS_0 = abk_0 %>% select(VIS) %>% distinct() %>% arrange(VIS) %>% unlist()
        
        for (i in 1:length(VIS_0)){
          console = console_collect(console, paste0(i, ": ", VIS_0[i], "\n"))
        }
        
        VIS_1 = VIS_0[as.integer(readline("Type Number to Select Baseline Visit: "))]
        cat("\n")
        
      } else {
        
        VIS_1 = abk_0 %>% select(VIS, VISN) %>% distinct() %>% arrange(VISN) %>% pull(VIS) %>% .[1]
        
        console = console_collect(console, paste0("Selected ", VIS_1, " as Baseline Visit"))
        
      }
      
      if (!"TPN" %in% colnames(abk_0)){
        
        TP_0 = abk_0 %>% select(TP) %>% distinct() %>% arrange(TP) %>% unlist()
        
        for (i in 1:length(TP_0)){
          console = console_collect(console, paste0(i, ": ", TP_0[i], "\n"))
          
        }
        
        TP_1 = TP_0[as.integer(readline("Type Number to Select Baseline Time Point: "))]
        cat("\n")
        
      } else {
        
        TP_1 = abk_0 %>% select(TP, TPN) %>% distinct() %>% arrange(TPN) %>% pull(TP) %>% .[1]
        
        console = console_collect(console, paste0("Selected ", TP_1, " as Baseline Time Point"))
      }
      
      temp = abk_0 %>% filter(VIS == VIS_1, TP == TP_1) %>% select(all_of(UID), ARESULT, assay_exist_3) %>%
        rename(BASEVAL = ARESULT) %>% unique()
      abk_0 = abk_0 %>% left_join(temp, by = c(UID, assay_exist_3))
      
    }
    else
      abk_0 = abk_0 %>% mutate(BASEVAL = as.numeric(BASEVAL))
    
    if (!"CHGVAL" %in% colnames(abk_0)){
      
      console = console_collect(console, "Absolute Change from Baseline Not Found, Pipeline Will Generate (May Not Be Accurate)")
      
      abk_0 = abk_0 %>% mutate(CHGVAL = ARESULT - BASEVAL)
    }
    
    else
      abk_0 = abk_0 %>% mutate(CHGVAL = as.numeric(CHGVAL))
    
    if (!"PCHGVAL" %in% colnames(abk_0)){
      
      console = console_collect(console,
                                "Percentage Change from Baseline Value Not Found, Pipeline Will Generate (May Not Be Accurate)")
      
      abk_0 = abk_0 %>% mutate(PCHGVAL = CHGVAL / BASEVAL * 100)
    }
    else
      abk_0 = abk_0 %>% mutate(PCHGVAL = as.numeric(PCHGVAL))
    
    if (!"FCHGVAL" %in% colnames(abk_0)){
      
      console = console_collect(console, "Fold Change from Baseline Not Found, Pipeline Will Generate (May Not Be Accurate)")
      
      abk_0 = abk_0 %>% mutate(FCHGVAL = ARESULT / BASEVAL)
    }
    
    else
      abk_0 = abk_0 %>% mutate(FCHGVAL = as.numeric(FCHGVAL))
    
    if (!"LOG2CHG" %in% colnames(abk_0)){
      
      console = console_collect(console,
                                "Log-2 Fold Change from Baseline Not Found, Pipeline Will Generate (May Not Be Accurate)")
      
      abk_0 = abk_0 %>% mutate(LOG2CHG = log2(FCHGVAL))
    }
    
    else
      abk_0 = abk_0 %>% mutate(LOG2CHG = as.numeric(LOG2CHG))
  }
  
  # Check Whether Categorical and Numerical Variables All Exist in ADSL When User Input
  {
    if (!all(is.na(variable_numerical))){
      if (!all(variable_numerical %in% colnames(adsl_0))){
        log_stop("Not All User Input Numerical Variables Exist in ADSL", log_path = log_path)
      }
    }
    
    if (!all(is.na(variable_categorical))){
      if (!all(variable_categorical %in% colnames(adsl_0))){
        log_stop("Not All User Input Categorical Variables Exist in ADSL", log_path = log_path)
      }
    }
  }
  
  {
    abk_1 = abk_0
    
    if (!"COUNTRY" %in% colnames(adsl_0))
      adsl_1 = adsl_0 %>% mutate(COUNTRY = "Not Available")
    else
      adsl_1 = adsl_0
  }
  
  # Merge ADSL into ABK
  # Check for Common Columns between ABK and ADSL
  column_common = data.frame(column = intersect(colnames(abk_1), colnames(adsl_1))) %>% filter(!column %in% UID)
  
  # Remove Common Columns
  {
    if (nrow(column_common) != 0){
      column_common = column_common %>% unlist() %>% as.character()
      abk_2 <- abk_1 %>% select(-all_of(column_common))
    } else
      abk_2 <- abk_1
  }
  
  #####
  console = console_collect(console, "Detecting Variable Types...")
  start_time <- Sys.time()
  
  c(adsl_2, waste, definition_4) := column_subset(adsl_1, UID, variable_numerical, variable_categorical,
                                                  variable_keep, definition_3)
  
  console = console_collect(console, paste0("Run Time: ",
                                           as.character(difftime(Sys.time(), start_time, unit = "secs")),
                                           " Seconds"))
  
  # Compute Log Fold Change if User Asked
  {
    if (!is.na(log_fold_change)){
      abk_3 = abk_2 %>% mutate(FCHGVAL = log(FCHGVAL, log_fold_change)) # Reuse
      log_fold_change = paste0("Log-", log_fold_change, " Fold Change from Baseline")
    } else {
      abk_3 = abk_2
      log_fold_change = "Fold Change from Baseline"
    }
  }
  
  # Pivot ABK into Long Format
  abk_4 <- abk_3 %>% pivot_longer(cols = all_of(c("ARESULT", "CHGVAL", "PCHGVAL", "FCHGVAL", "LOG2CHG")),
                                  names_to = "Measurement",
                                  values_to = "Measurement Value") %>%
    mutate(Measurement = recode(Measurement, "ARESULT" = "Raw Value", "CHGVAL" = "Absolute Change from Baseline",
                                "PCHGVAL" = "Percentage Change from Baseline", "FCHGVAL" = log_fold_change,
                                "LOG2CHG" = "Log-2 Fold Change from Baseline"))
  
  # Original Columns in ABK
  variable_old = colnames(abk_4)
  
  # ADSL Merge into ABK by UID
  abk_5 = abk_4 %>% inner_join(adsl_2, by = all_of(UID))
  
  console = console_collect(console, "Filtering Variables...")
  start_time <- Sys.time()
  
  c(variable_clinical, variable_threshold) := type_variable(abk_5, distinct_threshold, waste[[1]], waste[[2]], waste[[6]],
                                                            waste[[7]], variable_keep)
  # Remove Factor from Categorical Variables
  variable_clinical[[2]] = data.frame(Variable = variable_clinical[[2]][!unlist(variable_clinical[[2]]) %in% waste[[9]], ])
  
  variable_threshold = all_of(variable_threshold[!variable_threshold %in% waste[[9]]])
  
  abk_6 = abk_5 %>% select(all_of(c(variable_old, variable_clinical[[1]] %>% select(Variable) %>% unlist() %>% unname(),
                                    variable_clinical[[2]] %>% select(Variable) %>% unlist() %>% unname())))
  # %>% mutate_at(vars(variable_threshold), list(~paste0("'", ., "'")))
  
  console = console_collect(console, paste0("Run Time: ",
                                           as.character(difftime(Sys.time(), start_time, unit = "secs")),
                                           " Seconds"))
  
  console = console_collect(console, paste0(nrow(variable_clinical[[1]]), " Numerical Variables\n",
                                            nrow(variable_clinical[[2]]), " Categorical Variables\n",
                                            "With any Unit, ID, Empty, Single, Date Variables Removed"))
  
  text_main = readLines(system.file("Main Template.twb", package = "GTableauPipeline"))
  
  {
    if (generate_correlation)
      text_correlation = NA
    # text_correlation = readLines("Correlation Template.twb")
    else
      text_correlation = NA
  }
  
  vaild_correlation = !unique(is.na(text_correlation))
  
  c(text_main, text_correlation) := brilliant(abk_6, text_main, text_correlation, vaild_correlation, study_name,
                                              about, reference_line, note, URL)
  
  console = console_collect(console, "Generating Numerical Variables...")
  start_time <- Sys.time()
  
  c(abk_7, text_main, text_correlation, variable_clinical[[1]]) := tableau_numeric(abk_6, definition_4, variable_clinical[[1]], text_main,
                                                                                   text_correlation,
                                                                                   vaild_correlation, save_path, study_name)
  
  console = console_collect(console, paste0("Run Time: ",
                                           as.character(difftime(Sys.time(), start_time, unit = "secs")),
                                           " Seconds"))
  
  console = console_collect(console, "Generating Categorical Variables...")
  start_time <- Sys.time()
  
  c(abk_8, text_main, text_correlation, variable_clinical[[2]]) := tableau_categorical(abk_7, definition_4, variable_clinical[[2]], text_main,
                                                                                       text_correlation,
                                                                                       vaild_correlation, save_path, study_name)
  
  console = console_collect(console, paste0("Run Time: ",
                                           as.character(difftime(Sys.time(), start_time, unit = "secs")),
                                           " Seconds"))
  
  console = console_collect(console, "Generating Jitter Columns...")
  start_time <- Sys.time()
  
  c(abk_9, text_main) := line_jitter(abk_8, adsl_2, UID, variable_clinical[[2]], waste[[9]], text_main, definition_4, study_name, save_path)
  
  console = console_collect(console, paste0("Run Time: ",
                                           as.character(difftime(Sys.time(), start_time, unit = "secs")),
                                           " Seconds"))
  
  # Change VIS, TST, TSTCD, TSTLBL to Humanreadable Names
  # Phase 1 Special: TP
  abk_10 = setname_subset(abk_9)
  
  console = console_collect(console, "Generating Dashboard Schema...")
  start_time <- Sys.time()
  
  c(text_main, column_type) := schema(abk_10, "Template", text_main)
  
  console = console_collect(console, paste0("Run Time: ",
                                           as.character(difftime(Sys.time(), start_time, unit = "secs")),
                                           " Seconds"))
  
  console = console_collect(console, "Dealing Empty Cells...")
  start_time <- Sys.time()
  # Deal with Empty Value
  # Important in Tableau Read
  abk_11 = deal_empty(abk_10, column_type, NA, " ")
  
  console = console_collect(console, paste0("Run Time: ",
                                           as.character(difftime(Sys.time(), start_time, unit = "secs")),
                                           " Seconds"))
  
  console = console_collect(console, "Writing Template Data...")
  {
    if (save_RDS)
      saveRDS(abk_11, paste0(save_path, "Template.rds"))
    else
      write_csv(abk_11, paste0(save_path, "Template.csv"))
    
    if(connection == "Extract"){
      
    }
  }
  
  if (generate_correlation){
    
    abk_12 = sharpening(abk_11, UID = UID, save_path = save_path)
    
    console = console_collect(console, "Generating Clinical Schema...")
    start_time <- Sys.time()
    
    # text_correlation = schema(abk_12, "Clinical", text_correlation)
    
    console = console_collect(console, paste0("Run Time: ",
                                             as.character(difftime(Sys.time(), start_time, unit = "secs")),
                                             " Seconds"))
    
    console = console_collect(console, "Writing Clinical Data...")
    write_csv(abk_12, paste0(save_path, "Clinical.csv"))
    
    # writeLines(text_correlation, "Correlation.twb")
    
    abk_13 = abk_11 %>% select(all_of(c(UID, "Visit", "Time Point", "Assay", "Assay Label", "Assay Short Name", "Measurement", "Measurement Value",
                                        "Treatment")))
    
    console = console_collect(console, "Generating Scatter Schema...")
    start_time <- Sys.time()
    
    # text_scatter = schema(abk_13, "Scatter", text_scatter)
    
    console = console_collect(console, paste0("Run Time: ",
                                             as.character(difftime(Sys.time(), start_time, unit = "secs")),
                                             " Seconds"))
    
    console = console_collect(console, "Writing Scatter Data...")
    write_csv(abk_13, paste0(save_path, "Scatter.csv"))
    
    # writeLines(text_scatter, "Scatter.twb")
  }
  
  console = console_collect(console, "End of the Pipeline")
  
  time = as.character(difftime(Sys.time(), total_time, unit = "mins"))
  console = console_collect(console, paste0("Total Run Time: ", time, " Minutes"))
  
  text_main = clever(text_main, "2.0", as.numeric(time))
  writeLines(text_main, paste0(save_path, study_name, ".twb"))
  
  if (!debug_mode){
    write.table(data.frame(`Study Name` = study_name,
                           `Number of Subject` = nrow(abk_0 %>% select(all_of(UID)) %>% distinct()),
                           `Number of Assay` = length(unique(abk_0$TSTCD)),
                           `Number of Numerical` = nrow(variable_clinical[[1]]), `Number of Categorical` = nrow(variable_clinical[[2]]),
                           `Template Size` = as.character(object.size(abk_11)),
                           `Template Row` = nrow(abk_11), `Template Column` = ncol(abk_11),
                           `Correlation` = generate_correlation, `Time` = time,
                           `Date` = Sys.Date(), `Version` = "2.0"),
                file = "Performance.csv", sep = ",",
                append = TRUE, quote = FALSE,
                col.names = FALSE, row.names = FALSE)
  }
  
  if (trash_can){
    
    variable_taken_0 = match_variable(data.frame(Variable = waste$variable_taken), definition_0) %>% mutate(`New Description` = gsub(" Taken$", "", Description))
    variable_taken_1 = definition_0 %>% filter(Description %in% variable_taken_0$`New Description`)
    variable_taken_2 = bind_rows(variable_taken_0, variable_taken_1) %>% arrange(Description)
    
    return(list(Empty = adsl_0 %>% select(waste$variable_empty),
                Date = adsl_0 %>% select(waste$variable_date),
                Extra = adsl_0 %>% select(waste$variable_extra),
                Taken = adsl_0 %>% select(variable_taken_2$Variable),
                Factor = adsl_0 %>% select(waste$variable_factor),
                Unit = adsl_0 %>% select(waste$variable_unit)))
  }
  
  if (shiny){
    return(console)
  } else
    return()
}



















column_subset = function(adsl_0, UID, variable_numerical, variable_categorical, variable_keep, definition_0){
  
  # definition_0 = definition_3
  
  # Input of Numerical and Categorical has been Given
  if (!all(is.na(variable_numerical)) && !all(is.na(variable_categorical))){
    
    adsl_3 = adsl_0 %>% select(all_of(unique(c(UID, variable_numerical, variable_categorical, variable_keep))))
    return(list(adsl_3,
                list(variable_numerical = variable_numerical, variable_categorical = variable_categorical,
                     variable_empty = c(), variable_date = c(), variable_extra = c(),
                     variable_numerical_all = variable_numerical, variable_categorical_all = variable_categorical,
                     variable_taken = c(), variable_factor = c(), variable_unit = c())
    ))
    
  } else {
    
    description = definition_0 %>% select(Description) %>% unlist() %>% unname()
    
    # Try to Convert to Date Format
    convert_date <- function(temp) tryCatch(as.Date(temp), error = function(test) temp)
    # Check Date Format
    is_date <- function(temp) inherits(temp, 'Date')
    
    # For Those are Not Able to Convert to Date is not Date Format
    date_able = adsl_0 %>% mutate(across(.cols = everything(), .fns = convert_date)) %>% mutate(across(.cols = everything(), .fns = is_date)) %>% colSums()
    
    # Variable with All NA
    variable_empty_0 = adsl_0 %>% summarise_all(~all(is.na(.))) %>% unlist()
    variable_empty_1 = names(variable_empty_0[variable_empty_0])
    
    # Date Variable
    variable_date_0 = setdiff(names(date_able[date_able > 0]), variable_empty_1)
    
    # Variable Serves for Date Variable
    variable_extra_0 = colnames(adsl_0)[colnames(adsl_0) %in% gsub("C$", "", variable_date_0)]
    variable_extra_1 = definition_0 %>% filter(Variable %in% variable_date_0, grepl("\\(C\\)$", Description)) %>%
      mutate(Description = gsub(" \\(C\\)$", "", Description)) %>% select(Description) %>% unlist()
    variable_extra_2 = definition_0 %>% filter(Description %in% variable_extra_1) %>% select(Variable) %>% unlist()
    
    variable_extra_3 = definition_0 %>% filter(grepl("DT$", Variable) & grepl("(?i)Date", Description)) %>% select(Variable) %>% unlist()
    variable_extra_4 = definition_0 %>% filter(grepl("DTM$", Variable) & grepl("(?i)Time", Description)) %>% select(Variable) %>% unlist()
    
    variable_extra_5 = setdiff(unique(c(variable_extra_0, variable_extra_2, variable_extra_3, variable_extra_4)), variable_date_0)
    
    # Taken Variable
    variable_taken_0 = colnames(adsl_0)[colnames(adsl_0) %in% paste0(colnames(adsl_0), "T")]
    variable_taken_1 = definition_0 %>% filter(Description %in% description[description %in% paste0(description, " Taken")]) %>% select(Variable) %>%
      unlist() %>% unname()
    variable_taken_2 = unique(c(variable_taken_0, variable_taken_1))
    
    # Delete Unnecessary Definitions
    definition_1 = definition_0 %>% filter(!Variable %in% unique(c(variable_empty_1, variable_date_0, variable_extra_5, variable_taken_2)))
    
    # Factor Variable
    variable_factor_0 = names(adsl_0)[colnames(adsl_0) %in% paste0(colnames(adsl_0), "N")]
    variable_factor_1 = definition_1 %>% filter(Variable %in% variable_factor_0, grepl("\\(N\\)", Description))
    # Find Contain (N)
    variable_factor_2 = definition_1 %>% filter(!Variable %in% variable_factor_1$Variable, grepl("\\(N\\)", Description))
    # Remove (N)
    variable_factor_3 = variable_factor_2 %>% mutate(`Defactorized Description` = gsub(" \\(N\\)", "", Description))
    # Find Definition Matched Variables After (N) Removed for Cases That Do Not Follow Add N Rule
    variable_factor_4 = definition_1 %>% filter(Description %in% variable_factor_3$`Defactorized Description`)
    variable_factor_5 = variable_factor_3 %>% filter(`Defactorized Description` %in% variable_factor_4$Description)
    
    # Variables that Did Not Match After (N) Removed
    variable_factor_6 = setdiff(variable_factor_3$Variable, variable_factor_5$Variable)
    variable_factor_7 = unique(c(variable_factor_1$Variable, variable_factor_5$Variable))
    
    # Unit Variable
    variable_unit_0 = colnames(adsl_0)[colnames(adsl_0) %in% paste0(colnames(adsl_0), "U")]
    variable_unit_1 = definition_1 %>% filter(Description %in% description[description %in% paste0(description, " Units")]) %>% select(Variable) %>%
      unlist()
    variable_unit_2 = union(variable_unit_0, variable_unit_1)
    
    definition_2 = definition_1 %>% filter(!Variable %in% variable_unit_2)
    # Remove Empty, Date, Date Related, Taken, Unit Except Factor
    adsl_1 = adsl_0 %>% select(-all_of(unique(c(variable_empty_1, variable_date_0, variable_extra_5, variable_taken_2, variable_unit_2))))
    
    # Try to Guess Variable Type
    adsl_2 <- tryCatch({
      tryCatch(adsl_1 %>% mutate_all(parse_guess) %>% mutate(across(all_of(UID), as.character)))
    },
    error = function(e) {
      adsl_1
    }
    )
    
    # Get Columns Type
    column_type = adsl_2 %>% summarise_all(class) %>% gather(variable, class) %>% unique()
    
    # Keep Key Columns like COUNTRY
    {
      if (!all(variable_keep %in% colnames(adsl_2))) {
        adsl_3 = merge(adsl_2, adsl_0 %>% select(all_of(c(UID, setdiff(variable_keep, colnames(adsl_2))))), by = UID)
      } else
        adsl_3 = adsl_2
    }
    
    variable_numerical_all = column_type %>% filter(class == "numeric") %>% select(variable) %>% unlist() %>% unname()
    variable_categorical_all = column_type %>% filter(class == "character") %>% select(variable) %>% unlist() %>% unname()
    
    # No Input of Numerical and Categorical has been Given
    if (all(is.na(variable_numerical)) && all(is.na(variable_categorical)))
      return(list(adsl_3,
                  list(variable_numerical = variable_numerical_all,
                       variable_categorical = variable_categorical_all,
                       variable_empty = variable_empty_1, variable_date = variable_date_0, variable_extra = variable_extra_5,
                       variable_numerical_all = variable_numerical_all,
                       variable_categorical_all = variable_categorical_all,
                       variable_taken = variable_taken_2, variable_factor = variable_factor_7, variable_unit = variable_unit_2),
                  definition_2))
    
    # Only Input of Numerical has been Given
    else if (!all(is.na(variable_numerical)))
      return(list(adsl_3,
                  list(variable_numerical = variable_numerical,
                       variable_categorical = variable_categorical_all,
                       variable_empty = variable_empty_1, variable_date = variable_date_0, variable_extra = variable_extra_5,
                       variable_numerical_all = variable_numerical_all,
                       variable_categorical_all = variable_categorical_all,
                       variable_taken = variable_taken_2, variable_factor = variable_factor_7, variable_unit = variable_unit_2),
                  definition_2))
    
    # Only Input of Categorical has been Given
    else
      return(list(adsl_3,
                  list(variable_numerical = variable_numerical_all,
                       variable_categorical = variable_categorical,
                       variable_empty = variable_empty_1, variable_date = variable_date_0, variable_extra = variable_extra_5,
                       variable_numerical_all = variable_numerical_all,
                       variable_categorical_all = variable_categorical_all,
                       variable_taken = variable_taken_2, variable_factor = variable_factor_7, variable_unit = variable_unit_2),
                  definition_2))
  }
}

deal_empty = function(abk_0, column_type, replacement_numerical, replacement_categorical){
  
  # abk_0 = abk_8
  # replacement_numerical = NA
  # replacement_categorical = ""
  
  abk_1 = abk_0 %>% select(all_of(column_type %>% filter(class == "numeric" | class == "integer") %>% select(variable) %>% unlist() %>% unname()))
  abk_1[is.na(abk_1)] = replacement_numerical
  
  abk_2 = abk_0 %>% select(all_of(column_type %>% filter(class == "character" | class == "logical") %>% select(variable) %>% unlist() %>% unname()))
  abk_2[is.na(abk_2)] = replacement_categorical
  
  return(cbind(abk_2, abk_1) %>% select(all_of(column_type %>% select(variable) %>% unlist() %>% unname())))
}

match_variable = function(input, definition_0){
  
  # input = variable_numerical_0
  # input = variable_categorical_0
  
  # Get Definition
  {
    if (typeof(definition_0) == "NULL")
      report = input %>% mutate(Description = NA)
    else
      report = definition_0
  }
  
  # Match Definition
  input = input %>% left_join(report, by = "Variable")
  
  return(input)
}

setname_variable = function(abk_0, input){
  
  # input = variable_numerical
  # input = variable_categorical
  
  # Change ABK Name to Human Readable
  colnames(abk_0) = data.frame("Variable" = colnames(abk_0)) %>% left_join(input, by = "Variable") %>%
    within(Description[is.na(Description)] <- Variable[is.na(Description)]) %>%
    select(Description) %>% unlist()
  return(abk_0)
}

type_variable = function(abk_0, distinct_threshold, variable_numerical, variable_categorical, variable_numerical_all, variable_categorical_all,
                         variable_keep, variable_numerical_flag = FALSE){
  
  # abk_0 = abk_5
  # variable_numerical = waste[[1]]
  # variable_categorical = waste[[2]]
  # variable_numerical_all = waste[[5]]
  # variable_categorical_all = waste[[6]]
  # variable_numerical_flag = FALSE
  
  if (length(setdiff(variable_numerical_all, variable_numerical)) != 0){
    
    abk_1 = abk_0 %>% select(all_of(variable_numerical))
    
    distinct_numerical = sapply(abk_1, function(x) n_distinct(x))
    
    # Remove Columns with only Single Value
    distinct_numerical = distinct_numerical[distinct_numerical != 1]
    
    # Remove Factor
    variable_numerical = names(distinct_numerical[distinct_numerical > distinct_threshold])
    
    variable_factor = distinct_numerical[distinct_numerical <= distinct_threshold]
    
    # Could be Decimal
    variable_decimal = sapply(abk_1 %>% select(all_of(names(variable_factor))), function(x) !all(floor(x) == x, na.rm = TRUE)) %>% unlist()
    
    variable_numerical = c(variable_numerical, names(variable_decimal)[variable_decimal])
    
    if (length(grep("ID", variable_numerical)) != 0){
      # Masked any ID
      variable_numerical = variable_numerical[-grep("ID", variable_numerical)]
    }
    
    variable_numerical_edited = variable_numerical
    variable_numerical = variable_numerical_all
    variable_numerical_flag = TRUE
  }
  
  abk_1 = abk_0 %>% select(all_of(variable_numerical))
  
  distinct_numerical = sapply(abk_1, function(x) n_distinct(x))
  
  # Remove Columns with only Single Value
  distinct_numerical = distinct_numerical[distinct_numerical != 1]
  
  # Remove Factor
  variable_numerical = names(distinct_numerical[distinct_numerical > distinct_threshold])
  
  variable_factor = distinct_numerical[distinct_numerical <= distinct_threshold]
  
  # Could be Double
  variable_decimal = sapply(abk_1 %>% select(all_of(names(variable_factor))), function(x) !all(floor(x) == x, na.rm = TRUE)) %>% unlist()
  
  variable_numerical = c(variable_numerical, names(variable_decimal)[variable_decimal])
  
  if (length(grep("ID", variable_numerical)) != 0){
    # Masked any ID
    variable_numerical = variable_numerical[-grep("ID", variable_numerical)]
  }
  
  abk_2 = abk_0 %>% select(all_of(variable_categorical))
  
  distinct_categorical = sapply(abk_2, function(x) n_distinct(x))
  
  # Remove Columns with only Single Value
  distinct_categorical = distinct_categorical[distinct_categorical != 1]
  
  {
    if (length(setdiff(variable_categorical_all, variable_categorical)) == 0)
      # Add Factor
      variable_categorical = c(names(distinct_categorical), setdiff(names(variable_factor), names(variable_decimal)[variable_decimal]))
    else
      variable_categorical = names(distinct_categorical)
  }
  
  if (length(grep("ID", variable_categorical)) != 0){
    # Masked any ID
    variable_categorical = variable_categorical[-grep("ID", variable_categorical)]
  }
  
  {
    if (variable_numerical_flag)
      return(list(list(variable_numerical = data.frame("Variable" = variable_numerical_edited),
                       variable_categorical = data.frame("Variable" = unique(c(variable_categorical, "TRT", variable_keep)))), names(variable_factor)))
    else
      return(list(list(variable_numerical = data.frame("Variable" = variable_numerical),
                       variable_categorical = data.frame("Variable" = unique(c(variable_categorical, "TRT", variable_keep)))), names(variable_factor)))
  }
}

tableau_numeric <- function(abk_0, definition_0, variable_numerical_0, text_main, text_correlation, vaild_correlation,
                            save_path, study_name){
  
  # abk_0 = abk_6
  # variable_numerical_0 = variable_clinical[[1]]
  
  # Match ADSL Definition
  variable_numerical_1 = match_variable(variable_numerical_0, definition_0)
  
  # Check Dictionary
  variable_numerical_2 = dictionary(variable_numerical_1)
  
  # Change ABK Column Name to Human Readable Name
  abk_1 = setname_variable(abk_0, variable_numerical_2)
  
  variable_numerical_3 = variable_numerical_2 %>% mutate(Description = coalesce(Description, Variable))
  
  variable_numerical_4 = variable_numerical_3
  # variable_numerical_4 = variable_numerical_3 %>% filter(!grepl('Date|Time|Day', Description))
  
  variable_numerical_5 = variable_numerical_4 %>% select(Description)
  
  abk_2 = abk_1 %>% select(!all_of(unname(unlist(setdiff(variable_numerical_3 %>% select(Description), variable_numerical_5)))))
  
  # Description
  variable_numerical_5 = variable_numerical_5 %>% filter(!grepl('(N)', Description)) # Bug
  if (nrow(variable_numerical_5) == 0)
    variable_numerical_5 = data.frame("Description" = "Country (N)")
  write.csv(rbind("Disable", variable_numerical_5), file = paste0(save_path, 'Numerical.csv'), row.names = F)
  
  text_main = intelligent(variable_numerical_5, text_main, "ReplacementNumerical", 5)
  
  {
    if (vaild_correlation)
      text_correlation = intelligent(variable_numerical_5, text_correlation, "ReplacementNumerical", 3)
  }
  
  return(list(abk_2, text_main, text_correlation, variable_numerical_4 %>% select(Variable)))
}

tableau_categorical <- function(abk_0, definition_0, variable_categorical_0, text_main, text_correlation, vaild_correlation,
                                save_path, study_name){
  
  # abk_0 = abk_7
  # variable_categorical_0 = variable_clinical[[2]]
  
  # Match ADSL Definition
  variable_categorical_1 = match_variable(variable_categorical_0, definition_0)
  
  # Check Dictionary
  variable_categorical_2 = dictionary(variable_categorical_1)
  
  # Change ABK Column Name to Human Readable Name
  abk_1 = setname_variable(abk_0, variable_categorical_2)
  
  variable_categorical_3 = variable_categorical_2 %>% mutate(Description = coalesce(Description, Variable))
  
  variable_categorical_4 = variable_categorical_3
  # variable_categorical_4 = variable_categorical_3 %>% filter(!grepl('Date|Time|Day', Description))
  
  variable_categorical_5 = variable_categorical_4 %>% select(Description)
  
  abk_2 = abk_1 %>% select(!all_of(unname(unlist(setdiff(variable_categorical_3 %>% select(Description), variable_categorical_5)))))
  
  # Description
  write.csv(rbind("Disable", variable_categorical_5), file = paste0(save_path, 'Categorical.csv'), row.names = F)
  
  # Convert to Character because Factor Recently Joined
  abk_3 = abk_2 %>% mutate(across(.cols = all_of(unname(na.omit(unlist(variable_categorical_5)))), .fns = as.character))
  
  text_main = intelligent(variable_categorical_5, text_main, "ReplacementCategorical", 6)
  text_main = intelligent(variable_categorical_5, text_main, "ReplacementSubgroup", 2)
  text_main = intelligent(variable_categorical_5, text_main, "ReplacementColor", 3)
  
  if (vaild_correlation){
    text_correlation = intelligent(variable_categorical_5, text_correlation, "ReplacementCategorical", 3)
    text_correlation = intelligent(variable_categorical_5, text_correlation, "ReplacementSubgroup", 2)
    text_correlation = intelligent(variable_categorical_5, text_correlation, "ReplacementColor", 1)
  }
  
  return(list(abk_3, text_main, text_correlation, variable_categorical_4 %>% select(Variable)))
}

line_jitter <- function(abk_0, adsl_0, UID, variable_categorical, variable_factor_0, text_main, definition_0, study_name, save_path) {
  
  # abk_0 = abk_8
  # adsl_0 = adsl_2
  # variable_categorical = variable_clinical[[2]]
  # variable_factor_0 = waste[[9]]
  
  # Subset of ADSL that only included ABK subjects
  adsl_1 = adsl_0 %>% filter(get(UID) %in% (abk_0 %>% select(all_of(UID)) %>% unlist()))
  
  # Find Out those Categorical Variables that Missing Factor Version
  variable_categorical = unname(unlist(variable_categorical))
  unfactor = variable_categorical[!paste0(variable_categorical, "N") %in% c(variable_factor_0, "TRTN")]
  
  adsl_2 = adsl_1 %>% select(all_of(c(UID, unfactor)))
  
  # Generate their Factor Version
  # Exclude UID
  adsl_3 = adsl_2 %>% select(-all_of(UID)) %>% sapply(function(x) unclass(as.factor(x)))
  
  adsl_3[is.na(adsl_3)] = 0
  
  # Match ADSL Definition
  variable_factor_1 = match_variable(data.frame(Variable = colnames(adsl_3)), definition_0)
  
  # Check Dictionary
  variable_factor_2 = dictionary(variable_factor_1)
  
  # Change ABK Column Name to Human Readable Name
  adsl_4 = setname_variable(adsl_3, variable_factor_2)
  
  # Add '(N)' to Name
  colnames(adsl_4) = paste0(colnames(adsl_4), " (N)")
  
  # Get UID back
  # Done
  adsl_5 = adsl_2 %>% select(all_of(UID)) %>% cbind(adsl_4)
  
  # Find those Categorical Variables that Already have Factor Version
  factored = variable_factor_0[gsub("N$", "", variable_factor_0) %in% variable_categorical]
  match_name_0 = data.frame(Categorical = gsub("N$", "", factored), Factor = factored)
  
  adsl_6 = adsl_1 %>% select(all_of(c(UID, factored)))
  
  # Exclude UID
  adsl_7 = adsl_6 %>% select(-all_of(UID))
  
  # Match ADSL Definition
  variable_factor_3 = match_variable(data.frame(Variable = colnames(adsl_7)), definition_0)
  
  # Check Dictionary
  variable_factor_4 = dictionary(variable_factor_3)
  variable_factor_5 = variable_factor_4 %>% rename(Factor = Variable) %>% left_join(match_name_0, by = "Factor") %>%
    mutate(Description = if_else(is.na(Description), paste(Categorical, "(N)"), Description)) %>% rename(Variable = Factor) %>% select(-Categorical)
  
  adsl_8 = setname_variable(adsl_7, variable_factor_5)
  
  # Combine Newly Generated Factor Version with Already have Factor Version
  # Be careful with the names
  adsl_9 = adsl_6 %>% select(all_of(UID)) %>% cbind(adsl_8) %>% left_join(adsl_5, by = "SUBJID")
  
  write_csv(adsl_9, paste0(save_path, "Jitter.csv", sep = ""))
  
  c(text_main, temp_0) := schema(adsl_9, "Jitter", text_main)
  
  # Treatment is an exception will Deal it Separately
  # Reorder TRTN in 1, 2, 3
  temp_1 = abk_0 %>% select(Treatment, TRTN) %>% distinct() %>% arrange(TRTN) %>% mutate(TRTN = 1:nrow(.)) %>% rename(`Treatment (N)` = TRTN)
  abk_1 = abk_0 %>% select(-TRTN) %>% left_join(temp_1, by = "Treatment")
  
  {
    # NA Goes Before Minimum TPN
    abk_1$TPN[is.na(abk_1$TPN)] = min(na.omit(abk_1$TPN)) - 1
    
    # Reorder TPN in 1, 2, 3
    temp_2 = abk_1 %>% select(TP, TPN) %>% distinct() %>% arrange(TPN) %>% mutate(TPN = 1:nrow(.))
    abk_2 = abk_1 %>% select(-TPN) %>% left_join(temp_2, by = "TP")
  }
  
  # Reorder VISN in 10, 20, 30
  temp_3 = abk_2 %>% select(VIS, VISN) %>% distinct() %>% arrange(VISN) %>% select(VIS) %>% distinct() %>% mutate(VISN = 1:nrow(.), test = 1:nrow(.) * 10)
  abk_3 = abk_2 %>% select(-VISN) %>% left_join(temp_3, by = "VIS")
  
  {
    # Generate Detailed Time Point
    abk_4 = abk_3 %>% unite(VISTP, c("VIS", "TP"), sep = " ", remove = FALSE) %>% mutate(VISTP = str_trim(VISTP, "right"))
    abk_5 = abk_4 %>% mutate(VISTPN = test + TPN) %>% select(-test)
    
    # Reorder VISTPN in 1, 2, 3
    temp_4 = abk_5 %>% select(VISTP, VISTPN) %>% distinct() %>% arrange(VISTPN) %>% mutate(VISTPN = 1:nrow(.))
    abk_6 = abk_5 %>% select(-VISTPN) %>% left_join(temp_4, by = "VISTP")
    
    abk_7 = abk_6 %>% select(VIS, TP) %>% distinct() %>% group_by(VIS) %>% mutate(ARRANGE = row_number())
    
    abk_8 = abk_6 %>% left_join(abk_7, by = c("VIS", "TP"))
  }
  
  variable_factor_6 = rbind(variable_factor_2, variable_factor_5, c("Treatment", "Treatment (N)"))
  
  variable_factor_7 = variable_factor_6 %>% mutate(Variable = coalesce(Description, Variable)) %>%
    mutate(Description = Variable)
  
  variable_factor_8 = variable_factor_7 %>% filter(grepl("\\(N\\)", Description)) %>% mutate(Variable = gsub(" \\(N\\)", "", Variable))
  
  variable_factor_9 = variable_factor_7 %>% filter(!grepl("\\(N\\)", Description)) %>% mutate(Description = paste(Variable, "(N)"))
  
  variable_factor_10 = rbind(variable_factor_8, variable_factor_9)
  
  # sink(paste0(save_path, "Jitter_Color_Group_Calculated_Field.txt"), append = F)
  # cat("CASE [Descriptioneters].[Color Group]\n")
  #
  # for(i in 1:nrow(variable_factor_6)){
  #   cat(paste0(" WHEN '", variable_factor_6[i, "Variable"], "' THEN ", "[", variable_factor_6[i, "Description"], "]\n"))
  # }
  # cat(paste0(" WHEN '", "Treatment", "' THEN ", "[", "Treatment (N)", "]\n"))
  #
  # cat("END\n")
  # sink()
  
  text_main = intelligent(variable_factor_10, text_main, "ReplacementCategoricalNumber", 1)
  
  abk_9 = abk_8 %>% select(-all_of(colnames(abk_8)[colnames(abk_8) %in% colnames(adsl_9 %>% select(-all_of(UID)))]))
  
  return(list(abk_9, text_main))
}

dictionary = function(input){
  
  # Self Edited
  definition_0 = data.frame(
    Variable = c("TRT", "RACE", "SEX", "AGE", "ETHNIC", "COUNTRY"),
    Description = c("Treatment", "Race", "Sex", "Age", "Ethnic", "Country")
  )
  
  # Find only Missing to Fill
  missing = input[which(is.na(input$Description)), ]
  input = input %>% filter(!is.na(Description))
  
  # Match Unicode Dictionary
  if (nrow(missing) > 0){
    
    missing = missing %>% select(Variable) %>% left_join(definition_0, by = "Variable")
    
    if (nrow(input) == 0)
      input = missing
    else
      input = input %>% bind_rows(missing)
    
  }
  
  return(input)
}

setname_subset = function(abk_0){
  
  # abk_0 = abk_9
  
  # "Treatment" is Modified in 'dictionary' function
  colnames(abk_0)[base::match(c("VIS", "TST", "TSTCD", "TSTLBL", "TP"), colnames(abk_0))] = c("Visit", "Assay", "Assay Short Name", "Assay Label",
                                                                                              "Time Point")
  
  return(abk_0)
}

schema = function(data_0, name, text){
  
  # data_0 = adsl_9
  # name = "Jitter"
  # data_0 = abk_10
  # name = "Template"
  # text = text_main
  
  column_type = data_0 %>% summarise_all(class) %>% gather(variable, class) %>% unique()
  information = column_type %>%
    mutate(class = recode(class, "character" = "string", "numeric" = "real", "logical" = "nominal", "integer" = "integer"))
  
  # name='Template.csv' table='[Template#csv]' type='table'>
  # name='Jitter.csv' table='[Jitter#csv]' type='table'>
  index = which(grepl(paste0("name='", name, ".csv' table='\\[", name, "#csv\\]' type='table'>"), text)) + 1
  
  sentence = c()
  for(i in 1:nrow(information)){
    sentence = c(sentence, paste0("<column datatype='", information[i, "class"], "' name='", information[i, "variable"],
                                  "' ordinal='", i - 1, "' />"))
  }
  
  for (i in 1:length(index)){
    if (name == "Template"){
      text = text[-((index[i] + 1):(index[i] + 23))]
      if (i < length(index))
        index[i + 1] = index[i + 1] - i * 23
    } else if (name == "Jitter"){
      text = text[-((index[i] + 1):(index[i] + 4))]
      if (i < length(index))
        index[i + 1] = index[i + 1] - i * 4
    }
  }
  
  for (i in 1:length(index)){
    text = append(text, sentence, after = index[1])
    if (i > length(index))
      break
    index = index[-1] + length(sentence)
  }
  
  return(list(text, column_type))
}

sharpening = function(abk_0, UID, save_path){
  
  # Serve Correlation Only
  # abk_0 = abk_11
  
  # Only Numerical are Clinical Variables
  numerical = c(unname(unlist(suppressMessages(read_csv(paste0(save_path, "Numerical.csv"))))))
  numerical = unique(numerical[!numerical %in% c("Disable")])
  
  abk_1 = abk_0 %>% select(all_of(c(UID, numerical))) %>% distinct()
  abk_2 = gather(abk_1, key = "Clinical", value = "Clinical Value", numerical)
  
  return(abk_2)
}

intelligent = function(variable, text, code, number){
  
  # code = "ReplacementNumerical"
  # number = 5
  # variable = variable_numerical_5
  # text = text_main
  
  index = head(which(grepl(code, text)), n = number)
  
  sentence = text[index]
  
  quote = "&apos;"
  
  if (code != "ReplacementCategoricalNumber"){
    for(row in variable){
      part_0 = paste0(" WHEN ", quote, row, quote, " THEN ", "[", row, "]&#13;&#10;")
    }
    
    {
      if (code == "ReplacementSubgroup")
        part_0 = c(part_0, paste0(" WHEN ", quote, "Disable", quote, " THEN ", "[", "Disable", "]&#13;&#10;"))
      else if (code == "ReplacementColor")
        part_0 = c(part_0, paste0(" WHEN ", quote, "Disable", quote, " THEN ", "[", "All", "]&#13;&#10;"))
    }
  } else {
    part_0 = paste0(" WHEN ", quote, "Treatment", quote, " THEN ", "[", "Treatment (N)", "]&#13;&#10;")
    for(row in rownames(variable)){
      part_0 = c(part_0, paste0(" WHEN ", quote, variable[row, "Variable"], quote, " THEN ", "[", variable[row, "Description"], "]&#13;&#10;"))
    }
  }
  
  part_1 = paste(part_0, collapse = "")
  
  text[index] = gsub(" WHEN.*;", part_1, sentence)
  
  return(text)
}

brilliant = function(abk_0, text_main, text_correlation, vaild_correlation, study_name, about, reference_line, message, URL){
  
  # abk_0 = abk_6
  
  # Edit Title
  index_main_0 = which(grepl("fontsize='36'>Simulated Data<", text_main))
  sentence_main_0 = text_main[index_main_0]
  text_main[index_main_0] = gsub("Simulated Data", study_name, sentence_main_0)
  
  # Abandoned
  # if (vaild_correlation){
  #   index_correlation_0 = which(grepl("ReplacementTitle", text_correlation))
  #   sentence_correlation_0 = text_correlation[index_correlation_0]
  #   text_correlation[index_correlation_0] = gsub("ReplacementTitle", study_name, sentence_correlation_0)
  # }
  # if (vaild_scatter){
  #   index_scatter_0 = which(grepl("ReplacementTitle", text_scatter))
  #   sentence_scatter_0 = text_scatter[index_scatter_0]
  #   text_scatter[index_scatter_0] = gsub("ReplacementTitle", study_name, sentence_scatter_0)
  # }
  
  # directory='./'
  
  # # Edit Images
  # ./Images/
  
  # Edit Default Assay
  index_main_3 = which(grepl("Fake Assay 1", text_main))
  part_0 = abk_0 %>% select(TST) %>% distinct() %>% arrange() %>% unlist() %>% unname()
  default_0 = head(part_0[!grepl("[[:punct:]&&[^_]]+", part_0)], n = 1)
  sentence_main_3 = gsub("Fake Assay 1", default_0, text_main[index_main_3])
  text_main[index_main_3] = sentence_main_3
  
  # Edit Study Information
  index_main_4 = which(grepl("A Phase 2, Simulated Randomized Double-Blind Placebo-Controlled Study Evaluating the Combination Therapy in Fake Patients",
                             text_main))
  if (!is.na(about))
    sentence_main_4 = gsub("A Phase 2, Simulated Randomized Double-Blind Placebo-Controlled Study Evaluating the Combination Therapy in Fake Patients",
                           about, text_main[index_main_4])
  else
    sentence_main_4 = gsub("A Phase 2, Simulated Randomized Double-Blind Placebo-Controlled Study Evaluating the Combination Therapy in Fake Patients",
                           "No Study Information", text_main[index_main_4])
  text_main[index_main_4] = sentence_main_4
  
  # Edit Creator Note
  index_main_5 = which(grepl("No Message", text_main))
  sentence_main_5 = gsub("No Message", message, text_main[index_main_5])
  text_main[index_main_5] = sentence_main_5
  
  # Edit Reference Line
  # <column caption='Reference Line' datatype='real' name='[Parameter 3]' param-domain-type='any' role='measure' type='quantitative' value='0.'>
  index_main_6 = which(grepl("type='quantitative' value='0.'>", text_main))
  if (is.na(reference_line)){
    reference_line = abk_0 %>% filter(TST == default_0, Measurement == "Raw Value") %>% summarise(Minimum = as.integer(min(`Measurement Value`))) %>%
      pull(Minimum)
  }
  sentence_main_6 = gsub("0.", paste0(reference_line, "."), text_main[index_main_6])
  text_main[index_main_6] = sentence_main_6
  
  # https://cbea.gilead.com/tableau
  if (!is.na(URL)){
    index_main_7 = which(grepl("https://cbea.gilead.com/tableau", text_main))
    sentence_main_7 = gsub("https://cbea.gilead.com/tableau", URL, text_main[index_main_7])
    text_main[index_main_7] = sentence_main_7
  }
  
  return(list(text_main, text_correlation))
}

clever = function(text_main, version, run_time){
  
  # Edit Pipeline Verison
  index_main_0 = which(grepl("Pipeline Version", text_main))
  sentence_main_0 = text_main[index_main_0]
  text_main[index_main_0] = gsub("Develop", version, sentence_main_0)
  
  # Edit Dashboard Generation Time
  index_main_1 = which(grepl("Generation Time", text_main))
  sentence_main_1 = text_main[index_main_1]
  text_main[index_main_1] = gsub("0.185", round(run_time, 3), sentence_main_1)
  
  # Edit Dashboard Last Update Time
  index_main_2 = which(grepl("Last Update", text_main))
  sentence_main_2 = text_main[index_main_2]
  text_main[index_main_2] = gsub("Develop", format(Sys.time(), "%Y-%m-%d %H:%M"), sentence_main_2)
  
  return(text_main)
}

define <- function(input) {
  
  output <-
    data.frame(Variable = character(0),
               Description = character(0))
  
  temp <- sapply(input, function(x)
    attributes(x)$label)
  
  # Remove NULL Objects
  temp[sapply(temp, is.null)] <- NULL
  temp <- data.frame(
    Variable = names(temp),
    Description = unlist(temp)
  )
  output <- rbind(output, temp)
  # Removing Duplicates and Sorting
  output <- unique(output)
  output <- output[order(output$Description), ]
  # Delete Row Mames
  rownames(output) = NULL
  
  return(output)
}

':=' <- function(lhs, rhs){
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL))
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  
  return(invisible(NULL))
}

log_stop <- function(input, log_path) {
  
  cat(paste0(input, "\n\n"))
  
  cat(paste("Stop:", input, "\n"), file = log_path, append = TRUE, sep = "\n")
  
  stop(input)
}

log_warning <- function(input, log_path) {
  
  cat(paste0(input, "\n\n"))
  
  cat(paste("Warning:", input, "\n"), file = log_path, append = TRUE, sep = "\n")
  
  warning(input)
}

log_clear <- function(log_path) {
  cat("", file = log_path)
}

log_table = function(input, log_path) {
  
  print(input)
  cat("\n")
  
  suppressWarnings(write.table(input, file = log_path, append = TRUE, sep = "\t", row.names = FALSE))
  
  cat("\n", file = log_path, append = TRUE)
  
}

console_collect = function(console, text){
  
  cat(paste0(text, "\n\n"))
  
  if (length(console) == 0)
    console = text
  else
    console = paste0(console, "\n\n", text)
  
  return(console)
}