library(gt)
library(stringr)
library(ggplot2)
library(kableExtra)

# setwd("/Users/jeremydumalig/Downloads/HOPES")

source("Database.R")

Sys.setenv(TZ='CST6CDT')

cld_colors <- read_csv("cld_colors.csv")

clean_data <- function(df) {
  df %>%
    mutate(Date = as.Date( substr(as.character(checkin), 1, 10) ),
           timeIn = substr(checkin, 12, 16),
           timeOut = substr(checkout, 12, 16),
           `Coach Checked-In` = str_detect(checked, "started"),
           Prepared = str_detect(prepared, "fully prepared"),
           Focused = str_detect(focused, "focused"),
           Distracted = !str_detect(distracted, "no noise/environmental issues"),
           Completed = str_detect(completed, "completed"),
           Offline = offline) %>%
    mutate_all(list(~str_replace_all(., "TRUE", "Yes"))) %>%
    mutate_all(list(~str_replace_all(., "FALSE", "No")))
}

color_helper <- function(gt, col, rev=FALSE) {
  gt %>%
    data_color(columns=col,
               method="factor",
               levels=c(if (!rev) "Yes" else "No"),
               palette = c("#ABDDC4"))
}

session_log <- function(df, report=FALSE, range=FALSE) {
  # df = get_sessions("All", "All", c("Language"))
  math = "Math" %in% unique(df$subject)
  language = "Language" %in% unique(df$subject)
  
  durations <-
    get_clocks() %>%
    group_by(Date, coach, student, stint) %>%
    summarize(timeIn = first(time),
              timeOut = last(time),
              .groups='drop') %>%
    mutate(`Duration (min)` = round(as.numeric(difftime(timeOut, timeIn, 
                                                        units=c("min"))), 1),
           Date = as.character(Date),
           timeIn = substr(timeIn, 12, 16),
           timeOut = substr(timeOut, 12, 16))
  
  current_year <- as.numeric( substr(Sys.Date(), 0, 4) )
  
  df <-
    df %>%
    clean_data() %>%
    select(-timeIn) %>%
    merge(durations,
          by=c("Date", "coach", "student", "timeOut"),
          all.y=TRUE) %>%
    drop_na(subject)
  
  df <-
    df %>%
    group_by(Date, coach, student) %>%
    fill(everything(), .direction="up") %>%
    ungroup() %>%
    group_by(Date, coach, student, checkout) %>%
    filter(`Duration (min)` == max(`Duration (min)`)) %>%
    ungroup()
  
  df <-
    df %>%
    mutate(Day = weekdays(as.Date(Date)),
           Start = paste0(timeIn, " CST"),
           End = paste0(timeOut, " CST"),
           Coach = coach,
           Subject = subject,
           Student = student,
           Khan = case_when((khan1 == "N/A") ~ "",
                            TRUE ~ khan1), # neglecting khan2 for now
           `Read Aloud` = case_when((str_detect(math_focused, "NOT read aloud")) ~ "No",
                                    (str_detect(math_focused, "read aloud")) ~ "Yes",
                                    TRUE ~ ""),
           # graduation = as.numeric(graduation),
           Grade = grade, # (8 - graduation + current_year),
           Grade = case_when((Grade > 0) ~ as.character(Grade),
                             TRUE ~ "K"),
           Assignments = assignment,
           Goals = goal) %>%
    arrange(Date)
  
  if (report) {
    return(select(df, Date, Coach, Student, Grade, Subject, `Duration (min)`, `Coach Checked-In`,
                  Completed, Offline))
  } else if (range) {
    return(df)
  }
  
  if (math) {
    df <-
      df %>%
      rowwise() %>%
      mutate(`Khan Summary 1` = khan_summary1,
             `Khan Summary 2` = khan_summary2,
             `Non-Khan Summary 1` = non_khan_summary1,
             `Non-Khan Summary 2` = non_khan_summary2,
             Core = core_topics, # length(str_split(core_topics, " - ")[[1]]),
             KMC = mms) %>%
      ungroup() %>%
      select(Student, Coach, Subject, Start, End, Day, Date, 
             Khan, KMC,
             Core, 
             `Khan Summary 1`, `Khan Summary 2`,
             `Non-Khan Summary 1`, `Non-Khan Summary 2`,
             Assignments, Goals, Offline)
  } else if (language) {
    df <-
      df %>%
      rowwise() %>%
      mutate(# Core = core_topics, # length(str_split(core_topics, " - ")[[1]]),
             # Words = words_to_review,
             `Personal Book` = personal_book_name, 
             `Personal Book Pages` = personal_book_pages,
             `Finished Personal Book?` = personal_book_finished,
             `School Book` = school_book_name, 
             `School Book Pages` = school_book_pages,
             `Finished School Book?` = school_book_finished,
             `Brain Book Extension` = brain_book_input,
             `Brain Book Chapter` = brain_book_chapter,
             `Brain Book Pages` = brain_book_pages,
             Assignments = language_assignment
             ) %>%
      ungroup() %>%
      select(Student, Coach, Subject, Start, End, Day, Date,
             Core,
             Words,
             `Personal Book`, `Personal Book Pages`, `Finished Personal Book?`,
             `School Book`, `School Book Pages`, `Finished School Book?`,
             `Brain Book Extension`, `Brain Book Chapter`, `Brain Book Pages`,
             Assignments, Goals, Offline)
  } else {
    df <-
      df %>%
      select(Date, Day, Start, End, Coach, Student, Grade, Subject, `Duration (min)`, `Coach Checked-In`,
             Completed, Assignments, Goals, Offline)
  }
  
  if (nrow(df) != 0) {
    df %>%
      arrange(desc(Date)) %>%
      gt() %>%
      cols_width(
        Date ~ px(120),
        Day ~ px(101),
        Start ~ px(75),
        End ~ px(75),
        Coach ~ px(100),
        Student ~ px(100),
        # Grade ~ px(75),
        Subject ~ px(120),
        # `Coach Checked-In` ~ px(120),
        # Completed ~ px(110),
        # `Duration (min)` ~ px(100),
        Assignments ~ px(500),
        Goals ~ px(500),
        Offline ~ px(500)
      ) %>%
      tab_header(title=md("**Session Log**")) %>%
      opt_interactive(use_search=TRUE,
                      use_highlight=TRUE,
                      use_page_size_select=TRUE,   
                      page_size_default=5,
                      page_size_values=c(5, 10))
  } else {
    gt(df)
  }
}

cld_log <- function(df, report=FALSE, range=FALSE) {
  math = ("Math" %in% unique(df$subject))
  
  durations <-
    get_clocks() %>%
    group_by(Date, coach, student, stint) %>%
    summarize(timeIn = first(time),
              timeOut = last(time)) %>%
    mutate(`Duration (min)` = round(as.numeric(difftime(timeOut, timeIn, 
                                                        units=c("min"))), 1),
           Date = as.character(Date),
           timeIn = substr(timeIn, 12, 16),
           timeOut = substr(timeOut, 12, 16))
  
  current_year <- as.numeric( substr(Sys.Date(), 0, 4) )
  
  df <-
    df %>%
    clean_data() %>%
    select(-timeIn) %>%
    merge(durations,
          by=c("Date", "coach", "student", "timeOut"),
          all.y=TRUE) %>%
    drop_na(subject)
  
  df <-
    df %>%
    group_by(Date, coach, student) %>%
    fill(everything(), .direction="up") %>%
    ungroup() %>%
    group_by(Date, coach, student, checkout) %>%
    filter(`Duration (min)` == max(`Duration (min)`)) %>%
    ungroup()
  
  df <-
    df %>%
    mutate(Day = weekdays(as.Date(Date)),
           Start = paste0(timeIn, " CST"),
           End = paste0(timeOut, " CST"),
           Coach = coach,
           Subject = subject,
           Student = student,
           Khan = case_when((khan1 == "N/A") ~ "",
                            TRUE ~ khan1), # neglecting khan2 for now
           `Read Aloud` = case_when((str_detect(math_focused, "NOT read aloud")) ~ "No",
                                    (str_detect(math_focused, "read aloud")) ~ "Yes",
                                    TRUE ~ ""),
           # graduation = as.numeric(graduation),
           Grade = grade, # (8 - graduation + current_year),
           Grade = case_when((Grade > 0) ~ as.character(Grade),
                             TRUE ~ "K"),
           Assignments = assignment,
           Goals = goal,
           "stayed focused on the content and did not get distracted." = str_detect(focused, "stayed focused on the content and did not get distracted."),
           "spent time unfocused, leading away from the math or language content." = str_detect(focused, "spent time unfocused, leading away from the math or language content."),
           "For math sessions, read aloud each question entirely including the title." = str_detect(math_focused, "For math sessions, read aloud each question entirely including the title."),
           "For math sessions, did NOT read aloud each question entirely including the title." = str_detect(math_focused, "For math sessions, did NOT read aloud each question entirely including the title."),
           "was fully prepared for the session." = str_detect(prepared, "was fully prepared for the session."),
           "was missing paper, pencil, white board, and/or lesson materials." = str_detect(prepared, "was missing paper, pencil, white board, and/or lesson materials."),
           "computer was not fully charged." = str_detect(prepared, "computer was not fully charged."),
           "was not at proper desk for the session." = str_detect(prepared, "was not at proper desk for the session."),
           "used up session time to look for and discuss the start content." = str_detect(prepared, "used up session time to look for and discuss the start content."),
           "used up session time to discuss Student’s unpreparedness." = str_detect(prepared, "used up session time to discuss Student’s unpreparedness."),
           "had no noise/environmental issues." = str_detect(distracted, "had no noise/environmental issues."),
           "had poor or erratic internet connection." = str_detect(distracted, "had poor or erratic internet connection."),
           "forgot to click “share sound” when sharing screen and Student had to stop and start again." = str_detect(distracted, "forgot to click “share sound” when sharing screen and Student had to stop and start again."),
           "appeared tired (could be due to lack of sleep, lack of food, stress, mood)." = str_detect(distracted, "appeared tired (could be due to lack of sleep, lack of food, stress, mood)."),
           "There were loud noises in the background (e.g., loud TV, others arguing or playing, parent cleaning around desk, etc.)." = str_detect(distracted, "There were loud noises in the background (e.g., loud TV, others arguing or playing, parent cleaning around desk, etc.).")) %>%
    arrange(Date)
  
  if (report) {
    return(select(df, Date, Coach, Student, Grade, Subject, `Duration (min)`, `Coach Checked-In`,
                  Completed, Offline))
  } else if (range) {
    return(df)
  }
  
  cols = colnames(df)[(length(colnames(df)) - 14):length(colnames(df))]
  
  if (math) {
    df <-
      df %>%
      select(Date, Day, Start, End, Coach, Student, Grade, Subject, all_of(cols))
  } else {
    df <-
      df %>%
      select(Date, Day, Start, End, Coach, Student, Grade, Subject, all_of(cols)[-3][-3])
  }
  
  logical_cols <- colnames(df)[-(1:8)]
  df[logical_cols] <- lapply(df[logical_cols], 
                             function(x) ifelse(is.na(x), "", 
                                                ifelse(!is.logical(x), x, 
                                                       ifelse(x, "Yes", "No"))))
  
  if (nrow(df) != 0) {
    df %>%
      gt() %>%
      cols_width(
        Date ~ px(120),
        Day ~ px(101),
        Start ~ px(75),
        End ~ px(75),
        Coach ~ px(100),
        Student ~ px(100),
        Grade ~ px(75),
        Subject ~ px(120),
        everything() ~ px(200)
      ) %>%
      tab_header(title=md("**Cognitive Load Disruptors (CLDs)**")) %>%
      opt_interactive(use_search=TRUE,
                      use_highlight=TRUE,
                      use_page_size_select=TRUE,   
                      page_size_default=5,
                      page_size_values=c(5, 10))
  } else {
    gt(df)
  }
}

khan_plot <- function(df, report=FALSE) {
  # df = get_sessions("All", "All", c("Math"))
  
  if (nrow(df) != 0) {
    df =
      df %>%
      filter(subject == "Math",
             !is.na(as.numeric(khan1))) %>%
      clean_data() %>%
      group_by(student) %>%
      mutate(Session = factor(row_number()),
             khan = as.numeric(khan1)) %>%
      ungroup()
    
    df %>%
      ggplot(aes(Date, khan, group=1)) +
      geom_point() +
      geom_line() +
      ylim(c(0, 100)) +
      labs(x="Sessions Over Time", 
           y="Mastery Percentage (%)",
           title="Khan Academy Mastery Progress") +
      theme_bw() +
      theme(strip.text.x = element_text(size = (if (report) 10 else 18)),
            axis.title = element_text(size=(if (report) 10 else 18)),
            axis.text = element_text(size=(if (report) 7 else 14)),
            title = element_text(size=(if (report) 10 else 18)),
            # axis.text.x = element_text(angle=45, hjust=1),
            legend.title = element_text(size=(if (report) 7 else 14)),
            legend.text = element_text(size=(if (report) 6 else 8))) +
      facet_wrap(~student, ncol=1)
  }
}

mms_plot <- function(df, report=FALSE) {
  # df = get_sessions("All", "All", c("Math"))
  
  if (nrow(df) != 0) {
    df =
      df %>%
      filter(subject == "Math",
             !is.na(as.numeric(mms))) %>%
      clean_data() %>%
      group_by(student) %>%
      mutate(Session = factor(row_number()),
             mms = as.numeric(mms)) %>%
      ungroup()
    
    df %>%
      ggplot(aes(Date, mms, group=1)) +
      geom_point() +
      geom_line() +
      ylim(c(0, 5 + max(as.numeric(get_all_sessions()$mms), na.rm=T))) +
      labs(x="Sessions Over Time", 
           y="Total Number of Mastered Skills",
           title="Mastered Skills Progress") +
      theme_bw() +
      theme(strip.text.x = element_text(size = (if (report) 10 else 18)),
            axis.title = element_text(size=(if (report) 10 else 18)),
            axis.text = element_text(size=(if (report) 7 else 14)),
            title = element_text(size=(if (report) 10 else 18)),
            # axis.text.x = element_text(angle=45, hjust=1),
            legend.title = element_text(size=(if (report) 7 else 14)),
            legend.text = element_text(size=(if (report) 6 else 8))) +
      facet_wrap(~student, ncol=1)
  }
}

cld_plot <- function(df, report=FALSE, table=FALSE) {
  # df=get_sessions("All", "All", "All")
  
  df <-
    df %>%
    clean_data() %>%
    group_by(student) %>%
    mutate(`Stayed Focus` = str_detect(focused, "stayed focused"),
           `Unfocused` = str_detect(focused, "unfocused"),
           
           `Read Each Question Aloud` = str_detect(focused, "NOT read aloud"),
           `Did Not Read Each Question Aloud` = str_detect(focused, "read aloud"),
           
           `Missing Materials` = (!(str_detect(prepared, " - ")) & str_detect(prepared, "missing paper")),
           `Computer Not Charged` = (!(str_detect(prepared, " - ")) & str_detect(prepared, "not fully charged")),
           `Fully Prepared` = (!(str_detect(prepared, " - ")) & str_detect(prepared, "was fully prepared")),
           `Not at Desk` = (!(str_detect(prepared, " - ")) & str_detect(prepared, "not at proper desk")),
           `Delayed by Start Content` = (!(str_detect(prepared, " - ")) & str_detect(prepared, "look for and discuss")),
           `Delayed by Unpreparedness` = (!(str_detect(prepared, " - ")) & str_detect(prepared, "discuss Student")),
           `Multiple` = str_detect(prepared, " - "),
           
           `None` = (!(str_detect(distracted, " - ")) & str_detect(distracted, "no noise/environmental")),
           `Internet Connection` = (!(str_detect(distracted, " - ")) & str_detect(distracted, "poor or erratic")),
           `Screen Share Issues` = (!(str_detect(distracted, " - ")) & str_detect(distracted, "forgot to click")),
           `Tired` = (!(str_detect(distracted, " - ")) & str_detect(distracted, "appeared tired")),
           `Loud Noise in Background` = (!(str_detect(distracted, " - ")) & str_detect(distracted, "loud noises")),
           `Multiple Distractions` = str_detect(distracted, " - "),
           
           Student = student,
           Session = factor(row_number())) %>%
    ungroup() %>%
    pivot_longer(cols = c("Stayed Focus", "Unfocused", 
                          "Read Each Question Aloud", "Did Not Read Each Question Aloud", 
                          "Fully Prepared", "Missing Materials", "Computer Not Charged", "Not at Desk", "Delayed by Start Content", "Delayed by Unpreparedness", "Multiple", 
                          "None", "Internet Connection", "Screen Share Issues", "Tired", "Loud Noise in Background", "Multiple Distractions"), 
                 names_to = "Cognitive Load Disruptors (CLDs)", 
                 values_to = "Value") %>% 
    mutate(Type = case_when(((`Cognitive Load Disruptors (CLDs)` == "Unfocused") | 
                               (`Cognitive Load Disruptors (CLDs)` == "Stayed Focus")) ~ "Focus",
                            
                            (str_detect(`Cognitive Load Disruptors (CLDs)`, "Aloud")) ~ "Focus (Math)",
                            
                            ((`Cognitive Load Disruptors (CLDs)` == "None") |
                               (`Cognitive Load Disruptors (CLDs)` == "Screen Share Issues") |
                               (`Cognitive Load Disruptors (CLDs)` == "Tired") |
                               (`Cognitive Load Disruptors (CLDs)` == "Internet Connection") |
                               (`Cognitive Load Disruptors (CLDs)` == "Loud Noise in Background") |
                               (`Cognitive Load Disruptors (CLDs)` == "Multiple Distractions")) ~ "Environment",
                            TRUE ~ "Preparedness")
    ) %>%
    # merge(cld_colors,
    #       by="Cognitive Load Disruptors (CLDs)",
    #       all.x=TRUE) %>%
    group_by(student, Type, `Cognitive Load Disruptors (CLDs)`) %>%
    summarize(Count = sum(Value),
              Total = n()) %>%
    ungroup() %>%
    mutate(Value = round(100 * Count / Total, 1)) %>%
    arrange(student, Type, `Cognitive Load Disruptors (CLDs)`)
  
  if (!table) {
    df %>%
      ggplot(aes(x="",
                 y=Value,
                 fill=`Cognitive Load Disruptors (CLDs)`)) +
      geom_bar(
        color='white',
        stat = "identity") +
      coord_polar("y") +
      facet_grid(student ~ Type) +
      # scale_fill_manual(guide="legend",
      #                   breaks=cld_colors$Code,
      #                   labels=cld_colors$`Cognitive Load Disruptors (CLDs)`) +
      labs(x="",
           y="",
           fill="",
           title="Cognitive Load Disruptors (CLDs)") +
      theme_bw() +
      theme(strip.text.x = element_text(size = (if (report) 8 else 14)),
            strip.text.y = element_text(size = (if (report) 8 else 14)),
            axis.title = element_text(size=(if (report) 10 else 18)),
            title = element_text(size=(if (report) 10 else 18)),
            axis.text=element_blank(),
            legend.title = element_text(size=(if (report) 7 else 14)),
            legend.text = element_text(size=(if (report) 6 else 10)),
            legend.position="bottom") +
      guides(fill = guide_legend(nrow=4))
  } else {
    df %>%
      mutate(Student = student) %>%
      select(-student, -Count, -Total, -Type) %>%
      group_by(Student) %>%
      pivot_wider(names_from = `Cognitive Load Disruptors (CLDs)`, values_from = Value) %>%
      ungroup() %>%
      gt() %>%
      cols_width(
        Student ~ px(150),
        everything() ~ px(100)
      ) %>%
      tab_spanner(
        label = "Focus",
        columns = c(
          `Stayed Focus`, `Unfocused`
        )
      ) %>%
      tab_spanner(
        label = "Math (Focus)",
        columns = c(
          `Read Each Question Aloud`, `Did Not Read Each Question Aloud`
        )
      ) %>%
      tab_spanner(
        label = "Environment",
        columns = c(
          None, `Screen Share Issues`, `Tired`, `Loud Noise in Background`, `Internet Connection`, `Multiple Distractions`
        )
      ) %>%
      tab_spanner(
        label = "Preparedness",
        columns = c(
          `Fully Prepared`, `Computer Not Charged`, `Not at Desk`, `Missing Materials`, `Delayed by Start Content`, `Delayed by Unpreparedness`, `Multiple`
        )
      ) %>%
      tab_header(title=md("**Cognitive Load Disruptors (CLDs)**"),
                 subtitle="% of Sessions with the Following CLDs")
  }
}

get_total_hours <- function() {
  get_all_clocks() %>%
    filter(action == "In") %>%
    merge(select(get_all_clock_submits(), coach, student, subject, Date, Time),
          by=c("coach", "student", "subject", "Date"),
          all.y=TRUE) %>%
    mutate(Coach = coach,
           Time.x = as.difftime(Time.x, format = "%H:%M"),
           Time.y = as.difftime(Time.y, format = "%H:%M"),
           Hours = as.numeric(Time.y - Time.x)) %>%
    group_by(Coach) %>%
    summarize(`Total Hours` = round(sum(Hours, na.rm=T), 1)) %>%
    ungroup() %>%
    select(Coach, `Total Hours`)
}

get_coach_hours <- function() {
  get_all_sessions() %>%
    session_log(report=TRUE) %>%
    group_by(Coach) %>%
    summarize(Sessions = n(),
              `Minutes Per Session` = sum(`Duration (min)`) / Sessions,
              .groups='drop') %>%
    mutate(`Minutes Per Session` = round(`Minutes Per Session`, 1),
           `Total Hours` = `Minutes Per Session` * Sessions) %>%
    select(Coach, Sessions, `Total Hours`, `Minutes Per Session`) %>%
    arrange(desc(`Total Hours`))
}

get_student_hours <- function() {
  get_all_sessions() %>%
    session_log(report=TRUE) %>%
    group_by(Student) %>%
    summarize(Sessions = n(),
              `Minutes Per Session` = sum(`Duration (min)`) / Sessions,
              .groups='drop') %>%
    mutate(`Minutes Per Session` = round(`Minutes Per Session`, 1),
           `Total Hours` = `Minutes Per Session` * Sessions) %>%
    select(Student, Sessions, `Total Hours`, `Minutes Per Session`) %>%
    arrange(desc(`Total Hours`))
}
