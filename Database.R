library(tidyverse)
library(mongolite)
library(comprehenr)

Sys.setenv(TZ='CST6CDT')

# setwd("/Users/jeremydumalig/Desktop/Productivity/HOPES")

connection_url = "mongodb+srv://jdumalig:SanFranciscoWheels2025@hopes.rmwnyws.mongodb.net/"

multiple_select_options <- 
  read_csv("multiple_select_options.csv") %>%
  mutate(Grade = as.integer(Grade))
vocab_choices =
  (multiple_select_options %>%
     filter(Grade == -1,
            Subject == "Language"))$Options

grade_grad_years_table <- function() {
  current_year <- as.integer(substr(Sys.Date(), 0, 4))
  
  grades <- c("K", seq(1:8))
  grad_years <- seq(from=2025, to=2033) # + (current_year - 2023)
  
  df <- data.frame(grades, rev(grad_years))
  colnames(df) <- c("Grade", "8th Grade Graduation Year")
  
  return(df)
}

get_coaches <- function() {
  db <- mongo(db="hopes4kids",
              collection="coaches",
              url=connection_url)
  
  sort(db$find()$coach)
}
add_coach <- function(coach) {
  db <- mongo(db="hopes4kids",
              collection="coaches",
              url=connection_url)
  
  if (!coach %in% db$find()$coach) {
    db$insert( data.frame(coach) )
  }
}
remove_coach <- function(coach){ 
  db <- mongo(db="hopes4kids",
              collection="coaches",
              url=connection_url)
  
  db$remove( paste0('{\"coach" : \"', coach, '\"}') )
}

get_students <- function(df=F) {
  db <- mongo(db="hopes4kids",
              collection="students",
              url=connection_url)
  
  if (df) {
    db$find()
  } else {
    sort(db$find()$student)
  }
}
get_students_grades <- function() {
  db <- mongo(db="hopes4kids",
              collection="students",
              url=connection_url)
  
  db$find()
}
add_student <- function(student, grade) {
  Sys.setenv(TZ='CST6CDT')
  
  db <- mongo(db="hopes4kids",
              collection="students",
              url=connection_url)
  
  entrance <- Sys.Date()
  # graduation <- (grade_grad_years_table() %>% 
  #                  filter(Grade == grade))$`8th Grade Graduation Year`
  
  if (!student %in% db$find()$student) {
    db$insert( data.frame(student, entrance, grade) )
  }
}
remove_student <- function(student){ 
  db <- mongo(db="hopes4kids",
              collection="students",
              url=connection_url)
  
  db$remove( paste0('{\"student" : \"', student, '\"}') )
}

get_subjects <- function() {
  db <- mongo(db="hopes4kids",
              collection="subjects",
              url=connection_url)
  
  sort(db$find()$subject)
}
add_subject <- function(subject) {
  db <- mongo(db="hopes4kids",
              collection="subjects",
              url=connection_url)
  
  if (!subject %in% db$find()$subject) {
    db$insert( data.frame(subject) )
  }
}
remove_subject <- function(subject){ 
  db <- mongo(db="hopes4kids",
              collection="subjects",
              url=connection_url)
  
  db$remove( paste0('{\"subject" : \"', subject, '\"}') )
}

clock <- function(time, zone, coach, students, subject, check_in=FALSE, check_out=FALSE) {
  db <- mongo(db="hopes4kids",
              collection="clocks",
              url=connection_url)
  
  action <- (if (check_in) "In" else "Out")
  
  for (student in students) {
    db$insert( data.frame(coach, student, subject,
                          time, zone, 
                          action) )
  }
}

clock_submit <- function(time, zone, coach, students, subject, action="Submit") {
  db <- mongo(db="hopes4kids",
              collection="clocks",
              url=connection_url)
  
  for (student in students) {
    db$insert( data.frame(coach, student, subject, time, zone, action) )
  }
}

get_all_clock_submits <- function() {
  db <-
    mongo(db="hopes4kids",
          collection="clocks",
          url=connection_url)
  
  db$find() %>%
    mutate(Date = as.Date( substr(as.character(time), 1, 10) ),
           Time = substr(as.character(time), 12, 16)) %>%
    filter(action == "Submit")
}

get_all_clocks <- function(unmatched=FALSE, filtered=TRUE) {
  db <-
    mongo(db="hopes4kids",
          collection="clocks",
          url=connection_url)
  
  df <-
    db$find() %>%
    mutate(Date = as.Date( substr(as.character(time), 1, 10) ),
           Time = substr(as.character(time), 12, 16)) %>%
    filter(action != "Submit")
  
  if (unmatched) {
    df <-
      df %>%
      group_by(coach, student, Date) %>%
      filter(!(any(action == "In") & any(action == "Out"))) %>%
      ungroup() %>%
      mutate(Selections = paste(Date, Time, coach, student, subject, sep=" | "))
    
    df
  } else if (filtered) {
    df <-
      df %>%
      group_by(Date, coach, student) %>%
      filter(any(action == "In") & any(action == "Out")) %>%
      ungroup() %>%
      group_by(Date, coach, student, action) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(Selections = paste(Date, Time, coach, student, subject, sep=" | ")) %>%
      arrange(Date, coach, student, action)
    
    df
  } else {
    df
  }
}

get_clocks <- function() {
  db <- mongo(db="hopes4kids",
              collection="clocks",
              url=connection_url)
  
  db$find() %>%
    mutate(Date = as.Date( substr(as.character(time), 1, 10) ),
           Time = substr(as.character(time), 12, 16)) %>%
    group_by(Date, coach, student) %>%
    mutate(previous = lag(action),
           lag = case_when((is.na(previous)) ~ FALSE,
                           TRUE ~ (previous == "Out")),
           stint = cumsum(lag)) %>%
    ungroup() %>%
    select(-previous, -lag) %>%
    group_by(Date, coach, student, stint) %>%
    filter(any(action == "In") & any(action == "Out")) %>%
    # slice(c(1, n())) %>%
    slice(c(n() - 1, n())) %>%
    ungroup()
}

mult_choice <- function(strings) {
  paste(strings, collapse=" - ")
}

submit_session <- function(checkin, checkout, coach, students, subject,
                           completed, focused, checked, prepared, distracted,
                           assignment="", goal="", offline="", 
                           math_focused="", khan1="N/A", khan2="N/A",
                           mms="N/A", brain=c(), core_topics=c(),
                           words_to_review="",
                           khan_summary1="", khan_summary2="",
                           non_khan_summary1="", non_khan_summary2="",
                           
                           language_reading=c(), 
                           language_comprehension=c(), 
                           language_writing=c(), 
                           language_speaking=c(), 
                           language_grammar=c(), 
                           brain_book_input="", 
                           language_notes="", 
                           language_assignment="", 
                           reading_input=c(), 
                           
                           brain_book_chapter="",
                           brain_book_pages="",
                           
                           personal_book_name="", 
                           personal_book_pages="", 
                           personal_book_finished=c(), 
                           school_book_name="", 
                           school_book_pages="", 
                           school_book_finished=c(), 
                           
                           comprehension_material="", 
                           comprehension_input=c(), 
                           writing_input="", 
                           speaking_input="", 
                           grammar_input="",
                           
                           initial1=c(),
                           initial2=c(),
                           initial3=c(),
                           initial4=c(),
                           initial5=c(),
                           initial6=c(),
                           initial7=c(),
                           initial8=c(),
                           initial9=c(),
                           initial10=c(),
                           initial11=c(),
                           chapter1=c(),
                           chapter2=c(),
                           chapter3=c(),
                           chapter4=c(),
                           chapter5=c(),
                           chapter6=c(),
                           chapter7=c(),
                           chapter8=c(),
                           chapter9=c(),
                           chapter10=c(),
                           chapter11=c(),
                           
                           other_comments="") {
  
  db <- mongo(db="hopes4kids",
              collection="sessions",
              url=connection_url)
  
  completed <- mult_choice(completed)
  focused <- mult_choice(focused)
  checked <- mult_choice(checked)
  prepared <- mult_choice(prepared)
  distracted <- mult_choice(distracted)
  
  language_reading <- mult_choice(language_reading)
  language_comprehension <- mult_choice(language_comprehension)
  language_writing <- mult_choice(language_writing)
  language_speaking <- mult_choice(language_speaking)
  language_grammar <- mult_choice(language_grammar)
  reading_input <- mult_choice(reading_input)
  personal_book_finished <- mult_choice(personal_book_finished)
  school_book_finished <- mult_choice(school_book_finished)
  comprehension_input <- mult_choice(comprehension_input)
  
  initial1=mult_choice(initial1)
  initial2=mult_choice(initial2)
  initial3=mult_choice(initial3)
  initial4=mult_choice(initial4)
  initial5=mult_choice(initial5)
  initial6=mult_choice(initial6)
  initial7=mult_choice(initial7)
  initial8=mult_choice(initial8)
  initial9=mult_choice(initial9)
  initial10=mult_choice(initial10)
  initial11=mult_choice(initial11)
  
  chapter1=mult_choice(chapter1)
  chapter2=mult_choice(chapter2)
  chapter3=mult_choice(chapter3)
  chapter4=mult_choice(chapter4)
  chapter5=mult_choice(chapter5)
  chapter6=mult_choice(chapter6)
  chapter7=mult_choice(chapter7)
  chapter8=mult_choice(chapter8)
  chapter9=mult_choice(chapter9)
  chapter10=mult_choice(chapter10)
  chapter11=mult_choice(chapter11)
  
  brain <- mult_choice(brain)
  core_topics <- mult_choice(core_topics)
  
  multiple <- (length(students) > 1)
  
  math_focused = if (is.null(math_focused)) "" else math_focused
  language_reading = if (is.null(language_reading)) "" else language_reading
  language_comprehension = if (is.null(language_comprehension)) "" else language_comprehension
  language_writing = if (is.null(language_writing)) "" else language_writing
  language_speaking = if (is.null(language_speaking)) "" else language_speaking
  language_grammar = if (is.null(language_grammar)) "" else language_grammar
  language_notes = if (is.null(language_notes)) "" else language_notes
  
  personal_book_pages = if (is.null(personal_book_pages)) "" else personal_book_pages
  school_book_pages = if (is.null(school_book_pages)) "" else school_book_pages
  
  for (student in students) {
    other <- if (multiple) to_vec(for(i in students) if (i != student) i) else NA
    
    df = data.frame(checkin, checkout, coach, student, other, subject, 
                    completed, 
                    focused, math_focused, 
                    checked, prepared, distracted, 
                    brain, core_topics,
                    khan1, khan2, mms, 
                    assignment, goal, offline,
                    multiple, words_to_review,
                    khan_summary1, khan_summary2,
                    non_khan_summary1, non_khan_summary2,
                    
                    language_reading, 
                    language_comprehension, 
                    language_writing, 
                    language_speaking, 
                    language_grammar, 
                    brain_book_input, 
                    language_notes, 
                    language_assignment, 
                    reading_input, 
                    
                    brain_book_chapter,
                    brain_book_pages,
                    
                    personal_book_name, 
                    personal_book_pages,
                    personal_book_finished,
                    school_book_name, 
                    school_book_pages,
                    school_book_finished,
                    
                    comprehension_material, 
                    comprehension_input, 
                    writing_input, 
                    speaking_input, 
                    grammar_input,
                    
                    initial1,
                    initial2,
                    initial3,
                    initial4,
                    initial5,
                    initial6,
                    initial7,
                    initial8,
                    initial9,
                    initial10,
                    initial11,
                    chapter1,
                    chapter2,
                    chapter3,
                    chapter4,
                    chapter5,
                    chapter6,
                    chapter7,
                    chapter8,
                    chapter9,
                    chapter10,
                    chapter11,
                    
                    other_comments)

    db$insert(df)
  }
}

get_all_sessions <- function() {
  db <- mongo(db="hopes4kids",
              collection="sessions",
              url=connection_url)
  
  if (nrow(db$find()) != 0) {
    db$find() %>%
      merge(dplyr::select(get_students_grades(), student, entrance, grade),
            by="student",
            all.x=TRUE) # %>%
    # mutate(khan1 = as.integer(khan1),
    #        khan2 = as.integer(khan2),
    #        mms = as.integer(mms))
  } else
    data.frame()
}

years_since2023 <- function() {
  Sys.setenv(TZ='CST6CDT')
  
  floor(as.numeric(difftime(Sys.Date(), as.Date("2023-08-01"))) / 365)
}

grad_year_to_grade <- function(graduation) {
  Sys.setenv(TZ='CST6CDT')
  
  current_year <- year_to_grad(Sys.Date())
  
  return( 2024 - graduation + 8 )
}

year_to_grad <- function(date) {
  month <- as.numeric( substr(date, 6, 7) )
  year <- as.numeric( substr(date, 0, 4) )
  
  return( if (month < 8) (year) else (year + 1) )
}

get_grade <- function(names) {
  (get_students_grades() %>% 
     filter(student %in% names))$grade#  %>%
    # grad_year_to_grade() %>%
    # as.integer() %>%
    # paste(collapse=", ")
}

get_previous_multiple <- function(select_students, select_subjects, prev=F) {
  Sys.setenv(TZ='CST6CDT')
  
  db <- mongo(db="hopes4kids",
              collection="sessions",
              url=connection_url)
  
  db = db$find()
  
  if ("All" %in% select_students) {
    select_students <- unique(db$student)
  }
  if ("All" %in% select_subjects) {
    select_subjects <- unique(db$subject)
  }
  
  student1 <- select_students[1]
  student2 <- select_students[2]
  
  if (nrow(db) != 0) {
    if (prev) {
      get_all_sessions() %>%
        filter((((student == student1) & (other == student2)) |
                  ((student == student2) & (other == student1))),
               multiple,
               subject %in% select_subjects) %>%
        tail(1)
    } else {
      get_all_sessions() %>%
        filter((((student == student1) & (other == student2)) |
                  ((student == student2) & (other == student1))),
               multiple,
               subject %in% select_subjects)
    }
  } else {
    data.frame()
  }
}

get_sessions <- function(select_coaches, select_students, select_subjects, 
                         s=c(), e=c(),
                         select_grades="All", prev=FALSE) {
  
  Sys.setenv(TZ='CST6CDT')
  
  db <- mongo(db="hopes4kids",
              collection="sessions",
              url=connection_url)
  
  df = db$find()
  
  if ("All" %in% select_coaches) {
    select_coaches <- unique(df$coach)
  }
  if ("All" %in% select_students) {
    select_students <- unique(df$student)
  }
  if ("All" %in% select_subjects) {
    select_subjects <- unique(df$subject)
  }
  if (is.null(s)) {
    earliest <- as.Date(substr(min(get_all_sessions()$checkin), 0, 10), "%Y-%m-%d")
    latest <- as.Date(substr(max(get_all_sessions()$checkin), 0, 10), "%Y-%m-%d")
    
    s <- c(earliest, latest)
  }
  if (is.null(e)) {
    current_year <- as.integer(substr(Sys.Date(), 0, 4))
    most_years_in_program <- current_year - as.integer(substr(min(get_students_grades()$entrance), 0, 4)) + 1
    
    e <- c(1, most_years_in_program)
  }
  if ("All" %in% select_grades) {
    grad_years <- seq(from=2024, to=2035)
    select_grades = c("K", seq(1, 8))
  } else {
    select_grades <- gsub("K", "0", select_grades)
    select_grades <- to_vec(for(i in select_grades) as.numeric(i))
    
    year <- year_to_grad( Sys.Date() )
    grad_years <- to_vec(for(i in select_grades) (2032 - i + year - 2024))
  }
  
  if (nrow(db$find()) != 0) {
    current_year <- as.integer(substr(Sys.Date(), 0, 4))
    
    student_subject <-
      df %>%
      merge(dplyr::select(get_students_grades(), student, entrance, grade),
            by="student",
            all.x=TRUE) %>%
      mutate(Date = as.Date( substr(as.character(checkin), 1, 10) ),
             years_in_program = current_year - as.integer(substr(entrance, 0, 4)) + 1) %>%
      filter(coach %in% select_coaches,
             student %in% select_students,
             subject %in% select_subjects,
             grade %in% select_grades,
             between(Date, s[1], s[2]),
             between(years_in_program, e[1], e[2])) %>%
      arrange(checkout)
  } else {
    student_subject <- data.frame()
  }
  
  if ((prev) & (nrow(student_subject) != 0)) {
    return (tail(filter(student_subject, !multiple), 1))
  } else {
    return (student_subject)
  }
}

db_backup <- function() {
  for (i in c("clocks", "coaches", "sessions", "students", "subjects", "pairings", "valid_users")) {
    db <- mongo(db="hopes4kids",
                collection=i,
                url=connection_url)
    
    db$find() %>%
      write.csv(paste0(i, Sys.Date(), ".csv"), 
                row.names=TRUE)
  }
}

get_valid_users <- function() {
  db <- mongo(db="hopes4kids",
              collection="valid_users",
              url=connection_url)
  
  db$find()
}
add_valid_user <- function(name, username, password, admin=F) {
  db <- mongo(db="hopes4kids",
              collection="valid_users",
              url=connection_url)
  
  if (!name %in% db$find()$name) {
    db$insert( data.frame(name, username, password, admin) )
  }
}
remove_valid_user <- function(username){ 
  db <- mongo(db="hopes4kids",
              collection="valid_users",
              url=connection_url)
  
  db$remove( paste0('{\"username" : \"', username, '\"}') )
}

get_pairings <- function() {
  db <- mongo(db="hopes4kids",
              collection="pairings",
              url=connection_url)
  
  db$find()
}
add_pairing <- function(input_coach, input_student) {
  db <- mongo(db="hopes4kids",
              collection="pairings",
              url=connection_url)
  
  if (nrow(filter(db$find(), coach == input_coach, 
                  student == input_student)) == 0) {
    db$insert( data.frame(coach=input_coach, student=input_student) )
  }
}
remove_pairing <- function(coach, student){ 
  db <- mongo(db="hopes4kids",
              collection="pairings",
              url=connection_url)
  
  db$remove( paste0('{\"coach" : \"', coach, 
                    '\", \"student" : \"', student, '\"}') )
}

chapter_helper = function(col) {
  
  unique_vals =
    (col %>%
       paste(collapse = " - ") %>%
       str_split(" - "))[[1]] %>%
    unique()
  
  unique_vals = unique(str_split(paste(col, collapse=" - "), " - ")[[1]])
  
  return(unique_vals[grepl("Practice", unique_vals)])
}
