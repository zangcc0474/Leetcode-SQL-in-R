library(tidyverse)

attendance_event <- tibble(date = c("2019-10-01",
                                    "2019-10-02",
                                    "2019-10-01",
                                    "2019-10-02",
                                    "2019-10-01",
                                    "2019-10-02"),
                           student_id = c("1", "1",
                                          "2", "2",
                                          "3", "3"),
                           attendance = c("Yes", "Yes", "No", "No", "Yes", "No"))

all_student <- tibble(student_id = c("1",
                                     "2",
                                     "3"),
                      school_id = c("1", "1", "1"),
                      grade_level = c("1", "2", "1"),
                      date_of_birth = c("2009-10-01",
                                        "2009-10-02",
                                        "2009-10-02"),
                      hometown = c("A", "B", "C"))

attendance_event %>%
  dplyr::full_join(all_student) %>%
  dplyr::mutate(birthday_attendance = str_sub(date, start = 6L, end = -1L) == str_sub(date_of_birth, start = 6L, end = -1L)&
                  attendance == "Yes") %>%
  dplyr::group_by(birthday_attendance) %>%
  dplyr::summarise(student_by_group = n_distinct(student_id)) %>%
  dplyr::mutate(percentage = student_by_group/sum(student_by_group))

attendance_event %>%
  dplyr::full_join(all_student) %>%
  dplyr::select(date, student_id, attendance, grade_level) %>%
  dplyr::distinct() %>%
  dplyr::group_by(date, grade_level) %>%
  dplyr::summarise(fck = mean(attendance == "Yes"))

#' Leetcode 1241
#' 

submission <- tibble::tibble(sub_id = c(1, 2, 1, 12, 3, 5, 3, 4, 9, 10, 6),
                             parent_id = c(NA, NA, NA, NA,
                                           1, 2, 1, 1, 1, 2, 7))

post_id <- submission %>%
  dplyr::filter(is.na(parent_id)) %>%
  dplyr::select(post_id = sub_id) 

post_id %>%
  dplyr::left_join(submission,
                   by = c("post_id" = "parent_id")) %>%
  dplyr::group_by(post_id) %>%
  # dplyr::distinct() %>%
  dplyr::summarise(sum(!is.na(unique(sub_id))))


#' Leetcode 1251
#' 

prices <- tibble::tibble(product_id = c(1, 1, 2, 2),
                         start_date = c("2019-02-17", "2019-03-01",
                                        "2019-02-01", "2019-02-21"),
                         end_date = c("2019-02-28", "2019-03-22",
                                      "2019-02-20", "2019-03-31"),
                         price = c(5, 20, 15, 30))

units_sold <- tibble::tibble(product_id = c(1, 1, 2, 2),
                             purchase_date = c("2019-02-25", "2019-03-01",
                                               "2019-02-10", "2019-03-22"),
                             units = c(100, 15, 200, 30))

prices %>%
  dplyr::full_join(units_sold) %>%
  dplyr::mutate_at(vars(contains("date")),
                   as.Date) %>%
  dplyr::filter(purchase_date <= end_date,
                purchase_date >= start_date) %>%
  dplyr::group_by(product_id) %>%
  # dplyr::summarise(average_price = round(weighted.mean(price, units), 2))
  dplyr::summarise(average_price = sum(units*price)/sum(units))


#' Leetcode 1280
#' 

student <- tibble::tibble(student_id = c(1, 2, 13, 6),
                          student_name = c("Alice", "Bob", "John", "Alex"))

subject <- tibble::tibble(subject_name = c("Math", "Physics", "Programming"))

examination <- tibble::tibble(student_id = c(1, 1, 1, 2,
                                             1, 1, 13, 13, 
                                             13, 2, 1),
                              subject_name = c("Math", "Physics", "Programming", "Programming",
                                               "Physics", "Math", "Math", "Programming",
                                               "Physics", "Math", "Math"))
expand.grid(student_name = student$student_name,
            subject_name = subject$subject_name) %>%
  dplyr::left_join(student) %>%
  dplyr::full_join(examination %>%
                     dplyr::group_by(student_id, subject_name) %>%
                     dplyr::add_count(name = "attended_exams") %>%
                     dplyr::distinct()) %>%
  dplyr::arrange(student_id) %>%
  dplyr::mutate(attended_exams = ifelse(is.na(attended_exams), 
                                      0,
                                      attended_exams))


#' Leetcode 176
#'

employee <- tibble::tibble(id = c("1", "2", "3"),
                           salary = c(100, 200, 300))

employee %>%
  dplyr::mutate(second_highest_salary = dplyr::nth(salary,2))


#' Leetcode 586
#' 

orders <- tibble::tibble(order_number = c(1:4),
                         customer_number = c(1, 2, 3, 3))

orders %>%
  dplyr::add_count(customer_number) %>%
  dplyr::filter(n == max(n))



#' Leetcode 597
#' 

friend_request <- tibble::tibble(sender_id = c(1, 1, 1, 2, 3),
                                 send_to_id = c(2, 3, 4, 3, 4),
                                 request_date = c("2016-06-01",
                                                  "2016-06-01",
                                                  "2016-06-01",
                                                  "2016-06-02",
                                                  "2016-06-09"))

request_accepted <- tibble::tibble(requester_id = c(1, 1, 2, 3, 3),
                                   accepter_id = c(2, 3, 3, 4, 4),
                                   accept_date = c("2016-06-03",
                                                   "2016-06-08",
                                                   "2016-06-08",
                                                   "2016-06-09",
                                                   "2016-06-10"))

friend_request %>%
  dplyr::full_join(request_accepted,
                   by = c("sender_id" = "requester_id",
                          "send_to_id" = "accepter_id")) %>%
  dplyr::distinct(sender_id, send_to_id, .keep_all = TRUE) %>%
  dplyr::mutate(accept_rate = sum(!is.na(accept_date))/dplyr::n())


#' Leetcode 534
#' 

activity <- tibble::tibble(player_id = c(1, 1, 1, 3, 3),
                           device_id = c(2, 2, 3, 1, 4),
                           event_date = c("2016-03-01", "2016-05-02",
                                          "2017-06-25", "2016-03-02",
                                          "2018-07-03"),
                           game_played = c(5, 6, 1, 0, 5))


activity %>%
  dplyr::mutate(unique_str = paste0(event_date, player_id)) %>%
  split(.$unique_str) %>%
  purrr::map(.f = ~.x %>%
               dplyr::left_join(dplyr::select(activity, 
                                              temp_player_id  = player_id,
                                              temp_event_date = event_date,
                                              temp_game_played = game_played),
                                by = c("player_id" = "temp_player_id")) %>%
               dplyr::filter(event_date >= temp_event_date) %>%
               dplyr::group_by(player_id, event_date) %>%
               dplyr::summarise(games_played_so_far = sum(temp_game_played))) %>%
  dplyr::bind_rows() %>%
  dplyr::arrange(player_id)


#' Leetcode 550
#' 
library(zoo)
activity <- tibble::tibble(player_id = c(1, 1, 2, 3, 3),
                           device_id = c(2, 2, 3, 1, 4),
                           event_date = c("2016-03-01", "2016-03-02",
                                          "2017-06-25", "2016-03-02",
                                          "2018-07-03"),
                           game_played = c(5, 6, 1, 0, 5))

activity %>%
  dplyr::mutate(event_date = as.Date(event_date)) %>%
  dplyr::group_by(player_id) %>%
  dplyr::arrange(event_date, .by_group = TRUE) %>%
  dplyr::mutate(cons = event_date - dplyr::lag(event_date)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(player_id, cons) %>%
  dplyr::summarise(fraction = sum(cons == 1, na.rm = TRUE)/n_distinct(player_id))


#' Leetcode 570
#' 

employee <- tibble::tibble(id = c(101, 102, 103, 104, 105, 106),
                           name = c("John", "Dan", "James", "Amy", "Anne", "Ron"),
                           department = c("A", "A", "A", "A", "A", "B"),
                           managerid = c(NA, 101, 101, 101, 101, 101))

employee %>%
  dplyr::filter(id %in% employee$managerid)


#‘ Leetcode 1077

project <- tibble::tibble(project_id = c(1, 1, 1, 2, 2),
                          employee_id = c(1, 2, 3, 1, 4))

employee <- tibble::tibble(employee_id = c(1, 2, 3, 4),
                              name = c("K", "A", "J", "D"),
                              experience_years = c(3, 2, 3, 2))

project %>%
  dplyr::full_join(employee,
                   by = c("employee_id" = "employee_id")) %>%
  dplyr::group_by(project_id) %>%
  dplyr::filter(experience_years == max(experience_years)) %>%
  dplyr::select(project_id, employee_id)


#' Leetcode 1098

#' subtract year from a date: 
#' library(lubridate)
#' ymd("2010/03/17") - years(2)
books <- tibble::tibble(book_id = c(1, 2, 3, 4, 5),
                        name = c("Kalila And Demna", "28 Letters",
                                 "The Hobbit", "13 Reasons Why",
                                 "The Hunger Games"),
                        available_from = c("2010-01-01", "2012-05-12",
                                           "2019-06-10", "2019-06-01",
                                           "2008-09-21"))

orders <- tibble::tibble(order_id = c(1, 2, 3, 4, 5, 6, 7),
                         book_id = c(1, 1, 3, 4, 4, 5, 5),
                         quantity = c(2, 1, 8, 6, 5, 9, 8),
                         dispatch_date = c("2018-07-26", "2018-11-05", "2019-06-11",
                                           "2019-06-05", "2019-06-20", "2009-02-02", 
                                           "2010-04-13"))

books %>%
  dplyr::mutate(available_from = zoo::as.Date(available_from)) %>%
  dplyr::full_join(orders,
                   by = c("book_id" = "book_id")) %>%
  dplyr::filter(available_from + 30 < zoo::as.Date("2019-06-23")) %>%
  dplyr::mutate(quliftied_quantity = case_when(is.na(dispatch_date) ~ 0,
                                               ymd("2018-06-23") < ymd(dispatch_date) &
                                               ymd(dispatch_date) < ymd("2019-06-23") ~ quantity,
                                               TRUE ~ 0)) %>%
  dplyr::group_by(book_id) %>%
  dplyr::filter(sum(quliftied_quantity) < 10) %>%
  dplyr::select(book_id, name) %>%
  dplyr::distinct()

books %>%
  dplyr::left_join(orders %>%
                     dplyr::mutate(dispatch_date = as.Date(dispatch_date)) %>%
                     dplyr::filter(dispatch_date < as.Date("2019-06-23"),
                                   dispatch_date > as.Date("2018-06-23")) %>%
                     dplyr::group_by(book_id) %>%
                     dplyr::summarise(quantity = sum(quantity)),
                   by = c("book_id" = "book_id")) %>%
  dplyr::mutate(quantity = ifelse(is.na(quantity),
                                  0, 
                                  quantity)) %>%
  dplyr::mutate(available_from = zoo::as.Date(available_from)) %>%
  dplyr::filter(available_from + 30 < zoo::as.Date("2019-06-23")) %>%
  dplyr::filter(quantity < 10)
  

#' Leetcode 1107

traffic <- tibble::tibble(user_id = c(1, 1, 1,
                                      2, 2,
                                      3, 3, 3,
                                      4, 4, 4,
                                      5, 5, 5, 5),
                          activity = c("in", "home", "out",
                                       "in", "out",
                                       "in", "jobs", "out",
                                       "in", "groups", "out",
                                       "in", "out", "in", "out"),
                          activity_date = c("2019-05-01", "2019-05-01", "2019-05-01",
                                            "2019-06-21", "2019-06-21",
                                            "2019-01-01", "2019-01-01", "2019-01-01",
                                            "2019-06-21", "2019-06-21", "2019-06-21",
                                            "2019-03-01", "2019-03-01", "2019-06-21", "2019-06-21"))

traffic %>%
  dplyr::mutate(activity_date = zoo::as.Date(activity_date)) %>%
  dplyr::left_join(traffic %>%
                     dplyr::distinct() %>%
                     dplyr::mutate(activity_date = zoo::as.Date(activity_date)) %>%
                     dplyr::group_by(user_id) %>%
                     dplyr::arrange(activity_date, .by_group = TRUE) %>%
                     dplyr::slice(1) %>%
                     dplyr::select(user_id,
                                   first_login_date = activity_date),
                   by = c("user_id" = "user_id")) %>%
  dplyr::filter(activity_date == first_login_date) %>%
  dplyr::filter(zoo::as.Date("2019-06-30") - activity_date <= 90) %>%
  dplyr::group_by(activity_date) %>%
  dplyr::summarise(user_count = dplyr::n_distinct(user_id))


#' Leetcode 1112

enrollments <- tibble::tibble(student_id = c(2, 2, 1, 1, 3, 3, 3),
                              course_id = c(2, 3, 1, 2, 1, 2, 3),
                              grade = c(95, 95, 90, 99, 80, 75, 82))

enrollments %>%
  dplyr::group_by(student_id) %>%
  dplyr::filter(grade == max(grade)) %>%
  dplyr::arrange(course_id, .by_group = TRUE) %>%
  dplyr::slice(1)

#' Leetcode 1126

events <- tibble::tibble(business_id = c(1, 3, 1, 2, 3, 1, 2),
                         event_type = c("r", "r", "a", "a", "a", "p", "p"),
                         occurentces = c(7, 3, 11, 7, 6, 3, 12))

events %>%
  dplyr::group_by(event_type) %>%
  dplyr::mutate(avg_event_occurentces = mean(occurentces)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(occurentces > avg_event_occurentces) %>%
  dplyr::group_by(business_id) %>%
  dplyr::filter(dplyr::n_distinct(event_type) > 1) %>%
  dplyr::select(business_id) %>%
  dplyr::distinct()


#' Leetcode 1132

actions <- tibble::tibble(user_id = c(1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 5),
                          post_id = c(1, 1, 1, 2, 2, 4, 4, 3, 3, 2, 2, 5, 5),
                          action_date = c("2019-07-01", "2019-07-01", "2019-07-01",
                                          "2019-07-04", "2019-07-04", "2019-07-04", "2019-07-04",
                                          "2019-07-02", "2019-07-02",
                                          "2019-07-03", "2019-07-03", "2019-07-03", "2019-07-03"),
                          action = c("v", "l", "s", "v", "r", "v", "r", "v", "r", "v", "r", "v", "r"),
                          extra = c("NULL", "NULL", "NULL", "NULL", "spam", "NULL", "spam", "NULL", "spam", "NULL",
                                    "racism", "NULL", "racism"))

removals <- tibble::tibble(post_id = c(2,3),
                           remove_date = c("2019-07-20",
                                           "2019-07-18"))

actions %>%
  dplyr::left_join(removals,
                   by = c("post_id" = "post_id")) %>%
  dplyr::filter(action == "r",
                extra == "spam") %>%
  dplyr::group_by(action_date) %>%
  dplyr::summarise(daily_spam_remove = mean(extra == "spam" & !is.na(remove_date))) %>%
  dplyr::summarise(average_daily_percent = mean(daily_spam_remove))


#' Leetcode 1149

views <- tibble::tibble(article_id = c(1, 3, 1, 2, 2, 4, 3, 3),
                        author_id = c(3, 4, 3, 7, 7, 7, 4, 4),
                        viewer_id = c(5, 5, 6, 7, 6, 1, 4, 4),
                        view_date = c("2019-08-01", "2019-08-01", "2019-08-02", "2019-08-01",
                                      "2019-08-02", "2019-07-22", "2019-07-21", "2019-07-21"))


views %>%
  dplyr::group_by(view_date) %>%
  dplyr::group_by(viewer_id, view_date) %>%
  dplyr::mutate(unique_view = dplyr::n_distinct(article_id)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(unique_view > 1) %>%
  dplyr::select(id = viewer_id) %>%
  dplyr::distinct()
  

#' Leetcode 1158

users <- tibble::tibble(user_id = c(1, 2, 3, 4),
                        join_date = c("2018-01-01", "2018-02-09", "2018-01-19", "2018-05-21"),
                        favorite_brand = c("Lenovo", "Samsung", "LG", "HP"))


orders <- tibble::tibble(order_id = c(1, 2, 3, 4, 5, 6),
                         order_date = c("2019-08-01", "2018-08-02", "2019-08-03", "2018-08-04", "2018-08-04", "2019-08-05"),
                         item_id = c(4, 2, 3, 1, 1, 2),
                         buyer_id = c(1, 1, 2, 4, 3, 2),
                         seller_id = c(2, 3, 3, 2, 4, 4))

users %>%
  dplyr::left_join(orders %>%
                     dplyr::mutate_at(dplyr::vars(dplyr::contains("date")),
                                      zoo::as.Date),
                   by = c("user_id" = "buyer_id")) %>%
  dplyr::group_by(user_id) %>%
  dplyr::mutate(orders_in_2019 = dplyr::n_distinct(order_id[str_sub(order_date, start = 1L, end = 4L) == "2019"])) %>%
  dplyr::select(buyer_id = user_id, join_date, orders_in_2019) %>%
  dplyr::distinct()


#' Leetcode 1164

products <- tibble::tibble(product_id = c(1, 2, 1, 1, 2, 3),
                           new_price = c(20, 50, 30, 35, 65, 20),
                           change_date = c("2019-08-14", "2019-08-14", "2019-08-15", "2019-08-16",
                                           "2019-08-17", "2019-08-18"))



products %>%
  dplyr::mutate_at(dplyr::vars(dplyr::contains("date")),
                   zoo::as.Date) %>%
  dplyr::group_by(product_id) %>%
  dplyr::arrange(change_date) %>%
  dplyr::slice(1) %>%
  dplyr::mutate(price = case_when(change_date > zoo::as.Date("2019-08-16") ~ 10,
                                  TRUE ~ new_price))


#' Leetcode 1174

delivery <- tibble::tibble(delivery_id = c(1, 2, 3, 4, 5, 6, 7),
                           customer_id = c(1, 2, 1, 3, 3, 2, 4),
                           order_date = c("2019-08-01", "2019-08-02", "2019-08-11",
                                          "2019-08-24", "2019-08-21", "2019-08-11",
                                          "2019-08-09"),
                           customer_pref_delivery_date = c("2019-08-02", "2019-08-02",
                                                           "2019-08-12", "2019-08-24",
                                                           "2019-08-22", "2019-08-13",
                                                           "2019-08-09"))



delivery %>%
  dplyr::mutate_at(dplyr::vars(contains("date")),
                   zoo::as.Date) %>%
  dplyr::group_by(customer_id) %>%
  dplyr::arrange(order_date) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::summarise(immediate_percentage = round(mean(order_date == customer_pref_delivery_date) * 100,
                                                digits = 2))

#' Leetcode 1193


transactions <- tibble::tibble(id = c(121, 122, 123, 124),
                               country = c("US", "US", "US", "DE"),
                               state = c("approved", "declined", "approved", "approved"),
                               amount = c(1000, 2000, 2000, 2000),
                               trans_date = c("2018-12-18", "2018-12-19",
                                              "2019-01-01", "2019-01-07"))

transactions %>%
  dplyr::mutate(month = str_sub(trans_date, start = 1L, end = 7L)) %>%
  dplyr::group_by(month, country) %>%
  dplyr::mutate(trans_count = dplyr::n_distinct(id),
                approved_count = sum(state == "approved"),
                trans_total_amount = sum(amount),
                approved_total_amount = sum(amount[state == "approved"])) %>%
  dplyr::ungroup() %>%
  dplyr::select(month, country, trans_count, approved_count,
                trans_total_amount, approved_total_amount) %>%
  dplyr::distinct()


#' Leetcode 1204

queue <- tibble::tibble(person_id = c(5, 3, 6, 2, 4, 1),
                        person_name = c("GW", "JA", "TJ", "WJ", "TJII", "JE"),
                        weight = c(250, 350, 400, 200, 175, 500),
                        turn = c(1, 2, 3, 4, 5, 6))




queue %>%
  dplyr::arrange(turn) %>%
  dplyr::mutate(accm_weight = cumsum(weight)) %>%
  dplyr::filter(accm_weight >= 1000) %>%
  dplyr::slice(1)


#' Leetcode 1205


# transactions <- tibble::tibble(id = c(101, 102, 103, 104, 105),
#                                country = c("US", "US", "US", "US", "US"),
#                                state = c("approved", "declined", "approved", "approved", "approved"),
#                                amount = c(1000, 2000, 3000, 4000, 5000),
#                                trans_date = c("2019-05-18", "2019-05-19", "2019-06-10",
#                                               "2019-06-13", "2019-06-15"))
# 
# chargebacks <- tibble::tibble(trans_id = c(102, 101, 105),
#                               trans_date = c("2019-05-29", "2019-06-30", "2019-09-18"))
# 
# transactions %>%
#   dplyr::full_join(chargebacks,
#                    by = c("id" = "trans_id")) %>% View()
#   dplyr::mutate(month = str_sub(trans_date, start = 1L, end = 7L)) %>%
#   dplyr::group_by(month, country) %>%
#   dplyr::mutate(approved_count = sum(state == "approved"),
#                 approved_amount = sum(amount[state == "approved"]),
#                 chargeback_count = sum(!is.na(trans_date_chargebacks)),
#                 chargeback_amount = sum(amount[!is.na(trans_date_chargebacks)])) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(month, country, contains("approved"),
#                 starts_with("chargeback")) %>%
#   dplyr::distinct()
  

#' Leetcode 1212

teams <- tibble::tibble(team_id = c(10, 20, 30, 40 , 50),
                        team_name = c("Leetcode", "NewYork", "Atlanta", "Chicago", "Toronto"))

matches <- tibble::tibble(match_id = c(1, 2, 3, 4, 5),
                          host_team = c(10, 30, 10, 20, 50),
                          guest_team = c(20, 10, 50, 30, 30),
                          host_goals = c(3, 2, 5, 1, 1),
                          guest_goals = c(0, 2, 1, 0, 0))

teams %>%
  dplyr::left_join(dplyr::select(matches,
                                 host_team, host_goals),
                   by = c("team_id" = "host_team")) %>%
  dplyr::group_by(team_id) %>%
  dplyr::summarise(host_goals = sum(host_goals))
  dplyr::left_join(dplyr::select(matches,
                                 guest_team, guest_goals),
                   by = c("team_id" = "guest_team")) %>%
  dplyr::mutate(num_points = host_goals + guest_goals) %>%
  dplyr::select(team_id, team_name, num_points)








  
  


