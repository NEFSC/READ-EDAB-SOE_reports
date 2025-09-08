## uses all_times object saved from chunk hooks
all_times2 <- unlist(all_times)
all_times3 <- tibble::tibble(chunk = names(all_times2), value = all_times2)

write.csv(all_times3, here::here("utils/chunk_times.csv"))
