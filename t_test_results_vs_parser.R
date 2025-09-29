t_test_results_vs <- data.frame()

for (col in names(mtcars)[1:7]) {
    t_testv <- t.test(mtcars[[col]] ~ mtcars$vs)
    print(col)
    print(t_testv)
    t_test_results_vs <- rbind(t_test_results_vs, data.frame(
        column = col,
        factor = "vs",
        mean_difference = diff(t_testv$estimate),
        p_value = t_testv$p.value
    ))
}