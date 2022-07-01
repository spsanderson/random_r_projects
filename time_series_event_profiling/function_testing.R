# Forward Basis
td_lagged_tbl <- td %>%
    dplyr::select(date_col, rw_val, dplyr::everything()) %>%
    # Manipulation
    dplyr::mutate(
        lag_val = dplyr::lag(rw_val, 1),
        adj_diff = (rw_val - lag_val),
        relative_change_raw = adj_diff / lag_val
    ) %>%
    tidyr::drop_na(lag_val) 

td_pct_change_tbl <- td_lagged_tbl %>%
    dplyr::mutate(
        relative_change = round(relative_change_raw, precision),
        pct_chg_mark = ifelse(relative_change == percent_change, TRUE, FALSE),
        event_base_change = ifelse(pct_chg_mark == TRUE, 0, relative_change_raw),
        group_number = cumsum(pct_chg_mark)
    )  %>%
    dplyr::mutate(numeric_group_number = group_number) %>%
    dplyr::mutate(group_number = as.factor(group_number)) %>%
    dplyr::group_by(group_number) %>%
    dplyr::mutate(x = dplyr::row_number()) %>%
    #dplyr::mutate(x = 1:n()) %>%
    dplyr::ungroup() 
