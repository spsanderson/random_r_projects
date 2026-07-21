library(dplyr)
library(tidyr)

set.seed(42)
n <- 3000

# Distribution setup
claim_types <- sample(
  c("Outpatient", "Inpatient", "Recurring"),
  size = n, replace = TRUE, prob = c(0.80, 0.10, 0.10)
)

payers <- c("Medicare NY", "NYS Medicaid", "Anthem BCBS", "Optum/UHC", "Aetna", 
            "Cigna", "Humana", "Centene", "Molina", "Travelers", 
            "State Farm", "Allstate", "Liberty Mutual", "EmblemHealth", "Fidelis")

payer_types <- c("Governmental", "Governmental", "Commercial PPO", "Commercial HMO", "Commercial PPO",
                 "Commercial HMO", "Commercial PPO", "Governmental Managed", "Governmental Managed", "Workers Comp",
                 "No-Fault", "No-Fault", "Workers Comp", "Commercial HMO", "Governmental Managed")

payer_df <- data.frame(PayerName = payers, PayerType = payer_types)

errors <- c("NONE", "CO-197", "CO-16", "CO-97", "CO-27", "CO-15", "CO-26")

df <- data.frame(
  ClaimID = paste0("CLM-", 100000 + 1:n),
  ClaimType = claim_types
) %>%
  mutate(
    TOB = case_when(
      ClaimType == "Outpatient" ~ "0131",
      ClaimType == "Inpatient" ~ "0111",
      ClaimType == "Recurring" ~ "0137"
    ),
    PayerIndex = sample(1:15, n, replace = TRUE),
    SubmissionDate = as.Date("2026-01-01") + sample(0:90, n, replace = TRUE),
    ErrorCode = sample(errors, n, replace = TRUE, prob = c(0.70, 0.08, 0.07, 0.06, 0.04, 0.03, 0.02)),
    ResubmissionCount = ifelse(ErrorCode == "NONE", 0, sample(1:4, n, replace = TRUE, prob = c(0.6, 0.25, 0.1, 0.05))),
    PaymentDays = ifelse(ErrorCode == "NONE", sample(10:25, n, replace = TRUE), sample(28:75, n, replace = TRUE)),
    BilledAmount = case_when(
      ClaimType == "Outpatient" ~ round(runif(n, 300, 3500), 2),
      ClaimType == "Inpatient" ~ round(runif(n, 12000, 85000), 2),
      ClaimType == "Recurring" ~ round(runif(n, 1500, 9000), 2)
    )
  ) %>%
  left_join(payer_df %>% mutate(PayerIndex = row_number()), by = "PayerIndex") %>%
  select(ClaimID, TOB, ClaimType, PayerName, PayerType, SubmissionDate, ErrorCode, ResubmissionCount, PaymentDays, BilledAmount)

# Head output
glimpse(df)
