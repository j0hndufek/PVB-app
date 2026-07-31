# ============================================================================
# actuarial_engine.R
# Illustrative Entry-Age-Normal style PVB calculation engine
# ALL ASSUMPTIONS BELOW ARE ILLUSTRATIVE / FOR TRAINING PURPOSES ONLY.
# Swap these out for your firm's actual assumption tables before using this
# for anything beyond teaching new analysts how the pieces fit together.
# ============================================================================

library(dplyr)

# ---------------------------------------------------------------------------
# 1. GLOBAL ECONOMIC / DEMOGRAPHIC ASSUMPTIONS
# ---------------------------------------------------------------------------

INTEREST_RATE      <- 0.065    # annual valuation discount rate
SALARY_SCALE       <- 0.035   # annual salary growth assumption
ACCRUAL_RATE       <- 0.016   # benefit accrual % of FAP per year of service
NRA                <- 65      # normal retirement age
MAX_AGE            <- 120     # oldest age in mortality tables (limiting age)
JS_SURVIVOR_PCT     <- 0.50   # % of benefit continuing to spouse under J&S form
PCT_ELECT_JS        <- 0.70   # % of retirees assumed to elect Joint & Survivor
DEATH_BENEFIT_MULT  <- 2.0    # pre-retirement death benefit = multiple x pay
SPOUSE_AGE_DIFF      <- -3    # spouse assumed 3 yrs younger than participant

# ---------------------------------------------------------------------------
# 2. PARTICIPANT PROFILES  ("Life Type" selector)
#    Current      = an actual active employee already on the census
#    Replacement  = a hypothetical new hire used to represent a future
#                    replacement life (e.g., open-group / workforce planning)
# ---------------------------------------------------------------------------

get_participant <- function(life_type = c("Current", "Replacement")) {
  life_type <- match.arg(life_type)
  if (life_type == "Current") {
    list(
      life_type      = "Current",
      entry_age      = 24,
      current_age    = 24,
      current_salary = 125593
    )
  } else {
    list(
      life_type      = "Replacement",
      entry_age      = 20,
      current_age    = 24,
      current_salary = 125593
    )
  }
}

# ---------------------------------------------------------------------------
# 3. DECREMENT TABLES (illustrative, ages 20-120)
#    - qx_mort : pre-retirement mortality
#    - qx_term : withdrawal / termination (stops at age 60)
#    - qx_dis  : disability incidence
#    - qx_ret  : retirement rate (only defined/nonzero ages 55-75; forced @75)
#    - qx_post : post-retirement / disabled mortality (for annuity factors)
# ---------------------------------------------------------------------------

# Active decrements
build_active_decrement_table <- function() {
  ages <- 20:75
  df_active <- data.frame(age = ages)
  df_active$qx_ret <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.26, 0.14, 0.135, 0.135, 0.135, 0.175, 0.12, 0.12, 0.25, 0.25, 0.27, 0.27, 0.27, 0.27, 0.28, 0.4, 0.4, 0.4, 0.4, 0.4, 1)
  df_active$qx_mort <- c(0.000370, 0.000357, 0.000333, 0.000309, 0.000284, 0.000281, 0.000314, 0.000336, 0.000358, 0.000394, 0.000416, 0.000451, 0.000471, 0.000503, 0.000531, 0.000570, 0.000589, 0.000616, 0.000636, 0.000662, 0.000680, 0.000692, 0.000708, 0.000729, 0.000745, 0.000767, 0.000795, 0.000820, 0.000852, 0.000884, 0.000925, 0.000973, 0.001024, 0.001084, 0.001153, 0.001232, 0.001320, 0.001416, 0.001519, 0.001626, 0.001741, 0.001861, 0.001983, 0.002108, 0.002245, 0.002376, 0.002519, 0.002661, 0.002816, 0.003004, 0.003201, 0.003422, 0.003685, 0.003969, 0.004302, 0.004302)
  df_active$qx_dis <- c(0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000060, 0.000060, 0.000060, 0.000060, 0.000060, 0.000120, 0.000120, 0.000120, 0.000120, 0.000120, 0.000120, 0.000120, 0.000120, 0.000120, 0.000120, 0.000180, 0.000180, 0.000240, 0.000240, 0.000240, 0.000300, 0.000300, 0.000360, 0.000420, 0.000480, 0.000540, 0.000660, 0.000840, 0.000900, 0.001080, 0.001260, 0.001440, 0.001620, 0.001860, 0.002100, 0.002400, 0.002640, 0.003000, 0.003360, 0.003480, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000)
  df_active$qx_turn <- c(0.0925, 0.0425, 0.0325, 0.0325, 0.0225, 0.0175, 0.015, 0.015, 0.015, 0.0125, 0.0075, 0.0075, 0.0075, 0.0075, 0.0075, 0.007, 0.007, 0.007, 0.007, 0.007, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  df_active
}

#build_decrement_table <- function() {
  #ages <- 20:MAX_AGE
  #df <- data.frame(age = ages)

  #df$qx_mort <- pmin(0.0004 * exp(0.075 * (df$age - 20)), 1)

  #df$qx_term <- ifelse(df$age < 60, pmax(0.15 - 0.0045 * (df$age - 20), 0.01), 0)

  #df$qx_dis <- pmin(0.00015 + 0.00006 * pmax(df$age - 20, 0), 0.02)

  # Retirement rates: only "live" ages 55-75, forced retirement (q=1) at 75
  #ret_ages <- 55:75
  #ret_rates <- c(0.26, 0.14, 0.135, 0.135, 0.135, 0.175, 0.12, 0.12, 0.25, 0.25,
                 #0.27,           # age 65 - NRA spike
                 #0.27, 0.27, 0.27, 0.28, 0.4, 0.4, 0.4, 0.4, 0.4,
                 #1.00)           # age 75 - forced retirement
  #df$qx_ret <- 0
  #df$qx_ret[match(ret_ages, df$age)] <- ret_rates

  # Post-retirement / disabled-life mortality (heavier than active mortality)
  #df$qx_post <- pmin(0.0009 * exp(0.09 * (df$age - 20)), 1)

  #df
#}

DECR <- build_active_decrement_table()

decrement_row <- function(age) DECR[DECR$age == age, ]

# Total decrement rate while ACTIVE (used for "in service" survivorship).
# Before retirement eligibility (age < 55) this is mort + term + disability.
# From age 55-74 it also includes the retirement decrement; at 75, q=1.
total_active_decrement <- function(age) {
  r <- decrement_row(age)
  if (nrow(r) == 0) return(1)
  q <- r$qx_mort + r$qx_term + r$qx_dis + r$qx_ret
  min(q, 1)
}

# ---------------------------------------------------------------------------
# 4. SURVIVAL PROBABILITIES
# ---------------------------------------------------------------------------

# Probability of remaining in active service from `from_age` to `to_age`
# (product of (1 - total decrement) for each intervening age)
survival_to_age <- function(from_age, to_age) {
  if (to_age <= from_age) return(1)
  ages <- from_age:(to_age - 1)
  prod(sapply(ages, function(a) 1 - total_active_decrement(a)))
}

# Year-by-year chain used for the drill-down display
survival_chain <- function(from_age, to_age) {
  if (to_age <= from_age) {
    return(data.frame(age = from_age, q_total = 0, p_survive = 1, cumulative_p = 1))
  }
  ages <- from_age:(to_age - 1)
  q <- sapply(ages, total_active_decrement)
  p <- 1 - q
  cum <- cumprod(p)
  data.frame(age = ages, q_total = q, p_survive = p, cumulative_p = cum)
}

# Post-retirement survival probability (for annuity factors), simple product
tpx_post <- function(age, t, mort_col = "qx_post") {
  if (t == 0) return(1)
  ages <- age:(age + t - 1)
  qs <- sapply(ages, function(a) {
    r <- decrement_row(a)
    if (nrow(r) == 0) return(1)
    r[[mort_col]]
  })
  prod(1 - qs)
}

# ---------------------------------------------------------------------------
# 5. DISCOUNTING
# ---------------------------------------------------------------------------

discount_factor <- function(n) 1 / (1 + INTEREST_RATE) ^ n

# ---------------------------------------------------------------------------
# 6. SALARY & BENEFIT PROJECTIONS
# ---------------------------------------------------------------------------

salary_at_age <- function(age, participant) {
  participant$current_salary *
    (1 + SALARY_SCALE) ^ (age - participant$current_age)
}

# Final Average Pay: average of projected salary in the 3 years ending at `age`
fap_at_age <- function(age, participant) {
  mean(sapply((age - 2):age, salary_at_age, participant = participant))
}

accrued_benefit_at_age <- function(age, participant) {
  years_of_service <- age - participant$entry_age
  fap_at_age(age, participant) * ACCRUAL_RATE * years_of_service
}

# Early/late retirement adjustment factor applied to the accrued benefit
retirement_adjustment_factor <- function(age) {
  if (age < NRA) {
    pmax(1 - 0.05 * (NRA - age), 0.40)   # 5%/yr early retirement reduction
  } else if (age > NRA) {
    1 + 0.03 * (age - NRA)               # simple late-retirement increase
  } else {
    1
  }
}

# ---------------------------------------------------------------------------
# 7. ANNUITY FACTORS
# ---------------------------------------------------------------------------

# Single life annuity-due factor at age x: sum_t v^t * tpx
life_annuity_factor <- function(age) {
  max_t <- MAX_AGE - age
  ts <- 0:max_t
  vs <- discount_factor(ts)
  ps <- sapply(ts, function(t) tpx_post(age, t))
  sum(vs * ps)
}

# Joint & X% Survivor annuity-due factor:
#   PV = a_x  +  survivor_pct * sum_t v^t * tpy * (1 - tpx)
# where y = contingent spouse age. Reflects benefit continuing to spouse
# only in years after the participant has died (assumes independence).
joint_survivor_annuity_factor <- function(age, survivor_pct = JS_SURVIVOR_PCT) {
  spouse_age <- age + SPOUSE_AGE_DIFF
  max_t <- MAX_AGE - age
  ts <- 0:max_t
  vs <- discount_factor(ts)
  tpx <- sapply(ts, function(t) tpx_post(age, t))
  tpy <- sapply(ts, function(t) tpx_post(spouse_age, t))
  a_x <- sum(vs * tpx)
  survivor_pv <- sum(vs * tpy * (1 - tpx))
  a_x + survivor_pct * survivor_pv
}

# ---------------------------------------------------------------------------
# 8. PVB BUILDERS PER BENEFIT TYPE
#    Each returns a data.frame, one row per decrement age, with every
#    intermediate component broken out as its own column so the app can
#    show / drill into any piece of the formula.
#    All survival & discounting is measured FROM ENTRY AGE (Entry Age Normal
#    style PVFB), consistent with the training sketch.
# ---------------------------------------------------------------------------

build_retirement_table <- function(participant) {
  entry_age <- participant$entry_age
  ages <- 55:75

  rows <- lapply(ages, function(r) {
    ab   <- accrued_benefit_at_age(r, participant) * retirement_adjustment_factor(r)
    laf  <- life_annuity_factor(r)
    jsf  <- joint_survivor_annuity_factor(r)

    life_annuity_value <- (1 - PCT_ELECT_JS) * laf * ab
    js_value            <- PCT_ELECT_JS * jsf * ab
    blended_value       <- life_annuity_value + js_value

    p_retire  <- decrement_row(r)$qx_ret
    p_survive <- survival_to_age(entry_age, r)
    v         <- discount_factor(r - entry_age)

    pvb <- blended_value * p_retire * p_survive * v

    data.frame(
      age = r,
      years_of_service = r - entry_age,
      fap = fap_at_age(r, participant),
      accrued_benefit = ab,
      life_annuity_factor = laf,
      js_annuity_factor = jsf,
      life_annuity_value = life_annuity_value,
      js_value = js_value,
      blended_annuity_value = blended_value,
      prob_retire = p_retire,
      prob_survive = p_survive,
      discount_factor = v,
      pvb = pvb
    )
  })
  bind_rows(rows)
}

build_death_table <- function(participant) {
  entry_age <- participant$entry_age
  ages <- entry_age:74
  ages <- ages[ages >= max(entry_age, participant$current_age - 0)] # from now on
  ages <- entry_age:74  # keep full working lifetime for teaching purposes

  rows <- lapply(ages, function(x) {
    sal <- salary_at_age(x, participant)
    death_benefit <- DEATH_BENEFIT_MULT * sal

    q_death   <- decrement_row(x)$qx_mort
    p_survive <- survival_to_age(entry_age, x)
    v         <- discount_factor(x - entry_age)

    pvb <- death_benefit * q_death * p_survive * v

    data.frame(
      age = x,
      salary = sal,
      death_benefit_multiple = DEATH_BENEFIT_MULT,
      death_benefit = death_benefit,
      prob_death = q_death,
      prob_survive = p_survive,
      discount_factor = v,
      pvb = pvb
    )
  })
  bind_rows(rows)
}

build_disability_table <- function(participant) {
  entry_age <- participant$entry_age
  ages <- entry_age:74

  rows <- lapply(ages, function(x) {
    ab  <- accrued_benefit_at_age(x, participant)
    laf <- life_annuity_factor(x)   # disabled-life annuity, reuses qx_post table
    disability_pv <- ab * laf

    q_dis     <- decrement_row(x)$qx_dis
    p_survive <- survival_to_age(entry_age, x)
    v         <- discount_factor(x - entry_age)

    pvb <- disability_pv * q_dis * p_survive * v

    data.frame(
      age = x,
      accrued_benefit = ab,
      annuity_factor = laf,
      disability_annuity_value = disability_pv,
      prob_disability = q_dis,
      prob_survive = p_survive,
      discount_factor = v,
      pvb = pvb
    )
  })
  bind_rows(rows)
}

build_vested_term_table <- function(participant) {
  entry_age <- participant$entry_age
  ages <- entry_age:59  # withdrawal assumption is 0 after age 60

  rows <- lapply(ages, function(x) {
    ab_at_term <- accrued_benefit_at_age(x, participant)
    laf_nra    <- life_annuity_factor(NRA)
    p_defer    <- tpx_post(x, NRA - x, mort_col = "qx_mort")  # survive to NRA
    v_defer    <- discount_factor(NRA - x)

    deferred_benefit_pv <- ab_at_term * laf_nra * p_defer * v_defer

    q_term    <- decrement_row(x)$qx_term
    p_survive <- survival_to_age(entry_age, x)
    v         <- discount_factor(x - entry_age)

    pvb <- deferred_benefit_pv * q_term * p_survive * v

    data.frame(
      age = x,
      accrued_benefit_at_term = ab_at_term,
      annuity_factor_at_NRA = laf_nra,
      prob_survive_to_NRA = p_defer,
      discount_factor_to_NRA = v_defer,
      deferred_benefit_pv = deferred_benefit_pv,
      prob_terminate = q_term,
      prob_survive = p_survive,
      discount_factor = v,
      pvb = pvb
    )
  })
  bind_rows(rows)
}

# ---------------------------------------------------------------------------
# 9. TOP-LEVEL PVB SUMMARY (sum of all four benefit types)
# ---------------------------------------------------------------------------

pvb_summary <- function(participant) {
  c(
    Retirement = sum(build_retirement_table(participant)$pvb),
    Death = sum(build_death_table(participant)$pvb),
    Disability = sum(build_disability_table(participant)$pvb),
    `Vested / Terminated` = sum(build_vested_term_table(participant)$pvb)
  )
}
