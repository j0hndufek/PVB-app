# ============================================================================
# Pension Valuation Trainer — Present Value of Benefits (PVB)
#
# Teaching tool: lets a new analyst pick a Life Type and a Benefit Type,
# see the PVB formula for that benefit laid out age-by-age, then click any
# component of that formula to see how it was built up.
#
# ALL ASSUMPTIONS ARE ILLUSTRATIVE — see actuarial_engine.R.
# ============================================================================

library(shiny)
library(DT)
library(dplyr)

source("actuarial_engine.R")

# ---------------------------------------------------------------------------
# Formatting helpers
# ---------------------------------------------------------------------------
fmt_dollar <- function(x, digits = 0) paste0("$", formatC(x, format = "f", digits = digits, big.mark = ","))
fmt_pct    <- function(x, digits = 2) paste0(formatC(100 * x, format = "f", digits = digits), "%")
fmt_num    <- function(x, digits = 4) formatC(x, format = "f", digits = digits)

# Column metadata per benefit type: label + formatting type, in display order
COLUMN_META <- list(
  Retirement = list(
    age                   = list(label = "Age",                      type = "int"),
    years_of_service      = list(label = "Years of Svc",              type = "int"),
    fap                   = list(label = "Final Avg Pay",             type = "dollar"),
    accrued_benefit       = list(label = "Accrued Benefit",           type = "dollar"),
    life_annuity_value    = list(label = "Life Annuity Value",        type = "dollar"),
    js_value              = list(label = "Joint & Survivor Value",    type = "dollar"),
    blended_annuity_value = list(label = "Blended Annuity Value",     type = "dollar"),
    prob_retire           = list(label = "Prob(Retirement)",          type = "pct"),
    prob_survive          = list(label = "Prob(Survival)",            type = "pct"),
    discount_factor       = list(label = "Discount Factor",           type = "num"),
    pvb                   = list(label = "PVB",                       type = "dollar")
  ),
  Death = list(
    age             = list(label = "Age",              type = "int"),
    salary          = list(label = "Salary",            type = "dollar"),
    death_benefit   = list(label = "Death Benefit",     type = "dollar"),
    prob_death      = list(label = "Prob(Death)",       type = "pct"),
    prob_survive    = list(label = "Prob(Survival)",    type = "pct"),
    discount_factor = list(label = "Discount Factor",   type = "num"),
    pvb             = list(label = "PVB",               type = "dollar")
  ),
  Disability = list(
    age                       = list(label = "Age",                    type = "int"),
    accrued_benefit           = list(label = "Accrued Benefit",        type = "dollar"),
    disability_annuity_value  = list(label = "Disability Annuity Value", type = "dollar"),
    prob_disability           = list(label = "Prob(Disability)",       type = "pct"),
    prob_survive              = list(label = "Prob(Survival)",         type = "pct"),
    discount_factor           = list(label = "Discount Factor",        type = "num"),
    pvb                       = list(label = "PVB",                    type = "dollar")
  ),
  `Vested / Terminated` = list(
    age                      = list(label = "Age",                    type = "int"),
    accrued_benefit_at_term  = list(label = "Accrued Benefit @ Term",  type = "dollar"),
    deferred_benefit_pv      = list(label = "Deferred Benefit PV",     type = "dollar"),
    prob_terminate           = list(label = "Prob(Termination)",       type = "pct"),
    prob_survive             = list(label = "Prob(Survival)",          type = "pct"),
    discount_factor          = list(label = "Discount Factor",         type = "num"),
    pvb                      = list(label = "PVB",                     type = "dollar")
  )
)

get_full_table <- function(benefit_type, participant) {
  switch(benefit_type,
    "Retirement"           = build_retirement_table(participant),
    "Death"                = build_death_table(participant),
    "Disability"           = build_disability_table(participant),
    "Vested / Terminated"  = build_vested_term_table(participant)
  )
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- fluidPage(
  title = "Pension Valuation Trainer",
  tags$head(tags$style(HTML("
    .well { background-color: #f7f8fa; }
    h4.panel-title { margin-top: 0; color: #1a3c5e; }
    .formula-box { background: #eef3f8; border-left: 4px solid #1a5f9e;
                   padding: 10px 14px; margin-bottom: 12px; font-size: 15px; }
    .hint-text { color: #6b7280; font-size: 12px; margin-top: 6px; }
    table.dataTable td { cursor: pointer; }
  "))),

  titlePanel("Pension Valuation Trainer: Present Value of Benefits (PVB)"),

  fluidRow(
    column(
      width = 3,
      wellPanel(
        h4("Life Type", class = "panel-title"),
        radioButtons("life_type", NULL,
                      choices = c("Replacement", "Current"),
                      selected = "Current")
      ),
      wellPanel(
        h4("Benefit Type", class = "panel-title"),
        radioButtons("benefit_type", NULL,
                      choices = c("Retirement", "Death", "Disability", "Vested / Terminated"),
                      selected = "Retirement")
      )
    ),

    column(
      width = 9,
      wellPanel(
        h4("Present Value of Benefits", class = "panel-title"),
        uiOutput("top_formula"),
        p(class = "hint-text", "Click any cell below to break that component down further."),
        DTOutput("pvb_table")
      ),
      wellPanel(
        h4("Component Breakdown", class = "panel-title"),
        uiOutput("bottom_formula"),
        DTOutput("component_table")
      )
    )
  )
)

# ---------------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  participant <- reactive({ get_participant(input$life_type) })

  full_table <- reactive({ get_full_table(input$benefit_type, participant()) })

  # Reset the drill-down selection whenever the inputs change
  observeEvent(list(input$life_type, input$benefit_type), {
    output$bottom_formula <- renderUI(
      p(class = "hint-text", "Click a component in the table above to see its breakdown.")
    )
    output$component_table <- renderDT(NULL)
  })

  # ---- TOP PANEL: overall formula + benefit-specific formula ----
  output$top_formula <- renderUI({
    bt <- input$benefit_type
    summ <- pvb_summary(participant())
    terms <- names(summ)
    term_strs <- sapply(terms, function(t) {
      label <- paste0("PVB_{", gsub(" ", "\\\\ ", t), "}")
      if (t == bt) paste0("\\mathbf{", label, "}") else label
    })
    top_eq <- paste0("\\[PVB = ", paste(term_strs, collapse = " + "), "\\]")

    formula_specific <- switch(bt,
      "Retirement" = "\\[PVB_{Retirement} = \\sum_{r=55}^{75} \\Big(JS_r + LA_r\\Big) \\times q^{(r)}_{r} \\times {}_{r-e}p^{(\\tau)}_{e} \\times v^{\\,r-e}\\]",
      "Death" = "\\[PVB_{Death} = \\sum_{x=e}^{74} DB_x \\times q^{(d)}_{x} \\times {}_{x-e}p^{(\\tau)}_{e} \\times v^{\\,x-e}\\]",
      "Disability" = "\\[PVB_{Disability} = \\sum_{x=e}^{74} \\Big(AB_x \\times \\ddot{a}_x\\Big) \\times q^{(i)}_{x} \\times {}_{x-e}p^{(\\tau)}_{e} \\times v^{\\,x-e}\\]",
      "Vested / Terminated" = "\\[PVB_{VT} = \\sum_{x=e}^{59} \\Big(AB_x \\times \\ddot{a}_{65} \\times {}_{65-x}p_{x} \\times v^{\\,65-x}\\Big) \\times q^{(w)}_{x} \\times {}_{x-e}p^{(\\tau)}_{e} \\times v^{\\,x-e}\\]"
    )

    p_active <- participant()
    withMathJax(
      div(class = "formula-box",
          HTML(top_eq),
          tags$hr(),
          HTML(formula_specific),
          p(class = "hint-text",
            sprintf("e = entry age (%d) \u00b7 %s life \u00b7 current age %d \u00b7 v computed at %.1f%% interest",
                    p_active$entry_age, p_active$life_type, p_active$current_age, INTEREST_RATE * 100))
      )
    )
  })

  # ---- TOP TABLE: age-by-age breakdown ----
  output$pvb_table <- renderDT({
    bt <- input$benefit_type
    meta <- COLUMN_META[[bt]]
    df <- full_table()[, names(meta), drop = FALSE]
    colnames(df) <- sapply(meta, function(m) m$label)

    dt <- datatable(
      df, rownames = FALSE, selection = list(mode = "single", target = "cell"),
      options = list(paging = FALSE, searching = FALSE, ordering = FALSE,
                      info = FALSE, dom = "t", scrollX = TRUE)
    )
    for (col in names(meta)) {
      lbl <- meta[[col]]$label
      dt <- switch(meta[[col]]$type,
        "dollar" = dt %>% formatCurrency(lbl, currency = "$", digits = 0),
        "pct"    = dt %>% formatPercentage(lbl, digits = 2),
        "num"    = dt %>% formatRound(lbl, digits = 4),
        dt
      )
    }
    last_label <- meta[[length(meta)]]$label
    dt %>% formatStyle(last_label, fontWeight = "bold") # bold PVB col
  })

  # ---- Click handler: figure out which component was clicked ----
  observeEvent(input$pvb_table_cell_clicked, {
    info <- input$pvb_table_cell_clicked
    req(info$col, !is.null(info$row))
    bt <- input$benefit_type
    meta <- COLUMN_META[[bt]]
    col_names <- names(meta)
    if (info$col < 0 || info$col >= length(col_names)) return(NULL)
    clicked_col <- col_names[info$col + 1]  # DT is 0-indexed
    row_data <- full_table()[info$row, ]
    p <- participant()

    result <- component_breakdown(bt, clicked_col, row_data, p)

    output$bottom_formula <- renderUI({
      withMathJax(
        div(class = "formula-box",
            h5(result$title),
            HTML(result$formula),
            if (!is.null(result$note)) p(class = "hint-text", result$note)
        )
      )
    })

    output$component_table <- renderDT({
      if (is.null(result$table)) return(NULL)
      datatable(result$table, rownames = FALSE,
                options = list(paging = FALSE, searching = FALSE, ordering = FALSE,
                                info = FALSE, dom = "t"))
    })
  })

  # ---------------------------------------------------------------------
  # component_breakdown(): builds the LaTeX + table for the bottom panel
  # ---------------------------------------------------------------------
  component_breakdown <- function(benefit_type, col, row, p) {
    e <- p$entry_age
    age <- row$age

    switch(paste(benefit_type, col, sep = "::"),

      # ---------------- RETIREMENT ----------------
      "Retirement::fap" = {
        yrs <- (age - 2):age
        sal <- sapply(yrs, salary_at_age, participant = p)
        tbl <- data.frame(Age = yrs, `Projected Salary` = fmt_dollar(sal), check.names = FALSE)
        list(title = sprintf("Final Average Pay @ age %d", age),
             formula = sprintf("\\[FAP_{%d} = \\frac{1}{3}\\sum_{k=%d}^{%d} Salary_k = %s\\]",
                                age, age - 2, age, fmt_dollar(row$fap)),
             table = tbl,
             note = sprintf("Salary projected forward from current salary (%s) at a %.1f%% annual scale.",
                             fmt_dollar(p$current_salary), SALARY_SCALE * 100))
      },
      "Retirement::accrued_benefit" = list(
        title = sprintf("Accrued Benefit @ age %d", age),
        formula = sprintf("\\[AB_{%d} = FAP_{%d} \\times \\text{Accrual Rate} \\times YOS = %s \\times %.1f\\%% \\times %d = %s\\]",
                           age, age, fmt_dollar(row$fap), ACCRUAL_RATE * 100, row$years_of_service, fmt_dollar(row$accrued_benefit)),
        table = NULL
      ),
      "Retirement::life_annuity_value" = list(
        title = sprintf("Life Annuity Value @ age %d", age),
        formula = sprintf(
          "\\[LA_{%d} = (1-\\%%JS) \\times \\ddot{a}_{%d} \\times AB_{%d} = %.0f\\%% \\times %s \\times %s = %s\\]",
          age, age, age, (1 - PCT_ELECT_JS) * 100, fmt_num(row$life_annuity_factor, 3),
          fmt_dollar(row$accrued_benefit), fmt_dollar(row$life_annuity_value)),
        table = NULL,
        note = sprintf("Assumes %.0f%% of retirees elect a single life annuity. \u00e4 is the life annuity-due factor: \u00e4_x = \u03a3 v^t \u00b7 tpx (post-retirement mortality).",
                        (1 - PCT_ELECT_JS) * 100)
      ),
      "Retirement::js_value" = list(
        title = sprintf("Joint & Survivor Value @ age %d", age),
        formula = sprintf(
          "\\[JS_{%d} = \\%%JS \\times \\ddot{a}^{JS}_{%d} \\times AB_{%d} = %.0f\\%% \\times %s \\times %s = %s\\]",
          age, age, age, PCT_ELECT_JS * 100, fmt_num(row$js_annuity_factor, 3),
          fmt_dollar(row$accrued_benefit), fmt_dollar(row$js_value)),
        table = NULL,
        note = sprintf("\u00e4^{JS} = \u00e4_x + %.0f%% \u00d7 \u03a3 v^t \u00b7 tpy \u00b7 (1-tpx), i.e. participant's own annuity plus the PV of %.0f%% continuing to a spouse (assumed 3 yrs younger) after the participant's death.",
                        JS_SURVIVOR_PCT * 100, JS_SURVIVOR_PCT * 100)
      ),
      "Retirement::blended_annuity_value" = list(
        title = sprintf("Blended Annuity Value @ age %d", age),
        formula = sprintf("\\[Blended_{%d} = LA_{%d} + JS_{%d} = %s + %s = %s\\]",
                           age, age, age, fmt_dollar(row$life_annuity_value), fmt_dollar(row$js_value),
                           fmt_dollar(row$blended_annuity_value)),
        table = NULL,
        note = "Expected annuity value blending the assumed election mix between the two payment forms."
      ),
      "Retirement::prob_retire" = ,
      "Retirement::prob_survive" = decrement_component(benefit_type, col, row, p, source_col = "qx_ret", label = "Retirement", age_range = 55:75),

      "Retirement::discount_factor" = discount_component(age, e),
      "Retirement::pvb" = list(
        title = sprintf("Full PVB Recomposition @ age %d", age),
        formula = sprintf(
          "\\[PVB_{%d} = (JS_{%d}+LA_{%d}) \\times q^{(r)}_{%d} \\times {}_{%d}p^{(\\tau)}_{%d} \\times v^{%d} \\]\\[= %s \\times %s \\times %s \\times %s = %s\\]",
          age, age, age, age, age - e, e, age - e,
          fmt_dollar(row$blended_annuity_value), fmt_pct(row$prob_retire), fmt_pct(row$prob_survive, 4),
          fmt_num(row$discount_factor, 4), fmt_dollar(row$pvb)),
        table = NULL
      ),

      # ---------------- DEATH ----------------
      "Death::salary" = list(
        title = sprintf("Salary @ age %d", age),
        formula = sprintf("\\[Salary_{%d} = Salary_{current} \\times (1+scale)^{%d-%d} = %s\\]",
                           age, age, p$current_age, fmt_dollar(row$salary)),
        table = NULL
      ),
      "Death::death_benefit" = list(
        title = sprintf("Death Benefit @ age %d", age),
        formula = sprintf("\\[DB_{%d} = %.1f \\times Salary_{%d} = %.1f \\times %s = %s\\]",
                           age, DEATH_BENEFIT_MULT, age, DEATH_BENEFIT_MULT, fmt_dollar(row$salary), fmt_dollar(row$death_benefit)),
        table = NULL,
        note = "Illustrative pre-retirement death benefit: a flat multiple of pay (common proxy for basic life insurance coverage in a pension valuation)."
      ),
      "Death::prob_death" = ,
      "Death::prob_survive" = decrement_component(benefit_type, col, row, p, source_col = "qx_mort", label = "Death"),
      "Death::discount_factor" = discount_component(age, e),
      "Death::pvb" = list(
        title = sprintf("Full PVB Recomposition @ age %d", age),
        formula = sprintf(
          "\\[PVB_{%d} = DB_{%d} \\times q^{(d)}_{%d} \\times {}_{%d}p^{(\\tau)}_{%d} \\times v^{%d} = %s \\times %s \\times %s \\times %s = %s\\]",
          age, age, age, age - e, e, age - e, fmt_dollar(row$death_benefit), fmt_pct(row$prob_death, 3),
          fmt_pct(row$prob_survive, 4), fmt_num(row$discount_factor, 4), fmt_dollar(row$pvb)),
        table = NULL
      ),

      # ---------------- DISABILITY ----------------
      "Disability::accrued_benefit" = list(
        title = sprintf("Accrued Benefit @ age %d", age),
        formula = sprintf("\\[AB_{%d} = FAP_{%d} \\times \\text{Accrual Rate} \\times YOS = %s\\]", age, age, fmt_dollar(row$accrued_benefit)),
        table = NULL
      ),
      "Disability::disability_annuity_value" = list(
        title = sprintf("Disability Annuity Value @ age %d", age),
        formula = sprintf("\\[DIS_{%d} = AB_{%d} \\times \\ddot{a}_{%d} = %s \\times %s = %s\\]",
                           age, age, age, fmt_dollar(row$accrued_benefit), fmt_num(row$annuity_factor, 3), fmt_dollar(row$disability_annuity_value)),
        table = NULL,
        note = "Assumes the accrued benefit becomes payable immediately for life upon disablement, valued using the same post-decrement mortality table used for retiree annuities (simplification)."
      ),
      "Disability::prob_disability" = ,
      "Disability::prob_survive" = decrement_component(benefit_type, col, row, p, source_col = "qx_dis", label = "Disability"),
      "Disability::discount_factor" = discount_component(age, e),
      "Disability::pvb" = list(
        title = sprintf("Full PVB Recomposition @ age %d", age),
        formula = sprintf(
          "\\[PVB_{%d} = DIS_{%d} \\times q^{(i)}_{%d} \\times {}_{%d}p^{(\\tau)}_{%d} \\times v^{%d} = %s \\times %s \\times %s \\times %s = %s\\]",
          age, age, age, age - e, e, age - e, fmt_dollar(row$disability_annuity_value), fmt_pct(row$prob_disability, 3),
          fmt_pct(row$prob_survive, 4), fmt_num(row$discount_factor, 4), fmt_dollar(row$pvb)),
        table = NULL
      ),

      # ---------------- VESTED / TERMINATED ----------------
      "Vested / Terminated::accrued_benefit_at_term" = list(
        title = sprintf("Accrued Benefit @ Termination, age %d", age),
        formula = sprintf("\\[AB_{%d} = FAP_{%d} \\times \\text{Accrual Rate} \\times YOS = %s\\]", age, age, fmt_dollar(row$accrued_benefit_at_term)),
        table = NULL
      ),
      "Vested / Terminated::deferred_benefit_pv" = list(
        title = sprintf("Deferred Vested Benefit PV @ age %d", age),
        formula = sprintf(
          "\\[DEF_{%d} = AB_{%d} \\times \\ddot{a}_{65} \\times {}_{%d}p_{%d} \\times v^{%d} = %s \\times %s \\times %s \\times %s = %s\\]",
          age, age, 65 - age, age, 65 - age, fmt_dollar(row$accrued_benefit_at_term), fmt_num(row$annuity_factor_at_NRA, 3),
          fmt_pct(row$prob_survive_to_NRA, 3), fmt_num(row$discount_factor_to_NRA, 4), fmt_dollar(row$deferred_benefit_pv)),
        table = NULL,
        note = "Benefit is frozen at termination and paid starting at Normal Retirement Age (65); value reflects surviving to 65 and discounting the deferral period."
      ),
      "Vested / Terminated::prob_terminate" = ,
      "Vested / Terminated::prob_survive" = decrement_component(benefit_type, col, row, p, source_col = "qx_term", label = "Termination", age_range = 20:59),
      "Vested / Terminated::discount_factor" = discount_component(age, e),
      "Vested / Terminated::pvb" = list(
        title = sprintf("Full PVB Recomposition @ age %d", age),
        formula = sprintf(
          "\\[PVB_{%d} = DEF_{%d} \\times q^{(w)}_{%d} \\times {}_{%d}p^{(\\tau)}_{%d} \\times v^{%d} = %s \\times %s \\times %s \\times %s = %s\\]",
          age, age, age, age - e, e, age - e, fmt_dollar(row$deferred_benefit_pv), fmt_pct(row$prob_terminate, 3),
          fmt_pct(row$prob_survive, 4), fmt_num(row$discount_factor, 4), fmt_dollar(row$pvb)),
        table = NULL
      ),

      # ---------------- default / age column ----------------
      list(title = "No further breakdown", formula = "", table = NULL,
           note = "Select a numeric component cell (not the Age column) to see its breakdown.")
    )
  }

  # Shared helper: discount factor breakdown
  discount_component <- function(age, e) {
    n <- age - e
    v <- discount_factor(n)
    list(
      title = sprintf("Discount Factor (entry age %d \u2192 %d)", e, age),
      formula = sprintf("\\[v^{%d} = \\frac{1}{(1+i)^{%d}} = \\frac{1}{(1+%.2f)^{%d}} = %s\\]",
                         n, n, INTEREST_RATE, n, fmt_num(v, 4)),
      table = NULL,
      note = "Valued from ENTRY AGE (Entry-Age-Normal style PVFB), not from the current valuation date -- that's why n = attained age minus entry age rather than minus current age."
    )
  }

  # Shared helper: decrement-rate breakdown (used for prob_retire/prob_death/
  # prob_disability/prob_terminate AND prob_survive, since both trace back to
  # the same decrement table / survival chain)
  decrement_component <- function(benefit_type, col, row, p, source_col, label, age_range = NULL) {
    age <- row$age
    e <- p$entry_age
    if (grepl("prob_survive$", col)) {
      chain <- survival_chain(e, age)
      chain_display <- data.frame(
        Age = chain$age,
        `Total Decrement q` = fmt_pct(chain$q_total, 2),
        `Survival p` = fmt_pct(chain$p_survive, 2),
        `Cumulative Survival` = fmt_pct(chain$cumulative_p, 2),
        check.names = FALSE
      )
      list(
        title = sprintf("Probability of Survival (entry age %d \u2192 %d)", e, age),
        formula = sprintf("\\[{}_{%d}p^{(\\tau)}_{%d} = \\prod_{k=%d}^{%d} (1 - q^{(\\tau)}_k) = %s\\]",
                           age - e, e, e, age - 1, fmt_pct(row$prob_survive, 3)),
        table = chain_display,
        note = "q^(\u03c4) is the TOTAL decrement rate (mortality + termination + disability + retirement, as applicable) — the probability of leaving active service for ANY reason at each age."
      )
    } else {
      lo <- max(20, age - 3); hi <- min(80, age + 3)
      if (!is.null(age_range)) { lo <- max(lo, min(age_range)); hi <- min(hi, max(age_range)) }
      ctx <- DECR[DECR$age >= lo & DECR$age <= hi, c("age", source_col)]
      colnames(ctx) <- c("Age", paste0(label, " Rate"))
      ctx[[2]] <- fmt_pct(ctx[[2]], 2)
      rate <- decrement_row(age)[[source_col]]
      list(
        title = sprintf("%s Decrement Rate @ age %d", label, age),
        formula = sprintf("\\[q^{(%s)}_{%d} = %s\\]", substr(label, 1, 1), age, fmt_pct(rate, 3)),
        table = ctx,
        note = "Illustrative assumption table shown for nearby ages — replace with your firm's actual decrement study results."
      )
    }
  }
}

shinyApp(ui, server)
