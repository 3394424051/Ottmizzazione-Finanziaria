# ==============================================================================
# CONFRONTO DI 3 APPROCCI ALLA SELEZIONE E OTTIMIZZAZIONE DI PORTAFOGLIO
# OTTIMIZZAZIONE RICORSIVA, MARKOWITZ CLASSICO E QAOA QUANTISTICO
# R + RETICULATE + QISKIT + YAHOO FINANCE + GRAFICI + EXCEL
# ==============================================================================
#
# OBIETTIVO
# ---------
# Verificare se tre procedure differenti producono portafogli, rendimenti e
# livelli di rischio differenti, per individuare l'approccio preferibile.
#
# Tutti i portafogli sono valutati ex post con la stessa funzione:
#
#   F(w) = risk_factor * w' * cov * w - mu' * w
#
# con:
#   w_i >= 0
#   somma(w_i) = 1
#
# Un valore F(w) più basso è migliore. La decisione finale non dipende però dal
# solo objective: vengono confrontati anche rendimento, volatilità, Sharpe,
# drawdown, stabilità statistica e tempo di calcolo.
#
# DIFFERENZA FRA I TRE APPROCCI
# ----------------------------
# 1. OTTIMIZZAZIONE RICORSIVA
#    Costruisce il portafoglio un titolo alla volta. A ogni livello aggiunge
#    l'asset che riduce maggiormente F(w), usando pesi uguali sugli asset già
#    selezionati. È una procedura greedy ricorsiva, veloce ma non globalmente
#    ottima. Seleziona esattamente `budget` asset.
#
# 2. MARKOWITZ CLASSICO EQUIPONDERATO
#    Esamina tutte le combinazioni di `budget` asset con pesi uniformi 1/budget
#    e conserva quella con objective media-varianza più basso. Con pochi asset
#    costituisce il benchmark classico esatto nello spazio equiponderato.
#
# 3. QAOA QUANTISTICO
#    Risolve una formulazione binaria QUBO con esattamente `budget` asset.
#    Gli asset selezionati ricevono peso 1/budget. StatevectorSampler simula il
#    circuito: non viene utilizzato hardware quantistico reale.
#
# CONFRONTO CORRETTO
# ------------------
# Tutti i metodi selezionano esattamente `budget` asset e assegnano a ciascuno
# peso 1/budget. Le differenze dipendono quindi soltanto dalla selezione e dal
# procedimento di ricerca, non dalla struttura dei pesi.
# ==============================================================================

rm(list = ls(all.names = TRUE))
invisible(gc())

# 1. PACCHETTI -----------------------------------------------------------------
r_packages <- c("reticulate", "quantmod", "xts", "openxlsx")
missing_r <- r_packages[!vapply(r_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_r) > 0L) install.packages(missing_r, dependencies = TRUE)

suppressPackageStartupMessages({
  library(reticulate)
  library(quantmod)
  library(xts)
  library(openxlsx)
})

py_require(c("numpy", "qiskit", "qiskit-optimization", "qiskit-algorithms"))

# 2. PARAMETRI -----------------------------------------------------------------
file_path <- "Quantum_dataset.csv"
selection_path <- "selezioni_3_metodi.csv"
weights_path <- "pesi_3_metodi.csv"
metrics_path <- "metriche_3_metodi.csv"
graph_path <- "confronto_rendimenti_oos_3_metodi.jpg"
panel_graph_path <- "addenda_oos_3_grafici_separati.jpg"
xlsx_path <- "validazione_oos_3_metodi.xlsx"
significance_csv_path <- "significativita_pairwise_3_metodi.csv"

tickers <- c("IONQ", "QBTS", "RGTI", "IBM", "GOOGL", "MSFT", "NVDA", "QQQ")
start_date <- as.Date("2023-01-01")
end_date <- Sys.Date() + 1
update_from_yahoo <- TRUE

budget <- 3L
risk_factor <- 0.50
trading_days <- 252
risk_free_rate <- 0
random_seed <- 42L

qaoa_reps <- 2L
qaoa_maxiter <- 150L
qaoa_shots <- 4096L
train_fraction <- 0.70

if (budget < 1L || budget > length(tickers)) stop("Budget non valido.")

# 3. DOWNLOAD E LETTURA DATI ---------------------------------------------------
download_yahoo_prices <- function(symbols, from, to) {
  downloaded <- list()
  failed <- character(0)
  cat("\n=========================================\n")
  cat("DOWNLOAD DA YAHOO FINANCE\n")
  cat("=========================================\n")

  for (symbol in symbols) {
    cat("Download ", symbol, "... ", sep = "")
    raw <- tryCatch(
      quantmod::getSymbols(
        Symbols = symbol, src = "yahoo", from = from, to = to,
        periodicity = "daily", auto.assign = FALSE, warnings = FALSE
      ),
      error = function(e) NULL
    )
    if (is.null(raw)) {
      failed <- c(failed, symbol)
      cat("ERRORE\n")
      next
    }
    price <- tryCatch(quantmod::Ad(raw), error = function(e) NULL)
    if (is.null(price) || NCOL(price) != 1L) price <- quantmod::Cl(raw)
    colnames(price) <- symbol
    downloaded[[symbol]] <- price
    cat(NROW(price), " osservazioni\n")
  }

  if (length(failed) > 0L) stop("Download fallito: ", paste(failed, collapse = ", "))
  prices <- do.call(merge, c(downloaded, all = FALSE))
  colnames(prices) <- symbols
  prices <- na.omit(prices)
  values <- as.matrix(prices)
  if (NROW(prices) < 3L || any(!is.finite(values)) || any(values <= 0)) {
    stop("Prezzi insufficienti o non validi.")
  }
  prices
}

read_prices_csv <- function(path, symbols) {
  df <- read.csv2(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (NCOL(df) < 2L) stop("CSV privo delle colonne necessarie.")
  names(df)[1] <- "Date"
  missing <- setdiff(symbols, names(df))
  if (length(missing) > 0L) stop("Colonne mancanti: ", paste(missing, collapse = ", "))

  formats <- c("%d/%m/%Y", "%Y-%m-%d", "%d-%m-%Y", "%m/%d/%Y")
  dates <- NULL
  for (fmt in formats) {
    candidate <- as.Date(df$Date, format = fmt)
    if (!anyNA(candidate)) { dates <- candidate; break }
  }
  if (is.null(dates)) stop("Formato Date non riconosciuto.")

  parse_number <- function(x) {
    if (is.numeric(x)) return(as.numeric(x))
    x <- trimws(as.character(x))
    both <- grepl(",", x, fixed = TRUE) & grepl(".", x, fixed = TRUE)
    x[both] <- gsub(".", "", x[both], fixed = TRUE)
    suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
  }

  values <- df[symbols]
  values[] <- lapply(values, parse_number)
  if (anyNA(values)) stop("Prezzi mancanti o non numerici nel CSV.")
  matrix_values <- as.matrix(values)
  storage.mode(matrix_values) <- "double"
  ord <- order(dates)
  result <- xts::xts(matrix_values[ord, , drop = FALSE], order.by = dates[ord])
  colnames(result) <- symbols
  result
}

# 4. PREZZI, RENDIMENTI E PARAMETRI -------------------------------------------
if (isTRUE(update_from_yahoo) || !file.exists(file_path)) {
  prices_xts <- download_yahoo_prices(tickers, start_date, end_date)
  dataset <- data.frame(
    Date = format(zoo::index(prices_xts), "%d/%m/%Y"),
    zoo::coredata(prices_xts), check.names = FALSE
  )
  write.csv2(dataset, file_path, row.names = FALSE, quote = FALSE)
} else {
  prices_xts <- read_prices_csv(file_path, tickers)
}

price_matrix <- as.matrix(prices_xts)
returns_matrix <- price_matrix[-1, , drop = FALSE] /
  price_matrix[-NROW(price_matrix), , drop = FALSE] - 1
colnames(returns_matrix) <- tickers
return_dates <- zoo::index(prices_xts)[-1]

if (NROW(returns_matrix) < 2L || any(!is.finite(returns_matrix))) {
  stop("Rendimenti insufficienti o non validi.")
}
if (max(abs(returns_matrix)) > 5) stop("Rendimenti giornalieri anomali.")

# Separazione temporale: il passato è usato per costruire i portafogli e il
# periodo successivo esclusivamente per valutarli. Non vi è riottimizzazione OOS.
split_index <- floor(NROW(returns_matrix) * train_fraction)
if (split_index < 2L || split_index >= NROW(returns_matrix)) {
  stop("Suddivisione in-sample/out-of-sample non valida.")
}
returns_in_sample <- returns_matrix[seq_len(split_index), , drop = FALSE]
returns_out_sample <- returns_matrix[(split_index + 1L):NROW(returns_matrix), , drop = FALSE]
dates_in_sample <- return_dates[seq_len(split_index)]
dates_out_sample <- return_dates[(split_index + 1L):length(return_dates)]

# Mu e Sigma sono stimati ESCLUSIVAMENTE sul periodo in-sample.
mu_vec <- as.numeric(colMeans(returns_in_sample) * trading_days)
cov_mat <- unname(stats::cov(returns_in_sample) * trading_days)
n_assets <- length(tickers)

common_objective <- function(weights) {
  as.numeric(risk_factor * crossprod(weights, cov_mat %*% weights) - crossprod(mu_vec, weights))
}

equal_weights_from_indices <- function(indices) {
  weights <- setNames(numeric(n_assets), tickers)
  weights[indices] <- 1 / budget
  weights
}

cat("\nPeriodo complessivo: ", format(min(return_dates), "%d/%m/%Y"), " - ",
    format(max(return_dates), "%d/%m/%Y"), "\n", sep = "")
cat("Periodo in-sample: ", format(min(dates_in_sample), "%d/%m/%Y"), " - ",
    format(max(dates_in_sample), "%d/%m/%Y"), "\n", sep = "")
cat("Periodo out-of-sample: ", format(min(dates_out_sample), "%d/%m/%Y"), " - ",
    format(max(dates_out_sample), "%d/%m/%Y"), "\n", sep = "")
cat("Sottoinsiemi Markowitz: ", choose(n_assets, budget), "\n", sep = "")

# 5. METODO 1: OTTIMIZZAZIONE RICORSIVA GREEDY -------------------------------
recursive_select <- function(selected = integer(0), available = seq_len(n_assets), level = 1L) {
  if (level > budget) return(selected)

  candidates <- lapply(available, function(candidate) {
    trial <- c(selected, candidate)
    weights <- numeric(n_assets)
    weights[trial] <- 1 / length(trial)
    list(candidate = candidate, objective = common_objective(weights))
  })
  candidate_values <- vapply(candidates, `[[`, numeric(1), "objective")
  best_candidate <- candidates[[which.min(candidate_values)]]$candidate

  recursive_select(
    selected = c(selected, best_candidate),
    available = setdiff(available, best_candidate),
    level = level + 1L
  )
}

t0 <- proc.time()[[3]]
recursive_indices <- recursive_select()
recursive_time <- proc.time()[[3]] - t0
recursive_weights <- equal_weights_from_indices(recursive_indices)
recursive_objective <- common_objective(recursive_weights)

# 6. METODO 2: MARKOWITZ CLASSICO CON PESI UNIFORMI ---------------------------
# Per garantire un confronto equo, Markowitz non ottimizza pesi continui.
# Esamina tutte le combinazioni di `budget` titoli e valuta ciascuna con pesi
# identici 1/budget mediante la stessa funzione media-varianza. Conserva quindi
# il sottoinsieme con objective in-sample più basso. Il metodo è esatto rispetto
# allo spazio delle combinazioni equiponderate esaminate.

t0 <- proc.time()[[3]]
markowitz_combinations <- combn(seq_len(n_assets), budget, simplify = FALSE)
markowitz_values <- vapply(markowitz_combinations, function(indices) {
  common_objective(equal_weights_from_indices(indices))
}, numeric(1))
markowitz_best_indices <- markowitz_combinations[[which.min(markowitz_values)]]
markowitz_weights <- equal_weights_from_indices(markowitz_best_indices)
markowitz_objective <- common_objective(markowitz_weights)
markowitz_time <- proc.time()[[3]] - t0

# 7. METODO 3: QAOA QUANTISTICO -----------------------------------------------
# QAOA usa x_i binarie e pesi w_i=x_i/budget. La funzione comune diventa:
#   risk_factor/budget^2 * x'cov*x - 1/budget * mu'x.
exchange_dir <- tempfile("three_methods_")
dir.create(exchange_dir, recursive = TRUE)
on.exit(unlink(exchange_dir, recursive = TRUE, force = TRUE), add = TRUE)

mu_file <- file.path(exchange_dir, "mu.csv")
cov_file <- file.path(exchange_dir, "cov.csv")
assets_file <- file.path(exchange_dir, "assets.txt")
params_file <- file.path(exchange_dir, "params.txt")
python_file <- file.path(exchange_dir, "solve_qaoa.py")
result_file <- file.path(exchange_dir, "qaoa_result.csv")
log_file <- file.path(exchange_dir, "python.log")

write.table(matrix(mu_vec, nrow = 1), mu_file, sep = ",", row.names = FALSE,
            col.names = FALSE, quote = FALSE)
write.table(cov_mat, cov_file, sep = ",", row.names = FALSE,
            col.names = FALSE, quote = FALSE)
writeLines(tickers, assets_file)
writeLines(c(
  paste0("risk_factor=", risk_factor), paste0("budget=", budget),
  paste0("seed=", random_seed), paste0("qaoa_reps=", qaoa_reps),
  paste0("qaoa_maxiter=", qaoa_maxiter), paste0("qaoa_shots=", qaoa_shots)
), params_file)

paths <- vapply(c(mu_file, cov_file, assets_file, params_file, result_file),
                normalizePath, character(1), winslash = "/", mustWork = FALSE)

python_code <- c(
  "import csv, time, numpy as np",
  "from qiskit_optimization import QuadraticProgram",
  "from qiskit_optimization.algorithms import MinimumEigenOptimizer",
  "from qiskit_algorithms import QAOA",
  "from qiskit_algorithms.optimizers import COBYLA",
  "from qiskit_algorithms.utils import algorithm_globals",
  "from qiskit.primitives import StatevectorSampler",
  sprintf("mu_file=r'%s'", paths[1]),
  sprintf("cov_file=r'%s'", paths[2]),
  sprintf("assets_file=r'%s'", paths[3]),
  sprintf("params_file=r'%s'", paths[4]),
  sprintf("result_file=r'%s'", paths[5]),
  "mu=np.loadtxt(mu_file,delimiter=',',ndmin=1).reshape(-1)",
  "cov=np.loadtxt(cov_file,delimiter=',',ndmin=2)",
  "with open(assets_file,encoding='utf-8') as f: assets=[v.strip() for v in f if v.strip()]",
  "params={}",
  "with open(params_file,encoding='utf-8') as f:",
  "    for line in f:",
  "        key,value=line.strip().split('=',1); params[key]=value",
  "risk_factor=float(params['risk_factor']); budget=int(params['budget'])",
  "seed=int(params['seed']); reps=int(params['qaoa_reps'])",
  "maxiter=int(params['qaoa_maxiter']); shots=int(params['qaoa_shots'])",
  "n=len(assets); qp=QuadraticProgram(name='Portfolio_QAOA_3_Methods')",
  "for asset in assets: qp.binary_var(name=asset)",
  "linear={assets[i]:float(-mu[i]/budget) for i in range(n)}",
  "quadratic={}",
  "for i in range(n):",
  "    quadratic[(assets[i],assets[i])]=float(risk_factor*cov[i,i]/(budget**2))",
  "    for j in range(i+1,n):",
  "        quadratic[(assets[i],assets[j])]=float(2*risk_factor*cov[i,j]/(budget**2))",
  "qp.minimize(linear=linear,quadratic=quadratic)",
  "qp.linear_constraint(linear={a:1.0 for a in assets},sense='==',rhs=float(budget),name='budget')",
  "algorithm_globals.random_seed=seed",
  "t0=time.perf_counter()",
  "sampler=StatevectorSampler(default_shots=shots,seed=seed)",
  "qaoa=QAOA(sampler=sampler,optimizer=COBYLA(maxiter=maxiter),reps=reps)",
  "result=MinimumEigenOptimizer(qaoa).solve(qp)",
  "elapsed=time.perf_counter()-t0",
  "if result.x is None: raise RuntimeError('QAOA non ha restituito una soluzione')",
  "x=np.rint(np.asarray(result.x,dtype=float)).astype(int)",
  "if int(x.sum())!=budget: raise RuntimeError('Soluzione QAOA non conforme al budget')",
  "weights=x.astype(float)/budget",
  "objective=float(risk_factor*(weights@cov@weights)-(mu@weights))",
  "selected=', '.join(assets[i] for i in range(n) if x[i]==1)",
  "with open(result_file,'w',newline='',encoding='utf-8') as f:",
  "    w=csv.writer(f,delimiter=';')",
  "    w.writerow(['Asset_selezionati',*[f'x_{a}' for a in assets],*[f'w_{a}' for a in assets],'Objective','Tempo_secondi'])",
  "    w.writerow([selected,*x.tolist(),*weights.tolist(),objective,elapsed])",
  "print('QAOA',x.tolist(),weights.tolist(),objective,elapsed)"
)
writeLines(python_code, python_file)

python_executable <- reticulate::py_config()$python
status <- suppressWarnings(system2(
  python_executable,
  args = shQuote(normalizePath(python_file, winslash = "/", mustWork = TRUE)),
  stdout = log_file, stderr = log_file, wait = TRUE
))
if (file.exists(log_file)) {
  log_lines <- readLines(log_file, warn = FALSE)
  if (length(log_lines) > 0L) cat(paste(log_lines, collapse = "\n"), "\n")
}
if (!identical(as.integer(status), 0L) || !file.exists(result_file)) {
  stop("Il processo Python/QAOA non ha prodotto risultati.")
}

qaoa_result <- read.csv(result_file, sep = ";", stringsAsFactors = FALSE, check.names = FALSE)
qaoa_weights <- setNames(as.numeric(qaoa_result[1, paste0("w_", tickers)]), tickers)
qaoa_objective <- common_objective(qaoa_weights)
qaoa_time <- qaoa_result$Tempo_secondi[1]

# 8. TABELLE DI SELEZIONE E PESI ----------------------------------------------
method_weights <- list(
  "Ottimizzazione Ricorsiva" = recursive_weights,
  "Markowitz Classico" = markowitz_weights,
  "QAOA Quantistico" = qaoa_weights
)

selected_text <- vapply(method_weights, function(w) {
  paste(names(w)[w > 1e-10], collapse = ", ")
}, character(1))

selections <- data.frame(
  Metodo = names(method_weights),
  Asset_selezionati = unname(selected_text),
  do.call(rbind, lapply(method_weights, function(w) as.integer(w > 1e-10))),
  check.names = FALSE
)
names(selections)[-(1:2)] <- paste0("x_", tickers)

weights_table <- data.frame(
  Metodo = names(method_weights),
  do.call(rbind, method_weights),
  check.names = FALSE
)

write.csv2(selections, selection_path, row.names = FALSE)
write.csv2(weights_table, weights_path, row.names = FALSE)

# 9. METRICHE IN-SAMPLE E OUT-OF-SAMPLE ---------------------------------------
portfolio_analysis <- function(weights, data_matrix) {
  weights <- weights[colnames(data_matrix)]
  daily <- as.numeric(data_matrix %*% weights)
  wealth <- cumprod(1 + daily)
  ann_return <- mean(daily) * trading_days
  ann_volatility <- sd(daily) * sqrt(trading_days)
  list(
    weights = weights, daily = daily, wealth = wealth,
    annual_return = ann_return,
    cumulative_return = tail(wealth, 1) - 1,
    annual_volatility = ann_volatility,
    sharpe = if (ann_volatility > 0) (ann_return-risk_free_rate)/ann_volatility else NA_real_,
    max_drawdown = min(wealth/cummax(wealth)-1)
  )
}

method_stats_in <- lapply(method_weights, portfolio_analysis, data_matrix = returns_in_sample)
method_stats_out <- lapply(method_weights, portfolio_analysis, data_matrix = returns_out_sample)
objectives <- c(recursive_objective, markowitz_objective, qaoa_objective)
times <- c(recursive_time, markowitz_time, qaoa_time)

metrics <- data.frame(
  Metodo = names(method_weights), Asset = unname(selected_text),
  Objective_in_sample = objectives, Tempo_secondi = times,
  IS_Rendimento_annualizzato = vapply(method_stats_in, `[[`, numeric(1), "annual_return"),
  IS_Rendimento_cumulato = vapply(method_stats_in, `[[`, numeric(1), "cumulative_return"),
  IS_Volatilita_annualizzata = vapply(method_stats_in, `[[`, numeric(1), "annual_volatility"),
  IS_Sharpe_ratio = vapply(method_stats_in, `[[`, numeric(1), "sharpe"),
  IS_Max_drawdown = vapply(method_stats_in, `[[`, numeric(1), "max_drawdown"),
  OOS_Rendimento_annualizzato = vapply(method_stats_out, `[[`, numeric(1), "annual_return"),
  OOS_Rendimento_cumulato = vapply(method_stats_out, `[[`, numeric(1), "cumulative_return"),
  OOS_Volatilita_annualizzata = vapply(method_stats_out, `[[`, numeric(1), "annual_volatility"),
  OOS_Sharpe_ratio = vapply(method_stats_out, `[[`, numeric(1), "sharpe"),
  OOS_Max_drawdown = vapply(method_stats_out, `[[`, numeric(1), "max_drawdown"),
  check.names = FALSE
)

best_objective <- min(metrics$Objective_in_sample)
metrics$Gap_vs_migliore_objective <- metrics$Objective_in_sample - best_objective
metrics$Rank_Objective_IS <- rank(metrics$Objective_in_sample, ties.method = "min")
metrics$Rank_Sharpe_OOS <- rank(-metrics$OOS_Sharpe_ratio, ties.method = "min")
metrics$Rank_Drawdown_OOS <- rank(abs(metrics$OOS_Max_drawdown), ties.method = "min")
metrics$Punteggio_rank_OOS <- metrics$Rank_Sharpe_OOS + metrics$Rank_Drawdown_OOS
metrics$Indicazione_OOS <- ifelse(
  metrics$Punteggio_rank_OOS == min(metrics$Punteggio_rank_OOS),
  "MIGLIORE COMPROMESSO OUT-OF-SAMPLE", "ALTERNATIVA"
)
write.csv2(metrics, metrics_path, row.names = FALSE)

cat("\n=========================================\n")
cat("CONFRONTO DEI TRE METODI\n")
cat("=========================================\n")
print(metrics)
cat("Migliore objective in-sample: ", metrics$Metodo[which.min(metrics$Objective_in_sample)], "\n", sep = "")
cat("Migliore Sharpe out-of-sample: ", metrics$Metodo[which.max(metrics$OOS_Sharpe_ratio)], "\n", sep = "")
cat("Migliore compromesso out-of-sample: ",
    metrics$Metodo[which.min(metrics$Punteggio_rank_OOS)], "\n", sep = "")

# 10. SIGNIFICATIVITA STATISTICA ----------------------------------------------
daily_return_matrix <- do.call(cbind, lapply(method_stats_out, `[[`, "daily"))
colnames(daily_return_matrix) <- names(method_stats_out)
if (any(!is.finite(daily_return_matrix))) stop("Rendimenti non finiti.")

alpha_1pct <- 0.01; alpha_5pct <- 0.05; alpha_10pct <- 0.10
classify_significance <- function(p) {
  if (is.na(p)) return("NON CALCOLABILE")
  if (p < alpha_1pct) return("SIGNIFICATIVO AL 1%, 5% E 10%")
  if (p < alpha_5pct) return("SIGNIFICATIVO AL 5% E 10%, NON ALL'1%")
  if (p < alpha_10pct) return("SIGNIFICATIVO SOLO AL 10%")
  "NON SIGNIFICATIVO AL 10%, 5% E 1%"
}

all_series_identical <- max(apply(daily_return_matrix, 1, function(z) max(z)-min(z))) < 1e-14
if (all_series_identical) {
  friedman_table <- data.frame(
    Test = "Friedman", Statistica = 0, Gradi_liberta = 2, P_value = 1,
    Livello_significativita = classify_significance(1),
    Interpretazione = "Serie identiche: nessuna differenza osservabile"
  )
} else {
  ft <- stats::friedman.test(daily_return_matrix)
  friedman_table <- data.frame(
    Test = "Friedman", Statistica = unname(ft$statistic),
    Gradi_liberta = unname(ft$parameter), P_value = ft$p.value,
    Livello_significativita = classify_significance(ft$p.value),
    Interpretazione = if (ft$p.value < 0.05)
      "Differenze globali statisticamente significative"
    else "Nessuna evidenza globale al 5%"
  )
}

method_pairs <- combn(colnames(daily_return_matrix), 2, simplify = FALSE)
pairwise_rows <- lapply(method_pairs, function(pair) {
  x <- daily_return_matrix[, pair[1]]; y <- daily_return_matrix[, pair[2]]
  difference <- x-y; identical_pair <- max(abs(difference)) < 1e-14
  if (identical_pair) {
    wp <- tp <- 1; ws <- ts <- 0; note <- "Serie identiche"
  } else {
    wr <- suppressWarnings(stats::wilcox.test(x, y, paired = TRUE, exact = FALSE))
    tr <- stats::t.test(x, y, paired = TRUE)
    wp <- wr$p.value; tp <- tr$p.value
    ws <- unname(wr$statistic); ts <- unname(tr$statistic); note <- "Confronto eseguito"
  }
  data.frame(
    Metodo_1 = pair[1], Metodo_2 = pair[2],
    Media_diff_giornaliera = mean(difference),
    Media_diff_annualizzata = mean(difference)*trading_days,
    Wilcoxon_statistica = ws, Wilcoxon_p_grezzo = wp,
    T_test_statistica = ts, T_test_p_grezzo = tp,
    Serie_identiche = ifelse(identical_pair, "SI", "NO"), Nota = note
  )
})
pairwise_significance <- do.call(rbind, pairwise_rows)
pairwise_significance$Wilcoxon_p_Holm <- p.adjust(pairwise_significance$Wilcoxon_p_grezzo, "holm")
pairwise_significance$T_test_p_Holm <- p.adjust(pairwise_significance$T_test_p_grezzo, "holm")
pairwise_significance$Wilcoxon_livello <- vapply(
  pairwise_significance$Wilcoxon_p_Holm, classify_significance, character(1))
pairwise_significance$T_test_livello <- vapply(
  pairwise_significance$T_test_p_Holm, classify_significance, character(1))
write.csv2(pairwise_significance, significance_csv_path, row.names = FALSE)

cat("\nTest globale di Friedman:\n")
print(friedman_table, row.names = FALSE)
cat("\nConfronti pairwise con correzione Holm:\n")
print(pairwise_significance, row.names = FALSE)

# 11. GRAFICO COMPARATIVO A TRE CURVE -----------------------------------------
plot_dates <- as.Date(dates_out_sample)
wealth_matrix <- do.call(cbind, lapply(method_stats_out, function(x) 100*x$wealth))
colnames(wealth_matrix) <- names(method_stats_out)
y_limits <- range(c(wealth_matrix, 100), finite = TRUE)

colors <- c(
  "Ottimizzazione Ricorsiva" = "#FF7F0E",
  "Markowitz Classico" = "#1F77B4",
  "QAOA Quantistico" = "#D62728"
)
line_types <- c(
  "Ottimizzazione Ricorsiva" = 3,
  "Markowitz Classico" = 1,
  "QAOA Quantistico" = 4
)
point_shapes <- c(
  "Ottimizzazione Ricorsiva" = 17,
  "Markowitz Classico" = 15,
  "QAOA Quantistico" = 21
)
point_index <- seq.int(1L, length(plot_dates), by = max(1L, floor(length(plot_dates)/24L)))

draw_three_curves <- function() {
  methods <- colnames(wealth_matrix); first <- methods[1]
  plot(plot_dates, wealth_matrix[, first], type = "l", col = colors[first],
       lty = line_types[first], lwd = 2.8, ylim = y_limits,
       xlab = "Data", ylab = "Valore del portafoglio",
       main = "Validazione out-of-sample dei tre approcci",
       sub = "Pesi fissati sul training set; capitale iniziale OOS = 100")
  for (method in methods[-1]) lines(plot_dates, wealth_matrix[, method],
    col = colors[method], lty = line_types[method], lwd = 2.8)
  for (method in methods) points(plot_dates[point_index], wealth_matrix[point_index, method],
    pch = point_shapes[method], col = colors[method],
    bg = if (method == "QAOA Quantistico") "white" else colors[method], cex = 0.65)
  abline(h = 100, col = "grey50", lty = 5)
  grid(col = "grey85", lty = "dotted")
  legend("topleft", legend = methods, col = colors[methods],
         lty = line_types[methods], lwd = 2.8, pch = point_shapes[methods], bty = "n")
}

try(grDevices::dev.new(), silent = TRUE)
draw_three_curves()
grDevices::jpeg(graph_path, width = 1900, height = 1100, units = "px", quality = 95, res = 160)
draw_three_curves(); grDevices::dev.off()
if (!file.exists(graph_path)) stop("Grafico comparativo non creato.")

# 12. ADDENDA: TRE GRAFICI SEPARATI -------------------------------------------
draw_three_panels <- function() {
  old_par <- par(no.readonly = TRUE); on.exit(par(old_par), add = TRUE)
  par(mfrow = c(2, 2), mar = c(4.2, 4.4, 3.5, 1.2)+0.1, oma = c(0,0,2,0))
  for (method in colnames(wealth_matrix)) {
    plot(plot_dates, wealth_matrix[, method], type = "l", col = colors[method],
         lty = line_types[method], lwd = 2.8, ylim = y_limits,
         xlab = "Data", ylab = "Valore del portafoglio", main = method)
    abline(h = 100, col = "grey50", lty = 5); grid(col = "grey85", lty = "dotted")
    points(plot_dates[point_index], wealth_matrix[point_index, method],
           pch = point_shapes[method], col = colors[method], cex = 0.65)
    final_return <- tail(wealth_matrix[, method], 1)-100
    legend("topleft", legend = paste0("Rendimento finale: ", sprintf("%.2f%%", final_return)),
           bty = "n", cex = 0.78)
  }
  plot.new()
  legend("center", legend = c(
    "Criterio di scelta:",
    "1. objective comune più basso",
    "2. Sharpe più alto",
    "3. drawdown meno negativo",
    "4. robustezza dei risultati"
  ), bty = "n", cex = 1.05)
  mtext("Addenda: andamento separato dei tre metodi", outer = TRUE,
        side = 3, line = 0.4, font = 2, cex = 1.1)
}

try(grDevices::dev.new(), silent = TRUE)
draw_three_panels()
grDevices::jpeg(panel_graph_path, width = 1900, height = 1300,
                units = "px", quality = 95, res = 160)
draw_three_panels(); grDevices::dev.off()
if (!file.exists(panel_graph_path)) stop("Grafici separati non creati.")

# 13. WORKBOOK EXCEL -----------------------------------------------------------
graph_data <- data.frame(Data = plot_dates, wealth_matrix, check.names = FALSE)
wealth_matrix_in <- do.call(cbind, lapply(method_stats_in, function(x) 100*x$wealth))
colnames(wealth_matrix_in) <- names(method_stats_in)
graph_data_in <- data.frame(Data = as.Date(dates_in_sample), wealth_matrix_in, check.names = FALSE)
daily_return_data <- data.frame(Data = plot_dates, daily_return_matrix, check.names = FALSE)
parameters_table <- data.frame(
  Parametro = c("Data iniziale", "Data finale", "Numero asset", "Budget",
                "Risk factor", "Trading days", "Risk free rate", "Seed",
                "QAOA reps", "QAOA maxiter", "QAOA shots", "Quota training",
                "Fine in-sample", "Inizio out-of-sample"),
  Valore = c(as.character(start_date), as.character(end_date-1), n_assets, budget,
             risk_factor, trading_days, risk_free_rate, random_seed,
             qaoa_reps, qaoa_maxiter, qaoa_shots, train_fraction,
             as.character(max(dates_in_sample)), as.character(min(dates_out_sample))),
  stringsAsFactors = FALSE
)
methodology_table <- data.frame(
  Metodo = names(method_weights),
  Tipo = c("Greedy ricorsivo", "Markowitz combinatorio equiponderato", "Quantistico variazionale simulato"),
  Pesi = c("Uniformi 1/budget", "Uniformi 1/budget", "Uniformi 1/budget"),
  Garanzia = c("Nessuna garanzia globale", "Ottimo fra le combinazioni equiponderate", "Soluzione approssimata"),
  stringsAsFactors = FALSE
)

workbook <- openxlsx::createWorkbook()
header_style <- openxlsx::createStyle(fontColour = "#FFFFFF", fgFill = "#1F4E78",
                                     textDecoration = "bold", halign = "center")
percent_style <- openxlsx::createStyle(numFmt = "0.00%")
date_style <- openxlsx::createStyle(numFmt = "dd/mm/yyyy")
pvalue_style <- openxlsx::createStyle(numFmt = "0.00000000E+00")

write_styled_sheet <- function(sheet_name, data) {
  openxlsx::addWorksheet(workbook, sheet_name, gridLines = FALSE)
  openxlsx::writeData(workbook, sheet_name, data, headerStyle = header_style)
  openxlsx::freezePane(workbook, sheet_name, firstRow = TRUE)
  openxlsx::setColWidths(workbook, sheet_name, cols = 1:ncol(data), widths = "auto")
  openxlsx::addFilter(workbook, sheet_name, rows = 1, cols = 1:ncol(data))
}

write_styled_sheet("Dati_Grafici_OOS", graph_data)
write_styled_sheet("Dati_Grafici_IS", graph_data_in)
write_styled_sheet("Rendimenti_Giornalieri", daily_return_data)
write_styled_sheet("Metriche", metrics)
write_styled_sheet("Selezioni", selections)
write_styled_sheet("Pesi", weights_table)
write_styled_sheet("Metodologia", methodology_table)
write_styled_sheet("Parametri", parameters_table)
write_styled_sheet("Significativita_Globale", friedman_table)
write_styled_sheet("Significativita_Pairwise", pairwise_significance)

openxlsx::addStyle(workbook, "Dati_Grafici_OOS", date_style,
                   rows = 2:(nrow(graph_data)+1), cols = 1, gridExpand = TRUE)
openxlsx::addStyle(workbook, "Rendimenti_Giornalieri", date_style,
                   rows = 2:(nrow(daily_return_data)+1), cols = 1, gridExpand = TRUE)
openxlsx::addStyle(workbook, "Rendimenti_Giornalieri", percent_style,
                   rows = 2:(nrow(daily_return_data)+1), cols = 2:ncol(daily_return_data), gridExpand = TRUE)
openxlsx::addStyle(workbook, "Pesi", percent_style,
                   rows = 2:(nrow(weights_table)+1), cols = 2:ncol(weights_table), gridExpand = TRUE)

for (sheet_name in c("Significativita_Globale", "Significativita_Pairwise")) {
  data_ref <- if (sheet_name == "Significativita_Globale") friedman_table else pairwise_significance
  p_cols <- grep("P_value|_p_", names(data_ref), ignore.case = TRUE)
  if (length(p_cols) > 0L) openxlsx::addStyle(
    workbook, sheet_name, pvalue_style,
    rows = 2:(nrow(data_ref)+1), cols = p_cols, gridExpand = TRUE)
}

openxlsx::addWorksheet(workbook, "Grafici", gridLines = FALSE)
openxlsx::writeData(workbook, "Grafici", "Grafico comparativo sovrapposto", startRow = 1)
openxlsx::insertImage(workbook, "Grafici", graph_path,
                      startRow = 3, startCol = 1, width = 11, height = 6.4, units = "in")
openxlsx::writeData(workbook, "Grafici", "Addenda con tre pannelli", startRow = 36)
openxlsx::insertImage(workbook, "Grafici", panel_graph_path,
                      startRow = 38, startCol = 1, width = 11, height = 7.5, units = "in")
openxlsx::saveWorkbook(workbook, xlsx_path, overwrite = TRUE)
if (!file.exists(xlsx_path)) stop("Workbook Excel non creato.")

cat("\n=========================================\n")
cat("FILE GENERATI\n")
cat("=========================================\n")
for (path in c(file_path, selection_path, weights_path, metrics_path, graph_path,
               panel_graph_path, xlsx_path, significance_csv_path)) {
  cat(normalizePath(path, mustWork = FALSE), "\n")
}

# LIMITI
# ------
# - L'ottimizzazione ricorsiva è greedy e dipende dall'ordine delle decisioni.
# - Mu e Sigma sono stimati nel training set e possono cambiare nel periodo OOS.
# - QAOA è approssimato e dipende da seed, reps, shots e ottimizzatore.
# - StatevectorSampler è un simulatore, non hardware quantistico reale.
# - La validazione è out-of-sample, ma non include costi, slippage o turnover.
# - Per decidere il metodo migliore è consigliata anche una verifica out-of-sample.
