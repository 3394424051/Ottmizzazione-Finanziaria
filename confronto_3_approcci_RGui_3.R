# ==============================================================================
# CONFRONTO COMPLETO DI 3 APPROCCI DI PORTAFOGLIO
# VALIDAZIONE OOS + FORECAST CONTINUO A 10 SEDUTE + UNICO WORKBOOK EXCEL
# Compatibile con RGui originale su Windows
# ==============================================================================

rm(list = ls(all.names = TRUE))
invisible(gc())

# 0. CONFIGURAZIONE ------------------------------------------------------------
install_missing_packages <- TRUE
update_from_yahoo <- TRUE
show_graphs_on_screen <- TRUE
file_path <- "Quantum_dataset.csv"
xlsx_path <- "risultati_completi_portafoglio.xlsx"
tickers <- c("IONQ", "QBTS", "RGTI", "IBM", "GOOGL", "MSFT", "NVDA", "QQQ")
start_date <- as.Date("2023-01-01")
end_date <- Sys.Date() + 1L
budget <- 3L
risk_factor <- 0.50
trading_days <- 252L
risk_free_rate <- 0
random_seed <- 42L
train_fraction <- 0.70
forecast_days <- 10L
qaoa_reps <- 2L
qaoa_maxiter <- 150L
qaoa_shots <- 4096L
qaoa_timeout_seconds <- 3600L
tolerance <- 1e-10
cran_repository <- "https://cloud.r-project.org"

if (budget < 1L || budget > length(tickers)) stop("Budget non valido.")
if (train_fraction <= 0 || train_fraction >= 1) stop("train_fraction deve essere fra 0 e 1.")
if (forecast_days < 1L) stop("forecast_days deve essere almeno 1.")

# 1. PACCHETTI R ---------------------------------------------------------------
required_r_packages <- c("reticulate", "quantmod", "xts", "zoo", "openxlsx")
missing_r <- required_r_packages[
  !vapply(required_r_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_r) > 0L) {
  if (!install_missing_packages) stop("Pacchetti R mancanti: ", paste(missing_r, collapse = ", "))
  install.packages(
    missing_r, repos = cran_repository,
    dependencies = c("Depends", "Imports", "LinkingTo")
  )
}
suppressPackageStartupMessages({
  library(reticulate)
  library(quantmod)
  library(xts)
  library(zoo)
  library(openxlsx)
})

# 2. PYTHON E QISKIT: CONTROLLO COMPATIBILE CON RGUI ---------------------------
# In RGui py_module_available() puo produrre falsi negativi. Le dipendenze
# vengono dichiarate con action="add" e verificate con lo stesso interprete
# Python che verra usato successivamente da system2().
reticulate::py_require(
  packages = c(
    "numpy>=1.26",
    "qiskit>=1.4",
    "qiskit-optimization>=0.6",
    "qiskit-algorithms>=0.3"
  ),
  python_version = ">=3.10,<3.14",
  action = "add"
)
py_cfg <- tryCatch(
  reticulate::py_config(),
  error = function(e) stop("Inizializzazione Python fallita: ", conditionMessage(e), call. = FALSE)
)
cat("Interprete Python: ", py_cfg$python, "\n", sep = "")

python_check_file <- tempfile(fileext = ".py")
python_check_log <- tempfile(fileext = ".log")
writeLines(c(
  "import numpy",
  "import qiskit",
  "import qiskit_optimization",
  "import qiskit_algorithms",
  "from qiskit.primitives import StatevectorSampler",
  "print('OK: moduli Python e Qiskit disponibili')"
), python_check_file)
python_check_status <- suppressWarnings(system2(
  command = py_cfg$python,
  args = shQuote(normalizePath(python_check_file, winslash = "/", mustWork = TRUE)),
  stdout = python_check_log,
  stderr = python_check_log,
  wait = TRUE
))
python_check_output <- if (file.exists(python_check_log)) {
  readLines(python_check_log, warn = FALSE)
} else character(0)
unlink(c(python_check_file, python_check_log), force = TRUE)
if (is.na(python_check_status) || as.integer(python_check_status) != 0L) {
  stop(
    "Verifica Python/Qiskit fallita. Interprete: ", py_cfg$python,
    ". Dettaglio: ", paste(python_check_output, collapse = " | "), call. = FALSE
  )
}
cat(paste(python_check_output, collapse = "\n"), "\n")

# Versioni Python senza affidarsi a py_module_available().
version_file <- tempfile(fileext = ".py")
version_csv <- tempfile(fileext = ".csv")
version_log <- tempfile(fileext = ".log")
version_csv_python <- normalizePath(version_csv, winslash = "/", mustWork = FALSE)
writeLines(c(
  "import csv, sys",
  "from importlib.metadata import version, PackageNotFoundError",
  "def pv(name):",
  "    try: return version(name)",
  "    except PackageNotFoundError: return 'NON INSTALLATO'",
  sprintf("out=r'%s'", version_csv_python),
  "rows=[['Python',sys.version.split()[0]],['NumPy',pv('numpy')],['Qiskit',pv('qiskit')],['Qiskit Optimization',pv('qiskit-optimization')],['Qiskit Algorithms',pv('qiskit-algorithms')]]",
  "with open(out,'w',newline='',encoding='utf-8') as f:",
  "    w=csv.writer(f,delimiter=';'); w.writerow(['Componente','Versione']); w.writerows(rows)"
), version_file)
version_status <- suppressWarnings(system2(
  py_cfg$python,
  shQuote(normalizePath(version_file, winslash = "/", mustWork = TRUE)),
  stdout = version_log, stderr = version_log, wait = TRUE
))
if (is.na(version_status) || version_status != 0L || !file.exists(version_csv)) {
  detail <- if (file.exists(version_log)) paste(readLines(version_log, warn = FALSE), collapse = " | ") else "Log non disponibile"
  stop("Impossibile leggere le versioni Python. ", detail)
}
python_versions <- read.csv(version_csv, sep = ";", stringsAsFactors = FALSE)
unlink(c(version_file, version_csv, version_log), force = TRUE)
version_table <- rbind(
  data.frame(Componente = "R", Versione = as.character(getRversion())),
  data.frame(
    Componente = required_r_packages,
    Versione = vapply(required_r_packages, function(p) as.character(packageVersion(p)), character(1))
  ),
  python_versions
)
cat("\n=========================================\nVERSIONI AMBIENTE\n=========================================\n")
print(version_table, row.names = FALSE)

# 3. DATI ----------------------------------------------------------------------
download_yahoo_prices <- function(symbols, from, to) {
  downloaded <- vector("list", length(symbols)); names(downloaded) <- symbols
  for (symbol in symbols) {
    cat("Download ", symbol, "... ", sep = "")
    raw <- tryCatch(
      quantmod::getSymbols(
        Symbols = symbol, src = "yahoo", from = from, to = to,
        periodicity = "daily", auto.assign = FALSE, warnings = FALSE
      ),
      error = function(e) NULL
    )
    if (is.null(raw)) stop("Download fallito per ", symbol)
    price <- tryCatch(quantmod::Ad(raw), error = function(e) NULL)
    if (is.null(price) || NCOL(price) != 1L) price <- quantmod::Cl(raw)
    colnames(price) <- symbol; downloaded[[symbol]] <- price
    cat(NROW(price), " osservazioni\n")
  }
  prices <- na.omit(do.call(merge, c(downloaded, all = FALSE)))
  colnames(prices) <- symbols
  values <- as.matrix(prices)
  if (NROW(prices) < 3L || any(!is.finite(values)) || any(values <= 0)) stop("Prezzi non validi.")
  prices
}

read_prices_csv <- function(path, symbols) {
  data <- read.csv2(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (NCOL(data) < 2L) stop("CSV privo delle colonne necessarie.")
  names(data)[1] <- "Date"
  missing_symbols <- setdiff(symbols, names(data))
  if (length(missing_symbols)) stop("Ticker mancanti: ", paste(missing_symbols, collapse = ", "))
  dates <- NULL
  for (fmt in c("%d/%m/%Y", "%Y-%m-%d", "%d-%m-%Y", "%m/%d/%Y")) {
    candidate <- as.Date(data$Date, format = fmt)
    if (!anyNA(candidate)) { dates <- candidate; break }
  }
  if (is.null(dates)) stop("Formato data non riconosciuto.")
  parse_number <- function(x) {
    if (is.numeric(x)) return(as.numeric(x))
    x <- trimws(as.character(x))
    both <- grepl(",", x, fixed = TRUE) & grepl(".", x, fixed = TRUE)
    x[both] <- gsub(".", "", x[both], fixed = TRUE)
    suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
  }
  values <- data[symbols]; values[] <- lapply(values, parse_number)
  if (anyNA(values)) stop("Prezzi mancanti o non numerici.")
  ord <- order(dates)
  result <- xts::xts(as.matrix(values)[ord, , drop = FALSE], order.by = dates[ord])
  colnames(result) <- symbols
  result
}

if (update_from_yahoo || !file.exists(file_path)) {
  prices_xts <- download_yahoo_prices(tickers, start_date, end_date)
  dataset <- data.frame(
    Date = format(zoo::index(prices_xts), "%d/%m/%Y"),
    zoo::coredata(prices_xts), check.names = FALSE
  )
  write.csv2(dataset, file_path, row.names = FALSE, quote = FALSE)
} else {
  cat("Uso dataset locale: ", file_path, "\n", sep = "")
  prices_xts <- read_prices_csv(file_path, tickers)
}

# 4. RENDIMENTI E SPLIT --------------------------------------------------------
price_matrix <- as.matrix(prices_xts)
returns_matrix <- price_matrix[-1, , drop = FALSE] /
  price_matrix[-NROW(price_matrix), , drop = FALSE] - 1
colnames(returns_matrix) <- tickers
return_dates <- as.Date(zoo::index(prices_xts)[-1])
if (NROW(returns_matrix) < 3L || any(!is.finite(returns_matrix))) stop("Rendimenti non validi.")
if (max(abs(returns_matrix)) > 5) stop("Rendimenti giornalieri anomali.")
split_index <- floor(NROW(returns_matrix) * train_fraction)
if (split_index < 2L || split_index >= NROW(returns_matrix)) stop("Split IS/OOS non valido.")
returns_in_sample <- returns_matrix[seq_len(split_index), , drop = FALSE]
returns_out_sample <- returns_matrix[(split_index + 1L):NROW(returns_matrix), , drop = FALSE]
dates_in_sample <- return_dates[seq_len(split_index)]
dates_out_sample <- return_dates[(split_index + 1L):length(return_dates)]
mu_daily_vec <- as.numeric(colMeans(returns_in_sample))
cov_daily_mat <- unname(stats::cov(returns_in_sample))
mu_vec <- mu_daily_vec * trading_days
cov_mat <- cov_daily_mat * trading_days
n_assets <- length(tickers)
common_objective <- function(weights) {
  as.numeric(risk_factor * crossprod(weights, cov_mat %*% weights) - crossprod(mu_vec, weights))
}
equal_weights_from_indices <- function(indices) {
  weights <- setNames(numeric(n_assets), tickers); weights[indices] <- 1 / budget; weights
}
cat("\nPeriodo IS: ", format(min(dates_in_sample), "%d/%m/%Y"), " - ", format(max(dates_in_sample), "%d/%m/%Y"), "\n", sep = "")
cat("Periodo OOS: ", format(min(dates_out_sample), "%d/%m/%Y"), " - ", format(max(dates_out_sample), "%d/%m/%Y"), "\n", sep = "")

# 5. APPROCCIO 1: RICORSIVO ----------------------------------------------------
recursive_select <- function(selected = integer(0), available = seq_len(n_assets), level = 1L) {
  if (level > budget) return(selected)
  values <- vapply(available, function(candidate) {
    trial <- c(selected, candidate); weights <- numeric(n_assets)
    weights[trial] <- 1 / length(trial); common_objective(weights)
  }, numeric(1))
  best_candidate <- available[which.min(values)]
  Recall(c(selected, best_candidate), setdiff(available, best_candidate), level + 1L)
}
t0 <- proc.time()[[3]]
recursive_indices <- recursive_select()
recursive_time <- proc.time()[[3]] - t0
recursive_weights <- equal_weights_from_indices(recursive_indices)
recursive_objective <- common_objective(recursive_weights)

# 6. APPROCCIO 2: MARKOWITZ ESATTO --------------------------------------------
t0 <- proc.time()[[3]]
markowitz_combinations <- combn(seq_len(n_assets), budget, simplify = FALSE)
markowitz_values <- vapply(markowitz_combinations, function(indices) {
  common_objective(equal_weights_from_indices(indices))
}, numeric(1))
markowitz_indices <- markowitz_combinations[[which.min(markowitz_values)]]
markowitz_weights <- equal_weights_from_indices(markowitz_indices)
markowitz_objective <- common_objective(markowitz_weights)
markowitz_time <- proc.time()[[3]] - t0

# 7. APPROCCIO 3: QAOA ---------------------------------------------------------
exchange_dir <- tempfile("qaoa_"); dir.create(exchange_dir, recursive = TRUE)
mu_file <- file.path(exchange_dir, "mu.csv")
cov_file <- file.path(exchange_dir, "cov.csv")
assets_file <- file.path(exchange_dir, "assets.txt")
params_file <- file.path(exchange_dir, "params.txt")
python_file <- file.path(exchange_dir, "solve_qaoa.py")
result_file <- file.path(exchange_dir, "result.csv")
log_file <- file.path(exchange_dir, "python.log")
write.table(matrix(mu_vec, nrow = 1), mu_file, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(cov_mat, cov_file, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)
writeLines(tickers, assets_file)
writeLines(c(
  paste0("risk_factor=", risk_factor), paste0("budget=", budget),
  paste0("seed=", random_seed), paste0("reps=", qaoa_reps),
  paste0("maxiter=", qaoa_maxiter), paste0("shots=", qaoa_shots)
), params_file)
paths <- vapply(
  c(mu_file, cov_file, assets_file, params_file, result_file),
  normalizePath, character(1), winslash = "/", mustWork = FALSE
)
python_code <- c(
  "import csv, time, numpy as np",
  "from qiskit_optimization import QuadraticProgram",
  "from qiskit_optimization.algorithms import MinimumEigenOptimizer",
  "from qiskit_algorithms import QAOA",
  "from qiskit_algorithms.optimizers import COBYLA",
  "from qiskit_algorithms.utils import algorithm_globals",
  "from qiskit.primitives import StatevectorSampler",
  sprintf("mu=np.loadtxt(r'%s',delimiter=',',ndmin=1).reshape(-1)", paths[1]),
  sprintf("cov=np.loadtxt(r'%s',delimiter=',',ndmin=2)", paths[2]),
  sprintf("assets=[x.strip() for x in open(r'%s',encoding='utf-8') if x.strip()]", paths[3]),
  sprintf("pa=dict(x.strip().split('=',1) for x in open(r'%s',encoding='utf-8') if x.strip())", paths[4]),
  "rf=float(pa['risk_factor']); budget=int(pa['budget']); seed=int(pa['seed'])",
  "reps=int(pa['reps']); maxiter=int(pa['maxiter']); shots=int(pa['shots'])",
  "n=len(assets); qp=QuadraticProgram(name='Portfolio_QAOA')",
  "for a in assets: qp.binary_var(name=a)",
  "linear={assets[i]:float(-mu[i]/budget) for i in range(n)}; quadratic={}",
  "for i in range(n):",
  "    quadratic[(assets[i],assets[i])]=float(rf*cov[i,i]/budget**2)",
  "    for j in range(i+1,n):",
  "        quadratic[(assets[i],assets[j])]=float(2*rf*cov[i,j]/budget**2)",
  "qp.minimize(linear=linear,quadratic=quadratic)",
  "qp.linear_constraint(linear={a:1.0 for a in assets},sense='==',rhs=float(budget),name='budget')",
  "algorithm_globals.random_seed=seed",
  "sampler=StatevectorSampler(default_shots=shots,seed=seed)",
  "t0=time.perf_counter()",
  "result=MinimumEigenOptimizer(QAOA(sampler=sampler,optimizer=COBYLA(maxiter=maxiter),reps=reps)).solve(qp)",
  "elapsed=time.perf_counter()-t0",
  "if result.x is None: raise RuntimeError('QAOA non ha restituito una soluzione')",
  "x=np.rint(np.asarray(result.x,dtype=float)).astype(int)",
  "if int(x.sum())!=budget: raise RuntimeError('QAOA non conforme al budget')",
  "weights=x.astype(float)/budget",
  sprintf("with open(r'%s','w',newline='',encoding='utf-8') as f:", paths[5]),
  "    w=csv.writer(f,delimiter=';')",
  "    w.writerow([*[f'w_{a}' for a in assets],'Tempo_secondi'])",
  "    w.writerow([*weights.tolist(),elapsed])"
)
writeLines(python_code, python_file)
status <- suppressWarnings(system2(
  py_cfg$python,
  shQuote(normalizePath(python_file, winslash = "/", mustWork = TRUE)),
  stdout = log_file, stderr = log_file, wait = TRUE,
  timeout = qaoa_timeout_seconds
))
if (is.na(status) || as.integer(status) != 0L || !file.exists(result_file)) {
  if (file.exists(log_file)) cat(paste(readLines(log_file, warn = FALSE), collapse = "\n"), "\n")
  stop("QAOA non ha prodotto risultati.", call. = FALSE)
}
qaoa_result <- read.csv(result_file, sep = ";", check.names = FALSE)
qaoa_weights <- setNames(as.numeric(qaoa_result[1, paste0("w_", tickers)]), tickers)
qaoa_objective <- common_objective(qaoa_weights)
qaoa_time <- qaoa_result$Tempo_secondi[1]

# 8. ANALISI DEI PORTAFOGLI ----------------------------------------------------
method_weights <- list(
  "Ottimizzazione Ricorsiva" = recursive_weights,
  "Markowitz Classico" = markowitz_weights,
  "QAOA Quantistico" = qaoa_weights
)
selected_text <- vapply(method_weights, function(w) paste(names(w)[w > tolerance], collapse = ", "), character(1))
selections <- data.frame(
  Metodo = names(method_weights), Asset_selezionati = unname(selected_text),
  do.call(rbind, lapply(method_weights, function(w) as.integer(w > tolerance))),
  check.names = FALSE
)
names(selections)[-(1:2)] <- paste0("x_", tickers)
weights_table <- data.frame(Metodo = names(method_weights), do.call(rbind, method_weights), check.names = FALSE)
portfolio_analysis <- function(weights, data_matrix) {
  aligned <- weights[colnames(data_matrix)]
  if (anyNA(aligned)) stop("Pesi non allineati ai ticker.")
  daily <- as.numeric(data_matrix %*% aligned)
  wealth <- cumprod(1 + daily)
  annual_return <- mean(daily) * trading_days
  annual_volatility <- sd(daily) * sqrt(trading_days)
  list(
    daily = daily, wealth = wealth, annual_return = annual_return,
    cumulative_return = tail(wealth, 1) - 1,
    annual_volatility = annual_volatility,
    sharpe = if (annual_volatility > 0) (annual_return - risk_free_rate) / annual_volatility else NA_real_,
    max_drawdown = min(wealth / cummax(wealth) - 1)
  )
}
method_stats_in <- lapply(method_weights, portfolio_analysis, data_matrix = returns_in_sample)
method_stats_out <- lapply(method_weights, portfolio_analysis, data_matrix = returns_out_sample)
wealth_oos <- do.call(cbind, lapply(method_stats_out, function(x) 100 * x$wealth))
colnames(wealth_oos) <- names(method_stats_out)
daily_oos <- do.call(cbind, lapply(method_stats_out, `[[`, "daily"))
colnames(daily_oos) <- names(method_stats_out)

# 9. FORECAST A 10 SEDUTE ------------------------------------------------------
next_business_days <- function(last_date, n_days) {
  candidates <- seq.Date(as.Date(last_date) + 1L, by = "day", length.out = n_days * 3L)
  business_days <- candidates[as.POSIXlt(candidates)$wday %in% 1:5]
  business_days[seq_len(n_days)]
}
forecast_dates <- next_business_days(max(dates_out_sample), forecast_days)
forecast_rows <- list()
for (method in names(method_weights)) {
  weights <- method_weights[[method]][tickers]
  daily_mean <- as.numeric(crossprod(mu_daily_vec, weights))
  daily_volatility <- sqrt(as.numeric(crossprod(weights, cov_daily_mat %*% weights)))
  initial_value <- tail(wealth_oos[, method], 1)
  horizon <- seq_len(forecast_days)
  expected_value <- initial_value * (1 + daily_mean)^horizon
  forecast_rows[[method]] <- data.frame(
    Metodo = method, Giorno = horizon, Data = forecast_dates,
    Valore_iniziale_OOS = initial_value, Valore_atteso = expected_value,
    Banda_inferiore_1sigma = expected_value * exp(-daily_volatility * sqrt(horizon)),
    Banda_superiore_1sigma = expected_value * exp(daily_volatility * sqrt(horizon)),
    Rendimento_atteso_dal_termine_OOS = expected_value / initial_value - 1,
    Mu_giornaliero = daily_mean, Sigma_giornaliera = daily_volatility,
    stringsAsFactors = FALSE
  )
}
forecast_table <- do.call(rbind, forecast_rows); rownames(forecast_table) <- NULL
forecast_final <- forecast_table[
  forecast_table$Giorno == forecast_days,
  c("Metodo", "Valore_iniziale_OOS", "Valore_atteso", "Banda_inferiore_1sigma",
    "Banda_superiore_1sigma", "Rendimento_atteso_dal_termine_OOS")
]

# 10. METRICHE -----------------------------------------------------------------
objectives <- c(recursive_objective, markowitz_objective, qaoa_objective)
calculation_times <- c(recursive_time, markowitz_time, qaoa_time)
metrics <- data.frame(
  Metodo = names(method_weights), Asset = unname(selected_text),
  Objective_in_sample = objectives, Tempo_secondi = calculation_times,
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
metrics <- merge(metrics, forecast_final, by = "Metodo", sort = FALSE)
metrics$Gap_vs_migliore_objective <- metrics$Objective_in_sample - min(metrics$Objective_in_sample)
metrics$Rank_Sharpe_OOS <- rank(-metrics$OOS_Sharpe_ratio, ties.method = "min")
metrics$Rank_Drawdown_OOS <- rank(abs(metrics$OOS_Max_drawdown), ties.method = "min")
metrics$Punteggio_rank_OOS <- metrics$Rank_Sharpe_OOS + metrics$Rank_Drawdown_OOS
metrics$Indicazione_OOS <- ifelse(
  metrics$Punteggio_rank_OOS == min(metrics$Punteggio_rank_OOS),
  "MIGLIORE COMPROMESSO OUT-OF-SAMPLE", "ALTERNATIVA"
)

# 11. TEST FRIEDMAN E PAIRWISE -------------------------------------------------
all_identical <- max(apply(daily_oos, 1, function(z) max(z) - min(z))) < 1e-14
if (all_identical) {
  friedman_table <- data.frame(Test = "Friedman", Statistica = 0, Gradi_liberta = 2, P_value = 1, Interpretazione = "Serie identiche")
} else {
  ft <- stats::friedman.test(daily_oos)
  friedman_table <- data.frame(
    Test = "Friedman", Statistica = unname(ft$statistic),
    Gradi_liberta = unname(ft$parameter), P_value = ft$p.value,
    Interpretazione = ifelse(ft$p.value < 0.05, "Differenze globali significative al 5%", "Nessuna evidenza globale al 5%")
  )
}
method_pairs <- combn(colnames(daily_oos), 2, simplify = FALSE)
pairwise_rows <- lapply(method_pairs, function(pair) {
  x <- daily_oos[, pair[1]]; y <- daily_oos[, pair[2]]; difference <- x - y
  identical_pair <- max(abs(difference)) < 1e-14
  if (identical_pair) { wilcoxon_p <- 1; t_test_p <- 1 } else {
    wilcoxon_p <- suppressWarnings(stats::wilcox.test(x, y, paired = TRUE, exact = FALSE)$p.value)
    t_test_p <- stats::t.test(x, y, paired = TRUE)$p.value
  }
  data.frame(
    Metodo_1 = pair[1], Metodo_2 = pair[2],
    Media_diff_giornaliera = mean(difference),
    Media_diff_annualizzata = mean(difference) * trading_days,
    Wilcoxon_p_grezzo = wilcoxon_p, T_test_p_grezzo = t_test_p,
    Serie_identiche = ifelse(identical_pair, "SI", "NO")
  )
})
pairwise_table <- do.call(rbind, pairwise_rows)
pairwise_table$Wilcoxon_p_Holm <- p.adjust(pairwise_table$Wilcoxon_p_grezzo, "holm")
pairwise_table$T_test_p_Holm <- p.adjust(pairwise_table$T_test_p_grezzo, "holm")

# 12. DATI COMBINATI E CONTROLLO CURVE -----------------------------------------
combined_rows <- list()
for (method in names(method_weights)) {
  historical <- data.frame(
    Metodo = method, Data = as.Date(dates_out_sample), Tipo = "STORICO_OOS",
    Valore_osservato = wealth_oos[, method], Valore_atteso = NA_real_,
    Banda_inferiore_1sigma = NA_real_, Banda_superiore_1sigma = NA_real_
  )
  f <- forecast_table[forecast_table$Metodo == method, ]
  bridge <- data.frame(
    Metodo = method, Data = max(as.Date(dates_out_sample)), Tipo = "INIZIO_FORECAST",
    Valore_osservato = f$Valore_iniziale_OOS[1], Valore_atteso = f$Valore_iniziale_OOS[1],
    Banda_inferiore_1sigma = f$Valore_iniziale_OOS[1], Banda_superiore_1sigma = f$Valore_iniziale_OOS[1]
  )
  future <- data.frame(
    Metodo = method, Data = f$Data, Tipo = "FORECAST_10GG", Valore_osservato = NA_real_,
    Valore_atteso = f$Valore_atteso, Banda_inferiore_1sigma = f$Banda_inferiore_1sigma,
    Banda_superiore_1sigma = f$Banda_superiore_1sigma
  )
  combined_rows[[method]] <- rbind(historical, bridge, future)
}
combined_data <- do.call(rbind, combined_rows); rownames(combined_data) <- NULL
curve_check <- data.frame(
  Confronto = c("Ricorsivo vs Markowitz", "Ricorsivo vs QAOA", "Markowitz vs QAOA"),
  Differenza_massima = c(
    max(abs(wealth_oos[, 1] - wealth_oos[, 2])),
    max(abs(wealth_oos[, 1] - wealth_oos[, 3])),
    max(abs(wealth_oos[, 2] - wealth_oos[, 3]))
  )
)
curve_check$Curve_identiche <- ifelse(curve_check$Differenza_massima < 1e-12, "SI", "NO")

# 13. GRAFICI CON RENDIMENTI ---------------------------------------------------
plot_dates <- as.Date(dates_out_sample)
methods <- names(method_weights)
colors <- c(
  "Ottimizzazione Ricorsiva" = "#E69F00",
  "Markowitz Classico" = "#0072B2",
  "QAOA Quantistico" = "#CC33CC"
)
historical_lty <- c("Ottimizzazione Ricorsiva" = 3, "Markowitz Classico" = 1, "QAOA Quantistico" = 4)
forecast_lty <- c("Ottimizzazione Ricorsiva" = 2, "Markowitz Classico" = 6, "QAOA Quantistico" = 5)
point_shapes <- c("Ottimizzazione Ricorsiva" = 17, "Markowitz Classico" = 15, "QAOA Quantistico" = 21)

oos_returns <- vapply(methods, function(method) method_stats_out[[method]]$cumulative_return, numeric(1))
forecast_returns <- vapply(methods, function(method) {
  forecast_table$Rendimento_atteso_dal_termine_OOS[
    forecast_table$Metodo == method & forecast_table$Giorno == forecast_days
  ][1]
}, numeric(1))
comparison_labels <- paste0(
  methods, " | OOS: ", sprintf("%.2f%%", 100 * oos_returns),
  " | Forecast 10g: ", sprintf("%.2f%%", 100 * forecast_returns)
)

base_step <- max(3L, floor(length(plot_dates) / 30L))
marker_index <- list(
  "Ottimizzazione Ricorsiva" = seq.int(1L, length(plot_dates), by = base_step),
  "Markowitz Classico" = seq.int(1L + floor(base_step / 3), length(plot_dates), by = base_step),
  "QAOA Quantistico" = seq.int(1L + floor(2 * base_step / 3), length(plot_dates), by = base_step)
)
marker_index <- lapply(marker_index, function(i) i[i <= length(plot_dates)])

graph_dir <- tempfile("portfolio_graphs_"); dir.create(graph_dir)
graph_main <- file.path(graph_dir, "validazione_oos.png")
graph_forecast <- file.path(graph_dir, "forecast_10gg.png")
graph_panels <- file.path(graph_dir, "pannelli_metodi.png")
graph_qaoa <- file.path(graph_dir, "qaoa_oos.png")

draw_validation <- function() {
  y_limits <- range(c(wealth_oos, forecast_table$Banda_inferiore_1sigma, forecast_table$Banda_superiore_1sigma, 100), finite = TRUE)
  x_limits <- range(c(plot_dates, forecast_dates))
  first <- methods[1]
  plot(
    plot_dates, wealth_oos[, first], type = "l", col = colors[first],
    lty = historical_lty[first], lwd = 3, xlim = x_limits, ylim = y_limits,
    xlab = "Data", ylab = "Valore del portafoglio",
    main = "Validazione OOS dei tre approcci con forecast a 10 sedute",
    sub = "La legenda riporta rendimento OOS e rendimento atteso a 10 sedute"
  )
  for (method in methods[-1]) lines(plot_dates, wealth_oos[, method], col = colors[method], lty = historical_lty[method], lwd = 2.7)
  for (method in methods) {
    i <- marker_index[[method]]
    points(plot_dates[i], wealth_oos[i, method], pch = point_shapes[method], col = colors[method],
           bg = if (method == "QAOA Quantistico") "white" else colors[method], cex = 0.7)
  }
  for (method in methods) {
    f <- forecast_table[forecast_table$Metodo == method, ]
    polygon(c(f$Data, rev(f$Data)), c(f$Banda_inferiore_1sigma, rev(f$Banda_superiore_1sigma)),
            col = adjustcolor(colors[method], alpha.f = 0.08), border = NA)
    lines(c(max(plot_dates), f$Data), c(tail(wealth_oos[, method], 1), f$Valore_atteso),
          col = colors[method], lty = forecast_lty[method], lwd = 3)
    points(f$Data, f$Valore_atteso, pch = point_shapes[method], col = colors[method],
           bg = if (method == "QAOA Quantistico") "white" else colors[method], cex = 0.8)
  }
  abline(v = max(plot_dates), col = "grey35", lty = 2)
  abline(h = 100, col = "grey55", lty = 5)
  grid(col = "grey85", lty = "dotted")
  legend(
    "topleft", legend = comparison_labels, col = colors[methods],
    lty = historical_lty[methods], lwd = 2.7, pch = point_shapes[methods],
    pt.bg = c(colors[1], colors[2], "white"), bty = "n", cex = 0.68,
    title = "Metodi e rendimenti"
  )
  if (any(curve_check$Curve_identiche == "SI")) {
    mtext("Nota: alcune curve coincidono perche i metodi hanno selezionato lo stesso portafoglio.",
          side = 1, line = 3.2, cex = 0.68, col = "grey35")
  }
}

draw_forecast <- function() {
  y_limits <- range(c(forecast_table$Valore_iniziale_OOS, forecast_table$Banda_inferiore_1sigma, forecast_table$Banda_superiore_1sigma), finite = TRUE)
  x_limits <- range(c(max(plot_dates), forecast_dates))
  plot(x_limits, y_limits, type = "n", xlab = "Data", ylab = "Valore del portafoglio",
       main = "Continuazione forecast a 10 sedute")
  for (method in methods) {
    f <- forecast_table[forecast_table$Metodo == method, ]
    polygon(c(f$Data, rev(f$Data)), c(f$Banda_inferiore_1sigma, rev(f$Banda_superiore_1sigma)),
            col = adjustcolor(colors[method], 0.08), border = NA)
    lines(c(max(plot_dates), f$Data), c(f$Valore_iniziale_OOS[1], f$Valore_atteso),
          col = colors[method], lty = forecast_lty[method], lwd = 3)
    points(f$Data, f$Valore_atteso, pch = point_shapes[method], col = colors[method],
           bg = if (method == "QAOA Quantistico") "white" else colors[method], cex = 0.8)
  }
  abline(v = max(plot_dates), lty = 2, col = "grey35"); grid(col = "grey85", lty = "dotted")
  legend(
    "topleft",
    legend = paste0(methods, " | Forecast 10g: ", sprintf("%.2f%%", 100 * forecast_returns)),
    col = colors[methods], lty = forecast_lty[methods], lwd = 3,
    pch = point_shapes[methods], pt.bg = c(colors[1], colors[2], "white"), bty = "n", cex = 0.72
  )
}

draw_panels <- function() {
  old_par <- par(no.readonly = TRUE); on.exit(par(old_par))
  par(mfrow = c(2, 2), mar = c(4.2, 4.4, 4.0, 1.1) + 0.1, oma = c(0, 0, 2, 0))
  common_ylim <- range(c(wealth_oos, forecast_table$Banda_inferiore_1sigma, forecast_table$Banda_superiore_1sigma, 100), finite = TRUE)
  common_xlim <- range(c(plot_dates, forecast_dates))
  for (method in methods) {
    f <- forecast_table[forecast_table$Metodo == method, ]
    panel_title <- paste0(
      method, "\nOOS: ", sprintf("%.2f%%", 100 * oos_returns[method]),
      " | Forecast 10g: ", sprintf("%.2f%%", 100 * forecast_returns[method])
    )
    plot(plot_dates, wealth_oos[, method], type = "l", col = colors[method],
         lty = historical_lty[method], lwd = 2.8, xlim = common_xlim, ylim = common_ylim,
         xlab = "Data", ylab = "Valore", main = panel_title, cex.main = 0.88)
    polygon(c(f$Data, rev(f$Data)), c(f$Banda_inferiore_1sigma, rev(f$Banda_superiore_1sigma)),
            col = adjustcolor(colors[method], 0.13), border = NA)
    lines(c(max(plot_dates), f$Data), c(tail(wealth_oos[, method], 1), f$Valore_atteso),
          col = colors[method], lty = forecast_lty[method], lwd = 3)
    i <- marker_index[[method]]
    points(plot_dates[i], wealth_oos[i, method], pch = point_shapes[method], col = colors[method],
           bg = if (method == "QAOA Quantistico") "white" else colors[method], cex = 0.6)
    abline(v = max(plot_dates), lty = 2, col = "grey35"); abline(h = 100, lty = 5, col = "grey55")
    grid(col = "grey85", lty = "dotted")
  }
  plot.new()
  legend(
    "center",
    legend = c("Arancione: Ricorsivo", "Blu: Markowitz", "Magenta: QAOA", "",
               "Linea storica: OOS", "Linea tratteggiata: forecast", "Area: banda +/- 1 sigma"),
    col = c(colors, NA, "grey25", "grey25", "grey60"),
    lty = c(historical_lty, NA, 1, 2, 1), lwd = c(2.5, 2.5, 2.5, NA, 2.5, 2.5, 7),
    bty = "n", cex = 0.84
  )
  mtext("Validazione OOS e forecast a 10 sedute", outer = TRUE, side = 3, line = 0.3, font = 2)
}

draw_qaoa <- function() {
  qaoa_cumulative_return <- wealth_oos[, "QAOA Quantistico"] / 100 - 1
  plot(
    plot_dates, 100 * qaoa_cumulative_return, type = "l",
    col = colors["QAOA Quantistico"], lwd = 3,
    xlab = "Data", ylab = "Rendimento cumulato (%)",
    main = "Rendimento cumulato OOS del portafoglio QAOA",
    sub = paste0(
      "OOS finale: ", sprintf("%.2f%%", 100 * oos_returns["QAOA Quantistico"]),
      " | Forecast 10g: ", sprintf("%.2f%%", 100 * forecast_returns["QAOA Quantistico"])
    )
  )
  abline(h = 0, col = "grey45", lty = 2); grid(col = "grey85", lty = "dotted")
}

# Apertura grafici in RGui originale.
show_plot <- function(fun, width = 12, height = 8) {
  if (!isTRUE(show_graphs_on_screen)) return(invisible(FALSE))
  tryCatch({
    if (.Platform$OS.type == "windows") {
      grDevices::windows(width = width, height = height)
    } else {
      grDevices::dev.new(width = width, height = height)
    }
    fun(); invisible(TRUE)
  }, error = function(e) {
    message("Grafico non disponibile: ", conditionMessage(e)); invisible(FALSE)
  })
}
show_plot(draw_validation, 13, 8)
show_plot(draw_forecast, 12, 7)
show_plot(draw_panels, 13, 9)
show_plot(draw_qaoa, 11, 7)

# Immagini temporanee da inserire nel workbook.
png(graph_main, width = 2100, height = 1250, res = 160); draw_validation(); dev.off()
png(graph_forecast, width = 1900, height = 1100, res = 160); draw_forecast(); dev.off()
png(graph_panels, width = 2100, height = 1400, res = 160); draw_panels(); dev.off()
png(graph_qaoa, width = 1800, height = 1000, res = 160); draw_qaoa(); dev.off()

# 14. UNICO WORKBOOK EXCEL -----------------------------------------------------
historical_values <- data.frame(Data = dates_out_sample, wealth_oos, check.names = FALSE)
daily_returns <- data.frame(Data = dates_out_sample, daily_oos, check.names = FALSE)
in_sample_values <- data.frame(
  Data = dates_in_sample,
  do.call(cbind, lapply(method_stats_in, function(x) 100 * x$wealth)),
  check.names = FALSE
)
names(in_sample_values)[-1] <- names(method_stats_in)
parameters_table <- data.frame(
  Parametro = c(
    "Data iniziale", "Data finale", "Numero asset", "Budget", "Risk factor",
    "Trading days", "Risk free rate", "Seed", "Quota training", "Forecast sedute",
    "QAOA reps", "QAOA maxiter", "QAOA shots", "Timeout QAOA secondi",
    "Fine in-sample", "Inizio out-of-sample", "Interprete Python"
  ),
  Valore = c(
    as.character(start_date), as.character(end_date - 1L), n_assets, budget, risk_factor,
    trading_days, risk_free_rate, random_seed, train_fraction, forecast_days,
    qaoa_reps, qaoa_maxiter, qaoa_shots, qaoa_timeout_seconds,
    as.character(max(dates_in_sample)), as.character(min(dates_out_sample)), py_cfg$python
  ), stringsAsFactors = FALSE
)
methodology_table <- data.frame(
  Metodo = names(method_weights),
  Tipo = c("Greedy ricorsivo", "Markowitz combinatorio equiponderato", "QAOA simulato"),
  Pesi = "Uniformi 1/budget",
  Garanzia = c("Nessuna garanzia globale", "Ottimo esatto equiponderato", "Soluzione approssimata"),
  stringsAsFactors = FALSE
)
summary_table <- data.frame(
  Voce = c(
    "Periodo complessivo", "Periodo in-sample", "Periodo out-of-sample",
    "Migliore objective IS", "Migliore Sharpe OOS", "Migliore compromesso OOS",
    "Forecast", "Output"
  ),
  Valore = c(
    paste(min(return_dates), max(return_dates), sep = " - "),
    paste(min(dates_in_sample), max(dates_in_sample), sep = " - "),
    paste(min(dates_out_sample), max(dates_out_sample), sep = " - "),
    metrics$Metodo[which.min(metrics$Objective_in_sample)],
    metrics$Metodo[which.max(metrics$OOS_Sharpe_ratio)],
    paste(metrics$Metodo[metrics$Punteggio_rank_OOS == min(metrics$Punteggio_rank_OOS)], collapse = ", "),
    paste(forecast_days, "sedute"), xlsx_path
  ), stringsAsFactors = FALSE
)

workbook <- openxlsx::createWorkbook()
header_style <- openxlsx::createStyle(
  fontColour = "#FFFFFF", fgFill = "#1F4E78",
  textDecoration = "bold", halign = "center", valign = "center"
)
date_style <- openxlsx::createStyle(numFmt = "dd/mm/yyyy")
percent_style <- openxlsx::createStyle(numFmt = "0.00%")
pvalue_style <- openxlsx::createStyle(numFmt = "0.00000000E+00")
add_styled_sheet <- function(sheet_name, data, filter = TRUE) {
  openxlsx::addWorksheet(workbook, sheet_name, gridLines = FALSE)
  openxlsx::writeData(workbook, sheet_name, data, headerStyle = header_style)
  openxlsx::freezePane(workbook, sheet_name, firstRow = TRUE)
  openxlsx::setColWidths(workbook, sheet_name, cols = seq_len(ncol(data)), widths = "auto")
  if (filter && nrow(data) > 0L) openxlsx::addFilter(workbook, sheet_name, rows = 1, cols = seq_len(ncol(data)))
}
add_styled_sheet("Riepilogo", summary_table, FALSE)
add_styled_sheet("Parametri", parameters_table)
add_styled_sheet("Versioni", version_table)
add_styled_sheet("Metodologia", methodology_table)
add_styled_sheet("Selezioni", selections)
add_styled_sheet("Pesi", weights_table)
add_styled_sheet("Metriche", metrics)
add_styled_sheet("Storico_IS", in_sample_values)
add_styled_sheet("Storico_OOS", historical_values)
add_styled_sheet("Rendimenti_OOS", daily_returns)
add_styled_sheet("Forecast_10gg", forecast_table)
add_styled_sheet("Storico_Forecast", combined_data)
add_styled_sheet("Friedman", friedman_table)
add_styled_sheet("Pairwise", pairwise_table)
add_styled_sheet("Controllo_Curve", curve_check)

for (sheet_name in c("Storico_IS", "Storico_OOS", "Rendimenti_OOS")) {
  sheet_data <- switch(sheet_name, Storico_IS = in_sample_values, Storico_OOS = historical_values, Rendimenti_OOS = daily_returns)
  openxlsx::addStyle(workbook, sheet_name, date_style, rows = 2:(nrow(sheet_data) + 1L), cols = 1, gridExpand = TRUE)
}
openxlsx::addStyle(workbook, "Forecast_10gg", date_style,
                   rows = 2:(nrow(forecast_table) + 1L), cols = match("Data", names(forecast_table)), gridExpand = TRUE)
openxlsx::addStyle(workbook, "Storico_Forecast", date_style,
                   rows = 2:(nrow(combined_data) + 1L), cols = match("Data", names(combined_data)), gridExpand = TRUE)
openxlsx::addStyle(workbook, "Pesi", percent_style,
                   rows = 2:(nrow(weights_table) + 1L), cols = 2:ncol(weights_table), gridExpand = TRUE)
openxlsx::addStyle(workbook, "Rendimenti_OOS", percent_style,
                   rows = 2:(nrow(daily_returns) + 1L), cols = 2:ncol(daily_returns), gridExpand = TRUE)
p_cols <- grep("_p_", names(pairwise_table), ignore.case = TRUE)
if (length(p_cols)) openxlsx::addStyle(workbook, "Pairwise", pvalue_style,
                                       rows = 2:(nrow(pairwise_table) + 1L), cols = p_cols, gridExpand = TRUE)
openxlsx::addStyle(workbook, "Friedman", pvalue_style,
                   rows = 2:(nrow(friedman_table) + 1L), cols = match("P_value", names(friedman_table)), gridExpand = TRUE)

openxlsx::addWorksheet(workbook, "Grafici", gridLines = FALSE)
openxlsx::writeData(workbook, "Grafici", "Validazione OOS con forecast a 10 sedute", startRow = 1)
openxlsx::insertImage(workbook, "Grafici", graph_main, startRow = 3, startCol = 1, width = 11.5, height = 6.8, units = "in")
openxlsx::writeData(workbook, "Grafici", "Dettaglio forecast", startRow = 39)
openxlsx::insertImage(workbook, "Grafici", graph_forecast, startRow = 41, startCol = 1, width = 11, height = 6.3, units = "in")
openxlsx::writeData(workbook, "Grafici", "Pannelli separati con rendimenti", startRow = 75)
openxlsx::insertImage(workbook, "Grafici", graph_panels, startRow = 77, startCol = 1, width = 11.5, height = 7.5, units = "in")
openxlsx::writeData(workbook, "Grafici", "Rendimento cumulato QAOA OOS", startRow = 118)
openxlsx::insertImage(workbook, "Grafici", graph_qaoa, startRow = 120, startCol = 1, width = 10.8, height = 6, units = "in")

openxlsx::saveWorkbook(workbook, xlsx_path, overwrite = TRUE)
if (!file.exists(xlsx_path)) stop("Il workbook Excel non e stato creato.")

# Pulizia esplicita dei file temporanei.
unlink(exchange_dir, recursive = TRUE, force = TRUE)
unlink(graph_dir, recursive = TRUE, force = TRUE)

cat("\n=========================================\nRISULTATI COMPLETATI\n=========================================\n")
print(metrics, row.names = FALSE)
cat("\nWorkbook unico: ", normalizePath(xlsx_path, mustWork = TRUE), "\n", sep = "")
cat("Fogli Excel: Riepilogo, Parametri, Versioni, Metodologia, Selezioni, Pesi, Metriche, Storico_IS, Storico_OOS, Rendimenti_OOS, Forecast_10gg, Storico_Forecast, Friedman, Pairwise, Controllo_Curve e Grafici.\n")
cat("\nNota: il forecast e una proiezione statistica e non garantisce rendimenti futuri.\n")
