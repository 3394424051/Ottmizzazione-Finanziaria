# ==============================================================================
# CONFRONTO DI 4 APPROCCI ALLA SELEZIONE DI PORTAFOGLIO
# BRUTE FORCE, OTTIMIZZAZIONE CLASSICA, QUBO ESATTO E QAOA
# R + RETICULATE + QISKIT + YAHOO FINANCE + GRAFICO A 4 CURVE
# ==============================================================================
#
# OBIETTIVO
# ---------
# Tutti i metodi risolvono lo stesso problema binario:
#
#   minimizzare  risk_factor * x' * cov * x - mu' * x
#   con x_i in {0,1} e somma(x) = budget.
#
# ==============================================================================
# I QUATTRO APPROCCI DI OTTIMIZZAZIONE
# ==============================================================================
#
# Tutti i metodi risolvono lo stesso problema di selezione binaria. Il modello
# matematico, i dati, il periodo, il budget e il risk factor sono comuni. Cambia
# soltanto il procedimento usato per cercare la soluzione.
#
# Per ogni vettore binario x, lo script minimizza:
#
#   risk_factor * x' * cov * x - mu' * x
#
# con:
#
#   x_i in {0,1}
#   somma(x_i) = budget
#
# Il primo termine penalizza il rischio del gruppo selezionato. Il secondo
# favorisce gli asset con rendimento atteso più elevato. Poiché si tratta di
# una minimizzazione, un objective più basso è migliore.
#
# I metodi selezionano asset, non pesi continui. Dopo la selezione, R assegna
# peso 1 / budget a ciascun titolo. Con budget 3, ogni asset scelto pesa 33,33%.
# In questo modo le differenze finanziarie dipendono solamente dalla selezione.
#
# ------------------------------------------------------------------------------
# 1. BRUTE FORCE, O RICERCA ESAUSTIVA
# ------------------------------------------------------------------------------
#
# Il Brute Force genera tutte le combinazioni ammissibili di 'budget' titoli.
# Per ogni combinazione costruisce il vettore x, calcola l'objective e conserva
# la soluzione con valore più basso.
#
# Il numero di combinazioni è choose(n, k), dove n è il numero di asset e k è il
# budget. Con 8 titoli e budget 3 vengono valutate choose(8,3) = 56 soluzioni.
#
# Procedimento:
#   1. genera una combinazione di k indici;
#   2. assegna 1 agli asset scelti e 0 agli altri;
#   3. calcola risk_factor * x' * cov * x - mu' * x;
#   4. confronta il risultato con il migliore valore già osservato;
#   5. restituisce il minimo globale dopo aver esaminato tutti i casi.
#
# Vantaggi:
#   - garantisce il minimo globale;
#   - non dipende da seed o punto iniziale;
#   - è semplice da verificare;
#   - costituisce il benchmark per tutti gli altri metodi.
#
# Limiti:
#   - il costo combinatorio cresce rapidamente;
#   - choose(20,5) = 15.504 e choose(30,10) = 30.045.015;
#   - diventa poco pratico su universi molto grandi.
#
# Interpretazione:
#   - Gap_vs_Brute_Force = 0 indica che un metodo ha raggiunto l'ottimo;
#   - gap positivo indica una soluzione ammissibile ma subottimale;
#   - gap negativo è inatteso e richiede una verifica dell'implementazione.
#
# ------------------------------------------------------------------------------
# 2. OTTIMIZZAZIONE CLASSICA CON SIMULATED ANNEALING
# ------------------------------------------------------------------------------
#
# Il simulated annealing è un'euristica ispirata al raffreddamento controllato
# dei materiali. Non enumera necessariamente tutte le combinazioni, ma esplora
# lo spazio delle soluzioni mediante mosse locali.
#
# Procedimento:
#   1. genera un portafoglio iniziale casuale con esattamente k asset;
#   2. sceglie un titolo presente e un titolo escluso;
#   3. scambia i due titoli, mantenendo sempre il vincolo di budget;
#   4. calcola la variazione delta dell'objective;
#   5. accetta sempre una soluzione migliore, cioè delta <= 0;
#   6. può accettare una soluzione peggiore con probabilità
#      exp(-delta / temperatura);
#   7. riduce gradualmente la temperatura moltiplicandola per il cooling rate;
#   8. conserva la migliore soluzione incontrata durante l'intera ricerca.
#
# L'accettazione temporanea di soluzioni peggiori consente di uscire da minimi
# locali. Quando la temperatura diminuisce, il metodo diventa più selettivo.
#
# Parametri:
#   classical_iterations: numero di tentativi di scambio;
#   classical_initial_temperature: libertà esplorativa iniziale;
#   classical_cooling_rate: velocità di raffreddamento;
#   random_seed: riproducibilità della sequenza casuale.
#
# Vantaggi:
#   - non richiede l'enumerazione completa;
#   - può superare minimi locali;
#   - è flessibile e adattabile a problemi più grandi.
#
# Limiti:
#   - non garantisce il minimo globale;
#   - dipende da seed, temperatura, cooling rate e iterazioni;
#   - esecuzioni differenti possono produrre portafogli differenti.
#
# Interpretazione:
#   - gap zero: il simulated annealing ha trovato l'ottimo globale;
#   - gap positivo: ha trovato una soluzione valida ma subottimale;
#   - stessa selezione del Brute Force: ha replicato l'ottimo esatto.
#
# ------------------------------------------------------------------------------
# 3. QUBO ESATTO CON QISKIT E NumPyMinimumEigensolver
# ------------------------------------------------------------------------------
#
# QUBO significa Quadratic Unconstrained Binary Optimization. Lo script crea un
# QuadraticProgram con variabili binarie, funzione quadratica e vincolo di
# budget. MinimumEigenOptimizer converte il problema vincolato in QUBO e poi
# nella corrispondente Hamiltoniana Ising.
#
# Procedimento:
#   1. crea una variabile binaria per ciascun titolo;
#   2. inserisce i coefficienti lineari -mu_i;
#   3. inserisce i coefficienti quadratici legati alla covarianza;
#   4. impone somma(x) = budget;
#   5. converte il modello in QUBO e successivamente in Ising;
#   6. NumPyMinimumEigensolver calcola esattamente il ground state;
#   7. MinimumEigenOptimizer riconverte il risultato nel vettore binario x.
#
# I termini fuori diagonale sono moltiplicati per 2 perché viene registrata solo
# una posizione del triangolo superiore, non entrambe le coppie (i,j) e (j,i).
#
# Vantaggi:
#   - soluzione esatta della formulazione QUBO/Ising;
#   - usa la stessa struttura matematica poi affidata a QAOA;
#   - verifica la conversione QuadraticProgram-QUBO-Ising.
#
# Limiti:
#   - è comunque un risolutore classico esatto;
#   - non costituisce un vantaggio quantistico;
#   - può diventare costoso aumentando il numero di variabili.
#
# Differenza dal Brute Force:
#   - Brute Force enumera direttamente tutte le combinazioni;
#   - QUBO esatto cerca lo stato a energia minima della rappresentazione Ising.
#
# I due metodi dovrebbero avere lo stesso objective. Se differiscono in modo
# significativo, verificare coefficienti, fattore 2, vincolo e ordine degli asset.
#
# ------------------------------------------------------------------------------
# 4. QAOA, QUANTUM APPROXIMATE OPTIMIZATION ALGORITHM
# ------------------------------------------------------------------------------
#
# QAOA è un algoritmo quantistico variazionale. Non definisce un problema nuovo:
# cerca una soluzione approssimata dello stesso QUBO usato dal metodo esatto.
#
# Componenti:
#   StatevectorSampler: simula il circuito e il campionamento;
#   QAOA: costruisce il circuito parametrizzato;
#   COBYLA: ottimizza classicamente i parametri del circuito;
#   MinimumEigenOptimizer: gestisce conversione e riconversione del problema.
#
# Procedimento concettuale:
#   1. converte il QUBO in Hamiltoniana di costo;
#   2. prepara un circuito che alterna operatori di costo e mixer;
#   3. il mixer esplora configurazioni binarie differenti;
#   4. il sampler valuta la distribuzione delle soluzioni;
#   5. COBYLA modifica i parametri per ridurre l'energia attesa;
#   6. la soluzione misurata viene tradotta nel vettore binario degli asset.
#
# Parametri:
#   qaoa_reps: profondità p del circuito. Più livelli aumentano espressività,
#              ma anche numero di parametri e costo computazionale;
#   qaoa_maxiter: iterazioni massime di COBYLA;
#   random_seed: inizializzazione e riproducibilità.
#
# Vantaggi:
#   - applica un metodo quantistico variazionale al QUBO;
#   - può raggiungere o avvicinare l'ottimo;
#   - consente un confronto diretto con un riferimento esatto.
#
# Limiti:
#   - non garantisce il minimo globale;
#   - dipende da seed, profondità e ottimizzazione classica;
#   - una sola esecuzione non misura la robustezza;
#   - StatevectorSampler è un simulatore, non hardware quantistico reale;
#   - la simulazione cresce rapidamente con il numero di qubit.
#
# Interpretazione:
#   - gap zero e stessa x: QAOA ha replicato l'ottimo esatto;
#   - objective uguale ma x diversa: possono esistere minimi multipli;
#   - gap positivo: soluzione QAOA valida ma subottimale;
#   - curve coincidenti: stessa selezione e stessi pesi equal-weight.
#
# ------------------------------------------------------------------------------
# LOGICA COMPLESSIVA DEL CONFRONTO
# ------------------------------------------------------------------------------
#
# Il Brute Force è il benchmark globale. Per ogni metodo viene calcolato:
#
#   Gap_vs_Brute_Force = Objective_metodo - Objective_Brute_Force
#
# Oltre all'objective, lo script confronta tempo di esecuzione, rendimento
# annualizzato, rendimento cumulato, volatilità, Sharpe ratio e max drawdown.
# Un rendimento cumulato più alto non implica automaticamente migliore Sharpe:
# il portafoglio potrebbe anche avere volatilità e drawdown maggiori.
#
# Possibili risultati:
#   A. Tutti i metodi coincidono: euristiche e QAOA replicano l'ottimo.
#   B. Solo i metodi esatti coincidono: le euristiche sono subottimali.
#   C. Objective uguali ma asset diversi: esistono minimi equivalenti.
#   D. Brute Force e QUBO esatto divergono: verificare la formulazione.
#
# Il confronto non deve necessariamente produrre quattro portafogli diversi.
# Curve uguali sono un risultato informativo: metodi differenti hanno raggiunto
# la stessa selezione. Restano diverse garanzia di ottimalità, costo, stabilità
# e percorso algoritmico.
#
# NOTA METODOLOGICA
# -----------------
# Brute Force e QUBO esatto dovrebbero produrre la stessa soluzione. Se esistono
# più minimi equivalenti, possono selezionare combinazioni diverse con objective
# identico. QAOA e simulated annealing possono coincidere oppure no con l'ottimo.
#
# PESI
# ----
# I metodi selezionano asset, non pesi continui. Dopo la selezione, ogni titolo
# riceve peso 1 / budget. Curve uguali significano stessi asset e stessi pesi.
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
selection_path <- "selezioni_4_metodi.csv"
metrics_path <- "metriche_4_metodi.csv"
graph_path <- "confronto_rendimenti_4_metodi.jpg"
panel_graph_path <- "addenda_4_grafici_separati.jpg"
xlsx_path <- "dati_grafici_4_metodi.xlsx"
significance_csv_path <- "significativita_pairwise_4_metodi.csv"

tickers <- c("IONQ", "QBTS", "RGTI", "IBM", "GOOGL", "MSFT", "NVDA", "QQQ")
start_date <- as.Date("2023-01-01")
end_date <- Sys.Date() + 1
update_from_yahoo <- TRUE

budget <- 3L
risk_factor <- 0.50
trading_days <- 252
risk_free_rate <- 0
random_seed <- 42L

# Parametri QAOA.
qaoa_reps <- 2L
qaoa_maxiter <- 150L

# Parametri simulated annealing classico.
classical_iterations <- 5000L
classical_initial_temperature <- 1.0
classical_cooling_rate <- 0.997

if (budget < 1L || budget > length(tickers)) stop("Budget non valido.")

# 3. DOWNLOAD YAHOO FINANCE ----------------------------------------------------
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

# 4. DATI E RENDIMENTI ---------------------------------------------------------
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

mu_vec <- as.numeric(colMeans(returns_matrix) * trading_days)
cov_mat <- unname(stats::cov(returns_matrix) * trading_days)

cat("\nPeriodo: ", format(min(return_dates), "%d/%m/%Y"), " - ",
    format(max(return_dates), "%d/%m/%Y"), "\n", sep = "")
cat("Combinazioni Brute Force: ", choose(length(tickers), budget), "\n", sep = "")

# 5. FILE DI SCAMBIO R-PYTHON -------------------------------------------------
# Reticulate individua Python, ma non converte direttamente matrici R.
exchange_dir <- tempfile("four_methods_")
dir.create(exchange_dir, recursive = TRUE)
on.exit(unlink(exchange_dir, recursive = TRUE, force = TRUE), add = TRUE)

mu_file <- file.path(exchange_dir, "mu.csv")
cov_file <- file.path(exchange_dir, "cov.csv")
assets_file <- file.path(exchange_dir, "assets.txt")
params_file <- file.path(exchange_dir, "params.txt")
python_file <- file.path(exchange_dir, "solve_four_methods.py")
result_file <- file.path(exchange_dir, "results.csv")
log_file <- file.path(exchange_dir, "python.log")

write.table(matrix(mu_vec, nrow = 1), mu_file, sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(cov_mat, cov_file, sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)
writeLines(tickers, assets_file)
writeLines(c(
  paste0("risk_factor=", risk_factor),
  paste0("budget=", budget),
  paste0("seed=", random_seed),
  paste0("qaoa_reps=", qaoa_reps),
  paste0("qaoa_maxiter=", qaoa_maxiter),
  paste0("classical_iterations=", classical_iterations),
  paste0("classical_initial_temperature=", classical_initial_temperature),
  paste0("classical_cooling_rate=", classical_cooling_rate)
), params_file)

paths <- vapply(c(mu_file, cov_file, assets_file, params_file, result_file),
                normalizePath, character(1), winslash = "/", mustWork = FALSE)

# 6. CODICE PYTHON PER I QUATTRO METODI ---------------------------------------
python_code <- c(
  "import csv, itertools, math, time, numpy as np",
  "from qiskit_optimization import QuadraticProgram",
  "from qiskit_optimization.algorithms import MinimumEigenOptimizer",
  "from qiskit_algorithms import QAOA, NumPyMinimumEigensolver",
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
  "risk_factor=float(params['risk_factor']); budget=int(params['budget']); seed=int(params['seed'])",
  "qaoa_reps=int(params['qaoa_reps']); qaoa_maxiter=int(params['qaoa_maxiter'])",
  "classical_iterations=int(params['classical_iterations'])",
  "initial_temperature=float(params['classical_initial_temperature'])",
  "cooling_rate=float(params['classical_cooling_rate']); n=len(assets)",
  "rng=np.random.default_rng(seed)",
  "def objective(x): return float(risk_factor*(x@cov@x)-(mu@x))",
  "",
  "# METODO 1: BRUTE FORCE ESATTO",
  "t0=time.perf_counter(); brute_value=float('inf'); brute_x=None",
  "for selected in itertools.combinations(range(n),budget):",
  "    x=np.zeros(n,dtype=int); x[list(selected)]=1; value=objective(x)",
  "    if value<brute_value: brute_value=value; brute_x=x.copy()",
  "brute_time=time.perf_counter()-t0",
  "",
  "# METODO 2: SIMULATED ANNEALING CLASSICO",
  "t0=time.perf_counter(); current=np.zeros(n,dtype=int)",
  "current[rng.choice(n,size=budget,replace=False)]=1",
  "current_value=objective(current); classical_x=current.copy(); classical_value=current_value",
  "temperature=initial_temperature",
  "for iteration in range(classical_iterations):",
  "    selected=np.flatnonzero(current==1); excluded=np.flatnonzero(current==0)",
  "    candidate=current.copy(); candidate[rng.choice(selected)]=0; candidate[rng.choice(excluded)]=1",
  "    candidate_value=objective(candidate); delta=candidate_value-current_value",
  "    if delta<=0 or rng.random()<math.exp(-delta/max(temperature,1e-12)):",
  "        current=candidate; current_value=candidate_value",
  "    if current_value<classical_value: classical_x=current.copy(); classical_value=current_value",
  "    temperature*=cooling_rate",
  "classical_time=time.perf_counter()-t0",
  "",
  "# QuadraticProgram comune a QUBO esatto Qiskit e QAOA",
  "qp=QuadraticProgram(name='Portfolio_QUBO_4_Methods')",
  "for asset in assets: qp.binary_var(name=asset)",
  "linear={assets[i]:float(-mu[i]) for i in range(n)}; quadratic={}",
  "for i in range(n):",
  "    quadratic[(assets[i],assets[i])]=float(risk_factor*cov[i,i])",
  "    for j in range(i+1,n): quadratic[(assets[i],assets[j])]=float(2*risk_factor*cov[i,j])",
  "qp.minimize(linear=linear,quadratic=quadratic)",
  "qp.linear_constraint(linear={a:1.0 for a in assets},sense='==',rhs=float(budget),name='budget')",
  "",
  "# METODO 3: QUBO ESATTO TRAMITE NUMPY MINIMUM EIGENSOLVER",
  "t0=time.perf_counter()",
  "exact_result=MinimumEigenOptimizer(NumPyMinimumEigensolver()).solve(qp)",
  "qubo_x=np.rint(np.asarray(exact_result.x,dtype=float)).astype(int)",
  "qubo_value=objective(qubo_x); qubo_time=time.perf_counter()-t0",
  "",
  "# METODO 4: QAOA",
  "t0=time.perf_counter(); algorithm_globals.random_seed=seed",
  "sampler=StatevectorSampler(seed=seed)",
  "qaoa=QAOA(sampler=sampler,optimizer=COBYLA(maxiter=qaoa_maxiter),reps=qaoa_reps)",
  "qaoa_result=MinimumEigenOptimizer(qaoa).solve(qp)",
  "if qaoa_result.x is None: raise RuntimeError('QAOA non ha restituito una soluzione')",
  "qaoa_x=np.rint(np.asarray(qaoa_result.x,dtype=float)).astype(int)",
  "if int(qaoa_x.sum())!=budget: raise RuntimeError('Soluzione QAOA non conforme al budget')",
  "qaoa_value=objective(qaoa_x); qaoa_time=time.perf_counter()-t0",
  "",
  "methods=[",
  " ('Brute Force',brute_x,brute_value,brute_time),",
  " ('Ottimizzazione Classica',classical_x,classical_value,classical_time),",
  " ('QUBO Esatto',qubo_x,qubo_value,qubo_time),",
  " ('QAOA',qaoa_x,qaoa_value,qaoa_time)]",
  "with open(result_file,'w',newline='',encoding='utf-8') as f:",
  "    w=csv.writer(f,delimiter=';')",
  "    w.writerow(['Metodo','Asset_selezionati',*[f'x_{a}' for a in assets],'Objective','Tempo_secondi'])",
  "    for name,x,value,elapsed in methods:",
  "        selected=', '.join(assets[i] for i in range(n) if x[i]==1)",
  "        w.writerow([name,selected,*[int(v) for v in x.tolist()],value,elapsed])",
  "for name,x,value,elapsed in methods: print(name, x.tolist(), value, elapsed)"
)
writeLines(python_code, python_file)

# 7. ESECUZIONE PYTHON ---------------------------------------------------------
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
  stop("Il processo Python non ha prodotto i risultati dei quattro metodi.")
}

# 8. LETTURA SELEZIONI E METRICHE ---------------------------------------------
selections <- read.csv(result_file, sep = ";", stringsAsFactors = FALSE, check.names = FALSE)
write.csv2(selections, selection_path, row.names = FALSE)

make_weights <- function(selected_assets) {
  weights <- setNames(rep(0, length(tickers)), tickers)
  weights[selected_assets] <- 1 / length(selected_assets)
  weights
}

portfolio_analysis <- function(selected_assets) {
  weights <- make_weights(selected_assets)
  daily <- as.numeric(returns_matrix %*% weights[colnames(returns_matrix)])
  wealth <- cumprod(1 + daily)
  ann_return <- mean(daily) * trading_days
  ann_volatility <- sd(daily) * sqrt(trading_days)
  list(
    weights = weights,
    daily = daily,
    wealth = wealth,
    annual_return = ann_return,
    cumulative_return = tail(wealth, 1) - 1,
    annual_volatility = ann_volatility,
    sharpe = if (ann_volatility > 0) (ann_return-risk_free_rate)/ann_volatility else NA_real_,
    max_drawdown = min(wealth/cummax(wealth)-1)
  )
}

method_stats <- lapply(selections$Asset_selezionati, function(text) {
  assets <- strsplit(text, ", ", fixed = TRUE)[[1]]
  portfolio_analysis(assets)
})
names(method_stats) <- selections$Metodo

metrics <- data.frame(
  Metodo = selections$Metodo,
  Asset = selections$Asset_selezionati,
  Objective = selections$Objective,
  Tempo_secondi = selections$Tempo_secondi,
  Rendimento_annualizzato = vapply(method_stats, `[[`, numeric(1), "annual_return"),
  Rendimento_cumulato = vapply(method_stats, `[[`, numeric(1), "cumulative_return"),
  Volatilita_annualizzata = vapply(method_stats, `[[`, numeric(1), "annual_volatility"),
  Sharpe_ratio = vapply(method_stats, `[[`, numeric(1), "sharpe"),
  Max_drawdown = vapply(method_stats, `[[`, numeric(1), "max_drawdown"),
  check.names = FALSE
)
write.csv2(metrics, metrics_path, row.names = FALSE)

cat("\n=========================================\n")
cat("CONFRONTO DEI QUATTRO METODI\n")
cat("=========================================\n")
print(metrics)

best_objective_method <- metrics$Metodo[which.min(metrics$Objective)]
best_sharpe_method <- metrics$Metodo[which.max(metrics$Sharpe_ratio)]
cat("Migliore objective: ", best_objective_method, "\n", sep = "")
cat("Migliore Sharpe ratio: ", best_sharpe_method, "\n", sep = "")

# Controllo rispetto al Brute Force, benchmark globale.
brute_objective <- metrics$Objective[metrics$Metodo == "Brute Force"]
metrics$Gap_vs_Brute_Force <- metrics$Objective - brute_objective
cat("\nGap dell'objective rispetto al Brute Force:\n")
print(metrics[, c("Metodo", "Gap_vs_Brute_Force")], row.names = FALSE)

# 9. VALUTAZIONE STATISTICA DELLA SIGNIFICATIVITA -------------------------------
#
# QUALE TEST USARE?
# -----------------
# Il test chi-quadrato non è il test principale per confrontare i rendimenti:
# il chi-quadrato è adatto a conteggi e frequenze categoriali, mentre qui ogni
# metodo produce una serie numerica giornaliera osservata nelle stesse date.
#
# Per questo motivo lo script usa:
#
# 1. TEST DI FRIEDMAN, test globale non parametrico per misure ripetute.
#    Ogni data costituisce un blocco e i quattro metodi sono trattamenti.
#    H0: le distribuzioni/ranghi dei rendimenti giornalieri sono equivalenti.
#
# 2. TEST DI WILCOXON APPAIATO, per ogni coppia di metodi.
#    H0: la distribuzione delle differenze giornaliere è centrata su zero.
#    I p-value sono corretti con Holm per i confronti multipli.
#
# 3. T-TEST APPAIATO, riportato come controllo parametrico supplementare.
#    H0: la media delle differenze giornaliere è zero. Il risultato va letto con
#    cautela perché i rendimenti possono essere non normali e autocorrelati.
#
# 4. CHI-QUADRATO SULLE SELEZIONI, solo come analisi descrittiva accessoria.
#    Confronta la frequenza selezionato/non selezionato fra i quattro metodi.
#    Con pochi asset le frequenze attese possono essere inferiori a 5; in quel
#    caso il p-value asintotico del chi-quadrato non è affidabile, perciò viene
#    usata una simulazione Monte Carlo.
#
# IMPORTANTE
# ----------
# Se i metodi selezionano gli stessi asset, le serie sono identiche. In tal caso
# non esiste alcuna differenza statistica da testare: lo script riporta p-value
# pari a 1 e la nota "serie identiche" invece di forzare un test degenerato.
#
# Questi test valutano differenze storiche fra le serie, non dimostrano che un
# algoritmo sia universalmente superiore, né correggono l'autocorrelazione o il
# fatto che costruzione e valutazione usano lo stesso campione.

daily_return_matrix <- do.call(
  cbind,
  lapply(method_stats, function(x) as.numeric(x$daily))
)
colnames(daily_return_matrix) <- names(method_stats)

if (any(!is.finite(daily_return_matrix))) {
  stop("Le serie dei rendimenti contengono valori non finiti.")
}

# Tre livelli convenzionali di significativita:
# 1%  = evidenza molto forte;
# 5%  = evidenza statisticamente significativa;
# 10% = evidenza debole o marginale.
alpha_1pct <- 0.01
alpha_5pct <- 0.05
alpha_10pct <- 0.10

classify_significance <- function(p_value) {
  if (is.na(p_value)) {
    return("NON CALCOLABILE")
  }
  if (p_value < alpha_1pct) {
    return("SIGNIFICATIVO AL 1%, 5% E 10%")
  }
  if (p_value < alpha_5pct) {
    return("SIGNIFICATIVO AL 5% E 10%, NON ALL'1%")
  }
  if (p_value < alpha_10pct) {
    return("SIGNIFICATIVO SOLO AL 10%")
  }
  "NON SIGNIFICATIVO AL 10%, 5% E 1%"
}

all_series_identical <- max(
  apply(daily_return_matrix, 1, function(row) max(row) - min(row))
) < 1e-14

# Test globale di Friedman. In presenza di serie tutte identiche il test non ha
# variabilità sufficiente; il risultato viene trattato esplicitamente.
if (all_series_identical) {
  friedman_table <- data.frame(
    Test = "Friedman",
    Statistica = 0,
    Gradi_liberta = ncol(daily_return_matrix) - 1L,
    P_value = 1,
    P_value_visualizzato = formatC(1, format = "e", digits = 8),
    Significativo_1pct = "NO",
    Significativo_5pct = "NO",
    Significativo_10pct = "NO",
    Livello_significativita = classify_significance(1),
    Interpretazione = "Serie identiche: nessuna differenza osservabile",
    stringsAsFactors = FALSE
  )
} else {
  friedman_result <- stats::friedman.test(daily_return_matrix)
  friedman_table <- data.frame(
    Test = "Friedman",
    Statistica = unname(friedman_result$statistic),
    Gradi_liberta = unname(friedman_result$parameter),
    P_value = friedman_result$p.value,
    P_value_visualizzato = formatC(friedman_result$p.value, format = "e", digits = 8),
    Significativo_1pct = if (friedman_result$p.value < alpha_1pct) "SI" else "NO",
    Significativo_5pct = if (friedman_result$p.value < alpha_5pct) "SI" else "NO",
    Significativo_10pct = if (friedman_result$p.value < alpha_10pct) "SI" else "NO",
    Livello_significativita = classify_significance(friedman_result$p.value),
    Interpretazione = if (friedman_result$p.value < alpha_1pct) {
      "Evidenza molto forte di differenze globali fra i metodi"
    } else if (friedman_result$p.value < alpha_5pct) {
      "Evidenza significativa di differenze globali fra i metodi"
    } else if (friedman_result$p.value < alpha_10pct) {
      "Evidenza marginale di differenze globali fra i metodi"
    } else {
      "Nessuna evidenza globale di differenze nei ranghi"
    },
    stringsAsFactors = FALSE
  )
}

method_pairs <- combn(colnames(daily_return_matrix), 2, simplify = FALSE)
pairwise_rows <- lapply(method_pairs, function(pair) {
  x <- daily_return_matrix[, pair[1]]
  y <- daily_return_matrix[, pair[2]]
  difference <- x - y
  identical_pair <- max(abs(difference)) < 1e-14

  if (identical_pair) {
    wilcox_statistic <- 0
    wilcox_p <- 1
    t_statistic <- 0
    t_p <- 1
    note <- "Serie identiche"
  } else {
    wilcox_result <- suppressWarnings(stats::wilcox.test(
      x, y, paired = TRUE, exact = FALSE, conf.int = FALSE
    ))
    t_result <- stats::t.test(x, y, paired = TRUE)
    wilcox_statistic <- unname(wilcox_result$statistic)
    wilcox_p <- wilcox_result$p.value
    t_statistic <- unname(t_result$statistic)
    t_p <- t_result$p.value
    note <- "Confronto eseguito"
  }

  data.frame(
    Metodo_1 = pair[1],
    Metodo_2 = pair[2],
    Media_diff_giornaliera = mean(difference),
    Media_diff_annualizzata = mean(difference) * trading_days,
    Mediana_diff_giornaliera = stats::median(difference),
    Wilcoxon_statistica = wilcox_statistic,
    Wilcoxon_p_grezzo = wilcox_p,
    T_test_statistica = t_statistic,
    T_test_p_grezzo = t_p,
    Serie_identiche = if (identical_pair) "SI" else "NO",
    Nota = note,
    stringsAsFactors = FALSE
  )
})
pairwise_significance <- do.call(rbind, pairwise_rows)

# Correzione di Holm: controlla l'errore familiare dovuto ai sei confronti.
pairwise_significance$Wilcoxon_p_Holm <- stats::p.adjust(
  pairwise_significance$Wilcoxon_p_grezzo,
  method = "holm"
)
pairwise_significance$T_test_p_Holm <- stats::p.adjust(
  pairwise_significance$T_test_p_grezzo,
  method = "holm"
)
pairwise_significance$Wilcoxon_p_grezzo_visualizzato <- formatC(
  pairwise_significance$Wilcoxon_p_grezzo, format = "e", digits = 8
)
pairwise_significance$Wilcoxon_p_Holm_visualizzato <- formatC(
  pairwise_significance$Wilcoxon_p_Holm, format = "e", digits = 8
)
pairwise_significance$T_test_p_grezzo_visualizzato <- formatC(
  pairwise_significance$T_test_p_grezzo, format = "e", digits = 8
)
pairwise_significance$T_test_p_Holm_visualizzato <- formatC(
  pairwise_significance$T_test_p_Holm, format = "e", digits = 8
)
pairwise_significance$Wilcoxon_significativo_1pct <- ifelse(
  pairwise_significance$Wilcoxon_p_Holm < alpha_1pct, "SI", "NO"
)
pairwise_significance$Wilcoxon_significativo_5pct <- ifelse(
  pairwise_significance$Wilcoxon_p_Holm < alpha_5pct, "SI", "NO"
)
pairwise_significance$Wilcoxon_significativo_10pct <- ifelse(
  pairwise_significance$Wilcoxon_p_Holm < alpha_10pct, "SI", "NO"
)
pairwise_significance$Wilcoxon_livello <- vapply(
  pairwise_significance$Wilcoxon_p_Holm,
  classify_significance,
  character(1)
)

pairwise_significance$T_test_significativo_1pct <- ifelse(
  pairwise_significance$T_test_p_Holm < alpha_1pct, "SI", "NO"
)
pairwise_significance$T_test_significativo_5pct <- ifelse(
  pairwise_significance$T_test_p_Holm < alpha_5pct, "SI", "NO"
)
pairwise_significance$T_test_significativo_10pct <- ifelse(
  pairwise_significance$T_test_p_Holm < alpha_10pct, "SI", "NO"
)
pairwise_significance$T_test_livello <- vapply(
  pairwise_significance$T_test_p_Holm,
  classify_significance,
  character(1)
)

# Analisi chi-quadrato delle selezioni. Le colonne x_ticker hanno valore 0/1.
x_columns <- paste0("x_", tickers)
selection_matrix <- as.matrix(selections[, x_columns, drop = FALSE])
storage.mode(selection_matrix) <- "integer"
selection_contingency <- cbind(
  Selezionati = rowSums(selection_matrix == 1L),
  Esclusi = rowSums(selection_matrix == 0L)
)
rownames(selection_contingency) <- selections$Metodo

# Se ogni metodo seleziona lo stesso numero di titoli, i totali di riga sono
# uguali; il chi-quadrato sui soli totali può non distinguere quali asset siano
# stati scelti. Il test è quindi accessorio, non sostituisce il confronto x.
chi_square_result <- suppressWarnings(stats::chisq.test(
  selection_contingency,
  simulate.p.value = TRUE,
  B = 10000
))

chi_square_table <- data.frame(
  Test = "Chi-quadrato Monte Carlo sulle frequenze selezionato/escluso",
  Statistica = unname(chi_square_result$statistic),
  Gradi_liberta = NA_real_,
  P_value = chi_square_result$p.value,
  P_value_visualizzato = formatC(chi_square_result$p.value, format = "e", digits = 8),
  Significativo_1pct = if (chi_square_result$p.value < alpha_1pct) "SI" else "NO",
  Significativo_5pct = if (chi_square_result$p.value < alpha_5pct) "SI" else "NO",
  Significativo_10pct = if (chi_square_result$p.value < alpha_10pct) "SI" else "NO",
  Livello_significativita = classify_significance(chi_square_result$p.value),
  Interpretazione = if (chi_square_result$p.value < alpha_1pct) {
    "Evidenza molto forte di differenze nelle frequenze aggregate"
  } else if (chi_square_result$p.value < alpha_5pct) {
    "Evidenza significativa di differenze nelle frequenze aggregate"
  } else if (chi_square_result$p.value < alpha_10pct) {
    "Evidenza marginale di differenze nelle frequenze aggregate"
  } else {
    "Nessuna evidenza di differenze nelle frequenze aggregate"
  },
  Nota = "Test accessorio: non identifica quali specifici asset differiscono",
  stringsAsFactors = FALSE
)

selection_frequency_table <- data.frame(
  Metodo = rownames(selection_contingency),
  Selezionati = selection_contingency[, "Selezionati"],
  Esclusi = selection_contingency[, "Esclusi"],
  stringsAsFactors = FALSE
)

write.csv2(pairwise_significance, significance_csv_path, row.names = FALSE)

cat("\n=========================================\n")
cat("SIGNIFICATIVITA STATISTICA DEI METODI\n")
cat("=========================================\n")
print(friedman_table, row.names = FALSE)
cat(
  "P-value Friedman (scientifico): ",
  formatC(friedman_table$P_value[1], format = "e", digits = 12),
  "\n",
  sep = ""
)
cat("\nConfronti appaiati con correzione Holm:\n")
print(
  pairwise_significance[, c(
    "Metodo_1", "Metodo_2", "Media_diff_annualizzata",
    "Wilcoxon_p_grezzo_visualizzato", "Wilcoxon_p_Holm_visualizzato",
    "Wilcoxon_significativo_1pct", "Wilcoxon_significativo_5pct",
    "Wilcoxon_significativo_10pct", "Wilcoxon_livello",
    "T_test_p_grezzo_visualizzato", "T_test_p_Holm_visualizzato",
    "T_test_significativo_1pct", "T_test_significativo_5pct",
    "T_test_significativo_10pct", "T_test_livello", "Serie_identiche"
  )],
  row.names = FALSE
)
cat("\nAnalisi accessoria delle frequenze di selezione:\n")
print(chi_square_table, row.names = FALSE)
cat(
  "P-value Chi-quadrato Monte Carlo (scientifico): ",
  formatC(chi_square_table$P_value[1], format = "e", digits = 12),
  "\n",
  sep = ""
)

# 9. GRAFICO CON QUATTRO CURVE -------------------------------------------------
# Curve coincidenti indicano che i metodi hanno scelto lo stesso portafoglio.
plot_dates <- as.Date(return_dates)
wealth_matrix <- do.call(cbind, lapply(method_stats, function(x) 100 * x$wealth))
colnames(wealth_matrix) <- names(method_stats)

y_limits <- range(c(wealth_matrix, 100), finite = TRUE)
colors <- c(
  "Brute Force" = "#000000",
  "Ottimizzazione Classica" = "#FF7F0E",
  "QUBO Esatto" = "#1F77B4",
  "QAOA" = "#D62728"
)
line_types <- c(
  "Brute Force" = 1,
  "Ottimizzazione Classica" = 3,
  "QUBO Esatto" = 2,
  "QAOA" = 4
)
line_widths <- c(
  "Brute Force" = 4,
  "Ottimizzazione Classica" = 2.5,
  "QUBO Esatto" = 3,
  "QAOA" = 2.5
)

# Marcatori periodici rendono visibili curve perfettamente sovrapposte.
point_index <- seq.int(1L, length(plot_dates), by = max(1L, floor(length(plot_dates)/24L)))
point_shapes <- c(
  "Brute Force" = 15,
  "Ottimizzazione Classica" = 17,
  "QUBO Esatto" = 22,
  "QAOA" = 21
)

draw_four_curves <- function() {
  methods <- colnames(wealth_matrix)
  first <- methods[1]

  plot(
    plot_dates, wealth_matrix[, first], type = "l",
    col = colors[first], lty = line_types[first], lwd = line_widths[first],
    ylim = y_limits, xlab = "Data", ylab = "Valore del portafoglio",
    main = "Confronto rendimenti cumulati dei quattro metodi",
    sub = "Capitale iniziale = 100"
  )

  if (length(methods) > 1L) {
    for (method in methods[-1]) {
      lines(plot_dates, wealth_matrix[, method],
            col = colors[method], lty = line_types[method], lwd = line_widths[method])
    }
  }

  for (method in methods) {
    points(
      plot_dates[point_index], wealth_matrix[point_index, method],
      pch = point_shapes[method], col = colors[method],
      bg = if (method %in% c("QUBO Esatto", "QAOA")) "white" else colors[method],
      cex = 0.65
    )
  }

  abline(h = 100, col = "grey50", lty = 5)
  grid(col = "grey85", lty = "dotted")
  legend(
    "topleft", legend = methods,
    col = colors[methods], lty = line_types[methods], lwd = line_widths[methods],
    pch = point_shapes[methods], bty = "n", cex = 0.82
  )
}

try(grDevices::dev.new(), silent = TRUE)
draw_four_curves()
grDevices::jpeg(
  graph_path, width = 1900, height = 1100,
  units = "px", quality = 95, res = 160
)
draw_four_curves()
grDevices::dev.off()

if (!file.exists(graph_path)) stop("Il grafico JPG non è stato creato.")

# 10. ADDENDA: QUATTRO GRAFICI SEPARATI CON par(mfrow = c(2, 2)) ---------------
# Il grafico sovrapposto è utile per confrontare direttamente i metodi, ma può
# risultare poco leggibile quando più curve coincidono. Questa addenda crea una
# griglia 2 x 2: ogni pannello usa la stessa scala temporale e la stessa scala Y.
# In questo modo forma, volatilità e drawdown di ogni portafoglio sono leggibili
# senza sovrapposizioni. La linea orizzontale a 100 è il capitale iniziale.

draw_four_panels <- function() {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(mfrow = c(2, 2), mar = c(4.2, 4.4, 3.5, 1.2) + 0.1, oma = c(0, 0, 2, 0))

  for (method in colnames(wealth_matrix)) {
    plot(
      plot_dates,
      wealth_matrix[, method],
      type = "l",
      col = colors[method],
      lty = line_types[method],
      lwd = 2.8,
      ylim = y_limits,
      xlab = "Data",
      ylab = "Valore del portafoglio",
      main = method
    )
    abline(h = 100, col = "grey50", lty = 5)
    grid(col = "grey85", lty = "dotted")
    points(
      plot_dates[point_index],
      wealth_matrix[point_index, method],
      pch = point_shapes[method],
      col = colors[method],
      bg = if (method %in% c("QUBO Esatto", "QAOA")) "white" else colors[method],
      cex = 0.65
    )

    # Il rendimento finale viene riportato in ogni pannello.
    final_return <- tail(wealth_matrix[, method], 1) - 100
    legend(
      "topleft",
      legend = paste0("Rendimento finale: ", sprintf("%.2f%%", final_return)),
      bty = "n",
      cex = 0.78
    )
  }

  mtext(
    "Addenda: andamento separato dei quattro metodi",
    outer = TRUE,
    side = 3,
    line = 0.4,
    font = 2,
    cex = 1.1
  )
}

# Visualizzazione a video della griglia 2 x 2.
try(grDevices::dev.new(), silent = TRUE)
draw_four_panels()

# Salvataggio della griglia 2 x 2 in un secondo file JPG.
grDevices::jpeg(
  panel_graph_path,
  width = 1900,
  height = 1300,
  units = "px",
  quality = 95,
  res = 160
)
draw_four_panels()
grDevices::dev.off()

if (!file.exists(panel_graph_path)) {
  stop("Il JPG con i quattro grafici separati non è stato creato.")
}

# 11. CARTELLA EXCEL CON I DATI NECESSARI AI GRAFICI ---------------------------
# Il workbook contiene:
# - Dati_Grafici: data e valore di ciascun portafoglio con base 100;
# - Rendimenti_Giornalieri: serie usate per costruire gli indici cumulati;
# - Metriche: risultato finanziario e tempo di calcolo per metodo;
# - Selezioni: variabili binarie e asset selezionati;
# - Parametri: ipotesi comuni del modello.
#
# L'Excel permette di riprodurre i grafici, verificare le curve e svolgere
# ulteriori analisi senza dover rieseguire Qiskit.

graph_data <- data.frame(
  Data = plot_dates,
  wealth_matrix,
  check.names = FALSE
)

daily_return_matrix <- do.call(
  cbind,
  lapply(method_stats, function(x) as.numeric(x$daily))
)
colnames(daily_return_matrix) <- names(method_stats)
daily_return_data <- data.frame(
  Data = plot_dates,
  daily_return_matrix,
  check.names = FALSE
)

parameters_table <- data.frame(
  Parametro = c(
    "Data iniziale", "Data finale", "Numero asset", "Budget",
    "Risk factor", "Trading days", "Risk free rate", "Seed",
    "QAOA reps", "QAOA maxiter", "Iterazioni classiche",
    "Temperatura iniziale", "Cooling rate"
  ),
  Valore = c(
    as.character(start_date), as.character(end_date - 1), length(tickers), budget,
    risk_factor, trading_days, risk_free_rate, random_seed,
    qaoa_reps, qaoa_maxiter, classical_iterations,
    classical_initial_temperature, classical_cooling_rate
  ),
  stringsAsFactors = FALSE
)

significance_levels_table <- data.frame(
  Livello = c("1%", "5%", "10%", "Non significativo"),
  Condizione_p_value = c(
    "p < 0,01",
    "0,01 <= p < 0,05",
    "0,05 <= p < 0,10",
    "p >= 0,10"
  ),
  Interpretazione = c(
    "Evidenza molto forte contro l'ipotesi nulla",
    "Evidenza statisticamente significativa",
    "Evidenza debole o marginale",
    "Evidenza insufficiente contro l'ipotesi nulla"
  ),
  stringsAsFactors = FALSE
)

pvalue_guide_table <- data.frame(
  Campo = c(
    "P_value", "P_value_visualizzato", "p_grezzo", "p_Holm",
    "Significativo_1pct", "Significativo_5pct", "Significativo_10pct"
  ),
  Significato = c(
    "Valore numerico completo usato nei calcoli",
    "Valore testuale in notazione scientifica, sempre visibile",
    "P-value prima della correzione per confronti multipli",
    "P-value dopo correzione Holm, da privilegiare nei confronti pairwise",
    "SI se p < 0,01", "SI se p < 0,05", "SI se p < 0,10"
  ),
  stringsAsFactors = FALSE
)

workbook <- openxlsx::createWorkbook()
header_style <- openxlsx::createStyle(
  fontColour = "#FFFFFF",
  fgFill = "#1F4E78",
  textDecoration = "bold",
  halign = "center"
)
percent_style <- openxlsx::createStyle(numFmt = "0.00%")
number_style <- openxlsx::createStyle(numFmt = "0.000000")
date_style <- openxlsx::createStyle(numFmt = "dd/mm/yyyy")

write_styled_sheet <- function(sheet_name, data) {
  openxlsx::addWorksheet(workbook, sheet_name, gridLines = FALSE)
  openxlsx::writeData(workbook, sheet_name, data, headerStyle = header_style)
  openxlsx::freezePane(workbook, sheet_name, firstRow = TRUE)
  openxlsx::setColWidths(workbook, sheet_name, cols = 1:ncol(data), widths = "auto")
  openxlsx::addFilter(workbook, sheet_name, rows = 1, cols = 1:ncol(data))
}

write_styled_sheet("Dati_Grafici", graph_data)
write_styled_sheet("Rendimenti_Giornalieri", daily_return_data)
write_styled_sheet("Metriche", metrics)
write_styled_sheet("Selezioni", selections)
write_styled_sheet("Parametri", parameters_table)
write_styled_sheet("Significativita_Globale", friedman_table)
write_styled_sheet("Significativita_Pairwise", pairwise_significance)
write_styled_sheet("Chi_Quadrato", chi_square_table)
write_styled_sheet("Frequenze_Selezione", selection_frequency_table)
write_styled_sheet("Livelli_Significativita", significance_levels_table)
write_styled_sheet("Guida_P_Value", pvalue_guide_table)

# Formattazione dei p-value nelle tabelle di significativita.
pvalue_style <- openxlsx::createStyle(numFmt = "0.00000000E+00")
for (sheet_name in c("Significativita_Globale", "Significativita_Pairwise", "Chi_Quadrato")) {
  sheet_data <- switch(
    sheet_name,
    "Significativita_Globale" = friedman_table,
    "Significativita_Pairwise" = pairwise_significance,
    "Chi_Quadrato" = chi_square_table
  )
  p_cols <- grep("P_value|_p_", names(sheet_data), ignore.case = TRUE)
  if (length(p_cols) > 0L) {
    openxlsx::addStyle(
      workbook, sheet_name, pvalue_style,
      rows = 2:(nrow(sheet_data) + 1), cols = p_cols, gridExpand = TRUE
    )
  }
}

# Evidenziazione visiva delle colonne SI/NO e dei livelli di significativita.
# Verde: significativo all'1%; azzurro: significativo al 5%; giallo: solo 10%;
# grigio: non significativo.
sig_green <- openxlsx::createStyle(fgFill = "#C6EFCE", fontColour = "#006100")
sig_blue <- openxlsx::createStyle(fgFill = "#DDEBF7", fontColour = "#1F4E78")
sig_yellow <- openxlsx::createStyle(fgFill = "#FFF2CC", fontColour = "#7F6000")
sig_gray <- openxlsx::createStyle(fgFill = "#E7E6E6", fontColour = "#595959")

apply_level_colors <- function(sheet_name, data) {
  level_cols <- grep("Livello_significativita|_livello$", names(data), ignore.case = TRUE)
  if (length(level_cols) == 0L || nrow(data) == 0L) return(invisible(NULL))

  for (col in level_cols) {
    values <- as.character(data[[col]])
    for (row_index in seq_along(values)) {
      style <- if (grepl("AL 1%", values[row_index], fixed = TRUE)) {
        sig_green
      } else if (grepl("AL 5%", values[row_index], fixed = TRUE)) {
        sig_blue
      } else if (grepl("SOLO AL 10%", values[row_index], fixed = TRUE)) {
        sig_yellow
      } else {
        sig_gray
      }
      openxlsx::addStyle(
        workbook, sheet_name, style,
        rows = row_index + 1L, cols = col, gridExpand = FALSE, stack = TRUE
      )
    }
  }
}

apply_level_colors("Significativita_Globale", friedman_table)
apply_level_colors("Significativita_Pairwise", pairwise_significance)
apply_level_colors("Chi_Quadrato", chi_square_table)

# Formattazione delle date nei primi due fogli.
openxlsx::addStyle(
  workbook, "Dati_Grafici", date_style,
  rows = 2:(nrow(graph_data) + 1), cols = 1, gridExpand = TRUE
)
openxlsx::addStyle(
  workbook, "Rendimenti_Giornalieri", date_style,
  rows = 2:(nrow(daily_return_data) + 1), cols = 1, gridExpand = TRUE
)

# I valori giornalieri sono percentuali; i valori cumulati restano in base 100.
openxlsx::addStyle(
  workbook, "Rendimenti_Giornalieri", percent_style,
  rows = 2:(nrow(daily_return_data) + 1), cols = 2:ncol(daily_return_data),
  gridExpand = TRUE
)

# Formattazione coerente di objective e tempi nella tabella delle metriche.
objective_col <- match("Objective", names(metrics))
time_col <- match("Tempo_secondi", names(metrics))
openxlsx::addStyle(
  workbook, "Metriche", number_style,
  rows = 2:(nrow(metrics) + 1), cols = c(objective_col, time_col),
  gridExpand = TRUE
)

# Inserisce nel workbook anche il grafico comparativo e l'addenda 2 x 2.
openxlsx::addWorksheet(workbook, "Grafici", gridLines = FALSE)
openxlsx::writeData(workbook, "Grafici", "Grafico comparativo sovrapposto", startRow = 1)
openxlsx::insertImage(
  workbook, "Grafici", graph_path,
  startRow = 3, startCol = 1, width = 11, height = 6.4, units = "in"
)
openxlsx::writeData(workbook, "Grafici", "Addenda con quattro pannelli", startRow = 36)
openxlsx::insertImage(
  workbook, "Grafici", panel_graph_path,
  startRow = 38, startCol = 1, width = 11, height = 7.5, units = "in"
)

openxlsx::saveWorkbook(workbook, xlsx_path, overwrite = TRUE)

if (!file.exists(xlsx_path)) {
  stop("Il file Excel con i dati dei grafici non è stato creato.")
}

cat("\n=========================================\n")
cat("FILE GENERATI\n")
cat("=========================================\n")
cat("Dataset: ", normalizePath(file_path, mustWork = FALSE), "\n", sep = "")
cat("Selezioni: ", normalizePath(selection_path, mustWork = FALSE), "\n", sep = "")
cat("Metriche: ", normalizePath(metrics_path, mustWork = FALSE), "\n", sep = "")
cat("Grafico sovrapposto: ", normalizePath(graph_path, mustWork = FALSE), "\n", sep = "")
cat("Addenda 2 x 2: ", normalizePath(panel_graph_path, mustWork = FALSE), "\n", sep = "")
cat("Dati Excel: ", normalizePath(xlsx_path, mustWork = FALSE), "\n", sep = "")
cat("Significativita CSV: ", normalizePath(significance_csv_path, mustWork = FALSE), "\n", sep = "")

# LIMITI
# ------
# - Il simulated annealing dipende da seed, temperatura, cooling e iterazioni.
# - StatevectorSampler è un simulatore e non hardware quantistico reale.
# - Le metriche sono in-sample e non includono costi o slippage.
# - I pesi sono equiponderati e implicano ribilanciamento giornaliero.
