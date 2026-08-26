# ==============================================================================
# CONFRONTO DI 3 APPROCCI DI PORTAFOGLIO CON FORECAST A 10 SEDUTE
# OTTIMIZZAZIONE RICORSIVA, MARKOWITZ EQUIPONDERATO E QAOA QUANTISTICO
# R + RETICULATE + QISKIT + YAHOO FINANCE + GRAFICI + EXCEL
# ==============================================================================
rm(list=ls(all.names=TRUE)); invisible(gc())

# ==============================================================================
# BLOCCO 1 - PARTE COMUNE AI TRE APPROCCI
# Pacchetti, parametri, dati, rendimenti e suddivisione temporale IS/OOS
# ==============================================================================

# 1.1 PACCHETTI ---------------------------------------------------------------
r_packages <- c("reticulate","quantmod","xts","zoo","openxlsx")
missing_r <- r_packages[!vapply(r_packages,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing_r)>0L) install.packages(missing_r,repos="https://cloud.r-project.org",
  dependencies=c("Depends","Imports","LinkingTo"))
suppressPackageStartupMessages({
  library(reticulate);library(quantmod);library(xts);library(zoo);library(openxlsx)
})
if(!reticulate::py_available(initialize=FALSE)) reticulate::py_require(
  c("numpy>=1.26","qiskit>=1.4","qiskit-optimization>=0.6","qiskit-algorithms>=0.3"),
  python_version=">=3.10,<3.14",action="set")
py_cfg <- reticulate::py_config()
for(m in c("numpy","qiskit","qiskit_optimization","qiskit_algorithms"))
  if(!reticulate::py_module_available(m)) stop("Modulo Python mancante: ",m,
    ". Riavviare R/RStudio e rilanciare lo script completo.")

# 1.2 PARAMETRI ----------------------------------------------------------------
file_path <- "Quantum_dataset.csv"
selection_path <- "selezioni_3_metodi.csv"
weights_path <- "pesi_3_metodi.csv"
metrics_path <- "metriche_3_metodi_forecast_10gg.csv"
forecast_path <- "forecast_10gg_3_metodi.csv"
graph_path <- "confronto_oos_con_continuazione_forecast_10gg.jpg"
panel_graph_path <- "addenda_oos_con_forecast_10gg.jpg"
forecast_graph_path <- "continuazione_forecast_10gg_3_metodi.jpg"
xlsx_path <- "validazione_oos_con_continuazione_forecast_10gg.xlsx"
significance_csv_path <- "significativita_pairwise_3_metodi.csv"
curve_check_path <- "controllo_sovrapposizione_curve.csv"

tickers <- c("IONQ","QBTS","RGTI","IBM","GOOGL","MSFT","NVDA","QQQ")
start_date <- as.Date("2023-01-01"); end_date <- Sys.Date()+1
update_from_yahoo <- TRUE
budget <- 3L; risk_factor <- .50; trading_days <- 252L; risk_free_rate <- 0
random_seed <- 42L; qaoa_reps <- 2L; qaoa_maxiter <- 150L; qaoa_shots <- 4096L
train_fraction <- .70; forecast_days <- 10L; show_graphs_on_screen <- TRUE
if(budget<1L||budget>length(tickers))stop("Budget non valido.")
if(forecast_days<1L)stop("forecast_days deve essere almeno 1.")

# 1.3 DOWNLOAD E LETTURA DATI --------------------------------------------------
download_yahoo_prices <- function(symbols,from,to){
  out<-vector("list",length(symbols));names(out)<-symbols
  for(s in symbols){
    cat("Download ",s,"... ",sep="")
    z<-tryCatch(getSymbols(s,src="yahoo",from=from,to=to,auto.assign=FALSE,
      periodicity="daily",warnings=FALSE),error=function(e)NULL)
    if(is.null(z))stop("Download fallito: ",s)
    p<-tryCatch(Ad(z),error=function(e)NULL);if(is.null(p)||NCOL(p)!=1L)p<-Cl(z)
    colnames(p)<-s;out[[s]]<-p;cat(NROW(p)," osservazioni\n")
  }
  ans<-na.omit(do.call(merge,c(out,all=FALSE)));colnames(ans)<-symbols
  if(NROW(ans)<3L||any(!is.finite(ans))||any(ans<=0))stop("Prezzi non validi.")
  ans
}
read_prices_csv <- function(path,symbols){
  d<-read.csv2(path,stringsAsFactors=FALSE,check.names=FALSE);names(d)[1]<-"Date"
  miss<-setdiff(symbols,names(d));if(length(miss))stop("Ticker mancanti: ",paste(miss,collapse=", "))
  dates<-NULL
  for(f in c("%d/%m/%Y","%Y-%m-%d","%d-%m-%Y","%m/%d/%Y")){
    x<-as.Date(d$Date,f);if(!anyNA(x)){dates<-x;break}}
  if(is.null(dates))stop("Formato data non riconosciuto.")
  parse_num<-function(x){
    if(is.numeric(x))return(as.numeric(x));x<-trimws(as.character(x))
    both<-grepl(",",x,fixed=TRUE)&grepl(".",x,fixed=TRUE)
    x[both]<-gsub(".","",x[both],fixed=TRUE);as.numeric(gsub(",",".",x,fixed=TRUE))
  }
  v<-d[symbols];v[]<-lapply(v,parse_num);if(anyNA(v))stop("Prezzi non numerici.")
  o<-order(dates);ans<-xts(as.matrix(v)[o,,drop=FALSE],order.by=dates[o]);colnames(ans)<-symbols;ans
}
if(update_from_yahoo||!file.exists(file_path)){
  prices_xts<-download_yahoo_prices(tickers,start_date,end_date)
  write.csv2(data.frame(Date=format(index(prices_xts),"%d/%m/%Y"),coredata(prices_xts),check.names=FALSE),
    file_path,row.names=FALSE,quote=FALSE)
}else prices_xts<-read_prices_csv(file_path,tickers)

# 1.4 RENDIMENTI E SUDDIVISIONE TEMPORALE -------------------------------------
price_matrix<-as.matrix(prices_xts)
returns_matrix<-price_matrix[-1,,drop=FALSE]/price_matrix[-NROW(price_matrix),,drop=FALSE]-1
colnames(returns_matrix)<-tickers;return_dates<-index(prices_xts)[-1]
if(NROW(returns_matrix)<2L||any(!is.finite(returns_matrix)))stop("Rendimenti non validi.")
if(max(abs(returns_matrix))>5)stop("Rendimenti giornalieri anomali.")
split_index<-floor(NROW(returns_matrix)*train_fraction)
if(split_index<2L||split_index>=NROW(returns_matrix))stop("Split IS/OOS non valido.")
returns_in_sample<-returns_matrix[seq_len(split_index),,drop=FALSE]
returns_out_sample<-returns_matrix[(split_index+1L):NROW(returns_matrix),,drop=FALSE]
dates_in_sample<-return_dates[seq_len(split_index)]
dates_out_sample<-return_dates[(split_index+1L):length(return_dates)]
mu_daily_vec<-as.numeric(colMeans(returns_in_sample));cov_daily_mat<-unname(cov(returns_in_sample))
mu_vec<-mu_daily_vec*trading_days;cov_mat<-cov_daily_mat*trading_days;n_assets<-length(tickers)
common_objective<-function(w)as.numeric(risk_factor*crossprod(w,cov_mat%*%w)-crossprod(mu_vec,w))
equal_weights_from_indices<-function(i){w<-setNames(numeric(n_assets),tickers);w[i]<-1/budget;w}
cat("\nPeriodo IS: ",format(min(dates_in_sample),"%d/%m/%Y")," - ",format(max(dates_in_sample),"%d/%m/%Y"),"\n",sep="")
cat("Periodo OOS: ",format(min(dates_out_sample),"%d/%m/%Y")," - ",format(max(dates_out_sample),"%d/%m/%Y"),"\n",sep="")

# ==============================================================================
# BLOCCO 2 - APPROCCIO 1: OTTIMIZZAZIONE RICORSIVA
# ==============================================================================

# 2.1 OTTIMIZZAZIONE RICORSIVA ------------------------------------------------
recursive_select<-function(selected=integer(0),available=seq_len(n_assets),level=1L){
  if(level>budget)return(selected)
  vals<-vapply(available,function(j){z<-c(selected,j);w<-numeric(n_assets);w[z]<-1/length(z);common_objective(w)},numeric(1))
  b<-available[which.min(vals)];Recall(c(selected,b),setdiff(available,b),level+1L)}
t0<-proc.time()[[3]];recursive_indices<-recursive_select();recursive_time<-proc.time()[[3]]-t0
recursive_weights<-equal_weights_from_indices(recursive_indices);recursive_objective<-common_objective(recursive_weights)

# ==============================================================================
# BLOCCO 3 - APPROCCIO 2: MARKOWITZ EQUIPONDERATO ESATTO
# Benchmark classico globale nello spazio delle combinazioni ammissibili
# ==============================================================================

# 3.1 MARKOWITZ EQUIPONDERATO ESATTO ------------------------------------------
t0<-proc.time()[[3]];markowitz_combinations<-combn(seq_len(n_assets),budget,simplify=FALSE)
markowitz_values<-vapply(markowitz_combinations,function(i)common_objective(equal_weights_from_indices(i)),numeric(1))
markowitz_best_indices<-markowitz_combinations[[which.min(markowitz_values)]]
markowitz_weights<-equal_weights_from_indices(markowitz_best_indices)
markowitz_objective<-common_objective(markowitz_weights);markowitz_time<-proc.time()[[3]]-t0

# ==============================================================================
# BLOCCO 4 - APPROCCIO 3: QAOA QUANTISTICO SIMULATO
# Formulazione QUBO, esecuzione tramite Qiskit e recupero della soluzione
# ==============================================================================

# 4.1 QAOA ---------------------------------------------------------------------
tmp<-tempfile("qaoa_");dir.create(tmp);on.exit(unlink(tmp,recursive=TRUE,force=TRUE),add=TRUE)
mu_f<-file.path(tmp,"mu.csv");cov_f<-file.path(tmp,"cov.csv");asset_f<-file.path(tmp,"assets.txt")
par_f<-file.path(tmp,"params.txt");py_f<-file.path(tmp,"solve.py");res_f<-file.path(tmp,"result.csv");log_f<-file.path(tmp,"log.txt")
write.table(matrix(mu_vec,nrow=1),mu_f,sep=",",row.names=FALSE,col.names=FALSE,quote=FALSE)
write.table(cov_mat,cov_f,sep=",",row.names=FALSE,col.names=FALSE,quote=FALSE);writeLines(tickers,asset_f)
writeLines(c(paste0("risk_factor=",risk_factor),paste0("budget=",budget),paste0("seed=",random_seed),
  paste0("reps=",qaoa_reps),paste0("maxiter=",qaoa_maxiter),paste0("shots=",qaoa_shots)),par_f)
p<-vapply(c(mu_f,cov_f,asset_f,par_f,res_f),normalizePath,"",winslash="/",mustWork=FALSE)
py<-c("import csv,time,numpy as np","from qiskit_optimization import QuadraticProgram",
 "from qiskit_optimization.algorithms import MinimumEigenOptimizer","from qiskit_algorithms import QAOA",
 "from qiskit_algorithms.optimizers import COBYLA","from qiskit_algorithms.utils import algorithm_globals",
 "from qiskit.primitives import StatevectorSampler",sprintf("mu=np.loadtxt(r'%s',delimiter=',',ndmin=1).reshape(-1)",p[1]),
 sprintf("cov=np.loadtxt(r'%s',delimiter=',',ndmin=2)",p[2]),sprintf("assets=[x.strip() for x in open(r'%s') if x.strip()]",p[3]),
 sprintf("pa=dict(x.strip().split('=',1) for x in open(r'%s') if x.strip())",p[4]),
 "rf=float(pa['risk_factor']);budget=int(pa['budget']);seed=int(pa['seed']);reps=int(pa['reps']);maxiter=int(pa['maxiter']);shots=int(pa['shots'])",
 "n=len(assets);qp=QuadraticProgram(name='Portfolio_QAOA')","for a in assets: qp.binary_var(name=a)",
 "lin={assets[i]:float(-mu[i]/budget) for i in range(n)};quad={}","for i in range(n):",
 "    quad[(assets[i],assets[i])]=float(rf*cov[i,i]/budget**2)",
 "    for j in range(i+1,n): quad[(assets[i],assets[j])]=float(2*rf*cov[i,j]/budget**2)",
 "qp.minimize(linear=lin,quadratic=quad)","qp.linear_constraint(linear={a:1.0 for a in assets},sense='==',rhs=float(budget),name='budget')",
 "algorithm_globals.random_seed=seed;t=time.perf_counter()","sampler=StatevectorSampler(default_shots=shots,seed=seed)",
 "r=MinimumEigenOptimizer(QAOA(sampler=sampler,optimizer=COBYLA(maxiter=maxiter),reps=reps)).solve(qp)",
 "elapsed=time.perf_counter()-t;x=np.rint(np.asarray(r.x,dtype=float)).astype(int)",
 "if int(x.sum())!=budget: raise RuntimeError('QAOA non conforme al budget')","w=x.astype(float)/budget",
 sprintf("with open(r'%s','w',newline='') as f:",p[5]),
 "    z=csv.writer(f,delimiter=';');z.writerow([*[f'w_{a}' for a in assets],'Tempo']);z.writerow([*w.tolist(),elapsed])")
writeLines(py,py_f)
status<-system2(py_cfg$python,shQuote(normalizePath(py_f,winslash="/")),stdout=log_f,stderr=log_f,wait=TRUE)
if(status!=0L||!file.exists(res_f)){if(file.exists(log_f))cat(paste(readLines(log_f),collapse="\n"));stop("QAOA fallito.")}
qr<-read.csv(res_f,sep=";",check.names=FALSE);qaoa_weights<-setNames(as.numeric(qr[1,paste0("w_",tickers)]),tickers)
qaoa_objective<-common_objective(qaoa_weights);qaoa_time<-qr$Tempo[1]

# ==============================================================================
# BLOCCO 5 - CONFRONTO DEI TRE APPROCCI
# Selezioni, pesi, metriche OOS, forecast, test, grafici ed esportazioni
# ==============================================================================

# 5.1 SELEZIONI, PESI E METRICHE -----------------------------------------------
method_weights<-list("Ottimizzazione Ricorsiva"=recursive_weights,"Markowitz Classico"=markowitz_weights,"QAOA Quantistico"=qaoa_weights)
selected_text<-vapply(method_weights,function(w)paste(names(w)[w>1e-10],collapse=", "),"")
selections<-data.frame(Metodo=names(method_weights),Asset_selezionati=unname(selected_text),
 do.call(rbind,lapply(method_weights,function(w)as.integer(w>1e-10))),check.names=FALSE)
names(selections)[-(1:2)]<-paste0("x_",tickers)
weights_table<-data.frame(Metodo=names(method_weights),do.call(rbind,method_weights),check.names=FALSE)
portfolio_analysis<-function(w,R){d<-as.numeric(R%*%w[colnames(R)]);wealth<-cumprod(1+d);ar<-mean(d)*trading_days;av<-sd(d)*sqrt(trading_days)
 list(daily=d,wealth=wealth,annual_return=ar,cumulative_return=tail(wealth,1)-1,annual_volatility=av,
  sharpe=if(av>0)(ar-risk_free_rate)/av else NA_real_,max_drawdown=min(wealth/cummax(wealth)-1))}
method_stats_in<-lapply(method_weights,portfolio_analysis,R=returns_in_sample)
method_stats_out<-lapply(method_weights,portfolio_analysis,R=returns_out_sample)
objectives<-c(recursive_objective,markowitz_objective,qaoa_objective);times<-c(recursive_time,markowitz_time,qaoa_time)

# 5.2 FORECAST A 10 SEDUTE -----------------------------------------------------
next_business_days<-function(last,n){x<-seq.Date(as.Date(last)+1L,by="day",length.out=n*3L);x<-x[as.POSIXlt(x)$wday%in%1:5];x[seq_len(n)]}
forecast_dates<-next_business_days(max(dates_out_sample),forecast_days);forecast_rows<-list();fid<-1L
for(method in names(method_weights)){
 w<-method_weights[[method]][tickers];m<-as.numeric(crossprod(mu_daily_vec,w));s<-sqrt(as.numeric(crossprod(w,cov_daily_mat%*%w)))
 start<-100*tail(method_stats_out[[method]]$wealth,1);h<-seq_len(forecast_days);expected<-start*(1+m)^h
 forecast_rows[[fid]]<-data.frame(Metodo=method,Giorno=h,Data=forecast_dates,Valore_iniziale_OOS=start,
  Valore_atteso=expected,Banda_inferiore_1sigma=expected*exp(-s*sqrt(h)),Banda_superiore_1sigma=expected*exp(s*sqrt(h)),
  Rendimento_atteso_dal_termine_OOS=expected/start-1,Mu_giornaliero=m,Sigma_giornaliera=s);fid<-fid+1L}
forecast_table<-do.call(rbind,forecast_rows)
forecast_final<-forecast_table[forecast_table$Giorno==forecast_days,c("Metodo","Valore_iniziale_OOS","Valore_atteso","Banda_inferiore_1sigma","Banda_superiore_1sigma","Rendimento_atteso_dal_termine_OOS")]
metrics<-data.frame(Metodo=names(method_weights),Asset=unname(selected_text),Objective_in_sample=objectives,Tempo_secondi=times,
 IS_Rendimento_annualizzato=vapply(method_stats_in,`[[`,numeric(1),"annual_return"),IS_Rendimento_cumulato=vapply(method_stats_in,`[[`,numeric(1),"cumulative_return"),
 IS_Volatilita_annualizzata=vapply(method_stats_in,`[[`,numeric(1),"annual_volatility"),IS_Sharpe_ratio=vapply(method_stats_in,`[[`,numeric(1),"sharpe"),
 IS_Max_drawdown=vapply(method_stats_in,`[[`,numeric(1),"max_drawdown"),OOS_Rendimento_annualizzato=vapply(method_stats_out,`[[`,numeric(1),"annual_return"),
 OOS_Rendimento_cumulato=vapply(method_stats_out,`[[`,numeric(1),"cumulative_return"),OOS_Volatilita_annualizzata=vapply(method_stats_out,`[[`,numeric(1),"annual_volatility"),
 OOS_Sharpe_ratio=vapply(method_stats_out,`[[`,numeric(1),"sharpe"),OOS_Max_drawdown=vapply(method_stats_out,`[[`,numeric(1),"max_drawdown"))
metrics<-merge(metrics,forecast_final,by="Metodo",sort=FALSE)
metrics$Gap_vs_migliore_objective<-metrics$Objective_in_sample-min(metrics$Objective_in_sample)
metrics$Rank_Sharpe_OOS<-rank(-metrics$OOS_Sharpe_ratio,ties.method="min");metrics$Rank_Drawdown_OOS<-rank(abs(metrics$OOS_Max_drawdown),ties.method="min")
metrics$Punteggio_rank_OOS<-metrics$Rank_Sharpe_OOS+metrics$Rank_Drawdown_OOS
metrics$Indicazione_OOS<-ifelse(metrics$Punteggio_rank_OOS==min(metrics$Punteggio_rank_OOS),"MIGLIORE COMPROMESSO OUT-OF-SAMPLE","ALTERNATIVA")

# 5.3 SIGNIFICATIVITA OOS ------------------------------------------------------
daily_return_matrix<-do.call(cbind,lapply(method_stats_out,`[[`,"daily"));colnames(daily_return_matrix)<-names(method_stats_out)
all_identical<-max(apply(daily_return_matrix,1,function(z)max(z)-min(z)))<1e-14
if(all_identical) friedman_table<-data.frame(Test="Friedman",Statistica=0,Gradi_liberta=2,P_value=1,Interpretazione="Serie identiche") else {
 ft<-friedman.test(daily_return_matrix);friedman_table<-data.frame(Test="Friedman",Statistica=unname(ft$statistic),Gradi_liberta=unname(ft$parameter),P_value=ft$p.value,Interpretazione=ifelse(ft$p.value<.05,"Significativo","Non significativo"))}
pairs<-combn(colnames(daily_return_matrix),2,simplify=FALSE)
pairwise_significance<-do.call(rbind,lapply(pairs,function(pair){x<-daily_return_matrix[,pair[1]];y<-daily_return_matrix[,pair[2]];same<-max(abs(x-y))<1e-14
 data.frame(Metodo_1=pair[1],Metodo_2=pair[2],Media_diff_giornaliera=mean(x-y),Wilcoxon_p=if(same)1 else suppressWarnings(wilcox.test(x,y,paired=TRUE,exact=FALSE)$p.value),T_test_p=if(same)1 else t.test(x,y,paired=TRUE)$p.value,Serie_identiche=ifelse(same,"SI","NO"))}))
pairwise_significance$Wilcoxon_p_Holm<-p.adjust(pairwise_significance$Wilcoxon_p,"holm");pairwise_significance$T_test_p_Holm<-p.adjust(pairwise_significance$T_test_p,"holm")

# 5.4 GRAFICI OOS E FORECAST ---------------------------------------------------
plot_dates<-as.Date(dates_out_sample)
wealth_matrix<-do.call(cbind,lapply(method_stats_out,function(x)100*x$wealth));colnames(wealth_matrix)<-names(method_stats_out)

# Palette ad alto contrasto e sicura anche per daltonismo.
colors<-c("Ottimizzazione Ricorsiva"="#E69F00","Markowitz Classico"="#0072B2","QAOA Quantistico"="#CC33CC")
hist_lty<-c("Ottimizzazione Ricorsiva"=3,"Markowitz Classico"=1,"QAOA Quantistico"=4)
forecast_lty<-c("Ottimizzazione Ricorsiva"=2,"Markowitz Classico"=6,"QAOA Quantistico"=5)
hist_lwd<-c("Ottimizzazione Ricorsiva"=4.0,"Markowitz Classico"=2.7,"QAOA Quantistico"=2.0)
forecast_lwd<-c("Ottimizzazione Ricorsiva"=4.0,"Markowitz Classico"=3.0,"QAOA Quantistico"=2.2)
pchv<-c("Ottimizzazione Ricorsiva"=17,"Markowitz Classico"=15,"QAOA Quantistico"=21)
methods<-names(method_weights);draw_order<-methods

# Marcatori sfalsati: non cambiano i dati, ma mostrano curve coincidenti.
base_step<-max(3L,floor(length(plot_dates)/30L))
marker_idx<-list(
 "Ottimizzazione Ricorsiva"=seq.int(1L,length(plot_dates),by=base_step),
 "Markowitz Classico"=seq.int(1L+floor(base_step/3),length(plot_dates),by=base_step),
 "QAOA Quantistico"=seq.int(1L+floor(2*base_step/3),length(plot_dates),by=base_step))
marker_idx<-lapply(marker_idx,function(i)i[i<=length(plot_dates)])

curve_check<-data.frame(
 Confronto=c("Ricorsivo vs Markowitz","Ricorsivo vs QAOA","Markowitz vs QAOA"),
 Differenza_massima=c(max(abs(wealth_matrix[,1]-wealth_matrix[,2])),max(abs(wealth_matrix[,1]-wealth_matrix[,3])),max(abs(wealth_matrix[,2]-wealth_matrix[,3]))))
curve_check$Curve_identiche<-ifelse(curve_check$Differenza_massima<1e-12,"SI","NO")
write.csv2(curve_check,curve_check_path,row.names=FALSE)
cat("\nCONTROLLO SOVRAPPOSIZIONE CURVE\n");print(curve_check,row.names=FALSE)

draw_three_curves<-function(){
 yr<-range(c(wealth_matrix,forecast_table$Banda_inferiore_1sigma,forecast_table$Banda_superiore_1sigma,100),finite=TRUE)
 xr<-range(c(plot_dates,forecast_dates));first<-draw_order[1]
 plot(plot_dates,wealth_matrix[,first],type="l",col=colors[first],lty=hist_lty[first],lwd=hist_lwd[first],xlim=xr,ylim=yr,
  xlab="Data",ylab="Valore del portafoglio",main="Validazione OOS dei tre approcci con forecast a 10 sedute",
  sub="Colori, linee e marcatori differenti evidenziano anche curve coincidenti")
 for(method in draw_order[-1])lines(plot_dates,wealth_matrix[,method],col=colors[method],lty=hist_lty[method],lwd=hist_lwd[method])
 # Marcatori storici sfalsati.
 for(method in draw_order){i<-marker_idx[[method]];points(plot_dates[i],wealth_matrix[i,method],pch=pchv[method],
  col=colors[method],bg=if(method=="QAOA Quantistico")"white" else colors[method],cex=if(method=="QAOA Quantistico")1 else .72)}
 # Bande prima delle linee forecast.
 for(method in draw_order){f<-forecast_table[forecast_table$Metodo==method,];polygon(c(f$Data,rev(f$Data)),
  c(f$Banda_inferiore_1sigma,rev(f$Banda_superiore_1sigma)),col=adjustcolor(colors[method],alpha.f=.08),border=NA)}
 # Forecast con stili distinti.
 for(method in draw_order){f<-forecast_table[forecast_table$Metodo==method,];lines(c(max(plot_dates),f$Data),
  c(tail(wealth_matrix[,method],1),f$Valore_atteso),col=colors[method],lty=forecast_lty[method],lwd=forecast_lwd[method]);
  points(f$Data,f$Valore_atteso,pch=pchv[method],col=colors[method],bg=if(method=="QAOA Quantistico")"white" else colors[method],cex=.8)}
 abline(v=max(plot_dates),col="#404040",lty=2,lwd=1.6);abline(h=100,col="#777777",lty=5);grid(col="#D9D9D9",lty="dotted")
 legend("topleft",legend=draw_order,col=colors[draw_order],lty=hist_lty[draw_order],lwd=hist_lwd[draw_order],pch=pchv[draw_order],
  pt.bg=c(colors[1],colors[2],"white"),bty="n",cex=.79,title="Metodi")
 legend("bottomleft",legend=c("Storico OOS","Forecast 10 sedute","Banda +/- 1 sigma","Inizio forecast"),
  col=c("#222222","#222222","#999999","#404040"),lty=c(1,2,NA,2),lwd=c(2.7,3,NA,1.6),pch=c(NA,NA,15,NA),
  pt.bg=c(NA,NA,"#D9D9D9",NA),bty="n",cex=.73,title="Tipo di informazione")
 if(any(curve_check$Curve_identiche=="SI"))mtext("Nota: alcune curve coincidono; i marcatori sfalsati ne mostrano la sovrapposizione.",side=1,line=3.2,cex=.7,col="#555555")
}

draw_forecast<-function(){
 yr<-range(c(forecast_table$Valore_iniziale_OOS,forecast_table$Banda_inferiore_1sigma,forecast_table$Banda_superiore_1sigma));xr<-range(c(max(plot_dates),forecast_dates))
 plot(xr,yr,type="n",xlab="Data",ylab="Valore del portafoglio",main="Continuazione forecast a 10 sedute",
  sub="Stili distinti per rendere visibili previsioni coincidenti")
 for(method in draw_order){f<-forecast_table[forecast_table$Metodo==method,];polygon(c(f$Data,rev(f$Data)),c(f$Banda_inferiore_1sigma,rev(f$Banda_superiore_1sigma)),col=adjustcolor(colors[method],.08),border=NA)}
 for(method in draw_order){f<-forecast_table[forecast_table$Metodo==method,];lines(c(max(plot_dates),f$Data),c(f$Valore_iniziale_OOS[1],f$Valore_atteso),
  col=colors[method],lty=forecast_lty[method],lwd=forecast_lwd[method]);points(f$Data,f$Valore_atteso,pch=pchv[method],col=colors[method],bg=if(method=="QAOA Quantistico")"white" else colors[method],cex=.85)}
 abline(v=max(plot_dates),lty=2,col="#404040");grid(col="#D9D9D9",lty="dotted")
 legend("topleft",legend=draw_order,col=colors[draw_order],lty=forecast_lty[draw_order],lwd=forecast_lwd[draw_order],pch=pchv[draw_order],pt.bg=c(colors[1],colors[2],"white"),bty="n")
}

draw_three_panels<-function(){
 op<-par(no.readonly=TRUE);on.exit(par(op));par(mfrow=c(2,2),mar=c(4.2,4.2,3.2,1.1)+.1)
 common_ylim<-range(c(wealth_matrix,forecast_table$Banda_inferiore_1sigma,forecast_table$Banda_superiore_1sigma,100));common_xlim<-range(c(plot_dates,forecast_dates))
 for(method in draw_order){f<-forecast_table[forecast_table$Metodo==method,];plot(plot_dates,wealth_matrix[,method],type="l",col=colors[method],lty=hist_lty[method],lwd=hist_lwd[method],xlim=common_xlim,ylim=common_ylim,xlab="Data",ylab="Valore",main=method)
  polygon(c(f$Data,rev(f$Data)),c(f$Banda_inferiore_1sigma,rev(f$Banda_superiore_1sigma)),col=adjustcolor(colors[method],.13),border=NA)
  lines(c(max(plot_dates),f$Data),c(tail(wealth_matrix[,method],1),f$Valore_atteso),col=colors[method],lty=forecast_lty[method],lwd=forecast_lwd[method])
  i<-marker_idx[[method]];points(plot_dates[i],wealth_matrix[i,method],pch=pchv[method],col=colors[method],bg=if(method=="QAOA Quantistico")"white" else colors[method],cex=.65)
  abline(v=max(plot_dates),lty=2,col="#404040");abline(h=100,lty=5,col="#777777");grid(col="#D9D9D9",lty="dotted")}
 plot.new();legend("center",legend=c("Arancione: Ricorsivo","Blu: Markowitz","Magenta: QAOA","","Continuo/puntinato: storico","Tratteggiato: forecast","Simboli: curve coincidenti"),
  col=c(colors,NA,"#222222","#222222","#222222"),lty=c(hist_lty,NA,1,2,NA),pch=c(pchv,NA,NA,NA,21),pt.bg=c(colors[1],colors[2],"white",NA,NA,NA,"white"),bty="n",cex=.86)
}
show_plot<-function(fun){if(isTRUE(show_graphs_on_screen)&&interactive())try({dev.new();fun()},silent=TRUE)}
show_plot(draw_three_curves);show_plot(draw_forecast);show_plot(draw_three_panels)
jpeg(graph_path,2100,1250,quality=95,res=160);draw_three_curves();dev.off()
jpeg(forecast_graph_path,1900,1100,quality=95,res=160);draw_forecast();dev.off()
jpeg(panel_graph_path,2100,1400,quality=95,res=160);draw_three_panels();dev.off()

# 5.5 EXCEL ED ESPORTAZIONI ----------------------------------------------------
write.csv2(selections,selection_path,row.names=FALSE);write.csv2(weights_table,weights_path,row.names=FALSE)
write.csv2(metrics,metrics_path,row.names=FALSE);write.csv2(forecast_table,forecast_path,row.names=FALSE)
write.csv2(pairwise_significance,significance_csv_path,row.names=FALSE)
graph_data<-data.frame(Data=plot_dates,wealth_matrix,check.names=FALSE)
wealth_in<-do.call(cbind,lapply(method_stats_in,function(x)100*x$wealth));colnames(wealth_in)<-names(method_stats_in)
graph_data_in<-data.frame(Data=as.Date(dates_in_sample),wealth_in,check.names=FALSE)
daily_data<-data.frame(Data=plot_dates,daily_return_matrix,check.names=FALSE)
parameters<-data.frame(Parametro=c("Data iniziale","Data finale","Budget","Risk factor","Trading days","Seed","QAOA reps","QAOA maxiter","QAOA shots","Quota training","Forecast sedute"),
 Valore=c(as.character(start_date),as.character(end_date-1),budget,risk_factor,trading_days,random_seed,qaoa_reps,qaoa_maxiter,qaoa_shots,train_fraction,forecast_days))
methodology<-data.frame(Metodo=names(method_weights),Tipo=c("Greedy ricorsivo","Markowitz combinatorio equiponderato","QAOA simulato"),Pesi="Uniformi 1/budget")
wb<-createWorkbook();header<-createStyle(fontColour="#FFFFFF",fgFill="#1F4E78",textDecoration="bold",halign="center")
add_sheet<-function(name,data){addWorksheet(wb,name,gridLines=FALSE);writeData(wb,name,data,headerStyle=header);freezePane(wb,name,firstRow=TRUE);setColWidths(wb,name,1:ncol(data),"auto");addFilter(wb,name,1,1:ncol(data))}
add_sheet("Dati_Grafici_OOS",graph_data);add_sheet("Dati_Grafici_IS",graph_data_in);add_sheet("Forecast_10gg_Continuazione",forecast_table)
add_sheet("Rendimenti_Giornalieri",daily_data);add_sheet("Metriche",metrics);add_sheet("Selezioni",selections);add_sheet("Pesi",weights_table)
add_sheet("Metodologia",methodology);add_sheet("Parametri",parameters);add_sheet("Controllo_Curve",curve_check)
add_sheet("Significativita_Globale",friedman_table);add_sheet("Significativita_Pairwise",pairwise_significance)
addWorksheet(wb,"Grafici",gridLines=FALSE);insertImage(wb,"Grafici",graph_path,3,1,11,6.4,"in");insertImage(wb,"Grafici",forecast_graph_path,38,1,11,6.4,"in");insertImage(wb,"Grafici",panel_graph_path,73,1,11,7.5,"in")
saveWorkbook(wb,xlsx_path,overwrite=TRUE)
cat("\nFILE GENERATI\n");for(path in c(file_path,selection_path,weights_path,metrics_path,forecast_path,curve_check_path,graph_path,forecast_graph_path,panel_graph_path,xlsx_path,significance_csv_path))cat(normalizePath(path,mustWork=FALSE),"\n")
cat("\nNota: calcoli invariati. Sono stati corretti soltanto colori e stili grafici.\n")
