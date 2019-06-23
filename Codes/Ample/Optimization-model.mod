
param m; 			#number of jobs
param n; 			#number of workers
param a;			#coefficient for LiFFT dataset
param p;			#coefficient for LiFFT dataset
param d;			#number of damage sources
param c;			#minimum cycle for each worker
param ad;			#coefficient for DUET dataset
param pd;			#coefficient for DUET dataset



set WORKERS = 1..n;
set JOBS = 1..m;
set SOURCES = 1..d;

#damage per cycle of job j from source k
param dpc {JOBS, SOURCES};

#number of cycles of job j need to be completed
param num_cycles {JOBS};

var B {WORKERS}    >= 0;
var X {WORKERS, JOBS}    >= 0;			         #number of cycles from job j performed by worker i
var Y {WORKERS, SOURCES} ;				 #logarithmic damage for worker i from source k
var T {WORKERS, SOURCES} ;				 #total damage for worker i from source k
var Prob {WORKERS, SOURCES} >= 0;			 #probability of injury for worker i from source k
var z >=0 ;
var Tprob {WORKERS} >= 0;			         #total probabilty for worker i

#Two scenarios for job rotation
#minimize total_risk: sum{i in WORKERS} (Tprob[i]);
minimize Minmax: z;

subject to Minwork {i in WORKERS}:
	sum{j in JOBS} X[i, j] = c;

#Total damage that worker i receives
subject to TotalDamage {i in WORKERS, k in SOURCES}: 
	T[i, k] = sum{j in JOBS} dpc[j, k] * X[i, j];

#damage from LiFFT dataset for worker i
subject to Ydamage_l {i in WORKERS, k in SOURCES}: 
	Y[i, 1] = a + p * (log(T[i, 1]));
#damage from DUET dataset for worker i	
subject to Ydamage_d {i in WORKERS, k in SOURCES}: 
	Y[i, 2] = ad + pd * (log(T[i, 2]));

#Injury probability for worker i from each source
subject to ProbInjury {i in WORKERS, k in SOURCES}:
	Prob[i, k] = 1 / (1 + exp(-Y[i, k]));
	
#Total injury probability	
subject to TotalProb {i in WORKERS}:
	Tprob[i] = sum {k in SOURCES} Prob[i, k] - (Prob[i, 1] * Prob[i, 2]);

subject to maxprob {i in WORKERS}:
	z >= Tprob[i];

#Each workers works for specefic number of cycles
subject to Cycles {j in JOBS}:
	sum {i in WORKERS} X[i, j] = num_cycles[j];
	
subject to Check {i in WORKERS}:
	sum{j in JOBS} X[i, j] = B[i]; 	
	
