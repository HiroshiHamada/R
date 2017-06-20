data {
  int N;
  real Weight[N];
  real Age[N];
  real Y[N];
}

parameters {
  real b[3];
  real<lower=0> s_Y;
}

transformed parameters {
  real mu[N];
  for (n in 1:N){
    mu[n] = b[1] + b[2]*Age[n] + b[3]*Weight[n];
  }
}

model {
	  for (n in 1:N) {
    Y[n] ~ normal(mu[n], s_Y);
  }
}

generated quantities{
	vector[N] log_lik;
	for(n in 1:N){
			log_lik[n] = normal_lpdf(Y[n] | mu[n], s_Y);
	}
}
