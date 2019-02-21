functions {
  matrix get_stan_ll(vector pi, matrix P, int[, ] a, int[] M, int m, int max_M, int n) {
    matrix[m, max_M] P_ak1; // transit prob k -> k+1 for each element in a
    
    P_ak1 = rep_matrix(1, m, max_M);
    for (m_i in 1:m) {
      P_ak1[m_i, 1] = pi[a[m_i, 1]];
      for (k in 1:(M[m_i]-1)) {
        row_vector[n] P_m;
        row_vector[k] a_k;
        P_m = P[a[m_i, k], ];
        for (k_i in 1:k) {
          P_m[a[m_i, k_i]] = 0;
        }
        P_m = P_m/sum(P_m);
        P_ak1[m_i, (k+1)] = P_m[a[m_i, k+1]];
      }
    }
    return log(P_ak1);
  }
}
data {
  int<lower=1> n;  // num words (states)
  int<lower=1> m;  // num lists
  int<lower=0> M[m];  //num words in each list
  int<lower=0> max_M; //longest list length
  int<lower=1> a[m, n]; // words (a[m][1:M[m]] is actual list, a[m][M[m]+1:] are unused words)
  vector<lower=0>[n] alpha;  // init prior
  real<lower=0> C;  // beta prior
  int<lower=0> beta_len;
  int<lower=0> beta_inds[n, n];
}
parameters {
  //Sum of 1:n = n(n+1)/2
  vector[beta_len] beta;  // untransformed P
  vector[n] pi_beta;  // init probs
}
transformed parameters {
  matrix [m, max_M] log_P_ak1;
  matrix<lower=0, upper=1>[n, n] P;  // transit probs
  simplex[n] pi;  // init probs
  
  pi = softmax(exp(pi_beta));
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      P[i, j] = exp(beta[beta_inds[i, j]]);
      P[j, i] = exp(beta[beta_inds[j, i]]);
    }
  }
  for (i in 1:n) {
    P[i, i] = 0;
    P[i, ] = P[i, ]/sum(P[i, ]); //transformed P
  }
  log_P_ak1 = get_stan_ll(pi, P, a, M, m, max_M, n);
} 
model {
  pi_beta ~ normal(0, sqrt(2/C)); //initial word prior
  beta ~ normal(0, sqrt(2/C));
  target += log_P_ak1; //probability of a specific topic being the next topic
}

