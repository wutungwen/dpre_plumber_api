functions {
  real partial_sum_lpmf(
    array[] int result,
    int start, int end,
    
    array[] int senator,
    array[] int bill,
    
    array[] vector senator_latent,
    vector bill_popularity,
    array[] vector bill_latent
  ){
    vector[end - start + 1] lambda;
    int count = 1;
    for (i in start:end){
      lambda[count] = bill_popularity[bill[i]] + bill_latent[bill[i]] '* senator_latent[senator[i]];
      count += 1;
    }
    return (
      bernoulli_logit_lupmf(result | lambda)
    );
  }
}
data {
  int N;
  int P;
  int senator_type;
  int bill_type;
  int K;
  
  //　投票データ
  array[N] int senator;
  array[N] int bill;
  array[N] int result;
  
  //　検証データ
  int val_N;
  array[val_N] int val_senator;
  array[val_N] int val_bill;
  array[val_N] int val_result;
}
parameters {
  real<lower=0> d_alpha;                                       // ディリクレ過程の全体のパラメータ
  vector<lower=0, upper=1>[P - 1] breaks;  // ディリクレ過程のstick-breaking representationのためのパラメータ
  
  vector[bill_type] bill_popularity;
  array[bill_type] vector[K] bill_latent;
  
  array[P] vector[K] P_latent;
  
  vector<lower=0>[senator_type] senator_sigma;
  array[senator_type] vector[K] senator_latent;
}
transformed parameters {
  simplex[P] z; // 
  // stick-breaking representationの変換開始
  // https://discourse.mc-stan.org/t/better-way-of-modelling-stick-breaking-process/2530/2 を参考
  z[1] = breaks[1];
  real sum = z[1];
  for (p in 2:(P - 1)) {
    z[p] = (1 - sum) * breaks[p];
    sum += z[p];
  }
  z[P] = 1 - sum;
}
model {
  d_alpha ~ gamma(1, 1);
  
  breaks ~ beta(1, d_alpha);
  
  for (p in 1:P){
    P_latent[p] ~ std_normal();
  }
  
  bill_popularity ~ std_normal();
  
  for (b in 1:bill_type){
    bill_latent[b] ~ std_normal();
  }
  
  senator_sigma ~ gamma(0.01, 0.01);
  
  for (s in 1:senator_type){
    vector[P] case_vector;
    for (p in 1:P){
      case_vector[p] = log(z[p]) + normal_lupdf(senator_latent[s] | P_latent[p], senator_sigma[s]);
    }
    target += log_sum_exp(case_vector);
  }
  
  int grainsize = 1;
  
  target += reduce_sum(
    partial_sum_lupmf, result, 
    
    grainsize,
    
    senator,
    bill,
    
    senator_latent,
    bill_popularity,
    bill_latent
  );
}
generated quantities {
  real F1_score;
  array[senator_type] vector[P] eta;
  array[senator_type, senator_type] real cos_sim;
  {
    array[val_N] int predicted;
    int TP = 0;
    int FP = 0;
    int FN = 0;
    for (i in 1:val_N){
      predicted[i] = bernoulli_logit_rng(bill_popularity[val_bill[i]] + bill_latent[val_bill[i]] '* senator_latent[val_senator[i]]);
      if (val_result[i] == 1 && predicted[i] == 1){
        TP += 1;
      }
      else if (val_result[i] == 0 && predicted[i] == 1){
        FP += 1;
      }
      else if (val_result[i] == 1 && predicted[i] == 0){
        FN += 1;
      }
    }
    F1_score = (2 * TP) * 1.0/(2 * TP + FP + FN);
    
    for (s in 1:senator_type){
      vector[P] this_eta;
      for (p in 1:P){
        this_eta[p] = z[p] * exp(normal_lpdf(senator_latent[s] | P_latent[p], senator_sigma[s]));
      }
      eta[s] = this_eta./sum(this_eta);
    }
    
    for (i in 1:senator_type){
      for (j in 1:senator_type){
        cos_sim[i, j] = (senator_latent[i] '* senator_latent[j])/sqrt(sum(senator_latent[i].^2) * sum(senator_latent[j].^2));
      }
    }
  }
}
