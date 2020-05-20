/*-------------------------------------------------------------------------------
  This file is part of generalized random forest (grf).

  grf is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  grf is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with grf. If not, see <http://www.gnu.org/licenses/>.
 #-------------------------------------------------------------------------------*/

 
// THERE NEED CHANGES
#include <algorithm>
#include <random> //n
#include <vector> //n
#include <complex> //n

#include "FourierSplittingRule.h"

namespace drf {

FourierSplittingRule::FourierSplittingRule(size_t max_num_unique_values,
                                                 double alpha,
                                                 double imbalance_penalty,
                                                 size_t dim_outcome,
                                                 size_t num_features,
                                                 double bandwidth,
                                                 unsigned int node_scaling):
    alpha(alpha),
    imbalance_penalty(imbalance_penalty),
    num_features(num_features),
    bandwidth(bandwidth),
    node_scaling(node_scaling){
  //this->counter = new size_t[max_num_unique_values];
  //this->sums = new double[max_num_unique_values];
  //s this->sums = new double[max_num_unique_values * dim_outcome];
  // pointers of pointers
 // this->sums = new double*[max_num_unique_values];
 // for (int i = 0; i < (max_num_unique_values-1); ++i) {
 //   this->sums[i] = new double[dim_outcome];
 // }
   
  //new std::vector<std::vector<double>>(max_num_unique_values, std::vector<double>(dim_outcome,0.0));
  //this->sums = new std::vector<std::vector<double>>(max_num_unique_values);
  //for (size_t d=0; d<=((this->sums).size()-1); ++d) {
  //  this->sums[d]->resize(dim_outcome, 0.0);
  //}
  //Free each sub-array
 
  
}

FourierSplittingRule::~FourierSplittingRule() {
  // if (counter != nullptr) {
  //   delete[] counter;
  // }
  // if (sums != nullptr) {
  //  // delete[] sums;
  //  // int s = sizeof(sums)/sizeof(sums[0]); // maybe need some check here
  //   //for(int i = 0; i < 10; ++i) {
  //   //  delete[] sums[i];   
  //   //}
  //   //Free the array of pointers
  //   delete[] sums;
  //   
  // }
}

bool FourierSplittingRule::find_best_split(const Data& data,
                                              size_t node,
                                              const std::vector<size_t>& possible_split_vars,
                                              std::vector<std::vector<double> >& responses_by_sample, // std::vector<double> -> std::vector<std::vector<double>> 
                                              const std::vector<std::vector<size_t> >& samples,
                                              std::vector<size_t>& split_vars,
                                              std::vector<double>& split_values) {

  size_t size_node = samples[node].size();
  size_t min_child_size = std::max<size_t>(std::ceil(size_node * alpha), 1uL);

 //n initialize the fourier features
 std::vector<std::vector<std::complex<double> > > fourier_features;
 fourier_features.resize(num_features);
 std::default_random_engine generator;
 std::normal_distribution<double> distribution(0.0, 1.0/(bandwidth*bandwidth));
  


 size_t d = data.get_outcome_index().size(); //int d = data[0].size(); //dimensionality of the response
 size_t n = samples[node].size(); //int n = sampleIDs.size(); //number of datapoints in the current node
 std::complex<double> i(0, 1); //imaginary unit
 
 std::vector<double> omega(d, 0);
 
 // center and scale

 if (node_scaling) {

   for(size_t k = 0; k < d; ++k) {
     double m = 0.0;
     double s = 0.0;
     // center 
     for(size_t j = 0; j < n; ++j) {
       m += responses_by_sample[samples[node][j]][k];     
     }
     m = m / n;
     for(size_t j = 0; j < n; ++j) {
       responses_by_sample[samples[node][j]][k] = responses_by_sample[samples[node][j]][k] - m;
     }
     // scale
     for(size_t j = 0; j < n; ++j) {
       s += responses_by_sample[samples[node][j]][k] * responses_by_sample[samples[node][j]][k];
     }
     s = sqrt(s/n);
     if (s > 10e-10) {
       for(size_t j = 0; j < n; ++j) {
         responses_by_sample[samples[node][j]][k] = responses_by_sample[samples[node][j]][k]/s;
       }
     }
   }
 }


 for(size_t l = 0; l < num_features; ++l){
   fourier_features[l].resize(n);
   
   for(size_t k = 0; k < d; ++k)
     omega[k] = distribution(generator);
   
   for(size_t j = 0; j < n; ++j){
     double theta = 0;
     for(size_t k = 0; k < d; ++k)
       theta += omega[k] * responses_by_sample[samples[node][j]][k];
     
     fourier_features[l][j] = exp(i * theta);
   }
 }
 
 // std::vector<std::complex<double>> sum_node(0, std::complex<double>(0, 0));
 // 
 // for(size_t i = 0; i < num_features; ++i){
 //   std::complex<double> sum_node(0, 0);
 //   for(int j = 0; j < n; ++j) {
 //     sum_node[i] += fourier_features[i][j];
 //   }
 
 //std::cout << "inside regression splitting" << std::endl; 
 //  //std::cout << responses_by_sample[0].size() << std::endl;
 //  // Precompute the sum of outcomes in this node.
 //  std::vector<double> sum_node(data.get_outcome_index().size(),0.0); //g
 //  for (auto& sample : samples[node]) {
 //    for (size_t d = 0; d <= (data.get_outcome_index().size()-1); ++d) {
 //      sum_node[d] += responses_by_sample[sample][d];
 //    }
 //  }
 // std::cout << "following split" << std::endl; 
  // Initialize the variables to track the best split variable.
  size_t best_var = 0;
  double best_value = 0;
  double best_decrease = -1.0;
  //std::cout << "entering the splits" << std::endl;
  

  // For all possible split variables
  for (auto& var : possible_split_vars) {
    
    // index in sorted x
    std::vector<size_t> idx_in_sorted_array;
    // for (size_t i=0; i < samples[node].size(); ++i) {
    //   idx_in_sorted_array.push_back(i);
    // }
    //idx_in_sorted_array.clear();
    //idx_in_sorted_array.resize(samples[node].size());
    
     for (auto& sample : samples[node]) {
       size_t index = data.get_index(sample, var);
       idx_in_sorted_array.push_back(index);
     }
     idx_in_sorted_array.resize(size_node);
    
    find_split(data, node, best_value, best_var, best_decrease, min_child_size,
               var, fourier_features, idx_in_sorted_array, samples);
    //std::cout << var << std::endl;
    // Use faster method for both cases
   // double q = (double) size_node / (double) data.get_num_unique_data_values(var);
  //  if (q < Q_THRESHOLD) {
  //    find_best_split_value_small_q(data, node, var, sum_node, size_node, min_child_size,
  //                                  best_value, best_var, best_decrease, responses_by_sample, samples);
  //  } else {
      // find_best_split_value_large_q(data, node, var, sum_node, size_node, min_child_size,
      //                               best_value, best_var, best_decrease, responses_by_sample, samples);
  //  }
  }

  // std::cout << "size of node" << std::endl;
  // std::cout << size_node << std::endl;
  // std::cout << "node" << std::endl;
  // std::cout << node << std::endl;
  // std::cout << "best decrease" << std::endl;
  // std::cout << best_decrease << std::endl;
  // std::cout << "best var" << std::endl;
  // std::cout << best_var << std::endl;
  // std::cout << "best value" << std::endl;
  // std::cout << best_value << std::endl;
  
  // Stop if no good split found
  if (best_decrease <= 0.0) {
    return true;
  }

  // Save best values
  split_vars[node] = best_var;
  split_values[node] = best_value;
  
  return false;
}

// void RegressionSplittingRule::find_best_split_value_small_q(const Data& data,
//                                                             size_t node, size_t var,
//                                                             std::vector<double> sum_node,
//                                                             size_t size_node,
//                                                             size_t min_child_size,
//                                                             double& best_value, size_t& best_var,
//                                                             double& best_decrease,
//                                                             const std::vector<std::vector<double>>& responses_by_sample,
//                                                             const std::vector<std::vector<size_t>>& samples) {
//   //std::cout << "inside value small q" << std::endl; 
//   std::vector<double> possible_split_values;
//   data.get_all_values(possible_split_values, samples[node], var);
// 
//   // Try next variable if all equal for this
//   if (possible_split_values.size() < 2) {
//     return;
//   } 
// 
//   // Remove largest value because no split possible
//   possible_split_values.pop_back();
// 
//   // Initialize with 0m if not in memory efficient mode, use pre-allocated space
//   size_t num_splits = possible_split_values.size();
//   double* sums_right;
//   size_t* n_right;
//   sums_right = sums;
//   n_right = counter;
//   std::fill(sums_right, sums_right + num_splits, 0);
//   std::fill(n_right, n_right + num_splits, 0);
// 
//   // Sum in right child and possible split
//   for (auto& sample : samples[node]) {
//     double value = data.get(sample, var);
//     double response = responses_by_sample[sample][0];
// 
//     // Count samples until split_value reached
//     for (size_t i = 0; i < num_splits; ++i) {
//       if (value > possible_split_values[i]) {
//         ++n_right[i];
//         sums_right[i] += response;
//       } else {
//         break;
//       }
//     }
//   }
// 
//   // Compute decrease of impurity for each possible split
//   for (size_t i = 0; i < num_splits; ++i) {
// 
//     // Skip this split if one child is too small.
//     size_t n_left = size_node - n_right[i];
//     if (n_left < min_child_size) {
//       continue;
//     }
// 
//     // Stop if the right child is too small.
//     if (n_right[i] < min_child_size) {
//       break;
//     }
// 
//     double sum_right = sums_right[i];
//     double sum_left = sum_node[0] - sum_right;
//     double decrease = sum_left * sum_left / (double) n_left + sum_right * sum_right / (double) n_right[i];
// 
//     // Penalize splits that are too close to the edges of the data.
//     double penalty = imbalance_penalty * (1.0 / n_left + 1.0 / n_right[i]);
//     decrease -= penalty;
// 
//     std::cout << decrease << std::endl;
//     // If better than before, use this
//     if (decrease > best_decrease) {
//       best_value = possible_split_values[i];
//       best_var = var;
//       best_decrease = decrease;
//     }
//   }
// }

// void FourierSplittingRule::find_best_split_value_large_q(const Data& data,
//                                                             size_t node,
//                                                             size_t var,
//                                                             std::vector<std::complex<double>> sum_node,
//                                                             size_t size_node,
//                                                             size_t min_child_size,
//                                                             double& best_value,
//                                                             size_t& best_var,
//                                                             double& best_decrease,
//                                                             const std::vector<std::vector<double>>& responses_by_sample,
//                                                             const std::vector<std::vector<size_t>>& samples) {
//   //std::cout << "inside value large q" << std::endl;
//   //std::cout << "size_node" << std::endl;
//   //std::cout << size_node << std::endl;e()
//   //std::cout << "sum_node" << std::endl;
//   //std::cout << sum_node << std::endl;
//   // Set counters to 0
//   size_t num_unique = data.get_num_unique_data_values(var);
//   std::fill(counter, counter + num_unique, 0);
// 
// 
//   //for (int i=0; i < num_unique; ++i) {
//   //  std::fill(sums[i], sums[i] + data.get_outcome_index().size(), 0.0);
//   //}
//   //std::fill(sums, sums + num_unique, 0); //n
//   std::fill(sums, sums + (num_unique * data.get_outcome_index().size()), 0); //n
//   //std::fill((*sums).begin(), (*sums).begin() + num_unique, std::vector<double>(data.get_outcome_index().size(), 0.0));
// 
//   for (auto& sample : samples[node]) {
//     size_t index = data.get_index(sample, var);
// 
//     for (size_t d=0; d<=(data.get_outcome_index().size()-1); ++d) { //n
//       sums[d*num_unique + index] += responses_by_sample[sample][d];
//     }
//     //sums[index] += responses_by_sample[sample][0];
// 
//     ++counter[index];
//   }
// 
//   //std::cout << "constructing the gain" << std::endl;
//   size_t n_left = 0;
//   //std::vector<double> sum_left(data.get_outcome_index().size(),0.0);
//   double sum_left = 0.0;
//   // Compute decrease of impurity for each split
//   for (size_t i = 0; i < num_unique - 1; ++i) {
//     // Continue if nothing here
//     if (counter[i] == 0) {
//      // std::cout << "continuing" << std::endl;
//       continue;
//     }
// 
//     n_left += counter[i];
//     for (size_t d=0; d<=(data.get_outcome_index().size()-1); ++d) { //n
//       sum_left[d] += sums[d*num_unique + i];
//     }
//     //sum_left += sums[i];
// 
//     // Skip to the next value if the left child is too small.
//     if (n_left < min_child_size) {
//      // std::cout << "min child too small on left" << std::endl;
//       continue;
//     }
// 
//     // Stop if the right child is too small.
//     size_t n_right = size_node - n_left;
//     if (n_right < min_child_size) {
//     //  std::cout << "min child too small on right" << std::endl;
//       break;
//     }
// 
//     //std::cout << "survived the child size condition" << std::endl;
//     sum_left += fourier_features[i][ordering[j]];
//     sum_right = sum - sum_left;
//     MMD[j] += pow(abs(sum_left/(double)(j+1) - sum_right/(double)(n-j-1)), 2) / num_features;
//     
//     double decrease = 0; // this is the MMD
//     
//     for (size_t d=0; d<=(data.get_outcome_index().size()-1); ++d) { //n
//       std::complex<double> sum_right = sum_node[d] - sum_left[d];
//       decrease += pow(abs(sum_left[d]/(double)(n_left) - sum_right/(double)(n_right)), 2) / num_features;
//       //sum_left[d] * sum_left[d] / (double) n_left + sum_right * sum_right / (double) n_right;
//     }
//     //Penalize splits that are too close to the edges of the data.
//     //double penalty = imbalance_penalty * (1.0 / n_left + 1.0 / n_right);
//     //decrease -= penalty;
// 
//     //std::cout << "decrease" << std::endl;
//     //std::cout << decrease << std::endl;
//     // If better than before, use this
//     if (decrease > best_decrease) {
//       best_value = data.get_unique_data_value(var, i);
//       best_var = var;
//       best_decrease = decrease;
//     }
//   }
// }

void FourierSplittingRule::find_split(const Data& data,
                                      size_t node,
                                      double& best_value,
                                      size_t& best_var,
                                      double& best_decrease,
                                      size_t min_child_size,
                                      size_t var,
                                      const std::vector<std::vector<std::complex<double> > >& fourier_features,
                                      const std::vector<size_t>& idx_in_sorted_array,
                                      const std::vector<std::vector<size_t>>& samples){
  
  //std::cout << "inside find_split" << std::endl;
  int n = idx_in_sorted_array.size();
  
  if (n <= min_child_size) {
    return;
  }
  
  std::vector<std::pair<size_t, size_t>> tmp;
  tmp.resize(n);
  
  for(size_t i = 0; i < n; ++i) {
    tmp[i] = std::make_pair(idx_in_sorted_array[i], i);
  }
  sort(tmp.begin(), tmp.end()); //sorts by default lexicographically
  
  std::vector<size_t> ordering(n, 0);
  for(size_t i = 0; i < n; ++i) {
    ordering[i] = tmp[i].second;
  }
  //how are the datapoints in the current node sorted according to X_i, ordering[i] is the index of the datapoint in the sampleIDs that is at place i
  //std::cout << "survived ordering building" << std::endl;
  size_t num_features = fourier_features.size();
  //std::vector<double> MMD(0, n-1);
  std::vector<double> MMD(n-1, 0.0);
  
  for(size_t i = 0; i < num_features; ++i){
    std::complex<double> sum(0, 0);
    for(int j = 0; j < n; ++j) {
      sum += fourier_features[i][j];
    }
    
    std::complex<double> sum_left(0, 0), sum_right(0, 0);
    for(size_t j = 0;  j < (n-1); ++j){
      sum_left += fourier_features[i][ordering[j]];
      sum_right = sum - sum_left;
      MMD[j] += ((j+1)*(n-j-1)/n)*pow(abs(sum_left/(double)(j+1) - sum_right/(double)(n-j-1)), 2) / num_features;
    }
  }
  
  //std::cout << "variable: " << var << std::endl;
  //std::cout << "MMD: " << n << std::endl;
  //int sampleID = -1;
  //double best_stat = 0;
  
  double best_dec = -1;
  double best_val = 0;
  double best_dec10 = -1;
  double best_val10 = 0;
  
  for(size_t i = 0; i < (n-1); ++i){
    if(tmp[i].first == tmp[i+1].first)
      continue;
    if(best_dec < MMD[i]){
      best_dec = MMD[i];
      best_val = data.get(samples[node][tmp[i].second], var);
    }
    if((i+1) < 0.1*n && (n-i-1) < 0.1*n)  {// we ensure at least 10% of datapoints is in each child to ensure logarithmic depth of the tree
      continue;
    }
    if(best_dec10 < MMD[i]){
      best_dec10 = MMD[i];
      best_val10 = data.get(samples[node][tmp[i].second], var);
    }
  }
  
  if(best_dec10 > -0.5){
    if(best_decrease < best_dec10){
      best_decrease = best_dec10;
      best_value = best_val10;
      best_var = var;
    }
  }
  else{
    if(best_decrease < best_dec){
      best_decrease = best_dec;
      best_value = best_val;
      best_var = var;
    }
  }
  //return make_pair(sampleID, best_stat);
}


} // namespace drf
