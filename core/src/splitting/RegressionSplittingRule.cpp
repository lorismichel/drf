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

#include "RegressionSplittingRule.h"

namespace drf {

RegressionSplittingRule::RegressionSplittingRule(size_t max_num_unique_values,
                                                 double alpha,
                                                 double imbalance_penalty,
                                                 size_t dim_outcome):
    alpha(alpha),
    imbalance_penalty(imbalance_penalty) {
  this->counter = new size_t[max_num_unique_values];
  this->sums = new double[max_num_unique_values * dim_outcome];
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

RegressionSplittingRule::~RegressionSplittingRule() {
  if (counter != nullptr) {
    delete[] counter;
  }
  if (sums != nullptr) {
   // delete[] sums;
   // int s = sizeof(sums)/sizeof(sums[0]); // maybe need some check here
    //for(int i = 0; i < 10; ++i) {
    //  delete[] sums[i];   
    //}
    //Free the array of pointers
    delete[] sums;
    
  }
}

bool RegressionSplittingRule::find_best_split(const Data& data,
                                              size_t node,
                                              const std::vector<size_t>& possible_split_vars,
                                              std::vector<std::vector<double>>& responses_by_sample, // std::vector<double> -> std::vector<std::vector<double>> 
                                              const std::vector<std::vector<size_t>>& samples,
                                              std::vector<size_t>& split_vars,
                                              std::vector<double>& split_values) {

  size_t size_node = samples[node].size();
  size_t min_child_size = std::max<size_t>(std::ceil(size_node * alpha), 1uL);

 // std::cout << "inside regression splitting" << std::endl; 
  //std::cout << responses_by_sample[0].size() << std::endl;
  // Precompute the sum of outcomes in this node.
  std::vector<double> sum_node(data.get_outcome_index().size(),0.0); //g
  for (auto& sample : samples[node]) {
    for (size_t d = 0; d <= (data.get_outcome_index().size()-1); ++d) {
      sum_node[d] += responses_by_sample[sample][d];
    }
  }
 // std::cout << "following split" << std::endl; 
  // Initialize the variables to track the best split variable.
  size_t best_var = 0;
  double best_value = 0;
  double best_decrease = 0.0;
  
  // For all possible split variables
  for (auto& var : possible_split_vars) {
    // Use faster method for both cases
    //double q = (double) size_node / (double) data.get_num_unique_data_values(var);
  //  if (q < Q_THRESHOLD) {
  //    find_best_split_value_small_q(data, node, var, sum_node, size_node, min_child_size,
  //                                  best_value, best_var, best_decrease, responses_by_sample, samples);
  //  } else {
      find_best_split_value_large_q(data, node, var, sum_node, size_node, min_child_size,
                                    best_value, best_var, best_decrease, responses_by_sample, samples);
  //  }
  }

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

void RegressionSplittingRule::find_best_split_value_large_q(const Data& data,
                                                            size_t node,
                                                            size_t var,
                                                            std::vector<double> sum_node,
                                                            size_t size_node,
                                                            size_t min_child_size,
                                                            double& best_value,
                                                            size_t& best_var,
                                                            double& best_decrease,
                                                            const std::vector<std::vector<double>>& responses_by_sample,
                                                            const std::vector<std::vector<size_t>>& samples) {
  //std::cout << "inside value large q" << std::endl; 
  //std::cout << "size_node" << std::endl;
  //std::cout << size_node << std::endl;e()
  //std::cout << "sum_node" << std::endl;
  //std::cout << sum_node << std::endl;
  // Set counters to 0
  size_t num_unique = data.get_num_unique_data_values(var);
  std::fill(counter, counter + num_unique, 0);
  
  
  //for (int i=0; i < num_unique; ++i) {
  //  std::fill(sums[i], sums[i] + data.get_outcome_index().size(), 0.0); 
  //}
  //std::fill(sums, sums + num_unique, 0); //n
  std::fill(sums, sums + (num_unique * data.get_outcome_index().size()), 0); //n
  //std::fill((*sums).begin(), (*sums).begin() + num_unique, std::vector<double>(data.get_outcome_index().size(), 0.0));
  
  for (auto& sample : samples[node]) {
    size_t index = data.get_index(sample, var);

    for (size_t d=0; d<=(data.get_outcome_index().size()-1); ++d) { //n
      sums[(d*num_unique) + index] += responses_by_sample[sample][d];
    }
    //sums[index] += responses_by_sample[sample][0];
    
    ++counter[index];
  }

  //std::cout << "constructing the gain" << std::endl;
  size_t n_left = 0;
  std::vector<double> sum_left(data.get_outcome_index().size(),0.0);
  //double sum_left = 0.0;
  // Compute decrease of impurity for each split
  for (size_t i = 0; i < num_unique - 1; ++i) {
    // Continue if nothing here
    if (counter[i] == 0) {
     // std::cout << "continuing" << std::endl;
      continue;
    }
    
    n_left += counter[i];
    for (size_t d=0; d<=(data.get_outcome_index().size()-1); ++d) { //n
      sum_left[d] += sums[(d*num_unique) + i];
    }
    //sum_left += sums[i];

    // Skip to the next value if the left child is too small.
    if (n_left < min_child_size) {
     // std::cout << "min child too small on left" << std::endl;
      continue;
    }

    // Stop if the right child is too small.
    size_t n_right = size_node - n_left;
    if (n_right < min_child_size) {
    //  std::cout << "min child too small on right" << std::endl;
      break;
    }

    //std::cout << "survived the child size condition" << std::endl;
    double decrease = 0;
    for (size_t d=0; d<=(data.get_outcome_index().size()-1); ++d) { //n
      double sum_right = sum_node[d] - sum_left[d];
      decrease += sum_left[d] * sum_left[d] / (double) n_left + sum_right * sum_right / (double) n_right;
    }
    // Penalize splits that are too close to the edges of the data.
    double penalty = imbalance_penalty * (1.0 / n_left + 1.0 / n_right);
    decrease -= penalty;
    
    //std::cout << "decrease" << std::endl;
    //std::cout << decrease << std::endl;
    // If better than before, use this
    if (decrease > best_decrease) {
      best_value = data.get_unique_data_value(var, i);
      best_var = var;
      best_decrease = decrease;
    }
  }
}

} // namespace drf
