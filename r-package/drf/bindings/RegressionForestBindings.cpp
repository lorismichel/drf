/*-------------------------------------------------------------------------------
  This file is part of ditributional-regression-forest (drf).

  drf is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  drf is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with drf. If not, see <http://www.gnu.org/licenses/>.
 #-------------------------------------------------------------------------------*/

#include <map>
#include <Rcpp.h>
#include <sstream>
#include <vector>

#include "commons/globals.h"
#include "Eigen/Sparse"
#include "forest/ForestPredictors.h"
#include "forest/ForestTrainers.h"
#include "RcppUtilities.h"

using namespace drf;

// [[Rcpp::export]]
Rcpp::List gini_train(Rcpp::NumericMatrix train_matrix,
                            Eigen::SparseMatrix<double> sparse_train_matrix,
                            std::vector<size_t> outcome_index, // 
                            size_t sample_weight_index,
                            bool use_sample_weights,
                            unsigned int mtry,
                            unsigned int num_trees,
                            unsigned int min_node_size,
                            double sample_fraction,
                            bool honesty,
                            double honesty_fraction,
                            bool honesty_prune_leaves,
                            size_t ci_group_size,
                            double alpha,
                            double imbalance_penalty,
                            std::vector<size_t> clusters,
                            unsigned int samples_per_cluster,
                            bool compute_oob_predictions,
                            unsigned int num_threads,
                            unsigned int seed,
                            size_t num_features,
                            double bandwidth, 
                            unsigned int node_scaling) {
  //std::cout << "regression_trainer will start" << std::endl;
  
  ForestTrainer trainer = gini_trainer(outcome_index.size());
  
 //std::cout << "convert_data will start" << std::endl;
  std::unique_ptr<Data> data = RcppUtilities::convert_data(train_matrix, sparse_train_matrix);
  //std::cout << "outcome_index will start and size will be printed" << std::endl;
  for (size_t i = 0; i < outcome_index.size(); ++i) {
    outcome_index[i] = outcome_index[i] - 1;
  }
  
 // std::cout << outcome_index.size() << std::endl;
  data->set_outcome_index(outcome_index);
  
  if(use_sample_weights) {
      data->set_weight_index(sample_weight_index - 1);
  }
  data->sort();
  //std::cout << "options will start" << std::endl;
  ForestOptions options(num_trees, ci_group_size, sample_fraction, mtry, min_node_size, honesty,
      honesty_fraction, honesty_prune_leaves, alpha, imbalance_penalty, num_threads, seed, clusters, samples_per_cluster, num_features, bandwidth, node_scaling);
  //std::cout << "trainer.train will start" << std::endl;
  Forest forest = trainer.train(*data, options);

  std::vector<Prediction> predictions;
  if (compute_oob_predictions) {
    ForestPredictor predictor = regression_predictor(num_threads, outcome_index.size());
    predictions = predictor.predict_oob(forest, *data, false);
  }

  return RcppUtilities::create_forest_object(forest, predictions);
}

// [[Rcpp::export]]
Rcpp::List fourier_train(Rcpp::NumericMatrix train_matrix,
                      Eigen::SparseMatrix<double> sparse_train_matrix,
                      std::vector<size_t> outcome_index, // 
                      size_t sample_weight_index,
                      bool use_sample_weights,
                      unsigned int mtry,
                      unsigned int num_trees,
                      unsigned int min_node_size,
                      double sample_fraction,
                      bool honesty,
                      double honesty_fraction,
                      bool honesty_prune_leaves,
                      size_t ci_group_size,
                      double alpha,
                      double imbalance_penalty,
                      std::vector<size_t> clusters,
                      unsigned int samples_per_cluster,
                      bool compute_oob_predictions,
                      unsigned int num_threads,
                      unsigned int seed,
                      size_t num_features,
                      double bandwidth,
                      unsigned int node_scaling) {
  //std::cout << "regression_trainer will start" << std::endl;
  
  ForestTrainer trainer = fourier_trainer(outcome_index.size());
  
  //std::cout << "convert_data will start" << std::endl;
  std::unique_ptr<Data> data = RcppUtilities::convert_data(train_matrix, sparse_train_matrix);
  //std::cout << "outcome_index will start and size will be printed" << std::endl;
  for (size_t i = 0; i < outcome_index.size(); ++i) {
    outcome_index[i] = outcome_index[i] - 1;
  }
  
  // std::cout << outcome_index.size() << std::endl;
  data->set_outcome_index(outcome_index);
  
  if(use_sample_weights) {
    data->set_weight_index(sample_weight_index - 1);
  }
  data->sort();
  //std::cout << "options will start" << std::endl;
  ForestOptions options(num_trees, ci_group_size, sample_fraction, mtry, min_node_size, honesty,
                        honesty_fraction, honesty_prune_leaves, alpha, imbalance_penalty, num_threads, seed, clusters, samples_per_cluster, num_features, bandwidth, node_scaling);
  //std::cout << "trainer.train will start" << std::endl;
  Forest forest = trainer.train(*data, options);
  
  std::vector<Prediction> predictions;
  if (compute_oob_predictions) {
    ForestPredictor predictor = regression_predictor(num_threads, outcome_index.size());
    predictions = predictor.predict_oob(forest, *data, false);
  }
  
  return RcppUtilities::create_forest_object(forest, predictions);
}

// [[Rcpp::export]]
Rcpp::List regression_predict(Rcpp::List forest_object,
                              Rcpp::NumericMatrix train_matrix,
                              Eigen::SparseMatrix<double> sparse_train_matrix,
                              std::vector<size_t> outcome_index,
                              Rcpp::NumericMatrix test_matrix,
                              Eigen::SparseMatrix<double> sparse_test_matrix,
                              unsigned int num_threads,
                              unsigned int estimate_variance) {
  std::unique_ptr<Data> train_data = RcppUtilities::convert_data(train_matrix, sparse_train_matrix);
  
  for (size_t i=0; i < outcome_index.size(); ++i) {
    outcome_index[i] = outcome_index[i] - 1;
  }
  train_data->set_outcome_index(outcome_index);

  std::unique_ptr<Data> data = RcppUtilities::convert_data(test_matrix, sparse_test_matrix);
  Forest forest = RcppUtilities::deserialize_forest(forest_object);

  ForestPredictor predictor = regression_predictor(num_threads, outcome_index.size());
  std::vector<Prediction> predictions = predictor.predict(forest, *train_data, *data, estimate_variance);

  return RcppUtilities::create_prediction_object(predictions);
}

// [[Rcpp::export]]
Rcpp::List regression_predict_oob(Rcpp::List forest_object,
                                  Rcpp::NumericMatrix train_matrix,
                                  Eigen::SparseMatrix<double> sparse_train_matrix,
                                  std::vector<size_t> outcome_index,
                                  unsigned int num_threads,
                                  bool estimate_variance) {
  //std::cout << "converting data" << std::endl;
  std::unique_ptr<Data> data = RcppUtilities::convert_data(train_matrix, sparse_train_matrix);
  
  for (size_t i=0; i < outcome_index.size(); ++i) {
    outcome_index[i] = outcome_index[i] - 1;
  }
  data->set_outcome_index(outcome_index);

  Forest forest = RcppUtilities::deserialize_forest(forest_object);
   
  ForestPredictor predictor = regression_predictor(num_threads, outcome_index.size());
  //std::cout << "predicting oob" << std::endl;
  std::vector<Prediction> predictions = predictor.predict_oob(forest, *data, estimate_variance);
  //std::cout << "creating predictions" << std::endl;
  Rcpp::List result = RcppUtilities::create_prediction_object(predictions);
  return result;
}
