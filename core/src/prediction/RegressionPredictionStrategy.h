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

#ifndef drf_REGRESSIONPREDICTIONSTRATEGY_H
#define drf_REGRESSIONPREDICTIONSTRATEGY_H

#include "commons/DefaultData.h"
#include "commons/Data.h"
#include "prediction/OptimizedPredictionStrategy.h"
#include "prediction/PredictionValues.h"
//#include "ObjectiveBayesDebiaser.h"

namespace drf {

class RegressionPredictionStrategy final: public OptimizedPredictionStrategy {
public:
  
  RegressionPredictionStrategy(size_t dim);

  
  size_t prediction_value_length() const;

  PredictionValues precompute_prediction_values(const std::vector<std::vector<size_t>>& leaf_samples,
                                                const Data& data) const;

  size_t prediction_length() const;

  std::vector<double> predict(const std::vector<double>& average) const; //

  std::vector<double> compute_variance( //
      const std::vector<double>& average, //
      const PredictionValues& leaf_values,
      size_t ci_group_size) const;

  // std::vector<std::pair<double, double>> compute_error( // to change
  //     size_t sample,
  //     const std::vector<std::vector<double>>& average,
  //     const PredictionValues& leaf_values,
  //     const Data& data) const;

private:
  static const std::size_t OUTCOME;
  size_t outcome_dim;
 // ObjectiveBayesDebiaser bayes_debiaser;
};

} // namespace drf

#endif //drf_REGRESSIONPREDICTIONSTRATEGY_H
