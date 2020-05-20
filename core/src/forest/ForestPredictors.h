/*-------------------------------------------------------------------------------
  This file is part of ditributional-regression-forest (drf).

  drf is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  frf is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with drf. If not, see <http://www.gnu.org/licenses/>.
 #-------------------------------------------------------------------------------*/

#ifndef DRF_FORESTPREDICTORS_H
#define DRF_FORESTPREDICTORS_H

#include "forest/ForestPredictor.h"

namespace drf {

//ForestPredictor custom_predictor(uint num_threads);

//ForestPredictor instrumental_predictor(uint num_threads);

//ForestPredictor quantile_predictor(uint num_threads,
//                                   const std::vector<double>& quantiles);

ForestPredictor regression_predictor(uint num_threads, size_t dim);

//ForestPredictor ll_regression_predictor(uint num_threads,
//                                        std::vector<double> lambdas,
//                                        bool weight_penalty,
//                                        std::vector<size_t> linear_correction_variables);

//ForestPredictor ll_causal_predictor(uint num_threads,
//                                   std::vector<double> lambdas,
//                                   bool weight_penalty,
//                                   std::vector<size_t> linear_correction_variables);

} // namespace drf

#endif //DRF_FORESTPREDICTORS_H
