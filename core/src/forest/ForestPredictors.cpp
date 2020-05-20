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

#include "forest/ForestPredictors.h"
//#include "prediction/CustomPredictionStrategy.h"
//#include "prediction/InstrumentalPredictionStrategy.h"
//#include "prediction/QuantilePredictionStrategy.h"
#include "prediction/RegressionPredictionStrategy.h"
//#include "prediction/LocalLinearPredictionStrategy.h"
//#include "prediction/LLCausalPredictionStrategy.h"

namespace drf {


ForestPredictor regression_predictor(uint num_threads, size_t dim) {
  num_threads = ForestOptions::validate_num_threads(num_threads);
  std::unique_ptr<OptimizedPredictionStrategy> prediction_strategy(new RegressionPredictionStrategy(dim));
  return ForestPredictor(num_threads, std::move(prediction_strategy));
}


} // namespace drf
