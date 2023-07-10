/*-------------------------------------------------------------------------------
 This file is part of distributional random forest (drf).
 
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

#ifndef DRF_SPLITFREQUENCYCOMPUTER_H
#define DRF_SPLITFREQUENCYCOMPUTER_H


#include "forest/Forest.h"

namespace drf {

/**
 * Computes a matrix of split depth by variable ID, where each value is
 * the number of times the variable was split on at that depth.
 *
 * forest: the forest for which split frequencies should be computed
 * max_depth: the maximum depth of splits to consider, exclusive
 */
class SplitFrequencyComputer {
public:
  std::vector<std::vector<size_t>> compute(const Forest& forest,
                                           size_t max_depth) const;
};

} // namespace drf

#endif //DRF_SPLITFREQUENCYCOMPUTER_H
