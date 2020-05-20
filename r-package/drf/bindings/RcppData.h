/*-------------------------------------------------------------------------------
  This file is part of ditributional-regression-forest.

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

#ifndef DRF_RCPPDATA_H
#define DRF_RCPPDATA_H

#include <Rcpp.h>

#include "commons/Data.h"
#include "commons/utility.h"

using namespace drf;

class RcppData final: public Data {
public:
  RcppData(Rcpp::NumericMatrix& data, size_t num_rows, size_t num_cols);

  double get(size_t row, size_t col) const;

  void reserve_memory();

  void set(size_t col, size_t row, double value, bool& error);

private:
  Rcpp::NumericMatrix data;

  DISALLOW_COPY_AND_ASSIGN(RcppData);
};

#endif //DRF_RCPPDATA_H
