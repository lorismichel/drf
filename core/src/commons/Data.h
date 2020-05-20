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

#ifndef DRF_DATA_H_
#define DRF_DATA_H_

#include <iostream>
#include <set>
#include <vector>

#include "globals.h"
#include "optional/optional.hpp"

namespace drf {

class Data {
public:
  Data();

  virtual ~Data() = default;

  virtual void reserve_memory() = 0;

  virtual double get(size_t row, size_t col) const = 0;

  virtual void set(size_t col, size_t row, double value, bool& error) = 0;

  void sort();

  bool load_from_file(const std::string& filename);

  bool load_from_whitespace_file(std::ifstream& input_file, const std::string& first_line);

  bool load_from_other_file(std::ifstream& input_file, const std::string& first_line, char seperator);

  void set_outcome_index(std::vector<size_t> index); //

  void set_treatment_index(size_t index);

  void set_instrument_index(size_t index);

  void set_weight_index(size_t index);

  void get_all_values(std::vector<double>& all_values, const std::vector<size_t>& samples, size_t var) const;

  size_t get_index(size_t row, size_t col) const;

  double get_unique_data_value(size_t var, size_t index) const;

  size_t get_num_unique_data_values(size_t var) const;

  size_t get_num_cols() const;

  size_t get_num_rows() const;

  size_t get_max_num_unique_values() const;

  std::vector<double> get_outcome(size_t row) const; //

  double get_treatment(size_t row) const;

  double get_instrument(size_t row) const;

  double get_weight(size_t row) const;

  const std::set<size_t>& get_disallowed_split_variables() const;
  
  std::vector<size_t> get_outcome_index() const;

protected:
  size_t num_rows;
  size_t num_cols;

  std::vector<size_t> index_data;
  std::vector<std::vector<double>> unique_data_values;
  size_t max_num_unique_values;

  std::set<size_t> disallowed_split_variables;
  std::vector<size_t> outcome_index; //
  nonstd::optional<size_t> treatment_index;
  nonstd::optional<size_t> instrument_index;
  nonstd::optional<size_t> weight_index;

private:
  DISALLOW_COPY_AND_ASSIGN(Data);
};

} // namespace drf
#endif /* DRF_DATA_H_ */
