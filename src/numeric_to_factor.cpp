#include <Rcpp.h>
#include <vector>
#include <math.h>
#include <algorithm>
#include <string>
using namespace Rcpp;

std::vector<double> _sort_actuals = std::vector<double>();

// Sort into the *permutation* that would give the correct shuffle
bool sortshuffle (int i, int j) {
  return (_sort_actuals[i] < _sort_actuals[j]);
}

// Equality comparison for floats
bool epsilon_compare(float a, float b) {
  return fabs(a - b) < std::numeric_limits<double>::epsilon();
}
// [[Rcpp::export]]
CharacterVector numeric_to_factor(NumericVector num,
    CharacterVector levs, bool na_to_missing = true) {
  CharacterVector charnums = CharacterVector(num.size());
  const int nlevs = levs.size();

  CharacterVector ranged_levels = CharacterVector();
  CharacterVector other_levels = CharacterVector();

  // keep track of negative infinity indices.  Positive infinity sorts itself out.
  int neg_inf_index = -1;
  for (int j = 0; j < nlevs; j++) {
    bool other = false;
    bool neg_inf = false;
    for (unsigned int k = 0; k < strlen(levs[j]); k++ ) {
      if (!(isdigit(levs[j][k]) || levs[j][k] == ',' || levs[j][k] == '.' ||
             levs[j][k] == '(' || levs[j][k] == ')' || levs[j][k] == '[' ||
             levs[j][k] == ']' || levs[j][k] == '-' || levs[j][k] == ' ' ||
             levs[j][k] == 'I' || levs[j][k] == 'n' || levs[j][k] == 'f')) {
        other = true;
        other_levels.push_back(levs[j]);
        break;
      }
      // look for infinity in levels
      if (levs[j][k] == '-' && k < strlen(levs[j]) - 3) {
        if(levs[j][k+1] == 'I' && levs[j][k+2] == 'n' && levs[j][k+3] == 'f')
        neg_inf = true;
      }
    }
    if (!other) {
      ranged_levels.push_back(levs[j]);
      if (neg_inf) {
        neg_inf_index = j;
      }
    }
  }
  if (ranged_levels.size() == 0) {
    for (int row = 0; row < num.size(); row++) charnums[row] = num[row];
    return charnums;
  }

  const int nrlevs = ranged_levels.size();
  std::vector<double> lefts, rights;      // left/right bounds on ranges
  std::vector<bool> leftinc, rightinc; // left/right inclusive

  CharacterVector clean_ranged_levels = CharacterVector(nrlevs);
  for (int j = 0; j < nrlevs; j++) {
    std::string tmp = "";
    for (int k = 0; k < strlen(ranged_levels[j]); k++) {
      if (ranged_levels[j][k] != ' ') tmp += ranged_levels[j][k];
    }
    clean_ranged_levels[j] = tmp;
  }

  // Save minimum value so we can sort -infinity as less than that value
  double min_value = 0;
  bool min_set = false;
  // Compute right and left bounds from ranges
  for (int j = 0; j < nrlevs; j++) {
    if (clean_ranged_levels[j][0] != '[' && clean_ranged_levels[j][0] != '(') {
      // Assume this is just a number
      lefts.push_back(atof(clean_ranged_levels[j]));
      //lefts[j] = atof(clean_ranged_levels[j]);
      rights.push_back(lefts[j]);
      //rights[j] = lefts[j];
      //leftinc[j] = rightinc[j] = true;
      leftinc.push_back(true); rightinc.push_back(true);
      continue;
    }

    int levsize = clean_ranged_levels[j].size();
    leftinc.push_back(clean_ranged_levels[j][0] == '[');
    rightinc.push_back(clean_ranged_levels[j][levsize - 1] == ']');
    //leftinc[j] = clean_ranged_levels[j][0] == '[';
    //rightinc[j] = clean_ranged_levels[j][levsize - 1] == ']';
    // Find the comma
    int comma; for (comma = 0; comma < levsize &&
                   clean_ranged_levels[j][comma] != ','; comma++);
    if (comma == levsize) {
      BEGIN_RCPP
      throw (Rcpp::exception("numeric_to_factor did not find a comma in expected range level", "numeric_to_factorC.cpp", 51));
      END_RCPP
    }
    lefts.push_back(atof( ((std::string)(clean_ranged_levels[j])).substr(1, comma - 1).c_str() ));
    rights.push_back(atof( ((std::string)(clean_ranged_levels[j])).substr(comma + 1, (levsize - 1) - (comma + 1)).c_str() ));
    // lefts[j] = atof( ((std::string)(clean_ranged_levels[j])).substr(1, comma - 1).c_str() );
    // rights[j] = atof( ((std::string)(clean_ranged_levels[j])).substr(comma + 1, (levsize - 1) - (comma + 1)).c_str() );

    // if not the negative infinity level then check if minimum
    if (neg_inf_index != j) {
      if (min_value > lefts[j] || !min_set){
        min_value = lefts[j];
        if (!min_set) min_set = true;
      }
    }
  } // Right & lefts bounds and inclusivity booleans have been set

  // correctly sort negative infinity values trick: Assign -Inf value to minimum left edge value minus 1
  if (neg_inf_index != -1){
    lefts.at(neg_inf_index) = min_value - 1;
  }

  // Optimization trick: sort lefts, remembering the order
  _sort_actuals = std::vector<double>();
  std::vector<int> sorted_indices = std::vector<int>();
  for (int k = 0; k < nrlevs; k++) {
    _sort_actuals.push_back(lefts[k]);
    sorted_indices.push_back(k);
  }
  std::sort(sorted_indices.begin(), sorted_indices.end(), sortshuffle);

  // Now actually restore the levels for each num
  int h, cur;
  for (int row = 0; row < num.size(); row++) {
    double mynum = num[row];

    //Explicitly look for Infinity values and assign to far left or far right bins
    if (mynum == R_NegInf) {
      charnums[row] = ranged_levels[sorted_indices[0]];
      continue;
    } else if (mynum == R_PosInf) {
        charnums[row] = ranged_levels[sorted_indices[sorted_indices.size() - 1]];
        continue;
    }

    //Explicitly look for missing, since out of bounds is no longer missing
    if (NumericVector::is_na(mynum)) {
      if (na_to_missing) {
        charnums[row] = (String)"Missing";
      } else {
        charnums[row] = NA_STRING;
      }
      continue;
    }
    // Round to 8 digits like R seems to do in discretizer.  Shouldn't be necessary but I think it is.
    mynum = roundf(mynum * 100000000) / 100000000;

    // Increment until our number is less than the left bin edge
    for (cur = 0; cur < nrlevs; cur++) {
      h = sorted_indices[cur];
      // To be in the negative infinity bin we have to be less than the next bin over's left-hand side
      if (neg_inf_index == h && cur < nrlevs -1) {
        int h2 = sorted_indices[cur+1];
        if (leftinc[h2] ? mynum < lefts[h2] && !epsilon_compare(mynum,lefts[h2]) : mynum < lefts[h2] || epsilon_compare(mynum,lefts[h2])) {
          break;
        }
      }
      if (leftinc[h] ? mynum < lefts[h] || epsilon_compare(mynum,lefts[h]) :  mynum < lefts[h] && !epsilon_compare(mynum,lefts[h]) ) {
        break;
      }
    }

    // If at leftmost level then either -infinity or out of factor bounds to the left so assign to current (cur == 0).
    // If we're at the last level then we're surely inside that level, since we've accounted for NA_REAL values.
    // If the current left edge is inclusive and we are within epsilon of that edge then we are in that level.
    if (cur == 0 ||
      (cur == nrlevs) ||
      (leftinc[h] && epsilon_compare(mynum, lefts[h]))) {
      charnums[row] = ranged_levels[h];
      continue;
    }
    h = sorted_indices[cur - 1]; // back up, we went too far!

    // we are surely not NA, and surely not at the beginning or end.
    charnums[row] = ranged_levels[h];
  }

  return charnums;
}
